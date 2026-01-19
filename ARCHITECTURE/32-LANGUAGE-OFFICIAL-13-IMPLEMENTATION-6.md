## Ω Language Specification v1.0

### Part 18 — Derived Expressions & Body Contexts à la SICP: `let*`, `letrec`, `scan-out-defines`; Stdlib Macros (`cond/and/or/when/unless`); Minimal Module System with Phase Separation; and the Self‑Hosting “Promote the Expander” Tower

This part is where Ω stops being “a macro toy + evaluator” and becomes a **real language tower** in the SICP sense:

* **Derived expressions** (syntactic sugar) are not bolted on— they are *compiled away* by the expander using hygienic transformations.
* **Body contexts** become first-class (scan-out-defines): internal `define` is not a runtime mutation gimmick; it is a compilation-time `letrec` discipline.
* A **standard library** is expressed in the language (syntax-rules), not in the host.
* A **module system** establishes *phase separation* and makes macros/compile-time code compositional across compilation units.
* A **self-hosting bootstrapping loop** (“strangler fig” / tower closure) moves the expander into Ω itself and then promotes it via a conformance gate.

I’m going to give you concrete, implementable patches to the TS reference implementation you now have, then show the macro library, then module + boot.

---

# 164. Implement `let*`, `letrec`, and `scan-out-defines` (Body Context Discipline)

## 164.1 Why these are the “SICP DNA” for the expander

In SICP terms, you’re taking the idea “most of the language is *derived* from a small kernel” and institutionalizing it in Ω:

* `let*` is *derivable* from `let` (nesting).
* `letrec` is a *compile-time transformation* plus a specific runtime discipline (pre-binding with uninitialized cells).
* Internal `define` is *not* a runtime construct. It is `scan-out-defines` → `letrec`.

In Ω, this is also a clean application of **Refactoring to Patterns**:

* convert ad-hoc runtime behaviors (“dynamic define”) into a stable derived form (letrec),
* enforce invariants (mutual recursion, lexical scope, hygiene),
* preserve deterministic compilation.

---

## 164.2 Add a runtime “uninitialized” sentinel (for correct `letrec`)

### 164.2.1 Patch `src/core/eval/values.ts`

Add a new value variant:

```ts
// src/core/eval/values.ts  (PATCH)
export type Val =
  | { tag: "Unit" }
  | { tag: "Uninit" } // <-- add
  | { tag: "Num"; n: number }
  | { tag: "Bool"; b: boolean }
  | { tag: "Str"; s: string }
  | { tag: "Sym"; name: string }
  | { tag: "Pair"; car: Val; cdr: Val }
  | { tag: "Vector"; items: Val[] }
  | { tag: "Map"; entries: Array<[Val, Val]> }
  | { tag: "Closure"; params: string[]; body: Expr; env: Env }
  | { tag: "Native"; name: string; arity: number | "variadic"; fn: (args: Val[], st: State) => State }
  | { tag: "Cont"; hid: string; boundaryIndex: number; resumption: Resumption };
```

### 164.2.2 Patch `src/core/eval/machineStep.ts` (Var read must reject Uninit)

In the `Var` case:

```ts
case "Var": {
  const addr = envGet(st.env, e.name);
  if (addr === undefined) throw new Error(`unbound var ${e.name}`);
  const v = st.store.read(addr);
  if ((v as any).tag === "Uninit") throw new Error(`uninitialized letrec var ${e.name}`);
  return { tag: "State", state: { ...st, control: { tag: "Val", v } } };
}
```

This gives you the classic Scheme invariant:

> accessing a `letrec` bound variable before initialization is a runtime error.

### 164.2.3 Add a primitive `__uninit` (so lowering can allocate placeholders)

Patch `test/helpers/prims.ts` and your compiler initial environment.

#### `test/helpers/prims.ts`:

```ts
def("__uninit", {
  tag: "Native",
  name: "__uninit",
  arity: 0,
  fn: (_args, s) => ({ ...s, control: { tag: "Val", v: { tag: "Uninit" } as any } })
});
```

#### `src/core/pipeline/compileText.ts` → `initialEnv(moduleScope)`:

Add `"__uninit"` to prims:

```ts
const prims = ["+", "-", "=", "not", "unit", "__uninit"];
```

Now your compiler can lower `letrec` using `(__uninit)` placeholders deterministically.

---

## 164.3 `scan-out-defines`: a general “body context” transform

### 164.3.1 Why “body context” must exist as a concept

A “body” is not just a list of expressions. In Scheme-family languages, it is a **definition context**:

* it may contain internal `define`s (and in richer systems, internal macros)
* those defines are mutually visible and visible throughout the body
* evaluation is `letrec`-like, not sequential mutation

Therefore:

> The expander must treat certain syntactic positions as *body contexts* and apply scan-out-defines there.

We will do it for:

* `lambda` bodies
* `let` / `letrec` bodies
* `begin` in expression position (optional but correct)

### 164.3.2 Patch `src/core/pipeline/compileText.ts` — add `desugarBodyContext`

Add the following helper functions near your expander code:

```ts
// src/core/pipeline/compileText.ts (ADD)
function isDefineForm(stx: Syntax): boolean {
  return isList(stx) && stx.items.length >= 3 && isIdent(stx.items[0]) && (stx.items[0] as SIdent).name === "define";
}

function desugarDefineToBinding(def: SList): { id: SIdent; rhs: Syntax } {
  // (define x rhs)
  // (define (f a b) body...) => (define f (lambda (a b) body...))
  const target = def.items[1];

  if (isList(target)) {
    const sig = target;
    if (sig.items.length < 1 || !isIdent(sig.items[0])) throw new Error("define: bad function signature");
    const f = sig.items[0] as SIdent;
    const args = sig.items.slice(1);

    const lam: Syntax = {
      tag: "List",
      scopes: def.scopes.slice(),
      items: [
        { tag: "Ident", name: "lambda", scopes: (def.items[0] as any).scopes ?? def.scopes.slice() },
        { tag: "List", scopes: def.scopes.slice(), items: args },
        ...def.items.slice(2),
      ],
    };
    return { id: f, rhs: lam };
  }

  const id = expectIdent(target, "define: name must be ident");
  const rhs = def.items[2];
  return { id, rhs };
}

/**
 * Scan out internal defines:
 *   body forms -> either:
 *     - single expr / (begin ...) if no defines
 *     - (letrec ((x rhs) ...) body...) if defines exist
 *
 * This is SICP scan-out-defines generalized.
 */
function desugarBodyContext(forms: Syntax[]): Syntax {
  const defs: Array<{ id: SIdent; rhs: Syntax }> = [];
  const nonDefs: Syntax[] = [];

  for (const f of forms) {
    if (isDefineForm(f)) {
      const d = expectList(f, "define must be list") as SList;
      defs.push(desugarDefineToBinding(d));
    } else {
      nonDefs.push(f);
    }
  }

  if (defs.length === 0) {
    if (nonDefs.length === 0) return { tag: "Atom", value: null, scopes: [] }; // Unit
    if (nonDefs.length === 1) return nonDefs[0];
    return {
      tag: "List",
      scopes: nonDefs[0].scopes.slice(),
      items: [{ tag: "Ident", name: "begin", scopes: nonDefs[0].scopes.slice() }, ...nonDefs],
    };
  }

  const scopeRef = (defs[0]?.id.scopes ?? nonDefs[0]?.scopes ?? []).slice();

  const bindsList: Syntax = {
    tag: "List",
    scopes: scopeRef,
    items: defs.map(({ id, rhs }) => ({
      tag: "List",
      scopes: scopeRef,
      items: [id, rhs],
    })),
  };

  const bodyTail = nonDefs.length === 0 ? [{ tag: "Atom", value: null, scopes: scopeRef }] : nonDefs;

  return {
    tag: "List",
    scopes: scopeRef,
    items: [{ tag: "Ident", name: "letrec", scopes: scopeRef }, bindsList, ...bodyTail],
  };
}
```

This is the compilation-time “definition context” transform.

---

## 164.4 Implement `letrec` binder-scope insertion in the expander

### 164.4.1 Patch `expandExpr` to recognize `let*` and `letrec`

In the `switch(h.name)` within `expandExpr`, add:

```ts
case "let*":
  return expandLetStar(stx, env, c);

case "letrec":
  return expandLetRec(stx, env, c);
```

### 164.4.2 Implement `expandLetStar` (derived from nested `let`)

```ts
// src/core/pipeline/compileText.ts (ADD)
function expandLetStar(stx: SList, env: Env, c: Counters): ExpandRes {
  // (let* ((x e) (y f) ...) body...)
  if (stx.items.length < 3) throw new Error("let*: expected (let* ((x e) ...) body...)");
  const bindsList = expectList(stx.items[1], "let*: bindings must be list");
  const bodyForms = stx.items.slice(2);

  // rewrite to nested lets:
  // (let* () body...) => (let () body...) or (begin body...)
  let nested: Syntax = {
    tag: "List",
    scopes: stx.scopes.slice(),
    items: [{ tag: "Ident", name: "let", scopes: stx.scopes.slice() }, { tag: "List", scopes: stx.scopes.slice(), items: [] }, ...bodyForms],
  };

  // fold from right to left
  for (let i = bindsList.items.length - 1; i >= 0; i--) {
    const bp = expectList(bindsList.items[i], "let*: binding must be list");
    if (bp.items.length !== 2) throw new Error("let*: binding must be (x init)");
    const id = bp.items[0];
    const init = bp.items[1];

    nested = {
      tag: "List",
      scopes: stx.scopes.slice(),
      items: [
        { tag: "Ident", name: "let", scopes: stx.scopes.slice() },
        { tag: "List", scopes: stx.scopes.slice(), items: [
          { tag: "List", scopes: stx.scopes.slice(), items: [id, init] }
        ]},
        nested,
      ],
    };
  }

  return expandExpr(nested, env, c);
}
```

This is textbook derived-expression compilation: `let*` becomes a syntax-level fold into nested `let`, and the existing binder-scope insertion for `let` provides correct hygiene and lexical semantics.

### 164.4.3 Implement `expandLetRec` (recursive binding region)

```ts
// src/core/pipeline/compileText.ts (ADD)
function expandLetRec(stx: SList, env: Env, c: Counters): ExpandRes {
  // (letrec ((x e) ...) body...)
  if (stx.items.length < 3) throw new Error("letrec: expected (letrec ((x e) ...) body...)");
  const bindsList = expectList(stx.items[1], "letrec: bindings must be list");
  const bodyForms = stx.items.slice(2);

  const B = freshScope(c.scope);

  // Scope binders with B; gather raw inits
  const bindPairs: Array<{ idB: SIdent; init0: Syntax }> = [];
  for (const bp0 of bindsList.items) {
    const bp = expectList(bp0, "letrec: binding must be list");
    if (bp.items.length !== 2) throw new Error("letrec: binding must be (x init)");
    const id = expectIdent(bp.items[0], "letrec: binder must be ident");
    const init0 = bp.items[1];
    const idB = addScope(id, B) as SIdent;
    bindPairs.push({ idB, init0 });
  }

  // Install binders first (recursive visibility)
  let Γ = env;
  for (const { idB } of bindPairs) {
    const bid = freshBid(c);
    Γ = bindValue(Γ, idB, bid, internalName(idB.name, bid));
  }

  // Expand initializers under Γ with binder scope in effect (addScope init0 B)
  const bindPairsExpanded: Array<{ idB: SIdent; init: Syntax }> = [];
  let Γ2 = Γ;
  for (const { idB, init0 } of bindPairs) {
    const initScoped = addScope(init0, B);
    const r = expandExpr(initScoped, Γ2, c);
    Γ2 = r.env;
    bindPairsExpanded.push({ idB, init: r.stx });
  }

  // Expand body as a body context (scan-out-defines) under binder scope
  const bodyScoped = bodyForms.map(b => addScope(b, B));
  const bodyDesugared = desugarBodyContext(bodyScoped);
  const bodyR = expandExpr(bodyDesugared, Γ2, c);
  Γ2 = bodyR.env;

  const bindsOut: Syntax = {
    ...bindsList,
    items: bindPairsExpanded.map(({ idB, init }) => ({
      tag: "List",
      scopes: stx.scopes.slice(),
      items: [idB, init],
    })),
  };

  const stx2: Syntax = { ...stx, items: [stx.items[0], bindsOut, bodyR.stx] };
  return { stx: stx2, env: Γ2 };
}
```

Key semantics:

* binders installed before initializer expansion
* binder scope applied to initializers and body
* body is a **body context** with scan-out-defines (so nested internal defines become inner letrec)

---

## 164.5 Update `expandLambda` and `expandLet` to use `desugarBodyContext`

### 164.5.1 Patch `expandLambda`

Replace the body expansion loop with:

```ts
// src/core/pipeline/compileText.ts (PATCH inside expandLambda)

// Add binder scope to body forms BEFORE expanding
const bodyScoped = bodyForms.map(b => addScope(b, B));

// body context: scan-out-defines -> letrec, then expand as single expression
const bodyDesugared = desugarBodyContext(bodyScoped);
const bodyR = expandExpr(bodyDesugared, Γ, c);
let Γ2 = bodyR.env;

const stx2: Syntax = {
  ...stx,
  items: [
    stx.items[0],
    { ...paramsList, items: paramsB },
    bodyR.stx, // single body expression
  ],
};

return { stx: stx2, env: Γ2 };
```

Now internal defines in lambda bodies work.

### 164.5.2 Patch `expandLet` similarly

At the end of `expandLet` (after installing binders), replace body expansion loop with:

```ts
const bodyScoped = bodyForms.map(b => addScope(b, B));
const bodyDesugared = desugarBodyContext(bodyScoped);
const bodyR = expandExpr(bodyDesugared, Γ, c);
let Γ2 = bodyR.env;

const stx2: Syntax = {
  ...stx,
  items: [stx.items[0], bindsOut, bodyR.stx],
};

return { stx: stx2, env: Γ2 };
```

Now internal defines work in `let` bodies too, which is the correct generalization of “definition contexts.”

### 164.5.3 Patch `begin` in expression position

You want `begin` in expression position to behave like a body context too. Modify the `case "begin"` in `expandExpr`:

Replace the previous sequential `expandSequence` behavior (which was top-level flavored) with:

```ts
case "begin": {
  const forms = stx.items.slice(1);
  const bodyDesugared = desugarBodyContext(forms);
  const r = expandExpr(bodyDesugared, env, c);
  return r;
}
```

This yields a **single expanded expression** for `(begin ...)` in expression position, and internal defines get `letrec` semantics.

> Keep `expandTop`’s handling of the top-level `(begin ...)` as *sequential define-syntax/define processing*. That is the “module body” context, not the general expression begin.

---

# 165. Hygienic Lowering for `letrec` (Correct, Bid‑Renamed, Uninit‑Safe)

Now that the expander produces `letrec`, you must lower it to core `Expr`. Your core doesn’t have `LetRec`; we’ll lower to lambda + placeholders + `set!`, which is the standard compilation strategy.

## 165.1 Patch `src/core/pipeline/lower.ts` to handle `"letrec"`

Add a case in the `switch (h.name)`:

```ts
case "letrec": {
  // (letrec ((x e) ...) body...)
  if (items.length < 3) throw new Error("letrec: expected (letrec ((x e) ...) body...)");
  const bindsList = expectList(items[1], "letrec: bindings must be list");
  const bodyForms = items.slice(2);

  const bindPairs = bindsList.items.map(bp0 => {
    const bp = expectList(bp0, "letrec: binding must be list");
    if (bp.items.length !== 2) throw new Error("letrec: binding must be (x init)");
    const id = expectIdent(bp.items[0], "letrec: binder must be ident");
    const b = resolveIdent(id, env, 0);
    if (!b || b.kind !== "value") throw new Error("letrec binder did not resolve");
    return { internal: b.value as string, init: lowerSyntax(bp.items[1], env) };
  });

  const params = bindPairs.map(p => p.internal);

  // placeholders are (__uninit) calls
  const placeholder: Expr = { tag: "App", fn: { tag: "Var", name: "__uninit" }, args: [] };
  const args = bindPairs.map(_ => placeholder);

  // body: (begin (set! x e) ... bodyForms...)
  const sets: Expr[] = bindPairs.map(p => ({ tag: "Set", name: p.internal, rhs: p.init }));
  const loweredBodyForms = bodyForms.map(x => lowerSyntax(x, env));
  const bodySeq = sets.concat(loweredBodyForms);
  const body: Expr = bodySeq.length === 1 ? bodySeq[0] : { tag: "Begin", exprs: bodySeq };

  const lam: Expr = { tag: "Lambda", params, body };
  return { tag: "App", fn: lam, args };
}
```

This gives you a `letrec` that:

* allocates cells before init evaluation,
* traps reads before initialization via `Uninit` checks,
* still allows init expressions to reference other letrec binders (as required).

---

# 166. Standard Library Macros (Written in Ω, Expanded Hygienically)

Now you can write the “derived forms” as `define-syntax` macros using `syntax-rules`, instead of hardcoding them.

A clean move is: create a `prelude.omega` that you load (or compile-in) before user modules.

## 166.1 `cond`

Minimal `cond` (no `=>` clause in this baseline):

```scheme
(define-syntax cond
  (syntax-rules (else)
    ((cond) #f)
    ((cond (else e1 e2 ...))
     (begin e1 e2 ...))
    ((cond (t e1 e2 ...) c1 c2 ...)
     (if t
         (begin e1 e2 ...)
         (cond c1 c2 ...)))))
```

Properties:

* `else` is a literal keyword
* macro recursion is hygienic
* derived solely from `if` and `begin`

## 166.2 `and`

```scheme
(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and x) x)
    ((and x y ...)
     (if x (and y ...) #f))))
```

## 166.3 `or` (requires a temp; hygiene matters)

```scheme
(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or x) x)
    ((or x y ...)
     (let ((t x))
       (if t t (or y ...))))))
```

Because your `syntax-rules` introducer scope is correct (Part 17 fix), the introduced `t` is hygienic and will not be captured by user code.

## 166.4 `when` / `unless`

```scheme
(define-syntax when
  (syntax-rules ()
    ((when t e1 e2 ...)
     (if t (begin e1 e2 ...) (unit)))))

(define-syntax unless
  (syntax-rules ()
    ((unless t e1 e2 ...)
     (if t (unit) (begin e1 e2 ...)))))
```

## 166.5 `let*` as a macro (optional: to make it purely derived)

Even though we implemented `let*` as an expander rewrite in TS, you can also define it in Ω:

```scheme
(define-syntax let*
  (syntax-rules ()
    ((let* () b1 b2 ...) (begin b1 b2 ...))
    ((let* ((x e) more ...) b1 b2 ...)
     (let ((x e))
       (let* (more ...) b1 b2 ...)))))
```

This version is a perfect SICP-style derived expression.

---

# 167. Minimal Module System With Phase Separation (0 Runtime / 1 Macro)

Now we go beyond the “single compilation unit harness” and formalize a true compilation pipeline.

## 167.1 The module form

A module is a syntactic object of the form:

```scheme
(module <ModuleName>
  (provide x y (for-syntax m n))
  (require A B)
  (require-for-syntax C)
  <body-forms...>)
```

### Semantics, precisely

* Each module has a **module scope** `M0` at phase 0 and `M1` at phase 1 (and so on if you later add phases).
* Body expansion happens at phase 0 with access to phase 1 transformers.
* `define` introduces phase 0 bindings.
* `define-syntax` introduces phase 1 bindings.
* `provide` exports selected bindings (by binding identity / bid).
* `require` imports a module’s:

  * phase 0 value bindings into the importing module’s phase 0 env (alias bindings)
  * phase 1 syntax bindings into the importing module’s phase 1 env (alias bindings)
  * **and** retains the imported module’s original bindings in the compilation universe for def-site expansion resolution (see below)
* `require-for-syntax` imports module **phase 0** values into importing module **phase 1** (phase shift +1), to allow compile-time helper functions.

## 167.2 The hard problem: macro def-site identifiers that refer to non-exported bindings

A macro from module A can expand into code referencing internal helpers from module A that were never exported.

To preserve correctness, the importing compilation environment must include:

* A’s **entire binding table** (or at least the reachable closure) so that introduced def-site identifiers can resolve by scopes and bids.

Therefore module artifacts must carry:

* full binding tables for phases 0 and 1
* not just exports

### The design pattern here

This is a **Linker** / **Repository** pattern:

* compilation produces a module artifact with symbol tables
* later compilation units import those tables for resolution and linking

## 167.3 Artifact types (TS skeleton)

```ts
// src/core/modules/artifact.ts
import type { Binding } from "../syntax/binding";
import type { Expr } from "../ast";

export type ModuleExports = {
  phase0: Array<{ name: string; bid: string; internal: string }>;
  phase1: Array<{ name: string; bid: string }>; // transformers referenced by bid
};

export type ModuleArtifact = {
  moduleName: string;
  moduleScope0: string;
  moduleScope1: string;

  // full binding tables (needed for def-site macro expansion)
  bindings: Binding[];

  // compiled init expression for runtime (phase0)
  init: Expr;

  exports: ModuleExports;

  // hermetic receipts and digests (if you enable strict builds)
  expandedHash?: string;
  coreHash?: string;
  receipts?: unknown[];
  deps: string[];
};
```

## 167.4 Require/provide mechanics (minimal but correct)

### Provide

When compiling a module, the `provide` form selects certain binding identities:

* `(provide x y)` exports those phase0 bindings
* `(provide (for-syntax m))` exports those phase1 macro bindings

In a hygienic system, `x` is resolved by `resolveIdent` in the module’s env at the corresponding phase and the **bid** is what’s exported.

### Require (compile-time)

Requiring module A into module B does two things:

1. **Universe import**: merge all of A’s bindings into B’s compilation environment unchanged.
   This enables def-site identifiers from A (scoped with A’s module scopes) to resolve.

2. **Alias import for user references**: for each exported binding from A, create an alias binding that applies in B’s module scope, but keeps the same bid.

For a phase0 export `x` from A, create in B:

* `bid`: same as A’s `x`
* `name`: `"x"`
* `scopes`: `[M0_B]` (so it matches occurrences in B)
* `phase`: 0
* `kind`: `"value"`
* `value`: the exported `internal` string (which is globally unique, because it contains the original bid)

For a phase1 macro export `m`, create in B:

* same bid/name
* scopes `[M1_B]` (or `[M0_B]` depending on how you represent macro use-site scopes; simplest is `[M0_B]` since macros are used to expand phase0 syntax)
* phase: 1
* kind: `"syntax"`
* value: transformer object (or a reference to it in the artifact store)

### Require-for-syntax (phase shift)

Import A’s phase0 exports into B’s phase1 environment:

* phase: 1
* scopes: `[M1_B]`
* value: same internal name string (so compile-time evaluation can call it)

This gives you compile-time helper functions without duplicating runtime logic.

---

# 168. Self‑Hosting: Ω₀ Boot Modules and “Promote the Expander” Tower Closure

Now that:

* the evaluator is correct (Part 16),
* the expander is correct (Parts 13–18),
* and you have modules + phase separation in the spec,

you can implement the self-host tower.

## 168.1 Ω₀ goal

Ω₀ is the **trusted base**:

* no macros required to run Ω₀ programs
* enough primitives to implement syntax objects, syntax-rules, and the expander *in Ω itself*

This is the classic compiler bootstrap strategy:

* implement a minimal evaluator in host
* implement a richer compiler in the language itself
* use the host only as a “seed”

## 168.2 Boot module set

You want modules:

1. `omega.boot.syntax0`

   * syntax objects, scopes, `add-scope`, `fresh-scope`
   * binding records with `bid`
   * `resolve-id`, `free-identifier=?`

2. `omega.boot.syntax_rules0`

   * `syntax-rules` compiler/matcher/template expander
   * nested ellipses

3. `omega.boot.expander0`

   * macro expansion loop
   * binder-scope insertion (`lambda/let/letrec/define/define-syntax`)
   * body context scan-out-defines

4. `omega.boot.build0`

   * artifact digesting
   * macro receipt replay rules (even if oracle is disabled in strict mode)

These are written in Ω₀ (core language only).

## 168.3 Bootstrap pipeline (deterministic)

**Stage A — Seed**

* Host TS loads Ω₀ module sources.
* Host compiles Ω₀ sources to core `Expr` using host expander (which can be configured to treat Ω₀ as macro-free).
* Host evaluator evaluates Ω₀ compiled modules, producing Ω-level functions:

  * `expand0`
  * `compile-syntax-rules0`
  * etc.

**Stage B — First tower step**

* Use Ω’s own `expand0` to expand and compile `omega.base_macros` (your stdlib macros in Ω).
* Store resulting module artifact in CAS.

**Stage C — Recompile expander**

* Now compile `omega.expander` (written in Ω) using the *same* expansion pipeline.
* Compare:

  * host-expanded core AST vs Ω-expanded core AST (alpha-normalized)
  * macro receipts and digests
* If equivalent on the conformance suite, mark Ω expander artifact as **candidate**.

**Stage D — Promote**

* Run macro torture suite (Hygiene + Ellipses + Phase + Body context)
* Run differential suite (host vs self-hosted)
* Mark as **trusted**
* Switch registry pointer `defaultExpanderDigest` to the Ω expander

This is your “Promote the Expander” command. It is literally a **Stage-Gate** / **Deployment Pipeline** specialized for language semantics.

## 168.4 Why this is robust even when inference exists

Even after you add inference to the language, strict compilation remains hermetic because:

* semantic macro expansions must be receipted
* obligations must be discharged before promotion
* replay uses receipts rather than re-calling oracles

Self-hosting doesn’t collapse into “whatever the model said” because compilation is governed by the artifact store and promotion gate.

---

# 169. Where We Go Next (The Remaining “Big Rocks”)

You’ve now got:

* real eval (CEKS)
* real macros (`syntax-rules` with nested ellipses + binding-identity literals + correct introducer behavior)
* real body contexts (`scan-out-defines` → `letrec`)
* derived expressions in the SICP sense
* the shape of a module system and bootstrap plan

If you say **continue**, Part 19 will do two things:

1. **Finish the module system in code** (TS reference implementation):

   * module artifact format
   * compile module with `provide/require/require-for-syntax`
   * runtime loader: topological init, linking runtime internals
   * phase env tower (0/1) and begin-for-syntax

2. **Start the inference plane “the right way”** (no hand-waving):

   * formal `infer` effect signature
   * semantic macro receipts + obligation objects
   * deterministic envelope + replay
   * integration of inference with nondet search as heuristic scoring (Strategy + Policy objects)

That’s where Ω becomes the “eval/apply + inference plane + search plane” language you’ve been driving at, without degenerating into an API wrapper.
