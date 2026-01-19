## Ω Language Specification v1.0

### Part 20 — Phase‑Tower Compilation and Instantiation: `begin-for-syntax` Evaluation, Procedural Transformers (Ω closures), a Minimal Syntax API (`SyntaxVal`), Module Graph Init (Topo + SCC), Transformer Serialization, CAS Persistence, and an End‑to‑End Macro That Calls `infer` at Compile Time (Receipted + Replayable)

You now have: CEKS evaluation, hygienic `syntax-rules`, derived forms, scan‑out‑defines, and a module artifact plan. What was still missing (per Part 19) was the *actual phase‑tower machinery*—the part that makes a Lisp truly “make up languages” instead of “expand some forms.”

This part closes that gap with a **Racket‑style phase tower**:

* expansion at phase `p` consults macro bindings at phase `p+1`
* `begin-for-syntax` shifts the expansion/evaluation context to phase `p+1`
* `define-syntax` RHS is evaluated at phase `p+1` **when it is procedural**, producing a transformer **value** (closure)
* procedural transformers run in the phase `p+1` evaluator and receive/return **syntax objects** (`SyntaxVal`)
* module artifacts don’t serialize closures; instead they serialize **phase init programs**, so closures can be reconstructed by instantiating the module at the right phase (Interpreter + Abstract Factory + Repository).

I’m going to deliver this as **concrete code-shaped reference** with very explicit invariants.

---

# 183. Fix the Core Binding Model: Separate Binding Spaces (`value` vs `syntax`) and Phase‑Parametric Resolution

Up to now, your compile-time `Env` resolution didn’t distinguish value bindings and syntax bindings. That becomes unsound the moment you want procedural macros (where the macro name is a **syntax binding** and may also be a **compile-time value**).

This is exactly the classic “two namespaces” issue (Scheme/Racket: value namespace vs syntax namespace). We represent it explicitly.

## 183.1 Patch `src/core/syntax/binding.ts`

```ts
// src/core/syntax/binding.ts  (PATCH)
import type { Scope, SIdent } from "./syntax";

export type Bid = string;

export type BindingKind = "value" | "syntax" | "module";

export type Binding = {
  bid: Bid;
  name: string;
  scopes: Scope[];
  phase: number;
  kind: BindingKind;
  value: unknown;         // internal name string for values, transformer refs for syntax, etc.
};

export type Env = Binding[];

function subset(a: Scope[], b: Scope[]): boolean {
  for (const sc of a) if (!b.includes(sc)) return false;
  return true;
}

/**
 * Resolve in a specific binding space (kind) at a specific phase.
 * This is the fundamental operation behind:
 *  - hygienic value references
 *  - hygienic macro lookup
 *  - free-identifier=? literal identity
 */
export function resolveIdent(
  id: SIdent,
  env: Env,
  phase: number,
  kind: BindingKind
): Binding | null {
  const candidates = env.filter(b =>
    b.kind === kind &&
    b.phase === phase &&
    b.name === id.name &&
    subset(b.scopes, id.scopes)
  );

  if (candidates.length === 0) return null;

  // maximal |scopes|
  candidates.sort((a, b) => b.scopes.length - a.scopes.length);
  const best = candidates[0];
  const second = candidates[1];
  if (second && second.scopes.length === best.scopes.length) {
    throw new Error(`resolveIdent ambiguity: ${id.name} at phase ${phase} in ${kind} space`);
  }
  return best;
}

/**
 * free-identifier=? parameterized by binding space.
 * Pattern literals should typically be checked in value space (shadowable keywords),
 * while macro lookup uses syntax space.
 */
export function freeIdentifierEq(
  id1: SIdent, env1: Env, phase: number, kind: BindingKind,
  id2: SIdent, env2: Env
): boolean {
  const b1 = resolveIdent(id1, env1, phase, kind);
  const b2 = resolveIdent(id2, env2, phase, kind);
  if (b1 && b2) return b1.bid === b2.bid;
  if (!b1 && !b2) return id1.name === id2.name;
  return false;
}
```

**Design pattern framing**: This is a **Type Object / Policy** split for identifier resolution. The “policy” is the binding space. It is also a clean **Separation of Concerns** refactoring: you stop accidentally conflating value resolution and macro lookup.

---

# 184. Introduce `SyntaxVal` as a Runtime Value (Procedural Macros Need It)

Procedural transformers must *consume and produce* syntax objects. That requires a runtime value type.

## 184.1 Patch `src/core/eval/values.ts`

```ts
// src/core/eval/values.ts  (PATCH)
import type { Syntax } from "../syntax/syntax";

// add:
export type Val =
  | { tag: "Unit" }
  | { tag: "Uninit" }
  | { tag: "Num"; n: number }
  | { tag: "Bool"; b: boolean }
  | { tag: "Str"; s: string }
  | { tag: "Sym"; name: string }
  | { tag: "Pair"; car: Val; cdr: Val }
  | { tag: "Vector"; items: Val[] }
  | { tag: "Map"; entries: Array<[Val, Val]> }
  | { tag: "Syntax"; stx: Syntax }             // <-- NEW
  | { tag: "Closure"; params: string[]; body: Expr; env: Env }
  | { tag: "Native"; name: string; arity: number | "variadic"; fn: (args: Val[], st: State) => State }
  | { tag: "Cont"; hid: string; boundaryIndex: number; resumption: Resumption };
```

Now syntax can exist at runtime—crucial for transformer procedures, inference rewriting, and later for reflective tooling (REPL expansions, macro stepping, etc.)

---

# 185. Syntax API Primitives for Transformer Phase (Minimal but Sufficient)

These are **compile-time** primitives. They belong in the module instantiation environment at phase ≥ 1.

You want the smallest set that makes procedural macros ergonomic and principled:

* `syntax? : any → bool`
* `syntax->datum : Syntax → DatumVal`
* `datum->syntax : Syntax × DatumVal → Syntax`
* `syntax-ident?`, `syntax-list?`
* `syntax-list-items : Syntax → Vector Syntax`
* `syntax-ident-name : Syntax → string`
* `syntax-add-scope : Syntax × string → Syntax` (for deliberate capture/escape hatches)
* `syntax-scopes : Syntax → Vector string` (debug/introspection only)

## 185.1 `src/core/eval/syntaxPrims.ts`

```ts
// src/core/eval/syntaxPrims.ts
import type { Val } from "./values";
import type { State } from "./machine";
import { VTrue, VFalse, VUnit } from "./values";
import type { Syntax } from "../syntax/syntax";
import { addScope, isIdent, isList } from "../syntax/syntax";

function isSyntaxVal(v: Val): v is { tag: "Syntax"; stx: Syntax } {
  return v.tag === "Syntax";
}

function valToDatum(v: Val): unknown {
  switch (v.tag) {
    case "Unit": return null;
    case "Num": return v.n;
    case "Bool": return v.b;
    case "Str": return v.s;
    case "Sym": return { sym: v.name };
    case "Vector": return v.items.map(valToDatum);
    case "Syntax": return syntaxToDatum(v.stx);
    default:
      return { opaque: v.tag };
  }
}

function datumToVal(d: unknown): Val {
  if (d === null) return { tag: "Unit" };
  if (typeof d === "number") return { tag: "Num", n: d };
  if (typeof d === "string") return { tag: "Str", s: d };
  if (typeof d === "boolean") return { tag: "Bool", b: d };
  if (typeof d === "object" && d !== null && !Array.isArray(d) && "sym" in d) {
    return { tag: "Sym", name: (d as any).sym };
  }
  if (Array.isArray(d)) return { tag: "Vector", items: d.map(datumToVal) };
  return { tag: "Str", s: JSON.stringify(d) };
}

function syntaxToDatum(stx: Syntax): unknown {
  if (stx.tag === "Atom") return stx.value;
  if (stx.tag === "Ident") return { sym: stx.name };
  return stx.items.map(syntaxToDatum);
}

function datumToSyntaxWithScopes(d: unknown, scopes: string[]): Syntax {
  if (d === null || typeof d === "number" || typeof d === "string" || typeof d === "boolean") {
    return { tag: "Atom", value: d as any, scopes: scopes.slice() };
  }
  if (typeof d === "object" && d !== null && !Array.isArray(d) && "sym" in d) {
    return { tag: "Ident", name: (d as any).sym, scopes: scopes.slice() };
  }
  if (Array.isArray(d)) {
    return { tag: "List", items: d.map(x => datumToSyntaxWithScopes(x, scopes)), scopes: scopes.slice() };
  }
  // fallback
  return { tag: "Atom", value: JSON.stringify(d), scopes: scopes.slice() };
}

export function syntaxPrims(): Array<{ name: string; arity: number | "variadic"; fn: (args: Val[], st: State) => State }> {
  return [
    {
      name: "syntax?",
      arity: 1,
      fn: ([v], st) => ({ ...st, control: { tag: "Val", v: isSyntaxVal(v) ? VTrue : VFalse } }),
    },
    {
      name: "syntax->datum",
      arity: 1,
      fn: ([v], st) => {
        if (!isSyntaxVal(v)) throw new Error("syntax->datum: expected Syntax");
        return { ...st, control: { tag: "Val", v: datumToVal(syntaxToDatum(v.stx)) } };
      },
    },
    {
      name: "datum->syntax",
      arity: 2,
      fn: ([ctx, dat], st) => {
        if (!isSyntaxVal(ctx)) throw new Error("datum->syntax: ctx must be Syntax");
        const scopes = ctx.stx.scopes;
        const d = valToDatum(dat);
        const out: Syntax = datumToSyntaxWithScopes(d, scopes);
        return { ...st, control: { tag: "Val", v: { tag: "Syntax", stx: out } } };
      },
    },
    {
      name: "syntax-ident?",
      arity: 1,
      fn: ([v], st) => {
        if (!isSyntaxVal(v)) return { ...st, control: { tag: "Val", v: VFalse } };
        return { ...st, control: { tag: "Val", v: isIdent(v.stx) ? VTrue : VFalse } };
      },
    },
    {
      name: "syntax-list?",
      arity: 1,
      fn: ([v], st) => {
        if (!isSyntaxVal(v)) return { ...st, control: { tag: "Val", v: VFalse } };
        return { ...st, control: { tag: "Val", v: isList(v.stx) ? VTrue : VFalse } };
      },
    },
    {
      name: "syntax-ident-name",
      arity: 1,
      fn: ([v], st) => {
        if (!isSyntaxVal(v) || v.stx.tag !== "Ident") throw new Error("syntax-ident-name: expected identifier syntax");
        return { ...st, control: { tag: "Val", v: { tag: "Str", s: v.stx.name } } };
      },
    },
    {
      name: "syntax-list-items",
      arity: 1,
      fn: ([v], st) => {
        if (!isSyntaxVal(v) || v.stx.tag !== "List") throw new Error("syntax-list-items: expected list syntax");
        const items: Val[] = v.stx.items.map(it => ({ tag: "Syntax", stx: it }));
        return { ...st, control: { tag: "Val", v: { tag: "Vector", items } } };
      },
    },
    {
      name: "syntax-add-scope",
      arity: 2,
      fn: ([v, sc], st) => {
        if (!isSyntaxVal(v)) throw new Error("syntax-add-scope: expected Syntax");
        if (sc.tag !== "Str") throw new Error("syntax-add-scope: expected string scope");
        return { ...st, control: { tag: "Val", v: { tag: "Syntax", stx: addScope(v.stx, sc.s) } } };
      },
    },
    {
      name: "syntax-scopes",
      arity: 1,
      fn: ([v], st) => {
        if (!isSyntaxVal(v)) throw new Error("syntax-scopes: expected Syntax");
        return { ...st, control: { tag: "Val", v: { tag: "Vector", items: v.stx.scopes.map(s => ({ tag: "Str", s })) } } };
      },
    },
    {
      name: "syntax-local-introduce",
      arity: 1,
      fn: ([v], st) => {
        // reference-grade: identity; in a full system this would add the current introducer scope.
        if (!isSyntaxVal(v)) throw new Error("syntax-local-introduce: expected Syntax");
        return { ...st, control: { tag: "Val", v } };
      },
    },
    {
      name: "unit",
      arity: 0,
      fn: (_args, st) => ({ ...st, control: { tag: "Val", v: VUnit } }),
    },
  ];
}
```

**Design patterns**:

* **Adapter**: the syntax API is a host adapter providing “operational semantics” for syntax objects.
* **Facade**: gives transformer authors a stable minimal interface.
* **Interpreter**: procedural transformers are programs in Ω interpreted by the CEKS machine.

---

# 186. Procedural Transformers: A Formal Protocol and a Runtime Reference Implementation

We now extend “transformer value” beyond `syntax-rules`.

## 186.1 Transformer binding value types

Create a small sum type:

```ts
// src/core/expand/transformer.ts
import type { SRTransformer } from "./syntaxRules";

export type TransformerVal =
  | { tag: "SyntaxRules"; tr: SRTransformer }
  | { tag: "ProcRef"; originModule: string; phase: number; internal: string }; // closure stored in module instance at phase
```

A `ProcRef` is serializable (unlike a closure), and can be resolved by instantiating `originModule` at `phase` and reading `internal` from its runtime environment.

---

# 187. Phase‑Parametric Lowering: `lowerSyntax(stx, Γ, phase)`

Your lowering pass must resolve identifiers at the *current* phase in the **value** namespace.

## 187.1 Patch signature in `src/core/pipeline/lower.ts`

Change:

```ts
export function lowerSyntax(stx: Syntax, env: Env): Expr
```

to:

```ts
export function lowerSyntax(stx: Syntax, env: Env, phase: number): Expr
```

And change `lowerVar` to resolve in **value** kind at the correct phase:

```ts
const b = resolveIdent(id, env, phase, "value");
```

All occurrences of `resolveIdent(..., 0)` become `resolveIdent(..., phase, "value")`.

This is not optional. It is the core of phase separation correctness.

---

# 188. Phase‑Parametric Expansion: `expandExpr(stx, Γ, phase)` With Macro Lookup at `phase+1`

Now we generalize the expander you wrote in Part 17/18:

* Value bindings (`define`, binder params) are at phase `p`
* Macro bindings (`define-syntax`) are at phase `p+1`, kind `"syntax"`
* Macro lookup for a form being expanded at phase `p` resolves head in **syntax** kind at phase `p+1`

### Critical: procedural `define-syntax` RHS evaluation happens at phase `p+1`.

This is exactly the “eval at transformer phase” that turns macros into a real language plane.

---

# 189. Module Instantiation by Phase (Interpreter Reuse): `instantiate(module, phase)`

This is the missing runtime for the phase tower.

## 189.1 Instance data model

```ts
// src/core/modules/instance.ts
import type { Env as RTEnv } from "../eval/env";
import type { Store } from "../eval/store";

export type PhaseInstance = {
  env: RTEnv;     // runtime environment (internalName -> addr)
  store: Store;   // runtime store
};

export type InstanceCache = Map<string, Map<number, PhaseInstance>>; // moduleName -> phase -> instance
```

**Invariant**:

> Module instantiation at phase `p` is memoized: it must run exactly once per (module, phase) in a compilation session (unless you explicitly support re-instantiation).

This is a **Flyweight** / **Cache** pattern requirement. It also matches Racket’s module instantiation model.

---

## 189.2 Instantiation algorithm (normative)

To instantiate module `M` at phase `p`:

1. ensure all `(require X)` dependencies are instantiated at phase `p`
2. ensure all `(require-for-syntax Y)` dependencies are instantiated at phase `p+1`
3. create a fresh runtime env/store for `M@p` with the appropriate primitives:

   * phase 0: base runtime primitives
   * phase ≥ 1: base primitives + syntax API primitives + inference/commit effects allowed by policy
4. evaluate `M.initByPhase[p]` (a core `Expr` produced by compilation for that phase)
5. cache and return the resulting `PhaseInstance`

---

# 190. Module Graph Initialization: Toposort + SCC Cycle Errors

A real module system must detect cycles and produce useful diagnostics.

This is the **Composite + Graph** problem plus a standard **Tarjan SCC** solution.

## 190.1 `src/core/modules/graph.ts`

```ts
// src/core/modules/graph.ts
export type DepGraph = Map<string, string[]>; // module -> deps

export function topoOrScc(graph: DepGraph): { tag: "Topo"; order: string[] } | { tag: "Cycle"; scc: string[] } {
  const index = new Map<string, number>();
  const lowlink = new Map<string, number>();
  const onStack = new Set<string>();
  const stack: string[] = [];
  let idx = 0;
  const sccs: string[][] = [];

  function strongconnect(v: string) {
    index.set(v, idx);
    lowlink.set(v, idx);
    idx++;
    stack.push(v);
    onStack.add(v);

    for (const w of graph.get(v) ?? []) {
      if (!index.has(w)) {
        strongconnect(w);
        lowlink.set(v, Math.min(lowlink.get(v)!, lowlink.get(w)!));
      } else if (onStack.has(w)) {
        lowlink.set(v, Math.min(lowlink.get(v)!, index.get(w)!));
      }
    }

    if (lowlink.get(v) === index.get(v)) {
      const scc: string[] = [];
      while (true) {
        const w = stack.pop()!;
        onStack.delete(w);
        scc.push(w);
        if (w === v) break;
      }
      sccs.push(scc);
    }
  }

  for (const v of graph.keys()) {
    if (!index.has(v)) strongconnect(v);
  }

  // if any SCC has size > 1 (or self-loop), it's a cycle
  for (const scc of sccs) {
    if (scc.length > 1) return { tag: "Cycle", scc };
    const v = scc[0];
    if ((graph.get(v) ?? []).includes(v)) return { tag: "Cycle", scc };
  }

  // If no cycles, a topo order can be obtained by reverse postorder via SCC list; simplest:
  // Here sccs is already in reverse topo of SCC DAG in Tarjan; flatten reversed:
  const order = sccs.flat().reverse();
  return { tag: "Topo", order };
}
```

This gives you:

* deterministic compile/load order
* explicit cycle groups for error messages

---

# 191. Transformer Serialization: Why Closures Are Not Stored, and How Proc Macros Still Work

**Non-negotiable**: closures are not serialized into artifacts. Instead:

* artifacts store **phase init programs** `initByPhase[p]` (core Expr)
* procedural macro transformers are created by instantiating the defining module at the right phase and reading the transformer variable

That’s the same approach used by industrial macro systems (compile code → load code → instantiate to create transformer procedures).

## 191.1 Artifact extension: `initByPhase`

Update `ModuleArtifact`:

```ts
export type ModuleArtifact = {
  moduleName: string;
  moduleId: string;
  scopeM0: string;
  scopeM1: string;
  bindings: Binding[];

  initByPhase: Record<number, Expr>; // <-- NEW: phase -> init program
  exports0: ExportEntry0[];
  exports1: Array<{ name: string; bid: string; transformer: TransformerVal }>;

  deps: string[];
  sourceHash: string;
  expandedHash: string;
  coreHash: string;
  receipts: unknown[];
};
```

For `exports1`, a transformer is either:

* `SyntaxRules` data (serializable)
* `ProcRef` (serializable reference to a runtime variable holding the closure)

---

# 192. Compile-Time Evaluation: `begin-for-syntax` and Procedural `define-syntax` RHS

This is the heart of the tower.

## 192.1 Key rule

> Compilation is a staged interpreter run.
> Expanding a module may require instantiating modules at higher phases to obtain transformer values.

This is SICP’s “metacircular evaluator” philosophy applied to macro towers.

---

# 193. End‑to‑End Demonstration: A Procedural Macro That Calls `infer` at Compile Time

Here is the canonical “show me it’s not an API call” demo:

* Module `A` defines a procedural macro `auto-when` that:

  1. receives syntax
  2. converts to datum
  3. asks inference to propose a rewrite (here: it rewrites `(auto-when t e...)` into `(if t (begin e...) (unit))`)
  4. returns syntax built from the rewrite datum

* Module `B` imports it and uses it.

Even with inference, this remains a *language feature* because:

* it’s invoked via `effect infer.op`
* it can be handled, receipted, replayed, and gated by obligations

## 193.1 Module A (procedural macro)

```scheme
(module A
  (provide (for-syntax auto-when))
  (require-for-syntax Prelude)

  (define-syntax auto-when
    (lambda (stx)
      (let ((d (syntax->datum stx)))
        ;; request is just a datum record; your oracle can interpret it
        (let ((m (infer (quote (kind "rewrite-auto-when" input d)))))
          ;; suppose m returns a record containing a field 'rewrite
          ;; For the reference demo, you can mock infer to return the rewrite datum directly
          (datum->syntax stx m))))))
```

### What the oracle does (mocked deterministically)

* If request.kind == `"rewrite-auto-when"`, return datum:

  * `(if <t> (begin <e...>) (unit))`
* attach a receipt with:

  * requestHash
  * responseHash
  * transcriptHash (mock)
  * obligations (optional: unit tests)

This gives you a procedural macro whose semantics are:

* governed by inference receipts
* replayable under strict builds
* usable as an ordinary macro by clients

## 193.2 Module B (uses macro)

```scheme
(module B
  (require A Prelude)
  (provide main)

  (define (main)
    (auto-when #t (+ 1 2) (+ 3 4))))
```

Expands to:

```scheme
(define (main)
  (if #t (begin (+ 1 2) (+ 3 4)) (unit)))
```

No runtime inference. The only inference is in compilation, controlled by receipts.

---

# 194. Where This Goes Next (And Why This Is the “Take It to the Limit” Inflection Point)

At this point, Ω has three orthogonal semantic planes, each formal and first-class:

1. **Evaluation plane**: CEKS + algebraic effects (`effect`, `handle`, resumptions)
2. **Syntactic plane**: hygienic macros (syntax-rules + procedural) with phase tower
3. **Inference plane**: `infer` as an effect producing structured `Meaning`, governed by receipts and obligations

Now the remaining “limit” moves are:

* **Syntax-case / syntax-parse** as library layers on top of `SyntaxVal` (pattern compilers, literal sets, syntax classes)
* **Semantic macro receipts** + **macro stepper** as a tool (derives from stored receipts and expanded syntax)
* **Search plane**: inference-guided nondeterminism (oracle-scored frontier) + fair scheduling (already built)
* **Training plane**: `learn` / `reinforce` as effects producing *policy artifacts* (weights/prompt digests), never ad-hoc mutation

---

# 195. Continue

If you reply **continue**, Part 21 will do the final “tower closure” work:

* Implement a **minimal phase‑tower expander in Ω itself** (Ω₀ → Ω₁) using `SyntaxVal`
* Provide a **procedural macro toolkit** (`syntax-case` mini) and show it bootstrapping `syntax-rules`
* Provide a **strict build mode** that:

  * forbids oracle calls unless receipted
  * replays all inference deterministically
  * enforces obligations before promotion
* Provide the *actual* “Promote the Expander” command that swaps `defaultExpanderDigest` only after differential + obligation gates pass