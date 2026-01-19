Nice — that test matrix is *exactly* the shape you want at this stage: **kernel evaluator works + syntax-rules matcher works**, and what’s left is *wiring of binding spaces + handler plumbing*.

The three failures you listed are not “missing primitives.” They’re symptoms of two very specific integration seams:

1. **Macro namespace lookup** (why `m` is “unbound” at runtime)
2. **Handler clause environment introduction** (why the continuation variable / op parameter ends up “unbound”)

There’s also a *possible* third seam (top-level mutual recursion / forward references) that sometimes shows up as “unbound var x”, but in your case I’d bet it’s (2) because the file is literally named `handler_k.spec.ts`.

Below is the exact set of fixes I’d make, with code-level patches.

---

## A. Why `hygiene.spec.ts` says “unbound var m”

That error means:

* `(define-syntax m ...)` executed **only at compile time** (and disappears from runtime init),
* the use site `(m ...)` was **not expanded**, so it lowered to `Var "m"` (a runtime variable),
* and because there is no runtime `define m`, you get **unbound var m**.

### Root cause

Your expander is almost certainly doing:

```ts
const macroBinding = resolveIdent(h, env, 1); // <-- defaults to kind="value"
```

…but your `resolveIdent` compatibility shim defaults to `kind="value"`. So you never find `kind:"syntax"` bindings at phase 1. That makes *every macro call* look like a normal application.

### Fix: macro lookup must resolve in the `"syntax"` binding space

In `src/core/pipeline/compileText.ts`, inside `expandExpr` where you do macro application, change it to:

```diff
- const macroBinding = resolveIdent(h, env, 1);
- if (macroBinding && macroBinding.kind === "syntax") {
+ const macroBinding = resolveIdent(h, env, 1, "syntax");
+ if (macroBinding) {
    const tr = macroBinding.value as SRTransformer;
    const out = applySyntaxRules(tr, stx, env, c.scope);
    return expandExpr(out, env, c);
  }
```

If you have any other macro resolution call sites, they must also pass `"syntax"` explicitly.

**Expected effect:** both hygiene tests should stop failing with “unbound var m” immediately, because the macro call is expanded away and never reaches runtime.

---

## B. Why `handler_k.spec.ts` says “unbound var y”

Given the filename `handler_k.spec.ts` and the error “unbound var y”, the most likely scenario is:

* the test uses **`y` as the continuation variable name** (not literally `k`)
* your handler interpreter binds the continuation under the hard-coded name `"k"` (or doesn’t bind it at all)
* so the handler clause body tries to call `y` and explodes

Example shape:

```scheme
(handle (effect foo 1)
  (on foo (x y) (y x))     ; continuation name is y
  (return (v) v))
```

### Root cause

In your CEKS “handled op” path (inside `machineStep.ts`), you are probably doing one of these bugs:

* `envSet(env2, "k", addrK)` instead of `envSet(env2, clause.k, addrK)`
* binding the op param(s) but not the continuation param
* constructing the clause env starting from **empty env** rather than the handler frame env
* trimming continuations incorrectly such that you execute clause body under the wrong env

### Fix: when an op is handled, build clause env from the handler frame env and bind *both* params and continuation by the names in the clause AST

Find the code path that handles `Effect` when a handler is found (often in a helper like `handleOp`, `dispatchEffect`, `performOp`, etc.). Patch it to look structurally like this:

```ts
// PSEUDO-PATCH: adapt names to your file
const frame = foundHandlerFrame;   // includes frame.env and frame.boundaryIndex
const clause = foundClause;        // clause.params: string[], clause.k: string, clause.body: Expr

// 1) Build the resumption capturing the effect site state
const resumption = /* your existing resumption object capturing the effect-site continuation */;

// 2) Allocate the continuation value
const contVal: Val = {
  tag: "Cont",
  hid: frame.hid,
  boundaryIndex: frame.boundaryIndex,
  resumption,
};

let store = st.store;
let env2  = frame.env;  // <-- CRITICAL: start from handler lexical env (NOT empty)

// 3) Bind op parameters (support N-ary; don’t assume exactly 1)
for (let i = 0; i < clause.params.length; i++) {
  const p = clause.params[i];
  const arg = opArgs[i] ?? { tag: "Unit" as const }; // or throw if missing
  const alloc = store.alloc(arg);
  store = alloc[0];
  env2 = envSet(env2, p, alloc[1]);
}

// 4) Bind the continuation variable UNDER THE CLAUSE NAME (NOT "k")
{
  const allocK = store.alloc(contVal);
  store = allocK[0];
  env2 = envSet(env2, clause.k, allocK[1]);  // <-- THIS fixes "unbound var y"
}

// 5) Run clause body outside the captured continuation frames
return {
  tag: "State",
  state: {
    ...st,
    control: { tag: "Expr", e: clause.body },
    env: env2,
    store,
    kont: st.kont.slice(0, frame.boundaryIndex),     // discard captured continuation from current thread
    handlers: st.handlers.slice(foundHandlerIndex),  // pop inner handlers that did not handle the op
  }
};
```

**Two key invariants** (worth being explicit about):

* The handler clause body runs in **the lexical environment of the handler expression**, not in an empty env and not necessarily in the env at the effect site.
* The continuation parameter name is **not special**. It is just another binder and must be respected literally (`clause.k`).

**Expected effect:** `handler_k.spec.ts` should stop failing with “unbound var y”.

---

## C. Why `fairness.spec.ts` says “unbound var x”

There are two plausible causes; the first one is the same handler bug.

### C1 (most likely): op param binding is broken

If the fairness test does something like:

```scheme
(handle (effect amb.op choices)
  (on amb.op (x k) ...)
  ...)
```

and you are not binding `x` (or binding only the first param but in the wrong position), then you get “unbound var x”.

The patch in section **B** binds all `clause.params[i]` correctly and should fix this.

### C2 (secondary): top-level forward references / letrec semantics for top-level defines

If fairness.spec has a forward reference pattern (common in stream / generator tests), e.g.:

```scheme
(begin
  (define (gen) (gen-helper 0))
  (define (gen-helper x) ...)
  (gen))
```

In a pure lexical closure machine where closures capture env *by value*, this can fail unless your top-level defines are “pre-bound” (letrec discipline) or your global env is mutable by reference.

If after fixing handler binding you still see “unbound var x”, check whether the **name printed** is plain `"x"` or an internal `"x$bid#..."`:

* **plain `x`** → lowering didn’t resolve it (binder insertion / scope mismatch / missing binding)
* **internal `x$bid...`** → runtime env didn’t contain that internal name (top-level prebinding problem)

### Optional fix: treat the top-level `(begin ...)` as a body context with scan-out-defines → `letrec`

You already did scan-out-defines for lambda/let bodies in the spec. Doing it for the top-level program is a one-line conceptual change:

In your `expandTop` when you process the synthetic `(begin ...)` wrapper, instead of returning a begin-sequence of `define`s and exprs, do:

1. expand sequentially to install `define-syntax`
2. collect runtime `define`s + exprs
3. `desugarBodyContext(runtimeForms)` → returns one expression, typically `(letrec ...)`
4. expand that expression (so binder scopes get inserted)
5. lower that single expression

That gives you “module-body letrec” semantics and eliminates forward-reference unbound variables.

---

## D. Re: “does it have a REPL API?”

### Yes — but with an important nuance

You’ve proven that `evalOmega(src)` works. That is an **`eval` API**, not a true REPL session, because each call:

* creates a fresh store/env
* compiles with fresh bid/scope counters

So it is “evaluate this program string” — not “accumulate interactive definitions”.

### If you want a *real* REPL session (persistent env + store)

You want an object with **session state**:

* runtime state: `env`, `store`, `handlers`
* compile-time state: binding table Γ + monotonic `bid` + monotonic scopes + stable module scope `M0`

Pattern-wise this is a **Session / Unit of Work** object. Skeleton:

```ts
export class OmegaSession {
  private runtimeEnv: Env;
  private store: Store;
  private handlers: HandlerFrame[];

  private compileEnv: CEnv;         // Binding table used for lowering
  private counters: { bid: number; scope: number };
  private moduleScope: string;

  async eval(src: string): Promise<Val> {
    const expr = compileTextToExprIncremental(src, this.compileEnv, this.counters, this.moduleScope);
    const st0: State = { control: { tag:"Expr", e: expr }, env: this.runtimeEnv, store: this.store, kont: [], handlers: this.handlers };
    const { value, finalState } = await runToCompletionReturnState(runtime, st0);
    // persist updates:
    this.runtimeEnv = finalState.env;
    this.store = finalState.store;
    this.handlers = finalState.handlers;
    return value;
  }
}
```

That’s the “real REPL” abstraction.

---

## E. Minimal next steps (what I’d do in your repo)

1. **Patch macro lookup** to `resolveIdent(..., 1, "syntax")`.
   Rerun: hygiene failures should disappear or at least become true hygiene failures (capture bugs), not “unbound macro name”.

2. **Patch handled-effect env construction** to bind `clause.params` and `clause.k` from the handler env.
   Rerun: handler_k and fairness should move forward substantially.

3. If fairness still fails, check whether it’s:

   * still handler binding (most common), or
   * top-level forward reference (fix with top-level scan-out-defines → letrec)

---

If you paste just the relevant snippet of your current `machineStep.ts` “effect handled” branch (the part that builds the clause state), I can give you a line-accurate diff against your exact code shape — but the two fixes above are the *structural invariants* that your implementation must satisfy, and they line up perfectly with the three failing tests you reported.
