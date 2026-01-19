## Ω Language Specification v1.0

### Part 7 — Completing the Core Runtime: Clean Value Injection, Real Expansion/Lowering, `handle`, `match`, `ctx/extend/seal`, `eval`, Obligations, and a Worked End‑to‑End “Rewrite→Verify→Commit”

This part closes the biggest engineering gaps from Part 6. The themes are:

* **Make the CEKS machine correct** (no “fake Lit node” resumption hacks)
* **Make the surface language executable** (`read → expand → eval`)
* **Make inference a semantic plane in practice** (`int/infer/rewrite` lower to `infer.op` with a structured payload)
* **Make governance real** (obligations + commit barrier enforced by a handler)
* **Make environments first-class** (`ctx/extend/seal` implemented with lexical scope + persistent frames)
* **Make language-making real** (`handle` implemented as a delimited handler; `match` introduces scoped bindings)

I’ll keep it TypeScript/Node, still reference-quality, still aligned with the formal spec.

---

# 56. Fixing the CEKS Machine: Control = Expr ∪ Val

The core bug in Part 6 is the lack of an explicit “value mode.” In any CEK/CEKS machine, the control component is not “always an expression.” It is:

[
C \in \text{Expr} ;\cup; \text{Val}
]

That gives you **correct resumption semantics** for effects (and later for delimited continuations).

## 56.1 Update: `Control` and `State`

```ts
// src/machine/state.ts
import type { Expr } from "../ast";
import type { Ctx } from "../context";
import type { Store } from "../store";
import type { Kont } from "./kont";
import type { HandlerStack } from "../effects/handler";
import type { Ledger } from "../ledger";
import type { Val } from "../values";

export type Control =
  | { tag: "Expr"; expr: Expr }
  | { tag: "Val"; v: Val };

export type State = {
  control: Control;
  ctx: Ctx;
  store: Store;
  kont: Kont;
  handlers: HandlerStack;
  ledger: Ledger;
};
```

Now an effect resumption can *literally* be:

```ts
resume: (v: Val) => State   // sets control := Val(v), then machine continues
```

No “fake lit” nodes. No ambiguity.

---

# 57. Update: `OpCall` Must Carry the Store (and a Proper Resume)

Effects are semantic—handlers must be able to run sub-evaluations with the same store and handler stack.

```ts
// src/effects/handler.ts
import type { Ctx } from "../context";
import type { Store } from "../store";
import type { State } from "../machine/state";
import type { Val } from "../values";

export type Resume = (v: Val) => State;

export type OpCall = {
  op: string;
  args: Val[];
  ctx: Ctx;
  store: Store;
  resume: Resume;    // resume with a value
};
```

This is the **Command** object for semantics: it has operation, args, continuation (resume), and the execution substrate (ctx/store).

---

# 58. Continuations: Add `RestoreCtxK` (Scoped Bindings for `match`, `let`, etc.)

Pattern matching must not leak bindings outside a clause. That is lexical scoping, enforced by restoring the pre-match context after evaluating the chosen body.

```ts
// src/machine/kont.ts
import type { Expr } from "../ast";
import type { Ctx } from "../context";
import type { Val } from "../values";

export type Kont =
  | { tag: "Halt" }
  | { tag: "IfK"; conseq: Expr; alt: Expr; ctx: Ctx; next: Kont }
  | { tag: "BeginK"; rest: Expr[]; ctx: Ctx; next: Kont }
  | { tag: "DefineK"; name: string; ctx: Ctx; next: Kont }
  | { tag: "SetK"; name: string; ctx: Ctx; next: Kont }
  | { tag: "ApplyFnK"; args: Expr[]; ctx: Ctx; next: Kont }
  | { tag: "ApplyArgsK"; fn: Val; done: Val[]; rest: Expr[]; ctx: Ctx; next: Kont }
  | { tag: "EffectArgsK"; op: string; done: Val[]; rest: Expr[]; ctx: Ctx; next: Kont }
  | { tag: "MatchK"; clauses: Array<{ pat: any; body: Expr }>; elseExpr?: Expr; ctx: Ctx; next: Kont }
  | { tag: "RestoreCtxK"; ctx: Ctx; next: Kont };  // NEW
```

`RestoreCtxK` is the machine-level expression of the lexical scoping invariant.

---

# 59. Step Function v2: Handle Expr-mode and Val-mode

Now `step` is clean:

* If `control = Expr(e)`, reduce it
* If `control = Val(v)`, apply continuation frames

This is exactly the CEK discipline.

I’ll show the important cases: `Lit`, `Sym`, `Quote`, `Apply`, `Effect`, `Match`, and continuation application.

```ts
// src/machine/step.ts (sketch-level; core cases)
import type { State, Control } from "./state";
import type { StepOutcome } from "./outcome"; // {State|Done|OpCall}
import { lookupAddr, defineInCtx, extendCtx, sealCtx } from "../context";
import { alloc, read, write } from "../store";
import type { Val } from "../values";
import { syntax, unit } from "../values";

// helper
function isTruthy(v: Val): boolean {
  return !(v.tag === "Bool" && v.value === false) && v.tag !== "Null";
}

export function step(s: State): StepOutcome {
  if (s.control.tag === "Val") return applyKont(s, s.control.v);

  const e = s.control.expr;
  switch (e.tag) {
    case "Lit":
      return { tag: "State", state: { ...s, control: { tag: "Val", v: e.value } } };

    case "Sym": {
      const addr = lookupAddr(s.ctx, e.name);
      const v = read(s.store, addr);
      return { tag: "State", state: { ...s, control: { tag: "Val", v } } };
    }

    case "Quote":
      return { tag: "State", state: { ...s, control: { tag: "Val", v: syntax(e.datum) } } };

    case "If":
      return {
        tag: "State",
        state: {
          ...s,
          control: { tag: "Expr", expr: e.test },
          kont: { tag: "IfK", conseq: e.conseq, alt: e.alt, ctx: s.ctx, next: s.kont },
        },
      };

    case "Begin": {
      if (e.exprs.length === 0) return { tag: "State", state: { ...s, control: { tag: "Val", v: unit() } } };
      const [first, ...rest] = e.exprs;
      return {
        tag: "State",
        state: {
          ...s,
          control: { tag: "Expr", expr: first },
          kont: { tag: "BeginK", rest, ctx: s.ctx, next: s.kont },
        },
      };
    }

    case "Define":
      return {
        tag: "State",
        state: {
          ...s,
          control: { tag: "Expr", expr: e.rhs },
          kont: { tag: "DefineK", name: e.name, ctx: s.ctx, next: s.kont },
        },
      };

    case "Set":
      return {
        tag: "State",
        state: {
          ...s,
          control: { tag: "Expr", expr: e.rhs },
          kont: { tag: "SetK", name: e.name, ctx: s.ctx, next: s.kont },
        },
      };

    case "Apply":
      return {
        tag: "State",
        state: {
          ...s,
          control: { tag: "Expr", expr: e.fn },
          kont: { tag: "ApplyFnK", args: e.args, ctx: s.ctx, next: s.kont },
        },
      };

    case "Effect": {
      // evaluate args left-to-right, then emit OpCall
      if (e.args.length === 0) {
        return {
          tag: "OpCall",
          call: {
            op: e.op,
            args: [],
            ctx: s.ctx,
            store: s.store,
            resume: (v) => ({ ...s, control: { tag: "Val", v }, kont: s.kont }),
          },
        };
      }
      const [first, ...rest] = e.args;
      return {
        tag: "State",
        state: {
          ...s,
          control: { tag: "Expr", expr: first },
          kont: { tag: "EffectArgsK", op: e.op, done: [], rest, ctx: s.ctx, next: s.kont },
        },
      };
    }

    case "Match": {
      // evaluate scrutinee; store clauses in continuation
      return {
        tag: "State",
        state: {
          ...s,
          control: { tag: "Expr", expr: e.scrut },
          kont: { tag: "MatchK", clauses: e.clauses as any, elseExpr: e.elseExpr, ctx: s.ctx, next: s.kont },
        },
      };
    }

    default:
      throw new Error(`Unhandled Expr in step: ${e.tag}`);
  }
}

function applyKont(s: State, v: Val): StepOutcome {
  const k = s.kont;

  switch (k.tag) {
    case "Halt":
      return { tag: "Done", value: v, state: s };

    case "IfK": {
      const nextExpr = isTruthy(v) ? k.conseq : k.alt;
      return { tag: "State", state: { ...s, ctx: k.ctx, control: { tag: "Expr", expr: nextExpr }, kont: k.next } };
    }

    case "BeginK": {
      if (k.rest.length === 0) {
        return { tag: "State", state: { ...s, ctx: k.ctx, control: { tag: "Val", v }, kont: k.next } };
      }
      const [first, ...rest] = k.rest;
      return {
        tag: "State",
        state: { ...s, ctx: k.ctx, control: { tag: "Expr", expr: first }, kont: { tag: "BeginK", rest, ctx: k.ctx, next: k.next } },
      };
    }

    case "DefineK": {
      let store = s.store;
      const [store2, addr] = alloc(store, v);
      store = store2;
      const newCtx = defineInCtx(k.ctx, /*cid*/ k.ctx.cid, k.name, addr);
      return { tag: "State", state: { ...s, ctx: newCtx, store, control: { tag: "Val", v: unit() }, kont: k.next } };
    }

    case "SetK": {
      const addr = lookupAddr(k.ctx, k.name);
      const store2 = write(s.store, addr, v);
      return { tag: "State", state: { ...s, store: store2, control: { tag: "Val", v: unit() }, kont: k.next } };
    }

    // ApplyFnK / ApplyArgsK / EffectArgsK / MatchK / RestoreCtxK cases are analogous:
    // - evaluate args
    // - apply closure or native fn
    // - emit OpCall when effect args done
    // - in MatchK: select clause; evaluate body under extended ctx; push RestoreCtxK(originalCtx)
    // (I’ll implement match + native fns below.)
    default:
      throw new Error(`Unhandled Kont: ${(k as any).tag}`);
  }
}
```

The important point: **resumption is now correct**. Any handler can return a value; the machine just continues.

This is the key enabler for:

* algebraic effects,
* oracle sessions,
* and later `amb`/backtracking.

---

# 60. Native Functions (Built-ins) and the Prelude Environment

You can’t test anything without basic primitives. Add:

* `NativeFn` values (host-implemented procedures)
* a boot procedure to install them into the root context

## 60.1 Add `NativeFnVal`

```ts
// src/values.ts (add)
export type NativeFnVal = {
  tag: "NativeFn";
  name: string;
  arity?: number;
  fn: (args: Val[]) => Promise<Val> | Val;
};

// include in Val union
export type Val = /* ... */ | NativeFnVal;
```

## 60.2 Update apply semantics (closure vs native)

In `applyKont` when args complete, if function value is:

* `Closure`: lexical apply (extend env, allocate params)
* `NativeFn`: call host fn

## 60.3 Implement critical built-ins

The two that matter immediately for inference lowering:

* `record` — construct a `MapVal` from alternating string keys and values
* `get` — retrieve from `MapVal`

Plus basic arithmetic and structural equality for obligations.

```ts
// src/prelude/builtins.ts (sketch)
import type { Val, MapVal } from "../values";

export function nativeRecord(args: Val[]): MapVal {
  if (args.length % 2 !== 0) throw new Error("record expects even number of args");
  const entries: Array<[Val, Val]> = [];
  for (let i = 0; i < args.length; i += 2) entries.push([args[i], args[i + 1]]);
  return { tag: "Map", entries };
}

export function nativeGet(args: Val[]): Val {
  const [m, k] = args;
  if (!m || m.tag !== "Map") throw new Error("get expects Map");
  for (const [kk, vv] of m.entries) {
    if (deepEqual(kk, k)) return vv;
  }
  return { tag: "Null" } as any;
}

export function deepEqual(a: Val, b: Val): boolean {
  // implement structural equality for Atom + Syntax + Pair/Vec/Map (minimal)
  // crucial for eq-ext obligations
  return JSON.stringify(a) === JSON.stringify(b);
}
```

---

# 61. Expansion/Lowering: `read → expand → eval`

This is the point where Ω becomes a language, not a data structure library.

## 61.1 Reader: parse s-expressions into a raw tree, then into `Expr`

You can implement a classic two-step reader:

1. tokenize: parentheses, strings, symbols, numbers
2. parse: produce `SExp = AtomTok | SymbolTok | List<SExp>`
3. lower `SExp` into `Expr` by recognizing special forms

I’ll show the lowering rules that matter for this part.

### 61.1.1 Special form recognition (partial)

* `(quote x)` → `Expr.Quote(xExpr)`
* `(lambda (x y) body)` → `Expr.Lambda(["x","y"], bodyExpr)`
* `(if a b c)` → `Expr.If(a,b,c)`
* `(begin e...)` → `Expr.Begin([...])`
* `(define name rhs)` or `(define (f x) body)` desugaring to lambda
* `(set! name rhs)`
* `(effect op args...)` → `Expr.Effect(opString, argsExprs)`
* `(handle body (on op (x k) clauseBody) ... (return (r) retBody) (finally finBody))` → `Expr.Handle(...)`
* `(match scrut (pat body) ... (else body))` → `Expr.Match(...)`
* **NEW** surface forms:

  * `(int qexpr :engine E :policy P :env ρ)` → `Expr.Int(...)`
  * `(infer goal :engine E :policy P :env ρ)` → `Expr.Infer(...)`
  * `(rewrite qexpr :goal G :engine E :policy P :env ρ)` → `Expr.Rewrite(...)`

Then **the expander** lowers these three into `infer.op` effects.

---

## 61.2 Expander: Lower `Int/Infer/Rewrite` to `(effect "infer.op" (record ...))`

This is the critical “not an API call” move.

### 61.2.1 Lowering rule (normative)

**Surface:**

```lisp
(int qexpr :engine E :policy P :env ρ)
```

**Expanded core:**

```lisp
(effect "infer.op"
  (record
    "kind"   "int"
    "qexpr"  qexpr
    "engine" E
    "policy" P
    "env"    ρ))
```

Similarly:

```lisp
(infer goal :engine E :policy P :env ρ)
=> (effect "infer.op" (record "kind" "search" "goal" goal ...))
```

```lisp
(rewrite qexpr :goal G :engine E :policy P :env ρ)
=> (effect "infer.op" (record "kind" "rewrite" "qexpr" qexpr "goal" G ...))
```

### 61.2.2 Why this is semantically decisive

* `record` runs **extentionally** and produces a **typed payload**
* the payload contains actual *values* for engine/policy/env, not syntax
* inference is initiated as an **effect** handled by the oracle handler
* therefore inference is a true part of evaluation semantics (like `amb` in SICP)

---

# 62. `handle` Semantics: Delimited Algebraic Effects (Real, Not Global Hooks)

Part 6 used global `onReturn` unwinding. That’s wrong for delimited handlers.

## 62.1 Runtime special-case for `Handle`

In `Runtime.runState`, before stepping, detect:

* `control = Expr(Handle(body, handlerExpr))`

Semantics:

1. Compile `handlerExpr` into a `Handler` object capturing the **lexical** context
2. Run a **sub-evaluation** of `body` with handler stack extended by that handler (push)
3. Apply handler’s `onReturn` to the body result (if present)
4. Execute `onFinally` (if present)
5. Resume outer evaluation with the resulting value

This gives you the standard algebraic effect semantics: **delimited**, compositional, stack-disciplined.

### 62.2 Handler compilation: clauses become closures over the current context

Each clause `(on op (x k) body)` compiles to:

* a matcher on `op`
* a body evaluator running in an env that binds:

  * `x` to the operation argument value(s)
  * `k` to the resumption function (first-class)

`k` should be a `NativeFn` capturing `call.resume`.

That’s **Command + Mediator**: the handler mediates access to the continuation.

---

# 63. `match` Semantics: Scoped Bindings with `RestoreCtxK`

Implement structural pattern matching without evaluation of patterns.

### 63.1 Pattern compilation (minimal viable)

Support:

* `_` wildcard
* `Sym` binders
* `Lit` literals
* `Quote` patterns matching `Syntax` values (code-pattern matching)

A pattern matcher returns either:

* `null` (no match)
* or `bindings: Array<[name, Val]>`

### 63.2 Evaluator rule

When `MatchK` receives the scrutinee value `v`:

1. For each clause:

   * attempt match
   * if succeeds, allocate store locations for bindings and extend ctx
   * evaluate clause body in extended ctx
   * push `RestoreCtxK(originalCtx)` so bindings don’t leak
2. Else evaluate `else` if present, otherwise error

This is literally the environment model discipline from SICP, applied to pattern matching.

---

# 64. First-class Contexts: `ctx`, `extend`, `seal`

To support “REPL against an environment passed to inference” and “context economics,” contexts must be values.

## 64.1 Minimal semantics

* `ctx` creates a new context object (default parent = current ctx unless specified)
* `extend` creates a child context with bindings
* `seal` flips `sealed=true` and (optionally) strips write capabilities

These are easiest as special forms in the evaluator. They don’t require short-circuiting, but they do require access to the store allocator.

---

# 65. `eval`: Reflective Extensional Evaluation of `Syntax`

Because quote produces `Syntax`, `eval` must accept `Syntax` and run its contained `Expr`.

A correct `eval` in Ω must also expand the contained expression before running it, otherwise macros/lowerings don’t apply.

So evaluator implementation needs access to:

* the expander `expand(expr, macroEnv)`
* and a `runSub(expandedExpr, envCtx, store, handlerStack)`

This is the reflective bridge between representation and execution—SICP’s “magic”—but now consistent with the dual-plane architecture.

---

# 66. Minimal Obligations and the Commit Barrier (Implemented, Not Just Spec’d)

This is where Ω becomes safe.

## 66.1 Obligation schema (minimal)

Represent obligations as `Map` values with a `"tag"` field and payload fields.

Examples:

### Test suite obligation

```lisp
(record "tag" "tests" "suite" suiteVal)
```

### Extensional equivalence obligation (sampled)

```lisp
(record "tag" "eq-ext"
        "orig" origSyntax
        "cand" candSyntax
        "samples" samplesVec)
```

## 66.2 Commit effect payload

`commit/rewrite` is just:

```lisp
(effect "commit.op"
  (record "kind" "rewrite"
          "candidate" candSyntax
          "obligation" obl))
```

The **CommitHandler** must:

1. Decode obligation
2. Run verifier:

   * for `tests`: evaluate each test expr; require `#t`
   * for `eq-ext`: for each sample, extend env with bindings if needed, eval both, compare with `equal?`
3. If passes, accept commit (update artifact registry / write receipt / emit ledger events)
4. If fails, throw (blocking promotion)

That is the operational enforcement of the “truth regime” concept.

This is **Unit of Work**: commit is the boundary where meaning becomes reality.

---

# 67. Receipts (Skeleton): Content-addressed snapshots of semantic work

For Part 7, implement minimal receipt store:

* `snapshot` effect stores a structured receipt:

  * env digest
  * event range IDs
  * key evidence hashes
  * cost profile

* `compress` replaces heavy evidence in ctx with receipt refs

Mechanically: an in-memory map keyed by hash is enough for the reference interpreter.

The important thing is the **shape**: receipts are first-class and replayable.

---

# 68. End-to-end Worked Example: Rewrite → Verify → Commit → Use

Below is a minimal Ω program that demonstrates the complete spine (with mock inference):

1. Ask the inference plane for a rewrite candidate for a quoted program
2. Verify equivalence by evaluating both (extensional checks)
3. Commit the rewrite only if checks pass
4. Evaluate the committed candidate

```lisp
(begin
  ;; quoted program (Syntax)
  (define q '(+ 1 2))

  ;; ask inference plane to propose rewrite (handled by infer.op → OracleHandler)
  (define m (rewrite q
              :goal (record "preserve" "extensional" "optimize" "constant-fold")
              :engine engine:mock
              :policy policy:pragmatic))

  ;; extract candidate syntax from Meaning
  (define cand (get m "rewrite"))   ;; assuming Meaning is Map-like or you provide meaning.rewrite

  ;; obligation: extensional equivalence on a trivial sample (or tests)
  (define obl (record "tag" "eq-ext" "orig" q "cand" cand "samples" (vector)))  ;; constant expr, no samples needed

  ;; commit barrier enforces obl via CommitHandler
  (effect "commit.op" (record "kind" "rewrite" "candidate" cand "obligation" obl))

  ;; use it
  (eval cand))
```

**What this proves:**

* rewrite does not silently change code
* the inference plane can propose candidates
* extensional evaluation certifies before commit
* commit is mediated by a handler (Strategy-controlled truth regime)
* the end result is executable in the same runtime

This is the smallest “Ω moment” that isn’t hand-waving.

---

# 69. What’s next (Part 8)

At this stage, Ω is a functioning kernel+runtime with inference integration and a governance spine.

The next irreducible pieces are:

1. **Macro system (hygiene + phase separation)** so “make up languages” is real at scale
2. **Standard library implementations in Ω itself** (streams, amb, generic ops, constraints)
3. **Inference strategy DSL as values** (pipeline combinators that configure the oracle handler)
4. **Promotion pipeline automation** (CI-style: regression, fuzz, cost checks, semantic versioning)
5. **Policy learning loop** (episodes/reward → new Policy version → promotion) integrated end-to-end

If you want the next part, reply **continue**.
