## Ω Language Specification v1.0

### Part 6 — A Concrete Reference Implementation (TypeScript/Node): CEKS Machine + Algebraic Effects + Oracle Sessions + Receipts + Promotion

I’m going to pick **TypeScript/Node** for the first “reference implementation skeleton” because it naturally supports:

* **discriminated unions** (for AST/values/continuations),
* **async generators** (for the interactive oracle protocol),
* and a clean separation between **pure core** and **impure adapters** (Hexagonal/Ports‑and‑Adapters).

You can later harden this into **Rust** (for sandboxing + perf) without changing the semantics; TypeScript is the fastest way to validate the design.

What you’ll get here is a **minimal but complete spine**:

* Reader (s-expr → AST)
* Expander hook (macro phase stub)
* Extensional evaluator implemented as a **CEKS** abstract machine (Env + Kont + Store)
* Algebraic effects runtime (`effect` + `handle`)
* Oracle handler implementing the **interactive** Req/Resp protocol (REPL re-entrancy)
* Receipt store + compression placeholders
* Commit/promote/training emission plumbing

This is the point where Ω stops being a thought experiment and becomes an executable system.

---

# 43. Repository layout

A sane monorepo structure:

```
omega/
  package.json
  tsconfig.json
  src/
    ast.ts
    reader.ts
    values.ts
    context.ts
    store.ts
    hash.ts
    ledger.ts

    machine/
      kont.ts
      state.ts
      step.ts
      runtime.ts

    effects/
      handler.ts
      builtins.ts

    oracle/
      protocol.ts
      engine.ts
      oracleHandler.ts
      mockEngines.ts

    receipts/
      receipt.ts
      receiptStore.ts
      compress.ts
      hydrate.ts

    artifacts/
      registry.ts
      promote.ts
      semver.ts

    training/
      example.ts
      datasetStore.ts

    stdlib/
      prelude.omega   (optional: Ω source)
      stream.omega
      nondet.omega
      match.omega

    demo.ts
```

Design patterns embodied structurally:

* `machine/*` = **Interpreter** (GoF) realized as an abstract machine
* `effects/*` = **Chain of Responsibility** (handler stack)
* `oracle/*` = **Mediator** + **Strategy** + **Command** (requests are command-like)
* `receipts/*` = **Memento** + **Event Sourcing** (Σ ledger is source of truth)
* `artifacts/*` = CI/CD “semantic promotion pipeline” (Build Pipeline / Stage Gate)

---

# 44. Core Types

## 44.1 `src/ast.ts` — AST (homoiconic surface, explicit nodes)

```ts
// src/ast.ts
export type Loc = { line: number; col: number; file?: string };

export type Atom =
  | { tag: "Num"; value: number }
  | { tag: "Str"; value: string }
  | { tag: "Bool"; value: boolean }
  | { tag: "Null" };

export type Ann = {
  effects?: string[];     // effect row (best-effort for now)
  caps?: string[];        // required capabilities
  type?: string;          // optional type annotation (gradual)
  contract?: unknown;     // optional contract AST
};

export type MatchClause = {
  pat: Expr;   // pattern as Expr (we'll interpret structurally)
  body: Expr;
};

export type Expr =
  | { tag: "Lit"; value: Atom; loc?: Loc }
  | { tag: "Sym"; name: string; loc?: Loc }
  | { tag: "Quote"; datum: Expr; loc?: Loc }
  | { tag: "Lambda"; params: string[]; body: Expr; ann?: Ann; loc?: Loc }
  | { tag: "If"; test: Expr; conseq: Expr; alt: Expr; loc?: Loc }
  | { tag: "Begin"; exprs: Expr[]; loc?: Loc }
  | { tag: "Define"; name: string; rhs: Expr; loc?: Loc }
  | { tag: "Set"; name: string; rhs: Expr; loc?: Loc }
  | { tag: "Apply"; fn: Expr; args: Expr[]; loc?: Loc }
  | { tag: "Effect"; op: string; args: Expr[]; loc?: Loc }
  | { tag: "Handle"; body: Expr; handler: HandlerExpr; loc?: Loc }
  | { tag: "Eval"; qexpr: Expr; env?: Expr; loc?: Loc }
  | { tag: "Int"; qexpr: Expr; engine?: Expr; policy?: Expr; env?: Expr; loc?: Loc }
  | { tag: "Infer"; goal: Expr; engine?: Expr; policy?: Expr; env?: Expr; loc?: Loc }
  | { tag: "Rewrite"; qexpr: Expr; goal: Expr; engine?: Expr; policy?: Expr; env?: Expr; loc?: Loc }
  | { tag: "Match"; scrut: Expr; clauses: MatchClause[]; elseExpr?: Expr; loc?: Loc }
  | { tag: "Ctx"; fields: [string, Expr][]; loc?: Loc }
  | { tag: "Extend"; base: Expr; binds: [string, Expr][]; loc?: Loc }
  | { tag: "Seal"; base: Expr; loc?: Loc };

export type HandlerExpr = {
  // minimal handler syntax: (handler (on op (x k) body) ... (return (r) body)?)
  ons: Array<{ op: string; params: string[]; k: string; body: Expr }>;
  onReturn?: { r: string; body: Expr };
  onFinally?: Expr;
};
```

This explicit AST is what makes:

* tracing,
* obligation attachment,
* and meaning-level rewrites
  **real** instead of “string prompts.”

---

## 44.2 `src/values.ts` — Values (including Syntax and Meaning)

Quoted code must be a value. In a homoiconic language, you can model syntax as values directly.

```ts
// src/values.ts
import type { Atom, Expr } from "./ast";
import type { Ctx } from "./context";
import type { Store } from "./store";

export type Hash = string;

export type SyntaxVal = { tag: "Syntax"; expr: Expr };

export type PairVal = { tag: "Pair"; car: Val; cdr: Val };
export type VectorVal = { tag: "Vec"; items: Val[] };
export type MapVal = { tag: "Map"; entries: Array<[Val, Val]> };

export type ClosureVal = {
  tag: "Closure";
  params: string[];
  body: Expr;
  env: Ctx;
  ann?: { effects?: string[]; caps?: string[]; type?: string; contract?: unknown };
};

export type DistVal = {
  tag: "Dist";
  // minimal: lazy sampler + optional finite support
  sample: (seed: number) => Val;
  support?: Val[];
  meta?: Record<string, unknown>;
};

export type MeaningVal = {
  tag: "Meaning";
  denotation?: Val | DistVal;
  residual?: SyntaxVal;
  rewrite?: SyntaxVal;
  invariants?: Val;    // can be a MapVal / SyntaxVal / any
  effects?: Val;
  cost?: Val;
  paths?: Val;
  deps?: Val;
  memo?: Val;
  evidence?: Val;
  obligation?: Val;
  confidence?: number; // [0,1]
  trace?: Val;
};

export type CapVal = { tag: "Cap"; name: string };

export type ReceiptVal = {
  tag: "Receipt";
  rid: Hash;
  summary: string;
  deps: Hash[];
  replay: Val;  // replay plan DSL
  checks: Val;  // list of check results
  cost: Val;
  voi: Val;
};

export type EvidenceVal = { tag: "Evidence"; eid: Hash; payload: Val };

export type EngineVal = { tag: "Engine"; digest: Hash; version: string; payload: Val };
export type PromptVal = { tag: "Prompt"; digest: Hash; payload: Val };
export type PolicyVal = { tag: "Policy"; digest: Hash; payload: Val };

export type Val =
  | Atom
  | SyntaxVal
  | PairVal
  | VectorVal
  | MapVal
  | ClosureVal
  | DistVal
  | MeaningVal
  | ReceiptVal
  | EvidenceVal
  | EngineVal
  | PromptVal
  | PolicyVal
  | CapVal
  | { tag: "Ctx"; ctx: Ctx }
  | { tag: "Unit" };

export function unit(): Val {
  return { tag: "Unit" };
}

export function syntax(expr: Expr): SyntaxVal {
  return { tag: "Syntax", expr };
}
```

This is the first hard line against “API call”: the intensional plane does not “return text”; it returns a structured `MeaningVal`.

---

# 45. Environment + Store (CEKS): lexical scoping + state without collapsing persistence

Instead of mutating environments, we map variables → locations, then keep a store mapping locations → values. This is the standard CESK discipline, and it makes:

* `define` and `lambda` purely lexical,
* `set!` a controlled store update,
* “sealed contexts” meaningful.

## 45.1 `src/store.ts`

```ts
// src/store.ts
import type { Val } from "./values";

export type LocAddr = number;

export type Store = {
  next: LocAddr;
  cells: Map<LocAddr, Val>;
};

export function mkStore(): Store {
  return { next: 0, cells: new Map() };
}

export function alloc(store: Store, v: Val): [Store, LocAddr] {
  const a = store.next;
  store.cells.set(a, v);
  store.next = a + 1;
  return [store, a];
}

export function read(store: Store, a: LocAddr): Val {
  const v = store.cells.get(a);
  if (v === undefined) throw new Error(`Uninitialized location ${a}`);
  return v;
}

export function write(store: Store, a: LocAddr, v: Val): Store {
  if (!store.cells.has(a)) throw new Error(`Unknown location ${a}`);
  store.cells.set(a, v);
  return store;
}
```

Yes, the store is mutable here for speed. If you want purely persistent semantics, implement `cells` as a HAMT and return new stores (it’s mechanical).

## 45.2 `src/context.ts`

```ts
// src/context.ts
import type { Hash } from "./values";
import type { LocAddr } from "./store";

export type Inv = { name: string; args?: unknown };
export type CapSet = Set<string>;

export type Ctx = {
  cid: Hash;
  id: string;
  parent?: Ctx;
  frame: Map<string, LocAddr>;
  constraints: Inv[];
  caps: CapSet;
  sealed: boolean;
  evidence: Hash[];
  receipt?: Hash;
};

export function mkRootCtx(cid: Hash = "root"): Ctx {
  return {
    cid,
    id: "root",
    frame: new Map(),
    constraints: [],
    caps: new Set(),
    sealed: false,
    evidence: [],
  };
}

export function lookupAddr(ctx: Ctx, name: string): LocAddr {
  for (let cur: Ctx | undefined = ctx; cur; cur = cur.parent) {
    const a = cur.frame.get(name);
    if (a !== undefined) return a;
  }
  throw new Error(`Unbound variable ${name}`);
}

export function hasCap(ctx: Ctx, cap: string): boolean {
  return ctx.caps.has(cap);
}

export function requireCap(ctx: Ctx, cap: string): void {
  if (!hasCap(ctx, cap)) throw new Error(`Capability denied: ${cap}`);
}

export function extendCtx(
  ctx: Ctx,
  cid: Hash,
  id: string,
  binds: Array<[string, LocAddr]>,
  opts?: Partial<Pick<Ctx, "constraints" | "caps" | "sealed">>
): Ctx {
  const frame = new Map<string, LocAddr>();
  for (const [k, a] of binds) frame.set(k, a);
  return {
    cid,
    id,
    parent: ctx,
    frame,
    constraints: opts?.constraints ?? ctx.constraints,
    caps: opts?.caps ?? new Set(ctx.caps),
    sealed: opts?.sealed ?? false,
    evidence: [],
  };
}

export function defineInCtx(ctx: Ctx, cid: Hash, name: string, addr: LocAddr): Ctx {
  // Define in *current* frame: persistent by copying the frame.
  const frame = new Map(ctx.frame);
  frame.set(name, addr);
  return { ...ctx, cid, frame };
}

export function sealCtx(ctx: Ctx, cid: Hash): Ctx {
  return { ...ctx, cid, sealed: true };
}
```

---

# 46. The Abstract Machine: Continuations + State

## 46.1 `src/machine/kont.ts`

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
  | { tag: "EvalEnvK"; qexpr: Expr; next: Kont }
  | { tag: "EvalQExprK"; envVal: Val; next: Kont }
  | { tag: "IntEnvK"; kind: "int" | "search" | "rewrite"; payload: any; next: Kont } // kind + payload being built
  | { tag: "MatchK"; clauses: any; elseExpr?: Expr; ctx: Ctx; next: Kont };
```

## 46.2 `src/machine/state.ts`

```ts
// src/machine/state.ts
import type { Expr } from "../ast";
import type { Ctx } from "../context";
import type { Store } from "../store";
import type { Kont } from "./kont";
import type { HandlerStack } from "../effects/handler";
import type { Ledger } from "../ledger";

export type State = {
  expr: Expr;
  ctx: Ctx;
  store: Store;
  kont: Kont;
  handlers: HandlerStack;
  ledger: Ledger;
};
```

---

# 47. Effect Runtime (Handlers): Chain of Responsibility for semantics

## 47.1 `src/effects/handler.ts`

```ts
// src/effects/handler.ts
import type { Ctx } from "../context";
import type { State } from "../machine/state";
import type { Val } from "../values";

export type Resume = (v: Val) => State;

export type OpCall = {
  op: string;
  args: Val[];
  ctx: Ctx;
  resume: Resume;
};

export type HandlerResult =
  | { tag: "Handled"; value: Val }
  | { tag: "Reemit"; call: OpCall };

export interface Handler {
  id: string;
  canHandle(op: string): boolean;
  handle(call: OpCall, runtime: Runtime): Promise<HandlerResult>;
  onReturn?(v: Val, runtime: Runtime): Promise<Val>;
  onFinally?(runtime: Runtime): Promise<void>;
}

export type HandlerStack = Handler[];

export type Runtime = {
  runState: (s: State) => Promise<Val>;
  runSub: (expr: any, ctx: Ctx, store: any, handlers: HandlerStack) => Promise<{ value: Val; ctx: Ctx; store: any }>;
  emitEvent: (ev: any) => void;
};
```

This is the **semantic dispatcher**. Every “special form” beyond the kernel can be realized as a handler that intercepts effects.

---

# 48. The Step Function (CEKS machine)

This is the heart of extensional evaluation.

## 48.1 `src/machine/step.ts`

```ts
// src/machine/step.ts
import type { Expr } from "../ast";
import { lookupAddr, defineInCtx, sealCtx } from "../context";
import type { State } from "./state";
import type { Kont } from "./kont";
import { alloc, read, write } from "../store";
import type { Val } from "../values";
import { syntax, unit } from "../values";
import type { OpCall } from "../effects/handler";

export type StepOutcome =
  | { tag: "State"; state: State }
  | { tag: "Done"; value: Val; state: State }
  | { tag: "OpCall"; call: OpCall };

function atomToVal(a: any): Val {
  return a; // Atom is already a Val variant in our representation
}

function isTruthy(v: Val): boolean {
  return !(v.tag === "Bool" && v.value === false) && v.tag !== "Null";
}

function asClosure(v: Val): Extract<Val, { tag: "Closure" }> {
  if (v.tag !== "Closure") throw new Error("Expected closure");
  return v;
}

function kontHalt(): Kont { return { tag: "Halt" }; }

export function step(s: State): StepOutcome {
  const { expr, ctx, store, kont } = s;

  // If expr is a value-Expr already? We keep Expr separate from Val, so Lit/Sym/etc.
  switch (expr.tag) {
    case "Lit": {
      return applyKont({ ...s, expr }, atomToVal(expr.value));
    }
    case "Sym": {
      const a = lookupAddr(ctx, expr.name);
      const v = read(store, a);
      return applyKont(s, v);
    }
    case "Quote": {
      return applyKont(s, syntax(expr.datum));
    }
    case "Lambda": {
      const clo: Val = {
        tag: "Closure",
        params: expr.params,
        body: expr.body,
        env: ctx,
        ann: expr.ann,
      };
      return applyKont(s, clo);
    }
    case "If": {
      return {
        tag: "State",
        state: {
          ...s,
          expr: expr.test,
          kont: { tag: "IfK", conseq: expr.conseq, alt: expr.alt, ctx, next: kont },
        },
      };
    }
    case "Begin": {
      if (expr.exprs.length === 0) return applyKont(s, unit());
      const [first, ...rest] = expr.exprs;
      return {
        tag: "State",
        state: {
          ...s,
          expr: first,
          kont: { tag: "BeginK", rest, ctx, next: kont },
        },
      };
    }
    case "Define": {
      return {
        tag: "State",
        state: {
          ...s,
          expr: expr.rhs,
          kont: { tag: "DefineK", name: expr.name, ctx, next: kont },
        },
      };
    }
    case "Set": {
      return {
        tag: "State",
        state: {
          ...s,
          expr: expr.rhs,
          kont: { tag: "SetK", name: expr.name, ctx, next: kont },
        },
      };
    }
    case "Apply": {
      return {
        tag: "State",
        state: {
          ...s,
          expr: expr.fn,
          kont: { tag: "ApplyFnK", args: expr.args, ctx, next: kont },
        },
      };
    }
    case "Effect": {
      if (expr.args.length === 0) {
        const call: OpCall = {
          op: expr.op,
          args: [],
          ctx,
          resume: (v: Val) => ({ ...s, kont, ctx, store, expr: { tag: "Lit", value: { tag: "Null" } } as any }), // not used
        };
        // We'll resume properly via applyKont once handled; build resume below:
        call.resume = (v) => ({ ...s, expr: { tag: "Lit", value: { tag: "Null" } } as any }); // placeholder
        // Better: represent “return v” by feeding v into kont; do that in runtime, not here.
      }
      // Evaluate args left-to-right
      const [first, ...rest] = expr.args;
      return {
        tag: "State",
        state: {
          ...s,
          expr: first,
          kont: { tag: "EffectArgsK", op: expr.op, done: [], rest, ctx, next: kont },
        },
      };
    }
    case "Handle": {
      // Evaluate body with handler stack extended is done in runtime, not step,
      // because handlers are not an Expr, they're runtime objects.
      // Here we reify handler AST as a Value and emit an internal effect to install it,
      // OR (simpler) treat Handle as a special node in runtime before stepping.
      throw new Error("Handle should be lowered before stepping (runtime special-case).");
    }
    case "Eval": {
      // Evaluate env if provided, else current ctx
      if (expr.env) {
        return { tag: "State", state: { ...s, expr: expr.env, kont: { tag: "EvalEnvK", qexpr: expr.qexpr, next: kont } } };
      }
      // No env: evaluate qexpr then run sub-eval
      return { tag: "State", state: { ...s, expr: expr.qexpr, kont: { tag: "EvalQExprK", envVal: { tag: "Ctx", ctx } as any, next: kont } } };
    }
    case "Int":
    case "Infer":
    case "Rewrite": {
      // These forms will be lowered to (effect "infer.op" <payload>) during expansion.
      throw new Error(`${expr.tag} should be lowered to Effect infer.op before stepping.`);
    }
    case "Ctx":
    case "Extend":
    case "Seal":
    case "Match": {
      throw new Error(`${expr.tag} should be lowered or implemented; see Part 7.`);
    }
    default: {
      const _exhaustive: never = expr;
      return _exhaustive;
    }
  }
}

function applyKont(s: State, v: Val): StepOutcome {
  const k = s.kont;
  switch (k.tag) {
    case "Halt":
      return { tag: "Done", value: v, state: s };

    case "IfK": {
      const nextExpr = isTruthy(v) ? k.conseq : k.alt;
      return { tag: "State", state: { ...s, expr: nextExpr, ctx: k.ctx, kont: k.next } };
    }

    case "BeginK": {
      if (k.rest.length === 0) return applyKont({ ...s, ctx: k.ctx, kont: k.next }, v);
      const [first, ...rest] = k.rest;
      return { tag: "State", state: { ...s, expr: first, ctx: k.ctx, kont: { tag: "BeginK", rest, ctx: k.ctx, next: k.next } } };
    }

    case "DefineK": {
      // allocate new location for v and bind name in current ctx
      let store = s.store;
      const [store2, addr] = alloc(store, v);
      store = store2;
      // define returns a *new ctx* (persistent)
      const newCtx = defineInCtx(k.ctx, k.ctx.cid, k.name, addr);
      return applyKont({ ...s, ctx: newCtx, store, kont: k.next }, unit());
    }

    case "SetK": {
      // update existing location
      const addr = lookupAddr(k.ctx, k.name);
      const store2 = write(s.store, addr, v);
      return applyKont({ ...s, ctx: k.ctx, store: store2, kont: k.next }, unit());
    }

    case "ApplyFnK": {
      // start evaluating args
      if (k.args.length === 0) {
        // apply with no args
        const clo = asClosure(v);
        return { tag: "State", state: applyClosureState(s, clo, [], k.ctx, k.next) };
      }
      const [first, ...rest] = k.args;
      return {
        tag: "State",
        state: { ...s, expr: first, ctx: k.ctx, kont: { tag: "ApplyArgsK", fn: v, done: [], rest, ctx: k.ctx, next: k.next } },
      };
    }

    case "ApplyArgsK": {
      const done = [...k.done, v];
      if (k.rest.length === 0) {
        const clo = asClosure(k.fn);
        return { tag: "State", state: applyClosureState(s, clo, done, k.ctx, k.next) };
      }
      const [first, ...rest] = k.rest;
      return {
        tag: "State",
        state: { ...s, expr: first, ctx: k.ctx, kont: { tag: "ApplyArgsK", fn: k.fn, done, rest, ctx: k.ctx, next: k.next } },
      };
    }

    case "EffectArgsK": {
      const done = [...k.done, v];
      if (k.rest.length === 0) {
        // emit effect opcall
        const call: OpCall = {
          op: k.op,
          args: done,
          ctx: k.ctx,
          resume: (rv: Val) => ({
            ...s,
            expr: { tag: "Lit", value: { tag: "Null" } } as any,
            ctx: k.ctx,
            kont: k.next,
            store: s.store,
          }),
        };
        // resume must feed rv into continuation, not discard it.
        call.resume = (rv: Val) => {
          const resumed: State = { ...s, ctx: k.ctx, kont: k.next, store: s.store, expr: { tag: "Lit", value: { tag: "Null" } } as any };
          // We'll re-enter applyKont by setting up a synthetic state; runtime will do it by calling a helper.
          // To keep step pure, runtime will wrap: resume(rv) => injectValue(rv, k.next).
          return resumed;
        };
        return { tag: "OpCall", call };
      }
      const [first, ...rest] = k.rest;
      return { tag: "State", state: { ...s, expr: first, ctx: k.ctx, kont: { tag: "EffectArgsK", op: k.op, done, rest, ctx: k.ctx, next: k.next } } };
    }

    default:
      throw new Error(`Unhandled continuation: ${(k as any).tag}`);
  }
}

function applyClosureState(s: State, clo: Extract<Val, { tag: "Closure" }>, args: Val[], callCtx: any, nextKont: Kont): State {
  if (clo.params.length !== args.length) throw new Error(`Arity mismatch: expected ${clo.params.length} got ${args.length}`);

  // Allocate locations for parameters
  let store = s.store;
  const binds: Array<[string, number]> = [];
  for (let i = 0; i < clo.params.length; i++) {
    const [store2, addr] = alloc(store, args[i]);
    store = store2;
    binds.push([clo.params[i], addr]);
  }

  // Extend lexical environment (closure env, not call env)
  const newCtx = {
    ...clo.env,
    parent: clo.env,
    frame: new Map([...binds]),
  } as any; // for brevity; in Part 7 we'd use extendCtx with hashing

  return { ...s, expr: clo.body, ctx: newCtx, store, kont: nextKont };
}
```

### Important note

I deliberately left `Handle`, `Ctx/Extend/Seal`, `Match`, and the `infer.op` lowering as “lowered before stepping.” That is not a dodge; it’s **phase separation**:

* The machine (`step`) should be small and stable.
* The expander lowers surface constructs into:

  * core forms + effects
  * runtime handler objects

That mirrors Scheme’s macro expansion and keeps the CEKS core minimal.

In Part 7, I’ll fill in the expander and show `Handle`/`Match` lowering cleanly.

---

# 49. Runtime Loop: run until value or effect, dispatch handlers

## 49.1 `src/machine/runtime.ts`

```ts
// src/machine/runtime.ts
import type { State } from "./state";
import { step } from "./step";
import type { Handler, OpCall, HandlerStack } from "../effects/handler";
import type { Val } from "../values";

export class Runtime {
  constructor(public handlers: HandlerStack) {}

  async runState(s: State): Promise<Val> {
    while (true) {
      const out = step(s);
      if (out.tag === "Done") {
        // onReturn hooks: unwind handlers if needed (deep handlers)
        let v = out.value;
        for (let i = this.handlers.length - 1; i >= 0; i--) {
          const h = this.handlers[i];
          if (h.onReturn) v = await h.onReturn(v, this);
        }
        return v;
      }
      if (out.tag === "State") {
        s = out.state;
        continue;
      }
      if (out.tag === "OpCall") {
        const v = await this.dispatch(out.call, s);
        // Resume: inject v into continuation.
        // We didn't fully model injection in step(), so we do: set expr to a value-literal surrogate and step.
        // For cleanliness, Part 7 will introduce a ValueExpr node or a distinct machine mode.
        s = out.call.resume(v);
        continue;
      }
    }
  }

  async runSub(expr: any, ctx: any, store: any, handlers: HandlerStack): Promise<{ value: Val; ctx: any; store: any }> {
    const subRuntime = new Runtime(handlers);
    const subState: State = { expr, ctx, store, kont: { tag: "Halt" }, handlers, ledger: ({} as any) };
    const value = await subRuntime.runState(subState);
    return { value, ctx: subState.ctx, store: subState.store };
  }

  emitEvent(ev: any): void {
    // hook ledger; for now no-op
  }

  private async dispatch(call: OpCall, s: State): Promise<Val> {
    for (let i = this.handlers.length - 1; i >= 0; i--) {
      const h = this.handlers[i];
      if (!h.canHandle(call.op)) continue;
      const res = await h.handle(call, this);
      if (res.tag === "Handled") return res.value;
      if (res.tag === "Reemit") return await this.dispatch(res.call, s);
    }
    throw new Error(`Unhandled effect op: ${call.op}`);
  }
}
```

This is the **Chain of Responsibility** concretely: the handler stack *is* the language’s semantic overlay mechanism.

---

# 50. The Oracle Protocol: interactive inference sessions with REPL re-entry

Now we implement the key non-negotiable: inference is not a single “completion” call; it is a session with structured Req/Resp.

## 50.1 `src/oracle/protocol.ts`

```ts
// src/oracle/protocol.ts
import type { Expr } from "../ast";
import type { Hash } from "../values";
import type { Val, MeaningVal, DistVal } from "../values";

export type InferKind = "int" | "search" | "rewrite";

export type InferRequest = {
  kind: InferKind;
  qexpr?: Expr;
  goal?: Val;
  envDigest: Hash;

  // engine configuration artifacts (already resolved)
  prompt: Val;
  policy: Val;
  engine: Val;

  caps: string[];
  budgets: { tokens: number; tools: number; timeMs: number };

  schema: "Meaning" | "DistMeaning" | "DistVal";
};

export type OracleReq =
  | { tag: "ReqEval"; qexpr: Expr; envDigest: Hash }
  | { tag: "ReqApply"; fn: Val; args: Val[]; envDigest: Hash }
  | { tag: "ReqObserve"; envDigest: Hash; schema: Val }
  | { tag: "ReqTool"; call: Val }
  | { tag: "ReqTest"; spec: Val }
  | { tag: "ReqEmitExample"; ex: Val }
  | { tag: "ReqReturn"; result: MeaningVal | DistVal | Val }
  | { tag: "ReqFail"; reason: string };

export type OracleResp =
  | { tag: "RespVal"; v: Val }
  | { tag: "RespMeaning"; m: MeaningVal }
  | { tag: "RespEvidence"; ev: Val }
  | { tag: "RespError"; err: string };

export type OracleSession = AsyncGenerator<OracleReq, { tag: "Return"; value: MeaningVal | DistVal | Val }, OracleResp>;
```

## 50.2 `src/oracle/engine.ts` — engine adapter interface

```ts
// src/oracle/engine.ts
import type { InferRequest, OracleSession } from "./protocol";

export interface EngineAdapter {
  name: string;
  start(req: InferRequest): OracleSession;
}
```

## 50.3 `src/oracle/oracleHandler.ts` — handler for `infer.op`

```ts
// src/oracle/oracleHandler.ts
import type { Handler, OpCall, HandlerResult, Runtime } from "../effects/handler";
import type { EngineAdapter } from "./engine";
import type { InferRequest, OracleReq, OracleResp } from "./protocol";
import type { Val, MeaningVal } from "../values";
import { requireCap } from "../context";

export class OracleHandler implements Handler {
  id = "oracle.handler";
  constructor(private engine: EngineAdapter) {}

  canHandle(op: string): boolean {
    return op === "infer.op";
  }

  async handle(call: OpCall, runtime: Runtime): Promise<HandlerResult> {
    // The infer.op payload is call.args[0] as a structured Val.
    const payload = call.args[0];
    const req = this.decodeInferRequest(payload, call.ctx);

    runtime.emitEvent({ tag: "OracleStart", req, sessionId: "S1", time: Date.now() });

    const session = this.engine.start(req);

    // Interactive loop
    let next = await session.next(undefined as any);
    while (!next.done) {
      const oreq: OracleReq = next.value;
      runtime.emitEvent({ tag: "OracleReq", sessionId: "S1", req: oreq, time: Date.now() });

      const oresp = await this.fulfillOracleReq(oreq, runtime, call);
      runtime.emitEvent({ tag: "OracleResp", sessionId: "S1", resp: oresp, time: Date.now() });

      next = await session.next(oresp);
    }

    const ret = next.value.value;
    return { tag: "Handled", value: ret as any };
  }

  private decodeInferRequest(payload: Val, ctx: any): InferRequest {
    // In the real system, payload is a MapVal/RecordVal; decode robustly.
    // For now, we assume payload is a MapVal of strings->vals or EngineVal etc.
    // We'll mock minimal fields.
    requireCap(ctx, "cap.infer"); // inference itself gated
    return {
      kind: "int",
      envDigest: "env:mock",
      prompt: payload,
      policy: payload,
      engine: payload,
      caps: Array.from(ctx.caps),
      budgets: { tokens: 9000, tools: 20, timeMs: 120000 },
      schema: "Meaning",
    };
  }

  private async fulfillOracleReq(oreq: OracleReq, runtime: Runtime, call: OpCall): Promise<OracleResp> {
    // Capability + budget checks should live here (or in decorators).
    switch (oreq.tag) {
      case "ReqEval": {
        requireCap(call.ctx, "cap.eval");
        const { value } = await runtime.runSub(oreq.qexpr, call.ctx, (call as any).store ?? ({} as any), runtime["handlers"] ?? []);
        return { tag: "RespVal", v: value };
      }
      case "ReqReturn": {
        return { tag: "RespVal", v: oreq.result as any };
      }
      default:
        return { tag: "RespError", err: `Unimplemented oracle req: ${oreq.tag}` };
    }
  }
}
```

This is still a skeleton, but structurally it already enforces the core thesis:

* inference is a **handler-controlled semantic session**
* REPL re-entrancy is a **capability-gated request**
* the engine is not “called once”; it is **driven** via Req/Resp

That’s the mechanism that makes inference a **semantic plane**.

---

# 51. Mock Engines (to test semantics without any external LLM)

You should never bring an actual LLM into the runtime until:

* the machine is deterministic,
* the handler stack is correct,
* the ledger and obligations are correct.

So we build mock engines that behave like oracle sessions.

## 51.1 `src/oracle/mockEngines.ts`

```ts
// src/oracle/mockEngines.ts
import type { EngineAdapter } from "./engine";
import type { InferRequest, OracleSession, OracleReq, OracleResp } from "./protocol";
import type { MeaningVal } from "../values";

export class ConstantMeaningEngine implements EngineAdapter {
  name = "constant-meaning";

  start(req: InferRequest): OracleSession {
    const self = this;

    async function* gen(): OracleSession {
      // No requests: just return a Meaning
      const m: MeaningVal = {
        tag: "Meaning",
        denotation: { tag: "Num", value: 42 } as any,
        confidence: 0.99,
        obligation: { tag: "Str", value: "speculative" } as any,
      };
      return { tag: "Return", value: m };
    }

    return gen();
  }
}

export class EvalAsksEngine implements EngineAdapter {
  name = "asks-for-eval";

  start(req: InferRequest): OracleSession {
    async function* gen(): OracleSession {
      // Ask host evaluator to evaluate (qexpr) — in a real engine, you’d build qexpr.
      const qexpr = req.qexpr!;
      const resp: OracleResp = yield { tag: "ReqEval", qexpr, envDigest: req.envDigest } as OracleReq;

      if (resp.tag !== "RespVal") {
        return { tag: "Return", value: { tag: "Meaning", confidence: 0.1 } as any };
      }

      const m: MeaningVal = {
        tag: "Meaning",
        denotation: resp.v as any,
        confidence: 0.95,
        obligation: { tag: "Str", value: "test-certified-required" } as any,
      };
      return { tag: "Return", value: m };
    }

    return gen();
  }
}
```

This gives you deterministic integration tests for:

* REPL re-entry semantics
* handler stack correctness
* evidence and obligations plumbing

---

# 52. Built-in Handlers: Tool, Commit, Receipt, Train

In the final system, these are distinct handlers installed by profiles.

Here’s the skeletal interface shape; each handler is a Strategy.

## 52.1 `src/effects/builtins.ts` — handler stubs

```ts
// src/effects/builtins.ts
import type { Handler, OpCall, HandlerResult, Runtime } from "./handler";
import { requireCap } from "../context";
import { unit } from "../values";

export class CommitHandler implements Handler {
  id = "commit.handler";
  canHandle(op: string): boolean { return op === "commit.op"; }

  async handle(call: OpCall, runtime: Runtime): Promise<HandlerResult> {
    requireCap(call.ctx, "cap.commit");
    // TODO: decode obligations, check certificates, update artifact registry
    runtime.emitEvent({ tag: "Commit", kind: "generic", payloadHash: "..." });
    return { tag: "Handled", value: unit() };
  }
}

export class ToolHandler implements Handler {
  id = "tool.handler";
  canHandle(op: string): boolean { return op === "tool.op"; }

  async handle(call: OpCall, runtime: Runtime): Promise<HandlerResult> {
    requireCap(call.ctx, "cap.tool");
    // TODO: cache key, execution, evidence blob, receipt
    return { tag: "Handled", value: unit() };
  }
}

export class TrainEmitHandler implements Handler {
  id = "train.emit.handler";
  canHandle(op: string): boolean { return op === "train.emit"; }

  async handle(call: OpCall, runtime: Runtime): Promise<HandlerResult> {
    requireCap(call.ctx, "cap.train.emit");
    // TODO: append to dataset store, ledger
    runtime.emitEvent({ tag: "TrainEmit", exampleHash: "..." });
    return { tag: "Handled", value: unit() };
  }
}

export class ReceiptHandler implements Handler {
  id = "receipt.handler";
  canHandle(op: string): boolean { return op === "ctx.snapshot" || op === "ctx.compress" || op === "ctx.hydrate"; }

  async handle(call: OpCall, runtime: Runtime): Promise<HandlerResult> {
    requireCap(call.ctx, "cap.ctx.manage");
    // TODO: construct ReceiptVal, store it, return it
    return { tag: "Handled", value: unit() };
  }
}
```

---

# 53. Demo Wiring (`src/demo.ts`): proving “infer is not an API call”

This shows:

* extensional evaluation runs CEKS machine
* inference is an effect handled by `OracleHandler`
* engine can request extensional eval (REPL) and return Meaning

```ts
// src/demo.ts
import { mkRootCtx } from "./context";
import { mkStore } from "./store";
import type { State } from "./machine/state";
import { Runtime } from "./machine/runtime";
import { ConstantMeaningEngine, EvalAsksEngine } from "./oracle/mockEngines";
import { OracleHandler } from "./oracle/oracleHandler";
import { CommitHandler, ToolHandler, TrainEmitHandler, ReceiptHandler } from "./effects/builtins";
import type { Expr } from "./ast";

async function main() {
  const ctx = mkRootCtx();
  ctx.caps.add("cap.infer");
  ctx.caps.add("cap.eval");       // allow REPL re-entry
  ctx.caps.add("cap.commit");
  ctx.caps.add("cap.tool");
  ctx.caps.add("cap.train.emit");
  ctx.caps.add("cap.ctx.manage");

  const store = mkStore();

  // Example “program”: (effect "infer.op" payload)
  // In the real system, (int '(+ 1 2) ...) expands to this.
  const payloadExpr: Expr = { tag: "Quote", datum: { tag: "Lit", value: { tag: "Str", value: "payload" } } };

  const prog: Expr = {
    tag: "Effect",
    op: "infer.op",
    args: [
      // pretend payload is literal; in real system, payload is structured MapVal assembled by expander
      payloadExpr,
    ],
  };

  const handlers = [
    new CommitHandler(),
    new ToolHandler(),
    new TrainEmitHandler(),
    new ReceiptHandler(),
    new OracleHandler(new ConstantMeaningEngine()),
  ];

  const runtime = new Runtime(handlers);

  const state: State = { expr: prog, ctx, store, kont: { tag: "Halt" }, handlers, ledger: ({} as any) };

  const v = await runtime.runState(state);
  console.log("result:", v);
}

main().catch((e) => console.error(e));
```

This demo is intentionally trivial: it proves the **semantic dispatch architecture**. In Part 7 we add:

* the expander lowering `(int ...)` to `(effect infer.op ...)` with real structured payloads,
* `handle` lowering into runtime handler objects,
* `match`, `ctx`, `extend`, `seal`,
* and real obligation checking.

---

# 54. What’s incomplete (deliberately) in this Part 6

This is the correct stopping point for “spine first”:

✅ CEKS machine skeleton
✅ Algebraic effects runtime skeleton
✅ Oracle interactive session skeleton
✅ Capability gate points
✅ Handler stack as semantic control surface
✅ Mock engines to test re-entrancy semantics

Still missing (next Part):

1. **A proper “value injection” mode** in the abstract machine (cleanly resume with a Value without faking Lit nodes)
2. The **expander**:

   * macro expansion
   * desugaring surface forms into core/effects
   * lowering `(handle ...)` into runtime handlers
3. Implementations for:

   * `Ctx / Extend / Seal`
   * `Match`
   * `Eval` properly (sub-run with env/store transfer)
4. **Obligations + certification**:

   * test runner integration
   * proof hooks (SMT adapter interface)
   * commit barrier enforcement
5. **Receipt store + compression/hydration** with VOI scoring
6. **Artifact registry + promote pipeline**
7. Stdlib bootstraps:

   * streams
   * amb/nondet
   * generic ops
   * constraints

That is not “missing ideas”; it’s the remaining engineering to make the semantics complete.

---

# 55. Next: Part 7 (the inevitable next layer)

If you reply **continue**, I will deliver **Part 7** with:

* a proper CEKS “Value mode” (or `Expr = Val | AST` bifurcation) so resumption is clean
* a real s-expression reader + macro expander pipeline (`read → expand → eval`)
* lowering rules for:

  * `Handle`
  * `Int/Infer/Rewrite`
  * `Ctx/Extend/Seal`
  * `Match`
* a minimal obligation system:

  * `Obligation` datatype
  * `CheckResult`
  * `commit` handler enforcing obligations
* an in-memory receipt store + content-addressing
* a working end-to-end example: **rewrite with verification then commit**

Reply **continue**.
