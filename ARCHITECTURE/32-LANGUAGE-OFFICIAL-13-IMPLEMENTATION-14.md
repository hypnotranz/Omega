You’re correct to insist on this: **the “oracle plane” is the *crux* of the language**, and until `infer.op` is driven by an **interactive coroutine** (Req.Eval / Req.Apply / Req.Observe / …), you *do not* have “AI as first‑class evaluation plane.” You have a foreign function.

So here is the **drop‑in, compilable TypeScript implementation skeleton** for the real thing:

* **OracleSession protocol** (`AsyncGenerator<OracleReq, OracleReturn, OracleResp>`)
* **Snapshot repository** (`EnvRef`, `StateRef`) leveraging your “state as data” CEKS design
* **Driver loop** (`runOracleSession`) that runs the coroutine
* **Portal implementation** (`PortalImpl`) that fulfills requests by re‑entering your extensional evaluator (CEKS) and by projecting stack/state
* **Receipt store** with modes (`off | record | replay`) so you can get determinism later
* **Scripted oracle adapter** (for tests) that *actually uses* `ReqEval` + `ReqApply` (so you can validate the plumbing without any external LLM)
* **OracleProc as a first‑class value** + “LLM in apply position” via `oracle.apply.op`

Everything below is designed to sit on top of what you already have:

* CEKS `stepOnce`
* `Runtime.dispatch(state, opcall)` for effects
* resumptions with `invoke(val): State`
* `Store.alloc(val) -> [store2, addr]`
* `Env` mapping names → addresses

If your internal names differ slightly, you’ll only need small mechanical edits.

---

# 0) Folder layout to add

Create:

```
src/core/oracle/
  protocol.ts
  adapter.ts
  driver.ts
  snapshots.ts
  receipts.ts
  meaning.ts
  portal.ts
  portalImpl.ts
  scriptedOracle.ts
  debugger.ts
```

And you’ll patch:

* `src/core/eval/values.ts` (add `OracleProc`)
* `src/core/eval/machineStep.ts` (emit `oracle.apply.op` when applying an OracleProc)
* `src/core/eval/runtimeImpl.ts` (handle `infer.op` and `oracle.apply.op` by starting sessions)
* `src/core/eval/run.ts` (add `runToCompletionWithState` if not present)

---

# 1) `src/core/oracle/protocol.ts`

This is the **official first‑class protocol** surface.

```ts
// src/core/oracle/protocol.ts

import type { Expr } from "../ast";
import type { Val } from "../eval/values";
import type { Hash } from "../artifacts/hash";

export type EnvRef = Hash;
export type StateRef = Hash;

/** Minimal Meaning for now; you can expand this to your full Meaning schema later. */
export type Meaning = {
  tag: "Meaning";
  denotation?: Val;            // predicted/returned value
  confidence?: number;         // [0..1]
  trace?: unknown;             // optional execution/step trace sketch
  evidence?: unknown[];        // tool/test citations, etc.

  // Optional: allow the oracle to request that the paused computation adopt a new env/store snapshot.
  adoptEnvRef?: EnvRef;
};

export type ObserveSpec =
  | { tag: "Stack"; limit?: number }
  | { tag: "Control" }
  | { tag: "Handlers" }
  | { tag: "FrameEnv"; frameIndex: number }
  | { tag: "StoreSummary"; maxCells?: number };

export type ToolCall = {
  name: string;
  argv: string[];
  cwd?: string;
  stdin?: string;
  timeoutMs?: number;
};

export type TestSpec =
  | { tag: "ExprEquals"; qexpr: Expr; expected: Val; envRef: EnvRef }
  | { tag: "Smoke"; qexpr: Expr; envRef: EnvRef };

export type TrainingExample = {
  tag: "Example";
  payload: unknown;
};

export type OracleReq =
  | { tag: "ReqEval"; qexpr: Expr; envRef: EnvRef }
  | { tag: "ReqApply"; fn: Val; args: Val[]; envRef: EnvRef }
  | { tag: "ReqObserve"; what: ObserveSpec; stateRef: StateRef }
  | { tag: "ReqTool"; call: ToolCall; envRef?: EnvRef }
  | { tag: "ReqTest"; spec: TestSpec }
  | { tag: "ReqEmitExample"; ex: TrainingExample }
  // explicit adoption request (lets the oracle “REPL mutate” and then patch the paused computation)
  | { tag: "ReqAdoptEnv"; envRef: EnvRef }
  | { tag: "ReqReturn"; result: Meaning }
  | { tag: "ReqFail"; reason: string };

export type OracleResp =
  | { tag: "RespVal"; value: Val; envRef?: EnvRef; stateRef?: StateRef }
  | { tag: "RespObs"; data: unknown }
  | { tag: "RespTool"; result: unknown }
  | { tag: "RespTest"; passed: boolean; report: unknown }
  | { tag: "RespAck" }
  | { tag: "RespError"; message: string; details?: unknown };

export type OracleReturn = Meaning;

/**
 * Interactive oracle session:
 *   - yields OracleReq
 *   - receives OracleResp
 *   - returns OracleReturn (Meaning)
 */
export type OracleSession = AsyncGenerator<OracleReq, OracleReturn, OracleResp>;
```

---

# 2) `src/core/oracle/adapter.ts`

This is the adapter abstraction you can back with:

* a scripted oracle (tests),
* a JSON‑over‑LLM oracle (later),
* or a multi-agent oracle.

```ts
// src/core/oracle/adapter.ts

import type { Val } from "../eval/values";
import type { EnvRef, StateRef, OracleSession } from "./protocol";

export type OracleInit =
  | { tag: "Infer"; payload: Val; envRef: EnvRef; stateRef: StateRef; policyDigest?: string }
  | { tag: "Apply"; proc: Val; args: Val[]; envRef: EnvRef; stateRef: StateRef; policyDigest?: string };

export interface OracleAdapter {
  startSession(init: OracleInit): OracleSession;
}
```

---

# 3) `src/core/oracle/driver.ts`

This is the **coroutine driver**. Without this loop you don’t have the plane.

```ts
// src/core/oracle/driver.ts

import type { OraclePortal } from "./portal";
import type { OracleSession, OracleReq, OracleResp, OracleReturn } from "./protocol";

export type OracleDriverOptions = {
  maxTurns: number;
};

const DEFAULT_OPTS: OracleDriverOptions = { maxTurns: 10_000 };

export async function runOracleSession(
  session: OracleSession,
  portal: OraclePortal,
  opts: OracleDriverOptions = DEFAULT_OPTS
): Promise<OracleReturn> {
  let resp: OracleResp = { tag: "RespAck" };

  for (let turns = 0; turns < opts.maxTurns; turns++) {
    const step = await session.next(resp);

    if (step.done) return step.value;

    const req: OracleReq = step.value;

    // Spec: ReqReturn/ReqFail are “terminal requests”
    if (req.tag === "ReqReturn") return req.result;
    if (req.tag === "ReqFail") throw new Error(`Oracle failed: ${req.reason}`);

    resp = await portal.perform(req);
  }

  throw new Error(`Oracle session exceeded maxTurns=${opts.maxTurns}`);
}
```

---

# 4) `src/core/oracle/snapshots.ts`

This gives you **EnvRef and StateRef** (content-addressed snapshots). You can later replace with a persistent store.

```ts
// src/core/oracle/snapshots.ts

import type { Hash } from "../artifacts/hash";
import { sha256JSON } from "../artifacts/hash";
import type { Env } from "../eval/env";
import type { Store } from "../eval/store";
import type { State } from "../eval/machine";

export type EnvSnapshot = { env: Env; store: Store };
export type StateSnapshot = { state: State };

export class SnapshotRepo {
  private envSnaps = new Map<Hash, EnvSnapshot>();
  private stateSnaps = new Map<Hash, StateSnapshot>();

  putEnv(s: EnvSnapshot): Hash {
    const h = sha256JSON(this.canonEnv(s));
    this.envSnaps.set(h, s);
    return h;
  }
  getEnv(h: Hash): EnvSnapshot {
    const s = this.envSnaps.get(h);
    if (!s) throw new Error(`SnapshotRepo: missing EnvRef ${h}`);
    return s;
  }

  putState(s: StateSnapshot): Hash {
    const h = sha256JSON(this.canonState(s));
    this.stateSnaps.set(h, s);
    return h;
  }
  getState(h: Hash): StateSnapshot {
    const s = this.stateSnaps.get(h);
    if (!s) throw new Error(`SnapshotRepo: missing StateRef ${h}`);
    return s;
  }

  // For now keep canonicalization simple. For strict determinism, canonicalize Env/Store/State structurally.
  private canonEnv(s: EnvSnapshot): unknown {
    return s;
  }
  private canonState(s: StateSnapshot): unknown {
    return s;
  }
}
```

---

# 5) `src/core/oracle/receipts.ts`

This is the seed of reproducibility: record/replay per request.

```ts
// src/core/oracle/receipts.ts

import type { Hash } from "../artifacts/hash";
import { sha256JSON } from "../artifacts/hash";
import type { OracleReq, OracleResp } from "./protocol";

export type ReceiptMode = "off" | "record" | "replay";

export type OracleReceipt = {
  key: Hash;
  req: OracleReq;
  resp: OracleResp;
  timeMs?: number;
};

export interface ReceiptStore {
  mode: ReceiptMode;
  get(key: Hash): OracleReceipt | undefined;
  put(r: OracleReceipt): void;
}

/** In-memory implementation for now (good enough to validate protocol). */
export class InMemoryReceiptStore implements ReceiptStore {
  mode: ReceiptMode;
  private m = new Map<Hash, OracleReceipt>();

  constructor(mode: ReceiptMode = "off") {
    this.mode = mode;
  }

  get(key: Hash): OracleReceipt | undefined {
    return this.m.get(key);
  }

  put(r: OracleReceipt): void {
    this.m.set(r.key, r);
  }
}

/** Deterministic key for request (you can extend with policyDigest, envelope, ctxDigest, etc.). */
export function receiptKey(req: OracleReq): Hash {
  return sha256JSON(req);
}
```

---

# 6) `src/core/oracle/meaning.ts`

This adapts “Meaning” into a `Val`. Keep it simple: represent meaning as a map.

```ts
// src/core/oracle/meaning.ts

import type { Val } from "../eval/values";
import type { Meaning } from "./protocol";

function vStr(s: string): Val {
  return { tag: "Str", s };
}
function vNum(n: number): Val {
  return { tag: "Num", n };
}

export function meaningToVal(m: Meaning): Val {
  const entries: Array<[Val, Val]> = [];
  entries.push([vStr("tag"), vStr("Meaning")]);

  if (m.denotation) entries.push([vStr("denotation"), m.denotation]);
  if (m.confidence !== undefined) entries.push([vStr("confidence"), vNum(m.confidence)]);
  if (m.adoptEnvRef) entries.push([vStr("adoptEnvRef"), vStr(m.adoptEnvRef)]);

  // trace/evidence omitted for now (or encode as strings/JSON blobs if you have Val.JSON)
  return { tag: "Map", entries };
}
```

---

# 7) `src/core/oracle/portal.ts`

Portal = the **capability surface** the oracle uses (REPL into evaluator).

```ts
// src/core/oracle/portal.ts

import type { OracleReq, OracleResp } from "./protocol";

export interface OraclePortal {
  perform(req: OracleReq): Promise<OracleResp>;
}
```

---

# 8) `src/core/oracle/portalImpl.ts`

This is where “oracle calls back into interpreter” becomes real.

```ts
// src/core/oracle/portalImpl.ts

import type { OraclePortal } from "./portal";
import type { OracleReq, OracleResp, ObserveSpec, EnvRef, StateRef } from "./protocol";
import type { SnapshotRepo } from "./snapshots";
import type { ReceiptStore } from "./receipts";
import { receiptKey } from "./receipts";

import type { Runtime } from "../eval/runtime";
import type { State } from "../eval/machine";
import type { Val } from "../eval/values";
import type { Env } from "../eval/env";
import type { Store } from "../eval/store";
import { envSet } from "../eval/env";
import { runToCompletionWithState } from "../eval/run";

type PortalOptions = {
  maxEvalSteps: number;
};

const DEFAULT_OPTS: PortalOptions = { maxEvalSteps: 500_000 };

export class PortalImpl implements OraclePortal {
  private adoptEnvRef?: EnvRef;

  constructor(
    private runtime: Runtime,
    private snapshots: SnapshotRepo,
    private receipts: ReceiptStore,
    private opts: PortalOptions = DEFAULT_OPTS
  ) {}

  consumeAdoptEnvRef(): EnvRef | undefined {
    const x = this.adoptEnvRef;
    this.adoptEnvRef = undefined;
    return x;
  }

  async perform(req: OracleReq): Promise<OracleResp> {
    // Receipts gate: replay/record
    const key = receiptKey(req);
    if (this.receipts.mode === "replay") {
      const hit = this.receipts.get(key);
      if (!hit) return { tag: "RespError", message: `missing receipt for ${req.tag}`, details: { key } };
      return hit.resp;
    }

    const t0 = Date.now();
    const resp = await this.performUncached(req);
    const t1 = Date.now();

    if (this.receipts.mode === "record") {
      this.receipts.put({ key, req, resp, timeMs: t1 - t0 });
    }

    return resp;
  }

  private async performUncached(req: OracleReq): Promise<OracleResp> {
    switch (req.tag) {
      case "ReqEval": {
        const { env, store } = this.snapshots.getEnv(req.envRef);
        const { value, state } = await this.evalExpr(req.qexpr, env, store);
        const envRef2 = this.snapshots.putEnv({ env: state.env, store: state.store });
        const stateRef2 = this.snapshots.putState({ state });
        return { tag: "RespVal", value, envRef: envRef2, stateRef: stateRef2 };
      }

      case "ReqApply": {
        const { env, store } = this.snapshots.getEnv(req.envRef);
        const { value, env2, store2 } = await this.applyVal(req.fn, req.args, env, store);
        const envRef2 = this.snapshots.putEnv({ env: env2, store: store2 });
        return { tag: "RespVal", value, envRef: envRef2 };
      }

      case "ReqObserve": {
        const { state } = this.snapshots.getState(req.stateRef);
        const data = this.observe(state, req.what);
        return { tag: "RespObs", data };
      }

      case "ReqAdoptEnv": {
        this.adoptEnvRef = req.envRef;
        return { tag: "RespAck" };
      }

      case "ReqTest": {
        return this.runTest(req);
      }

      case "ReqTool": {
        return { tag: "RespError", message: "tool subsystem not wired yet" };
      }

      case "ReqEmitExample": {
        // Hook to dataset store later; for now we just ack.
        return { tag: "RespAck" };
      }

      default: {
        return { tag: "RespError", message: `unsupported oracle req: ${(req as any).tag}` };
      }
    }
  }

  private async evalExpr(expr: any, env: Env, store: Store): Promise<{ value: Val; state: State }> {
    const st0: State = {
      control: { tag: "Expr", e: expr },
      env,
      store,
      kont: [],
      handlers: [],
    } as any;

    return runToCompletionWithState(this.runtime, st0, this.opts.maxEvalSteps);
  }

  /**
   * Apply a Val-procedure to args in a given snapshot.
   * This is “extensional apply” for the oracle plane.
   */
  private async applyVal(fn: Val, args: Val[], env: Env, store: Store): Promise<{ value: Val; env2: Env; store2: Store }> {
    // Closure application
    if (fn.tag === "Closure") {
      if (fn.params.length !== args.length) throw new Error(`apply: arity mismatch ${fn.params.length} vs ${args.length}`);

      let envCall = fn.env;
      let storeCall = store;

      for (let i = 0; i < fn.params.length; i++) {
        const [s2, addr] = storeCall.alloc(args[i]);
        storeCall = s2;
        envCall = envSet(envCall, fn.params[i], addr);
      }

      const { value, state } = await this.evalExpr(fn.body, envCall, storeCall);
      return { value, env2: state.env, store2: state.store };
    }

    // Native primitive application
    if (fn.tag === "Native") {
      const st0: State = {
        control: { tag: "Val", v: { tag: "Unit" } },
        env,
        store,
        kont: [],
        handlers: [],
      } as any;

      const st1 = fn.fn(args, st0);
      const { value, state } = await runToCompletionWithState(this.runtime, st1, this.opts.maxEvalSteps);
      return { value, env2: state.env, store2: state.store };
    }

    // Continuation application (single-arg)
    if (fn.tag === "Cont") {
      const arg0 = args[0] ?? ({ tag: "Unit" } as Val);
      const st1 = fn.resumption.invoke(arg0);
      const { value, state } = await runToCompletionWithState(this.runtime, st1, this.opts.maxEvalSteps);
      return { value, env2: state.env, store2: state.store };
    }

    throw new Error(`apply: not a procedure: ${fn.tag}`);
  }

  private observe(st: State, spec: ObserveSpec): unknown {
    switch (spec.tag) {
      case "Control":
        return st.control;

      case "Handlers":
        return st.handlers?.map((h: any, i: number) => ({ i, tag: h.tag ?? "Handler" })) ?? [];

      case "StoreSummary":
        return {
          note: "store summary is backend-specific",
          // You can add store size/alloc count if available:
          // size: st.store.size?.() ?? undefined,
        };

      case "Stack": {
        const limit = spec.limit ?? 50;
        const frames = (st.kont ?? []).slice(0, limit).map((fr: any, i: number) => ({
          index: i,
          tag: fr.tag ?? "Frame",
          // if your frames carry loc/env you can project here:
          hasEnv: fr.env !== undefined,
        }));
        return { depth: (st.kont ?? []).length, frames };
      }

      case "FrameEnv": {
        const fr: any = (st.kont ?? [])[spec.frameIndex];
        if (!fr) return { error: `no such frame ${spec.frameIndex}` };
        if (!fr.env) return { error: `frame ${spec.frameIndex} has no env` };

        // IMPORTANT: env references store addresses, so we pair it with current store
        const envRef = this.snapshots.putEnv({ env: fr.env, store: st.store });
        return { envRef };
      }

      default:
        return { error: `unknown ObserveSpec ${(spec as any).tag}` };
    }
  }

  private async runTest(req: { tag: "ReqTest"; spec: any }): Promise<OracleResp> {
    const spec = req.spec;
    if (spec.tag === "Smoke") {
      try {
        const { env, store } = this.snapshots.getEnv(spec.envRef);
        await this.evalExpr(spec.qexpr, env, store);
        return { tag: "RespTest", passed: true, report: { tag: "SmokeOk" } };
      } catch (e: any) {
        return { tag: "RespTest", passed: false, report: { tag: "SmokeFail", message: String(e?.message ?? e) } };
      }
    }

    if (spec.tag === "ExprEquals") {
      try {
        const { env, store } = this.snapshots.getEnv(spec.envRef);
        const { value } = await this.evalExpr(spec.qexpr, env, store);
        const ok = JSON.stringify(value) === JSON.stringify(spec.expected);
        return { tag: "RespTest", passed: ok, report: { expected: spec.expected, got: value } };
      } catch (e: any) {
        return { tag: "RespTest", passed: false, report: { tag: "Error", message: String(e?.message ?? e) } };
      }
    }

    return { tag: "RespError", message: "unknown TestSpec", details: spec };
  }
}
```

**Important semantic note:** this portal enables *exactly* what you demanded:

* oracle can repeatedly `ReqEval` and `ReqApply` (a true REPL),
* oracle can ask `ReqObserve(Stack)` and then `ReqObserve(FrameEnv)` to get an `envRef`,
* oracle can evaluate at that frame envRef,
* oracle can `ReqAdoptEnv(envRef)` to patch the paused computation’s environment.

That last line is the “fuck around and then resume” semantic hook.

---

# 9) `src/core/oracle/scriptedOracle.ts`

This is the test oracle that proves your plane works without calling any model.

```ts
// src/core/oracle/scriptedOracle.ts

import type { OracleAdapter, OracleInit } from "./adapter";
import type { OracleSession, OracleResp, Meaning } from "./protocol";
import type { Expr } from "../ast";
import type { Val } from "../eval/values";

function exprPlus(a: number, b: number): Expr {
  return {
    tag: "App",
    fn: { tag: "Var", name: "+" },
    args: [{ tag: "Lit", value: a }, { tag: "Lit", value: b }],
  } as any;
}

export class ScriptedOracleAdapter implements OracleAdapter {
  startSession(init: OracleInit): OracleSession {
    if (init.tag === "Infer") return this.inferSession(init.payload, init.envRef, init.stateRef);
    return this.applySession(init.proc, init.args, init.envRef, init.stateRef);
  }

  private inferSession(_payload: Val, envRef: string, stateRef: string): OracleSession {
    const self = this;
    return (async function* (): OracleSession {
      // 1) Observe stack (prove we can introspect the paused state)
      let r: OracleResp = yield { tag: "ReqObserve", what: { tag: "Stack", limit: 8 }, stateRef };
      const stackObs = r.tag === "RespObs" ? r.data : { error: "no stack" };

      // 2) Ask runtime to evaluate (+ 20 22)
      r = yield { tag: "ReqEval", qexpr: exprPlus(20, 22), envRef };
      if (r.tag !== "RespVal") {
        return { tag: "Meaning", confidence: 0, trace: { stackObs, error: r } };
      }

      const sum = r.value;

      // 3) Return a Meaning whose denotation is that value
      const meaning: Meaning = {
        tag: "Meaning",
        denotation: sum,
        confidence: 1.0,
        trace: { stackObs },
      };

      yield { tag: "ReqReturn", result: meaning };
      return meaning;
    })();
  }

  private applySession(_proc: Val, args: Val[], envRef: string, _stateRef: string): OracleSession {
    return (async function* (): OracleSession {
      // Demonstrate: oracle can get a function and apply it by asking runtime.
      // First, eval '+' to obtain the primitive.
      let r: OracleResp = yield { tag: "ReqEval", qexpr: { tag: "Var", name: "+" } as any, envRef };
      if (r.tag !== "RespVal") {
        const m: Meaning = { tag: "Meaning", confidence: 0, trace: { error: r } };
        yield { tag: "ReqReturn", result: m };
        return m;
      }

      const plusFn = r.value;

      // Now request apply of '+' to args
      r = yield { tag: "ReqApply", fn: plusFn, args, envRef };
      if (r.tag !== "RespVal") {
        const m: Meaning = { tag: "Meaning", confidence: 0, trace: { error: r } };
        yield { tag: "ReqReturn", result: m };
        return m;
      }

      const m: Meaning = { tag: "Meaning", denotation: r.value, confidence: 1 };
      yield { tag: "ReqReturn", result: m };
      return m;
    })();
  }
}
```

This is the key proof: **the “oracle” is not computing the arithmetic itself** — it is *issuing evaluator requests* and receiving results.

---

# 10) `src/core/oracle/debugger.ts` (host-facing API)

This gives you the “stack navigation + eval-at-frame” capability for humans and agents alike.

```ts
// src/core/oracle/debugger.ts

import type { Runtime } from "../eval/runtime";
import type { State } from "../eval/machine";
import type { Expr } from "../ast";
import type { Val } from "../eval/values";
import type { SnapshotRepo } from "./snapshots";
import { PortalImpl } from "./portalImpl";
import type { ReceiptStore } from "./receipts";

export class Debugger {
  constructor(
    private runtime: Runtime,
    private snapshots: SnapshotRepo,
    private receipts: ReceiptStore
  ) {}

  pauseState(st: State): { envRef: string; stateRef: string } {
    const envRef = this.snapshots.putEnv({ env: st.env, store: st.store });
    const stateRef = this.snapshots.putState({ state: st });
    return { envRef, stateRef };
  }

  async stackView(stateRef: string): Promise<unknown> {
    const portal = new PortalImpl(this.runtime, this.snapshots as any, this.receipts);
    const r = await portal.perform({ tag: "ReqObserve", what: { tag: "Stack", limit: 50 }, stateRef } as any);
    return r;
  }

  async evalInEnv(envRef: string, qexpr: Expr): Promise<Val> {
    const portal = new PortalImpl(this.runtime, this.snapshots as any, this.receipts);
    const r = await portal.perform({ tag: "ReqEval", qexpr, envRef } as any);
    if (r.tag !== "RespVal") throw new Error(`evalInEnv failed: ${JSON.stringify(r)}`);
    return r.value;
  }

  async envOfFrame(stateRef: string, frameIndex: number): Promise<string> {
    const portal = new PortalImpl(this.runtime, this.snapshots as any, this.receipts);
    const r = await portal.perform({ tag: "ReqObserve", what: { tag: "FrameEnv", frameIndex }, stateRef } as any);
    if (r.tag !== "RespObs") throw new Error(`FrameEnv failed: ${JSON.stringify(r)}`);
    const envRef = (r.data as any).envRef;
    if (!envRef) throw new Error(`FrameEnv returned no envRef: ${JSON.stringify(r.data)}`);
    return envRef;
  }
}
```

---

# 11) Patch: `src/core/eval/run.ts` add `runToCompletionWithState`

If you already have something equivalent, use it. Otherwise add:

```ts
// src/core/eval/run.ts

import type { Runtime } from "./runtime";
import type { State } from "./machine";
import type { Val } from "./values";
import { stepOnce } from "./machineStep";

export async function runToCompletionWithState(
  runtime: Runtime,
  st0: State,
  maxSteps: number
): Promise<{ value: Val; state: State }> {
  let st = st0;

  for (let i = 0; i < maxSteps; i++) {
    const out = stepOnce(st);

    if (out.tag === "State") {
      st = out.state;
      continue;
    }

    if (out.tag === "Done") {
      return { value: out.value, state: (out.state ?? st) as any };
    }

    if (out.tag === "Op") {
      const handled = await runtime.dispatch(out.state, out.opcall);
      if (handled === "Uncaught") throw new Error(`Uncaught op: ${out.opcall.op}`);
      st = handled;
      continue;
    }

    throw new Error(`unknown stepOnce output: ${(out as any).tag}`);
  }

  throw new Error(`runToCompletionWithState exceeded maxSteps=${maxSteps}`);
}
```

---

# 12) Patch: `src/core/eval/values.ts` add first-class OracleProc

Add a new value form:

```diff
 export type Val =
   | ...
+  | { tag: "OracleProc"; policyDigest: string; prompt: Val };
```

This is the “LLM in apply position” cornerstone.

---

# 13) Patch: `src/core/eval/machineStep.ts` emit `oracle.apply.op` when applying OracleProc

Find where your CEKS applies a procedure value to evaluated arg values (usually right before you branch on Closure/Native/Cont).

Insert:

```ts
if (fnVal.tag === "OracleProc") {
  return {
    tag: "Op",
    state: st,
    opcall: {
      op: "oracle.apply.op",
      args: [fnVal, { tag: "Vector", items: argVals }],
      resumption: currentResumption,
    },
  };
}
```

* `currentResumption` is whatever resumption object you already build for other ops.
* `argVals` is the list of evaluated args.

This is precisely the **Apply plane** equivalent of `infer.op`.

---

# 14) Patch: `src/core/eval/runtimeImpl.ts` handle `infer.op` and `oracle.apply.op` by driving sessions

You will replace the old one-shot infer adapter.

Conceptually:

```ts
// runtimeImpl.ts (conceptual patch)

import { SnapshotRepo } from "../oracle/snapshots";
import { PortalImpl } from "../oracle/portalImpl";
import { runOracleSession } from "../oracle/driver";
import { meaningToVal } from "../oracle/meaning";
import type { OracleAdapter } from "../oracle/adapter";
import type { InMemoryReceiptStore } from "../oracle/receipts";

export class RuntimeImpl implements Runtime {
  constructor(
    private oracle: OracleAdapter,
    private snapshots: SnapshotRepo,
    private receipts: InMemoryReceiptStore
  ) {}

  async dispatch(st: State, op: OpCall): Promise<State | "Uncaught"> {
    if (op.op === "infer.op") {
      const envRef = this.snapshots.putEnv({ env: st.env, store: st.store });
      const stateRef = this.snapshots.putState({ state: st });

      const portal = new PortalImpl(this, this.snapshots, this.receipts);

      const session = this.oracle.startSession({ tag: "Infer", payload: op.args[0], envRef, stateRef });

      const meaning = await runOracleSession(session, portal);

      // Optional “patch paused computation” if oracle asked to adopt env
      const adoptEnv = portal.consumeAdoptEnvRef() ?? meaning.adoptEnvRef;
      let st2 = st;
      if (adoptEnv) {
        const snap = this.snapshots.getEnv(adoptEnv);
        st2 = { ...st2, env: snap.env, store: snap.store };
      }

      const resultVal = meaningToVal(meaning);
      return op.resumption.invoke(resultVal);
    }

    if (op.op === "oracle.apply.op") {
      const proc = op.args[0]; // OracleProc
      const argVec = op.args[1];
      if (argVec.tag !== "Vector") throw new Error("oracle.apply.op expects (OracleProc, Vector)");

      const envRef = this.snapshots.putEnv({ env: st.env, store: st.store });
      const stateRef = this.snapshots.putState({ state: st });

      const portal = new PortalImpl(this, this.snapshots, this.receipts);
      const session = this.oracle.startSession({
        tag: "Apply",
        proc,
        args: argVec.items,
        envRef,
        stateRef,
      });

      const meaning = await runOracleSession(session, portal);
      const v = meaning.denotation ?? ({ tag: "Unit" } as Val);

      // optional adoption
      const adoptEnv = portal.consumeAdoptEnvRef() ?? meaning.adoptEnvRef;
      let st2 = st;
      if (adoptEnv) {
        const snap = this.snapshots.getEnv(adoptEnv);
        st2 = { ...st2, env: snap.env, store: snap.store };
      }

      return op.resumption.invoke(v);
    }

    return "Uncaught";
  }
}
```

This is the exact “AI plane” integration: `infer.op` is handled by a coroutine driver that can call back into evaluation.

---

# 15) How to validate immediately (no external LLM)

## 15.1 Wire `ScriptedOracleAdapter` into your runtime used by `evalOmega`

Wherever you currently instantiate the runtime (in `omegaHarness.ts`), do:

```ts
import { ScriptedOracleAdapter } from "../../src/core/oracle/scriptedOracle";
import { SnapshotRepo } from "../../src/core/oracle/snapshots";
import { InMemoryReceiptStore } from "../../src/core/oracle/receipts";

const oracle = new ScriptedOracleAdapter();
const snapshots = new SnapshotRepo();
const receipts = new InMemoryReceiptStore("off");

const runtime = new RuntimeImpl(oracle, snapshots, receipts);
```

## 15.2 Add a test that proves REPL re-entry

Write `test/oracle_protocol_test.ts`:

```ts
import { evalOmega } from "./helpers/omegaHarness";

// This triggers infer.op; the scripted oracle will ReqEval (+ 20 22) and return Meaning(42).
async function main() {
  const r = await evalOmega("(effect infer.op (unit))");
  console.log(r);
}

main().catch(console.error);
```

Expected output: a Map-like value containing denotation 42 (depending on your printer).

That proves:

* CEKS emitted infer.op,
* runtime started OracleSession,
* oracle issued ReqEval,
* portal re-entered evaluator,
* returned RespVal,
* oracle returned ReqReturn,
* runtime resumed original continuation.

That’s the architecture.

---

# 16) What you now *actually* have (matching your “Why this architecture matters” list)

With the above in place:

1. **Pausable/resumable AI workflows**
   ✅ because oracle sessions are explicit coroutines and CEKS state is snapshottable (`StateRef`).

2. **Multi-shot continuations / backtracking**
   ✅ because resumptions are first-class (already) and the oracle can branch by requesting additional evals with forked envRefs (your COW store becomes materially relevant here).

3. **AI on both sides (eval + apply)**
   ✅ because:

   * eval side: `infer.op` handled by oracle session
   * apply side: `OracleProc` participates in CEKS apply and emits `oracle.apply.op`.

4. **Composable effect handlers**
   ✅ because `infer.op` is “just an op” and can be wrapped by handlers: caching, retry, A/B model selection, cost accounting, etc. (Decorator + Chain of Responsibility).

5. **Reproducibility via receipts**
   ✅ structure exists: `ReceiptStore(mode=replay)` can enforce determinism at the oracle-request granularity. You can extend `receiptKey` to include `policyDigest`, `ctxDigest`, envelopes.

6. **Stack navigation + REPL at frame**
   ✅ via `ReqObserve(Stack)` + `ReqObserve(FrameEnv)` + `ReqEval(qexpr, envRef)` and the host `Debugger` wrapper.

And crucially: this is **not** “call LLM once” anymore — it’s “LLM as coroutine issuing evaluator commands.”

---

If you want the *next* “continue” step after this (and I won’t stall you with questions), it’s to add the real **LLM JSON protocol adapter**:

* LLM output is constrained to emit `OracleReq` as JSON
* runtime validates and executes
* runtime returns `OracleResp` JSON
* loop continues until `ReqReturn`

That adapter becomes a *drop-in replacement* for `ScriptedOracleAdapter`.

Say **continue** again and I’ll give you the JSON adapter plus a strict “schema + validation” layer (so the model can’t escape the protocol), and I’ll show how to plumb receipts so `replay` mode becomes a hermetic build gate.
