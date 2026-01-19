// src/core/oracle/portalImpl.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Patch Set C4: Extended portal with ReqMatch, ReqAssert, ReqSnapshot, ReqCompress, ReqHydrate

import type { OraclePortal } from "./portal";
import type { OracleReq, OracleResp, ObserveSpec, EnvRef, StateRef, QExpr, TestSpec } from "./protocol";
import type { SnapshotRepo } from "./snapshots";
import type { ReceiptStore } from "./receipts";
import { receiptKey } from "./receipts";

import type { Runtime } from "../eval/runtime";
import type { State } from "../eval/machine";
import type { Val } from "../eval/values";
import type { Env } from "../eval/env";
import type { Store } from "../eval/store";
import { envSet, envGet } from "../eval/env";
import { runToCompletionWithState } from "../eval/run";

// Patch Set C imports
import { matchAST } from "./match";
import { CtxReceiptRepo } from "./ctxReceipts";

// Tool registry import
import type { ToolRegistry } from "../tools/registry";

// Governance imports for capability checking
import { capRequire, capHas, type CapSet } from "../governance/caps";

type PortalOptions = {
  maxEvalSteps: number;
  parseText?: (src: string) => unknown;
  toolRegistry?: ToolRegistry;
  caps?: CapSet;  // Capabilities for governance checks
};

const DEFAULT_OPTS: PortalOptions = { maxEvalSteps: 500_000, caps: ["*"] };

export class PortalImpl implements OraclePortal {
  private adoptEnvRef?: EnvRef;
  private ctxReceipts = new CtxReceiptRepo();

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

  private normalizeQExpr(q: QExpr): unknown {
    if (typeof q === "string") {
      // If we have a parser, use it
      if (this.opts.parseText) return this.opts.parseText(q);
      // Otherwise return as-is (string matching)
      return q;
    }
    if (q && typeof q === "object" && "tag" in q && q.tag === "Text" && "text" in q && typeof q.text === "string") {
      if (this.opts.parseText) return this.opts.parseText(q.text);
      return q.text;
    }
    return q;
  }

  private async performUncached(req: OracleReq): Promise<OracleResp> {
    switch (req.tag) {
      case "ReqEval": {
        const { env, store } = this.snapshots.getEnv(req.envRef);
        const expr = this.normalizeQExpr(req.qexpr);
        const { value, state } = await this.evalExpr(expr, env, store);
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

      // Patch Set C: ReqMatch - structural AST matching
      case "ReqMatch": {
        const q = this.normalizeQExpr(req.qexpr);
        const p = this.normalizeQExpr(req.pattern);
        const { ok, bindings } = matchAST(p, q);
        return { tag: "RespObs", data: { ok, bindings } };
      }

      // Patch Set C: ReqAssert - evaluate predicate or check value
      case "ReqAssert": {
        const sev = req.severity ?? "error";
        let truth = false;

        if (typeof req.predicate === "string" ||
            (req.predicate && typeof req.predicate === "object" && "tag" in req.predicate && req.predicate.tag === "Text")) {
          // evaluate predicate in the interpreter
          const expr = this.normalizeQExpr(req.predicate as QExpr);
          const { env, store } = this.snapshots.getEnv(req.envRef);
          try {
            const { value } = await this.evalExpr(expr, env, store);
            truth = value.tag === "Bool" ? value.b : !!value;
          } catch (e: any) {
            return { tag: "RespError", message: `assertion eval failed: ${e?.message ?? e}` };
          }
        } else {
          const v = req.predicate as Val;
          truth = v?.tag === "Bool" ? !!v.b : !!v;
        }

        if (!truth) {
          const msg = `assertion failed: ${req.msg}`;
          if (sev === "warn") return { tag: "RespObs", data: { warning: msg } };
          return { tag: "RespError", message: msg };
        }
        return { tag: "RespAck" };
      }

      // Patch Set C: ReqSnapshot - create context receipt
      case "ReqSnapshot": {
        const r = this.ctxReceipts.snapshot(req.envRef, req.stateRef, req.meta);
        return { tag: "RespObs", data: r };
      }

      // Patch Set C: ReqCompress - create compress receipt
      case "ReqCompress": {
        const r = this.ctxReceipts.compress(req.envRef, req.meta);
        return { tag: "RespObs", data: r };
      }

      // Patch Set C: ReqHydrate - retrieve stored receipt
      case "ReqHydrate": {
        const r = this.ctxReceipts.get(req.receiptId);
        if (!r) return { tag: "RespError", message: `missing receipt ${req.receiptId}` };
        return { tag: "RespObs", data: { envRef: r.envRef, stateRef: r.stateRef } };
      }

      case "ReqTest": {
        return this.runTest(req);
      }

      case "ReqTool": {
        const registry = this.opts.toolRegistry;
        if (!registry) {
          return { tag: "RespError", message: "tool registry not configured" };
        }
        const result = await registry.execute(req.call);
        return { tag: "RespTool", result };
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

  private async evalExpr(expr: unknown, env: Env, store: Store): Promise<{ value: Val; state: State }> {
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
   * This is "extensional apply" for the oracle plane.
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
        };

      case "Stack": {
        const limit = spec.limit ?? 50;
        const frames = (st.kont ?? []).slice(0, limit).map((fr: any, i: number) => {
          const frameInfo: any = {
            index: i,
            tag: fr.tag ?? "Frame",
            hasEnv: fr.env !== undefined,
          };
          // Include envRef for frames that have environments
          if (fr.env !== undefined) {
            frameInfo.envRef = this.snapshots.putEnv({ env: fr.env, store: st.store });
          }
          // Include expression hint if available (for debugging)
          if (fr.e) {
            frameInfo.exprTag = fr.e.tag;
          }
          return frameInfo;
        });
        return { depth: (st.kont ?? []).length, frames };
      }

      case "FrameEnv": {
        const fr: any = (st.kont ?? [])[spec.frameIndex];
        if (!fr) return { error: `no such frame ${spec.frameIndex}` };
        if (!fr.env) return { error: `frame ${spec.frameIndex} has no env` };

        const envRef = this.snapshots.putEnv({ env: fr.env, store: st.store });
        return { envRef };
      }

      case "Env": {
        // List all bindings in the environment
        const { env, store } = spec.envRef
          ? this.snapshots.getEnv(spec.envRef)
          : { env: st.env, store: st.store };

        const bindings: Array<{ name: string; addr: number; value?: unknown }> = [];
        const seen = new Set<string>();

        // Walk the env chain collecting all bindings
        for (let cur: Env | undefined = env; cur && (cur as any).tag === "Ctx"; cur = (cur as any).parent) {
          const frame = (cur as any).frame as Map<string, number>;
          for (const [name, addr] of frame) {
            if (!seen.has(name)) {
              seen.add(name);
              try {
                const val = store.read(addr);
                bindings.push({
                  name,
                  addr,
                  value: val ? { tag: val.tag } : undefined
                });
              } catch {
                // addr not in store (stale reference)
                bindings.push({ name, addr, value: undefined });
              }
            }
          }
        }

        return { bindings, count: bindings.length };
      }

      case "EnvLookup": {
        // Lookup a specific binding by name
        const { env, store } = spec.envRef
          ? this.snapshots.getEnv(spec.envRef)
          : { env: st.env, store: st.store };

        const addr = envGet(env, spec.name);
        if (addr === undefined) {
          return { found: false, name: spec.name };
        }

        let value: Val | null = null;
        try {
          value = store.read(addr);
        } catch {
          // addr not in store
        }
        return {
          found: true,
          name: spec.name,
          addr,
          value
        };
      }

      case "Defs": {
        // List top-level definitions (user-defined, excluding primitives)
        const { env, store } = spec.envRef
          ? this.snapshots.getEnv(spec.envRef)
          : { env: st.env, store: st.store };

        const defs: Array<{ name: string; type: string }> = [];
        const seen = new Set<string>();

        // Walk the env chain, collect non-Native values (user definitions)
        for (let cur: Env | undefined = env; cur && (cur as any).tag === "Ctx"; cur = (cur as any).parent) {
          const frame = (cur as any).frame as Map<string, number>;
          for (const [name, addr] of frame) {
            if (!seen.has(name)) {
              seen.add(name);
              try {
                const val = store.read(addr);
                // Only include non-Native values (user-defined)
                if (val && val.tag !== "Native") {
                  defs.push({ name, type: val.tag });
                }
              } catch {
                // addr not in store (stale reference)
              }
            }
          }
        }

        return { defs, count: defs.length };
      }

      default:
        return { error: `unknown ObserveSpec ${(spec as any).tag}` };
    }
  }

  private async runTest(req: { tag: "ReqTest"; spec: TestSpec }): Promise<OracleResp> {
    const spec = req.spec;
    const caps = this.opts.caps ?? ["*"];

    // Capability check: require "test" capability for all test operations
    try {
      capRequire(caps, "test", "ReqTest");
    } catch (e: any) {
      return { tag: "RespError", message: e.message };
    }

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

    // Tests: batch test execution - each expr should return truthy
    if (spec.tag === "Tests") {
      const { env, store } = this.snapshots.getEnv(spec.envRef);
      const results: Array<{ index: number; passed: boolean; error?: string }> = [];
      let allPassed = true;

      for (let i = 0; i < spec.tests.length; i++) {
        const qexpr = spec.tests[i];
        try {
          const expr = this.normalizeQExpr(qexpr);
          const { value } = await this.evalExpr(expr, env, store);
          // Check if truthy (Bool true, or non-false/non-unit value)
          const passed = value.tag === "Bool" ? value.b : (value.tag !== "Unit");
          results.push({ index: i, passed });
          if (!passed) allPassed = false;
        } catch (e: any) {
          results.push({ index: i, passed: false, error: String(e?.message ?? e) });
          allPassed = false;
        }
      }

      return {
        tag: "RespTest",
        passed: allPassed,
        report: {
          tag: "BatchTests",
          total: spec.tests.length,
          passed: results.filter(r => r.passed).length,
          failed: results.filter(r => !r.passed).length,
          results
        }
      };
    }

    // TestSuite: named test suite with individual test names
    if (spec.tag === "TestSuite") {
      const { env, store } = this.snapshots.getEnv(spec.envRef);
      const results: Array<{ name: string; passed: boolean; error?: string }> = [];
      let allPassed = true;

      for (const test of spec.tests) {
        try {
          const expr = this.normalizeQExpr(test.qexpr);
          const { value } = await this.evalExpr(expr, env, store);
          const passed = value.tag === "Bool" ? value.b : (value.tag !== "Unit");
          results.push({ name: test.name, passed });
          if (!passed) allPassed = false;
        } catch (e: any) {
          results.push({ name: test.name, passed: false, error: String(e?.message ?? e) });
          allPassed = false;
        }
      }

      return {
        tag: "RespTest",
        passed: allPassed,
        report: {
          tag: "TestSuite",
          suiteName: spec.name,
          total: spec.tests.length,
          passed: results.filter(r => r.passed).length,
          failed: results.filter(r => !r.passed).length,
          results
        }
      };
    }

    return { tag: "RespError", message: "unknown TestSpec", details: spec };
  }
}
