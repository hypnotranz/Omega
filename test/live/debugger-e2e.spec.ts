// test/live/debugger-e2e.spec.ts
// End-to-end test: Real LLM using real debugger via PortalImpl
//
// Run with: RUN_LIVE_TESTS=true npm test

import { describe, it, expect } from "vitest";
import { COWStore } from "../../src/core/eval/store";
import { RuntimeImpl } from "../../src/core/effects/runtimeImpl";
import { PortalImpl } from "../../src/core/oracle/portalImpl";
import { SnapshotRepo } from "../../src/core/oracle/snapshots";
import { InMemoryReceiptStore } from "../../src/core/oracle/receipts";
import { ScriptedOracleAdapter } from "../../src/core/oracle/scriptedOracle";
import { installPrims } from "../helpers/prims";
import { compileTextToExpr } from "../../src/core/pipeline/compileText";
import { runToCompletionWithState } from "../../src/core/eval/run";
import type { State } from "../../src/core/eval/machine";
import type { Val } from "../../src/core/eval/values";
import { VUnit } from "../../src/core/eval/values";
import "../../src/core/oracle/plugins";
import { ModelSelectorAdapter } from "../../src/core/oracle/plugins";
import type { OracleResp, OracleReq, EnvRef, StateRef } from "../../src/core/oracle/protocol";
import { runLive, OPENAI_API_KEY } from "./config";

// Setup real runtime and portal with pre-evaluated state
async function setupRealRuntime(src: string) {
  const oracle = new ScriptedOracleAdapter();
  const snapshots = new SnapshotRepo();
  const receipts = new InMemoryReceiptStore("off");

  const runtime = new RuntimeImpl(oracle, snapshots, receipts, {
    async commit() { return VUnit; }
  });

  const store0 = new COWStore();
  const prim = installPrims(store0);

  // Compile and evaluate the source code
  const expr = compileTextToExpr(`(begin ${src})`);
  const state0: State = {
    control: { tag: "Expr", e: expr },
    env: prim.env,
    store: prim.store,
    kont: [],
    handlers: [],
  };

  const { value, state } = await runToCompletionWithState(runtime, state0, 100_000);

  const envRef = snapshots.putEnv({ env: state.env, store: state.store });
  const stateRef = snapshots.putState({ state });

  const portal = new PortalImpl(runtime, snapshots, receipts, { maxEvalSteps: 100_000 });

  return { portal, snapshots, envRef, stateRef, value, state };
}

describe("Debugger End-to-End with Real LLM", () => {
  it.runIf(runLive)("PortalImpl returns REAL debugger data", async () => {
    // Setup with pre-defined values
    const { portal, stateRef } = await setupRealRuntime(`
      (define my-value 42)
      (define (my-func x) (* x 2))
      my-value
    `);

    // Test the debugger - observe Defs
    const defsResp = await portal.perform({
      tag: "ReqObserve",
      what: { tag: "Defs" },
      stateRef,
    });

    console.log("Defs response:", JSON.stringify(defsResp, null, 2));

    expect(defsResp.tag).toBe("RespObs");
    const defsData = (defsResp as any).data;
    expect(defsData).toHaveProperty("defs");
    expect(defsData).toHaveProperty("count");

    // Should have at least 2 definitions (my-value and my-func)
    console.log("Definitions found:", defsData.defs);
    expect(defsData.count).toBeGreaterThanOrEqual(2);

    // Verify the types
    const types = defsData.defs.map((d: any) => d.type);
    expect(types).toContain("Num");      // my-value = 42
    expect(types).toContain("Closure");  // my-func = lambda

    // Test EnvLookup for a primitive
    const lookupResp = await portal.perform({
      tag: "ReqObserve",
      what: { tag: "EnvLookup", name: "+" },
      stateRef,
    });

    console.log("Lookup + response:", JSON.stringify(lookupResp, null, 2));

    expect(lookupResp.tag).toBe("RespObs");
    const lookupData = (lookupResp as any).data;
    expect(lookupData.found).toBe(true);
    expect(lookupData.value.tag).toBe("Native");

    // Test Env (list all bindings)
    const envResp = await portal.perform({
      tag: "ReqObserve",
      what: { tag: "Env" },
      stateRef,
    });

    console.log("Env response count:", (envResp as any).data?.count);

    expect(envResp.tag).toBe("RespObs");
    const envData = (envResp as any).data;
    expect(envData.count).toBeGreaterThan(10); // Primitives + user defs
  }, 30000);

  it.runIf(runLive)("Real LLM calls omega_observe, PortalImpl returns REAL data", async () => {
    // Setup with a test variable
    const { portal, envRef, stateRef } = await setupRealRuntime(`
      (define test-var 123)
      test-var
    `);

    let currentEnvRef = envRef;
    let currentStateRef = stateRef;

    // Create LLM adapter
    const selector = new ModelSelectorAdapter({
      defaultModel: "gpt-4o-mini",
      defaultPlugin: "openai",
      sharedConfig: {
        apiKey: OPENAI_API_KEY,
        maxTokens: 300,
      },
    });

    const init = {
      tag: "Infer" as const,
      payload: {
        tag: "Str" as const,
        s: "Use omega_observe with what='defs' to list the user definitions. Report how many you found and what types they are."
      },
      envRef: currentEnvRef,
      stateRef: currentStateRef,
    };

    const session = selector.startSession(init);
    let result: any;
    let resp: OracleResp = { tag: "RespAck" };
    let turnCount = 0;
    let usedRealDebugger = false;

    while (turnCount < 10) {
      turnCount++;
      const step = await session.next(resp);

      if (step.done) {
        result = step.value;
        break;
      }

      const req = step.value as OracleReq;
      console.log(`Turn ${turnCount}: ${req.tag}`);

      // Use REAL PortalImpl for all requests!
      if (req.tag === "ReqObserve") {
        const what = (req as any).what;
        console.log(`  Real observe: ${what.tag}`);

        if (what.tag === "Defs" || what.tag === "Env" || what.tag === "EnvLookup") {
          usedRealDebugger = true;
        }

        // Use PortalImpl to handle this!
        resp = await portal.perform({
          tag: "ReqObserve",
          what,
          stateRef: currentStateRef,
        });

        console.log(`  Real response:`, JSON.stringify((resp as any).data, null, 2));
      } else if (req.tag === "ReqEval") {
        // Handle eval through portal too
        resp = await portal.perform({
          tag: "ReqEval",
          qexpr: (req as any).qexpr,
          envRef: currentEnvRef,
        });
        if ((resp as any).envRef) currentEnvRef = (resp as any).envRef;
        if ((resp as any).stateRef) currentStateRef = (resp as any).stateRef;
      } else {
        resp = { tag: "RespAck" };
      }
    }

    console.log(`Completed in ${turnCount} turns`);
    console.log("Used real debugger:", usedRealDebugger);
    console.log("Result:", result);

    expect(result).toBeDefined();
    expect(result.tag).toBe("Meaning");
    expect(usedRealDebugger).toBe(true);
  }, 60000);
});
