// test/live/openai-live.spec.ts
// Live integration test against real OpenAI API with REAL PortalImpl
// NO MOCKS - all eval/observe requests handled by real interpreter

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
import { VUnit } from "../../src/core/eval/values";
import "../../src/core/oracle/plugins";
import { ModelSelectorAdapter } from "../../src/core/oracle/plugins";
import type { OracleResp, OracleReq } from "../../src/core/oracle/protocol";
import * as fs from "fs";
import * as path from "path";

function loadApiKey(): string | undefined {
  try {
    const configPath = path.join(__dirname, "../../../LambdaRLM/config.yaml");
    const content = fs.readFileSync(configPath, "utf8");
    const match = content.match(/api_key:\s*(\S+)/);
    return match?.[1];
  } catch {
    return process.env.OPENAI_API_KEY;
  }
}

const OPENAI_API_KEY = loadApiKey();
const hasKey = !!OPENAI_API_KEY;

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

  const portal = new PortalImpl(runtime, snapshots, receipts, {
    maxEvalSteps: 100_000,
    parseText: (src: string) => compileTextToExpr(src),
  });

  return { portal, snapshots, envRef, stateRef, value, state, runtime };
}

describe("OpenAI Live Integration", () => {
  it.runIf(hasKey)("makes real API call to OpenAI - simple query", async () => {
    // Simple test - LLM just answers a question, no tool use needed
    const selector = new ModelSelectorAdapter({
      defaultModel: "gpt-4o-mini",
      defaultPlugin: "openai",
      sharedConfig: {
        apiKey: OPENAI_API_KEY,
        maxTokens: 100,
      },
    });

    const init = {
      tag: "Infer" as const,
      payload: { tag: "Str" as const, s: "What is 2 + 2? Reply with just the number." },
      envRef: "test-env" as any,
      stateRef: "test-state" as any,
    };

    const session = selector.startSession(init);
    let result: any;
    let resp: OracleResp = { tag: "RespAck" };

    for (let i = 0; i < 5; i++) {
      const step = await session.next(resp);
      if (step.done) {
        result = step.value;
        break;
      }
      // For simple queries, LLM shouldn't need tools
      resp = { tag: "RespAck" };
    }

    console.log("OpenAI result:", result);

    expect(result).toBeDefined();
    expect(result.tag).toBe("Meaning");

    // The answer should contain "4" somewhere
    const denotation = result.denotation;
    if (denotation?.tag === "Str") {
      expect(denotation.s).toMatch(/4/);
    }
  }, 30000);

  it.runIf(hasKey)("handles multi-turn with REAL eval", async () => {
    // Setup real runtime with primitives
    const { portal, envRef, stateRef } = await setupRealRuntime("1");

    let currentEnvRef = envRef;
    let currentStateRef = stateRef;

    const selector = new ModelSelectorAdapter({
      defaultModel: "gpt-4o-mini",
      defaultPlugin: "openai",
      sharedConfig: {
        apiKey: OPENAI_API_KEY,
        maxTokens: 200,
      },
    });

    const init = {
      tag: "Infer" as const,
      payload: {
        tag: "Str" as const,
        s: "Use omega_eval to compute (+ 10 20), then return the result."
      },
      envRef: currentEnvRef,
      stateRef: currentStateRef,
    };

    const session = selector.startSession(init);
    let result: any;
    let resp: OracleResp = { tag: "RespAck" };
    let turnCount = 0;
    let usedRealEval = false;

    while (turnCount < 10) {
      turnCount++;
      const step = await session.next(resp);

      if (step.done) {
        result = step.value;
        break;
      }

      const req = step.value as OracleReq;
      console.log(`Turn ${turnCount}: ${req.tag}`);

      // Use REAL PortalImpl for all requests
      if (req.tag === "ReqEval") {
        usedRealEval = true;
        console.log(`  Real eval: ${JSON.stringify((req as any).qexpr)}`);
        resp = await portal.perform({
          tag: "ReqEval",
          qexpr: (req as any).qexpr,
          envRef: currentEnvRef,
        });
        if ((resp as any).envRef) currentEnvRef = (resp as any).envRef;
        if ((resp as any).stateRef) currentStateRef = (resp as any).stateRef;
        console.log(`  Real result:`, (resp as any).value);
      } else if (req.tag === "ReqObserve") {
        resp = await portal.perform({
          tag: "ReqObserve",
          what: (req as any).what,
          stateRef: currentStateRef,
        });
      } else {
        resp = { tag: "RespAck" };
      }
    }

    console.log(`Completed in ${turnCount} turns, usedRealEval=${usedRealEval}`);
    console.log("Result:", result);

    expect(result).toBeDefined();
    expect(result.tag).toBe("Meaning");
    expect(usedRealEval).toBe(true);
  }, 60000);

  it.runIf(hasKey)("LLM uses omega_observe with REAL definitions", async () => {
    // Setup runtime with a pre-defined function
    const { portal, envRef, stateRef } = await setupRealRuntime(`
      (define (double x) (* x 2))
      (define my-value 42)
      1
    `);

    let currentEnvRef = envRef;
    let currentStateRef = stateRef;

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
    let observedDefs = false;
    let realDefsData: any = null;

    while (turnCount < 15) {
      turnCount++;
      const step = await session.next(resp);

      if (step.done) {
        result = step.value;
        break;
      }

      const req = step.value as OracleReq;
      console.log(`Turn ${turnCount}: ${req.tag}`);

      // Use REAL PortalImpl for all requests
      if (req.tag === "ReqEval") {
        resp = await portal.perform({
          tag: "ReqEval",
          qexpr: (req as any).qexpr,
          envRef: currentEnvRef,
        });
        if ((resp as any).envRef) currentEnvRef = (resp as any).envRef;
        if ((resp as any).stateRef) currentStateRef = (resp as any).stateRef;
      } else if (req.tag === "ReqObserve") {
        const what = (req as any).what;
        console.log(`  Real observe: ${what.tag}`);

        if (what.tag === "Defs") {
          observedDefs = true;
        }

        // Use PortalImpl to handle this with REAL data
        resp = await portal.perform({
          tag: "ReqObserve",
          what,
          stateRef: currentStateRef,
        });

        if (what.tag === "Defs") {
          realDefsData = (resp as any).data;
          console.log(`  Real defs response:`, JSON.stringify(realDefsData, null, 2));
        }
      } else {
        resp = { tag: "RespAck" };
      }
    }

    console.log(`Completed in ${turnCount} turns, observedDefs=${observedDefs}`);
    console.log("Result:", result);

    expect(result).toBeDefined();
    expect(result.tag).toBe("Meaning");
    expect(observedDefs).toBe(true);

    // Verify real data was returned
    expect(realDefsData).not.toBeNull();
    expect(realDefsData.count).toBeGreaterThanOrEqual(2); // double and my-value
  }, 60000);

  it.runIf(hasKey)("LLM can eval multiple expressions", async () => {
    // Test multiple real evals in sequence
    const { portal, envRef, stateRef } = await setupRealRuntime("1");

    let currentEnvRef = envRef;
    let currentStateRef = stateRef;

    const selector = new ModelSelectorAdapter({
      defaultModel: "gpt-4o-mini",
      defaultPlugin: "openai",
      sharedConfig: {
        apiKey: OPENAI_API_KEY,
        maxTokens: 400,
      },
    });

    const init = {
      tag: "Infer" as const,
      payload: {
        tag: "Str" as const,
        s: "Use omega_eval to compute (* 7 8), then use omega_eval to compute (+ 100 (* 7 8)). Return the final result."
      },
      envRef: currentEnvRef,
      stateRef: currentStateRef,
    };

    const session = selector.startSession(init);
    let result: any;
    let resp: OracleResp = { tag: "RespAck" };
    let turnCount = 0;
    let evalCount = 0;

    while (turnCount < 20) {
      turnCount++;
      const step = await session.next(resp);

      if (step.done) {
        result = step.value;
        break;
      }

      const req = step.value as OracleReq;
      console.log(`Turn ${turnCount}: ${req.tag}`);

      if (req.tag === "ReqEval") {
        const qexpr = (req as any).qexpr;
        console.log(`  Real eval: ${typeof qexpr === 'string' ? qexpr : JSON.stringify(qexpr)}`);
        evalCount++;

        resp = await portal.perform({
          tag: "ReqEval",
          qexpr,
          envRef: currentEnvRef,
        });

        if ((resp as any).envRef) currentEnvRef = (resp as any).envRef;
        if ((resp as any).stateRef) currentStateRef = (resp as any).stateRef;
        console.log(`  Result:`, (resp as any).value);
      } else if (req.tag === "ReqObserve") {
        resp = await portal.perform({
          tag: "ReqObserve",
          what: (req as any).what,
          stateRef: currentStateRef,
        });
      } else {
        resp = { tag: "RespAck" };
      }
    }

    console.log(`Completed in ${turnCount} turns, evalCount=${evalCount}`);
    console.log("Result:", result);

    expect(result).toBeDefined();
    expect(result.tag).toBe("Meaning");
    expect(evalCount).toBeGreaterThanOrEqual(1);
  }, 90000);
});
