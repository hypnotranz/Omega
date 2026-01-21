// test/live/plugins.integration.spec.ts
// LIVE TESTS - Makes real API calls to LLM providers
// Integration tests for real LLM plugins with REAL PortalImpl
// NO MOCKS - all eval/observe requests handled by real interpreter
//
// Run with: RUN_LIVE_TESTS=true npx vitest run test/live/

import { describe, it, expect, beforeAll } from "vitest";
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
import {
  registry,
  createAdapterFromModel,
  createModelSelector,
} from "../../src/core/oracle/plugins";
import type { OracleAdapter } from "../../src/core/oracle/adapter";
import type { OracleResp, OracleReq, EnvRef, StateRef } from "../../src/core/oracle/protocol";
import type { MeaningVal } from "../../src/core/oracle/meaning";

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

// Run an oracle session with REAL PortalImpl handling requests
async function runRealInference(
  adapter: OracleAdapter,
  query: string,
  portal: PortalImpl,
  initialEnvRef: EnvRef,
  initialStateRef: StateRef
): Promise<{ meaning: MeaningVal; turns: number; usedRealEval: boolean; usedRealObserve: boolean }> {
  const session = adapter.startSession({
    tag: "Infer",
    payload: { tag: "Str", s: query },
    envRef: initialEnvRef,
    stateRef: initialStateRef,
  });

  let turns = 0;
  let resp: OracleResp = { tag: "RespAck" };
  let currentEnvRef = initialEnvRef;
  let currentStateRef = initialStateRef;
  let usedRealEval = false;
  let usedRealObserve = false;

  while (true) {
    turns++;
    const step = await session.next(resp);

    if (step.done) {
      return { meaning: step.value as MeaningVal, turns, usedRealEval, usedRealObserve };
    }

    // Use REAL PortalImpl for all requests
    const req = step.value as OracleReq;

    if (req.tag === "ReqEval") {
      usedRealEval = true;
      resp = await portal.perform({
        tag: "ReqEval",
        qexpr: (req as any).qexpr,
        envRef: currentEnvRef,
      });
      if ((resp as any).envRef) currentEnvRef = (resp as any).envRef;
      if ((resp as any).stateRef) currentStateRef = (resp as any).stateRef;
    } else if (req.tag === "ReqObserve") {
      usedRealObserve = true;
      resp = await portal.perform({
        tag: "ReqObserve",
        what: (req as any).what,
        stateRef: currentStateRef,
      });
    } else {
      resp = { tag: "RespAck" };
    }

    // Safety limit
    if (turns > 25) {
      throw new Error("Too many turns");
    }
  }
}

// ANTHROPIC INTEGRATION TESTS
describe("Anthropic Integration", () => {
  const hasApiKey = !!process.env.ANTHROPIC_API_KEY;

  beforeAll(() => {
    if (!hasApiKey) {
      console.log("  ANTHROPIC_API_KEY not set - skipping real API tests");
    }
  });

  it.skipIf(!hasApiKey)("creates adapter and validates config", () => {
    const plugin = registry.get("anthropic");
    expect(plugin).toBeDefined();

    const validation = plugin!.validateConfig({});
    expect(validation.valid).toBe(true);
  });

  it.skipIf(!hasApiKey)("health check passes", async () => {
    const plugin = registry.get("anthropic");
    const health = await plugin!.healthCheck?.();

    expect(health?.ok).toBe(true);
    expect(health?.latencyMs).toBeGreaterThan(0);
  }, 30000);

  it.skipIf(!hasApiKey)("runs inference with REAL eval", async () => {
    const { portal, envRef, stateRef } = await setupRealRuntime("1");
    const adapter = createAdapterFromModel("anthropic:claude-3-haiku-20240307");

    const { meaning, turns, usedRealEval } = await runRealInference(
      adapter,
      "Use omega_eval to compute (+ 5 7) and return the result.",
      portal,
      envRef,
      stateRef
    );

    expect(meaning.tag).toBe("Meaning");
    expect(meaning.confidence).toBeGreaterThan(0);
    expect(turns).toBeGreaterThanOrEqual(1);
    // LLM should use real eval
    console.log(`Anthropic: ${turns} turns, usedRealEval=${usedRealEval}`);
  }, 60000);
});

// OPENAI INTEGRATION TESTS
describe("OpenAI Integration", () => {
  const hasApiKey = !!process.env.OPENAI_API_KEY;

  beforeAll(() => {
    if (!hasApiKey) {
      console.log("  OPENAI_API_KEY not set - skipping real API tests");
    }
  });

  it.skipIf(!hasApiKey)("creates adapter and validates config", () => {
    const plugin = registry.get("openai");
    expect(plugin).toBeDefined();

    const validation = plugin!.validateConfig({});
    expect(validation.valid).toBe(true);
  });

  it.skipIf(!hasApiKey)("health check passes", async () => {
    const plugin = registry.get("openai");
    const health = await plugin!.healthCheck?.();

    expect(health?.ok).toBe(true);
  }, 30000);

  it.skipIf(!hasApiKey)("runs inference with REAL eval", async () => {
    const { portal, envRef, stateRef } = await setupRealRuntime("1");
    const adapter = createAdapterFromModel("openai:gpt-4o-mini");

    const { meaning, turns, usedRealEval } = await runRealInference(
      adapter,
      "Use omega_eval to compute (* 6 7) and return the result.",
      portal,
      envRef,
      stateRef
    );

    expect(meaning.tag).toBe("Meaning");
    expect(meaning.confidence).toBeGreaterThan(0);
    console.log(`OpenAI: ${turns} turns, usedRealEval=${usedRealEval}`);
  }, 60000);

  it.skipIf(!hasApiKey)("runs observe with REAL definitions", async () => {
    const { portal, envRef, stateRef } = await setupRealRuntime(`
      (define test-fn (lambda (x) (+ x 1)))
      (define test-val 100)
      1
    `);
    const adapter = createAdapterFromModel("openai:gpt-4o-mini");

    const { meaning, turns, usedRealObserve } = await runRealInference(
      adapter,
      "Use omega_observe with what='defs' to list definitions. Report how many you found.",
      portal,
      envRef,
      stateRef
    );

    expect(meaning.tag).toBe("Meaning");
    expect(usedRealObserve).toBe(true);
    console.log(`OpenAI observe: ${turns} turns, usedRealObserve=${usedRealObserve}`);
  }, 60000);
});

// OLLAMA INTEGRATION TESTS
describe("Ollama Integration", () => {
  let isOllamaRunning = false;

  beforeAll(async () => {
    try {
      const response = await fetch("http://localhost:11434/api/tags");
      isOllamaRunning = response.ok;
    } catch {
      isOllamaRunning = false;
    }

    if (!isOllamaRunning) {
      console.log("  Ollama not running - skipping Ollama tests");
    }
  });

  it.skipIf(!isOllamaRunning)("health check passes", async () => {
    const plugin = registry.get("ollama");
    const health = await plugin!.healthCheck?.();

    expect(health?.ok).toBe(true);
  }, 10000);

  it.skipIf(!isOllamaRunning)("runs inference with REAL eval", async () => {
    const { portal, envRef, stateRef } = await setupRealRuntime("1");
    const adapter = createAdapterFromModel("ollama:llama3.2:1b", {
      baseUrl: "http://localhost:11434",
    });

    const { meaning, turns } = await runRealInference(
      adapter,
      "What is 2 + 2? Just say the number.",
      portal,
      envRef,
      stateRef
    );

    expect(meaning.tag).toBe("Meaning");
    console.log(`Ollama: ${turns} turns`);
  }, 120000);
});

// MODEL SELECTOR INTEGRATION
describe("ModelSelector Integration", () => {
  const hasAnyKey = !!(process.env.ANTHROPIC_API_KEY || process.env.OPENAI_API_KEY);

  it.skipIf(!hasAnyKey)("auto-detects available plugin", () => {
    const selector = createModelSelector();
    const config = selector.getConfig();

    expect(config.defaultModel).toBeDefined();
    expect(registry.has(config.defaultPlugin!)).toBe(true);
  });

  it.skipIf(!hasAnyKey)("routes to correct adapter with REAL portal", async () => {
    const { portal, envRef, stateRef } = await setupRealRuntime("1");
    const selector = createModelSelector();

    const session = selector.startSession({
      tag: "Infer",
      payload: { tag: "Str", s: "What is 3 + 4? Just answer with the number." },
      envRef,
      stateRef,
    });

    let currentEnvRef = envRef;
    let currentStateRef = stateRef;
    let resp: OracleResp = { tag: "RespAck" };
    let turns = 0;

    while (true) {
      turns++;
      const step = await session.next(resp);

      if (step.done) {
        expect(step.value?.tag).toBe("Meaning");
        break;
      }

      const req = step.value as OracleReq;

      // Use REAL PortalImpl
      if (req.tag === "ReqEval") {
        resp = await portal.perform({
          tag: "ReqEval",
          qexpr: (req as any).qexpr,
          envRef: currentEnvRef,
        });
        if ((resp as any).envRef) currentEnvRef = (resp as any).envRef;
        if ((resp as any).stateRef) currentStateRef = (resp as any).stateRef;
      } else if (req.tag === "ReqObserve") {
        resp = await portal.perform({
          tag: "ReqObserve",
          what: (req as any).what,
          stateRef: currentStateRef,
        });
      } else {
        resp = { tag: "RespAck" };
      }

      if (turns > 25) throw new Error("Too many turns");
    }

    console.log(`ModelSelector: completed in ${turns} turns`);
  }, 60000);
});
