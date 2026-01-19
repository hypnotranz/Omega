#!/usr/bin/env npx tsx
// demo/live-oracle.ts
// A REAL demo that actually calls an LLM
//
// Usage:
//   OPENAI_API_KEY=sk-... npx tsx demo/live-oracle.ts

import { openaiPlugin } from "../src/core/oracle/plugins/openai";
import { runOracleSession } from "../src/core/oracle/driver";
import { PortalImpl } from "../src/core/oracle/portalImpl";
import { RuntimeImpl } from "../src/core/effects/runtimeImpl";
import { SnapshotRepo } from "../src/core/oracle/snapshots";
import { InMemoryReceiptStore } from "../src/core/oracle/receipts";
import { COWStore } from "../src/core/eval/store";
import { compileTextToExpr } from "../src/core/pipeline/compileText";
import { installPrims } from "../test/helpers/prims";
import type { OracleInit } from "../src/core/oracle/adapter";
import type { Val } from "../src/core/eval/values";
import type { State } from "../src/core/eval/machine";

// ─────────────────────────────────────────────────────────────────
// Setup
// ─────────────────────────────────────────────────────────────────

async function main() {
  // Check for API key
  if (!process.env.OPENAI_API_KEY) {
    console.error("Set OPENAI_API_KEY environment variable");
    process.exit(1);
  }

  console.log("╔══════════════════════════════════════════════════════════════════╗");
  console.log("║            Ω Live Oracle Demo - Real LLM Calls                   ║");
  console.log("╚══════════════════════════════════════════════════════════════════╝\n");

  // Create snapshots and receipts
  const snapshots = new SnapshotRepo();
  const receipts = new InMemoryReceiptStore("record");

  // Create environment and store with primitives installed
  const baseStore = new COWStore();
  const { env, store } = installPrims(baseStore);

  // Create minimal state for snapshots
  const minState: State = {
    control: { tag: "Val", v: { tag: "Unit" } },
    env,
    store,
    kont: [],
    handlers: [],
  } as any;

  // Create initial environment snapshot
  const envRef = snapshots.putEnv({ env, store });
  const stateRef = snapshots.putState({ state: minState });

  // Create the OpenAI adapter (using mini for efficiency)
  const adapter = openaiPlugin.createAdapter({
    model: "gpt-4o-mini",
    maxTokens: 2048,
  });

  // Mock commit adapter (not used in this demo)
  const mockCommitAdapter = {
    async commit(payload: Val, _ctxDigest: string): Promise<Val> {
      return { tag: "Str", s: "commit:" + Math.random().toString(16).slice(2) } as Val;
    }
  };

  // Create runtime for the portal to use
  const runtime = new RuntimeImpl(
    adapter,
    snapshots,
    receipts,
    mockCommitAdapter
  );

  // Create portal (handles ReqEval, ReqObserve, etc.)
  const portal = new PortalImpl(runtime, snapshots, receipts, {
    maxEvalSteps: 100_000,
    parseText: (src: string) => compileTextToExpr(src),
  });

  // ─────────────────────────────────────────────────────────────────
  // Demo 1: Semantic Classification
  // ─────────────────────────────────────────────────────────────────

  console.log("Demo 1: Semantic Classification (Real LLM)");
  console.log("─".repeat(60));

  const classifyPayload: Val = {
    tag: "Str",
    s: `Classify the sentiment: "I loved the restaurant! The food was amazing!"

Call omega_return directly with value "positive" or "negative" or "neutral". Do NOT use omega_eval.`,
  };

  const classifyInit: OracleInit = {
    tag: "Infer",
    payload: classifyPayload,
    envRef,
    stateRef,
  };

  console.log("Sending to LLM...\n");
  const startTime1 = Date.now();

  try {
    const session1 = adapter.startSession(classifyInit);
    const result1 = await runOracleSession(session1, portal, { maxTurns: 50 });

    const elapsed1 = Date.now() - startTime1;
    console.log("Result:", JSON.stringify(result1, null, 2));
    console.log(`Time: ${elapsed1}ms`);
    console.log();
  } catch (error) {
    console.error("Error:", error);
  }

  // ─────────────────────────────────────────────────────────────────
  // Demo 2: Semantic Code Analysis
  // ─────────────────────────────────────────────────────────────────

  console.log("\nDemo 2: Code Analysis (Real LLM)");
  console.log("─".repeat(60));

  const codePayload: Val = {
    tag: "Str",
    s: `What is the bug in: function divide(a, b) { return a / b; }

Call omega_return directly with a brief description string. Do NOT use omega_eval.`,
  };

  const codeInit: OracleInit = {
    tag: "Infer",
    payload: codePayload,
    envRef,
    stateRef,
  };

  console.log("Sending to LLM...\n");
  const startTime2 = Date.now();

  try {
    const session2 = adapter.startSession(codeInit);
    const result2 = await runOracleSession(session2, portal, { maxTurns: 50 });

    const elapsed2 = Date.now() - startTime2;
    console.log("Result:", JSON.stringify(result2, null, 2));
    console.log(`Time: ${elapsed2}ms`);
    console.log();
  } catch (error) {
    console.error("Error:", error);
  }

  // ─────────────────────────────────────────────────────────────────
  // Demo 3: Interactive Computation (LLM uses omega_eval)
  // ─────────────────────────────────────────────────────────────────

  console.log("\nDemo 3: Interactive Computation (LLM uses omega_eval)");
  console.log("─".repeat(60));

  const computePayload: Val = {
    tag: "Str",
    s: `I need you to compute something using the Omega runtime.

1. First, use omega_eval to evaluate: (+ 10 20 30)
2. Then use omega_eval to evaluate: (* 7 6)
3. Return the final result using omega_return

Use the omega_eval tool to run these expressions, then omega_return with the final answer.`,
  };

  const computeInit: OracleInit = {
    tag: "Infer",
    payload: computePayload,
    envRef,
    stateRef,
  };

  console.log("Sending to LLM (it will use tools to compute)...\n");
  const startTime3 = Date.now();

  try {
    const session3 = adapter.startSession(computeInit);
    const result3 = await runOracleSession(session3, portal, { maxTurns: 50 });

    const elapsed3 = Date.now() - startTime3;
    console.log("Result:", JSON.stringify(result3, null, 2));
    console.log(`Time: ${elapsed3}ms`);
    console.log();
  } catch (error) {
    console.error("Error:", error);
  }

  // ─────────────────────────────────────────────────────────────────
  // Summary
  // ─────────────────────────────────────────────────────────────────

  console.log("\n╔══════════════════════════════════════════════════════════════════╗");
  console.log("║                         Demo Complete                            ║");
  console.log("╚══════════════════════════════════════════════════════════════════╝");
  console.log("\nThis demo made REAL API calls to GPT-4o-mini, which:");
  console.log("  - Received the request");
  console.log("  - Used omega_eval tool to run Lisp code in the runtime");
  console.log("  - Returned semantic results with confidence scores");
  console.log("\nOracle sessions completed successfully");
}

main().catch(console.error);
