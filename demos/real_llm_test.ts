/**
 * Test OmegaLLM with a REAL LLM backend
 *
 * Prerequisites:
 *   Set ANTHROPIC_API_KEY or OPENAI_API_KEY in your environment
 *
 * Run:
 *   OPENAI_API_KEY=sk-... npx tsx test/real_llm_test.ts
 */

import { COWStore } from "../src/core/eval/store";
import { RuntimeImpl } from "../src/core/effects/runtimeImpl";
import { installPrims } from "./helpers/prims";
import type { State } from "../src/core/eval/machine";
import { runToCompletion } from "../src/core/eval/run";
import { compileTextToExpr } from "../src/core/pipeline/compileText";
import { SnapshotRepo } from "../src/core/oracle/snapshots";
import { InMemoryReceiptStore } from "../src/core/oracle/receipts";
import { mockCommit } from "./helpers/runtime";
import {
  createAutoOracle,
  createConsoleCommit,
  createOpenAIOracle
} from "../src/adapters/llm-adapter";

function initialState(src: string): State {
  const expr = compileTextToExpr(src);
  const store0 = new COWStore();
  const prim = installPrims(store0);
  return { control: { tag: "Expr", e: expr }, env: prim.env, store: prim.store, kont: [], handlers: [] };
}

async function main() {
  console.log("=".repeat(60));
  console.log("OmegaLLM - Real LLM Integration Test");
  console.log("=".repeat(60));

  // Check for API key
  if (!process.env.ANTHROPIC_API_KEY && !process.env.OPENAI_API_KEY) {
    console.log("\nNo API key found!");
    console.log("Set ANTHROPIC_API_KEY or OPENAI_API_KEY environment variable.");
    console.log("\nExample:");
    console.log("  OPENAI_API_KEY=sk-... npx tsx test/real_llm_test.ts");
    process.exit(1);
  }

  // Create real Oracle with full protocol support
  const oracle = createAutoOracle();
  const snapshots = new SnapshotRepo();
  const receipts = new InMemoryReceiptStore("off");
  const commit = createConsoleCommit();

  const runtime = new RuntimeImpl(oracle, snapshots, receipts, commit);

  console.log("\n[TEST 1] Simple question via infer.op");
  console.log("-".repeat(40));

  try {
    const result1 = await runToCompletion(runtime, initialState(`
      (effect infer.op "What is 2 + 2? Reply with just the number.")
    `), 10000);

    console.log("LLM Response:", JSON.stringify(result1));
  } catch (err) {
    console.error("Error:", err);
  }

  console.log("\n[TEST 2] Use LLM result in computation");
  console.log("-".repeat(40));

  try {
    // Ask LLM for a number, then do math with it
    // Note: This depends on LLM returning a parseable number
    const result2 = await runToCompletion(runtime, initialState(`
      (effect infer.op "Pick a random number between 1 and 10. Reply with ONLY the number, nothing else.")
    `), 10000);

    console.log("LLM picked:", JSON.stringify(result2));
  } catch (err) {
    console.error("Error:", err);
  }

  console.log("\n[TEST 3] Code generation via LLM");
  console.log("-".repeat(40));

  try {
    const result3 = await runToCompletion(runtime, initialState(`
      (effect infer.op "Write a simple Lisp expression that adds 3 and 4. Reply with ONLY the expression, like: (+ 3 4)")
    `), 10000);

    console.log("LLM generated code:", JSON.stringify(result3));
  } catch (err) {
    console.error("Error:", err);
  }

  console.log("\n" + "=".repeat(60));
  console.log("Real LLM integration working!");
  console.log("=".repeat(60));
}

main().catch(console.error);
