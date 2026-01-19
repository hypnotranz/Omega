#!/usr/bin/env npx tsx
// run-demo.ts - RUNNABLE DEMO showing LLM solving a complex problem
// Run with: npx tsx run-demo.ts

import { COWStore } from "./src/core/eval/store";
import { RuntimeImpl } from "./src/core/effects/runtimeImpl";
import { PortalImpl } from "./src/core/oracle/portalImpl";
import { SnapshotRepo } from "./src/core/oracle/snapshots";
import { InMemoryReceiptStore } from "./src/core/oracle/receipts";
import { ScriptedOracleAdapter } from "./src/core/oracle/scriptedOracle";
import { compileTextToExpr } from "./src/core/pipeline/compileText";
import { runToCompletionWithState, runWithTrace, formatTrace } from "./src/core/eval/run";
import type { State } from "./src/core/eval/machine";
import { VUnit } from "./src/core/eval/values";
import "./src/core/oracle/plugins"; // Register plugins
import { ModelSelectorAdapter } from "./src/core/oracle/plugins";
import type { OracleResp, OracleReq } from "./src/core/oracle/protocol";
import * as fs from "fs";
import * as path from "path";

// Load API keys - prefer Anthropic (Claude) over OpenAI
function loadApiKeys(): { anthropic?: string; openai?: string } {
  const keys: { anthropic?: string; openai?: string } = {};

  // Check environment variables first
  keys.anthropic = process.env.ANTHROPIC_API_KEY;
  keys.openai = process.env.OPENAI_API_KEY;

  // Try config file for OpenAI key
  if (!keys.openai) {
    try {
      const paths = [
        path.resolve("config.yaml"),  // Local config first
        path.join(__dirname, "config.yaml"),
        path.resolve("../LambdaRLM/config.yaml"),
        path.join(__dirname, "../LambdaRLM/config.yaml"),
      ];
      for (const configPath of paths) {
        if (fs.existsSync(configPath)) {
          const content = fs.readFileSync(configPath, "utf8");
          // Match api_key: regardless of indentation
          const match = content.match(/api_key:\s*(\S+)/);
          if (match) {
            keys.openai = match[1];
            break;
          }
        }
      }
    } catch {}
  }

  return keys;
}

const API_KEYS = loadApiKeys();
const USE_ANTHROPIC = !!API_KEYS.anthropic;
const API_KEY = API_KEYS.anthropic || API_KEYS.openai;

if (!API_KEY) {
  console.error("ERROR: No API key found.");
  console.error("  Set ANTHROPIC_API_KEY (preferred) or OPENAI_API_KEY");
  process.exit(1);
}

console.log(`Using ${USE_ANTHROPIC ? "Anthropic (Claude)" : "OpenAI"} API`);

// Setup real runtime with Lisp code
async function setupRuntime(lispCode: string) {
  const oracle = new ScriptedOracleAdapter();
  const snapshots = new SnapshotRepo();
  const receipts = new InMemoryReceiptStore("off");
  const runtime = new RuntimeImpl(oracle, snapshots, receipts, {
    async commit() { return VUnit; }
  });

  const store0 = new COWStore();

  // Install primitives
  const { installPrims } = await import("./test/helpers/prims");
  const prim = installPrims(store0);

  // Compile and run the Lisp code
  const expr = compileTextToExpr(`(begin ${lispCode})`);
  const state0: State = {
    control: { tag: "Expr", e: expr },
    env: prim.env,
    store: prim.store,
    kont: [],
    handlers: [],
  };

  console.log("\n=== Executing Lisp code ===");
  const { value, state, trace } = await runWithTrace(runtime, state0, 100_000);

  // Show some trace output
  const traceLines = formatTrace(trace, { compact: true }).split("\n");
  console.log(`Trace (${trace.length} steps, showing first 10 and last 5):`);
  console.log(traceLines.slice(0, 10).join("\n"));
  if (trace.length > 15) {
    console.log(`  ... (${trace.length - 15} steps omitted) ...`);
    console.log(traceLines.slice(-5).join("\n"));
  }
  console.log(`Final value: ${JSON.stringify(value)}`);

  const envRef = snapshots.putEnv({ env: state.env, store: state.store });
  const stateRef = snapshots.putState({ state });
  const portal = new PortalImpl(runtime, snapshots, receipts, {
    maxEvalSteps: 100_000,
    parseText: (src: string) => compileTextToExpr(src),
  });

  return { portal, envRef, stateRef, snapshots, runtime };
}

// Run LLM inference with detailed logging
async function runLLMInference(
  task: string,
  portal: PortalImpl,
  envRef: any,
  stateRef: any
) {
  console.log("\n=== Starting LLM Session ===");
  console.log(`Task: ${task}`);
  console.log("\n--- System Prompt (sent to LLM) ---");
  console.log(`You are an Oracle for the Omega Lisp runtime.

LISP SYNTAX RULES:
- Function calls: (function-name arg1 arg2) NOT (function-name (arg1) (arg2))
- Example: (+ 1 2) returns 3
- Example: (factorial 6) NOT (factorial (6))

TOOLS:
- omega_eval: Evaluate Lisp expressions
- omega_observe: Observe runtime state (defs, stack, env)
- omega_return: Return your final answer (MUST call when done)
`);

  const selector = new ModelSelectorAdapter({
    defaultModel: USE_ANTHROPIC ? "claude-sonnet-4-20250514" : "gpt-4o-mini",
    defaultPlugin: USE_ANTHROPIC ? "anthropic" : "openai",
    sharedConfig: {
      apiKey: API_KEY,
      maxTokens: 1000,
    },
  });

  const session = selector.startSession({
    tag: "Infer",
    payload: { tag: "Str", s: task },
    envRef,
    stateRef,
  });

  let currentEnvRef = envRef;
  let currentStateRef = stateRef;
  let resp: OracleResp = { tag: "RespAck" };
  let turns = 0;
  const maxTurns = 15;

  console.log("\n--- LLM Conversation ---");

  while (turns < maxTurns) {
    turns++;
    const step = await session.next(resp);

    if (step.done) {
      console.log(`\n=== LLM Finished (Turn ${turns}) ===`);
      console.log("Final result:", JSON.stringify(step.value, null, 2));
      return step.value;
    }

    const req = step.value as OracleReq;
    console.log(`\nTurn ${turns}: LLM requests ${req.tag}`);

    if (req.tag === "ReqEval") {
      const expr = (req as any).qexpr;
      console.log(`  omega_eval("${expr}")`);

      try {
        resp = await portal.perform({
          tag: "ReqEval",
          qexpr: expr,
          envRef: currentEnvRef,
        });

        if ((resp as any).envRef) currentEnvRef = (resp as any).envRef;
        if ((resp as any).stateRef) currentStateRef = (resp as any).stateRef;
        console.log(`  -> Result: ${JSON.stringify((resp as any).value)}`);
      } catch (err) {
        const errMsg = err instanceof Error ? err.message : String(err);
        console.log(`  -> ERROR: ${errMsg}`);
        resp = { tag: "RespError", message: errMsg } as any;
      }

    } else if (req.tag === "ReqObserve") {
      const what = (req as any).what;
      console.log(`  omega_observe("${what.tag}"${what.name ? `, name="${what.name}"` : ""})`);

      resp = await portal.perform({
        tag: "ReqObserve",
        what,
        stateRef: currentStateRef,
      });

      const data = (resp as any).data;
      console.log(`  -> Result: ${JSON.stringify(data, null, 2).slice(0, 500)}${JSON.stringify(data).length > 500 ? "..." : ""}`);

    } else {
      console.log(`  (unhandled request type)`);
      resp = { tag: "RespAck" };
    }
  }

  console.log(`\nERROR: Max turns (${maxTurns}) exceeded`);
  return null;
}

// Main demo
async function main() {
  console.log("╔═══════════════════════════════════════════════════════════════╗");
  console.log("║  OmegaLLM Demo: LLM using Lisp REPL to solve problems         ║");
  console.log("╚═══════════════════════════════════════════════════════════════╝");

  // DEMO 1: Define functions, then ask LLM to use them
  console.log("\n\n" + "=".repeat(70));
  console.log("DEMO 1: LLM discovers and uses pre-defined functions");
  console.log("=".repeat(70));

  const { portal: p1, envRef: e1, stateRef: s1 } = await setupRuntime(`
    ; Define a factorial function
    (define (factorial n)
      (if (<= n 1)
          1
          (* n (factorial (- n 1)))))

    ; Define a fibonacci function
    (define (fib n)
      (if (<= n 1)
          n
          (+ (fib (- n 1)) (fib (- n 2)))))

    ; Define a helper
    (define (square x) (* x x))

    ; Return something to show setup worked
    "Functions defined"
  `);

  await runLLMInference(
    "Use omega_observe(defs) to see available functions. Then compute (+ (factorial$bid#1 6) (fib$bid#3 10)) using omega_eval. IMPORTANT: Arguments are NOT wrapped in parens - use (factorial$bid#1 6) not (factorial$bid#1 (6)).",
    p1, e1, s1
  );

  // DEMO 2: Simple multi-step computation
  console.log("\n\n" + "=".repeat(70));
  console.log("DEMO 2: LLM performs multi-step computation");
  console.log("=".repeat(70));

  const { portal: p2, envRef: e2, stateRef: s2 } = await setupRuntime(`
    (define base-value 100)
    (define multiplier 3)
    (define (add-ten x) (+ x 10))
    "Ready"
  `);

  await runLLMInference(
    "Use omega_observe(defs) to see what's defined. You will see names like 'add-ten$bid#3' - use EXACTLY those names in omega_eval. Compute: (* (add-ten$bid#3 base-value$bid#1) multiplier$bid#2). Use omega_eval.",
    p2, e2, s2
  );

  // DEMO 3: LLM computes factorial step by step
  console.log("\n\n" + "=".repeat(70));
  console.log("DEMO 3: LLM uses recursion to compute factorial");
  console.log("=".repeat(70));

  const { portal: p3, envRef: e3, stateRef: s3 } = await setupRuntime(`
    (define (fact n)
      (if (<= n 1)
          1
          (* n (fact (- n 1)))))
    "Ready"
  `);

  await runLLMInference(
    "Use omega_observe(defs) to see the function name (it will be like 'fact$bid#1'). Then use omega_eval to compute (fact$bid#1 7). Use the EXACT name from defs. Arguments are NOT wrapped - use (f 7) not (f (7)).",
    p3, e3, s3
  );

  console.log("\n" + "=".repeat(70));
  console.log("DEMO COMPLETE");
  console.log("=".repeat(70));
}

main().catch(console.error);
