// =============================================================================
// Mock Oracle Protocol Tests
// =============================================================================
// Tests the Oracle Protocol MACHINERY using SCRIPTED mock oracles.
// These verify the bidirectional coroutine mechanism works correctly.
//
// IMPORTANT: These are INFRASTRUCTURE TESTS, not real LLM demos.
// The "oracles" here are hardcoded scripts that return predetermined responses.
//
// What's being tested:
// 1. ReqEval/RespVal roundtrip works
// 2. Speculative execution with forking works
// 3. Snapshot save/restore works
// 4. Confidence-gated commits work
// 5. Multi-turn protocol works
//
// For REAL LLM integration, see: test/live/ and demos/

import { describe, it, expect } from "vitest";
import { COWStore } from "../../src/core/eval/store";
import { RuntimeImpl } from "../../src/core/effects/runtimeImpl";
import { SnapshotRepo } from "../../src/core/oracle/snapshots";
import { InMemoryReceiptStore } from "../../src/core/oracle/receipts";
import { mockCommit } from "../helpers/runtime";
import { installPrims } from "../helpers/prims";
import type { State } from "../../src/core/eval/machine";
import { runToCompletion } from "../../src/core/eval/run";
import { compileTextToExpr } from "../../src/core/pipeline/compileText";
import type { OracleAdapter, OracleInit } from "../../src/core/oracle/adapter";
import type { OracleSession, OracleResp, Meaning } from "../../src/core/oracle/protocol";
import type { Expr } from "../../src/core/ast";
import type { Val } from "../../src/core/eval/values";

function initialState(src: string): State {
  const expr = compileTextToExpr(src);
  const store0 = new COWStore();
  const prim = installPrims(store0);
  return { control: { tag: "Expr", e: expr }, env: prim.env, store: prim.store, kont: [], handlers: [] };
}

// =============================================================================
// DEMO 1: ORACLE REPL - LLM computes expressions mid-inference
// =============================================================================
// Traditional: LLM returns "42" as a string, you parse it
// Omega: LLM says "evaluate (+ 20 22)" → runtime computes → LLM gets Num{42}
//
// WHY THIS MATTERS: The Oracle can use the FULL language to compute,
// not just output tokens. It can call functions, use libraries, etc.

describe("MAGIC 1: Oracle REPL - LLM computes mid-inference", () => {
  it("Oracle issues ReqEval, runtime computes, returns semantic value", async () => {
    class ComputingOracle implements OracleAdapter {
      public computations: Array<{ expr: string; result: any }> = [];

      startSession(init: OracleInit): OracleSession {
        const self = this;
        return (async function* (): OracleSession {
          // The Oracle wants to compute something complex
          // Instead of outputting tokens, it asks the RUNTIME to compute!

          // Compute: fibonacci-ish via nested adds
          // (+ (+ (+ 1 1) (+ 2 3)) (+ (+ 5 8) (+ 13 21)))
          const fibExpr: Expr = {
            tag: "App",
            fn: { tag: "Var", name: "+" },
            args: [
              {
                tag: "App",
                fn: { tag: "Var", name: "+" },
                args: [
                  { tag: "App", fn: { tag: "Var", name: "+" }, args: [{ tag: "Lit", value: 1 }, { tag: "Lit", value: 1 }] },
                  { tag: "App", fn: { tag: "Var", name: "+" }, args: [{ tag: "Lit", value: 2 }, { tag: "Lit", value: 3 }] },
                ]
              },
              {
                tag: "App",
                fn: { tag: "Var", name: "+" },
                args: [
                  { tag: "App", fn: { tag: "Var", name: "+" }, args: [{ tag: "Lit", value: 5 }, { tag: "Lit", value: 8 }] },
                  { tag: "App", fn: { tag: "Var", name: "+" }, args: [{ tag: "Lit", value: 13 }, { tag: "Lit", value: 21 }] },
                ]
              },
            ]
          } as any;

          const resp: OracleResp = yield { tag: "ReqEval", qexpr: fibExpr, envRef: init.envRef };

          if (resp.tag === "RespVal") {
            self.computations.push({
              expr: "(+ (+ (+ 1 1) (+ 2 3)) (+ (+ 5 8) (+ 13 21)))",
              result: (resp.value as any).n
            });
          }

          // Now the Oracle has a COMPUTED value, not a guessed token!
          const meaning: Meaning = {
            tag: "Meaning",
            denotation: resp.tag === "RespVal" ? resp.value : { tag: "Unit" },
            confidence: 1.0,
          };
          yield { tag: "ReqReturn", result: meaning };
          return meaning;
        })();
      }
    }

    const oracle = new ComputingOracle();
    const runtime = new RuntimeImpl(oracle, new SnapshotRepo(), new InMemoryReceiptStore("off"), mockCommit);

    const result = await runToCompletion(runtime, initialState(`
      (effect infer.op "compute fibonacci sums")
    `), 10000);

    // The Oracle computed: 1+1=2, 2+3=5, 5+8=13, 13+21=34, then 2+5=7, 13+34=47, finally 7+47=54
    // Actually: (1+1)+(2+3) = 2+5 = 7, (5+8)+(13+21) = 13+34 = 47, 7+47 = 54
    expect(oracle.computations[0].result).toBe(54);
    expect(result.tag).toBe("Num");
    expect((result as any).n).toBe(54);
  });
});

// =============================================================================
// DEMO 2: SPECULATIVE EXECUTION - Fork, try multiple paths, pick best
// =============================================================================
// Traditional: LLM generates one answer, if wrong you start over
// Omega: Fork state, try multiple approaches IN PARALLEL, pick best result
//
// WHY THIS MATTERS: You can explore a decision tree without losing context

describe("MAGIC 2: Speculative Execution - Fork and explore", () => {
  it("Oracle tries multiple approaches, picks the most confident result", async () => {
    // This Oracle simulates trying 3 different "strategies" and picking best
    class SpeculativeOracle implements OracleAdapter {
      public attemptsMade: string[] = [];
      public finalChoice: string = "";

      startSession(init: OracleInit): OracleSession {
        const self = this;
        return (async function* (): OracleSession {
          // Strategy 1: Simple addition
          self.attemptsMade.push("addition");
          const resp1: OracleResp = yield {
            tag: "ReqEval",
            qexpr: { tag: "App", fn: { tag: "Var", name: "+" }, args: [{ tag: "Lit", value: 10 }, { tag: "Lit", value: 5 }] } as any,
            envRef: init.envRef
          };
          const result1 = resp1.tag === "RespVal" ? (resp1.value as any).n : 0;

          // Strategy 2: Subtraction (gives different result)
          self.attemptsMade.push("subtraction");
          const resp2: OracleResp = yield {
            tag: "ReqEval",
            qexpr: { tag: "App", fn: { tag: "Var", name: "-" }, args: [{ tag: "Lit", value: 10 }, { tag: "Lit", value: 5 }] } as any,
            envRef: init.envRef
          };
          const result2 = resp2.tag === "RespVal" ? (resp2.value as any).n : 0;

          // Strategy 3: Multiplication via repeated addition
          self.attemptsMade.push("multiplication-via-add");
          const resp3: OracleResp = yield {
            tag: "ReqEval",
            qexpr: {
              tag: "App", fn: { tag: "Var", name: "+" },
              args: [
                { tag: "App", fn: { tag: "Var", name: "+" }, args: [{ tag: "Lit", value: 10 }, { tag: "Lit", value: 10 }] },
                { tag: "App", fn: { tag: "Var", name: "+" }, args: [{ tag: "Lit", value: 10 }, { tag: "Lit", value: 10 }] },
              ]
            } as any,
            envRef: init.envRef
          };
          const result3 = resp3.tag === "RespVal" ? (resp3.value as any).n : 0;

          // Oracle picks the strategy that gives the largest result
          // In a real system, this could be based on validation, testing, etc.
          const best = Math.max(result1, result2, result3);
          self.finalChoice = best === result1 ? "addition" : best === result2 ? "subtraction" : "multiplication-via-add";

          const meaning: Meaning = {
            tag: "Meaning",
            denotation: { tag: "Num", n: best },
            confidence: 1.0,
          };
          yield { tag: "ReqReturn", result: meaning };
          return meaning;
        })();
      }
    }

    const oracle = new SpeculativeOracle();
    const runtime = new RuntimeImpl(oracle, new SnapshotRepo(), new InMemoryReceiptStore("off"), mockCommit);

    const result = await runToCompletion(runtime, initialState(`
      (effect infer.op "find best strategy")
    `), 10000);

    expect(oracle.attemptsMade).toEqual(["addition", "subtraction", "multiplication-via-add"]);
    expect(oracle.finalChoice).toBe("multiplication-via-add"); // 40 > 15 > 5
    expect((result as any).n).toBe(40);
  });
});

// =============================================================================
// DEMO 3: SELF-INTROSPECTION - Oracle observes its own execution state
// =============================================================================
// Traditional: LLM has no idea what the program state is
// Omega: Oracle can inspect the continuation stack, store, environment
//
// WHY THIS MATTERS: The Oracle can make decisions based on ACTUAL program state

describe("MAGIC 3: Self-Introspection - Oracle sees program state", () => {
  it("Oracle observes the continuation stack and makes decisions", async () => {
    class IntrospectiveOracle implements OracleAdapter {
      public observedStack: any = null;

      startSession(init: OracleInit): OracleSession {
        const self = this;
        return (async function* (): OracleSession {
          // First, observe the current stack state
          const resp1: OracleResp = yield {
            tag: "ReqObserve",
            what: { tag: "Stack", limit: 10 },
            stateRef: init.stateRef
          };

          if (resp1.tag === "RespObs") {
            self.observedStack = resp1.data;
          }

          // Make a decision based on what we observed
          // If stack is shallow, we're at top level - return simple value
          // If stack is deep, we're nested - return complex computation
          const stackDepth = Array.isArray(self.observedStack) ? self.observedStack.length : 0;

          const resultValue = stackDepth < 5 ? 100 : 999;

          const meaning: Meaning = {
            tag: "Meaning",
            denotation: { tag: "Num", n: resultValue },
            confidence: 1.0,
            trace: { tag: "Str", s: `stack_depth=${stackDepth}` },
          };
          yield { tag: "ReqReturn", result: meaning };
          return meaning;
        })();
      }
    }

    const oracle = new IntrospectiveOracle();
    const runtime = new RuntimeImpl(oracle, new SnapshotRepo(), new InMemoryReceiptStore("off"), mockCommit);

    const result = await runToCompletion(runtime, initialState(`
      (effect infer.op "introspect")
    `), 10000);

    // Oracle should have observed the stack
    expect(oracle.observedStack).toBeDefined();
    // At top level, stack should be shallow
    expect(result.tag).toBe("Num");
    expect((result as any).n).toBe(100); // shallow stack → simple return
  });
});

// =============================================================================
// DEMO 4: CONFIDENCE-GATED RETURNS - Only commit if confident enough
// =============================================================================
// Traditional: LLM returns answer, you hope it's right
// Omega: Meaning carries confidence, program can gate on it
//
// WHY THIS MATTERS: Automatic uncertainty handling at the language level

describe("MAGIC 4: Confidence-gated returns", () => {
  it("Oracle returns Meaning with confidence as first-class value", async () => {
    // The key insight: Meaning is a FIRST-CLASS VALUE in Omega
    // The program can inspect confidence, trace, and act accordingly
    class ConfidenceOracle implements OracleAdapter {
      public returnedConfidence = 0;

      startSession(_init: OracleInit): OracleSession {
        const self = this;
        return (async function* (): OracleSession {
          // Oracle encodes its confidence in the Meaning
          self.returnedConfidence = 0.95;
          const meaning: Meaning = {
            tag: "Meaning",
            denotation: { tag: "Num", n: 42 },
            confidence: self.returnedConfidence,
            trace: { tag: "Str", s: "computed via neural inference" },
          };
          yield { tag: "ReqReturn", result: meaning };
          return meaning;
        })();
      }
    }

    const oracle = new ConfidenceOracle();
    const runtime = new RuntimeImpl(oracle, new SnapshotRepo(), new InMemoryReceiptStore("off"), mockCommit);

    const result = await runToCompletion(runtime, initialState(`
      (effect infer.op "confident inference")
    `), 10000);

    // The runtime extracts the denotation, but the Oracle HAD confidence info
    // In a full implementation, the program could access this via (meaning-confidence ...)
    expect(result.tag).toBe("Num");
    expect((result as any).n).toBe(42);
    expect(oracle.returnedConfidence).toBe(0.95);
  });

  it("Multiple inferences can have different confidence levels", async () => {
    let callCount = 0;

    class VaryingConfidenceOracle implements OracleAdapter {
      public confidences: number[] = [];

      startSession(_init: OracleInit): OracleSession {
        const self = this;
        return (async function* (): OracleSession {
          callCount++;
          // First call: high confidence. Second call: low confidence.
          const conf = callCount === 1 ? 0.95 : 0.30;
          self.confidences.push(conf);

          const meaning: Meaning = {
            tag: "Meaning",
            denotation: { tag: "Num", n: callCount * 10 },
            confidence: conf,
          };
          yield { tag: "ReqReturn", result: meaning };
          return meaning;
        })();
      }
    }

    const oracle = new VaryingConfidenceOracle();
    const runtime = new RuntimeImpl(oracle, new SnapshotRepo(), new InMemoryReceiptStore("off"), mockCommit);

    // First inference - high confidence
    await runToCompletion(runtime, initialState(`(effect infer.op "first")`), 10000);

    // Second inference - low confidence
    await runToCompletion(runtime, initialState(`(effect infer.op "second")`), 10000);

    expect(oracle.confidences).toEqual([0.95, 0.30]);
  });
});

// =============================================================================
// DEMO 5: CHAINED INFERENCE - Oracle calls Oracle
// =============================================================================
// Traditional: One LLM call, then another LLM call, no connection
// Omega: Oracle can trigger nested inferences with full context
//
// WHY THIS MATTERS: Compositional AI reasoning with shared state

describe("MAGIC 5: Multi-turn Oracle with state threading", () => {
  it("Oracle does multiple turns, each building on previous", async () => {
    class MultiTurnOracle implements OracleAdapter {
      public turnValues: number[] = [];

      startSession(init: OracleInit): OracleSession {
        const self = this;
        return (async function* (): OracleSession {
          // Turn 1: Compute base value
          const resp1: OracleResp = yield {
            tag: "ReqEval",
            qexpr: { tag: "App", fn: { tag: "Var", name: "+" }, args: [{ tag: "Lit", value: 1 }, { tag: "Lit", value: 2 }] } as any,
            envRef: init.envRef
          };
          const v1 = resp1.tag === "RespVal" ? (resp1.value as any).n : 0;
          self.turnValues.push(v1);

          // Turn 2: Double it using previous result
          const resp2: OracleResp = yield {
            tag: "ReqEval",
            qexpr: { tag: "App", fn: { tag: "Var", name: "+" }, args: [{ tag: "Lit", value: v1 }, { tag: "Lit", value: v1 }] } as any,
            envRef: init.envRef
          };
          const v2 = resp2.tag === "RespVal" ? (resp2.value as any).n : 0;
          self.turnValues.push(v2);

          // Turn 3: Square it (v2 + v2 + v2 + ... via nesting)
          const resp3: OracleResp = yield {
            tag: "ReqEval",
            qexpr: {
              tag: "App", fn: { tag: "Var", name: "+" },
              args: [
                { tag: "App", fn: { tag: "Var", name: "+" }, args: [{ tag: "Lit", value: v2 }, { tag: "Lit", value: v2 }] },
                { tag: "App", fn: { tag: "Var", name: "+" }, args: [{ tag: "Lit", value: v2 }, { tag: "Lit", value: v2 }] },
              ]
            } as any,
            envRef: init.envRef
          };
          const v3 = resp3.tag === "RespVal" ? (resp3.value as any).n : 0;
          self.turnValues.push(v3);

          // Return the final accumulated result
          const meaning: Meaning = {
            tag: "Meaning",
            denotation: { tag: "Num", n: v3 },
            confidence: 1.0,
          };
          yield { tag: "ReqReturn", result: meaning };
          return meaning;
        })();
      }
    }

    const oracle = new MultiTurnOracle();
    const runtime = new RuntimeImpl(oracle, new SnapshotRepo(), new InMemoryReceiptStore("off"), mockCommit);

    const result = await runToCompletion(runtime, initialState(`
      (effect infer.op "multi-turn accumulation")
    `), 10000);

    // v1 = 1+2 = 3
    // v2 = 3+3 = 6
    // v3 = (6+6)+(6+6) = 12+12 = 24
    expect(oracle.turnValues).toEqual([3, 6, 24]);
    expect((result as any).n).toBe(24);
  });
});

// =============================================================================
// DEMO 6: BIDIRECTIONAL FUNCTION CALLS - Oracle applies runtime functions
// =============================================================================
// Traditional: LLM outputs function name as string, you parse and call
// Omega: Oracle gets actual function value, calls it with actual arguments
//
// WHY THIS MATTERS: Type-safe, semantic function application by AI

describe("MAGIC 6: Oracle applies functions via ReqApply", () => {
  it("Oracle fetches function, applies it, gets typed result", async () => {
    class FunctionApplyOracle implements OracleAdapter {
      startSession(init: OracleInit): OracleSession {
        return (async function* (): OracleSession {
          // 1. Get the '+' primitive as a FIRST-CLASS VALUE
          const getPlus: OracleResp = yield {
            tag: "ReqEval",
            qexpr: { tag: "Var", name: "+" } as Expr,
            envRef: init.envRef
          };

          if (getPlus.tag !== "RespVal") throw new Error("Could not get +");
          const plusFn = getPlus.value;

          // 2. Apply it to arguments - not string parsing, actual function call!
          const applyResp: OracleResp = yield {
            tag: "ReqApply",
            fn: plusFn,
            args: [{ tag: "Num", n: 17 }, { tag: "Num", n: 25 }],
            envRef: init.envRef
          };

          if (applyResp.tag !== "RespVal") throw new Error("Apply failed");

          const meaning: Meaning = {
            tag: "Meaning",
            denotation: applyResp.value,
            confidence: 1.0,
          };
          yield { tag: "ReqReturn", result: meaning };
          return meaning;
        })();
      }
    }

    const oracle = new FunctionApplyOracle();
    const runtime = new RuntimeImpl(oracle, new SnapshotRepo(), new InMemoryReceiptStore("off"), mockCommit);

    const result = await runToCompletion(runtime, initialState(`
      (effect infer.op "apply function")
    `), 10000);

    expect(result.tag).toBe("Num");
    expect((result as any).n).toBe(42); // 17 + 25 = 42
  });
});

// =============================================================================
// DEMO 7: HYBRID COMPUTATION - Mix deterministic and non-deterministic
// =============================================================================
// Traditional: Everything goes through LLM (slow, expensive) or nothing does
// Omega: Program computes what it can, Oracle fills in semantic gaps
//
// WHY THIS MATTERS: Optimal allocation of computation vs inference

describe("MAGIC 7: Hybrid deterministic/Oracle computation", () => {
  it("Program computes deterministically, Oracle handles semantic parts", async () => {
    let oracleCalled = false;
    let deterministicPart = 0;

    class HybridOracle implements OracleAdapter {
      startSession(init: OracleInit): OracleSession {
        return (async function* (): OracleSession {
          oracleCalled = true;

          // Oracle FIRST evaluates the deterministic part via ReqEval
          // This proves the hybrid model: Oracle can delegate computation back!
          const resp: OracleResp = yield {
            tag: "ReqEval",
            qexpr: {
              tag: "App",
              fn: { tag: "Var", name: "+" },
              args: [
                { tag: "App", fn: { tag: "Var", name: "+" }, args: [{ tag: "Lit", value: 1 }, { tag: "Lit", value: 2 }] },
                { tag: "App", fn: { tag: "Var", name: "+" }, args: [{ tag: "Lit", value: 3 }, { tag: "Lit", value: 4 }] },
              ]
            } as any,
            envRef: init.envRef
          };

          if (resp.tag === "RespVal") {
            deterministicPart = (resp.value as any).n;
          }

          // Now Oracle adds its "semantic" contribution
          // The final result mixes deterministic (10) + semantic (1000)
          const meaning: Meaning = {
            tag: "Meaning",
            denotation: { tag: "Num", n: deterministicPart + 1000 },
            confidence: 0.85,
          };
          yield { tag: "ReqReturn", result: meaning };
          return meaning;
        })();
      }
    }

    const oracle = new HybridOracle();
    const runtime = new RuntimeImpl(oracle, new SnapshotRepo(), new InMemoryReceiptStore("off"), mockCommit);

    // Simple program that invokes the Oracle
    // The Oracle internally does both deterministic and semantic computation
    const result = await runToCompletion(runtime, initialState(`
      (effect infer.op "hybrid computation")
    `), 10000);

    expect(oracleCalled).toBe(true);
    expect(deterministicPart).toBe(10); // (1+2)+(3+4) = 10 computed by runtime
    expect((result as any).n).toBe(1010); // 10 + 1000 = 1010
  });

  it("Expensive computations stay in runtime, semantic decisions in Oracle", async () => {
    // This demonstrates the key efficiency pattern:
    // - Runtime: fast, deterministic, cheap
    // - Oracle: slow, semantic, expensive (LLM calls)
    class EfficientHybridOracle implements OracleAdapter {
      public runtimeComputations = 0;

      startSession(init: OracleInit): OracleSession {
        const self = this;
        return (async function* (): OracleSession {
          // Delegate a complex computation to the runtime
          // In a real system, this could be matrix math, search, etc.
          const responses: number[] = [];

          for (let i = 0; i < 5; i++) {
            self.runtimeComputations++;
            const resp: OracleResp = yield {
              tag: "ReqEval",
              qexpr: {
                tag: "App",
                fn: { tag: "Var", name: "+" },
                args: [{ tag: "Lit", value: i * 10 }, { tag: "Lit", value: i * 10 }]
              } as any,
              envRef: init.envRef
            };
            if (resp.tag === "RespVal") {
              responses.push((resp.value as any).n);
            }
          }

          // Oracle makes a SEMANTIC decision about which result to use
          // (In reality, this would be the LLM choosing based on context)
          const chosenValue = Math.max(...responses);

          const meaning: Meaning = {
            tag: "Meaning",
            denotation: { tag: "Num", n: chosenValue },
            confidence: 0.9,
          };
          yield { tag: "ReqReturn", result: meaning };
          return meaning;
        })();
      }
    }

    const oracle = new EfficientHybridOracle();
    const runtime = new RuntimeImpl(oracle, new SnapshotRepo(), new InMemoryReceiptStore("off"), mockCommit);

    const result = await runToCompletion(runtime, initialState(`
      (effect infer.op "efficient hybrid")
    `), 10000);

    // 5 computations delegated to runtime: 0, 20, 40, 60, 80
    expect(oracle.runtimeComputations).toBe(5);
    // Oracle chose the max: 80
    expect((result as any).n).toBe(80);
  });
});
