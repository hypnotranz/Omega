// =============================================================================
// Oracle Protocol Capability Tests
// =============================================================================
// Tests the INFRASTRUCTURE of the Oracle Protocol using MOCK oracles.
// These verify that the bidirectional coroutine mechanism works correctly.
//
// NOTE: These tests use scripted mock oracles, NOT real LLMs.
// For real LLM integration tests, see: test/live/ and demos/
//
// The protocol capabilities being tested:
// 1. ReqEval - Oracle can request runtime to evaluate expressions
// 2. ReqApply - Oracle can apply functions
// 3. ReqObserve - Oracle can inspect program state
// 4. Meaning values - Typed semantic results with confidence

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

function initialState(src: string): State {
  const expr = compileTextToExpr(src);
  const store0 = new COWStore();
  const prim = installPrims(store0);
  return { control: { tag: "Expr", e: expr }, env: prim.env, store: prim.store, kont: [], handlers: [] };
}

// =============================================================================
// MIND-BLOWING 1: ORACLE WRITES CODE AND EXECUTES IT
// =============================================================================
// Traditional: LLM outputs code as STRING, you parse it externally, hope it works
// Omega: Oracle constructs AST, asks runtime to evaluate it, gets typed result
//
// THIS IS IMPOSSIBLE IN TRADITIONAL SYSTEMS because:
// - The LLM would need to output valid code
// - You'd need to parse it
// - You'd need to create a new execution context
// - Errors wouldn't propagate back to the LLM's reasoning

describe("MINDBLOWING 1: Oracle generates and executes code", () => {
  it("Oracle constructs a computation dynamically and executes it", async () => {
    class CodeGenOracle implements OracleAdapter {
      public generatedCode = false;
      public computationResult = 0;

      startSession(init: OracleInit): OracleSession {
        const self = this;
        return (async function* (): OracleSession {
          // Oracle decides to create a complex computation dynamically
          // It constructs the AST directly - no string parsing!
          self.generatedCode = true;

          // Generate: ((+ 20 1) + (+ 20 1)) = 21 + 21 = 42
          // This is dynamic code generation - the Oracle BUILDS the AST
          const computationAST: Expr = {
            tag: "App",
            fn: { tag: "Var", name: "+" },
            args: [
              { tag: "App", fn: { tag: "Var", name: "+" }, args: [{ tag: "Lit", value: 20 }, { tag: "Lit", value: 1 }] },
              { tag: "App", fn: { tag: "Var", name: "+" }, args: [{ tag: "Lit", value: 20 }, { tag: "Lit", value: 1 }] },
            ]
          } as any;

          // Have the runtime evaluate our generated code
          const resp: OracleResp = yield {
            tag: "ReqEval",
            qexpr: computationAST,
            envRef: init.envRef
          };

          if (resp.tag !== "RespVal") throw new Error("Failed to execute generated code");
          self.computationResult = (resp.value as any).n;

          const meaning: Meaning = {
            tag: "Meaning",
            denotation: resp.value,
            confidence: 1.0,
            trace: { tag: "Str", s: "generated and executed dynamic computation" }
          };
          yield { tag: "ReqReturn", result: meaning };
          return meaning;
        })();
      }
    }

    const oracle = new CodeGenOracle();
    const runtime = new RuntimeImpl(oracle, new SnapshotRepo(), new InMemoryReceiptStore("off"), mockCommit);

    const result = await runToCompletion(runtime, initialState(`
      (effect infer.op "generate and run code")
    `), 10000);

    expect(oracle.generatedCode).toBe(true);
    expect(oracle.computationResult).toBe(42); // (20+1) + (20+1) = 42
    expect((result as any).n).toBe(42);
  });

  it("Oracle uses ReqApply to invoke functions it retrieves", async () => {
    class FunctionInvokeOracle implements OracleAdapter {
      public functionWasInvoked = false;
      public invokeResult = 0;

      startSession(init: OracleInit): OracleSession {
        const self = this;
        return (async function* (): OracleSession {
          // Step 1: Get the '+' primitive function
          const resp1: OracleResp = yield {
            tag: "ReqEval",
            qexpr: { tag: "Var", name: "+" } as Expr,
            envRef: init.envRef
          };

          if (resp1.tag !== "RespVal") throw new Error("Failed to get function");
          const plusFn = resp1.value;

          // Step 2: APPLY the function using ReqApply
          self.functionWasInvoked = true;
          const resp2: OracleResp = yield {
            tag: "ReqApply",
            fn: plusFn,
            args: [{ tag: "Num", n: 21 }, { tag: "Num", n: 21 }],
            envRef: init.envRef
          };

          if (resp2.tag !== "RespVal") throw new Error("Failed to apply function");
          self.invokeResult = (resp2.value as any).n;

          const meaning: Meaning = {
            tag: "Meaning",
            denotation: resp2.value,
            confidence: 1.0,
          };
          yield { tag: "ReqReturn", result: meaning };
          return meaning;
        })();
      }
    }

    const oracle = new FunctionInvokeOracle();
    const runtime = new RuntimeImpl(oracle, new SnapshotRepo(), new InMemoryReceiptStore("off"), mockCommit);

    const result = await runToCompletion(runtime, initialState(`
      (effect infer.op "invoke function")
    `), 10000);

    expect(oracle.functionWasInvoked).toBe(true);
    expect(oracle.invokeResult).toBe(42); // 21 + 21 = 42
    expect((result as any).n).toBe(42);
  });

  it("Oracle generates recursive computation", async () => {
    class RecursiveGenOracle implements OracleAdapter {
      public iterationsComputed = 0;

      startSession(init: OracleInit): OracleSession {
        const self = this;
        return (async function* (): OracleSession {
          // Oracle wants to compute: 1 + 2 + 3 + 4 + 5 = 15
          // It generates the computation dynamically!
          let acc = 0;

          for (let i = 1; i <= 5; i++) {
            self.iterationsComputed++;

            // Generate: (+ acc i)
            const addExpr: Expr = {
              tag: "App",
              fn: { tag: "Var", name: "+" },
              args: [{ tag: "Lit", value: acc }, { tag: "Lit", value: i }]
            } as any;

            const resp: OracleResp = yield {
              tag: "ReqEval",
              qexpr: addExpr,
              envRef: init.envRef
            };

            if (resp.tag === "RespVal") {
              acc = (resp.value as any).n;
            }
          }

          const meaning: Meaning = {
            tag: "Meaning",
            denotation: { tag: "Num", n: acc },
            confidence: 1.0,
          };
          yield { tag: "ReqReturn", result: meaning };
          return meaning;
        })();
      }
    }

    const oracle = new RecursiveGenOracle();
    const runtime = new RuntimeImpl(oracle, new SnapshotRepo(), new InMemoryReceiptStore("off"), mockCommit);

    const result = await runToCompletion(runtime, initialState(`
      (effect infer.op "compute sum 1..5")
    `), 10000);

    expect(oracle.iterationsComputed).toBe(5);
    expect((result as any).n).toBe(15); // 1+2+3+4+5 = 15
  });
});

// =============================================================================
// MIND-BLOWING 2: ORACLE INSPECTS AND MODIFIES ITS OWN REASONING
// =============================================================================
// Traditional: LLM has no access to its own internal state during generation
// Omega: Oracle can observe stack, store, and make decisions based on real state
//
// THIS IS IMPOSSIBLE IN TRADITIONAL SYSTEMS because:
// - LLMs are stateless transformers - they can't inspect their own execution
// - Agent frameworks give summaries, not live access to machine state

describe("MINDBLOWING 2: Oracle inspects its own execution", () => {
  it("Oracle observes stack depth and changes behavior accordingly", async () => {
    class MetaOracle implements OracleAdapter {
      public stackWasObserved = false;
      public observedDepth = 0;
      public behaviorChanged = "";

      startSession(init: OracleInit): OracleSession {
        const self = this;
        return (async function* (): OracleSession {
          // First, observe current execution state
          const resp1: OracleResp = yield {
            tag: "ReqObserve",
            what: { tag: "Stack", limit: 20 },
            stateRef: init.stateRef
          };

          if (resp1.tag === "RespObs") {
            self.stackWasObserved = true;
            self.observedDepth = Array.isArray(resp1.data) ? resp1.data.length : 0;
          }

          // Change behavior based on what we observed
          // In a real system, this could be: "stack too deep = simplify approach"
          if (self.observedDepth < 3) {
            self.behaviorChanged = "simple_path";
            const meaning: Meaning = {
              tag: "Meaning",
              denotation: { tag: "Num", n: 100 },
              confidence: 1.0,
            };
            yield { tag: "ReqReturn", result: meaning };
            return meaning;
          } else {
            self.behaviorChanged = "complex_path";
            const meaning: Meaning = {
              tag: "Meaning",
              denotation: { tag: "Num", n: 999 },
              confidence: 0.5, // Lower confidence for complex path
            };
            yield { tag: "ReqReturn", result: meaning };
            return meaning;
          }
        })();
      }
    }

    const oracle = new MetaOracle();
    const runtime = new RuntimeImpl(oracle, new SnapshotRepo(), new InMemoryReceiptStore("off"), mockCommit);

    const result = await runToCompletion(runtime, initialState(`
      (effect infer.op "meta-observe")
    `), 10000);

    expect(oracle.stackWasObserved).toBe(true);
    expect(oracle.behaviorChanged).toBe("simple_path"); // Top-level = shallow stack
    expect((result as any).n).toBe(100);
  });
});

// =============================================================================
// MIND-BLOWING 3: SEMANTIC SEARCH WITH TYPED RESULTS
// =============================================================================
// Traditional: LLM returns JSON string, you parse it, hope fields exist
// Omega: Oracle returns TYPED VALUES with semantic metadata
//
// THIS IS IMPOSSIBLE IN TRADITIONAL SYSTEMS because:
// - LLM outputs are untyped strings
// - No guarantee of structure
// - No confidence/trace metadata

describe("MINDBLOWING 3: Typed semantic results", () => {
  it("Oracle returns structured semantic value, not string", async () => {
    class StructuredOracle implements OracleAdapter {
      startSession(init: OracleInit): OracleSession {
        return (async function* (): OracleSession {
          // Oracle computes multiple things and bundles them
          // This is a TYPED TUPLE, not a JSON string!

          // Compute first value
          const resp1: OracleResp = yield {
            tag: "ReqEval",
            qexpr: { tag: "App", fn: { tag: "Var", name: "+" }, args: [{ tag: "Lit", value: 10 }, { tag: "Lit", value: 20 }] } as any,
            envRef: init.envRef
          };
          const v1 = resp1.tag === "RespVal" ? (resp1.value as any).n : 0;

          // Compute second value
          const resp2: OracleResp = yield {
            tag: "ReqEval",
            qexpr: { tag: "App", fn: { tag: "Var", name: "-" }, args: [{ tag: "Lit", value: 100 }, { tag: "Lit", value: 58 }] } as any,
            envRef: init.envRef
          };
          const v2 = resp2.tag === "RespVal" ? (resp2.value as any).n : 0;

          // Return a structured result with full metadata
          const meaning: Meaning = {
            tag: "Meaning",
            denotation: { tag: "Num", n: v1 + v2 }, // 30 + 42 = 72
            confidence: 0.95,
            trace: { tag: "Str", s: `computed ${v1} and ${v2}, summed to ${v1 + v2}` },
          };
          yield { tag: "ReqReturn", result: meaning };
          return meaning;
        })();
      }
    }

    const oracle = new StructuredOracle();
    const runtime = new RuntimeImpl(oracle, new SnapshotRepo(), new InMemoryReceiptStore("off"), mockCommit);

    const result = await runToCompletion(runtime, initialState(`
      (effect infer.op "structured computation")
    `), 10000);

    expect((result as any).n).toBe(72); // 30 + 42
  });
});

// =============================================================================
// MIND-BLOWING 4: ORACLE AS DEBUGGER - INSPECT AND FIX
// =============================================================================
// Traditional: Error occurs, you get stack trace, start debugging manually
// Omega: Oracle CAN BE the debugger - inspect state, diagnose, fix
//
// THIS IS IMPOSSIBLE IN TRADITIONAL SYSTEMS because:
// - No bidirectional control flow
// - No access to live machine state
// - No ability to inject fixes

describe("MINDBLOWING 4: Oracle as intelligent debugger", () => {
  it("Oracle detects issue, computes fix, returns corrected result", async () => {
    class DebuggerOracle implements OracleAdapter {
      public detectedProblem = false;
      public appliedFix = false;

      startSession(init: OracleInit): OracleSession {
        const self = this;
        return (async function* (): OracleSession {
          // Simulate: Oracle tries something, finds a problem

          // First attempt: try to compute something that reveals an issue
          const resp1: OracleResp = yield {
            tag: "ReqEval",
            qexpr: { tag: "App", fn: { tag: "Var", name: "-" }, args: [{ tag: "Lit", value: 10 }, { tag: "Lit", value: 20 }] } as any,
            envRef: init.envRef
          };

          // Check result: negative number might be "wrong" for this use case
          const v1 = resp1.tag === "RespVal" ? (resp1.value as any).n : 0;

          if (v1 < 0) {
            self.detectedProblem = true;

            // Oracle decides to FIX IT by negating
            self.appliedFix = true;
            const resp2: OracleResp = yield {
              tag: "ReqEval",
              qexpr: { tag: "App", fn: { tag: "Var", name: "-" }, args: [{ tag: "Lit", value: 0 }, { tag: "Lit", value: v1 }] } as any,
              envRef: init.envRef
            };

            const fixed = resp2.tag === "RespVal" ? resp2.value : { tag: "Num", n: 0 };

            const meaning: Meaning = {
              tag: "Meaning",
              denotation: fixed,
              confidence: 0.8, // Slightly lower confidence due to fix
              trace: { tag: "Str", s: `detected negative ${v1}, applied abs fix` },
            };
            yield { tag: "ReqReturn", result: meaning };
            return meaning;
          }

          // No fix needed
          const meaning: Meaning = {
            tag: "Meaning",
            denotation: { tag: "Num", n: v1 },
            confidence: 1.0,
          };
          yield { tag: "ReqReturn", result: meaning };
          return meaning;
        })();
      }
    }

    const oracle = new DebuggerOracle();
    const runtime = new RuntimeImpl(oracle, new SnapshotRepo(), new InMemoryReceiptStore("off"), mockCommit);

    const result = await runToCompletion(runtime, initialState(`
      (effect infer.op "debug and fix")
    `), 10000);

    expect(oracle.detectedProblem).toBe(true);
    expect(oracle.appliedFix).toBe(true);
    expect((result as any).n).toBe(10); // |10-20| = |-10| = 10
  });
});

// =============================================================================
// MIND-BLOWING 5: COMPOSITIONAL ORACLE CALLS
// =============================================================================
// Traditional: Each LLM call is isolated, no shared computation state
// Omega: Oracle can build up computation across multiple turns, sharing state
//
// THIS IS IMPOSSIBLE IN TRADITIONAL SYSTEMS because:
// - Each API call starts fresh
// - No persistent computation context
// - No ability to "pick up where we left off"

describe("MINDBLOWING 5: Compositional multi-turn reasoning", () => {
  it("Oracle builds computation across many turns, all sharing state", async () => {
    class CompositionalOracle implements OracleAdapter {
      public turnsTaken = 0;
      public intermediateResults: number[] = [];

      startSession(init: OracleInit): OracleSession {
        const self = this;
        return (async function* (): OracleSession {
          // This Oracle simulates a complex reasoning chain
          // Each "turn" builds on the previous

          // Turn 1: Establish base
          self.turnsTaken++;
          const resp1: OracleResp = yield {
            tag: "ReqEval",
            qexpr: { tag: "Lit", value: 1 } as any,
            envRef: init.envRef
          };
          const base = resp1.tag === "RespVal" ? (resp1.value as any).n : 0;
          self.intermediateResults.push(base);

          // Turn 2-6: Each doubles the previous (simulating iterative refinement)
          let current = base;
          for (let i = 0; i < 5; i++) {
            self.turnsTaken++;
            const resp: OracleResp = yield {
              tag: "ReqEval",
              qexpr: {
                tag: "App",
                fn: { tag: "Var", name: "+" },
                args: [{ tag: "Lit", value: current }, { tag: "Lit", value: current }]
              } as any,
              envRef: init.envRef
            };
            if (resp.tag === "RespVal") {
              current = (resp.value as any).n;
              self.intermediateResults.push(current);
            }
          }

          // Final result: 1 -> 2 -> 4 -> 8 -> 16 -> 32
          const meaning: Meaning = {
            tag: "Meaning",
            denotation: { tag: "Num", n: current },
            confidence: 1.0,
            trace: { tag: "Str", s: `composed ${self.turnsTaken} turns: ${self.intermediateResults.join(" -> ")}` },
          };
          yield { tag: "ReqReturn", result: meaning };
          return meaning;
        })();
      }
    }

    const oracle = new CompositionalOracle();
    const runtime = new RuntimeImpl(oracle, new SnapshotRepo(), new InMemoryReceiptStore("off"), mockCommit);

    const result = await runToCompletion(runtime, initialState(`
      (effect infer.op "compositional reasoning")
    `), 10000);

    expect(oracle.turnsTaken).toBe(6); // 1 base + 5 doublings
    expect(oracle.intermediateResults).toEqual([1, 2, 4, 8, 16, 32]);
    expect((result as any).n).toBe(32);
  });
});
