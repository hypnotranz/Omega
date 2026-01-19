// Test Oracle REPL Re-entry: the Oracle can call back INTO the interpreter
// This is the core capability - LLM issues ReqEval/ReqApply, runtime executes, returns result
//
// The ScriptedOracleAdapter proves this works without a real LLM:
// 1. Receives infer.op
// 2. Issues ReqEval to evaluate (+ 20 22)
// 3. Runtime evaluates it → 42
// 4. Oracle receives 42, wraps in Meaning, returns to program

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

// Custom Oracle that tracks what it does
class TrackingOracleAdapter implements OracleAdapter {
  public reqEvalCalls: Array<{ qexpr: Expr; envRef: string }> = [];
  public reqApplyCalls: Array<{ fn: Val; args: Val[]; envRef: string }> = [];
  public reqObserveCalls: Array<{ what: any; stateRef: string }> = [];
  public gotResponses: OracleResp[] = [];

  startSession(init: OracleInit): OracleSession {
    const self = this;

    if (init.tag === "Infer") {
      return (async function* (): OracleSession {
        // 1. Issue ReqEval to evaluate (+ 20 22)
        const qexpr: Expr = {
          tag: "App",
          fn: { tag: "Var", name: "+" },
          args: [{ tag: "Lit", value: 20 }, { tag: "Lit", value: 22 }],
        } as any;

        self.reqEvalCalls.push({ qexpr, envRef: init.envRef });

        const resp1: OracleResp = yield { tag: "ReqEval", qexpr, envRef: init.envRef };
        self.gotResponses.push(resp1);

        // 2. Verify we got the right answer
        if (resp1.tag !== "RespVal") {
          const m: Meaning = { tag: "Meaning", confidence: 0, trace: { tag: "Str", s: "error: no RespVal" } };
          yield { tag: "ReqReturn", result: m };
          return m;
        }

        // 3. Return the result wrapped in Meaning
        const meaning: Meaning = {
          tag: "Meaning",
          denotation: resp1.value,
          confidence: 1.0,
        };
        yield { tag: "ReqReturn", result: meaning };
        return meaning;
      })();
    }

    // Apply session - calls a function via ReqApply
    if (init.tag === "Apply") {
      return (async function* (): OracleSession {
        // 1. First get the '+' function via ReqEval
        const plusExpr: Expr = { tag: "Var", name: "+" } as any;
        self.reqEvalCalls.push({ qexpr: plusExpr, envRef: init.envRef });

        const resp1: OracleResp = yield { tag: "ReqEval", qexpr: plusExpr, envRef: init.envRef };
        self.gotResponses.push(resp1);

        if (resp1.tag !== "RespVal") {
          const m: Meaning = { tag: "Meaning", confidence: 0 };
          yield { tag: "ReqReturn", result: m };
          return m;
        }

        // 2. Now call the function via ReqApply
        self.reqApplyCalls.push({ fn: resp1.value, args: init.args, envRef: init.envRef });

        const resp2: OracleResp = yield { tag: "ReqApply", fn: resp1.value, args: init.args, envRef: init.envRef };
        self.gotResponses.push(resp2);

        if (resp2.tag !== "RespVal") {
          const m: Meaning = { tag: "Meaning", confidence: 0 };
          yield { tag: "ReqReturn", result: m };
          return m;
        }

        const meaning: Meaning = { tag: "Meaning", denotation: resp2.value, confidence: 1.0 };
        yield { tag: "ReqReturn", result: meaning };
        return meaning;
      })();
    }

    // Fallback
    return (async function* (): OracleSession {
      const m: Meaning = { tag: "Meaning", confidence: 0 };
      yield { tag: "ReqReturn", result: m };
      return m;
    })();
  }
}

describe("Oracle REPL Re-entry", () => {
  it("R1: Oracle issues ReqEval, runtime evaluates, returns result", async () => {
    const oracle = new TrackingOracleAdapter();
    const snapshots = new SnapshotRepo();
    const receipts = new InMemoryReceiptStore("off");
    const runtime = new RuntimeImpl(oracle, snapshots, receipts, mockCommit);

    const result = await runToCompletion(runtime, initialState(`
      (effect infer.op "trigger oracle")
    `), 10000);

    // Verify the Oracle issued ReqEval
    expect(oracle.reqEvalCalls.length).toBe(1);
    expect(oracle.reqEvalCalls[0].qexpr.tag).toBe("App");

    // Verify the runtime responded with RespVal
    expect(oracle.gotResponses.length).toBe(1);
    expect(oracle.gotResponses[0].tag).toBe("RespVal");
    expect((oracle.gotResponses[0] as any).value.tag).toBe("Num");
    expect((oracle.gotResponses[0] as any).value.n).toBe(42);

    // Verify the final result
    expect(result.tag).toBe("Num");
    expect((result as any).n).toBe(42);
  });

  it("R2: ReqEval can evaluate arbitrary expressions", async () => {
    // Custom oracle that evaluates a more complex expression
    class ComplexEvalOracle implements OracleAdapter {
      startSession(init: OracleInit): OracleSession {
        return (async function* (): OracleSession {
          // Evaluate: (+ (+ 1 2) (+ 3 4)) = 10
          const qexpr: Expr = {
            tag: "App",
            fn: { tag: "Var", name: "+" },
            args: [
              { tag: "App", fn: { tag: "Var", name: "+" }, args: [{ tag: "Lit", value: 1 }, { tag: "Lit", value: 2 }] },
              { tag: "App", fn: { tag: "Var", name: "+" }, args: [{ tag: "Lit", value: 3 }, { tag: "Lit", value: 4 }] },
            ],
          } as any;

          const resp: OracleResp = yield { tag: "ReqEval", qexpr, envRef: init.envRef };
          if (resp.tag !== "RespVal") throw new Error("Expected RespVal");

          const meaning: Meaning = { tag: "Meaning", denotation: resp.value, confidence: 1.0 };
          yield { tag: "ReqReturn", result: meaning };
          return meaning;
        })();
      }
    }

    const oracle = new ComplexEvalOracle();
    const snapshots = new SnapshotRepo();
    const receipts = new InMemoryReceiptStore("off");
    const runtime = new RuntimeImpl(oracle, snapshots, receipts, mockCommit);

    const result = await runToCompletion(runtime, initialState(`
      (effect infer.op "compute")
    `), 10000);

    expect(result.tag).toBe("Num");
    expect((result as any).n).toBe(10); // (1+2) + (3+4) = 10
  });

  it("R3: Oracle can use primitives from the environment", async () => {
    // NOTE: let-bound variables get hygienic renaming (x -> x$bid#1),
    // so the Oracle can't access them by source name directly.
    // But Oracle CAN access primitives (+, -, etc.) and top-level defines.
    //
    // This test verifies Oracle can call primitives with computed values.
    class ComputeOracle implements OracleAdapter {
      startSession(init: OracleInit): OracleSession {
        return (async function* (): OracleSession {
          // Compute (+ 50 51) = 101 using primitives from env
          const qexpr: Expr = {
            tag: "App",
            fn: { tag: "Var", name: "+" },
            args: [{ tag: "Lit", value: 50 }, { tag: "Lit", value: 51 }],
          } as any;

          const resp: OracleResp = yield { tag: "ReqEval", qexpr, envRef: init.envRef };
          if (resp.tag !== "RespVal") throw new Error("Expected RespVal");

          const meaning: Meaning = { tag: "Meaning", denotation: resp.value, confidence: 1.0 };
          yield { tag: "ReqReturn", result: meaning };
          return meaning;
        })();
      }
    }

    const oracle = new ComputeOracle();
    const snapshots = new SnapshotRepo();
    const receipts = new InMemoryReceiptStore("off");
    const runtime = new RuntimeImpl(oracle, snapshots, receipts, mockCommit);

    const result = await runToCompletion(runtime, initialState(`
      (effect infer.op "compute")
    `), 10000);

    expect(result.tag).toBe("Num");
    expect((result as any).n).toBe(101);
  });

  it("R4: Oracle can apply native functions via ReqApply", async () => {
    // Test ReqApply with a native function (primitives don't get renamed)
    class ApplyOracle implements OracleAdapter {
      startSession(init: OracleInit): OracleSession {
        return (async function* (): OracleSession {
          // 1. Get the '+' function from env
          const getPlus: Expr = { tag: "Var", name: "+" } as any;
          const resp1: OracleResp = yield { tag: "ReqEval", qexpr: getPlus, envRef: init.envRef };
          if (resp1.tag !== "RespVal") throw new Error("Expected RespVal for +");

          // 2. Apply it to (21, 21) via ReqApply
          const resp2: OracleResp = yield {
            tag: "ReqApply",
            fn: resp1.value,
            args: [{ tag: "Num", n: 21 }, { tag: "Num", n: 21 }],
            envRef: init.envRef
          };
          if (resp2.tag !== "RespVal") throw new Error("Expected RespVal for apply");

          const meaning: Meaning = { tag: "Meaning", denotation: resp2.value, confidence: 1.0 };
          yield { tag: "ReqReturn", result: meaning };
          return meaning;
        })();
      }
    }

    const oracle = new ApplyOracle();
    const snapshots = new SnapshotRepo();
    const receipts = new InMemoryReceiptStore("off");
    const runtime = new RuntimeImpl(oracle, snapshots, receipts, mockCommit);

    const result = await runToCompletion(runtime, initialState(`
      (effect infer.op "apply +")
    `), 10000);

    expect(result.tag).toBe("Num");
    expect((result as any).n).toBe(42); // + 21 21 = 42
  });

  it("R5: Oracle can do multiple re-entries in one session", async () => {
    // Oracle does multiple ReqEval calls before returning
    class MultiTurnOracle implements OracleAdapter {
      public turns = 0;

      startSession(init: OracleInit): OracleSession {
        const self = this;
        return (async function* (): OracleSession {
          // Turn 1: evaluate (+ 1 2)
          self.turns++;
          const resp1: OracleResp = yield {
            tag: "ReqEval",
            qexpr: { tag: "App", fn: { tag: "Var", name: "+" }, args: [{ tag: "Lit", value: 1 }, { tag: "Lit", value: 2 }] } as any,
            envRef: init.envRef
          };

          // Turn 2: evaluate (+ 3 4)
          self.turns++;
          const resp2: OracleResp = yield {
            tag: "ReqEval",
            qexpr: { tag: "App", fn: { tag: "Var", name: "+" }, args: [{ tag: "Lit", value: 3 }, { tag: "Lit", value: 4 }] } as any,
            envRef: init.envRef
          };

          // Turn 3: add the results together
          self.turns++;
          const a = resp1.tag === "RespVal" ? (resp1.value as any).n : 0;
          const b = resp2.tag === "RespVal" ? (resp2.value as any).n : 0;
          const resp3: OracleResp = yield {
            tag: "ReqEval",
            qexpr: { tag: "App", fn: { tag: "Var", name: "+" }, args: [{ tag: "Lit", value: a }, { tag: "Lit", value: b }] } as any,
            envRef: init.envRef
          };

          if (resp3.tag !== "RespVal") throw new Error("Expected RespVal");
          const meaning: Meaning = { tag: "Meaning", denotation: resp3.value, confidence: 1.0 };
          yield { tag: "ReqReturn", result: meaning };
          return meaning;
        })();
      }
    }

    const oracle = new MultiTurnOracle();
    const snapshots = new SnapshotRepo();
    const receipts = new InMemoryReceiptStore("off");
    const runtime = new RuntimeImpl(oracle, snapshots, receipts, mockCommit);

    const result = await runToCompletion(runtime, initialState(`
      (effect infer.op "multi-turn")
    `), 10000);

    expect(oracle.turns).toBe(3); // Three ReqEval calls
    expect(result.tag).toBe("Num");
    expect((result as any).n).toBe(10); // (1+2) + (3+4) = 10
  });
});
