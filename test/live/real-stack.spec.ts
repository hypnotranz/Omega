// test/live/real-stack.spec.ts
// Test the REAL stack observation when Lisp calls infer.op

import { describe, it, expect } from "vitest";
import { COWStore } from "../../src/core/eval/store";
import { RuntimeImpl } from "../../src/core/effects/runtimeImpl";
import { SnapshotRepo } from "../../src/core/oracle/snapshots";
import { InMemoryReceiptStore } from "../../src/core/oracle/receipts";
import { installPrims } from "../helpers/prims";
import { compileTextToExpr } from "../../src/core/pipeline/compileText";
import { runToCompletion } from "../../src/core/eval/run";
import type { State } from "../../src/core/eval/machine";
import { VUnit } from "../../src/core/eval/values";
import type { OracleAdapter, InferInit, ApplyInit } from "../../src/core/oracle/adapter";
import type { OracleSession, OracleReq, OracleResp, OracleReturn } from "../../src/core/oracle/protocol";
import type { MeaningVal } from "../../src/core/oracle/meaning";

// Custom oracle that captures the stack when invoked
class StackCapturingOracle implements OracleAdapter {
  capturedStacks: any[] = [];
  capturedStateRefs: string[] = [];

  startSession(init: InferInit | ApplyInit): OracleSession {
    const self = this;

    // Capture the stateRef from init
    this.capturedStateRefs.push(init.stateRef as string);

    return (async function* (): OracleSession {
      // First, observe the stack to see if it's non-empty
      const stackResp: OracleResp = yield {
        tag: "ReqObserve",
        what: { tag: "Stack" },
        stateRef: init.stateRef,
      };

      console.log("Oracle observed stack:", JSON.stringify((stackResp as any).data, null, 2));
      self.capturedStacks.push((stackResp as any).data);

      // Also observe control to see where we paused
      const controlResp: OracleResp = yield {
        tag: "ReqObserve",
        what: { tag: "Control" },
        stateRef: init.stateRef,
      };
      console.log("Oracle observed control:", JSON.stringify((controlResp as any).data, null, 2));

      // Return a simple meaning
      const meaning: MeaningVal = {
        tag: "Meaning",
        denotation: { tag: "Str", s: "oracle-response" },
        confidence: 1,
      };

      return meaning;
    })();
  }
}

describe("Real Stack Observation from Lisp→LLM flow", () => {
  it("LLM sees non-empty stack when Lisp calls infer.op", async () => {
    const oracle = new StackCapturingOracle();
    const snapshots = new SnapshotRepo();
    const receipts = new InMemoryReceiptStore("off");

    const runtime = new RuntimeImpl(oracle, snapshots, receipts, {
      async commit() { return VUnit; }
    });

    const store0 = new COWStore();
    const prim = installPrims(store0);

    // Lisp code that:
    // 1. Defines a function
    // 2. Calls the function
    // 3. Inside the function, calls infer.op
    // This should create a REAL stack (function call frame)
    const src = `
      (begin
        (define (ask-llm question)
          (effect infer.op question))
        (ask-llm "What is the answer?"))
    `;

    const expr = compileTextToExpr(src);
    const state0: State = {
      control: { tag: "Expr", e: expr },
      env: prim.env,
      store: prim.store,
      kont: [],
      handlers: [],
    };

    const result = await runToCompletion(runtime, state0, 100_000);
    console.log("Final result:", result);

    // Verify oracle was called
    expect(oracle.capturedStacks.length).toBe(1);

    // Verify the stack was NOT empty when oracle was invoked
    const capturedStack = oracle.capturedStacks[0];
    console.log("Captured stack depth:", capturedStack.depth);
    console.log("Captured frames:", capturedStack.frames);

    // THIS IS THE KEY TEST:
    // When infer.op is triggered mid-execution, the stack should be non-empty
    // because we're inside the (ask-llm ...) function call
    expect(capturedStack.depth).toBeGreaterThan(0);
  });

  it("LLM sees nested stack when Lisp has deep recursion", async () => {
    const oracle = new StackCapturingOracle();
    const snapshots = new SnapshotRepo();
    const receipts = new InMemoryReceiptStore("off");

    const runtime = new RuntimeImpl(oracle, snapshots, receipts, {
      async commit() { return VUnit; }
    });

    const store0 = new COWStore();
    const prim = installPrims(store0);

    // Deep recursion that eventually hits infer.op
    const src = `
      (begin
        (define (countdown n)
          (if (<= n 0)
              (effect infer.op "bottom!")
              (+ 1 (countdown (- n 1)))))
        (countdown 5))
    `;

    const expr = compileTextToExpr(src);
    const state0: State = {
      control: { tag: "Expr", e: expr },
      env: prim.env,
      store: prim.store,
      kont: [],
      handlers: [],
    };

    const result = await runToCompletion(runtime, state0, 100_000);
    console.log("Final result:", result);

    expect(oracle.capturedStacks.length).toBe(1);
    const capturedStack = oracle.capturedStacks[0];
    console.log("Captured stack depth:", capturedStack.depth);

    // Should have multiple stack frames from recursion
    // countdown(5) → countdown(4) → countdown(3) → countdown(2) → countdown(1) → countdown(0) → infer.op
    // Each pending (+ 1 ...) should be a frame
    expect(capturedStack.depth).toBeGreaterThan(3);
  });

  it("LLM can call omega_eval and create nested execution", async () => {
    let evalCount = 0;

    // Oracle that calls back into the evaluator
    class CallbackOracle implements OracleAdapter {
      capturedStack: any = null;

      startSession(init: InferInit | ApplyInit): OracleSession {
        const self = this;

        return (async function* (): OracleSession {
          // Observe initial stack
          const stackResp: OracleResp = yield {
            tag: "ReqObserve",
            what: { tag: "Stack" },
            stateRef: init.stateRef,
          };
          self.capturedStack = (stackResp as any).data;
          console.log("Initial stack depth:", self.capturedStack.depth);

          // Call back into the evaluator!
          const evalResp: OracleResp = yield {
            tag: "ReqEval",
            qexpr: "(+ 100 200)",
            envRef: init.envRef,
          };
          evalCount++;
          console.log("Eval result:", (evalResp as any).value);

          const meaning: MeaningVal = {
            tag: "Meaning",
            denotation: (evalResp as any).value ?? { tag: "Num", n: 0 },
            confidence: 1,
          };

          return meaning;
        })();
      }
    }

    const oracle = new CallbackOracle();
    const snapshots = new SnapshotRepo();
    const receipts = new InMemoryReceiptStore("off");

    const runtime = new RuntimeImpl(oracle, snapshots, receipts, {
      async commit() { return VUnit; }
    });

    // Need to create portal with parseText for ReqEval to work
    // But that's handled inside RuntimeImpl, so we need to check if it works
    // Actually the issue is PortalImpl needs parseText option

    const store0 = new COWStore();
    const prim = installPrims(store0);

    const src = `(effect infer.op "do some math")`;

    const expr = compileTextToExpr(src);
    const state0: State = {
      control: { tag: "Expr", e: expr },
      env: prim.env,
      store: prim.store,
      kont: [],
      handlers: [],
    };

    const result = await runToCompletion(runtime, state0, 100_000);
    console.log("Final result:", result);

    expect(evalCount).toBe(1);
    expect(result.tag).toBe("Num");
    expect((result as any).n).toBe(300);
  });
});
