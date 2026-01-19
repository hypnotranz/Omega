// test/live/trace-demo.spec.ts
// Demonstrates REAL execution traces - showing step-by-step what the machine does

import { describe, it, expect } from "vitest";
import { COWStore } from "../../src/core/eval/store";
import { RuntimeImpl } from "../../src/core/effects/runtimeImpl";
import { SnapshotRepo } from "../../src/core/oracle/snapshots";
import { InMemoryReceiptStore } from "../../src/core/oracle/receipts";
import { installPrims } from "../helpers/prims";
import { compileTextToExpr } from "../../src/core/pipeline/compileText";
import { runWithTrace, formatTrace } from "../../src/core/eval/run";
import type { State } from "../../src/core/eval/machine";
import { VUnit } from "../../src/core/eval/values";
import type { OracleAdapter, InferInit, ApplyInit } from "../../src/core/oracle/adapter";
import type { OracleSession, OracleResp } from "../../src/core/oracle/protocol";
import type { MeaningVal } from "../../src/core/oracle/meaning";

// Oracle that captures the stack and returns immediately
class TracingOracle implements OracleAdapter {
  callCount = 0;
  capturedStacks: any[] = [];

  startSession(init: InferInit | ApplyInit): OracleSession {
    const self = this;
    self.callCount++;

    return (async function* (): OracleSession {
      // Observe the stack
      const stackResp: OracleResp = yield {
        tag: "ReqObserve",
        what: { tag: "Stack" },
        stateRef: init.stateRef,
      };
      self.capturedStacks.push((stackResp as any).data);

      // Return simple meaning
      const meaning: MeaningVal = {
        tag: "Meaning",
        denotation: { tag: "Str", s: "oracle-done" },
        confidence: 1,
      };
      return meaning;
    })();
  }
}

describe("Execution Traces - REAL step-by-step output", () => {
  it("traces simple arithmetic: (+ 1 2)", async () => {
    const oracle = new TracingOracle();
    const snapshots = new SnapshotRepo();
    const receipts = new InMemoryReceiptStore("off");
    const runtime = new RuntimeImpl(oracle, snapshots, receipts, {
      async commit() { return VUnit; }
    });

    const store0 = new COWStore();
    const prim = installPrims(store0);

    const src = "(+ 1 2)";
    const expr = compileTextToExpr(src);
    const state0: State = {
      control: { tag: "Expr", e: expr },
      env: prim.env,
      store: prim.store,
      kont: [],
      handlers: [],
    };

    const { value, trace } = await runWithTrace(runtime, state0, 1000);

    console.log("\n=== TRACE: (+ 1 2) ===");
    console.log(formatTrace(trace, { compact: true }));
    console.log("Result:", value);

    expect(value.tag).toBe("Num");
    expect((value as any).n).toBe(3);
    expect(trace.length).toBeGreaterThan(0);
  });

  it("traces function call with stack frames", async () => {
    const oracle = new TracingOracle();
    const snapshots = new SnapshotRepo();
    const receipts = new InMemoryReceiptStore("off");
    const runtime = new RuntimeImpl(oracle, snapshots, receipts, {
      async commit() { return VUnit; }
    });

    const store0 = new COWStore();
    const prim = installPrims(store0);

    const src = `
      (begin
        (define (double x) (* x 2))
        (double 21))
    `;
    const expr = compileTextToExpr(src);
    const state0: State = {
      control: { tag: "Expr", e: expr },
      env: prim.env,
      store: prim.store,
      kont: [],
      handlers: [],
    };

    const { value, trace } = await runWithTrace(runtime, state0, 1000);

    console.log("\n=== TRACE: (double 21) ===");
    console.log(formatTrace(trace, { compact: true }));
    console.log("Result:", value);

    // Find the max stack depth during execution
    const maxStack = Math.max(...trace.map(e => e.stackDepth));
    console.log("Max stack depth:", maxStack);

    expect(value.tag).toBe("Num");
    expect((value as any).n).toBe(42);
    expect(maxStack).toBeGreaterThan(0); // Should have frames during call
  });

  it("traces recursion and shows growing stack", async () => {
    const oracle = new TracingOracle();
    const snapshots = new SnapshotRepo();
    const receipts = new InMemoryReceiptStore("off");
    const runtime = new RuntimeImpl(oracle, snapshots, receipts, {
      async commit() { return VUnit; }
    });

    const store0 = new COWStore();
    const prim = installPrims(store0);

    const src = `
      (begin
        (define (fact n)
          (if (<= n 1)
              1
              (* n (fact (- n 1)))))
        (fact 5))
    `;
    const expr = compileTextToExpr(src);
    const state0: State = {
      control: { tag: "Expr", e: expr },
      env: prim.env,
      store: prim.store,
      kont: [],
      handlers: [],
    };

    const { value, trace } = await runWithTrace(runtime, state0, 10000);

    console.log("\n=== TRACE: (fact 5) ===");
    // Show abbreviated trace - first 20, last 10
    const compactTrace = formatTrace(trace, { compact: true }).split("\n");
    console.log("First 20 steps:");
    console.log(compactTrace.slice(0, 20).join("\n"));
    console.log(`... (${trace.length - 30} steps omitted) ...`);
    console.log("Last 10 steps:");
    console.log(compactTrace.slice(-10).join("\n"));

    console.log(`\nTotal steps: ${trace.length}`);
    console.log("Result:", value);

    // Track stack depth over time
    const stackDepths = trace.map(e => e.stackDepth);
    const maxStack = Math.max(...stackDepths);
    console.log("Max stack depth:", maxStack);

    expect(value.tag).toBe("Num");
    expect((value as any).n).toBe(120); // 5! = 120
    expect(maxStack).toBeGreaterThan(5); // Deep recursion
  });

  it("traces infer.op and shows stack at oracle call", async () => {
    const oracle = new TracingOracle();
    const snapshots = new SnapshotRepo();
    const receipts = new InMemoryReceiptStore("off");
    const runtime = new RuntimeImpl(oracle, snapshots, receipts, {
      async commit() { return VUnit; }
    });

    const store0 = new COWStore();
    const prim = installPrims(store0);

    const src = `
      (begin
        (define (ask-oracle question)
          (effect infer.op question))
        (ask-oracle "What is life?"))
    `;
    const expr = compileTextToExpr(src);
    const state0: State = {
      control: { tag: "Expr", e: expr },
      env: prim.env,
      store: prim.store,
      kont: [],
      handlers: [],
    };

    const { value, trace } = await runWithTrace(runtime, state0, 1000);

    console.log("\n=== TRACE: (ask-oracle ...) -> infer.op ===");
    console.log(formatTrace(trace, { compact: true }));

    // Find where the effect was triggered
    const effectStep = trace.find(e => e.opName === "infer.op");
    console.log("\nEffect triggered at step:", effectStep?.step);
    console.log("Stack depth at effect:", effectStep?.stackDepth);
    console.log("Stack frames at effect:", effectStep?.stackTags);

    // Show what the oracle saw
    console.log("\nOracle captured stack:", JSON.stringify(oracle.capturedStacks[0], null, 2));

    expect(oracle.callCount).toBe(1);
    expect(effectStep).toBeDefined();
    expect(effectStep!.stackDepth).toBeGreaterThan(0); // Stack is non-empty at effect
  });

  it("traces nested function calls with oracle", async () => {
    const oracle = new TracingOracle();
    const snapshots = new SnapshotRepo();
    const receipts = new InMemoryReceiptStore("off");
    const runtime = new RuntimeImpl(oracle, snapshots, receipts, {
      async commit() { return VUnit; }
    });

    const store0 = new COWStore();
    const prim = installPrims(store0);

    const src = `
      (begin
        (define (level3) (effect infer.op "deep"))
        (define (level2) (level3))
        (define (level1) (level2))
        (level1))
    `;
    const expr = compileTextToExpr(src);
    const state0: State = {
      control: { tag: "Expr", e: expr },
      env: prim.env,
      store: prim.store,
      kont: [],
      handlers: [],
    };

    const { value, trace } = await runWithTrace(runtime, state0, 1000);

    console.log("\n=== TRACE: level1 -> level2 -> level3 -> infer.op ===");
    console.log(formatTrace(trace, { compact: true }));

    const effectStep = trace.find(e => e.opName === "infer.op");
    console.log("\nEffect triggered at step:", effectStep?.step);
    console.log("Stack depth at effect:", effectStep?.stackDepth);
    console.log("Stack frames:", effectStep?.stackTags);

    console.log("\nOracle saw stack:", JSON.stringify(oracle.capturedStacks[0], null, 2));

    expect(oracle.capturedStacks[0].depth).toBeGreaterThan(3); // Nested calls
  });
});
