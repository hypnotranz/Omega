// Test OracleProc - First-class inference procedures (SICP principles)
// These tests prove that OracleProc works like a first-class procedure:
// - Callable like normal functions (C1)
// - Works with higher-order functions and recursion (C2)
// - Supports currying (C3)
// - Can re-enter the evaluator via ReqEval/ReqApply (C4)
// - Supports multi-shot rollback via snapshot/hydrate (C5)

import { describe, it, expect } from "vitest";
import { COWStore } from "../../src/core/eval/store";
import { RuntimeImpl } from "../../src/core/effects/runtimeImpl";
import { ScriptedOracleAdapter } from "../../src/core/oracle/scriptedOracle";
import { SnapshotRepo } from "../../src/core/oracle/snapshots";
import { InMemoryReceiptStore } from "../../src/core/oracle/receipts";
import { mockCommit } from "../helpers/runtime";
import { installPrims } from "../helpers/prims";
import type { State } from "../../src/core/eval/machine";
import { runToCompletion } from "../../src/core/eval/run";
import { compileTextToExpr } from "../../src/core/pipeline/compileText";
import type { Val } from "../../src/core/eval/values";

function createTestEnv() {
  const oracle = new ScriptedOracleAdapter();
  const snapshots = new SnapshotRepo();
  const receipts = new InMemoryReceiptStore("off");
  const runtime = new RuntimeImpl(oracle, snapshots, receipts, mockCommit);
  return { runtime, oracle, snapshots, receipts };
}

function initialState(src: string): State {
  const expr = compileTextToExpr(src);
  const store0 = new COWStore();
  const prim = installPrims(store0);
  return { control: { tag: "Expr", e: expr }, env: prim.env, store: prim.store, kont: [], handlers: [] };
}

async function evalOmega(src: string): Promise<Val> {
  const { runtime } = createTestEnv();
  const st0 = initialState(src);
  return runToCompletion(runtime, st0);
}

describe("OracleProc - SICP Principles for Inference", () => {

  describe("C1: OracleProc is first-class and callable", () => {
    it("oracle-lambda creates a callable OracleProc", async () => {
      // Define an oracle procedure that adds 1 to its argument
      const result = await evalOmega(`
        (begin
          (define smart-add1
            (oracle-lambda (x)
              "apply-add1"))
          (smart-add1 5))
      `);

      expect(result.tag).toBe("Num");
      expect((result as { tag: "Num"; n: number }).n).toBe(6);
    });

    it("oracle-lambda captures spec value", async () => {
      // The spec can be any expression that evaluates to a value
      const result = await evalOmega(`
        (begin
          (define smart-add1
            (oracle-lambda (x)
              (quote apply-add1)))
          (smart-add1 10))
      `);

      expect(result.tag).toBe("Num");
      expect((result as { tag: "Num"; n: number }).n).toBe(11);
    });
  });

  describe("C2: Higher-order + recursion over OracleProc", () => {
    it("fold-n works with OracleProc", async () => {
      // Define fold-n that applies a function n times
      // fold-n smart-add1 0 10 should give 10
      const result = await evalOmega(`
        (begin
          (define (fold-n f acc n)
            (if (= n 0)
                acc
                (fold-n f (f acc) (- n 1))))

          (define smart-add1
            (oracle-lambda (x) "apply-add1"))

          (fold-n smart-add1 0 10))
      `);

      expect(result.tag).toBe("Num");
      expect((result as { tag: "Num"; n: number }).n).toBe(10);
    });

    it("OracleProc can be passed as argument to HOF", async () => {
      // A simple apply-twice function
      const result = await evalOmega(`
        (begin
          (define (apply-twice f x)
            (f (f x)))

          (define smart-add1
            (oracle-lambda (x) "apply-add1"))

          (apply-twice smart-add1 5))
      `);

      expect(result.tag).toBe("Num");
      expect((result as { tag: "Num"; n: number }).n).toBe(7);
    });
  });

  describe("C3: Currying works with OracleProc", () => {
    it("curry2 works with OracleProc", async () => {
      // Define a curry2 function and use it with a 2-arg oracle
      const result = await evalOmega(`
        (begin
          (define (curry2 f a)
            (lambda (b) (f a b)))

          (define smart-add
            (oracle-lambda (x y) "apply-add"))

          ((curry2 smart-add 2) 3))
      `);

      expect(result.tag).toBe("Num");
      expect((result as { tag: "Num"; n: number }).n).toBe(5);
    });

    it("partial application via closure over OracleProc", async () => {
      // Create a partial application
      const result = await evalOmega(`
        (begin
          (define smart-add
            (oracle-lambda (x y) "apply-add"))

          (define add-10
            (lambda (y) (smart-add 10 y)))

          (add-10 7))
      `);

      expect(result.tag).toBe("Num");
      expect((result as { tag: "Num"; n: number }).n).toBe(17);
    });
  });

  describe("C4: Re-entrant evaluator use from oracle", () => {
    it("oracle uses ReqEval and ReqApply", async () => {
      // The oracle will:
      // 1. ReqEval '+' to get the plus function
      // 2. ReqApply that function to [x, 1]
      const result = await evalOmega(`
        (begin
          (define smart-add1
            (oracle-lambda (x) "must-reenter-eval"))
          (smart-add1 41))
      `);

      expect(result.tag).toBe("Num");
      expect((result as { tag: "Num"; n: number }).n).toBe(42);
    });

    it("re-entrant eval works in nested context", async () => {
      // Apply smart-add1 inside another function
      const result = await evalOmega(`
        (begin
          (define smart-add1
            (oracle-lambda (x) "must-reenter-eval"))

          (define (use-oracle n)
            (+ (smart-add1 n) 100))

          (use-oracle 10))
      `);

      expect(result.tag).toBe("Num");
      expect((result as { tag: "Num"; n: number }).n).toBe(111); // 10 + 1 + 100
    });
  });

  describe("C5: Multi-shot rollback (snapshot/hydrate)", () => {
    it("oracle can take snapshot and hydrate back", async () => {
      // The repair-loop oracle will:
      // 1. ReqSnapshot to save state
      // 2. Attempt wrong computation
      // 3. ReqHydrate to roll back
      // 4. Compute correctly
      const result = await evalOmega(`
        (begin
          (define smart-add1
            (oracle-lambda (x) "repair-loop"))
          (smart-add1 5))
      `);

      expect(result.tag).toBe("Num");
      expect((result as { tag: "Num"; n: number }).n).toBe(6);
    });
  });

  describe("OracleProc is a first-class value", () => {
    it("can be stored in variables and retrieved", async () => {
      const result = await evalOmega(`
        (begin
          (define f (oracle-lambda (x) "apply-add1"))
          (define g f)
          (g 100))
      `);

      expect(result.tag).toBe("Num");
      expect((result as { tag: "Num"; n: number }).n).toBe(101);
    });

    it("can be returned from a function", async () => {
      const result = await evalOmega(`
        (begin
          (define (make-adder)
            (oracle-lambda (x) "apply-add1"))

          (define adder (make-adder))
          (adder 50))
      `);

      expect(result.tag).toBe("Num");
      expect((result as { tag: "Num"; n: number }).n).toBe(51);
    });

    it("closure captures lexical scope", async () => {
      // The oracle-lambda captures its definition environment
      const result = await evalOmega(`
        (begin
          (define (make-oracle-with-offset n)
            (oracle-lambda (x) "apply-add1"))

          (define add1 (make-oracle-with-offset 100))
          (add1 5))
      `);

      expect(result.tag).toBe("Num");
      expect((result as { tag: "Num"; n: number }).n).toBe(6);
    });
  });
});
