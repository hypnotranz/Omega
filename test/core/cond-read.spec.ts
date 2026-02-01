/**
 * ═══════════════════════════════════════════════════════════════════════════
 * COND and READ Tests
 * ═══════════════════════════════════════════════════════════════════════════
 *
 * Tests for the `cond` special form and `read` primitive.
 * These are essential for the README's flagship `solve` function example.
 *
 * - `cond`: Multi-way conditional (desugars to nested if)
 * - `read`: Parse string to s-expression (enables homoiconicity)
 * ═══════════════════════════════════════════════════════════════════════════
 */

import { describe, it, expect } from "vitest";
import { COWStore } from "../../src/core/eval/store";
import { RuntimeImpl } from "../../src/core/effects/runtimeImpl";
import { ScriptedOracleAdapter } from "../../src/core/oracle/scriptedOracle";
import { SnapshotRepo } from "../../src/core/oracle/snapshots";
import { InMemoryReceiptStore } from "../../src/core/oracle/receipts";
import { mockCommit } from "../helpers/runtime";
import { installPrims } from "../../src/core/prims";
import type { State } from "../../src/core/eval/machine";
import { runToCompletion } from "../../src/core/eval/run";
import { compileTextToExpr } from "../../src/core/pipeline/compileText";
import type { Val } from "../../src/core/eval/values";
import type { Profile } from "../../src/core/governance/profile";
import { readForms } from "../../src/core/compiler/reader";
import { desugarForm } from "../../src/core/compiler/desugar";

// ═══════════════════════════════════════════════════════════════════════════
// Test Harness
// ═══════════════════════════════════════════════════════════════════════════

const fullProfile: Profile = {
  name: "cond-read-test",
  caps: ["*"],
  budgets: {
    maxOracleTurns: 100,
    maxEvalSteps: 100_000,
    maxToolCalls: 100,
  },
  truth: "speculative",
};

function createTestEnv() {
  const oracle = new ScriptedOracleAdapter();
  const snapshots = new SnapshotRepo();
  const receipts = new InMemoryReceiptStore("off");
  const runtime = new RuntimeImpl(oracle, snapshots, receipts, mockCommit, fullProfile);
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

// Helper extractors
function getNum(v: Val): number {
  if (v.tag === "Num") return v.n;
  throw new Error(`Expected Num, got ${v.tag}`);
}

function getStr(v: Val): string {
  if (v.tag === "Str") return v.s;
  throw new Error(`Expected Str, got ${v.tag}`);
}

function getBool(v: Val): boolean {
  if (v.tag === "Bool") return v.b;
  throw new Error(`Expected Bool, got ${v.tag}`);
}

function getSym(v: Val): string {
  if (v.tag === "Sym") return v.name;
  throw new Error(`Expected Sym, got ${v.tag}`);
}

function getVector(v: Val): Val[] {
  if (v.tag === "Vector") return v.items;
  throw new Error(`Expected Vector, got ${v.tag}`);
}

// ═══════════════════════════════════════════════════════════════════════════
// Note: The desugar.ts path is for a different compilation pipeline.
// The REPL uses compileTextToExpr → lower.ts, which is tested via evalOmega.
// ═══════════════════════════════════════════════════════════════════════════

// ═══════════════════════════════════════════════════════════════════════════
// COND Integration Tests (Full Evaluation)
// ═══════════════════════════════════════════════════════════════════════════

describe("cond evaluation", () => {
  it("returns #f for empty cond", async () => {
    const result = await evalOmega("(cond)");
    expect(getBool(result)).toBe(false);
  });

  it("evaluates single true clause", async () => {
    const result = await evalOmega("(cond (#t 42))");
    expect(getNum(result)).toBe(42);
  });

  it("evaluates single false clause to #f", async () => {
    const result = await evalOmega("(cond (#f 42))");
    expect(getBool(result)).toBe(false);
  });

  it("evaluates else clause when no test matches", async () => {
    const result = await evalOmega("(cond (#f 1) (#f 2) (else 999))");
    expect(getNum(result)).toBe(999);
  });

  it("evaluates first matching clause", async () => {
    const result = await evalOmega(`
      (cond
        (#f 1)
        (#t 2)
        (#t 3))
    `);
    expect(getNum(result)).toBe(2);
  });

  it("evaluates cond with equality tests", async () => {
    const result = await evalOmega(`
      (let ((x 2))
        (cond
          ((= x 1) "one")
          ((= x 2) "two")
          ((= x 3) "three")
          (else "other")))
    `);
    expect(getStr(result)).toBe("two");
  });

  it("evaluates cond with string equality", async () => {
    const result = await evalOmega(`
      (let ((strategy "decompose"))
        (cond
          ((equal? strategy "direct") 100)
          ((equal? strategy "decompose") 200)
          (else 0)))
    `);
    expect(getNum(result)).toBe(200);
  });

  it("evaluates multi-expression body with begin semantics", async () => {
    const result = await evalOmega(`
      (cond
        (#t (+ 1 2) (+ 3 4) (+ 5 6)))
    `);
    expect(getNum(result)).toBe(11); // Last expression
  });

  it("works with nested cond", async () => {
    const result = await evalOmega(`
      (cond
        (#t (cond
              (#f 1)
              (#t 2))))
    `);
    expect(getNum(result)).toBe(2);
  });

  it("works with let and define", async () => {
    const result = await evalOmega(`
      (let ((classify (lambda (n)
              (cond
                ((< n 0) "negative")
                ((= n 0) "zero")
                (else "positive")))))
        (classify 5))
    `);
    expect(getStr(result)).toBe("positive");
  });
});

// ═══════════════════════════════════════════════════════════════════════════
// READ Unit Tests
// ═══════════════════════════════════════════════════════════════════════════

describe("read primitive", () => {
  it("parses a number", async () => {
    const result = await evalOmega('(read "42")');
    expect(getNum(result)).toBe(42);
  });

  it("parses a negative number", async () => {
    const result = await evalOmega('(read "-17")');
    expect(getNum(result)).toBe(-17);
  });

  it("parses a float", async () => {
    const result = await evalOmega('(read "3.14")');
    expect(getNum(result)).toBeCloseTo(3.14);
  });

  it("parses a string", async () => {
    const result = await evalOmega('(read "\\"hello\\"")');
    expect(getStr(result)).toBe("hello");
  });

  it("parses #t as true", async () => {
    const result = await evalOmega('(read "#t")');
    expect(getBool(result)).toBe(true);
  });

  it("parses #f as false", async () => {
    const result = await evalOmega('(read "#f")');
    expect(getBool(result)).toBe(false);
  });

  it("parses a symbol", async () => {
    const result = await evalOmega('(read "foo")');
    expect(getSym(result)).toBe("foo");
  });

  it("parses a list into a vector", async () => {
    const result = await evalOmega('(read "(1 2 3)")');
    const items = getVector(result);
    expect(items.length).toBe(3);
    expect(getNum(items[0])).toBe(1);
    expect(getNum(items[1])).toBe(2);
    expect(getNum(items[2])).toBe(3);
  });

  it("parses nested lists", async () => {
    const result = await evalOmega('(read "(a (b c) d)")');
    const items = getVector(result);
    expect(items.length).toBe(3);
    expect(getSym(items[0])).toBe("a");
    const inner = getVector(items[1]);
    expect(inner.length).toBe(2);
    expect(getSym(inner[0])).toBe("b");
    expect(getSym(inner[1])).toBe("c");
    expect(getSym(items[2])).toBe("d");
  });

  it("parses (list ...) form", async () => {
    const result = await evalOmega('(read "(list 1 2 3)")');
    const items = getVector(result);
    expect(items.length).toBe(4);
    expect(getSym(items[0])).toBe("list");
    expect(getNum(items[1])).toBe(1);
    expect(getNum(items[2])).toBe(2);
    expect(getNum(items[3])).toBe(3);
  });

  it("parses empty list", async () => {
    const result = await evalOmega('(read "()")');
    const items = getVector(result);
    expect(items.length).toBe(0);
  });

  it("parses quoted expression", async () => {
    const result = await evalOmega("(read \"'foo\")");
    const items = getVector(result);
    expect(items.length).toBe(2);
    expect(getSym(items[0])).toBe("quote");
    expect(getSym(items[1])).toBe("foo");
  });

  it("throws on invalid input", async () => {
    await expect(evalOmega('(read "(unclosed")')).rejects.toThrow();
  });

  it("throws on non-string argument", async () => {
    await expect(evalOmega("(read 42)")).rejects.toThrow("read: expected string");
  });
});

// ═══════════════════════════════════════════════════════════════════════════
// Combined cond + read Integration Tests (README example support)
// ═══════════════════════════════════════════════════════════════════════════

describe("cond and read integration", () => {
  it("can use read result in cond", async () => {
    const result = await evalOmega(`
      (let ((x (read "42")))
        (cond
          ((= x 41) "wrong")
          ((= x 42) "correct")
          (else "unknown")))
    `);
    expect(getStr(result)).toBe("correct");
  });

  it("can parse and branch on string values", async () => {
    const result = await evalOmega(`
      (let ((strategy (read "\\"direct\\"")))
        (cond
          ((equal? strategy "direct") 100)
          ((equal? strategy "decompose") 200)
          (else 0)))
    `);
    expect(getNum(result)).toBe(100);
  });

  it("can use read with nested let", async () => {
    const result = await evalOmega(`
      (let ((data (read "(1 2 3)")))
        (let ((first (vector-ref data 0)))
          (let ((second (vector-ref data 1)))
            (+ first second))))
    `);
    expect(getNum(result)).toBe(3);
  });

  it("supports the README decompose pattern (parsing list output)", async () => {
    // Simulates what happens when LLM returns "(list \\"sub1\\" \\"sub2\\" \\"sub3\\")"
    const result = await evalOmega(`
      (let ((llm-output (read "(list \\"sub1\\" \\"sub2\\" \\"sub3\\")")))
        ;; llm-output is now a vector: [list "sub1" "sub2" "sub3"]
        (vector-ref llm-output 1))  ;; Get first subproblem string
    `);
    expect(getStr(result)).toBe("sub1");
  });
});

// ═══════════════════════════════════════════════════════════════════════════
// Edge Cases and Error Handling
// ═══════════════════════════════════════════════════════════════════════════

describe("edge cases", () => {
  it("cond with boolean test and no body returns test result", async () => {
    // OmegaLLM requires boolean tests (stricter than Scheme's truthy/falsy)
    // (cond (#t)) should return #t since there's no body
    const result = await evalOmega("(cond (#t))");
    expect(getBool(result)).toBe(true);
  });

  it("cond rejects non-boolean test", async () => {
    // Unlike Scheme, OmegaLLM requires strictly boolean tests
    await expect(evalOmega("(cond (42))")).rejects.toThrow();
  });

  it("read handles strings with escapes", async () => {
    const result = await evalOmega('(read "\\"hello\\\\nworld\\"")');
    expect(getStr(result)).toBe("hello\nworld");
  });

  it("read handles complex nested structure", async () => {
    const result = await evalOmega('(read "((a 1) (b 2) (c 3))")');
    const items = getVector(result);
    expect(items.length).toBe(3);
    // Each item should be a 2-element vector
    const first = getVector(items[0]);
    expect(first.length).toBe(2);
    expect(getSym(first[0])).toBe("a");
    expect(getNum(first[1])).toBe(1);
  });
});
