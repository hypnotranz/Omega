// Stream Interleave Tests (Job 009)
// Tests for fair merging of two streams

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
import type { Profile } from "../../src/core/governance/profile";

function createTestEnv(profile?: Profile) {
  const oracle = new ScriptedOracleAdapter();
  const snapshots = new SnapshotRepo();
  const receipts = new InMemoryReceiptStore("off");
  const runtime = new RuntimeImpl(oracle, snapshots, receipts, mockCommit, profile);
  return { runtime, oracle, snapshots, receipts };
}

function initialState(src: string): State {
  const expr = compileTextToExpr(src);
  const store0 = new COWStore();
  const prim = installPrims(store0);
  return { control: { tag: "Expr", e: expr }, env: prim.env, store: prim.store, kont: [], handlers: [] };
}

const fullProfile: Profile = {
  name: "stream-interleave-test",
  caps: ["*"],
  budgets: {
    maxOracleTurns: 1000,
    maxEvalSteps: 500_000,
    maxToolCalls: 1000,
  },
  truth: "speculative",
};

async function evalOmega(src: string, profile?: Profile): Promise<Val> {
  const { runtime } = createTestEnv(profile ?? fullProfile);
  const st0 = initialState(src);
  return runToCompletion(runtime, st0);
}

// Helper to extract list from result
function getList(v: Val): Val[] {
  if (v.tag === "List") return v.elements;
  // Handle cons-cell based lists (Vector with 2 items)
  if (v.tag === "Vector") {
    const result: Val[] = [];
    let current = v;
    while (current.tag === "Vector" && current.items.length >= 2) {
      result.push(current.items[0]);
      current = current.items[1];
    }
    // If there's a non-Unit tail, it's an improper list - ignore for now
    return result;
  }
  throw new Error(`Expected List or Vector, got ${v.tag}`);
}

// Helper to extract number from result
function getNum(v: Val): number {
  if (v.tag === "Num") return v.n;
  if (v.tag === "Int") return Number(v.value);
  throw new Error(`Expected Num or Int, got ${v.tag}`);
}

// Helper to extract symbol from result
function getSym(v: Val): string {
  if (v.tag === "Sym") return v.name;
  throw new Error(`Expected Sym, got ${v.tag}`);
}

// Helper to extract boolean from result
function getBool(v: Val): boolean {
  if (v.tag === "Bool") return v.b;
  throw new Error(`Expected Bool, got ${v.tag}`);
}

describe("stream-interleave", () => {
  // HP-1: Alternates elements from two streams
  it("alternates elements from two finite streams", async () => {
    const result = await evalOmega(`
      (define s1 (cons-stream 1 (cons-stream 2 the-empty-stream)))
      (define s2 (cons-stream 'a (cons-stream 'b the-empty-stream)))

      (define (my-stream->list s n)
        (if (or (= n 0) (stream-null? s))
            '()
            (cons (stream-car s)
                  (my-stream->list (stream-cdr s) (- n 1)))))

      (my-stream->list (stream-interleave s1 s2) 4)
    `);
    const lst = getList(result);
    expect(lst.length).toBe(4);
    expect(getNum(lst[0])).toBe(1);
    expect(getSym(lst[1])).toBe("a");
    expect(getNum(lst[2])).toBe(2);
    expect(getSym(lst[3])).toBe("b");
  });

  // HP-2: Returns other stream if one is empty
  it("returns s2 if s1 is empty", async () => {
    const result = await evalOmega(`
      (define s2 (cons-stream 1 (cons-stream 2 the-empty-stream)))

      (define (my-stream->list2 s n)
        (if (or (= n 0) (stream-null? s))
            '()
            (cons (stream-car s)
                  (my-stream->list2 (stream-cdr s) (- n 1)))))

      (my-stream->list2 (stream-interleave the-empty-stream s2) 2)
    `);
    const lst = getList(result);
    expect(lst.length).toBe(2);
    expect(getNum(lst[0])).toBe(1);
    expect(getNum(lst[1])).toBe(2);
  });

  // HP-3: Returns s1 if s2 is empty
  it("returns s1 if s2 is empty", async () => {
    const result = await evalOmega(`
      (define s1 (cons-stream 1 (cons-stream 2 the-empty-stream)))

      (define (my-stream->list3 s n)
        (if (or (= n 0) (stream-null? s))
            '()
            (cons (stream-car s)
                  (my-stream->list3 (stream-cdr s) (- n 1)))))

      (my-stream->list3 (stream-interleave s1 the-empty-stream) 2)
    `);
    const lst = getList(result);
    expect(lst.length).toBe(2);
    expect(getNum(lst[0])).toBe(1);
    expect(getNum(lst[1])).toBe(2);
  });

  // HP-4: Works with infinite streams
  it("works with infinite streams (takes first 6)", async () => {
    const result = await evalOmega(`
      (define (ones) (cons-stream 1 (ones)))
      (define (twos) (cons-stream 2 (twos)))

      (define (my-stream->list4 s n)
        (if (or (= n 0) (stream-null? s))
            '()
            (cons (stream-car s)
                  (my-stream->list4 (stream-cdr s) (- n 1)))))

      (my-stream->list4 (stream-interleave (ones) (twos)) 6)
    `);
    const lst = getList(result);
    expect(lst.length).toBe(6);
    expect(getNum(lst[0])).toBe(1);
    expect(getNum(lst[1])).toBe(2);
    expect(getNum(lst[2])).toBe(1);
    expect(getNum(lst[3])).toBe(2);
    expect(getNum(lst[4])).toBe(1);
    expect(getNum(lst[5])).toBe(2);
  });

  // HP-5: Handles different length streams
  it("handles streams of different lengths", async () => {
    const result = await evalOmega(`
      (define s1 (cons-stream 1 the-empty-stream))
      (define s2 (cons-stream 'a (cons-stream 'b (cons-stream 'c the-empty-stream))))

      (define (my-stream->list5 s n)
        (if (or (= n 0) (stream-null? s))
            '()
            (cons (stream-car s)
                  (my-stream->list5 (stream-cdr s) (- n 1)))))

      (my-stream->list5 (stream-interleave s1 s2) 4)
    `);
    const lst = getList(result);
    expect(lst.length).toBe(4);
    expect(getNum(lst[0])).toBe(1);
    expect(getSym(lst[1])).toBe("a");
    expect(getSym(lst[2])).toBe("b");
    expect(getSym(lst[3])).toBe("c");
  });

  // EC-1: Empty with empty
  it("returns empty when both streams are empty", async () => {
    const result = await evalOmega(`
      (stream-null? (stream-interleave the-empty-stream the-empty-stream))
    `);
    expect(getBool(result)).toBe(true);
  });
});
