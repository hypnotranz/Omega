// Stream interleave tests for the TypeScript stream library (production primitives)
// These tests ensure the TS-level stream utilities interleave fairly and handle errors.

import { beforeEach, describe, expect, it } from "vitest";
import { clearThunkRegistry, consStream, emptyStream, forceN, streamInterleave, streamRepeat, streamToList } from "../../src/core/stream/stream";
import { resetPromiseState } from "../../src/core/stream/promise";
import { createStreamContext, type StreamContext } from "../../src/core/stream/types";
import type { Val } from "../../src/core/eval/values";

// Helpers to build test values
const num = (n: number): Val => ({ tag: "Num", n });
const sym = (name: string): Val => ({ tag: "Sym", name });

// Build a finite stream from an array of values.
function finiteStream(ctx: StreamContext, values: Val[]): Val {
  return values.reduceRight<Val>(
    (tail, head) => consStream(ctx, head, () => tail, "finite-stream-tail"),
    emptyStream(),
  );
}

// Convert Val to a plain JS value for assertions.
function toJs(v: Val): unknown {
  switch (v.tag) {
    case "Num": return v.n;
    case "Int": return Number(v.value);
    case "Sym": return v.name;
    case "Str": return v.s;
    default: return v;
  }
}

describe("streamInterleave (TypeScript runtime)", () => {
  let ctx: StreamContext;

  beforeEach(() => {
    clearThunkRegistry();
    resetPromiseState();
    ctx = createStreamContext({ logging: false });
  });

  it("alternates elements from two finite streams", () => {
    const s1 = finiteStream(ctx, [num(1), num(2)]);
    const s2 = finiteStream(ctx, [sym("a"), sym("b")]);

    const result = streamInterleave(ctx, s1, s2);
    const list = streamToList(ctx, result, 4).map(toJs);

    expect(list).toEqual([1, "a", 2, "b"]);
  });

  it("returns the second stream when the first is empty", () => {
    const s2 = finiteStream(ctx, [num(1), num(2)]);

    const result = streamInterleave(ctx, emptyStream(), s2);
    const list = streamToList(ctx, result, 2).map(toJs);

    expect(list).toEqual([1, 2]);
  });

  it("returns the first stream when the second is empty", () => {
    const s1 = finiteStream(ctx, [num(1), num(2)]);

    const result = streamInterleave(ctx, s1, emptyStream());
    const list = streamToList(ctx, result, 2).map(toJs);

    expect(list).toEqual([1, 2]);
  });

  it("interleaves infinite streams without blocking", () => {
    const ones = streamRepeat(ctx, num(1));
    const twos = streamRepeat(ctx, num(2));

    const result = streamInterleave(ctx, ones, twos);
    const firstSix = forceN(ctx, result, 6).map(toJs);

    expect(firstSix).toEqual([1, 2, 1, 2, 1, 2]);
  });

  it("handles streams of different lengths", () => {
    const s1 = finiteStream(ctx, [num(1)]);
    const s2 = finiteStream(ctx, [sym("a"), sym("b"), sym("c")]);

    const result = streamInterleave(ctx, s1, s2);
    const list = streamToList(ctx, result, 4).map(toJs);

    expect(list).toEqual([1, "a", "b", "c"]);
  });

  it("throws when provided non-stream inputs", () => {
    expect(() => streamInterleave(ctx, num(1), emptyStream())).toThrow(/stream-interleave: expected streams/);
  });
});
