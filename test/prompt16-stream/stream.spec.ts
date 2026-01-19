// test/prompt16-stream/stream.spec.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Tests 16.1-16.8: Streams + Laziness

import { describe, it, expect, beforeEach, afterEach } from "vitest";
import type { Val, PromiseVal, IntVal, ListVal } from "../../src/core/eval/values";
import { VUnit } from "../../src/core/eval/values";
import type { StreamContext, PromiseEvent, FusionCandidate } from "../../src/core/stream";
import {
  createStreamContext,
  createPromise,
  resetPromiseIds,
  beginForce,
  completeForce,
  forceSync,
  getPromiseCell,
  isPromiseForced,
  getForcedValue,
  getPromiseStats,
  getAllPromiseIds,
  getForcedPromiseIds,
  getUnforcedPromiseIds,
  countTotalForceStarts,
  countCacheHits,
  findMultiForced,
  isPromise,
  makePromiseVal,
  clearStreamContext,
  resetPromiseState,
  emptyStream,
  consStream,
  streamCar,
  streamCdr,
  isStreamEmpty,
  isStream,
  streamMap,
  streamFilter,
  streamTake,
  streamDrop,
  streamFold,
  streamRange,
  listToStream,
  forceN,
  clearThunkRegistry,
  materializeSegment,
  createStreamReceipt,
  hydrateFromReceipt,
  analyzeStream,
  analyzeStrictness,
  analyzeProductivity,
  analyzeSpaceLeaks,
  identifyFusionCandidates,
  compareFusionCost,
  applyFusion,
  getPromiseEventsForId,
  getForceTimeline,
} from "../../src/core/stream";

// ─────────────────────────────────────────────────────────────────
// Test Utilities
// ─────────────────────────────────────────────────────────────────

function makeInt(n: number): IntVal {
  return { tag: "Int", value: BigInt(n) };
}

function makeList(...vals: Val[]): ListVal {
  return { tag: "List", elements: vals };
}

function intVal(v: Val): bigint {
  if (v.tag !== "Int") throw new Error(`Expected Int, got ${v.tag}`);
  return v.value;
}

function identityEvaluator(thunk: Val): { value: Val; oracleCalls: number } {
  return { value: thunk, oracleCalls: 0 };
}

function createCountingEvaluator(): {
  evaluator: (thunk: Val) => { value: Val; oracleCalls: number };
  getCount: () => number;
} {
  let count = 0;
  return {
    evaluator: (thunk: Val) => {
      count++;
      return { value: thunk, oracleCalls: 1 };
    },
    getCount: () => count,
  };
}

// ─────────────────────────────────────────────────────────────────
// Test 16.1: Laziness - Promises Not Forced Until Needed
// ─────────────────────────────────────────────────────────────────

describe("Test 16.1: Laziness", () => {
  let ctx: StreamContext;

  beforeEach(() => {
    resetPromiseState();
    clearThunkRegistry();
    ctx = createStreamContext({ logging: true, maxFuel: 20 });
  });

  afterEach(() => {
    clearThunkRegistry();
  });

  it("16.1.1: Creating a promise does not force it", () => {
    const thunk = makeInt(42);
    const promise = createPromise(ctx, thunk, "lazy");

    expect(isPromise(promise)).toBe(true);
    expect(isPromiseForced(ctx, promise.id)).toBe(false);
    expect(getUnforcedPromiseIds(ctx)).toContain(promise.id);
  });

  it("16.1.2: Force evaluates the thunk exactly once", () => {
    const { evaluator, getCount } = createCountingEvaluator();
    const thunk = makeInt(10);
    const promise = createPromise(ctx, thunk, "force-once");

    expect(getCount()).toBe(0);
    forceSync(ctx, promise, evaluator);
    expect(getCount()).toBe(1);
    forceSync(ctx, promise, evaluator); // Cache hit
    expect(getCount()).toBe(1);
  });

  it("16.1.3: Stream tail is lazy", () => {
    const s = consStream(ctx, makeInt(1), () =>
      consStream(ctx, makeInt(2), () => emptyStream())
    );

    const head = streamCar(s);
    expect(intVal(head)).toBe(1n);

    const statsBefore = getPromiseStats(ctx);
    const forcedBefore = statsBefore.forced;

    streamCdr(ctx, s, identityEvaluator);
    const statsAfter = getPromiseStats(ctx);
    // Forcing the tail increases forced count
    expect(statsAfter.forced).toBeGreaterThan(forcedBefore);
  });

  it("16.1.4: forceN limits elements", () => {
    const s = consStream(ctx, makeInt(1), () =>
      consStream(ctx, makeInt(2), () =>
        consStream(ctx, makeInt(3), () => emptyStream())
      )
    );

    const first2 = forceN(ctx, s, 2, identityEvaluator);
    expect(first2).toHaveLength(2);
    expect(intVal(first2[0])).toBe(1n);
    expect(intVal(first2[1])).toBe(2n);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 16.2: Memoization - Force-Once Semantics
// ─────────────────────────────────────────────────────────────────

describe("Test 16.2: Memoization", () => {
  let ctx: StreamContext;

  beforeEach(() => {
    resetPromiseState();
    clearThunkRegistry();
    ctx = createStreamContext({ logging: true, maxFuel: 20 });
  });

  afterEach(() => {
    clearThunkRegistry();
  });

  it("16.2.1: Memoization caches forced values", () => {
    const { evaluator, getCount } = createCountingEvaluator();
    const promise = createPromise(ctx, makeInt(42), "memo-test");

    forceSync(ctx, promise, evaluator);
    forceSync(ctx, promise, evaluator);
    forceSync(ctx, promise, evaluator);

    expect(getCount()).toBe(1);
    expect(countCacheHits(ctx)).toBe(2);
  });

  it("16.2.2: No promise is forced more than once", () => {
    const { evaluator } = createCountingEvaluator();

    const p1 = createPromise(ctx, makeInt(1), "p1");
    const p2 = createPromise(ctx, makeInt(2), "p2");

    forceSync(ctx, p1, evaluator);
    forceSync(ctx, p1, evaluator);
    forceSync(ctx, p2, evaluator);
    forceSync(ctx, p2, evaluator);

    const multiForced = findMultiForced(ctx);
    expect(multiForced).toHaveLength(0);
  });

  it("16.2.3: Promise state transitions correctly", () => {
    const promise = createPromise(ctx, makeInt(100), "state-test");

    const cell1 = getPromiseCell(ctx, promise.id);
    expect(cell1?.tag).toBe("Unforced");

    const result = beginForce(ctx, promise.id);
    expect(result.tag).toBe("thunk");
    const cell2 = getPromiseCell(ctx, promise.id);
    expect(cell2?.tag).toBe("Forcing");

    completeForce(ctx, promise.id, makeInt(100), 0);
    const cell3 = getPromiseCell(ctx, promise.id);
    expect(cell3?.tag).toBe("Forced");
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 16.3: Receipt-Backed Stream Hydration
// ─────────────────────────────────────────────────────────────────

describe("Test 16.3: Receipt Hydration", () => {
  let ctx: StreamContext;

  beforeEach(() => {
    resetPromiseState();
    clearThunkRegistry();
    ctx = createStreamContext({ logging: true, maxFuel: 20 });
  });

  afterEach(() => {
    clearThunkRegistry();
  });

  it("16.3.1: Materialize stream segment", () => {
    const s = consStream(ctx, makeInt(1), () =>
      consStream(ctx, makeInt(2), () => emptyStream())
    );

    const segment = materializeSegment(ctx, s, 10, identityEvaluator);
    expect(segment.elements).toHaveLength(2);
    expect(intVal(segment.elements[0])).toBe(1n);
    expect(intVal(segment.elements[1])).toBe(2n);
    expect(segment.terminal).toBe(true);
  });

  it("16.3.2: Create receipt for segment", () => {
    const s = consStream(ctx, makeInt(10), () => emptyStream());

    const segment = materializeSegment(ctx, s, 10, identityEvaluator);
    const receipt = createStreamReceipt(segment);

    expect(receipt.elementCount).toBe(1);
    expect(receipt.rid).toBeDefined();
  });

  it("16.3.3: Hydrate stream from receipt", () => {
    const original = consStream(ctx, makeInt(1), () =>
      consStream(ctx, makeInt(2), () => emptyStream())
    );

    const segment = materializeSegment(ctx, original, 10, identityEvaluator);
    const receipt = createStreamReceipt(segment);

    clearStreamContext(ctx);
    clearThunkRegistry();
    const hydrated = hydrateFromReceipt(ctx, receipt, segment, () => emptyStream());

    const elements = forceN(ctx, hydrated, 5, identityEvaluator);
    expect(elements).toHaveLength(2);
    expect(intVal(elements[0])).toBe(1n);
    expect(intVal(elements[1])).toBe(2n);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 16.4: Strictness Analysis
// ─────────────────────────────────────────────────────────────────

describe("Test 16.4: Strictness Analysis", () => {
  let ctx: StreamContext;

  beforeEach(() => {
    resetPromiseState();
    clearThunkRegistry();
    ctx = createStreamContext({ logging: true, maxFuel: 20 });
  });

  afterEach(() => {
    clearThunkRegistry();
  });

  it("16.4.1: Track demanded vs forced elements", () => {
    const s = consStream(ctx, makeInt(1), () =>
      consStream(ctx, makeInt(2), () => emptyStream())
    );

    forceN(ctx, s, 2, identityEvaluator);

    const strictness = analyzeStrictness(ctx, 2);
    expect(strictness.demandedCount).toBe(2);
    expect(strictness.forcedCount).toBeGreaterThanOrEqual(2);
  });

  it("16.4.2: Empty multi-forced list when memoized", () => {
    const { evaluator } = createCountingEvaluator();
    const p = createPromise(ctx, makeInt(42), "memo");

    forceSync(ctx, p, evaluator);
    forceSync(ctx, p, evaluator);

    const strictness = analyzeStrictness(ctx, 1);
    expect(strictness.multiForced).toHaveLength(0);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 16.5: Productivity Analysis
// ─────────────────────────────────────────────────────────────────

describe("Test 16.5: Productivity Analysis", () => {
  let ctx: StreamContext;

  beforeEach(() => {
    resetPromiseState();
    clearThunkRegistry();
    ctx = createStreamContext({ logging: true, maxFuel: 15 });
  });

  afterEach(() => {
    clearThunkRegistry();
  });

  it("16.5.1: Finite stream is productive", () => {
    const s = consStream(ctx, makeInt(1), () =>
      consStream(ctx, makeInt(2), () => emptyStream())
    );

    const productivity = analyzeProductivity(ctx, s, identityEvaluator, 10);
    expect(productivity.productive).toBe(true);
    expect(productivity.producedCount).toBe(2);
  });

  it("16.5.2: Empty stream is productive", () => {
    const s = emptyStream();

    const productivity = analyzeProductivity(ctx, s, identityEvaluator, 10);
    expect(productivity.productive).toBe(true);
    expect(productivity.producedCount).toBe(0);
  });

  it("16.5.3: streamRange is productive", () => {
    const range = streamRange(ctx, 0, 3);

    const productivity = analyzeProductivity(ctx, range, identityEvaluator, 10);
    expect(productivity.productive).toBe(true);
    expect(productivity.producedCount).toBe(3);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 16.6: Stream Fusion / CSE
// ─────────────────────────────────────────────────────────────────

describe("Test 16.6: Stream Fusion / CSE", () => {
  let ctx: StreamContext;

  beforeEach(() => {
    resetPromiseState();
    clearThunkRegistry();
    ctx = createStreamContext({ logging: true, maxFuel: 15 });
  });

  afterEach(() => {
    clearThunkRegistry();
  });

  it("16.6.1: Identify fusion candidates", () => {
    const pattern: Val = {
      tag: "List",
      elements: [
        { tag: "Sym", name: "map-map" },
        { tag: "Sym", name: "f" },
        { tag: "Sym", name: "g" },
      ],
    };

    const candidates = identifyFusionCandidates(ctx, pattern);
    expect(Array.isArray(candidates)).toBe(true);
  });

  it("16.6.2: Apply fusion returns rewrite", () => {
    const candidate: FusionCandidate = {
      id: "test-fusion",
      kind: "map-map",
      description: "Test fusion",
      estimatedSaving: 1,
      pattern: { tag: "Sym", name: "original" },
      rewrite: { tag: "Sym", name: "fused" },
      confidence: 0.9,
    };

    const result = applyFusion(ctx, candidate);
    expect(result).toEqual({ tag: "Sym", name: "fused" });
  });

  it("16.6.3: Memoization provides implicit CSE", () => {
    const { evaluator, getCount } = createCountingEvaluator();

    const promise = createPromise(ctx, makeInt(100), "shared");
    forceSync(ctx, promise, evaluator);
    forceSync(ctx, promise, evaluator);
    forceSync(ctx, promise, evaluator);

    expect(getCount()).toBe(1);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 16.7: Space Leak Detection
// ─────────────────────────────────────────────────────────────────

describe("Test 16.7: Space Leak Detection", () => {
  let ctx: StreamContext;

  beforeEach(() => {
    resetPromiseState();
    clearThunkRegistry();
    ctx = createStreamContext({ logging: true, maxFuel: 15 });
  });

  afterEach(() => {
    clearThunkRegistry();
  });

  it("16.7.1: Analyze space leaks returns info", () => {
    for (let i = 0; i < 20; i++) {
      createPromise(ctx, makeInt(i), `test-${i}`);
    }

    const leakInfo = analyzeSpaceLeaks(ctx);
    expect(leakInfo).toBeDefined();
    expect(leakInfo.retainedGrowthRate).toBeGreaterThanOrEqual(0);
  });

  it("16.7.2: No leak when all consumed", () => {
    const promises: PromiseVal[] = [];
    for (let i = 0; i < 5; i++) {
      promises.push(createPromise(ctx, makeInt(i), `consumed-${i}`));
    }

    for (const p of promises) {
      forceSync(ctx, p, identityEvaluator);
    }

    const stats = getPromiseStats(ctx);
    expect(stats.forced).toBe(5);
    expect(stats.unforced).toBe(0);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 16.8: Event Replay
// ─────────────────────────────────────────────────────────────────

describe("Test 16.8: Event Replay", () => {
  let ctx: StreamContext;

  beforeEach(() => {
    resetPromiseState();
    clearThunkRegistry();
    ctx = createStreamContext({ logging: true, maxFuel: 15 });
  });

  afterEach(() => {
    clearThunkRegistry();
  });

  it("16.8.1: All promise operations emit events", () => {
    const promise = createPromise(ctx, makeInt(1), "event-test");

    expect(ctx.events.some(e => e.tag === "PromiseCreated")).toBe(true);

    forceSync(ctx, promise, identityEvaluator);

    expect(ctx.events.some(e => e.tag === "PromiseForceStart")).toBe(true);
    expect(ctx.events.some(e => e.tag === "PromiseForceDone")).toBe(true);
  });

  it("16.8.2: Cache hits emit events", () => {
    const promise = createPromise(ctx, makeInt(2), "cache-event");

    forceSync(ctx, promise, identityEvaluator);
    forceSync(ctx, promise, identityEvaluator);

    const hitEvents = ctx.events.filter(e => e.tag === "PromiseForceHit");
    expect(hitEvents.length).toBeGreaterThanOrEqual(1);
  });

  it("16.8.3: Events have timestamps", () => {
    createPromise(ctx, makeInt(3), "timestamp-test");

    for (const event of ctx.events) {
      expect(event.timestamp).toBeDefined();
      expect(typeof event.timestamp).toBe("number");
    }
  });

  it("16.8.4: Get promise events for ID", () => {
    const p1 = createPromise(ctx, makeInt(1), "filter-1");
    forceSync(ctx, p1, identityEvaluator);

    const p1Events = getPromiseEventsForId(ctx, p1.id);
    expect(p1Events.length).toBeGreaterThan(0);
  });

  it("16.8.5: Get force timeline", () => {
    const p1 = createPromise(ctx, makeInt(10), "timeline-1");
    forceSync(ctx, p1, identityEvaluator);

    const timeline = getForceTimeline(ctx);
    expect(timeline.length).toBeGreaterThan(0);
  });

  it("16.8.6: Clear context resets events", () => {
    createPromise(ctx, makeInt(1), "clear-test");
    expect(ctx.events.length).toBeGreaterThan(0);

    clearStreamContext(ctx);
    expect(ctx.events.length).toBe(0);
  });

  it("16.8.7: Logging can be disabled", () => {
    const quietCtx = createStreamContext({ logging: false });
    createPromise(quietCtx, makeInt(1), "quiet");
    expect(quietCtx.events.length).toBe(0);
  });
});

// ─────────────────────────────────────────────────────────────────
// Stream Integration Tests
// ─────────────────────────────────────────────────────────────────

describe("Stream Integration", () => {
  let ctx: StreamContext;

  beforeEach(() => {
    resetPromiseState();
    clearThunkRegistry();
    ctx = createStreamContext({ logging: true, maxFuel: 30 });
  });

  afterEach(() => {
    clearThunkRegistry();
  });

  it("Stream map works", () => {
    const s = listToStream(ctx, makeList(makeInt(1), makeInt(2), makeInt(3)));
    const doubled = streamMap(ctx, s, v => makeInt(Number((v as IntVal).value) * 2));

    const result = forceN(ctx, doubled, 5, identityEvaluator);
    expect(result).toHaveLength(3);
    expect(intVal(result[0])).toBe(2n);
    expect(intVal(result[1])).toBe(4n);
    expect(intVal(result[2])).toBe(6n);
  });

  it("Stream filter works", () => {
    const s = streamRange(ctx, 0, 5);
    const evens = streamFilter(ctx, s, v => (v as IntVal).value % 2n === 0n);

    const result = forceN(ctx, evens, 5, identityEvaluator);
    expect(result.length).toBeGreaterThanOrEqual(2);
    expect(intVal(result[0])).toBe(0n);
    expect(intVal(result[1])).toBe(2n);
  });

  it("Stream fold works", () => {
    const s = streamRange(ctx, 0, 5);

    const sum = streamFold(
      ctx,
      s,
      makeInt(0),
      (acc, x) => makeInt(Number((acc as IntVal).value) + Number((x as IntVal).value)),
      identityEvaluator
    );

    expect(intVal(sum)).toBe(10n); // 0+1+2+3+4
  });

  it("Full analysis on stream", () => {
    const s = streamRange(ctx, 0, 3);
    forceN(ctx, s, 3, identityEvaluator);

    const analysis = analyzeStream(ctx, s, {
      strictness: true,
      productivity: true,
    });

    expect(analysis.strictness).toBeDefined();
    expect(analysis.productivity).toBeDefined();
    expect(analysis.events.length).toBeGreaterThan(0);
  });
});
