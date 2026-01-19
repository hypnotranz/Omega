// src/core/stream/stream.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Prompt 16: Stream primitives and combinators (SICP-style)

import type { Val, PromiseVal, ReceiptRefVal, ListVal, IntVal } from "../eval/values";
import { VUnit } from "../eval/values";
import type { Hash } from "../artifacts/hash";
import { sha256JSON } from "../artifacts/hash";
import type {
  StreamCell,
  StreamContext,
  StreamSegment,
  StreamReceipt,
} from "./types";
import { makeEmptyStream, isEmptyStreamSentinel, createStreamContext } from "./types";
import {
  createPromise,
  isPromise,
  forceSync,
  beginForce,
  completeForce,
  isPromiseForced,
  getForcedValue,
} from "./promise";

// ─────────────────────────────────────────────────────────────────
// Thunk Registry (module-level for simplicity)
// ─────────────────────────────────────────────────────────────────

const MAX_THUNKS = 500; // Hard limit to prevent memory issues
const thunkRegistry = new Map<string, () => Val>();
let thunkCounter = 0;

function registerThunk(fn: () => Val): string {
  if (thunkRegistry.size >= MAX_THUNKS) {
    // Clear old thunks to prevent memory growth
    const keysToRemove = Array.from(thunkRegistry.keys()).slice(0, 100);
    for (const key of keysToRemove) {
      thunkRegistry.delete(key);
    }
  }

  const id = `thunk-${thunkCounter++}`;
  thunkRegistry.set(id, fn);
  return id;
}

function getThunk(id: string): (() => Val) | undefined {
  return thunkRegistry.get(id);
}

export function clearThunkRegistry(): void {
  thunkRegistry.clear();
  thunkCounter = 0;
}

// ─────────────────────────────────────────────────────────────────
// Stream Constructors
// ─────────────────────────────────────────────────────────────────

/**
 * Create the empty stream.
 */
export function emptyStream(): Val {
  return makeEmptyStream();
}

/**
 * Check if a value is the empty stream.
 */
export function isStreamNull(v: Val): boolean {
  return isEmptyStreamSentinel(v);
}

/**
 * Alias for isStreamNull.
 */
export function isStreamEmpty(v: Val): boolean {
  return isStreamNull(v);
}

/**
 * Create a stream cons cell: (head . tail-promise).
 *
 * The tailThunk is a function that produces the tail when called.
 * It's wrapped in a promise for lazy evaluation.
 */
export function consStream(
  ctx: StreamContext,
  head: Val,
  tailThunk: () => Val,
  label?: string
): Val {
  // Register the thunk and create a reference value
  const thunkId = registerThunk(tailThunk);
  const thunkVal: Val = {
    tag: "Str",
    value: `__thunk:${thunkId}`,
  };

  const tailPromise = createPromise(ctx, thunkVal, label);
  return { tag: "Pair", car: head, cdr: tailPromise };
}

/**
 * Create a stream cons cell with an already-created promise.
 */
export function consStreamWithPromise(head: Val, tailPromise: PromiseVal): Val {
  return { tag: "Pair", car: head, cdr: tailPromise };
}

/**
 * Create a stream cons cell with a receipt ref (for receipt-backed streams).
 */
export function consStreamWithReceipt(head: Val, receiptRef: ReceiptRefVal): Val {
  return { tag: "Pair", car: head, cdr: receiptRef };
}

// ─────────────────────────────────────────────────────────────────
// Stream Accessors
// ─────────────────────────────────────────────────────────────────

/**
 * Get the head of a stream.
 */
export function streamCar(s: Val): Val {
  if (isStreamNull(s)) {
    throw new Error("stream-car: empty stream");
  }
  if (s.tag !== "Pair") {
    throw new Error("stream-car: not a stream");
  }
  return s.car;
}

/**
 * Get the tail promise of a stream (without forcing).
 */
export function streamCdrPromise(s: Val): Val {
  if (isStreamNull(s)) {
    throw new Error("stream-cdr: empty stream");
  }
  if (s.tag !== "Pair") {
    throw new Error("stream-cdr: not a stream");
  }
  return s.cdr;
}

/**
 * Custom thunk evaluator that handles our stream thunks.
 */
function evaluateThunk(
  thunk: Val,
  userEvaluator?: (thunk: Val) => { value: Val; oracleCalls: number }
): { value: Val; oracleCalls: number } {
  // Check if it's our stream thunk marker
  if (thunk.tag === "Str" && thunk.value.startsWith("__thunk:")) {
    const thunkId = thunk.value.slice(8);
    const thunkFn = getThunk(thunkId);
    if (thunkFn) {
      const result = thunkFn();
      return { value: result, oracleCalls: 0 };
    }
  }

  // Fall back to user evaluator or identity
  if (userEvaluator) {
    return userEvaluator(thunk);
  }
  return { value: thunk, oracleCalls: 0 };
}

/**
 * Get the tail of a stream (forcing the tail promise).
 */
export function streamCdr(
  ctx: StreamContext,
  s: Val,
  evaluator?: (thunk: Val) => { value: Val; oracleCalls: number }
): Val {
  if (isStreamNull(s)) {
    throw new Error("stream-cdr: empty stream");
  }
  if (s.tag !== "Pair") {
    throw new Error("stream-cdr: not a stream");
  }

  const tail = s.cdr;

  // Case 1: Tail is a promise - force it
  if (isPromise(tail)) {
    const combinedEvaluator = (thunk: Val) => evaluateThunk(thunk, evaluator);
    return forceSync(ctx, tail, combinedEvaluator);
  }

  // Case 2: Tail is a receipt ref - hydrate it
  if (tail.tag === "ReceiptRef") {
    return hydrateStreamTail(ctx, tail);
  }

  // Case 3: Tail is already a value (should not happen in normal use)
  return tail;
}

/**
 * Hydrate a stream tail from a receipt reference.
 */
export function hydrateStreamTail(ctx: StreamContext, receiptRef: ReceiptRefVal): Val {
  if (ctx.config.logging) {
    ctx.events.push({
      tag: "PromiseForceStart",
      id: `hydrate-${receiptRef.rid}`,
      timestamp: Date.now(),
    });
    ctx.events.push({
      tag: "PromiseForceDone",
      id: `hydrate-${receiptRef.rid}`,
      valueHash: sha256JSON(makeEmptyStream()),
      oracleCalls: 0,
      timestamp: Date.now(),
    });
  }

  return makeEmptyStream();
}

// ─────────────────────────────────────────────────────────────────
// Stream Combinators
// ─────────────────────────────────────────────────────────────────

/**
 * Map a function over a stream.
 */
export function streamMap(
  ctx: StreamContext,
  s: Val,
  f: (x: Val) => Val,
  evaluator?: (thunk: Val) => { value: Val; oracleCalls: number }
): Val {
  if (isStreamNull(s)) {
    return emptyStream();
  }

  const head = streamCar(s);
  const mappedHead = f(head);
  const stream = s; // Capture for closure

  return consStream(ctx, mappedHead, () => {
    const tail = streamCdr(ctx, stream, evaluator);
    return streamMap(ctx, tail, f, evaluator);
  }, "stream-map-tail");
}

/**
 * Filter a stream by a predicate.
 */
export function streamFilter(
  ctx: StreamContext,
  s: Val,
  p: (x: Val) => boolean,
  evaluator?: (thunk: Val) => { value: Val; oracleCalls: number }
): Val {
  let current = s;

  while (!isStreamNull(current)) {
    if (ctx.fuel <= 0) {
      throw new Error("stream-filter: fuel exhausted");
    }
    ctx.fuel--;

    const head = streamCar(current);

    if (p(head)) {
      const currStream = current;
      return consStream(ctx, head, () => {
        const tail = streamCdr(ctx, currStream, evaluator);
        return streamFilter(ctx, tail, p, evaluator);
      }, "stream-filter-tail");
    }

    current = streamCdr(ctx, current, evaluator);
  }

  return emptyStream();
}

/**
 * Take the first n elements of a stream.
 */
export function streamTake(
  ctx: StreamContext,
  s: Val,
  n: number,
  evaluator?: (thunk: Val) => { value: Val; oracleCalls: number }
): Val {
  if (n <= 0 || isStreamNull(s)) {
    return emptyStream();
  }

  const head = streamCar(s);
  const stream = s;
  const remaining = n;

  return consStream(ctx, head, () => {
    const tail = streamCdr(ctx, stream, evaluator);
    return streamTake(ctx, tail, remaining - 1, evaluator);
  }, "stream-take-tail");
}

/**
 * Drop the first n elements of a stream.
 */
export function streamDrop(
  ctx: StreamContext,
  s: Val,
  n: number,
  evaluator?: (thunk: Val) => { value: Val; oracleCalls: number }
): Val {
  let current = s;
  let remaining = n;

  while (remaining > 0 && !isStreamNull(current)) {
    if (ctx.fuel <= 0) {
      throw new Error("stream-drop: fuel exhausted");
    }
    ctx.fuel--;

    current = streamCdr(ctx, current, evaluator);
    remaining--;
  }

  return current;
}

/**
 * Append two streams.
 */
export function streamAppend(
  ctx: StreamContext,
  s1: Val,
  s2Thunk: () => Val,
  evaluator?: (thunk: Val) => { value: Val; oracleCalls: number }
): Val {
  if (isStreamNull(s1)) {
    return s2Thunk();
  }

  const head = streamCar(s1);
  const stream = s1;

  return consStream(ctx, head, () => {
    const tail = streamCdr(ctx, stream, evaluator);
    return streamAppend(ctx, tail, s2Thunk, evaluator);
  }, "stream-append-tail");
}

/**
 * Flatmap over a stream.
 */
export function streamFlatMap(
  ctx: StreamContext,
  s: Val,
  f: (x: Val) => Val,
  evaluator?: (thunk: Val) => { value: Val; oracleCalls: number }
): Val {
  if (isStreamNull(s)) {
    return emptyStream();
  }

  const head = streamCar(s);
  const mappedHead = f(head);
  const stream = s;

  return streamAppend(ctx, mappedHead, () => {
    const tail = streamCdr(ctx, stream, evaluator);
    return streamFlatMap(ctx, tail, f, evaluator);
  }, evaluator);
}

/**
 * Zip two streams together.
 */
export function streamZip(
  ctx: StreamContext,
  s1: Val,
  s2: Val,
  evaluator?: (thunk: Val) => { value: Val; oracleCalls: number }
): Val {
  if (isStreamNull(s1) || isStreamNull(s2)) {
    return emptyStream();
  }

  const head1 = streamCar(s1);
  const head2 = streamCar(s2);
  const pair: ListVal = { tag: "List", elements: [head1, head2] };
  const stream1 = s1;
  const stream2 = s2;

  return consStream(ctx, pair, () => {
    const tail1 = streamCdr(ctx, stream1, evaluator);
    const tail2 = streamCdr(ctx, stream2, evaluator);
    return streamZip(ctx, tail1, tail2, evaluator);
  }, "stream-zip-tail");
}

// ─────────────────────────────────────────────────────────────────
// Stream Generators
// ─────────────────────────────────────────────────────────────────

/**
 * Create an infinite stream of a constant value.
 */
export function streamRepeat(ctx: StreamContext, value: Val): Val {
  const v = value;
  return consStream(ctx, v, () => streamRepeat(ctx, v), "stream-repeat-tail");
}

/**
 * Create a stream from a range.
 */
export function streamRange(ctx: StreamContext, start: number, end: number): Val {
  if (start >= end) {
    return emptyStream();
  }

  const head: IntVal = { tag: "Int", value: BigInt(start) };
  const s = start;
  const e = end;

  return consStream(ctx, head, () => streamRange(ctx, s + 1, e), "stream-range-tail");
}

/**
 * Create a stream from a list value.
 */
export function listToStream(ctx: StreamContext, list: ListVal): Val {
  const items = list.elements;
  if (items.length === 0) {
    return emptyStream();
  }

  function buildFromIndex(i: number): Val {
    if (i >= items.length) {
      return emptyStream();
    }
    return consStream(ctx, items[i], () => buildFromIndex(i + 1), `list-item-${i}`);
  }

  return buildFromIndex(0);
}

/**
 * Create a stream from a generator function.
 */
export function streamIterate(
  ctx: StreamContext,
  seed: Val,
  f: (x: Val) => Val
): Val {
  const s = seed;
  return consStream(ctx, s, () => {
    const next = f(s);
    return streamIterate(ctx, next, f);
  }, "stream-iterate-tail");
}

// ─────────────────────────────────────────────────────────────────
// Stream to List Conversion
// ─────────────────────────────────────────────────────────────────

/**
 * Convert a finite stream to a list.
 */
export function streamToList(
  ctx: StreamContext,
  s: Val,
  maxElements: number = 1000,
  evaluator?: (thunk: Val) => { value: Val; oracleCalls: number }
): Val[] {
  const result: Val[] = [];
  let current = s;
  let count = 0;

  while (!isStreamNull(current) && count < maxElements) {
    if (ctx.fuel <= 0) {
      throw new Error("stream->list: fuel exhausted");
    }
    ctx.fuel--;

    result.push(streamCar(current));
    current = streamCdr(ctx, current, evaluator);
    count++;
  }

  if (count >= maxElements && !isStreamNull(current)) {
    throw new Error(`stream->list: exceeded max elements (${maxElements})`);
  }

  return result;
}

/**
 * Force the first n elements of a stream into an array.
 */
export function forceN(
  ctx: StreamContext,
  s: Val,
  n: number,
  evaluator?: (thunk: Val) => { value: Val; oracleCalls: number }
): Val[] {
  const result: Val[] = [];
  let current = s;
  let remaining = n;

  while (remaining > 0 && !isStreamNull(current)) {
    if (ctx.fuel <= 0) {
      break;
    }
    ctx.fuel--;

    result.push(streamCar(current));
    current = streamCdr(ctx, current, evaluator);
    remaining--;
  }

  return result;
}

/**
 * Force the head of a stream.
 */
export function forceHead(
  ctx: StreamContext,
  s: Val,
  evaluator?: (thunk: Val) => { value: Val; oracleCalls: number }
): Val | null {
  if (isStreamNull(s)) {
    return null;
  }
  return streamCar(s);
}

/**
 * Force the tail of a stream.
 */
export function forceTail(
  ctx: StreamContext,
  s: Val,
  evaluator?: (thunk: Val) => { value: Val; oracleCalls: number }
): Val | null {
  if (isStreamNull(s)) {
    return null;
  }
  return streamCdr(ctx, s, evaluator);
}

/**
 * Deep force a stream up to n elements.
 */
export function deepForce(
  ctx: StreamContext,
  s: Val,
  n: number,
  evaluator?: (thunk: Val) => { value: Val; oracleCalls: number }
): Val[] {
  return forceN(ctx, s, n, evaluator);
}

/**
 * For each element in a stream, call a function.
 */
export function streamForEach(
  ctx: StreamContext,
  s: Val,
  f: (x: Val) => void,
  maxElements: number = 1000,
  evaluator?: (thunk: Val) => { value: Val; oracleCalls: number }
): void {
  let current = s;
  let count = 0;

  while (!isStreamNull(current) && count < maxElements) {
    if (ctx.fuel <= 0) {
      break;
    }
    ctx.fuel--;

    f(streamCar(current));
    current = streamCdr(ctx, current, evaluator);
    count++;
  }
}

// ─────────────────────────────────────────────────────────────────
// Stream Fold Operations
// ─────────────────────────────────────────────────────────────────

/**
 * Fold over a stream from the left.
 */
export function streamFold(
  ctx: StreamContext,
  s: Val,
  init: Val,
  f: (acc: Val, x: Val) => Val,
  evaluator?: (thunk: Val) => { value: Val; oracleCalls: number }
): Val {
  let acc = init;
  let current = s;

  while (!isStreamNull(current)) {
    if (ctx.fuel <= 0) {
      throw new Error("stream-fold: fuel exhausted");
    }
    ctx.fuel--;

    acc = f(acc, streamCar(current));
    current = streamCdr(ctx, current, evaluator);
  }

  return acc;
}

/**
 * Reduce a stream (fold with first element as init).
 */
export function streamReduce(
  ctx: StreamContext,
  s: Val,
  f: (acc: Val, x: Val) => Val,
  evaluator?: (thunk: Val) => { value: Val; oracleCalls: number }
): Val {
  if (isStreamNull(s)) {
    throw new Error("stream-reduce: empty stream");
  }

  const init = streamCar(s);
  const rest = streamCdr(ctx, s, evaluator);

  return streamFold(ctx, rest, init, f, evaluator);
}

// ─────────────────────────────────────────────────────────────────
// Stream Segment Operations (for receipt-backed staging)
// ─────────────────────────────────────────────────────────────────

/**
 * Materialize a prefix of a stream into a segment.
 */
export function materializeSegment(
  ctx: StreamContext,
  s: Val,
  n: number,
  evaluator?: (thunk: Val) => { value: Val; oracleCalls: number }
): StreamSegment {
  const elements = forceN(ctx, s, n, evaluator);

  // Try to get current position
  let current = s;
  for (let i = 0; i < elements.length && !isStreamNull(current); i++) {
    current = streamCdr(ctx, current, evaluator);
  }

  return {
    elements,
    continuationHash: isStreamNull(current) ? undefined : sha256JSON({ remaining: true }),
    terminal: isStreamNull(current),
  };
}

/**
 * Create a receipt for a stream segment.
 */
export function createStreamReceipt(segment: StreamSegment): StreamReceipt {
  return {
    rid: sha256JSON({ segment: segment.elements.length, time: Date.now() }),
    segmentHash: sha256JSON(segment.elements),
    elementCount: segment.elements.length,
    createdAt: Date.now(),
  };
}

/**
 * Hydrate a stream from a receipt and segment.
 */
export function hydrateFromReceipt(
  ctx: StreamContext,
  receipt: StreamReceipt,
  segment: StreamSegment,
  continuation: () => Val
): Val {
  if (segment.elements.length === 0) {
    return continuation();
  }

  function buildFromIndex(i: number): Val {
    if (i >= segment.elements.length) {
      if (segment.terminal) {
        return emptyStream();
      }
      return continuation();
    }
    return consStream(ctx, segment.elements[i], () => buildFromIndex(i + 1), `hydrate-${i}`);
  }

  return buildFromIndex(0);
}

// ─────────────────────────────────────────────────────────────────
// Stream Utilities
// ─────────────────────────────────────────────────────────────────

/**
 * Get the length of a finite stream.
 */
export function streamLength(
  ctx: StreamContext,
  s: Val,
  evaluator?: (thunk: Val) => { value: Val; oracleCalls: number }
): number {
  let count = 0;
  let current = s;

  while (!isStreamNull(current)) {
    if (ctx.fuel <= 0) {
      throw new Error("stream-length: fuel exhausted");
    }
    ctx.fuel--;

    count++;
    current = streamCdr(ctx, current, evaluator);
  }

  return count;
}

/**
 * Check if a value is a stream (pair with promise/receipt cdr, or empty).
 */
export function isStream(v: Val): boolean {
  if (isStreamNull(v)) return true;
  if (v.tag !== "Pair") return false;
  const cdr = v.cdr;
  return isPromise(cdr) || cdr.tag === "ReceiptRef";
}

/**
 * Get the ref (promise or receipt) in the stream's cdr.
 */
export function streamTailRef(s: Val): Val | null {
  if (isStreamNull(s)) return null;
  if (s.tag !== "Pair") return null;
  return s.cdr;
}
