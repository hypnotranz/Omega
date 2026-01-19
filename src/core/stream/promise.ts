// src/core/stream/promise.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Prompt 16: Memoized promises for lazy evaluation

import type { Val, PromiseId, PromiseVal } from "../eval/values";
import { VUnit } from "../eval/values";
import type { Hash } from "../artifacts/hash";
import { sha256JSON } from "../artifacts/hash";
import type {
  PromiseCell,
  PromiseStore,
  PromiseEvent,
  StreamContext,
} from "./types";

// ─────────────────────────────────────────────────────────────────
// Promise ID Generation
// ─────────────────────────────────────────────────────────────────

let nextPromiseId = 0;

/**
 * Generate a fresh promise ID.
 */
export function freshPromiseId(): PromiseId {
  return `promise-${nextPromiseId++}`;
}

/**
 * Reset promise ID counter (for testing).
 */
export function resetPromiseIds(): void {
  nextPromiseId = 0;
}

// ─────────────────────────────────────────────────────────────────
// Promise Store Operations
// ─────────────────────────────────────────────────────────────────

/**
 * Create an empty promise store.
 */
export function createPromiseStore(): PromiseStore {
  return new Map();
}

/**
 * Create a new promise in the store.
 */
export function createPromise(
  ctx: StreamContext,
  thunk: Val,
  label?: string
): PromiseVal {
  const id = freshPromiseId();
  const cell: PromiseCell = {
    tag: "Unforced",
    thunk,
    createdAt: Date.now(),
  };

  ctx.store.set(id, cell);

  if (ctx.config.logging) {
    const thunkHash = sha256JSON(thunk);
    ctx.events.push({
      tag: "PromiseCreated",
      id,
      thunkHash,
      timestamp: Date.now(),
    });
  }

  return { tag: "Promise", id, label };
}

/**
 * Get the current state of a promise.
 */
export function getPromiseCell(ctx: StreamContext, id: PromiseId): PromiseCell | undefined {
  return ctx.store.get(id);
}

/**
 * Check if a promise is forced.
 */
export function isPromiseForced(ctx: StreamContext, id: PromiseId): boolean {
  const cell = ctx.store.get(id);
  return cell?.tag === "Forced";
}

/**
 * Check if a promise is currently being forced.
 */
export function isPromiseForcing(ctx: StreamContext, id: PromiseId): boolean {
  const cell = ctx.store.get(id);
  return cell?.tag === "Forcing";
}

/**
 * Get the cached value of a forced promise.
 */
export function getForcedValue(ctx: StreamContext, id: PromiseId): Val | undefined {
  const cell = ctx.store.get(id);
  if (cell?.tag === "Forced") {
    return cell.value;
  }
  return undefined;
}

// ─────────────────────────────────────────────────────────────────
// Promise Force Operations
// ─────────────────────────────────────────────────────────────────

/**
 * ForceResult: Result of forcing a promise.
 */
export type ForceResult =
  | { tag: "value"; value: Val; cached: boolean }
  | { tag: "thunk"; thunk: Val }
  | { tag: "waiting"; ivarId: string }
  | { tag: "error"; message: string };

/**
 * Begin forcing a promise.
 *
 * Returns:
 * - { tag: "value", value, cached: true } if already forced
 * - { tag: "thunk", thunk } if needs to evaluate
 * - { tag: "waiting", ivarId } if another fiber is forcing (singleflight)
 * - { tag: "error", message } if promise doesn't exist
 */
export function beginForce(ctx: StreamContext, id: PromiseId): ForceResult {
  const cell = ctx.store.get(id);

  if (!cell) {
    return { tag: "error", message: `Promise ${id} not found` };
  }

  switch (cell.tag) {
    case "Forced":
      // Already forced - return cached value
      if (ctx.config.logging) {
        ctx.events.push({
          tag: "PromiseForceHit",
          id,
          timestamp: Date.now(),
        });
      }
      return { tag: "value", value: cell.value, cached: true };

    case "Forcing":
      // Another force is in progress - singleflight
      if (ctx.config.singleflight) {
        if (ctx.config.logging) {
          ctx.events.push({
            tag: "PromiseForceJoin",
            id,
            waiterId: "current",
            timestamp: Date.now(),
          });
        }
        return { tag: "waiting", ivarId: cell.ivarId };
      }
      // Fall through if singleflight disabled (not recommended)
      return { tag: "error", message: `Promise ${id} is being forced (no singleflight)` };

    case "Unforced":
      // Start forcing
      if (ctx.config.logging) {
        ctx.events.push({
          tag: "PromiseForceStart",
          id,
          timestamp: Date.now(),
        });
      }

      ctx.forceCount++;

      // Transition to Forcing state
      const ivarId = `ivar-${id}`;
      ctx.store.set(id, {
        tag: "Forcing",
        ivarId,
        startedAt: Date.now(),
      });

      return { tag: "thunk", thunk: cell.thunk };
  }
}

/**
 * Complete forcing a promise with the computed value.
 */
export function completeForce(
  ctx: StreamContext,
  id: PromiseId,
  value: Val,
  oracleCalls: number = 0
): void {
  ctx.store.set(id, {
    tag: "Forced",
    value,
    forcedAt: Date.now(),
  });

  ctx.oracleCallCount += oracleCalls;

  if (ctx.config.logging) {
    const valueHash = sha256JSON(value);
    ctx.events.push({
      tag: "PromiseForceDone",
      id,
      valueHash,
      oracleCalls,
      timestamp: Date.now(),
    });
  }
}

/**
 * Force a promise synchronously (for simple cases where thunk is pure).
 *
 * Note: In a real evaluator, forcing would be handled by the machine.
 * This is a simplified version for testing and pure computations.
 */
export function forceSync(
  ctx: StreamContext,
  promise: PromiseVal,
  evaluator?: (thunk: Val) => { value: Val; oracleCalls: number }
): Val {
  const result = beginForce(ctx, promise.id);

  switch (result.tag) {
    case "value":
      return result.value;

    case "error":
      throw new Error(result.message);

    case "waiting":
      // In a real system, we'd wait on the IVar
      // For sync version, this shouldn't happen if singleflight is off
      throw new Error(`Cannot wait synchronously on ${result.ivarId}`);

    case "thunk":
      if (!evaluator) {
        // No evaluator provided - return thunk as-is for now
        completeForce(ctx, promise.id, result.thunk, 0);
        return result.thunk;
      }

      // Evaluate the thunk
      const { value, oracleCalls } = evaluator(result.thunk);
      completeForce(ctx, promise.id, value, oracleCalls);
      return value;
  }
}

// ─────────────────────────────────────────────────────────────────
// Promise Introspection
// ─────────────────────────────────────────────────────────────────

/**
 * Get statistics about promises in the store.
 */
export function getPromiseStats(ctx: StreamContext): {
  total: number;
  unforced: number;
  forcing: number;
  forced: number;
  oracleCalls: number;
} {
  let unforced = 0;
  let forcing = 0;
  let forced = 0;

  for (const cell of ctx.store.values()) {
    switch (cell.tag) {
      case "Unforced":
        unforced++;
        break;
      case "Forcing":
        forcing++;
        break;
      case "Forced":
        forced++;
        break;
    }
  }

  return {
    total: ctx.store.size,
    unforced,
    forcing,
    forced,
    oracleCalls: ctx.oracleCallCount,
  };
}

/**
 * Get all promise IDs in the store.
 */
export function getAllPromiseIds(ctx: StreamContext): PromiseId[] {
  return Array.from(ctx.store.keys());
}

/**
 * Get forced promise IDs only.
 */
export function getForcedPromiseIds(ctx: StreamContext): PromiseId[] {
  const ids: PromiseId[] = [];
  for (const [id, cell] of ctx.store.entries()) {
    if (cell.tag === "Forced") {
      ids.push(id);
    }
  }
  return ids;
}

/**
 * Get unforced promise IDs only.
 */
export function getUnforcedPromiseIds(ctx: StreamContext): PromiseId[] {
  const ids: PromiseId[] = [];
  for (const [id, cell] of ctx.store.entries()) {
    if (cell.tag === "Unforced") {
      ids.push(id);
    }
  }
  return ids;
}

// ─────────────────────────────────────────────────────────────────
// Event Analysis
// ─────────────────────────────────────────────────────────────────

/**
 * Count force starts for a specific promise.
 */
export function countForceStarts(ctx: StreamContext, id: PromiseId): number {
  return ctx.events.filter(
    e => e.tag === "PromiseForceStart" && e.id === id
  ).length;
}

/**
 * Count total force starts.
 */
export function countTotalForceStarts(ctx: StreamContext): number {
  return ctx.events.filter(e => e.tag === "PromiseForceStart").length;
}

/**
 * Count cache hits.
 */
export function countCacheHits(ctx: StreamContext): number {
  return ctx.events.filter(e => e.tag === "PromiseForceHit").length;
}

/**
 * Get total oracle calls from events.
 */
export function getTotalOracleCallsFromEvents(ctx: StreamContext): number {
  return ctx.events
    .filter((e): e is Extract<PromiseEvent, { tag: "PromiseForceDone" }> =>
      e.tag === "PromiseForceDone"
    )
    .reduce((sum, e) => sum + e.oracleCalls, 0);
}

/**
 * Find promises that were forced multiple times (should be 0 with memoization).
 */
export function findMultiForced(ctx: StreamContext): PromiseId[] {
  const forceCounts = new Map<PromiseId, number>();

  for (const event of ctx.events) {
    if (event.tag === "PromiseForceStart") {
      forceCounts.set(event.id, (forceCounts.get(event.id) ?? 0) + 1);
    }
  }

  const multiForced: PromiseId[] = [];
  for (const [id, count] of forceCounts) {
    if (count > 1) {
      multiForced.push(id);
    }
  }

  return multiForced;
}

// ─────────────────────────────────────────────────────────────────
// Promise Value Helpers
// ─────────────────────────────────────────────────────────────────

/**
 * Check if a value is a promise.
 */
export function isPromise(v: Val): v is PromiseVal {
  return v.tag === "Promise";
}

/**
 * Make a promise value (without creating in store).
 */
export function makePromiseVal(id: PromiseId, label?: string): PromiseVal {
  return { tag: "Promise", id, label };
}

// ─────────────────────────────────────────────────────────────────
// Reset State (for testing)
// ─────────────────────────────────────────────────────────────────

/**
 * Clear a stream context.
 */
export function clearStreamContext(ctx: StreamContext): void {
  ctx.store.clear();
  ctx.events.length = 0;
  ctx.oracleCallCount = 0;
  ctx.forceCount = 0;
  ctx.fuel = ctx.config.maxFuel;
}

/**
 * Reset all promise state (for testing).
 */
export function resetPromiseState(): void {
  resetPromiseIds();
}
