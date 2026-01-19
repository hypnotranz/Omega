// src/core/concurrency/singleflight.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Prompt 13: Singleflight memoization for deduplicating concurrent oracle calls

import type { Val, FiberId, IVarVal } from "../eval/values";
import { VUnit } from "../eval/values";
import type { Hash } from "../artifacts/hash";
import { sha256JSON } from "../artifacts/hash";
import type { SchedulerState } from "./scheduler";
import { createIVar, putIVar, takeIVar, getIVarState, isIVarFull } from "./sync";

// ─────────────────────────────────────────────────────────────────
// Singleflight types
// ─────────────────────────────────────────────────────────────────

/**
 * SingleflightKey: Hash of the call signature for deduplication.
 */
export type SingleflightKey = Hash;

/**
 * SingleflightEntry: An in-flight or completed computation.
 */
export type SingleflightEntry = {
  /** The IVar holding the result */
  ivar: IVarVal;
  /** The fiber that initiated the call */
  initiator: FiberId;
  /** Fibers waiting for the result */
  waiters: FiberId[];
  /** Whether the computation is complete */
  complete: boolean;
  /** The result (if complete) */
  result?: Val;
  /** Creation timestamp */
  createdAt: number;
};

/**
 * SingleflightGroup: A group of deduplicated calls.
 */
export type SingleflightGroup = {
  /** Active entries by key */
  entries: Map<SingleflightKey, SingleflightEntry>;
  /** Name for debugging */
  name?: string;
  /** Statistics */
  stats: {
    hits: number;
    misses: number;
    completions: number;
  };
};

// ─────────────────────────────────────────────────────────────────
// Singleflight registry
// ─────────────────────────────────────────────────────────────────

const groupRegistry = new Map<string, SingleflightGroup>();
let nextGroupId = 0;

/**
 * Generate a unique group ID.
 */
function genGroupId(): string {
  return `sfgroup-${nextGroupId++}`;
}

/**
 * Reset the singleflight registry (for testing).
 */
export function resetSingleflightRegistry(): void {
  groupRegistry.clear();
  nextGroupId = 0;
}

// ─────────────────────────────────────────────────────────────────
// Singleflight group operations
// ─────────────────────────────────────────────────────────────────

/**
 * Create a new singleflight group.
 */
export function createSingleflightGroup(name?: string): { id: string; group: SingleflightGroup } {
  const id = genGroupId();
  const group: SingleflightGroup = {
    entries: new Map(),
    name,
    stats: {
      hits: 0,
      misses: 0,
      completions: 0,
    },
  };
  groupRegistry.set(id, group);
  return { id, group };
}

/**
 * Get a singleflight group by ID.
 */
export function getSingleflightGroup(id: string): SingleflightGroup | undefined {
  return groupRegistry.get(id);
}

// ─────────────────────────────────────────────────────────────────
// Singleflight call operations
// ─────────────────────────────────────────────────────────────────

/**
 * Result of a singleflight request.
 */
export type SingleflightResult =
  | { tag: "hit"; result: Val }           // Result already available
  | { tag: "initiated"; ivar: IVarVal }   // This fiber initiates the call
  | { tag: "waiting"; ivar: IVarVal };    // Another fiber is computing

/**
 * Request a singleflight call.
 *
 * If a call with the same key is in-flight, returns the IVar to wait on.
 * If the call is new, creates an entry and returns the IVar for the caller to fill.
 */
export function singleflightDo(
  groupId: string,
  key: SingleflightKey,
  fiberId: FiberId
): SingleflightResult {
  const group = groupRegistry.get(groupId);
  if (!group) {
    // Group doesn't exist, create one inline
    const { id } = createSingleflightGroup();
    return singleflightDo(id, key, fiberId);
  }

  // Check for existing entry
  const existing = group.entries.get(key);
  if (existing) {
    if (existing.complete && existing.result !== undefined) {
      // Already complete, return result directly
      group.stats.hits++;
      return { tag: "hit", result: existing.result };
    }

    // In-flight, add to waiters
    if (!existing.waiters.includes(fiberId)) {
      existing.waiters.push(fiberId);
    }
    group.stats.hits++;
    return { tag: "waiting", ivar: existing.ivar };
  }

  // New call - create entry
  group.stats.misses++;
  const ivar = createIVar(`sf-${key.slice(0, 8)}`);
  const entry: SingleflightEntry = {
    ivar,
    initiator: fiberId,
    waiters: [],
    complete: false,
    createdAt: Date.now(),
  };
  group.entries.set(key, entry);

  return { tag: "initiated", ivar };
}

/**
 * Complete a singleflight call with a result.
 */
export function singleflightComplete(
  scheduler: SchedulerState,
  groupId: string,
  key: SingleflightKey,
  result: Val,
  fiberId: FiberId
): boolean {
  const group = groupRegistry.get(groupId);
  if (!group) return false;

  const entry = group.entries.get(key);
  if (!entry) return false;

  // Only the initiator can complete
  if (entry.initiator !== fiberId) return false;

  // Already complete?
  if (entry.complete) return false;

  // Mark complete and store result
  entry.complete = true;
  entry.result = result;
  group.stats.completions++;

  // Put value in IVar (wakes all waiters)
  putIVar(scheduler, entry.ivar.id, result, fiberId);

  return true;
}

/**
 * Wait for a singleflight result (blocking).
 * Returns the result if available, undefined if blocked.
 */
export function singleflightWait(
  scheduler: SchedulerState,
  groupId: string,
  key: SingleflightKey,
  fiberId: FiberId
): Val | undefined {
  const group = groupRegistry.get(groupId);
  if (!group) return undefined;

  const entry = group.entries.get(key);
  if (!entry) return undefined;

  // Check if already complete
  if (entry.complete && entry.result !== undefined) {
    return entry.result;
  }

  // Wait on the IVar
  return takeIVar(scheduler, entry.ivar.id, fiberId);
}

/**
 * Get the result of a singleflight call (non-blocking).
 */
export function singleflightTryGet(
  groupId: string,
  key: SingleflightKey
): Val | undefined {
  const group = groupRegistry.get(groupId);
  if (!group) return undefined;

  const entry = group.entries.get(key);
  if (!entry || !entry.complete) return undefined;

  return entry.result;
}

/**
 * Check if a singleflight call is in-flight.
 */
export function singleflightInFlight(groupId: string, key: SingleflightKey): boolean {
  const group = groupRegistry.get(groupId);
  if (!group) return false;

  const entry = group.entries.get(key);
  return entry !== undefined && !entry.complete;
}

/**
 * Get singleflight group statistics.
 */
export function singleflightStats(groupId: string): { hits: number; misses: number; completions: number } | undefined {
  const group = groupRegistry.get(groupId);
  if (!group) return undefined;
  return { ...group.stats };
}

// ─────────────────────────────────────────────────────────────────
// Convenience: Hash a call signature
// ─────────────────────────────────────────────────────────────────

/**
 * Create a singleflight key from a function name and arguments.
 */
export function singleflightKey(fnName: string, args: Val[]): SingleflightKey {
  return sha256JSON({ fn: fnName, args });
}

// ─────────────────────────────────────────────────────────────────
// Memoization table (longer-lived caching)
// ─────────────────────────────────────────────────────────────────

/**
 * MemoEntry: A cached result with metadata.
 */
export type MemoEntry = {
  result: Val;
  createdAt: number;
  accessCount: number;
  lastAccessedAt: number;
};

/**
 * MemoTable: A memoization table for caching results.
 */
export type MemoTable = {
  entries: Map<SingleflightKey, MemoEntry>;
  name?: string;
  maxSize: number;
  stats: {
    hits: number;
    misses: number;
    evictions: number;
  };
};

const memoRegistry = new Map<string, MemoTable>();
let nextMemoId = 0;

/**
 * Create a new memo table.
 */
export function createMemoTable(maxSize: number = 1000, name?: string): { id: string; table: MemoTable } {
  const id = `memo-${nextMemoId++}`;
  const table: MemoTable = {
    entries: new Map(),
    name,
    maxSize,
    stats: {
      hits: 0,
      misses: 0,
      evictions: 0,
    },
  };
  memoRegistry.set(id, table);
  return { id, table };
}

/**
 * Get a value from the memo table.
 */
export function memoGet(tableId: string, key: SingleflightKey): Val | undefined {
  const table = memoRegistry.get(tableId);
  if (!table) return undefined;

  const entry = table.entries.get(key);
  if (!entry) {
    table.stats.misses++;
    return undefined;
  }

  // Update access statistics
  entry.accessCount++;
  entry.lastAccessedAt = Date.now();
  table.stats.hits++;

  return entry.result;
}

/**
 * Set a value in the memo table.
 */
export function memoSet(tableId: string, key: SingleflightKey, value: Val): void {
  const table = memoRegistry.get(tableId);
  if (!table) return;

  // Evict if at capacity (simple LRU approximation)
  if (table.entries.size >= table.maxSize && !table.entries.has(key)) {
    // Find least recently accessed entry
    let oldestKey: SingleflightKey | undefined;
    let oldestTime = Infinity;

    for (const [k, e] of table.entries) {
      if (e.lastAccessedAt < oldestTime) {
        oldestTime = e.lastAccessedAt;
        oldestKey = k;
      }
    }

    if (oldestKey) {
      table.entries.delete(oldestKey);
      table.stats.evictions++;
    }
  }

  const now = Date.now();
  table.entries.set(key, {
    result: value,
    createdAt: now,
    accessCount: 1,
    lastAccessedAt: now,
  });
}

/**
 * Clear a memo table.
 */
export function memoClear(tableId: string): void {
  const table = memoRegistry.get(tableId);
  if (table) {
    table.entries.clear();
  }
}

/**
 * Get memo table statistics.
 */
export function memoStats(tableId: string): { hits: number; misses: number; evictions: number; size: number } | undefined {
  const table = memoRegistry.get(tableId);
  if (!table) return undefined;
  return {
    ...table.stats,
    size: table.entries.size,
  };
}

/**
 * Reset memo registry (for testing).
 */
export function resetMemoRegistry(): void {
  memoRegistry.clear();
  nextMemoId = 0;
}
