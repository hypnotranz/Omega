// src/core/stream/types.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Prompt 16: Streams + Laziness - Core types

import type { Val, PromiseId } from "../eval/values";
import type { Hash } from "../artifacts/hash";

// ─────────────────────────────────────────────────────────────────
// Promise Store Types
// ─────────────────────────────────────────────────────────────────

/**
 * PromiseCell: Store-side state for a promise.
 *
 * Three states:
 * - Unforced: Has a thunk waiting to be evaluated
 * - Forcing: Currently being evaluated (singleflight coordination)
 * - Forced: Evaluation complete, value cached
 */
export type PromiseCell =
  | { tag: "Unforced"; thunk: Val; createdAt: number }
  | { tag: "Forcing"; ivarId: string; startedAt: number }
  | { tag: "Forced"; value: Val; forcedAt: number };

/**
 * PromiseStore: Store for managing promise state.
 */
export type PromiseStore = Map<PromiseId, PromiseCell>;

/**
 * PromiseEvent: Events logged during promise operations.
 */
export type PromiseEvent =
  | { tag: "PromiseCreated"; id: PromiseId; thunkHash: Hash; timestamp: number }
  | { tag: "PromiseForceStart"; id: PromiseId; timestamp: number }
  | { tag: "PromiseForceHit"; id: PromiseId; timestamp: number }
  | { tag: "PromiseForceDone"; id: PromiseId; valueHash: Hash; oracleCalls: number; timestamp: number }
  | { tag: "PromiseForceJoin"; id: PromiseId; waiterId: string; timestamp: number };

// ─────────────────────────────────────────────────────────────────
// Stream Types
// ─────────────────────────────────────────────────────────────────

/**
 * StreamCell: Representation of a stream cell.
 *
 * Either empty or a head with a tail (promise or receipt ref).
 */
export type StreamCell =
  | { tag: "Empty" }
  | { tag: "Cons"; head: Val; tail: Val };

/**
 * StreamSegment: A materialized segment of a stream.
 *
 * Used for receipt-backed staging.
 */
export type StreamSegment = {
  /** Elements in this segment */
  elements: Val[];
  /** Hash of the continuation (if any) */
  continuationHash?: Hash;
  /** Whether this segment is terminal */
  terminal: boolean;
};

/**
 * StreamReceipt: Receipt for a stream segment.
 */
export type StreamReceipt = {
  rid: Hash;
  segmentHash: Hash;
  elementCount: number;
  createdAt: number;
};

// ─────────────────────────────────────────────────────────────────
// Stream Analysis Types
// ─────────────────────────────────────────────────────────────────

/**
 * StrictnessInfo: Information about demand/strictness.
 */
export type StrictnessInfo = {
  /** How many elements were demanded */
  demandedCount: number;
  /** How many promises were forced */
  forcedCount: number;
  /** Whether the consumer forced beyond demanded prefix */
  forcedAhead: boolean;
  /** Promises that were forced multiple times (potential bug) */
  multiForced: PromiseId[];
  /** Oracle calls triggered */
  oracleCalls: number;
};

/**
 * ProductivityInfo: Information about productivity.
 */
export type ProductivityInfo = {
  /** Whether the stream is productive under the given fuel */
  productive: boolean;
  /** How many elements were produced before fuel exhaustion */
  producedCount: number;
  /** Fuel remaining */
  fuelRemaining: number;
  /** Reason if non-productive */
  reason?: string;
};

/**
 * FusionCandidate: A candidate for stream fusion.
 */
export type FusionCandidate = {
  id: string;
  kind: "map-map" | "filter-map" | "map-filter" | "cse" | "deforest";
  description: string;
  /** Estimated oracle call reduction */
  estimatedSaving: number;
  /** Original pattern (AST-like) */
  pattern: Val;
  /** Rewritten pattern */
  rewrite: Val;
  /** Confidence score */
  confidence: number;
};

/**
 * SpaceLeakInfo: Information about potential space leaks.
 */
export type SpaceLeakInfo = {
  /** Whether a leak is suspected */
  leakSuspected: boolean;
  /** Growth rate of retained promises */
  retainedGrowthRate: number;
  /** Promises that are retained longer than expected */
  longRetained: PromiseId[];
  /** Suspected cause */
  cause?: "head-retention" | "closure-capture" | "accumulator" | "unknown";
  /** Suggested fix */
  suggestion?: string;
};

/**
 * StreamAnalysisResult: Result of stream analysis.
 */
export type StreamAnalysisResult = {
  strictness?: StrictnessInfo;
  productivity?: ProductivityInfo;
  fusionCandidates?: FusionCandidate[];
  spaceLeaks?: SpaceLeakInfo;
  /** Events collected during analysis */
  events: PromiseEvent[];
};

// ─────────────────────────────────────────────────────────────────
// Stream Configuration
// ─────────────────────────────────────────────────────────────────

/**
 * StreamConfig: Configuration for stream operations.
 */
export type StreamConfig = {
  /** Maximum fuel for productivity analysis */
  maxFuel: number;
  /** Enable event logging */
  logging: boolean;
  /** Enable memoization (normally always on) */
  memoization: boolean;
  /** Enable singleflight for concurrent forces */
  singleflight: boolean;
  /** Profile name for governance */
  profileName?: string;
};

export const DEFAULT_STREAM_CONFIG: StreamConfig = {
  maxFuel: 1000,
  logging: true,
  memoization: true,
  singleflight: true,
};

// ─────────────────────────────────────────────────────────────────
// Stream Context
// ─────────────────────────────────────────────────────────────────

/**
 * StreamContext: Context for stream operations.
 */
export type StreamContext = {
  /** Promise store */
  store: PromiseStore;
  /** Event log */
  events: PromiseEvent[];
  /** Configuration */
  config: StreamConfig;
  /** Oracle call counter */
  oracleCallCount: number;
  /** Force counter (for debugging) */
  forceCount: number;
  /** Current fuel (for bounded execution) */
  fuel: number;
};

/**
 * Create a fresh stream context.
 */
export function createStreamContext(config: Partial<StreamConfig> = {}): StreamContext {
  return {
    store: new Map(),
    events: [],
    config: { ...DEFAULT_STREAM_CONFIG, ...config },
    oracleCallCount: 0,
    forceCount: 0,
    fuel: config.maxFuel ?? DEFAULT_STREAM_CONFIG.maxFuel,
  };
}

/**
 * Empty stream sentinel.
 */
export const EMPTY_STREAM_SYMBOL = Symbol.for("empty-stream");

/**
 * Check if a value is the empty stream sentinel.
 */
export function isEmptyStreamSentinel(v: Val): boolean {
  return v.tag === "Sym" && v.name === "empty-stream";
}

/**
 * Create the empty stream value.
 */
export function makeEmptyStream(): Val {
  return { tag: "Sym", name: "empty-stream" };
}
