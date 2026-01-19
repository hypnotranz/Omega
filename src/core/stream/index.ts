// src/core/stream/index.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Prompt 16: Streams + Laziness - Module exports

// ─────────────────────────────────────────────────────────────────
// Types
// ─────────────────────────────────────────────────────────────────

export type {
  // Promise types
  PromiseCell,
  PromiseStore,
  PromiseEvent,
  // Stream types
  StreamCell,
  StreamSegment,
  StreamReceipt,
  // Analysis types
  StrictnessInfo,
  ProductivityInfo,
  FusionCandidate,
  SpaceLeakInfo,
  StreamAnalysisResult,
  // Configuration
  StreamConfig,
  StreamContext,
} from "./types";

export {
  DEFAULT_STREAM_CONFIG,
  createStreamContext,
  EMPTY_STREAM_SYMBOL,
  isEmptyStreamSentinel,
  makeEmptyStream,
} from "./types";

// ─────────────────────────────────────────────────────────────────
// Promise Operations
// ─────────────────────────────────────────────────────────────────

export type { ForceResult } from "./promise";

export {
  // ID generation
  freshPromiseId,
  resetPromiseIds,
  // Store operations
  createPromiseStore,
  createPromise,
  getPromiseCell,
  isPromiseForced,
  isPromiseForcing,
  getForcedValue,
  // Force operations
  beginForce,
  completeForce,
  forceSync,
  // Introspection
  getPromiseStats,
  getAllPromiseIds,
  getForcedPromiseIds,
  getUnforcedPromiseIds,
  // Event analysis
  countForceStarts,
  countTotalForceStarts,
  countCacheHits,
  getTotalOracleCallsFromEvents,
  findMultiForced,
  // Value helpers
  isPromise,
  makePromiseVal,
  // Reset
  clearStreamContext,
  resetPromiseState,
} from "./promise";

// ─────────────────────────────────────────────────────────────────
// Stream Operations
// ─────────────────────────────────────────────────────────────────

export {
  // Constructors
  emptyStream,
  consStream,
  consStreamWithPromise,
  // Accessors
  streamCar,
  streamCdr,
  streamCdrPromise,
  isStreamEmpty,
  isStreamNull,
  isStream,
  // Combinators
  streamMap,
  streamFilter,
  streamTake,
  streamDrop,
  streamAppend,
  streamFlatMap,
  streamZip,
  streamFold,
  streamReduce,
  // Generators
  streamRepeat,
  streamIterate,
  streamRange,
  listToStream,
  // Collectors
  streamToList,
  streamForEach,
  streamLength,
  // Segment operations
  materializeSegment,
  createStreamReceipt,
  hydrateFromReceipt,
  // Force helpers
  forceHead,
  forceTail,
  deepForce,
  forceN,
  // Cleanup
  clearThunkRegistry,
} from "./stream";

// ─────────────────────────────────────────────────────────────────
// Analysis Operations
// ─────────────────────────────────────────────────────────────────

export {
  // Core analysis
  analyzeStream,
  // Individual analyses
  analyzeStrictness,
  analyzeProductivity,
  analyzeSpaceLeaks,
  identifyFusionCandidates,
  // Fusion utilities
  compareFusionCost,
  applyFusion,
  // Event filtering
  getPromiseEventsForId,
  getForceTimeline,
} from "./analysis";
