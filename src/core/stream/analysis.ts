// src/core/stream/analysis.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Prompt 16: Stream analysis - strictness, productivity, fusion, space leaks

import type { Val, PromiseId } from "../eval/values";
import { VUnit } from "../eval/values";
import type { Hash } from "../artifacts/hash";
import { sha256JSON } from "../artifacts/hash";
import type {
  StreamContext,
  StrictnessInfo,
  ProductivityInfo,
  FusionCandidate,
  SpaceLeakInfo,
  StreamAnalysisResult,
  PromiseEvent,
} from "./types";
import { createStreamContext } from "./types";
import {
  countTotalForceStarts,
  countCacheHits,
  getTotalOracleCallsFromEvents,
  findMultiForced,
  getAllPromiseIds,
  getForcedPromiseIds,
  getUnforcedPromiseIds,
} from "./promise";
import {
  isStreamNull,
  streamCar,
  streamCdr,
  isStream,
  forceN,
} from "./stream";

// ─────────────────────────────────────────────────────────────────
// Strictness Analysis
// ─────────────────────────────────────────────────────────────────

/**
 * Analyze strictness based on current context state.
 *
 * Call this after forcing a stream to see how many elements were demanded vs forced.
 */
export function analyzeStrictness(
  ctx: StreamContext,
  demandedCount: number
): StrictnessInfo {
  const forceStarts = countTotalForceStarts(ctx);
  const cacheHits = countCacheHits(ctx);
  const oracleCalls = ctx.oracleCallCount;
  const multiForced = findMultiForced(ctx);

  // Check if we forced more than demanded (forced ahead)
  const forcedAhead = forceStarts > demandedCount;

  return {
    demandedCount,
    forcedCount: forceStarts,
    forcedAhead,
    multiForced,
    oracleCalls,
  };
}

// ─────────────────────────────────────────────────────────────────
// Productivity Analysis
// ─────────────────────────────────────────────────────────────────

/**
 * Analyze productivity of a stream.
 *
 * Productivity = "forcing the next cell terminates within fuel"
 */
export function analyzeProductivity(
  ctx: StreamContext,
  stream: Val,
  evaluator?: (thunk: Val) => { value: Val; oracleCalls: number },
  fuel?: number
): ProductivityInfo {
  // Save original fuel
  const originalFuel = ctx.fuel;
  const maxFuel = fuel ?? ctx.config.maxFuel;
  ctx.fuel = maxFuel;

  let producedCount = 0;
  let current = stream;
  let productive = true;
  let reason: string | undefined;

  try {
    while (!isStreamNull(current) && ctx.fuel > 0) {
      // Try to access the head (should be immediate)
      streamCar(current);
      producedCount++;

      // Try to force the tail (this is where non-productivity shows)
      const fuelBefore = ctx.fuel;
      current = streamCdr(ctx, current, evaluator);

      // If we used all fuel on one step, might be non-productive
      if (ctx.fuel <= 0 && fuelBefore > 0 && !isStreamNull(current)) {
        productive = false;
        reason = "Tail force consumed all remaining fuel";
        break;
      }
    }
  } catch (e) {
    productive = false;
    reason = e instanceof Error ? e.message : "Unknown error during productivity check";
  }

  const fuelRemaining = Math.max(0, ctx.fuel);
  ctx.fuel = originalFuel; // Restore fuel

  return {
    productive,
    producedCount,
    fuelRemaining,
    reason,
  };
}

// ─────────────────────────────────────────────────────────────────
// Fusion / Deforestation Analysis
// ─────────────────────────────────────────────────────────────────

/**
 * Identify fusion candidates in a stream pipeline.
 */
export function identifyFusionCandidates(
  ctx: StreamContext,
  pipelineDescription: Val
): FusionCandidate[] {
  const candidates: FusionCandidate[] = [];

  // Look for CSE opportunities
  const cseCandidate = findCSEOpportunity(ctx);
  if (cseCandidate) {
    candidates.push(cseCandidate);
  }

  // Look for map-map fusion
  const mapMapCandidate = findMapMapFusion(pipelineDescription);
  if (mapMapCandidate) {
    candidates.push(mapMapCandidate);
  }

  // Look for filter-map fusion
  const filterMapCandidate = findFilterMapFusion(pipelineDescription);
  if (filterMapCandidate) {
    candidates.push(filterMapCandidate);
  }

  return candidates;
}

/**
 * Find common subexpression elimination opportunities.
 */
function findCSEOpportunity(ctx: StreamContext): FusionCandidate | null {
  const oracleCalls = new Map<string, number>();

  for (const event of ctx.events) {
    if (event.tag === "PromiseForceDone" && event.oracleCalls > 0) {
      const key = event.id.split("-")[0];
      oracleCalls.set(key, (oracleCalls.get(key) ?? 0) + 1);
    }
  }

  for (const [key, count] of oracleCalls) {
    if (count > 1) {
      return {
        id: `cse-${key}`,
        kind: "cse",
        description: `Operation '${key}' called ${count} times - consider memoizing`,
        estimatedSaving: count - 1,
        pattern: { tag: "Sym", name: key },
        rewrite: { tag: "Sym", name: `memoized-${key}` },
        confidence: 0.7,
      };
    }
  }

  return null;
}

/**
 * Find map-map fusion opportunities.
 */
function findMapMapFusion(desc: Val): FusionCandidate | null {
  if (desc.tag === "List" && desc.elements.length >= 1) {
    const head = desc.elements[0];
    if (head.tag === "Sym" && head.name === "map-map") {
      return {
        id: "map-map-fusion",
        kind: "map-map",
        description: "Two consecutive stream-map operations can be fused",
        estimatedSaving: 1,
        pattern: desc,
        rewrite: {
          tag: "List",
          elements: [
            { tag: "Sym", name: "map" },
            { tag: "Sym", name: "composed" },
          ],
        },
        confidence: 0.9,
      };
    }
  }
  return null;
}

/**
 * Find filter-map fusion opportunities.
 */
function findFilterMapFusion(desc: Val): FusionCandidate | null {
  if (desc.tag === "List" && desc.elements.length >= 1) {
    const head = desc.elements[0];
    if (head.tag === "Sym" && head.name === "filter-map") {
      return {
        id: "filter-map-fusion",
        kind: "filter-map",
        description: "Filter after map might be fusible",
        estimatedSaving: 0.5,
        pattern: desc,
        rewrite: { tag: "Sym", name: "fused-filter-map" },
        confidence: 0.6,
      };
    }
  }
  return null;
}

/**
 * Apply a fusion rewrite.
 */
export function applyFusion(
  ctx: StreamContext,
  candidate: FusionCandidate
): Val {
  // Return the rewritten pattern
  return candidate.rewrite;
}

/**
 * Compare oracle cost with and without fusion.
 */
export function compareFusionCost(
  ctx: StreamContext,
  candidate: FusionCandidate,
  evaluator?: (thunk: Val) => { value: Val; oracleCalls: number }
): {
  originalCost: number;
  fusedCost: number;
  saving: number;
  percentSaved: number;
} {
  const originalCost = ctx.oracleCallCount;
  const estimatedSaving = candidate.estimatedSaving;
  const fusedCost = Math.max(0, originalCost - estimatedSaving);
  const saving = estimatedSaving;
  const percentSaved = originalCost > 0 ? (saving / originalCost) * 100 : 0;

  return {
    originalCost,
    fusedCost,
    saving,
    percentSaved,
  };
}

// ─────────────────────────────────────────────────────────────────
// Space Leak Detection
// ─────────────────────────────────────────────────────────────────

/**
 * Analyze potential space leaks in stream processing.
 */
export function analyzeSpaceLeaks(ctx: StreamContext): SpaceLeakInfo {
  const total = ctx.store.size;
  const forcedIds = getForcedPromiseIds(ctx);
  const unforcedIds = getUnforcedPromiseIds(ctx);

  // Calculate growth rate based on unforced vs forced ratio
  const growthRate = total > 0 ? unforcedIds.length / total : 0;

  // Long retained = unforced promises
  const longRetained = unforcedIds.slice(0, 10);

  // Determine cause and suggestion
  let cause: SpaceLeakInfo["cause"];
  let suggestion: string | undefined;
  let leakSuspected = false;

  if (unforcedIds.length > 10) {
    leakSuspected = true;
    if (growthRate > 0.5) {
      cause = "head-retention";
      suggestion = "Avoid retaining reference to stream head while traversing. Use stream-fold instead.";
    } else {
      cause = "closure-capture";
      suggestion = "Check for closures that capture stream references.";
    }
  }

  return {
    leakSuspected,
    retainedGrowthRate: growthRate,
    longRetained,
    cause,
    suggestion,
  };
}

// ─────────────────────────────────────────────────────────────────
// Event Filtering
// ─────────────────────────────────────────────────────────────────

/**
 * Get all events for a specific promise ID.
 */
export function getPromiseEventsForId(
  ctx: StreamContext,
  id: PromiseId
): PromiseEvent[] {
  return ctx.events.filter(e => {
    if ("id" in e) {
      return e.id === id;
    }
    return false;
  });
}

/**
 * Get the force timeline (all force-related events in order).
 */
export function getForceTimeline(ctx: StreamContext): PromiseEvent[] {
  return ctx.events
    .filter(e =>
      e.tag === "PromiseForceStart" ||
      e.tag === "PromiseForceDone" ||
      e.tag === "PromiseForceHit" ||
      e.tag === "PromiseForceJoin"
    )
    .sort((a, b) => a.timestamp - b.timestamp);
}

// ─────────────────────────────────────────────────────────────────
// Combined Analysis
// ─────────────────────────────────────────────────────────────────

/**
 * Run a comprehensive analysis on a stream pipeline.
 */
export function analyzeStream(
  ctx: StreamContext,
  stream: Val,
  options: {
    strictness?: boolean;
    productivity?: boolean;
    fusion?: boolean;
    spaceLeaks?: boolean;
    demandCount?: number;
    fuel?: number;
    evaluator?: (thunk: Val) => { value: Val; oracleCalls: number };
  } = {}
): StreamAnalysisResult {
  const result: StreamAnalysisResult = {
    events: ctx.events,
  };

  const demandCount = options.demandCount ?? 10;

  if (options.strictness !== false) {
    result.strictness = analyzeStrictness(ctx, demandCount);
  }

  if (options.productivity !== false) {
    result.productivity = analyzeProductivity(
      ctx,
      stream,
      options.evaluator,
      options.fuel
    );
  }

  if (options.fusion) {
    result.fusionCandidates = identifyFusionCandidates(ctx, stream);
  }

  if (options.spaceLeaks) {
    result.spaceLeaks = analyzeSpaceLeaks(ctx);
  }

  return result;
}
