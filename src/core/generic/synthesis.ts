// src/core/generic/synthesis.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Prompt 14: Method and coercion synthesis via inference + search

import type { Val } from "../eval/values";
import { VUnit, VTrue, VFalse } from "../eval/values";
import type { Hash } from "../artifacts/hash";
import { sha256JSON } from "../artifacts/hash";
import {
  type TypeTag,
  type TypeSignature,
  type GenericMissPayload,
  type SynthesisCandidate,
  type SynthesisResult,
  type CommitRequest,
  type CommitResult,
  type ObligationRef,
} from "./types";
import {
  getRegistry,
  defMethod,
  defCoercion,
  commitMethod,
  commitCoercion,
  incrementSynthesisCalls,
  logGenericEvent,
} from "./registry";

// ─────────────────────────────────────────────────────────────────
// Singleflight for Synthesis
// ─────────────────────────────────────────────────────────────────

/**
 * SynthesisKey: Key for deduplicating synthesis requests.
 */
export type SynthesisKey = string;

/**
 * Generate a synthesis key from a miss payload.
 */
export function generateSynthesisKey(miss: GenericMissPayload): SynthesisKey {
  return sha256JSON({
    op: miss.op,
    signature: miss.signature,
    registryId: miss.registryId,
  });
}

/**
 * In-flight synthesis tracking for singleflight.
 */
const inFlightSynthesis = new Map<SynthesisKey, {
  promise: Promise<SynthesisResult>;
  startedAt: number;
}>();

/**
 * Check if a synthesis is in-flight.
 */
export function isSynthesisInFlight(key: SynthesisKey): boolean {
  return inFlightSynthesis.has(key);
}

/**
 * Get the in-flight synthesis promise.
 */
export function getInFlightSynthesis(key: SynthesisKey): Promise<SynthesisResult> | undefined {
  return inFlightSynthesis.get(key)?.promise;
}

/**
 * Register an in-flight synthesis.
 */
export function registerInFlightSynthesis(
  key: SynthesisKey,
  promise: Promise<SynthesisResult>
): void {
  inFlightSynthesis.set(key, { promise, startedAt: Date.now() });

  // Clean up after completion
  promise.finally(() => {
    inFlightSynthesis.delete(key);
  });
}

/**
 * Clear in-flight synthesis (for testing).
 */
export function clearInFlightSynthesis(): void {
  inFlightSynthesis.clear();
}

// ─────────────────────────────────────────────────────────────────
// Synthesis Strategies
// ─────────────────────────────────────────────────────────────────

/**
 * SynthesisStrategy: Strategy for generating method candidates.
 */
export type SynthesisStrategy = {
  name: string;
  generateCandidates: (miss: GenericMissPayload) => Promise<SynthesisCandidate[]>;
};

/**
 * Default synthesis strategies.
 */
export const defaultStrategies: SynthesisStrategy[] = [];

/**
 * Register a synthesis strategy.
 */
export function registerStrategy(strategy: SynthesisStrategy): void {
  defaultStrategies.push(strategy);
}

/**
 * Clear strategies (for testing).
 */
export function clearStrategies(): void {
  defaultStrategies.length = 0;
}

// ─────────────────────────────────────────────────────────────────
// Synthesis Orchestration
// ─────────────────────────────────────────────────────────────────

/**
 * SynthesisConfig: Configuration for synthesis.
 */
export type SynthesisConfig = {
  /** Maximum candidates to consider */
  maxCandidates: number;
  /** Minimum confidence threshold */
  minConfidence: number;
  /** Whether to require passing tests before returning */
  requireTests: boolean;
  /** Strategies to use */
  strategies: SynthesisStrategy[];
  /** Test runner function */
  testRunner?: (proc: Val, sig: TypeSignature) => Promise<boolean>;
  /** Inference function */
  inferenceFn?: (prompt: Val) => Promise<Val>;
};

export const DEFAULT_SYNTHESIS_CONFIG: SynthesisConfig = {
  maxCandidates: 5,
  minConfidence: 0.5,
  requireTests: true,
  strategies: [],
};

/**
 * Synthesize a method for a miss.
 */
export async function synthesizeMethod(
  miss: GenericMissPayload,
  config: Partial<SynthesisConfig> = {}
): Promise<SynthesisResult> {
  const fullConfig = { ...DEFAULT_SYNTHESIS_CONFIG, ...config };
  const key = generateSynthesisKey(miss);

  // Check for singleflight
  const existing = getInFlightSynthesis(key);
  if (existing) {
    return existing;
  }

  // Track synthesis call
  incrementSynthesisCalls(miss.registryId);

  // Create promise and register for singleflight
  const promise = doSynthesis(miss, fullConfig);
  registerInFlightSynthesis(key, promise);

  return promise;
}

/**
 * Internal synthesis implementation.
 */
async function doSynthesis(
  miss: GenericMissPayload,
  config: SynthesisConfig
): Promise<SynthesisResult> {
  logGenericEvent({
    tag: "synthesis",
    op: miss.op,
    signature: miss.signature,
    candidateCount: 0,
    timestamp: Date.now(),
  });

  // Collect candidates from all strategies
  const candidates: SynthesisCandidate[] = [];
  const strategies = config.strategies.length > 0 ? config.strategies : defaultStrategies;

  for (const strategy of strategies) {
    try {
      const strategyCandidates = await strategy.generateCandidates(miss);
      candidates.push(...strategyCandidates);
    } catch {
      // Strategy failed, continue with others
    }
  }

  // Filter by confidence
  const viableCandidates = candidates
    .filter(c => c.confidence >= config.minConfidence)
    .sort((a, b) => b.confidence - a.confidence)
    .slice(0, config.maxCandidates);

  if (viableCandidates.length === 0) {
    return {
      tag: "failure",
      reason: "No viable candidates found",
      candidates: candidates.slice(0, 3),
    };
  }

  // Test candidates if required
  if (config.requireTests && config.testRunner) {
    for (const candidate of viableCandidates) {
      const passed = await config.testRunner(candidate.proc, miss.signature);
      if (passed) {
        return {
          tag: "success",
          candidate,
          testsPassed: true,
        };
      }
    }

    return {
      tag: "failure",
      reason: "All candidates failed tests",
      candidates: viableCandidates,
    };
  }

  // Return best candidate without tests
  return {
    tag: "success",
    candidate: viableCandidates[0],
    testsPassed: false,
  };
}

// ─────────────────────────────────────────────────────────────────
// Commitment
// ─────────────────────────────────────────────────────────────────

/**
 * CommitConfig: Configuration for committing synthesized methods.
 */
export type CommitConfig = {
  /** Required capabilities for commit */
  requiredCaps: string[];
  /** Whether to verify obligations before commit */
  verifyObligations: boolean;
  /** Obligation verifier function */
  obligationVerifier?: (obligations: ObligationRef[]) => Promise<ObligationRef[]>;
};

export const DEFAULT_COMMIT_CONFIG: CommitConfig = {
  requiredCaps: ["cap.generic.method.define"],
  verifyObligations: true,
};

/**
 * Commit a synthesized method to the registry.
 */
export async function commitSynthesizedMethod(
  request: CommitRequest,
  availableCaps: Set<string>,
  config: Partial<CommitConfig> = {}
): Promise<CommitResult> {
  const fullConfig = { ...DEFAULT_COMMIT_CONFIG, ...config };

  // Check capabilities
  const missingCaps = fullConfig.requiredCaps.filter(cap => !availableCaps.has(cap));
  if (missingCaps.length > 0) {
    return {
      tag: "denied",
      reason: "Missing required capabilities",
      missingCaps,
    };
  }

  // Verify obligations if required
  if (fullConfig.verifyObligations && fullConfig.obligationVerifier) {
    const verifiedObligations = await fullConfig.obligationVerifier(request.obligations);
    const failedObligations = verifiedObligations.filter(o => !o.satisfied);

    if (failedObligations.length > 0) {
      return {
        tag: "failed",
        failedObligations,
      };
    }
  }

  // Perform commit based on kind
  if (request.kind === "method" && request.op && request.signature) {
    const hash = commitMethod(
      request.registryId,
      request.op,
      request.signature,
      request.proc
    );

    if (!hash) {
      return {
        tag: "denied",
        reason: "Failed to commit method to registry",
      };
    }

    const entry = {
      op: request.op,
      signature: request.signature,
      proc: request.proc,
      synthesized: true,
      commitHash: hash,
      createdAt: Date.now(),
    };

    logGenericEvent({
      tag: "commit",
      kind: "method",
      hash,
      timestamp: Date.now(),
    });

    return { tag: "committed", hash, entry };
  }

  if (request.kind === "coercion" && request.fromTag && request.toTag) {
    const hash = commitCoercion(
      request.registryId,
      request.fromTag,
      request.toTag,
      request.proc
    );

    if (!hash) {
      return {
        tag: "denied",
        reason: "Failed to commit coercion to registry",
      };
    }

    const entry = {
      fromTag: request.fromTag,
      toTag: request.toTag,
      proc: request.proc,
      lossy: false,
      synthesized: true,
      commitHash: hash,
      createdAt: Date.now(),
      cost: 1,
    };

    logGenericEvent({
      tag: "commit",
      kind: "coercion",
      hash,
      timestamp: Date.now(),
    });

    return { tag: "committed", hash, entry };
  }

  return {
    tag: "denied",
    reason: "Invalid commit request",
  };
}

// ─────────────────────────────────────────────────────────────────
// Obligation Management
// ─────────────────────────────────────────────────────────────────

let nextObligationId = 0;

/**
 * Create an obligation reference.
 */
export function createObligation(
  kind: ObligationRef["kind"],
  description?: string
): ObligationRef {
  return {
    id: `obligation-${nextObligationId++}`,
    kind,
    description,
    satisfied: false,
  };
}

/**
 * Mark an obligation as satisfied.
 */
export function satisfyObligation(obligation: ObligationRef): ObligationRef {
  return { ...obligation, satisfied: true };
}

/**
 * Create test obligations for a method.
 */
export function createTestObligations(
  op: string,
  signature: TypeSignature,
  testCases: Array<{ input: Val; expected: Val }>
): ObligationRef[] {
  return testCases.map((tc, i) =>
    createObligation("test", `Test ${i + 1}: ${op}(${signature.join(", ")})`)
  );
}

/**
 * Create metamorphic test obligations.
 */
export function createMetamorphicObligations(
  op: string,
  properties: string[]
): ObligationRef[] {
  return properties.map(prop =>
    createObligation("metamorphic", `Metamorphic: ${prop}`)
  );
}

// ─────────────────────────────────────────────────────────────────
// Cost-Aware Selection
// ─────────────────────────────────────────────────────────────────

/**
 * SelectionCriteria: Criteria for selecting among candidates.
 */
export type SelectionCriteria = {
  /** Weight for confidence score */
  confidenceWeight: number;
  /** Weight for estimated cost */
  costWeight: number;
  /** Weight for complexity */
  complexityWeight: number;
  /** Hard constraints that must be satisfied */
  hardConstraints: Array<(c: SynthesisCandidate) => boolean>;
};

export const DEFAULT_SELECTION_CRITERIA: SelectionCriteria = {
  confidenceWeight: 0.5,
  costWeight: 0.3,
  complexityWeight: 0.2,
  hardConstraints: [],
};

/**
 * Score a candidate based on criteria.
 */
export function scoreCandidate(
  candidate: SynthesisCandidate,
  criteria: Partial<SelectionCriteria> = {}
): number {
  const fullCriteria = { ...DEFAULT_SELECTION_CRITERIA, ...criteria };

  // Check hard constraints
  for (const constraint of fullCriteria.hardConstraints) {
    if (!constraint(candidate)) {
      return -Infinity;
    }
  }

  // Calculate score
  const confidenceScore = candidate.confidence * fullCriteria.confidenceWeight;

  // Estimate cost (lower is better)
  const costEstimate = estimateCandidateCost(candidate);
  const costScore = (1 - Math.min(costEstimate / 10, 1)) * fullCriteria.costWeight;

  // Estimate complexity (lower is better)
  const complexityEstimate = estimateCandidateComplexity(candidate);
  const complexityScore = (1 - Math.min(complexityEstimate / 10, 1)) * fullCriteria.complexityWeight;

  return confidenceScore + costScore + complexityScore;
}

/**
 * Estimate the cost of a candidate (oracle calls, etc.).
 */
function estimateCandidateCost(candidate: SynthesisCandidate): number {
  // This would analyze the procedure to estimate oracle call count
  // For now, return a simple estimate
  return candidate.kind === "coercion" ? 1 : 2;
}

/**
 * Estimate the complexity of a candidate.
 */
function estimateCandidateComplexity(candidate: SynthesisCandidate): number {
  // This would analyze the procedure structure
  // For now, return a simple estimate
  return 1;
}

/**
 * Select the best candidate from a list.
 */
export function selectBestCandidate(
  candidates: SynthesisCandidate[],
  criteria: Partial<SelectionCriteria> = {}
): SynthesisCandidate | undefined {
  if (candidates.length === 0) return undefined;

  const scored = candidates.map(c => ({
    candidate: c,
    score: scoreCandidate(c, criteria),
  }));

  scored.sort((a, b) => b.score - a.score);

  // Return best if score is valid
  if (scored[0].score > -Infinity) {
    return scored[0].candidate;
  }

  return undefined;
}

// ─────────────────────────────────────────────────────────────────
// Profile Integration
// ─────────────────────────────────────────────────────────────────

/**
 * Check if synthesis is allowed under a profile.
 */
export function canSynthesize(
  profileName: string,
  kind: "method" | "coercion"
): boolean {
  // Profile checking would integrate with governance system
  // For now, basic rules:
  switch (profileName) {
    case "explore":
      return true; // Can synthesize but not commit
    case "pragmatic":
      return true; // Can synthesize and commit with tests
    case "strict":
      return true; // Can synthesize with heavy obligations
    case "airgap":
      return false; // No synthesis allowed
    default:
      return true;
  }
}

/**
 * Check if commit is allowed under a profile.
 */
export function canCommit(
  profileName: string,
  kind: "method" | "coercion"
): boolean {
  switch (profileName) {
    case "explore":
      return false; // Cannot commit
    case "pragmatic":
      return true; // Can commit with tests
    case "strict":
      return true; // Can commit with obligations
    case "airgap":
      return false; // No commits allowed
    default:
      return true;
  }
}

/**
 * Get required obligations for a profile.
 */
export function getRequiredObligations(
  profileName: string,
  kind: "method" | "coercion"
): ObligationRef["kind"][] {
  switch (profileName) {
    case "explore":
      return [];
    case "pragmatic":
      return ["test"];
    case "strict":
      return ["test", "metamorphic", "invariant"];
    case "airgap":
      return [];
    default:
      return ["test"];
  }
}

// ─────────────────────────────────────────────────────────────────
// Reset for Testing
// ─────────────────────────────────────────────────────────────────

/**
 * Reset all synthesis state (for testing).
 */
export function resetSynthesisState(): void {
  clearInFlightSynthesis();
  clearStrategies();
  nextObligationId = 0;
}
