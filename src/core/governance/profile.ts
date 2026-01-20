// src/core/governance/profile.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Patch Set D: Execution profiles with truth regimes
// Prompt 9: Full governance enforcement

import type { CapSet } from "./caps";
import type { BudgetLimits } from "./budgets";

// ─────────────────────────────────────────────────────────────────
// Truth Regimes: promotion barriers for commit operations
// ─────────────────────────────────────────────────────────────────

export type TruthRegime =
  | "speculative"         // No commits allowed (or explicit override required)
  | "test-certified"      // Requires OblTests / OblEqExt
  | "proof-certified"     // Requires proof evidence (stub for now)
  | "consensus-certified"; // Requires consensus evidence (stub for now)

// ─────────────────────────────────────────────────────────────────
// Oracle Request Tags: what the oracle can ask for
// ─────────────────────────────────────────────────────────────────

export type OracleReqTag =
  | "ReqEval"      // Evaluate expression in current env
  | "ReqApply"     // Apply procedure to arguments
  | "ReqObserve"   // Observe machine state
  | "ReqTool"      // Call external tool
  | "ReqTest"      // Run tests
  | "ReqCommit";   // Request commit (checked separately)

// ─────────────────────────────────────────────────────────────────
// Effect Operations: which effects can be emitted
// ─────────────────────────────────────────────────────────────────

export type EffectOp =
  | "infer.op"     // Oracle inference
  | "tool.op"      // Tool invocation
  | "commit.op"    // Commit to ledger
  | "amb.choose"   // Nondeterministic choice
  | "observe.op";  // Observe state

// ─────────────────────────────────────────────────────────────────
// Budget Profile: runtime resource limits
// ─────────────────────────────────────────────────────────────────

export type BudgetProfile = BudgetLimits & {
  maxOracleReqs?: number;   // Max oracle requests per session
  maxCommits?: number;      // Max commits per session
};

// ─────────────────────────────────────────────────────────────────
// Profile: First-class runtime governance object
// ─────────────────────────────────────────────────────────────────

/**
 * Profile: A complete governance configuration for execution.
 *
 * This is the "evaluator variant" that determines what the language can do.
 * Different profiles = different languages (SICP-style).
 *
 * Enforced at three chokepoints:
 * - Effect emission (what effects can be emitted)
 * - Oracle session loop (what the oracle can request)
 * - Commit barrier (truth regime + obligations)
 */
export type Profile = {
  /** Profile identifier */
  name: string;

  /** Capability envelope - what authorities this profile grants */
  allowedCaps?: Set<string>;

  /** Which effect ops are allowed to be handled */
  allowedOps?: Set<string>;

  /** Which Oracle requests are legal during inference */
  allowedOracleReqTags?: Set<OracleReqTag>;

  /** Budget envelope - hard stops */
  budgets: BudgetProfile;

  /** Truth regime for promotion barriers */
  truthRegime?: TruthRegime;

  /** If true, only scripted/replay oracles allowed */
  deterministicEnvelope?: boolean;

  // Legacy compatibility
  caps?: CapSet;
  truth?: TruthRegime;
  // Demo flags (legacy wow pack)
  allowCommit?: boolean;
  allowPromotion?: boolean;
  requireTests?: boolean;
  inferBudget?: number;
};

/** Fully-populated profile after normalization */
export type NormalizedProfile = Profile & {
  allowedCaps: Set<string>;
  allowedOps: Set<string>;
  allowedOracleReqTags: Set<OracleReqTag>;
  truthRegime: TruthRegime;
  deterministicEnvelope: boolean;
};

const DEFAULT_ALLOWED_OPS = new Set<string>([
  "infer.op", "int.op", "search.op", "rewrite.op", "oracle.apply.op",
  "amb.op", "amb.choose", "amb.fail", "observe.op", "tool.op", "commit.op",
]);

const DEFAULT_ORACLE_REQS: OracleReqTag[] = [
  "ReqEval",
  "ReqApply",
  "ReqObserve",
  "ReqTool",
  "ReqTest",
  "ReqCommit",
];

function toSet(value?: Set<string> | string[]): Set<string> {
  if (!value) return new Set();
  return value instanceof Set ? new Set(value) : new Set(value);
}

/**
 * Normalize a profile into the fully-populated runtime shape.
 * Accepts legacy profiles that only specify caps/budgets/truth.
 */
export function normalizeProfile(profile: Profile): NormalizedProfile {
  if (
    profile.allowedCaps &&
    profile.allowedOps &&
    profile.allowedOracleReqTags &&
    profile.truthRegime !== undefined &&
    profile.deterministicEnvelope !== undefined
  ) {
    return profile as NormalizedProfile;
  }

  const allowedCaps = toSet(profile.allowedCaps ?? (profile.caps as string[] | undefined));
  const allowedOps = profile.allowedOps ? new Set(profile.allowedOps) : new Set(DEFAULT_ALLOWED_OPS);
  const allowedOracleReqTags = profile.allowedOracleReqTags
    ? new Set(profile.allowedOracleReqTags)
    : new Set(DEFAULT_ORACLE_REQS);
  const truthRegime = profile.truthRegime ?? profile.truth ?? "speculative";
  const deterministicEnvelope = profile.deterministicEnvelope ?? (profile as { deterministic?: boolean }).deterministic ?? false;

  const budgets: BudgetProfile = {
    maxOracleTurns: profile.budgets.maxOracleTurns,
    maxEvalSteps: profile.budgets.maxEvalSteps,
    maxToolCalls: profile.budgets.maxToolCalls,
    maxNestedDepth: profile.budgets.maxNestedDepth ?? 8,
    maxOracleReqs: profile.budgets.maxOracleReqs ?? profile.budgets.maxOracleTurns,
    maxCommits: profile.budgets.maxCommits ?? (profile.allowCommit ? 100 : 0),
  };

  return {
    ...profile,
    allowedCaps,
    allowedOps,
    allowedOracleReqTags,
    truthRegime,
    deterministicEnvelope,
    caps: profile.caps ?? Array.from(allowedCaps),
    truth: truthRegime,
    budgets,
  };
}

// ─────────────────────────────────────────────────────────────────
// Runtime Budget: mutable state for tracking consumption
// ─────────────────────────────────────────────────────────────────

export type RuntimeBudget = {
  stepsLeft: number;
  inferCallsLeft: number;
  oracleReqLeft: number;
  toolCallsLeft: number;
  commitLeft: number;
};

// ─────────────────────────────────────────────────────────────────
// Runtime Security: current authority set
// ─────────────────────────────────────────────────────────────────

export type RuntimeSecurity = {
  /** Current authority set (intersection of context caps + profile allowedCaps) */
  caps: Set<string>;
};

// ─────────────────────────────────────────────────────────────────
// Governance Violation Errors
// ─────────────────────────────────────────────────────────────────

export class ProfileViolation extends Error {
  constructor(
    public readonly op: string,
    public readonly profile: string,
    public readonly context?: string
  ) {
    super(`ProfileViolation: operation '${op}' not allowed in profile '${profile}'${context ? ` (${context})` : ""}`);
    this.name = "ProfileViolation";
  }
}

export class CapabilityViolation extends Error {
  constructor(
    public readonly op: string,
    public readonly requiredCap: string,
    public readonly context?: string
  ) {
    super(`CapabilityViolation: operation '${op}' requires capability '${requiredCap}'${context ? ` (${context})` : ""}`);
    this.name = "CapabilityViolation";
  }
}

export class BudgetExceeded extends Error {
  constructor(
    public readonly kind: "steps" | "inferCalls" | "oracleReq" | "toolCalls" | "commit",
    public readonly limit: number
  ) {
    super(`BudgetExceeded: ${kind} limit (${limit}) exceeded`);
    this.name = "BudgetExceeded";
  }
}

export class TruthRegimeViolation extends Error {
  constructor(
    public readonly regime: TruthRegime,
    public readonly reason: string
  ) {
    super(`TruthRegimeViolation: ${reason} (regime: ${regime})`);
    this.name = "TruthRegimeViolation";
  }
}

// ─────────────────────────────────────────────────────────────────
// Helper: Create Profile from simplified config
// ─────────────────────────────────────────────────────────────────

export function makeProfile(config: {
  name: string;
  caps: string[];
  ops: string[];
  oracleReqs: OracleReqTag[];
  budgets: BudgetProfile;
  truth: TruthRegime;
  deterministic?: boolean;
}): NormalizedProfile {
  // Generate legacy caps that include both new-style (cap.eval) and old-style (eval) names
  // for backward compatibility with existing capRequire calls
  const legacyCaps = [...config.caps];
  for (const cap of config.caps) {
    // If cap starts with "cap.", also add the version without prefix
    if (cap.startsWith("cap.")) {
      const oldStyle = cap.slice(4); // Remove "cap." prefix
      if (!legacyCaps.includes(oldStyle)) {
        legacyCaps.push(oldStyle);
      }
    }
  }

  return normalizeProfile({
    name: config.name,
    allowedCaps: new Set(config.caps),
    allowedOps: new Set(config.ops),
    allowedOracleReqTags: new Set(config.oracleReqs),
    budgets: config.budgets,
    truthRegime: config.truth,
    deterministicEnvelope: config.deterministic ?? false,
    // Legacy - includes both cap.X and X style names for compatibility
    caps: legacyCaps,
    truth: config.truth,
  });
}

// ─────────────────────────────────────────────────────────────────
// Helper: Create RuntimeBudget from Profile
// ─────────────────────────────────────────────────────────────────

export function makeRuntimeBudget(profile: Profile): RuntimeBudget {
  const normalized = normalizeProfile(profile);
  return {
    stepsLeft: normalized.budgets.maxEvalSteps,
    inferCallsLeft: normalized.budgets.maxOracleTurns,
    oracleReqLeft: normalized.budgets.maxOracleReqs ?? normalized.budgets.maxOracleTurns,
    toolCallsLeft: normalized.budgets.maxToolCalls,
    commitLeft: normalized.budgets.maxCommits ?? 100,
  };
}

// ─────────────────────────────────────────────────────────────────
// Canonical Profiles (Prompt 9)
// ─────────────────────────────────────────────────────────────────

/**
 * profile:explore - Rapid exploration without commits
 *
 * - Allowed: infer.op, amb.*, observe
 * - Oracle REPL: ReqEval, ReqApply, ReqObserve
 * - NO commits by default
 * - Generous budgets, but still bounded
 *
 * Use: rapid exploration where you can't accidentally promote
 */
export const PROFILE_EXPLORE: NormalizedProfile = makeProfile({
  name: "explore",
  caps: ["cap.eval", "cap.apply", "cap.observe", "cap.infer"],
  ops: ["infer.op", "int.op", "search.op", "rewrite.op", "oracle.apply.op", "amb.op", "amb.choose", "amb.fail", "observe.op"],
  oracleReqs: ["ReqEval", "ReqApply", "ReqObserve"],
  budgets: {
    maxOracleTurns: 1000,
    maxEvalSteps: 100_000,
    maxToolCalls: 0,        // No tools in explore
    maxNestedDepth: 8,
    maxOracleReqs: 500,
    maxCommits: 0,          // No commits in explore
  },
  truth: "speculative",
  deterministic: false,
});

/**
 * profile:pragmatic - Real engineering with CI-like discipline
 *
 * - Allow: infer + test + commit
 * - Oracle REPL: full, but tools only if explicitly granted
 * - Truth: test-certified
 * - Required obligations on commit
 *
 * Use: real engineering tasks with CI-like discipline
 */
export const PROFILE_PRAGMATIC: NormalizedProfile = makeProfile({
  name: "pragmatic",
  caps: [
    "cap.eval", "cap.apply", "cap.observe", "cap.infer",
    "cap.test", "cap.commit.rewrite", "cap.commit.method",
    "cap.generic.autofill"
  ],
  ops: ["infer.op", "int.op", "search.op", "rewrite.op", "oracle.apply.op", "amb.op", "amb.choose", "amb.fail", "observe.op", "commit.op"],
  oracleReqs: ["ReqEval", "ReqApply", "ReqObserve", "ReqTest", "ReqCommit"],
  budgets: {
    maxOracleTurns: 10_000,
    maxEvalSteps: 500_000,
    maxToolCalls: 100,
    maxNestedDepth: 16,
    maxOracleReqs: 5000,
    maxCommits: 50,
  },
  truth: "test-certified",
  deterministic: false,
});

/**
 * profile:strict - Production semantics
 *
 * - Allow infer only under deterministic envelope (scripted or replay)
 * - Allow ReqTest, ReqEval/Apply only if cap.eval explicitly present
 * - Truth: test-certified with heavier obligations
 * - Forbid ReqTool by default
 * - Require receipts at all promotion boundaries
 *
 * Use: production semantics
 */
export const PROFILE_STRICT: NormalizedProfile = makeProfile({
  name: "strict",
  caps: ["cap.observe", "cap.test", "cap.commit.rewrite", "cap.commit.method"],
  ops: ["infer.op", "int.op", "search.op", "rewrite.op", "oracle.apply.op", "commit.op"],  // No amb, limited ops
  oracleReqs: ["ReqObserve", "ReqTest"],  // No ReqEval/ReqApply by default
  budgets: {
    maxOracleTurns: 100,
    maxEvalSteps: 50_000,
    maxToolCalls: 10,
    maxNestedDepth: 4,
    maxOracleReqs: 50,
    maxCommits: 10,
  },
  truth: "test-certified",
  deterministic: true,  // Only scripted/replay
});

/**
 * profile:airgap - Private/offline, reproducible semantics
 *
 * - Allow infer.op but FORBID ReqTool
 * - Optionally forbid ReqEval (depending on security stance)
 * - Allow ReqObserve only on redacted projections
 * - Truth: speculative unless tests are hermetic
 * - Tight budgets
 *
 * Use: private/offline, reproducible semantics
 */
export const PROFILE_AIRGAP: NormalizedProfile = makeProfile({
  name: "airgap",
  caps: ["cap.observe", "cap.infer"],
  ops: ["infer.op", "int.op", "search.op", "rewrite.op", "oracle.apply.op", "observe.op"],  // No tool.op, no commit.op
  oracleReqs: ["ReqObserve"],       // NO ReqTool, NO ReqEval/ReqApply
  budgets: {
    maxOracleTurns: 50,
    maxEvalSteps: 10_000,
    maxToolCalls: 0,               // Airgap: no tools
    maxNestedDepth: 4,
    maxOracleReqs: 25,
    maxCommits: 0,                 // Airgap: no commits
  },
  truth: "speculative",
  deterministic: true,
});

// Legacy profiles (for backward compatibility)
export const PROFILE_SPECULATIVE: NormalizedProfile = PROFILE_EXPLORE;
export const PROFILE_TEST_CERTIFIED: NormalizedProfile = PROFILE_PRAGMATIC;
export const PROFILE_PROOF_CERTIFIED: NormalizedProfile = makeProfile({
  name: "proof-certified",
  caps: ["*"],
  ops: ["infer.op", "int.op", "search.op", "rewrite.op", "oracle.apply.op", "tool.op", "commit.op", "amb.op", "amb.choose", "amb.fail", "observe.op"],
  oracleReqs: ["ReqEval", "ReqApply", "ReqObserve", "ReqTool", "ReqTest", "ReqCommit"],
  budgets: {
    maxOracleTurns: 100_000,
    maxEvalSteps: 5_000_000,
    maxToolCalls: 10_000,
    maxNestedDepth: 32,
    maxOracleReqs: 50_000,
    maxCommits: 1000,
  },
  truth: "proof-certified",
  deterministic: false,
});

export const DEFAULT_PROFILE = PROFILE_PRAGMATIC;

// ─────────────────────────────────────────────────────────────────
// Profile lookup by name
// ─────────────────────────────────────────────────────────────────

const PROFILE_REGISTRY: Map<string, NormalizedProfile> = new Map([
  ["explore", PROFILE_EXPLORE],
  ["pragmatic", PROFILE_PRAGMATIC],
  ["strict", PROFILE_STRICT],
  ["airgap", PROFILE_AIRGAP],
  ["speculative", PROFILE_SPECULATIVE],
  ["test-certified", PROFILE_TEST_CERTIFIED],
  ["proof-certified", PROFILE_PROOF_CERTIFIED],
]);

export function getProfile(name: string): NormalizedProfile | undefined {
  const p = PROFILE_REGISTRY.get(name);
  return p ? normalizeProfile(p) : undefined;
}

export function getProfileOrDefault(name: string): NormalizedProfile {
  return normalizeProfile(PROFILE_REGISTRY.get(name) ?? DEFAULT_PROFILE);
}
