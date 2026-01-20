// src/core/governance/enforcement.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Prompt 9: Governance enforcement at three chokepoints
//
// This module enforces governance rules at three critical points:
// - Chokepoint A: Effect emission (what effects can be emitted)
// - Chokepoint B: Oracle session loop (what oracle requests are legal)
// - Chokepoint C: Commit barrier (truth regime + obligations)

import type { State } from "../eval/machine";
import type { Val } from "../eval/values";
import type { OpCall } from "../effects/opcall";
import {
  Profile,
  RuntimeBudget,
  RuntimeSecurity,
  TruthRegime,
  OracleReqTag,
  ProfileViolation,
  CapabilityViolation,
  BudgetExceeded,
  TruthRegimeViolation,
  makeRuntimeBudget,
  DEFAULT_PROFILE,
  normalizeProfile,
} from "./profile";

// ─────────────────────────────────────────────────────────────────
// State initialization helpers
// ─────────────────────────────────────────────────────────────────

/**
 * Initialize governance state with a profile.
 * Creates RuntimeBudget and RuntimeSecurity from the profile.
 */
export function initGovernance(state: State, profile: Profile): State {
  const normalized = normalizeProfile(profile);
  return {
    ...state,
    profile: normalized,
    budget: makeRuntimeBudget(normalized),
    sec: { caps: new Set(normalized.allowedCaps) },
  };
}

/**
 * Get effective profile from state (or default).
 */
export function getEffectiveProfile(state: State): Profile {
  return normalizeProfile(state.profile ?? DEFAULT_PROFILE);
}

/**
 * Get effective budget from state.
 */
export function getEffectiveBudget(state: State): RuntimeBudget | undefined {
  return state.budget;
}

/**
 * Get effective security context from state.
 */
export function getEffectiveSecurity(state: State): RuntimeSecurity | undefined {
  return state.sec;
}

// ─────────────────────────────────────────────────────────────────
// Chokepoint A: Effect Emission Enforcement
// ─────────────────────────────────────────────────────────────────

/**
 * Built-in kernel operations that are subject to governance.
 * User-defined effects (with custom handlers) are NOT governed here.
 */
const KERNEL_OPS = new Set([
  "infer.op", "int.op", "search.op", "rewrite.op", "oracle.apply.op",
  "tool.op", "commit.op",
  "amb.op", "amb.choose", "amb.fail",
  "observe.op",
  "ctx.snapshot", "ctx.compress", "ctx.hydrate",
]);

/**
 * Check if an op is a built-in kernel operation (subject to governance).
 */
function isKernelOp(op: string): boolean {
  return KERNEL_OPS.has(op);
}

/**
 * Check if an effect operation is allowed by the current profile.
 * Called when an Effect expression is about to emit an operation.
 *
 * NOTE: Only built-in kernel ops are governed. User-defined effects
 * (e.g., custom handlers like "foo") are allowed through - they will
 * either be caught by a handler or fail at runtime dispatch.
 *
 * @throws ProfileViolation if op is not allowed
 * @throws BudgetExceeded if budget is exhausted
 */
export function checkEffectAllowed(state: State, op: string): void {
  // User-defined effects are not governed - they rely on handlers
  if (!isKernelOp(op)) {
    return;
  }

  const profile = getEffectiveProfile(state);

  // Check if kernel operation is allowed
  if (!profile.allowedOps.has(op)) {
    throw new ProfileViolation(op, profile.name, "effect emission");
  }

  // Check budget based on operation type
  const budget = state.budget;
  if (budget) {
    if (op === "infer.op" || op === "infer.batch.op") {
      if (budget.inferCallsLeft <= 0) {
        throw new BudgetExceeded("inferCalls", profile.budgets.maxOracleTurns);
      }
    } else if (op === "tool.op") {
      if (budget.toolCallsLeft <= 0) {
        throw new BudgetExceeded("toolCalls", profile.budgets.maxToolCalls);
      }
    } else if (op === "commit.op") {
      if (budget.commitLeft <= 0) {
        throw new BudgetExceeded("commit", profile.budgets.maxCommits ?? 100);
      }
    }
  }
}

/**
 * Decrement budget after effect is emitted.
 */
export function debitEffect(state: State, op: string): State {
  const budget = state.budget;
  if (!budget) return state;

  const newBudget = { ...budget };

  if (op === "infer.op" || op === "infer.batch.op") {
    newBudget.inferCallsLeft--;
  } else if (op === "tool.op") {
    newBudget.toolCallsLeft--;
  } else if (op === "commit.op") {
    newBudget.commitLeft--;
  }

  // Always decrement steps
  newBudget.stepsLeft--;

  return { ...state, budget: newBudget };
}

// ─────────────────────────────────────────────────────────────────
// Chokepoint B: Oracle Session Loop Enforcement
// ─────────────────────────────────────────────────────────────────

/**
 * Check if an oracle request type is allowed.
 * Called during the oracle session loop when the oracle makes a request.
 *
 * @throws ProfileViolation if request type is not allowed
 * @throws BudgetExceeded if oracle request budget exhausted
 */
export function checkOracleRequestAllowed(state: State, reqTag: OracleReqTag): void {
  const profile = getEffectiveProfile(state);

  // Check if request type is allowed
  if (!profile.allowedOracleReqTags.has(reqTag)) {
    throw new ProfileViolation(reqTag, profile.name, "oracle request");
  }

  // Check oracle request budget
  const budget = state.budget;
  if (budget && budget.oracleReqLeft <= 0) {
    throw new BudgetExceeded(
      "oracleReq",
      profile.budgets.maxOracleReqs ?? profile.budgets.maxOracleTurns
    );
  }

  // Special check: ReqTool requires tool capability
  if (reqTag === "ReqTool") {
    const sec = state.sec;
    if (sec && !sec.caps.has("cap.tool") && !sec.caps.has("*")) {
      throw new CapabilityViolation("ReqTool", "cap.tool", "oracle request");
    }
  }

  // Special check: ReqEval/ReqApply may require eval capability in strict mode
  if (reqTag === "ReqEval" || reqTag === "ReqApply") {
    if (profile.deterministicEnvelope) {
      const sec = state.sec;
      if (sec && !sec.caps.has("cap.eval") && !sec.caps.has("*")) {
        throw new CapabilityViolation(reqTag, "cap.eval", "oracle request in deterministic mode");
      }
    }
  }
}

/**
 * Decrement oracle request budget.
 */
export function debitOracleRequest(state: State): State {
  const budget = state.budget;
  if (!budget) return state;

  return {
    ...state,
    budget: {
      ...budget,
      oracleReqLeft: budget.oracleReqLeft - 1,
    },
  };
}

// ─────────────────────────────────────────────────────────────────
// Chokepoint C: Commit Barrier Enforcement
// ─────────────────────────────────────────────────────────────────

/**
 * Obligation evidence for commit verification.
 * Different truth regimes require different evidence.
 */
export type CommitEvidence =
  | { tag: "None" }
  | { tag: "TestResults"; passed: boolean; coverage?: number; testIds?: string[] }
  | { tag: "ProofCertificate"; proofHash: string; verifier: string }
  | { tag: "ConsensusReceipt"; signatures: string[]; threshold: number };

/**
 * Check if a commit is allowed under the current truth regime.
 * Called when code attempts to commit (promote speculative to persistent).
 *
 * @throws TruthRegimeViolation if commit not allowed
 * @throws ProfileViolation if commit.op not allowed
 * @throws BudgetExceeded if commit budget exhausted
 */
export function checkCommitAllowed(
  state: State,
  evidence: CommitEvidence,
  commitType: "rewrite" | "method" | "data"
): void {
  const profile = getEffectiveProfile(state);

  // First check if commit.op is allowed at all
  if (!profile.allowedOps.has("commit.op")) {
    throw new ProfileViolation("commit.op", profile.name, "commit barrier");
  }

  // Check commit budget
  const budget = state.budget;
  if (budget && budget.commitLeft <= 0) {
    throw new BudgetExceeded("commit", profile.budgets.maxCommits ?? 100);
  }

  // Check capability for this commit type
  const sec = state.sec;
  const requiredCap = `cap.commit.${commitType}`;
  if (sec && !sec.caps.has(requiredCap) && !sec.caps.has("cap.commit.*") && !sec.caps.has("*")) {
    throw new CapabilityViolation("commit", requiredCap, "commit barrier");
  }

  // Check truth regime requirements
  const regime = profile.truthRegime;

  switch (regime) {
    case "speculative":
      // Speculative: no commits allowed (unless explicitly overridden)
      throw new TruthRegimeViolation(regime, "commits not allowed in speculative mode");

    case "test-certified":
      // Test-certified: require passing tests
      if (evidence.tag !== "TestResults") {
        throw new TruthRegimeViolation(regime, "test results required for commit");
      }
      if (!evidence.passed) {
        throw new TruthRegimeViolation(regime, "tests must pass for commit");
      }
      break;

    case "proof-certified":
      // Proof-certified: require proof certificate
      if (evidence.tag !== "ProofCertificate") {
        throw new TruthRegimeViolation(regime, "proof certificate required for commit");
      }
      break;

    case "consensus-certified":
      // Consensus-certified: require consensus receipt
      if (evidence.tag !== "ConsensusReceipt") {
        throw new TruthRegimeViolation(regime, "consensus receipt required for commit");
      }
      if (evidence.signatures.length < evidence.threshold) {
        throw new TruthRegimeViolation(
          regime,
          `insufficient signatures: ${evidence.signatures.length} < ${evidence.threshold}`
        );
      }
      break;
  }
}

/**
 * Decrement commit budget.
 */
export function debitCommit(state: State): State {
  const budget = state.budget;
  if (!budget) return state;

  return {
    ...state,
    budget: {
      ...budget,
      commitLeft: budget.commitLeft - 1,
    },
  };
}

// ─────────────────────────────────────────────────────────────────
// General step budget enforcement
// ─────────────────────────────────────────────────────────────────

/**
 * Check if we have steps remaining.
 * Called on each evaluation step.
 *
 * @throws BudgetExceeded if step budget exhausted
 */
export function checkStepBudget(state: State): void {
  const budget = state.budget;
  const profile = getEffectiveProfile(state);

  if (budget && budget.stepsLeft <= 0) {
    throw new BudgetExceeded("steps", profile.budgets.maxEvalSteps);
  }
}

/**
 * Decrement step budget.
 */
export function debitStep(state: State): State {
  const budget = state.budget;
  if (!budget) return state;

  return {
    ...state,
    budget: {
      ...budget,
      stepsLeft: budget.stepsLeft - 1,
    },
  };
}

// ─────────────────────────────────────────────────────────────────
// Capability enforcement
// ─────────────────────────────────────────────────────────────────

/**
 * Check if a capability is held.
 *
 * @throws CapabilityViolation if capability not held
 */
export function checkCapability(state: State, cap: string, context: string): void {
  const sec = state.sec;
  if (!sec) return; // No security context = no enforcement

  if (sec.caps.has(cap) || sec.caps.has("*")) return;

  // Check domain wildcards (e.g., "cap.tool.*" grants "cap.tool.read")
  const parts = cap.split(".");
  if (parts.length >= 2) {
    const domain = parts.slice(0, -1).join(".");
    if (sec.caps.has(`${domain}.*`)) return;
  }

  throw new CapabilityViolation(context, cap);
}

/**
 * Check if a capability is held (non-throwing version).
 */
export function hasCapability(state: State, cap: string): boolean {
  const sec = state.sec;
  if (!sec) return true; // No security context = granted

  if (sec.caps.has(cap) || sec.caps.has("*")) return true;

  const parts = cap.split(".");
  if (parts.length >= 2) {
    const domain = parts.slice(0, -1).join(".");
    if (sec.caps.has(`${domain}.*`)) return true;
  }

  return false;
}

// ─────────────────────────────────────────────────────────────────
// Profile switching (for with-profile)
// ─────────────────────────────────────────────────────────────────

/**
 * Install a new profile for the dynamic extent.
 * The new profile's caps are intersected with the current caps (can only restrict).
 */
export function withProfile(state: State, newProfile: Profile): State {
  const currentSec = state.sec;

  // Intersect capabilities (new profile can only restrict, not expand)
  let newCaps: Set<string>;
  if (currentSec) {
    newCaps = new Set<string>();
    for (const cap of newProfile.allowedCaps) {
      if (currentSec.caps.has(cap) || currentSec.caps.has("*")) {
        newCaps.add(cap);
      }
    }
  } else {
    newCaps = new Set(newProfile.allowedCaps);
  }

  // Create new budget from profile
  const newBudget = makeRuntimeBudget(newProfile);

  // If there's existing budget, take the minimum
  if (state.budget) {
    newBudget.stepsLeft = Math.min(newBudget.stepsLeft, state.budget.stepsLeft);
    newBudget.inferCallsLeft = Math.min(newBudget.inferCallsLeft, state.budget.inferCallsLeft);
    newBudget.oracleReqLeft = Math.min(newBudget.oracleReqLeft, state.budget.oracleReqLeft);
    newBudget.toolCallsLeft = Math.min(newBudget.toolCallsLeft, state.budget.toolCallsLeft);
    newBudget.commitLeft = Math.min(newBudget.commitLeft, state.budget.commitLeft);
  }

  return {
    ...state,
    profile: newProfile,
    budget: newBudget,
    sec: { caps: newCaps },
  };
}

/**
 * Add capabilities to the current security context.
 * Only works if the profile allows those capabilities.
 */
export function withCaps(state: State, additionalCaps: string[]): State {
  const profile = getEffectiveProfile(state);
  const currentSec = state.sec ?? { caps: new Set<string>() };

  const newCaps = new Set(currentSec.caps);

  for (const cap of additionalCaps) {
    // Can only add caps that the profile allows
    if (profile.allowedCaps.has(cap) || profile.allowedCaps.has("*")) {
      newCaps.add(cap);
    }
  }

  return {
    ...state,
    sec: { caps: newCaps },
  };
}

/**
 * Restrict capabilities (remove from current set).
 */
export function withoutCaps(state: State, capsToRemove: string[]): State {
  const currentSec = state.sec;
  if (!currentSec) return state;

  const newCaps = new Set(currentSec.caps);
  for (const cap of capsToRemove) {
    newCaps.delete(cap);
  }

  return {
    ...state,
    sec: { caps: newCaps },
  };
}

// ─────────────────────────────────────────────────────────────────
// Deterministic envelope enforcement
// ─────────────────────────────────────────────────────────────────

/**
 * Check if running in deterministic envelope.
 */
export function isDeterministic(state: State): boolean {
  const profile = getEffectiveProfile(state);
  return profile.deterministicEnvelope;
}

/**
 * Check if non-deterministic operations are allowed.
 *
 * @throws ProfileViolation if deterministic but non-deterministic op attempted
 */
export function checkNonDeterministicAllowed(state: State, context: string): void {
  if (isDeterministic(state)) {
    const profile = getEffectiveProfile(state);
    throw new ProfileViolation(
      "non-deterministic operation",
      profile.name,
      `${context} - deterministic envelope active`
    );
  }
}
