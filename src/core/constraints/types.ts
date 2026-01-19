// src/core/constraints/types.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Prompt 12: SICP-style constraint propagation networks for semantic pipelines

import type { Val, ConnRefVal, NetRefVal, ExplanationVal, ContradictionVal } from "../eval/values";
import type { Hash } from "../artifacts/hash";

// ─────────────────────────────────────────────────────────────────
// Connector: Store-backed cell in a constraint network
// ─────────────────────────────────────────────────────────────────

/**
 * ConnectorState: The internal state of a connector.
 * Stored in the network's connector map.
 */
export type ConnectorState = {
  /** Connector ID */
  id: string;
  /** Network this connector belongs to */
  netId: string;
  /** Human-readable name */
  name?: string;
  /** Current value (undefined if no value set) */
  value?: Val;
  /** Hash of current value (for quick comparison) */
  valueHash?: Hash;
  /** Explanation of how the value was derived */
  explanation?: ExplanationVal;
  /** IDs of propagators that read from this connector */
  readers: Set<string>;
  /** IDs of propagators that write to this connector */
  writers: Set<string>;
};

// ─────────────────────────────────────────────────────────────────
// Propagator: Procedure that reads inputs and writes outputs
// ─────────────────────────────────────────────────────────────────

/**
 * PropagatorKind: Classification of propagator behavior.
 */
export type PropagatorKind = "extensional" | "oracle" | "constraint" | "validator";

/**
 * PropagatorState: The internal state of a propagator.
 */
export type PropagatorState = {
  /** Propagator ID */
  id: string;
  /** Human-readable name */
  name: string;
  /** Kind of propagator */
  kind: PropagatorKind;
  /** IDs of input connectors */
  inputs: string[];
  /** IDs of output connectors */
  outputs: string[];
  /** The procedure to execute (stored as closure or reference) */
  procRef?: Val;
  /** Is this propagator a hard constraint? */
  isConstraint: boolean;
  /** Priority for scheduling (lower = higher priority) */
  priority: number;
  /** Number of times this propagator has fired */
  fireCount: number;
  /** Whether this propagator requires oracle calls */
  requiresOracle: boolean;
};

// ─────────────────────────────────────────────────────────────────
// Network: Container for connectors, propagators, and constraints
// ─────────────────────────────────────────────────────────────────

/**
 * NetworkStatus: Current state of the network.
 */
export type NetworkStatus =
  | { tag: "quiescent" }                                // No pending work
  | { tag: "pending"; count: number }                   // Propagators waiting to fire
  | { tag: "contradiction"; contradiction: ContradictionVal }  // Constraint violation
  | { tag: "suspended"; op: string; args: Val[] }       // Waiting for oracle
  | { tag: "budgetExhausted"; resource: string };       // Ran out of fuel

/**
 * NetworkState: The internal state of a constraint network.
 */
export type NetworkState = {
  /** Network ID */
  id: string;
  /** Human-readable name */
  name?: string;
  /** Connectors by ID */
  connectors: Map<string, ConnectorState>;
  /** Propagators by ID */
  propagators: Map<string, PropagatorState>;
  /** IDs of propagators that need to fire (scheduling queue) */
  agenda: string[];
  /** Current network status */
  status: NetworkStatus;
  /** Step counter for budgeting */
  stepCount: number;
  /** Contradiction history (for diagnosis) */
  contradictions: ContradictionVal[];
  /** Ledger of propagation events (for replay) */
  ledger: PropagationEvent[];
};

/**
 * PropagationEvent: Record of a single propagation step.
 */
export type PropagationEvent = {
  /** Event type */
  type: "set" | "derive" | "conflict" | "forget" | "fire";
  /** Timestamp */
  timestamp: number;
  /** Connector ID (if applicable) */
  connId?: string;
  /** Propagator ID (if applicable) */
  propId?: string;
  /** Value hash (if applicable) */
  valueHash?: Hash;
  /** Step count at time of event */
  stepCount: number;
};

// ─────────────────────────────────────────────────────────────────
// Propagation budget and configuration
// ─────────────────────────────────────────────────────────────────

/**
 * PropagationBudget: Resource limits for propagation.
 */
export type PropagationBudget = {
  /** Maximum propagator firings */
  maxFirings: number;
  /** Maximum connector sets */
  maxSets: number;
  /** Maximum oracle calls */
  maxOracleCalls: number;
  /** Maximum depth of explanation graphs */
  maxExplanationDepth: number;
};

export const DEFAULT_PROPAGATION_BUDGET: PropagationBudget = {
  maxFirings: 10000,
  maxSets: 5000,
  maxOracleCalls: 100,
  maxExplanationDepth: 50,
};

/**
 * PropagationConfig: Configuration for propagation engine.
 */
export type PropagationConfig = {
  /** Budget limits */
  budget: PropagationBudget;
  /** Strategy for selecting next propagator */
  schedulingStrategy: "fifo" | "priority" | "random";
  /** Whether to record ledger events */
  recordLedger: boolean;
  /** Seed for deterministic scheduling (if random) */
  seed?: number;
};

export const DEFAULT_PROPAGATION_CONFIG: PropagationConfig = {
  budget: DEFAULT_PROPAGATION_BUDGET,
  schedulingStrategy: "fifo",
  recordLedger: true,
};

// ─────────────────────────────────────────────────────────────────
// Repair options for constraint-directed backtracking
// ─────────────────────────────────────────────────────────────────

/**
 * RepairOption: A single repair strategy.
 */
export type RepairOption =
  | { tag: "dropConstraint"; constraintId: string }
  | { tag: "relaxConstraint"; constraintId: string; newSpec: Val }
  | { tag: "swapPropagator"; oldId: string; newProcRef: Val }
  | { tag: "addValidator"; inputs: string[]; validatorProc: Val }
  | { tag: "retryWithValue"; connId: string; newValue: Val };

/**
 * RepairResult: Result of attempting a repair.
 */
export type RepairResult = {
  /** The repair option that was tried */
  option: RepairOption;
  /** Whether the repair succeeded */
  success: boolean;
  /** New network state (if successful) */
  newNetwork?: NetworkState;
  /** New contradiction (if failed) */
  newContradiction?: ContradictionVal;
};

// ─────────────────────────────────────────────────────────────────
// Type guards and helpers
// ─────────────────────────────────────────────────────────────────

export function isConnRef(v: Val): v is ConnRefVal {
  return v.tag === "ConnRef";
}

export function isNetRef(v: Val): v is NetRefVal {
  return v.tag === "NetRef";
}

export function isExplanation(v: Val): v is ExplanationVal {
  return v.tag === "Explanation";
}

export function isContradiction(v: Val): v is ContradictionVal {
  return v.tag === "Contradiction";
}

/**
 * Create a ConnRefVal.
 */
export function makeConnRef(id: string, netId: string, name?: string): ConnRefVal {
  return { tag: "ConnRef", id, netId, name };
}

/**
 * Create a NetRefVal.
 */
export function makeNetRef(id: string, name?: string): NetRefVal {
  return { tag: "NetRef", id, name };
}

/**
 * Create an assumption explanation.
 */
export function makeAssumption(
  conn: ConnRefVal,
  valueHash: Hash,
  because: Val
): ExplanationVal {
  return { tag: "Explanation", kind: "assumption", conn, valueHash, because };
}

/**
 * Create a derived explanation.
 */
export function makeDerived(
  conn: ConnRefVal,
  valueHash: Hash,
  rule: string,
  deps: ExplanationVal[]
): ExplanationVal {
  return { tag: "Explanation", kind: "derived", conn, valueHash, rule, deps };
}

/**
 * Create a conflict explanation.
 */
export function makeConflict(
  conn: ConnRefVal,
  left: ExplanationVal,
  right: ExplanationVal,
  message?: string
): ExplanationVal {
  return { tag: "Explanation", kind: "conflict", conn, left, right, message };
}

/**
 * Create a denied explanation (for capability denial).
 */
export function makeDenied(op: string, reason: string, profile?: string): ExplanationVal {
  return { tag: "Explanation", kind: "denied", op, reason, profile };
}

/**
 * Create a contradiction value.
 */
export function makeContradiction(
  explanation: ExplanationVal,
  constraintId?: string,
  netId?: string
): ContradictionVal {
  return { tag: "Contradiction", explanation, constraintId, netId };
}
