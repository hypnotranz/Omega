// src/core/constraints/diagnosis.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Prompt 12: Diagnosis API for constraint networks (unsat core, explanation extraction)

import type { Val, NetRefVal, ConnRefVal, ContradictionVal, ExplanationVal } from "../eval/values";
import type { Hash } from "../artifacts/hash";
import { sha256JSON } from "../artifacts/hash";
import {
  type NetworkState,
  type PropagatorState,
  type ConnectorState,
} from "./types";
import { getNetwork } from "./network";

// ─────────────────────────────────────────────────────────────────
// Unsat core extraction
// ─────────────────────────────────────────────────────────────────

/**
 * UnsatCore: A minimal set of constraints that are in conflict.
 */
export type UnsatCore = {
  /** IDs of constraints in the core */
  constraintIds: string[];
  /** IDs of connectors involved */
  connectorIds: string[];
  /** Assumptions (initial values) in the core */
  assumptions: Array<{ connId: string; valueHash: Hash }>;
  /** The explanation graph leading to contradiction */
  explanation: ExplanationVal;
};

/**
 * Extract the unsat core from a contradiction.
 * Uses the explanation graph to find the minimal set of constraints.
 */
export function extractUnsatCore(
  netRef: NetRefVal,
  contradiction: ContradictionVal
): UnsatCore {
  const net = getNetwork(netRef.id);
  if (!net) throw new Error(`Network not found: ${netRef.id}`);

  const constraintIds = new Set<string>();
  const connectorIds = new Set<string>();
  const assumptions: Array<{ connId: string; valueHash: Hash }> = [];

  // Traverse explanation graph to collect contributors
  collectFromExplanation(contradiction.explanation, constraintIds, connectorIds, assumptions, net);

  return {
    constraintIds: Array.from(constraintIds),
    connectorIds: Array.from(connectorIds),
    assumptions,
    explanation: contradiction.explanation,
  };
}

/**
 * Recursively collect constraints and connectors from an explanation.
 */
function collectFromExplanation(
  expl: ExplanationVal,
  constraintIds: Set<string>,
  connectorIds: Set<string>,
  assumptions: Array<{ connId: string; valueHash: Hash }>,
  net: NetworkState
): void {
  switch (expl.kind) {
    case "assumption":
      connectorIds.add(expl.conn.id);
      assumptions.push({ connId: expl.conn.id, valueHash: expl.valueHash });
      break;

    case "derived":
      connectorIds.add(expl.conn.id);
      // Find the propagator that produced this
      for (const [propId, prop] of net.propagators) {
        if (prop.name === expl.rule) {
          if (prop.isConstraint) {
            constraintIds.add(propId);
          }
          break;
        }
      }
      // Recurse into dependencies
      for (const dep of expl.deps) {
        collectFromExplanation(dep, constraintIds, connectorIds, assumptions, net);
      }
      break;

    case "conflict":
      connectorIds.add(expl.conn.id);
      collectFromExplanation(expl.left, constraintIds, connectorIds, assumptions, net);
      collectFromExplanation(expl.right, constraintIds, connectorIds, assumptions, net);
      break;

    case "denied":
      // No connectors/constraints for denied operations
      break;
  }
}

// ─────────────────────────────────────────────────────────────────
// Explanation graph traversal
// ─────────────────────────────────────────────────────────────────

/**
 * ExplanationVisitor: Callbacks for traversing an explanation graph.
 */
export type ExplanationVisitor = {
  onAssumption?: (conn: ConnRefVal, valueHash: Hash, because: Val) => void;
  onDerived?: (conn: ConnRefVal, valueHash: Hash, rule: string, deps: ExplanationVal[]) => void;
  onConflict?: (conn: ConnRefVal, left: ExplanationVal, right: ExplanationVal, message?: string) => void;
  onDenied?: (op: string, reason: string, profile?: string) => void;
};

/**
 * Traverse an explanation graph with a visitor.
 */
export function traverseExplanation(
  expl: ExplanationVal,
  visitor: ExplanationVisitor
): void {
  switch (expl.kind) {
    case "assumption":
      visitor.onAssumption?.(expl.conn, expl.valueHash, expl.because);
      break;

    case "derived":
      visitor.onDerived?.(expl.conn, expl.valueHash, expl.rule, expl.deps);
      for (const dep of expl.deps) {
        traverseExplanation(dep, visitor);
      }
      break;

    case "conflict":
      visitor.onConflict?.(expl.conn, expl.left, expl.right, expl.message);
      traverseExplanation(expl.left, visitor);
      traverseExplanation(expl.right, visitor);
      break;

    case "denied":
      visitor.onDenied?.(expl.op, expl.reason, expl.profile);
      break;
  }
}

/**
 * Get all connectors mentioned in an explanation.
 */
export function getExplanationConnectors(expl: ExplanationVal): ConnRefVal[] {
  const connectors: ConnRefVal[] = [];
  traverseExplanation(expl, {
    onAssumption: (conn) => connectors.push(conn),
    onDerived: (conn) => connectors.push(conn),
    onConflict: (conn) => connectors.push(conn),
  });
  return connectors;
}

/**
 * Get all rules (propagator names) mentioned in an explanation.
 */
export function getExplanationRules(expl: ExplanationVal): string[] {
  const rules: string[] = [];
  traverseExplanation(expl, {
    onDerived: (_, __, rule) => rules.push(rule),
  });
  return rules;
}

/**
 * Get the depth of an explanation graph.
 */
export function getExplanationDepth(expl: ExplanationVal): number {
  switch (expl.kind) {
    case "assumption":
    case "denied":
      return 1;

    case "derived": {
      const depDepths = expl.deps.map(getExplanationDepth);
      return 1 + Math.max(0, ...depDepths);
    }

    case "conflict":
      return 1 + Math.max(getExplanationDepth(expl.left), getExplanationDepth(expl.right));
  }
}

/**
 * Serialize an explanation to a string for display.
 */
export function explanationToString(expl: ExplanationVal, indent = 0): string {
  const pad = "  ".repeat(indent);

  switch (expl.kind) {
    case "assumption":
      return `${pad}[assumption] ${expl.conn.name ?? expl.conn.id} = ${expl.valueHash}`;

    case "derived":
      return [
        `${pad}[derived by ${expl.rule}] ${expl.conn.name ?? expl.conn.id} = ${expl.valueHash}`,
        ...expl.deps.map(d => explanationToString(d, indent + 1)),
      ].join("\n");

    case "conflict":
      return [
        `${pad}[CONFLICT] ${expl.message ?? "conflicting values"}`,
        `${pad}  left:`,
        explanationToString(expl.left, indent + 2),
        `${pad}  right:`,
        explanationToString(expl.right, indent + 2),
      ].join("\n");

    case "denied":
      return `${pad}[DENIED] ${expl.op}: ${expl.reason}`;
  }
}

/**
 * Hash an explanation for comparison.
 */
export function hashExplanation(expl: ExplanationVal): Hash {
  return sha256JSON(expl).slice(0, 16);
}

// ─────────────────────────────────────────────────────────────────
// Constraint analysis
// ─────────────────────────────────────────────────────────────────

/**
 * Get all constraints from a network.
 */
export function getConstraints(netRef: NetRefVal): PropagatorState[] {
  const net = getNetwork(netRef.id);
  if (!net) return [];
  return Array.from(net.propagators.values()).filter(p => p.isConstraint);
}

/**
 * Get all propagators that contributed to a connector's value.
 */
export function getContributingPropagators(
  netRef: NetRefVal,
  connRef: ConnRefVal
): PropagatorState[] {
  const net = getNetwork(netRef.id);
  if (!net) return [];
  const conn = net.connectors.get(connRef.id);
  if (!conn) return [];

  // Find all propagators that write to this connector
  const writers = Array.from(conn.writers);
  return writers
    .map(id => net.propagators.get(id))
    .filter((p): p is PropagatorState => p !== undefined);
}

/**
 * Find propagators that could potentially resolve a contradiction.
 * These are propagators that write to connectors in the unsat core
 * but are not themselves in the core.
 */
export function findPotentialResolvers(
  netRef: NetRefVal,
  core: UnsatCore
): PropagatorState[] {
  const net = getNetwork(netRef.id);
  if (!net) return [];

  const coreConstraints = new Set(core.constraintIds);
  const coreConnectors = new Set(core.connectorIds);
  const resolvers: PropagatorState[] = [];

  for (const [propId, prop] of net.propagators) {
    // Skip if in core
    if (coreConstraints.has(propId)) continue;

    // Check if outputs include core connectors
    const writesToCore = prop.outputs.some(connId => coreConnectors.has(connId));
    if (writesToCore) {
      resolvers.push(prop);
    }
  }

  return resolvers;
}
