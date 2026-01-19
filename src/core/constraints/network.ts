// src/core/constraints/network.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Prompt 12: Constraint network implementation

import type { Val, ConnRefVal, NetRefVal, ExplanationVal, ContradictionVal } from "../eval/values";
import { VUnit } from "../eval/values";
import { sha256JSON } from "../artifacts/hash";
import type { Hash } from "../artifacts/hash";
import {
  type ConnectorState,
  type PropagatorState,
  type NetworkState,
  type NetworkStatus,
  type PropagationEvent,
  type PropagationConfig,
  type PropagatorKind,
  DEFAULT_PROPAGATION_CONFIG,
  makeConnRef,
  makeNetRef,
  makeAssumption,
  makeDerived,
  makeConflict,
  makeContradiction,
} from "./types";

// ─────────────────────────────────────────────────────────────────
// ID generation
// ─────────────────────────────────────────────────────────────────

let networkCounter = 0;
let connectorCounter = 0;
let propagatorCounter = 0;

function makeNetworkId(): string {
  return `net_${++networkCounter}`;
}

function makeConnectorId(): string {
  return `conn_${++connectorCounter}`;
}

function makePropagatorId(): string {
  return `prop_${++propagatorCounter}`;
}

/** Reset counters for testing. */
export function resetConstraintCounters(): void {
  networkCounter = 0;
  connectorCounter = 0;
  propagatorCounter = 0;
}

// ─────────────────────────────────────────────────────────────────
// Network registry (global store for networks)
// ─────────────────────────────────────────────────────────────────

const networkRegistry = new Map<string, NetworkState>();

/**
 * Get a network by ID.
 */
export function getNetwork(id: string): NetworkState | undefined {
  return networkRegistry.get(id);
}

/**
 * Set a network in the registry.
 */
export function setNetwork(net: NetworkState): void {
  networkRegistry.set(net.id, net);
}

/**
 * Delete a network from the registry.
 */
export function deleteNetwork(id: string): boolean {
  return networkRegistry.delete(id);
}

/**
 * Clear all networks (for testing).
 */
export function clearNetworks(): void {
  networkRegistry.clear();
}

// ─────────────────────────────────────────────────────────────────
// Network creation and management
// ─────────────────────────────────────────────────────────────────

/**
 * Create a new constraint network.
 */
export function createNetwork(name?: string): NetRefVal {
  const id = makeNetworkId();
  const state: NetworkState = {
    id,
    name,
    connectors: new Map(),
    propagators: new Map(),
    agenda: [],
    status: { tag: "quiescent" },
    stepCount: 0,
    contradictions: [],
    ledger: [],
  };
  setNetwork(state);
  return makeNetRef(id, name);
}

/**
 * Get network status.
 */
export function getNetworkStatus(netRef: NetRefVal): NetworkStatus {
  const net = getNetwork(netRef.id);
  if (!net) throw new Error(`Network not found: ${netRef.id}`);
  return net.status;
}

/**
 * Clone a network state for forking.
 */
export function cloneNetwork(net: NetworkState): NetworkState {
  const newId = makeNetworkId();
  const clone: NetworkState = {
    id: newId,
    name: net.name ? `${net.name}_clone` : undefined,
    connectors: new Map(
      Array.from(net.connectors.entries()).map(([k, v]) => [
        k,
        {
          ...v,
          readers: new Set(v.readers),
          writers: new Set(v.writers),
        },
      ])
    ),
    propagators: new Map(
      Array.from(net.propagators.entries()).map(([k, v]) => [k, { ...v }])
    ),
    agenda: [...net.agenda],
    status: { ...net.status },
    stepCount: net.stepCount,
    contradictions: [...net.contradictions],
    ledger: [...net.ledger],
  };
  setNetwork(clone);
  return clone;
}

// ─────────────────────────────────────────────────────────────────
// Connector operations
// ─────────────────────────────────────────────────────────────────

/**
 * Create a new connector in a network.
 */
export function createConnector(netRef: NetRefVal, name?: string): ConnRefVal {
  const net = getNetwork(netRef.id);
  if (!net) throw new Error(`Network not found: ${netRef.id}`);

  const id = makeConnectorId();
  const state: ConnectorState = {
    id,
    netId: netRef.id,
    name,
    readers: new Set(),
    writers: new Set(),
  };
  net.connectors.set(id, state);
  return makeConnRef(id, netRef.id, name);
}

/**
 * Check if a connector has a value.
 */
export function connectorHasValue(connRef: ConnRefVal): boolean {
  const net = getNetwork(connRef.netId);
  if (!net) throw new Error(`Network not found: ${connRef.netId}`);
  const conn = net.connectors.get(connRef.id);
  if (!conn) throw new Error(`Connector not found: ${connRef.id}`);
  return conn.value !== undefined;
}

/**
 * Get the value of a connector.
 */
export function connectorGetValue(connRef: ConnRefVal): Val {
  const net = getNetwork(connRef.netId);
  if (!net) throw new Error(`Network not found: ${connRef.netId}`);
  const conn = net.connectors.get(connRef.id);
  if (!conn) throw new Error(`Connector not found: ${connRef.id}`);
  if (conn.value === undefined) return VUnit;
  return conn.value;
}

/**
 * Get the explanation for a connector's value.
 */
export function connectorGetExplanation(connRef: ConnRefVal): ExplanationVal | undefined {
  const net = getNetwork(connRef.netId);
  if (!net) throw new Error(`Network not found: ${connRef.netId}`);
  const conn = net.connectors.get(connRef.id);
  if (!conn) throw new Error(`Connector not found: ${connRef.id}`);
  return conn.explanation;
}

/**
 * Compute hash of a value for explanation tracking.
 */
function hashValue(v: Val): Hash {
  try {
    return sha256JSON(v).slice(0, 16);
  } catch {
    return `${v.tag}_hash`;
  }
}

/**
 * Check if two values are equal (deep comparison).
 */
function valuesEqual(a: Val, b: Val): boolean {
  // Simple comparison - could be improved
  try {
    return JSON.stringify(a) === JSON.stringify(b);
  } catch {
    return false;
  }
}

/**
 * Set the value of a connector with provenance.
 * Returns contradiction if value conflicts with existing value.
 */
export function connectorSetValue(
  connRef: ConnRefVal,
  value: Val,
  because: Val,
  rule?: string,
  deps?: ExplanationVal[]
): ContradictionVal | undefined {
  const net = getNetwork(connRef.netId);
  if (!net) throw new Error(`Network not found: ${connRef.netId}`);
  const conn = net.connectors.get(connRef.id);
  if (!conn) throw new Error(`Connector not found: ${connRef.id}`);

  const newHash = hashValue(value);

  // Build explanation
  const explanation: ExplanationVal = deps && deps.length > 0
    ? makeDerived(connRef, newHash, rule ?? "propagator", deps)
    : makeAssumption(connRef, newHash, because);

  // Check for conflict
  if (conn.value !== undefined) {
    if (!valuesEqual(conn.value, value)) {
      // Contradiction: different values for same connector
      const conflict = makeConflict(
        connRef,
        conn.explanation!,
        explanation,
        `Connector ${connRef.name ?? connRef.id} has conflicting values`
      );
      const contradiction = makeContradiction(conflict, undefined, connRef.netId);
      net.contradictions.push(contradiction);
      net.status = { tag: "contradiction", contradiction };
      return contradiction;
    }
    // Same value, no change needed
    return undefined;
  }

  // Set the value
  conn.value = value;
  conn.valueHash = newHash;
  conn.explanation = explanation;

  // Record event
  net.ledger.push({
    type: "set",
    timestamp: Date.now(),
    connId: connRef.id,
    valueHash: newHash,
    stepCount: net.stepCount,
  });

  // Schedule dependent propagators
  for (const propId of conn.readers) {
    if (!net.agenda.includes(propId)) {
      net.agenda.push(propId);
    }
  }

  // Update status
  if (net.agenda.length > 0 && net.status.tag === "quiescent") {
    net.status = { tag: "pending", count: net.agenda.length };
  }

  return undefined;
}

/**
 * Forget (clear) the value of a connector.
 */
export function connectorForgetValue(connRef: ConnRefVal, because: Val): void {
  const net = getNetwork(connRef.netId);
  if (!net) throw new Error(`Network not found: ${connRef.netId}`);
  const conn = net.connectors.get(connRef.id);
  if (!conn) throw new Error(`Connector not found: ${connRef.id}`);

  conn.value = undefined;
  conn.valueHash = undefined;
  conn.explanation = undefined;

  // Record event
  net.ledger.push({
    type: "forget",
    timestamp: Date.now(),
    connId: connRef.id,
    stepCount: net.stepCount,
  });
}

// ─────────────────────────────────────────────────────────────────
// Propagator operations
// ─────────────────────────────────────────────────────────────────

/**
 * Register a propagator in a network.
 */
export function registerPropagator(
  netRef: NetRefVal,
  name: string,
  inputs: ConnRefVal[],
  outputs: ConnRefVal[],
  proc: Val,
  options: {
    kind?: PropagatorKind;
    isConstraint?: boolean;
    priority?: number;
    requiresOracle?: boolean;
  } = {}
): string {
  const net = getNetwork(netRef.id);
  if (!net) throw new Error(`Network not found: ${netRef.id}`);

  const id = makePropagatorId();
  const state: PropagatorState = {
    id,
    name,
    kind: options.kind ?? "extensional",
    inputs: inputs.map(c => c.id),
    outputs: outputs.map(c => c.id),
    procRef: proc,
    isConstraint: options.isConstraint ?? false,
    priority: options.priority ?? 0,
    fireCount: 0,
    requiresOracle: options.requiresOracle ?? false,
  };

  net.propagators.set(id, state);

  // Register as reader/writer on connectors
  for (const connRef of inputs) {
    const conn = net.connectors.get(connRef.id);
    if (conn) conn.readers.add(id);
  }
  for (const connRef of outputs) {
    const conn = net.connectors.get(connRef.id);
    if (conn) conn.writers.add(id);
  }

  // Schedule if all inputs have values
  const allInputsReady = inputs.every(c => connectorHasValue(c));
  if (allInputsReady && !net.agenda.includes(id)) {
    net.agenda.push(id);
    if (net.status.tag === "quiescent") {
      net.status = { tag: "pending", count: net.agenda.length };
    }
  }

  return id;
}

/**
 * Register a constraint (propagator + invariant).
 */
export function registerConstraint(
  netRef: NetRefVal,
  name: string,
  inputs: ConnRefVal[],
  proc: Val,
  options: {
    priority?: number;
    requiresOracle?: boolean;
  } = {}
): string {
  return registerPropagator(netRef, name, inputs, [], proc, {
    kind: "constraint",
    isConstraint: true,
    priority: options.priority ?? -10, // Constraints have high priority
    requiresOracle: options.requiresOracle,
  });
}

/**
 * Get propagator state by ID.
 */
export function getPropagator(netRef: NetRefVal, propId: string): PropagatorState | undefined {
  const net = getNetwork(netRef.id);
  if (!net) return undefined;
  return net.propagators.get(propId);
}

/**
 * Remove a propagator from a network.
 */
export function removePropagator(netRef: NetRefVal, propId: string): boolean {
  const net = getNetwork(netRef.id);
  if (!net) return false;

  const prop = net.propagators.get(propId);
  if (!prop) return false;

  // Remove from connector readers/writers
  for (const connId of prop.inputs) {
    const conn = net.connectors.get(connId);
    if (conn) conn.readers.delete(propId);
  }
  for (const connId of prop.outputs) {
    const conn = net.connectors.get(connId);
    if (conn) conn.writers.delete(propId);
  }

  // Remove from agenda
  const agendaIdx = net.agenda.indexOf(propId);
  if (agendaIdx >= 0) {
    net.agenda.splice(agendaIdx, 1);
  }

  net.propagators.delete(propId);
  return true;
}

// ─────────────────────────────────────────────────────────────────
// Network snapshot/restore for COW semantics
// ─────────────────────────────────────────────────────────────────

/**
 * Create a snapshot of network state.
 */
export function snapshotNetwork(netRef: NetRefVal): NetworkState {
  const net = getNetwork(netRef.id);
  if (!net) throw new Error(`Network not found: ${netRef.id}`);
  return cloneNetwork(net);
}

/**
 * Restore network from snapshot.
 */
export function restoreNetwork(snapshot: NetworkState): NetRefVal {
  setNetwork(snapshot);
  return makeNetRef(snapshot.id, snapshot.name);
}

/**
 * Get network digest for content-addressing.
 */
export function getNetworkDigest(netRef: NetRefVal): Hash {
  const net = getNetwork(netRef.id);
  if (!net) throw new Error(`Network not found: ${netRef.id}`);

  // Create a serializable representation
  const connectors = Array.from(net.connectors.entries()).map(([id, c]) => ({
    id,
    name: c.name,
    valueHash: c.valueHash,
  }));
  const propagators = Array.from(net.propagators.entries()).map(([id, p]) => ({
    id,
    name: p.name,
    kind: p.kind,
    inputs: p.inputs,
    outputs: p.outputs,
  }));

  return sha256JSON({ connectors, propagators, status: net.status.tag });
}
