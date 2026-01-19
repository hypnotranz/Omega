// src/core/constraints/engine.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Prompt 12: Propagation engine for constraint networks

import type { Val, NetRefVal, ConnRefVal, ContradictionVal, ExplanationVal } from "../eval/values";
import { VUnit, VTrue, VFalse } from "../eval/values";
import type { Runtime } from "../eval/runtime";
import type { State } from "../eval/machine";
import {
  type NetworkState,
  type NetworkStatus,
  type PropagatorState,
  type PropagationConfig,
  type PropagationBudget,
  DEFAULT_PROPAGATION_CONFIG,
  makeContradiction,
  makeDenied,
} from "./types";
import {
  getNetwork,
  setNetwork,
  connectorHasValue,
  connectorGetValue,
  connectorGetExplanation,
  connectorSetValue,
  cloneNetwork,
} from "./network";

// ─────────────────────────────────────────────────────────────────
// Propagation result types
// ─────────────────────────────────────────────────────────────────

/**
 * PropagationResult: Result of running propagation.
 */
export type PropagationResult = {
  /** Final network status */
  status: NetworkStatus;
  /** Number of propagator firings */
  firings: number;
  /** Number of connector sets */
  sets: number;
  /** Whether budget was exhausted */
  budgetExhausted: boolean;
  /** Contradiction (if any) */
  contradiction?: ContradictionVal;
};

// ─────────────────────────────────────────────────────────────────
// Propagator execution context
// ─────────────────────────────────────────────────────────────────

/**
 * PropagatorContext: Context for executing a propagator.
 */
export type PropagatorContext = {
  /** Input values (keyed by connector ID) */
  inputs: Map<string, Val>;
  /** Input explanations (keyed by connector ID) */
  inputExplanations: Map<string, ExplanationVal>;
  /** Propagator name (for explanation building) */
  propagatorName: string;
  /** Network reference */
  netRef: NetRefVal;
};

/**
 * PropagatorOutput: Output from a propagator execution.
 */
export type PropagatorOutput =
  | { tag: "values"; values: Map<string, Val> }
  | { tag: "contradiction"; message: string }
  | { tag: "noChange" }
  | { tag: "suspend"; op: string; args: Val[] };

// ─────────────────────────────────────────────────────────────────
// Scripted propagator registry (for testing)
// ─────────────────────────────────────────────────────────────────

/**
 * ScriptedPropagator: A propagator with scripted behavior for testing.
 */
export type ScriptedPropagator = {
  name: string;
  execute: (ctx: PropagatorContext) => PropagatorOutput;
};

const scriptedPropagators = new Map<string, ScriptedPropagator>();

/**
 * Register a scripted propagator.
 */
export function registerScriptedPropagator(prop: ScriptedPropagator): void {
  scriptedPropagators.set(prop.name, prop);
}

/**
 * Clear all scripted propagators.
 */
export function clearScriptedPropagators(): void {
  scriptedPropagators.clear();
}

/**
 * Get a scripted propagator by name.
 */
export function getScriptedPropagator(name: string): ScriptedPropagator | undefined {
  return scriptedPropagators.get(name);
}

// ─────────────────────────────────────────────────────────────────
// Propagation engine
// ─────────────────────────────────────────────────────────────────

/**
 * Run propagation on a network until quiescence or contradiction.
 */
export async function runPropagation(
  netRef: NetRefVal,
  config: Partial<PropagationConfig> = {}
): Promise<PropagationResult> {
  const fullConfig: PropagationConfig = {
    ...DEFAULT_PROPAGATION_CONFIG,
    ...config,
    budget: { ...DEFAULT_PROPAGATION_CONFIG.budget, ...config.budget },
  };

  const net = getNetwork(netRef.id);
  if (!net) throw new Error(`Network not found: ${netRef.id}`);

  let firings = 0;
  let sets = 0;
  let budgetExhausted = false;

  // Main propagation loop
  while (net.agenda.length > 0) {
    // Check budget
    if (firings >= fullConfig.budget.maxFirings) {
      budgetExhausted = true;
      net.status = { tag: "budgetExhausted", resource: "firings" };
      break;
    }
    if (sets >= fullConfig.budget.maxSets) {
      budgetExhausted = true;
      net.status = { tag: "budgetExhausted", resource: "sets" };
      break;
    }

    // Select next propagator
    const propId = selectNextPropagator(net, fullConfig);
    if (!propId) break;

    const prop = net.propagators.get(propId);
    if (!prop) continue;

    // Check if all inputs are ready
    const allInputsReady = prop.inputs.every(connId => {
      const conn = net.connectors.get(connId);
      return conn && conn.value !== undefined;
    });

    if (!allInputsReady) continue;

    // Execute propagator
    firings++;
    prop.fireCount++;
    net.stepCount++;

    // Record fire event
    if (fullConfig.recordLedger) {
      net.ledger.push({
        type: "fire",
        timestamp: Date.now(),
        propId,
        stepCount: net.stepCount,
      });
    }

    const output = await executePropagator(prop, net, netRef);

    // Handle output
    if (output.tag === "values") {
      for (const [connId, value] of output.values) {
        const conn = net.connectors.get(connId);
        if (!conn) continue;

        const connRef: ConnRefVal = {
          tag: "ConnRef",
          id: connId,
          netId: netRef.id,
          name: conn.name,
        };

        // Build dependencies for explanation
        const deps: ExplanationVal[] = prop.inputs
          .map(inputId => net.connectors.get(inputId)?.explanation)
          .filter((e): e is ExplanationVal => e !== undefined);

        const contradiction = connectorSetValue(
          connRef,
          value,
          { tag: "Str", s: `set by ${prop.name}` },
          prop.name,
          deps
        );

        if (contradiction) {
          return {
            status: net.status,
            firings,
            sets,
            budgetExhausted: false,
            contradiction,
          };
        }
        sets++;
      }
    } else if (output.tag === "contradiction") {
      // Constraint violation
      const connRef: ConnRefVal = prop.outputs.length > 0
        ? { tag: "ConnRef", id: prop.outputs[0], netId: netRef.id }
        : { tag: "ConnRef", id: prop.inputs[0], netId: netRef.id };

      const deps: ExplanationVal[] = prop.inputs
        .map(inputId => net.connectors.get(inputId)?.explanation)
        .filter((e): e is ExplanationVal => e !== undefined);

      const explanation: ExplanationVal = {
        tag: "Explanation",
        kind: "conflict",
        conn: connRef,
        left: deps[0] ?? { tag: "Explanation", kind: "assumption", conn: connRef, valueHash: "", because: VUnit },
        right: { tag: "Explanation", kind: "assumption", conn: connRef, valueHash: "", because: { tag: "Str", s: output.message } },
        message: output.message,
      };

      const contradiction = makeContradiction(explanation, prop.id, netRef.id);
      net.contradictions.push(contradiction);
      net.status = { tag: "contradiction", contradiction };

      return {
        status: net.status,
        firings,
        sets,
        budgetExhausted: false,
        contradiction,
      };
    } else if (output.tag === "suspend") {
      // Oracle needed
      net.status = { tag: "suspended", op: output.op, args: output.args };
      return {
        status: net.status,
        firings,
        sets,
        budgetExhausted: false,
      };
    }
    // noChange: continue

    // Update status
    if (net.agenda.length === 0) {
      net.status = { tag: "quiescent" };
    } else {
      net.status = { tag: "pending", count: net.agenda.length };
    }
  }

  // Final status
  if (!budgetExhausted && net.status.tag !== "contradiction" && net.status.tag !== "suspended") {
    net.status = { tag: "quiescent" };
  }

  return {
    status: net.status,
    firings,
    sets,
    budgetExhausted,
  };
}

/**
 * Select the next propagator from the agenda.
 */
function selectNextPropagator(
  net: NetworkState,
  config: PropagationConfig
): string | undefined {
  if (net.agenda.length === 0) return undefined;

  switch (config.schedulingStrategy) {
    case "fifo":
      return net.agenda.shift();

    case "priority": {
      // Sort by priority (lower = higher priority)
      let bestIdx = 0;
      let bestPriority = Infinity;
      for (let i = 0; i < net.agenda.length; i++) {
        const prop = net.propagators.get(net.agenda[i]);
        if (prop && prop.priority < bestPriority) {
          bestPriority = prop.priority;
          bestIdx = i;
        }
      }
      return net.agenda.splice(bestIdx, 1)[0];
    }

    case "random": {
      const idx = Math.floor(Math.random() * net.agenda.length);
      return net.agenda.splice(idx, 1)[0];
    }

    default:
      return net.agenda.shift();
  }
}

/**
 * Execute a propagator.
 */
async function executePropagator(
  prop: PropagatorState,
  net: NetworkState,
  netRef: NetRefVal
): Promise<PropagatorOutput> {
  // Build context
  const inputs = new Map<string, Val>();
  const inputExplanations = new Map<string, ExplanationVal>();

  for (const connId of prop.inputs) {
    const conn = net.connectors.get(connId);
    if (conn && conn.value !== undefined) {
      inputs.set(connId, conn.value);
      if (conn.explanation) {
        inputExplanations.set(connId, conn.explanation);
      }
    }
  }

  const ctx: PropagatorContext = {
    inputs,
    inputExplanations,
    propagatorName: prop.name,
    netRef,
  };

  // Try scripted propagator first
  const scripted = getScriptedPropagator(prop.name);
  if (scripted) {
    return scripted.execute(ctx);
  }

  // Built-in propagators
  return executeBuiltinPropagator(prop, ctx);
}

/**
 * Execute a built-in propagator.
 */
function executeBuiltinPropagator(
  prop: PropagatorState,
  ctx: PropagatorContext
): PropagatorOutput {
  // Identity propagator (for testing)
  if (prop.name === "identity" && prop.inputs.length === 1 && prop.outputs.length === 1) {
    const inputVal = ctx.inputs.get(prop.inputs[0]);
    if (inputVal !== undefined) {
      return { tag: "values", values: new Map([[prop.outputs[0], inputVal]]) };
    }
  }

  // Boolean constraint: value must be true
  if (prop.name === "mustBeTrue" && prop.isConstraint) {
    const inputVal = ctx.inputs.get(prop.inputs[0]);
    if (inputVal && inputVal.tag === "Bool" && !inputVal.b) {
      return { tag: "contradiction", message: "Constraint 'mustBeTrue' violated: value is false" };
    }
    return { tag: "noChange" };
  }

  // Default: no change
  return { tag: "noChange" };
}

// ─────────────────────────────────────────────────────────────────
// Constraint-directed operations
// ─────────────────────────────────────────────────────────────────

/**
 * Check if a network has a contradiction.
 */
export function hasContradiction(netRef: NetRefVal): boolean {
  const net = getNetwork(netRef.id);
  if (!net) return false;
  return net.status.tag === "contradiction";
}

/**
 * Get the latest contradiction from a network.
 */
export function getContradiction(netRef: NetRefVal): ContradictionVal | undefined {
  const net = getNetwork(netRef.id);
  if (!net) return undefined;
  if (net.status.tag === "contradiction") {
    return net.status.contradiction;
  }
  return net.contradictions[net.contradictions.length - 1];
}

/**
 * Check if a network is quiescent.
 */
export function isQuiescent(netRef: NetRefVal): boolean {
  const net = getNetwork(netRef.id);
  if (!net) return false;
  return net.status.tag === "quiescent";
}

/**
 * Get the number of pending propagators.
 */
export function getPendingCount(netRef: NetRefVal): number {
  const net = getNetwork(netRef.id);
  if (!net) return 0;
  return net.agenda.length;
}
