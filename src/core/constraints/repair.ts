// src/core/constraints/repair.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Prompt 12: Repair synthesis via amb search for constraint contradictions

import type { Val, NetRefVal, ConnRefVal, ContradictionVal } from "../eval/values";
import { VUnit, VTrue } from "../eval/values";
import type { DistVal, DistItem } from "../eval/dist";
import { distFrom } from "../eval/dist";
import {
  type RepairOption,
  type RepairResult,
  type NetworkState,
  type PropagatorState,
} from "./types";
import {
  getNetwork,
  setNetwork,
  cloneNetwork,
  removePropagator,
  connectorForgetValue,
  connectorSetValue,
  registerPropagator,
} from "./network";
import {
  runPropagation,
  type PropagationResult,
} from "./engine";
import {
  extractUnsatCore,
  type UnsatCore,
} from "./diagnosis";

// ─────────────────────────────────────────────────────────────────
// Repair option generation
// ─────────────────────────────────────────────────────────────────

/**
 * Generate repair options from an unsat core.
 */
export function generateRepairOptions(
  netRef: NetRefVal,
  core: UnsatCore
): RepairOption[] {
  const net = getNetwork(netRef.id);
  if (!net) return [];

  const options: RepairOption[] = [];

  // Option 1: Drop constraints in the core
  for (const constraintId of core.constraintIds) {
    const prop = net.propagators.get(constraintId);
    if (prop && prop.isConstraint) {
      options.push({ tag: "dropConstraint", constraintId });
    }
  }

  // Option 2: Retry with different values for assumptions
  for (const assumption of core.assumptions) {
    // This would need alternative values - for now just mark as option
    options.push({
      tag: "retryWithValue",
      connId: assumption.connId,
      newValue: VUnit, // Placeholder - caller should provide alternatives
    });
  }

  return options;
}

/**
 * Add custom repair options to the list.
 */
export function addRepairOption(options: RepairOption[], option: RepairOption): RepairOption[] {
  return [...options, option];
}

// ─────────────────────────────────────────────────────────────────
// Repair execution
// ─────────────────────────────────────────────────────────────────

/**
 * Try a single repair option on a network.
 * Returns the result with success/failure and new state.
 */
export async function tryRepair(
  netRef: NetRefVal,
  option: RepairOption
): Promise<RepairResult> {
  const net = getNetwork(netRef.id);
  if (!net) {
    return {
      option,
      success: false,
    };
  }

  // Clone the network for this repair attempt
  const cloned = cloneNetwork(net);
  const clonedRef: NetRefVal = { tag: "NetRef", id: cloned.id, name: cloned.name };

  // Apply the repair
  const applied = applyRepairOption(clonedRef, option);
  if (!applied) {
    return {
      option,
      success: false,
    };
  }

  // Run propagation on the repaired network
  const result = await runPropagation(clonedRef);

  if (result.status.tag === "quiescent") {
    return {
      option,
      success: true,
      newNetwork: getNetwork(clonedRef.id),
    };
  } else if (result.status.tag === "contradiction") {
    return {
      option,
      success: false,
      newContradiction: result.contradiction,
    };
  } else {
    return {
      option,
      success: false,
    };
  }
}

/**
 * Apply a repair option to a network.
 */
function applyRepairOption(netRef: NetRefVal, option: RepairOption): boolean {
  const net = getNetwork(netRef.id);
  if (!net) return false;

  switch (option.tag) {
    case "dropConstraint": {
      return removePropagator(netRef, option.constraintId);
    }

    case "relaxConstraint": {
      // TODO: Implement constraint relaxation
      return false;
    }

    case "swapPropagator": {
      // Remove old propagator
      const oldProp = net.propagators.get(option.oldId);
      if (!oldProp) return false;

      removePropagator(netRef, option.oldId);

      // Add new propagator with same inputs/outputs
      const inputs = oldProp.inputs.map(id => {
        const conn = net.connectors.get(id);
        return { tag: "ConnRef" as const, id, netId: netRef.id, name: conn?.name };
      });
      const outputs = oldProp.outputs.map(id => {
        const conn = net.connectors.get(id);
        return { tag: "ConnRef" as const, id, netId: netRef.id, name: conn?.name };
      });

      registerPropagator(netRef, `${oldProp.name}_replaced`, inputs, outputs, option.newProcRef, {
        kind: oldProp.kind,
        isConstraint: oldProp.isConstraint,
        priority: oldProp.priority,
        requiresOracle: false, // New proc might not need oracle
      });
      return true;
    }

    case "addValidator": {
      const inputs = option.inputs.map(id => {
        const conn = net.connectors.get(id);
        return { tag: "ConnRef" as const, id, netId: netRef.id, name: conn?.name };
      });

      registerPropagator(netRef, "validator", inputs, [], option.validatorProc, {
        kind: "validator",
        isConstraint: true,
        priority: -5,
      });
      return true;
    }

    case "retryWithValue": {
      const conn = net.connectors.get(option.connId);
      if (!conn) return false;

      const connRef: ConnRefVal = {
        tag: "ConnRef",
        id: option.connId,
        netId: netRef.id,
        name: conn.name,
      };

      // Clear old value
      connectorForgetValue(connRef, { tag: "Str", s: "repair retry" });

      // Set new value (if provided and not Unit)
      if (option.newValue.tag !== "Unit") {
        connectorSetValue(connRef, option.newValue, { tag: "Str", s: "repair value" });
      }
      return true;
    }

    default:
      return false;
  }
}

// ─────────────────────────────────────────────────────────────────
// Search-based repair (amb-style exploration)
// ─────────────────────────────────────────────────────────────────

/**
 * RepairSearchConfig: Configuration for repair search.
 */
export type RepairSearchConfig = {
  /** Maximum number of repair attempts */
  maxAttempts: number;
  /** Search strategy */
  strategy: "bfs" | "dfs" | "beam";
  /** Beam width (for beam search) */
  beamWidth?: number;
  /** Scoring function for repairs (higher = better) */
  scoreFn?: (result: RepairResult) => number;
};

export const DEFAULT_REPAIR_SEARCH_CONFIG: RepairSearchConfig = {
  maxAttempts: 10,
  strategy: "bfs",
};

/**
 * RepairSearchResult: Result of searching for repairs.
 */
export type RepairSearchResult = {
  /** Did we find a successful repair? */
  found: boolean;
  /** The successful repair (if found) */
  successfulRepair?: RepairResult;
  /** All repair attempts */
  attempts: RepairResult[];
  /** Number of attempts made */
  attemptCount: number;
  /** Distribution of repair options with scores */
  repairDist?: DistVal;
};

/**
 * Search for a valid repair using amb-style exploration.
 */
export async function searchRepairs(
  netRef: NetRefVal,
  contradiction: ContradictionVal,
  options: RepairOption[],
  config: Partial<RepairSearchConfig> = {}
): Promise<RepairSearchResult> {
  const fullConfig: RepairSearchConfig = {
    ...DEFAULT_REPAIR_SEARCH_CONFIG,
    ...config,
  };

  const attempts: RepairResult[] = [];
  let attemptCount = 0;
  let successfulRepair: RepairResult | undefined;

  // Generate frontier based on strategy
  const frontier: RepairOption[] = [...options];

  while (frontier.length > 0 && attemptCount < fullConfig.maxAttempts) {
    // Select next option based on strategy
    let option: RepairOption;
    switch (fullConfig.strategy) {
      case "dfs":
        option = frontier.pop()!;
        break;
      case "bfs":
      default:
        option = frontier.shift()!;
        break;
    }

    attemptCount++;
    const result = await tryRepair(netRef, option);
    attempts.push(result);

    if (result.success) {
      successfulRepair = result;
      break;
    }

    // If beam search, prune based on scores
    if (fullConfig.strategy === "beam" && fullConfig.beamWidth) {
      // Score remaining options and keep top k
      // (simplified - in full implementation would score based on partial results)
    }
  }

  // Build distribution of repair options
  const scoreFn = fullConfig.scoreFn ?? ((r: RepairResult) => r.success ? 1 : 0);
  const items: DistItem[] = attempts.map(r => ({
    v: repairOptionToVal(r.option),
    w: scoreFn(r),
  }));

  return {
    found: successfulRepair !== undefined,
    successfulRepair,
    attempts,
    attemptCount,
    repairDist: distFrom(items, { kind: "repair-search" }),
  };
}

/**
 * Convert a repair option to a Val for distribution.
 */
function repairOptionToVal(option: RepairOption): Val {
  return {
    tag: "Map",
    entries: [
      [{ tag: "Sym", name: "tag" }, { tag: "Str", s: option.tag }],
      ...Object.entries(option)
        .filter(([k]) => k !== "tag")
        .map(([k, v]): [Val, Val] => [
          { tag: "Sym", name: k },
          typeof v === "string" ? { tag: "Str", s: v } : (v as Val),
        ]),
    ],
  };
}

// ─────────────────────────────────────────────────────────────────
// High-level repair API
// ─────────────────────────────────────────────────────────────────

/**
 * Repair a network contradiction automatically.
 * Extracts unsat core, generates options, and searches for a fix.
 */
export async function repair(
  netRef: NetRefVal,
  contradiction: ContradictionVal,
  additionalOptions: RepairOption[] = [],
  config: Partial<RepairSearchConfig> = {}
): Promise<RepairSearchResult> {
  // Extract unsat core
  const core = extractUnsatCore(netRef, contradiction);

  // Generate repair options
  const options = generateRepairOptions(netRef, core);

  // Add additional options
  const allOptions = [...options, ...additionalOptions];

  // Search for repairs
  return searchRepairs(netRef, contradiction, allOptions, config);
}
