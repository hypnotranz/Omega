// src/core/generic/coercion.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Prompt 14: Coercion graph and path finding for type conversions

import type { Val } from "../eval/values";
import {
  type TypeTag,
  type TypeSignature,
  type CoercionEntry,
  type CoercionPath,
} from "./types";
import {
  getRegistry,
  getCoercionsFrom,
  getAllTypeTags,
  lookupCoercion,
  logGenericEvent,
} from "./registry";

// ─────────────────────────────────────────────────────────────────
// Coercion Graph Construction
// ─────────────────────────────────────────────────────────────────

/**
 * CoercionGraph: Adjacency list representation of the coercion graph.
 */
export type CoercionGraph = Map<TypeTag, Map<TypeTag, CoercionEntry>>;

/**
 * Build the coercion graph from a registry.
 */
export function buildCoercionGraph(registryId: string): CoercionGraph {
  const registry = getRegistry(registryId);
  if (!registry) return new Map();

  return registry.coercions;
}

/**
 * Get all outgoing edges from a type in the graph.
 */
export function getOutgoingCoercions(
  registryId: string,
  fromTag: TypeTag
): CoercionEntry[] {
  return getCoercionsFrom(registryId, fromTag);
}

// ─────────────────────────────────────────────────────────────────
// Path Finding
// ─────────────────────────────────────────────────────────────────

/**
 * Find the shortest coercion path from one type to another.
 * Uses BFS for shortest path, considering costs.
 */
export function findCoercionPath(
  registryId: string,
  fromTag: TypeTag,
  toTag: TypeTag,
  maxDepth: number = 5
): CoercionPath | undefined {
  if (fromTag === toTag) {
    // No coercion needed
    return { steps: [], totalCost: 0, argIndices: [], targetSig: [toTag] };
  }

  // BFS with cost tracking
  const visited = new Set<TypeTag>();
  const queue: Array<{ tag: TypeTag; path: CoercionEntry[]; cost: number }> = [
    { tag: fromTag, path: [], cost: 0 },
  ];

  while (queue.length > 0) {
    // Sort by cost for Dijkstra-like behavior
    queue.sort((a, b) => a.cost - b.cost);
    const current = queue.shift()!;

    if (current.path.length >= maxDepth) {
      continue;
    }

    if (visited.has(current.tag)) {
      continue;
    }
    visited.add(current.tag);

    // Get outgoing edges
    const outgoing = getCoercionsFrom(registryId, current.tag);
    for (const coercion of outgoing) {
      const newPath = [...current.path, coercion];
      const newCost = current.cost + coercion.cost;

      if (coercion.toTag === toTag) {
        // Found path
        return {
          steps: newPath,
          totalCost: newCost,
          argIndices: [],
          targetSig: [toTag],
        };
      }

      if (!visited.has(coercion.toTag)) {
        queue.push({ tag: coercion.toTag, path: newPath, cost: newCost });
      }
    }
  }

  // No path found
  return undefined;
}

/**
 * Find all coercion paths from one type to another.
 * Returns paths sorted by cost.
 */
export function findAllCoercionPaths(
  registryId: string,
  fromTag: TypeTag,
  toTag: TypeTag,
  maxDepth: number = 5,
  maxPaths: number = 10
): CoercionPath[] {
  if (fromTag === toTag) {
    return [{ steps: [], totalCost: 0, argIndices: [], targetSig: [toTag] }];
  }

  const paths: CoercionPath[] = [];

  // DFS to find all paths
  function dfs(
    current: TypeTag,
    path: CoercionEntry[],
    cost: number,
    visited: Set<TypeTag>
  ): void {
    if (paths.length >= maxPaths) return;
    if (path.length >= maxDepth) return;

    const outgoing = getCoercionsFrom(registryId, current);
    for (const coercion of outgoing) {
      if (visited.has(coercion.toTag)) continue;

      const newPath = [...path, coercion];
      const newCost = cost + coercion.cost;

      if (coercion.toTag === toTag) {
        paths.push({
          steps: newPath,
          totalCost: newCost,
          argIndices: [],
          targetSig: [toTag],
        });
      } else {
        const newVisited = new Set(visited);
        newVisited.add(coercion.toTag);
        dfs(coercion.toTag, newPath, newCost, newVisited);
      }
    }
  }

  dfs(fromTag, [], 0, new Set([fromTag]));

  // Sort by cost
  paths.sort((a, b) => a.totalCost - b.totalCost);
  return paths;
}

// ─────────────────────────────────────────────────────────────────
// Signature-Level Coercion
// ─────────────────────────────────────────────────────────────────

/**
 * Find a coercion path for a full signature.
 * Returns paths that coerce the source signature to the target signature.
 */
export function findSignatureCoercion(
  registryId: string,
  sourceSig: TypeSignature,
  targetSig: TypeSignature,
  maxDepth: number = 5
): CoercionPath | undefined {
  if (sourceSig.length !== targetSig.length) {
    return undefined;
  }

  const allSteps: CoercionEntry[] = [];
  const argIndices: number[] = [];
  let totalCost = 0;

  for (let i = 0; i < sourceSig.length; i++) {
    if (sourceSig[i] === targetSig[i]) {
      // No coercion needed for this argument
      continue;
    }

    const path = findCoercionPath(registryId, sourceSig[i], targetSig[i], maxDepth);
    if (!path) {
      // Can't coerce this argument
      return undefined;
    }

    allSteps.push(...path.steps);
    argIndices.push(i);
    totalCost += path.totalCost;
  }

  return {
    steps: allSteps,
    totalCost,
    argIndices,
    targetSig,
  };
}

/**
 * Find all possible target signatures that the source can be coerced to.
 */
export function findReachableSignatures(
  registryId: string,
  sourceSig: TypeSignature,
  maxDepth: number = 3
): Array<{ signature: TypeSignature; path: CoercionPath }> {
  const allTags = getAllTypeTags(registryId);
  const results: Array<{ signature: TypeSignature; path: CoercionPath }> = [];

  // For single-argument signatures, enumerate reachable types
  if (sourceSig.length === 1) {
    for (const targetTag of allTags) {
      const path = findCoercionPath(registryId, sourceSig[0], targetTag, maxDepth);
      if (path) {
        results.push({
          signature: [targetTag],
          path: { ...path, argIndices: [0] },
        });
      }
    }
  }
  // For multi-argument signatures, this becomes combinatorial
  // For now, just return direct coercions
  else {
    // Get all coercions for each position and combine
    const optionsPerArg: TypeTag[][] = [];
    for (let i = 0; i < sourceSig.length; i++) {
      const outgoing = getCoercionsFrom(registryId, sourceSig[i]);
      optionsPerArg.push([sourceSig[i], ...outgoing.map(c => c.toTag)]);
    }

    // Generate combinations (limited to avoid explosion)
    const combinations = generateCombinations(optionsPerArg, 100);
    for (const combo of combinations) {
      const path = findSignatureCoercion(registryId, sourceSig, combo, maxDepth);
      if (path) {
        results.push({ signature: combo, path });
      }
    }
  }

  // Sort by cost
  results.sort((a, b) => a.path.totalCost - b.path.totalCost);
  return results;
}

/**
 * Generate combinations of options (limited count).
 */
function generateCombinations<T>(options: T[][], maxCount: number): T[][] {
  const results: T[][] = [];

  function gen(index: number, current: T[]): void {
    if (results.length >= maxCount) return;
    if (index >= options.length) {
      results.push([...current]);
      return;
    }
    for (const opt of options[index]) {
      current.push(opt);
      gen(index + 1, current);
      current.pop();
    }
  }

  gen(0, []);
  return results;
}

// ─────────────────────────────────────────────────────────────────
// Ambiguity Detection
// ─────────────────────────────────────────────────────────────────

/**
 * Check if there are multiple equally-good coercion paths.
 */
export function detectAmbiguity(
  registryId: string,
  fromTag: TypeTag,
  toTag: TypeTag
): { ambiguous: boolean; paths: CoercionPath[] } {
  const paths = findAllCoercionPaths(registryId, fromTag, toTag, 5, 10);

  if (paths.length <= 1) {
    return { ambiguous: false, paths };
  }

  // Check if multiple paths have the same minimum cost
  const minCost = paths[0].totalCost;
  const minCostPaths = paths.filter(p => p.totalCost === minCost);

  if (minCostPaths.length > 1) {
    logGenericEvent({
      tag: "ambiguity",
      op: "coercion",
      signature: [fromTag, toTag],
      pathCount: minCostPaths.length,
      timestamp: Date.now(),
    });
    return { ambiguous: true, paths: minCostPaths };
  }

  return { ambiguous: false, paths };
}

/**
 * Check for ambiguity at the signature level.
 */
export function detectSignatureAmbiguity(
  registryId: string,
  sourceSig: TypeSignature,
  targetSig: TypeSignature
): { ambiguous: boolean; paths: CoercionPath[] } {
  const paths: CoercionPath[] = [];

  // For each argument that needs coercion, check for ambiguity
  for (let i = 0; i < sourceSig.length; i++) {
    if (sourceSig[i] !== targetSig[i]) {
      const { ambiguous, paths: argPaths } = detectAmbiguity(
        registryId,
        sourceSig[i],
        targetSig[i]
      );

      if (ambiguous) {
        // Found ambiguity in this argument
        for (const p of argPaths) {
          paths.push({ ...p, argIndices: [i] });
        }
        return { ambiguous: true, paths };
      }
    }
  }

  return { ambiguous: false, paths: [] };
}

// ─────────────────────────────────────────────────────────────────
// Coercion Application
// ─────────────────────────────────────────────────────────────────

/**
 * CoercionResult: Result of applying a coercion path.
 */
export type CoercionResult =
  | { tag: "success"; value: Val }
  | { tag: "error"; step: number; message: string };

/**
 * Apply a single coercion step.
 * Note: This is a stub that needs integration with the evaluator.
 */
export function applyCoercionStep(
  coercion: CoercionEntry,
  value: Val,
  applyFn: (proc: Val, args: Val[]) => Val
): CoercionResult {
  try {
    const result = applyFn(coercion.proc, [value]);
    return { tag: "success", value: result };
  } catch (error) {
    return {
      tag: "error",
      step: 0,
      message: error instanceof Error ? error.message : String(error),
    };
  }
}

/**
 * Apply a full coercion path.
 */
export function applyCoercionPath(
  path: CoercionPath,
  value: Val,
  applyFn: (proc: Val, args: Val[]) => Val
): CoercionResult {
  let current = value;

  for (let i = 0; i < path.steps.length; i++) {
    const result = applyCoercionStep(path.steps[i], current, applyFn);
    if (result.tag === "error") {
      return { ...result, step: i };
    }
    current = result.value;
  }

  return { tag: "success", value: current };
}

// ─────────────────────────────────────────────────────────────────
// Type Hierarchy Utilities
// ─────────────────────────────────────────────────────────────────

/**
 * Check if a type is a subtype of another (based on coercion path).
 */
export function isSubtype(
  registryId: string,
  subTag: TypeTag,
  superTag: TypeTag
): boolean {
  if (subTag === superTag) return true;
  const path = findCoercionPath(registryId, subTag, superTag);
  return path !== undefined;
}

/**
 * Find the common supertype of two types (if any).
 */
export function findCommonSupertype(
  registryId: string,
  tag1: TypeTag,
  tag2: TypeTag
): TypeTag | undefined {
  if (tag1 === tag2) return tag1;

  const allTags = getAllTypeTags(registryId);

  // Find types reachable from both
  for (const candidate of allTags) {
    const path1 = findCoercionPath(registryId, tag1, candidate);
    const path2 = findCoercionPath(registryId, tag2, candidate);
    if (path1 && path2) {
      return candidate;
    }
  }

  return undefined;
}

/**
 * Get the transitive closure of reachable types from a source type.
 */
export function getReachableTypes(
  registryId: string,
  fromTag: TypeTag,
  maxDepth: number = 5
): Set<TypeTag> {
  const reachable = new Set<TypeTag>([fromTag]);
  const queue: Array<{ tag: TypeTag; depth: number }> = [{ tag: fromTag, depth: 0 }];

  while (queue.length > 0) {
    const { tag, depth } = queue.shift()!;
    if (depth >= maxDepth) continue;

    const outgoing = getCoercionsFrom(registryId, tag);
    for (const coercion of outgoing) {
      if (!reachable.has(coercion.toTag)) {
        reachable.add(coercion.toTag);
        queue.push({ tag: coercion.toTag, depth: depth + 1 });
      }
    }
  }

  return reachable;
}
