// src/core/generic/registry.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Prompt 14: Generic registry for method and coercion tables

import type { Val, GenericRegistryVal } from "../eval/values";
import type { Hash } from "../artifacts/hash";
import { sha256JSON } from "../artifacts/hash";
import {
  type TypeTag,
  type TypeSignature,
  type SignatureKey,
  type MethodEntry,
  type MethodTable,
  type CoercionEntry,
  type CoercionTable,
  type GenericOpDef,
  type RegistryState,
  type GenericEvent,
  signatureToKey,
  makeGenericRegistry,
} from "./types";

// ─────────────────────────────────────────────────────────────────
// Registry Store
// ─────────────────────────────────────────────────────────────────

const registryStore = new Map<string, RegistryState>();
let nextRegistryId = 0;

/**
 * Generate a unique registry ID.
 */
function genRegistryId(): string {
  return `registry-${nextRegistryId++}`;
}

/**
 * Reset the registry store (for testing).
 */
export function resetRegistryStore(): void {
  registryStore.clear();
  nextRegistryId = 0;
}

// ─────────────────────────────────────────────────────────────────
// Registry Creation and Access
// ─────────────────────────────────────────────────────────────────

/**
 * Create a new generic operations registry.
 */
export function createRegistry(name?: string): GenericRegistryVal {
  const id = genRegistryId();
  const state: RegistryState = {
    id,
    name,
    ops: new Map(),
    methods: new Map(),
    coercions: new Map(),
    stats: {
      methodHits: 0,
      methodMisses: 0,
      coercionHits: 0,
      coercionMisses: 0,
      synthesisCalls: 0,
    },
    createdAt: Date.now(),
  };
  registryStore.set(id, state);
  return makeGenericRegistry(id, name);
}

/**
 * Get a registry by ID.
 */
export function getRegistry(id: string): RegistryState | undefined {
  return registryStore.get(id);
}

/**
 * Clone a registry (for exploration/speculation).
 */
export function cloneRegistry(state: RegistryState): RegistryState {
  const id = genRegistryId();
  const cloned: RegistryState = {
    id,
    name: state.name ? `${state.name}-clone` : undefined,
    ops: new Map(state.ops),
    methods: cloneMethodTable(state.methods),
    coercions: cloneCoercionTable(state.coercions),
    stats: { ...state.stats },
    createdAt: Date.now(),
  };
  registryStore.set(id, cloned);
  return cloned;
}

function cloneMethodTable(table: MethodTable): MethodTable {
  const cloned = new Map<string, Map<SignatureKey, MethodEntry>>();
  for (const [op, sigMap] of table) {
    cloned.set(op, new Map(sigMap));
  }
  return cloned;
}

function cloneCoercionTable(table: CoercionTable): CoercionTable {
  const cloned = new Map<TypeTag, Map<TypeTag, CoercionEntry>>();
  for (const [from, toMap] of table) {
    cloned.set(from, new Map(toMap));
  }
  return cloned;
}

// ─────────────────────────────────────────────────────────────────
// Generic Operation Definition
// ─────────────────────────────────────────────────────────────────

/**
 * Define a generic operation.
 */
export function defGeneric(
  registryId: string,
  name: string,
  arity: number,
  options: { contract?: Val; sealed?: boolean } = {}
): boolean {
  const registry = registryStore.get(registryId);
  if (!registry) return false;

  if (registry.ops.has(name)) {
    // Already defined
    return false;
  }

  const def: GenericOpDef = {
    name,
    arity,
    contract: options.contract,
    sealed: options.sealed ?? false,
    createdAt: Date.now(),
  };

  registry.ops.set(name, def);
  return true;
}

/**
 * Get a generic operation definition.
 */
export function getGenericOp(registryId: string, name: string): GenericOpDef | undefined {
  const registry = registryStore.get(registryId);
  if (!registry) return undefined;
  return registry.ops.get(name);
}

/**
 * Check if a generic operation is defined.
 */
export function hasGenericOp(registryId: string, name: string): boolean {
  const registry = registryStore.get(registryId);
  if (!registry) return false;
  return registry.ops.has(name);
}

// ─────────────────────────────────────────────────────────────────
// Method Table Operations
// ─────────────────────────────────────────────────────────────────

/**
 * Register a method for a generic operation.
 */
export function defMethod(
  registryId: string,
  op: string,
  signature: TypeSignature,
  proc: Val,
  options: { synthesized?: boolean; commitHash?: Hash; meta?: MethodEntry["meta"] } = {}
): boolean {
  const registry = registryStore.get(registryId);
  if (!registry) return false;

  // Check if operation is sealed
  const opDef = registry.ops.get(op);
  if (opDef?.sealed) {
    return false;
  }

  // Get or create method map for this operation
  let sigMap = registry.methods.get(op);
  if (!sigMap) {
    sigMap = new Map();
    registry.methods.set(op, sigMap);
  }

  const key = signatureToKey(signature);
  const entry: MethodEntry = {
    op,
    signature,
    proc,
    synthesized: options.synthesized ?? false,
    commitHash: options.commitHash,
    createdAt: Date.now(),
    meta: options.meta,
  };

  sigMap.set(key, entry);
  return true;
}

/**
 * Look up a method by operation and signature.
 */
export function lookupMethod(
  registryId: string,
  op: string,
  signature: TypeSignature
): MethodEntry | undefined {
  const registry = registryStore.get(registryId);
  if (!registry) return undefined;

  const sigMap = registry.methods.get(op);
  if (!sigMap) return undefined;

  const key = signatureToKey(signature);
  const entry = sigMap.get(key);

  // Update stats
  if (entry) {
    registry.stats.methodHits++;
  } else {
    registry.stats.methodMisses++;
  }

  return entry;
}

/**
 * Remove a method.
 */
export function removeMethod(
  registryId: string,
  op: string,
  signature: TypeSignature
): boolean {
  const registry = registryStore.get(registryId);
  if (!registry) return false;

  const sigMap = registry.methods.get(op);
  if (!sigMap) return false;

  const key = signatureToKey(signature);
  return sigMap.delete(key);
}

/**
 * Get all methods for an operation.
 */
export function getMethodsForOp(registryId: string, op: string): MethodEntry[] {
  const registry = registryStore.get(registryId);
  if (!registry) return [];

  const sigMap = registry.methods.get(op);
  if (!sigMap) return [];

  return Array.from(sigMap.values());
}

/**
 * Get all registered operations with their method counts.
 */
export function listOperations(registryId: string): Array<{ op: string; methodCount: number }> {
  const registry = registryStore.get(registryId);
  if (!registry) return [];

  return Array.from(registry.methods.entries()).map(([op, sigMap]) => ({
    op,
    methodCount: sigMap.size,
  }));
}

// ─────────────────────────────────────────────────────────────────
// Coercion Table Operations
// ─────────────────────────────────────────────────────────────────

/**
 * Register a coercion between types.
 */
export function defCoercion(
  registryId: string,
  fromTag: TypeTag,
  toTag: TypeTag,
  proc: Val,
  options: { lossy?: boolean; synthesized?: boolean; commitHash?: Hash; cost?: number } = {}
): boolean {
  const registry = registryStore.get(registryId);
  if (!registry) return false;

  // Get or create coercion map for source type
  let toMap = registry.coercions.get(fromTag);
  if (!toMap) {
    toMap = new Map();
    registry.coercions.set(fromTag, toMap);
  }

  const entry: CoercionEntry = {
    fromTag,
    toTag,
    proc,
    lossy: options.lossy ?? false,
    synthesized: options.synthesized ?? false,
    commitHash: options.commitHash,
    createdAt: Date.now(),
    cost: options.cost ?? 1,
  };

  toMap.set(toTag, entry);
  return true;
}

/**
 * Look up a direct coercion.
 */
export function lookupCoercion(
  registryId: string,
  fromTag: TypeTag,
  toTag: TypeTag
): CoercionEntry | undefined {
  const registry = registryStore.get(registryId);
  if (!registry) return undefined;

  const toMap = registry.coercions.get(fromTag);
  if (!toMap) return undefined;

  const entry = toMap.get(toTag);

  // Update stats
  if (entry) {
    registry.stats.coercionHits++;
  } else {
    registry.stats.coercionMisses++;
  }

  return entry;
}

/**
 * Remove a coercion.
 */
export function removeCoercion(
  registryId: string,
  fromTag: TypeTag,
  toTag: TypeTag
): boolean {
  const registry = registryStore.get(registryId);
  if (!registry) return false;

  const toMap = registry.coercions.get(fromTag);
  if (!toMap) return false;

  return toMap.delete(toTag);
}

/**
 * Get all coercions from a type.
 */
export function getCoercionsFrom(registryId: string, fromTag: TypeTag): CoercionEntry[] {
  const registry = registryStore.get(registryId);
  if (!registry) return [];

  const toMap = registry.coercions.get(fromTag);
  if (!toMap) return [];

  return Array.from(toMap.values());
}

/**
 * Get all coercions to a type.
 */
export function getCoercionsTo(registryId: string, toTag: TypeTag): CoercionEntry[] {
  const registry = registryStore.get(registryId);
  if (!registry) return [];

  const results: CoercionEntry[] = [];
  for (const toMap of registry.coercions.values()) {
    const entry = toMap.get(toTag);
    if (entry) {
      results.push(entry);
    }
  }
  return results;
}

/**
 * Get all registered type tags (from coercion table).
 */
export function getAllTypeTags(registryId: string): TypeTag[] {
  const registry = registryStore.get(registryId);
  if (!registry) return [];

  const tags = new Set<TypeTag>();
  for (const [from, toMap] of registry.coercions) {
    tags.add(from);
    for (const to of toMap.keys()) {
      tags.add(to);
    }
  }
  return Array.from(tags);
}

// ─────────────────────────────────────────────────────────────────
// Registry Statistics
// ─────────────────────────────────────────────────────────────────

/**
 * Get registry statistics.
 */
export function getRegistryStats(registryId: string): RegistryState["stats"] | undefined {
  const registry = registryStore.get(registryId);
  if (!registry) return undefined;
  return { ...registry.stats };
}

/**
 * Increment synthesis call count.
 */
export function incrementSynthesisCalls(registryId: string): void {
  const registry = registryStore.get(registryId);
  if (registry) {
    registry.stats.synthesisCalls++;
  }
}

// ─────────────────────────────────────────────────────────────────
// Commit Operations
// ─────────────────────────────────────────────────────────────────

/**
 * Commit a method with a hash.
 */
export function commitMethod(
  registryId: string,
  op: string,
  signature: TypeSignature,
  proc: Val
): Hash | undefined {
  const registry = registryStore.get(registryId);
  if (!registry) return undefined;

  const commitHash = sha256JSON({ op, signature, proc, timestamp: Date.now() });

  const success = defMethod(registryId, op, signature, proc, {
    synthesized: true,
    commitHash,
    meta: { source: "inferred" },
  });

  return success ? commitHash : undefined;
}

/**
 * Commit a coercion with a hash.
 */
export function commitCoercion(
  registryId: string,
  fromTag: TypeTag,
  toTag: TypeTag,
  proc: Val
): Hash | undefined {
  const registry = registryStore.get(registryId);
  if (!registry) return undefined;

  const commitHash = sha256JSON({ fromTag, toTag, proc, timestamp: Date.now() });

  const success = defCoercion(registryId, fromTag, toTag, proc, {
    synthesized: true,
    commitHash,
  });

  return success ? commitHash : undefined;
}

// ─────────────────────────────────────────────────────────────────
// Event Logging
// ─────────────────────────────────────────────────────────────────

const eventLog: GenericEvent[] = [];

/**
 * Log a generic event.
 */
export function logGenericEvent(event: GenericEvent): void {
  eventLog.push(event);
}

/**
 * Get recent events.
 */
export function getRecentEvents(limit: number = 100): GenericEvent[] {
  return eventLog.slice(-limit);
}

/**
 * Clear event log (for testing).
 */
export function clearEventLog(): void {
  eventLog.length = 0;
}

/**
 * Count events by type.
 */
export function countEvents(tag: GenericEvent["tag"]): number {
  return eventLog.filter(e => e.tag === tag).length;
}

// ─────────────────────────────────────────────────────────────────
// Registry Introspection
// ─────────────────────────────────────────────────────────────────

/**
 * Get a summary of the registry.
 */
export function getRegistrySummary(registryId: string): {
  id: string;
  name?: string;
  opCount: number;
  methodCount: number;
  coercionCount: number;
  stats: RegistryState["stats"];
} | undefined {
  const registry = registryStore.get(registryId);
  if (!registry) return undefined;

  let methodCount = 0;
  for (const sigMap of registry.methods.values()) {
    methodCount += sigMap.size;
  }

  let coercionCount = 0;
  for (const toMap of registry.coercions.values()) {
    coercionCount += toMap.size;
  }

  return {
    id: registry.id,
    name: registry.name,
    opCount: registry.ops.size,
    methodCount,
    coercionCount,
    stats: { ...registry.stats },
  };
}
