// src/core/generic/types.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Prompt 14: Generic operations types (data-directed programming + coercion towers)

import type { Val, TaggedVal, GenericRegistryVal, GenericMissVal } from "../eval/values";
import type { Hash } from "../artifacts/hash";

// ─────────────────────────────────────────────────────────────────
// Type Tag Types
// ─────────────────────────────────────────────────────────────────

/**
 * TypeTag: A semantic type identifier (e.g., "Text/Plain", "Doc/Email").
 * Uses hierarchical naming convention.
 */
export type TypeTag = string;

/**
 * TypeSignature: A list of type tags representing method signature.
 */
export type TypeSignature = TypeTag[];

/**
 * SignatureKey: Canonical string representation of a signature for table lookup.
 */
export type SignatureKey = string;

/**
 * Convert a signature to a canonical key.
 */
export function signatureToKey(sig: TypeSignature): SignatureKey {
  return sig.join(",");
}

/**
 * Parse a key back to a signature.
 */
export function keyToSignature(key: SignatureKey): TypeSignature {
  return key.split(",").filter(s => s.length > 0);
}

// ─────────────────────────────────────────────────────────────────
// Method Table
// ─────────────────────────────────────────────────────────────────

/**
 * MethodEntry: A registered method for a generic operation.
 */
export type MethodEntry = {
  /** The operation name */
  op: string;
  /** Type signature this method handles */
  signature: TypeSignature;
  /** The implementation procedure */
  proc: Val;
  /** Whether this method was synthesized */
  synthesized: boolean;
  /** Commit hash if committed via truth regime */
  commitHash?: Hash;
  /** Creation timestamp */
  createdAt: number;
  /** Metadata */
  meta?: {
    description?: string;
    source?: "manual" | "inferred" | "coerced";
  };
};

/**
 * MethodTable: Maps op × signature to method entries.
 */
export type MethodTable = Map<string, Map<SignatureKey, MethodEntry>>;

// ─────────────────────────────────────────────────────────────────
// Coercion Table
// ─────────────────────────────────────────────────────────────────

/**
 * CoercionEntry: A registered coercion between types.
 */
export type CoercionEntry = {
  /** Source type tag */
  fromTag: TypeTag;
  /** Target type tag */
  toTag: TypeTag;
  /** The coercion procedure */
  proc: Val;
  /** Whether this coercion is lossy */
  lossy: boolean;
  /** Whether this coercion was synthesized */
  synthesized: boolean;
  /** Commit hash if committed via truth regime */
  commitHash?: Hash;
  /** Creation timestamp */
  createdAt: number;
  /** Cost/priority for path selection (lower = preferred) */
  cost: number;
};

/**
 * CoercionTable: Maps fromTag × toTag to coercion entries.
 */
export type CoercionTable = Map<TypeTag, Map<TypeTag, CoercionEntry>>;

// ─────────────────────────────────────────────────────────────────
// Generic Operation Definition
// ─────────────────────────────────────────────────────────────────

/**
 * GenericOpDef: Definition of a generic operation.
 */
export type GenericOpDef = {
  /** Operation name */
  name: string;
  /** Expected arity */
  arity: number;
  /** Optional contract/spec for validation */
  contract?: Val;
  /** Whether the operation is sealed (no more methods can be added) */
  sealed: boolean;
  /** Creation timestamp */
  createdAt: number;
};

// ─────────────────────────────────────────────────────────────────
// Registry State
// ─────────────────────────────────────────────────────────────────

/**
 * RegistryState: Internal state of a generic operations registry.
 */
export type RegistryState = {
  /** Registry ID */
  id: string;
  /** Human-readable name */
  name?: string;
  /** Generic operation definitions */
  ops: Map<string, GenericOpDef>;
  /** Method table */
  methods: MethodTable;
  /** Coercion table */
  coercions: CoercionTable;
  /** Statistics */
  stats: {
    methodHits: number;
    methodMisses: number;
    coercionHits: number;
    coercionMisses: number;
    synthesisCalls: number;
  };
  /** Creation timestamp */
  createdAt: number;
};

// ─────────────────────────────────────────────────────────────────
// Dispatch Result Types
// ─────────────────────────────────────────────────────────────────

/**
 * DispatchResult: Result of attempting to dispatch a generic operation.
 */
export type DispatchResult =
  | { tag: "found"; method: MethodEntry }
  | { tag: "coerced"; path: CoercionPath; method: MethodEntry }
  | { tag: "miss"; miss: GenericMissPayload }
  | { tag: "ambiguous"; paths: CoercionPath[] }
  | { tag: "error"; message: string };

/**
 * CoercionPath: A sequence of coercions to apply.
 */
export type CoercionPath = {
  /** Steps in the coercion path */
  steps: CoercionEntry[];
  /** Total cost of the path */
  totalCost: number;
  /** Indices of arguments that need coercion */
  argIndices: number[];
  /** Target signature after coercion */
  targetSig: TypeSignature;
};

/**
 * GenericMissPayload: Information about a failed dispatch for synthesis.
 */
export type GenericMissPayload = {
  /** The operation that missed */
  op: string;
  /** The type signature */
  signature: TypeSignature;
  /** Preview of arguments (possibly redacted) */
  argsPreview: Val[];
  /** Registry reference */
  registryId: string;
  /** Environment hash for context */
  envHash?: Hash;
  /** Profile name */
  profileName?: string;
  /** Available budget */
  budgets?: {
    inferOps?: number;
    testRuns?: number;
  };
};

// ─────────────────────────────────────────────────────────────────
// Synthesis Types
// ─────────────────────────────────────────────────────────────────

/**
 * SynthesisCandidate: A candidate method or coercion from inference.
 */
export type SynthesisCandidate = {
  /** Candidate ID */
  id: string;
  /** The candidate procedure */
  proc: Val;
  /** Confidence score (0-1) */
  confidence: number;
  /** Description of the approach */
  description?: string;
  /** Whether this is a method or coercion */
  kind: "method" | "coercion";
  /** For coercion: target type */
  targetType?: TypeTag;
};

/**
 * SynthesisResult: Result of attempting to synthesize a method.
 */
export type SynthesisResult =
  | { tag: "success"; candidate: SynthesisCandidate; testsPassed: boolean }
  | { tag: "failure"; reason: string; candidates?: SynthesisCandidate[] }
  | { tag: "pending"; ivarId: string };

/**
 * CommitRequest: Request to commit a synthesized method or coercion.
 */
export type CommitRequest = {
  /** Type of commit */
  kind: "method" | "coercion";
  /** For method: operation name */
  op?: string;
  /** For method: signature */
  signature?: TypeSignature;
  /** For coercion: from type */
  fromTag?: TypeTag;
  /** For coercion: to type */
  toTag?: TypeTag;
  /** The procedure to commit */
  proc: Val;
  /** Required obligations (tests that must pass) */
  obligations: ObligationRef[];
  /** Registry to commit to */
  registryId: string;
};

/**
 * ObligationRef: Reference to an obligation that must be satisfied.
 */
export type ObligationRef = {
  /** Obligation ID */
  id: string;
  /** Kind of obligation */
  kind: "test" | "invariant" | "metamorphic";
  /** Description */
  description?: string;
  /** Whether satisfied */
  satisfied?: boolean;
};

/**
 * CommitResult: Result of a commit attempt.
 */
export type CommitResult =
  | { tag: "committed"; hash: Hash; entry: MethodEntry | CoercionEntry }
  | { tag: "denied"; reason: string; missingCaps?: string[] }
  | { tag: "failed"; failedObligations: ObligationRef[] };

// ─────────────────────────────────────────────────────────────────
// Event Types (for ledger)
// ─────────────────────────────────────────────────────────────────

/**
 * GenericEvent: Events for the generic operations ledger.
 */
export type GenericEvent =
  | { tag: "dispatch"; op: string; signature: TypeSignature; result: "hit" | "miss" | "coerced"; timestamp: number }
  | { tag: "miss"; op: string; signature: TypeSignature; registryId: string; timestamp: number }
  | { tag: "synthesis"; op: string; signature: TypeSignature; candidateCount: number; timestamp: number }
  | { tag: "commit"; kind: "method" | "coercion"; hash: Hash; timestamp: number }
  | { tag: "ambiguity"; op: string; signature: TypeSignature; pathCount: number; timestamp: number };

// ─────────────────────────────────────────────────────────────────
// Type Guards
// ─────────────────────────────────────────────────────────────────

/**
 * Check if a value is tagged.
 */
export function isTagged(v: Val): v is TaggedVal {
  return v.tag === "Tagged";
}

/**
 * Check if a value is a generic registry.
 */
export function isGenericRegistry(v: Val): v is GenericRegistryVal {
  return v.tag === "GenericRegistry";
}

/**
 * Check if a value is a generic miss.
 */
export function isGenericMiss(v: Val): v is GenericMissVal {
  return v.tag === "GenericMiss";
}

// ─────────────────────────────────────────────────────────────────
// Tagged Value Helpers
// ─────────────────────────────────────────────────────────────────

/**
 * Create a tagged value.
 */
export function attachTag(typeTag: TypeTag, payload: Val): TaggedVal {
  return { tag: "Tagged", typeTag, payload };
}

/**
 * Get the type tag of a value.
 * Returns the typeTag if tagged, or infers a primitive tag otherwise.
 */
export function getTypeTag(v: Val): TypeTag | undefined {
  if (v.tag === "Tagged") {
    return v.typeTag;
  }
  // Infer primitive tags
  switch (v.tag) {
    case "Num": return "Primitive/Num";
    case "Str": return "Primitive/Str";
    case "Bool": return "Primitive/Bool";
    case "Sym": return "Primitive/Sym";
    case "Pair": return "Primitive/Pair";
    case "Vector": return "Primitive/Vector";
    case "Map": return "Primitive/Map";
    default: return undefined;
  }
}

/**
 * Get the contents (payload) of a value.
 * Returns the payload if tagged, or the value itself otherwise.
 */
export function getContents(v: Val): Val {
  if (v.tag === "Tagged") {
    return v.payload;
  }
  return v;
}

/**
 * Extract signature from arguments.
 */
export function extractSignature(args: Val[]): TypeSignature {
  return args.map(arg => getTypeTag(arg) ?? "unknown");
}

// ─────────────────────────────────────────────────────────────────
// Registry Value Helpers
// ─────────────────────────────────────────────────────────────────

/**
 * Create a GenericRegistryVal.
 */
export function makeGenericRegistry(id: string, name?: string): GenericRegistryVal {
  return { tag: "GenericRegistry", id, name };
}

/**
 * Create a GenericMissVal.
 */
export function makeGenericMiss(
  op: string,
  signature: TypeSignature,
  argsPreview: Val[],
  registryId: string,
  profileName?: string
): GenericMissVal {
  return {
    tag: "GenericMiss",
    op,
    signature,
    argsPreview,
    registryId,
    profileName,
  };
}
