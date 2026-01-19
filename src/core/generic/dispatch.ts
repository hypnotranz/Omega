// src/core/generic/dispatch.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Prompt 14: Generic dispatch (apply-generic) implementation

import type { Val, GenericMissVal } from "../eval/values";
import { VUnit } from "../eval/values";
import {
  type TypeTag,
  type TypeSignature,
  type DispatchResult,
  type CoercionPath,
  type GenericMissPayload,
  type MethodEntry,
  extractSignature,
  getContents,
  isTagged,
  makeGenericMiss,
} from "./types";
import {
  getRegistry,
  lookupMethod,
  getMethodsForOp,
  logGenericEvent,
} from "./registry";
import {
  findSignatureCoercion,
  findReachableSignatures,
  detectSignatureAmbiguity,
  applyCoercionPath,
} from "./coercion";

// ─────────────────────────────────────────────────────────────────
// Dispatch Configuration
// ─────────────────────────────────────────────────────────────────

/**
 * DispatchConfig: Configuration for generic dispatch.
 */
export type DispatchConfig = {
  /** Maximum coercion depth */
  maxCoercionDepth: number;
  /** Whether to allow synthesized methods */
  allowSynthesis: boolean;
  /** Whether to reject on ambiguity */
  rejectAmbiguous: boolean;
  /** Profile name for miss payloads */
  profileName?: string;
  /** Budget limits */
  budgets?: {
    inferOps?: number;
    testRuns?: number;
  };
};

export const DEFAULT_DISPATCH_CONFIG: DispatchConfig = {
  maxCoercionDepth: 5,
  allowSynthesis: true,
  rejectAmbiguous: true,
};

// ─────────────────────────────────────────────────────────────────
// Core Dispatch
// ─────────────────────────────────────────────────────────────────

/**
 * Resolve a generic dispatch: find the method to apply.
 *
 * Resolution order:
 * 1. Exact signature match
 * 2. Coercion path to matching signature
 * 3. Return miss for synthesis
 */
export function resolveDispatch(
  registryId: string,
  op: string,
  args: Val[],
  config: Partial<DispatchConfig> = {}
): DispatchResult {
  const fullConfig = { ...DEFAULT_DISPATCH_CONFIG, ...config };
  const signature = extractSignature(args);

  // Log dispatch attempt
  logGenericEvent({
    tag: "dispatch",
    op,
    signature,
    result: "miss", // Will be updated if found
    timestamp: Date.now(),
  });

  // 1. Try exact match
  const exactMethod = lookupMethod(registryId, op, signature);
  if (exactMethod) {
    return { tag: "found", method: exactMethod };
  }

  // 2. Try coercion
  const coercionResult = tryCoercionDispatch(
    registryId,
    op,
    signature,
    fullConfig
  );
  if (coercionResult) {
    if (coercionResult.tag === "ambiguous" && fullConfig.rejectAmbiguous) {
      return coercionResult;
    }
    if (coercionResult.tag === "coerced") {
      return coercionResult;
    }
  }

  // 3. Return miss
  const missPayload: GenericMissPayload = {
    op,
    signature,
    argsPreview: args.map(redactArgForMiss),
    registryId,
    profileName: fullConfig.profileName,
    budgets: fullConfig.budgets,
  };

  logGenericEvent({
    tag: "miss",
    op,
    signature,
    registryId,
    timestamp: Date.now(),
  });

  return { tag: "miss", miss: missPayload };
}

/**
 * Try to find a method via coercion.
 */
function tryCoercionDispatch(
  registryId: string,
  op: string,
  sourceSig: TypeSignature,
  config: DispatchConfig
): DispatchResult | undefined {
  // Get all methods for this operation
  const methods = getMethodsForOp(registryId, op);
  if (methods.length === 0) {
    return undefined;
  }

  // For each method, check if we can coerce to its signature
  const candidates: Array<{ method: MethodEntry; path: CoercionPath }> = [];

  for (const method of methods) {
    const path = findSignatureCoercion(
      registryId,
      sourceSig,
      method.signature,
      config.maxCoercionDepth
    );

    if (path) {
      candidates.push({ method, path });
    }
  }

  if (candidates.length === 0) {
    return undefined;
  }

  // Sort by cost
  candidates.sort((a, b) => a.path.totalCost - b.path.totalCost);

  // Check for ambiguity (multiple candidates with same min cost)
  const minCost = candidates[0].path.totalCost;
  const minCostCandidates = candidates.filter(c => c.path.totalCost === minCost);

  if (minCostCandidates.length > 1) {
    return {
      tag: "ambiguous",
      paths: minCostCandidates.map(c => c.path),
    };
  }

  return {
    tag: "coerced",
    path: candidates[0].path,
    method: candidates[0].method,
  };
}

/**
 * Redact an argument for inclusion in a miss payload.
 */
function redactArgForMiss(arg: Val): Val {
  // For now, just include type info, not full content
  if (isTagged(arg)) {
    return {
      tag: "Map",
      entries: [
        [{ tag: "Sym", name: "typeTag" }, { tag: "Str", s: arg.typeTag }],
        [{ tag: "Sym", name: "redacted" }, { tag: "Bool", b: true }],
      ],
    };
  }
  // For primitives, include limited info
  switch (arg.tag) {
    case "Str":
      return {
        tag: "Map",
        entries: [
          [{ tag: "Sym", name: "type" }, { tag: "Str", s: "string" }],
          [{ tag: "Sym", name: "length" }, { tag: "Num", n: arg.s.length }],
        ],
      };
    case "Num":
      return arg; // Numbers are safe
    case "Bool":
      return arg; // Booleans are safe
    default:
      return { tag: "Sym", name: arg.tag };
  }
}

// ─────────────────────────────────────────────────────────────────
// Apply Generic
// ─────────────────────────────────────────────────────────────────

/**
 * ApplyResult: Result of applying a generic operation.
 */
export type ApplyResult =
  | { tag: "success"; value: Val }
  | { tag: "miss"; miss: GenericMissVal }
  | { tag: "ambiguous"; paths: CoercionPath[] }
  | { tag: "error"; message: string };

/**
 * Apply a generic operation to arguments.
 *
 * This is the main entry point for generic dispatch.
 */
export function applyGeneric(
  registryId: string,
  op: string,
  args: Val[],
  applyFn: (proc: Val, args: Val[]) => Val,
  config: Partial<DispatchConfig> = {}
): ApplyResult {
  const fullConfig = { ...DEFAULT_DISPATCH_CONFIG, ...config };

  // Resolve dispatch
  const dispatchResult = resolveDispatch(registryId, op, args, fullConfig);

  switch (dispatchResult.tag) {
    case "found": {
      // Direct method application
      try {
        // Extract contents from tagged values
        const unwrappedArgs = args.map(getContents);
        const value = applyFn(dispatchResult.method.proc, unwrappedArgs);
        return { tag: "success", value };
      } catch (error) {
        return {
          tag: "error",
          message: error instanceof Error ? error.message : String(error),
        };
      }
    }

    case "coerced": {
      // Apply coercions then method
      try {
        const coercedArgs = [...args];

        // Apply coercions to relevant arguments
        for (let i = 0; i < args.length; i++) {
          if (dispatchResult.path.argIndices.includes(i)) {
            // Find the coercion steps for this argument
            const argCoercions = dispatchResult.path.steps.filter(
              step => step.fromTag === extractSignature([args[i]])[0]
            );

            if (argCoercions.length > 0) {
              let current = getContents(args[i]);
              for (const step of argCoercions) {
                current = applyFn(step.proc, [current]);
              }
              coercedArgs[i] = current;
            }
          }
        }

        // Apply the method
        const unwrappedArgs = coercedArgs.map(getContents);
        const value = applyFn(dispatchResult.method.proc, unwrappedArgs);
        return { tag: "success", value };
      } catch (error) {
        return {
          tag: "error",
          message: error instanceof Error ? error.message : String(error),
        };
      }
    }

    case "miss": {
      const signature = extractSignature(args);
      const missVal = makeGenericMiss(
        op,
        signature,
        args.map(redactArgForMiss),
        registryId,
        fullConfig.profileName
      );
      return { tag: "miss", miss: missVal };
    }

    case "ambiguous": {
      return { tag: "ambiguous", paths: dispatchResult.paths };
    }

    case "error": {
      return { tag: "error", message: dispatchResult.message };
    }
  }
}

// ─────────────────────────────────────────────────────────────────
// Dispatch with Miss Handler
// ─────────────────────────────────────────────────────────────────

/**
 * MissHandler: Handler for generic misses.
 */
export type MissHandler = (miss: GenericMissPayload) => Promise<MissHandlerResult>;

/**
 * MissHandlerResult: Result of handling a miss.
 */
export type MissHandlerResult =
  | { tag: "resolved"; proc: Val; commit: boolean }
  | { tag: "failed"; reason: string };

/**
 * Apply generic with miss handling.
 */
export async function applyGenericWithMissHandler(
  registryId: string,
  op: string,
  args: Val[],
  applyFn: (proc: Val, args: Val[]) => Val,
  missHandler: MissHandler,
  config: Partial<DispatchConfig> = {}
): Promise<ApplyResult> {
  // First try normal dispatch
  const result = applyGeneric(registryId, op, args, applyFn, config);

  if (result.tag !== "miss") {
    return result;
  }

  // Handle the miss
  const missPayload: GenericMissPayload = {
    op,
    signature: extractSignature(args),
    argsPreview: args.map(redactArgForMiss),
    registryId,
    profileName: config.profileName,
    budgets: config.budgets,
  };

  const handlerResult = await missHandler(missPayload);

  if (handlerResult.tag === "failed") {
    return { tag: "error", message: handlerResult.reason };
  }

  // Apply the resolved procedure
  try {
    const unwrappedArgs = args.map(getContents);
    const value = applyFn(handlerResult.proc, unwrappedArgs);
    return { tag: "success", value };
  } catch (error) {
    return {
      tag: "error",
      message: error instanceof Error ? error.message : String(error),
    };
  }
}

// ─────────────────────────────────────────────────────────────────
// Introspection
// ─────────────────────────────────────────────────────────────────

/**
 * Get dispatch info without actually dispatching.
 */
export function getDispatchInfo(
  registryId: string,
  op: string,
  signature: TypeSignature
): {
  exactMatch: boolean;
  coercionAvailable: boolean;
  candidateCount: number;
  ambiguous: boolean;
} {
  const exactMethod = lookupMethod(registryId, op, signature);
  if (exactMethod) {
    return {
      exactMatch: true,
      coercionAvailable: false,
      candidateCount: 1,
      ambiguous: false,
    };
  }

  const methods = getMethodsForOp(registryId, op);
  let candidateCount = 0;
  let ambiguousPaths = 0;

  for (const method of methods) {
    const path = findSignatureCoercion(registryId, signature, method.signature);
    if (path) {
      candidateCount++;
    }
  }

  return {
    exactMatch: false,
    coercionAvailable: candidateCount > 0,
    candidateCount,
    ambiguous: candidateCount > 1,
  };
}

/**
 * List all applicable methods for a signature (including via coercion).
 */
export function listApplicableMethods(
  registryId: string,
  op: string,
  signature: TypeSignature
): Array<{ method: MethodEntry; path?: CoercionPath }> {
  const results: Array<{ method: MethodEntry; path?: CoercionPath }> = [];

  // Check exact match
  const exactMethod = lookupMethod(registryId, op, signature);
  if (exactMethod) {
    results.push({ method: exactMethod });
  }

  // Check coerced matches
  const methods = getMethodsForOp(registryId, op);
  for (const method of methods) {
    if (method === exactMethod) continue;

    const path = findSignatureCoercion(registryId, signature, method.signature);
    if (path) {
      results.push({ method, path });
    }
  }

  return results;
}
