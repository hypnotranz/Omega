// src/core/compiler/pipeline.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-17.md
// Prompt 17: Compiler pipeline with obligation-carrying rewrites

import type { Expr } from "../ast";
import type { Val, IRVal } from "../eval/values";
import { sha256JSON } from "../artifacts/hash";
import type {
  ANFProgram,
  BytecodeProgram,
  IRProgram,
  ProgramArtifact,
  CompilerObligation,
  EvidenceRef,
  PassRecord,
  SourceMap,
  CompilerConfig,
  DEFAULT_COMPILER_CONFIG,
  OptimizationResult,
} from "./types";
import { toANF, createSourceMap, countBindings } from "./anf";
import { toBytecode, countInstructions } from "./bytecode";
import { optimizeANF, optimizeCSE, applyCSE, countOracleCalls } from "./optimize";

// ─────────────────────────────────────────────────────────────────
// Default Configuration
// ─────────────────────────────────────────────────────────────────

export const defaultCompilerConfig: CompilerConfig = {
  target: "anf",
  enableCSE: true,
  enableClosureConversion: false,
  enableDefunctionalization: false,
  enableTCO: true,
  maxInlineDepth: 3,
  profileName: "pragmatic",
  allowCompileTimeInference: false,
  debug: true,
};

// ─────────────────────────────────────────────────────────────────
// IR Store (content-addressed storage for IR programs)
// ─────────────────────────────────────────────────────────────────

/**
 * Global IR store - maps digest to IR program.
 */
const irStore = new Map<string, IRProgram>();

/**
 * Store an IR program and return its digest.
 */
export function storeIR(ir: IRProgram): string {
  const digest = sha256JSON(ir);
  irStore.set(digest, ir);
  return digest;
}

/**
 * Retrieve an IR program by digest.
 */
export function getIR(digest: string): IRProgram | undefined {
  return irStore.get(digest);
}

/**
 * Clear the IR store (for testing).
 */
export function clearIRStore(): void {
  irStore.clear();
}

// ─────────────────────────────────────────────────────────────────
// Compiler Pipeline
// ─────────────────────────────────────────────────────────────────

/**
 * Compilation result with IR and metadata.
 */
export type CompilationResult = {
  /** The compiled IR value */
  ir: IRVal;
  /** The stored IR program */
  program: IRProgram;
  /** Pass records */
  passes: PassRecord[];
  /** Obligations from optimization passes */
  obligations: CompilerObligation[];
  /** Optimization results */
  optimizations: OptimizationResult[];
  /** Source map */
  sourceMap: SourceMap;
};

/**
 * Compile an expression to IR.
 *
 * This is the main compiler entry point implementing the pipeline:
 * 1. Core AST → ANF (lowering)
 * 2. ANF optimization (CSE, DCE)
 * 3. ANF → Bytecode (optional, if target is bytecode)
 * 4. Verification obligations
 */
export function compile(
  expr: Expr,
  config: Partial<CompilerConfig> = {}
): CompilationResult {
  const cfg: CompilerConfig = { ...defaultCompilerConfig, ...config };
  const passes: PassRecord[] = [];
  const obligations: CompilerObligation[] = [];
  const optimizations: OptimizationResult[] = [];
  const sourceMapEntries: SourceMap["entries"] = [];

  const startTime = Date.now();

  // ─────────────────────────────────────────────────────────────
  // Phase 1: Lowering to ANF
  // ─────────────────────────────────────────────────────────────

  const anfProgram = toANF(expr, "source");

  passes.push({
    name: "toANF",
    inputForm: "core-ast",
    outputForm: "anf",
    timestamp: Date.now(),
    metrics: {
      bindings: countBindings(anfProgram.body),
      freeVars: anfProgram.freeVars.length,
    },
  });

  // ─────────────────────────────────────────────────────────────
  // Phase 2: Optimization (governed by profile)
  // ─────────────────────────────────────────────────────────────

  let optimizedANF = anfProgram;

  if (cfg.enableCSE) {
    // Check profile allows optimization
    const profileAllowsCSE = cfg.profileName !== "airgap";

    if (profileAllowsCSE) {
      const { program: cseProgram, results } = optimizeANF(anfProgram, {
        enableCSE: true,
        enableDCE: true,
      });

      optimizedANF = cseProgram;
      optimizations.push(...results);

      // Collect obligations from optimizations
      for (const result of results) {
        obligations.push(...result.obligations);
      }

      passes.push({
        name: "optimize",
        inputForm: "anf",
        outputForm: "anf",
        timestamp: Date.now(),
        metrics: {
          oracleCallsBefore: countOracleCalls(anfProgram),
          oracleCallsAfter: countOracleCalls(optimizedANF),
          bindingsBefore: countBindings(anfProgram.body),
          bindingsAfter: countBindings(optimizedANF.body),
        },
      });
    } else {
      // Profile forbids optimization - record why
      obligations.push({
        kind: "profile-compliance",
        description: `CSE optimization skipped: profile '${cfg.profileName}' forbids it`,
        status: "satisfied",
      });
    }
  }

  // ─────────────────────────────────────────────────────────────
  // Phase 3: Bytecode Generation (if requested)
  // ─────────────────────────────────────────────────────────────

  let irProgram: IRProgram;

  if (cfg.target === "bytecode") {
    const bytecodeProgram = toBytecode(optimizedANF);

    passes.push({
      name: "toBytecode",
      inputForm: "anf",
      outputForm: "bytecode",
      timestamp: Date.now(),
      metrics: {
        instructions: countInstructions(bytecodeProgram),
        functions: bytecodeProgram.functions.length,
      },
    });

    irProgram = { form: "bytecode", program: bytecodeProgram };
  } else {
    irProgram = { form: "anf", program: optimizedANF };
  }

  // ─────────────────────────────────────────────────────────────
  // Phase 4: Store and Create IR Value
  // ─────────────────────────────────────────────────────────────

  const digest = storeIR(irProgram);

  const sourceMap: SourceMap = {
    entries: sourceMapEntries,
  };

  const irVal: IRVal = {
    tag: "IR",
    form: cfg.target,
    digest,
    irRef: digest,
  };

  // Add verification obligations
  obligations.push({
    kind: "differential-test",
    description: "Compiled code must produce same output as interpreter",
    status: "pending",
  });

  obligations.push({
    kind: "effect-preservation",
    description: "Effect emissions must be preserved in compiled code",
    status: "pending",
  });

  return {
    ir: irVal,
    program: irProgram,
    passes,
    obligations,
    optimizations,
    sourceMap,
  };
}

// ─────────────────────────────────────────────────────────────────
// Artifact Creation
// ─────────────────────────────────────────────────────────────────

/**
 * Create a program artifact from an expression.
 */
export function createArtifact(
  expr: Expr,
  config: Partial<CompilerConfig> = {}
): ProgramArtifact {
  const result = compile(expr, config);

  const payload = result.program.form === "anf"
    ? result.program.program
    : result.program.program;

  return {
    form: result.program.form === "anf" ? "anf" : "bytecode",
    payload,
    sourceMap: result.sourceMap,
    obligations: result.obligations,
    evidence: [],
    digest: result.ir.digest,
    passHistory: result.passes,
  };
}

/**
 * Record evidence for a satisfied obligation.
 */
export function recordEvidence(
  artifact: ProgramArtifact,
  obligationIndex: number,
  evidence: EvidenceRef
): ProgramArtifact {
  const obligations = [...artifact.obligations];

  if (obligationIndex >= 0 && obligationIndex < obligations.length) {
    obligations[obligationIndex] = {
      ...obligations[obligationIndex],
      status: "satisfied",
      evidence: evidence.ref,
    };
  }

  return {
    ...artifact,
    obligations,
    evidence: [...artifact.evidence, evidence],
  };
}

/**
 * Check if all obligations are satisfied.
 */
export function allObligationsSatisfied(artifact: ProgramArtifact): boolean {
  return artifact.obligations.every(o => o.status === "satisfied");
}

/**
 * Get pending obligations.
 */
export function getPendingObligations(
  artifact: ProgramArtifact
): CompilerObligation[] {
  return artifact.obligations.filter(o => o.status === "pending");
}

// ─────────────────────────────────────────────────────────────────
// Profile-Governed Compilation
// ─────────────────────────────────────────────────────────────────

/**
 * Profile configuration for compilation.
 */
export type CompileProfile = {
  name: string;
  allowCSE: boolean;
  allowCompileTimeInference: boolean;
  requireDifferentialTest: boolean;
  maxOptimizationPasses: number;
};

/**
 * Built-in compile profiles.
 */
export const COMPILE_PROFILES: Record<string, CompileProfile> = {
  strict: {
    name: "strict",
    allowCSE: false,
    allowCompileTimeInference: false,
    requireDifferentialTest: true,
    maxOptimizationPasses: 0,
  },
  pragmatic: {
    name: "pragmatic",
    allowCSE: true,
    allowCompileTimeInference: false,
    requireDifferentialTest: true,
    maxOptimizationPasses: 10,
  },
  explore: {
    name: "explore",
    allowCSE: true,
    allowCompileTimeInference: true,
    requireDifferentialTest: false,
    maxOptimizationPasses: 20,
  },
  airgap: {
    name: "airgap",
    allowCSE: false,
    allowCompileTimeInference: false,
    requireDifferentialTest: true,
    maxOptimizationPasses: 0,
  },
};

/**
 * Compile with profile governance.
 */
export function compileWithProfile(
  expr: Expr,
  profileName: string,
  additionalConfig: Partial<CompilerConfig> = {}
): CompilationResult {
  const profile = COMPILE_PROFILES[profileName] ?? COMPILE_PROFILES.pragmatic;

  const config: Partial<CompilerConfig> = {
    ...additionalConfig,
    profileName: profile.name,
    enableCSE: profile.allowCSE,
    allowCompileTimeInference: profile.allowCompileTimeInference,
  };

  const result = compile(expr, config);

  // Add profile-specific obligations
  if (profile.requireDifferentialTest) {
    const hasTest = result.obligations.some(o => o.kind === "differential-test");
    if (!hasTest) {
      result.obligations.push({
        kind: "differential-test",
        description: `Profile '${profile.name}' requires differential testing`,
        status: "pending",
      });
    }
  }

  return result;
}

// ─────────────────────────────────────────────────────────────────
// IR Inspection (for ReqObserve)
// ─────────────────────────────────────────────────────────────────

/**
 * Get IR summary for observation.
 */
export function getIRSummary(digest: string): object | undefined {
  const ir = getIR(digest);
  if (!ir) return undefined;

  if (ir.form === "anf") {
    const anf = ir.program as ANFProgram;
    return {
      form: "anf",
      bindings: countBindings(anf.body),
      freeVars: anf.freeVars,
      oracleCalls: countOracleCalls(anf),
    };
  } else {
    const bc = ir.program as BytecodeProgram;
    return {
      form: "bytecode",
      instructions: countInstructions(bc),
      functions: bc.functions.length,
      constants: bc.constants.length,
      freeVars: bc.freeVars,
    };
  }
}

/**
 * Get source location for an IP in bytecode.
 */
export function getSourceLocationForIP(
  digest: string,
  fnId: number,
  ip: number
): object | undefined {
  const ir = getIR(digest);
  if (!ir || ir.form !== "bytecode") return undefined;

  const bc = ir.program as BytecodeProgram;
  const fn = bc.functions[fnId];
  if (!fn || !fn.sourceMap) return undefined;

  const entry = fn.sourceMap.entries.find(e => e.irPos === ip);
  return entry?.loc;
}

/**
 * Disassemble a function from IR.
 */
export function disassembleFromDigest(
  digest: string,
  fnId: number
): string | undefined {
  const ir = getIR(digest);
  if (!ir || ir.form !== "bytecode") return undefined;

  const bc = ir.program as BytecodeProgram;
  const fn = bc.functions[fnId];
  if (!fn) return undefined;

  // Use bytecode disassembler
  const { disassemble } = require("./bytecode");
  return disassemble(fn);
}
