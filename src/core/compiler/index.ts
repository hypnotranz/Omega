// src/core/compiler/index.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-17.md
// Prompt 17: Compiler Pipeline - Module exports

// ─────────────────────────────────────────────────────────────────
// Types
// ─────────────────────────────────────────────────────────────────

export type {
  // Source mapping
  SourceLocation,
  SourceMapEntry,
  SourceMap,
  // ANF types
  ANFAtom,
  ANFPrim,
  ANFExpr,
  ANFProgram,
  ANFPattern,
  ANFHandler,
  ANFMatchClause,
  // Bytecode types
  Instr,
  BytecodeFunction,
  BytecodeHandler,
  BytecodeProgram,
  // IR union
  IRProgram,
  // Pipeline types
  ObligationKind,
  CompilerObligation,
  EvidenceRef,
  ProgramArtifact,
  PassRecord,
  CompilerPass,
  // Testing types
  EffectTraceEntry,
  DifferentialReport,
  // Optimization types
  CSECandidate,
  OptimizationResult,
  // Configuration
  CompilerConfig,
  VMConfig,
  VMState,
  VMFrame,
} from "./types";

export {
  DEFAULT_COMPILER_CONFIG,
  DEFAULT_VM_CONFIG,
} from "./types";

// ─────────────────────────────────────────────────────────────────
// ANF Conversion
// ─────────────────────────────────────────────────────────────────

export {
  toANF,
  createSourceMap,
  countBindings,
  countEffects,
  findEffectOps,
  anfToString,
} from "./anf";

// ─────────────────────────────────────────────────────────────────
// Bytecode Generation
// ─────────────────────────────────────────────────────────────────

export {
  toBytecode,
  countInstructions,
  countEffectInstructions,
  findEffectOpsInBytecode,
  disassemble,
  disassembleProgram,
} from "./bytecode";

// ─────────────────────────────────────────────────────────────────
// VM Execution
// ─────────────────────────────────────────────────────────────────

export type { EffectHandler, EffectHandlerRegistry } from "./vm";

export {
  defaultVMConfig,
  createDefaultHandlers,
  createVMState,
  step,
  run,
  getResult,
  getSourceLocation,
  getCallStack,
  setBreakpoint,
  atBreakpoint,
} from "./vm";

// ─────────────────────────────────────────────────────────────────
// Optimization Passes
// ─────────────────────────────────────────────────────────────────

export {
  findCSECandidates,
  applyCSE,
  countOracleCalls,
  optimizeCSE,
  eliminateDeadCode,
  optimizeANF,
} from "./optimize";

// ─────────────────────────────────────────────────────────────────
// Compiler Pipeline
// ─────────────────────────────────────────────────────────────────

export type { CompilationResult, CompileProfile } from "./pipeline";

export {
  defaultCompilerConfig,
  storeIR,
  getIR,
  clearIRStore,
  compile,
  createArtifact,
  recordEvidence,
  allObligationsSatisfied,
  getPendingObligations,
  COMPILE_PROFILES,
  compileWithProfile,
  getIRSummary,
  getSourceLocationForIP,
  disassembleFromDigest,
} from "./pipeline";

// ─────────────────────────────────────────────────────────────────
// Differential Testing
// ─────────────────────────────────────────────────────────────────

export type { InterpreterState } from "./differential";

export {
  interpretANF,
  differentialRun,
  compareEffectTraces,
  createTestEvidence,
  satisfyDifferentialObligation,
  metamorphicTest,
  runDifferentialTestSuite,
} from "./differential";
