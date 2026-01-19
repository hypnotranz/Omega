// src/core/compiler/types.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-17.md
// Prompt 17: Compiler Pipeline - Type definitions

import type { Val } from "../eval/values";
import type { Expr } from "../ast";
import type { Hash } from "../artifacts/hash";

// ─────────────────────────────────────────────────────────────────
// Source Location and Source Maps
// ─────────────────────────────────────────────────────────────────

/**
 * Source location in the original program.
 */
export type SourceLocation = {
  /** Source file or expression identifier */
  source: string;
  /** Line number (1-based) */
  line: number;
  /** Column number (1-based) */
  column: number;
  /** Optional end position */
  endLine?: number;
  endColumn?: number;
};

/**
 * Source map entry mapping IR position to source location.
 */
export type SourceMapEntry = {
  /** IR instruction index or ANF binding name */
  irPos: number | string;
  /** Source location */
  loc: SourceLocation;
  /** Lexical scope/frame name for debugging */
  scope?: string;
  /** Local variable names in scope at this point */
  locals?: string[];
};

/**
 * Source map for an IR program.
 */
export type SourceMap = {
  /** Entries mapping IR positions to source */
  entries: SourceMapEntry[];
  /** Original source text (optional, for display) */
  originalSource?: string;
};

// ─────────────────────────────────────────────────────────────────
// ANF (A-Normal Form) IR
// ─────────────────────────────────────────────────────────────────

/**
 * ANF atomic value - no computation, just a reference.
 */
export type ANFAtom =
  | { tag: "Lit"; v: Val }
  | { tag: "Var"; name: string };

/**
 * ANF primitive operation - the right-hand side of a let binding.
 */
export type ANFPrim =
  | { tag: "Lambda"; params: string[]; body: ANFExpr; label?: string }
  | { tag: "Call"; fn: ANFAtom; args: ANFAtom[] }
  | { tag: "Effect"; op: string; args: ANFAtom[] }    // Preserved effect emission!
  | { tag: "Quote"; datum: unknown }
  | { tag: "Prim"; name: string; args: ANFAtom[] }    // Built-in ops: +, -, etc.
  | { tag: "Match"; scrut: ANFAtom; clauses: ANFMatchClause[] }
  | { tag: "Handle"; body: ANFExpr; handler: ANFHandler }
  | { tag: "MakeClosure"; lambda: ANFPrim; captured: string[] };  // After closure conversion

/**
 * ANF match clause.
 */
export type ANFMatchClause = {
  pat: ANFPattern;
  body: ANFExpr;
};

/**
 * ANF pattern for match expressions.
 */
export type ANFPattern =
  | { tag: "PWild" }
  | { tag: "PVar"; name: string }
  | { tag: "PLit"; v: Val }
  | { tag: "PVector"; items: ANFPattern[] };

/**
 * ANF handler (for Handle expressions).
 */
export type ANFHandler = {
  on: Array<{ op: string; params: string[]; k: string; body: ANFExpr }>;
  ret?: { v: string; body: ANFExpr };
  fin?: { body: ANFExpr };
};

/**
 * ANF expression - either a let binding, conditional, or return.
 */
export type ANFExpr =
  | { tag: "Let"; name: string; rhs: ANFPrim; body: ANFExpr; loc?: SourceLocation }
  | { tag: "LetRec"; bindings: Array<{ name: string; rhs: ANFPrim }>; body: ANFExpr; loc?: SourceLocation }
  | { tag: "If"; test: ANFAtom; thn: ANFExpr; els: ANFExpr; loc?: SourceLocation }
  | { tag: "Seq"; first: ANFExpr; second: ANFExpr; loc?: SourceLocation }  // For begin sequences
  | { tag: "Set"; name: string; rhs: ANFAtom; body: ANFExpr; loc?: SourceLocation }
  | { tag: "Return"; v: ANFAtom; loc?: SourceLocation };

/**
 * ANF program - a complete compiled unit.
 */
export type ANFProgram = {
  /** Top-level expression */
  body: ANFExpr;
  /** Top-level definitions (for modules) */
  defs?: Map<string, ANFPrim>;
  /** Free variables required from environment */
  freeVars: string[];
};

// ─────────────────────────────────────────────────────────────────
// Bytecode IR (Stack-based VM)
// ─────────────────────────────────────────────────────────────────

/**
 * Bytecode instruction.
 */
export type Instr =
  | { op: "CONST"; k: number }                    // Push constant pool[k]
  | { op: "LOAD"; slot: number }                  // Push locals[slot]
  | { op: "STORE"; slot: number }                 // Pop to locals[slot]
  | { op: "GLOAD"; name: string }                 // Load global/free var
  | { op: "GSTORE"; name: string }                // Store global/free var
  | { op: "CLOSURE"; fnId: number; captured: number[] }  // Create closure from fn pool
  | { op: "CALL"; argc: number }                  // Call fn with argc args
  | { op: "TAILCALL"; argc: number }              // Tail call optimization
  | { op: "RET" }                                 // Return top of stack
  | { op: "POP" }                                 // Discard top of stack
  | { op: "DUP" }                                 // Duplicate top of stack
  | { op: "JMP"; label: number }                  // Unconditional jump
  | { op: "JMPIF"; labelTrue: number; labelFalse: number }  // Conditional jump
  | { op: "EFFECT"; opName: string; argc: number }  // Emit algebraic effect!
  | { op: "HANDLE"; handlerId: number }           // Begin handler region
  | { op: "UNHANDLE" }                            // End handler region
  | { op: "PRIM"; name: string; argc: number }    // Primitive operation
  | { op: "MATCH"; clauseLabels: number[] }       // Pattern match dispatch
  | { op: "FAIL"; reasonK: number }               // Fail with reason from constant pool
  | { op: "NOP" }                                 // No operation (for alignment)
  | { op: "DEBUG"; info: string };                // Debug breakpoint/info

/**
 * Bytecode function.
 */
export type BytecodeFunction = {
  /** Function identifier */
  id: number;
  /** Number of parameters */
  arity: number;
  /** Number of local variable slots */
  localCount: number;
  /** Instruction sequence */
  code: Instr[];
  /** Source map for this function */
  sourceMap?: SourceMap;
  /** Label for debugging */
  label?: string;
};

/**
 * Bytecode handler definition.
 */
export type BytecodeHandler = {
  id: number;
  ops: Map<string, { paramSlots: number[]; kSlot: number; codeStart: number }>;
  retSlot?: number;
  retCodeStart?: number;
  finCodeStart?: number;
};

/**
 * Bytecode program - a complete compiled unit.
 */
export type BytecodeProgram = {
  /** Constant pool */
  constants: Val[];
  /** Function pool */
  functions: BytecodeFunction[];
  /** Handler definitions */
  handlers: BytecodeHandler[];
  /** Entry point function ID */
  entryFn: number;
  /** Free variables required from environment */
  freeVars: string[];
  /** Global source map */
  sourceMap: SourceMap;
};

// ─────────────────────────────────────────────────────────────────
// IR Program (Union of ANF and Bytecode)
// ─────────────────────────────────────────────────────────────────

export type IRProgram =
  | { form: "anf"; program: ANFProgram }
  | { form: "bytecode"; program: BytecodeProgram };

// ─────────────────────────────────────────────────────────────────
// Compiler Pipeline Types
// ─────────────────────────────────────────────────────────────────

/**
 * Obligation kind for compiler passes.
 */
export type ObligationKind =
  | "differential-test"    // Must pass differential test against interpreter
  | "metamorphic-test"     // Must pass metamorphic invariant tests
  | "effect-preservation"  // Must preserve effect emissions
  | "ledger-equivalence"   // Must produce equivalent ledger events
  | "profile-compliance";  // Must respect profile constraints

/**
 * Compiler obligation - a proof requirement for a pass.
 */
export type CompilerObligation = {
  kind: ObligationKind;
  description: string;
  /** Status: pending, satisfied, failed */
  status: "pending" | "satisfied" | "failed";
  /** Evidence reference (e.g., test run receipt) */
  evidence?: string;
  /** Error message if failed */
  error?: string;
};

/**
 * Evidence reference for satisfied obligations.
 */
export type EvidenceRef = {
  /** Type of evidence */
  kind: "test-run" | "proof" | "manual-review";
  /** Reference to the evidence (receipt hash, proof hash, etc.) */
  ref: string;
  /** Timestamp */
  timestamp: number;
};

/**
 * Program artifact - output of a compiler pass.
 */
export type ProgramArtifact = {
  /** Form of the artifact */
  form: "core-ast" | "anf" | "bytecode";
  /** The payload (AST, ANF program, or bytecode program) */
  payload: Expr | ANFProgram | BytecodeProgram;
  /** Source map */
  sourceMap: SourceMap;
  /** Attached obligations */
  obligations: CompilerObligation[];
  /** Evidence for satisfied obligations */
  evidence: EvidenceRef[];
  /** Content-addressed digest */
  digest: Hash;
  /** Pass history */
  passHistory: PassRecord[];
};

/**
 * Record of a compiler pass application.
 */
export type PassRecord = {
  /** Pass name */
  name: string;
  /** Input form */
  inputForm: string;
  /** Output form */
  outputForm: string;
  /** Timestamp */
  timestamp: number;
  /** Metrics (e.g., oracle call reduction) */
  metrics?: Record<string, number>;
};

/**
 * Compiler pass function signature.
 */
export type CompilerPass = (artifact: ProgramArtifact) => ProgramArtifact;

// ─────────────────────────────────────────────────────────────────
// Differential Testing Types
// ─────────────────────────────────────────────────────────────────

/**
 * Effect trace entry - records an effect emission.
 */
export type EffectTraceEntry = {
  /** Effect operation name */
  op: string;
  /** Arguments (hashed for comparison) */
  argsDigest: Hash;
  /** Timestamp/ordering */
  seq: number;
  /** Result (if available) */
  result?: Val;
};

/**
 * Differential test report.
 */
export type DifferentialReport = {
  /** Did outputs match? */
  outputsMatch: boolean;
  /** Did effect traces match? */
  effectsMatch: boolean;
  /** Did oracle consumption match? */
  oracleMatch: boolean;
  /** Interpreter output */
  interpOutput: Val;
  /** Compiled output */
  compiledOutput: Val;
  /** Interpreter effect trace */
  interpEffects: EffectTraceEntry[];
  /** Compiled effect trace */
  compiledEffects: EffectTraceEntry[];
  /** Interpreter oracle calls */
  interpOracleCalls: number;
  /** Compiled oracle calls */
  compiledOracleCalls: number;
  /** Any mismatches found */
  mismatches: string[];
  /** Overall pass/fail */
  passed: boolean;
};

// ─────────────────────────────────────────────────────────────────
// Optimization Types
// ─────────────────────────────────────────────────────────────────

/**
 * CSE (Common Subexpression Elimination) candidate.
 */
export type CSECandidate = {
  /** Expression being duplicated */
  exprDigest: Hash;
  /** Locations of duplicates */
  locations: string[];
  /** Estimated oracle call savings */
  estimatedSaving: number;
  /** Whether it's safe to eliminate (pure or idempotent) */
  safe: boolean;
};

/**
 * Optimization result with before/after metrics.
 */
export type OptimizationResult = {
  /** Pass name */
  passName: string;
  /** Before metrics */
  before: { oracleCalls: number; bindings: number; instructions?: number };
  /** After metrics */
  after: { oracleCalls: number; bindings: number; instructions?: number };
  /** Candidates found and applied */
  candidates: CSECandidate[];
  /** Obligations generated */
  obligations: CompilerObligation[];
};

// ─────────────────────────────────────────────────────────────────
// Compiler Configuration
// ─────────────────────────────────────────────────────────────────

/**
 * Compiler configuration options.
 */
export type CompilerConfig = {
  /** Target IR form */
  target: "anf" | "bytecode";
  /** Enable CSE optimization */
  enableCSE: boolean;
  /** Enable closure conversion */
  enableClosureConversion: boolean;
  /** Enable defunctionalization */
  enableDefunctionalization: boolean;
  /** Enable tail call optimization */
  enableTCO: boolean;
  /** Maximum inlining depth */
  maxInlineDepth: number;
  /** Profile for governance constraints */
  profileName: string;
  /** Whether compile-time inference is allowed */
  allowCompileTimeInference: boolean;
  /** Debug mode (include extra source map info) */
  debug: boolean;
};

/**
 * Default compiler configuration.
 */
export const DEFAULT_COMPILER_CONFIG: CompilerConfig = {
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
// VM State Types
// ─────────────────────────────────────────────────────────────────

/**
 * VM stack frame.
 */
export type VMFrame = {
  /** Function ID being executed */
  fnId: number;
  /** Instruction pointer */
  ip: number;
  /** Local variable slots */
  locals: Val[];
  /** Return address (fnId, ip) */
  returnAddr?: { fnId: number; ip: number };
  /** Source location for debugging */
  sourceLoc?: SourceLocation;
};

/**
 * VM execution state.
 */
export type VMState = {
  /** Value stack */
  stack: Val[];
  /** Call stack (frames) */
  frames: VMFrame[];
  /** Current frame index */
  currentFrame: number;
  /** Handler stack */
  handlerStack: { handlerId: number; frameIndex: number }[];
  /** Global environment */
  globals: Map<string, Val>;
  /** Effect trace */
  effectTrace: EffectTraceEntry[];
  /** Oracle call count */
  oracleCallCount: number;
  /** Execution status */
  status: "running" | "paused" | "completed" | "error";
  /** Error message if status is error */
  error?: string;
  /** Fuel/operation limit remaining */
  fuel: number;
};

/**
 * VM configuration.
 */
export type VMConfig = {
  /** Maximum operation count (fuel) */
  maxOperations: number;
  /** Maximum stack depth */
  maxStackDepth: number;
  /** Maximum call depth */
  maxCallDepth: number;
  /** Enable stepping mode */
  stepping: boolean;
  /** Breakpoints (by instruction index) */
  breakpoints: Set<number>;
};

/**
 * Default VM configuration.
 */
export const DEFAULT_VM_CONFIG: VMConfig = {
  maxOperations: 50,  // Low default for safety
  maxStackDepth: 1000,
  maxCallDepth: 100,
  stepping: false,
  breakpoints: new Set(),
};
