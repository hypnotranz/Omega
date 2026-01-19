// src/core/meta/index.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-18.md
// Prompt 18: Meta-circular Ω evaluator - Module exports

// ─────────────────────────────────────────────────────────────────
// Types
// ─────────────────────────────────────────────────────────────────

export type {
  // Expression types
  Omega0Expr,
  Omega0List,
  Omega0Symbol,
  // Value types
  Omega0Val,
  Omega0Pair,
  Omega0Closure,
  Omega0Prim,
  Omega0LLMProc,
  Omega0Meaning,
  // Environment types
  Omega0Frame,
  Omega0Env,
  // Evaluation types
  EvalTraceEntry,
  EvalContext,
} from "./types";

// ─────────────────────────────────────────────────────────────────
// Type Constructors and Predicates
// ─────────────────────────────────────────────────────────────────

export {
  // Symbol operations
  sym,
  isSymbol,
  symbolName,
  // Pair operations
  cons,
  isPair,
  car,
  cdr,
  isNull,
  isList,
  arrayToList,
  listToArray,
  listLength,
  // Closure operations
  makeClosure,
  isClosure,
  // Primitive operations
  makePrim,
  isPrim,
  // LLM procedure operations
  makeLLMProc,
  isLLMProc,
  // Meaning operations
  makeMeaning,
  isMeaning,
  // Environment operations
  emptyEnv,
  makeFrame,
  // Expression predicates
  isSelfEvaluating,
  isVariable,
  isTaggedList,
  isQuoted,
  isIf,
  isLambda,
  isBegin,
  isDefine,
  isSet,
  isApplication,
  // Context
  createEvalContext,
  // Display
  omega0ToString,
} from "./types";

// ─────────────────────────────────────────────────────────────────
// Environment Operations
// ─────────────────────────────────────────────────────────────────

export {
  lookup,
  tryLookup,
  isBound,
  extendEnv,
  defineVar,
  setVar,
  copyEnv,
  envNames,
  topFrameBindings,
  envSnapshot,
  storeEnv,
  getEnv,
  clearEnvStore,
} from "./env";

// ─────────────────────────────────────────────────────────────────
// Meta-Circular Evaluator (eval0/apply0)
// ─────────────────────────────────────────────────────────────────

export {
  isTruthy,
  eval0,
  apply0,
  evalProgram,
  evalString,
  parseSimpleSExpr,
} from "./eval0";

// ─────────────────────────────────────────────────────────────────
// Primitives and LLM Procedures
// ─────────────────────────────────────────────────────────────────

export type { LLMInferHandler } from "./primitives";

export {
  createBaseEnv,
  createLLMProc,
  createScriptedLLMProc,
  addLLMProcToEnv,
  createMakeLLMProcPrim,
} from "./primitives";

// ─────────────────────────────────────────────────────────────────
// Meta-Intensional Evaluator (int0)
// ─────────────────────────────────────────────────────────────────

export type {
  OracleReqEval,
  OracleReqApply,
  OracleRequest,
  OracleResponse,
  OracleHandler,
  IntContext,
} from "./int0";

export {
  createIntContext,
  int0,
  createFallbackOracle,
  createScriptedOracle,
  commitMeaning,
  isValidated,
  addObligation,
  obligationsSatisfied,
} from "./int0";

// ─────────────────────────────────────────────────────────────────
// DSL Definition and Interpretation
// ─────────────────────────────────────────────────────────────────

export type {
  SyntaxRule,
  SemanticRule,
  DSLDefinition,
  DSLInstance,
  SyntaxMatch,
  DSLAnalysis,
} from "./dsl";

export {
  defineDSL,
  addSyntax,
  addSemantics,
  addBinding,
  addPrimitive,
  instantiateDSL,
  createCalculatorDSL,
  createQueryDSL,
  createStateMachineDSL,
  composeDSLs,
  analyzeDSL,
  serializeDSL,
  documentDSL,
} from "./dsl";
