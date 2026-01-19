// src/core/meta/int0.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-18.md
// Prompt 18: Meta-intensional evaluator - int0

import type {
  Omega0Expr,
  Omega0Val,
  Omega0Env,
  Omega0Meaning,
  Omega0Closure,
  Omega0LLMProc,
  EvalContext,
} from "./types";
import {
  isSymbol,
  symbolName,
  isSelfEvaluating,
  isVariable,
  isQuoted,
  isIf,
  isLambda,
  isBegin,
  isDefine,
  isSet,
  isApplication,
  makeMeaning,
  makeClosure,
  isClosure,
  isPrim,
  isLLMProc,
  isMeaning,
  omega0ToString,
  createEvalContext,
} from "./types";
import { lookup, extendEnv, defineVar, storeEnv, getEnv } from "./env";
import { eval0, apply0 } from "./eval0";

// ─────────────────────────────────────────────────────────────────
// Oracle Protocol Types
// ─────────────────────────────────────────────────────────────────

/**
 * Oracle request for evaluation.
 */
export type OracleReqEval = {
  kind: "ReqEval";
  expr: Omega0Expr;
  envRef: string;
};

/**
 * Oracle request for application.
 */
export type OracleReqApply = {
  kind: "ReqApply";
  proc: Omega0Val;
  args: Omega0Val[];
};

/**
 * Oracle request union.
 */
export type OracleRequest = OracleReqEval | OracleReqApply;

/**
 * Oracle response with meaning.
 */
export type OracleResponse = {
  denotation: Omega0Val;
  confidence: number;
  evidence?: string;
  /** Optional requests for the meta-evaluator */
  requests?: OracleRequest[];
};

/**
 * Oracle handler type.
 */
export type OracleHandler = (
  expr: Omega0Expr,
  envRef: string,
  ctx: IntContext
) => OracleResponse;

// ─────────────────────────────────────────────────────────────────
// Intensional Evaluation Context
// ─────────────────────────────────────────────────────────────────

/**
 * Context for intensional evaluation.
 */
export type IntContext = {
  /** Oracle handler for inference */
  oracle: OracleHandler;
  /** Evaluation context for extensional fallback */
  evalCtx: EvalContext;
  /** Cache of computed meanings */
  meaningCache: Map<string, Omega0Meaning>;
  /** Maximum oracle calls */
  maxOracleCalls: number;
  /** Current oracle call count */
  oracleCallCount: number;
  /** Depth limit */
  maxDepth: number;
  /** Current depth */
  depth: number;
  /** Whether to validate meanings against eval0 */
  validate: boolean;
  /** Confidence threshold for accepting predictions */
  confidenceThreshold: number;
};

/**
 * Create a default intensional context.
 */
export function createIntContext(
  oracle: OracleHandler,
  options: Partial<IntContext> = {}
): IntContext {
  return {
    oracle,
    evalCtx: createEvalContext({
      maxDepth: options.maxDepth ?? 100,
      maxOracleCalls: options.maxOracleCalls ?? 10,
    }),
    meaningCache: new Map(),
    maxOracleCalls: options.maxOracleCalls ?? 10,
    oracleCallCount: 0,
    maxDepth: options.maxDepth ?? 100,
    depth: 0,
    validate: options.validate ?? true,
    confidenceThreshold: options.confidenceThreshold ?? 0.5,
  };
}

// ─────────────────────────────────────────────────────────────────
// Expression Accessors (mirrored from eval0)
// ─────────────────────────────────────────────────────────────────

function quotedDatum(e: Omega0Expr): Omega0Val {
  if (Array.isArray(e) && e.length >= 2) {
    return exprToVal(e[1]);
  }
  throw new Error("Invalid quote expression");
}

function exprToVal(e: Omega0Expr): Omega0Val {
  if (e === null) return null;
  if (typeof e === "number") return e;
  if (typeof e === "string") return e;
  if (typeof e === "boolean") return e;
  if (isSymbol(e)) return e;
  if (Array.isArray(e)) {
    if (e.length === 0) return null;
    let result: Omega0Val = null;
    for (let i = e.length - 1; i >= 0; i--) {
      result = { tag: "Pair", car: exprToVal(e[i]), cdr: result };
    }
    return result;
  }
  return null;
}

function ifTest(e: Omega0Expr): Omega0Expr {
  if (Array.isArray(e) && e.length >= 2) return e[1];
  throw new Error("Invalid if expression");
}

function ifConseq(e: Omega0Expr): Omega0Expr {
  if (Array.isArray(e) && e.length >= 3) return e[2];
  throw new Error("Invalid if expression");
}

function ifAlt(e: Omega0Expr): Omega0Expr {
  if (Array.isArray(e) && e.length >= 4) return e[3];
  return null;
}

function lambdaParams(e: Omega0Expr): string[] {
  if (Array.isArray(e) && e.length >= 2 && Array.isArray(e[1])) {
    return e[1].map(p => {
      if (isSymbol(p)) return p.name;
      throw new Error(`Invalid parameter: ${omega0ToString(p as Omega0Val)}`);
    });
  }
  throw new Error("Invalid lambda expression");
}

function lambdaBody(e: Omega0Expr): Omega0Expr {
  if (Array.isArray(e) && e.length >= 3) {
    if (e.length > 3) {
      return [{ tag: "Symbol", name: "begin" }, ...e.slice(2)];
    }
    return e[2];
  }
  throw new Error("Invalid lambda expression");
}

function beginExprs(e: Omega0Expr): Omega0Expr[] {
  if (Array.isArray(e)) return e.slice(1);
  throw new Error("Invalid begin expression");
}

function defineNameAndRhs(e: Omega0Expr): { name: string; rhs: Omega0Expr } {
  if (Array.isArray(e) && e.length >= 3) {
    const nameOrFn = e[1];
    if (isSymbol(nameOrFn)) {
      return { name: nameOrFn.name, rhs: e[2] };
    }
    if (Array.isArray(nameOrFn) && nameOrFn.length > 0 && isSymbol(nameOrFn[0])) {
      const fnName = nameOrFn[0].name;
      const params = nameOrFn.slice(1);
      const body = e.slice(2);
      return {
        name: fnName,
        rhs: [{ tag: "Symbol", name: "lambda" }, params, ...body],
      };
    }
  }
  throw new Error("Invalid define expression");
}

function setNameAndRhs(e: Omega0Expr): { name: string; rhs: Omega0Expr } {
  if (Array.isArray(e) && e.length >= 3 && isSymbol(e[1])) {
    return { name: e[1].name, rhs: e[2] };
  }
  throw new Error("Invalid set! expression");
}

function appOperator(e: Omega0Expr): Omega0Expr {
  if (Array.isArray(e) && e.length > 0) return e[0];
  throw new Error("Invalid application");
}

function appOperands(e: Omega0Expr): Omega0Expr[] {
  if (Array.isArray(e)) return e.slice(1);
  throw new Error("Invalid application");
}

// ─────────────────────────────────────────────────────────────────
// int0 - Meta-Intensional Evaluator
// ─────────────────────────────────────────────────────────────────

/**
 * int0 - Intensional evaluation of Ω₀ expressions.
 *
 * Returns a Meaning with predicted denotation and confidence.
 * May consult the oracle for semantic inference.
 */
export function int0(
  e: Omega0Expr,
  env: Omega0Env,
  ctx: IntContext
): Omega0Meaning {
  // Check depth limit
  if (ctx.depth > ctx.maxDepth) {
    return makeMeaning(null, 0, "Maximum depth exceeded", false);
  }

  ctx.depth++;

  try {
    // Check cache first
    const cacheKey = makeCacheKey(e, env);
    if (ctx.meaningCache.has(cacheKey)) {
      return ctx.meaningCache.get(cacheKey)!;
    }

    // Self-evaluating: high confidence, no oracle needed
    if (isSelfEvaluating(e)) {
      const meaning = makeMeaning(e as Omega0Val, 1.0, "self-evaluating", true);
      ctx.meaningCache.set(cacheKey, meaning);
      return meaning;
    }

    // Variable reference
    if (isVariable(e)) {
      try {
        const value = lookup(env, symbolName(e));
        const meaning = makeMeaning(value, 1.0, "variable lookup", true);
        ctx.meaningCache.set(cacheKey, meaning);
        return meaning;
      } catch {
        return makeMeaning(null, 0, `Unbound variable: ${symbolName(e)}`, false);
      }
    }

    // Null
    if (e === null) {
      return makeMeaning(null, 1.0, "null literal", true);
    }

    // Must be a list
    if (!Array.isArray(e)) {
      return makeMeaning(null, 0, `Unknown expression type: ${typeof e}`, false);
    }

    if (e.length === 0) {
      return makeMeaning(null, 1.0, "empty list", true);
    }

    // Quote: high confidence
    if (isQuoted(e)) {
      const meaning = makeMeaning(quotedDatum(e), 1.0, "quote", true);
      ctx.meaningCache.set(cacheKey, meaning);
      return meaning;
    }

    // If: consult oracle for branch prediction or evaluate both
    if (isIf(e)) {
      return intIf(e, env, ctx);
    }

    // Lambda: high confidence (just creates closure)
    if (isLambda(e)) {
      const params = lambdaParams(e);
      const body = lambdaBody(e);
      const closure = makeClosure(params, body, env);
      const meaning = makeMeaning(closure, 1.0, "lambda", true);
      ctx.meaningCache.set(cacheKey, meaning);
      return meaning;
    }

    // Begin: sequence of meanings
    if (isBegin(e)) {
      return intSequence(beginExprs(e), env, ctx);
    }

    // Define
    if (isDefine(e)) {
      const { name, rhs } = defineNameAndRhs(e);
      const rhsMeaning = int0(rhs, env, ctx);
      if (rhsMeaning.confidence >= ctx.confidenceThreshold) {
        defineVar(env, name, rhsMeaning.denotation);
      }
      return rhsMeaning;
    }

    // Set!
    if (isSet(e)) {
      const { name, rhs } = setNameAndRhs(e);
      const rhsMeaning = int0(rhs, env, ctx);
      // set! is side-effectful, confidence depends on rhs
      return makeMeaning(
        rhsMeaning.denotation,
        rhsMeaning.confidence * 0.9, // slight confidence reduction for mutation
        `set! ${name}`,
        rhsMeaning.validated
      );
    }

    // Application: may consult oracle
    if (isApplication(e)) {
      return intApplication(e, env, ctx);
    }

    return makeMeaning(null, 0, `Unknown expression: ${omega0ToString(e as Omega0Val)}`, false);
  } finally {
    ctx.depth--;
  }
}

/**
 * Intensional evaluation of if expressions.
 */
function intIf(e: Omega0Expr, env: Omega0Env, ctx: IntContext): Omega0Meaning {
  const testMeaning = int0(ifTest(e), env, ctx);

  // If test has high confidence, evaluate appropriate branch
  if (testMeaning.confidence >= ctx.confidenceThreshold) {
    const testValue = testMeaning.denotation;
    if (testValue !== false && testValue !== null) {
      const conseqMeaning = int0(ifConseq(e), env, ctx);
      return makeMeaning(
        conseqMeaning.denotation,
        testMeaning.confidence * conseqMeaning.confidence,
        `if-then: ${conseqMeaning.evidence}`,
        testMeaning.validated && conseqMeaning.validated
      );
    } else {
      const altMeaning = int0(ifAlt(e), env, ctx);
      return makeMeaning(
        altMeaning.denotation,
        testMeaning.confidence * altMeaning.confidence,
        `if-else: ${altMeaning.evidence}`,
        testMeaning.validated && altMeaning.validated
      );
    }
  }

  // Low confidence: consult oracle or evaluate both branches
  const conseqMeaning = int0(ifConseq(e), env, ctx);
  const altMeaning = int0(ifAlt(e), env, ctx);

  // Return the branch with higher confidence, reduced overall
  if (conseqMeaning.confidence >= altMeaning.confidence) {
    return makeMeaning(
      conseqMeaning.denotation,
      testMeaning.confidence * conseqMeaning.confidence * 0.5,
      `if-uncertain: conseq branch`,
      false
    );
  } else {
    return makeMeaning(
      altMeaning.denotation,
      testMeaning.confidence * altMeaning.confidence * 0.5,
      `if-uncertain: alt branch`,
      false
    );
  }
}

/**
 * Intensional evaluation of a sequence.
 */
function intSequence(
  exprs: Omega0Expr[],
  env: Omega0Env,
  ctx: IntContext
): Omega0Meaning {
  if (exprs.length === 0) {
    return makeMeaning(null, 1.0, "empty sequence", true);
  }

  let result: Omega0Meaning = makeMeaning(null, 1.0, "sequence start", true);
  for (const expr of exprs) {
    result = int0(expr, env, ctx);
    // If confidence drops too low, stop early
    if (result.confidence < ctx.confidenceThreshold * 0.5) {
      break;
    }
  }
  return result;
}

/**
 * Intensional evaluation of applications.
 */
function intApplication(
  e: Omega0Expr,
  env: Omega0Env,
  ctx: IntContext
): Omega0Meaning {
  const procMeaning = int0(appOperator(e), env, ctx);

  if (procMeaning.confidence < ctx.confidenceThreshold) {
    return makeMeaning(
      null,
      procMeaning.confidence,
      "low confidence in procedure",
      false
    );
  }

  const proc = procMeaning.denotation;

  // Evaluate arguments intensionally
  const argMeanings = appOperands(e).map(arg => int0(arg, env, ctx));
  const minArgConfidence = Math.min(1, ...argMeanings.map(m => m.confidence));

  if (minArgConfidence < ctx.confidenceThreshold) {
    return makeMeaning(
      null,
      minArgConfidence,
      "low confidence in arguments",
      false
    );
  }

  const args = argMeanings.map(m => m.denotation);

  // LLM procedure: consult oracle
  if (isLLMProc(proc)) {
    return intLLMProc(proc as Omega0LLMProc, args, ctx);
  }

  // Closure: intensional application
  if (isClosure(proc)) {
    return intClosure(proc as Omega0Closure, args, ctx);
  }

  // Primitive: high confidence, apply directly
  if (isPrim(proc)) {
    try {
      const result = proc.fn(args);
      return makeMeaning(
        result,
        procMeaning.confidence * minArgConfidence,
        `prim ${proc.name}`,
        true
      );
    } catch (err) {
      return makeMeaning(
        null,
        0,
        `prim error: ${err instanceof Error ? err.message : String(err)}`,
        false
      );
    }
  }

  return makeMeaning(null, 0, `Cannot apply: ${omega0ToString(proc)}`, false);
}

/**
 * Intensional application of LLM procedure.
 */
function intLLMProc(
  proc: Omega0LLMProc,
  args: Omega0Val[],
  ctx: IntContext
): Omega0Meaning {
  // Check oracle call limit
  if (ctx.oracleCallCount >= ctx.maxOracleCalls) {
    return makeMeaning(null, 0, "oracle call limit exceeded", false);
  }

  ctx.oracleCallCount++;

  // Construct oracle request
  const envRef = storeEnv([]);

  try {
    // Consult oracle
    const response = ctx.oracle(
      [{ tag: "Symbol", name: "apply" }, proc, ...args] as Omega0Expr,
      envRef,
      ctx
    );

    // Handle any oracle requests for meta-evaluator
    if (response.requests) {
      for (const req of response.requests) {
        handleOracleRequest(req, ctx);
      }
    }

    const meaning = makeMeaning(
      response.denotation,
      response.confidence,
      response.evidence ?? `llm-proc ${proc.name}`,
      false // Not validated until checked against eval0
    );

    // Optionally validate against extensional evaluation
    if (ctx.validate && response.confidence >= ctx.confidenceThreshold) {
      try {
        const actualResult = proc.infer(args);
        if (deepEqual(response.denotation, actualResult)) {
          meaning.validated = true;
          meaning.evidence = (meaning.evidence ?? "") + " [validated]";
        } else {
          meaning.confidence *= 0.5;
          meaning.evidence = (meaning.evidence ?? "") + " [validation failed]";
        }
      } catch {
        // Validation failed, keep original confidence
      }
    }

    return meaning;
  } catch (err) {
    return makeMeaning(
      null,
      0,
      `oracle error: ${err instanceof Error ? err.message : String(err)}`,
      false
    );
  }
}

/**
 * Intensional application of closure.
 */
function intClosure(
  closure: Omega0Closure,
  args: Omega0Val[],
  ctx: IntContext
): Omega0Meaning {
  // Check arity
  if (args.length !== closure.params.length) {
    return makeMeaning(
      null,
      0,
      `Arity mismatch: expected ${closure.params.length}, got ${args.length}`,
      false
    );
  }

  // Extend environment
  const newEnv = extendEnv(closure.params, args, closure.env);

  // Evaluate body intensionally
  return int0(closure.body, newEnv, ctx);
}

/**
 * Handle oracle request for meta-evaluator.
 */
function handleOracleRequest(req: OracleRequest, ctx: IntContext): Omega0Val {
  if (req.kind === "ReqEval") {
    const env = getEnv(req.envRef);
    if (!env) {
      throw new Error(`Invalid environment reference: ${req.envRef}`);
    }
    return eval0(req.expr, env, ctx.evalCtx);
  }

  if (req.kind === "ReqApply") {
    return apply0(req.proc, req.args, ctx.evalCtx);
  }

  throw new Error(`Unknown oracle request kind: ${(req as any).kind}`);
}

/**
 * Deep equality check for values.
 */
function deepEqual(a: Omega0Val, b: Omega0Val): boolean {
  if (a === b) return true;
  if (typeof a !== typeof b) return false;
  if (a === null || b === null) return a === b;

  if (typeof a === "object" && typeof b === "object") {
    if ((a as any).tag !== (b as any).tag) return false;

    if ((a as any).tag === "Pair") {
      return deepEqual((a as any).car, (b as any).car) &&
             deepEqual((a as any).cdr, (b as any).cdr);
    }

    if ((a as any).tag === "Symbol") {
      return (a as any).name === (b as any).name;
    }
  }

  return false;
}

/**
 * Create cache key for expression + environment.
 */
function makeCacheKey(e: Omega0Expr, env: Omega0Env): string {
  // Simple cache key based on expression string
  // In production, would use more sophisticated hashing
  return omega0ToString(e as Omega0Val) + ":" + env.length;
}

// ─────────────────────────────────────────────────────────────────
// Default Oracle Implementation
// ─────────────────────────────────────────────────────────────────

/**
 * Create a simple oracle that falls back to eval0.
 */
export function createFallbackOracle(): OracleHandler {
  return (expr, envRef, ctx) => {
    const env = getEnv(envRef) ?? [];
    try {
      const result = eval0(expr, env, ctx.evalCtx);
      return {
        denotation: result,
        confidence: 1.0,
        evidence: "eval0 fallback",
      };
    } catch (err) {
      return {
        denotation: null,
        confidence: 0,
        evidence: `eval0 error: ${err instanceof Error ? err.message : String(err)}`,
      };
    }
  };
}

/**
 * Create a scripted oracle for testing.
 */
export function createScriptedOracle(
  script: Map<string, OracleResponse> | ((expr: Omega0Expr) => OracleResponse)
): OracleHandler {
  return (expr, _envRef, _ctx) => {
    if (typeof script === "function") {
      return script(expr);
    }

    const key = omega0ToString(expr as Omega0Val);
    if (script.has(key)) {
      return script.get(key)!;
    }

    // Default: low confidence
    return {
      denotation: null,
      confidence: 0.1,
      evidence: "no scripted response",
    };
  };
}

// ─────────────────────────────────────────────────────────────────
// Utility Functions
// ─────────────────────────────────────────────────────────────────

/**
 * Extract the denotation from a meaning, throwing if low confidence.
 */
export function commitMeaning(meaning: Omega0Meaning, threshold: number = 0.5): Omega0Val {
  if (meaning.confidence < threshold) {
    throw new Error(
      `Cannot commit meaning with confidence ${meaning.confidence.toFixed(2)} ` +
      `(threshold: ${threshold}): ${meaning.evidence}`
    );
  }
  return meaning.denotation;
}

/**
 * Check if a meaning is validated.
 */
export function isValidated(meaning: Omega0Meaning): boolean {
  return meaning.validated;
}

/**
 * Add an obligation to a meaning.
 */
export function addObligation(meaning: Omega0Meaning, obligation: string): Omega0Meaning {
  return {
    ...meaning,
    obligations: [...(meaning.obligations ?? []), obligation],
  };
}

/**
 * Check if all obligations are satisfied.
 */
export function obligationsSatisfied(meaning: Omega0Meaning): boolean {
  return !meaning.obligations || meaning.obligations.length === 0;
}
