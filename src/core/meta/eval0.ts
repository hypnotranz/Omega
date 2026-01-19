// src/core/meta/eval0.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-18.md
// Prompt 18: Meta-circular evaluator - eval0 and apply0

import type {
  Omega0Expr,
  Omega0Val,
  Omega0Env,
  Omega0Closure,
  Omega0Prim,
  Omega0LLMProc,
  Omega0Symbol,
  EvalContext,
} from "./types";
import {
  isSymbol,
  symbolName,
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
  makeClosure,
  isClosure,
  isPrim,
  isLLMProc,
  isPair,
  omega0ToString,
  createEvalContext,
} from "./types";
import {
  lookup,
  extendEnv,
  defineVar,
  setVar,
} from "./env";

// ─────────────────────────────────────────────────────────────────
// Truthiness
// ─────────────────────────────────────────────────────────────────

/**
 * Check if a value is truthy in Ω₀.
 * Only #f (false) and null are falsy.
 */
export function isTruthy(v: Omega0Val): boolean {
  if (v === false) return false;
  if (v === null) return false;
  return true;
}

// ─────────────────────────────────────────────────────────────────
// Expression Accessors
// ─────────────────────────────────────────────────────────────────

/**
 * Get the quoted datum from a quote expression.
 */
function quotedDatum(e: Omega0Expr): Omega0Val {
  if (Array.isArray(e) && e.length >= 2) {
    return exprToVal(e[1]);
  }
  throw new Error("Invalid quote expression");
}

/**
 * Convert an expression to a value (for quote).
 */
function exprToVal(e: Omega0Expr): Omega0Val {
  if (e === null) return null;
  if (typeof e === "number") return e;
  if (typeof e === "string") return e;
  if (typeof e === "boolean") return e;
  if (isSymbol(e)) return e;
  if (Array.isArray(e)) {
    // Convert to proper list
    if (e.length === 0) return null;
    let result: Omega0Val = null;
    for (let i = e.length - 1; i >= 0; i--) {
      result = { tag: "Pair", car: exprToVal(e[i]), cdr: result };
    }
    return result;
  }
  return null;
}

/**
 * Get if-test from an if expression.
 */
function ifTest(e: Omega0Expr): Omega0Expr {
  if (Array.isArray(e) && e.length >= 2) {
    return e[1];
  }
  throw new Error("Invalid if expression");
}

/**
 * Get if-consequent from an if expression.
 */
function ifConseq(e: Omega0Expr): Omega0Expr {
  if (Array.isArray(e) && e.length >= 3) {
    return e[2];
  }
  throw new Error("Invalid if expression");
}

/**
 * Get if-alternative from an if expression.
 */
function ifAlt(e: Omega0Expr): Omega0Expr {
  if (Array.isArray(e) && e.length >= 4) {
    return e[3];
  }
  return null; // default to null if no alt
}

/**
 * Get lambda parameters from a lambda expression.
 */
function lambdaParams(e: Omega0Expr): string[] {
  if (Array.isArray(e) && e.length >= 2 && Array.isArray(e[1])) {
    return e[1].map(p => {
      if (isSymbol(p)) return p.name;
      throw new Error(`Invalid parameter: ${omega0ToString(p as Omega0Val)}`);
    });
  }
  throw new Error("Invalid lambda expression");
}

/**
 * Get lambda body from a lambda expression.
 */
function lambdaBody(e: Omega0Expr): Omega0Expr {
  if (Array.isArray(e) && e.length >= 3) {
    // If multiple body expressions, wrap in begin
    if (e.length > 3) {
      return [{ tag: "Symbol", name: "begin" } as Omega0Symbol, ...e.slice(2)];
    }
    return e[2];
  }
  throw new Error("Invalid lambda expression");
}

/**
 * Get begin expressions.
 */
function beginExprs(e: Omega0Expr): Omega0Expr[] {
  if (Array.isArray(e)) {
    return e.slice(1);
  }
  throw new Error("Invalid begin expression");
}

/**
 * Get define name and value expression.
 */
function defineNameAndRhs(e: Omega0Expr): { name: string; rhs: Omega0Expr } {
  if (Array.isArray(e) && e.length >= 3) {
    const nameOrFn = e[1];

    // Simple define: (define x rhs)
    if (isSymbol(nameOrFn)) {
      return { name: nameOrFn.name, rhs: e[2] };
    }

    // Function define: (define (f x y) body) -> (define f (lambda (x y) body))
    if (Array.isArray(nameOrFn) && nameOrFn.length > 0 && isSymbol(nameOrFn[0])) {
      const fnName = nameOrFn[0].name;
      const params = nameOrFn.slice(1);
      const body = e.slice(2);
      const lambdaExpr: Omega0Expr = [
        { tag: "Symbol", name: "lambda" } as Omega0Symbol,
        params,
        ...body,
      ];
      return { name: fnName, rhs: lambdaExpr };
    }
  }
  throw new Error("Invalid define expression");
}

/**
 * Get set! name and value expression.
 */
function setNameAndRhs(e: Omega0Expr): { name: string; rhs: Omega0Expr } {
  if (Array.isArray(e) && e.length >= 3 && isSymbol(e[1])) {
    return { name: e[1].name, rhs: e[2] };
  }
  throw new Error("Invalid set! expression");
}

/**
 * Get application operator.
 */
function appOperator(e: Omega0Expr): Omega0Expr {
  if (Array.isArray(e) && e.length > 0) {
    return e[0];
  }
  throw new Error("Invalid application");
}

/**
 * Get application operands.
 */
function appOperands(e: Omega0Expr): Omega0Expr[] {
  if (Array.isArray(e)) {
    return e.slice(1);
  }
  throw new Error("Invalid application");
}

// ─────────────────────────────────────────────────────────────────
// eval0 - The Meta-Circular Evaluator
// ─────────────────────────────────────────────────────────────────

/**
 * eval0 - Evaluate an Ω₀ expression in the given environment.
 *
 * This is the heart of the meta-circular evaluator.
 */
export function eval0(
  e: Omega0Expr,
  env: Omega0Env,
  ctx: EvalContext = createEvalContext()
): Omega0Val {
  // Check depth limit
  if (ctx.depth > ctx.maxDepth) {
    throw new Error(`Maximum evaluation depth exceeded (${ctx.maxDepth})`);
  }

  ctx.depth++;

  try {
    // Trace if enabled
    if (ctx.tracing) {
      ctx.trace.push({
        kind: "eval",
        subject: omega0ToString(e as Omega0Val),
        depth: ctx.depth,
      });
    }

    // Self-evaluating: numbers, strings, booleans
    if (isSelfEvaluating(e)) {
      return e as Omega0Val;
    }

    // Variable reference
    if (isVariable(e)) {
      const result = lookup(env, symbolName(e));
      if (ctx.tracing) {
        ctx.trace.push({
          kind: "lookup",
          subject: symbolName(e),
          result: omega0ToString(result),
          depth: ctx.depth,
        });
      }
      return result;
    }

    // Null
    if (e === null) {
      return null;
    }

    // Must be a list from here
    if (!Array.isArray(e)) {
      throw new Error(`Unknown expression type: ${typeof e}`);
    }

    // Empty list evaluates to null
    if (e.length === 0) {
      return null;
    }

    // Quote
    if (isQuoted(e)) {
      return quotedDatum(e);
    }

    // If
    if (isIf(e)) {
      const testVal = eval0(ifTest(e), env, ctx);
      if (isTruthy(testVal)) {
        return eval0(ifConseq(e), env, ctx);
      } else {
        return eval0(ifAlt(e), env, ctx);
      }
    }

    // Lambda
    if (isLambda(e)) {
      const params = lambdaParams(e);
      const body = lambdaBody(e);
      return makeClosure(params, body, env);
    }

    // Begin
    if (isBegin(e)) {
      return evalSequence(beginExprs(e), env, ctx);
    }

    // Define
    if (isDefine(e)) {
      const { name, rhs } = defineNameAndRhs(e);
      const value = eval0(rhs, env, ctx);
      defineVar(env, name, value);
      if (ctx.tracing) {
        ctx.trace.push({
          kind: "define",
          subject: name,
          result: omega0ToString(value),
          depth: ctx.depth,
        });
      }
      return value;
    }

    // Set!
    if (isSet(e)) {
      const { name, rhs } = setNameAndRhs(e);
      const value = eval0(rhs, env, ctx);
      setVar(env, name, value);
      return value;
    }

    // Application
    if (isApplication(e)) {
      const proc = eval0(appOperator(e), env, ctx);
      const args = appOperands(e).map(arg => eval0(arg, env, ctx));
      return apply0(proc, args, ctx);
    }

    throw new Error(`Unknown expression: ${omega0ToString(e as Omega0Val)}`);
  } finally {
    ctx.depth--;
  }
}

/**
 * Evaluate a sequence of expressions, returning the last value.
 */
function evalSequence(
  exprs: Omega0Expr[],
  env: Omega0Env,
  ctx: EvalContext
): Omega0Val {
  if (exprs.length === 0) {
    return null;
  }

  let result: Omega0Val = null;
  for (const expr of exprs) {
    result = eval0(expr, env, ctx);
  }
  return result;
}

// ─────────────────────────────────────────────────────────────────
// apply0 - Procedure Application
// ─────────────────────────────────────────────────────────────────

/**
 * apply0 - Apply a procedure to arguments.
 *
 * This handles closures, primitives, and LLM procedures.
 */
export function apply0(
  proc: Omega0Val,
  args: Omega0Val[],
  ctx: EvalContext = createEvalContext()
): Omega0Val {
  // Trace if enabled
  if (ctx.tracing) {
    ctx.trace.push({
      kind: "apply",
      subject: omega0ToString(proc),
      depth: ctx.depth,
    });
  }

  // Closure application
  if (isClosure(proc)) {
    const closure = proc as Omega0Closure;

    // Check arity
    if (args.length !== closure.params.length) {
      throw new Error(
        `Arity mismatch: expected ${closure.params.length} args, got ${args.length}`
      );
    }

    // Extend environment with parameter bindings
    const newEnv = extendEnv(closure.params, args, closure.env);

    // Evaluate body in extended environment
    return eval0(closure.body, newEnv, ctx);
  }

  // Primitive application
  if (isPrim(proc)) {
    const prim = proc as Omega0Prim;
    return prim.fn(args);
  }

  // LLM procedure application
  if (isLLMProc(proc)) {
    const llmProc = proc as Omega0LLMProc;

    // Check oracle call limit
    if (ctx.oracleCallCount >= ctx.maxOracleCalls) {
      throw new Error(`Maximum oracle calls exceeded (${ctx.maxOracleCalls})`);
    }

    ctx.oracleCallCount++;

    // Trace
    if (ctx.tracing) {
      ctx.trace.push({
        kind: "infer",
        subject: llmProc.name,
        depth: ctx.depth,
      });
    }

    // Call the inference handler
    return llmProc.infer(args);
  }

  throw new Error(`Cannot apply non-procedure: ${omega0ToString(proc)}`);
}

// ─────────────────────────────────────────────────────────────────
// Top-Level Driver
// ─────────────────────────────────────────────────────────────────

/**
 * Evaluate a program (sequence of expressions) in a fresh environment.
 */
export function evalProgram(
  program: Omega0Expr[],
  baseEnv: Omega0Env,
  ctx: EvalContext = createEvalContext()
): Omega0Val {
  let result: Omega0Val = null;

  for (const expr of program) {
    result = eval0(expr, baseEnv, ctx);
  }

  return result;
}

/**
 * Parse and evaluate an Ω₀ expression from a simple S-expression representation.
 */
export function evalString(
  source: string,
  env: Omega0Env,
  ctx: EvalContext = createEvalContext()
): Omega0Val {
  // Simple S-expression parser (for testing)
  const expr = parseSimpleSExpr(source);
  return eval0(expr, env, ctx);
}

/**
 * Simple S-expression parser.
 */
export function parseSimpleSExpr(source: string): Omega0Expr {
  const tokens = tokenize(source);
  let pos = 0;

  function peek(): string | null {
    return pos < tokens.length ? tokens[pos] : null;
  }

  function consume(): string {
    return tokens[pos++];
  }

  function parseExpr(): Omega0Expr {
    const tok = peek();

    if (tok === null) {
      throw new Error("Unexpected end of input");
    }

    if (tok === "(") {
      consume();
      const items: Omega0Expr[] = [];
      while (peek() !== ")" && peek() !== null) {
        items.push(parseExpr());
      }
      if (peek() !== ")") {
        throw new Error("Expected )");
      }
      consume();
      return items;
    }

    if (tok === "'") {
      consume();
      const quoted = parseExpr();
      return [{ tag: "Symbol", name: "quote" } as Omega0Symbol, quoted];
    }

    consume();

    // Number
    if (/^-?\d+(\.\d+)?$/.test(tok)) {
      return parseFloat(tok);
    }

    // Boolean
    if (tok === "#t" || tok === "true") return true;
    if (tok === "#f" || tok === "false") return false;

    // String
    if (tok.startsWith('"') && tok.endsWith('"')) {
      return tok.slice(1, -1);
    }

    // Symbol
    return { tag: "Symbol", name: tok } as Omega0Symbol;
  }

  return parseExpr();
}

/**
 * Tokenize S-expression source.
 */
function tokenize(source: string): string[] {
  const tokens: string[] = [];
  let i = 0;

  while (i < source.length) {
    const c = source[i];

    // Skip whitespace
    if (/\s/.test(c)) {
      i++;
      continue;
    }

    // Skip comments
    if (c === ";") {
      while (i < source.length && source[i] !== "\n") {
        i++;
      }
      continue;
    }

    // Parentheses
    if (c === "(" || c === ")") {
      tokens.push(c);
      i++;
      continue;
    }

    // Quote
    if (c === "'") {
      tokens.push(c);
      i++;
      continue;
    }

    // String
    if (c === '"') {
      let str = '"';
      i++;
      while (i < source.length && source[i] !== '"') {
        if (source[i] === "\\") {
          str += source[i++];
        }
        str += source[i++];
      }
      str += '"';
      i++;
      tokens.push(str);
      continue;
    }

    // Atom (symbol or number)
    let atom = "";
    while (i < source.length && !/[\s()'";]/.test(source[i])) {
      atom += source[i++];
    }
    if (atom) {
      tokens.push(atom);
    }
  }

  return tokens;
}
