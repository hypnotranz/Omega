// src/core/meta/types.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-18.md
// Prompt 18: Meta-circular evaluator - Type definitions for Ω₀

import type { Val } from "../eval/values";

// ─────────────────────────────────────────────────────────────────
// Ω₀ Expression Types (S-expression representation)
// ─────────────────────────────────────────────────────────────────

/**
 * Ω₀ is a deliberately small object language subset:
 * - literals: numbers/strings/bools
 * - variables: symbols
 * - (quote datum)
 * - (if test conseq alt)
 * - (lambda (x ...) body)
 * - (begin e1 e2 ... en)
 * - (define x rhs)
 * - (set! x rhs)
 * - application: (f a1 ... an)
 *
 * No macros, no handle/effect, no infer, no amb.
 * Those exist at the host Ω level.
 */

/**
 * Ω₀ expression - S-expression style datum.
 */
export type Omega0Expr =
  | number
  | string
  | boolean
  | symbol
  | null
  | Omega0List;

/**
 * Ω₀ list - a proper list of expressions.
 */
export type Omega0List = Omega0Expr[];

/**
 * Symbol representation for Ω₀ (using string with marker).
 */
export type Omega0Symbol = {
  tag: "Symbol";
  name: string;
};

/**
 * Create an Ω₀ symbol.
 */
export function sym(name: string): Omega0Symbol {
  return { tag: "Symbol", name };
}

/**
 * Check if value is an Ω₀ symbol.
 */
export function isSymbol(x: unknown): x is Omega0Symbol {
  return typeof x === "object" && x !== null && (x as any).tag === "Symbol";
}

/**
 * Get symbol name.
 */
export function symbolName(s: Omega0Symbol): string {
  return s.name;
}

// ─────────────────────────────────────────────────────────────────
// Ω₀ Values (evaluation results)
// ─────────────────────────────────────────────────────────────────

/**
 * Ω₀ value - result of evaluation.
 */
export type Omega0Val =
  | number
  | string
  | boolean
  | null
  | Omega0Symbol
  | Omega0Pair
  | Omega0Closure
  | Omega0Prim
  | Omega0LLMProc
  | Omega0Meaning;

/**
 * Ω₀ pair (cons cell).
 */
export type Omega0Pair = {
  tag: "Pair";
  car: Omega0Val;
  cdr: Omega0Val;
};

/**
 * Create a pair.
 */
export function cons(car: Omega0Val, cdr: Omega0Val): Omega0Pair {
  return { tag: "Pair", car, cdr };
}

/**
 * Check if value is a pair.
 */
export function isPair(x: unknown): x is Omega0Pair {
  return typeof x === "object" && x !== null && (x as any).tag === "Pair";
}

/**
 * Get car of pair.
 */
export function car(p: Omega0Pair): Omega0Val {
  return p.car;
}

/**
 * Get cdr of pair.
 */
export function cdr(p: Omega0Pair): Omega0Val {
  return p.cdr;
}

/**
 * Check if value is null (empty list).
 */
export function isNull(x: unknown): x is null {
  return x === null;
}

/**
 * Check if value is a list (null or pair with list cdr).
 */
export function isList(x: unknown): boolean {
  if (x === null) return true;
  if (isPair(x)) return isList(x.cdr);
  return false;
}

/**
 * Convert array to proper list.
 */
export function arrayToList(arr: Omega0Val[]): Omega0Val {
  if (arr.length === 0) return null;
  return cons(arr[0], arrayToList(arr.slice(1)));
}

/**
 * Convert proper list to array.
 */
export function listToArray(list: Omega0Val): Omega0Val[] {
  const result: Omega0Val[] = [];
  let current = list;
  while (isPair(current)) {
    result.push(current.car);
    current = current.cdr;
  }
  return result;
}

/**
 * Get length of a list.
 */
export function listLength(list: Omega0Val): number {
  let count = 0;
  let current = list;
  while (isPair(current)) {
    count++;
    current = current.cdr;
  }
  return count;
}

// ─────────────────────────────────────────────────────────────────
// Ω₀ Closures
// ─────────────────────────────────────────────────────────────────

/**
 * Ω₀ closure - a lambda with captured environment.
 */
export type Omega0Closure = {
  tag: "Closure";
  params: string[];
  body: Omega0Expr;
  env: Omega0Env;
  label?: string;
};

/**
 * Create a closure.
 */
export function makeClosure(
  params: string[],
  body: Omega0Expr,
  env: Omega0Env,
  label?: string
): Omega0Closure {
  return { tag: "Closure", params, body, env, label };
}

/**
 * Check if value is a closure.
 */
export function isClosure(x: unknown): x is Omega0Closure {
  return typeof x === "object" && x !== null && (x as any).tag === "Closure";
}

// ─────────────────────────────────────────────────────────────────
// Ω₀ Primitives
// ─────────────────────────────────────────────────────────────────

/**
 * Ω₀ primitive - a built-in operation.
 */
export type Omega0Prim = {
  tag: "Prim";
  name: string;
  fn: (args: Omega0Val[]) => Omega0Val;
};

/**
 * Create a primitive.
 */
export function makePrim(
  name: string,
  fn: (args: Omega0Val[]) => Omega0Val
): Omega0Prim {
  return { tag: "Prim", name, fn };
}

/**
 * Check if value is a primitive.
 */
export function isPrim(x: unknown): x is Omega0Prim {
  return typeof x === "object" && x !== null && (x as any).tag === "Prim";
}

// ─────────────────────────────────────────────────────────────────
// Ω₀ LLM Procedures (Semantic Functions)
// ─────────────────────────────────────────────────────────────────

/**
 * Ω₀ LLM procedure - a semantic function backed by inference.
 */
export type Omega0LLMProc = {
  tag: "LLMProc";
  name: string;
  prompt: string;
  schema?: unknown;
  /** Handler that performs inference */
  infer: (args: Omega0Val[]) => Omega0Val;
};

/**
 * Create an LLM procedure.
 */
export function makeLLMProc(
  name: string,
  prompt: string,
  infer: (args: Omega0Val[]) => Omega0Val,
  schema?: unknown
): Omega0LLMProc {
  return { tag: "LLMProc", name, prompt, infer, schema };
}

/**
 * Check if value is an LLM procedure.
 */
export function isLLMProc(x: unknown): x is Omega0LLMProc {
  return typeof x === "object" && x !== null && (x as any).tag === "LLMProc";
}

// ─────────────────────────────────────────────────────────────────
// Ω₀ Meaning (Intensional Results)
// ─────────────────────────────────────────────────────────────────

/**
 * Ω₀ meaning - result of intensional evaluation (int0).
 */
export type Omega0Meaning = {
  tag: "Meaning";
  /** The predicted/computed denotation */
  denotation: Omega0Val;
  /** Confidence level (0-1) */
  confidence: number;
  /** Evidence/justification */
  evidence?: string;
  /** Whether validated against extensional eval */
  validated: boolean;
  /** Obligations for commit */
  obligations?: string[];
};

/**
 * Create a meaning.
 */
export function makeMeaning(
  denotation: Omega0Val,
  confidence: number,
  evidence?: string,
  validated: boolean = false
): Omega0Meaning {
  return { tag: "Meaning", denotation, confidence, evidence, validated };
}

/**
 * Check if value is a meaning.
 */
export function isMeaning(x: unknown): x is Omega0Meaning {
  return typeof x === "object" && x !== null && (x as any).tag === "Meaning";
}

// ─────────────────────────────────────────────────────────────────
// Ω₀ Environment
// ─────────────────────────────────────────────────────────────────

/**
 * Environment frame - association list of (name, value) pairs.
 */
export type Omega0Frame = Map<string, Omega0Val>;

/**
 * Environment - list of frames (most recent first).
 */
export type Omega0Env = Omega0Frame[];

/**
 * Create an empty environment.
 */
export function emptyEnv(): Omega0Env {
  return [];
}

/**
 * Create a new frame.
 */
export function makeFrame(): Omega0Frame {
  return new Map();
}

// ─────────────────────────────────────────────────────────────────
// Expression Predicates
// ─────────────────────────────────────────────────────────────────

/**
 * Check if expression is self-evaluating (number, string, boolean).
 */
export function isSelfEvaluating(e: Omega0Expr): boolean {
  return typeof e === "number" || typeof e === "string" || typeof e === "boolean";
}

/**
 * Check if expression is a variable (symbol).
 */
export function isVariable(e: Omega0Expr): e is Omega0Symbol {
  return isSymbol(e);
}

/**
 * Check if expression is a tagged list (e.g., (quote ...), (if ...), etc.).
 */
export function isTaggedList(e: Omega0Expr, tag: string): boolean {
  if (!Array.isArray(e) || e.length === 0) return false;
  const head = e[0];
  return isSymbol(head) && head.name === tag;
}

/**
 * Check if expression is a quote form.
 */
export function isQuoted(e: Omega0Expr): boolean {
  return isTaggedList(e, "quote");
}

/**
 * Check if expression is an if form.
 */
export function isIf(e: Omega0Expr): boolean {
  return isTaggedList(e, "if");
}

/**
 * Check if expression is a lambda form.
 */
export function isLambda(e: Omega0Expr): boolean {
  return isTaggedList(e, "lambda");
}

/**
 * Check if expression is a begin form.
 */
export function isBegin(e: Omega0Expr): boolean {
  return isTaggedList(e, "begin");
}

/**
 * Check if expression is a define form.
 */
export function isDefine(e: Omega0Expr): boolean {
  return isTaggedList(e, "define");
}

/**
 * Check if expression is a set! form.
 */
export function isSet(e: Omega0Expr): boolean {
  return isTaggedList(e, "set!");
}

/**
 * Check if expression is an application (any list that's not a special form).
 */
export function isApplication(e: Omega0Expr): boolean {
  return Array.isArray(e) && e.length > 0;
}

// ─────────────────────────────────────────────────────────────────
// Evaluation Trace (for differential testing)
// ─────────────────────────────────────────────────────────────────

/**
 * Trace entry for evaluation.
 */
export type EvalTraceEntry = {
  /** Kind of operation */
  kind: "eval" | "apply" | "lookup" | "define" | "infer";
  /** Expression or procedure being processed */
  subject: string;
  /** Result (stringified) */
  result?: string;
  /** Depth in call stack */
  depth: number;
};

/**
 * Evaluation context for tracing and governance.
 */
export type EvalContext = {
  /** Trace of evaluation steps */
  trace: EvalTraceEntry[];
  /** Current call depth */
  depth: number;
  /** Maximum depth allowed */
  maxDepth: number;
  /** Oracle call count */
  oracleCallCount: number;
  /** Maximum oracle calls allowed */
  maxOracleCalls: number;
  /** Whether tracing is enabled */
  tracing: boolean;
};

/**
 * Create a default evaluation context.
 */
export function createEvalContext(options: Partial<EvalContext> = {}): EvalContext {
  return {
    trace: [],
    depth: 0,
    maxDepth: options.maxDepth ?? 100,
    oracleCallCount: 0,
    maxOracleCalls: options.maxOracleCalls ?? 10,
    tracing: options.tracing ?? false,
  };
}

// ─────────────────────────────────────────────────────────────────
// Value Display
// ─────────────────────────────────────────────────────────────────

/**
 * Convert Ω₀ value to string for display.
 */
export function omega0ToString(v: Omega0Val): string {
  if (v === null) return "()";
  if (typeof v === "number") return String(v);
  if (typeof v === "string") return JSON.stringify(v);
  if (typeof v === "boolean") return v ? "#t" : "#f";
  if (isSymbol(v)) return v.name;
  if (isPair(v)) {
    const items: string[] = [];
    let current: Omega0Val = v;
    while (isPair(current)) {
      items.push(omega0ToString(current.car));
      current = current.cdr;
    }
    if (current !== null) {
      return `(${items.join(" ")} . ${omega0ToString(current)})`;
    }
    return `(${items.join(" ")})`;
  }
  if (isClosure(v)) return `#<closure${v.label ? ` ${v.label}` : ""}>`;
  if (isPrim(v)) return `#<prim ${v.name}>`;
  if (isLLMProc(v)) return `#<llm-proc ${v.name}>`;
  if (isMeaning(v)) return `#<meaning conf=${v.confidence.toFixed(2)}>`;
  if (Array.isArray(v)) {
    return `(${v.map(omega0ToString).join(" ")})`;
  }
  return String(v);
}
