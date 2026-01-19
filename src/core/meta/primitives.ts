// src/core/meta/primitives.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-18.md
// Prompt 18: Meta-circular evaluator - Primitives and LLM procedures

import type {
  Omega0Val,
  Omega0Env,
  Omega0Prim,
  Omega0LLMProc,
  Omega0Pair,
} from "./types";
import {
  makePrim,
  makeLLMProc,
  cons,
  car,
  cdr,
  isPair,
  isNull,
  isSymbol,
  isClosure,
  isPrim,
  isLLMProc,
  isMeaning,
  arrayToList,
  listToArray,
  listLength,
  omega0ToString,
  sym,
} from "./types";
import { defineVar, emptyEnv } from "./env";

// ─────────────────────────────────────────────────────────────────
// Arithmetic Primitives
// ─────────────────────────────────────────────────────────────────

const primPlus = makePrim("+", (args) => {
  return args.reduce((sum, x) => {
    if (typeof sum === "number" && typeof x === "number") {
      return sum + x;
    }
    return sum;
  }, 0) as Omega0Val;
});

const primMinus = makePrim("-", (args) => {
  if (args.length === 0) return 0;
  if (args.length === 1 && typeof args[0] === "number") return -args[0];
  const first = typeof args[0] === "number" ? args[0] : 0;
  return args.slice(1).reduce((diff, x) => {
    if (typeof diff === "number" && typeof x === "number") {
      return diff - x;
    }
    return diff;
  }, first) as Omega0Val;
});

const primMult = makePrim("*", (args) => {
  return args.reduce((prod, x) => {
    if (typeof prod === "number" && typeof x === "number") {
      return prod * x;
    }
    return prod;
  }, 1) as Omega0Val;
});

const primDiv = makePrim("/", (args) => {
  if (args.length < 2) return 0;
  const first = typeof args[0] === "number" ? args[0] : 0;
  return args.slice(1).reduce((quot, x) => {
    if (typeof quot === "number" && typeof x === "number" && x !== 0) {
      return quot / x;
    }
    return quot;
  }, first) as Omega0Val;
});

const primMod = makePrim("modulo", (args) => {
  if (args.length >= 2 && typeof args[0] === "number" && typeof args[1] === "number") {
    return args[0] % args[1];
  }
  return 0;
});

// ─────────────────────────────────────────────────────────────────
// Comparison Primitives
// ─────────────────────────────────────────────────────────────────

const primEq = makePrim("=", (args) => {
  if (args.length < 2) return true;
  const first = args[0];
  return args.slice(1).every(x => x === first);
});

const primLt = makePrim("<", (args) => {
  if (args.length < 2) return true;
  for (let i = 0; i < args.length - 1; i++) {
    if (typeof args[i] !== "number" || typeof args[i + 1] !== "number") return false;
    if (args[i] as number >= (args[i + 1] as number)) return false;
  }
  return true;
});

const primGt = makePrim(">", (args) => {
  if (args.length < 2) return true;
  for (let i = 0; i < args.length - 1; i++) {
    if (typeof args[i] !== "number" || typeof args[i + 1] !== "number") return false;
    if (args[i] as number <= (args[i + 1] as number)) return false;
  }
  return true;
});

const primLe = makePrim("<=", (args) => {
  if (args.length < 2) return true;
  for (let i = 0; i < args.length - 1; i++) {
    if (typeof args[i] !== "number" || typeof args[i + 1] !== "number") return false;
    if (args[i] as number > (args[i + 1] as number)) return false;
  }
  return true;
});

const primGe = makePrim(">=", (args) => {
  if (args.length < 2) return true;
  for (let i = 0; i < args.length - 1; i++) {
    if (typeof args[i] !== "number" || typeof args[i + 1] !== "number") return false;
    if (args[i] as number < (args[i + 1] as number)) return false;
  }
  return true;
});

const primEqQuestion = makePrim("eq?", (args) => {
  if (args.length < 2) return true;
  return args[0] === args[1];
});

const primEqualQuestion = makePrim("equal?", (args) => {
  if (args.length < 2) return true;
  return deepEqual(args[0], args[1]);
});

function deepEqual(a: Omega0Val, b: Omega0Val): boolean {
  if (a === b) return true;
  if (typeof a !== typeof b) return false;
  if (isPair(a) && isPair(b)) {
    return deepEqual(a.car, b.car) && deepEqual(a.cdr, b.cdr);
  }
  if (isSymbol(a) && isSymbol(b)) {
    return a.name === b.name;
  }
  return false;
}

// ─────────────────────────────────────────────────────────────────
// Boolean Primitives
// ─────────────────────────────────────────────────────────────────

const primNot = makePrim("not", (args) => {
  if (args.length === 0) return true;
  return args[0] === false || args[0] === null;
});

const primAnd = makePrim("and", (args) => {
  for (const arg of args) {
    if (arg === false || arg === null) return false;
  }
  return args.length > 0 ? args[args.length - 1] : true;
});

const primOr = makePrim("or", (args) => {
  for (const arg of args) {
    if (arg !== false && arg !== null) return arg;
  }
  return false;
});

// ─────────────────────────────────────────────────────────────────
// List Primitives
// ─────────────────────────────────────────────────────────────────

const primCons = makePrim("cons", (args) => {
  if (args.length < 2) return null;
  return cons(args[0], args[1]);
});

const primCar = makePrim("car", (args) => {
  if (args.length === 0 || !isPair(args[0])) {
    throw new Error("car expects a pair");
  }
  return car(args[0] as Omega0Pair);
});

const primCdr = makePrim("cdr", (args) => {
  if (args.length === 0 || !isPair(args[0])) {
    throw new Error("cdr expects a pair");
  }
  return cdr(args[0] as Omega0Pair);
});

const primList = makePrim("list", (args) => {
  return arrayToList(args);
});

const primLength = makePrim("length", (args) => {
  if (args.length === 0) return 0;
  return listLength(args[0]);
});

const primAppend = makePrim("append", (args) => {
  if (args.length === 0) return null;
  if (args.length === 1) return args[0];

  const result: Omega0Val[] = [];
  for (const arg of args) {
    if (arg !== null) {
      result.push(...listToArray(arg));
    }
  }
  return arrayToList(result);
});

const primReverse = makePrim("reverse", (args) => {
  if (args.length === 0) return null;
  const arr = listToArray(args[0]);
  return arrayToList(arr.reverse());
});

const primMap = makePrim("map", (args) => {
  if (args.length < 2) return null;
  const fn = args[0];
  const list = args[1];

  if (!isClosure(fn) && !isPrim(fn) && !isLLMProc(fn)) {
    throw new Error("map expects a procedure");
  }

  const arr = listToArray(list);
  // Note: map needs to call the function, but we can't do that here without eval0
  // This is a simplified version that just returns the list
  // In a full implementation, map would be a special form or use continuation passing
  return list;
});

// ─────────────────────────────────────────────────────────────────
// Type Predicates
// ─────────────────────────────────────────────────────────────────

const primNullQuestion = makePrim("null?", (args) => {
  return args.length > 0 && isNull(args[0]);
});

const primPairQuestion = makePrim("pair?", (args) => {
  return args.length > 0 && isPair(args[0]);
});

const primListQuestion = makePrim("list?", (args) => {
  if (args.length === 0) return false;
  let current = args[0];
  while (current !== null) {
    if (!isPair(current)) return false;
    current = (current as Omega0Pair).cdr;
  }
  return true;
});

const primSymbolQuestion = makePrim("symbol?", (args) => {
  return args.length > 0 && isSymbol(args[0]);
});

const primNumberQuestion = makePrim("number?", (args) => {
  return args.length > 0 && typeof args[0] === "number";
});

const primStringQuestion = makePrim("string?", (args) => {
  return args.length > 0 && typeof args[0] === "string";
});

const primBooleanQuestion = makePrim("boolean?", (args) => {
  return args.length > 0 && typeof args[0] === "boolean";
});

const primProcedureQuestion = makePrim("procedure?", (args) => {
  return args.length > 0 && (isClosure(args[0]) || isPrim(args[0]) || isLLMProc(args[0]));
});

// ─────────────────────────────────────────────────────────────────
// String Primitives
// ─────────────────────────────────────────────────────────────────

const primStringAppend = makePrim("string-append", (args) => {
  return args.map(a => typeof a === "string" ? a : omega0ToString(a)).join("");
});

const primStringLength = makePrim("string-length", (args) => {
  if (args.length === 0 || typeof args[0] !== "string") return 0;
  return args[0].length;
});

const primSubstring = makePrim("substring", (args) => {
  if (args.length < 2 || typeof args[0] !== "string") return "";
  const str = args[0] as string;
  const start = typeof args[1] === "number" ? args[1] : 0;
  const end = args.length > 2 && typeof args[2] === "number" ? args[2] : str.length;
  return str.substring(start, end);
});

const primStringToList = makePrim("string->list", (args) => {
  if (args.length === 0 || typeof args[0] !== "string") return null;
  const chars = (args[0] as string).split("").map(c => c);
  return arrayToList(chars);
});

const primListToString = makePrim("list->string", (args) => {
  if (args.length === 0) return "";
  const arr = listToArray(args[0]);
  return arr.map(c => typeof c === "string" ? c : "").join("");
});

// ─────────────────────────────────────────────────────────────────
// I/O Primitives (simplified)
// ─────────────────────────────────────────────────────────────────

const primDisplay = makePrim("display", (args) => {
  if (args.length > 0) {
    // In a real implementation, this would output to a port
    // For now, just return the value
  }
  return null;
});

const primNewline = makePrim("newline", (_args) => {
  return null;
});

// ─────────────────────────────────────────────────────────────────
// Error Handling
// ─────────────────────────────────────────────────────────────────

const primError = makePrim("error", (args) => {
  const msg = args.map(a => omega0ToString(a)).join(" ");
  throw new Error(msg);
});

// ─────────────────────────────────────────────────────────────────
// Base Environment
// ─────────────────────────────────────────────────────────────────

/**
 * Create the base environment with all primitives.
 */
export function createBaseEnv(): Omega0Env {
  let env = emptyEnv();

  // Arithmetic
  env = defineVar(env, "+", primPlus);
  env = defineVar(env, "-", primMinus);
  env = defineVar(env, "*", primMult);
  env = defineVar(env, "/", primDiv);
  env = defineVar(env, "modulo", primMod);

  // Comparison
  env = defineVar(env, "=", primEq);
  env = defineVar(env, "<", primLt);
  env = defineVar(env, ">", primGt);
  env = defineVar(env, "<=", primLe);
  env = defineVar(env, ">=", primGe);
  env = defineVar(env, "eq?", primEqQuestion);
  env = defineVar(env, "equal?", primEqualQuestion);

  // Boolean
  env = defineVar(env, "not", primNot);
  env = defineVar(env, "and", primAnd);
  env = defineVar(env, "or", primOr);

  // List
  env = defineVar(env, "cons", primCons);
  env = defineVar(env, "car", primCar);
  env = defineVar(env, "cdr", primCdr);
  env = defineVar(env, "list", primList);
  env = defineVar(env, "length", primLength);
  env = defineVar(env, "append", primAppend);
  env = defineVar(env, "reverse", primReverse);

  // Type predicates
  env = defineVar(env, "null?", primNullQuestion);
  env = defineVar(env, "pair?", primPairQuestion);
  env = defineVar(env, "list?", primListQuestion);
  env = defineVar(env, "symbol?", primSymbolQuestion);
  env = defineVar(env, "number?", primNumberQuestion);
  env = defineVar(env, "string?", primStringQuestion);
  env = defineVar(env, "boolean?", primBooleanQuestion);
  env = defineVar(env, "procedure?", primProcedureQuestion);

  // String
  env = defineVar(env, "string-append", primStringAppend);
  env = defineVar(env, "string-length", primStringLength);
  env = defineVar(env, "substring", primSubstring);
  env = defineVar(env, "string->list", primStringToList);
  env = defineVar(env, "list->string", primListToString);

  // I/O
  env = defineVar(env, "display", primDisplay);
  env = defineVar(env, "newline", primNewline);

  // Error
  env = defineVar(env, "error", primError);

  return env;
}

// ─────────────────────────────────────────────────────────────────
// LLM Procedure Factory
// ─────────────────────────────────────────────────────────────────

/**
 * LLM inference handler type.
 */
export type LLMInferHandler = (
  prompt: string,
  args: Omega0Val[]
) => Omega0Val;

/**
 * Create an LLM procedure with a custom inference handler.
 */
export function createLLMProc(
  name: string,
  prompt: string,
  handler: LLMInferHandler
): Omega0LLMProc {
  return makeLLMProc(
    name,
    prompt,
    (args) => handler(prompt, args)
  );
}

/**
 * Create a scripted LLM procedure (for testing).
 *
 * The script maps input patterns to outputs.
 */
export function createScriptedLLMProc(
  name: string,
  prompt: string,
  script: Map<string, Omega0Val> | ((args: Omega0Val[]) => Omega0Val)
): Omega0LLMProc {
  const handler: LLMInferHandler = (_prompt, args) => {
    if (typeof script === "function") {
      return script(args);
    }

    // Try to match args against script keys
    const key = args.map(omega0ToString).join(",");
    if (script.has(key)) {
      return script.get(key)!;
    }

    // Default: return first arg or null
    return args.length > 0 ? args[0] : null;
  };

  return createLLMProc(name, prompt, handler);
}

/**
 * Add an LLM procedure to an environment.
 */
export function addLLMProcToEnv(
  env: Omega0Env,
  name: string,
  proc: Omega0LLMProc
): Omega0Env {
  return defineVar(env, name, proc);
}

/**
 * Primitive for creating LLM procedures at runtime.
 */
export function createMakeLLMProcPrim(
  defaultHandler: LLMInferHandler
): Omega0Prim {
  return makePrim("make-llm-proc", (args) => {
    if (args.length < 1 || typeof args[0] !== "string") {
      throw new Error("make-llm-proc expects a string prompt");
    }
    const prompt = args[0] as string;
    const name = args.length > 1 && typeof args[1] === "string"
      ? args[1] as string
      : "anonymous-llm-proc";

    return createLLMProc(name, prompt, defaultHandler);
  });
}
