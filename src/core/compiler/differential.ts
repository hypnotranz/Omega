// src/core/compiler/differential.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-17.md
// Prompt 17: Differential testing harness - comparing interpreter vs compiled

import type { Expr } from "../ast";
import type { Val } from "../eval/values";
import { VUnit } from "../eval/values";
import { sha256JSON } from "../artifacts/hash";
import type {
  ANFProgram,
  BytecodeProgram,
  IRProgram,
  DifferentialReport,
  EffectTraceEntry,
  CompilerObligation,
  EvidenceRef,
  VMConfig,
} from "./types";
import { compile, getIR, CompilationResult } from "./pipeline";
import { run, getResult, createDefaultHandlers, EffectHandler, defaultVMConfig } from "./vm";
import { toBytecode } from "./bytecode";

// ─────────────────────────────────────────────────────────────────
// Interpreter Simulation
// ─────────────────────────────────────────────────────────────────

/**
 * Simulated interpreter state for differential testing.
 */
export type InterpreterState = {
  result: Val;
  effectTrace: EffectTraceEntry[];
  oracleCallCount: number;
};

/**
 * Simple interpreter for ANF (for differential testing).
 *
 * This interprets ANF directly without going through bytecode,
 * allowing us to compare against the full compiler pipeline.
 */
export function interpretANF(
  program: ANFProgram,
  env: Map<string, Val> = new Map(),
  handlers: Map<string, EffectHandler> = createDefaultHandlers(),
  maxOps: number = 50
): InterpreterState {
  const effectTrace: EffectTraceEntry[] = [];
  let oracleCallCount = 0;
  let ops = 0;

  // Environment for variable bindings
  const bindings = new Map<string, Val>(env);

  function evalAtom(atom: { tag: "Lit"; v: Val } | { tag: "Var"; name: string }): Val {
    if (atom.tag === "Lit") {
      return atom.v;
    }
    return bindings.get(atom.name) ?? VUnit;
  }

  function evalPrim(prim: any): Val {
    ops++;
    if (ops > maxOps) {
      throw new Error("Operation limit exceeded");
    }

    switch (prim.tag) {
      case "Lambda": {
        // Create a closure-like value
        return {
          tag: "Closure",
          params: prim.params,
          body: prim.body,
          env: Array.from(bindings.entries()),
        } as any;
      }

      case "Call": {
        const fnVal = evalAtom(prim.fn);
        const args = prim.args.map(evalAtom);

        if ((fnVal as any).tag === "Closure") {
          const closure = fnVal as any;
          const savedBindings = new Map(bindings);

          // Start with closure's captured environment
          const closureEnv = new Map<string, Val>();
          for (const [k, v] of closure.env) {
            closureEnv.set(k, v);
          }

          // Add current bindings that aren't shadowed by closure env
          for (const [k, v] of bindings) {
            if (!closureEnv.has(k)) {
              closureEnv.set(k, v);
            }
          }

          // Set up new environment for call
          bindings.clear();
          for (const [k, v] of closureEnv) {
            bindings.set(k, v);
          }

          // Bind parameters
          for (let i = 0; i < closure.params.length; i++) {
            bindings.set(closure.params[i], args[i] ?? VUnit);
          }

          const result = evalExpr(closure.body);

          // Restore bindings
          bindings.clear();
          for (const [k, v] of savedBindings) {
            bindings.set(k, v);
          }

          return result;
        }

        return VUnit;
      }

      case "Effect": {
        const args = prim.args.map(evalAtom);

        // Record effect
        const entry: EffectTraceEntry = {
          op: prim.op,
          argsDigest: sha256JSON(args),
          seq: effectTrace.length,
        };

        if (prim.op === "infer.op") {
          oracleCallCount++;
        }

        // Invoke handler
        const handler = handlers.get(prim.op);
        if (handler) {
          const result = handler(prim.op, args, (r) => r);
          entry.result = result;
          effectTrace.push(entry);
          return result;
        }

        effectTrace.push(entry);
        return VUnit;
      }

      case "Quote": {
        return datumToVal(prim.datum);
      }

      case "Prim": {
        const args = prim.args.map(evalAtom);
        return evalPrimOp(prim.name, args);
      }

      case "Match": {
        const scrut = evalAtom(prim.scrut);

        for (const clause of prim.clauses) {
          const match = matchPattern(clause.pat, scrut);
          if (match) {
            // Bind pattern variables
            for (const [name, val] of match) {
              bindings.set(name, val);
            }
            return evalExpr(clause.body);
          }
        }

        return VUnit;
      }

      case "Handle": {
        // Simplified - just evaluate body
        return evalExpr(prim.body);
      }

      default:
        return VUnit;
    }
  }

  function evalExpr(expr: any): Val {
    ops++;
    if (ops > maxOps) {
      throw new Error("Operation limit exceeded");
    }

    switch (expr.tag) {
      case "Return":
        return evalAtom(expr.v);

      case "Let": {
        const val = evalPrim(expr.rhs);
        bindings.set(expr.name, val);
        return evalExpr(expr.body);
      }

      case "LetRec": {
        // Initialize bindings
        for (const b of expr.bindings) {
          bindings.set(b.name, VUnit);
        }
        // Evaluate RHS
        for (const b of expr.bindings) {
          bindings.set(b.name, evalPrim(b.rhs));
        }
        return evalExpr(expr.body);
      }

      case "If": {
        const test = evalAtom(expr.test);
        if (isTruthy(test)) {
          return evalExpr(expr.thn);
        } else {
          return evalExpr(expr.els);
        }
      }

      case "Seq":
        evalExpr(expr.first);
        return evalExpr(expr.second);

      case "Set": {
        const val = evalAtom(expr.rhs);
        bindings.set(expr.name, val);
        return evalExpr(expr.body);
      }

      default:
        return VUnit;
    }
  }

  try {
    const result = evalExpr(program.body);
    return { result, effectTrace, oracleCallCount };
  } catch (e) {
    return {
      result: { tag: "Sym", name: `error:${e instanceof Error ? e.message : String(e)}` },
      effectTrace,
      oracleCallCount,
    };
  }
}

/**
 * Evaluate a primitive operation.
 */
function evalPrimOp(name: string, args: Val[]): Val {
  switch (name) {
    case "+":
      if (args[0]?.tag === "Num" && args[1]?.tag === "Num") {
        return { tag: "Num", n: args[0].n + args[1].n };
      }
      return VUnit;
    case "-":
      if (args[0]?.tag === "Num" && args[1]?.tag === "Num") {
        return { tag: "Num", n: args[0].n - args[1].n };
      }
      return VUnit;
    case "*":
      if (args[0]?.tag === "Num" && args[1]?.tag === "Num") {
        return { tag: "Num", n: args[0].n * args[1].n };
      }
      return VUnit;
    case "=":
    case "eq?":
      return valEqual(args[0], args[1]) ? { tag: "Bool", b: true } : { tag: "Bool", b: false };
    case "<":
      if (args[0]?.tag === "Num" && args[1]?.tag === "Num") {
        return { tag: "Bool", b: args[0].n < args[1].n };
      }
      return { tag: "Bool", b: false };
    case "identity":
      return args[0] ?? VUnit;
    case "undefined":
      return VUnit;
    case "cons":
      return { tag: "Pair", car: args[0] ?? VUnit, cdr: args[1] ?? VUnit };
    case "car":
      return args[0]?.tag === "Pair" ? args[0].car : VUnit;
    case "cdr":
      return args[0]?.tag === "Pair" ? args[0].cdr : VUnit;
    default:
      return VUnit;
  }
}

/**
 * Match a pattern against a value.
 */
function matchPattern(pat: any, val: Val): Map<string, Val> | null {
  switch (pat.tag) {
    case "PWild":
      return new Map();
    case "PVar":
      return new Map([[pat.name, val]]);
    case "PLit":
      return valEqual(pat.v, val) ? new Map() : null;
    case "PVector":
      if (val.tag !== "Vector") return null;
      if (pat.items.length !== val.items.length) return null;
      const bindings = new Map<string, Val>();
      for (let i = 0; i < pat.items.length; i++) {
        const sub = matchPattern(pat.items[i], val.items[i]);
        if (!sub) return null;
        for (const [k, v] of sub) {
          bindings.set(k, v);
        }
      }
      return bindings;
    default:
      return null;
  }
}

/**
 * Convert datum to Val.
 */
function datumToVal(datum: unknown): Val {
  if (datum === null || datum === undefined) return VUnit;
  if (typeof datum === "boolean") return { tag: "Bool", b: datum };
  if (typeof datum === "number") return { tag: "Num", n: datum };
  if (typeof datum === "string") return { tag: "Str", s: datum };
  if (Array.isArray(datum)) {
    return { tag: "Vector", items: datum.map(datumToVal) };
  }
  return { tag: "Sym", name: String(datum) };
}

/**
 * Check if a value is truthy.
 */
function isTruthy(val: Val): boolean {
  if (val.tag === "Bool") return val.b;
  if (val.tag === "Unit") return false;
  return true;
}

/**
 * Check value equality.
 */
function valEqual(a: Val | undefined, b: Val | undefined): boolean {
  if (!a || !b) return a === b;
  if (a.tag !== b.tag) return false;

  switch (a.tag) {
    case "Unit":
      return true;
    case "Num":
      return a.n === (b as { tag: "Num"; n: number }).n;
    case "Bool":
      return a.b === (b as { tag: "Bool"; b: boolean }).b;
    case "Str":
      return a.s === (b as { tag: "Str"; s: string }).s;
    case "Sym":
      return a.name === (b as { tag: "Sym"; name: string }).name;
    default:
      return JSON.stringify(a) === JSON.stringify(b);
  }
}

// ─────────────────────────────────────────────────────────────────
// Differential Testing
// ─────────────────────────────────────────────────────────────────

/**
 * Run differential test comparing interpreter vs compiled execution.
 */
export function differentialRun(
  expr: Expr,
  env: Map<string, Val> = new Map(),
  config: {
    maxOps?: number;
    handlers?: Map<string, EffectHandler>;
    strict?: boolean;
  } = {}
): DifferentialReport {
  const maxOps = config.maxOps ?? 50;
  const handlers = config.handlers ?? createDefaultHandlers();
  const strict = config.strict ?? true;

  // Compile to ANF
  const compiled = compile(expr, { target: "anf" });
  const anfProgram = compiled.program.program as ANFProgram;

  // Run interpreter
  const interpResult = interpretANF(anfProgram, env, handlers, maxOps);

  // Compile to bytecode and run VM
  const bytecodeProgram = toBytecode(anfProgram);
  const vmConfig: VMConfig = {
    ...defaultVMConfig,
    maxOperations: maxOps,
  };
  const vmState = run(bytecodeProgram, [], env, vmConfig, handlers);
  const compiledResult = getResult(vmState);

  // Compare outputs
  const outputsMatch = valEqual(interpResult.result, compiledResult);

  // Compare effect traces
  const effectsMatch = compareEffectTraces(
    interpResult.effectTrace,
    vmState.effectTrace,
    strict
  );

  // Compare oracle consumption
  const oracleMatch = strict
    ? interpResult.oracleCallCount === vmState.oracleCallCount
    : vmState.oracleCallCount <= interpResult.oracleCallCount;

  // Collect mismatches
  const mismatches: string[] = [];

  if (!outputsMatch) {
    mismatches.push(
      `Output mismatch: interp=${JSON.stringify(interpResult.result)}, compiled=${JSON.stringify(compiledResult)}`
    );
  }

  if (!effectsMatch) {
    mismatches.push(
      `Effect trace mismatch: interp=${interpResult.effectTrace.length} effects, compiled=${vmState.effectTrace.length} effects`
    );
  }

  if (!oracleMatch) {
    mismatches.push(
      `Oracle call mismatch: interp=${interpResult.oracleCallCount}, compiled=${vmState.oracleCallCount}`
    );
  }

  return {
    outputsMatch,
    effectsMatch,
    oracleMatch,
    interpOutput: interpResult.result,
    compiledOutput: compiledResult,
    interpEffects: interpResult.effectTrace,
    compiledEffects: vmState.effectTrace,
    interpOracleCalls: interpResult.oracleCallCount,
    compiledOracleCalls: vmState.oracleCallCount,
    mismatches,
    passed: outputsMatch && effectsMatch && oracleMatch,
  };
}

/**
 * Compare two effect traces.
 */
export function compareEffectTraces(
  interp: EffectTraceEntry[],
  compiled: EffectTraceEntry[],
  strict: boolean
): boolean {
  if (strict) {
    // Strict: exact match
    if (interp.length !== compiled.length) return false;

    for (let i = 0; i < interp.length; i++) {
      if (interp[i].op !== compiled[i].op) return false;
      if (interp[i].argsDigest !== compiled[i].argsDigest) return false;
    }

    return true;
  } else {
    // Pragmatic: compiled may have fewer effects if outputs match
    // Just check that all interp effects appear in compiled
    for (const interpEffect of interp) {
      const found = compiled.some(
        c => c.op === interpEffect.op && c.argsDigest === interpEffect.argsDigest
      );
      if (!found) return false;
    }
    return true;
  }
}

/**
 * Create evidence from differential test result.
 */
export function createTestEvidence(
  report: DifferentialReport,
  testId: string
): EvidenceRef {
  return {
    kind: "test-run",
    ref: sha256JSON({ testId, report }),
    timestamp: Date.now(),
  };
}

/**
 * Satisfy differential test obligation.
 */
export function satisfyDifferentialObligation(
  obligations: CompilerObligation[],
  report: DifferentialReport,
  testId: string
): { obligations: CompilerObligation[]; evidence: EvidenceRef | null } {
  const evidence = report.passed ? createTestEvidence(report, testId) : null;

  const updated = obligations.map(o => {
    if (o.kind === "differential-test" && o.status === "pending") {
      return {
        ...o,
        status: report.passed ? ("satisfied" as const) : ("failed" as const),
        evidence: evidence?.ref,
        error: report.passed ? undefined : report.mismatches.join("; "),
      };
    }
    return o;
  });

  return { obligations: updated, evidence };
}

// ─────────────────────────────────────────────────────────────────
// Metamorphic Testing
// ─────────────────────────────────────────────────────────────────

/**
 * Run metamorphic test: f(g(x)) == g(f(x)) for commutative operations.
 */
export function metamorphicTest(
  expr1: Expr,
  expr2: Expr,
  env: Map<string, Val> = new Map(),
  maxOps: number = 50
): { passed: boolean; result1: Val; result2: Val } {
  const handlers = createDefaultHandlers();

  const compiled1 = compile(expr1, { target: "anf" });
  const compiled2 = compile(expr2, { target: "anf" });

  const anf1 = compiled1.program.program as ANFProgram;
  const anf2 = compiled2.program.program as ANFProgram;

  const result1 = interpretANF(anf1, env, handlers, maxOps).result;
  const result2 = interpretANF(anf2, env, handlers, maxOps).result;

  return {
    passed: valEqual(result1, result2),
    result1,
    result2,
  };
}

// ─────────────────────────────────────────────────────────────────
// Test Harness
// ─────────────────────────────────────────────────────────────────

/**
 * Full differential test harness.
 */
export function runDifferentialTestSuite(
  testCases: Array<{ name: string; expr: Expr; env?: Map<string, Val> }>,
  config: { maxOps?: number; strict?: boolean } = {}
): { passed: number; failed: number; reports: Map<string, DifferentialReport> } {
  let passed = 0;
  let failed = 0;
  const reports = new Map<string, DifferentialReport>();

  for (const testCase of testCases) {
    const report = differentialRun(testCase.expr, testCase.env, config);
    reports.set(testCase.name, report);

    if (report.passed) {
      passed++;
    } else {
      failed++;
    }
  }

  return { passed, failed, reports };
}
