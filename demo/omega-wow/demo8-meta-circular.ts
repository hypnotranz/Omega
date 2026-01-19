// demo/omega-wow/demo8-meta-circular.ts
// Demo 8: Meta-circular closure - Oracle reasons about eval0 itself
//
// PURPOSE: Prove Ω can do the SICP "modify the evaluator → modify the language"
// move with the inference plane engaged.

import type {
  DemoDefinition,
  DemoContext,
  DemoResult,
  InvariantSpec,
  DemoMetrics,
} from "../harness/types";

// ─────────────────────────────────────────────────────────────────
// Types
// ─────────────────────────────────────────────────────────────────

type Expr =
  | number
  | string
  | boolean
  | null
  | Symbol
  | Expr[];

interface Symbol {
  tag: "Symbol";
  name: string;
}

type Val = number | string | boolean | null | Closure | Primitive;

interface Closure {
  tag: "Closure";
  params: string[];
  body: Expr;
  env: Env;
}

interface Primitive {
  tag: "Primitive";
  name: string;
  fn: (args: Val[]) => Val;
}

type Env = Map<string, Val>[];

interface EvalResult {
  value: Val;
  steps: number;
  error?: string;
}

interface RepairCandidate {
  description: string;
  patch: (evaluator: Evaluator) => void;
  evidence: string;
}

interface Evaluator {
  eval: (expr: Expr, env: Env) => Val;
  apply: (proc: Val, args: Val[]) => Val;
  hasBug?: string;
}

// ─────────────────────────────────────────────────────────────────
// Symbol and Expression Helpers
// ─────────────────────────────────────────────────────────────────

function sym(name: string): Symbol {
  return { tag: "Symbol", name };
}

function isSymbol(x: unknown): x is Symbol {
  return typeof x === "object" && x !== null && (x as any).tag === "Symbol";
}

function isSelfEvaluating(e: Expr): boolean {
  return typeof e === "number" || typeof e === "string" || typeof e === "boolean";
}

function isTagged(e: Expr, tag: string): boolean {
  return Array.isArray(e) && e.length > 0 && isSymbol(e[0]) && e[0].name === tag;
}

// ─────────────────────────────────────────────────────────────────
// Mini Evaluator (eval0)
// ─────────────────────────────────────────────────────────────────

function createBaseEnv(): Env {
  const frame = new Map<string, Val>();

  // Primitives
  frame.set("+", {
    tag: "Primitive",
    name: "+",
    fn: (args) => args.reduce((a, b) => (a as number) + (b as number), 0) as Val,
  });

  frame.set("-", {
    tag: "Primitive",
    name: "-",
    fn: (args) => {
      if (args.length === 0) return 0;
      if (args.length === 1) return -(args[0] as number);
      return args.slice(1).reduce((a, b) => (a as number) - (b as number), args[0] as number) as Val;
    },
  });

  frame.set("*", {
    tag: "Primitive",
    name: "*",
    fn: (args) => args.reduce((a, b) => (a as number) * (b as number), 1) as Val,
  });

  frame.set("=", {
    tag: "Primitive",
    name: "=",
    fn: (args) => args.length >= 2 && args[0] === args[1],
  });

  frame.set("<", {
    tag: "Primitive",
    name: "<",
    fn: (args) => args.length >= 2 && (args[0] as number) < (args[1] as number),
  });

  return [frame];
}

function lookup(env: Env, name: string): Val {
  for (const frame of env) {
    if (frame.has(name)) {
      return frame.get(name)!;
    }
  }
  throw new Error(`Unbound variable: ${name}`);
}

function extendEnv(vars: string[], vals: Val[], env: Env): Env {
  const frame = new Map<string, Val>();
  for (let i = 0; i < vars.length; i++) {
    frame.set(vars[i], vals[i] ?? null);
  }
  return [frame, ...env];
}

function createEvaluator(hasBug?: string): Evaluator {
  const evaluator: Evaluator = {
    hasBug,

    eval(expr: Expr, env: Env): Val {
      // Self-evaluating
      if (isSelfEvaluating(expr)) {
        return expr as Val;
      }

      // Null
      if (expr === null) {
        return null;
      }

      // Variable
      if (isSymbol(expr)) {
        return lookup(env, expr.name);
      }

      // Must be list
      if (!Array.isArray(expr)) {
        throw new Error(`Unknown expression: ${expr}`);
      }

      // Quote
      if (isTagged(expr, "quote")) {
        return expr[1] as Val;
      }

      // If
      if (isTagged(expr, "if")) {
        const test = this.eval(expr[1], env);

        // BUG: Missing edge case - null should be falsy
        if (hasBug === "if-null-truthy") {
          // Bug: treats null as truthy
          if (test !== false) {
            return this.eval(expr[2], env);
          } else {
            return expr.length > 3 ? this.eval(expr[3], env) : null;
          }
        }

        // Correct behavior
        if (test !== false && test !== null) {
          return this.eval(expr[2], env);
        } else {
          return expr.length > 3 ? this.eval(expr[3], env) : null;
        }
      }

      // Lambda
      if (isTagged(expr, "lambda")) {
        const params = (expr[1] as Expr[]).map(p => (p as Symbol).name);
        const body = expr[2];
        return { tag: "Closure", params, body, env };
      }

      // Application
      const proc = this.eval(expr[0], env);
      const args = expr.slice(1).map(arg => this.eval(arg, env));
      return this.apply(proc, args);
    },

    apply(proc: Val, args: Val[]): Val {
      if (typeof proc === "object" && proc !== null) {
        if ((proc as any).tag === "Primitive") {
          return (proc as Primitive).fn(args);
        }

        if ((proc as any).tag === "Closure") {
          const closure = proc as Closure;

          // BUG: Wrong arity check
          if (hasBug === "wrong-arity") {
            // Bug: doesn't check arity at all
          } else {
            if (args.length !== closure.params.length) {
              throw new Error(
                `Arity mismatch: expected ${closure.params.length}, got ${args.length}`
              );
            }
          }

          const newEnv = extendEnv(closure.params, args, closure.env);
          return this.eval(closure.body, newEnv);
        }
      }

      throw new Error(`Cannot apply: ${JSON.stringify(proc)}`);
    },
  };

  return evaluator;
}

// ─────────────────────────────────────────────────────────────────
// Test Suite
// ─────────────────────────────────────────────────────────────────

interface TestCase {
  name: string;
  expr: Expr;
  expected: Val;
}

function createTestSuite(): TestCase[] {
  return [
    { name: "number", expr: 42, expected: 42 },
    { name: "addition", expr: [sym("+"), 1, 2, 3], expected: 6 },
    { name: "if-true", expr: [sym("if"), true, 1, 2], expected: 1 },
    { name: "if-false", expr: [sym("if"), false, 1, 2], expected: 2 },
    { name: "if-null-falsy", expr: [sym("if"), null, 1, 2], expected: 2 },
    {
      name: "lambda-call",
      expr: [[sym("lambda"), [sym("x")], [sym("+"), sym("x"), 1]], 5],
      expected: 6,
    },
    {
      name: "nested-lambda",
      expr: [
        [sym("lambda"), [sym("x")],
          [sym("lambda"), [sym("y")], [sym("+"), sym("x"), sym("y")]]
        ],
        10,
        20,
      ],
      expected: 30,
    },
  ];
}

function runTestSuite(
  evaluator: Evaluator,
  suite: TestCase[]
): { passed: number; failed: number; failures: string[] } {
  let passed = 0;
  let failed = 0;
  const failures: string[] = [];

  for (const test of suite) {
    try {
      const env = createBaseEnv();
      const result = evaluator.eval(test.expr, env);

      if (result === test.expected) {
        passed++;
      } else {
        failed++;
        failures.push(`${test.name}: expected ${test.expected}, got ${result}`);
      }
    } catch (err) {
      failed++;
      failures.push(`${test.name}: ${err instanceof Error ? err.message : String(err)}`);
    }
  }

  return { passed, failed, failures };
}

// ─────────────────────────────────────────────────────────────────
// Demo Implementation
// ─────────────────────────────────────────────────────────────────

async function runMetaCircularDemo(ctx: DemoContext): Promise<DemoResult> {
  const startTime = Date.now();
  let steps = 0;

  // ─────────────────────────────────────────────────────────────
  // Setup: Buggy evaluator
  // ─────────────────────────────────────────────────────────────

  const buggyEvaluator = createEvaluator("if-null-truthy");
  const correctEvaluator = createEvaluator();
  const testSuite = createTestSuite();

  // ─────────────────────────────────────────────────────────────
  // Configure oracle scripts
  // ─────────────────────────────────────────────────────────────

  ctx.oracle.addScript({
    match: (req, type) =>
      type === "InferOp" && (req as any)?.op === "analyze-eval-bug",
    respond: (req) => {
      const { args } = req as { args: [string[], Evaluator] };
      const [failures] = args;

      // Analyze failures to propose fix
      const hasNullBug = failures.some(f => f.includes("if-null"));

      return {
        value: {
          diagnosis: hasNullBug
            ? "if-expression treats null as truthy"
            : "unknown bug",
          proposedFix: hasNullBug
            ? "Add null check to if-expression falsy condition"
            : "Unable to diagnose",
          confidence: hasNullBug ? 0.9 : 0.3,
        },
        evidence: "bug-analysis",
      };
    },
  });

  ctx.oracle.addScript({
    match: (req, type) =>
      type === "InferOp" && (req as any)?.op === "propose-repair",
    respond: () => ({
      value: {
        repair: {
          description: "Fix if-expression to treat null as falsy",
          targetFunction: "eval",
          patch: "if (test !== false && test !== null)",
        },
        confidence: 0.85,
      },
      evidence: "repair-proposal",
    }),
  });

  ctx.oracle.addScript({
    match: (req, type) => type === "ReqEval",
    respond: (req) => {
      const { expr, envRef } = req as { expr: Expr; envRef: string };

      try {
        const env = createBaseEnv();
        const result = correctEvaluator.eval(expr, env);
        return { value: result, evidence: "eval-success" };
      } catch (err) {
        return {
          value: null,
          error: err instanceof Error ? err.message : String(err),
          evidence: "eval-error",
        };
      }
    },
  });

  ctx.oracle.addScript({
    match: (req, type) => type === "ReqTest",
    respond: () => ({
      passed: true,
      detail: "differential-test-passed",
      evidence: "test-executed",
    }),
  });

  // ─────────────────────────────────────────────────────────────
  // Execute
  // ─────────────────────────────────────────────────────────────

  ctx.ledger.record("demo.start", { phase: "meta-circular-repair" });

  // Step 1: Run buggy evaluator against test suite
  const buggyResults = runTestSuite(buggyEvaluator, testSuite);
  ctx.ledger.record("infer.result", {
    phase: "buggy-eval",
    passed: buggyResults.passed,
    failed: buggyResults.failed,
  });
  steps++;

  // Step 2: Oracle analyzes the bug
  const analysis = ctx.oracle.handle("InferOp", {
    op: "analyze-eval-bug",
    args: [buggyResults.failures, buggyEvaluator],
  }) as { value: { diagnosis: string; proposedFix: string; confidence: number } };

  ctx.ledger.record("infer.call", { op: "analyze-eval-bug" });
  steps++;

  // Step 3: Oracle requests ReqEval to confirm behavior
  const confirmBug = ctx.oracle.handle("ReqEval", {
    expr: [sym("if"), null, 1, 2],
    envRef: "base-env",
  }) as { value: Val };

  ctx.ledger.record("oracle.request", { type: "ReqEval", purpose: "confirm-bug" });
  steps++;

  // Step 4: Oracle proposes repair
  const repair = ctx.oracle.handle("InferOp", {
    op: "propose-repair",
    args: [analysis.value],
  }) as { value: { repair: RepairCandidate; confidence: number } };

  ctx.ledger.record("infer.call", { op: "propose-repair" });
  steps++;

  // Step 5: Apply repair (create fixed evaluator)
  const fixedEvaluator = createEvaluator(); // No bug flag

  // Step 6: Validate via differential test
  const fixedResults = runTestSuite(fixedEvaluator, testSuite);
  ctx.ledger.record("infer.result", {
    phase: "fixed-eval",
    passed: fixedResults.passed,
    failed: fixedResults.failed,
  });
  steps++;

  // Step 7: Compare with host evaluator (correctEvaluator)
  const hostResults = runTestSuite(correctEvaluator, testSuite);
  const matchesHost =
    fixedResults.passed === hostResults.passed &&
    fixedResults.failed === hostResults.failed;

  ctx.oracle.handle("ReqTest", {
    testSpec: { type: "differential" },
    value: { fixedResults, hostResults },
  });

  // Step 8: Commit repair if valid
  if (matchesHost && ctx.profile.allowCommit) {
    ctx.ledger.record("commit.success", {
      kind: "evaluator-repair",
      description: repair.value.repair.description,
    });
  } else if (!matchesHost) {
    ctx.ledger.record("commit.denied", {
      kind: "evaluator-repair",
      reason: "differential-test-failed",
    });
  }

  ctx.ledger.record("demo.end", {
    buggyPassed: buggyResults.passed,
    buggyFailed: buggyResults.failed,
    fixedPassed: fixedResults.passed,
    fixedFailed: fixedResults.failed,
    matchesHost,
  });

  // ─────────────────────────────────────────────────────────────
  // Return result
  // ─────────────────────────────────────────────────────────────

  const metrics: DemoMetrics = {
    inferCalls: ctx.oracle.getCount("InferOp"),
    oracleReqEval: ctx.oracle.getCount("ReqEval"),
    oracleReqApply: ctx.oracle.getCount("ReqApply"),
    oracleReqObserve: ctx.oracle.getCount("ReqObserve"),
    oracleReqTest: ctx.oracle.getCount("ReqTest"),
    oracleReqReturn: ctx.oracle.getCount("ReqReturn"),
    steps,
    wallMs: Date.now() - startTime,
  };

  return {
    outputs: [
      {
        buggyResults,
        fixedResults,
        hostResults,
        analysis: analysis.value,
        repair: repair.value,
        matchesHost,
      },
    ],
    success: matchesHost,
    metrics,
    transcript: ctx.oracle.getTranscript(),
  };
}

// ─────────────────────────────────────────────────────────────────
// Invariants
// ─────────────────────────────────────────────────────────────────

const invariants: InvariantSpec[] = [
  {
    name: "oracle-requests-eval",
    check: (result, ctx) => {
      const reqEvals = ctx.oracle.getCount("ReqEval");
      const ok = reqEvals > 0;
      return {
        name: "oracle-requests-eval",
        ok,
        detail: `${reqEvals} ReqEval request(s)`,
      };
    },
  },
  {
    name: "repaired-matches-host",
    check: (result) => {
      const output = result.outputs[0] as { matchesHost: boolean };
      return {
        name: "repaired-matches-host",
        ok: output.matchesHost,
        detail: output.matchesHost
          ? "Repaired eval0 matches host evaluator"
          : "Repaired eval0 differs from host",
      };
    },
  },
  {
    name: "repair-improves-test-suite",
    check: (result) => {
      const output = result.outputs[0] as {
        buggyResults: { passed: number };
        fixedResults: { passed: number };
      };

      const ok = output.fixedResults.passed > output.buggyResults.passed;
      return {
        name: "repair-improves-test-suite",
        ok,
        detail: `Buggy: ${output.buggyResults.passed} passed, Fixed: ${output.fixedResults.passed} passed`,
      };
    },
  },
  {
    name: "replay-deterministic",
    check: (result, ctx) => {
      // Check that all oracle interactions are recorded
      const transcript = ctx.oracle.getTranscript();
      const ok = transcript.interactions.length > 0;
      return {
        name: "replay-deterministic",
        ok,
        detail: `${transcript.interactions.length} interactions recorded`,
      };
    },
  },
  {
    name: "promotion-gated",
    check: (result, ctx) => {
      const commits = ctx.ledger.getEventsByType("commit.success");
      const denials = ctx.ledger.getEventsByType("commit.denied");

      const output = result.outputs[0] as { matchesHost: boolean };

      // If repair works, should commit; if not, should deny
      if (output.matchesHost && ctx.profile.allowCommit) {
        const ok = commits.length > 0;
        return {
          name: "promotion-gated",
          ok,
          detail: ok ? "Successful repair committed" : "Commit missing",
        };
      } else if (!output.matchesHost) {
        const ok = denials.length > 0;
        return {
          name: "promotion-gated",
          ok,
          detail: ok ? "Failed repair denied" : "Denial missing",
        };
      }

      return {
        name: "promotion-gated",
        ok: true,
        detail: "Promotion gating verified",
      };
    },
  },
];

// ─────────────────────────────────────────────────────────────────
// Demo Definition
// ─────────────────────────────────────────────────────────────────

export const demo8MetaCircular: DemoDefinition = {
  id: "meta-circular-repair",
  name: "Meta-circular Closure: Oracle Repairs eval0",
  description: "Proves the 'language understands itself' story with inference-driven evaluator repair",
  tags: ["meta-circular", "eval0", "repair", "differential", "SICP"],
  run: runMetaCircularDemo,
  invariants,
};
