// src/core/test/testRunner.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-18.md
// Test runner with OmegaTests and CommandTests support

import type { Hash } from "../artifacts/hash";
import type { BudgetLimits } from "../governance/budgets";
import type { Budget } from "../governance/budgets";
import { budgetConsumeToolCall } from "../governance/budgets";
import type { CapSet } from "../governance/caps";
import { capRequire } from "../governance/caps";
import type { Val } from "../eval/values";

// =========================================================================
// Test Specification Types
// =========================================================================

export type OmegaTestCase = {
  name?: string;
  expr: string;
  expect: string;
};

export type OmegaTestSpec = {
  tag: "OmegaTests";
  cases: OmegaTestCase[];
  stepsLimit?: number;
};

export type CommandTestSpec = {
  tag: "CommandTests";
  argv: string[];
  cwd?: string;
};

export type TestSpec = OmegaTestSpec | CommandTestSpec;

// =========================================================================
// Test Report Types
// =========================================================================

export type TestCaseResult = {
  name: string;
  passed: boolean;
  actual?: string;
  expected?: string;
  error?: string;
};

export type TestReport = {
  tag: "TestReport";
  passed: boolean;
  cases: TestCaseResult[];
};

// =========================================================================
// Dependency Injection Types
// =========================================================================

export type EvalInEnv = (
  exprText: string,
  envRef: Hash,
  opts?: { stepsLimit?: number }
) => Promise<{ v: Val; envRef: Hash }>;

export type ShowVal = (v: Val) => string;

export type RunCommand = (
  argv: string[],
  cwd?: string
) => Promise<{ ok: boolean; stdout: string; stderr: string; code: number | null }>;

// =========================================================================
// Test Runner
// =========================================================================

export class TestRunner {
  constructor(
    private readonly evalInEnv: EvalInEnv,
    private readonly showVal: ShowVal,
    private readonly runCommand: RunCommand,
  ) {}

  async run(
    spec: TestSpec,
    envRef: Hash,
    caps: CapSet,
    budget: Budget,
  ): Promise<{ report: TestReport; budget: Budget }> {
    capRequire(caps, "test", "ReqTest");
    let b = budgetConsumeToolCall(budget); // Tests count as tool calls for budget

    if (spec.tag === "OmegaTests") {
      const cases: TestCaseResult[] = [];

      for (const tc of spec.cases) {
        const name = tc.name ?? tc.expr;
        try {
          const { v: actual } = await this.evalInEnv(tc.expr, envRef, { stepsLimit: spec.stepsLimit });
          const { v: expected } = await this.evalInEnv(tc.expect, envRef, { stepsLimit: spec.stepsLimit });

          const aS = this.showVal(actual);
          const eS = this.showVal(expected);
          const ok = aS === eS;

          cases.push({ name, passed: ok, actual: aS, expected: eS });
        } catch (e: unknown) {
          const msg = e instanceof Error ? e.message : String(e);
          cases.push({ name, passed: false, error: msg });
        }
      }

      const report: TestReport = {
        tag: "TestReport",
        passed: cases.every(c => c.passed),
        cases,
      };

      return { report, budget: b };
    }

    if (spec.tag === "CommandTests") {
      const { ok, stdout, stderr, code } = await this.runCommand(spec.argv, spec.cwd);

      const report: TestReport = {
        tag: "TestReport",
        passed: ok,
        cases: [{
          name: `command: ${spec.argv.join(" ")}`,
          passed: ok,
          actual: stdout.slice(0, 10_000),
          expected: "exitCode=0",
          error: ok ? undefined : (stderr.slice(0, 10_000) || `exitCode=${code}`),
        }],
      };

      return { report, budget: b };
    }

    // Unknown spec type
    const report: TestReport = {
      tag: "TestReport",
      passed: false,
      cases: [{ name: "unknown TestSpec", passed: false, error: "unknown TestSpec tag" }],
    };

    return { report, budget: b };
  }
}

// =========================================================================
// Convenience Factory
// =========================================================================

export function createTestRunner(
  evalInEnv: EvalInEnv,
  showVal: ShowVal,
  runCommand: RunCommand,
): TestRunner {
  return new TestRunner(evalInEnv, showVal, runCommand);
}
