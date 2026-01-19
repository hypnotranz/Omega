// test/core/commit/commit.spec.ts
// Tests for commit barrier with truth regime gating

import { describe, it, expect } from "vitest";
import {
  canCommitMeaning,
  attachTestReport,
  isRegimeAtLeast,
  strictestRegime,
} from "../../../src/core/commit/commit";
import type { MeaningVal } from "../../../src/core/oracle/meaning";
import type { Profile } from "../../../src/core/governance/profile";
import type { TestReport } from "../../../src/core/test/testRunner";

const PROFILE_SPECULATIVE: Profile = {
  name: "speculative",
  caps: ["eval", "apply", "observe"],
  budgets: { maxOracleTurns: 1000, maxEvalSteps: 100_000, maxToolCalls: 100 },
  truth: "speculative",
};

const PROFILE_TEST_CERTIFIED: Profile = {
  name: "test-certified",
  caps: ["eval", "apply", "observe", "test", "tool.*", "commit.rewrite"],
  budgets: { maxOracleTurns: 10_000, maxEvalSteps: 500_000, maxToolCalls: 1000 },
  truth: "test-certified",
};

const PROFILE_PROOF_CERTIFIED: Profile = {
  name: "proof-certified",
  caps: ["*"],
  budgets: { maxOracleTurns: 100_000, maxEvalSteps: 5_000_000, maxToolCalls: 10_000 },
  truth: "proof-certified",
};

function makeMeaning(overrides: Partial<MeaningVal> = {}): MeaningVal {
  return {
    tag: "Meaning",
    confidence: 0.9,
    ...overrides,
  };
}

function makePassingReport(): TestReport {
  return {
    tag: "TestReport",
    passed: true,
    cases: [{ name: "test1", passed: true }],
  };
}

function makeFailingReport(): TestReport {
  return {
    tag: "TestReport",
    passed: false,
    cases: [{ name: "test1", passed: false, error: "failed" }],
  };
}

describe("canCommitMeaning", () => {
  describe("speculative regime", () => {
    it("allows commits without obligation", () => {
      const m = makeMeaning();
      const result = canCommitMeaning(PROFILE_SPECULATIVE, m);
      expect(result.ok).toBe(true);
    });

    it("allows commits with any obligation", () => {
      const m = makeMeaning({
        obligation: { tag: "OblTests", spec: {}, passed: false } as any,
      });
      const result = canCommitMeaning(PROFILE_SPECULATIVE, m);
      expect(result.ok).toBe(true);
    });
  });

  describe("test-certified regime", () => {
    it("rejects commits without obligation", () => {
      const m = makeMeaning();
      const result = canCommitMeaning(PROFILE_TEST_CERTIFIED, m);
      expect(result.ok).toBe(false);
      expect(result.why).toContain("missing obligation");
    });

    it("rejects commits with OblNone", () => {
      const m = makeMeaning({
        obligation: { tag: "OblNone" } as any,
      });
      const result = canCommitMeaning(PROFILE_TEST_CERTIFIED, m);
      expect(result.ok).toBe(false);
      expect(result.why).toContain("no obligation specified");
    });

    it("rejects commits with failed tests", () => {
      const m = makeMeaning({
        obligation: {
          tag: "OblTests",
          spec: {},
          passed: false,
          report: makeFailingReport(),
        } as any,
      });
      const result = canCommitMeaning(PROFILE_TEST_CERTIFIED, m);
      expect(result.ok).toBe(false);
      expect(result.why).toContain("not marked passed");
    });

    it("rejects commits with failing report", () => {
      const m = makeMeaning({
        obligation: {
          tag: "OblTests",
          spec: {},
          passed: true,
          report: makeFailingReport(),
        } as any,
      });
      const result = canCommitMeaning(PROFILE_TEST_CERTIFIED, m);
      expect(result.ok).toBe(false);
      expect(result.why).toContain("test report indicates failure");
    });

    it("allows commits with passing tests", () => {
      const m = makeMeaning({
        obligation: {
          tag: "OblTests",
          spec: {},
          passed: true,
          report: makePassingReport(),
        } as any,
      });
      const result = canCommitMeaning(PROFILE_TEST_CERTIFIED, m);
      expect(result.ok).toBe(true);
    });
  });
});

describe("attachTestReport", () => {
  it("creates OblTests when no obligation exists", () => {
    const m = makeMeaning();
    const report = makePassingReport();
    const result = attachTestReport(m, report);

    expect(result.obligation).toBeDefined();
    const obl = result.obligation as any;
    expect(obl.tag).toBe("OblTests");
    expect(obl.passed).toBe(true);
    expect(obl.report).toBe(report);
  });

  it("updates existing OblTests obligation", () => {
    const m = makeMeaning({
      obligation: {
        tag: "OblTests",
        spec: { tag: "OmegaTests", cases: [] },
        passed: false,
      } as any,
    });
    const report = makePassingReport();
    const result = attachTestReport(m, report);

    const obl = result.obligation as any;
    expect(obl.passed).toBe(true);
    expect(obl.report).toBe(report);
  });

  it("preserves other meaning fields", () => {
    const m = makeMeaning({
      denotation: { tag: "Num", n: 42 } as any,
      confidence: 0.95,
      rewrite: { tag: "Str", s: "code" } as any,
    });
    const report = makePassingReport();
    const result = attachTestReport(m, report);

    expect(result.denotation).toEqual({ tag: "Num", n: 42 });
    expect(result.confidence).toBe(0.95);
    expect(result.rewrite).toEqual({ tag: "Str", s: "code" });
  });
});

describe("isRegimeAtLeast", () => {
  it("speculative is at least speculative", () => {
    expect(isRegimeAtLeast("speculative", "speculative")).toBe(true);
  });

  it("test-certified is at least speculative", () => {
    expect(isRegimeAtLeast("test-certified", "speculative")).toBe(true);
  });

  it("proof-certified is at least test-certified", () => {
    expect(isRegimeAtLeast("proof-certified", "test-certified")).toBe(true);
  });

  it("speculative is not at least test-certified", () => {
    expect(isRegimeAtLeast("speculative", "test-certified")).toBe(false);
  });
});

describe("strictestRegime", () => {
  it("returns the stricter of two regimes", () => {
    expect(strictestRegime("speculative", "test-certified")).toBe("test-certified");
    expect(strictestRegime("test-certified", "speculative")).toBe("test-certified");
    expect(strictestRegime("proof-certified", "test-certified")).toBe("proof-certified");
  });

  it("returns same regime when equal", () => {
    expect(strictestRegime("test-certified", "test-certified")).toBe("test-certified");
  });
});
