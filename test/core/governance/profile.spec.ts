// test/core/governance/profile.spec.ts
// Tests for execution profiles with truth regimes
// Updated for Prompt 9: Full governance with canonical profiles

import { describe, it, expect } from "vitest";
import {
  PROFILE_SPECULATIVE,
  PROFILE_TEST_CERTIFIED,
  PROFILE_PROOF_CERTIFIED,
  PROFILE_EXPLORE,
  PROFILE_PRAGMATIC,
  PROFILE_STRICT,
  PROFILE_AIRGAP,
  DEFAULT_PROFILE,
  makeProfile,
  makeRuntimeBudget,
  getProfile,
  getProfileOrDefault,
} from "../../../src/core/governance/profile";

describe("profile system", () => {
  describe("PROFILE_SPECULATIVE (legacy alias for PROFILE_EXPLORE)", () => {
    it("has speculative truth regime", () => {
      expect(PROFILE_SPECULATIVE.truth).toBe("speculative");
      expect(PROFILE_SPECULATIVE.truthRegime).toBe("speculative");
    });

    it("has explore capabilities", () => {
      expect(PROFILE_SPECULATIVE.allowedCaps.has("cap.eval")).toBe(true);
      expect(PROFILE_SPECULATIVE.allowedCaps.has("cap.infer")).toBe(true);
      expect(PROFILE_SPECULATIVE.allowedCaps.has("cap.observe")).toBe(true);
    });

    it("has reasonable budget limits", () => {
      expect(PROFILE_SPECULATIVE.budgets.maxOracleTurns).toBe(1000);
      expect(PROFILE_SPECULATIVE.budgets.maxEvalSteps).toBe(100_000);
    });

    it("is the same as PROFILE_EXPLORE", () => {
      expect(PROFILE_SPECULATIVE).toBe(PROFILE_EXPLORE);
    });
  });

  describe("PROFILE_TEST_CERTIFIED (legacy alias for PROFILE_PRAGMATIC)", () => {
    it("has test-certified truth regime", () => {
      expect(PROFILE_TEST_CERTIFIED.truth).toBe("test-certified");
      expect(PROFILE_TEST_CERTIFIED.truthRegime).toBe("test-certified");
    });

    it("has test capability", () => {
      expect(PROFILE_TEST_CERTIFIED.allowedCaps.has("cap.test")).toBe(true);
    });

    it("has commit capabilities", () => {
      expect(PROFILE_TEST_CERTIFIED.allowedCaps.has("cap.commit.rewrite")).toBe(true);
      expect(PROFILE_TEST_CERTIFIED.allowedCaps.has("cap.commit.method")).toBe(true);
    });

    it("has higher limits than speculative", () => {
      expect(PROFILE_TEST_CERTIFIED.budgets.maxOracleTurns).toBeGreaterThan(
        PROFILE_SPECULATIVE.budgets.maxOracleTurns
      );
      expect(PROFILE_TEST_CERTIFIED.budgets.maxEvalSteps).toBeGreaterThan(
        PROFILE_SPECULATIVE.budgets.maxEvalSteps
      );
    });

    it("is the same as PROFILE_PRAGMATIC", () => {
      expect(PROFILE_TEST_CERTIFIED).toBe(PROFILE_PRAGMATIC);
    });
  });

  describe("PROFILE_PROOF_CERTIFIED", () => {
    it("has proof-certified truth regime", () => {
      expect(PROFILE_PROOF_CERTIFIED.truth).toBe("proof-certified");
      expect(PROFILE_PROOF_CERTIFIED.truthRegime).toBe("proof-certified");
    });

    it("has full capabilities (wildcard)", () => {
      expect(PROFILE_PROOF_CERTIFIED.allowedCaps.has("*")).toBe(true);
    });

    it("has highest limits", () => {
      expect(PROFILE_PROOF_CERTIFIED.budgets.maxOracleTurns).toBeGreaterThan(
        PROFILE_TEST_CERTIFIED.budgets.maxOracleTurns
      );
    });
  });

  describe("DEFAULT_PROFILE", () => {
    it("is pragmatic by default (CI-like discipline)", () => {
      expect(DEFAULT_PROFILE).toBe(PROFILE_PRAGMATIC);
      expect(DEFAULT_PROFILE.name).toBe("pragmatic");
    });
  });

  describe("Canonical profiles (Prompt 9)", () => {
    it("PROFILE_EXPLORE allows infer but not commits", () => {
      expect(PROFILE_EXPLORE.allowedOps.has("infer.op")).toBe(true);
      expect(PROFILE_EXPLORE.allowedOps.has("commit.op")).toBe(false);
      expect(PROFILE_EXPLORE.budgets.maxCommits).toBe(0);
    });

    it("PROFILE_PRAGMATIC allows infer, test, and commit", () => {
      expect(PROFILE_PRAGMATIC.allowedOps.has("infer.op")).toBe(true);
      expect(PROFILE_PRAGMATIC.allowedOps.has("commit.op")).toBe(true);
      expect(PROFILE_PRAGMATIC.allowedOracleReqTags.has("ReqTest")).toBe(true);
    });

    it("PROFILE_STRICT has deterministic envelope", () => {
      expect(PROFILE_STRICT.deterministicEnvelope).toBe(true);
      expect(PROFILE_STRICT.allowedOps.has("amb.choose")).toBe(false);
    });

    it("PROFILE_AIRGAP forbids tools and commits", () => {
      expect(PROFILE_AIRGAP.budgets.maxToolCalls).toBe(0);
      expect(PROFILE_AIRGAP.budgets.maxCommits).toBe(0);
      expect(PROFILE_AIRGAP.allowedOracleReqTags.has("ReqTool")).toBe(false);
    });
  });

  describe("profile structure", () => {
    it("all profiles have required fields", () => {
      const profiles = [
        PROFILE_EXPLORE,
        PROFILE_PRAGMATIC,
        PROFILE_STRICT,
        PROFILE_AIRGAP,
        PROFILE_PROOF_CERTIFIED,
      ];
      for (const p of profiles) {
        expect(p.name).toBeDefined();
        expect(p.allowedCaps).toBeDefined();
        expect(p.allowedOps).toBeDefined();
        expect(p.allowedOracleReqTags).toBeDefined();
        expect(p.budgets).toBeDefined();
        expect(p.truthRegime).toBeDefined();
        expect(typeof p.deterministicEnvelope).toBe("boolean");
        // Legacy fields
        expect(p.caps).toBeDefined();
        expect(p.truth).toBeDefined();
      }
    });
  });

  describe("makeProfile helper", () => {
    it("creates a profile with correct structure", () => {
      const p = makeProfile({
        name: "test-profile",
        caps: ["cap.eval", "cap.test"],
        ops: ["infer.op"],
        oracleReqs: ["ReqEval"],
        budgets: {
          maxOracleTurns: 100,
          maxEvalSteps: 1000,
          maxToolCalls: 0,
          maxNestedDepth: 4,
        },
        truth: "speculative",
      });

      expect(p.name).toBe("test-profile");
      expect(p.allowedCaps.has("cap.eval")).toBe(true);
      expect(p.allowedOps.has("infer.op")).toBe(true);
      expect(p.allowedOracleReqTags.has("ReqEval")).toBe(true);
      expect(p.truthRegime).toBe("speculative");
      expect(p.deterministicEnvelope).toBe(false);
    });
  });

  describe("makeRuntimeBudget helper", () => {
    it("creates runtime budget from profile", () => {
      const budget = makeRuntimeBudget(PROFILE_PRAGMATIC);

      expect(budget.stepsLeft).toBe(PROFILE_PRAGMATIC.budgets.maxEvalSteps);
      expect(budget.inferCallsLeft).toBe(PROFILE_PRAGMATIC.budgets.maxOracleTurns);
      expect(budget.toolCallsLeft).toBe(PROFILE_PRAGMATIC.budgets.maxToolCalls);
    });
  });

  describe("profile lookup", () => {
    it("getProfile returns profile by name", () => {
      expect(getProfile("explore")).toBe(PROFILE_EXPLORE);
      expect(getProfile("pragmatic")).toBe(PROFILE_PRAGMATIC);
      expect(getProfile("strict")).toBe(PROFILE_STRICT);
      expect(getProfile("airgap")).toBe(PROFILE_AIRGAP);
    });

    it("getProfile returns undefined for unknown name", () => {
      expect(getProfile("nonexistent")).toBeUndefined();
    });

    it("getProfileOrDefault returns default for unknown name", () => {
      expect(getProfileOrDefault("nonexistent")).toBe(DEFAULT_PROFILE);
    });
  });
});
