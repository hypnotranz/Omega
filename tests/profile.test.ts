import { describe, it, expect } from "vitest";
import { normalizeProfile } from "../src/core/governance/profile";
import type { Profile } from "../src/core/governance/profile";

describe("normalizeProfile", () => {
  it("hydrates legacy profile objects into full Profile instances", () => {
    const legacy = {
      name: "legacy",
      caps: ["cap.eval", "cap.infer"],
      budgets: {
        maxOracleTurns: 5,
        maxEvalSteps: 100,
        maxToolCalls: 0,
      },
      truth: "speculative" as const,
    };

    const normalized = normalizeProfile(legacy);

    expect(normalized.name).toBe("legacy");
    expect(normalized.allowedCaps.has("cap.eval")).toBe(true);
    expect(normalized.allowedOps.size).toBeGreaterThan(0);
    expect(normalized.allowedOracleReqTags.size).toBeGreaterThan(0);
    expect(normalized.truthRegime).toBe("speculative");
    expect(normalized.deterministicEnvelope).toBe(false);
  });

  it("returns already-normalized profiles unchanged", () => {
    const existing: Profile = {
      name: "p",
      allowedCaps: new Set(["cap.eval"]),
      allowedOps: new Set(["infer.op"]),
      allowedOracleReqTags: new Set(["ReqEval"]),
      budgets: {
        maxOracleTurns: 1,
        maxEvalSteps: 2,
        maxToolCalls: 3,
        maxNestedDepth: 1,
        maxCommits: 0,
      },
      truthRegime: "speculative",
      deterministicEnvelope: true,
    };

    const normalized = normalizeProfile(existing);
    expect(normalized).toBe(existing);
  });
});
