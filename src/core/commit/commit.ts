// src/core/commit/commit.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-18.md
// Commit barrier with truth regime gating

import type { Profile, TruthRegime } from "../governance/profile";
import type { CapSet } from "../governance/caps";
import { capRequire } from "../governance/caps";
import type { Hash } from "../artifacts/hash";
import type { MeaningVal } from "../oracle/meaning";
import type { TestReport } from "../test/testRunner";

// =========================================================================
// Obligation Types
// =========================================================================

export type ObligationNone = { tag: "OblNone" };

export type ObligationTests = {
  tag: "OblTests";
  spec: unknown; // TestSpec
  passed?: boolean;
  report?: TestReport;
};

export type Obligation = ObligationNone | ObligationTests;

// =========================================================================
// Commit Gate
// =========================================================================

export type CommitGateResult = {
  ok: boolean;
  why?: string;
};

/**
 * Check if a Meaning can be committed under the given truth regime.
 *
 * - speculative: always allowed (no verification required)
 * - test-certified: requires obligation tests to be passed
 * - proof-certified: requires formal proof (stubbed for now)
 */
export function canCommitMeaning(profile: Profile, m: MeaningVal): CommitGateResult {
  // Speculative truth regime allows all commits
  if (profile.truth === "speculative") {
    return { ok: true };
  }

  // Test-certified and stricter require obligation discharge
  const obl = parseObligation(m.obligation);

  if (!obl) {
    return { ok: false, why: "missing obligation" };
  }

  if (obl.tag === "OblNone") {
    // No obligation specified - only allowed in speculative
    return { ok: false, why: "no obligation specified for non-speculative regime" };
  }

  if (obl.tag === "OblTests") {
    if (!obl.passed) {
      return { ok: false, why: "tests not marked passed" };
    }
    if (obl.report && !obl.report.passed) {
      return { ok: false, why: "test report indicates failure" };
    }
    return { ok: true };
  }

  return { ok: false, why: `unsupported obligation kind: ${(obl as Obligation).tag}` };
}

/**
 * Parse an obligation from the Meaning's obligation field.
 * Handles both Val representations and raw Obligation objects.
 */
function parseObligation(val: unknown): Obligation | null {
  if (!val) return null;

  if (typeof val === "object" && "tag" in val) {
    const v = val as { tag: string };
    if (v.tag === "OblNone" || v.tag === "OblTests") {
      return val as Obligation;
    }

    // Handle Val representation
    if (v.tag === "Map" && "entries" in val) {
      const entries = (val as { entries: Array<[unknown, unknown]> }).entries;
      const tagEntry = entries.find(([k]) =>
        typeof k === "object" && k && "tag" in k && (k as { tag: string }).tag === "Str" &&
        (k as { s: string }).s === "tag"
      );
      if (tagEntry) {
        const tagVal = tagEntry[1] as { tag: string; s?: string };
        if (tagVal.tag === "Str" && tagVal.s) {
          if (tagVal.s === "OblNone") return { tag: "OblNone" };
          if (tagVal.s === "OblTests") {
            // Extract passed and report from entries
            const passedEntry = entries.find(([k]) =>
              typeof k === "object" && k && "tag" in k && (k as { tag: string }).tag === "Str" &&
              (k as { s: string }).s === "passed"
            );
            const passed = passedEntry
              ? ((passedEntry[1] as { b?: boolean }).b ?? false)
              : false;
            return { tag: "OblTests", spec: {}, passed };
          }
        }
      }
    }
  }

  return null;
}

// =========================================================================
// Commit Rewrite
// =========================================================================

export type ApplyRewrite = (rewriteExpr: unknown, envRef: Hash) => Promise<Hash>;

/**
 * Commit a rewrite Meaning to the runtime environment.
 *
 * 1. Checks capability (cap.commit.rewrite)
 * 2. Checks truth regime gate
 * 3. Applies the rewrite to the environment
 */
export async function commitRewriteMeaning(
  profile: Profile,
  envRef: Hash,
  m: MeaningVal,
  applyRewrite: ApplyRewrite,
): Promise<Hash> {
  // Check capability
  capRequire(profile.caps, "commit.rewrite", "commit/rewrite");

  // Check truth regime gate
  const gate = canCommitMeaning(profile, m);
  if (!gate.ok) {
    throw new Error(`commit blocked by truth regime (${profile.truth}): ${gate.why}`);
  }

  // Ensure rewrite field exists
  if (!m.rewrite) {
    throw new Error("commit/rewrite: Meaning has no rewrite field");
  }

  // Apply rewrite by evaluating rewrite expr in envRef and returning new envRef
  return await applyRewrite(m.rewrite, envRef);
}

// =========================================================================
// Helper: Attach Test Report to Meaning
// =========================================================================

/**
 * Attach a test report to a Meaning's obligation field.
 * Creates an OblTests obligation if none exists.
 */
export function attachTestReport(m: MeaningVal, report: TestReport): MeaningVal {
  const obl = parseObligation(m.obligation);

  if (!obl || obl.tag !== "OblTests") {
    // Create new OblTests obligation
    return {
      ...m,
      obligation: {
        tag: "OblTests",
        spec: { tag: "OmegaTests", cases: [] },
        passed: report.passed,
        report,
      } as unknown as typeof m.obligation,
    };
  }

  // Update existing OblTests
  return {
    ...m,
    obligation: {
      ...obl,
      passed: report.passed,
      report,
    } as unknown as typeof m.obligation,
  };
}

// =========================================================================
// Truth Regime Helpers
// =========================================================================

/**
 * Check if a truth regime is at least as strict as another.
 */
export function isRegimeAtLeast(have: TruthRegime, need: TruthRegime): boolean {
  const order: Record<TruthRegime, number> = {
    "speculative": 0,
    "test-certified": 1,
    "proof-certified": 2,
  };
  return (order[have] ?? 0) >= (order[need] ?? 0);
}

/**
 * Get the strictest regime between two.
 */
export function strictestRegime(a: TruthRegime, b: TruthRegime): TruthRegime {
  const order: Record<TruthRegime, number> = {
    "speculative": 0,
    "test-certified": 1,
    "proof-certified": 2,
  };
  return order[a] >= order[b] ? a : b;
}
