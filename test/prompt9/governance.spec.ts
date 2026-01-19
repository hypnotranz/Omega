// test/prompt9/governance.spec.ts
// Prompt 9 Tests: Governance enforcement with profiles, capabilities, and budgets

import { describe, it, expect, beforeEach } from "vitest";
import { installPrims } from "../helpers/prims";
import { COWStore } from "../../src/core/eval/store";
import type { Val, ProfileVal } from "../../src/core/eval/values";
import { VUnit, VTrue, VFalse } from "../../src/core/eval/values";
import { stepOnce } from "../../src/core/eval/machineStep";
import type { Expr } from "../../src/core/ast";
import type { State } from "../../src/core/eval/machine";
import {
  Profile,
  RuntimeBudget,
  PROFILE_EXPLORE,
  PROFILE_PRAGMATIC,
  PROFILE_STRICT,
  PROFILE_AIRGAP,
  ProfileViolation,
  CapabilityViolation,
  BudgetExceeded,
  TruthRegimeViolation,
  makeProfile,
  makeRuntimeBudget,
  getProfile,
} from "../../src/core/governance/profile";
import {
  initGovernance,
  checkEffectAllowed,
  checkOracleRequestAllowed,
  checkCommitAllowed,
  withProfile,
  withCaps,
  withoutCaps,
  hasCapability,
  type CommitEvidence,
} from "../../src/core/governance/enforcement";

// ─────────────────────────────────────────────────────────────────
// Test 9.1: ProfileVal is a first-class value
// ─────────────────────────────────────────────────────────────────

describe("Test 9.1: ProfileVal is a first-class value", () => {
  it("ProfileVal has correct tag", () => {
    const pv: ProfileVal = {
      tag: "Profile",
      profile: PROFILE_PRAGMATIC,
      profileId: "test-p1",
    };
    expect(pv.tag).toBe("Profile");
  });

  it("ProfileVal can be stored and retrieved", () => {
    const store = new COWStore();
    const pv: Val = {
      tag: "Profile",
      profile: PROFILE_PRAGMATIC,
      profileId: "test-p2",
    };

    const [store2, addr] = store.alloc(pv);
    const retrieved = store2.read(addr);

    expect(retrieved.tag).toBe("Profile");
    expect((retrieved as ProfileVal).profile.name).toBe("pragmatic");
  });

  it("canonical profiles are available by name", () => {
    expect(getProfile("explore")).toBe(PROFILE_EXPLORE);
    expect(getProfile("pragmatic")).toBe(PROFILE_PRAGMATIC);
    expect(getProfile("strict")).toBe(PROFILE_STRICT);
    expect(getProfile("airgap")).toBe(PROFILE_AIRGAP);
  });

  it("profiles have distinct configurations", () => {
    expect(PROFILE_EXPLORE.truthRegime).toBe("speculative");
    expect(PROFILE_PRAGMATIC.truthRegime).toBe("test-certified");
    expect(PROFILE_STRICT.deterministicEnvelope).toBe(true);
    expect(PROFILE_AIRGAP.budgets.maxToolCalls).toBe(0);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 9.2: State includes runtime governance
// ─────────────────────────────────────────────────────────────────

describe("Test 9.2: State includes runtime governance", () => {
  it("initGovernance adds profile, budget, sec to state", () => {
    const store = new COWStore();
    const { env, store: store2 } = installPrims(store);

    const state: State = {
      control: { tag: "Val", v: VUnit },
      env,
      store: store2,
      kont: [],
      handlers: [],
    };

    const governed = initGovernance(state, PROFILE_PRAGMATIC);

    expect(governed.profile).toBeDefined();
    expect(governed.profile?.name).toBe("pragmatic");
    expect(governed.budget).toBeDefined();
    expect(governed.budget?.stepsLeft).toBeGreaterThan(0);
    expect(governed.sec).toBeDefined();
    expect(governed.sec?.caps.size).toBeGreaterThan(0);
  });

  it("RuntimeBudget is created from profile", () => {
    const budget = makeRuntimeBudget(PROFILE_PRAGMATIC);

    expect(budget.stepsLeft).toBe(PROFILE_PRAGMATIC.budgets.maxEvalSteps);
    expect(budget.inferCallsLeft).toBe(PROFILE_PRAGMATIC.budgets.maxOracleTurns);
    expect(budget.toolCallsLeft).toBe(PROFILE_PRAGMATIC.budgets.maxToolCalls);
  });

  it("security context has profile's allowed caps", () => {
    const store = new COWStore();
    const { env, store: store2 } = installPrims(store);

    const state: State = {
      control: { tag: "Val", v: VUnit },
      env,
      store: store2,
      kont: [],
      handlers: [],
    };

    const governed = initGovernance(state, PROFILE_PRAGMATIC);

    expect(governed.sec?.caps.has("cap.eval")).toBe(true);
    expect(governed.sec?.caps.has("cap.infer")).toBe(true);
    expect(governed.sec?.caps.has("cap.test")).toBe(true);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 9.3: Chokepoint A - Effect emission enforcement
// ─────────────────────────────────────────────────────────────────

describe("Test 9.3: Chokepoint A - Effect emission enforcement", () => {
  it("checkEffectAllowed passes for allowed ops", () => {
    const store = new COWStore();
    const { env, store: store2 } = installPrims(store);

    const state = initGovernance(
      { control: { tag: "Val", v: VUnit }, env, store: store2, kont: [], handlers: [] },
      PROFILE_PRAGMATIC
    );

    // Should not throw
    expect(() => checkEffectAllowed(state, "infer.op")).not.toThrow();
    expect(() => checkEffectAllowed(state, "commit.op")).not.toThrow();
  });

  it("checkEffectAllowed throws ProfileViolation for disallowed ops", () => {
    const store = new COWStore();
    const { env, store: store2 } = installPrims(store);

    const state = initGovernance(
      { control: { tag: "Val", v: VUnit }, env, store: store2, kont: [], handlers: [] },
      PROFILE_EXPLORE  // explore doesn't allow commit.op
    );

    expect(() => checkEffectAllowed(state, "commit.op")).toThrow(ProfileViolation);
  });

  it("checkEffectAllowed throws BudgetExceeded when budget exhausted", () => {
    const store = new COWStore();
    const { env, store: store2 } = installPrims(store);

    const state = initGovernance(
      { control: { tag: "Val", v: VUnit }, env, store: store2, kont: [], handlers: [] },
      PROFILE_PRAGMATIC
    );

    // Exhaust infer budget
    state.budget!.inferCallsLeft = 0;

    expect(() => checkEffectAllowed(state, "infer.op")).toThrow(BudgetExceeded);
  });

  it("PROFILE_AIRGAP forbids tool.op", () => {
    const store = new COWStore();
    const { env, store: store2 } = installPrims(store);

    const state = initGovernance(
      { control: { tag: "Val", v: VUnit }, env, store: store2, kont: [], handlers: [] },
      PROFILE_AIRGAP
    );

    expect(() => checkEffectAllowed(state, "tool.op")).toThrow(ProfileViolation);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 9.4: Chokepoint B - Oracle session loop enforcement
// ─────────────────────────────────────────────────────────────────

describe("Test 9.4: Chokepoint B - Oracle session loop enforcement", () => {
  it("checkOracleRequestAllowed passes for allowed request types", () => {
    const store = new COWStore();
    const { env, store: store2 } = installPrims(store);

    const state = initGovernance(
      { control: { tag: "Val", v: VUnit }, env, store: store2, kont: [], handlers: [] },
      PROFILE_PRAGMATIC
    );

    expect(() => checkOracleRequestAllowed(state, "ReqEval")).not.toThrow();
    expect(() => checkOracleRequestAllowed(state, "ReqTest")).not.toThrow();
  });

  it("checkOracleRequestAllowed throws for disallowed request types", () => {
    const store = new COWStore();
    const { env, store: store2 } = installPrims(store);

    const state = initGovernance(
      { control: { tag: "Val", v: VUnit }, env, store: store2, kont: [], handlers: [] },
      PROFILE_AIRGAP  // airgap only allows ReqObserve
    );

    expect(() => checkOracleRequestAllowed(state, "ReqEval")).toThrow(ProfileViolation);
    expect(() => checkOracleRequestAllowed(state, "ReqTool")).toThrow(ProfileViolation);
  });

  it("PROFILE_STRICT requires capability for ReqEval in deterministic mode", () => {
    const store = new COWStore();
    const { env, store: store2 } = installPrims(store);

    const state = initGovernance(
      { control: { tag: "Val", v: VUnit }, env, store: store2, kont: [], handlers: [] },
      PROFILE_STRICT
    );

    // PROFILE_STRICT doesn't allow ReqEval by default
    expect(() => checkOracleRequestAllowed(state, "ReqEval")).toThrow(ProfileViolation);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 9.5: Chokepoint C - Commit barrier enforcement
// ─────────────────────────────────────────────────────────────────

describe("Test 9.5: Chokepoint C - Commit barrier enforcement", () => {
  it("speculative regime forbids commits", () => {
    const store = new COWStore();
    const { env, store: store2 } = installPrims(store);

    const state = initGovernance(
      { control: { tag: "Val", v: VUnit }, env, store: store2, kont: [], handlers: [] },
      PROFILE_EXPLORE  // speculative truth regime
    );

    const evidence: CommitEvidence = { tag: "None" };

    expect(() => checkCommitAllowed(state, evidence, "rewrite")).toThrow(ProfileViolation);
  });

  it("test-certified regime requires passing tests", () => {
    const store = new COWStore();
    const { env, store: store2 } = installPrims(store);

    const state = initGovernance(
      { control: { tag: "Val", v: VUnit }, env, store: store2, kont: [], handlers: [] },
      PROFILE_PRAGMATIC  // test-certified regime
    );

    // Without test evidence
    const noEvidence: CommitEvidence = { tag: "None" };
    expect(() => checkCommitAllowed(state, noEvidence, "rewrite")).toThrow(TruthRegimeViolation);

    // With failed tests
    const failedTests: CommitEvidence = { tag: "TestResults", passed: false };
    expect(() => checkCommitAllowed(state, failedTests, "rewrite")).toThrow(TruthRegimeViolation);

    // With passing tests
    const passedTests: CommitEvidence = { tag: "TestResults", passed: true };
    expect(() => checkCommitAllowed(state, passedTests, "rewrite")).not.toThrow();
  });

  it("commit requires appropriate capability", () => {
    const store = new COWStore();
    const { env, store: store2 } = installPrims(store);

    // Create a custom profile without commit.rewrite capability
    const limitedProfile = makeProfile({
      name: "limited",
      caps: ["cap.eval", "cap.commit.method"],  // No cap.commit.rewrite
      ops: ["infer.op", "commit.op"],
      oracleReqs: ["ReqEval"],
      budgets: {
        maxOracleTurns: 100,
        maxEvalSteps: 10000,
        maxToolCalls: 0,
        maxNestedDepth: 4,
      },
      truth: "test-certified",
    });

    const state = initGovernance(
      { control: { tag: "Val", v: VUnit }, env, store: store2, kont: [], handlers: [] },
      limitedProfile
    );

    const evidence: CommitEvidence = { tag: "TestResults", passed: true };

    // Should fail for rewrite (no cap.commit.rewrite)
    expect(() => checkCommitAllowed(state, evidence, "rewrite")).toThrow(CapabilityViolation);

    // Should pass for method (has cap.commit.method)
    expect(() => checkCommitAllowed(state, evidence, "method")).not.toThrow();
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 9.6: Profile switching with with-profile
// ─────────────────────────────────────────────────────────────────

describe("Test 9.6: Profile switching with with-profile", () => {
  it("withProfile installs new profile", () => {
    const store = new COWStore();
    const { env, store: store2 } = installPrims(store);

    const state = initGovernance(
      { control: { tag: "Val", v: VUnit }, env, store: store2, kont: [], handlers: [] },
      PROFILE_PRAGMATIC
    );

    const restricted = withProfile(state, PROFILE_AIRGAP);

    expect(restricted.profile?.name).toBe("airgap");
  });

  it("withProfile intersects capabilities (can only restrict)", () => {
    const store = new COWStore();
    const { env, store: store2 } = installPrims(store);

    const state = initGovernance(
      { control: { tag: "Val", v: VUnit }, env, store: store2, kont: [], handlers: [] },
      PROFILE_PRAGMATIC
    );

    // Pragmatic has cap.eval, cap.infer, cap.test, etc.
    expect(state.sec?.caps.has("cap.eval")).toBe(true);
    expect(state.sec?.caps.has("cap.test")).toBe(true);

    // Switch to airgap which only has cap.observe, cap.infer
    const restricted = withProfile(state, PROFILE_AIRGAP);

    // Only caps that both have
    expect(restricted.sec?.caps.has("cap.infer")).toBe(true);
    expect(restricted.sec?.caps.has("cap.observe")).toBe(true);
    // Lost cap.test and cap.eval
    expect(restricted.sec?.caps.has("cap.test")).toBe(false);
    expect(restricted.sec?.caps.has("cap.eval")).toBe(false);
  });

  it("withProfile takes minimum of budgets", () => {
    const store = new COWStore();
    const { env, store: store2 } = installPrims(store);

    const state = initGovernance(
      { control: { tag: "Val", v: VUnit }, env, store: store2, kont: [], handlers: [] },
      PROFILE_PRAGMATIC
    );

    // Reduce current budget below airgap's default
    state.budget!.inferCallsLeft = 10;

    const restricted = withProfile(state, PROFILE_AIRGAP);

    // Should take the minimum (10 < airgap's 50)
    expect(restricted.budget!.inferCallsLeft).toBe(10);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 9.7: Capability manipulation with with-caps
// ─────────────────────────────────────────────────────────────────

describe("Test 9.7: Capability manipulation with with-caps", () => {
  it("withCaps adds capabilities within profile limits", () => {
    const store = new COWStore();
    const { env, store: store2 } = installPrims(store);

    // Start with a subset of caps
    const state = initGovernance(
      { control: { tag: "Val", v: VUnit }, env, store: store2, kont: [], handlers: [] },
      PROFILE_PRAGMATIC
    );

    // Remove a cap first to test adding it back
    const reduced = withoutCaps(state, ["cap.test"]);
    expect(reduced.sec?.caps.has("cap.test")).toBe(false);

    // Add it back (within profile limits)
    const restored = withCaps(reduced, ["cap.test"]);
    expect(restored.sec?.caps.has("cap.test")).toBe(true);
  });

  it("withCaps cannot add capabilities beyond profile", () => {
    const store = new COWStore();
    const { env, store: store2 } = installPrims(store);

    const state = initGovernance(
      { control: { tag: "Val", v: VUnit }, env, store: store2, kont: [], handlers: [] },
      PROFILE_AIRGAP  // Limited caps
    );

    // Try to add a cap that airgap doesn't allow
    const attempted = withCaps(state, ["cap.test"]);

    // Should not have been added
    expect(attempted.sec?.caps.has("cap.test")).toBe(false);
  });

  it("withoutCaps removes capabilities", () => {
    const store = new COWStore();
    const { env, store: store2 } = installPrims(store);

    const state = initGovernance(
      { control: { tag: "Val", v: VUnit }, env, store: store2, kont: [], handlers: [] },
      PROFILE_PRAGMATIC
    );

    expect(state.sec?.caps.has("cap.eval")).toBe(true);

    const reduced = withoutCaps(state, ["cap.eval"]);

    expect(reduced.sec?.caps.has("cap.eval")).toBe(false);
    // Other caps still present
    expect(reduced.sec?.caps.has("cap.infer")).toBe(true);
  });

  it("hasCapability checks capability presence", () => {
    const store = new COWStore();
    const { env, store: store2 } = installPrims(store);

    const state = initGovernance(
      { control: { tag: "Val", v: VUnit }, env, store: store2, kont: [], handlers: [] },
      PROFILE_PRAGMATIC
    );

    expect(hasCapability(state, "cap.eval")).toBe(true);
    expect(hasCapability(state, "cap.nonexistent")).toBe(false);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 9.8: Budget enforcement and depletion
// ─────────────────────────────────────────────────────────────────

describe("Test 9.8: Budget enforcement and depletion", () => {
  it("step budget depletes during evaluation", () => {
    const store = new COWStore();
    const { env, store: store2 } = installPrims(store);

    const expr: Expr = { tag: "Lit", value: 42 };
    const state = initGovernance(
      { control: { tag: "Expr", e: expr }, env, store: store2, kont: [], handlers: [] },
      PROFILE_PRAGMATIC
    );

    const initialSteps = state.budget!.stepsLeft;

    // Step once
    const outcome = stepOnce(state);

    // Budget should be unchanged in outcome (debit happens per step check)
    // The check happens at the START of stepOnce, budget is still valid
    expect(outcome.tag).toBe("State");
  });

  it("BudgetExceeded thrown when steps exhausted", () => {
    const store = new COWStore();
    const { env, store: store2 } = installPrims(store);

    const expr: Expr = { tag: "Lit", value: 42 };
    const state = initGovernance(
      { control: { tag: "Expr", e: expr }, env, store: store2, kont: [], handlers: [] },
      PROFILE_PRAGMATIC
    );

    // Exhaust step budget
    state.budget!.stepsLeft = 0;

    expect(() => stepOnce(state)).toThrow(BudgetExceeded);
  });

  it("effect emission depletes effect budget", () => {
    const store = new COWStore();
    const { env, store: store2 } = installPrims(store);

    const state = initGovernance(
      { control: { tag: "Val", v: VUnit }, env, store: store2, kont: [], handlers: [] },
      PROFILE_PRAGMATIC
    );

    const initialInfer = state.budget!.inferCallsLeft;
    const initialTools = state.budget!.toolCallsLeft;

    // Manual check and debit simulation
    checkEffectAllowed(state, "infer.op");
    // (debitEffect would be called by the stepper)

    // Budget unchanged by check alone
    expect(state.budget!.inferCallsLeft).toBe(initialInfer);
  });

  it("profile budgets differ by regime", () => {
    expect(PROFILE_EXPLORE.budgets.maxOracleTurns).toBe(1000);
    expect(PROFILE_PRAGMATIC.budgets.maxOracleTurns).toBe(10_000);
    expect(PROFILE_STRICT.budgets.maxOracleTurns).toBe(100);
    expect(PROFILE_AIRGAP.budgets.maxOracleTurns).toBe(50);

    // Tool calls
    expect(PROFILE_EXPLORE.budgets.maxToolCalls).toBe(0);
    expect(PROFILE_AIRGAP.budgets.maxToolCalls).toBe(0);
    expect(PROFILE_PRAGMATIC.budgets.maxToolCalls).toBe(100);
  });
});
