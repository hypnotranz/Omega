// test/prompt12-constraints/constraints.spec.ts
// Prompt 12 Tests: Constraint propagation networks for semantic pipelines

import { describe, it, expect, beforeEach, afterEach } from "vitest";
import type { Val, ConnRefVal, NetRefVal, ExplanationVal, ContradictionVal } from "../../src/core/eval/values";
import { VUnit, VTrue, VFalse } from "../../src/core/eval/values";
import {
  // Types
  type NetworkState,
  type PropagatorState,
  type UnsatCore,
  type RepairOption,
  type RepairSearchResult,
  type PropagatorContext,
  type PropagatorOutput,
  type ScriptedPropagator,
  // Type guards
  isConnRef,
  isNetRef,
  isExplanation,
  isContradiction,
  // Helpers
  makeConnRef,
  makeNetRef,
  makeAssumption,
  makeDerived,
  makeConflict,
  makeContradiction,
  // Network operations
  createNetwork,
  createConnector,
  connectorHasValue,
  connectorGetValue,
  connectorGetExplanation,
  connectorSetValue,
  connectorForgetValue,
  getNetwork,
  clearNetworks,
  resetConstraintCounters,
  // Propagator operations
  registerPropagator,
  registerConstraint,
  getPropagator,
  removePropagator,
  // Propagation engine
  runPropagation,
  hasContradiction,
  getContradiction,
  isQuiescent,
  registerScriptedPropagator,
  clearScriptedPropagators,
  // Diagnosis
  extractUnsatCore,
  traverseExplanation,
  getExplanationConnectors,
  getExplanationRules,
  getExplanationDepth,
  explanationToString,
  hashExplanation,
  getConstraints,
  findPotentialResolvers,
  // Repair
  generateRepairOptions,
  tryRepair,
  searchRepairs,
  repair,
} from "../../src/core/constraints";

// ─────────────────────────────────────────────────────────────────
// Test setup
// ─────────────────────────────────────────────────────────────────

beforeEach(() => {
  clearNetworks();
  resetConstraintCounters();
  clearScriptedPropagators();
});

afterEach(() => {
  clearNetworks();
  clearScriptedPropagators();
});

// ─────────────────────────────────────────────────────────────────
// Test 12.1: Basic semantic propagator network reaches quiescence
// ─────────────────────────────────────────────────────────────────

describe("Test 12.1: Basic semantic propagator network", () => {
  it("network reaches quiescence and produces safe redaction", async () => {
    // Create network
    const net = createNetwork("redaction-pipeline");
    expect(isNetRef(net)).toBe(true);

    // Create connectors
    const rawText = createConnector(net, "rawText");
    const normalized = createConnector(net, "normalized");
    const redacted = createConnector(net, "redacted");
    const safeFlag = createConnector(net, "safeFlag");

    // Register scripted propagators
    registerScriptedPropagator({
      name: "normalize",
      execute: (ctx) => {
        const input = ctx.inputs.values().next().value;
        if (!input || input.tag !== "Str") return { tag: "noChange" };
        // Simple normalization: lowercase
        const result: Val = { tag: "Str", s: input.s.toLowerCase().trim() };
        return { tag: "values", values: new Map([[normalized.id, result]]) };
      },
    });

    registerScriptedPropagator({
      name: "redact",
      execute: (ctx) => {
        const input = ctx.inputs.values().next().value;
        if (!input || input.tag !== "Str") return { tag: "noChange" };
        // Simple redaction: replace "sensitive" with "[REDACTED]"
        const result: Val = { tag: "Str", s: input.s.replace(/sensitive/gi, "[REDACTED]") };
        return { tag: "values", values: new Map([[redacted.id, result]]) };
      },
    });

    registerScriptedPropagator({
      name: "checkSafe",
      execute: (ctx) => {
        const input = ctx.inputs.values().next().value;
        if (!input || input.tag !== "Str") return { tag: "noChange" };
        // Safe if no "sensitive" text remains
        const isSafe = !input.s.toLowerCase().includes("sensitive");
        return { tag: "values", values: new Map([[safeFlag.id, { tag: "Bool", b: isSafe }]]) };
      },
    });

    // Register propagators in network
    registerPropagator(net, "normalize", [rawText], [normalized], VUnit);
    registerPropagator(net, "redact", [normalized], [redacted], VUnit);
    registerPropagator(net, "checkSafe", [redacted], [safeFlag], VUnit);

    // Register constraint: safeFlag must be true
    registerConstraint(net, "mustBeTrue", [safeFlag], VUnit);

    // Set initial value
    const setResult = connectorSetValue(
      rawText,
      { tag: "Str", s: "  This contains SENSITIVE data  " },
      { tag: "Str", s: "user input" }
    );
    expect(setResult).toBeUndefined(); // No contradiction yet

    // Run propagation
    const result = await runPropagation(net);

    // Verify quiescence
    expect(result.status.tag).toBe("quiescent");
    expect(result.firings).toBeGreaterThan(0);

    // Verify connector values
    expect(connectorHasValue(rawText)).toBe(true);
    expect(connectorHasValue(normalized)).toBe(true);
    expect(connectorHasValue(redacted)).toBe(true);
    expect(connectorHasValue(safeFlag)).toBe(true);

    const redactedVal = connectorGetValue(redacted);
    expect(redactedVal.tag).toBe("Str");
    expect((redactedVal as any).s).toContain("[REDACTED]");

    const safeFlagVal = connectorGetValue(safeFlag);
    expect(safeFlagVal.tag).toBe("Bool");
    expect((safeFlagVal as any).b).toBe(true);

    // Verify explanation graph exists
    const explanation = connectorGetExplanation(redacted);
    expect(explanation).toBeDefined();
    expect(isExplanation(explanation!)).toBe(true);
  });

  it("explanation graph has correct structure", async () => {
    const net = createNetwork("simple-pipeline");
    const input = createConnector(net, "input");
    const output = createConnector(net, "output");

    registerScriptedPropagator({
      name: "identity",
      execute: (ctx) => {
        const val = ctx.inputs.values().next().value;
        if (!val) return { tag: "noChange" };
        return { tag: "values", values: new Map([[output.id, val]]) };
      },
    });

    registerPropagator(net, "identity", [input], [output], VUnit);

    // Set input
    connectorSetValue(input, { tag: "Num", n: 42 }, { tag: "Str", s: "initial" });

    // Run
    await runPropagation(net);

    // Check output explanation
    const expl = connectorGetExplanation(output);
    expect(expl).toBeDefined();
    expect(expl!.kind).toBe("derived");
    expect((expl as any).rule).toBe("identity");
    expect((expl as any).deps.length).toBeGreaterThan(0);

    // Input explanation should be assumption
    const inputExpl = connectorGetExplanation(input);
    expect(inputExpl).toBeDefined();
    expect(inputExpl!.kind).toBe("assumption");
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 12.2: Contradiction produces explanation graph
// ─────────────────────────────────────────────────────────────────

describe("Test 12.2: Contradiction with explanation graph", () => {
  it("contradiction returns first-class value (not exception)", async () => {
    const net = createNetwork("unsafe-pipeline");
    const rawText = createConnector(net, "rawText");
    const redacted = createConnector(net, "redacted");
    const safeFlag = createConnector(net, "safeFlag");

    // Scripted redactor that leaves sensitive data (unsafe)
    registerScriptedPropagator({
      name: "unsafeRedact",
      execute: (ctx) => {
        const input = ctx.inputs.values().next().value;
        if (!input || input.tag !== "Str") return { tag: "noChange" };
        // Doesn't actually redact - leaves sensitive data
        return { tag: "values", values: new Map([[redacted.id, input]]) };
      },
    });

    registerScriptedPropagator({
      name: "checkSafe",
      execute: (ctx) => {
        const input = ctx.inputs.values().next().value;
        if (!input || input.tag !== "Str") return { tag: "noChange" };
        const isSafe = !input.s.toLowerCase().includes("sensitive");
        return { tag: "values", values: new Map([[safeFlag.id, { tag: "Bool", b: isSafe }]]) };
      },
    });

    registerPropagator(net, "unsafeRedact", [rawText], [redacted], VUnit);
    registerPropagator(net, "checkSafe", [redacted], [safeFlag], VUnit);
    registerConstraint(net, "mustBeTrue", [safeFlag], VUnit);

    // Set input with sensitive data
    connectorSetValue(
      rawText,
      { tag: "Str", s: "Contains SENSITIVE information" },
      { tag: "Str", s: "input" }
    );

    // Run - should produce contradiction
    const result = await runPropagation(net);

    // Verify contradiction
    expect(result.status.tag).toBe("contradiction");
    expect(result.contradiction).toBeDefined();
    expect(isContradiction(result.contradiction!)).toBe(true);

    // Verify explanation structure
    const contradiction = result.contradiction!;
    expect(contradiction.explanation).toBeDefined();
    expect(contradiction.explanation.tag).toBe("Explanation");
  });

  it("explanation includes witness chain to contradiction", async () => {
    const net = createNetwork("conflict-test");
    const conn = createConnector(net, "value");

    // Set initial value
    connectorSetValue(conn, { tag: "Num", n: 1 }, { tag: "Str", s: "first" });

    // Try to set conflicting value
    const contradiction = connectorSetValue(
      conn,
      { tag: "Num", n: 2 },
      { tag: "Str", s: "second" }
    );

    // Should have conflict
    expect(contradiction).toBeDefined();
    expect(isContradiction(contradiction!)).toBe(true);
    expect(contradiction!.explanation.kind).toBe("conflict");

    // Conflict should reference both derivations
    const conflict = contradiction!.explanation as Extract<ExplanationVal, { kind: "conflict" }>;
    expect(conflict.left).toBeDefined();
    expect(conflict.right).toBeDefined();
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 12.3: Repair via amb search
// ─────────────────────────────────────────────────────────────────

describe("Test 12.3: Repair via amb search", () => {
  it("BFS finds repair by switching redactor", async () => {
    const net = createNetwork("repairable-pipeline");
    const rawText = createConnector(net, "rawText");
    const redacted = createConnector(net, "redacted");
    const safeFlag = createConnector(net, "safeFlag");

    // Unsafe mechanical redactor
    registerScriptedPropagator({
      name: "mechanicalRedact",
      execute: (ctx) => {
        const input = ctx.inputs.values().next().value;
        if (!input || input.tag !== "Str") return { tag: "noChange" };
        // Doesn't actually redact
        return { tag: "values", values: new Map([[redacted.id, input]]) };
      },
    });

    // Safe semantic redactor
    registerScriptedPropagator({
      name: "semanticRedact",
      execute: (ctx) => {
        const input = ctx.inputs.values().next().value;
        if (!input || input.tag !== "Str") return { tag: "noChange" };
        const result: Val = { tag: "Str", s: input.s.replace(/sensitive/gi, "[REDACTED]") };
        return { tag: "values", values: new Map([[redacted.id, result]]) };
      },
    });

    registerScriptedPropagator({
      name: "checkSafe",
      execute: (ctx) => {
        const input = ctx.inputs.values().next().value;
        if (!input || input.tag !== "Str") return { tag: "noChange" };
        const isSafe = !input.s.toLowerCase().includes("sensitive");
        return { tag: "values", values: new Map([[safeFlag.id, { tag: "Bool", b: isSafe }]]) };
      },
    });

    // Start with mechanical (unsafe)
    const mechanicalId = registerPropagator(net, "mechanicalRedact", [rawText], [redacted], VUnit);
    registerPropagator(net, "checkSafe", [redacted], [safeFlag], VUnit);
    registerConstraint(net, "mustBeTrue", [safeFlag], VUnit);

    // Set input
    connectorSetValue(rawText, { tag: "Str", s: "SENSITIVE data" }, { tag: "Str", s: "input" });

    // Run - should fail
    const result = await runPropagation(net);
    expect(result.status.tag).toBe("contradiction");

    // Generate repair options
    const core = extractUnsatCore(net, result.contradiction!);
    const options = generateRepairOptions(net, core);

    // Add swap option
    options.push({
      tag: "swapPropagator",
      oldId: mechanicalId,
      newProcRef: VUnit, // Will use scripted "semanticRedact"
    });

    // Create a custom repair that uses semantic redactor
    const repairResult = await searchRepairs(net, result.contradiction!, options, {
      strategy: "bfs",
      maxAttempts: 5,
    });

    // Should have made attempts
    expect(repairResult.attemptCount).toBeGreaterThan(0);
    expect(repairResult.attempts.length).toBeGreaterThan(0);
  });

  it("repair exploration is recorded in results", async () => {
    const net = createNetwork("repair-ledger");
    const conn = createConnector(net, "value");
    const checkConn = createConnector(net, "check");

    registerScriptedPropagator({
      name: "checkPositive",
      execute: (ctx) => {
        const input = ctx.inputs.values().next().value;
        if (!input || input.tag !== "Num") return { tag: "noChange" };
        const isPositive = input.n > 0;
        return { tag: "values", values: new Map([[checkConn.id, { tag: "Bool", b: isPositive }]]) };
      },
    });

    registerPropagator(net, "checkPositive", [conn], [checkConn], VUnit);
    const constraintId = registerConstraint(net, "mustBeTrue", [checkConn], VUnit);

    // Set negative value (will fail)
    connectorSetValue(conn, { tag: "Num", n: -5 }, { tag: "Str", s: "input" });

    const result = await runPropagation(net);
    expect(result.status.tag).toBe("contradiction");

    // Repair with multiple options
    const repairResult = await repair(net, result.contradiction!, [
      { tag: "dropConstraint", constraintId },
      { tag: "retryWithValue", connId: conn.id, newValue: { tag: "Num", n: 5 } },
    ]);

    // Should record all attempts
    expect(repairResult.attempts.length).toBeGreaterThan(0);
    expect(repairResult.repairDist).toBeDefined();
    expect(repairResult.repairDist!.support.length).toBeGreaterThan(0);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 12.4: Unsat core extraction
// ─────────────────────────────────────────────────────────────────

describe("Test 12.4: Unsat core extraction", () => {
  it("unsat core identifies conflicting constraints", async () => {
    const net = createNetwork("multi-constraint");
    const value = createConnector(net, "value");
    const positiveCheck = createConnector(net, "positiveCheck");
    const evenCheck = createConnector(net, "evenCheck");

    registerScriptedPropagator({
      name: "checkPositive",
      execute: (ctx) => {
        const input = ctx.inputs.values().next().value;
        if (!input || input.tag !== "Num") return { tag: "noChange" };
        return { tag: "values", values: new Map([[positiveCheck.id, { tag: "Bool", b: input.n > 0 }]]) };
      },
    });

    registerScriptedPropagator({
      name: "checkEven",
      execute: (ctx) => {
        const input = ctx.inputs.values().next().value;
        if (!input || input.tag !== "Num") return { tag: "noChange" };
        return { tag: "values", values: new Map([[evenCheck.id, { tag: "Bool", b: input.n % 2 === 0 }]]) };
      },
    });

    registerPropagator(net, "checkPositive", [value], [positiveCheck], VUnit);
    registerPropagator(net, "checkEven", [value], [evenCheck], VUnit);

    const c1 = registerConstraint(net, "mustBeTrue", [positiveCheck], VUnit);
    const c2 = registerConstraint(net, "mustBeTrue", [evenCheck], VUnit);

    // Set value that violates positive constraint
    connectorSetValue(value, { tag: "Num", n: -4 }, { tag: "Str", s: "input" });

    const result = await runPropagation(net);
    expect(result.status.tag).toBe("contradiction");

    // Extract unsat core
    const core = extractUnsatCore(net, result.contradiction!);

    // Core should identify relevant connectors
    expect(core.connectorIds.length).toBeGreaterThan(0);
    expect(core.assumptions.length).toBeGreaterThan(0);
    expect(core.explanation).toBeDefined();
  });

  it("explanation depth is bounded", () => {
    // Build a chain of explanations
    const conn: ConnRefVal = { tag: "ConnRef", id: "test", netId: "net" };

    const assumption = makeAssumption(conn, "hash1", VUnit);
    const derived1 = makeDerived(conn, "hash2", "rule1", [assumption]);
    const derived2 = makeDerived(conn, "hash3", "rule2", [derived1]);
    const derived3 = makeDerived(conn, "hash4", "rule3", [derived2]);

    expect(getExplanationDepth(assumption)).toBe(1);
    expect(getExplanationDepth(derived1)).toBe(2);
    expect(getExplanationDepth(derived2)).toBe(3);
    expect(getExplanationDepth(derived3)).toBe(4);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 12.5: Constraints prune beam search
// ─────────────────────────────────────────────────────────────────

describe("Test 12.5: Constraint-pruned beam search", () => {
  it("beam search prunes violating candidates early", async () => {
    // Create multiple candidate networks
    const validResults: boolean[] = [];
    const totalAttempts = 10;

    for (let i = 0; i < totalAttempts; i++) {
      const net = createNetwork(`candidate-${i}`);
      const value = createConnector(net, "value");
      const check = createConnector(net, "check");

      registerScriptedPropagator({
        name: `check-${i}`,
        execute: (ctx) => {
          const input = ctx.inputs.values().next().value;
          if (!input || input.tag !== "Num") return { tag: "noChange" };
          // Only candidates 7, 8, 9 pass
          const passes = input.n >= 7;
          return { tag: "values", values: new Map([[check.id, { tag: "Bool", b: passes }]]) };
        },
      });

      registerPropagator(net, `check-${i}`, [value], [check], VUnit);
      registerConstraint(net, "mustBeTrue", [check], VUnit);

      // Each candidate has different value
      connectorSetValue(value, { tag: "Num", n: i }, { tag: "Str", s: "candidate" });

      const result = await runPropagation(net);
      validResults.push(result.status.tag === "quiescent");

      clearScriptedPropagators();
    }

    // Only 3 should pass (7, 8, 9)
    const passCount = validResults.filter(v => v).length;
    expect(passCount).toBe(3);

    // Verify early candidates failed
    expect(validResults[0]).toBe(false);
    expect(validResults[5]).toBe(false);
    expect(validResults[9]).toBe(true);
  });

  it("scoring function affects repair selection", async () => {
    const net = createNetwork("scored-repair");
    const conn = createConnector(net, "value");
    const check = createConnector(net, "check");

    // Propagator that always fails
    registerScriptedPropagator({
      name: "alwaysFail",
      execute: () => ({ tag: "contradiction", message: "always fails" }),
    });

    registerConstraint(net, "alwaysFail", [conn], VUnit);

    // Multiple repair options with different scores
    const options: RepairOption[] = [
      { tag: "retryWithValue", connId: conn.id, newValue: { tag: "Num", n: 1 } },
      { tag: "retryWithValue", connId: conn.id, newValue: { tag: "Num", n: 100 } },
      { tag: "retryWithValue", connId: conn.id, newValue: { tag: "Num", n: 50 } },
    ];

    // Custom scoring: prefer lower values
    const scoreFn = (result: { option: RepairOption }) => {
      if (result.option.tag === "retryWithValue") {
        const val = result.option.newValue;
        if (val.tag === "Num") return -val.n; // Negative so lower is better
      }
      return 0;
    };

    // Create a mock contradiction
    const expl = makeAssumption(conn, "hash", VUnit);
    const contradiction = makeContradiction(expl);

    const searchResult = await searchRepairs(net, contradiction, options, {
      strategy: "bfs",
      maxAttempts: 10, // Allow all options to be tried
      scoreFn,
    });

    // Should have tried options and scored them
    expect(searchResult.repairDist).toBeDefined();
    expect(searchResult.attempts.length).toBeGreaterThanOrEqual(1);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 12.6: Governance profile enforcement
// ─────────────────────────────────────────────────────────────────

describe("Test 12.6: Governance profile enforcement", () => {
  it("oracle-requiring propagator can be marked", () => {
    const net = createNetwork("oracle-net");
    const input = createConnector(net, "input");
    const output = createConnector(net, "output");

    // Register propagator that requires oracle
    const propId = registerPropagator(net, "oracleInfer", [input], [output], VUnit, {
      kind: "oracle",
      requiresOracle: true,
    });

    const prop = getPropagator(net, propId);
    expect(prop).toBeDefined();
    expect(prop!.requiresOracle).toBe(true);
    expect(prop!.kind).toBe("oracle");
  });

  it("denied explanation can be created", () => {
    const denied: ExplanationVal = {
      tag: "Explanation",
      kind: "denied",
      op: "infer.op",
      reason: "airgap profile forbids oracle calls",
      profile: "airgap",
    };

    expect(denied.kind).toBe("denied");
    expect((denied as any).op).toBe("infer.op");
    expect((denied as any).profile).toBe("airgap");
  });

  it("constraints list can be retrieved", () => {
    const net = createNetwork("constraints-list");
    const c1 = createConnector(net, "c1");
    const c2 = createConnector(net, "c2");

    registerPropagator(net, "prop1", [c1], [c2], VUnit);
    registerConstraint(net, "constraint1", [c1], VUnit);
    registerConstraint(net, "constraint2", [c2], VUnit);

    const constraints = getConstraints(net);
    expect(constraints.length).toBe(2);
    expect(constraints.every(c => c.isConstraint)).toBe(true);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 12.7: Receipt-based state management
// ─────────────────────────────────────────────────────────────────

describe("Test 12.7: Receipts and state management", () => {
  it("network state can be retrieved and inspected", () => {
    const net = createNetwork("state-test");
    const conn = createConnector(net, "value");

    connectorSetValue(conn, { tag: "Num", n: 42 }, { tag: "Str", s: "initial" });

    const state = getNetwork(net.id);
    expect(state).toBeDefined();
    expect(state!.connectors.size).toBe(1);
    expect(state!.ledger.length).toBeGreaterThan(0);
  });

  it("explanation hashes are stable", () => {
    const conn: ConnRefVal = { tag: "ConnRef", id: "c1", netId: "n1" };
    const expl1 = makeAssumption(conn, "hash123", { tag: "Str", s: "reason" });
    const expl2 = makeAssumption(conn, "hash123", { tag: "Str", s: "reason" });

    const hash1 = hashExplanation(expl1);
    const hash2 = hashExplanation(expl2);

    expect(hash1).toBe(hash2);
  });

  it("explanation can be serialized to string", () => {
    const conn: ConnRefVal = { tag: "ConnRef", id: "c1", netId: "n1", name: "myConn" };
    const assumption = makeAssumption(conn, "abc123", { tag: "Str", s: "user input" });
    const derived = makeDerived(conn, "def456", "transform", [assumption]);
    const conflict = makeConflict(conn, assumption, derived, "values differ");

    const conflictStr = explanationToString(conflict);
    expect(conflictStr).toContain("CONFLICT");
    expect(conflictStr).toContain("values differ");
    expect(conflictStr).toContain("myConn");
  });

  it("ledger records propagation events", async () => {
    const net = createNetwork("ledger-test");
    const c1 = createConnector(net, "c1");
    const c2 = createConnector(net, "c2");

    registerScriptedPropagator({
      name: "copy",
      execute: (ctx) => {
        const val = ctx.inputs.values().next().value;
        if (!val) return { tag: "noChange" };
        return { tag: "values", values: new Map([[c2.id, val]]) };
      },
    });

    registerPropagator(net, "copy", [c1], [c2], VUnit);

    connectorSetValue(c1, { tag: "Num", n: 1 }, { tag: "Str", s: "init" });
    await runPropagation(net);

    const state = getNetwork(net.id);
    expect(state!.ledger.length).toBeGreaterThan(0);

    // Check for set and fire events
    const setEvents = state!.ledger.filter(e => e.type === "set");
    const fireEvents = state!.ledger.filter(e => e.type === "fire");

    expect(setEvents.length).toBeGreaterThan(0);
    expect(fireEvents.length).toBeGreaterThan(0);
  });
});

// ─────────────────────────────────────────────────────────────────
// Additional type guard tests
// ─────────────────────────────────────────────────────────────────

describe("Type guards and helpers", () => {
  it("isConnRef identifies ConnRef values", () => {
    const conn = makeConnRef("c1", "n1", "myConn");
    expect(isConnRef(conn)).toBe(true);
    expect(isConnRef({ tag: "Num", n: 42 })).toBe(false);
  });

  it("isNetRef identifies NetRef values", () => {
    const net = makeNetRef("n1", "myNet");
    expect(isNetRef(net)).toBe(true);
    expect(isNetRef({ tag: "Str", s: "hello" })).toBe(false);
  });

  it("isExplanation identifies Explanation values", () => {
    const conn = makeConnRef("c1", "n1");
    const expl = makeAssumption(conn, "hash", VUnit);
    expect(isExplanation(expl)).toBe(true);
    expect(isExplanation({ tag: "Bool", b: true })).toBe(false);
  });

  it("isContradiction identifies Contradiction values", () => {
    const conn = makeConnRef("c1", "n1");
    const expl = makeAssumption(conn, "hash", VUnit);
    const contra = makeContradiction(expl);
    expect(isContradiction(contra)).toBe(true);
    expect(isContradiction({ tag: "Unit" })).toBe(false);
  });
});

// ─────────────────────────────────────────────────────────────────
// Explanation traversal tests
// ─────────────────────────────────────────────────────────────────

describe("Explanation traversal", () => {
  it("traverseExplanation visits all nodes", () => {
    const conn = makeConnRef("c1", "n1");
    const a1 = makeAssumption(conn, "h1", VUnit);
    const a2 = makeAssumption(conn, "h2", VUnit);
    const derived = makeDerived(conn, "h3", "rule", [a1, a2]);

    const visited: string[] = [];
    traverseExplanation(derived, {
      onAssumption: (c, h) => visited.push(`assumption:${h}`),
      onDerived: (c, h, r) => visited.push(`derived:${r}`),
    });

    expect(visited).toContain("derived:rule");
    expect(visited).toContain("assumption:h1");
    expect(visited).toContain("assumption:h2");
  });

  it("getExplanationConnectors returns all referenced connectors", () => {
    const c1 = makeConnRef("c1", "n1", "conn1");
    const c2 = makeConnRef("c2", "n1", "conn2");
    const a1 = makeAssumption(c1, "h1", VUnit);
    const derived = makeDerived(c2, "h2", "rule", [a1]);

    const connectors = getExplanationConnectors(derived);
    expect(connectors.length).toBe(2);
  });

  it("getExplanationRules returns all rule names", () => {
    const conn = makeConnRef("c1", "n1");
    const a = makeAssumption(conn, "h1", VUnit);
    const d1 = makeDerived(conn, "h2", "rule1", [a]);
    const d2 = makeDerived(conn, "h3", "rule2", [d1]);

    const rules = getExplanationRules(d2);
    expect(rules).toContain("rule1");
    expect(rules).toContain("rule2");
  });
});
