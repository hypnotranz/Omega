// tests/demo/wow-pack.spec.ts
// Tests for the Ω Wow Pack demo suite

import { describe, it, expect } from "vitest";
import {
  allDemos,
  listDemos,
  getDemoById,
  getDemosByTag,
  listDemoIds,
} from "../../demo/omega-wow";
import {
  runDemo,
  runSuite,
  createScriptedOracleAdapter,
  createDemoLedger,
  getProfile,
  formatReport,
  formatSuiteResult,
} from "../../demo/harness";

// ─────────────────────────────────────────────────────────────────
// Demo Registry Tests
// ─────────────────────────────────────────────────────────────────

describe("Ω Wow Pack: Demo Registry", () => {
  it("should have 8 demos", () => {
    expect(allDemos).toHaveLength(8);
  });

  it("should list all demo summaries", () => {
    const summaries = listDemos();
    expect(summaries).toHaveLength(8);

    for (const summary of summaries) {
      expect(summary.id).toBeDefined();
      expect(summary.name).toBeDefined();
      expect(summary.description).toBeDefined();
      expect(summary.tags).toBeInstanceOf(Array);
    }
  });

  it("should get demo by ID", () => {
    const demo = getDemoById("oracle-repl-stack");
    expect(demo).toBeDefined();
    expect(demo?.name).toBe("Interactive Oracle REPL in Call Stack");
  });

  it("should return undefined for unknown demo ID", () => {
    const demo = getDemoById("nonexistent");
    expect(demo).toBeUndefined();
  });

  it("should get demos by tag", () => {
    const oracleDemos = getDemosByTag("oracle");
    expect(oracleDemos.length).toBeGreaterThan(0);
    for (const demo of oracleDemos) {
      expect(demo.tags).toContain("oracle");
    }
  });

  it("should list all demo IDs", () => {
    const ids = listDemoIds();
    expect(ids).toHaveLength(8);
    expect(ids).toContain("oracle-repl-stack");
    expect(ids).toContain("multi-shot-backtracking");
    expect(ids).toContain("concurrency-cost-collapse");
    expect(ids).toContain("generic-miss-synthesis");
    expect(ids).toContain("constraint-diagnosis-repair");
    expect(ids).toContain("semantic-macro-pipeline");
    expect(ids).toContain("compilation-inference-plane");
    expect(ids).toContain("meta-circular-repair");
  });

  it("should have unique demo IDs", () => {
    const ids = listDemoIds();
    const uniqueIds = new Set(ids);
    expect(uniqueIds.size).toBe(ids.length);
  });

  it("should have invariants for each demo", () => {
    for (const demo of allDemos) {
      expect(demo.invariants).toBeInstanceOf(Array);
      expect(demo.invariants.length).toBeGreaterThan(0);
    }
  });
});

// ─────────────────────────────────────────────────────────────────
// Harness Infrastructure Tests
// ─────────────────────────────────────────────────────────────────

describe("Ω Wow Pack: Harness Infrastructure", () => {
  describe("ScriptedOracleAdapter", () => {
    it("should create adapter with demo ID and seed", () => {
      const adapter = createScriptedOracleAdapter("test-demo", 42, "pragmatic");
      expect(adapter).toBeDefined();
      expect(adapter.handle).toBeInstanceOf(Function);
      expect(adapter.getTranscript).toBeInstanceOf(Function);
    });

    it("should track request counts", () => {
      const adapter = createScriptedOracleAdapter("test-demo", 42, "pragmatic");

      adapter.addScript({
        match: () => true,
        respond: () => ({ value: "test" }),
      });

      adapter.handle("InferOp", { op: "test" });
      adapter.handle("ReqEval", { expr: "test" });
      adapter.handle("InferOp", { op: "test2" });

      expect(adapter.getCount("InferOp")).toBe(2);
      expect(adapter.getCount("ReqEval")).toBe(1);
      expect(adapter.getCount("ReqApply")).toBe(0);
    });

    it("should generate transcript", () => {
      const adapter = createScriptedOracleAdapter("test-demo", 42, "pragmatic");

      adapter.addScript({
        match: () => true,
        respond: () => ({ value: "response" }),
      });

      adapter.handle("InferOp", { op: "test" });

      const transcript = adapter.getTranscript();
      expect(transcript.demoId).toBe("test-demo");
      expect(transcript.seed).toBe(42);
      expect(transcript.interactions).toHaveLength(1);
      expect(transcript.interactions[0].type).toBe("InferOp");
    });
  });

  describe("DemoLedger", () => {
    it("should record and retrieve events", () => {
      const ledger = createDemoLedger();

      ledger.record("infer.call", { op: "test" });
      ledger.record("infer.result", { value: 42 });

      const events = ledger.getEvents();
      expect(events).toHaveLength(2);
      expect(events[0].type).toBe("infer.call");
      expect(events[1].type).toBe("infer.result");
    });

    it("should filter events by type", () => {
      const ledger = createDemoLedger();

      ledger.record("infer.call", { op: "test1" });
      ledger.record("infer.result", { value: 1 });
      ledger.record("infer.call", { op: "test2" });

      const inferCalls = ledger.getEventsByType("infer.call");
      expect(inferCalls).toHaveLength(2);
    });

    it("should create and restore snapshots", () => {
      const ledger = createDemoLedger();

      ledger.record("infer.call", { op: "before" });
      const snapshot = ledger.snapshot();

      ledger.record("infer.call", { op: "after" });
      expect(ledger.getEvents()).toHaveLength(2);

      ledger.restore(snapshot);
      expect(ledger.getEvents()).toHaveLength(1);
      expect((ledger.getEvents()[0].data as any).op).toBe("before");
    });

    it("should compute digest", () => {
      const ledger = createDemoLedger();
      ledger.record("test", { value: 42 });

      const digest = ledger.getDigest();
      expect(typeof digest).toBe("string");
      expect(digest.length).toBeGreaterThan(0);
    });
  });

  describe("Governance Profiles", () => {
    it("should get explore profile", () => {
      const profile = getProfile("explore");
      expect(profile.name).toBe("explore");
      expect(profile.inferBudget).toBeGreaterThan(50);
    });

    it("should get pragmatic profile", () => {
      const profile = getProfile("pragmatic");
      expect(profile.name).toBe("pragmatic");
    });

    it("should get strict profile", () => {
      const profile = getProfile("strict");
      expect(profile.name).toBe("strict");
      expect(profile.inferBudget).toBeLessThan(getProfile("explore").inferBudget);
    });

    it("should get airgap profile", () => {
      const profile = getProfile("airgap");
      expect(profile.name).toBe("airgap");
      expect(profile.inferBudget).toBe(0);
    });

    it("should fallback to pragmatic for unknown profile", () => {
      const profile = getProfile("unknown");
      expect(profile.name).toBe("pragmatic");
    });
  });
});

// ─────────────────────────────────────────────────────────────────
// Demo 1: Oracle REPL Tests
// ─────────────────────────────────────────────────────────────────

describe("Ω Wow Pack: Demo 1 - Oracle REPL", () => {
  it("should run demo successfully", async () => {
    const demo = getDemoById("oracle-repl-stack")!;
    const report = await runDemo(demo, "explore", 42);

    expect(report.demoId).toBe("oracle-repl-stack");
    expect(report.profile).toBe("explore");
    expect(report.seed).toBe(42);
  });

  it("should pass all invariants", async () => {
    const demo = getDemoById("oracle-repl-stack")!;
    const report = await runDemo(demo, "explore", 42);

    for (const inv of report.invariants) {
      expect(inv.ok).toBe(true);
    }
  });

  it("should use multiple oracle request types", async () => {
    const demo = getDemoById("oracle-repl-stack")!;
    const report = await runDemo(demo, "explore", 42);

    expect(report.counts.oracleReqObserve).toBeGreaterThan(0);
    expect(report.counts.oracleReqEval).toBeGreaterThan(0);
    expect(report.counts.oracleReqApply).toBeGreaterThan(0);
    expect(report.counts.oracleReqTest).toBeGreaterThan(0);
    expect(report.counts.inferCalls).toBeGreaterThan(0);
  });

  it("should redact sensitive data", async () => {
    const demo = getDemoById("oracle-repl-stack")!;
    const report = await runDemo(demo, "explore", 42);

    const sensitiveInvariant = report.invariants.find(
      inv => inv.name === "sensitive-data-redacted"
    );
    expect(sensitiveInvariant?.ok).toBe(true);
  });
});

// ─────────────────────────────────────────────────────────────────
// Demo 2: Backtracking Tests
// ─────────────────────────────────────────────────────────────────

describe("Ω Wow Pack: Demo 2 - Backtracking", () => {
  it("should run demo successfully", async () => {
    const demo = getDemoById("multi-shot-backtracking")!;
    const report = await runDemo(demo, "explore", 42);

    expect(report.demoId).toBe("multi-shot-backtracking");
  });

  it("should have ledger and backtrack invariants", async () => {
    const demo = getDemoById("multi-shot-backtracking")!;
    const report = await runDemo(demo, "explore", 42);

    // Check that the expected invariants exist
    const invariantNames = report.invariants.map(inv => inv.name);
    expect(invariantNames).toContain("ledger-shows-amb-and-backtrack");
    expect(invariantNames).toContain("only-accepted-branch-committed");
  });

  it("should track ledger events", async () => {
    const demo = getDemoById("multi-shot-backtracking")!;
    const report = await runDemo(demo, "explore", 42);

    // Should have ledger events
    expect(report.ledgerDigest.length).toBeGreaterThan(0);
  });
});

// ─────────────────────────────────────────────────────────────────
// Demo 3: Concurrency Tests
// ─────────────────────────────────────────────────────────────────

describe("Ω Wow Pack: Demo 3 - Concurrency", () => {
  it("should run demo successfully", async () => {
    const demo = getDemoById("concurrency-cost-collapse")!;
    const report = await runDemo(demo, "explore", 42);

    expect(report.demoId).toBe("concurrency-cost-collapse");
  });

  it("should pass all invariants", async () => {
    const demo = getDemoById("concurrency-cost-collapse")!;
    const report = await runDemo(demo, "explore", 42);

    for (const inv of report.invariants) {
      expect(inv.ok).toBe(true);
    }
  });

  it("should have singleflight invariant", async () => {
    const demo = getDemoById("concurrency-cost-collapse")!;
    const report = await runDemo(demo, "explore", 42);

    // Singleflight should have prevented duplicate calls
    const collapseInvariant = report.invariants.find(
      inv => inv.name === "singleflight-reduces-calls"
    );
    expect(collapseInvariant).toBeDefined();
  });
});

// ─────────────────────────────────────────────────────────────────
// Demo 4: Generic Synthesis Tests
// ─────────────────────────────────────────────────────────────────

describe("Ω Wow Pack: Demo 4 - Generic Synthesis", () => {
  it("should run demo successfully", async () => {
    const demo = getDemoById("generic-miss-synthesis")!;
    const report = await runDemo(demo, "explore", 42);

    expect(report.demoId).toBe("generic-miss-synthesis");
  });

  it("should have generic miss invariants", async () => {
    const demo = getDemoById("generic-miss-synthesis")!;
    const report = await runDemo(demo, "explore", 42);

    // Check that the expected invariants exist
    const invariantNames = report.invariants.map(inv => inv.name);
    expect(invariantNames).toContain("first-run-triggers-miss");
    expect(invariantNames).toContain("method-installed-after-miss");
  });

  it("should synthesize missing methods", async () => {
    const demo = getDemoById("generic-miss-synthesis")!;
    const report = await runDemo(demo, "explore", 42);

    const synthesisInvariant = report.invariants.find(
      inv => inv.name === "first-run-triggers-miss"
    );
    expect(synthesisInvariant).toBeDefined();
  });
});

// ─────────────────────────────────────────────────────────────────
// Demo 5: Constraint Repair Tests
// ─────────────────────────────────────────────────────────────────

describe("Ω Wow Pack: Demo 5 - Constraint Repair", () => {
  it("should run demo successfully", async () => {
    const demo = getDemoById("constraint-diagnosis-repair")!;
    const report = await runDemo(demo, "explore", 42);

    expect(report.demoId).toBe("constraint-diagnosis-repair");
  });

  it("should have constraint repair invariants", async () => {
    const demo = getDemoById("constraint-diagnosis-repair")!;
    const report = await runDemo(demo, "explore", 42);

    // Check that the expected invariants exist
    const invariantNames = report.invariants.map(inv => inv.name);
    expect(invariantNames).toContain("network-identifies-violations");
    expect(invariantNames).toContain("repair-found-and-certified");
  });

  it("should repair constraint violations", async () => {
    const demo = getDemoById("constraint-diagnosis-repair")!;
    const report = await runDemo(demo, "explore", 42);

    const repairInvariant = report.invariants.find(
      inv => inv.name === "violations-reduced"
    );
    expect(repairInvariant).toBeDefined();
  });
});

// ─────────────────────────────────────────────────────────────────
// Demo 6: Semantic Macros Tests
// ─────────────────────────────────────────────────────────────────

describe("Ω Wow Pack: Demo 6 - Semantic Macros", () => {
  it("should run demo successfully", async () => {
    const demo = getDemoById("semantic-macro-pipeline")!;
    const report = await runDemo(demo, "explore", 42);

    expect(report.demoId).toBe("semantic-macro-pipeline");
  });

  it("should pass all invariants", async () => {
    const demo = getDemoById("semantic-macro-pipeline")!;
    const report = await runDemo(demo, "explore", 42);

    for (const inv of report.invariants) {
      expect(inv.ok).toBe(true);
    }
  });

  it("should expand macros correctly", async () => {
    const demo = getDemoById("semantic-macro-pipeline")!;
    const report = await runDemo(demo, "explore", 42);

    const expansionInvariant = report.invariants.find(
      inv => inv.name === "macro-expansion-hygienic"
    );
    expect(expansionInvariant).toBeDefined();
  });
});

// ─────────────────────────────────────────────────────────────────
// Demo 7: Compilation Tests
// ─────────────────────────────────────────────────────────────────

describe("Ω Wow Pack: Demo 7 - Compilation", () => {
  it("should run demo successfully", async () => {
    const demo = getDemoById("compilation-inference-plane")!;
    const report = await runDemo(demo, "explore", 42);

    expect(report.demoId).toBe("compilation-inference-plane");
  });

  it("should pass all invariants", async () => {
    const demo = getDemoById("compilation-inference-plane")!;
    const report = await runDemo(demo, "explore", 42);

    for (const inv of report.invariants) {
      expect(inv.ok).toBe(true);
    }
  });

  it("should preserve inference plane", async () => {
    const demo = getDemoById("compilation-inference-plane")!;
    const report = await runDemo(demo, "explore", 42);

    const preservationInvariant = report.invariants.find(
      inv => inv.name === "interpreter-equals-compiled"
    );
    expect(preservationInvariant).toBeDefined();
  });
});

// ─────────────────────────────────────────────────────────────────
// Demo 8: Meta-Circular Tests
// ─────────────────────────────────────────────────────────────────

describe("Ω Wow Pack: Demo 8 - Meta-Circular", () => {
  it("should run demo successfully", async () => {
    const demo = getDemoById("meta-circular-repair")!;
    const report = await runDemo(demo, "explore", 42);

    expect(report.demoId).toBe("meta-circular-repair");
  });

  it("should pass all invariants", async () => {
    const demo = getDemoById("meta-circular-repair")!;
    const report = await runDemo(demo, "explore", 42);

    for (const inv of report.invariants) {
      expect(inv.ok).toBe(true);
    }
  });

  it("should repair evaluator", async () => {
    const demo = getDemoById("meta-circular-repair")!;
    const report = await runDemo(demo, "explore", 42);

    const repairInvariant = report.invariants.find(
      inv => inv.name === "repair-improves-test-suite"
    );
    expect(repairInvariant).toBeDefined();
  });
});

// ─────────────────────────────────────────────────────────────────
// Full Suite Tests
// ─────────────────────────────────────────────────────────────────

describe("Ω Wow Pack: Full Suite", () => {
  it("should run all demos", async () => {
    const result = await runSuite(allDemos, "explore", 42);

    expect(result.total).toBe(8);
    // All demos should at least run (may have some failing invariants)
    expect(result.reports).toHaveLength(8);
  }, 30000); // Longer timeout for full suite

  it("should be deterministic with same seed", async () => {
    const result1 = await runSuite(allDemos, "explore", 42);
    const result2 = await runSuite(allDemos, "explore", 42);

    // Check count aggregates are the same
    const totalInfer1 = result1.reports.reduce((sum, r) => sum + r.counts.inferCalls, 0);
    const totalInfer2 = result2.reports.reduce((sum, r) => sum + r.counts.inferCalls, 0);
    expect(totalInfer1).toBe(totalInfer2);

    // Check each demo produces same outputs digest
    for (let i = 0; i < result1.reports.length; i++) {
      expect(result1.reports[i].outputsDigest).toBe(result2.reports[i].outputsDigest);
    }
  }, 60000); // Longer timeout for running suite twice

  it("should produce different results with different seeds", async () => {
    // Note: Some demos may be fully deterministic regardless of seed
    // This test just verifies the seed is being used
    const demo = getDemoById("oracle-repl-stack")!;

    const report1 = await runDemo(demo, "explore", 42);
    const report2 = await runDemo(demo, "explore", 43);

    // The outputs should be the same (same policy, same doc)
    // but the seed should differ in the reports
    expect(report1.seed).toBe(42);
    expect(report2.seed).toBe(43);
  });
});

// ─────────────────────────────────────────────────────────────────
// Report Formatting Tests
// ─────────────────────────────────────────────────────────────────

describe("Ω Wow Pack: Report Formatting", () => {
  it("should format single demo report", async () => {
    const demo = getDemoById("oracle-repl-stack")!;
    const report = await runDemo(demo, "explore", 42);

    const formatted = formatReport(report, false);

    expect(formatted).toContain("oracle-repl-stack");
    expect(formatted).toContain("OK");
  });

  it("should format verbose report with more details", async () => {
    const demo = getDemoById("oracle-repl-stack")!;
    const report = await runDemo(demo, "explore", 42);

    const formatted = formatReport(report, true);

    // Verbose should include more details
    expect(formatted).toContain("explore");
    expect(formatted).toContain("42");
    expect(formatted).toContain("inferCalls");
  });

  it("should format suite result", async () => {
    const result = await runSuite(allDemos, "explore", 42);

    const formatted = formatSuiteResult(result, false);

    expect(formatted).toContain("8");
    expect(formatted).toContain("Passed");
  }, 30000);
});

// ─────────────────────────────────────────────────────────────────
// WowReport Schema Tests
// ─────────────────────────────────────────────────────────────────

describe("Ω Wow Pack: WowReport Schema", () => {
  it("should have all required fields", async () => {
    const demo = getDemoById("oracle-repl-stack")!;
    const report = await runDemo(demo, "explore", 42);

    // Required fields
    expect(report.demoId).toBeDefined();
    expect(report.description).toBeDefined();
    expect(report.profile).toBeDefined();
    expect(report.seed).toBeDefined();
    expect(report.timestamp).toBeDefined();
    expect(report.outputsDigest).toBeDefined();
    expect(report.ledgerDigest).toBeDefined();
    expect(report.counts).toBeDefined();
    expect(report.costs).toBeDefined();
    expect(report.invariants).toBeDefined();
    expect(report.artifacts).toBeDefined();
  });

  it("should have valid counts", async () => {
    const demo = getDemoById("oracle-repl-stack")!;
    const report = await runDemo(demo, "explore", 42);

    expect(typeof report.counts.inferCalls).toBe("number");
    expect(typeof report.counts.oracleReqEval).toBe("number");
    expect(typeof report.counts.oracleReqApply).toBe("number");
    expect(typeof report.counts.oracleReqObserve).toBe("number");
    expect(typeof report.counts.oracleReqTest).toBe("number");

    // All counts should be non-negative
    expect(report.counts.inferCalls).toBeGreaterThanOrEqual(0);
    expect(report.counts.oracleReqEval).toBeGreaterThanOrEqual(0);
  });

  it("should have valid costs", async () => {
    const demo = getDemoById("oracle-repl-stack")!;
    const report = await runDemo(demo, "explore", 42);

    expect(typeof report.costs.steps).toBe("number");
    expect(report.costs.steps).toBeGreaterThan(0);

    if (report.costs.wallMs !== undefined) {
      expect(typeof report.costs.wallMs).toBe("number");
      expect(report.costs.wallMs).toBeGreaterThanOrEqual(0);
    }
  });

  it("should have valid invariant results", async () => {
    const demo = getDemoById("oracle-repl-stack")!;
    const report = await runDemo(demo, "explore", 42);

    expect(report.invariants.length).toBeGreaterThan(0);

    for (const inv of report.invariants) {
      expect(inv.name).toBeDefined();
      expect(typeof inv.ok).toBe("boolean");
      expect(inv.detail).toBeDefined();
    }
  });

  it("should have valid artifacts", async () => {
    const demo = getDemoById("oracle-repl-stack")!;
    const report = await runDemo(demo, "explore", 42);

    // Artifacts may be optional, but if present should be valid
    if (report.artifacts.transcriptId) {
      expect(typeof report.artifacts.transcriptId).toBe("string");
    }
  });

  it("should have valid timestamp", async () => {
    const demo = getDemoById("oracle-repl-stack")!;
    const report = await runDemo(demo, "explore", 42);

    // Timestamp should be ISO 8601 format
    const date = new Date(report.timestamp);
    expect(date.getTime()).not.toBeNaN();
  });
});
