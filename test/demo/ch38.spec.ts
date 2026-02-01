// test/demo/ch38.spec.ts
// Test for Chapter 38: Constraint Propagation Networks

import { describe, it, expect } from "vitest";
import { runDemo } from "../../demo/harness/runner";
import { chapterConfigs } from "../../demo/by-chapter/config";
import { createChapterDemo } from "../../demo/by-chapter/shared";

describe("Chapter 38: Constraint Propagation Networks", () => {
  it("should validate constraint satisfaction using LLM", async () => {
    const config = chapterConfigs["ch38-constraint-propagation"];
    expect(config).toBeDefined();

    const demo = createChapterDemo(config);
    const report = await runDemo(demo, "pragmatic", 42);

    // Check that demo succeeded
    const demoSuccess = report.invariants.find(inv => inv.name === "demo-success");
    expect(demoSuccess).toBeDefined();

    // Log error if demo failed
    if (!demoSuccess?.ok) {
      console.log("Demo failed with error:", demoSuccess?.detail);
      console.log("Full report:", JSON.stringify(report, null, 2));
    }

    expect(demoSuccess?.ok).toBe(true);

    // Check that correct number of outputs were produced
    const outputCount = report.invariants.find(inv => inv.name === "output-count");
    expect(outputCount).toBeDefined();
    expect(outputCount?.ok).toBe(true);

    expect(report.demoId).toBe("ch38-constraint-propagation");
  });
});
