import { describe, expect, it } from "vitest";
import { createFlowPipeline, defaultFlowCompileConfig } from "../../src/core/compiler/pipeline";
import { hashNode } from "../../src/frameir/hash";

const compileConfig = {
  ...defaultFlowCompileConfig,
  normalize: {
    flattenSequences: true,
    flattenPrompts: true,
    insertImplicitBudgets: false,
    insertImplicitTimeouts: false,
  },
};

describe("compiler pipeline integration", () => {
  it("compiles source to FlowIR bundle with source map", () => {
    const pipeline = createFlowPipeline();
    const result = pipeline.compile("(pure 1)", compileConfig);

    expect(result.ok).toBe(true);
    expect(result.bundle?.entry.tag).toBe("FPure");
    const entryHash = hashNode(result.bundle!.entry);
    expect(result.sourceMap?.irToSource.get(entryHash)?.startCol).toBe(2);
  });

  it("aggregates diagnostics from failing phases", () => {
    const pipeline = createFlowPipeline();
    const result = pipeline.compile("(unknown 1)", compileConfig);

    expect(result.ok).toBe(false);
    expect(result.diagnostics.length).toBeGreaterThan(0);
  });
});
