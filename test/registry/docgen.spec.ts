import { describe, it, expect, beforeEach } from "vitest";
import { generateJSON, generateMarkdown, PrimitiveRegistry } from "../../src/registry";
import { frameLispDescriptors } from "../../src/registry/descriptors/framelisp";

let registry: PrimitiveRegistry;

beforeEach(() => {
  registry = new PrimitiveRegistry();
  for (const desc of frameLispDescriptors) {
    registry.register(desc);
  }
});

describe("registry docgen", () => {
  it("renders markdown with layers, tables, and formatted signatures", () => {
    const md = generateMarkdown(registry);

    expect(md).toContain("# Generated Primitive Reference");
    expect(md).toContain("## FrameLisp");
    expect(md).toMatch(/\| `infer` \|/);
    expect(md).toContain("(prompt, options?) -> Str");
    expect(md).toContain("| Oracle |");
    expect(md).not.toContain("LambdaLLM");
  });

  it("emits parseable JSON containing seeded descriptors", () => {
    const json = generateJSON(registry);
    const parsed = JSON.parse(json) as Record<string, unknown>;

    expect(parsed["framelisp/infer"]).toBeDefined();
    expect((parsed["framelisp/infer"] as any).effects).toContain("Oracle");
  });
});
