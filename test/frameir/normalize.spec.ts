import { describe, it, expect } from "vitest";
import { normalizeFlow, normalizePrompt } from "../../src/frameir/normalize";
import { CURRENT_IR_VERSION } from "../../src/frameir/version";
import type { FSequence, FAll, FPure } from "../../src/frameir/flow";
import type { PromptDoc, PromptPart } from "../../src/frameir/prompt";

const V = CURRENT_IR_VERSION;

describe("frameir normalize", () => {
  it("flattens nested FSequence nodes", () => {
    const inner: FSequence = { v: V, tag: "FSequence", flows: [{ v: V, tag: "FPure", value: { v: V, tag: "VNil" } } as FPure] };
    const outer: FSequence = { v: V, tag: "FSequence", flows: [inner, inner] };

    const normalized = normalizeFlow(outer) as FSequence;
    expect(normalized.flows.length).toBe(2);
    expect(normalized.flows.every(f => f.tag !== "FSequence")).toBe(true);
  });

  it("converts FAll with no flows into FPure([])", () => {
    const empty: FAll = { v: V, tag: "FAll", flows: [] };
    const normalized = normalizeFlow(empty);
    expect(normalized.tag).toBe("FPure");
    const pure = normalized as FPure;
    expect((pure.value as any).tag).toBe("VList");
    expect((pure.value as any).items).toEqual([]);
  });

  it("flattens nested prompt docs and removes empty text nodes", () => {
    const parts: PromptPart[] = [
      { v: V, tag: "PSystem", text: "" },
      { v: V, tag: "PromptDoc", parts: [{ v: V, tag: "PUser", text: "Hello" }] } as any,
      { v: V, tag: "PAssistant", text: "World" },
    ];
    const prompt: PromptDoc = { v: V, tag: "PromptDoc", parts };

    const normalized = normalizePrompt(prompt);
    expect(normalized.parts.length).toBe(2);
    expect(normalized.parts[0].tag).toBe("PUser");
    expect(normalized.parts[1].tag).toBe("PAssistant");
  });
});
