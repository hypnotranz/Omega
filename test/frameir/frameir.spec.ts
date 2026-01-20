import { describe, expect, it } from "vitest";
import {
  canonicalJson,
  merkleHash,
  merkleize,
  type FlowIR,
  type MerkleNode,
  type PromptIR,
  type ValueIR,
} from "../../src/frameir";

describe("canonicalJson", () => {
  it("sorts object keys deterministically at every level", () => {
    const value: ValueIR = {
      zebra: 2,
      alpha: { z: true, y: false },
      list: [{ b: 2, a: 1 }],
    };

    const json = canonicalJson(value);
    expect(json).toBe('{"alpha":{"y":false,"z":true},"list":[{"a":1,"b":2}],"zebra":2}');
  });

  it("rejects NaN and Infinity values", () => {
    expect(() => canonicalJson({ value: Number.NaN } as unknown as ValueIR)).toThrow(/NaN/);
    expect(() => canonicalJson({ value: Number.POSITIVE_INFINITY } as unknown as ValueIR)).toThrow(
      /finite number/
    );
  });

  it("rejects non-serializable inputs like functions", () => {
    const invalid = { step: () => 1 } as unknown as ValueIR;
    expect(() => canonicalJson(invalid)).toThrow(/Unsupported type/);
  });
});

describe("merkleization", () => {
  it("produces stable hashes regardless of object key order", () => {
    const first: ValueIR = { b: 1, a: 2 };
    const second: ValueIR = { a: 2, b: 1 };

    expect(merkleHash(first)).toBe(merkleHash(second));
  });

  it("merkleizes nested structures with child hashes", () => {
    const config: ValueIR = [1, { nested: "ok" }];
    const value: ValueIR = { config };

    const tree = merkleize(value) as MerkleNode;
    if (tree.type !== "object") throw new Error("expected object node");
    const entry = tree.entries.find(e => e.key === "config");
    expect(entry).toBeDefined();
    const arrayNode = entry?.node;
    expect(arrayNode?.type).toBe("array");
    expect(arrayNode?.items).toHaveLength(2);
    expect(arrayNode?.hash).toBe(merkleHash(config));
  });

  it("hashes FlowIR content deterministically", () => {
    const prompt: PromptIR = {
      kind: "prompt",
      messages: [
        { role: "system", content: "be concise" },
        { role: "user", content: { question: "What is up?" } },
      ],
    };

    const flow: FlowIR = {
      kind: "flow",
      steps: [
        {
          id: "step-1",
          prompt,
          input: { topic: "testing" },
          metadata: { order: 1 },
        },
      ],
      metadata: { version: 1 },
    };

    const rearranged: FlowIR = {
      metadata: { version: 1 },
      kind: "flow",
      steps: [
        {
          metadata: { order: 1 },
          input: { topic: "testing" },
          prompt,
          id: "step-1",
        },
      ],
    };

    expect(canonicalJson(flow)).toBe(canonicalJson(rearranged));
    expect(merkleHash(flow)).toBe(merkleHash(rearranged));
  });
});
