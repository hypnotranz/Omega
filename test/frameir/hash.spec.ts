import { describe, it, expect } from "vitest";
import { hashNode, merkleize } from "../../src/frameir/hash";
import { CURRENT_IR_VERSION } from "../../src/frameir/version";
import type { VInt } from "../../src/frameir/value";
import type { FPure, FSequence } from "../../src/frameir/flow";

const V = CURRENT_IR_VERSION;

describe("frameir hashing", () => {
  it("produces stable hashes for identical nodes", () => {
    const a: VInt = { v: V, tag: "VInt", value: "42" };
    const b: VInt = { ...a, meta: { doc: "ignored" } };

    expect(hashNode(a)).toMatch(/^ir:sha256:[a-f0-9]{32}$/);
    expect(hashNode(a)).toBe(hashNode(a));
    expect(hashNode(a)).toBe(hashNode(b));
  });

  it("changes hashes when semantic content changes or salt provided", () => {
    const a: VInt = { v: V, tag: "VInt", value: "42" };
    const b: VInt = { v: V, tag: "VInt", value: "43" };
    expect(hashNode(a)).not.toBe(hashNode(b));

    const salted1 = hashNode(a, "salt1");
    const salted2 = hashNode(a, "salt2");
    expect(salted1).not.toBe(salted2);
  });

  it("merkleizes flows and de-duplicates shared nodes", () => {
    const leaf: FPure = { v: V, tag: "FPure", value: { v: V, tag: "VInt", value: "1" } };
    const seq: FSequence = { v: V, tag: "FSequence", flows: [leaf, leaf] };

    const hashes = merkleize(seq);
    expect(hashes.size).toBe(2); // shared leaf hashed once

    const leafHash = hashes.get(leaf)!;
    const expectedSeqHash = hashNode({ ...seq, flows: [leafHash, leafHash] } as any);
    expect(hashes.get(seq)).toBe(expectedSeqHash);
  });
});
