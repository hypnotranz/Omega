import { describe, it, expect } from "vitest";
import { encodeCanonical, decode } from "../../src/frameir/codec";
import { CURRENT_IR_VERSION } from "../../src/frameir/version";
import type { VInt, VRecord, VList, VStr, VBool } from "../../src/frameir/value";

const V = CURRENT_IR_VERSION;

describe("frameir codec", () => {
  it("encodes a simple value deterministically", () => {
    const node: VInt = { v: V, tag: "VInt", value: "42" };
    const encoded = encodeCanonical(node);
    expect(encoded).toBe('{"tag":"VInt","v":"frameir@1","value":"42"}');
    expect(encoded).toBe(encodeCanonical(node));
  });

  it("sorts object keys lexicographically", () => {
    const rec: VRecord = {
      v: V,
      tag: "VRecord",
      entries: [
        { k: { v: V, tag: "VKeyword", name: "zebra" }, v: { v: V, tag: "VInt", value: "1" } },
        { k: { v: V, tag: "VKeyword", name: "apple" }, v: { v: V, tag: "VInt", value: "2" } },
      ],
    };
    const encoded = encodeCanonical(rec);
    const idxEntries = encoded.indexOf("\"entries\"");
    const idxTag = encoded.indexOf("\"tag\"");
    const idxV = encoded.indexOf("\"v\"");
    expect(idxEntries).toBeGreaterThanOrEqual(0);
    expect(idxEntries).toBeLessThan(idxTag);
    expect(idxTag).toBeLessThan(idxV);
  });

  it("excludes meta by default and includes when requested", () => {
    const node: VInt = { v: V, tag: "VInt", value: "7", meta: { doc: "note" } };
    const withoutMeta = encodeCanonical(node);
    expect(withoutMeta).not.toContain("meta");

    const withMeta = encodeCanonical(node, { includeMeta: true });
    expect(withMeta).toContain("\"meta\"");
    expect(withMeta).toContain("\"doc\"");
  });

  it("encodes and decodes empty structures", () => {
    const emptyList: VList = { v: V, tag: "VList", items: [] };
    const encoded = encodeCanonical(emptyList);
    const decoded = decode<VList>(encoded);
    expect(decoded.items).toHaveLength(0);
  });

  it("round-trips string and boolean values", () => {
    const strNode: VStr = { v: V, tag: "VStr", value: "" };
    const boolNode: VBool = { v: V, tag: "VBool", value: true };

    expect(decode<VStr>(encodeCanonical(strNode))).toEqual(strNode);
    expect(decode<VBool>(encodeCanonical(boolNode))).toEqual(boolNode);
  });

  it("throws on invalid decode inputs", () => {
    expect(() => decode("not json")).toThrow();
    expect(() => decode('{"tag":"VInt","value":"42"}')).toThrow(/missing v/i);
    expect(() => decode('{"v":"frameir@1","value":"42"}')).toThrow(/missing tag/i);
  });
});
