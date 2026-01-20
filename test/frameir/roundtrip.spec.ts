import { describe, it, expect } from "vitest";
import { encodeCanonical, decode } from "../../src/frameir/codec";
import { CURRENT_IR_VERSION } from "../../src/frameir/version";
import type { VBool, VRecord, VList } from "../../src/frameir/value";
import type { SchemaIR } from "../../src/frameir/schema";
import type { PromptDoc } from "../../src/frameir/prompt";
import type { FPure } from "../../src/frameir/flow";

const V = CURRENT_IR_VERSION;

describe("frameir round-trip", () => {
  it("round-trips basic value nodes", () => {
    const rec: VRecord = {
      v: V,
      tag: "VRecord",
      entries: [
        { k: { v: V, tag: "VKeyword", name: "answer" }, v: { v: V, tag: "VInt", value: "42" } },
      ],
    };
    const list: VList = { v: V, tag: "VList", items: [rec] };
    expect(decode<VRecord>(encodeCanonical(rec))).toEqual(rec);
    expect(decode<VList>(encodeCanonical(list))).toEqual(list);
  });

  it("round-trips prompt and flow nodes", () => {
    const prompt: PromptDoc = {
      v: V,
      tag: "PromptDoc",
      parts: [
        { v: V, tag: "PSystem", text: "You are a test." },
        { v: V, tag: "PUser", text: "Hello" },
      ],
    };
    const flow: FPure = { v: V, tag: "FPure", value: { v: V, tag: "VBool", value: true } as VBool };

    expect(decode<PromptDoc>(encodeCanonical(prompt))).toEqual(prompt);
    expect(decode<FPure>(encodeCanonical(flow))).toEqual(flow);
  });

  it("round-trips schema nodes with json schema payload", () => {
    const schema: SchemaIR = {
      v: V,
      tag: "Schema",
      id: "schema:test",
      kind: "JsonSchema",
      jsonSchema: { type: "object", properties: { name: { type: "string" } } },
    };
    expect(decode<SchemaIR>(encodeCanonical(schema))).toEqual(schema);
  });
});
