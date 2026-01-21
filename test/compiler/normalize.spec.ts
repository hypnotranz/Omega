import { describe, expect, it } from "vitest";
import { normalizeFlowIR } from "../../src/core/compiler/normalize";
import { CURRENT_IR_VERSION } from "../../src/frameir/version";
import type { FlowIR } from "../../src/frameir/flow";

const configBase = {
  flattenSequences: true,
  flattenPrompts: false,
  insertImplicitBudgets: false,
  insertImplicitTimeouts: false,
};

function pure(value: string | number): FlowIR {
  return {
    v: CURRENT_IR_VERSION,
    tag: "FPure",
    value: {
      v: CURRENT_IR_VERSION,
      tag: typeof value === "number" ? "VInt" : "VStr",
      value: String(value),
    } as any,
  };
}

describe("compiler normalizer", () => {
  it("flattens nested FSequence nodes", () => {
    const flow: FlowIR = {
      v: CURRENT_IR_VERSION,
      tag: "FSequence",
      flows: [
        {
          v: CURRENT_IR_VERSION,
          tag: "FSequence",
          flows: [pure(1), pure(2)],
        },
        pure(3),
      ],
    };

    const result = normalizeFlowIR(flow, { ...configBase });
    expect(result.ok).toBe(true);
    const seq = result.ir as any;
    expect(seq.flows.length).toBe(3);
  });

  it("wraps flows with implicit budget/timeout when configured", () => {
    const flow = pure(0);
    const result = normalizeFlowIR(flow, {
      flattenSequences: false,
      flattenPrompts: false,
      insertImplicitBudgets: true,
      insertImplicitTimeouts: true,
      defaultBudget: { llmCalls: 1, tokens: 10, timeMs: 1000 },
      defaultTimeoutMs: 500,
    });

    expect(result.ok).toBe(true);
    expect(result.ir.tag).toBe("FWithBudget");
    const inner = (result.ir as any).flow;
    expect(inner.tag).toBe("FWithTimeout");
  });
});
