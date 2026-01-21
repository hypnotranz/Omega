import { describe, it, expect, beforeEach } from "vitest";
import { budgetDominatorPass } from "../../src/lint/passes/budgetDominator";
import { timeoutGuardPass } from "../../src/lint/passes/timeoutGuard";
import { toolContractPass } from "../../src/lint/passes/toolContract";
import { CURRENT_IR_VERSION } from "../../src/frameir/version";
import type { FlowIR, FInfer, FToolCall, FWithBudget, FWithTimeout, FBranch } from "../../src/frameir/flow";
import type { IRBundle } from "../../src/frameir/bundle";
import type { ToolContractIR } from "../../src/frameir/contract";
import type { VRef, ValueIR } from "../../src/frameir/value";
import type { PromptIR } from "../../src/frameir/prompt";
import type { PrimitiveDescriptor } from "../../src/registry/types";
import { PrimitiveRegistry } from "../../src/registry/registry";

const V = CURRENT_IR_VERSION;

describe("lint passes", () => {
  let registry: PrimitiveRegistry;

  beforeEach(() => {
    registry = createTestRegistry();
  });

  describe("budgetDominatorPass", () => {
    it("flags oracle calls missing with-budget", () => {
      const flow = inferNode();
      const bundle = makeBundle(flow);

      const result = budgetDominatorPass.run(bundle, registry);
      expect(result.diagnostics).toHaveLength(1);
      expect(result.diagnostics[0].code).toBe("E0600");
    });

    it("flags tool calls missing with-budget", () => {
      const flow = toolCallNode();
      const bundle = makeBundle(flow);

      const result = budgetDominatorPass.run(bundle, registry);
      expect(result.diagnostics).toEqual([
        expect.objectContaining({ code: "E0601", severity: "error" }),
      ]);
    });

    it("accepts oracle dominated by with-budget", () => {
      const flow: FWithBudget = {
        v: V,
        tag: "FWithBudget",
        budget: intVal("10"),
        flow: inferNode(),
      };
      const bundle = makeBundle(flow);

      const result = budgetDominatorPass.run(bundle, registry);
      expect(result.diagnostics).toHaveLength(0);
    });

    it("detects unguarded oracle in alternate branch", () => {
      const guarded = withBudget(inferNode());
      const unguarded = inferNode();
      const flow: FBranch = {
        v: V,
        tag: "FBranch",
        pred: boolVal(true),
        then: guarded,
        else: unguarded,
      };
      const bundle = makeBundle(flow);

      const result = budgetDominatorPass.run(bundle, registry);
      expect(result.diagnostics).toHaveLength(1);
      expect(result.diagnostics[0].code).toBe("E0600");
    });
  });

  describe("timeoutGuardPass", () => {
    it("warns when oracle lacks timeout guard", () => {
      const bundle = makeBundle(inferNode());
      const result = timeoutGuardPass.run(bundle, registry);

      expect(result.diagnostics).toHaveLength(1);
      expect(result.diagnostics[0]).toMatchObject({ code: "W0010", severity: "warning" });
    });

    it("passes when oracle is dominated by with-timeout", () => {
      const flow: FWithTimeout = {
        v: V,
        tag: "FWithTimeout",
        ms: intVal("1000"),
        flow: inferNode(),
      };
      const bundle = makeBundle(flow);

      const result = timeoutGuardPass.run(bundle, registry);
      expect(result.diagnostics).toHaveLength(0);
    });
  });

  describe("toolContractPass", () => {
    it("errors when tool call is missing a contract", () => {
      const bundle = makeBundle(toolCallNode(undefined));
      const result = toolContractPass.run(bundle, registry);

      expect(result.diagnostics).toHaveLength(1);
      expect(result.diagnostics[0].code).toBe("E0610");
    });

    it("errors when contract reference is not in bundle", () => {
      const missingRef = contractRef("not-there");
      const bundle = makeBundle(toolCallNode(missingRef));
      const result = toolContractPass.run(bundle, registry);

      expect(result.diagnostics).toHaveLength(1);
      expect(result.diagnostics[0].code).toBe("E0611");
    });

    it("errors when tool contract references missing schemas", () => {
      const contractId = "tool-2";
      const tc = toolContract(contractId);
      const bundle = makeBundle(toolCallNode(contractRef(contractId)), {
        toolContracts: { [contractId]: tc },
        schemas: {},
      });

      const result = toolContractPass.run(bundle, registry);

      expect(result.diagnostics).toEqual(
        expect.arrayContaining([
          expect.objectContaining({ code: "E0612" }),
          expect.objectContaining({ code: "E0613" }),
        ])
      );
    });

    it("passes when contract exists in bundle", () => {
      const contractId = "tool-1";
      const tc = toolContract(contractId);
      const bundle = makeBundle(toolCallNode(contractRef(contractId)), {
        toolContracts: { [contractId]: tc },
        schemas: {
          "schema-in": schemaNode("schema-in"),
          "schema-out": schemaNode("schema-out"),
        },
      });

      const result = toolContractPass.run(bundle, registry);
      expect(result.diagnostics).toHaveLength(0);
    });
  });
});

function createTestRegistry(): PrimitiveRegistry {
  const registry = new PrimitiveRegistry();
  const descriptors: PrimitiveDescriptor[] = [
    {
      id: "framelisp/infer",
      layer: "FrameLisp",
      kind: "SpecialForm",
      signature: { params: [], returns: "Str" },
      effects: ["Oracle"],
      doc: { summary: "infer" },
      version: "1.0.0",
      lowering: { kind: "Intrinsic", irTag: "FInfer" },
      constraints: { mustBeDominatedByBudget: true, mustBeDominatedByTimeout: true },
    },
    {
      id: "framelisp/call-tool",
      layer: "FrameLisp",
      kind: "SpecialForm",
      signature: { params: [], returns: "Any" },
      effects: ["Tool"],
      doc: { summary: "tool" },
      version: "1.0.0",
      lowering: { kind: "Intrinsic", irTag: "FToolCall" },
      constraints: { requiresToolContract: true, mustBeDominatedByBudget: true },
    },
    {
      id: "framelisp/with-budget",
      layer: "FrameLisp",
      kind: "SpecialForm",
      signature: { params: [], returns: "Any" },
      effects: ["Control"],
      doc: { summary: "budget" },
      version: "1.0.0",
      lowering: { kind: "Intrinsic", irTag: "FWithBudget" },
    },
    {
      id: "framelisp/with-timeout",
      layer: "FrameLisp",
      kind: "SpecialForm",
      signature: { params: [], returns: "Any" },
      effects: ["Control"],
      doc: { summary: "timeout" },
      version: "1.0.0",
      lowering: { kind: "Intrinsic", irTag: "FWithTimeout" },
    },
  ];

  descriptors.forEach(d => registry.register(d));
  return registry;
}

function inferNode(): FInfer {
  return {
    v: V,
    tag: "FInfer",
    prompt: promptDoc("hi"),
  };
}

function toolCallNode(contract?: VRef): FToolCall {
  return {
    v: V,
    tag: "FToolCall",
    tool: strVal("echo"),
    args: recordVal([]),
    contract,
  };
}

function withBudget(flow: FlowIR): FWithBudget {
  return { v: V, tag: "FWithBudget", budget: intVal("1"), flow };
}

function promptDoc(text: string): PromptIR {
  return { v: V, tag: "PromptDoc", parts: [{ v: V, tag: "PUser", text }] as any };
}

function intVal(n: string): ValueIR {
  return { v: V, tag: "VInt", value: n };
}

function boolVal(value: boolean): ValueIR {
  return { v: V, tag: "VBool", value };
}

function strVal(value: string): ValueIR {
  return { v: V, tag: "VStr", value };
}

function recordVal(entries: Array<{ k: ValueIR; v: ValueIR }>): ValueIR {
  return { v: V, tag: "VRecord", entries };
}

function contractRef(id: string): VRef {
  return { v: V, tag: "VRef", ref: { kind: "ToolContract", id } };
}

function schemaRef(id: string): VRef {
  return { v: V, tag: "VRef", ref: { kind: "Schema", id } };
}

function schemaNode(id: string) {
  return { v: V, tag: "Schema", id, kind: "FrameSchema" };
}

function toolContract(id: string): ToolContractIR {
  return {
    v: V,
    tag: "ToolContract",
    id,
    name: `tool-${id}`,
    version: "1.0.0",
    inputSchema: schemaRef("schema-in"),
    outputSchema: schemaRef("schema-out"),
    idempotency: "unknown",
    capabilityTag: "default",
  };
}

function makeBundle(entry: FlowIR, overrides?: Partial<IRBundle>): IRBundle {
  return {
    v: V,
    entry,
    fns: {},
    schemas: {},
    toolContracts: {},
    ...overrides,
  };
}
