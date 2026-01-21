import type { PrimitiveDescriptor } from "../types";

export const frameLispDescriptors: PrimitiveDescriptor[] = [
  // === Core Effects ===
  {
    id: "framelisp/infer",
    layer: "FrameLisp",
    kind: "SpecialForm",
    signature: {
      params: [
        { name: "prompt", type: "Prompt" },
        { name: "options", type: "Record", optional: true },
      ],
      returns: "Str",
    },
    effects: ["Oracle"],
    resources: {
      estimate: { llmCalls: 1, tokens: "promptTokens + maxTokens" },
    },
    doc: {
      summary: "Core LLM inference primitive.",
      detail: "Sends prompt to oracle and returns response text. Subject to budget constraints.",
      laws: [
        "infer(prompt) is referentially opaque (Oracle effect).",
        "infer must be dominated by with-budget in linted bundles.",
      ],
      examples: [
        { input: '(infer "What is 2+2?")', output: '"4"', description: "Simple inference" },
      ],
    },
    lowering: { kind: "Intrinsic", irTag: "FInfer" },
    constraints: { mustBeDominatedByBudget: true },
    version: "1.0.0",
  },

  {
    id: "framelisp/call-tool",
    layer: "FrameLisp",
    kind: "SpecialForm",
    signature: {
      params: [
        { name: "name", type: "Str" },
        { name: "args", type: "Record" },
        { name: "contract", type: "ToolContract", optional: true },
      ],
      returns: "Any",
    },
    effects: ["Tool"],
    resources: {
      estimate: { toolCalls: 1 },
    },
    doc: {
      summary: "Call an external tool with validated arguments.",
      laws: [
        "call-tool must have associated ToolContract for audited execution.",
        "Arguments validated against contract.inputSchema before call.",
      ],
    },
    lowering: { kind: "Intrinsic", irTag: "FToolCall" },
    constraints: { requiresToolContract: true },
    version: "1.0.0",
  },

  // === Monad Operations ===
  {
    id: "framelisp/bind",
    layer: "FrameLisp",
    kind: "Function",
    signature: {
      params: [
        { name: "m", type: "Flow[A]" },
        { name: "k", type: "Fn[A -> Flow[B]]" },
      ],
      returns: "Flow[B]",
    },
    effects: ["Control"],
    doc: {
      summary: "Monadic bind for Flow.",
      laws: [
        "Left identity: bind(pure(x), k) == k(x).",
        "Right identity: bind(m, pure) == m.",
        "Associativity: bind(bind(m, f), g) == bind(m, (lambda x. bind(f(x), g))).",
      ],
    },
    lowering: { kind: "Intrinsic", irTag: "FBind" },
    version: "1.0.0",
  },

  {
    id: "framelisp/pure",
    layer: "FrameLisp",
    kind: "Function",
    signature: {
      params: [{ name: "x", type: "A" }],
      returns: "Flow[A]",
    },
    effects: ["Pure"],
    doc: {
      summary: "Lift a value into Flow.",
      laws: ["pure(x) has no effects."],
    },
    lowering: { kind: "Intrinsic", irTag: "FPure" },
    version: "1.0.0",
  },

  {
    id: "framelisp/fail",
    layer: "FrameLisp",
    kind: "Function",
    signature: {
      params: [
        { name: "reason", type: "Keyword" },
        { name: "ctx", type: "Record", optional: true },
      ],
      returns: "Flow[Never]",
    },
    effects: ["Control"],
    doc: {
      summary: "Signal a failure.",
      detail: "Unwinds until a catch handler is found.",
    },
    lowering: { kind: "Intrinsic", irTag: "FFail" },
    version: "1.0.0",
  },

  {
    id: "framelisp/catch",
    layer: "FrameLisp",
    kind: "Function",
    signature: {
      params: [
        { name: "flow", type: "Flow[A]" },
        { name: "handler", type: "Fn[Failure -> Flow[A]]" },
      ],
      returns: "Flow[A]",
    },
    effects: ["Control"],
    doc: {
      summary: "Handle failures from a flow.",
      laws: [
        "catch(pure(x), h) == pure(x).",
        "catch(fail(r), h) == h(Failure(r)).",
      ],
    },
    lowering: { kind: "Intrinsic", irTag: "FCatch" },
    version: "1.0.0",
  },

  // === Resource Control ===
  {
    id: "framelisp/with-budget",
    layer: "FrameLisp",
    kind: "SpecialForm",
    signature: {
      params: [
        { name: "budget", type: "Budget" },
        { name: "flow", type: "Flow[A]" },
      ],
      returns: "Flow[A]",
    },
    effects: ["Control"],
    doc: {
      summary: "Run flow with budget constraint.",
      detail: "Fails with :budget-exceeded if resources exhausted.",
    },
    lowering: { kind: "Intrinsic", irTag: "FWithBudget" },
    version: "1.0.0",
  },

  {
    id: "framelisp/with-timeout",
    layer: "FrameLisp",
    kind: "SpecialForm",
    signature: {
      params: [
        { name: "ms", type: "Int" },
        { name: "flow", type: "Flow[A]" },
      ],
      returns: "Flow[A]",
    },
    effects: ["Control", "Clock"],
    doc: {
      summary: "Run flow with timeout.",
      detail: "Fails with :timeout if time exceeded.",
    },
    lowering: { kind: "Intrinsic", irTag: "FWithTimeout" },
    version: "1.0.0",
  },

  // === Concurrency ===
  {
    id: "framelisp/all",
    layer: "FrameLisp",
    kind: "Function",
    signature: {
      params: [{ name: "flows", type: "List[Flow[A]]" }],
      returns: "Flow[List[A]]",
    },
    effects: ["Concurrency"],
    doc: {
      summary: "Run all flows, collect all results.",
      detail: "Fails if any flow fails (fail-fast by default).",
    },
    lowering: { kind: "Intrinsic", irTag: "FAll" },
    version: "1.0.0",
  },

  {
    id: "framelisp/race",
    layer: "FrameLisp",
    kind: "Function",
    signature: {
      params: [{ name: "flows", type: "List[Flow[A]]" }],
      returns: "Flow[A]",
    },
    effects: ["Concurrency"],
    doc: {
      summary: "Run flows, return first success.",
      detail: "Cancels remaining flows after first completion.",
    },
    lowering: { kind: "Intrinsic", irTag: "FRace" },
    version: "1.0.0",
  },

  {
    id: "framelisp/any",
    layer: "FrameLisp",
    kind: "Function",
    signature: {
      params: [{ name: "flows", type: "List[Flow[A]]" }],
      returns: "Flow[A]",
    },
    effects: ["Concurrency"],
    doc: {
      summary: "Run flows, return first non-failure.",
      detail: "Tries flows in order, returns first success.",
    },
    lowering: { kind: "Intrinsic", irTag: "FAny" },
    version: "1.0.0",
  },

  // === Storage & IO ===
  {
    id: "framelisp/commit",
    layer: "FrameLisp",
    kind: "Function",
    signature: {
      params: [
        { name: "store", type: "StoreRef" },
        { name: "key", type: "Str" },
        { name: "value", type: "Any" },
      ],
      returns: "Flow[Unit]",
    },
    effects: ["Store"],
    doc: {
      summary: "Write value to persistent store.",
    },
    lowering: { kind: "Intrinsic", irTag: "FCommit" },
    version: "1.0.0",
  },

  {
    id: "framelisp/emit",
    layer: "FrameLisp",
    kind: "Function",
    signature: {
      params: [
        { name: "sink", type: "SinkRef" },
        { name: "item", type: "Any" },
      ],
      returns: "Flow[Unit]",
    },
    effects: ["Sink"],
    doc: {
      summary: "Emit item to output sink.",
    },
    lowering: { kind: "Intrinsic", irTag: "FEmit" },
    version: "1.0.0",
  },

  {
    id: "framelisp/observe",
    layer: "FrameLisp",
    kind: "Function",
    signature: {
      params: [
        { name: "source", type: "SourceRef" },
        { name: "query", type: "Any", optional: true },
      ],
      returns: "Flow[Any]",
    },
    effects: ["Source"],
    doc: {
      summary: "Observe from input source.",
    },
    lowering: { kind: "Intrinsic", irTag: "FObserve" },
    version: "1.0.0",
  },

  // === Validation ===
  {
    id: "framelisp/validate",
    layer: "FrameLisp",
    kind: "Function",
    signature: {
      params: [
        { name: "schema", type: "SchemaRef" },
        { name: "value", type: "Any" },
      ],
      returns: "Flow[Any]",
    },
    effects: ["Pure"],
    doc: {
      summary: "Validate value against schema.",
      detail: "Fails with :validation-failed if invalid.",
    },
    lowering: { kind: "Intrinsic", irTag: "FValidate" },
    version: "1.0.0",
  },
];
