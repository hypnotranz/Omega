import type { PrimitiveDescriptor } from "../types";

export const lambdaLLMDescriptors: PrimitiveDescriptor[] = [
  {
    id: "lambdallm.llm/complete",
    layer: "LambdaLLM",
    kind: "Function",
    signature: {
      params: [
        { name: "prompt", type: "Prompt" },
        { name: "system", type: "Str", optional: true },
      ],
      returns: "Str",
    },
    effects: ["Oracle"],
    resources: { estimate: { llmCalls: 1, tokens: "promptTokens + maxTokens" } },
    doc: {
      summary: "High-level completion wrapper over FrameLisp infer.",
      detail: "Packages prompt and optional system instructions into a single inference call.",
    },
    lowering: { kind: "MacroExpand", irTag: "FInfer" },
    constraints: { mustBeDominatedByBudget: true },
    version: "1.0.0",
  },
  {
    id: "lambdallm.llm/chat",
    layer: "LambdaLLM",
    kind: "Function",
    signature: {
      params: [
        { name: "messages", type: "List[Message]" },
        { name: "tools", type: "List[ToolDef]", optional: true },
      ],
      returns: "Message",
    },
    effects: ["Oracle", "Tool"],
    resources: { estimate: { llmCalls: 1, tokens: "promptTokens + responseTokens" } },
    doc: {
      summary: "Multi-turn chat with optional tool calls.",
      detail: "Expands to FrameLisp infer with tool contracts included when provided.",
    },
    lowering: { kind: "LowerHook", hook: "lambdallm.lower/chat" },
    constraints: { mustBeDominatedByBudget: true, requiresToolContract: true },
    version: "1.0.0",
  },
  {
    id: "lambdallm.prompt/prompt+",
    layer: "LambdaLLM",
    kind: "Function",
    signature: {
      params: [
        { name: "p1", type: "Prompt" },
        { name: "p2", type: "Prompt" },
      ],
      returns: "Prompt",
    },
    effects: ["Pure"],
    doc: {
      summary: "Concatenate prompts in sequence.",
      detail: "Used to build larger prompts from reusable blocks.",
      laws: ["prompt+(p1, p2) is associative."],
    },
    lowering: { kind: "MacroExpand", hook: "lambdallm.lower/promptConcat" },
    version: "1.0.0",
  },
  {
    id: "lambdallm.tooling/with-tools",
    layer: "LambdaLLM",
    kind: "Macro",
    signature: {
      params: [
        { name: "tools", type: "List[ToolContract]" },
        { name: "body", type: "Flow[A]" },
      ],
      returns: "Flow[A]",
    },
    effects: ["Control", "Tool"],
    doc: {
      summary: "Scope tool availability for a body of code.",
      detail: "Ensures tool contracts are attached and validated in the compiled bundle.",
    },
    lowering: { kind: "LowerHook", hook: "lambdallm.lower/withTools" },
    constraints: { requiresToolContract: true },
    version: "1.0.0",
  },
];
