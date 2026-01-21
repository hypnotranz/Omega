import type { PrimitiveDescriptor } from "../types";

export const omegaLLMDescriptors: PrimitiveDescriptor[] = [
  {
    id: "omegallm/runtime/eval-omega",
    layer: "OmegaLLM",
    kind: "Function",
    signature: {
      params: [{ name: "source", type: "Str" }],
      returns: "Outcome",
    },
    effects: ["Control"],
    doc: {
      summary: "Evaluate Omega code and return an Outcome.",
      detail: "Entry point for running small snippets inside the runtime evaluator.",
    },
    lowering: { kind: "LowerHook", hook: "omega.runtime/eval" },
    version: "1.0.0",
  },
  {
    id: "omegallm/stream/map",
    layer: "OmegaLLM",
    kind: "Function",
    signature: {
      params: [
        { name: "f", type: "Fn[A -> B]" },
        { name: "stream", type: "Stream[A]" },
      ],
      returns: "Stream[B]",
    },
    effects: ["Pure"],
    doc: {
      summary: "Map over a lazy stream.",
      detail: "Pure transformation; evaluation is deferred to stream forcing.",
    },
    lowering: { kind: "Intrinsic", irTag: "StreamMap" },
    version: "1.0.0",
  },
  {
    id: "omegallm/provenance/log-evidence",
    layer: "OmegaLLM",
    kind: "Function",
    signature: {
      params: [
        { name: "evidence", type: "Evidence" },
        { name: "sink", type: "SinkRef", optional: true },
      ],
      returns: "Unit",
    },
    effects: ["Sink"],
    doc: {
      summary: "Record provenance evidence to a sink.",
      detail: "Used by runtime to emit receipts and provenance metadata.",
    },
    lowering: { kind: "LowerHook", hook: "omega.provenance/logEvidence" },
    version: "1.0.0",
  },
];
