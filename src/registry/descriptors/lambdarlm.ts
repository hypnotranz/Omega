import type { PrimitiveDescriptor } from "../types";

export const lambdaRLMDescriptors: PrimitiveDescriptor[] = [
  {
    id: "lambdarlm/compose-sequential",
    layer: "LambdaRLM",
    kind: "Function",
    signature: {
      params: [
        { name: "s1", type: "Solver" },
        { name: "s2", type: "Solver" },
      ],
      returns: "Solver",
    },
    effects: ["Control"],
    doc: {
      summary: "Pipe the output of s1 into s2 sequentially.",
      laws: [
        "compose-sequential is associative when solver metadata merge is associative.",
      ],
    },
    lowering: { kind: "LowerHook", hook: "lambdarlm.lower/composeSequential" },
    version: "1.0.0",
  },
  {
    id: "lambdarlm/compose-parallel",
    layer: "LambdaRLM",
    kind: "Function",
    signature: {
      params: [
        { name: "solvers", type: "List[Solver]" },
        { name: "merger", type: "Fn[List[Result] -> Result]" },
      ],
      returns: "Solver",
    },
    effects: ["Concurrency"],
    doc: {
      summary: "Run solvers in parallel and merge the results.",
      detail: "Useful for exploration where the best answer is chosen by a reducer.",
    },
    lowering: { kind: "LowerHook", hook: "lambdarlm.lower/composeParallel" },
    version: "1.0.0",
  },
  {
    id: "lambdarlm/repair-until-valid",
    layer: "LambdaRLM",
    kind: "Function",
    signature: {
      params: [
        { name: "generator", type: "Fn[Problem -> Candidate]" },
        { name: "validate", type: "Fn[Candidate -> Bool]" },
        { name: "repair", type: "Fn[Candidate -> Candidate]" },
        { name: "maxIters", type: "Int", optional: true },
      ],
      returns: "Result",
    },
    effects: ["Constraint", "Control"],
    doc: {
      summary: "Iteratively generate, validate, and repair until the candidate passes validation.",
    },
    lowering: { kind: "LowerHook", hook: "lambdarlm.lower/repairUntilValid" },
    version: "1.0.0",
  },
];
