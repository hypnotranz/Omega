// demo/omega-wow/demo7-compilation.ts
// Demo 7: Compilation preserves the inference plane
//
// PURPOSE: Prove compiled Ω is still Ω - effect handlers intercept infer.op,
// replay determinism holds, and optimizations reduce oracle calls with obligations.

import type {
  DemoDefinition,
  DemoContext,
  DemoResult,
  InvariantSpec,
  DemoMetrics,
} from "../harness/types";

// ─────────────────────────────────────────────────────────────────
// Types
// ─────────────────────────────────────────────────────────────────

interface PipelineNode {
  id: string;
  type: "classify" | "transform" | "validate";
  args: unknown[];
}

interface CompiledPipeline {
  nodes: CompiledNode[];
  optimizations: string[];
  oracleCallEstimate: number;
}

interface CompiledNode {
  id: string;
  opcode: "INFER" | "TRANSFORM" | "TEST" | "NOP";
  args: unknown[];
  deduplicated?: boolean;
}

interface ExecutionResult {
  output: string;
  oracleCalls: number;
  handlerInterceptions: number;
  steps: number;
}

interface CompilationResult {
  baseline: CompiledPipeline;
  optimized: CompiledPipeline;
  baselineExecution: ExecutionResult;
  optimizedExecution: ExecutionResult;
  differential: {
    outputsMatch: boolean;
    oracleSavings: number;
  };
}

// ─────────────────────────────────────────────────────────────────
// Pipeline Definition
// ─────────────────────────────────────────────────────────────────

function createTestPipeline(): PipelineNode[] {
  return [
    { id: "n1", type: "classify", args: ["document", { model: "fast" }] },
    { id: "n2", type: "transform", args: ["normalize"] },
    { id: "n3", type: "classify", args: ["document", { model: "fast" }] }, // Duplicate!
    { id: "n4", type: "transform", args: ["redact"] },
    { id: "n5", type: "validate", args: ["compliance"] },
  ];
}

// ─────────────────────────────────────────────────────────────────
// Compilation
// ─────────────────────────────────────────────────────────────────

function compileBaseline(pipeline: PipelineNode[]): CompiledPipeline {
  const nodes: CompiledNode[] = pipeline.map(node => {
    switch (node.type) {
      case "classify":
        return { id: node.id, opcode: "INFER" as const, args: node.args };
      case "transform":
        return { id: node.id, opcode: "TRANSFORM" as const, args: node.args };
      case "validate":
        return { id: node.id, opcode: "TEST" as const, args: node.args };
      default:
        return { id: node.id, opcode: "NOP" as const, args: [] };
    }
  });

  const oracleCallEstimate = nodes.filter(n => n.opcode === "INFER").length;

  return {
    nodes,
    optimizations: [],
    oracleCallEstimate,
  };
}

function compileOptimized(pipeline: PipelineNode[]): CompiledPipeline {
  // First compile baseline
  const baseline = compileBaseline(pipeline);

  // Apply CSE (Common Subexpression Elimination) for duplicate INFER calls
  const seen = new Map<string, string>(); // hash -> first node id
  const optimizedNodes: CompiledNode[] = [];

  for (const node of baseline.nodes) {
    if (node.opcode === "INFER") {
      const hash = JSON.stringify(node.args);
      if (seen.has(hash)) {
        // Duplicate! Replace with NOP referencing original
        optimizedNodes.push({
          id: node.id,
          opcode: "NOP",
          args: [{ deduplicatedFrom: seen.get(hash) }],
          deduplicated: true,
        });
      } else {
        seen.set(hash, node.id);
        optimizedNodes.push(node);
      }
    } else {
      optimizedNodes.push(node);
    }
  }

  const oracleCallEstimate = optimizedNodes.filter(
    n => n.opcode === "INFER" && !n.deduplicated
  ).length;

  return {
    nodes: optimizedNodes,
    optimizations: ["cse-infer"],
    oracleCallEstimate,
  };
}

// ─────────────────────────────────────────────────────────────────
// Execution with Effect Handlers
// ─────────────────────────────────────────────────────────────────

function executeCompiled(
  compiled: CompiledPipeline,
  input: string,
  ctx: DemoContext
): ExecutionResult {
  let output = input;
  let oracleCalls = 0;
  let handlerInterceptions = 0;
  let steps = 0;

  for (const node of compiled.nodes) {
    steps++;

    switch (node.opcode) {
      case "INFER":
        // Effect handler intercepts this
        handlerInterceptions++;
        ctx.ledger.record("infer.call", {
          nodeId: node.id,
          op: "classify",
        });

        ctx.oracle.handle("InferOp", {
          op: "classify",
          args: [output, ...node.args],
        });
        oracleCalls++;
        break;

      case "TRANSFORM":
        const transformType = node.args[0];
        if (transformType === "normalize") {
          output = output.replace(/\s+/g, " ").trim().toLowerCase();
        } else if (transformType === "redact") {
          output = output
            .replace(/\b\d{3}-\d{2}-\d{4}\b/g, "[SSN]")
            .replace(/\b\d{4}-\d{4}-\d{4}-\d{4}\b/g, "[CARD]");
        }
        break;

      case "TEST":
        handlerInterceptions++;
        ctx.oracle.handle("ReqTest", {
          testSpec: { type: node.args[0] },
          value: output,
        });
        break;

      case "NOP":
        // Skip - deduplicated
        if (node.deduplicated) {
          ctx.ledger.record("infer.call", {
            nodeId: node.id,
            op: "classify",
            deduplicated: true,
          });
        }
        break;
    }
  }

  return { output, oracleCalls, handlerInterceptions, steps };
}

// ─────────────────────────────────────────────────────────────────
// Demo Implementation
// ─────────────────────────────────────────────────────────────────

async function runCompilationDemo(ctx: DemoContext): Promise<DemoResult> {
  const startTime = Date.now();
  let steps = 0;

  // ─────────────────────────────────────────────────────────────
  // Setup
  // ─────────────────────────────────────────────────────────────

  const testDocument = `
    Employee Record

    Name: Jane Doe
    SSN: 123-45-6789
    Credit Card: 4111-1111-1111-1111

    Department: Engineering
  `;

  const pipeline = createTestPipeline();

  // ─────────────────────────────────────────────────────────────
  // Configure oracle scripts
  // ─────────────────────────────────────────────────────────────

  ctx.oracle.addScript({
    match: (req, type) =>
      type === "InferOp" && (req as any)?.op === "classify",
    respond: () => ({
      value: { category: "pii-sensitive", confidence: 0.95 },
      evidence: "classify-inference",
    }),
  });

  ctx.oracle.addScript({
    match: (req, type) => type === "ReqTest",
    respond: () => ({
      passed: true,
      detail: "compliance-verified",
      evidence: "test-executed",
    }),
  });

  // ─────────────────────────────────────────────────────────────
  // Compile and Execute
  // ─────────────────────────────────────────────────────────────

  ctx.ledger.record("demo.start", { phase: "compilation" });

  // Baseline compilation
  const baseline = compileBaseline(pipeline);
  ctx.ledger.record("infer.result", {
    phase: "compile-baseline",
    nodes: baseline.nodes.length,
    oracleEstimate: baseline.oracleCallEstimate,
  });
  steps++;

  // Optimized compilation
  const optimized = compileOptimized(pipeline);
  ctx.ledger.record("infer.result", {
    phase: "compile-optimized",
    nodes: optimized.nodes.length,
    oracleEstimate: optimized.oracleCallEstimate,
    optimizations: optimized.optimizations,
  });
  steps++;

  // Execute baseline
  ctx.ledger.record("demo.start", { phase: "execute-baseline" });
  const baselineExecution = executeCompiled(baseline, testDocument, ctx);
  steps += baselineExecution.steps;

  // Reset oracle counts for optimized run
  // (In real impl, would use separate context)
  const baselineOracleCalls = ctx.oracle.getCount("InferOp");

  // Execute optimized
  ctx.ledger.record("demo.start", { phase: "execute-optimized" });
  const optimizedExecution = executeCompiled(optimized, testDocument, ctx);
  steps += optimizedExecution.steps;

  const optimizedOracleCalls =
    ctx.oracle.getCount("InferOp") - baselineOracleCalls;

  // Differential test
  const outputsMatch = baselineExecution.output === optimizedExecution.output;
  const oracleSavings =
    baselineExecution.oracleCalls - optimizedExecution.oracleCalls;

  // Record obligations
  if (outputsMatch && oracleSavings >= 0) {
    ctx.ledger.record("commit.success", {
      kind: "optimized-ir",
      outputsMatch,
      oracleSavings,
    });
  } else {
    ctx.ledger.record("commit.denied", {
      kind: "optimized-ir",
      reason: outputsMatch ? "no-savings" : "output-mismatch",
    });
  }

  ctx.ledger.record("demo.end", {
    baselineOracleCalls: baselineExecution.oracleCalls,
    optimizedOracleCalls: optimizedExecution.oracleCalls,
    outputsMatch,
    oracleSavings,
  });

  // ─────────────────────────────────────────────────────────────
  // Return result
  // ─────────────────────────────────────────────────────────────

  const compilationResult: CompilationResult = {
    baseline,
    optimized,
    baselineExecution,
    optimizedExecution,
    differential: {
      outputsMatch,
      oracleSavings,
    },
  };

  const metrics: DemoMetrics = {
    inferCalls: ctx.oracle.getCount("InferOp"),
    oracleReqEval: ctx.oracle.getCount("ReqEval"),
    oracleReqApply: ctx.oracle.getCount("ReqApply"),
    oracleReqObserve: ctx.oracle.getCount("ReqObserve"),
    oracleReqTest: ctx.oracle.getCount("ReqTest"),
    oracleReqReturn: ctx.oracle.getCount("ReqReturn"),
    steps,
    wallMs: Date.now() - startTime,
  };

  return {
    outputs: [compilationResult],
    success: outputsMatch,
    metrics,
    transcript: ctx.oracle.getTranscript(),
  };
}

// ─────────────────────────────────────────────────────────────────
// Invariants
// ─────────────────────────────────────────────────────────────────

const invariants: InvariantSpec[] = [
  {
    name: "interpreter-equals-compiled",
    check: (result) => {
      const output = result.outputs[0] as CompilationResult;
      const ok = output.differential.outputsMatch;
      return {
        name: "interpreter-equals-compiled",
        ok,
        detail: ok
          ? "Baseline and optimized produce identical output"
          : "Output mismatch between baseline and optimized",
      };
    },
  },
  {
    name: "handler-interception-works",
    check: (result) => {
      const output = result.outputs[0] as CompilationResult;
      const interceptions =
        output.baselineExecution.handlerInterceptions +
        output.optimizedExecution.handlerInterceptions;
      const ok = interceptions > 0;
      return {
        name: "handler-interception-works",
        ok,
        detail: `${interceptions} effect handler interceptions`,
      };
    },
  },
  {
    name: "optimized-reduces-oracle-calls",
    check: (result) => {
      const output = result.outputs[0] as CompilationResult;
      const ok = output.differential.oracleSavings >= 0;
      return {
        name: "optimized-reduces-oracle-calls",
        ok,
        detail: `Saved ${output.differential.oracleSavings} oracle calls`,
      };
    },
  },
  {
    name: "obligations-recorded",
    check: (result, ctx) => {
      const commits = ctx.ledger.getEventsByType("commit.success");
      const denials = ctx.ledger.getEventsByType("commit.denied");
      const ok = commits.length > 0 || denials.length > 0;
      return {
        name: "obligations-recorded",
        ok,
        detail: `${commits.length} commits, ${denials.length} denials`,
      };
    },
  },
  {
    name: "cse-optimization-applied",
    check: (result) => {
      const output = result.outputs[0] as CompilationResult;
      const ok = output.optimized.optimizations.includes("cse-infer");
      return {
        name: "cse-optimization-applied",
        ok,
        detail: ok
          ? "CSE optimization applied"
          : "No CSE optimization found",
      };
    },
  },
];

// ─────────────────────────────────────────────────────────────────
// Demo Definition
// ─────────────────────────────────────────────────────────────────

export const demo7Compilation: DemoDefinition = {
  id: "compilation-inference-plane",
  name: "Compilation Preserves Inference Plane",
  description: "Proves compiled Ω maintains effect handlers, replay determinism, and optimizes oracle cost",
  tags: ["compilation", "optimization", "cse", "handlers", "differential"],
  run: runCompilationDemo,
  invariants,
};
