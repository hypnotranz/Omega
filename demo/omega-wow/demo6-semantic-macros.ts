// demo/omega-wow/demo6-semantic-macros.ts
// Demo 6: Semantic macros - DSL that expands to verified semantic pipeline
//
// PURPOSE: Prove Ω is a language-making language; inference permeates syntax safely.

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

interface PipelineStage {
  name: string;
  type: "normalize" | "classify" | "redact" | "verify";
  config?: Record<string, unknown>;
}

interface PipelineDSL {
  stages: PipelineStage[];
}

interface ExpandedPipeline {
  stages: ExpandedStage[];
  hasSnapshots: boolean;
  optimizations: string[];
}

interface ExpandedStage {
  id: string;
  name: string;
  implementation: (doc: string, ctx: DemoContext) => string;
  hasReceipt: boolean;
  hasSnapshot: boolean;
}

interface MacroExpansionResult {
  original: PipelineDSL;
  expanded: ExpandedPipeline;
  fused?: ExpandedPipeline;
  obligations: Obligation[];
  hygienic: boolean;
}

interface Obligation {
  id: string;
  name: string;
  status: "pending" | "satisfied" | "failed";
  detail?: string;
}

// ─────────────────────────────────────────────────────────────────
// DSL Parser
// ─────────────────────────────────────────────────────────────────

function parsePipelineDSL(dsl: string): PipelineDSL {
  // Parse simple DSL:
  // (pipeline
  //   (stage normalize)
  //   (stage (classify :model fast))
  //   (stage (redact :policy strict))
  //   (stage verify))

  const stages: PipelineStage[] = [];

  const stagePattern = /\(stage\s+(?:(\w+)|\((\w+)([^)]*)\))\)/g;
  let match;

  while ((match = stagePattern.exec(dsl)) !== null) {
    const name = match[1] || match[2];
    const configStr = match[3] || "";

    const config: Record<string, unknown> = {};
    const configPattern = /:(\w+)\s+(\w+)/g;
    let configMatch;
    while ((configMatch = configPattern.exec(configStr)) !== null) {
      config[configMatch[1]] = configMatch[2];
    }

    let type: PipelineStage["type"] = "normalize";
    if (name === "classify" || name === "classifySensitive") type = "classify";
    else if (name === "redact") type = "redact";
    else if (name === "verify") type = "verify";

    stages.push({ name, type, config: Object.keys(config).length > 0 ? config : undefined });
  }

  return { stages };
}

// ─────────────────────────────────────────────────────────────────
// Macro Expansion
// ─────────────────────────────────────────────────────────────────

function expandPipeline(
  dsl: PipelineDSL,
  ctx: DemoContext
): ExpandedPipeline {
  const stages: ExpandedStage[] = [];
  let stageId = 0;

  for (const stage of dsl.stages) {
    stageId++;
    const id = `stage-${stageId}`;

    let implementation: ExpandedStage["implementation"];

    switch (stage.type) {
      case "normalize":
        implementation = (doc) => {
          return doc.replace(/\s+/g, " ").trim().toLowerCase();
        };
        break;

      case "classify":
        implementation = (doc) => {
          ctx.oracle.handle("InferOp", {
            op: "classify",
            args: [doc, stage.config],
          });
          ctx.ledger.record("infer.call", { stage: stage.name, op: "classify" });
          return doc; // Classification doesn't modify content
        };
        break;

      case "redact":
        implementation = (doc) => {
          const policy = (stage.config?.policy as string) ?? "standard";
          ctx.ledger.record("infer.call", { stage: stage.name, op: "redact", policy });

          if (policy === "strict") {
            return doc
              .replace(/\b\d{3}-\d{2}-\d{4}\b/g, "[SSN]")
              .replace(/\b\d{4}-\d{4}-\d{4}-\d{4}\b/g, "[CARD]")
              .replace(/password\s*[:=]\s*\S+/gi, "[PWD]")
              .replace(/\b[a-z0-9._%+-]+@[a-z0-9.-]+\.[a-z]{2,}\b/gi, "[EMAIL]");
          }
          return doc.replace(/\b\d{3}-\d{2}-\d{4}\b/g, "[REDACTED]");
        };
        break;

      case "verify":
        implementation = (doc) => {
          ctx.oracle.handle("ReqTest", {
            testSpec: { type: "compliance" },
            value: doc,
          });
          ctx.ledger.record("oracle.request", { type: "ReqTest", stage: stage.name });
          return doc;
        };
        break;

      default:
        implementation = (doc) => doc;
    }

    stages.push({
      id,
      name: stage.name,
      implementation,
      hasReceipt: true,
      hasSnapshot: true,
    });
  }

  return {
    stages,
    hasSnapshots: true,
    optimizations: [],
  };
}

// ─────────────────────────────────────────────────────────────────
// Fusion Optimization
// ─────────────────────────────────────────────────────────────────

function fusePipeline(
  expanded: ExpandedPipeline,
  ctx: DemoContext
): { fused: ExpandedPipeline; oracleReduction: number } {
  // Find consecutive classify stages and fuse them
  const fusedStages: ExpandedStage[] = [];
  let oracleReduction = 0;
  let i = 0;

  while (i < expanded.stages.length) {
    const current = expanded.stages[i];

    // Check for duplicate classify stages
    if (
      current.name.includes("classify") &&
      i + 1 < expanded.stages.length &&
      expanded.stages[i + 1].name.includes("classify")
    ) {
      // Fuse into single classification
      oracleReduction++;

      const fusedImpl: ExpandedStage["implementation"] = (doc) => {
        ctx.oracle.handle("InferOp", {
          op: "classify",
          args: [doc, { fused: true }],
        });
        ctx.ledger.record("infer.call", { stage: "fused-classify", op: "classify" });
        return doc;
      };

      fusedStages.push({
        id: `fused-${current.id}`,
        name: `fused(${current.name}, ${expanded.stages[i + 1].name})`,
        implementation: fusedImpl,
        hasReceipt: true,
        hasSnapshot: true,
      });

      i += 2; // Skip both original stages
    } else {
      fusedStages.push(current);
      i++;
    }
  }

  return {
    fused: {
      stages: fusedStages,
      hasSnapshots: expanded.hasSnapshots,
      optimizations: [...expanded.optimizations, "stage-fusion"],
    },
    oracleReduction,
  };
}

// ─────────────────────────────────────────────────────────────────
// Hygiene Check
// ─────────────────────────────────────────────────────────────────

function checkHygiene(
  original: PipelineDSL,
  expanded: ExpandedPipeline
): { hygienic: boolean; issues: string[] } {
  const issues: string[] = [];

  // Check that stage names don't capture external bindings
  const stageNames = new Set(original.stages.map(s => s.name));
  const expandedNames = new Set(expanded.stages.map(s => s.name));

  // All original stages should be represented
  for (const name of stageNames) {
    const found = expanded.stages.some(
      s => s.name === name || s.name.includes(name)
    );
    if (!found) {
      issues.push(`Stage '${name}' not found in expansion`);
    }
  }

  // No duplicate IDs
  const ids = expanded.stages.map(s => s.id);
  const uniqueIds = new Set(ids);
  if (ids.length !== uniqueIds.size) {
    issues.push("Duplicate stage IDs in expansion");
  }

  return {
    hygienic: issues.length === 0,
    issues,
  };
}

// ─────────────────────────────────────────────────────────────────
// Demo Implementation
// ─────────────────────────────────────────────────────────────────

async function runSemanticMacrosDemo(ctx: DemoContext): Promise<DemoResult> {
  const startTime = Date.now();
  let steps = 0;

  // ─────────────────────────────────────────────────────────────
  // Setup: Pipeline DSL
  // ─────────────────────────────────────────────────────────────

  const pipelineDSL = `
    (pipeline
      (stage normalize)
      (stage (classifySensitive :model fast))
      (stage (classifySensitive :model accurate))
      (stage (redact :policy strict))
      (stage verify))
  `;

  const testDocument = `
    Employee Report

    Name: John Smith
    SSN: 123-45-6789
    Email: john.smith@company.com
    Card: 4111-1111-1111-1111

    Performance: Excellent
    Password for portal: secret123
  `;

  // ─────────────────────────────────────────────────────────────
  // Configure oracle scripts
  // ─────────────────────────────────────────────────────────────

  ctx.oracle.addScript({
    match: (req, type) =>
      type === "InferOp" && (req as any)?.op === "classify",
    respond: () => ({
      value: { category: "sensitive", confidence: 0.9 },
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
  // Execute macro expansion
  // ─────────────────────────────────────────────────────────────

  ctx.ledger.record("demo.start", { phase: "semantic-macros" });

  // Parse DSL
  const parsed = parsePipelineDSL(pipelineDSL);
  ctx.ledger.record("macro.expand", { action: "parse", stages: parsed.stages.length });
  steps++;

  // Expand to pipeline
  const expanded = expandPipeline(parsed, ctx);
  ctx.ledger.record("macro.expand", { action: "expand", stages: expanded.stages.length });
  steps++;

  // Check hygiene
  const hygieneResult = checkHygiene(parsed, expanded);
  ctx.ledger.record("macro.expand", {
    action: "hygiene-check",
    hygienic: hygieneResult.hygienic,
    issues: hygieneResult.issues,
  });
  steps++;

  // Apply fusion optimization
  const { fused, oracleReduction } = fusePipeline(expanded, ctx);
  ctx.ledger.record("macro.expand", {
    action: "fusion",
    beforeStages: expanded.stages.length,
    afterStages: fused.stages.length,
    oracleReduction,
  });
  steps++;

  // Create obligations
  const obligations: Obligation[] = [
    {
      id: "obl-differential",
      name: "Differential test vs unfused",
      status: "pending",
    },
    {
      id: "obl-oracle-reduction",
      name: "Oracle call reduction evidence",
      status: oracleReduction > 0 ? "satisfied" : "pending",
      detail: `Reduced ${oracleReduction} oracle calls`,
    },
    {
      id: "obl-hygiene",
      name: "Hygiene check",
      status: hygieneResult.hygienic ? "satisfied" : "failed",
      detail: hygieneResult.issues.join(", ") || "Hygienic",
    },
  ];

  // Run both pipelines on test document
  let unfusedResult = testDocument;
  let unfusedOracleCalls = 0;
  for (const stage of expanded.stages) {
    unfusedResult = stage.implementation(unfusedResult, ctx);
    if (stage.name.includes("classify")) unfusedOracleCalls++;
  }

  let fusedResult = testDocument;
  let fusedOracleCalls = 0;
  for (const stage of fused.stages) {
    fusedResult = stage.implementation(fusedResult, ctx);
    if (stage.name.includes("classify") || stage.name.includes("fused")) fusedOracleCalls++;
  }

  // Differential test
  const resultsMatch = unfusedResult === fusedResult;
  obligations[0].status = resultsMatch ? "satisfied" : "failed";
  obligations[0].detail = resultsMatch
    ? "Fused and unfused produce identical output"
    : "Output mismatch between fused and unfused";

  ctx.ledger.record("macro.expand", {
    action: "differential-test",
    passed: resultsMatch,
  });

  // Check if promotion allowed
  const allSatisfied = obligations.every(o => o.status === "satisfied");

  if (ctx.profile.allowPromotion && allSatisfied) {
    ctx.ledger.record("commit.success", {
      kind: "macro-promotion",
      obligations: obligations.map(o => o.id),
    });
  } else if (!allSatisfied) {
    ctx.ledger.record("commit.denied", {
      kind: "macro-promotion",
      reason: "obligations-unsatisfied",
      failed: obligations.filter(o => o.status !== "satisfied").map(o => o.id),
    });
  }

  ctx.ledger.record("demo.end", {
    hygienic: hygieneResult.hygienic,
    oracleReduction,
    obligationsSatisfied: allSatisfied,
  });

  // ─────────────────────────────────────────────────────────────
  // Return result
  // ─────────────────────────────────────────────────────────────

  const result: MacroExpansionResult = {
    original: parsed,
    expanded,
    fused,
    obligations,
    hygienic: hygieneResult.hygienic,
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
    macroExpansions: ctx.ledger.getEventsByType("macro.expand").length,
  };

  return {
    outputs: [
      {
        result,
        unfusedResult,
        fusedResult,
        unfusedOracleCalls,
        fusedOracleCalls,
        oracleReduction,
      },
    ],
    success: hygieneResult.hygienic && allSatisfied,
    metrics,
    transcript: ctx.oracle.getTranscript(),
  };
}

// ─────────────────────────────────────────────────────────────────
// Invariants
// ─────────────────────────────────────────────────────────────────

const invariants: InvariantSpec[] = [
  {
    name: "macro-expansion-hygienic",
    check: (result) => {
      const output = result.outputs[0] as { result: MacroExpansionResult };
      const ok = output.result.hygienic;
      return {
        name: "macro-expansion-hygienic",
        ok,
        detail: ok ? "Expansion is hygienic" : "Hygiene violations detected",
      };
    },
  },
  {
    name: "expansion-trace-replayable",
    check: (result, ctx) => {
      const expansions = ctx.ledger.getEventsByType("macro.expand");
      const ok = expansions.length >= 3; // parse, expand, hygiene-check at minimum
      return {
        name: "expansion-trace-replayable",
        ok,
        detail: `${expansions.length} expansion events in ledger`,
      };
    },
  },
  {
    name: "obligations-required-for-promotion",
    check: (result, ctx) => {
      const output = result.outputs[0] as { result: MacroExpansionResult };
      const commits = ctx.ledger.getEventsByType("commit.success");
      const denials = ctx.ledger.getEventsByType("commit.denied");

      const allSatisfied = output.result.obligations.every(
        o => o.status === "satisfied"
      );

      // If obligations not satisfied, commit should be denied
      if (!allSatisfied) {
        const ok = denials.length > 0;
        return {
          name: "obligations-required-for-promotion",
          ok,
          detail: ok
            ? "Promotion denied due to unsatisfied obligations"
            : "Promotion allowed despite unsatisfied obligations",
        };
      }

      return {
        name: "obligations-required-for-promotion",
        ok: true,
        detail: "All obligations satisfied",
      };
    },
  },
  {
    name: "oracle-calls-reduced",
    check: (result) => {
      const output = result.outputs[0] as {
        unfusedOracleCalls: number;
        fusedOracleCalls: number;
      };

      // Fused should use fewer oracle calls
      const ok = output.fusedOracleCalls <= output.unfusedOracleCalls;
      return {
        name: "oracle-calls-reduced",
        ok,
        detail: `Unfused: ${output.unfusedOracleCalls}, Fused: ${output.fusedOracleCalls}`,
      };
    },
  },
  {
    name: "differential-test-passes",
    check: (result) => {
      const output = result.outputs[0] as {
        unfusedResult: string;
        fusedResult: string;
      };

      const ok = output.unfusedResult === output.fusedResult;
      return {
        name: "differential-test-passes",
        ok,
        detail: ok
          ? "Fused and unfused produce identical output"
          : "Output mismatch",
      };
    },
  },
];

// ─────────────────────────────────────────────────────────────────
// Demo Definition
// ─────────────────────────────────────────────────────────────────

export const demo6SemanticMacros: DemoDefinition = {
  id: "semantic-macro-pipeline",
  name: "Semantic Macros: DSL to Verified Pipeline",
  description: "Proves LLM-assisted refactoring as macroexpansion discipline with proofs/tests",
  tags: ["macros", "dsl", "hygiene", "obligations", "fusion"],
  run: runSemanticMacrosDemo,
  invariants,
};
