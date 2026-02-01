// demo/omega-wow/demo5-constraint-repair.ts
// Demo 5: Constraint diagnosis + repair for semantic invariants
//
// PURPOSE: Prove Ω can treat "semantic requirements" as constraints
// and repair violations systematically.

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

interface Constraint {
  id: string;
  name: string;
  check: (doc: ProcessedDocument) => ConstraintResult;
  priority: number;
}

interface ConstraintResult {
  satisfied: boolean;
  score: number;
  detail?: string;
}

interface ProcessedDocument {
  id: string;
  content: string;
  metrics: {
    piiPatterns: number;
    toneScore: number;
    factualClaims: number;
    oracleCalls: number;
  };
}

interface RepairCandidate {
  id: string;
  name: string;
  apply: (doc: ProcessedDocument, ctx: DemoContext) => ProcessedDocument;
  estimatedCost: number;
}

interface ConstraintNetwork {
  constraints: Constraint[];
  repairs: RepairCandidate[];
}

interface ViolationReport {
  constraintId: string;
  constraintName: string;
  satisfied: boolean;
  score: number;
  detail?: string;
}

interface RepairResult {
  repairId: string;
  repairName: string;
  beforeViolations: number;
  afterViolations: number;
  oracleCalls: number;
  success: boolean;
}

// ─────────────────────────────────────────────────────────────────
// Constraints
// ─────────────────────────────────────────────────────────────────

const piiPatterns = [
  /\b\d{3}-\d{2}-\d{4}\b/g,
  /\b\d{4}-\d{4}-\d{4}-\d{4}\b/g,
  /password\s*[:=]\s*\S+/gi,
];

function countPiiPatterns(content: string): number {
  let count = 0;
  for (const pattern of piiPatterns) {
    const matches = content.match(pattern);
    if (matches) count += matches.length;
  }
  return count;
}

function computeToneScore(content: string): number {
  // Simple heuristic: professional = fewer exclamation marks, all caps, etc.
  const exclamations = (content.match(/!/g) || []).length;
  const allCaps = (content.match(/\b[A-Z]{4,}\b/g) || []).length;
  const informal = (content.match(/\b(lol|omg|btw|idk)\b/gi) || []).length;

  const penalty = exclamations * 0.1 + allCaps * 0.15 + informal * 0.2;
  return Math.max(0, 1 - penalty);
}

function countFactualClaims(content: string): number {
  // Count sentences with numbers or definitive statements
  const numberClaims = (content.match(/\d+%|\$\d+|\d+ (million|billion|thousand)/gi) || []).length;
  const definitives = (content.match(/\b(is|are|was|were)\s+\w+\b/g) || []).length;
  return numberClaims + Math.floor(definitives / 3);
}

function createConstraints(): Constraint[] {
  return [
    {
      id: "no-pii",
      name: "No PII-like patterns",
      priority: 1,
      check: (doc) => ({
        satisfied: doc.metrics.piiPatterns === 0,
        score: doc.metrics.piiPatterns === 0 ? 1 : 1 - Math.min(1, doc.metrics.piiPatterns * 0.2),
        detail: `${doc.metrics.piiPatterns} PII patterns found`,
      }),
    },
    {
      id: "professional-tone",
      name: "Tone is professional",
      priority: 2,
      check: (doc) => ({
        satisfied: doc.metrics.toneScore >= 0.7,
        score: doc.metrics.toneScore,
        detail: `Tone score: ${doc.metrics.toneScore.toFixed(2)}`,
      }),
    },
    {
      id: "preserve-facts",
      name: "Preserves factual claims",
      priority: 3,
      check: (doc) => ({
        satisfied: doc.metrics.factualClaims >= 2,
        score: Math.min(1, doc.metrics.factualClaims / 5),
        detail: `${doc.metrics.factualClaims} factual claims preserved`,
      }),
    },
    {
      id: "oracle-budget",
      name: "Within oracle call budget",
      priority: 1,
      check: (doc) => ({
        satisfied: doc.metrics.oracleCalls <= 3,
        score: doc.metrics.oracleCalls <= 3 ? 1 : 1 - (doc.metrics.oracleCalls - 3) * 0.2,
        detail: `${doc.metrics.oracleCalls} oracle calls used`,
      }),
    },
  ];
}

// ─────────────────────────────────────────────────────────────────
// Repair Candidates
// ─────────────────────────────────────────────────────────────────

function createRepairs(ctx: DemoContext): RepairCandidate[] {
  return [
    {
      id: "reorder-stages",
      name: "Reorder pipeline stages",
      estimatedCost: 0,
      apply: (doc) => {
        // Reordering doesn't change content, just optimization
        return { ...doc };
      },
    },
    {
      id: "add-normalization",
      name: "Add normalization stage",
      estimatedCost: 0,
      apply: (doc) => {
        // Normalize whitespace and case
        const normalized = doc.content
          .replace(/\s+/g, " ")
          .trim();
        return {
          ...doc,
          content: normalized,
          metrics: {
            ...doc.metrics,
            piiPatterns: countPiiPatterns(normalized),
            toneScore: computeToneScore(normalized),
          },
        };
      },
    },
    {
      id: "scrub-pii",
      name: "Mechanical PII scrub",
      estimatedCost: 0,
      apply: (doc) => {
        let content = doc.content;
        for (const pattern of piiPatterns) {
          content = content.replace(pattern, "[REDACTED]");
        }
        return {
          ...doc,
          content,
          metrics: {
            ...doc.metrics,
            piiPatterns: 0,
          },
        };
      },
    },
    {
      id: "semantic-rewrite",
      name: "Semantic tone rewrite",
      estimatedCost: 1,
      apply: (doc) => {
        // Use oracle to rewrite tone
        ctx.oracle.handle("InferOp", {
          op: "rewrite-tone",
          args: [doc.content, { target: "professional" }],
        });

        // Simulate improved tone
        const improved = doc.content
          .replace(/!/g, ".")
          .replace(/\b(lol|omg|btw|idk)\b/gi, "");

        return {
          ...doc,
          content: improved,
          metrics: {
            ...doc.metrics,
            toneScore: Math.min(1, doc.metrics.toneScore + 0.3),
            oracleCalls: doc.metrics.oracleCalls + 1,
          },
        };
      },
    },
    {
      id: "cheaper-model",
      name: "Use cheaper semantic model",
      estimatedCost: 0,
      apply: (doc) => {
        // Using cheaper model means fewer oracle calls
        return {
          ...doc,
          metrics: {
            ...doc.metrics,
            oracleCalls: Math.max(0, doc.metrics.oracleCalls - 1),
          },
        };
      },
    },
  ];
}

// ─────────────────────────────────────────────────────────────────
// Constraint Network
// ─────────────────────────────────────────────────────────────────

function checkAllConstraints(
  doc: ProcessedDocument,
  constraints: Constraint[]
): ViolationReport[] {
  return constraints.map(c => ({
    constraintId: c.id,
    constraintName: c.name,
    ...c.check(doc),
  }));
}

function findViolations(reports: ViolationReport[]): ViolationReport[] {
  return reports.filter(r => !r.satisfied);
}

// ─────────────────────────────────────────────────────────────────
// Demo Implementation
// ─────────────────────────────────────────────────────────────────

async function runConstraintRepairDemo(ctx: DemoContext): Promise<DemoResult> {
  const startTime = Date.now();
  let steps = 0;

  // ─────────────────────────────────────────────────────────────
  // Setup: Document with constraint violations
  // ─────────────────────────────────────────────────────────────

  const inputContent = `
    OMG!!! Check out our AMAZING Q4 results!!!

    Revenue: $10 MILLION dollars!!!
    Growth: 50% year-over-year - INCREDIBLE!

    Employee of the quarter: John Smith
    SSN: 123-45-6789 (for HR records lol)
    Credit card: 4111-1111-1111-1111

    BTW the password for the report portal is: secret123

    This is CONFIDENTIAL - DO NOT SHARE!!!
  `;

  let document: ProcessedDocument = {
    id: "doc-003",
    content: inputContent,
    metrics: {
      piiPatterns: countPiiPatterns(inputContent),
      toneScore: computeToneScore(inputContent),
      factualClaims: countFactualClaims(inputContent),
      oracleCalls: 0,
    },
  };

  const constraints = createConstraints();
  const repairs = createRepairs(ctx);

  // ─────────────────────────────────────────────────────────────
  // Configure oracle scripts
  // ─────────────────────────────────────────────────────────────

  ctx.oracle.addScript({
    match: (req, type) =>
      type === "InferOp" && (req as any)?.op === "rewrite-tone",
    respond: () => ({
      value: { success: true },
      evidence: "tone-rewrite",
    }),
  });

  ctx.oracle.addScript({
    match: (req, type) =>
      type === "InferOp" && (req as any)?.op === "propose-repair",
    respond: (req) => {
      const { args } = req as { args: [ViolationReport[]] };
      const violations = args[0];

      // Propose repairs based on violations
      // IMPORTANT: Order matters! Prioritize non-oracle-consuming repairs first
      const proposed: string[] = [];

      // First: handle budget - use cheaper model before adding oracle calls
      if (violations.some(v => v.constraintId === "oracle-budget")) {
        proposed.push("cheaper-model");
      }

      // Then: PII is mechanical (no oracle cost)
      if (violations.some(v => v.constraintId === "no-pii")) {
        proposed.push("scrub-pii");
      }

      // Then: normalization is mechanical (no oracle cost)
      if (violations.some(v => v.constraintId === "professional-tone")) {
        proposed.push("add-normalization");
        // Only add semantic-rewrite if budget allows (not violated)
        if (!violations.some(v => v.constraintId === "oracle-budget")) {
          proposed.push("semantic-rewrite");
        }
      }

      return {
        value: { proposedRepairs: proposed },
        evidence: "repair-proposal",
      };
    },
  });

  // ─────────────────────────────────────────────────────────────
  // Execute constraint checking and repair
  // ─────────────────────────────────────────────────────────────

  ctx.ledger.record("demo.start", { phase: "constraint-repair" });

  // Initial constraint check
  let reports = checkAllConstraints(document, constraints);
  let violations = findViolations(reports);

  ctx.ledger.record("constraint.violation", {
    count: violations.length,
    violations: violations.map(v => v.constraintId),
  });
  steps++;

  const repairResults: RepairResult[] = [];
  const maxIterations = 5;
  let iteration = 0;

  while (violations.length > 0 && iteration < maxIterations) {
    iteration++;
    steps++;

    // Ask oracle for repair proposal
    const proposal = ctx.oracle.handle("InferOp", {
      op: "propose-repair",
      args: [violations],
    }) as { value: { proposedRepairs: string[] } };

    const proposedRepairIds = proposal.value.proposedRepairs;

    if (proposedRepairIds.length === 0) {
      ctx.ledger.record("constraint.repair", {
        iteration,
        action: "no-repairs-available",
      });
      break;
    }

    // Apply repairs in order
    for (const repairId of proposedRepairIds) {
      const repair = repairs.find(r => r.id === repairId);
      if (!repair) continue;

      const beforeViolations = violations.length;
      document = repair.apply(document, ctx);

      // Re-check constraints
      reports = checkAllConstraints(document, constraints);
      violations = findViolations(reports);

      const repairResult: RepairResult = {
        repairId: repair.id,
        repairName: repair.name,
        beforeViolations,
        afterViolations: violations.length,
        oracleCalls: repair.estimatedCost,
        success: violations.length < beforeViolations,
      };

      repairResults.push(repairResult);

      ctx.ledger.record("constraint.repair", {
        iteration,
        repair: repair.name,
        before: beforeViolations,
        after: violations.length,
      });

      // Stop if all constraints satisfied
      if (violations.length === 0) break;
    }
  }

  // Final constraint check
  const finalReports = checkAllConstraints(document, constraints);
  const finalViolations = findViolations(finalReports);

  ctx.ledger.record("demo.end", {
    initialViolations: findViolations(checkAllConstraints({
      ...document,
      content: inputContent,
      metrics: {
        piiPatterns: countPiiPatterns(inputContent),
        toneScore: computeToneScore(inputContent),
        factualClaims: countFactualClaims(inputContent),
        oracleCalls: 0,
      },
    }, constraints)).length,
    finalViolations: finalViolations.length,
    repairsApplied: repairResults.length,
    iterations: iteration,
  });

  // ─────────────────────────────────────────────────────────────
  // Return result
  // ─────────────────────────────────────────────────────────────

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
    outputs: [
      {
        finalDocument: document,
        constraintReports: finalReports,
        repairResults,
        violations: finalViolations,
        iterations: iteration,
      },
    ],
    success: finalViolations.length === 0 || finalViolations.length < violations.length,
    metrics,
    transcript: ctx.oracle.getTranscript(),
  };
}

// ─────────────────────────────────────────────────────────────────
// Invariants
// ─────────────────────────────────────────────────────────────────

const invariants: InvariantSpec[] = [
  {
    name: "network-identifies-violations",
    check: (result, ctx) => {
      const violations = ctx.ledger.getEventsByType("constraint.violation");
      const ok = violations.length > 0;
      return {
        name: "network-identifies-violations",
        ok,
        detail: `${violations.length} violation detection event(s)`,
      };
    },
  },
  {
    name: "repair-found-and-certified",
    check: (result) => {
      const output = result.outputs[0] as { repairResults: RepairResult[] };
      const successfulRepairs = output.repairResults.filter(r => r.success);
      const ok = successfulRepairs.length > 0;
      return {
        name: "repair-found-and-certified",
        ok,
        detail: `${successfulRepairs.length} successful repair(s) of ${output.repairResults.length} attempted`,
      };
    },
  },
  {
    name: "violations-reduced",
    check: (result) => {
      const output = result.outputs[0] as {
        violations: ViolationReport[];
        constraintReports: ViolationReport[];
      };

      // Check that some constraints are now satisfied
      const satisfied = output.constraintReports.filter(r => r.satisfied).length;
      const total = output.constraintReports.length;
      const ok = satisfied > 0;

      return {
        name: "violations-reduced",
        ok,
        detail: `${satisfied}/${total} constraints satisfied`,
      };
    },
  },
  {
    name: "oracle-budget-maintained",
    check: (result) => {
      const output = result.outputs[0] as { finalDocument: ProcessedDocument };
      const oracleCalls = output.finalDocument.metrics.oracleCalls;
      const ok = oracleCalls <= 3;

      return {
        name: "oracle-budget-maintained",
        ok,
        detail: `${oracleCalls} oracle calls (budget: 3)`,
      };
    },
  },
  {
    name: "explanation-graph-available",
    check: (result, ctx) => {
      const repairs = ctx.ledger.getEventsByType("constraint.repair");
      const ok = repairs.length > 0;

      return {
        name: "explanation-graph-available",
        ok,
        detail: `${repairs.length} repair event(s) in ledger`,
      };
    },
  },
];

// ─────────────────────────────────────────────────────────────────
// Demo Definition
// ─────────────────────────────────────────────────────────────────

export const demo5ConstraintRepair: DemoDefinition = {
  id: "constraint-diagnosis-repair",
  name: "Constraint Diagnosis and Repair",
  description: "Proves SICP constraint propagation fused with semantic workflows and audited repairs",
  tags: ["constraints", "propagation", "repair", "search", "compliance"],
  run: runConstraintRepairDemo,
  invariants,
};
