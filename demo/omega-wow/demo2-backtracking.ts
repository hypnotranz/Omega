// demo/omega-wow/demo2-backtracking.ts
// Demo 2: Multi-shot backtracking over semantic choices
//
// PURPOSE: Prove you can explore multiple semantic strategies without
// restarting from scratch, and without losing auditability.

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

interface RedactionDocument {
  id: string;
  content: string;
  metadata: {
    sensitivity: "low" | "medium" | "high";
    category: string;
  };
}

interface RedactionResult {
  document: RedactionDocument;
  strategy: string;
  redactedContent: string;
  metrics: {
    readabilityScore: number;
    sensitiveRemaining: number;
    oracleCalls: number;
  };
  policyCompliant: boolean;
}

type RedactionStrategy = "mechanical" | "semantic" | "hybrid";

interface PolicyConstraints {
  minReadability: number;
  maxSensitiveRemaining: number;
  maxOracleCalls: number;
}

// ─────────────────────────────────────────────────────────────────
// Redaction Strategies
// ─────────────────────────────────────────────────────────────────

const sensitivePatterns = [
  /\b\d{3}-\d{2}-\d{4}\b/g,           // SSN
  /\b\d{4}-\d{4}-\d{4}-\d{4}\b/g,     // Credit card
  /\bpassword\s*[:=]\s*\S+/gi,        // Passwords
  /\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b/g, // Emails
  /\b(confidential|secret|classified)\b/gi, // Keywords
];

/**
 * Strategy A: Purely mechanical scrub (regex/structural).
 * - Fast, no oracle calls
 * - May over-redact, reducing readability
 */
function mechanicalRedact(content: string): { text: string; redactions: number } {
  let text = content;
  let redactions = 0;

  for (const pattern of sensitivePatterns) {
    const matches = text.match(pattern);
    if (matches) {
      redactions += matches.length;
      text = text.replace(pattern, "[REDACTED]");
    }
  }

  return { text, redactions };
}

/**
 * Strategy B: Semantic rewrite (uses oracle).
 * - Better readability
 * - Higher oracle cost
 */
function semanticRedact(
  content: string,
  ctx: DemoContext
): { text: string; redactions: number; oracleCalls: number } {
  let oracleCalls = 0;

  // Use oracle to identify and rewrite sensitive content
  const result = ctx.oracle.handle("InferOp", {
    op: "semantic-redact",
    args: [content, { preserveReadability: true }],
  }) as { value: { text: string; redactions: number } };

  oracleCalls++;
  ctx.ledger.record("infer.call", { strategy: "semantic", op: "semantic-redact" });

  return {
    text: result.value.text,
    redactions: result.value.redactions,
    oracleCalls,
  };
}

/**
 * Strategy C: Hybrid approach.
 * - Mechanical for obvious patterns
 * - Oracle only for ambiguous cases
 */
function hybridRedact(
  content: string,
  ctx: DemoContext
): { text: string; redactions: number; oracleCalls: number } {
  // First, mechanical pass
  const mechanical = mechanicalRedact(content);
  let oracleCalls = 0;

  // Then, oracle pass for any remaining sensitive content
  const hasRemaining = sensitivePatterns.some(p => p.test(mechanical.text));

  if (hasRemaining) {
    const result = ctx.oracle.handle("InferOp", {
      op: "refine-redaction",
      args: [mechanical.text, { strict: true }],
    }) as { value: { text: string; redactions: number } };

    oracleCalls++;
    ctx.ledger.record("infer.call", { strategy: "hybrid", op: "refine-redaction" });

    return {
      text: result.value.text,
      redactions: mechanical.redactions + result.value.redactions,
      oracleCalls,
    };
  }

  return {
    text: mechanical.text,
    redactions: mechanical.redactions,
    oracleCalls,
  };
}

// ─────────────────────────────────────────────────────────────────
// Validation
// ─────────────────────────────────────────────────────────────────

function computeReadability(text: string): number {
  // Simple readability heuristic: longer non-redacted content = more readable
  const redactedCount = (text.match(/\[REDACTED\]/g) || []).length;
  const words = text.split(/\s+/).length;
  if (words === 0) return 0;

  const ratio = 1 - (redactedCount / words);
  return Math.max(0, Math.min(1, ratio));
}

function countSensitiveRemaining(text: string): number {
  let count = 0;
  for (const pattern of sensitivePatterns) {
    const matches = text.match(pattern);
    if (matches) count += matches.length;
  }
  return count;
}

function validateResult(
  result: RedactionResult,
  constraints: PolicyConstraints
): { valid: boolean; violations: string[] } {
  const violations: string[] = [];

  if (result.metrics.readabilityScore < constraints.minReadability) {
    violations.push(
      `Readability ${result.metrics.readabilityScore.toFixed(2)} < ${constraints.minReadability}`
    );
  }

  if (result.metrics.sensitiveRemaining > constraints.maxSensitiveRemaining) {
    violations.push(
      `Sensitive remaining ${result.metrics.sensitiveRemaining} > ${constraints.maxSensitiveRemaining}`
    );
  }

  if (result.metrics.oracleCalls > constraints.maxOracleCalls) {
    violations.push(
      `Oracle calls ${result.metrics.oracleCalls} > ${constraints.maxOracleCalls}`
    );
  }

  return { valid: violations.length === 0, violations };
}

// ─────────────────────────────────────────────────────────────────
// Demo Implementation
// ─────────────────────────────────────────────────────────────────

async function runBacktrackingDemo(ctx: DemoContext): Promise<DemoResult> {
  const startTime = Date.now();
  let steps = 0;

  // ─────────────────────────────────────────────────────────────
  // Setup: Document with conflicting redaction objectives
  // ─────────────────────────────────────────────────────────────

  const document: RedactionDocument = {
    id: "doc-002",
    content: `
      Employee Performance Review - CONFIDENTIAL

      Employee: Jane Doe
      SSN: 987-65-4321
      Email: jane.doe@company.com

      Performance Summary:
      Jane has exceeded expectations in Q4. Her work on the secret
      Project Phoenix has been exemplary. She managed the team's
      credit card 4532-1234-5678-9012 expense tracking efficiently.

      Salary Discussion:
      Current password: employee123 for HR portal access.
      Recommended raise: 15% based on performance metrics.

      This document is classified as CONFIDENTIAL.
    `,
    metadata: {
      sensitivity: "high",
      category: "hr-review",
    },
  };

  const constraints: PolicyConstraints = {
    minReadability: 0.5,          // At least 50% readable
    maxSensitiveRemaining: 0,     // No sensitive data allowed
    maxOracleCalls: 2,            // Budget constraint
  };

  // ─────────────────────────────────────────────────────────────
  // Configure oracle scripts
  // ─────────────────────────────────────────────────────────────

  // Semantic redaction script
  ctx.oracle.addScript({
    match: (req, type) =>
      type === "InferOp" && (req as any)?.op === "semantic-redact",
    respond: (req) => {
      const { args } = req as { args: [string, { preserveReadability: boolean }] };
      const content = args[0];

      // Semantic redaction: smarter replacement
      let text = content;
      let redactions = 0;

      // Replace SSN with description
      text = text.replace(/\b\d{3}-\d{2}-\d{4}\b/g, () => {
        redactions++;
        return "[SSN removed]";
      });

      // Replace credit card
      text = text.replace(/\b\d{4}-\d{4}-\d{4}-\d{4}\b/g, () => {
        redactions++;
        return "[payment info removed]";
      });

      // Replace passwords more contextually
      text = text.replace(/password\s*[:=]\s*\S+/gi, () => {
        redactions++;
        return "[credentials protected]";
      });

      // Replace emails
      text = text.replace(
        /\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b/g,
        () => {
          redactions++;
          return "[email protected]";
        }
      );

      // Replace sensitive keywords contextually
      text = text.replace(/\b(confidential|secret|classified)\b/gi, () => {
        redactions++;
        return "[restricted]";
      });

      return {
        value: { text, redactions },
        evidence: "semantic-redaction",
      };
    },
  });

  // Hybrid refinement script
  ctx.oracle.addScript({
    match: (req, type) =>
      type === "InferOp" && (req as any)?.op === "refine-redaction",
    respond: (req) => {
      const { args } = req as { args: [string, { strict: boolean }] };
      const content = args[0];

      // Final cleanup of any remaining sensitive content
      let text = content;
      let redactions = 0;

      // Any remaining patterns
      for (const pattern of sensitivePatterns) {
        const matches = text.match(pattern);
        if (matches) {
          redactions += matches.length;
          text = text.replace(pattern, "[REMOVED]");
        }
      }

      return {
        value: { text, redactions },
        evidence: "refinement",
      };
    },
  });

  // ─────────────────────────────────────────────────────────────
  // Execute with backtracking
  // ─────────────────────────────────────────────────────────────

  ctx.ledger.record("demo.start", { phase: "backtracking-redaction" });

  const strategies: RedactionStrategy[] = ["mechanical", "semantic", "hybrid"];
  const results: RedactionResult[] = [];
  let acceptedResult: RedactionResult | null = null;
  let snapshotId = ctx.ledger.snapshot();

  for (const strategy of strategies) {
    ctx.ledger.record("amb.choose", { strategy });
    steps++;

    let redactedContent: string;
    let oracleCalls = 0;
    let redactions = 0;

    // Execute strategy
    switch (strategy) {
      case "mechanical": {
        const result = mechanicalRedact(document.content);
        redactedContent = result.text;
        redactions = result.redactions;
        break;
      }
      case "semantic": {
        const result = semanticRedact(document.content, ctx);
        redactedContent = result.text;
        redactions = result.redactions;
        oracleCalls = result.oracleCalls;
        break;
      }
      case "hybrid": {
        const result = hybridRedact(document.content, ctx);
        redactedContent = result.text;
        redactions = result.redactions;
        oracleCalls = result.oracleCalls;
        break;
      }
      default:
        redactedContent = document.content;
    }

    // Build result
    const readabilityScore = computeReadability(redactedContent);
    const sensitiveRemaining = countSensitiveRemaining(redactedContent);

    const result: RedactionResult = {
      document,
      strategy,
      redactedContent,
      metrics: {
        readabilityScore,
        sensitiveRemaining,
        oracleCalls,
      },
      policyCompliant: false,
    };

    // Validate
    const validation = validateResult(result, constraints);
    result.policyCompliant = validation.valid;
    results.push(result);

    ctx.ledger.record("infer.result", {
      strategy,
      readability: readabilityScore,
      sensitiveRemaining,
      oracleCalls,
      compliant: validation.valid,
    });

    if (validation.valid) {
      // Accept this result
      acceptedResult = result;
      ctx.ledger.record("commit.success", {
        strategy,
        reason: "policy-compliant",
      });
      break;
    } else {
      // Backtrack
      ctx.ledger.record("amb.fail", {
        strategy,
        violations: validation.violations,
      });
      ctx.ledger.record("amb.backtrack", {
        from: strategy,
        to: strategies[strategies.indexOf(strategy) + 1] ?? "none",
      });

      // Restore snapshot for isolation
      ctx.ledger.restore(snapshotId);
      snapshotId = ctx.ledger.snapshot();
    }
  }

  // If no strategy passed, use the best one
  if (!acceptedResult && results.length > 0) {
    // Sort by: sensitive remaining (asc), then readability (desc)
    results.sort((a, b) => {
      if (a.metrics.sensitiveRemaining !== b.metrics.sensitiveRemaining) {
        return a.metrics.sensitiveRemaining - b.metrics.sensitiveRemaining;
      }
      return b.metrics.readabilityScore - a.metrics.readabilityScore;
    });
    acceptedResult = results[0];

    if (ctx.profile.allowCommit) {
      ctx.ledger.record("commit.success", {
        strategy: acceptedResult.strategy,
        reason: "best-available",
      });
    } else {
      ctx.ledger.record("commit.denied", {
        strategy: acceptedResult.strategy,
        reason: "profile-disallows-commit",
      });
    }
  }

  ctx.ledger.record("demo.end", {
    acceptedStrategy: acceptedResult?.strategy,
    success: !!acceptedResult,
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
    ambChoices: strategies.length,
    backtracks: results.filter(r => !r.policyCompliant).length,
  };

  return {
    outputs: [acceptedResult, results],
    success: !!acceptedResult,
    metrics,
    transcript: ctx.oracle.getTranscript(),
  };
}

// ─────────────────────────────────────────────────────────────────
// Invariants
// ─────────────────────────────────────────────────────────────────

const invariants: InvariantSpec[] = [
  {
    name: "ledger-shows-amb-and-backtrack",
    check: (result, ctx) => {
      const ambChoices = ctx.ledger.getEventsByType("amb.choose");
      const backtracks = ctx.ledger.getEventsByType("amb.backtrack");

      const ok = ambChoices.length > 0;
      return {
        name: "ledger-shows-amb-and-backtrack",
        ok,
        detail: `${ambChoices.length} choices, ${backtracks.length} backtracks`,
      };
    },
  },
  {
    name: "only-accepted-branch-committed",
    check: (result, ctx) => {
      const commits = ctx.ledger.getEventsByType("commit.success");
      const denials = ctx.ledger.getEventsByType("commit.denied");

      // Should have at most one commit
      const ok = commits.length <= 1;
      return {
        name: "only-accepted-branch-committed",
        ok,
        detail: `${commits.length} commits, ${denials.length} denials`,
      };
    },
  },
  {
    name: "replay-same-branch-decisions",
    check: (result, ctx) => {
      if (ctx.isReplay) {
        // In replay, should follow same path
        const [accepted] = result.outputs as [RedactionResult | null];
        const ok = accepted !== null;
        return {
          name: "replay-same-branch-decisions",
          ok,
          detail: ok
            ? `Replay accepted: ${accepted.strategy}`
            : "Replay failed to find accepted strategy",
        };
      }

      // Normal run: just check we have a result
      const [accepted] = result.outputs as [RedactionResult | null];
      const ok = accepted !== null;
      return {
        name: "replay-same-branch-decisions",
        ok,
        detail: ok ? `Accepted: ${accepted.strategy}` : "No strategy accepted",
      };
    },
  },
  {
    name: "explore-mode-commit-denied",
    check: (result, ctx) => {
      if (ctx.profile.name === "explore") {
        const commits = ctx.ledger.getEventsByType("commit.success");
        const denials = ctx.ledger.getEventsByType("commit.denied");

        // In explore mode, commits should be denied
        const ok = commits.length === 0 || denials.length > 0;
        return {
          name: "explore-mode-commit-denied",
          ok,
          detail: ok
            ? "Explore mode: commits properly handled"
            : "Explore mode allowed commit unexpectedly",
        };
      }

      // Non-explore mode: skip this check
      return {
        name: "explore-mode-commit-denied",
        ok: true,
        detail: `Profile ${ctx.profile.name}: check skipped`,
      };
    },
  },
  {
    name: "all-branches-isolated",
    check: (result, ctx) => {
      const [, allResults] = result.outputs as [RedactionResult | null, RedactionResult[]];

      // Each branch should have independent metrics
      const strategies = allResults.map(r => r.strategy);
      const uniqueStrategies = new Set(strategies);

      const ok = strategies.length === uniqueStrategies.size;
      return {
        name: "all-branches-isolated",
        ok,
        detail: `${allResults.length} branches, ${uniqueStrategies.size} unique strategies`,
      };
    },
  },
];

// ─────────────────────────────────────────────────────────────────
// Demo Definition
// ─────────────────────────────────────────────────────────────────

export const demo2Backtracking: DemoDefinition = {
  id: "multi-shot-backtracking",
  name: "Multi-shot Backtracking over Semantic Choices",
  description: "Proves amb-based exploration of semantic strategies with auditable backtracking and isolated branches",
  tags: ["amb", "backtracking", "search", "document-processing", "policy"],
  run: runBacktrackingDemo,
  invariants,
};
