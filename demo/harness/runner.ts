// demo/harness/runner.ts
// Demo Runner - Execute demos and generate reports

import { createHash } from "crypto";
import type {
  DemoDefinition,
  DemoContext,
  DemoResult,
  DemoOptions,
  DemoMetrics,
  WowReport,
  InvariantResult,
  SuiteResult,
  OracleTranscript,
} from "./types";
import { createScriptedOracleAdapter } from "./oracle-adapter";
import { createDemoLedger, countEventsByType } from "./ledger";
import type { Profile } from "../../src/core/governance/profile";

// ─────────────────────────────────────────────────────────────────
// Seeded Random Number Generator
// ─────────────────────────────────────────────────────────────────

/**
 * Create a seeded random number generator (mulberry32).
 */
export function createSeededRandom(seed: number): () => number {
  let state = seed;
  return () => {
    state |= 0;
    state = (state + 0x6d2b79f5) | 0;
    let t = Math.imul(state ^ (state >>> 15), 1 | state);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

// ─────────────────────────────────────────────────────────────────
// Profile Loader
// ─────────────────────────────────────────────────────────────────

/**
 * Get profile by name.
 */
export function getProfile(name: string): Profile {
  // Import profiles from governance
  const profiles: Record<string, Profile> = {
    explore: {
      name: "explore",
      inferBudget: 100,
      oracleBudget: 50,
      stepBudget: 10000,
      allowCommit: false,
      allowPromotion: false,
      requireTests: false,
      allowAmbient: true,
    },
    pragmatic: {
      name: "pragmatic",
      inferBudget: 50,
      oracleBudget: 25,
      stepBudget: 5000,
      allowCommit: true,
      allowPromotion: true,
      requireTests: true,
      allowAmbient: true,
    },
    strict: {
      name: "strict",
      inferBudget: 20,
      oracleBudget: 10,
      stepBudget: 2000,
      allowCommit: true,
      allowPromotion: true,
      requireTests: true,
      allowAmbient: false,
    },
    airgap: {
      name: "airgap",
      inferBudget: 0,
      oracleBudget: 0,
      stepBudget: 1000,
      allowCommit: false,
      allowPromotion: false,
      requireTests: true,
      allowAmbient: false,
    },
  };

  return profiles[name] ?? profiles.pragmatic;
}

// ─────────────────────────────────────────────────────────────────
// Demo Runner
// ─────────────────────────────────────────────────────────────────

/**
 * Run a single demo and generate report.
 */
export async function runDemo(
  demo: DemoDefinition,
  profileName: string,
  seed: number,
  options: DemoOptions = {},
  replayTranscript?: OracleTranscript
): Promise<WowReport> {
  const startTime = Date.now();
  const profile = getProfile(profileName);
  const oracle = createScriptedOracleAdapter(demo.id, seed, profileName);
  const ledger = createDemoLedger();
  const random = createSeededRandom(seed);

  // Load replay transcript if provided
  if (replayTranscript) {
    oracle.loadTranscript(replayTranscript);
  }

  // Create context
  const ctx: DemoContext = {
    profile,
    seed,
    random,
    oracle,
    ledger,
    isReplay: !!replayTranscript,
    replayTranscript,
    options,
  };

  // Set context on oracle adapter
  (oracle as any).setContext?.(ctx);

  // Record demo start
  ledger.record("demo.start", { demoId: demo.id, profile: profileName, seed });

  // Run the demo
  let result: DemoResult;
  try {
    result = await demo.run(ctx);
  } catch (error) {
    result = {
      outputs: [],
      success: false,
      error: error instanceof Error ? error : new Error(String(error)),
      metrics: createEmptyMetrics(),
      transcript: oracle.getTranscript(),
    };
  }

  // Record demo end
  ledger.record("demo.end", {
    success: result.success,
    error: result.error?.message,
  });

  // Compute metrics
  const oracleCounts = oracle.getCounts();
  const ledgerCounts = countEventsByType(ledger);

  const metrics: DemoMetrics = {
    inferCalls: oracleCounts.InferOp,
    oracleReqEval: oracleCounts.ReqEval,
    oracleReqApply: oracleCounts.ReqApply,
    oracleReqObserve: oracleCounts.ReqObserve,
    oracleReqTest: oracleCounts.ReqTest,
    oracleReqReturn: oracleCounts.ReqReturn,
    steps: result.metrics?.steps ?? 0,
    wallMs: Date.now() - startTime,
    scheduleDecisions: ledgerCounts["schedule.decision"] ?? 0,
    ambChoices: ledgerCounts["amb.choose"] ?? 0,
    backtracks: ledgerCounts["amb.backtrack"] ?? 0,
    commits: (ledgerCounts["commit.success"] ?? 0) + (ledgerCounts["commit.denied"] ?? 0),
    genericMiss: ledgerCounts["generic.miss"] ?? 0,
    methodsInstalled: ledgerCounts["generic.install"] ?? 0,
    macroExpansions: ledgerCounts["macro.expand"] ?? 0,
  };

  // Check invariants
  const invariants: InvariantResult[] = demo.invariants.map(spec => {
    try {
      return spec.check(result, ctx);
    } catch (error) {
      return {
        name: spec.name,
        ok: false,
        detail: error instanceof Error ? error.message : String(error),
      };
    }
  });

  // Compute digests
  const outputsDigest = computeDigest(result.outputs);
  const ledgerDigest = ledger.getDigest();

  // Build report
  const report: WowReport = {
    demoId: demo.id,
    description: demo.description,
    profile: profileName,
    seed,
    timestamp: new Date().toISOString(),
    outputsDigest,
    ledgerDigest,
    counts: {
      inferCalls: metrics.inferCalls,
      oracleReqEval: metrics.oracleReqEval,
      oracleReqApply: metrics.oracleReqApply,
      oracleReqObserve: metrics.oracleReqObserve,
      oracleReqTest: metrics.oracleReqTest,
      oracleReqReturn: metrics.oracleReqReturn,
      scheduleDecisions: metrics.scheduleDecisions,
      ambChoices: metrics.ambChoices,
      backtracks: metrics.backtracks,
      commits: metrics.commits,
      genericMiss: metrics.genericMiss,
      methodsInstalled: metrics.methodsInstalled,
      macroExpansions: metrics.macroExpansions,
    },
    costs: {
      steps: metrics.steps,
      wallMs: metrics.wallMs,
    },
    invariants,
    artifacts: {
      transcriptId: result.transcript.id,
      receiptIds: [],
      irDigest: undefined,
      sourceMapDigest: undefined,
    },
  };

  return report;
}

/**
 * Run multiple demos and generate suite result.
 */
export async function runSuite(
  demos: DemoDefinition[],
  profileName: string,
  seed: number,
  options: DemoOptions = {}
): Promise<SuiteResult> {
  const reports: WowReport[] = [];
  let passed = 0;
  let failed = 0;
  let skipped = 0;

  for (const demo of demos) {
    try {
      const report = await runDemo(demo, profileName, seed, options);
      reports.push(report);

      const allPassed = report.invariants.every(i => i.ok);
      if (allPassed) {
        passed++;
      } else {
        failed++;
      }
    } catch (error) {
      skipped++;
      // Create error report
      reports.push({
        demoId: demo.id,
        description: demo.description,
        profile: profileName,
        seed,
        timestamp: new Date().toISOString(),
        outputsDigest: "",
        ledgerDigest: "",
        counts: {
          inferCalls: 0,
          oracleReqEval: 0,
          oracleReqApply: 0,
          oracleReqObserve: 0,
          oracleReqTest: 0,
        },
        costs: { steps: 0 },
        invariants: [{
          name: "demo-execution",
          ok: false,
          detail: error instanceof Error ? error.message : String(error),
        }],
        artifacts: {},
      });
    }
  }

  return {
    total: demos.length,
    passed,
    failed,
    skipped,
    reports,
    success: failed === 0 && skipped === 0,
  };
}

/**
 * Create empty metrics.
 */
function createEmptyMetrics(): DemoMetrics {
  return {
    inferCalls: 0,
    oracleReqEval: 0,
    oracleReqApply: 0,
    oracleReqObserve: 0,
    oracleReqTest: 0,
    oracleReqReturn: 0,
    steps: 0,
    wallMs: 0,
  };
}

/**
 * Compute digest of outputs.
 */
function computeDigest(outputs: unknown[]): string {
  const content = JSON.stringify(outputs);
  return createHash("sha256").update(content).digest("hex").slice(0, 16);
}

// ─────────────────────────────────────────────────────────────────
// Report Formatting
// ─────────────────────────────────────────────────────────────────

/**
 * Format a single report for console output.
 */
export function formatReport(report: WowReport, verbose: boolean = false): string {
  const lines: string[] = [];
  const allPassed = report.invariants.every(i => i.ok);
  const status = allPassed ? "\x1b[32mOK\x1b[0m" : "\x1b[31mFAIL\x1b[0m";

  lines.push(`Demo: ${report.demoId} ... ${status}`);

  if (verbose || !allPassed) {
    lines.push(`  Description: ${report.description}`);
    lines.push(`  Profile: ${report.profile}, Seed: ${report.seed}`);
    lines.push(`  Counts:`);
    lines.push(`    inferCalls: ${report.counts.inferCalls}`);
    lines.push(`    oracleReqEval: ${report.counts.oracleReqEval}`);
    lines.push(`    oracleReqApply: ${report.counts.oracleReqApply}`);
    lines.push(`    oracleReqObserve: ${report.counts.oracleReqObserve}`);
    lines.push(`    oracleReqTest: ${report.counts.oracleReqTest}`);

    if (report.counts.ambChoices) {
      lines.push(`    ambChoices: ${report.counts.ambChoices}`);
    }
    if (report.counts.backtracks) {
      lines.push(`    backtracks: ${report.counts.backtracks}`);
    }
    if (report.counts.genericMiss) {
      lines.push(`    genericMiss: ${report.counts.genericMiss}`);
    }

    lines.push(`  Costs: ${report.costs.steps} steps, ${report.costs.wallMs}ms`);
    lines.push(`  Digests: outputs=${report.outputsDigest}, ledger=${report.ledgerDigest}`);

    lines.push(`  Invariants:`);
    for (const inv of report.invariants) {
      const invStatus = inv.ok ? "\x1b[32m✓\x1b[0m" : "\x1b[31m✗\x1b[0m";
      lines.push(`    ${invStatus} ${inv.name}${inv.detail ? `: ${inv.detail}` : ""}`);
    }
  }

  return lines.join("\n");
}

/**
 * Format suite result for console output.
 */
export function formatSuiteResult(result: SuiteResult, verbose: boolean = false): string {
  const lines: string[] = [];

  lines.push("\n" + "=".repeat(60));
  lines.push("Ω Wow Pack Demo Suite Results");
  lines.push("=".repeat(60) + "\n");

  for (const report of result.reports) {
    lines.push(formatReport(report, verbose));
    lines.push("");
  }

  lines.push("=".repeat(60));
  lines.push(`Total: ${result.total} | Passed: ${result.passed} | Failed: ${result.failed} | Skipped: ${result.skipped}`);

  if (result.success) {
    lines.push("\x1b[32mAll demos passed!\x1b[0m");
  } else {
    lines.push("\x1b[31mSome demos failed.\x1b[0m");
  }

  lines.push("=".repeat(60));

  return lines.join("\n");
}
