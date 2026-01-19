#!/usr/bin/env npx tsx
// demo/runWow.ts
// CLI runner for the Ω Wow Pack demo suite
//
// Usage:
//   npx tsx demo/runWow.ts --all --profile pragmatic --seed 7
//   npx tsx demo/runWow.ts --demo oracle-repl-stack --verbose
//   npx tsx demo/runWow.ts --list
//   npx tsx demo/runWow.ts --demo backtracking --report out.json

import { parseArgs } from "node:util";
import * as fs from "node:fs";
import * as path from "node:path";

import { allDemos, listDemos, getDemoById } from "./omega-wow";
import { runDemo, runSuite, formatReport, formatSuiteResult, getProfile } from "./harness";
import type { CLIOptions, WowReport, SuiteResult } from "./harness";

// ─────────────────────────────────────────────────────────────────
// CLI Argument Parsing
// ─────────────────────────────────────────────────────────────────

const { values: args } = parseArgs({
  options: {
    demo: {
      type: "string",
      short: "d",
      description: "Run a specific demo by ID",
    },
    all: {
      type: "boolean",
      short: "a",
      description: "Run all demos",
    },
    profile: {
      type: "string",
      short: "p",
      default: "pragmatic",
      description: "Governance profile: explore|pragmatic|strict|airgap",
    },
    seed: {
      type: "string",
      short: "s",
      default: "42",
      description: "Random seed for deterministic execution",
    },
    replay: {
      type: "string",
      short: "r",
      description: "Replay from transcript ID",
    },
    report: {
      type: "string",
      short: "o",
      description: "Output report to JSON file",
    },
    verbose: {
      type: "boolean",
      short: "v",
      description: "Verbose output",
    },
    list: {
      type: "boolean",
      short: "l",
      description: "List available demos",
    },
    help: {
      type: "boolean",
      short: "h",
      description: "Show help",
    },
  },
  strict: true,
  allowPositionals: false,
});

// ─────────────────────────────────────────────────────────────────
// Help Text
// ─────────────────────────────────────────────────────────────────

function showHelp(): void {
  console.log(`
╔══════════════════════════════════════════════════════════════════╗
║                     Ω Wow Pack Demo Suite                        ║
║  Proving that Ω's language features are first-class, not just    ║
║  API calls — through 8 hermetic, deterministic demos.            ║
╚══════════════════════════════════════════════════════════════════╝

Usage:
  npx tsx demo/runWow.ts [options]

Options:
  -d, --demo <id>      Run a specific demo by ID
  -a, --all            Run all demos
  -p, --profile <name> Governance profile (default: pragmatic)
                       Options: explore, pragmatic, strict, airgap
  -s, --seed <number>  Random seed for deterministic execution (default: 42)
  -r, --replay <id>    Replay from a transcript ID
  -o, --report <file>  Output report to JSON file
  -v, --verbose        Verbose output with full transcripts
  -l, --list           List available demos
  -h, --help           Show this help

Examples:
  # Run all demos with pragmatic profile
  npx tsx demo/runWow.ts --all --profile pragmatic --seed 7

  # Run a specific demo with verbose output
  npx tsx demo/runWow.ts --demo oracle-repl-stack --verbose

  # Run and save report
  npx tsx demo/runWow.ts --all --report wow-report.json

  # List available demos
  npx tsx demo/runWow.ts --list

Demos:
  1. oracle-repl-stack      - Interactive Oracle REPL in call stack
  2. backtracking           - Multi-shot backtracking over semantic choices
  3. concurrency-collapse   - Concurrency collapses semantic cost
  4. generic-synthesis      - Generic operations synthesize missing methods
  5. constraint-repair      - Constraint diagnosis + repair
  6. semantic-macros        - Semantic macros with DSL expansion
  7. compilation            - Compilation preserves inference plane
  8. meta-circular          - Meta-circular oracle repairs eval0

Profiles:
  explore   - Maximum flexibility, all oracle types enabled
  pragmatic - Balanced production use (default)
  strict    - Minimal oracle calls, prefer determinism
  airgap    - No oracle calls at all (replay only)
`);
}

// ─────────────────────────────────────────────────────────────────
// Demo Listing
// ─────────────────────────────────────────────────────────────────

function showDemoList(): void {
  const demos = listDemos();

  console.log("\n╔══════════════════════════════════════════════════════════════════╗");
  console.log("║                     Available Demos                              ║");
  console.log("╚══════════════════════════════════════════════════════════════════╝\n");

  for (const demo of demos) {
    console.log(`  ${demo.id}`);
    console.log(`    ${demo.name}`);
    console.log(`    ${demo.description}`);
    console.log(`    Tags: ${demo.tags.join(", ")}`);
    console.log();
  }

  console.log(`Total: ${demos.length} demos available`);
}

// ─────────────────────────────────────────────────────────────────
// Result Display
// ─────────────────────────────────────────────────────────────────

function displayReport(report: WowReport, verbose: boolean): void {
  console.log(formatReport(report, { verbose }));
}

function displaySuiteResult(result: SuiteResult, verbose: boolean): void {
  console.log(formatSuiteResult(result, { verbose }));
}

// ─────────────────────────────────────────────────────────────────
// Report Saving
// ─────────────────────────────────────────────────────────────────

function saveReport(data: WowReport | SuiteResult, filePath: string): void {
  const fullPath = path.resolve(filePath);
  const content = JSON.stringify(data, null, 2);
  fs.writeFileSync(fullPath, content, "utf-8");
  console.log(`\n✓ Report saved to: ${fullPath}`);
}

// ─────────────────────────────────────────────────────────────────
// Main
// ─────────────────────────────────────────────────────────────────

async function main(): Promise<void> {
  // Handle help
  if (args.help) {
    showHelp();
    process.exit(0);
  }

  // Handle list
  if (args.list) {
    showDemoList();
    process.exit(0);
  }

  // Validate arguments
  if (!args.demo && !args.all) {
    console.error("Error: Must specify --demo <id> or --all");
    console.error("Use --help for usage information");
    process.exit(1);
  }

  if (args.demo && args.all) {
    console.error("Error: Cannot specify both --demo and --all");
    process.exit(1);
  }

  // Parse seed
  const seed = parseInt(args.seed ?? "42", 10);
  if (isNaN(seed)) {
    console.error("Error: Invalid seed value");
    process.exit(1);
  }

  // Validate profile
  const profileName = args.profile ?? "pragmatic";
  const validProfiles = ["explore", "pragmatic", "strict", "airgap"];
  if (!validProfiles.includes(profileName)) {
    console.error(`Error: Invalid profile '${profileName}'`);
    console.error(`Valid profiles: ${validProfiles.join(", ")}`);
    process.exit(1);
  }

  const verbose = args.verbose ?? false;

  // Build CLI options
  const options: CLIOptions = {
    verbose,
    profileName,
    seed,
  };

  console.log("\n╔══════════════════════════════════════════════════════════════════╗");
  console.log("║                     Ω Wow Pack Demo Suite                        ║");
  console.log("╚══════════════════════════════════════════════════════════════════╝\n");
  console.log(`Profile: ${profileName}`);
  console.log(`Seed: ${seed}`);
  console.log(`Verbose: ${verbose}`);
  console.log();

  try {
    if (args.all) {
      // Run all demos
      console.log("Running all demos...\n");
      console.log("─".repeat(66));

      const result = await runSuite(allDemos, profileName, seed, { verbose });

      displaySuiteResult(result, verbose);

      if (args.report) {
        saveReport(result, args.report);
      }

      // Exit with error code if any demos failed
      if (!result.success) {
        process.exit(1);
      }
    } else if (args.demo) {
      // Run specific demo
      const demo = getDemoById(args.demo);
      if (!demo) {
        console.error(`Error: Demo '${args.demo}' not found`);
        console.error("\nAvailable demos:");
        for (const d of listDemos()) {
          console.error(`  - ${d.id}`);
        }
        process.exit(1);
      }

      console.log(`Running demo: ${demo.name}\n`);
      console.log("─".repeat(66));

      const report = await runDemo(demo, profileName, seed, { verbose });

      displayReport(report, verbose);

      if (args.report) {
        saveReport(report, args.report);
      }

      // Exit with error code if demo failed
      const failed = report.invariants.some(inv => !inv.ok);
      if (failed) {
        process.exit(1);
      }
    }
  } catch (error) {
    console.error("\n❌ Error running demo(s):");
    console.error(error instanceof Error ? error.message : String(error));
    if (verbose && error instanceof Error && error.stack) {
      console.error("\nStack trace:");
      console.error(error.stack);
    }
    process.exit(1);
  }

  console.log("\n✓ Done");
}

// Run main
main().catch((error) => {
  console.error("Fatal error:", error);
  process.exit(1);
});
