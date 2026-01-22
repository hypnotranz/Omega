#!/usr/bin/env npx tsx
// bin/omega.ts
// Unified Omega Language CLI - combines REPL and debugger functionality
//
// This is the single entry point for all Omega CLI operations:
//   - Interactive REPL with integrated debugger
//   - File execution
//   - Expression evaluation
//
// Run:  npx tsx bin/omega.ts [options] [file]

import * as readline from "readline";
import * as fs from "fs";
import * as path from "path";
import {
  parseCliArgs,
  getHelpText,
  getVersion,
  buildConfig,
} from "./omega-cli-lib";

// ═══════════════════════════════════════════════════════════════════════════════
// MAIN ENTRY POINT
// ═══════════════════════════════════════════════════════════════════════════════

async function main() {
  // Parse command-line arguments
  const cliArgs = parseCliArgs(process.argv.slice(2));

  // Handle --help
  if (cliArgs.help) {
    console.log(getHelpText());
    process.exit(0);
  }

  // Handle --version
  if (cliArgs.version) {
    console.log(getVersion());
    process.exit(0);
  }

  // Build configuration
  const config = buildConfig(cliArgs);

  // Load environment variables from .env
  loadEnvFile();

  // Execute based on mode
  if (config.mode === "exec") {
    await executeMode(config);
  } else {
    await replMode(config);
  }
}

// ═══════════════════════════════════════════════════════════════════════════════
// ENVIRONMENT SETUP
// ═══════════════════════════════════════════════════════════════════════════════

function loadEnvFile(): void {
  const envPath = path.join(process.cwd(), ".env");
  if (fs.existsSync(envPath)) {
    const envContent = fs.readFileSync(envPath, "utf8");
    for (const line of envContent.split("\n")) {
      const match = line.match(/^([^=]+)=(.*)$/);
      if (match && !process.env[match[1]]) {
        process.env[match[1]] = match[2].trim();
      }
    }
  }
}

// ═══════════════════════════════════════════════════════════════════════════════
// EXECUTE MODE (file or --eval)
// ═══════════════════════════════════════════════════════════════════════════════

async function executeMode(config: { code?: string; file?: string; verbose?: boolean; debug?: boolean }): Promise<void> {
  // Import dependencies from public API (lazy load for faster startup)
  const {
    COWStore,
    RuntimeImpl,
    SnapshotRepo,
    InMemoryReceiptStore,
    installPrims,
    runToCompletion,
    compileTextToExpr,
  } = await import("../src");

  try {
    // Get code to execute
    let code: string;
    if (config.file) {
      code = fs.readFileSync(config.file, "utf8");
    } else if (config.code) {
      code = config.code;
    } else {
      console.error("Error: No code or file specified");
      process.exit(1);
    }

    // Setup runtime
    const store0 = new COWStore();
    const prim = installPrims(store0);
    const rt = new RuntimeImpl(undefined as any, new SnapshotRepo(), new InMemoryReceiptStore(), undefined as any);

    // Compile
    const expr = compileTextToExpr(code);
    if (!expr) {
      console.error("Error: Failed to compile code");
      process.exit(1);
    }

    if (config.verbose) {
      console.log("Executing...");
    }

    // Execute
    const result = await runToCompletion(rt, {
      control: { tag: "Expr", e: expr },
      env: prim.env,
      kont: [],
      store: prim.store,
      handlers: [],
    });

    // Output result (runToCompletion now returns Val directly)
    console.log(formatValue(result));
    process.exit(0);
  } catch (error: any) {
    console.error("Error:", error.message);
    if (config.verbose) {
      console.error(error.stack);
    }
    process.exit(1);
  }
}

// ═══════════════════════════════════════════════════════════════════════════════
// REPL MODE (interactive with integrated debugger)
// ═══════════════════════════════════════════════════════════════════════════════

async function replMode(config: { verbose?: boolean; debug?: boolean; session?: string }): Promise<void> {
  // Spawn the REPL as a child process with inherited stdio
  // This delegates to omega-repl.ts which has all debugger commands merged in
  const { spawn } = await import("child_process");
  const replPath = path.join(__dirname, "omega-repl.ts");

  const args = ["tsx", replPath];
  if (config.verbose) args.push("--verbose");
  if (config.debug) args.push("--debug");
  if (config.session) args.push("--session", config.session);

  const child = spawn("npx", args, {
    stdio: "inherit",
    shell: true,
    cwd: process.cwd(),
  });

  child.on("exit", (code) => {
    process.exit(code ?? 0);
  });

  child.on("error", (err) => {
    console.error("Failed to start REPL:", err.message);
    process.exit(1);
  });
}

// ═══════════════════════════════════════════════════════════════════════════════
// UTILITIES
// ═══════════════════════════════════════════════════════════════════════════════

function formatValue(val: any): string {
  if (val === null || val === undefined) return "()";

  switch (val.tag) {
    case "Unit": return "()";
    case "Num": return String(val.n);
    case "Bool": return val.b ? "#t" : "#f";
    case "Str": return val.s;
    case "Sym": return `'${val.name}`;
    case "Vector": return `[${val.items.map(formatValue).join(" ")}]`;
    default: return `<${val.tag}>`;
  }
}

// ═══════════════════════════════════════════════════════════════════════════════
// ENTRY POINT
// ═══════════════════════════════════════════════════════════════════════════════

main().catch((error) => {
  console.error("Fatal error:", error.message);
  process.exit(1);
});
