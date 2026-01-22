// bin/omega-cli-lib.ts
// Shared CLI utilities for the unified omega command
// Exported functions for testing

import * as fs from "fs";
import * as path from "path";

// ═══════════════════════════════════════════════════════════════════════════════
// TYPE DEFINITIONS
// ═══════════════════════════════════════════════════════════════════════════════

export type CliArgs = {
  help?: boolean;
  version?: boolean;
  eval?: string;
  file?: string;
  debug?: boolean;
  verbose?: boolean;
  session?: string;
  mode?: "repl" | "exec";
};

export type CliConfig = {
  mode: "repl" | "exec";
  debugEnabled: boolean;
  verbose: boolean;
  code?: string;
  file?: string;
  session?: string;
};

// ═══════════════════════════════════════════════════════════════════════════════
// ARGUMENT PARSING
// ═══════════════════════════════════════════════════════════════════════════════

export function parseCliArgs(args: string[]): CliArgs {
  const result: CliArgs = {};

  for (let i = 0; i < args.length; i++) {
    const arg = args[i];

    if (arg === "--help" || arg === "-h") {
      result.help = true;
    } else if (arg === "--version" || arg === "-v") {
      result.version = true;
    } else if (arg === "--debug" || arg === "-d") {
      result.debug = true;
    } else if (arg === "--verbose") {
      result.verbose = true;
    } else if (arg === "--eval" || arg === "-e") {
      result.eval = args[++i] || "";
      result.mode = "exec";
    } else if (arg === "--session" || arg === "-s") {
      result.session = args[++i];
    } else if (!arg.startsWith("--") && !arg.startsWith("-")) {
      // First non-flag argument is the file
      if (!result.file) {
        result.file = arg;
        result.mode = "exec";
      }
    }
    // Ignore unknown flags
  }

  // Default mode is REPL if no exec mode was set
  if (!result.mode) {
    result.mode = "repl";
  }

  return result;
}

// ═══════════════════════════════════════════════════════════════════════════════
// HELP TEXT
// ═══════════════════════════════════════════════════════════════════════════════

export function getHelpText(): string {
  return `
omega - Unified Omega Language CLI

USAGE:
  omega [options]                     Start interactive REPL with debugger
  omega [options] <file>              Execute an Omega file
  omega --eval <code>                 Evaluate Omega code directly

OPTIONS:
  -h, --help                         Show this help message
  -v, --version                      Show version information
  -e, --eval <code>                  Evaluate code and exit
  -d, --debug                        Enable debug mode (step-through execution)
  --verbose                          Show detailed execution information
  -s, --session <name>               Set session name for persistence

REPL COMMANDS:
  Basic:
    :help, :h                        Show REPL help
    :quit, :q                        Exit the REPL
    :env [name]                      Show environment bindings
    :defs                            Show all definitions

  Execution:
    :step [n]                        Execute n steps (default: 1)
    :run, :continue, :c              Run to completion
    :stop                            Stop current execution
    :state, :st                      Show current machine state
    :control                         Show control state
    :stack                           Show continuation stack

  Debugging:
    :debug <expr>                    Start debugging an expression
    :break <type> <condition>        Set breakpoint (step N | expr TAG | effect OP)
    :breaks, :breakpoints            List all breakpoints
    :delbreak <id>, :del <id>        Delete breakpoint
    :toggle <id>                     Enable/disable breakpoint

  Time Travel:
    :trace                           Show execution trace
    :goto <step>                     Jump to specific step
    :back [n]                        Rewind n steps (default: 1)
    :history [n]                     Show recent history

  Snapshots:
    :save <name>                     Save snapshot
    :restore <name>                  Load snapshot
    :snapshots, :snaps               List snapshots
    :export <name> <file>            Export snapshot to file

  Recording:
    :record on|off                   Toggle trace recording
    :dump <file>                     Save trace to file
    :load <file>                     Load trace from file

EXAMPLES:
  omega                              # Start REPL
  omega --debug program.omega        # Debug a file
  omega --eval "(+ 1 2)"            # Evaluate expression
  omega --session my-work            # Resume session

See https://github.com/your-org/omega-llm for documentation.
`.trim();
}

// ═══════════════════════════════════════════════════════════════════════════════
// VERSION
// ═══════════════════════════════════════════════════════════════════════════════

export function getVersion(): string {
  try {
    const pkgPath = path.join(__dirname, "..", "package.json");
    const pkg = JSON.parse(fs.readFileSync(pkgPath, "utf8"));
    return `omega-llm v${pkg.version}`;
  } catch {
    return "omega-llm v0.1.0";
  }
}

// ═══════════════════════════════════════════════════════════════════════════════
// MODE DETECTION
// ═══════════════════════════════════════════════════════════════════════════════

export function detectMode(args: Partial<CliArgs>): "repl" | "exec" {
  if (args.mode) {
    return args.mode;
  }
  if (args.eval || args.file) {
    return "exec";
  }
  return "repl";
}

// ═══════════════════════════════════════════════════════════════════════════════
// CONFIGURATION BUILDING
// ═══════════════════════════════════════════════════════════════════════════════

export function buildConfig(args: Partial<CliArgs>): CliConfig {
  const mode = detectMode(args);

  const config: CliConfig = {
    mode,
    debugEnabled: args.debug !== false, // Debug enabled by default in REPL
    verbose: args.verbose || false,
    session: args.session,
  };

  if (args.eval) {
    config.code = args.eval;
  }

  if (args.file) {
    config.file = args.file;
  }

  return config;
}
