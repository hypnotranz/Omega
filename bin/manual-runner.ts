#!/usr/bin/env npx tsx
// bin/manual-runner.ts
// Runner for SICP manual chapter demos
// Shows code, then executes it

import * as fs from "fs";
import * as path from "path";
import { spawn } from "child_process";
import { fileURLToPath } from "url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// Load .env file (check .omega/.env first, then .env)
const omegaEnvPath = path.join(__dirname, "..", ".omega", ".env");
const rootEnvPath = path.join(__dirname, "..", ".env");
const envPath = fs.existsSync(omegaEnvPath) ? omegaEnvPath : rootEnvPath;

if (fs.existsSync(envPath)) {
  const envContent = fs.readFileSync(envPath, "utf8");
  for (const line of envContent.split("\n")) {
    const match = line.match(/^([^=]+)=(.*)$/);
    if (match && !process.env[match[1]]) {
      process.env[match[1]] = match[2].trim();
    }
  }
}

const MANUAL_DIR = path.join(__dirname, "..", "MANUAL--STRUCTURE-AND-INTERPRETATION-OF-LINGUISTIC-PROGRAMS");
// TODO: After migration, use: path.join(MANUAL_DIR, "examples")
// For now, demos are still in demo/lisp/
const EXAMPLES_DIR = path.join(__dirname, "..", "demo", "lisp");

// Chapter name mappings
const CHAPTER_NAMES: Record<string, string> = {
  "1": "ch01-getting-started",
  "2": "ch02-llm-calls",
  "3": "ch03-composition",
  "4": "ch04-higher-order",
  "5": "ch05-nondeterministic",
  "6": "ch06-multi-shot",
  "7": "ch07-lazy-streams",
  "8": "ch08-debugger",
  "9": "ch09-agentic-repl",
  "10": "ch10-api-reference",
  "11": "ch11-semantic-procedures",
  "12": "ch12-inference-processes",
  "13": "ch13-higher-order-inference",
  "14": "ch14-semantic-data",
  "15": "ch15-sequences",
  "16": "ch16-symbolic-semantic",
  "17": "ch17-multiple-representations",
  "18": "ch18-generic-semantic",
  "19": "ch19-conversational-state",
  "20": "ch20-semantic-environment",
  "21": "ch21-mutable-semantic",
  "22": "ch22-concurrent-inference",
  "23": "ch23-streams-of-inference",
  "24": "ch24-metacircular",
  "25": "ch25-lazy-semantic",
  "26": "ch26-amb-inference",
  "27": "ch27-logic-programming",
  // Aliases
  "getting-started": "ch01-getting-started",
  "llm-calls": "ch02-llm-calls",
  "composition": "ch03-composition",
  "higher-order": "ch04-higher-order",
  "nondeterministic": "ch05-nondeterministic",
  "multi-shot": "ch06-multi-shot",
  "lazy-streams": "ch07-lazy-streams",
  "debugger": "ch08-debugger",
  "agentic-repl": "ch09-agentic-repl",
  "api-reference": "ch10-api-reference",
  "semantic-procedures": "ch11-semantic-procedures",
  "inference-processes": "ch12-inference-processes",
  "higher-order-inference": "ch13-higher-order-inference",
  "semantic-data": "ch14-semantic-data",
  "sequences": "ch15-sequences",
  "symbolic-semantic": "ch16-symbolic-semantic",
  "multiple-representations": "ch17-multiple-representations",
  "generic-semantic": "ch18-generic-semantic",
  "conversational-state": "ch19-conversational-state",
  "semantic-environment": "ch20-semantic-environment",
  "mutable-semantic": "ch21-mutable-semantic",
  "concurrent-inference": "ch22-concurrent-inference",
  "streams-of-inference": "ch23-streams-of-inference",
  "metacircular": "ch24-metacircular",
  "lazy-semantic": "ch25-lazy-semantic",
  "amb-inference": "ch26-amb-inference",
  "logic-programming": "ch27-logic-programming",
};

function printUsage() {
  console.log("╔════════════════════════════════════════════════════════════╗");
  console.log("║  OmegaLLM Manual - Structure & Interpretation of Inference ║");
  console.log("╚════════════════════════════════════════════════════════════╝");
  console.log("");
  console.log("Usage:");
  console.log("  npm run manual <chapter>");
  console.log("");
  console.log("Examples:");
  console.log("  npm run manual 1                    # Chapter 1");
  console.log("  npm run manual 5                    # Chapter 5");
  console.log("  npm run manual getting-started      # Chapter 1 (by name)");
  console.log("  npm run manual nondeterministic     # Chapter 5 (by name)");
  console.log("  npm run manual ch13-higher-order-inference");
  console.log("");
  console.log("Available chapters: 1-27");
  console.log("");
  process.exit(1);
}

function resolveChapter(arg: string): string | null {
  // Try direct lookup
  if (CHAPTER_NAMES[arg]) {
    return CHAPTER_NAMES[arg];
  }

  // Try with ch prefix
  if (arg.startsWith("ch") && arg.includes("-")) {
    return arg;
  }

  return null;
}

function displayCode(filePath: string, chapterName: string) {
  const content = fs.readFileSync(filePath, "utf8");
  const lines = content.split("\n");

  console.log("");
  console.log("═".repeat(70));
  console.log(`  ${chapterName.toUpperCase()}`);
  console.log("═".repeat(70));
  console.log("");

  // Print code with line numbers
  lines.forEach((line, idx) => {
    const lineNum = (idx + 1).toString().padStart(3, " ");
    console.log(`${lineNum} │ ${line}`);
  });

  console.log("");
  console.log("─".repeat(70));
  console.log("  OUTPUT:");
  console.log("─".repeat(70));
  console.log("");
}

async function runDemo(filePath: string): Promise<void> {
  return new Promise((resolve, reject) => {
    const replPath = path.join(__dirname, "omega-repl.ts");
    const child = spawn("npx", ["tsx", replPath, "--file", filePath], {
      stdio: "inherit",
      shell: true,
      env: process.env,  // Pass environment variables to child process
    });

    child.on("close", (code) => {
      if (code === 0) {
        resolve();
      } else {
        reject(new Error(`Process exited with code ${code}`));
      }
    });

    child.on("error", reject);
  });
}

async function main() {
  const args = process.argv.slice(2);

  if (args.length === 0 || args[0] === "--help" || args[0] === "-h") {
    printUsage();
  }

  const chapter = resolveChapter(args[0]);
  if (!chapter) {
    console.error(`Error: Unknown chapter '${args[0]}'`);
    console.error("");
    printUsage();
  }

  const filePath = path.join(EXAMPLES_DIR, `${chapter}.lisp`);
  if (!fs.existsSync(filePath)) {
    console.error(`Error: File not found: ${filePath}`);
    console.error("");
    console.error("This chapter demo may not exist yet.");
    process.exit(1);
  }

  // Display code
  displayCode(filePath, chapter);

  // Run demo
  try {
    await runDemo(filePath);
    console.log("");
    console.log("═".repeat(70));
    console.log("");
  } catch (err: any) {
    console.error("");
    console.error("Error running demo:", err.message);
    process.exit(1);
  }
}

main();
