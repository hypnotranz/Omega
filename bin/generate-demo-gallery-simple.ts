#!/usr/bin/env npx tsx
// Simpler, more reliable version using spawn

import * as fs from "fs";
import * as path from "path";
import { spawn } from "child_process";
import { fileURLToPath } from "url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const DEMO_DIR = path.join(__dirname, "..", "demo", "lisp");
const OUTPUT_FILE = path.join(__dirname, "..", "MANUAL--STRUCTURE-AND-INTERPRETATION-OF-LINGUISTIC-PROGRAMS", "DEMO-GALLERY.md");

async function runDemo(num: number): Promise<string> {
  return new Promise((resolve, reject) => {
    const child = spawn("npx", ["tsx", "bin/manual-runner.ts", String(num)], {
      cwd: path.join(__dirname, ".."),
      shell: true,
    });

    let stdout = "";
    let stderr = "";

    child.stdout?.on("data", (data) => {
      stdout += data.toString();
    });

    child.stderr?.on("data", (data) => {
      stderr += data.toString();
    });

    child.on("close", (code) => {
      if (code === 0) {
        resolve(stdout);
      } else {
        reject(new Error(`Exit code ${code}: ${stderr}`));
      }
    });

    child.on("error", reject);

    // Timeout after 30s
    setTimeout(() => {
      child.kill();
      reject(new Error("Timeout"));
    }, 30000);
  });
}

function extractOutput(fullOutput: string): string {
  const lines = fullOutput.split("\n");
  const outputStartIdx = lines.findIndex(line => line.includes("OUTPUT:"));
  if (outputStartIdx === -1) return "No output captured";

  const outputEndIdx = lines.findIndex((line, idx) =>
    idx > outputStartIdx && line.match(/^═+$/));

  if (outputEndIdx === -1) {
    return lines.slice(outputStartIdx + 2).join("\n").trim();
  }

  return lines.slice(outputStartIdx + 2, outputEndIdx).join("\n").trim();
}

const chapters = [
  { num: 1, name: "Getting Started" },
  { num: 2, name: "LLM Calls as Functions" },
  { num: 3, name: "Functional Composition" },
  // Add rest...
];

async function main() {
  console.log("Testing with first chapter...\n");

  try {
    const output = await runDemo(1);
    const extracted = extractOutput(output);
    console.log("Full output length:", output.length);
    console.log("Extracted output:");
    console.log(extracted);
  } catch (err: any) {
    console.error("Error:", err.message);
  }
}

main();
