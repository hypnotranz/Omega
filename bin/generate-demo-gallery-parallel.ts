#!/usr/bin/env npx tsx
// bin/generate-demo-gallery-parallel.ts
// Generates showcase document with ALL demos running IN PARALLEL

import * as fs from "fs";
import * as path from "path";
import { exec } from "child_process";
import { promisify } from "util";
import { fileURLToPath } from "url";

const execAsync = promisify(exec);

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const DEMO_DIR = path.join(__dirname, "..", "demo", "lisp");
const OUTPUT_FILE = path.join(__dirname, "..", "MANUAL--STRUCTURE-AND-INTERPRETATION-OF-LINGUISTIC-PROGRAMS", "DEMO-GALLERY.md");

const CHAPTERS = [
  { num: 1, name: "Getting Started", file: "ch01-getting-started.lisp" },
  { num: 2, name: "LLM Calls as Functions", file: "ch02-llm-calls.lisp" },
  { num: 3, name: "Functional Composition", file: "ch03-composition.lisp" },
  { num: 4, name: "Higher-Order LLM Functions", file: "ch04-higher-order.lisp" },
  { num: 5, name: "Nondeterministic Search", file: "ch05-nondeterministic.lisp" },
  { num: 6, name: "Multi-Shot Sampling", file: "ch06-multi-shot.lisp" },
  { num: 7, name: "Lazy Streams", file: "ch07-lazy-streams.lisp" },
  { num: 8, name: "The Debugger", file: "ch08-debugger.lisp" },
  { num: 9, name: "The Agentic REPL", file: "ch09-agentic-repl.lisp" },
  { num: 10, name: "Full API Reference", file: "ch10-api-reference.lisp" },
  { num: 11, name: "Semantic Procedures as Black Boxes", file: "ch11-semantic-procedures.lisp" },
  { num: 12, name: "Inference Processes", file: "ch12-inference-processes.lisp" },
  { num: 13, name: "Higher-Order Inference", file: "ch13-higher-order-inference.lisp" },
  { num: 14, name: "Semantic Data Abstraction", file: "ch14-semantic-data.lisp" },
  { num: 15, name: "Sequences as Semantic Interfaces", file: "ch15-sequences.lisp" },
  { num: 16, name: "Symbolic Semantic Data", file: "ch16-symbolic-semantic.lisp" },
  { num: 17, name: "Multiple Representations of Meaning", file: "ch17-multiple-representations.lisp" },
  { num: 18, name: "Generic Semantic Operations", file: "ch18-generic-semantic.lisp" },
  { num: 19, name: "Conversational State and Memory", file: "ch19-conversational-state.lisp" },
  { num: 20, name: "The Semantic Environment Model", file: "ch20-semantic-environment.lisp" },
  { num: 21, name: "Mutable Semantic Structures", file: "ch21-mutable-semantic.lisp" },
  { num: 22, name: "Concurrent Inference", file: "ch22-concurrent-inference.lisp" },
  { num: 23, name: "Streams of Inference", file: "ch23-streams-of-inference.lisp" },
  { num: 24, name: "Metalinguistic Abstraction: Oracle in Evaluator", file: "ch24-metacircular.lisp" },
  { num: 25, name: "Lazy Semantic Evaluation", file: "ch25-lazy-semantic.lisp" },
  { num: 26, name: "The AMB Inference Engine", file: "ch26-amb-inference.lisp" },
  { num: 27, name: "Logic Programming with Semantic Facts", file: "ch27-logic-programming.lisp" },
];

function readCode(filePath: string): string {
  return fs.readFileSync(filePath, "utf8");
}

async function runDemo(chapterNum: number): Promise<string> {
  try {
    const chapter = CHAPTERS.find(c => c.num === chapterNum);
    if (!chapter) return "Chapter not found";

    const filePath = path.join(DEMO_DIR, chapter.file);
    const code = readCode(filePath);

    // Call omega-repl directly with the Lisp file
    const { stdout } = await execAsync(`npx tsx bin/omega-repl.ts --file ${filePath}`, {
      cwd: path.join(__dirname, ".."),
      timeout: 90000,  // 90 seconds for LLM calls
      maxBuffer: 10 * 1024 * 1024,
    });

    // Format like manual-runner would
    const chName = chapter.file.replace(".lisp", "").toUpperCase();
    let output = `\n${"=".repeat(70)}\n  ${chName}\n${"=".repeat(70)}\n\n`;
    output += code.split("\n").map((line, idx) => {
      const num = (idx + 1).toString().padStart(3, " ");
      return `${num} │ ${line}`;
    }).join("\n");
    output += `\n\n${"-".repeat(70)}\n  OUTPUT:\n${"-".repeat(70)}\n\n`;
    output += stdout.trim();
    output += `\n\n${"=".repeat(70)}\n`;

    return output;
  } catch (err: any) {
    return `Error running demo: ${err.message}`;
  }
}

function extractOutput(fullOutput: string): string {
  const lines = fullOutput.split("\n");
  const outputStartIdx = lines.findIndex(line => line.includes("OUTPUT:"));
  if (outputStartIdx === -1) return fullOutput;

  const outputEndIdx = lines.findIndex((line, idx) =>
    idx > outputStartIdx && line.match(/^═+$/));

  if (outputEndIdx === -1) {
    return lines.slice(outputStartIdx + 2).join("\n").trim();
  }

  return lines.slice(outputStartIdx + 2, outputEndIdx).join("\n").trim();
}

async function generateGallery() {
  console.log("🚀 Generating demo gallery IN PARALLEL...\n");
  console.log(`Running all ${CHAPTERS.length} demos concurrently...\n`);

  // Run ALL demos in parallel
  const startTime = Date.now();
  const results = await Promise.all(
    CHAPTERS.map(async (chapter) => {
      console.log(`▶️  Chapter ${chapter.num}: ${chapter.name}`);

      const filePath = path.join(DEMO_DIR, chapter.file);
      if (!fs.existsSync(filePath)) {
        console.log(`  ⚠️  File not found: ${chapter.file}`);
        return null;
      }

      const code = readCode(filePath);
      const fullOutput = await runDemo(chapter.num);
      const cleanOutput = extractOutput(fullOutput);

      console.log(`  ✅ Chapter ${chapter.num} complete`);

      return {
        chapter,
        code,
        output: cleanOutput,
      };
    })
  );

  const duration = ((Date.now() - startTime) / 1000).toFixed(1);
  console.log(`\n⚡ All ${CHAPTERS.length} demos completed in ${duration}s!\n`);
  console.log("📝 Generating markdown...\n");

  // Build markdown
  let markdown = `# OmegaLLM Demo Gallery

**All 27 Chapter Demos with Live Outputs**

*Generated: ${new Date().toISOString().split('T')[0]}*

This document showcases all chapter demos from the SICP-style manual.
Each demo shows the code and its actual execution output.

**To run any demo yourself:**
\`\`\`bash
npm run manual <chapter-number>
# Examples:
npm run manual 1
npm run manual 5
npm run manual nondeterministic
\`\`\`

---

`;

  for (const result of results) {
    if (!result) continue;

    const { chapter, code, output } = result;

    markdown += `## Chapter ${chapter.num}: ${chapter.name}\n\n`;
    markdown += `**Run:** \`npm run manual ${chapter.num}\`\n\n`;
    markdown += `### Code\n\n`;
    markdown += `\`\`\`lisp\n${code}\`\`\`\n\n`;
    markdown += `### Output\n\n`;
    markdown += `\`\`\`\n${output}\`\`\`\n\n`;
    markdown += `---\n\n`;
  }

  // Write to file
  fs.writeFileSync(OUTPUT_FILE, markdown, "utf8");
  console.log(`✅ Gallery generated: ${OUTPUT_FILE}`);
  console.log(`   ${results.filter(r => r !== null).length} chapters documented`);
  console.log(`   Total time: ${duration}s`);
}

generateGallery().catch(err => {
  console.error("❌ Error generating gallery:", err);
  process.exit(1);
});
