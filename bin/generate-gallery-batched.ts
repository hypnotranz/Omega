#!/usr/bin/env npx tsx
// Batched gallery generation to avoid rate limits

import * as fs from "fs";
import * as path from "path";
import { spawn } from "child_process";
import { fileURLToPath } from "url";

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

function runDemoWithSpawn(filePath: string): Promise<string> {
  return new Promise((resolve, reject) => {
    console.log(`    [SPAWNING] ${path.basename(filePath)}`);

    const child = spawn("npx", ["tsx", "bin/omega-repl.ts", "--file", filePath], {
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
      console.log(`    [DONE] ${path.basename(filePath)}`);
      // Success if we got any output, regardless of exit code
      if (stdout.trim()) {
        resolve(stdout);
      } else if (code === 0) {
        resolve(stdout || stderr);
      } else {
        reject(new Error(`Exit ${code}: ${stderr}`));
      }
    });

    child.on("error", (err) => {
      console.log(`    [ERROR] ${path.basename(filePath)}: ${err.message}`);
      reject(err);
    });

    // 2 minute timeout for LLM calls
    setTimeout(() => {
      child.kill();
      console.log(`    [TIMEOUT] ${path.basename(filePath)}`);
      reject(new Error("Timeout"));
    }, 120000);
  });
}

// Run chapters in batches to avoid rate limits
async function runBatch(batch: typeof CHAPTERS): Promise<Array<{ chapter: typeof CHAPTERS[0], code: string, output: string }>> {
  return Promise.all(
    batch.map(async (chapter) => {
      try {
        const filePath = path.join(DEMO_DIR, chapter.file);
        const code = fs.readFileSync(filePath, "utf8");
        const output = await runDemoWithSpawn(filePath);

        return {
          chapter,
          code,
          output: output.trim(),
        };
      } catch (err: any) {
        return {
          chapter,
          code: fs.readFileSync(path.join(DEMO_DIR, chapter.file), "utf8"),
          output: `Error: ${err.message}`,
        };
      }
    })
  );
}

async function generateGallery() {
  console.log("Generating gallery with REAL LLM calls (batched to avoid rate limits)...\n");

  const BATCH_SIZE = 5;  // Run 5 at a time
  const results: Array<{ chapter: typeof CHAPTERS[0], code: string, output: string }> = [];

  for (let i = 0; i < CHAPTERS.length; i += BATCH_SIZE) {
    const batch = CHAPTERS.slice(i, Math.min(i + BATCH_SIZE, CHAPTERS.length));
    console.log(`\n📦 Batch ${Math.floor(i / BATCH_SIZE) + 1}/${Math.ceil(CHAPTERS.length / BATCH_SIZE)}: Chapters ${batch[0].num}-${batch[batch.length - 1].num}`);

    const batchResults = await runBatch(batch);
    results.push(...batchResults);

    // Small delay between batches
    if (i + BATCH_SIZE < CHAPTERS.length) {
      console.log("  ⏳ Waiting 5s before next batch...");
      await new Promise(resolve => setTimeout(resolve, 5000));
    }
  }

  console.log("\n📝 Generating markdown...\n");

  let markdown = `# OmegaLLM Demo Gallery\n\n**All 27 Chapter Demos with Live LLM Outputs**\n\n*Generated: ${new Date().toISOString().split('T')[0]}*\n\n**To run any demo:**\n\`\`\`bash\nnpm run manual <chapter-number>\n\`\`\`\n\n---\n\n`;

  for (const result of results) {
    markdown += `## Chapter ${result.chapter.num}: ${result.chapter.name}\n\n`;
    markdown += `**Run:** \`npm run manual ${result.chapter.num}\`\n\n`;
    markdown += `### Code\n\n\`\`\`lisp\n${result.code}\`\`\`\n\n`;
    markdown += `### Output\n\n\`\`\`\n${result.output}\`\`\`\n\n---\n\n`;
  }

  fs.writeFileSync(OUTPUT_FILE, markdown, "utf8");
  console.log(`✅ Gallery saved: ${OUTPUT_FILE}`);
  console.log(`   ${results.length} chapters documented\n`);

  // Count successes and failures
  const successes = results.filter(r => !r.output.startsWith("Error:")).length;
  const failures = results.length - successes;
  console.log(`   ✅ ${successes} successful`);
  console.log(`   ❌ ${failures} failed\n`);
}

generateGallery().catch(console.error);
