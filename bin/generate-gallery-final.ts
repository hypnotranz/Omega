#!/usr/bin/env npx tsx
// Final working version using spawn

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
      // Success if we got any output, regardless of exit code
      if (stdout.trim()) {
        resolve(stdout);
      } else if (code === 0) {
        resolve(stdout || stderr);
      } else {
        reject(new Error(`Exit ${code}: ${stderr}`));
      }
    });

    child.on("error", reject);

    // 2 minute timeout for LLM calls
    setTimeout(() => {
      child.kill();
      reject(new Error("Timeout"));
    }, 120000);
  });
}

async function generateGallery() {
  console.log("Generating gallery with REAL LLM calls...\n");

  const results = await Promise.all(
    CHAPTERS.map(async (chapter) => {
      console.log(`Running Chapter ${chapter.num}: ${chapter.name}...`);
      try {
        const filePath = path.join(DEMO_DIR, chapter.file);
        const code = fs.readFileSync(filePath, "utf8");
        const output = await runDemoWithSpawn(filePath);

        console.log(`  ✅ Complete`);

        return {
          chapter,
          code,
          output: output.trim(),
        };
      } catch (err: any) {
        console.log(`  ❌ Failed: ${err.message}`);
        return {
          chapter,
          code: fs.readFileSync(path.join(DEMO_DIR, chapter.file), "utf8"),
          output: `Error: ${err.message}`,
        };
      }
    })
  );

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
}

generateGallery().catch(console.error);
