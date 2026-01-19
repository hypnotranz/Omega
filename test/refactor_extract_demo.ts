// test/refactor_extract_demo.ts
// Demo: LLM extracts common pattern into helper function

import { evalOmega } from "./helpers/omegaHarness";
import * as fs from "fs";
import * as path from "path";

function loadApiKey(): string | undefined {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  try {
    const configPath = path.join(process.cwd(), "../LambdaRLM/config.yaml");
    const content = fs.readFileSync(configPath, "utf8");
    const match = content.match(/api_key:\s*(\S+)/);
    if (match?.[1]) return match[1];
  } catch { }
  return undefined;
}

async function askLLM(prompt: string, apiKey: string): Promise<string> {
  const response = await fetch("https://api.openai.com/v1/chat/completions", {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      "Authorization": `Bearer ${apiKey}`,
    },
    body: JSON.stringify({
      model: "gpt-4o",
      messages: [{ role: "user", content: prompt }],
      max_tokens: 1500,
    }),
  });
  const data = await response.json() as any;
  return data.choices?.[0]?.message?.content || "";
}

interface TestCase {
  name: string;
  expr: string;
  expected: any;
}

async function runTests(code: string, tests: TestCase[]): Promise<{ passed: boolean; results: string[] }> {
  const results: string[] = [];
  let allPassed = true;

  for (const test of tests) {
    try {
      const program = `(begin ${code} ${test.expr})`;
      const actual = await evalOmega(program);
      const passed = actual === test.expected;
      if (!passed) allPassed = false;
      results.push(`  ${test.name}: ${passed ? "✓" : "✗"} (got ${actual}, expected ${test.expected})`);
    } catch (e: any) {
      allPassed = false;
      results.push(`  ${test.name}: ✗ (error: ${e.message})`);
    }
  }

  return { passed: allPassed, results };
}

async function main() {
  const apiKey = loadApiKey();
  if (!apiKey) {
    console.log("No API key found");
    process.exit(1);
  }

  console.log("═".repeat(70));
  console.log("REFACTORING DEMO: Extract common pattern into helper");
  console.log("═".repeat(70));
  console.log();

  // ═══════════════════════════════════════════════════════════════════
  // REFACTORING TASK: Extract repeated pattern
  // ═══════════════════════════════════════════════════════════════════

  const originalCode = `
(define (double-all-and-sum a b c)
  (+ (+ (* a 2) (* b 2)) (* c 2)))

(define (triple-all-and-sum a b c)
  (+ (+ (* a 3) (* b 3)) (* c 3)))

(define (square-all-and-sum a b c)
  (+ (+ (* a a) (* b b)) (* c c)))
`.trim();

  const tests: TestCase[] = [
    { name: "double-all(1,2,3)", expr: "(double-all-and-sum 1 2 3)", expected: 12 },
    { name: "double-all(5,5,5)", expr: "(double-all-and-sum 5 5 5)", expected: 30 },
    { name: "triple-all(1,2,3)", expr: "(triple-all-and-sum 1 2 3)", expected: 18 },
    { name: "triple-all(2,2,2)", expr: "(triple-all-and-sum 2 2 2)", expected: 18 },
    { name: "square-all(2,3,4)", expr: "(square-all-and-sum 2 3 4)", expected: 29 },
    { name: "square-all(1,1,1)", expr: "(square-all-and-sum 1 1 1)", expected: 3 },
  ];

  console.log("ORIGINAL CODE (repetitive pattern):");
  console.log("─".repeat(70));
  console.log(originalCode);
  console.log();

  // Verify original works
  const origResult = await runTests(originalCode, tests);
  console.log("Original code test results:", origResult.passed ? "PASS" : "FAIL");
  for (const r of origResult.results) console.log(r);
  console.log();

  // ═══════════════════════════════════════════════════════════════════
  // ASK LLM TO REFACTOR: Extract the common pattern
  // ═══════════════════════════════════════════════════════════════════

  console.log("─".repeat(70));
  console.log("ASKING LLM TO EXTRACT COMMON PATTERN...");
  console.log("─".repeat(70));

  const prompt = `You are refactoring Lisp code. The goal is to extract the common pattern into a higher-order helper function.

ORIGINAL CODE:
${originalCode}

OBSERVATION: All three functions apply some operation to each argument, then sum the results.

REQUIREMENTS:
1. Create a helper function "map-and-sum" that takes a function f and three values a, b, c
   and returns (+ (f a) (f b) (f c))
2. Rewrite the three functions to use map-and-sum
3. All original tests must still pass:
   - (double-all-and-sum 1 2 3) = 12
   - (double-all-and-sum 5 5 5) = 30
   - (triple-all-and-sum 1 2 3) = 18
   - (triple-all-and-sum 2 2 2) = 18
   - (square-all-and-sum 2 3 4) = 29
   - (square-all-and-sum 1 1 1) = 3

Return ONLY the refactored code (all define forms). No explanation.
The dialect supports: define, lambda, if, +, -, *, /, =, <, >.`;

  let refactoredCode: string | null = null;
  let attempts = 0;
  const maxAttempts = 3;

  while (attempts < maxAttempts) {
    attempts++;
    console.log(`\nAttempt ${attempts}/${maxAttempts}:`);

    try {
      const feedbackPrompt = attempts === 1 ? prompt :
        `${prompt}\n\nYour previous attempt failed. Make sure map-and-sum takes a function as its first argument and applies it to each of a, b, c.`;

      const response = await askLLM(feedbackPrompt, apiKey);

      // Extract code
      let code = response.replace(/```[a-z]*\n?/g, "").replace(/```/g, "").trim();
      console.log("LLM generated:");
      console.log(code.split("\n").map(l => "  " + l).join("\n"));

      // Run tests
      const testResult = await runTests(code, tests);
      console.log("\nTest results:", testResult.passed ? "PASS ✓" : "FAIL ✗");
      for (const r of testResult.results) console.log(r);

      if (testResult.passed) {
        refactoredCode = code;
        console.log("\n🎉 Refactoring successful!");
        break;
      }

      console.log("\n⟳ Tests failed, asking LLM to try again...");
    } catch (e: any) {
      console.log("Error:", e.message);
    }
  }

  // ═══════════════════════════════════════════════════════════════════
  // FINAL RESULT
  // ═══════════════════════════════════════════════════════════════════

  console.log();
  console.log("═".repeat(70));
  console.log("REFACTORING RESULT");
  console.log("═".repeat(70));

  if (refactoredCode) {
    console.log("\nREFACTORED CODE (with extracted helper):");
    console.log(refactoredCode);
    console.log("\n✓ Common pattern extracted into map-and-sum");
    console.log("✓ All 6 tests pass - behavior preserved");
  } else {
    console.log("\nRefactoring failed after", maxAttempts, "attempts");
  }
}

main().catch(console.error);
