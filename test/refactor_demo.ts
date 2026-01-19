// test/refactor_demo.ts
// Demo: LLM refactors code while tests verify behavior preservation

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
      model: "gpt-4o",  // Use smarter model for refactoring
      messages: [{ role: "user", content: prompt }],
      max_tokens: 1000,
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
  console.log("REFACTORING DEMO: LLM transforms code, tests verify behavior");
  console.log("═".repeat(70));
  console.log();

  // ═══════════════════════════════════════════════════════════════════
  // REFACTORING TASK: Convert imperative-style to functional style
  // ═══════════════════════════════════════════════════════════════════

  const originalCode = `
(define (sum-squares-imperative n)
  (letrec ((loop (lambda (i acc)
                   (if (> i n)
                       acc
                       (loop (+ i 1) (+ acc (* i i)))))))
    (loop 1 0)))
`.trim();

  const tests: TestCase[] = [
    { name: "sum-squares(1)", expr: "(sum-squares 1)", expected: 1 },
    { name: "sum-squares(3)", expr: "(sum-squares 3)", expected: 14 },  // 1 + 4 + 9
    { name: "sum-squares(5)", expr: "(sum-squares 5)", expected: 55 }, // 1+4+9+16+25
    { name: "sum-squares(10)", expr: "(sum-squares 10)", expected: 385 },
  ];

  console.log("ORIGINAL CODE (imperative style with loop):");
  console.log("─".repeat(70));
  console.log(originalCode);
  console.log();

  // Verify original works
  const origTests = tests.map(t => ({ ...t, expr: t.expr.replace("sum-squares", "sum-squares-imperative") }));
  const origResult = await runTests(originalCode, origTests);
  console.log("Original code test results:", origResult.passed ? "PASS" : "FAIL");
  for (const r of origResult.results) console.log(r);
  console.log();

  // ═══════════════════════════════════════════════════════════════════
  // ASK LLM TO REFACTOR
  // ═══════════════════════════════════════════════════════════════════

  console.log("─".repeat(70));
  console.log("ASKING LLM TO REFACTOR...");
  console.log("─".repeat(70));

  const prompt = `You are refactoring Lisp code. The goal is to transform this imperative-style code into a cleaner recursive style WITHOUT the explicit loop variable.

ORIGINAL CODE:
${originalCode}

REQUIREMENTS:
1. Create a function called "sum-squares" (not sum-squares-imperative)
2. Use direct recursion instead of the loop helper
3. The function must pass these tests:
   - (sum-squares 1) = 1
   - (sum-squares 3) = 14  ; 1² + 2² + 3²
   - (sum-squares 5) = 55
   - (sum-squares 10) = 385

Return ONLY the refactored (define ...) form. No explanation.
The dialect supports: define, lambda, if, +, -, *, /, =, <, >, <=, >=, let, letrec.`;

  let refactoredCode: string | null = null;
  let attempts = 0;
  const maxAttempts = 3;

  while (attempts < maxAttempts) {
    attempts++;
    console.log(`\nAttempt ${attempts}/${maxAttempts}:`);

    try {
      const response = await askLLM(
        attempts === 1 ? prompt : `${prompt}\n\nYour previous attempt failed the tests. Try again.`,
        apiKey
      );

      // Extract code from response
      let code = response.replace(/```[a-z]*\n?/g, "").replace(/```/g, "").trim();
      code = code.replace(/\s+/g, " ");
      console.log("LLM generated:", code.slice(0, 80) + (code.length > 80 ? "..." : ""));

      // Run tests
      const testResult = await runTests(code, tests);
      console.log("Test results:", testResult.passed ? "PASS ✓" : "FAIL ✗");
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
    console.log("\nREFACTORED CODE:");
    console.log(refactoredCode);
    console.log("\nBEHAVIOR VERIFIED BY TESTS ✓");

    // Show it actually runs
    console.log("\nLIVE VERIFICATION:");
    const v = await evalOmega(`(begin ${refactoredCode} (sum-squares 10))`);
    console.log(`(sum-squares 10) => ${v}`);
  } else {
    console.log("\nRefactoring failed after", maxAttempts, "attempts");
  }
}

main().catch(console.error);
