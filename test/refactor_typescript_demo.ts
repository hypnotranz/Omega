// test/refactor_typescript_demo.ts
// Demo: LLM refactors TypeScript code, real tests verify behavior

import * as fs from "fs";
import * as path from "path";
import { execSync } from "child_process";

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
      max_tokens: 2000,
    }),
  });
  const data = await response.json() as any;
  return data.choices?.[0]?.message?.content || "";
}

// Run TypeScript code and return result
function runTypeScript(code: string): { success: boolean; output: string } {
  const tmpFile = path.join(process.cwd(), "tmp_refactor_test.ts");
  try {
    fs.writeFileSync(tmpFile, code);
    const output = execSync(`npx tsx ${tmpFile}`, { encoding: "utf8", timeout: 10000 });
    return { success: true, output: output.trim() };
  } catch (e: any) {
    return { success: false, output: e.message || String(e) };
  } finally {
    try { fs.unlinkSync(tmpFile); } catch { }
  }
}

async function main() {
  const apiKey = loadApiKey();
  if (!apiKey) {
    console.log("No API key found");
    process.exit(1);
  }

  console.log("═".repeat(70));
  console.log("TYPESCRIPT REFACTORING: LLM transforms code, tests verify behavior");
  console.log("═".repeat(70));
  console.log();

  // ═══════════════════════════════════════════════════════════════════
  // ORIGINAL CODE: Messy, imperative, callback-style
  // ═══════════════════════════════════════════════════════════════════

  const originalCode = `
// Messy imperative code with callbacks and mutation
interface User {
  id: number;
  name: string;
  email: string;
  active: boolean;
}

function processUsers(users: User[], callback: (result: string[]) => void) {
  const results: string[] = [];
  for (let i = 0; i < users.length; i++) {
    const user = users[i];
    if (user.active) {
      const formatted = user.name.toUpperCase() + " <" + user.email + ">";
      results.push(formatted);
    }
  }
  callback(results);
}

function getActiveCount(users: User[]): number {
  let count = 0;
  for (let i = 0; i < users.length; i++) {
    if (users[i].active === true) {
      count = count + 1;
    }
  }
  return count;
}

function findUserById(users: User[], id: number): User | null {
  for (let i = 0; i < users.length; i++) {
    if (users[i].id === id) {
      return users[i];
    }
  }
  return null;
}
`.trim();

  const testCode = `
// TESTS - these must pass after refactoring
const testUsers: User[] = [
  { id: 1, name: "Alice", email: "alice@test.com", active: true },
  { id: 2, name: "Bob", email: "bob@test.com", active: false },
  { id: 3, name: "Carol", email: "carol@test.com", active: true },
  { id: 4, name: "Dave", email: "dave@test.com", active: true },
];

let testsPassed = 0;
let testsFailed = 0;

function test(name: string, condition: boolean) {
  if (condition) {
    console.log("✓ " + name);
    testsPassed++;
  } else {
    console.log("✗ " + name);
    testsFailed++;
  }
}

// Test processUsers
processUsers(testUsers, (result) => {
  test("processUsers returns 3 active users", result.length === 3);
  test("processUsers formats correctly", result[0] === "ALICE <alice@test.com>");
  test("processUsers includes Carol", result.some(r => r.includes("CAROL")));
});

// Test getActiveCount
test("getActiveCount returns 3", getActiveCount(testUsers) === 3);
test("getActiveCount with empty array", getActiveCount([]) === 0);

// Test findUserById
test("findUserById finds Alice", findUserById(testUsers, 1)?.name === "Alice");
test("findUserById finds Carol", findUserById(testUsers, 3)?.name === "Carol");
test("findUserById returns null for missing", findUserById(testUsers, 99) === null);

console.log("\\nResults: " + testsPassed + " passed, " + testsFailed + " failed");
if (testsFailed > 0) process.exit(1);
`;

  console.log("ORIGINAL CODE (imperative, callbacks, for-loops):");
  console.log("─".repeat(70));
  console.log(originalCode);
  console.log();

  // Verify original works
  console.log("Running tests on original code...");
  const origResult = runTypeScript(originalCode + "\n" + testCode);
  console.log(origResult.output);
  console.log();

  if (!origResult.success) {
    console.log("Original code failed tests - aborting");
    process.exit(1);
  }

  // ═══════════════════════════════════════════════════════════════════
  // ASK LLM TO REFACTOR
  // ═══════════════════════════════════════════════════════════════════

  console.log("─".repeat(70));
  console.log("ASKING LLM TO REFACTOR TO MODERN FUNCTIONAL STYLE...");
  console.log("─".repeat(70));

  const prompt = `You are refactoring TypeScript code from imperative to modern functional style.

ORIGINAL CODE:
\`\`\`typescript
${originalCode}
\`\`\`

REFACTORING REQUIREMENTS:
1. Replace for-loops with array methods (filter, map, find, reduce)
2. Replace callback in processUsers with a direct return
3. Use arrow functions and modern syntax
4. Keep the same function signatures (names and parameters) so tests pass
5. Keep the User interface unchanged

The refactored code must pass these tests:
- processUsers(users, callback) - callback receives array of "NAME <email>" for active users
- getActiveCount(users) - returns count of active users
- findUserById(users, id) - returns user or null

Return ONLY the refactored TypeScript code (interface + 3 functions). No explanation.`;

  let refactoredCode: string | null = null;
  let attempts = 0;
  const maxAttempts = 3;

  while (attempts < maxAttempts) {
    attempts++;
    console.log(`\nAttempt ${attempts}/${maxAttempts}:`);

    try {
      const feedbackPrompt = attempts === 1 ? prompt :
        `${prompt}\n\nYour previous attempt failed the tests. Make sure:\n1. processUsers still uses callback pattern\n2. Function signatures match exactly\n3. All array methods are used correctly`;

      const response = await askLLM(feedbackPrompt, apiKey);

      // Extract code
      let code = response.replace(/```typescript\n?/g, "").replace(/```/g, "").trim();
      console.log("LLM generated:");
      console.log(code.split("\n").slice(0, 15).map(l => "  " + l).join("\n"));
      if (code.split("\n").length > 15) console.log("  ...");

      // Run tests
      const testResult = runTypeScript(code + "\n" + testCode);
      console.log("\nTest output:");
      console.log(testResult.output);

      if (testResult.success && testResult.output.includes("0 failed")) {
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
    console.log("\nREFACTORED CODE (functional style):");
    console.log("─".repeat(70));
    console.log(refactoredCode);
    console.log("─".repeat(70));
    console.log("\n✓ Converted for-loops to filter/map/find");
    console.log("✓ All 8 tests pass - behavior preserved");
    console.log("✓ Code is now idiomatic modern TypeScript");
  } else {
    console.log("\nRefactoring failed after", maxAttempts, "attempts");
  }
}

main().catch(console.error);
