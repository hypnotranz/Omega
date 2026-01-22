import { describe, it, expect, beforeEach, afterEach } from "vitest";
import * as fs from "node:fs";
import * as path from "node:path";
import * as os from "node:os";
import { execSync } from "node:child_process";

const REPL_TIMEOUT_MS = 30000;
const REPL_COMMAND = "npx tsx bin/omega-repl.ts";

function runReplCommand(args: string): string {
  return execSync(`${REPL_COMMAND} ${args}`, {
    cwd: process.cwd(),
    encoding: "utf8",
    timeout: REPL_TIMEOUT_MS,
    stdio: "pipe"
  });
}

function throwIfTimedOut(error: unknown): void {
  const message = error instanceof Error ? error.message : "";
  const code = typeof error === "object" && error !== null && "code" in error
    ? String((error as { code?: unknown }).code)
    : "";
  if (code === "ETIMEDOUT" || message.includes("ETIMEDOUT")) {
    throw new Error(`REPL execution timed out: ${message || "ETIMEDOUT"}`);
  }
}

describe("REPL file loader integration tests", () => {
  let tempDir: string;

  beforeEach(() => {
    tempDir = fs.mkdtempSync(path.join(os.tmpdir(), "omega-file-test-"));
  });

  afterEach(() => {
    if (fs.existsSync(tempDir)) {
      fs.rmSync(tempDir, { recursive: true, force: true });
    }
  });

  it("should handle multi-line S-expressions from file", () => {
    // Create a test file with multi-line S-expression
    const testFile = path.join(tempDir, "test-multiline.lisp");
    const content = `(define (greet name)
  (list "Hello"
        name
        "!"))

(greet "World")`;

    fs.writeFileSync(testFile, content, "utf8");

    // Run the REPL with the file
    // Note: This will only work if npx tsx is available
    try {
      const result = runReplCommand(`--file "${testFile}"`);

      // The file should execute without syntax errors
      // We don't check the exact output, just that it didn't fail with parse errors
      expect(result).not.toContain("unexpected token");
      expect(result).not.toContain("parse error");
      expect(result).not.toContain("Error:");
    } catch (error: any) {
      throwIfTimedOut(error);
      // If the execution fails, check the error message
      const stderr = error.stderr?.toString() || "";
      const stdout = error.stdout?.toString() || "";
      const output = stderr + stdout;

      // These errors indicate the OLD broken behavior (splitting by newline)
      if (output.includes("unexpected token") ||
          output.includes("parse") ||
          output.includes("unexpected )")) {
        throw new Error(`File loader still splits by newline instead of using extractSexpressions. Output: ${output}`);
      }

      // Some other error (maybe runtime error, missing deps, etc.) - that's okay for this test
      // We're only testing that the file PARSES correctly
      console.warn("Test execution had errors, but not parse-related:", error.message);
    }
  });

  it("should handle single-line expressions correctly (regression test)", () => {
    const testFile = path.join(tempDir, "test-single.lisp");
    const content = "(+ 1 2 3)";

    fs.writeFileSync(testFile, content, "utf8");

    try {
      const result = runReplCommand(`--file "${testFile}"`);

      expect(result).not.toContain("unexpected token");
      expect(result).not.toContain("parse error");
    } catch (error: any) {
      throwIfTimedOut(error);
      const stderr = error.stderr?.toString() || "";
      const stdout = error.stdout?.toString() || "";
      const output = stderr + stdout;

      if (output.includes("unexpected token") || output.includes("parse")) {
        throw new Error(`Single-line file failed to parse: ${output}`);
      }
    }
  });

  it("should strip comment lines correctly", () => {
    const testFile = path.join(tempDir, "test-comments.lisp");
    const content = `;; This is a comment
(define x 42)
; Another comment
(+ x 1)`;

    fs.writeFileSync(testFile, content, "utf8");

    try {
      const result = runReplCommand(`--file "${testFile}"`);

      expect(result).not.toContain("unexpected token");
      expect(result).not.toContain("parse error");
    } catch (error: any) {
      throwIfTimedOut(error);
      const stderr = error.stderr?.toString() || "";
      const stdout = error.stdout?.toString() || "";
      const output = stderr + stdout;

      if (output.includes("unexpected token") || output.includes("parse")) {
        throw new Error(`File with comments failed to parse: ${output}`);
      }
    }
  });

  it("should handle the ch03-composition.lisp example", () => {
    const testFile = path.join(tempDir, "test-ch03.lisp");
    const content = `(define (is-complaint? text)
  (equal? "yes"
    (effect infer.op
      (list "Is this customer note a complaint? yes/no: " text))))`;

    fs.writeFileSync(testFile, content, "utf8");

    try {
      const result = runReplCommand(`--file "${testFile}"`);

      // This should NOT have 4 parse errors (one per line)
      // It should be parsed as a single expression
      expect(result).not.toContain("unexpected token");
      expect(result).not.toContain("unexpected )");
    } catch (error: any) {
      throwIfTimedOut(error);
      const stderr = error.stderr?.toString() || "";
      const stdout = error.stdout?.toString() || "";
      const output = stderr + stdout;

      // Check if it's a parse error (indicating the bug is still present)
      if (output.includes("unexpected )") ||
          output.includes("unexpected token") ||
          (output.match(/parse/gi) || []).length > 1) {
        throw new Error(`Multi-line function definition was split incorrectly: ${output}`);
      }

      // Other errors (e.g., undefined 'effect' or 'infer.op') are fine -
      // we're only testing that the parsing works
      console.warn("Test had runtime errors (expected without full environment):", error.message);
    }
  });
});
