import { describe, it, expect, beforeEach, afterEach } from "vitest";
import * as fs from "node:fs";
import * as path from "node:path";
import * as os from "node:os";

// Import the extractSexpressions function from omega-repl
// Since it's not exported, we'll need to test it indirectly through file loading
// or we can extract and test it separately

/**
 * Helper to extract S-expressions from text.
 * This is a copy of the function from bin/omega-repl.ts for testing purposes.
 */
function extractSexpressions(text: string): string[] {
  const results: string[] = [];
  let current = "";
  let depth = 0;
  let inString = false;
  let escape = false;

  for (const char of text) {
    if (escape) {
      current += char;
      escape = false;
      continue;
    }

    if (char === "\\") {
      current += char;
      escape = true;
      continue;
    }

    if (char === '"') {
      inString = !inString;
      current += char;
      continue;
    }

    if (inString) {
      current += char;
      continue;
    }

    if (char === "(") {
      if (depth === 0 && current.trim()) {
        // Push any non-paren content before this
        results.push(current.trim());
        current = "";
      }
      depth++;
      current += char;
    } else if (char === ")") {
      depth--;
      current += char;
      if (depth === 0) {
        results.push(current.trim());
        current = "";
      }
    } else {
      current += char;
    }
  }

  // Handle any remaining content (atoms, numbers, etc.)
  if (current.trim()) {
    results.push(current.trim());
  }

  return results.filter(s => s.length > 0);
}

describe("extractSexpressions", () => {
  describe("single-line S-expressions", () => {
    it("should extract a simple atom", () => {
      const result = extractSexpressions("42");
      expect(result).toEqual(["42"]);
    });

    it("should extract a simple list", () => {
      const result = extractSexpressions("(+ 1 2)");
      expect(result).toEqual(["(+ 1 2)"]);
    });

    it("should extract multiple single-line expressions", () => {
      const result = extractSexpressions("(+ 1 2) (* 3 4)");
      expect(result).toEqual(["(+ 1 2)", "(* 3 4)"]);
    });

    it("should extract atoms before and after lists", () => {
      const result = extractSexpressions("x (+ 1 2) y");
      expect(result).toEqual(["x", "(+ 1 2)", "y"]);
    });
  });

  describe("multi-line S-expressions", () => {
    it("should extract a multi-line list", () => {
      const input = `(define (greet name)
  (list "Hello"
        name
        "!"))`;
      const result = extractSexpressions(input);
      expect(result).toHaveLength(1);
      expect(result[0]).toContain("define");
      expect(result[0]).toContain("greet");
      expect(result[0]).toContain("Hello");
    });

    it("should extract multi-line nested lists", () => {
      const input = `(define (is-complaint? text)
  (equal? "yes"
    (effect infer.op
      (list "Is this customer note a complaint? yes/no: " text))))`;
      const result = extractSexpressions(input);
      expect(result).toHaveLength(1);
      expect(result[0]).toContain("define");
      expect(result[0]).toContain("is-complaint?");
      expect(result[0]).toContain("equal?");
      expect(result[0]).toContain("effect");
    });

    it("should extract multiple multi-line expressions", () => {
      const input = `(define (foo x)
  (+ x 1))

(define (bar y)
  (* y 2))`;
      const result = extractSexpressions(input);
      expect(result).toHaveLength(2);
      expect(result[0]).toContain("foo");
      expect(result[1]).toContain("bar");
    });
  });

  describe("string handling", () => {
    it("should handle strings with parentheses", () => {
      const result = extractSexpressions('(print "hello (world)")');
      expect(result).toEqual(['(print "hello (world)")']);
    });

    it("should handle strings with escaped quotes", () => {
      const result = extractSexpressions('(print "say \\"hello\\"")');
      expect(result).toEqual(['(print "say \\"hello\\"")']);
    });

    it("should handle multi-line strings", () => {
      const input = `(print "line 1
line 2
line 3")`;
      const result = extractSexpressions(input);
      expect(result).toHaveLength(1);
      expect(result[0]).toContain("line 1");
      expect(result[0]).toContain("line 2");
    });
  });

  describe("edge cases", () => {
    it("should handle empty input", () => {
      const result = extractSexpressions("");
      expect(result).toEqual([]);
    });

    it("should handle whitespace-only input", () => {
      const result = extractSexpressions("   \n  \t  ");
      expect(result).toEqual([]);
    });

    it("should handle nested lists", () => {
      const result = extractSexpressions("(a (b (c (d))))");
      expect(result).toEqual(["(a (b (c (d))))"]);
    });

    it("should handle unbalanced parentheses gracefully", () => {
      // This is an error case, but we test current behavior
      const result = extractSexpressions("(+ 1 2");
      // The function doesn't complete the expression
      expect(result.length).toBeLessThanOrEqual(1);
    });

    it("should skip pure whitespace between expressions", () => {
      const input = `(+ 1 2)


(* 3 4)`;
      const result = extractSexpressions(input);
      expect(result).toEqual(["(+ 1 2)", "(* 3 4)"]);
    });
  });

  describe("comment handling", () => {
    it("should NOT strip comments (that's handled by preprocessing)", () => {
      // extractSexpressions doesn't handle comments, that's done before calling it
      const input = "(+ 1 2) ; comment here";
      const result = extractSexpressions(input);
      expect(result[0]).toContain("(+ 1 2)");
      // The semicolon and comment are included as trailing content
    });
  });
});

describe("file loading with multi-line S-expressions", () => {
  let tempDir: string;

  beforeEach(() => {
    // Create a temporary directory for test files
    tempDir = fs.mkdtempSync(path.join(os.tmpdir(), "omega-test-"));
  });

  afterEach(() => {
    // Clean up temporary directory
    if (fs.existsSync(tempDir)) {
      fs.rmSync(tempDir, { recursive: true, force: true });
    }
  });

  describe("comment stripping", () => {
    it("should strip comment-only lines", () => {
      const fileContent = `;; This is a comment
(+ 1 2)
; Another comment
(* 3 4)`;

      const cleanedContent = fileContent
        .split("\n")
        .filter(line => !line.trim().startsWith(";"))
        .join("\n");

      const sexprs = extractSexpressions(cleanedContent);
      expect(sexprs).toEqual(["(+ 1 2)", "(* 3 4)"]);
    });

    it("should preserve inline comments for now", () => {
      // Note: inline comments (after code on same line) are not stripped
      // This is acceptable as the tokenizer handles them
      const fileContent = "(+ 1 2) ; inline comment";

      const cleanedContent = fileContent
        .split("\n")
        .filter(line => !line.trim().startsWith(";"))
        .join("\n");

      const sexprs = extractSexpressions(cleanedContent);
      expect(sexprs.length).toBeGreaterThanOrEqual(1);
      expect(sexprs[0]).toContain("(+ 1 2)");
    });

    it("should handle mixed comments and multi-line code", () => {
      const fileContent = `;; Function definition
(define (greet name)
  ; Nested comment
  (list "Hello"
        name
        "!"))
;; End of file`;

      const cleanedContent = fileContent
        .split("\n")
        .filter(line => !line.trim().startsWith(";"))
        .join("\n");

      const sexprs = extractSexpressions(cleanedContent);
      expect(sexprs).toHaveLength(1);
      expect(sexprs[0]).toContain("define");
      expect(sexprs[0]).toContain("greet");
    });
  });

  describe("integration test scenarios", () => {
    it("should handle the example from ch03-composition.lisp", () => {
      const fileContent = `(define (is-complaint? text)
  (equal? "yes"
    (effect infer.op
      (list "Is this customer note a complaint? yes/no: " text))))`;

      const cleanedContent = fileContent
        .split("\n")
        .filter(line => !line.trim().startsWith(";"))
        .join("\n");

      const sexprs = extractSexpressions(cleanedContent);

      // Should be extracted as a single S-expression, not 4 separate ones
      expect(sexprs).toHaveLength(1);
      expect(sexprs[0]).toContain("is-complaint?");
      expect(sexprs[0]).toContain("equal?");
      expect(sexprs[0]).toContain("effect");
      expect(sexprs[0]).toContain("infer.op");
    });

    it("should handle a complete file with multiple definitions", () => {
      const fileContent = `;; Test file with multiple functions
(define (add x y)
  (+ x y))

(define (multiply x y)
  (* x y))

;; Test the functions
(add 2 3)
(multiply 4 5)`;

      const cleanedContent = fileContent
        .split("\n")
        .filter(line => !line.trim().startsWith(";"))
        .join("\n");

      const sexprs = extractSexpressions(cleanedContent);

      expect(sexprs).toHaveLength(4);
      expect(sexprs[0]).toContain("add");
      expect(sexprs[1]).toContain("multiply");
      expect(sexprs[2]).toContain("(add 2 3)");
      expect(sexprs[3]).toContain("(multiply 4 5)");
    });

    it("should handle empty lines and whitespace correctly", () => {
      const fileContent = `

(define x 42)


(define y 24)

`;

      const cleanedContent = fileContent
        .split("\n")
        .filter(line => !line.trim().startsWith(";"))
        .join("\n");

      const sexprs = extractSexpressions(cleanedContent);

      expect(sexprs).toHaveLength(2);
      expect(sexprs[0]).toContain("x");
      expect(sexprs[1]).toContain("y");
    });
  });
});
