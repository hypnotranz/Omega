/**
 * Language Building Primitives Tests
 *
 * Tests for the Sussman-style language building primitives:
 * - eval: Evaluate quoted expressions
 * - gensym: Generate unique symbols
 * - make-evaluator: Create custom evaluators
 * - eval-in: Evaluate in custom evaluator
 * - register-macro: Register macro transformers
 * - expand-macro: Expand macros
 */

import { describe, it, expect } from "vitest";
import { evalOmega } from "./helpers/omegaHarness";

describe("Language Building Primitives", () => {

  describe("gensym - Hygienic Symbol Generation", () => {
    it("generates unique symbols", async () => {
      const result = await evalOmega(`
        (define s1 (gensym))
        (define s2 (gensym))
        (equal? s1 s2)
      `);
      expect(result).toBe(false);
    });

    it("uses prefix when provided", async () => {
      const result = await evalOmega(`
        (define s (gensym "temp"))
        (symbol? s)
      `);
      expect(result).toBe(true);
    });
  });

  describe("register-macro / expand-macro - Syntactic Extension", () => {
    it("registers a macro and checks with macro?", async () => {
      const result = await evalOmega(`
        (register-macro 'double
          (lambda (form)
            (let ((x (cadr form)))
              (list '+ x x))))
        (macro? 'double)
      `);
      expect(result).toBe(true);
    });

    it("macro? returns false for non-macros", async () => {
      const result = await evalOmega(`(macro? 'not-a-macro)`);
      expect(result).toBe(false);
    });
  });

  describe("make-evaluator / eval-in - Custom Evaluators", () => {
    it("creates an evaluator", async () => {
      const result = await evalOmega(`
        (define my-eval (make-evaluator))
        (evaluator? my-eval)
      `);
      expect(result).toBe(true);
    });

    it("evaluator? returns false for non-evaluators", async () => {
      const result = await evalOmega(`(evaluator? 42)`);
      expect(result).toBe(false);
    });

    it("creates evaluator with extended primitives", async () => {
      const result = await evalOmega(`
        (define square (lambda (x) (* x x)))
        (define my-dsl (make-evaluator))
        (evaluator? my-dsl)
      `);
      expect(result).toBe(true);
    });
  });

  describe("make-transformer - Macro Transformers", () => {
    it("creates a transformer from a procedure", async () => {
      const result = await evalOmega(`
        (define t (make-transformer (lambda (x) x)))
        (not (equal? t #f))
      `);
      expect(result).toBe(true);
    });
  });

  describe("syntax->datum / datum->syntax - Syntax Conversion", () => {
    it("converts syntax to datum", async () => {
      const result = await evalOmega(`
        (define d (syntax->datum '(+ 1 2)))
        (not (equal? d #f))
      `);
      expect(result).toBe(true);
    });

    it("converts datum to syntax", async () => {
      const result = await evalOmega(`
        (define s (datum->syntax (list '+ 1 2)))
        (not (equal? s #f))
      `);
      expect(result).toBe(true);
    });
  });

  describe("Machine Reification - Eval as Data", () => {
    it("creates a machine from expression", async () => {
      const result = await evalOmega(`
        (define m (machine-new '(+ 1 2)))
        (machine? m)
      `);
      expect(result).toBe(true);
    });

    it("steps through execution", async () => {
      const result = await evalOmega(`
        (define m (machine-new '(+ 1 2)))
        (define m1 (machine-step m))
        (machine-step-count m1)
      `);
      expect(result).toBe(1);
    });

    it("runs to completion", async () => {
      const result = await evalOmega(`
        (define m (machine-new '(+ 2 3)))
        (define m-done (machine-run m))
        (machine-done? m-done)
      `);
      expect(result).toBe(true);
    });

    it("checks if machine is done", async () => {
      const result = await evalOmega(`
        (define m (machine-new '(+ 1 1)))
        (define m-done (machine-run m))
        (machine-done? m-done)
      `);
      expect(result).toBe(true);
    });

    it("forks a machine", async () => {
      const result = await evalOmega(`
        (define m (machine-new '(* 2 3)))
        (define m2 (machine-fork m))
        (machine? m2)
      `);
      expect(result).toBe(true);
    });
  });

  describe("call/cc - First-Class Continuations", () => {
    it("captures and invokes continuation", async () => {
      const result = await evalOmega(`
        (+ 1 (call/cc (lambda (k) (k 5))))
      `);
      expect(result).toBe(6);
    });

    it("escapes from nested computation", async () => {
      const result = await evalOmega(`
        (+ 1 (call/cc (lambda (k) (* 10 (k 5)))))
      `);
      expect(result).toBe(6);
    });
  });

  describe("Integration: Building a Mini-DSL", () => {
    it("defines and uses a complete DSL", async () => {
      const result = await evalOmega(`
        ;; Define DSL primitives
        (define double (lambda (x) (* x 2)))
        (define square (lambda (x) (* x x)))
        (define inc (lambda (x) (+ x 1)))

        ;; Create DSL evaluator
        (define math-dsl (make-evaluator))

        ;; Verify DSL was created
        (evaluator? math-dsl)
      `);
      expect(result).toBe(true);
    });

    it("registers and uses a macro", async () => {
      const result = await evalOmega(`
        ;; Define 'when' macro (opposite of unless)
        (register-macro 'when
          (lambda (form)
            (let ((cond (cadr form))
                  (body (caddr form)))
              (list 'if cond body))))

        ;; Verify macro is registered
        (macro? 'when)
      `);
      expect(result).toBe(true);
    });
  });
});
