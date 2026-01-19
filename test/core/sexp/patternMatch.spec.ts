// test/core/sexp/patternMatch.spec.ts
// Tests for pattern matcher with _, ?x, and ... ellipsis

import { describe, it, expect } from "vitest";
import { matchSexp, type Bindings } from "../../../src/core/sexp/patternMatch";
import { parseSexp, sexpToString, sym, num, list } from "../../../src/core/sexp/sexp";

function match(pattern: string, value: string) {
  return matchSexp(parseSexp(pattern), parseSexp(value));
}

function bindings(r: ReturnType<typeof match>): Record<string, string> {
  if (!r.ok) return {};
  const out: Record<string, string> = {};
  for (const [k, v] of r.bindings.entries()) {
    out[k] = sexpToString(v);
  }
  return out;
}

describe("pattern matching", () => {
  describe("wildcards", () => {
    it("matches anything with _", () => {
      expect(match("_", "foo").ok).toBe(true);
      expect(match("_", "42").ok).toBe(true);
      expect(match("_", "(a b c)").ok).toBe(true);
    });

    it("matches wildcard in list", () => {
      expect(match("(a _ c)", "(a b c)").ok).toBe(true);
      expect(match("(a _ c)", "(a 42 c)").ok).toBe(true);
    });
  });

  describe("pattern variables", () => {
    it("binds ?x to value", () => {
      const r = match("?x", "foo");
      expect(r.ok).toBe(true);
      expect(bindings(r)).toEqual({ x: "foo" });
    });

    it("binds multiple variables", () => {
      const r = match("(?a ?b)", "(foo bar)");
      expect(r.ok).toBe(true);
      expect(bindings(r)).toEqual({ a: "foo", b: "bar" });
    });

    it("requires consistent bindings", () => {
      const r = match("(?x ?x)", "(foo foo)");
      expect(r.ok).toBe(true);

      const r2 = match("(?x ?x)", "(foo bar)");
      expect(r2.ok).toBe(false);
    });

    it("binds nested structures", () => {
      const r = match("(?fn ?arg)", "((lambda (x) x) 42)");
      expect(r.ok).toBe(true);
      expect(bindings(r).fn).toBe("(lambda (x) x)");
      expect(bindings(r).arg).toBe("42");
    });
  });

  describe("literal matching", () => {
    it("matches identical atoms", () => {
      expect(match("foo", "foo").ok).toBe(true);
      expect(match("42", "42").ok).toBe(true);
      expect(match("#t", "#t").ok).toBe(true);
    });

    it("rejects different atoms", () => {
      expect(match("foo", "bar").ok).toBe(false);
      expect(match("42", "43").ok).toBe(false);
    });

    it("matches identical lists", () => {
      expect(match("(a b c)", "(a b c)").ok).toBe(true);
    });

    it("rejects lists of different length", () => {
      expect(match("(a b)", "(a b c)").ok).toBe(false);
      expect(match("(a b c)", "(a b)").ok).toBe(false);
    });
  });

  describe("ellipsis patterns", () => {
    it("matches zero elements", () => {
      const r = match("(add ?x ...)", "(add)");
      expect(r.ok).toBe(true);
      expect(bindings(r).x).toBe("()"); // empty list
    });

    it("matches one element", () => {
      const r = match("(add ?x ...)", "(add 1)");
      expect(r.ok).toBe(true);
      expect(bindings(r).x).toBe("(1)");
    });

    it("matches multiple elements", () => {
      const r = match("(add ?x ...)", "(add 1 2 3)");
      expect(r.ok).toBe(true);
      expect(bindings(r).x).toBe("(1 2 3)");
    });

    it("matches with prefix", () => {
      const r = match("(fn ?name ?arg ...)", "(fn foo 1 2 3)");
      expect(r.ok).toBe(true);
      expect(bindings(r).name).toBe("foo");
      expect(bindings(r).arg).toBe("(1 2 3)");
    });

    it("matches with suffix", () => {
      const r = match("(?x ... end)", "(a b c end)");
      expect(r.ok).toBe(true);
      expect(bindings(r).x).toBe("(a b c)");
    });

    it("matches with both prefix and suffix", () => {
      const r = match("(begin ?middle ... end)", "(begin a b c end)");
      expect(r.ok).toBe(true);
      expect(bindings(r).middle).toBe("(a b c)");
    });

    it("handles ellipsis with wildcards", () => {
      const r = match("(list _ ...)", "(list 1 2 3)");
      expect(r.ok).toBe(true);
    });

    it("handles multiple ellipsis patterns via backtracking", () => {
      // (a ... x b ...) matching (1 2 x 3 4)
      // a = (1 2), b = (3 4)
      const r = match("(?a ... x ?b ...)", "(1 2 x 3 4)");
      expect(r.ok).toBe(true);
      expect(bindings(r).a).toBe("(1 2)");
      expect(bindings(r).b).toBe("(3 4)");
    });
  });

  describe("complex patterns", () => {
    it("matches lambda pattern", () => {
      const r = match("(lambda (?param ...) ?body)", "(lambda (x y) (+ x y))");
      expect(r.ok).toBe(true);
      expect(bindings(r).param).toBe("(x y)");
      expect(bindings(r).body).toBe("(+ x y)");
    });

    it("matches let pattern", () => {
      const r = match("(let ((?var ?val) ...) ?body ...)", "(let ((x 1) (y 2)) (+ x y) x)");
      expect(r.ok).toBe(true);
      expect(bindings(r).var).toBe("(x y)");
      expect(bindings(r).val).toBe("(1 2)");
      expect(bindings(r).body).toBe("((+ x y) x)");
    });

    it("matches define pattern", () => {
      const r = match("(define (?name ?param ...) ?body ...)", "(define (add x y) (+ x y))");
      expect(r.ok).toBe(true);
      expect(bindings(r).name).toBe("add");
      expect(bindings(r).param).toBe("(x y)");
      expect(bindings(r).body).toBe("((+ x y))");
    });

    it("matches nested ellipsis", () => {
      const r = match("((?x ?y) ...)", "((a 1) (b 2) (c 3))");
      expect(r.ok).toBe(true);
      expect(bindings(r).x).toBe("(a b c)");
      expect(bindings(r).y).toBe("(1 2 3)");
    });
  });

  describe("error cases", () => {
    it("fails on type mismatch", () => {
      expect(match("(a b)", "foo").ok).toBe(false);
    });

    it("fails when pattern is longer", () => {
      expect(match("(a b c d)", "(a b c)").ok).toBe(false);
    });

    it("provides failure reason", () => {
      const r = match("foo", "bar");
      expect(r.ok).toBe(false);
      if (!r.ok) {
        expect(r.reason).toContain("mismatch");
      }
    });
  });
});
