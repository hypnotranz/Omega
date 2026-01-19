// test/core/sexp/sexp.spec.ts
// Tests for S-expression parser and printer

import { describe, it, expect } from "vitest";
import {
  sym, num, str, bool, list,
  parseSexp, sexpToString, sexpEq,
} from "../../../src/core/sexp/sexp";

describe("sexp constructors", () => {
  it("creates symbols", () => {
    const s = sym("foo");
    expect(s.tag).toBe("Sym");
    expect(s.name).toBe("foo");
  });

  it("creates numbers", () => {
    const n = num(42);
    expect(n.tag).toBe("Num");
    expect(n.n).toBe(42);
  });

  it("creates strings", () => {
    const s = str("hello");
    expect(s.tag).toBe("Str");
    expect(s.s).toBe("hello");
  });

  it("creates booleans", () => {
    expect(bool(true).b).toBe(true);
    expect(bool(false).b).toBe(false);
  });

  it("creates lists", () => {
    const l = list([sym("a"), num(1)]);
    expect(l.tag).toBe("List");
    expect(l.items.length).toBe(2);
  });
});

describe("sexpEq", () => {
  it("compares symbols", () => {
    expect(sexpEq(sym("a"), sym("a"))).toBe(true);
    expect(sexpEq(sym("a"), sym("b"))).toBe(false);
  });

  it("compares numbers", () => {
    expect(sexpEq(num(1), num(1))).toBe(true);
    expect(sexpEq(num(1), num(2))).toBe(false);
  });

  it("compares lists recursively", () => {
    const a = list([sym("x"), num(1)]);
    const b = list([sym("x"), num(1)]);
    const c = list([sym("x"), num(2)]);
    expect(sexpEq(a, b)).toBe(true);
    expect(sexpEq(a, c)).toBe(false);
  });

  it("rejects mismatched tags", () => {
    expect(sexpEq(sym("1"), num(1))).toBe(false);
  });
});

describe("parseSexp", () => {
  it("parses symbols", () => {
    expect(parseSexp("foo")).toEqual(sym("foo"));
  });

  it("parses numbers", () => {
    expect(parseSexp("42")).toEqual(num(42));
    expect(parseSexp("-17")).toEqual(num(-17));
    expect(parseSexp("3.14")).toEqual(num(3.14));
  });

  it("parses booleans", () => {
    expect(parseSexp("#t")).toEqual(bool(true));
    expect(parseSexp("#f")).toEqual(bool(false));
    expect(parseSexp("true")).toEqual(bool(true));
    expect(parseSexp("false")).toEqual(bool(false));
  });

  it("parses strings", () => {
    expect(parseSexp('"hello"')).toEqual(str("hello"));
    expect(parseSexp('"with\\nescapes"')).toEqual(str("with\nescapes"));
  });

  it("parses empty list", () => {
    expect(parseSexp("()")).toEqual(list([]));
  });

  it("parses simple lists", () => {
    expect(parseSexp("(a b c)")).toEqual(list([sym("a"), sym("b"), sym("c")]));
    expect(parseSexp("(+ 1 2)")).toEqual(list([sym("+"), num(1), num(2)]));
  });

  it("parses nested lists", () => {
    expect(parseSexp("(a (b c))")).toEqual(
      list([sym("a"), list([sym("b"), sym("c")])])
    );
  });

  it("parses quoted expressions", () => {
    expect(parseSexp("'x")).toEqual(list([sym("quote"), sym("x")]));
    expect(parseSexp("'(a b)")).toEqual(
      list([sym("quote"), list([sym("a"), sym("b")])])
    );
  });

  it("handles whitespace", () => {
    expect(parseSexp("  ( a   b  )  ")).toEqual(list([sym("a"), sym("b")]));
  });

  it("handles comments", () => {
    expect(parseSexp(`
      ; comment
      (a ; inline
       b)
    `)).toEqual(list([sym("a"), sym("b")]));
  });

  it("throws on unbalanced parens", () => {
    expect(() => parseSexp("(a b")).toThrow();
    expect(() => parseSexp("a b)")).toThrow();
  });

  it("throws on trailing tokens", () => {
    expect(() => parseSexp("a b")).toThrow("trailing tokens");
  });
});

describe("sexpToString", () => {
  it("prints symbols", () => {
    expect(sexpToString(sym("foo"))).toBe("foo");
  });

  it("prints numbers", () => {
    expect(sexpToString(num(42))).toBe("42");
    expect(sexpToString(num(3.14))).toBe("3.14");
  });

  it("prints booleans", () => {
    expect(sexpToString(bool(true))).toBe("#t");
    expect(sexpToString(bool(false))).toBe("#f");
  });

  it("prints strings with JSON escaping", () => {
    expect(sexpToString(str("hello"))).toBe('"hello"');
    expect(sexpToString(str('with"quote'))).toBe('"with\\"quote"');
  });

  it("prints lists", () => {
    expect(sexpToString(list([sym("+"), num(1), num(2)]))).toBe("(+ 1 2)");
  });

  it("prints quote sugar", () => {
    const quoted = list([sym("quote"), sym("x")]);
    expect(sexpToString(quoted)).toBe("'x");
  });

  it("roundtrips through parse -> print -> parse", () => {
    const exprs = [
      "(+ 1 2)",
      "(lambda (x) (+ x 1))",
      "(if #t \"yes\" \"no\")",
      "(list 'a 'b 'c)",
    ];

    for (const src of exprs) {
      const parsed = parseSexp(src);
      const printed = sexpToString(parsed);
      const reparsed = parseSexp(printed);
      expect(sexpEq(parsed, reparsed)).toBe(true);
    }
  });
});
