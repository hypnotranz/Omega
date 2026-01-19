// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-4.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import { describe, it, expect } from "vitest";
import type { Syntax, SIdent } from "../../src/core/syntax/syntax";
import { compileSyntaxRules, applySyntaxRules } from "../../src/core/expand/syntaxRules";
import type { Env } from "../../src/core/syntax/binding";

function id(name: string): SIdent { return { tag: "Ident", name, scopes: [] }; }
function list(items: Syntax[]): Syntax { return { tag: "List", items, scopes: [] }; }
function atomNum(n: number): Syntax { return { tag: "Atom", value: n, scopes: [] }; }

describe("syntax-rules ellipses", () => {
  it("E1: (mylist x ...) -> (list x ...)", () => {
    const phaseOut = 0;
    const envDefOut: Env = []; // unbound literals ok for this test
    const literals: SIdent[] = [];
    const rule = {
      pat: list([id("mylist"), id("x"), id("...")]),
      tmpl: list([id("list"), id("x"), id("...")]),
    };
    const tr = compileSyntaxRules(phaseOut, envDefOut, literals, [rule]);

    const call = list([id("mylist"), atomNum(1), atomNum(2), atomNum(3)]);
    const out = applySyntaxRules(tr, call, envDefOut, { n: 0 });

    expect(out.tag).toBe("List");
    const items = (out as any).items as Syntax[];
    expect((items[0] as any).name).toBe("list");
    expect((items[1] as any).value).toBe(1);
    expect((items[2] as any).value).toBe(2);
    expect((items[3] as any).value).toBe(3);
  });

  it("E4: zip mismatch should throw (lockstep)", () => {
    const phaseOut = 0;
    const envDefOut: Env = [];
    const literals: SIdent[] = [];
    // (zip (a ...) (b ...)) -> (pairs (pair a b) ...)
    const rule = {
      pat: list([
        id("zip"),
        list([id("a"), id("...")]),
        list([id("b"), id("...")]),
      ]),
      tmpl: list([
        id("pairs"),
        list([id("pair"), id("a"), id("b")]),
        id("..."),
      ]),
    };
    const tr = compileSyntaxRules(phaseOut, envDefOut, literals, [rule]);

    const call = list([
      id("zip"),
      list([atomNum(1), atomNum(2)]),
      list([atomNum(3), atomNum(4), atomNum(5)]),
    ]);

    expect(() => applySyntaxRules(tr, call, envDefOut, { n: 0 })).toThrow();
  });
});