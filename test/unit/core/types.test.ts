import { describe, it, expectTypeOf } from "vitest";
import type { Expr } from "../src/core/ast";
import type { Val, IntVal, ListVal } from "../src/core/eval/values";

describe("AST compatibility", () => {
  it("supports let/letrec/quote-syntax shapes", () => {
    const letExpr: Expr = {
      tag: "Let",
      bindings: [{ name: "x", init: { tag: "Lit", value: 1 } }],
      body: { tag: "Var", name: "x" },
    };

    const letrecExpr: Expr = {
      tag: "Letrec",
      bindings: [{ name: "fact", init: { tag: "Var", name: "fact" } }],
      body: { tag: "Var", name: "fact" },
    };

    const quoteSyntax: Expr = {
      tag: "QuoteSyntax",
      datum: { tag: "Sym", name: "x" } as unknown,
    };

    expectTypeOf(letExpr).toMatchTypeOf<Expr>();
    expectTypeOf(letrecExpr).toMatchTypeOf<Expr>();
    expectTypeOf(quoteSyntax).toMatchTypeOf<Expr>();
  });
});

describe("Value compatibility", () => {
  it("supports Int and List tagged values used by stream analysis", () => {
    const intVal: IntVal = { tag: "Int", value: 1n };
    const listVal: ListVal = { tag: "List", elements: [intVal] };

    expectTypeOf(intVal).toMatchTypeOf<Val>();
    expectTypeOf(listVal.elements[0]).toMatchTypeOf<Val>();
  });
});
