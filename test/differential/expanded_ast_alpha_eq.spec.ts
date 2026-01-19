// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-4.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import { describe, it, expect } from "vitest";
import { alphaEqual } from "../../src/core/syntax/alpha"; // Part 15
import type { Expr } from "../../src/core/ast";

// Assumed API:
//   expandWithHost(src)->Expr
//   expandWithBoot(src)->Expr
import { expandWithHost, expandWithBoot } from "../helpers/differentialHarness";

describe("differential: host expander vs boot expander", () => {
  it("D1: expanded core AST alpha-equal", async () => {
    const src = `(begin (define (f x) (+ x 1)) (f 41))`;
    const a: Expr = await expandWithHost(src);
    const b: Expr = await expandWithBoot(src);
    expect(alphaEqual(a, b)).toBe(true);
  });
});