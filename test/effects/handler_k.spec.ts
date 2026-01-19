// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-4.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import { describe, it, expect } from "vitest";
import { COWStore } from "../../src/core/eval/store";
import { createTestRuntime } from "../helpers/runtime";
import { installPrims } from "../helpers/prims";
import type { State } from "../../src/core/eval/machine";
import type { Expr } from "../../src/core/ast";
import { runToCompletion } from "../../src/core/eval/run";

function initial(expr: Expr): State {
  const store0 = new COWStore();
  const prim = installPrims(store0);
  return { control: { tag: "Expr", e: expr }, env: prim.env, store: prim.store, kont: [], handlers: [] };
}

describe("deep handler semantics: k is a delimited resumption returning to caller", () => {
  it("k(v) should resume the suspended continuation and return its result to the handler clause", async () => {
    // handle {
    //   let y = effect foo(10)
    //   y + 1
    // } with {
    //   on foo(x,k) -> (k (x + 5)) + 100
    //   return v -> v
    // }
    //
    // Expected:
    //   effect foo(10) handled:
    //     k(15) resumes to compute y+1 = 16, returns 16 to clause
    //     clause adds 100 => 116
    //   boundary return returns 116
    const prog: Expr = {
      tag: "Handle",
      body: {
        tag: "Begin",
        exprs: [
          { tag: "Define", name: "y", rhs: { tag: "Effect", op: "foo", args: [{ tag: "Lit", value: 10 }] } },
          { tag: "App", fn: { tag: "Var", name: "+" }, args: [{ tag: "Var", name: "y" }, { tag: "Lit", value: 1 }] },
        ],
      },
      handler: {
        on: [
          {
            op: "foo",
            params: ["x"],
            k: "k",
            body: {
              tag: "App",
              fn: { tag: "Var", name: "+" },
              args: [
                { tag: "App", fn: { tag: "Var", name: "k" }, args: [
                  { tag: "App", fn: { tag: "Var", name: "+" }, args: [{ tag: "Var", name: "x" }, { tag: "Lit", value: 5 }] }
                ]},
                { tag: "Lit", value: 100 }
              ],
            },
          },
        ],
        ret: { v: "v", body: { tag: "Var", name: "v" } },
      },
    };

    const runtime = createTestRuntime();
    const v = await runToCompletion(runtime, initial(prog), 200_000);

    expect(v.tag).toBe("Num");
    expect((v as any).n).toBe(116);
  });
});