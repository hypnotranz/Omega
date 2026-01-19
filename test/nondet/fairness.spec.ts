// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-4.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import { describe, it, expect } from "vitest";
import { COWStore } from "../../src/core/eval/store";
import { createTestRuntime } from "../helpers/runtime";
import { installPrims } from "../helpers/prims";
import type { State } from "../../src/core/eval/machine";
import type { Expr } from "../../src/core/ast";
import { runNondet } from "../../src/core/effects/nondet/runner"; // from Part 15
import { VUnit } from "../../src/core/eval/values";

function initialState(expr: Expr): State {
  const store0 = new COWStore();
  const prim = installPrims(store0);

  return {
    control: { tag: "Expr", e: expr },
    env: prim.env,
    store: prim.store,
    kont: [],
    handlers: [],
  };
}

describe("nondet fairness (quantum scheduling)", () => {
  it("BFS/quantum should find 42 even if another branch diverges", async () => {
    // Program:
    // let x = amb(0,1)
    // if x == 0 then diverge else 42
    //
    // We encode amb as (Effect "amb.op" [Vector [0,1]]), then continuation uses x.
    //
    // NOTE: This assumes your language uses amb runner to interpret amb.op by forking at op boundaries.
    const diverge: Expr = {
      tag: "App",
      fn: { tag: "Lambda", params: ["x"], body: { tag: "App", fn: { tag: "Var", name: "x" }, args: [{ tag: "Var", name: "x" }] } },
      args: [{ tag: "Lambda", params: ["x"], body: { tag: "App", fn: { tag: "Var", name: "x" }, args: [{ tag: "Var", name: "x" }] } }],
    };

    const amb01: Expr = {
      tag: "Effect",
      op: "amb.op",
      args: [{ tag: "Quote", datum: ["vector", 0, 1] } as any], // simplest placeholder; if you have a real vector literal, use it
    };

    // Instead, directly use Effect args as already-evaluated Vector value by embedding it as Lit-like quote->vector.
    // If your Quote->datumToVal makes Vector, interpret ["vector",0,1] accordingly; else create Vector via primitives.

    const prog: Expr = {
      tag: "Begin",
      exprs: [
        { tag: "Define", name: "x", rhs: amb01 },
        {
          tag: "If",
          test: { tag: "App", fn: { tag: "Var", name: "=" }, args: [{ tag: "Var", name: "x" }, { tag: "Lit", value: 0 }] },
          conseq: diverge,
          alt: { tag: "Lit", value: 42 },
        },
      ],
    };

    const runtime = createTestRuntime();
    const st0 = initialState(prog);

    const res = await runNondet(runtime, st0, {
      mode: "first",
      frontier: "bfs",
      quantumSteps: 50,           // fairness quantum
      maxTotalSteps: 200_000,
      maxJobs: 10_000,
    });

    expect(res.tag).toBe("One");
    if (res.tag === "One") {
      expect(res.value.tag).toBe("Num");
      expect((res.value as any).n).toBe(42);
    }
  });
});