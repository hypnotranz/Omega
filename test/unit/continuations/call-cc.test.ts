import { describe, it, expect } from "vitest";
import { COWStore } from "../src/core/eval/store";
import type { State } from "../src/core/eval/machine";
import type { Val } from "../src/core/eval/values";
import { runToCompletion } from "../src/core/eval/run";
import { compileTextToExpr } from "../src/core/pipeline/compileText";
import { installPrims } from "../test/helpers/prims";
import { createTestRuntime } from "../test/helpers/runtime";

function initialState(src: string): State {
  const expr = compileTextToExpr(src);
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

async function evalOmega(src: string): Promise<Val> {
  const runtime = createTestRuntime();
  const st0 = initialState(src);
  return runToCompletion(runtime, st0);
}

function expectNum(v: Val): number {
  if (v.tag !== "Num") throw new Error(`expected Num, got ${v.tag}`);
  return v.n;
}

describe("call/cc primitives", () => {
  it("returns from continuation with provided value", async () => {
    const result = await evalOmega(`(call/cc (lambda (k) (+ 1 (k 5))))`);
    expectNum(result);
    expect(result).toMatchObject({ tag: "Num", n: 5 });
  });

  it("saves continuation and continues with normal return value", async () => {
    const result = await evalOmega(`
      (begin
        (define saved-k #f)
        (+ 1 (call/cc (lambda (k) (set! saved-k k) 10))))
    `);
    expectNum(result);
    expect(result).toMatchObject({ tag: "Num", n: 11 });
  });

  it("treats captured continuations as callable and usable with apply", async () => {
    const result = await evalOmega(`
      (begin
        (define saved #f)
        (define resumed? #f)
        (call/cc (lambda (k) (set! saved k) 0))
        (if resumed?
            7
            (begin
              (set! resumed? #t)
              (if (procedure? saved)
                  (apply saved (list 7))
                  'not-procedure))))
    `);
    expectNum(result);
    expect(result).toMatchObject({ tag: "Num", n: 7 });
  });

  it("allows invoking the same continuation multiple times", async () => {
    const result = await evalOmega(`
      (begin
        (define saved #f)
        (define counter 0)
        (call/cc (lambda (k) (set! saved k) 0))
        (set! counter (+ counter 1))
        (if (< counter 3)
            (saved counter)
            counter))
    `);
    expectNum(result);
    expect(result).toMatchObject({ tag: "Num", n: 3 });
  });
});
