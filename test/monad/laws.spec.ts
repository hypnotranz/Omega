import { describe, it, expect } from "vitest";
import type { Val } from "../../src/core/eval/values";
import { evalAll, valToJs } from "./utils";

function toJs(vals: Val[]): unknown[] {
  return vals.map(valToJs);
}

describe("monad laws for nondeterminism", () => {
  it("LAW-1: left identity - bind (unit x) f == f x", async () => {
    const f = "(lambda (n) (unit (* n 2)))";
    const left = await evalAll(`(bind (unit 5) ${f})`);
    const right = await evalAll(`(${f} 5)`);
    expect(toJs(left)).toEqual(toJs(right));
  });

  it("LAW-2: right identity - bind m unit == m", async () => {
    const m = "(mplus (unit 1) (unit 2) (unit 3))";
    const left = await evalAll(`(bind ${m} unit)`);
    const right = await evalAll(m);
    expect(toJs(left)).toEqual(toJs(right));
  });

  it("LAW-3: associativity", async () => {
    const m = "(mplus (unit 1) (unit 2))";
    const f = "(lambda (x) (mplus (unit x) (unit (* x 10))))";
    const g = "(lambda (x) (unit (+ x 1)))";

    const left = await evalAll(`(bind (bind ${m} ${f}) ${g})`);
    const right = await evalAll(`(bind ${m} (lambda (x) (bind (${f} x) ${g})))`);
    expect(toJs(left)).toEqual(toJs(right));
  });

  it("LAW-4: mzero is left zero for bind", async () => {
    const f = "(lambda (x) (unit (* x 2)))";
    const values = await evalAll(`(bind (mzero) ${f})`);
    expect(values).toHaveLength(0);
  });

  it("LAW-5: mzero is right zero for bind", async () => {
    const m = "(mplus (unit 1) (unit 2))";
    const values = await evalAll(`(bind ${m} (lambda (_) (mzero)))`);
    expect(values).toHaveLength(0);
  });
});
