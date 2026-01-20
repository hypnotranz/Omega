import { describe, it, expect } from "vitest";
import { evalAll, evalOne, listToArray, expectBool, expectNum, expectSym, asResult } from "./utils";
import type { Val } from "../../src/core/eval/values";

function resultSolutions(values: Val[]): Val[] {
  return values.map(v => asResult(v).solution!).filter(Boolean);
}

describe("solver interface", () => {
  it("creates a solver and exposes name and predicate", async () => {
    const result = await evalOne(`
      (begin
        (define s (make-solver "identity"
                    (lambda (p b) (unit (make-result 'success p 1)))
                    (lambda (p) (make-estimate 1 1 1 1))))
        (list (solver? s) (solver-name s)))
    `);
    const [isSolver, nameVal] = listToArray(result);
    expect(expectBool(isSolver)).toBe(true);
    expect(expectSym(nameVal)).toBe("identity");
  });

  it("runs a solver and returns a success result", async () => {
    const values = await evalAll(`
      (begin
        (define s (make-solver "double"
                    (lambda (p b) (unit (make-result 'success (* p 2) 1)))
                    (lambda (p) (make-estimate 1 1 1 1))))
        (solver-solve s 5 (make-budget 10 1 10)))
    `);
    expect(values).toHaveLength(1);
    const res = asResult(values[0]);
    expect(res.kind).toBe("success");
    expect(expectNum(res.solution!)).toBe(10);
    expect(res.cost).toBe(1);
  });
});

describe("compose-sequential", () => {
  it("threads solutions through the pipeline", async () => {
    const values = await evalAll(`
      (begin
        (define add1 (make-solver "add1"
                        (lambda (p b) (unit (make-result 'success (+ p 1) 1)))
                        (lambda (p) (make-estimate 1 1 1 1))))
        (define mult2 (make-solver "mult2"
                        (lambda (p b) (unit (make-result 'success (* p 2) 1)))
                        (lambda (p) (make-estimate 1 1 1 1))))
        (define seq (compose-sequential (list add1 mult2)))
        (solver-solve seq 5 (make-budget 100 10 1000)))
    `);
    expect(values).toHaveLength(1);
    const res = asResult(values[0]);
    expect(res.kind).toBe("success");
    expect(expectNum(res.solution!)).toBe(12);
  });

  it("fails if an early stage fails", async () => {
    const values = await evalAll(`
      (begin
        (define failer (make-solver "fail"
                        (lambda (p b) (unit (make-result 'failure #f "oops" 1)))
                        (lambda (p) (make-estimate 1 1 1 1))))
        (define add1 (make-solver "add1"
                        (lambda (p b) (unit (make-result 'success (+ p 1) 1)))
                        (lambda (p) (make-estimate 1 1 1 1))))
        (define seq (compose-sequential (list failer add1)))
        (solver-solve seq 5 (make-budget 50 5 50)))
    `);
    expect(values).toHaveLength(1);
    const res = asResult(values[0]);
    expect(res.kind).toBe("failure");
    expect(res.reason).toMatch(/Stage 0/);
  });

  it("acts as identity with no solvers", async () => {
    const values = await evalAll(`(solver-solve (compose-sequential (list)) 42 (make-budget 1 1 1))`);
    expect(values).toHaveLength(1);
    const res = asResult(values[0]);
    expect(res.kind).toBe("success");
    expect(expectNum(res.solution!)).toBe(42);
  });
});

describe("compose-parallel", () => {
  it("returns results from all solvers", async () => {
    const values = await evalAll(`
      (begin
        (define a (make-solver "a" (lambda (p b) (unit (make-result 'success 'a 1))) (lambda (p) (make-estimate 1 1 1 1))))
        (define b (make-solver "b" (lambda (p b) (unit (make-result 'success 'b 1))) (lambda (p) (make-estimate 1 1 1 1))))
        (define par (compose-parallel (list a b)))
        (solver-solve par 'x (make-budget 10 2 10)))
    `);
    const sols = resultSolutions(values).map(expectSym);
    expect(sols.sort()).toEqual(["a", "b"]);
  });

  it("propagates failure results too", async () => {
    const values = await evalAll(`
      (begin
        (define good (make-solver "good" (lambda (p b) (unit (make-result 'success 'ok 1))) (lambda (p) (make-estimate 1 1 1 1))))
        (define bad (make-solver "bad" (lambda (p b) (unit (make-result 'failure #f "nope" 1))) (lambda (p) (make-estimate 1 1 1 1))))
        (define par (compose-parallel (list good bad)))
        (solver-solve par 'x (make-budget 10 2 10)))
    `);
    const kinds = values.map(v => asResult(v).kind).sort();
    expect(kinds).toEqual(["failure", "success"]);
  });
});

describe("compose-fallback", () => {
  it("returns the first successful result", async () => {
    const values = await evalAll(`
      (begin
        (define f1 (make-solver "fail"
                     (lambda (p b) (unit (make-result 'failure #f "nope" 1)))
                     (lambda (p) (make-estimate 1 1 1 1))))
        (define f2 (make-solver "succ"
                     (lambda (p b) (unit (make-result 'success 'yes 1)))
                     (lambda (p) (make-estimate 1 1 1 1))))
        (define fb (compose-fallback (list f1 f2)))
        (solver-solve fb 'q (make-budget 10 2 10)))
    `);
    expect(values).toHaveLength(1);
    const res = asResult(values[0]);
    expect(res.kind).toBe("success");
    expect(expectSym(res.solution!)).toBe("yes");
  });

  it("fails if all solvers fail", async () => {
    const values = await evalAll(`
      (begin
        (define a (make-solver "a" (lambda (p b) (unit (make-result 'failure #f "a" 1))) (lambda (p) (make-estimate 1 1 1 1))))
        (define b (make-solver "b" (lambda (p b) (unit (make-result 'failure #f "b" 1))) (lambda (p) (make-estimate 1 1 1 1))))
        (define fb (compose-fallback (list a b)))
        (solver-solve fb 'x (make-budget 10 2 10)))
    `);
    expect(values).toHaveLength(1);
    const res = asResult(values[0]);
    expect(res.kind).toBe("failure");
    expect(res.reason).toMatch(/fallback/i);
  });
});

describe("compose-retry", () => {
  it("succeeds on a later attempt", async () => {
    const values = await evalAll(`
      (begin
        (define attempt 0)
        (define maybe (make-solver "eventual"
                        (lambda (p b)
                          (set! attempt (+ attempt 1))
                          (if (>= attempt 3)
                              (unit (make-result 'success 'finally attempt))
                              (unit (make-result 'failure #f "not yet" attempt))))
                        (lambda (p) (make-estimate 1 1 1 1))))
        (define retried (compose-retry maybe 5))
        (solver-solve retried 'x (make-budget 20 5 20)))
    `);
    expect(values).toHaveLength(1);
    const res = asResult(values[0]);
    expect(res.kind).toBe("success");
    expect(expectSym(res.solution!)).toBe("finally");
  });

  it("fails after exhausting retries", async () => {
    const values = await evalAll(`
      (begin
        (define always (make-solver "fail"
                        (lambda (p b) (unit (make-result 'failure #f "nope" 1)))
                        (lambda (p) (make-estimate 1 1 1 1))))
        (define retried (compose-retry always 2))
        (solver-solve retried 'x (make-budget 10 2 10)))
    `);
    expect(values).toHaveLength(1);
    const res = asResult(values[0]);
    expect(res.kind).toBe("failure");
    expect(res.reason).toMatch(/Failed after 2 retries/);
  });
});
