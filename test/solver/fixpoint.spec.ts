import { describe, it, expect } from "vitest";
import { evalOne, expectNum, expectBool, asResult } from "./utils";

describe("fixpoint", () => {
  it("detects immediate convergence", async () => {
    const value = await evalOne(`(fixpoint 10 (lambda (x) x) (lambda (a b) (equal? a b)) 5)`);
    const res = asResult(value);
    expect(res.kind).toBe("success");
    expect(expectNum(res.solution!)).toBe(10);
    expect(res.cost).toBe(1);
  });

  it("converges after several steps", async () => {
    const value = await evalOne(`
      (fixpoint
        1
        (lambda (x) (if (< x 5) (+ x 1) x))
        (lambda (a b) (equal? a b))
        10)
    `);
    const res = asResult(value);
    expect(res.kind).toBe("success");
    expect(expectNum(res.solution!)).toBe(5);
    expect(res.cost).toBe(5);
  });

  it("returns partial when fuel runs out", async () => {
    const value = await evalOne(`
      (fixpoint
        0
        (lambda (x) (+ x 1))
        (lambda (a b) (equal? a b))
        2)
    `);
    const res = asResult(value);
    expect(res.kind).toBe("partial");
    expect(expectNum(res.solution!)).toBe(2);
    expect(res.reason).toMatch(/Did not converge/);
  });
});

describe("fixpoint-detect-cycle", () => {
  it("reports a cycle when hashes repeat", async () => {
    const value = await evalOne(`
      (fixpoint-detect-cycle
        'a
        (lambda (x) (if (equal? x 'a) 'b (if (equal? x 'b) 'c 'a)))
        (lambda (x) (if (equal? x 'a) "a" (if (equal? x 'b) "b" "c")))
        10)
    `);
    const res = asResult(value);
    expect(res.kind).toBe("failure");
    expect(res.reason).toMatch(/Cycle detected/);
  });

  it("allows convergence without cycles", async () => {
    const value = await evalOne(`
      (fixpoint-detect-cycle
        3
        (lambda (x) (if (> x 0) (- x 1) x))
        (lambda (x) (if (equal? x 0) "0" (if (equal? x 1) "1" (if (equal? x 2) "2" "3"))))
        10)
    `);
    const res = asResult(value);
    expect(res.kind).toBe("success");
    expect(expectNum(res.solution!)).toBe(0);
  });
});
