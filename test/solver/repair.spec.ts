import { describe, it, expect } from "vitest";
import { evalOne, expectNum, expectSym, asResult } from "./utils";

describe("repair-until-valid", () => {
  it("returns immediately when validator succeeds", async () => {
    const value = await evalOne(`(repair-until-valid 'ok (lambda (x) #t) (lambda (x) x) 5)`);
    const res = asResult(value);
    expect(res.kind).toBe("success");
    expect(expectSym(res.solution!)).toBe("ok");
    expect(res.cost).toBe(0);
  });

  it("iterates repairs until valid", async () => {
    const value = await evalOne(`
      (repair-until-valid
        0
        (lambda (x) (>= x 3))
        (lambda (x) (+ x 1))
        10)
    `);
    const res = asResult(value);
    expect(res.kind).toBe("success");
    expect(expectNum(res.solution!)).toBe(3);
    expect(res.cost).toBe(3);
  });

  it("returns partial when iterations are exhausted", async () => {
    const value = await evalOne(`
      (repair-until-valid
        0
        (lambda (x) (>= x 100))
        (lambda (x) (+ x 1))
        5)
    `);
    const res = asResult(value);
    expect(res.kind).toBe("partial");
    expect(expectNum(res.solution!)).toBe(5);
    expect(res.reason).toMatch(/Max iterations/);
  });

  it("propagates failure from repair function", async () => {
    const value = await evalOne(`
      (repair-until-valid
        'x
        (lambda (x) #f)
        (lambda (x) (make-result 'failure #f "can't repair" 1))
        3)
    `);
    const res = asResult(value);
    expect(res.kind).toBe("failure");
    expect(res.reason).toMatch(/can't repair/);
  });
});
