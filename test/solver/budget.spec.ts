import { describe, it, expect } from "vitest";
import { evalOne, listToArray, asBudget } from "./utils";

describe("solver budgets", () => {
  it("splits budget evenly across parts", async () => {
    const value = await evalOne("(budget-split (make-budget 100 10 1000) 4)");
    const parts = listToArray(value).map(asBudget);
    expect(parts).toHaveLength(4);
    expect(parts.map(p => p.tokens)).toEqual([25, 25, 25, 25]);
    expect(parts.map(p => p.calls)).toEqual([2, 2, 2, 2]);
  });

  it("allocates budget according to weights", async () => {
    const value = await evalOne("(budget-allocate (make-budget 100 10 1000) (list 1 2 1))");
    const parts = listToArray(value).map(asBudget);
    expect(parts.map(p => p.tokens)).toEqual([25, 50, 25]);
    expect(parts.map(p => p.calls)).toEqual([2, 5, 2]);
  });

  it("preserves total tokens within rounding loss", async () => {
    const value = await evalOne("(budget-split (make-budget 101 10 1000) 3)");
    const parts = listToArray(value).map(asBudget);
    const total = parts.reduce((sum, p) => sum + p.tokens, 0);
    expect(total).toBeLessThanOrEqual(101);
    expect(total).toBeGreaterThanOrEqual(99);
  });

  it("supports uneven small splits", async () => {
    const value = await evalOne("(budget-split (make-budget 3 1 10) 5)");
    const parts = listToArray(value).map(asBudget);
    expect(parts).toHaveLength(5);
    expect(parts.every(p => p.tokens === 0)).toBe(true);
    expect(parts.every(p => p.calls === 0)).toBe(true);
  });

  it("rejects zero or negative splits", async () => {
    await expect(evalOne("(budget-split (make-budget 100 10 1000) 0)")).rejects.toThrow();
    await expect(evalOne("(budget-split (make-budget 100 10 1000) -1)")).rejects.toThrow();
  });

  it("rejects empty or zero weights for allocation", async () => {
    await expect(evalOne("(budget-allocate (make-budget 100 10 1000) (list))")).rejects.toThrow();
    await expect(evalOne("(budget-allocate (make-budget 100 10 1000) (list 0 0))")).rejects.toThrow();
  });
});
