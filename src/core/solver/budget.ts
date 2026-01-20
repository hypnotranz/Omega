import type { BudgetVal } from "./types";
import { asNumber } from "./common";

export function makeBudget(tokens: number, calls: number, time: number): BudgetVal {
  return { tag: "Budget", tokens, calls, time };
}

export function asBudget(v: any): BudgetVal {
  if (v && v.tag === "Budget") return v as BudgetVal;
  throw new Error("expected Budget");
}

export function budgetSplit(budget: BudgetVal, n: number): BudgetVal[] {
  if (n <= 0) {
    throw new Error("Cannot split into zero or negative parts");
  }
  const tokensEach = Math.floor(budget.tokens / n);
  const callsEach = Math.floor(budget.calls / n);
  const timeEach = Math.floor(budget.time / n);
  return Array.from({ length: n }, () => ({
    tag: "Budget",
    tokens: tokensEach,
    calls: callsEach,
    time: timeEach,
  }));
}

export function budgetAllocate(budget: BudgetVal, weights: number[]): BudgetVal[] {
  if (weights.length === 0) {
    throw new Error("Weights list cannot be empty");
  }
  const total = weights.reduce((a, b) => a + b, 0);
  if (total <= 0) {
    throw new Error("Weights must sum to a positive value");
  }

  return weights.map(w => ({
    tag: "Budget",
    tokens: Math.floor(budget.tokens * (w / total)),
    calls: Math.floor(budget.calls * (w / total)),
    time: Math.floor(budget.time * (w / total)),
  }));
}

export function budgetFromArgs(args: any[]): BudgetVal {
  const tokens = asNumber(args[0], "tokens");
  const calls = asNumber(args[1], "calls");
  const time = asNumber(args[2], "time");
  return makeBudget(tokens, calls, time);
}
