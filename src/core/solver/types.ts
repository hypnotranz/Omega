import type { Val, SolverVal, ResultVal, CostEstimateVal, BudgetVal, FactStoreVal } from "../eval/values";

export type { SolverVal, ResultVal, CostEstimateVal, BudgetVal, FactStoreVal } from "../eval/values";

export function isSolver(val: Val): val is SolverVal {
  return (val as any)?.tag === "Solver";
}

export function isResult(val: Val): val is ResultVal {
  return (val as any)?.tag === "Result";
}

export function isBudget(val: Val): val is BudgetVal {
  return (val as any)?.tag === "Budget";
}

export function isFactStore(val: Val): val is FactStoreVal {
  return (val as any)?.tag === "FactStore";
}
