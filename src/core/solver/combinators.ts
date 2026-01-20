import type { SolverVal, BudgetVal, ResultVal, CostEstimateVal } from "./types";
import type { Val } from "../eval/values";
import type { State } from "../eval/machine";
import type { ApplyFn } from "./common";
import { budgetSplit } from "./budget";
import { makeResultVal } from "./common";

type SolverRun = { results: ResultVal[]; state: State };

function firstResult(run: SolverRun): ResultVal | undefined {
  return run.results[0];
}

function cloneState(state: State): State {
  return { ...state, store: state.store.snapshot() };
}

function defaultEstimate(name: string): CostEstimateVal {
  return { tag: "CostEstimate", minCost: 0, maxCost: 0, expectedCost: 0, confidence: 1 };
}

export function composeSequential(solvers: SolverVal[], apply: ApplyFn): SolverVal {
  const name = `sequential(${solvers.map(s => s.name).join(", ")})`;
  return {
    tag: "Solver",
    name,
    solve: (problem: Val, budget: BudgetVal, state: State): SolverRun => {
      if (solvers.length === 0) {
        return { results: [makeResultVal("success", problem, undefined, undefined, 0)], state };
      }
      const parts = budgetSplit(budget, solvers.length);
      let current = problem;
      let currentState = state;

      for (let i = 0; i < solvers.length; i++) {
        const run = solvers[i].solve(current, parts[i], currentState, apply);
        const res = firstResult(run);
        currentState = run.state;
        if (!res || res.kind === "failure") {
          return { results: [makeResultVal("failure", undefined, undefined, `Stage ${i} failed`, 0)], state: currentState };
        }
        current = res.solution ?? current;
      }

      return { results: [makeResultVal("success", current, undefined, undefined, 0)], state: currentState };
    },
    estimate: (problem: Val, state: State): { estimate: CostEstimateVal; state: State } => {
      let min = 0, max = 0, exp = 0, conf = 1;
      for (const solver of solvers) {
        const { estimate } = solver.estimate(problem, state, apply);
        min += estimate.minCost;
        max += estimate.maxCost;
        exp += estimate.expectedCost;
        conf *= estimate.confidence;
      }
      return { estimate: { tag: "CostEstimate", minCost: min, maxCost: max, expectedCost: exp, confidence: conf }, state };
    }
  };
}

export function composeParallel(solvers: SolverVal[], apply: ApplyFn): SolverVal {
  const name = `parallel(${solvers.map(s => s.name).join(", ")})`;
  return {
    tag: "Solver",
    name,
    solve: (problem: Val, budget: BudgetVal, state: State): SolverRun => {
      if (solvers.length === 0) return { results: [], state };
      const parts = budgetSplit(budget, solvers.length);
      const all: ResultVal[] = [];
      for (let i = 0; i < solvers.length; i++) {
        const childState = cloneState(state);
        const run = solvers[i].solve(problem, parts[i], childState, apply);
        all.push(...run.results);
      }
      return { results: all, state };
    },
    estimate: (problem: Val, state: State): { estimate: CostEstimateVal; state: State } => {
      if (solvers.length === 0) return { estimate: defaultEstimate(name), state };
      const estimates = solvers.map(s => s.estimate(problem, state, apply).estimate);
      return {
        estimate: {
          tag: "CostEstimate",
          minCost: Math.min(...estimates.map(e => e.minCost)),
          maxCost: Math.max(...estimates.map(e => e.maxCost)),
          expectedCost: estimates.reduce((a, e) => a + e.expectedCost, 0) / estimates.length,
          confidence: Math.max(...estimates.map(e => e.confidence)),
        },
        state,
      };
    }
  };
}

export function composeFallback(solvers: SolverVal[], apply: ApplyFn): SolverVal {
  const name = `fallback(${solvers.map(s => s.name).join(", ")})`;
  return {
    tag: "Solver",
    name,
    solve: (problem: Val, budget: BudgetVal, state: State): SolverRun => {
      const parts = budgetSplit(budget, Math.max(solvers.length, 1));
      let currentState = state;
      for (let i = 0; i < solvers.length; i++) {
        const run = solvers[i].solve(problem, parts[i], currentState, apply);
        const res = firstResult(run);
        currentState = run.state;
        if (res && res.kind !== "failure") {
          return { results: [res], state: currentState };
        }
      }
      return { results: [makeResultVal("failure", undefined, undefined, "All fallbacks failed", 0)], state: currentState };
    },
    estimate: (problem: Val, state: State): { estimate: CostEstimateVal; state: State } => {
      if (solvers.length === 0) return { estimate: defaultEstimate(name), state };
      return solvers[0].estimate(problem, state, apply);
    }
  };
}

export function composeRetry(solver: SolverVal, maxRetries: number, apply: ApplyFn): SolverVal {
  const name = `retry(${solver.name}, ${maxRetries})`;
  return {
    tag: "Solver",
    name,
    solve: (problem: Val, budget: BudgetVal, state: State): SolverRun => {
      const parts = budgetSplit(budget, Math.max(maxRetries, 1));
      let currentState = state;
      for (let i = 0; i < maxRetries; i++) {
        const run = solver.solve(problem, parts[i], currentState, apply);
        const res = firstResult(run);
        currentState = run.state;
        if (res && res.kind !== "failure") {
          return { results: [res], state: currentState };
        }
      }
      return { results: [makeResultVal("failure", undefined, undefined, `Failed after ${maxRetries} retries`, 0)], state: currentState };
    },
    estimate: (problem: Val, state: State): { estimate: CostEstimateVal; state: State } => {
      const { estimate } = solver.estimate(problem, state, apply);
      return { estimate: { ...estimate, maxCost: estimate.maxCost * Math.max(1, maxRetries) }, state };
    }
  };
}
