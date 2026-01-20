import type { State, StepOutcome } from "../eval/machine";
import type { Val } from "../eval/values";
import { VFalse, VTrue } from "../eval/values";
import { envSet } from "../eval/env";
import { asString, asNumber, evalProcedureToValue, makeResultVal } from "./common";
import type { SolverVal, CostEstimateVal, ResultVal, BudgetVal, FactStoreVal } from "./types";
import { isSolver } from "./types";
import { makeBudget, budgetSplit, budgetAllocate, asBudget, budgetFromArgs } from "./budget";
import { composeSequential, composeParallel, composeFallback, composeRetry } from "./combinators";
import { repairUntilValid } from "./repair";
import { fixpoint, fixpointWithCycleDetection } from "./fixpoint";
import { createFactStore, assertFact, queryFact, queryFactsByPattern, asFactStore } from "./facts";

export type SolverPrimHelpers = {
  applyProcedure: (proc: Val, args: Val[], state: State) => State | StepOutcome;
  ensureArity: (proc: Val, expected: number, name: string) => void;
  isCallable: (proc: Val) => proc is Val;
  emitAmb: (choices: Val[], state: State) => StepOutcome;
};

function asSolver(val: Val): SolverVal {
  if (isSolver(val)) return val;
  throw new Error("expected solver");
}

function asList(v: Val): Val[] {
  const out: Val[] = [];
  let cur: Val = v;
  while (cur.tag === "Vector" && cur.items.length === 2) {
    out.push(cur.items[0]);
    cur = cur.items[1];
  }
  if (cur.tag !== "Unit") {
    throw new Error("expected proper list");
  }
  return out;
}

function listFromArray(items: Val[]): Val {
  let result: Val = { tag: "Unit" };
  for (let i = items.length - 1; i >= 0; i--) {
    result = { tag: "Vector", items: [items[i], result] };
  }
  return result;
}

function makeEstimate(minCost: number, maxCost: number, expectedCost: number, confidence: number): CostEstimateVal {
  return { tag: "CostEstimate", minCost, maxCost, expectedCost, confidence };
}

let resultThunkCounter = 0;
function wrapResultsAsThunks(results: ResultVal[], baseState: State): { thunks: Val[]; state: State } {
  let workingState = baseState;
  const thunks: Val[] = [];

  for (const res of results) {
    const binding = `__solver_result_${resultThunkCounter++}`;
    const [store2, addr] = workingState.store.alloc(res as Val);
    const envForThunk = envSet(workingState.env, binding, addr);
    const thunk: Val = { tag: "Closure", params: [], body: { tag: "Var", name: binding }, env: envForThunk };
    thunks.push(thunk);
    workingState = { ...workingState, store: store2 };
  }

  return { thunks, state: workingState };
}

export function registerSolverPrims(def: (name: string, v: Val) => void, helpers: SolverPrimHelpers): void {
  // make-solver: wraps callable solve/estimate into a Solver value
  def("make-solver", {
    tag: "Native",
    name: "make-solver",
    arity: 3,
    fn: (args, state) => {
      const [nameVal, solveFn, estimateFn] = args;
      if (!helpers.isCallable(solveFn)) throw new Error("solver solve function must be a procedure");
      if (!helpers.isCallable(estimateFn)) throw new Error("solver estimate function must be a procedure");
      const solver: SolverVal = {
        tag: "Solver",
        name: asString(nameVal, "solver name"),
        solve: (problem: Val, budget: BudgetVal, st: State) => {
          const { value, state: nextState } = evalProcedureToValue(solveFn, [problem, budget], st, helpers.applyProcedure);
          const result = (value as ResultVal).tag === "Result" ? (value as ResultVal) : makeResultVal("success", value, undefined, undefined, 0);
          return { results: [result], state: nextState };
        },
        estimate: (problem: Val, st: State) => {
          const { value, state: nextState } = evalProcedureToValue(estimateFn, [problem], st, helpers.applyProcedure);
          if ((value as any).tag !== "CostEstimate") {
            throw new Error("solver estimate must return CostEstimate");
          }
          return { estimate: value as CostEstimateVal, state: nextState };
        },
      };
      return { ...state, control: { tag: "Val", v: solver } };
    },
  });

  def("solver?", {
    tag: "Native",
    name: "solver?",
    arity: 1,
    fn: (args, state) => ({ ...state, control: { tag: "Val", v: isSolver(args[0]) ? VTrue : VFalse } }),
  });

  def("solver-name", {
    tag: "Native",
    name: "solver-name",
    arity: 1,
    fn: (args, state) => {
      const solver = asSolver(args[0]);
      return { ...state, control: { tag: "Val", v: { tag: "Sym", name: solver.name } } };
    },
  });

  def("solver-estimate", {
    tag: "Native",
    name: "solver-estimate",
    arity: 2,
    fn: (args, state) => {
      const solver = asSolver(args[0]);
      const { estimate } = solver.estimate(args[1], state, helpers.applyProcedure);
      return { ...state, control: { tag: "Val", v: estimate } };
    },
  });

  def("solver-solve", {
    tag: "Native",
    name: "solver-solve",
    arity: 3,
    fn: (args, state) => {
      const solver = asSolver(args[0]);
      const problem = args[1];
      const budget = asBudget(args[2]);
      const { results, state: nextState } = solver.solve(problem, budget, state, helpers.applyProcedure);
      if (results.length === 0) {
        const failure = makeResultVal("failure", undefined, undefined, "No results", 0);
        return { ...nextState, control: { tag: "Val", v: failure } };
      }
      if (results.length === 1) {
        return { ...nextState, control: { tag: "Val", v: results[0] } };
      }
      const { thunks, state: branchState } = wrapResultsAsThunks(results, nextState);
      return helpers.emitAmb(thunks, branchState);
    },
  });

  // Result / estimate constructors
  def("make-result", {
    tag: "Native",
    name: "make-result",
    arity: "variadic",
    fn: (args, state) => {
      if (args.length < 2) throw new Error("make-result: expected at least kind and solution");
      const kindVal = args[0];
      const kind = kindVal.tag === "Sym" ? kindVal.name : asString(kindVal, "result kind");
      const solution = args[1];
      let remaining: Val | undefined;
      let reason: string | undefined;
      let cost = 0;
      if (kind === "failure") {
        reason = args.length >= 3 ? asString(args[2], "reason") : undefined;
        cost = args.length >= 4 ? asNumber(args[3], "cost") : 0;
      } else if (kind === "partial") {
        remaining = args.length >= 3 ? args[2] : undefined;
        cost = args.length >= 4 ? asNumber(args[3], "cost") : 0;
      } else {
        cost = args.length >= 3 ? asNumber(args[2], "cost") : 0;
      }
      const res = makeResultVal(kind, solution, remaining, reason, cost);
      return { ...state, control: { tag: "Val", v: res } };
    },
  });

  def("make-estimate", {
    tag: "Native",
    name: "make-estimate",
    arity: 4,
    fn: (args, state) => {
      const est = makeEstimate(asNumber(args[0], "minCost"), asNumber(args[1], "maxCost"), asNumber(args[2], "expectedCost"), asNumber(args[3], "confidence"));
      return { ...state, control: { tag: "Val", v: est } };
    },
  });

  // Budget primitives
  def("make-budget", {
    tag: "Native",
    name: "make-budget",
    arity: 3,
    fn: (args, state) => ({ ...state, control: { tag: "Val", v: budgetFromArgs(args) } }),
  });

  def("budget-split", {
    tag: "Native",
    name: "budget-split",
    arity: 2,
    fn: (args, state) => {
      const budget = asBudget(args[0]);
      const n = asNumber(args[1], "parts");
      const parts = budgetSplit(budget, n);
      return { ...state, control: { tag: "Val", v: listFromArray(parts as Val[]) } };
    },
  });

  def("budget-allocate", {
    tag: "Native",
    name: "budget-allocate",
    arity: 2,
    fn: (args, state) => {
      const budget = asBudget(args[0]);
      const weights = asList(args[1]).map(v => asNumber(v, "weight"));
      const parts = budgetAllocate(budget, weights);
      return { ...state, control: { tag: "Val", v: listFromArray(parts as Val[]) } };
    },
  });

  // Combinators
  def("compose-sequential", {
    tag: "Native",
    name: "compose-sequential",
    arity: 1,
    fn: (args, state) => {
      const solvers = asList(args[0]).map(asSolver);
      const composed = composeSequential(solvers, helpers.applyProcedure);
      return { ...state, control: { tag: "Val", v: composed } };
    },
  });

  def("compose-parallel", {
    tag: "Native",
    name: "compose-parallel",
    arity: 1,
    fn: (args, state) => {
      const solvers = asList(args[0]).map(asSolver);
      const composed = composeParallel(solvers, helpers.applyProcedure);
      return { ...state, control: { tag: "Val", v: composed } };
    },
  });

  def("compose-fallback", {
    tag: "Native",
    name: "compose-fallback",
    arity: 1,
    fn: (args, state) => {
      const solvers = asList(args[0]).map(asSolver);
      const composed = composeFallback(solvers, helpers.applyProcedure);
      return { ...state, control: { tag: "Val", v: composed } };
    },
  });

  def("compose-retry", {
    tag: "Native",
    name: "compose-retry",
    arity: 2,
    fn: (args, state) => {
      const solver = asSolver(args[0]);
      const retries = asNumber(args[1], "maxRetries");
      const composed = composeRetry(solver, retries, helpers.applyProcedure);
      return { ...state, control: { tag: "Val", v: composed } };
    },
  });

  // Repair / fixpoint
  def("repair-until-valid", {
    tag: "Native",
    name: "repair-until-valid",
    arity: 4,
    fn: (args, state) => {
      const [initial, validator, repairFn, maxIterVal] = args;
      if (!helpers.isCallable(validator)) throw new Error("Validator must be a procedure");
      if (!helpers.isCallable(repairFn)) throw new Error("Repair function must be a procedure");
      const maxIter = asNumber(maxIterVal, "maxIterations");
      const result = repairUntilValid(initial, validator, repairFn, maxIter, state, helpers.applyProcedure);
      return { ...state, control: { tag: "Val", v: result } };
    },
  });

  def("fixpoint", {
    tag: "Native",
    name: "fixpoint",
    arity: 4,
    fn: (args, state) => {
      const [initial, stepFn, equalFn, maxIterVal] = args;
      if (!helpers.isCallable(stepFn)) throw new Error("Step function must be a procedure");
      if (!helpers.isCallable(equalFn)) throw new Error("Equality function must be a procedure");
      const maxIter = asNumber(maxIterVal, "maxIterations");
      const result = fixpoint(initial, stepFn, equalFn, maxIter, state, helpers.applyProcedure);
      return { ...state, control: { tag: "Val", v: result } };
    },
  });

  def("fixpoint-detect-cycle", {
    tag: "Native",
    name: "fixpoint-detect-cycle",
    arity: 4,
    fn: (args, state) => {
      const [initial, stepFn, hashFn, maxIterVal] = args;
      if (!helpers.isCallable(stepFn)) throw new Error("Step function must be a procedure");
      if (!helpers.isCallable(hashFn)) throw new Error("Hash function must be a procedure");
      const maxIter = asNumber(maxIterVal, "maxIterations");
      const result = fixpointWithCycleDetection(initial, stepFn, hashFn, maxIter, state, helpers.applyProcedure);
      return { ...state, control: { tag: "Val", v: result } };
    },
  });

  // Fact store
  def("make-fact-store", {
    tag: "Native",
    name: "make-fact-store",
    arity: 0,
    fn: (_args, state) => ({ ...state, control: { tag: "Val", v: createFactStore() as Val } }),
  });

  def("fact-store?", {
    tag: "Native",
    name: "fact-store?",
    arity: 1,
    fn: (args, state) => ({ ...state, control: { tag: "Val", v: (args[0] as FactStoreVal)?.tag === "FactStore" ? VTrue : VFalse } }),
  });

  def("assert-fact", {
    tag: "Native",
    name: "assert-fact",
    arity: 3,
    fn: (args, state) => {
      const store = asFactStore(args[0]);
      const key = asString(args[1], "key");
      const value = args[2];
      return { ...state, control: { tag: "Val", v: assertFact(store, key, value) as Val } };
    },
  });

  def("query-fact", {
    tag: "Native",
    name: "query-fact",
    arity: 2,
    fn: (args, state) => {
      const store = asFactStore(args[0]);
      const key = asString(args[1], "key");
      const result = queryFact(store, key);
      return { ...state, control: { tag: "Val", v: result ?? VFalse } };
    },
  });

  def("query-facts", {
    tag: "Native",
    name: "query-facts",
    arity: 2,
    fn: (args, state) => {
      const store = asFactStore(args[0]);
      const pattern = asString(args[1], "pattern");
      const results = queryFactsByPattern(store, pattern);
      const pairs = results.map(([k, v]) => listFromArray([{ tag: "Str", s: k } as Val, v]));
      return { ...state, control: { tag: "Val", v: listFromArray(pairs as Val[]) } };
    },
  });
}
