# JOB-008: Search Patterns & Composable Solver Interface (Phase C)

**Priority**: P2 - Nice to Have (Phase C)
**Estimated Effort**: 3-5 days
**Skills Required**: TypeScript, Search algorithms, Solver architecture
**Status**: DONE

> **MUST READ**: [LOGISTICS.md](LOGISTICS.md) before starting - covers testing, file locations, and proof of completion requirements.

### Quick Start

```bash
cd c:\Users\Richa\parmenides-dev\agent-harness\OmegaLLM

# Verify prerequisites are complete first
npx vitest run test/conditions/   # Job 005 must pass
npx vitest run test/monad/        # Job 006 must pass

# Run existing tests (verify baseline)
npm run test

# After implementation, run new tests
npx vitest run test/solver/

# Watch mode during development
npm run test:watch
```

### Output Files

| File | Action | Package |
|------|--------|---------|
| `src/core/solver/types.ts` | **CREATE** - SolverVal, ResultVal, CostEstimateVal | `@omega/solver` |
| `src/core/solver/combinators.ts` | **CREATE** - compose-sequential, parallel, fallback, retry | `@omega/solver` |
| `src/core/solver/repair.ts` | **CREATE** - repair-until-valid | `@omega/solver` |
| `src/core/solver/fixpoint.ts` | **CREATE** - fixpoint, fixpoint-detect-cycle | `@omega/solver` |
| `src/core/solver/facts.ts` | **CREATE** - FactStore, assert-fact, query-fact | `@omega/solver` |
| `src/core/solver/budget.ts` | **CREATE** - budget-split, budget-allocate | `@omega/solver` |
| `src/core/solver/prims.ts` | **CREATE** - All solver primitives | `@omega/solver` |
| `src/core/prims.ts` | Register solver primitives | `@omega/core` |
| `test/solver/combinators.spec.ts` | **CREATE** - combinator tests | test |
| `test/solver/repair.spec.ts` | **CREATE** - repair loop tests | test |
| `test/solver/fixpoint.spec.ts` | **CREATE** - fixpoint tests | test |
| `test/solver/facts.spec.ts` | **CREATE** - fact store tests | test |

### LambdaRLM Reference

| LambdaRLM Job | Relevance |
|---------------|-----------|
| [05-COMPOSABLE.md](../../LambdaRLM/CLAUDE-JOBS/05-COMPOSABLE.md) | **DIRECT EQUIVALENT** - solver composition |
| [01-BUDGET.md](../../LambdaRLM/CLAUDE-JOBS/01-BUDGET.md) | Budget primitives (budget-split, budget-allocate) |
| [09-STRATEGIES.md](../../LambdaRLM/CLAUDE-JOBS/09-STRATEGIES.md) | Strategy patterns |

---

## Executive Summary

Port the **strategic problem-solving patterns** from LambdaRLM:

1. **Composable Solvers** - Combine solvers with `compose-sequential`, `compose-parallel`, `compose-fallback`
2. **Repair Loops** - Iterate solutions until constraints pass
3. **Solver Estimation** - Cost prediction before execution
4. **Fact Store** - Monotone accumulation of facts
5. **Fixpoint Iteration** - Converge to stable state

These are **Layer 4** features - patterns built on top of the core evaluation and effects layers.

**Why This Matters**: LLM-based problem solving isn't "one shot". It requires:
- Trying multiple approaches
- Iterating on partial solutions
- Combining specialized solvers
- Managing resource budgets

---

## Package

**Package**: `@omega/solver` (optional)

This is an **optional package** for strategic problem-solving patterns.

```
@omega/solver/
├── types.ts            # SolverVal, ResultVal, CostEstimateVal
├── combinators.ts      # compose-sequential, compose-parallel, compose-fallback, compose-retry
├── repair.ts           # repair-until-valid
├── fixpoint.ts         # fixpoint, fixpoint-detect-cycle
├── facts.ts            # FactStore, assert-fact, query-fact, query-facts
├── budget.ts           # budget-split, budget-allocate
└── prims.ts            # All solver primitives
```

**Use Cases**:
- Multi-step LLM workflows
- Agentic loops with repair
- Constraint solving
- Resource-budgeted search

**Dependencies**:
- Requires `@omega/core` (for monadic composition)
- Requires `@omega/conditions` (for repair loop error handling)

---

## Dependencies

- **REQUIRES** (both must be complete before starting):
  - [Job 005](005-NON-UNWINDING-CONDITIONS.md) (Conditions) - `signal`, `error`, `handler-bind`, `restart-bind` for repair loop error handling
    - Verify: `npx vitest run test/conditions/` must pass
  - [Job 006](006-MONADIC-PRIMITIVES.md) (Monadic Primitives) - `unit`, `bind`, `mplus` for composable search
    - Verify: `npx vitest run test/monad/` must pass
- **Builds on**: Existing frontier/budget systems in `src/core/effects/nondet/` and `src/core/governance/budgets.ts`
- **Optional for**: Domain-specific DSLs (Layer 5)

---

## What Already Exists

### Frontier System

**Files**: [src/core/nondet/frontier.ts](../src/core/nondet/frontier.ts)

```typescript
// OmegaLLM has frontier kinds
type FrontierKind = "DFS" | "BFS" | "Beam";

// Fair scheduling exists
// Job scoring exists
```

### Budget System

**File**: [src/core/governance/budgets.ts](../src/core/governance/budgets.ts)

```typescript
// Budget tracking exists
type Budget = { ... };

// But no split/allocate functions
```

### Gap Analysis

| Exists | Missing |
|--------|---------|
| Frontier kinds | Composable solver interface |
| Budget tracking | budget-split, budget-allocate |
| Basic search | Repair loops |
| No | Solver estimation |
| No | Fact store |
| No | Fixpoint iteration |

---

## Design: Composable Solver Architecture

### Solver Interface

```typescript
// A Solver takes a problem and budget, returns results
type Solver<P, S> = {
  solve: (problem: P, budget: Budget) => ResultStream<S>;
  estimate: (problem: P) => CostEstimate;
  name: string;
  description?: string;
};

// Results can be success, partial, or failure
type Result<S> =
  | { tag: "Success"; solution: S; cost: number }
  | { tag: "Partial"; solution: S; remaining: Problem[]; cost: number }
  | { tag: "Failure"; reason: string; cost: number };

// Cost estimate before running
type CostEstimate = {
  minCost: number;
  maxCost: number;
  expectedCost: number;
  confidence: number;  // 0-1
};
```

### Solver Combinators

```
┌─────────────────────────────────────────────────────────────┐
│                   Solver Combinators                         │
│                                                              │
│  compose-sequential: A then B then C                         │
│  ┌───┐    ┌───┐    ┌───┐                                   │
│  │ A │───▶│ B │───▶│ C │                                   │
│  └───┘    └───┘    └───┘                                   │
│                                                              │
│  compose-parallel: A and B and C (all run)                   │
│  ┌───┐                                                       │
│  │ A │───┐                                                   │
│  └───┘   │                                                   │
│  ┌───┐   ├───▶ merge results                                │
│  │ B │───┤                                                   │
│  └───┘   │                                                   │
│  ┌───┐   │                                                   │
│  │ C │───┘                                                   │
│  └───┘                                                       │
│                                                              │
│  compose-fallback: A or B or C (first success)              │
│  ┌───┐    ╳    ┌───┐    ╳    ┌───┐                         │
│  │ A │───fail──│ B │───fail──│ C │                         │
│  └───┘         └───┘         └───┘                         │
│                                                              │
│  compose-retry: A with retry on failure                      │
│  ┌───┐                                                       │
│  │ A │◀──fail──┐                                            │
│  └───┘─────────┘ (up to N times)                            │
└─────────────────────────────────────────────────────────────┘
```

---

## Implementation Plan

### Task 1: Budget Split/Allocate (2 hours)

**File to Modify**: `src/core/governance/budgets.ts`

```typescript
// budget-split: Divide budget into N equal parts
export function budgetSplit(budget: Budget, n: number): Budget[] {
  if (n <= 0) throw new Error("Cannot split into zero parts");

  return Array.from({ length: n }, () => ({
    ...budget,
    tokens: Math.floor(budget.tokens / n),
    calls: Math.floor(budget.calls / n),
    time: Math.floor(budget.time / n),
  }));
}

// budget-allocate: Divide budget by weights
export function budgetAllocate(budget: Budget, weights: number[]): Budget[] {
  const total = weights.reduce((a, b) => a + b, 0);
  if (total <= 0) throw new Error("Weights must be positive");

  return weights.map(w => ({
    ...budget,
    tokens: Math.floor(budget.tokens * (w / total)),
    calls: Math.floor(budget.calls * (w / total)),
    time: Math.floor(budget.time * (w / total)),
  }));
}
```

**File to Modify**: `src/core/prims.ts`

```typescript
def("budget-split", {
  arity: 2,
  fn: (args) => {
    const budget = asBudget(args[0]);
    const n = asNumber(args[1]);
    const parts = budgetSplit(budget, n);
    return { tag: "List", elems: parts.map(budgetToVal) };
  }
});

def("budget-allocate", {
  arity: 2,
  fn: (args) => {
    const budget = asBudget(args[0]);
    const weights = asList(args[1]).map(asNumber);
    const parts = budgetAllocate(budget, weights);
    return { tag: "List", elems: parts.map(budgetToVal) };
  }
});
```

### Task 2: Solver Interface (4 hours)

**File to Create**: `src/core/solver/types.ts`

```typescript
import { Val } from "../eval/values";
import { Budget } from "../governance/budgets";

// Solver as a tagged value
export type SolverVal = {
  tag: "Solver";
  name: string;
  solve: (problem: Val, budget: Budget) => Generator<ResultVal>;
  estimate: (problem: Val) => CostEstimateVal;
};

export type ResultVal = {
  tag: "Result";
  kind: "success" | "partial" | "failure";
  solution?: Val;
  remaining?: Val[];
  reason?: string;
  cost: number;
};

export type CostEstimateVal = {
  tag: "CostEstimate";
  minCost: number;
  maxCost: number;
  expectedCost: number;
  confidence: number;
};

// Check if value is a solver
export function isSolver(val: Val): val is SolverVal {
  return val.tag === "Solver";
}
```

**File to Modify**: `src/core/prims.ts`

```typescript
// make-solver: Create a solver from functions
def("make-solver", {
  arity: 3,
  fn: (args) => {
    const [name, solveFn, estimateFn] = args;
    return {
      tag: "Solver",
      name: asString(name),
      solve: (problem, budget) => invokeSolveFn(solveFn, problem, budget),
      estimate: (problem) => invokeEstimateFn(estimateFn, problem),
    };
  }
});

// solver?: Type predicate
def("solver?", {
  arity: 1,
  pure: true,
  fn: (args) => ({ tag: "Bool", b: isSolver(args[0]) }),
});

// solver-solve: Run solver
def("solver-solve", {
  arity: 3,
  fn: (args, state) => {
    const solver = asSolver(args[0]);
    const problem = args[1];
    const budget = asBudget(args[2]);

    // Returns a stream of results
    return runSolverAsStream(solver, problem, budget, state);
  }
});

// solver-estimate: Get cost estimate
def("solver-estimate", {
  arity: 2,
  fn: (args) => {
    const solver = asSolver(args[0]);
    const problem = args[1];
    return solver.estimate(problem);
  }
});

// solver-name: Get solver name
def("solver-name", {
  arity: 1,
  fn: (args) => {
    const solver = asSolver(args[0]);
    return { tag: "Str", s: solver.name };
  }
});
```

### Task 3: Solver Combinators (6 hours)

**File to Create**: `src/core/solver/combinators.ts`

```typescript
// compose-sequential: Run solvers in sequence
export function composeSequential(solvers: SolverVal[]): SolverVal {
  return {
    tag: "Solver",
    name: `sequential(${solvers.map(s => s.name).join(", ")})`,
    solve: function*(problem, budget) {
      let current = problem;
      const parts = budgetSplit(budget, solvers.length);

      for (let i = 0; i < solvers.length; i++) {
        const results = solvers[i].solve(current, parts[i]);
        let anySuccess = false;

        for (const result of results) {
          if (result.kind === "success") {
            current = result.solution;
            anySuccess = true;
            break;
          } else if (result.kind === "partial") {
            current = result.solution;
            anySuccess = true;
            break;
          }
        }

        if (!anySuccess) {
          yield { tag: "Result", kind: "failure", reason: `Stage ${i} failed`, cost: 0 };
          return;
        }
      }

      yield { tag: "Result", kind: "success", solution: current, cost: 0 };
    },
    estimate: (problem) => {
      // Sum of estimates
      const estimates = solvers.map(s => s.estimate(problem));
      return {
        tag: "CostEstimate",
        minCost: estimates.reduce((a, e) => a + e.minCost, 0),
        maxCost: estimates.reduce((a, e) => a + e.maxCost, 0),
        expectedCost: estimates.reduce((a, e) => a + e.expectedCost, 0),
        confidence: estimates.reduce((a, e) => a * e.confidence, 1),
      };
    },
  };
}

// compose-parallel: Run solvers in parallel, merge results
export function composeParallel(solvers: SolverVal[]): SolverVal {
  return {
    tag: "Solver",
    name: `parallel(${solvers.map(s => s.name).join(", ")})`,
    solve: function*(problem, budget) {
      const parts = budgetSplit(budget, solvers.length);
      const allResults: ResultVal[][] = [];

      // Run all in parallel (conceptually)
      for (let i = 0; i < solvers.length; i++) {
        allResults.push([...solvers[i].solve(problem, parts[i])]);
      }

      // Merge: yield best results first
      const merged = mergeResultStreams(allResults);
      for (const result of merged) {
        yield result;
      }
    },
    estimate: (problem) => {
      // Max of estimates (parallel doesn't reduce time much)
      const estimates = solvers.map(s => s.estimate(problem));
      return {
        tag: "CostEstimate",
        minCost: Math.min(...estimates.map(e => e.minCost)),
        maxCost: Math.max(...estimates.map(e => e.maxCost)),
        expectedCost: estimates.reduce((a, e) => a + e.expectedCost, 0) / solvers.length,
        confidence: Math.max(...estimates.map(e => e.confidence)),
      };
    },
  };
}

// compose-fallback: Try each until one succeeds
export function composeFallback(solvers: SolverVal[]): SolverVal {
  return {
    tag: "Solver",
    name: `fallback(${solvers.map(s => s.name).join(", ")})`,
    solve: function*(problem, budget) {
      for (const solver of solvers) {
        const results = solver.solve(problem, budget);
        for (const result of results) {
          if (result.kind === "success") {
            yield result;
            return;
          }
        }
      }
      yield { tag: "Result", kind: "failure", reason: "All fallbacks failed", cost: 0 };
    },
    estimate: (problem) => {
      // First solver's estimate (optimistic)
      return solvers[0].estimate(problem);
    },
  };
}

// compose-retry: Retry a solver N times
export function composeRetry(solver: SolverVal, maxRetries: number): SolverVal {
  return {
    tag: "Solver",
    name: `retry(${solver.name}, ${maxRetries})`,
    solve: function*(problem, budget) {
      const parts = budgetSplit(budget, maxRetries);

      for (let i = 0; i < maxRetries; i++) {
        const results = solver.solve(problem, parts[i]);
        for (const result of results) {
          if (result.kind === "success") {
            yield result;
            return;
          }
        }
      }
      yield { tag: "Result", kind: "failure", reason: `Failed after ${maxRetries} retries`, cost: 0 };
    },
    estimate: (problem) => {
      const base = solver.estimate(problem);
      return {
        ...base,
        maxCost: base.maxCost * maxRetries,
      };
    },
  };
}
```

**File to Modify**: `src/core/prims.ts`

```typescript
def("compose-sequential", {
  arity: 1,
  fn: (args) => {
    const solvers = asList(args[0]).map(asSolver);
    return composeSequential(solvers);
  }
});

def("compose-parallel", {
  arity: 1,
  fn: (args) => {
    const solvers = asList(args[0]).map(asSolver);
    return composeParallel(solvers);
  }
});

def("compose-fallback", {
  arity: 1,
  fn: (args) => {
    const solvers = asList(args[0]).map(asSolver);
    return composeFallback(solvers);
  }
});

def("compose-retry", {
  arity: 2,
  fn: (args) => {
    const solver = asSolver(args[0]);
    const maxRetries = asNumber(args[1]);
    return composeRetry(solver, maxRetries);
  }
});
```

### Task 4: Repair Loop (4 hours)

**File to Create**: `src/core/solver/repair.ts`

```typescript
// repair-until-valid: Iterate until validator passes
// (repair-until-valid initial validator repair-fn max-iterations)
export function repairUntilValid(
  initial: Val,
  validator: ClosureVal,
  repairFn: ClosureVal,
  maxIterations: number,
  state: State
): Val {
  let current = initial;

  for (let i = 0; i < maxIterations; i++) {
    // Check if valid
    const isValid = invoke(validator, [current], state);
    if (isTruthy(isValid)) {
      return {
        tag: "Result",
        kind: "success",
        solution: current,
        cost: i,  // iterations as cost
      };
    }

    // Try to repair
    const repaired = invoke(repairFn, [current], state);
    if (repaired.tag === "Result" && repaired.kind === "failure") {
      return repaired;  // Repair failed
    }

    current = repaired.solution || repaired;
  }

  return {
    tag: "Result",
    kind: "partial",
    solution: current,
    reason: `Max iterations (${maxIterations}) reached`,
    cost: maxIterations,
  };
}
```

**File to Modify**: `src/core/prims.ts`

```typescript
def("repair-until-valid", {
  arity: 4,
  fn: (args, state) => {
    const [initial, validator, repairFn, maxIterations] = args;
    return repairUntilValid(
      initial,
      asClosure(validator),
      asClosure(repairFn),
      asNumber(maxIterations),
      state
    );
  }
});
```

### Task 5: Fact Store (4 hours)

**File to Create**: `src/core/solver/facts.ts`

```typescript
// Monotone fact store - facts can only be added, never removed
export type FactStore = {
  tag: "FactStore";
  facts: Map<string, Val>;  // fact-key -> fact-value
  derived: Map<string, string[]>;  // fact-key -> derived-from
};

export function createFactStore(): FactStore {
  return {
    tag: "FactStore",
    facts: new Map(),
    derived: new Map(),
  };
}

export function assertFact(
  store: FactStore,
  key: string,
  value: Val,
  sources: string[] = []
): FactStore {
  // Monotone: can only add, not update
  if (store.facts.has(key)) {
    const existing = store.facts.get(key)!;
    if (!equal(existing, value)) {
      throw new Error(`Fact ${key} already exists with different value`);
    }
    return store;  // Already have this fact
  }

  // Clone and add
  const newFacts = new Map(store.facts);
  newFacts.set(key, value);

  const newDerived = new Map(store.derived);
  newDerived.set(key, sources);

  return {
    tag: "FactStore",
    facts: newFacts,
    derived: newDerived,
  };
}

export function queryFact(store: FactStore, key: string): Val | undefined {
  return store.facts.get(key);
}

export function queryFactsByPattern(store: FactStore, pattern: string): [string, Val][] {
  const regex = new RegExp(pattern);
  const results: [string, Val][] = [];
  for (const [key, value] of store.facts) {
    if (regex.test(key)) {
      results.push([key, value]);
    }
  }
  return results;
}
```

**File to Modify**: `src/core/prims.ts`

```typescript
def("make-fact-store", {
  arity: 0,
  fn: () => createFactStore(),
});

def("fact-store?", {
  arity: 1,
  pure: true,
  fn: (args) => ({ tag: "Bool", b: args[0].tag === "FactStore" }),
});

def("assert-fact", {
  arity: 3,  // (assert-fact store key value)
  fn: (args) => {
    const store = asFactStore(args[0]);
    const key = asString(args[1]);
    const value = args[2];
    return assertFact(store, key, value);
  }
});

def("query-fact", {
  arity: 2,  // (query-fact store key)
  fn: (args) => {
    const store = asFactStore(args[0]);
    const key = asString(args[1]);
    const result = queryFact(store, key);
    return result || { tag: "Bool", b: false };
  }
});

def("query-facts", {
  arity: 2,  // (query-facts store pattern)
  fn: (args) => {
    const store = asFactStore(args[0]);
    const pattern = asString(args[1]);
    const results = queryFactsByPattern(store, pattern);
    return {
      tag: "List",
      elems: results.map(([k, v]) => ({
        tag: "List",
        elems: [{ tag: "Str", s: k }, v],
      })),
    };
  }
});
```

### Task 6: Fixpoint Iteration (4 hours)

**File to Create**: `src/core/solver/fixpoint.ts`

```typescript
// fixpoint: Iterate until state stabilizes
// (fixpoint initial step-fn equal-fn max-iterations)
export function fixpoint(
  initial: Val,
  stepFn: ClosureVal,
  equalFn: ClosureVal,
  maxIterations: number,
  state: State
): Val {
  let current = initial;

  for (let i = 0; i < maxIterations; i++) {
    const next = invoke(stepFn, [current], state);

    // Check if converged
    const isEqual = invoke(equalFn, [current, next], state);
    if (isTruthy(isEqual)) {
      return {
        tag: "Result",
        kind: "success",
        solution: next,
        cost: i + 1,  // iterations as cost
      };
    }

    current = next;
  }

  return {
    tag: "Result",
    kind: "partial",
    solution: current,
    reason: `Did not converge in ${maxIterations} iterations`,
    cost: maxIterations,
  };
}

// fixpoint-with-detection: Detect cycles
export function fixpointWithCycleDetection(
  initial: Val,
  stepFn: ClosureVal,
  hashFn: ClosureVal,
  maxIterations: number,
  state: State
): Val {
  let current = initial;
  const seen = new Set<string>();

  for (let i = 0; i < maxIterations; i++) {
    const hash = asString(invoke(hashFn, [current], state));

    if (seen.has(hash)) {
      return {
        tag: "Result",
        kind: "failure",
        reason: `Cycle detected at iteration ${i}`,
        cost: i,
      };
    }

    seen.add(hash);
    current = invoke(stepFn, [current], state);
  }

  return {
    tag: "Result",
    kind: "partial",
    solution: current,
    reason: `Max iterations (${maxIterations}) reached without convergence or cycle`,
    cost: maxIterations,
  };
}
```

**File to Modify**: `src/core/prims.ts`

```typescript
def("fixpoint", {
  arity: 4,  // (fixpoint initial step-fn equal-fn max-iterations)
  fn: (args, state) => {
    const [initial, stepFn, equalFn, maxIterations] = args;
    return fixpoint(
      initial,
      asClosure(stepFn),
      asClosure(equalFn),
      asNumber(maxIterations),
      state
    );
  }
});

def("fixpoint-detect-cycle", {
  arity: 4,  // (fixpoint-detect-cycle initial step-fn hash-fn max-iterations)
  fn: (args, state) => {
    const [initial, stepFn, hashFn, maxIterations] = args;
    return fixpointWithCycleDetection(
      initial,
      asClosure(stepFn),
      asClosure(hashFn),
      asNumber(maxIterations),
      state
    );
  }
});
```

---

## Verification

### Test 1: Composable Solvers

```lisp
;; Create individual solvers
(define analyzer (make-solver "analyze"
  (lambda (p b) (unit (analyze-problem p)))
  (lambda (p) (make-estimate 10 20 15 0.8))))

(define fixer (make-solver "fix"
  (lambda (p b) (unit (fix-issue p)))
  (lambda (p) (make-estimate 5 10 7 0.9))))

;; Compose them
(define pipeline (compose-sequential (list analyzer fixer)))

;; Run
(solver-solve pipeline problem budget)
```

### Test 2: Repair Loop

```lisp
;; Repair code until tests pass
(define fixed
  (repair-until-valid
    initial-code
    (lambda (code) (run-tests code))
    (lambda (code) (oracle-repair code (get-test-failures code)))
    5))

(result-success? fixed)  ; => #t if fixed within 5 iterations
```

### Test 3: Fixpoint

```lisp
;; Iterate dataflow analysis to fixpoint
(define final-state
  (fixpoint
    initial-state
    (lambda (s) (propagate-constraints s))
    equal?
    100))
```

---

## Test Plan

### Budget Tests (`test/solver/budget.spec.ts`)

#### Happy Path Tests

```lisp
;; HP-1: budget-split divides evenly
(define b (make-budget tokens=100 calls=10 time=1000))
(define parts (budget-split b 4))
(length parts)
;; => 4
(budget-tokens (car parts))
;; => 25

;; HP-2: budget-allocate by weights
(define b (make-budget tokens=100 calls=10 time=1000))
(define parts (budget-allocate b '(1 2 1)))  ; 25%, 50%, 25%
(budget-tokens (car parts))
;; => 25
(budget-tokens (cadr parts))
;; => 50

;; HP-3: budget-split preserves total
(define b (make-budget tokens=100 calls=10 time=1000))
(define parts (budget-split b 3))
(apply + (map budget-tokens parts))
;; => 99 (floor division may lose 1)
```

#### Edge Case Tests

```lisp
;; EC-1: budget-split into 1 part
(define b (make-budget tokens=100 calls=10 time=1000))
(define parts (budget-split b 1))
(= (budget-tokens (car parts)) 100)
;; => #t

;; EC-2: budget-allocate with unequal weights
(define b (make-budget tokens=100 calls=10 time=1000))
(define parts (budget-allocate b '(9 1)))  ; 90%, 10%
(budget-tokens (car parts))
;; => 90
(budget-tokens (cadr parts))
;; => 10

;; EC-3: budget-split with small budget
(define b (make-budget tokens=3 calls=1 time=10))
(define parts (budget-split b 5))
;; Each part gets 0 tokens (floor(3/5) = 0)
(budget-tokens (car parts))
;; => 0
```

#### Error Cases

```lisp
;; ERR-1: budget-split with n=0
(budget-split (make-budget tokens=100 calls=10 time=1000) 0)
;; => Error: Cannot split into zero parts

;; ERR-2: budget-split with negative n
(budget-split (make-budget tokens=100 calls=10 time=1000) -1)
;; => Error: Cannot split into negative parts

;; ERR-3: budget-allocate with empty weights
(budget-allocate (make-budget tokens=100 calls=10 time=1000) '())
;; => Error: Weights list cannot be empty

;; ERR-4: budget-allocate with all-zero weights
(budget-allocate (make-budget tokens=100 calls=10 time=1000) '(0 0 0))
;; => Error: Weights must be positive
```

### Solver Interface Tests (`test/solver/interface.spec.ts`)

#### Happy Path Tests

```lisp
;; HP-1: Create simple solver
(define s (make-solver "identity"
            (lambda (p b) (unit (make-result 'success p 0)))
            (lambda (p) (make-estimate 1 1 1 1.0))))
(solver? s)
;; => #t
(solver-name s)
;; => "identity"

;; HP-2: Run solver
(define s (make-solver "double"
            (lambda (p b) (unit (make-result 'success (* p 2) 1)))
            (lambda (p) (make-estimate 1 1 1 1.0))))
(define results (collect-all (solver-solve s 5 (make-budget tokens=100))))
(result-solution (car results))
;; => 10

;; HP-3: Get cost estimate
(define s (make-solver "expensive"
            (lambda (p b) (unit (make-result 'success p 10)))
            (lambda (p) (make-estimate 5 15 10 0.8))))
(define est (solver-estimate s 'any-problem))
(estimate-expected-cost est)
;; => 10
(estimate-confidence est)
;; => 0.8

;; HP-4: Solver returns stream of results
(define s (make-solver "multi"
            (lambda (p b)
              (mplus (unit (make-result 'success (+ p 1) 1))
                     (unit (make-result 'success (+ p 2) 2))))
            (lambda (p) (make-estimate 1 2 1.5 0.9))))
(length (collect-all (solver-solve s 0 (make-budget tokens=100))))
;; => 2
```

#### Edge Case Tests

```lisp
;; EC-1: Solver returns no results (failure)
(define s (make-solver "fail"
            (lambda (p b) (unit (make-result 'failure #f "always fails" 0)))
            (lambda (p) (make-estimate 1 1 1 0.1))))
(define results (collect-all (solver-solve s 'x (make-budget tokens=100))))
(result-kind (car results))
;; => 'failure

;; EC-2: Solver returns partial result
(define s (make-solver "partial"
            (lambda (p b) (unit (make-result 'partial (list 'partial-sol) (list 'remaining) 5)))
            (lambda (p) (make-estimate 5 10 7 0.5))))
(define r (car (collect-all (solver-solve s 'x (make-budget tokens=100)))))
(result-kind r)
;; => 'partial
(result-remaining r)
;; => (remaining)

;; EC-3: Solver with zero-cost operation
(define s (make-solver "free"
            (lambda (p b) (unit (make-result 'success p 0)))
            (lambda (p) (make-estimate 0 0 0 1.0))))
(result-cost (car (collect-all (solver-solve s 'x (make-budget tokens=100)))))
;; => 0
```

#### Error Cases

```lisp
;; ERR-1: make-solver with non-string name
(make-solver 123 (lambda (p b) (unit 'x)) (lambda (p) 'x))
;; => Error: Solver name must be a string

;; ERR-2: make-solver with non-procedure solve-fn
(make-solver "bad" 42 (lambda (p) 'x))
;; => Error: Solver solve function must be a procedure

;; ERR-3: solver-solve with non-solver
(solver-solve 42 'problem (make-budget tokens=100))
;; => Error: First argument must be a solver
```

### Combinator Tests (`test/solver/combinators.spec.ts`)

#### compose-sequential Tests

```lisp
;; HP-1: Sequential composition succeeds
(define s1 (make-solver "add1" (lambda (p b) (unit (make-result 'success (+ p 1) 1))) ...))
(define s2 (make-solver "mult2" (lambda (p b) (unit (make-result 'success (* p 2) 1))) ...))
(define seq (compose-sequential (list s1 s2)))
(solver-name seq)
;; => "sequential(add1, mult2)"
(define r (car (collect-all (solver-solve seq 5 (make-budget tokens=100)))))
(result-solution r)
;; => 12  ; (5+1)*2

;; HP-2: Sequential stops on failure
(define s1 (make-solver "fail" (lambda (p b) (unit (make-result 'failure #f "oops" 1))) ...))
(define s2 (make-solver "add1" (lambda (p b) (unit (make-result 'success (+ p 1) 1))) ...))
(define seq (compose-sequential (list s1 s2)))
(define r (car (collect-all (solver-solve seq 5 (make-budget tokens=100)))))
(result-kind r)
;; => 'failure

;; EC-1: Sequential with single solver
(define s1 (make-solver "only" (lambda (p b) (unit (make-result 'success 'done 1))) ...))
(define seq (compose-sequential (list s1)))
(result-solution (car (collect-all (solver-solve seq 'x (make-budget tokens=100)))))
;; => 'done

;; EC-2: Sequential with empty list
(define seq (compose-sequential '()))
(result-kind (car (collect-all (solver-solve seq 'x (make-budget tokens=100)))))
;; => 'success (identity - problem passes through unchanged)
```

#### compose-parallel Tests

```lisp
;; HP-1: Parallel runs all solvers
(define s1 (make-solver "a" (lambda (p b) (unit (make-result 'success 'a 1))) ...))
(define s2 (make-solver "b" (lambda (p b) (unit (make-result 'success 'b 1))) ...))
(define par (compose-parallel (list s1 s2)))
(length (collect-all (solver-solve par 'x (make-budget tokens=100))))
;; => 2 (both results)

;; HP-2: Parallel includes partial results
(define s1 (make-solver "good" (lambda (p b) (unit (make-result 'success 'good 1))) ...))
(define s2 (make-solver "partial" (lambda (p b) (unit (make-result 'partial 'part '(more) 1))) ...))
(define par (compose-parallel (list s1 s2)))
(define results (collect-all (solver-solve par 'x (make-budget tokens=100))))
(any (lambda (r) (eq? (result-kind r) 'success)) results)
;; => #t

;; EC-1: Parallel where all fail
(define s1 (make-solver "f1" (lambda (p b) (unit (make-result 'failure #f "f1" 1))) ...))
(define s2 (make-solver "f2" (lambda (p b) (unit (make-result 'failure #f "f2" 1))) ...))
(define par (compose-parallel (list s1 s2)))
(all (lambda (r) (eq? (result-kind r) 'failure)) (collect-all (solver-solve par 'x (make-budget tokens=100))))
;; => #t
```

#### compose-fallback Tests

```lisp
;; HP-1: Fallback tries until success
(define s1 (make-solver "fail" (lambda (p b) (unit (make-result 'failure #f "nope" 1))) ...))
(define s2 (make-solver "succeed" (lambda (p b) (unit (make-result 'success 'yes 1))) ...))
(define fb (compose-fallback (list s1 s2)))
(define r (car (collect-all (solver-solve fb 'x (make-budget tokens=100)))))
(result-solution r)
;; => 'yes

;; HP-2: Fallback returns first success
(define s1 (make-solver "first" (lambda (p b) (unit (make-result 'success 'first 1))) ...))
(define s2 (make-solver "second" (lambda (p b) (unit (make-result 'success 'second 1))) ...))
(define fb (compose-fallback (list s1 s2)))
(result-solution (car (collect-all (solver-solve fb 'x (make-budget tokens=100)))))
;; => 'first (doesn't try second)

;; EC-1: All fallbacks fail
(define s1 (make-solver "f1" (lambda (p b) (unit (make-result 'failure #f "f1" 1))) ...))
(define s2 (make-solver "f2" (lambda (p b) (unit (make-result 'failure #f "f2" 1))) ...))
(define fb (compose-fallback (list s1 s2)))
(result-kind (car (collect-all (solver-solve fb 'x (make-budget tokens=100)))))
;; => 'failure
(result-reason (car (collect-all (solver-solve fb 'x (make-budget tokens=100)))))
;; => "All fallbacks failed"
```

#### compose-retry Tests

```lisp
;; HP-1: Retry succeeds on first try
(define s (make-solver "good" (lambda (p b) (unit (make-result 'success 'ok 1))) ...))
(define r (compose-retry s 3))
(result-solution (car (collect-all (solver-solve r 'x (make-budget tokens=100)))))
;; => 'ok

;; HP-2: Retry succeeds on later try (with state)
(define attempt (box 0))
(define s (make-solver "eventual"
            (lambda (p b)
              (set-box! attempt (+ (unbox attempt) 1))
              (if (>= (unbox attempt) 3)
                  (unit (make-result 'success 'finally 1))
                  (unit (make-result 'failure #f "not yet" 1))))
            ...))
(define r (compose-retry s 5))
(result-solution (car (collect-all (solver-solve r 'x (make-budget tokens=100)))))
;; => 'finally

;; EC-1: Retry exhausts attempts
(define s (make-solver "always-fail" (lambda (p b) (unit (make-result 'failure #f "nope" 1))) ...))
(define r (compose-retry s 3))
(result-reason (car (collect-all (solver-solve r 'x (make-budget tokens=100)))))
;; => "Failed after 3 retries"

;; EC-2: Retry with max=1 (no actual retries)
(define s (make-solver "fail" (lambda (p b) (unit (make-result 'failure #f "nope" 1))) ...))
(define r (compose-retry s 1))
(result-kind (car (collect-all (solver-solve r 'x (make-budget tokens=100)))))
;; => 'failure
```

### Repair Loop Tests (`test/solver/repair.spec.ts`)

#### Happy Path Tests

```lisp
;; HP-1: Already valid, no repair needed
(define r (repair-until-valid
            'valid-input
            (lambda (x) #t)  ; always valid
            (lambda (x) x)   ; identity repair
            5))
(result-kind r)
;; => 'success
(result-solution r)
;; => 'valid-input
(result-cost r)
;; => 0 (no iterations)

;; HP-2: Repairs until valid
(define r (repair-until-valid
            0
            (lambda (x) (>= x 3))   ; valid when >= 3
            (lambda (x) (+ x 1))    ; add 1 each repair
            10))
(result-kind r)
;; => 'success
(result-solution r)
;; => 3
(result-cost r)
;; => 3 (three iterations)

;; HP-3: Complex repair with state
(define r (repair-until-valid
            (list 'a 'b 'c 'd)
            (lambda (xs) (= (length xs) 2))       ; valid when length is 2
            (lambda (xs) (cdr xs))                 ; drop first element
            10))
(result-solution r)
;; => (c d)
```

#### Edge Case Tests

```lisp
;; EC-1: Repair reaches max iterations
(define r (repair-until-valid
            0
            (lambda (x) (>= x 100))  ; valid when >= 100
            (lambda (x) (+ x 1))     ; add 1 each repair
            5))                       ; but only 5 iterations allowed
(result-kind r)
;; => 'partial
(result-solution r)
;; => 5
(result-reason r)
;; => "Max iterations (5) reached"

;; EC-2: Repair function fails
(define r (repair-until-valid
            'x
            (lambda (x) #f)                        ; never valid
            (lambda (x) (make-result 'failure #f "can't repair" 1))
            5))
(result-kind r)
;; => 'failure

;; EC-3: Zero max iterations
(define r (repair-until-valid
            0
            (lambda (x) #f)   ; never valid
            (lambda (x) x)
            0))
(result-kind r)
;; => 'partial
(result-cost r)
;; => 0
```

#### Error Cases

```lisp
;; ERR-1: Non-procedure validator
(repair-until-valid 'x 42 (lambda (x) x) 5)
;; => Error: Validator must be a procedure

;; ERR-2: Non-procedure repair function
(repair-until-valid 'x (lambda (x) #t) 42 5)
;; => Error: Repair function must be a procedure

;; ERR-3: Negative max iterations
(repair-until-valid 'x (lambda (x) #t) (lambda (x) x) -1)
;; => Error: Max iterations must be non-negative
```

### Fact Store Tests (`test/solver/facts.spec.ts`)

#### Happy Path Tests

```lisp
;; HP-1: Create empty fact store
(define fs (make-fact-store))
(fact-store? fs)
;; => #t

;; HP-2: Assert and query fact
(define fs (make-fact-store))
(define fs2 (assert-fact fs "x.type" 'integer))
(query-fact fs2 "x.type")
;; => 'integer

;; HP-3: Query multiple facts by pattern
(define fs (make-fact-store))
(define fs2 (assert-fact fs "user.name" "Alice"))
(define fs3 (assert-fact fs2 "user.age" 30))
(define fs4 (assert-fact fs3 "config.debug" #t))
(define results (query-facts fs4 "user\\..*"))
(length results)
;; => 2 (user.name and user.age)

;; HP-4: Fact store is immutable (functional update)
(define fs1 (make-fact-store))
(define fs2 (assert-fact fs1 "x" 1))
(query-fact fs1 "x")
;; => #f (fs1 unchanged)
(query-fact fs2 "x")
;; => 1
```

#### Edge Case Tests

```lisp
;; EC-1: Assert same fact twice (idempotent)
(define fs (make-fact-store))
(define fs2 (assert-fact fs "x" 42))
(define fs3 (assert-fact fs2 "x" 42))
(eq? fs2 fs3)
;; => #t (same store, no change)

;; EC-2: Query non-existent fact
(define fs (make-fact-store))
(query-fact fs "nonexistent")
;; => #f

;; EC-3: Query with pattern that matches nothing
(define fs (assert-fact (make-fact-store) "foo" 1))
(query-facts fs "bar.*")
;; => ()

;; EC-4: Empty pattern matches all
(define fs (assert-fact (assert-fact (make-fact-store) "a" 1) "b" 2))
(length (query-facts fs ".*"))
;; => 2
```

#### Error Cases

```lisp
;; ERR-1: Assert fact with different value (monotonicity violation)
(define fs (make-fact-store))
(define fs2 (assert-fact fs "x" 1))
(assert-fact fs2 "x" 2)
;; => Error: Fact x already exists with different value

;; ERR-2: Invalid regex pattern
(query-facts (make-fact-store) "[invalid")
;; => Error: Invalid regex pattern
```

### Fixpoint Tests (`test/solver/fixpoint.spec.ts`)

#### Happy Path Tests

```lisp
;; HP-1: Converges immediately (already at fixpoint)
(define r (fixpoint
            10
            (lambda (x) x)     ; identity - already stable
            equal?
            100))
(result-kind r)
;; => 'success
(result-solution r)
;; => 10
(result-cost r)
;; => 1

;; HP-2: Converges after iterations
(define r (fixpoint
            1
            (lambda (x) (if (< x 10) (+ x 1) x))  ; increment until 10
            equal?
            100))
(result-solution r)
;; => 10
(result-cost r)
;; => 10

;; HP-3: Converges on compound data
(define r (fixpoint
            (list 1 2 3)
            (lambda (xs) (if (> (length xs) 1) (cdr xs) xs))
            equal?
            100))
(result-solution r)
;; => (3)
```

#### Edge Case Tests

```lisp
;; EC-1: Max iterations reached without convergence
(define r (fixpoint
            0
            (lambda (x) (+ x 1))  ; always changes
            equal?
            5))
(result-kind r)
;; => 'partial
(result-reason r)
;; => "Did not converge in 5 iterations"
(result-solution r)
;; => 5 (last value)

;; EC-2: Max iterations = 1
(define r (fixpoint
            0
            (lambda (x) (+ x 1))
            equal?
            1))
(result-kind r)
;; => 'partial
(result-cost r)
;; => 1

;; EC-3: Custom equality function
(define r (fixpoint
            1.0
            (lambda (x) (/ (+ x (/ 2 x)) 2))  ; Newton's method for sqrt(2)
            (lambda (a b) (< (abs (- a b)) 0.0001))  ; converge within epsilon
            100))
(result-kind r)
;; => 'success
(< (abs (- (result-solution r) 1.414)) 0.001)
;; => #t
```

#### fixpoint-detect-cycle Tests

```lisp
;; HP-1: Detects cycle
(define r (fixpoint-detect-cycle
            0
            (lambda (x) (modulo (+ x 1) 3))  ; cycles: 0->1->2->0
            number->string
            100))
(result-kind r)
;; => 'failure
(result-reason r)
;; => "Cycle detected at iteration 3"

;; HP-2: No cycle, converges
(define r (fixpoint-detect-cycle
            10
            (lambda (x) (if (> x 0) (- x 1) x))
            number->string
            100))
(result-kind r)
;; => 'success
(result-solution r)
;; => 0

;; EC-1: Immediate cycle (step returns same value)
(define r (fixpoint-detect-cycle
            5
            (lambda (x) 5)  ; always returns 5
            number->string
            100))
;; This is actually convergence, not a cycle
(result-kind r)
;; => 'success (or could be detected as cycle, implementation-dependent)
```

#### Error Cases

```lisp
;; ERR-1: Non-procedure step function
(fixpoint 0 42 equal? 100)
;; => Error: Step function must be a procedure

;; ERR-2: Non-procedure equality function
(fixpoint 0 (lambda (x) x) 42 100)
;; => Error: Equality function must be a procedure

;; ERR-3: Negative max iterations
(fixpoint 0 (lambda (x) x) equal? -1)
;; => Error: Max iterations must be non-negative
```

### Integration Tests

1. Multi-stage code repair pipeline
2. Parallel solver exploration
3. Fallback recovery scenarios
4. Large fact store performance

---

## Checklist

### Task 1: Budget
- [ ] Implement `budget-split`
- [ ] Implement `budget-allocate`
- [ ] Add primitives to prims.ts
- [ ] Add to compileText.ts

### Task 2: Solver Interface
- [ ] Create `src/core/solver/types.ts`
- [ ] Implement `make-solver`
- [ ] Implement `solver?`
- [ ] Implement `solver-solve`
- [ ] Implement `solver-estimate`
- [ ] Implement `solver-name`

### Task 3: Combinators
- [ ] Create `src/core/solver/combinators.ts`
- [ ] Implement `compose-sequential`
- [ ] Implement `compose-parallel`
- [ ] Implement `compose-fallback`
- [ ] Implement `compose-retry`

### Task 4: Repair Loop
- [ ] Create `src/core/solver/repair.ts`
- [ ] Implement `repair-until-valid`
- [ ] Add condition integration (Job 005)

### Task 5: Fact Store
- [ ] Create `src/core/solver/facts.ts`
- [ ] Implement `make-fact-store`
- [ ] Implement `assert-fact`
- [ ] Implement `query-fact`
- [ ] Implement `query-facts`

### Task 6: Fixpoint
- [ ] Create `src/core/solver/fixpoint.ts`
- [ ] Implement `fixpoint`
- [ ] Implement `fixpoint-detect-cycle`

### Verification
- [ ] All existing tests pass (1124+)
- [ ] New solver tests pass
- [ ] REPL examples work

---

## Notes

### Why Solvers as First-Class Values?

In LambdaRLM, solvers are composable functions. Making them first-class in OmegaLLM means:
- Can store solvers in data structures
- Can pass solvers to higher-order functions
- Can build solver libraries
- Enables meta-programming (solvers that build solvers)

### Relationship to Existing Systems

| OmegaLLM Existing | This Job Adds |
|-------------------|---------------|
| Frontier (DFS/BFS/Beam) | Solver combinators use frontiers |
| Budget tracking | budget-split/allocate |
| amb/nondet | Solvers can use monadic interface |
| Conditions (Job 005) | Repair loops use conditions |

### Future: Domain Algebra (Layer 5)

Job 008 provides the foundation. Domain algebra (if needed) would add:
- `define-sort` - Define problem/solution types
- `define-operation` - Define operations on domains
- `algebra-simplify` - Algebraic simplification
- `algebra-unify` - Problem unification

That's a separate job only if formal reasoning is required.

---

---

## Proof of Completion

When marking this job DONE:

1. **Prerequisite checks**:
   - `npx vitest run test/conditions/` - Job 005 tests pass
   - `npx vitest run test/monad/` - Job 006 tests pass
2. **Build passes**: `npm run build` - no TypeScript errors
3. **Baseline tests pass**: `npm run test` - 1124+ tests still green
4. **New tests pass**:
   - `npx vitest run test/solver/combinators.spec.ts`
   - `npx vitest run test/solver/repair.spec.ts`
   - `npx vitest run test/solver/fixpoint.spec.ts`
   - `npx vitest run test/solver/facts.spec.ts`
5. **REPL verification**: Test examples from Verification section manually
6. **Update status**: Change `NOT STARTED` → `DONE` in this file
7. **Update README**: Mark Job 008 as DONE in [README.md](README.md)

---

*Created: 2026-01-19*
*Related: [005-NON-UNWINDING-CONDITIONS.md](./005-NON-UNWINDING-CONDITIONS.md), [006-MONADIC-PRIMITIVES.md](./006-MONADIC-PRIMITIVES.md)*
*Depends on: Jobs 005 and 006*
