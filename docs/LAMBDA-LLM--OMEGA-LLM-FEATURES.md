# Comprehensive Feature Audit: LambdaRLM x LambdaLLM x OmegaLLM x FrameLisp

> **Purpose**: Map every Lisp function/primitive/keyword across all projects with VERIFIED file references.
>
> **Projects**:
> - **LambdaRLM**: Lisp implementations (~400KB across 28 files in `lib/*.lisp`)
> - **LambdaLLM**: TypeScript specifications (71 ARCHITECTURE/*.md files)
> - **OmegaLLM**: TypeScript runtime implementation (90+ files in `src/core/`)
> - **FrameLisp**: Algebraic specification (REFERENCE-ALGEBRA.md)

---

## Executive Summary

| Project | Files | Primary Language | Status | Notes |
|---------|-------|------------------|--------|-------|
| **LambdaRLM** | 28 | Lisp | Implemented | Budget, results, streams, nondet, domain algebra |
| **LambdaLLM** | 71 | TypeScript specs | Specification | Full language spec + enterprise features |
| **OmegaLLM** | 90+ | TypeScript | Implemented | Eval, streams, nondet, concurrency, constraints, generic dispatch |
| **FrameLisp** | 1 | Algebraic spec | Specification | Prompt algebra, combinators |

### Overlap Analysis
- **High Overlap**: Streams, Nondeterminism, Budget management
- **Partial Overlap**: Results/Outcomes, Provenance
- **Unique to LambdaRLM**: Domain algebra, Meta-search, Composable solvers, Obligations
- **Unique to OmegaLLM**: Concurrency (fibers/actors), Constraints (propagation), Generic dispatch
- **Unique to LambdaLLM**: Facts, Fixpoint, Experts, Sessions

---

## ✅ FIXED: Production Primitives Now Operational

> **STATUS**: FIXED on 2026-01-19
>
> **What was done**: Moved primitives from `test/helpers/prims.ts` to `src/core/prims.ts` and wired into `OmegaRuntime`.
> - [src/core/prims.ts](../src/core/prims.ts) - 117 production primitives
> - [src/runtime.ts](../src/runtime.ts) - now imports from `./core/prims`
> - [src/core/pipeline/compileText.ts](../src/core/pipeline/compileText.ts) - compiler knows all primitive names
> - All 1124 tests pass
>
> **Job file**: [CLAUDE-JOBS/001-FIX-PRODUCTION-PRIMITIVES.md](../CLAUDE-JOBS/001-FIX-PRODUCTION-PRIMITIVES.md)

### Historical Context: What Was Broken (Now Fixed)

The **production OmegaRuntime** can only evaluate these primitives:

| Primitive | That's It |
|-----------|-----------|
| `+` | addition |
| `-` | subtraction |
| `=` | equality |
| `not` | negation |
| `unit` | unit value |

**THAT'S ALL.** No `cons`, no `car`, no `cdr`, no `list`, no streams, no nothing.

**Evidence**: [compileText.ts:77](../src/core/pipeline/compileText.ts#L77):
```typescript
function initialEnv(moduleScope: string): Env {
  const prims = ["+", "-", "=", "not", "unit"];  // <-- ONLY 5
  ...
}
```

### CATASTROPHE LEVEL 2: Runtime Returns Empty Environment

[runtime.ts:98-103](../src/runtime.ts#L98):
```typescript
function installPrims(store: COWStore): { env: ReturnType<typeof envEmpty>; store: COWStore } {
  const env = envEmpty();  // <-- EMPTY!
  // The prims are installed by the compile-time env in compileTextToExpr
  // We just return empty env here; the compiler handles prim bindings
  return { env, store };
}
```

The comment is a LIE. The compiler only knows 5 primitives.

### CATASTROPHE LEVEL 3: Tests Use Completely Separate Primitives

**28 test files** import from `test/helpers/prims.ts` which has **117 primitives** with correct Lisp names:

| Component | Primitives | Used By |
|-----------|------------|---------|
| `compileText.ts` | **5** | Production OmegaRuntime |
| `runtime.ts` installPrims | **0** | Production (returns empty) |
| `test/helpers/prims.ts` | **117** | Tests ONLY |
| `src/core/meta/primitives.ts` | **38** | Omega0 meta-circular (separate) |

**Tests pass because they bypass the production runtime entirely.**

### The Two Evaluator Systems

OmegaLLM has TWO completely separate evaluator systems:

1. **CEKS Machine** (`src/core/eval/` + `runtime.ts`)
   - Used by `OmegaRuntime` class
   - Production entry point
   - Has **5 primitives** from compiler, **0 primitives** from runtime
   - **BASICALLY NON-FUNCTIONAL**

2. **Omega0 Meta-Circular** (`src/core/meta/`)
   - Separate interpreter with `createBaseEnv()`
   - Has **38 primitives** (arithmetic, list ops, type predicates, string ops)
   - Used by DSL tools in `meta/dsl.ts`
   - **NOT the main runtime**

### Streams Are Also Broken (But That's Minor Compared to Above)

**The Problem**:
1. **Compiler** ([lower.ts:228](../src/core/pipeline/lower.ts#L228)) recognizes `cons-stream` as syntax sugar - CORRECT
2. **Runtime** ([stream.ts](../src/core/stream/stream.ts)) has TypeScript functions with camelCase names - WRONG NAMES
3. **NO PRIMITIVES REGISTERED** - [primitives.ts](../src/core/meta/primitives.ts) has NO stream primitives - FATAL

**Result**: You can write `(cons-stream 1 2)` but you **CANNOT** call `(stream-car s)` because that primitive doesn't exist. The entire stream module is TypeScript-internal only and **CANNOT BE USED FROM LISP**.

### What Tests Have vs What Production Has

**Test helper primitives** ([test/helpers/prims.ts](../test/helpers/prims.ts)) - 117 primitives with CORRECT names:
- **List ops**: `cons`, `car`, `cdr`, `null?`, `pair?`, `list`, `length`, `append`, `reverse`, `nth`, `map`, `filter`, `foldr`, `foldl`
- **Stream ops**: `stream-car`, `stream-cdr`, `stream-null?`, `the-empty-stream`, `stream-take`, `stream-map`, `stream-filter`
- **Higher-order**: `map`, `filter`, `apply`, `compose`
- **Arithmetic**: `+`, `-`, `*`, `/`, `<`, `<=`, `=`, `>=`, `>`, `abs`, `min`, `max`, `modulo`
- **Type predicates**: `number?`, `string?`, `symbol?`, `boolean?`, `procedure?`, `list?`, `pair?`
- **String ops**: `string-append`, `string-length`, `substring`, `string=?`
- **Control**: `if`, `cond`, `begin`, `and`, `or`, `not`
- **Effects**: `amb`, `fail`, `handle`, `effect`
- **Generic dispatch**: `apply-generic`, many more...

**Production primitives** ([compileText.ts:77](../src/core/pipeline/compileText.ts#L77)) - **5 PRIMITIVES ONLY**:
- `+`, `-`, `=`, `not`, `unit`

**Gap**: 112 primitives (96%) exist ONLY in tests, not in production.

### Why The REPL and Tests Work (But Production Doesn't)

The REPL and tests **bypass** the production `OmegaRuntime` class entirely:

| Component | Code Path | Primitives | Status |
|-----------|-----------|------------|--------|
| **CLI REPL** | `import { installPrims } from "../test/helpers/prims"` | 117 | Works |
| **Tests** | `import { installPrims } from "../helpers/prims"` | 117 | Works |
| **omegaHarness** | `import { installPrims } from "./prims"` | 117 | Works |
| **OmegaRuntime** | `compileTextToExpr()` → `initialEnv()` | 5 | **BROKEN** |

The REPL works because [omega-repl.ts:28](../bin/omega-repl.ts#L28) imports directly from test helpers:
```typescript
import { installPrims } from "../test/helpers/prims";  // <-- 117 primitives!
```

The `OmegaRuntime` class in [runtime.ts](../src/runtime.ts) is **the only broken path** - but it's also the **only production API**.

### Required Fix

**Option 1**: Copy `test/helpers/prims.ts` to `src/core/pipeline/prims.ts` and wire it into `compileTextToExpr`

**Option 2**: Register primitives in `initialEnv()` like they should be:
```typescript
function initialEnv(moduleScope: string): Env {
  // ALL primitives need to be here, not just 5
  const prims = [
    "+", "-", "*", "/", "<", "<=", "=", ">=", ">",
    "cons", "car", "cdr", "null?", "pair?", "list", "length", "append", "reverse",
    "map", "filter", "foldr", "foldl", "apply",
    "stream-car", "stream-cdr", "stream-null?", "the-empty-stream",
    // ... ALL 117 primitives
  ];
  // ...
}
```

### Missing Primitive Registrations

These functions exist in TypeScript but are **NOT CALLABLE FROM LISP**:

| Missing Primitive | TypeScript Function | Status |
|-------------------|---------------------|--------|
| `stream-car` | `streamCar()` | **NOT REGISTERED** |
| `stream-cdr` | `streamCdr()` | **NOT REGISTERED** |
| `stream-null?` | `isStreamNull()` | **NOT REGISTERED** |
| `the-empty-stream` | `emptyStream()` | **NOT REGISTERED** |
| `stream-map` | `streamMap()` | **NOT REGISTERED** |
| `stream-filter` | `streamFilter()` | **NOT REGISTERED** |
| `stream-take` | `streamTake()` | **NOT REGISTERED** |
| `stream-append` | `streamAppend()` | **NOT REGISTERED** |
| `stream->list` | `streamToList()` | **NOT REGISTERED** |
| `list->stream` | `listToStream()` | **NOT REGISTERED** |

### Required Fix

1. Create proper Lisp primitives in [primitives.ts](../src/core/meta/primitives.ts):
```typescript
const primStreamCar = makePrim("stream-car", (args) => {
  if (args.length === 0 || !isPair(args[0])) {
    throw new Error("stream-car expects a stream");
  }
  return car(args[0] as Omega0Pair);
});
// etc. for all stream primitives
```

2. Register them in `createBaseEnv()`:
```typescript
env = defineVar(env, "stream-car", primStreamCar);
env = defineVar(env, "stream-cdr", primStreamCdr);
env = defineVar(env, "stream-null?", primStreamNull);
// etc.
```

### Also Wrong: camelCase Function Names

The TypeScript functions themselves use invalid Lisp naming (camelCase instead of hyphenated):

| TypeScript (WRONG) | Lisp Convention |
|--------------------|-----------------|
| `streamCar` | `stream-car` |
| `consStream` | `cons-stream` |
| `isStreamNull` | `stream-null?` |
| `listToStream` | `list->stream` |
| `streamToList` | `stream->list` |
| `runNondet` | `run-nondet` |
| `isEvalExhausted` | `budget-exhausted?` |

**Note**: `streamCar` in TypeScript is NOT VALID in any Lisp dialect. Lisp symbols are case-insensitive and use hyphens, not camelCase. `streamCar` would be read as `STREAMCAR` - a completely different symbol from `STREAM-CAR`.

---

## Vocabulary & Architecture Alignment

### The Projects Are NOT Cleanly Orthogonal

Based on analysis against [REFERENCE-ALGEBRA.md](./REFERENCE-ALGEBRA.md) and [ARCHITECTURE-EXPLANATION.md](./ARCHITECTURE-EXPLANATION.md):

- ~30% **identical concepts with different names** (easy migration)
- ~40% **similar concepts at different abstraction layers** (need layer decisions)
- ~30% **genuinely unique capabilities** (keep or port)

### The Correct Layering (from REFERENCE-ALGEBRA.md)

```
┌─────────────────────────────────────────────────────────────────┐
│ LAYER 4: DOMAIN DSLs                                            │
│   - LambdaRLM: Domain Algebra, Meta-Search, Composable Solvers  │
│   - LambdaLLM: Experts, Sessions (specs only)                   │
└─────────────────────────────────────────────────────────────────┘
                              ▲
┌─────────────────────────────────────────────────────────────────┐
│ LAYER 3: PROTOCOL LIBRARIES                                     │
│   - chat-turn, tool-loop, rag, graph/workflow                   │
│   - ALL PROJECTS MISSING THIS - NEEDS IMPLEMENTATION            │
└─────────────────────────────────────────────────────────────────┘
                              ▲
┌─────────────────────────────────────────────────────────────────┐
│ LAYER 2: EXECUTION ALGEBRA (Flow combinators)                   │
│   OmegaLLM: concurrency/, effects/nondet/, constraints/         │
│   LambdaRLM: nondet.lisp, repair_loop.lisp, composable.lisp     │
│   → MERGE: Port LambdaRLM patterns to OmegaLLM                  │
└─────────────────────────────────────────────────────────────────┘
                              ▲
┌─────────────────────────────────────────────────────────────────┐
│ LAYER 1: PROMPT ARTIFACT ALGEBRA (pure)                         │
│   - Prompt constructors, transformers, currying, quoting        │
│   → CREATE: Explicit prompt artifact library (MISSING)          │
└─────────────────────────────────────────────────────────────────┘
                              ▲
┌─────────────────────────────────────────────────────────────────┐
│ LAYER 0: KERNEL (eval/apply + kernel effects)                   │
│   → OmegaLLM IS THE CANONICAL LAYER 0                           │
│   src/core/eval/ + src/core/oracle/                             │
└─────────────────────────────────────────────────────────────────┘
```

### Key Decisions

1. **OmegaLLM is the canonical runtime** (Layer 0)
2. **Execution Algebra should merge** - Port LambdaRLM patterns to OmegaLLM
3. **Prompt Artifact Algebra is MISSING everywhere** - Needs creation
4. **Protocol Libraries are MISSING everywhere** - Needs implementation
5. **Domain DSLs stay separate** - Unique per project

---

## Same Concept, Different Names (Rename Required)

### Identical Implementations - Just Rename to SICP Conventions

| Concept | OmegaLLM (WRONG) | LambdaRLM (CORRECT) | Canonical Name |
|---------|------------------|---------------------|----------------|
| Lazy stream cons | `consStream` | `stream-cons` | `stream-cons` |
| Stream head | `streamCar` | `stream-car` | `stream-car` |
| Stream tail | `streamCdr` | `stream-cdr` | `stream-cdr` |
| Stream map | `streamMap` | `stream-map` | `stream-map` |
| Stream filter | `streamFilter` | `stream-filter` | `stream-filter` |
| Stream null check | `isStreamNull` | `stream-null?` | `stream-null?` |
| Nondet search | `runNondet` | `run-nondet` | `run-nondet` |
| Budget check | `isEvalExhausted` | `budget-exhausted?` | `budget-exhausted?` |
| Budget consume | `consumeOracleTurn` | `budget-consume` | `budget-consume` |

### Core Runtime Effect Types (Already Aligned)

| Formal Name (REFERENCE-ALGEBRA) | OmegaLLM | LambdaLLM | Notes |
|--------------------------------|----------|-----------|-------|
| **EffectReq** | `StepOutcome.Op` | `OracleReq` | Same concept |
| **EffectResp** | `StepOutcome` | `OracleResp` | Same concept |
| **EffectSession** | `AsyncGenerator` | `OracleSession` | Same concept |
| **RuntimeDriver** | `Runtime.dispatch()` | "switchboard" | Same concept |
| **Portal** | `oracle/portal.ts` | `PortalImpl` | Same concept |

---

## Legend

| Symbol | Meaning |
|--------|---------|
| **[file](path)** | Clickable link to source file |
| `:function` | Specific function/type in file |
| `[N funcs]` | Count of related functions |
| Checkmark | Feature is implemented/specified |
| `---` | Not present |

---

## 1. STREAMS (Lazy/SICP-style)

### Summary
All three implementation projects have streams. OmegaLLM has the most complete implementation but **uses wrong naming conventions**.

| Feature | LambdaRLM | LambdaLLM | OmegaLLM | FrameLisp |
|---------|-----------|-----------|----------|-----------|
| **Core** | [streams.lisp](../../../LambdaRLM/lib/streams.lisp) | --- | [stream.ts](../src/core/stream/stream.ts) | Spec only |
| **the-empty-stream** | `the-empty-stream` | --- | `emptyStream` (BUG) | --- |
| **stream-null?** | `stream-null?` | --- | `isStreamNull` (BUG) | --- |
| **stream-cons** | `stream-cons` | --- | `consStream` (BUG) | --- |
| **stream-car** | `stream-car` | --- | `streamCar` (BUG) | --- |
| **stream-cdr** | `stream-cdr` | --- | `streamCdr` (BUG) | --- |
| **stream-map** | `stream-map` | --- | `streamMap` (BUG) | `(stream-map f es)` |
| **stream-filter** | `stream-filter` | --- | `streamFilter` (BUG) | `(stream-filter p es)` |
| **stream-take** | `stream-take` | --- | `streamTake` (BUG) | `(stream-take n es)` |
| **stream-drop** | --- | --- | `streamDrop` (BUG) | --- |
| **stream-append** | `stream-append` | --- | `streamAppend` (BUG) | --- |
| **stream-flatmap** | `stream-flatmap` | --- | `streamFlatMap` (BUG) | --- |
| **stream-zip** | --- | --- | `streamZip` (BUG) | `(stream-zip es1 es2)` |
| **stream-fold** | --- | --- | `streamFold` (BUG) | --- |
| **stream-reduce** | --- | --- | `streamReduce` (BUG) | `(stream-reduce f init es)` |
| **stream-interleave** | `stream-interleave` | --- | **MISSING** | `(stream-merge)` |
| **list->stream** | `list->stream` | --- | `listToStream` (BUG) | --- |
| **stream->list** | `stream->list` | --- | `streamToList` (BUG) | --- |
| **integers-from** | `integers-from` | --- | `streamRange` (BUG) | --- |
| **Receipt-backed** | --- | --- | `materializeSegment` | --- |

### OmegaLLM Stream Files
- [stream.ts](../src/core/stream/stream.ts) - 35+ functions, SICP-style lazy streams (WRONG NAMES)
- [types.ts](../src/core/stream/types.ts) - StreamCell, StreamContext, StreamSegment
- [promise.ts](../src/core/stream/promise.ts) - Lazy promise implementation for stream tails

### Migration Notes
- **FIX REQUIRED**: Rename all OmegaLLM stream functions to SICP conventions
- **LambdaRLM -> OmegaLLM**: `stream-interleave` needs porting (fair merge)
- **OmegaLLM unique**: Receipt-backed streams for persistence

---

## 2. NONDETERMINISM (amb/choice)

### Summary
Both LambdaRLM and OmegaLLM have full nondeterminism. OmegaLLM has more frontier strategies.

| Feature | LambdaRLM | LambdaLLM | OmegaLLM | FrameLisp |
|---------|-----------|-----------|----------|-----------|
| **Core** | [nondet.lisp](../../../LambdaRLM/lib/nondet.lisp) | --- | [runner.ts](../src/core/effects/nondet/runner.ts) | --- |
| **unit** | `unit` | --- | --- | `(pure x)` |
| **mzero** | `mzero` | --- | **MISSING** | --- |
| **mplus** | `mplus` | --- | **MISSING** | --- |
| **bind** | `bind` | --- | **MISSING** (implicit) | `(bind m f)` |
| **choose/amb** | `choose`, `amb-list` | --- | via `amb.op` effect | --- |
| **guard/require** | `guard`, `require` | --- | via `ConstraintObs` | --- |
| **amb-fail** | `amb-fail` | --- | --- | --- |
| **run-nondet** | `run-nondet` | --- | `runNondet` (BUG) | --- |
| **run-all** | `run-all` | --- | `mode: "all"` | --- |
| **run-first** | `amb-one` | --- | `mode: "first"` | --- |
| **beam-select** | `beam-select` | --- | `frontier: "beam"` | --- |
| **DFS frontier** | implicit | --- | `frontier: "dfs"` | --- |
| **BFS frontier** | --- | --- | `frontier: "bfs"` | --- |
| **Sample frontier** | --- | --- | `frontier: "sample"` | --- |
| **Fair scheduling** | --- | --- | `quantumSteps` | --- |

### OmegaLLM Nondet Files
- [runner.ts](../src/core/effects/nondet/runner.ts) - Main search loop with frontier strategies
- [types.ts](../src/core/effects/nondet/types.ts) - NondetMode, FrontierKind, NondetPolicy, Job
- [frontier.ts](../src/core/effects/nondet/frontier.ts) - Frontier implementations

### LambdaRLM Nondet Features (28 functions)
```lisp
;; From lib/nondet.lisp:
unit, mzero, mplus, bind, choose, guard, require
amb-list, amb-1, amb-2, amb-3, amb-4, amb-5
amb-fail, amb-require, amb-collect, amb-one, amb-n
let-amb, run-nondet, run-all, run-first-matching, beam-select
```

### Migration Notes
- **PORT**: LambdaRLM monadic primitives (unit, mzero, mplus, bind) to OmegaLLM
- **OmegaLLM unique**: Fair scheduling with quantum, multiple frontier strategies, Job scoring

---

## 3. CONCURRENCY (Fibers/Actors)

### Summary
OmegaLLM has full SICP-style concurrency. LambdaRLM/LambdaLLM have minimal support.

| Feature | LambdaRLM | LambdaLLM | OmegaLLM | FrameLisp |
|---------|-----------|-----------|----------|-----------|
| **Core** | --- | [12-CONCURRENCY.md](../../../LambdaLLM/ARCHITECTURE/12-CONCURRENCY.md) | [concurrency/](../src/core/concurrency/) | --- |
| **Fiber** | --- | spec only | [types.ts](../src/core/concurrency/types.ts):FiberState | --- |
| **spawn** | --- | spec | `make-fiber` | --- |
| **join** | --- | spec | `BlockReason.join` | --- |
| **yield** | [yield.lisp](../../../LambdaRLM/lib/yield.lisp) | spec | `BlockReason.yield` | --- |
| **Mutex** | --- | spec | [sync.ts](../src/core/concurrency/sync.ts):MutexState | --- |
| **IVar** | --- | spec | `IVarState` | --- |
| **Channel** | --- | spec | `ChannelState` | --- |
| **Actor** | --- | spec | [actor.ts](../src/core/concurrency/actor.ts):ActorState | --- |
| **Scheduler** | --- | spec | [scheduler.ts](../src/core/concurrency/scheduler.ts):SchedulerState | --- |
| **Policies** | --- | --- | RoundRobin, FairRR, Random, Replay | --- |
| **Deadlock detection** | --- | --- | `SchedulerStatus.deadlock` | --- |
| **Budget** | --- | --- | `ConcurrencyBudget` | --- |

### OmegaLLM Concurrency Files (7 files)
- [types.ts](../src/core/concurrency/types.ts) - FiberState, FiberStatus, SchedulerState, MutexState, IVarState, ChannelState, ActorState
- [scheduler.ts](../src/core/concurrency/scheduler.ts) - Fiber scheduling with multiple policies
- [sync.ts](../src/core/concurrency/sync.ts) - Mutex, IVar operations
- [actor.ts](../src/core/concurrency/actor.ts) - Actor model implementation
- [singleflight.ts](../src/core/concurrency/singleflight.ts) - Deduplication for concurrent requests
- [critic.ts](../src/core/concurrency/critic.ts) - Concurrency analysis/checking

---

## 4. CONSTRAINTS (Propagation Networks)

### Summary
OmegaLLM has full constraint propagation networks. Unique to OmegaLLM.

| Feature | LambdaRLM | LambdaLLM | OmegaLLM | FrameLisp |
|---------|-----------|-----------|----------|-----------|
| **Core** | --- | --- | [constraints/](../src/core/constraints/) | --- |
| **Network** | --- | --- | [network.ts](../src/core/constraints/network.ts) | --- |
| **Propagator** | --- | --- | [engine.ts](../src/core/constraints/engine.ts):PropagatorState | --- |
| **Connector** | --- | --- | `ConnectorState` | --- |
| **run-propagation** | --- | --- | `runPropagation` | --- |
| **Contradiction** | --- | --- | `ContradictionVal`, `makeContradiction` | --- |
| **Explanation** | --- | --- | `ExplanationVal` | --- |
| **Scheduling** | --- | --- | fifo, priority, random | --- |
| **Diagnosis** | --- | --- | [diagnosis.ts](../src/core/constraints/diagnosis.ts) | --- |
| **Repair** | --- | --- | [repair.ts](../src/core/constraints/repair.ts) | --- |

### OmegaLLM Constraint Files (6 files)
- [types.ts](../src/core/constraints/types.ts) - NetworkState, PropagatorState, ConnectorState
- [network.ts](../src/core/constraints/network.ts) - Network operations, connector get/set
- [engine.ts](../src/core/constraints/engine.ts) - Propagation loop, scheduling strategies
- [diagnosis.ts](../src/core/constraints/diagnosis.ts) - Contradiction analysis
- [repair.ts](../src/core/constraints/repair.ts) - Automated repair strategies

---

## 5. GENERIC DISPATCH (Coercion/Synthesis)

### Summary
OmegaLLM has full generic dispatch with coercion. Unique to OmegaLLM.

| Feature | LambdaRLM | LambdaLLM | OmegaLLM | FrameLisp |
|---------|-----------|-----------|----------|-----------|
| **Core** | --- | --- | [generic/](../src/core/generic/) | --- |
| **Registry** | --- | --- | [registry.ts](../src/core/generic/registry.ts) | --- |
| **Dispatch** | --- | --- | [dispatch.ts](../src/core/generic/dispatch.ts) | --- |
| **Coercion** | --- | --- | [coercion.ts](../src/core/generic/coercion.ts) | --- |
| **Synthesis** | --- | --- | [synthesis.ts](../src/core/generic/synthesis.ts) | --- |
| **resolve-dispatch** | --- | --- | `resolveDispatch` | --- |
| **apply-generic** | --- | --- | `applyGeneric` | --- |
| **Method lookup** | --- | --- | exact, coerced, miss | --- |
| **Miss handler** | --- | --- | `applyGenericWithMissHandler` | --- |

### OmegaLLM Generic Files (6 files)
- [types.ts](../src/core/generic/types.ts) - TypeTag, TypeSignature, MethodEntry
- [registry.ts](../src/core/generic/registry.ts) - Method registry, lookup
- [dispatch.ts](../src/core/generic/dispatch.ts) - Generic dispatch with coercion
- [coercion.ts](../src/core/generic/coercion.ts) - Type coercion paths
- [synthesis.ts](../src/core/generic/synthesis.ts) - Method synthesis for misses

---

## 6. BUDGET MANAGEMENT

### Summary
All projects have budget management with different approaches.

| Feature | LambdaRLM | LambdaLLM | OmegaLLM | FrameLisp |
|---------|-----------|-----------|----------|-----------|
| **Core** | [budget.lisp](../../../LambdaRLM/lib/budget.lisp) | [25-BUDGET.md](../../../LambdaLLM/ARCHITECTURE/25-BUDGET.md) | [budgets.ts](../src/core/governance/budgets.ts) | `(with-budget)` |
| **make-budget** | `make-budget` | `Budget.constructor` | `budgetDefault` | --- |
| **budget?** | `budget?` | --- | --- | --- |
| **budget-llm-calls** | `budget-llm-calls` | `Budget.consumed` | `oracleTurns` | --- |
| **budget-tokens** | `budget-tokens` | `consumeTokens` | --- | --- |
| **budget-remaining** | `budget-remaining` | `Budget.remaining` | `budgetRemaining` | --- |
| **budget-consume** | `budget-consume` | `consumeIteration` | `consumeOracleTurn` | --- |
| **budget-exhausted?** | `budget-exhausted?` | `hasRemaining` | `isEvalExhausted` | --- |
| **budget-split** | `budget-split` | --- | **MISSING** | --- |
| **budget-allocate** | `budget-allocate` | --- | **MISSING** | --- |
| **BudgetTracker** | --- | `BudgetTracker` | `BudgetTracker` | --- |

### LambdaRLM Budget Functions (10 functions)
```lisp
;; From lib/budget.lisp:
make-budget, budget?, budget-llm-calls, budget-tokens, budget-time-ms
budget-remaining, budget-consume, budget-exhausted?, budget-split, budget-allocate
```

### OmegaLLM Governance Files
- [budgets.ts](../src/core/governance/budgets.ts) - BudgetTracker, BudgetLimits, consumption
- [caps.ts](../src/core/governance/caps.ts) - Capability limits
- [profile.ts](../src/core/governance/profile.ts) - Execution profiles
- [enforcement.ts](../src/core/governance/enforcement.ts) - Policy enforcement

### Migration Notes
- **PORT**: `budget-split` and `budget-allocate` from LambdaRLM (for parallel work allocation)

---

## 7. RESULTS / OUTCOMES

### Summary
Different result types across projects.

| Feature | LambdaRLM | LambdaLLM | OmegaLLM | FrameLisp |
|---------|-----------|-----------|----------|-----------|
| **Core** | [results.lisp](../../../LambdaRLM/lib/results.lisp) | [27-OUTCOMES.md](../../../LambdaLLM/ARCHITECTURE/27-OUTCOMES.md) | [machine.ts](../src/core/eval/machine.ts) | `(pure x)` |
| **success/ok** | `success` | `ok()` | `StepOutcome.Done` | --- |
| **partial** | `partial` | --- | --- | --- |
| **failure/error** | `failure` | `ErrorOutcome` | `StepOutcome.Error` | --- |
| **proposed** | --- | `ProposedOutcome` | --- | --- |
| **nonconverged** | --- | `NonconvergedOutcome` | --- | --- |
| **cycle** | --- | `CycleOutcome` | --- | --- |
| **needs** | --- | `NeedsOutcome` | --- | --- |
| **result-map** | `result-map` | --- | --- | `(mapf f flow)` |
| **result-bind** | `result-bind` | `bindOutcome()` | --- | `(bind flow f)` |

### LambdaRLM Results Functions (12 functions)
```lisp
;; From lib/results.lisp:
success, partial, failure
success?, partial?, failure?, result?
result-value, result-metadata, result-progress, failure-reason
result-map, result-bind
```

---

## 8. DOMAIN ALGEBRA (Unique to LambdaRLM)

### Summary
Algebraic domain modeling - unique to LambdaRLM.

| Feature | LambdaRLM | LambdaLLM | OmegaLLM | FrameLisp |
|---------|-----------|-----------|----------|-----------|
| **Core** | [domain_algebra.lisp](../../../LambdaRLM/lib/domain_algebra.lisp) | --- | --- | --- |
| **define-sort** | `define-sort` | --- | --- | --- |
| **define-operation** | `define-operation` | --- | --- | --- |
| **define-equation** | `define-equation` | --- | --- | --- |
| **algebra-simplify** | `algebra-simplify` | --- | --- | --- |
| **algebra-unify** | `algebra-unify` | --- | --- | --- |
| **with-domain-algebra** | `with-domain-algebra` | --- | --- | --- |

### LambdaRLM Domain Algebra Files
- [domain_algebra.lisp](../../../LambdaRLM/lib/domain_algebra.lisp) - Core algebra definitions
- [checks_domain_algebra_v2_interpreter.lisp](../../../LambdaRLM/lib/checks_domain_algebra_v2_interpreter.lisp) - Interpreter

### Migration Notes
- **Would need to write from scratch** for OmegaLLM
- **Benefit**: Formal reasoning about problem domains, automated simplification

---

## 9. META-SEARCH / STRATEGIES (Unique to LambdaRLM)

### Summary
Strategy selection and meta-level search - unique to LambdaRLM.

| Feature | LambdaRLM | LambdaLLM | OmegaLLM | FrameLisp |
|---------|-----------|-----------|----------|-----------|
| **Strategies** | [strategies.lisp](../../../LambdaRLM/lib/strategies.lisp) | --- | --- | --- |
| **Meta-search** | [meta_search.lisp](../../../LambdaRLM/lib/meta_search.lisp) | --- | --- | --- |
| **strategy-beam-search** | `strategy-beam-search` | --- | --- | --- |
| **strategy-depth-first** | `strategy-depth-first` | --- | --- | --- |
| **strategy-mcts** | `strategy-mcts` | --- | --- | --- |
| **meta-analyze** | `meta-analyze` | --- | --- | --- |
| **meta-select-strategy** | `meta-select-strategy` | --- | --- | --- |
| **meta-solve** | `meta-solve` | --- | --- | --- |

### Migration Notes
- **Would need to write from scratch** for OmegaLLM
- **Benefit**: Adaptive problem solving, strategy selection based on problem features

---

## 10. COMPOSABLE SOLVERS (Unique to LambdaRLM)

### Summary
Solver composition primitives - unique to LambdaRLM.

| Feature | LambdaRLM | LambdaLLM | OmegaLLM | FrameLisp |
|---------|-----------|-----------|----------|-----------|
| **Core** | [composable.lisp](../../../LambdaRLM/lib/composable.lisp) | --- | --- | `(defop name)` |
| **make-solver** | `make-solver` | --- | --- | --- |
| **solver?** | `solver?` | --- | --- | --- |
| **solver-solve** | `solver-solve` | --- | --- | --- |
| **solver-estimate** | `solver-estimate` | --- | --- | --- |
| **compose-sequential** | `compose-sequential` | --- | --- | `(bind flow f)` |
| **compose-parallel** | `compose-parallel` | --- | --- | `(all flows)` |
| **compose-fallback** | `compose-fallback` | --- | --- | `(race flows)` |

---

## 11. OBLIGATIONS / CLOSURES (Unique to LambdaRLM)

### Summary
Obligation tracking and closure requirements - unique to LambdaRLM.

| Feature | LambdaRLM | LambdaLLM | OmegaLLM | FrameLisp |
|---------|-----------|-----------|----------|-----------|
| **Core** | [obligations.lisp](../../../LambdaRLM/lib/obligations.lisp) | --- | --- | --- |
| **obligation** | `obligation` | --- | --- | --- |
| **closure** | `closure` | --- | --- | --- |
| **require-closure** | `require-closure` | --- | --- | --- |
| **discharge-with-evidence** | `discharge-with-evidence` | --- | --- | --- |

---

## 12. PROVENANCE / EVIDENCE

### Summary
Evidence tracking across projects.

| Feature | LambdaRLM | LambdaLLM | OmegaLLM | FrameLisp |
|---------|-----------|-----------|----------|-----------|
| **Core** | [provenance.lisp](../../../LambdaRLM/lib/provenance.lisp) | [22-PROVENANCE.md](../../../LambdaLLM/ARCHITECTURE/22-PROVENANCE.md) | partial in constraints | --- |
| **evidence?** | `evidence?` | `Evidence` | --- | --- |
| **evidence-id** | `evidence-id` | `EvidenceId` | --- | --- |
| **validate-evidence** | `validate-evidence` | `verifyEvidence()` | --- | --- |
| **evidence-stale?** | `evidence-stale?` | `evidence/stale?` | --- | --- |
| **EpistemicMode** | `valid-epistemic-mode?` | `EpistemicMode` | --- | --- |
| **TypedClaim** | --- | `TypedClaim` | --- | --- |
| **ProvenanceGraph** | --- | `ProvenanceGraph` | --- | --- |
| **captureEvidence** | --- | `captureEvidence()` | --- | --- |

### Receipt/Provenance Alignment (per ARCHITECTURE-EXPLANATION.md RSR-03)

| Project | Has Receipts? | Has Replay? | Has Provenance DAG? |
|---------|---------------|-------------|---------------------|
| OmegaLLM | Partial (`receipts.ts`) | Partial | No |
| LambdaRLM | `provenance.lisp` | No | Partial |
| LambdaLLM | Spec only | Spec only | Spec only |

**Verdict**: None have complete RSR-03. **Implement full receipt ledger in OmegaLLM**.

---

## 13. FACTS (Unique to LambdaLLM)

### Summary
Fact store and assertion - unique to LambdaLLM spec.

| Feature | LambdaRLM | LambdaLLM | OmegaLLM | FrameLisp |
|---------|-----------|-----------|----------|-----------|
| **Core** | --- | [23-FACTS.md](../../../LambdaLLM/ARCHITECTURE/23-FACTS.md) | --- | --- |
| **FactStore** | --- | `FactStore` | --- | --- |
| **assert** | --- | `assert` | --- | --- |
| **fact?** | --- | `fact?` | --- | --- |
| **facts** | --- | `facts` | --- | --- |
| **retract** | --- | `retract` | --- | --- |

---

## 14. FIXPOINT (Unique to LambdaLLM)

### Summary
Fixpoint iteration - unique to LambdaLLM spec.

| Feature | LambdaRLM | LambdaLLM | OmegaLLM | FrameLisp |
|---------|-----------|-----------|----------|-----------|
| **Core** | --- | [24-FIXPOINT.md](../../../LambdaLLM/ARCHITECTURE/24-FIXPOINT.md) | --- | --- |
| **eval-fixpoint** | --- | `evalFixpoint` | --- | --- |
| **compute-state-signature** | --- | `computeStateSignature` | --- | --- |
| **detect-cycle** | --- | `detectCycle` | --- | --- |

---

## 15. EXPERTS / SESSIONS (Unique to LambdaLLM)

### Summary
Expert roles and session management - unique to LambdaLLM spec.

| Feature | LambdaRLM | LambdaLLM | OmegaLLM | FrameLisp |
|---------|-----------|-----------|----------|-----------|
| **Experts** | --- | [29-EXPERTS.md](../../../LambdaLLM/ARCHITECTURE/29-EXPERTS.md) | --- | --- |
| **Sessions** | --- | [28-SESSION.md](../../../LambdaLLM/ARCHITECTURE/28-SESSION.md) | --- | --- |
| **ExpertRole** | --- | `ExpertRole` | --- | --- |
| **TaskEnvelope** | --- | `TaskEnvelope` | --- | --- |
| **Session** | --- | `Session` | --- | --- |
| **SessionPolicy** | --- | `SessionPolicy` | --- | --- |
| **AuditLog** | --- | `AuditLog` | --- | --- |

---

## 16. ARTIFACTS / CAS (Content-Addressable Storage)

### Summary
Artifact management across projects.

| Feature | LambdaRLM | LambdaLLM | OmegaLLM | FrameLisp |
|---------|-----------|-----------|----------|-----------|
| **Core** | --- | [26-ARTIFACTS.md](../../../LambdaLLM/ARCHITECTURE/26-ARTIFACTS.md) | [artifacts/](../src/core/artifacts/) | --- |
| **CAS** | --- | `ArtifactStore` | [cas.ts](../src/core/artifacts/cas.ts) | --- |
| **Registry** | --- | `Registry` | [registry.ts](../src/core/artifacts/registry.ts) | --- |
| **Hash** | --- | `ContentHash` | [hash.ts](../src/core/artifacts/hash.ts) | --- |
| **memo** | --- | `memo` | --- | --- |
| **DependencyTracker** | --- | `DependencyTracker` | --- | --- |

---

## 17. ORACLE / LLM INTERFACE

### Summary
LLM interface across projects.

| Feature | LambdaRLM | LambdaLLM | OmegaLLM | FrameLisp |
|---------|-----------|-----------|----------|-----------|
| **Core** | via Python | [08-PROTOCOL.md](../../../LambdaLLM/ARCHITECTURE/08-PROTOCOL.md) | [oracle/](../src/core/oracle/) | `(oracle/call)` |
| **Portal** | --- | spec | [portal.ts](../src/core/oracle/portal.ts) | --- |
| **Driver** | --- | spec | [driver.ts](../src/core/oracle/driver.ts) | --- |
| **Adapter** | --- | spec | [adapter.ts](../src/core/oracle/adapter.ts) | --- |
| **Anthropic** | --- | --- | [anthropic.ts](../src/core/oracle/plugins/anthropic.ts) | --- |
| **OpenAI** | --- | --- | [openai.ts](../src/core/oracle/plugins/openai.ts) | --- |
| **Ollama** | --- | --- | [ollama.ts](../src/core/oracle/plugins/ollama.ts) | --- |
| **Scripted** | --- | --- | [scriptedOracle.ts](../src/core/oracle/scriptedOracle.ts) | --- |
| **Receipts** | --- | --- | [receipts.ts](../src/core/oracle/receipts.ts) | --- |

---

## 18. EVALUATION / MACHINE

### Summary
Core evaluation machinery.

| Feature | LambdaRLM | LambdaLLM | OmegaLLM | FrameLisp |
|---------|-----------|-----------|----------|-----------|
| **Core** | Python eval | [04-EVALUATOR.md](../../../LambdaLLM/ARCHITECTURE/04-EVALUATOR.md) | [eval/](../src/core/eval/) | --- |
| **Machine** | --- | spec | [machine.ts](../src/core/eval/machine.ts) | --- |
| **Step** | --- | spec | [machineStep.ts](../src/core/eval/machineStep.ts) | --- |
| **Runtime** | --- | spec | [runtime.ts](../src/core/eval/runtime.ts) | --- |
| **Environment** | --- | `Environment` | [env.ts](../src/core/eval/env.ts) | --- |
| **Store** | --- | --- | [store.ts](../src/core/eval/store.ts) | --- |
| **Values** | --- | [02-TYPES.md](../../../LambdaLLM/ARCHITECTURE/02-TYPES.md) | values.ts | --- |

---

## Kernel Effects (per REFERENCE-ALGEBRA.md)

These MUST be in Layer 0 (core eval/apply):

| Primitive | OmegaLLM Location | Status |
|-----------|-------------------|--------|
| `infer` | `oracle/portal.ts` + machine effect handler | Implemented |
| `embed` | `oracle/` (partial) | Partial |
| `retrieve` | --- | **MISSING** |
| `call-tool` | `ReqTool` in machine | Implemented |
| `emit/observe` | Partial in effects/ | Partial |
| `validate` | Partial in constraints/ | Should be Core |
| `commit` | `store.ts` COWStore | Implemented |

---

## File Reference Summary

### LambdaRLM (28 files in lib/)
```
budget.lisp          - Budget management
results.lisp         - Result types (success/partial/failure)
failure.lisp         - Failure handling and recovery hints
context.lisp         - Execution context
composable.lisp      - Composable solvers
yield.lisp           - Yield primitive
streams.lisp         - SICP-style lazy streams
streams_v1.lisp      - Legacy streams
strategies.lisp      - Search strategies
nondet.lisp          - Nondeterminism (amb)
domain_algebra.lisp  - Algebraic domain modeling
compile_solver.lisp  - Solver compilation
meta_search.lisp     - Meta-level strategy selection
provenance.lisp      - Evidence tracking
schema_v2.lisp       - Schema definitions
core_utils_v1.lisp   - Core utilities
obligations.lisp     - Obligation/closure tracking
session_v2.lisp      - Session management
normalize.lisp       - Normalization utilities
scoring.lisp         - Scoring functions
repair_loop.lisp     - Repair iteration
approach_validator.lisp
validators_java.lisp
objectives_java_v1.lisp
design_validators_java.lisp
certificate_plan_v2.lisp
fair_search_refactor_v3.lisp
checks_domain_algebra_v2_interpreter.lisp
```

### LambdaLLM (Major ARCHITECTURE/*.md files)
```
00-SPECIFICATION.md  - Overview
01-READER.md         - S-expression reader
02-TYPES.md          - Value types
03-ENVIRONMENT.md    - Environment model
04-EVALUATOR.md      - Core evaluator
05-CONTINUATIONS.md  - Continuation support
06-CONDITIONS.md     - Condition/restart system
07-FFI.md            - Foreign function interface
08-PROTOCOL.md       - Oracle protocol
09-MODULES.md        - Module system
10-PERSISTENCE.md    - State persistence
11-MACROS.md         - Macro system
12-CONCURRENCY.md    - Concurrency model
13-MEMORY.md         - Memory management
14-STDLIB.md         - Standard library
22-PROVENANCE.md     - Evidence/provenance
23-FACTS.md          - Fact store
24-FIXPOINT.md       - Fixpoint iteration
25-BUDGET.md         - Budget management
26-ARTIFACTS.md      - Artifact store
27-OUTCOMES.md       - Outcome types
28-SESSION.md        - Session management
29-EXPERTS.md        - Expert roles
32-LANGUAGE-OFFICIAL-*.md - Official language spec (19 parts)
```

### OmegaLLM (src/core/ directories)
```
eval/           - Core evaluation (machine, runtime, env, store, values)
stream/         - SICP-style streams (stream.ts, types.ts, promise.ts) [NAMING BUG]
effects/        - Effect handling
  nondet/       - Nondeterminism (runner.ts, types.ts, frontier.ts)
  search/       - Search operations
concurrency/    - Fibers, actors, channels, mutexes, IVars (7 files)
constraints/    - Propagation networks (6 files)
generic/        - Generic dispatch with coercion (6 files)
governance/     - Budgets, caps, profiles (4 files)
oracle/         - LLM interface (15+ files)
artifacts/      - Content-addressable storage (cas.ts, registry.ts, hash.ts)
modules/        - Module system
syntax/         - Syntax handling
macro/          - Macro expansion
reader/         - S-expression reading
pipeline/       - Compilation pipeline
```

---

## Migration Priorities

### P0: Critical Fixes

| Task | Effort | Impact | Status |
|------|--------|--------|--------|
| ~~**Wire production primitives into OmegaRuntime**~~ | Low | **BLOCKING** - Production API is non-functional | ✅ DONE |
| ~~**[Fix OmegaLLM naming conventions](../CLAUDE-JOBS/002-FIX-PRIMITIVE-NAMING-CONVENTIONS.md)**~~ | Medium | High - 17 primitives renamed to use `-` (SICP-compliant) | ✅ DONE |
| **Implement full RSR-03 receipt ledger** | Medium | Critical - Provenance/replay foundation | --- |

### P1: High-Value Ports
| Component | From | To | Effort | Benefit |
|-----------|------|-----|--------|---------|
| `unit/mzero/mplus/bind` | LambdaRLM | OmegaLLM | Medium | Monadic combinators |
| `repair_loop.lisp` pattern | LambdaRLM | OmegaLLM | Medium | Explicit retry-until |
| `stream-interleave` | LambdaRLM | OmegaLLM | Low | Fair stream merge |
| `budget-split/allocate` | LambdaRLM | OmegaLLM | Low | Parallel work allocation |

### P2: New Implementations
| Component | Spec Source | Effort | Priority |
|-----------|-------------|--------|----------|
| **Prompt Artifact Algebra** | REFERENCE-ALGEBRA.md | Medium | P1 |
| **Protocol Libraries** | REFERENCE-ALGEBRA.md | High | P2 |
| **Facts** | LambdaLLM 23-FACTS.md | Medium | P1 |
| **Fixpoint** | LambdaLLM 24-FIXPOINT.md | Medium | P1 |
| **retrieve** primitive | REFERENCE-ALGEBRA.md | Medium | P2 |

### P3: Optional Domain DSLs
| Component | From | Effort | Notes |
|-----------|------|--------|-------|
| **Domain Algebra** | LambdaRLM | High | Unique - port if needed |
| **Meta-Search** | LambdaRLM | High | Unique - port if needed |
| **Experts/Sessions** | LambdaLLM | High | Implement from spec |

---

## Summary Statistics

| Metric | LambdaRLM | LambdaLLM | OmegaLLM | FrameLisp |
|--------|-----------|-----------|----------|-----------|
| **Files** | 28 | 71 | 90+ | 1 |
| **Language** | Lisp | TS specs | TypeScript | Algebraic |
| **Status** | Impl | Spec | Impl | Spec |
| **Streams** | Yes | No | Yes (WRONG NAMES) | Spec |
| **Nondet** | Yes | No | Yes | No |
| **Concurrency** | Minimal | Spec | Full | No |
| **Constraints** | No | No | Full | No |
| **Generic** | No | No | Full | No |
| **Budget** | Yes | Spec | Yes | Spec |
| **Domain Algebra** | Yes | No | No | No |
| **Meta-Search** | Yes | No | No | No |
| **Facts** | No | Spec | No | No |
| **Fixpoint** | No | Spec | No | No |
| **Experts** | No | Spec | No | No |

---

*Document generated: 2026-01-19*
*Verified against actual source files*
*Based on REFERENCE-ALGEBRA.md and ARCHITECTURE-EXPLANATION.md formal specifications*
