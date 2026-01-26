# OmegaLLM Traceability Matrix

> **Purpose**: Map implementation → tests → demos → requirements
>
> **Approach**: Code-first - start from what's ACTUALLY IMPLEMENTED
>
> **Last Updated**: 2026-01-23
>
> **Totals**: 165 primitives, 110 test files, 27 demo chapters

---

## Executive Summary

| Area | Primitives | Test Dir | Test Files | Demo | Status |
|------|------------|----------|------------|------|--------|
| **Continuations** | 3 | `test/continuations/` | 2 | ch11 | ✅ COMPLETE |
| **Conditions** | 7 | `test/conditions/` | 3 | - | ✅ COMPLETE |
| **Monadic** | 4 | `test/monad/` | 2 | ch04 | ✅ COMPLETE |
| **Streams** | 11 | `test/streams/`, `test/prompt16-stream/` | 3 | ch15 | ✅ COMPLETE |
| **Provenance** | 5 | `test/provenance/` | 3 | - | ✅ COMPLETE |
| **Solver** | 22 | `test/solver/` | 5 | ch21 | ✅ COMPLETE |
| **Generic Dispatch** | 18 | `test/generic/`, `test/prompt14-generic/` | 2 | ch14 | ✅ COMPLETE |
| **Machine Introspection** | 13 | `test/prompt8/` | 1 | ch08 | ✅ COMPLETE |
| **Evidence** | 3 | `test/evidence/` | 1 | - | ✅ COMPLETE |
| **Lists** | 18 | (integrated in most tests) | ~50+ | ch02 | ✅ USED |
| **Strings** | 12 | (integrated) | ~20+ | - | ✅ USED |
| **Arithmetic** | 11 | (integrated) | ~80+ | ch01 | ✅ USED |
| **Higher-Order** | 8 | (integrated) | ~30+ | ch04 | ✅ USED |
| **Distributions** | 10 | (integrated) | ~5+ | - | ⚠️ MINIMAL |
| **Term Rewriting** | 8 | `test/semantic-rewriting/` | 1 | ch06 | ⚠️ MINIMAL |
| **Oracle/Effects** | - | `test/oracle/` | 13 | ch01-07 | ✅ COMPLETE |
| **Compiler** | - | `test/compiler/` | 6 | ch17 | ✅ COMPLETE |
| **REPL/Debugger** | - | `test/repl/` | 5 | - | ✅ COMPLETE |
| **Session/Replay** | - | `test/session/` | 5 | - | ✅ COMPLETE |

---

## Primitive → Test File Mapping

### 1. Continuations (3 primitives)

**Implementation**: `src/core/prims.ts:222-300`

| Primitive | Line | Test Files |
|-----------|------|------------|
| `call/cc` | 222 | `test/continuations/call-cc.spec.ts` |
| `call-with-prompt` | 240 | `test/continuations/delimited.spec.ts` |
| `abort-to-prompt` | 260 | `test/continuations/delimited.spec.ts` |

### 2. Conditions (7 primitives)

**Implementation**: `src/core/conditions/prims.ts`

| Primitive | Test Files |
|-----------|------------|
| `signal` | `test/conditions/signal.spec.ts` |
| `error` | `test/conditions/error.spec.ts` |
| `handler-bind` | `test/conditions/signal.spec.ts` |
| `restart-bind` | `test/conditions/restart.spec.ts` |
| `invoke-restart` | `test/conditions/restart.spec.ts` |
| `find-restart` | `test/conditions/restart.spec.ts` |
| `compute-restarts` | `test/conditions/restart.spec.ts` |

### 3. Monadic Primitives (4 primitives)

**Implementation**: `src/core/prims.ts:185-220`

| Primitive | Line | Test Files |
|-----------|------|------------|
| `unit` | 194 | `test/monad/primitives.spec.ts`, `test/monad/laws.spec.ts` |
| `mzero` | 196 | `test/monad/primitives.spec.ts`, `test/monad/laws.spec.ts` |
| `mplus` | 198 | `test/monad/primitives.spec.ts`, `test/monad/laws.spec.ts` |
| `bind` | 202 | `test/monad/primitives.spec.ts`, `test/monad/laws.spec.ts` |

### 4. Streams (11 primitives)

**Implementation**: `src/core/prims.ts:1174-1598`

| Primitive | Line | Test Files |
|-----------|------|------------|
| `the-empty-stream` | 1214 | `test/streams/stream-interleave.spec.ts` |
| `stream-null?` | 1217 | `test/prompt16-stream/stream.spec.ts` |
| `stream-car` | 1224 | `test/prompt16-stream/stream.spec.ts` |
| `stream-cdr` | 1233 | `test/prompt16-stream/stream.spec.ts` |
| `list->stream` | 1274 | `test/prompt16-stream/stream.spec.ts` |
| `stream->list` | 1283 | `test/prompt16-stream/stream.spec.ts` |
| `stream-take` | 1330 | `test/prompt16-stream/stream.spec.ts` |
| `stream-interleave` | 1409 | `test/streams/stream-interleave.spec.ts` |
| `stream-interleave-lazy` | 1416 | `test/streams/stream-interleave.spec.ts` |
| `stream-map` | 1433 | `test/prompt16-stream/stream.spec.ts` |
| `stream-filter` | 1532 | `test/prompt16-stream/stream.spec.ts` |

### 5. Provenance (5 primitives)

**Implementation**: `src/core/provenance/prims.ts`

| Primitive | Test Files |
|-----------|------------|
| `provenance-graph` | `test/provenance/graph.spec.ts` |
| `provenance-record` | `test/provenance/graph.spec.ts` |
| `provenance-trace` | `test/provenance/graph.spec.ts` |
| `provenance-check-staleness` | `test/provenance/staleness.spec.ts` |
| `evidence->val` | `test/provenance/graph.spec.ts` |

### 6. Solver (22 primitives)

**Implementation**: `src/core/solver/prims.ts`

| Primitive | Test Files |
|-----------|------------|
| `make-solver` | `test/solver/combinators.spec.ts` |
| `solver?` | `test/solver/combinators.spec.ts` |
| `solver-name` | `test/solver/combinators.spec.ts` |
| `solver-estimate` | `test/solver/combinators.spec.ts` |
| `solver-solve` | `test/solver/combinators.spec.ts` |
| `make-result` | `test/solver/combinators.spec.ts` |
| `make-estimate` | `test/solver/combinators.spec.ts` |
| `make-budget` | `test/solver/budget.spec.ts` |
| `budget-split` | `test/solver/budget.spec.ts` |
| `budget-allocate` | `test/solver/budget.spec.ts` |
| `compose-sequential` | `test/solver/combinators.spec.ts` |
| `compose-parallel` | `test/solver/combinators.spec.ts` |
| `compose-fallback` | `test/solver/combinators.spec.ts` |
| `compose-retry` | `test/solver/combinators.spec.ts` |
| `repair-until-valid` | `test/solver/repair.spec.ts` |
| `fixpoint` | `test/solver/fixpoint.spec.ts` |
| `fixpoint-detect-cycle` | `test/solver/fixpoint.spec.ts` |
| `make-fact-store` | `test/solver/facts.spec.ts` |
| `fact-store?` | `test/solver/facts.spec.ts` |
| `assert-fact` | `test/solver/facts.spec.ts` |
| `query-fact` | `test/solver/facts.spec.ts` |
| `query-facts` | `test/solver/facts.spec.ts` |

### 7. Generic Dispatch & Coercion (18 primitives)

**Implementation**: `src/core/prims.ts:1595-2082`

| Primitive | Line | Test Files |
|-----------|------|------------|
| `make-op-table` | 1600 | `test/generic/generic.spec.ts` |
| `op-table-put` | 1610 | `test/generic/generic.spec.ts` |
| `op-table-get` | 1639 | `test/generic/generic.spec.ts` |
| `op-table?` | 1670 | `test/generic/generic.spec.ts` |
| `attach-tag` | 1681 | `test/prompt14-generic/generic.spec.ts` |
| `type-tag` | 1699 | `test/prompt14-generic/generic.spec.ts` |
| `contents` | 1710 | `test/prompt14-generic/generic.spec.ts` |
| `tagged?` | 1721 | `test/prompt14-generic/generic.spec.ts` |
| `apply-generic` | 1733 | `test/prompt14-generic/generic.spec.ts` |
| `apply-generic-coerced` | 1774 | `test/prompt14-generic/generic.spec.ts` |
| `make-coercion-table` | 1860 | `test/prompt14-generic/generic.spec.ts` |
| `put-coercion` | 1870 | `test/prompt14-generic/generic.spec.ts` |
| `get-coercion` | 1896 | `test/prompt14-generic/generic.spec.ts` |
| `coercion-table?` | 1912 | `test/prompt14-generic/generic.spec.ts` |
| `find-coercion-path` | 1919 | `test/prompt14-generic/generic.spec.ts` |
| `find-all-coercion-paths` | 1968 | `test/prompt14-generic/generic.spec.ts` |
| `coerce-value` | 2015 | `test/prompt14-generic/generic.spec.ts` |

### 8. Machine Introspection (13 primitives)

**Implementation**: `src/core/prims.ts:2407-2680`

| Primitive | Line | Test Files |
|-----------|------|------------|
| `machine-new` | 2414 | `test/prompt8/machine-stepper.spec.ts` |
| `machine-step` | 2446 | `test/prompt8/machine-stepper.spec.ts` |
| `machine-run` | 2474 | `test/prompt8/machine-stepper.spec.ts` |
| `machine-stack` | 2520 | `test/prompt8/machine-stepper.spec.ts` |
| `machine-control` | 2538 | `test/prompt8/machine-stepper.spec.ts` |
| `machine-done?` | 2552 | `test/prompt8/machine-stepper.spec.ts` |
| `machine-value` | 2560 | `test/prompt8/machine-stepper.spec.ts` |
| `machine-fork` | 2577 | `test/prompt8/machine-stepper.spec.ts` |
| `machine-resume` | 2603 | `test/prompt8/machine-stepper.spec.ts` |
| `machine-add-breakpoint` | 2623 | `test/prompt8/machine-stepper.spec.ts` |
| `machine-step-count` | 2645 | `test/prompt8/machine-stepper.spec.ts` |
| `machine-last-op` | 2653 | `test/prompt8/machine-stepper.spec.ts` |
| `machine?` | 2665 | `test/prompt8/machine-stepper.spec.ts` |

### 9. Term Rewriting (8 primitives)

**Implementation**: `src/core/prims.ts:2075-2366`

| Primitive | Line | Test Files |
|-----------|------|------------|
| `make-rule` | 2084 | `test/semantic-rewriting/semantic-rewriting.spec.ts` |
| `make-rule-where` | 2107 | `test/semantic-rewriting/semantic-rewriting.spec.ts` |
| `rule?` | 2130 | `test/semantic-rewriting/semantic-rewriting.spec.ts` |
| `rewrite-once` | 2180 | `test/semantic-rewriting/semantic-rewriting.spec.ts` |
| `rewrite-fixpoint` | 2212 | `test/semantic-rewriting/semantic-rewriting.spec.ts` |
| `rewrite-trace` | 2242 | `test/semantic-rewriting/semantic-rewriting.spec.ts` |
| `rewrite-conflicts` | 2277 | `test/semantic-rewriting/semantic-rewriting.spec.ts` |
| `match-pattern` | 2304 | `test/semantic-rewriting/semantic-rewriting.spec.ts` |
| `substitute-template` | 2324 | `test/semantic-rewriting/semantic-rewriting.spec.ts` |

### 10. Evidence (3 primitives)

**Implementation**: `src/core/prims.ts:2368-2412`

| Primitive | Line | Test Files |
|-----------|------|------------|
| `evidence-id` | 2368 | `test/evidence/evidence.spec.ts` |
| `verify-evidence` | 2377 | `test/evidence/evidence.spec.ts` |
| `evidence-stale?` | 2383 | `test/evidence/evidence.spec.ts` |

### 11. Distributions (10 primitives)

**Implementation**: `src/core/prims.ts:393-536`

| Primitive | Line | Test Files |
|-----------|------|------------|
| `dist` | 398 | (integrated in semantic tests) |
| `dist?` | 405 | (integrated in semantic tests) |
| `dist-count` | 412 | (integrated in semantic tests) |
| `dist-value-at` | 420 | (integrated in semantic tests) |
| `dist-weight-at` | 429 | (integrated in semantic tests) |
| `dist-normalize` | 438 | (integrated in semantic tests) |
| `dist-sample` | 452 | (integrated in semantic tests) |
| `dist-topk` | 485 | (integrated in semantic tests) |
| `dist-from-list` | 497 | (integrated in semantic tests) |
| `dist-to-list` | 518 | (integrated in semantic tests) |

---

## Test Directory → Feature Mapping

| Test Directory | Feature Area | Test Count |
|----------------|--------------|------------|
| `test/continuations/` | Continuations | 2 |
| `test/conditions/` | Condition System | 3 |
| `test/monad/` | Monadic Primitives | 2 |
| `test/streams/` | Stream Primitives | 2 |
| `test/prompt16-stream/` | Stream Operations | 1 |
| `test/provenance/` | Provenance System | 3 |
| `test/solver/` | Solver Combinators | 5 |
| `test/generic/` | Generic Dispatch | 1 |
| `test/prompt14-generic/` | Generic + Coercion | 1 |
| `test/prompt8/` | Machine Introspection | 1 |
| `test/evidence/` | Evidence Primitives | 1 |
| `test/semantic-rewriting/` | Term Rewriting | 1 |
| `test/oracle/` | Oracle Protocol | 13 |
| `test/compiler/` | Compiler Pipeline | 6 |
| `test/repl/` | REPL & Debugger | 5 |
| `test/session/` | Session Management | 5 |
| `test/macros/` | Macro System | 2 |
| `test/amb/` | Nondeterminism | 1 |
| `test/frameir/` | Frame IR | 5 |
| `test/live/` | Live Integration | 7 |
| `test/demo/` | Demo Validation | 2 |

---

## Demo → Feature Mapping

| Demo File | Features Demonstrated |
|-----------|----------------------|
| `ch01-hello-world.lisp` | Basic Lisp, arithmetic |
| `ch02-first-class.lisp` | First-class procedures |
| `ch03-semantics.lisp` | Semantic values (Meaning) |
| `ch04-higher-order.lisp` | Higher-order functions |
| `ch05-distributions.lisp` | Distribution primitives |
| `ch06-semantic-rewrite.lisp` | Term rewriting |
| `ch07-oracle-control.lisp` | Oracle protocol |
| `ch08-machine-introspection.lisp` | Machine primitives |
| `ch09-governance.lisp` | Capability system |
| `ch10-modules.lisp` | Module system |
| `ch11-backtracking.lisp` | Nondeterminism, continuations |
| `ch12-constraints.lisp` | Constraint propagation |
| `ch13-concurrency.lisp` | Fibers, actors |
| `ch14-generic.lisp` | Generic dispatch |
| `ch15-streams.lisp` | Lazy streams |
| `ch16-compilation.lisp` | Compiler |
| `ch17-meta.lisp` | Metacircular evaluator |

---

## Requirements → Implementation Mapping

| Requirement Source | Implemented In | Coverage |
|--------------------|----------------|----------|
| LAMBDA-LLM--OMEGA-LLM-FEATURES.md | All core areas | 95%+ |
| REFERENCE-LIBRARIES.md | prims.ts | 100% |
| OMEGA-SPECS.md | prims.ts + subsystems | 100% |
| FrameLisp spec (84 items) | See below | 90%+ |

### FrameLisp Layer Coverage

| Layer | Description | Implementation Status |
|-------|-------------|----------------------|
| 4a | Data Model | ✅ Values, Meaning, Dist |
| 4b | Kernel Primitives | ✅ 131 primitives |
| 4c | Prompt Algebra | ✅ Oracle protocol |
| 4d | Execution Algebra | ✅ CEKS machine |
| 5a | Effect Algebra | ✅ Handlers, conditions |
| 5b | Nondeterminism | ✅ amb, mplus, mzero |
| 5c | Meta-evaluation | ✅ machine-* primitives |

---

## Coverage Statistics

```
Total Primitives:     165
  - prims.ts:         131
  - conditions:         7
  - provenance:         5
  - solver:            22

Total Test Files:     110
  - Unit tests:        85
  - Integration:       15
  - E2E/Live:          10

Demo Chapters:         27
  - With .lisp file:   27
  - With harness test:  2
```

---

## How to Verify

```bash
# Count primitives
grep -c 'def("' src/core/prims.ts src/core/*/prims.ts

# Run all tests
npx vitest run

# Run specific area tests
npx vitest run test/continuations/
npx vitest run test/conditions/
npx vitest run test/monad/

# Run demos
npx tsx demo/by-chapter/index.ts --chapter ch04-higher-order
```

---

## Auto-Generation

This matrix can be regenerated using:

```bash
# TypeScript version
npx tsx scripts/generate-traceability.ts

# OmegaLLM version (requires shell, file.read, file.write caps)
omega --file demo/lisp/auto-traceability.lisp \
      --caps shell,file.read,file.write \
      -e '(generate-traceability-matrix "docs/TRACE-AUTO.md")'
```
