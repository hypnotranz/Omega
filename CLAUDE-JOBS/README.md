# CLAUDE-JOBS

This folder contains detailed job specifications for implementing features in OmegaLLM. Each job is documented with enough detail that someone unfamiliar with the codebase can execute the work.

> **⚠️ BEFORE STARTING ANY JOB**:
> 1. Read **[LOGISTICS.md](./LOGISTICS.md)** - testing procedures, file locations, proof of completion
> 2. Read **[000-STRATEGY-AND-ARCHITECTURE.md](./000-STRATEGY-AND-ARCHITECTURE.md)** - strategy, what LambdaLLM has, abstraction boundaries

## Job Format

Each job file includes:
- **Executive Summary**: What to build and why
- **Dependencies**: What must be done first (blockers)
- **What Already Exists**: Foundation to build on
- **Implementation Plan**: Step-by-step instructions with code
- **Verification Steps**: How to confirm the work is correct
- **Checklist**: Task list for tracking progress

---

## Current Jobs

### Phase A: Core Magic (Foundation)

| Job ID | Title | Priority | Status | Depends On |
|--------|-------|----------|--------|------------|
| [001](./001-FIX-PRODUCTION-PRIMITIVES.md) | Fix Production Primitives | P0 - CRITICAL | ✅ DONE | - |
| [002](./002-FIX-PRIMITIVE-NAMING-CONVENTIONS.md) | Fix Primitive Naming Conventions | P1 - Important | ✅ DONE | 001 |
| [003](./003-COMPREHENSIVE-FEATURE-AUDIT-AND-LAYERING.md) | Comprehensive Feature Audit & Layering | P0 - Strategic | ✅ DONE | 001, 002 |
| [004](./004-IMPLEMENT-CORE-MAGIC.md) | Implement Core Magic (call/cc + Evidence) | P0 - Foundation | Гo. DONE | 001, 002, 003 |

### Phase B: Clean Composition

| Job ID | Title | Priority | Status | Depends On |
|--------|-------|----------|--------|------------|
| [005](./005-NON-UNWINDING-CONDITIONS.md) | Non-Unwinding Conditions | P1 - Important | ✅ DONE | **004** |
| [006](./006-MONADIC-PRIMITIVES.md) | Monadic Primitives (unit/bind/mzero/mplus) | P1 - Important | NOT STARTED | - (can parallel with 004) |
| [007](./007-FULL-PROVENANCE-SYSTEM.md) | Full Provenance System | P1 - Important | NOT STARTED | 004 (Task 2) |

### Phase C: Patterns

| Job ID | Title | Priority | Status | Depends On |
|--------|-------|----------|--------|------------|
| [008](./008-SEARCH-PATTERNS-SOLVERS.md) | Search Patterns & Composable Solvers | P2 - Nice to Have | NOT STARTED | 005, 006 |

---

## Dependency Graph

```
                       ┌─────────────────────────────────────────────────┐
                       │              PHASE A: Foundation                 │
                       │                                                  │
                       │    ┌─────┐    ┌─────┐    ┌─────┐    ┌─────┐    │
                       │    │ 001 │───▶│ 002 │───▶│ 003 │───▶│ 004 │    │
                       │    │Done │    │Done │    │Done │    │ TODO│    │
                       │    └─────┘    └─────┘    └─────┘    └──┬──┘    │
                       │                                        │        │
                       └────────────────────────────────────────┼────────┘
                                                                │
                    ┌───────────────────────────────────────────┼─────────────┐
                    │               PHASE B: Composition         │             │
                    │                                            │             │
                    │    ┌─────┐                           ┌─────┴─────┐      │
                    │    │ 006 │ (can run parallel)        │           │      │
                    │    │Monad│                           ▼           ▼      │
                    │    └──┬──┘                       ┌─────┐    ┌─────┐    │
                    │       │                          │ 005 │    │ 007 │    │
                    │       │                          │Conds│    │Prov │    │
                    │       │                          └──┬──┘    └─────┘    │
                    │       │                             │                   │
                    └───────┼─────────────────────────────┼───────────────────┘
                            │                             │
                    ┌───────┼─────────────────────────────┼───────────────────┐
                    │       │     PHASE C: Patterns       │                   │
                    │       │                             │                   │
                    │       └─────────────┬───────────────┘                   │
                    │                     ▼                                   │
                    │                 ┌─────┐                                 │
                    │                 │ 008 │                                 │
                    │                 │Solvr│                                 │
                    │                 └─────┘                                 │
                    │                                                         │
                    └─────────────────────────────────────────────────────────┘
```

---

## Package Structure

Jobs are organized into **four packages**:

### `@omega/core` (Required)
The minimal OmegaLLM - a Lisp with first-class continuations and nondeterminism.

| Job | Tasks | Primitives |
|-----|-------|------------|
| 004 | Tasks 1 & 3 | `call/cc`, `call-with-prompt`, `abort-to-prompt` |
| 006 | All | `unit`, `mzero`, `mplus`, `bind` |

```
@omega/core/
├── eval/           # CEKS machine, values, frames (KBind, ContinuationVal)
├── prims/          # Core primitives
├── nondet/         # amb, frontiers
└── lib/monad.lisp  # mdo macro, guard, msum
```

### `@omega/conditions` (Optional)
Resumable error handling - Common Lisp-style conditions.

| Job | Primitives |
|-----|------------|
| 005 | `signal`, `error`, `handler-bind`, `restart-bind`, `invoke-restart`, `find-restart` |

**Requires**: `@omega/core`

### `@omega/provenance` (Optional)
LLM audit trails and evidence tracking.

| Job | Primitives |
|-----|------------|
| 004 Task 2 | `evidence-id`, `verify-evidence`, `evidence-stale?` |
| 007 | `provenance-trace`, `provenance-record`, `provenance-check-staleness`, `ProvenanceGraph` |

**Requires**: `@omega/core`

### `@omega/solver` (Optional)
Strategic problem-solving patterns.

| Job | Primitives |
|-----|------------|
| 008 | `make-solver`, `compose-*`, `repair-until-valid`, `fixpoint`, `fact-store`, `budget-split` |

**Requires**: `@omega/core`, `@omega/conditions`

### Package Dependency Graph

```
                    ┌─────────────────┐
                    │   @omega/core   │
                    │                 │
                    │ • CEKS machine  │
                    │ • call/cc       │
                    │ • amb           │
                    │ • unit/bind     │
                    └────────┬────────┘
                             │
          ┌──────────────────┼──────────────────┐
          │                  │                  │
          ▼                  ▼                  ▼
┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐
│@omega/conditions│ │@omega/provenance│ │  (user code)    │
│                 │ │                 │ │                 │
│ • signal/error  │ │ • evidence      │ │ Can use core    │
│ • restarts      │ │ • graph         │ │ directly        │
│ • handler-bind  │ │ • persistence   │ │                 │
└────────┬────────┘ └─────────────────┘ └─────────────────┘
         │
         ▼
┌─────────────────┐
│  @omega/solver  │
│                 │
│ • combinators   │
│ • repair loops  │
│ • fact store    │
└─────────────────┘
```

---

## Effort Estimates

| Phase | Jobs | Total Effort | Unlocks |
|-------|------|--------------|---------|
| **A** | 004 | 2-3 days | call/cc, evidence primitives, delimited continuations |
| **B** | 005, 006, 007 | 3-5 days | Conditions, monadic composition, full provenance |
| **C** | 008 | 3-5 days | Composable solvers, repair loops, fact store, fixpoint |

**Total**: ~2-3 weeks for complete implementation

---

## Recommended Execution Order

1. **Start with Job 004** - Everything else depends on call/cc
2. **Run 006 in parallel** - Monadic primitives don't need call/cc
3. **Then 005 and 007** - Both need 004 complete
4. **Finally 008** - Needs 005 and 006

```
Week 1: Job 004 (call/cc + evidence) + Job 006 (monads) [parallel]
Week 2: Job 005 (conditions) + Job 007 (provenance) [parallel]
Week 3: Job 008 (solvers)
```

---

## Job Status Legend

- **NOT STARTED**: Job documented, no work begun
- **IN PROGRESS**: Someone is working on it
- **BLOCKED**: Waiting on external dependency
- **DONE**: Fix completed and verified

---

## Creating New Jobs

When documenting a new job:

1. Use sequential numbering: `009-SHORT-DESCRIPTION.md`
2. Follow the format of existing jobs
3. Include ALL file paths as relative links (e.g., `[file.ts](../src/file.ts)`)
4. Include code snippets showing implementation
5. Add verification steps that can be run
6. Include a checklist
7. **Document dependencies clearly**
8. Update this README with the new job

---

## Related Documents

- [LAMBDA-LLM--OMEGA-LLM-FEATURES.md](../docs/LAMBDA-LLM--OMEGA-LLM-FEATURES.md) - Feature audit and inside-out worklist
- [ARCHITECTURE-EXPLANATION.md](../docs/ARCHITECTURE-EXPLANATION.md) - System architecture
- [REFERENCE-ALGEBRA.md](../docs/REFERENCE-ALGEBRA.md) - Formal specification

---

## Quick Reference: What Each Job Adds

| Job | Package | New Primitives | New Files |
|-----|---------|----------------|-----------|
| 004 (T1,T3) | `@omega/core` | `call/cc`, `call-with-prompt`, `abort-to-prompt` | values.ts, prims.ts, machineStep.ts |
| 004 (T2) | `@omega/provenance` | `evidence-id`, `verify-evidence`, `evidence-stale?` | prims.ts update |
| 005 | `@omega/conditions` | `signal`, `error`, `handler-bind`, `restart-bind`, `invoke-restart`, `find-restart` | conditions/types.ts, machine.ts update |
| 006 | `@omega/core` | `unit`, `mzero`, `mplus`, `bind`, `guard`, `msum` | machine.ts (KBind frame), lib/monad.lisp |
| 007 | `@omega/provenance` | `provenance-trace`, `provenance-record`, `provenance-check-staleness` | provenance/graph.ts, provenance/persistentStore.ts |
| 008 | `@omega/solver` | `budget-split`, `budget-allocate`, `make-solver`, `solver-solve`, `compose-*`, `repair-until-valid`, `fixpoint`, fact store | solver/*.ts |
