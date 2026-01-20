# STRATEGY DOCUMENT: OmegaLLM Feature Implementation

## 🔴 CRITICAL FINDING: LambdaLLM Already Has Working Implementations

**Before planning reimplementation, we discovered:**

| Project | Tests | Pass Rate | Has call/cc? | Has conditions? |
|---------|-------|-----------|--------------|-----------------|
| **LambdaLLM** | 1785 | 96.7% (1725 pass) | ✅ YES | ✅ YES |
| **OmegaLLM** | 1124 | 100% | ❌ No (internal only) | ❌ No |

LambdaLLM is **not just specs** - it has:
- `src/core/continuation.ts` - 397 lines, working continuation system
- `src/core/conditions.ts` - 323 lines, non-unwinding conditions
- `src/core/eval.ts` - 1045 lines, evaluator with call/cc built-in
- 66 test files, 1725 passing tests

---

## Strategy Decision: Port vs Reimplement

### Architecture Comparison

```
┌─────────────────────────────────────────────────────────────────────────┐
│                     EVALUATOR ARCHITECTURE COMPARISON                    │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  LambdaLLM: Continuation-Passing Style                                   │
│  ─────────────────────────────────────                                   │
│                                                                          │
│  evalExpr(expr, env, cont, ffi) → Value | Promise<Value>                │
│                                                                          │
│  Continuation = {                                                        │
│    frames: Frame[]  // Each frame has handler FUNCTION                   │
│    handlers: Map    // Condition handlers                                │
│  }                                                                       │
│                                                                          │
│  Frame = {                                                               │
│    id, type, expr, envSnapshot, marks,                                  │
│    handler: (value, cont) => Value  // FUNCTION in frame                │
│  }                                                                       │
│                                                                          │
│  ✓ Clean design                                                          │
│  ✓ call/cc is trivial (cont is already a value)                         │
│  ✗ Harder to introspect (functions not data)                            │
│  ✗ Harder to serialize/fork                                             │
│                                                                          │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  OmegaLLM: CEKS Machine (Explicit State)                                 │
│  ───────────────────────────────────────                                 │
│                                                                          │
│  machineStep(state) → StepOutcome                                       │
│                                                                          │
│  State = {                                                               │
│    control: Expr | Val,                                                 │
│    env: Env,                                                            │
│    store: Store,                                                        │
│    kont: Frame[],      // Stack of DATA frames                          │
│    handlers: HandlerFrame[]                                             │
│  }                                                                       │
│                                                                          │
│  Frame = KIf | KBegin | KAppFun | KAppArg | ...  // TAGGED DATA         │
│                                                                          │
│  ✓ Fully introspectable (all data, no functions in state)               │
│  ✓ Can fork, serialize, replay, step-debug                              │
│  ✓ Time-travel debugging                                                │
│  ✗ call/cc requires reifying Frame[] as callable                        │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Strategy: REIMPLEMENT with LambdaLLM as Reference

**Why not copy directly:**
1. Different evaluator architectures (CPS vs CEKS)
2. LambdaLLM frames have handler functions; OmegaLLM frames are pure data
3. OmegaLLM's introspection is MORE powerful but requires different approach

**What we CAN copy:**
1. **Tests** - Adapt LambdaLLM tests for OmegaLLM
2. **Types/Interfaces** - `Condition`, `Restart` types are portable
3. **Design patterns** - How conditions/restarts interact
4. **API** - Same primitive names and semantics

**What we must reimplement:**
1. **call/cc** - Must reify CEKS `kont: Frame[]` as callable value
2. **Continuation application** - Must restore CEKS state from ContinuationVal
3. **Condition signaling** - Must integrate with CEKS stepping

---

## Abstraction Boundaries: What Uses What

```
┌─────────────────────────────────────────────────────────────────────────┐
│  LAYER 5: Domain DSLs                                                    │
│  ┌─────────────────────────────────────────────────────────────────┐    │
│  │  Experts, Sessions, Task Envelopes, Domain Algebra               │    │
│  │  (NOT PLANNED - build only if needed)                            │    │
│  └─────────────────────────────────────────────────────────────────┘    │
│                              │                                           │
│                              │ uses                                      │
│                              ▼                                           │
├─────────────────────────────────────────────────────────────────────────┤
│  LAYER 4: Search & Strategy  [Job 008]                                   │
│  ┌─────────────────────────────────────────────────────────────────┐    │
│  │  • compose-sequential, compose-parallel, compose-fallback        │    │
│  │  • repair-until-valid                                            │    │
│  │  • solver-estimate                                               │    │
│  │  • fact-store, fixpoint                                          │    │
│  └─────────────────────────────────────────────────────────────────┘    │
│         │                              │                                 │
│         │ uses                         │ uses                            │
│         ▼                              ▼                                 │
├─────────────────────────────────────────────────────────────────────────┤
│  LAYER 3: Effects & Control  [Jobs 005, 006]                             │
│  ┌────────────────────────┐    ┌────────────────────────────────────┐   │
│  │ Job 005: Conditions    │    │ Job 006: Monadic Primitives        │   │
│  │ • signal               │    │ • unit, mzero, mplus, bind         │   │
│  │ • error                │    │ • guard, msum, mfilter             │   │
│  │ • handler-bind         │    │ • mdo macro                        │   │
│  │ • restart-bind         │    └────────────────────────────────────┘   │
│  │ • invoke-restart       │              │                              │
│  └────────────────────────┘              │                              │
│         │                                │                               │
│         │ uses call/cc                   │ uses amb                      │
│         ▼                                ▼                               │
├─────────────────────────────────────────────────────────────────────────┤
│  LAYER 2: Oracle & Provenance  [Jobs 004.2, 007]                         │
│  ┌─────────────────────────────────────────────────────────────────┐    │
│  │  • evidence-id, verify-evidence, evidence-stale?                 │    │
│  │  • provenance-trace, provenance-check-staleness                  │    │
│  │  • ProvenanceGraph (DAG)                                         │    │
│  │  • Persistent receipt store                                      │    │
│  └─────────────────────────────────────────────────────────────────┘    │
│         │                                                                │
│         │ uses continuations for async                                   │
│         ▼                                                                │
├─────────────────────────────────────────────────────────────────────────┤
│  LAYER 1: Evaluation Core  [Job 004.1, 004.3]                            │
│  ┌─────────────────────────────────────────────────────────────────┐    │
│  │  NEW: call/cc, call-with-prompt, abort-to-prompt                 │    │
│  │  NEW: ContinuationVal type                                       │    │
│  │  NEW: Continuation application handling in machineStep           │    │
│  │  ─────────────────────────────────────────────────────────────── │    │
│  │  EXISTING:                                                       │    │
│  │  • CEKS machine (machine.ts, machineStep.ts)                     │    │
│  │  • kont: Frame[] continuation stack                              │    │
│  │  • machine-new, machine-step, machine-fork, machine-run          │    │
│  │  • Effect handlers with resumeTo                                 │    │
│  │  • KHandleBoundary, KHandleReturn                                │    │
│  └─────────────────────────────────────────────────────────────────┘    │
│         │                                                                │
│         │ builds on                                                      │
│         ▼                                                                │
├─────────────────────────────────────────────────────────────────────────┤
│  LAYER 0: Primitives  [Jobs 001, 002 - DONE]                             │
│  ┌─────────────────────────────────────────────────────────────────┐    │
│  │  117 primitives: cons, car, cdr, +, -, *, /, map, filter, etc.  │    │
│  │  SICP-compliant naming (hyphenated, predicates end with ?)       │    │
│  └─────────────────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Dependency Graph (What Blocks What)

```
                    ┌─────────────────────────────────────┐
                    │  Jobs 001, 002, 003 (DONE)          │
                    │  Primitives, naming, audit          │
                    └──────────────┬──────────────────────┘
                                   │
                    ┌──────────────▼──────────────────────┐
                    │  Job 004: Core Magic                │
                    │  • call/cc (4h)                     │
                    │  • evidence primitives (4h)         │
                    │  • call-with-prompt (4h)            │
                    └──────────────┬──────────────────────┘
                                   │
          ┌────────────────────────┼────────────────────────┐
          │                        │                        │
          ▼                        ▼                        ▼
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│ Job 005         │    │ Job 006         │    │ Job 007         │
│ Conditions      │    │ Monads          │    │ Provenance      │
│ (needs call/cc) │    │ (no dep)        │    │ (needs evid)    │
└────────┬────────┘    └────────┬────────┘    └─────────────────┘
         │                      │
         └──────────┬───────────┘
                    │
                    ▼
          ┌─────────────────┐
          │ Job 008         │
          │ Solvers         │
          │ (needs 005,006) │
          └─────────────────┘
```

---

## Implementation Order with Validation Gates

### Phase A: Core Magic (Week 1)

**Gate 0**: Before starting, run OmegaLLM tests
```bash
cd OmegaLLM && npm test  # Must pass 1124 tests
```

**Job 004.1: call/cc** (Start here)
- Add `ContinuationVal` type to values.ts
- Add `call/cc` primitive to prims.ts
- Add continuation application to machineStep.ts
- **VALIDATION**:
  ```lisp
  (call/cc (lambda (k) (+ 1 (k 5))))  ; => 5
  ```

**Job 004.3: Delimited continuations**
- Add `call-with-prompt` primitive
- Add `abort-to-prompt` primitive
- **VALIDATION**:
  ```lisp
  (call-with-prompt 'p
    (lambda () (+ 1 (abort-to-prompt 'p 5)))
    (lambda (k v) (* v 2)))  ; => 10
  ```

**Job 004.2: Evidence primitives**
- Add `evidence-id`, `verify-evidence`, `evidence-stale?`
- Build on existing Evidence type in meaning.ts
- **VALIDATION**:
  ```lisp
  (define r (oracle-infer "test"))
  (evidence-id r)  ; => "ev-..."
  ```

**Gate A**: All OmegaLLM tests still pass + new continuation tests

### Phase B: Composition (Week 2)

**Job 006: Monads** (can parallel with anything)
- Add `unit`, `mzero`, `mplus`, `bind`
- Add `KBind` frame to machine.ts
- **VALIDATION**: Monad laws pass

**Job 005: Conditions** (needs 004.1)
- Copy types from LambdaLLM's conditions.ts
- Reimplement for CEKS architecture
- **VALIDATION**:
  ```lisp
  (handler-bind ((error (lambda (c) (invoke-restart 'use-value 0))))
    (restart-bind ((use-value (lambda (v) v)))
      (error 'division-by-zero "oops" '())))  ; => 0
  ```

**Job 007: Provenance** (needs 004.2)
- Build ProvenanceGraph
- Add persistent store
- **VALIDATION**: Trace oracle calls

**Gate B**: All tests pass + condition/monad/provenance tests

### Phase C: Patterns (Week 3)

**Job 008: Solvers** (needs 005, 006)
- Budget split/allocate
- Composable solver interface
- repair-until-valid
- fact store, fixpoint
- **VALIDATION**: Composable solver test suite

**Gate C**: Full test suite passes

---

## What We Copy vs Reimplement

### Copy from LambdaLLM

| Item | Source | Target | Notes |
|------|--------|--------|-------|
| `Condition` interface | conditions.ts:24-36 | OmegaLLM conditions/types.ts | Direct copy |
| `Restart` interface | conditions.ts:41-50 | OmegaLLM conditions/types.ts | Direct copy |
| `makeCondition` | conditions.ts:78-90 | OmegaLLM prims.ts | Direct copy |
| `makeRestart` | conditions.ts:99-109 | OmegaLLM prims.ts | Direct copy |
| `findRestart` | conditions.ts:122-127 | OmegaLLM prims.ts | Direct copy |
| Tests | tests/conditions.test.ts | OmegaLLM test/conditions/ | Adapt for CEKS |

### Reimplement for CEKS

| Item | Why Reimplement |
|------|-----------------|
| call/cc | LambdaLLM passes cont as argument; CEKS must reify Frame[] |
| signalCondition | LambdaLLM uses handler functions; CEKS uses Frame data |
| withRestarts | Different continuation architecture |
| Frame handlers | LambdaLLM has functions in frames; CEKS uses tagged data |

---

## Files Summary: What Gets Modified

### Job 004 (Core Magic)

| File | Changes |
|------|---------|
| `src/core/eval/values.ts` | Add `ContinuationVal` type |
| `src/core/eval/machine.ts` | No change (Frame types already exist) |
| `src/core/eval/machineStep.ts` | Handle continuation application |
| `src/core/prims.ts` | Add `call/cc`, `call-with-prompt`, `abort-to-prompt`, evidence prims |
| `src/core/pipeline/compileText.ts` | Register new primitives |

### Job 005 (Conditions)

| File | Changes |
|------|---------|
| `src/core/conditions/types.ts` | NEW - Copy Condition/Restart from LambdaLLM |
| `src/core/eval/machine.ts` | Add `KHandlerBind`, `KRestartBind` frames |
| `src/core/eval/machineStep.ts` | Handle condition signaling |
| `src/core/prims.ts` | Add `signal`, `error`, `invoke-restart`, `find-restart` |
| `src/core/pipeline/compileText.ts` | Add `handler-bind`, `restart-bind` special forms |

### Job 006 (Monads)

| File | Changes |
|------|---------|
| `src/core/eval/machine.ts` | Add `KBind` frame |
| `src/core/eval/machineStep.ts` | Handle bind continuation |
| `src/core/prims.ts` | Add `unit`, `mzero`, `mplus`, `bind` |

### Job 007 (Provenance)

| File | Changes |
|------|---------|
| `src/core/provenance/graph.ts` | NEW - ProvenanceGraph class |
| `src/core/provenance/store.ts` | NEW - Persistent store |
| `src/core/oracle/meaning.ts` | Extend Evidence types |
| `src/core/prims.ts` | Add provenance primitives |

### Job 008 (Solvers)

| File | Changes |
|------|---------|
| `src/core/solver/types.ts` | NEW - Solver interface |
| `src/core/solver/combinators.ts` | NEW - compose-* functions |
| `src/core/solver/repair.ts` | NEW - repair-until-valid |
| `src/core/solver/facts.ts` | NEW - Fact store |
| `src/core/solver/fixpoint.ts` | NEW - Fixpoint iteration |
| `src/core/governance/budgets.ts` | Add split/allocate |
| `src/core/prims.ts` | Add solver primitives |

---

## Risk Assessment

### Low Risk (90%+ confidence)
- Job 006: Monads are well-understood, straightforward
- Job 008 Tasks 1-3: Budget, basic solver interface

### Medium Risk (70-90% confidence)
- Job 004.1: call/cc - CEKS has the machinery, just need to expose
- Job 004.2: Evidence - Types exist, need primitives
- Job 007: Provenance - Clear design, execution risk

### Higher Risk (50-70% confidence)
- Job 005: Conditions - Interaction with CEKS stepping is complex
- Job 004.3: Delimited continuations - Integration with existing handlers

### Mitigation
- Run tests after EVERY change
- Use LambdaLLM tests as validation
- If stuck on conditions, simplify to basic signal/handle first

---

*Created: 2026-01-19*
*Based on analysis of LambdaLLM (1725 passing tests) and OmegaLLM (1124 passing tests)*
