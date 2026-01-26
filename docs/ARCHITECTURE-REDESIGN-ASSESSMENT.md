# ARCHITECTURE Redesign Assessment

## Executive Summary

**Your intuition is correct.** The ARCHITECTURE docs split cleanly into two eras:

| Era | Docs | Kernel | Status |
|-----|------|--------|--------|
| LambdaLLM (v1) | 00-29 | Recursive eval (~200 lines) | Mostly implemented |
| Omega (v2) | 32-series | CEKS machine + dual-plane semantics | ~12% implemented |

The 32-series is a **complete redesign** that puts kernel stuff in the kernel. The original 00-30 specs don't need to be "redone" - they need to be **replaced** by implementing the 32 specs, which already exist.

---

## Document Classification

### Pre-CEKS Specifications (Docs 00-29)

These specify **LambdaLLM** with recursive evaluation:

| Doc | Topic | Kernel Impact |
|-----|-------|---------------|
| 00-SPECIFICATION | Grammar, syntax | Keep (surface syntax) |
| 01-READER | S-expression parsing | Keep (unchanged) |
| 02-TYPES | Core data types | **Superseded by 32-6** (adds Meaning, Dist, etc.) |
| 03-ENVIRONMENT | Namespaces, bindings | **Superseded by 32-6** (adds Ctx as value, seal) |
| 04-EVALUATOR | **Recursive eval** | **OBSOLETE** - replaced by CEKS in 32-6 |
| 05-CONTINUATIONS | CPS-style | **Superseded by 32-6** (explicit Kont frames) |
| 06-CONDITIONS | Non-unwinding errors | Keep (adds to handlers in 32) |
| 07-FFI | Foreign functions | Keep (becomes effect handlers) |
| 08-PROTOCOL | nREPL-style | Keep (wraps CEKS runtime) |
| 09-MODULES | Module system | Keep (add phase separation from 32-10) |
| 10-PERSISTENCE | Image saving | Keep (now via State serialization) |
| 11-MACROS | Compile-time transforms | **Superseded by 32-10** (hygienic syntax-rules) |
| 12-CONCURRENCY | Async/futures | **Missing SICP primitives** (no amb, limited streams) |
| 13-MEMORY | GC considerations | Keep |
| 14-STDLIB | Standard library | **Needs extension** (omega.* modules from 32-3) |
| 15-20 | Tooling, testing | Keep |
| 21-SECURITY | Capabilities | **Superseded by 32-4** (governance profiles) |
| 22-PROVENANCE | Evidence tracking | Keep (integrate with Meaning) |
| 23-FACTS | Monotone knowledge | Keep |
| 24-FIXPOINT | Convergence | Keep |
| 25-BUDGET | Resource limits | **Superseded** (now in State.budget) |
| 26-ARTIFACTS | Memoization | Keep (integrate with receipts) |
| 27-OUTCOMES | ok/needs/error | Keep |
| 28-SESSION | Host control | Keep (becomes Oracle protocol) |
| 29-EXPERTS | Intent compilation | Keep (layers above kernel) |

### Post-CEKS Specifications (Docs 32-series)

These specify **Omega** with CEKS machine and dual-plane semantics:

| Doc | Topic | Status |
|-----|-------|--------|
| 32-1 | Foundations, kernel forms, dual-plane | **CRITICAL** - ~20% implemented |
| 32-2 | Oracle protocol, truth, learning | **0% implemented** |
| 32-3 | SICP tower modules | **0% implemented** |
| 32-4 | Governance profiles | **0% implemented** |
| 32-5 | Implementation blueprint | Reference only |
| 32-6 | CEKS machine TypeScript reference | **~30% implemented** |
| 32-7 | Completing core runtime | **~10% implemented** |
| 32-8-9 | Additional runtime | Minimal |
| 32-10 | amb + syntax-rules | **0% implemented** |
| 32-11 | Deep hardening | **0% implemented** |

---

## SICP Primitive Coverage

### What's Specified in 32-series

| SICP Feature | Doc | Module | Status |
|--------------|-----|--------|--------|
| Streams (delay/force/cons-stream) | 32-9 | omega.stream | **NOT IMPLEMENTED** |
| Nondeterminism (amb/require) | 32-10 | omega.nondet | **NOT IMPLEMENTED** |
| Generic operations (apply-generic) | 32-9 | omega.generic | **NOT IMPLEMENTED** |
| Constraint propagation | 32-9 | omega.constraints | **NOT IMPLEMENTED** |
| Meta-circular evaluator | 32-9 | omega.meta | **NOT IMPLEMENTED** |
| Compilation passes | 32-9 | omega.compiler | **NOT IMPLEMENTED** |
| Pattern matching | 32-7 | omega.match | **NOT IMPLEMENTED** |
| Hygienic macros (syntax-rules) | 32-10 | expander | **NOT IMPLEMENTED** |

### What's Missing from 12-CONCURRENCY

The original 12-CONCURRENCY spec has:
- Promises/async ✓
- Futures ✓

But **lacks**:
- SICP streams (lazy sequences)
- amb/backtracking
- Coroutines via continuations
- Propagators/constraint networks

These are all specified in 32-9 and 32-10.

---

## Current Implementation vs 32-Spec

### What Omega Has (partial CEKS)

From reading the actual implementation:

```typescript
// OmegaLLM/src/core/eval/machine.ts
export type State = {
  control: Control;      // ✓ Implemented
  env: Env;              // ✓ Implemented
  store: Store;          // ✓ Implemented
  kont: Frame[];         // ✓ Implemented
  handlers: HandlerFrame[]; // ✓ Implemented
  profile?: Profile;     // EXISTS but NOT ENFORCED
  budget?: RuntimeBudget; // EXISTS but NOT ENFORCED
  sec?: RuntimeSecurity;  // EXISTS but NOT ENFORCED
  provenanceGraph?: ...;  // EXISTS but NOT ENFORCED
  provenanceStore?: ...;  // EXISTS but NOT ENFORCED
};
```

### What's Missing (from 32-CHECKLIST)

| Category | Required | Have | Gap |
|----------|----------|------|-----|
| Kernel forms | 18 | 10 | 8 missing (int, infer, rewrite, ctx, extend, seal, match) |
| Value types | 17 | 7 | 10 missing (Meaning, Dist, Engine, Prompt, Policy, etc.) |
| Oracle protocol | 13 | 0 | **100% missing** |
| SICP modules | 14 | 2 | 12 missing |
| **TOTAL** | ~450 | ~54 | **~88% missing** |

---

## Redesign Recommendation

### Option 1: Finish 32-Spec Implementation (RECOMMENDED)

The 32-series already specifies exactly what you want:
- Security IN the kernel (State.budget, State.sec, capabilities)
- SICP primitives as language features (omega.stream, omega.nondet)
- Dual-plane semantics (int/infer/rewrite as special forms)

**Work required:**
1. Implement missing kernel forms (int, infer, rewrite)
2. Implement Meaning value type
3. Implement Oracle protocol
4. Implement SICP modules (streams, amb, generic ops)
5. Enforce governance fields that already exist in State

**Effort estimate:** The architecture is already designed. This is implementation work, not design work.

### Option 2: Backport to 00-29 Specs

NOT RECOMMENDED. The original specs assume recursive eval and wrapper-layer security. You'd be fighting the architecture.

---

## Critical Path

To get from current state to "CEKS with kernel-level governance and SICP primitives":

### Phase 1: Core Kernel (P0)

1. **`Meaning` value type** (32-6, lines 192-207)
   - denotation, residual, rewrite, invariants, effects, cost, paths, deps, memo, evidence, obligation, confidence, trace

2. **`int`/`infer`/`rewrite` special forms** (32-1, lines 263-265)
   - Lower to `(effect "infer.op" ...)`
   - These make inference a semantic plane, not an API call

3. **Oracle Protocol** (32-5, lines 259-294)
   - Req.Eval, Req.Apply, Req.Tool, Req.Test, Req.Return, etc.
   - This enables REPL re-entrancy under discipline

4. **Commit barrier** (32-7, lines 586-605)
   - Enforce obligations before accepting rewrites

### Phase 2: SICP Tower (P1)

5. **omega.stream** (32-9)
   - delay, force, cons-stream, stream-map, stream-filter

6. **omega.nondet** (32-9, 32-10)
   - amb, require, cut, all-solutions, first-solution, best-solution
   - Handler-based backtracking

7. **omega.generic** (32-9)
   - attach-tag, type-tag, apply-generic, coercion

8. **omega.match** (32-7)
   - Pattern matching as first-class

### Phase 3: Governance Integration (P2)

9. **Enforce State.budget**
   - Token limits, cost tracking, iteration caps

10. **Enforce State.sec**
    - Capability checking on every operation

11. **Governance profiles** (32-4)
    - profile:explore, profile:pragmatic, profile:strict, profile:airgap

---

## What "Kernel Stuff in Kernel" Means Concretely

### Before (LambdaLLM approach - wrappers)

```
┌──────────────────────────────────────┐
│  Enterprise Layer (approval, DLP)    │  ← CAN BE BYPASSED
├──────────────────────────────────────┤
│  Recursive Eval (no security)        │  ← NO ENFORCEMENT
└──────────────────────────────────────┘
```

### After (Omega approach - kernel)

```
┌──────────────────────────────────────┐
│  CEKS Step Function                  │
│  ┌────────────────────────────────┐  │
│  │  CHECK budget EVERY step       │  │  ← CAN'T BYPASS
│  │  CHECK capabilities EVERY op   │  │  ← ENFORCED IN KERNEL
│  │  RECORD provenance EVERY frame │  │  ← AUDITABLE
│  └────────────────────────────────┘  │
└──────────────────────────────────────┘
```

The 32-spec already puts the hooks in the right place. You just need to implement them.

---

## Summary

| Question | Answer |
|----------|--------|
| Are 00-30 specs obsolete? | For kernel (04, 05, 11, 21), YES. For surface features (22-29), keep and integrate. |
| Do we need to "redo" 30 sections? | NO. The 32-series already redid them. Implement 32 instead. |
| Are SICP primitives specified? | YES, in 32-9 and 32-10 (streams, amb, generic ops, macros) |
| Are they implemented? | NO. ~0% of SICP modules exist. |
| How hard is the redesign? | DESIGN IS DONE. 32-series is 450+ features spec'd in detail. Implementation is ~88% remaining. |
| Is security in kernel or wrapper? | 32-spec puts it IN KERNEL (State.budget, State.sec). Current implementation has fields but doesn't enforce. |

**Bottom line:** The 32-series specifies the kernel, but the enterprise features (21-29) need to be REDESIGNED to use kernel affordances.

---

## CORRECTION: The Gap You Identified

The 32-series introduced CEKS but **didn't fully redesign the enterprise specs to use it.**

### What 00-30 Assumes (Wrapper Pattern)

Looking at the actual specs:

**21-SECURITY.md** (line 277):
```typescript
// Security as external wrapper around recursive eval
function evalExpr(expr, env, cont, ffi, counter: StepCounter): Value {
  counter.step();  // ← Passed as parameter, not in kernel
}
```

**25-BUDGET.md** (line 277):
```typescript
// Budget as external wrapper
function evalFixpoint(body, env, options, budget): Value {
  budget.consumeIteration();  // ← External check, not kernel
}
```

**22-PROVENANCE.md** (line 466):
```
// Provenance as external integration layer
Provenance Integration
├── FACTS System      ← Separate layer
├── CERTIFICATES      ← Separate layer
├── SESSION           ← Separate layer
```

### What CEKS Enables (Kernel Pattern)

The 32-series State type has:
```typescript
type State = {
  control: Control;
  env: Ctx;
  store: Store;
  kont: Kont;
  handlers: HandlerStack;
  // THESE FIELDS EXIST BUT USAGE ISN'T SPECIFIED:
  budget?: RuntimeBudget;
  sec?: RuntimeSecurity;
  provenanceGraph?: ProvenanceGraph;
  provenanceStore?: ProvenanceStore;
};
```

**The fields are there, but 21-29 don't know to use them.**

### The Real Gap - Complete Analysis (All 9 Enterprise Specs)

| Feature | 00-30 Spec | Current Pattern | Kernel Affordance | Redesign Needed |
|---------|------------|-----------------|-------------------|-----------------|
| **21-SECURITY** | Wrapper around FFI | `StepCounter` passed as param, `EnforcedFFI` wraps checks | State.sec checked in step() | **YES** |
| **22-PROVENANCE** | External registry | `EvidenceRegistry` class, separate layer | State.provenanceGraph + Kont marks | **YES** |
| **23-FACTS** | External store | `Environment.factStore`, `env.facts.assert()` | State.facts or effect | **YES** |
| **24-FIXPOINT** | External loop | `evalFixpoint()` wraps `evalExpr` in for-loop | Effect + handler | **MAYBE** |
| **25-BUDGET** | Wrapper class | External `Budget` class, manual `check()` calls | State.budget checked in step() | **YES** |
| **26-ARTIFACTS** | External cache | `evalMemo()` wrapper, `DependencyTracker` proxy | Effect + handler | **MAYBE** |
| **27-OUTCOMES** | Return type | Structured sum types (ok/proposed/etc) | Already language-level | **NO** |
| **28-SESSION** | External controller | `EnforcedFFI` wraps base, `SessionEnforcer` external | Oracle protocol | **PARTIAL** |
| **29-EXPERTS** | Application layer | Intent compilation, LLM prompting | Layers above kernel | **NO** |

### Detailed Evidence Per Spec

#### 23-FACTS (Wrapper Pattern - Needs Redesign)

From 23-FACTS.md lines 227-252:
```typescript
// Facts stored in Environment, not State
if (isAssert(expr)) {
  const factExpr = evalExpr(expr[1], env, cont, ffi);
  const evidence = expr.length > 3 ? evalExpr(expr[3], env, cont, ffi) : undefined;
  const isNew = env.facts.assert(factExpr, evidence);  // ← env.facts, NOT State
  metrics.increment('facts_added', isNew ? 1 : 0);
  return applyCont(cont, isNew);
}
```

From lines 149-196: `Environment` class holds `factStore` as external field.
From lines 294-313: `evalSubeval` creates isolated fact stores externally.

**Kernel alternative**: Facts could be `State.facts` with monotone merge on frame pop.

#### 24-FIXPOINT (Wrapper Pattern - Maybe Redesign)

From 24-FIXPOINT.md lines 119-186:
```typescript
// External iteration loop wrapping recursive eval
function evalFixpoint(
  body: Value,
  env: Environment,
  world: World,
  artifacts: ArtifactStore,
  options: FixpointOptions,  // ← External options
  cont: Continuation,
  ffi: FFI
): Value {
  for (let i = 0; i < maxIterations; i++) {  // ← External loop
    lastValue = evalExpr(body, env, cont, ffi);
    const sig = computeStateSignature(env, world, artifacts, mode);
    if (sig === lastSig) { status = 'converged'; break; }
    // ...
  }
}
```

**Kernel alternative**: Could be an effect handler that captures continuation and re-invokes.

#### 26-ARTIFACTS (Wrapper Pattern - Maybe Redesign)

From 26-ARTIFACTS.md lines 194-232:
```typescript
// External memoization wrapper
function evalMemo(
  deps: string[],
  body: Value,
  env: Environment,
  world: World,
  artifacts: ArtifactStore,  // ← External store
  cont: Continuation,
  ffi: FFI
): Value {
  const cached = artifacts.get(key);
  if (cached !== undefined) {
    metrics.increment('memo_hits');
    return applyCont(cont, cached);  // ← External cache check
  }
  // ... evaluate and store
}
```

From lines 246-286: `DependencyTracker` as proxy layer.

**Kernel alternative**: Could be `(effect "memo" key body)` with Store-based caching.

#### 27-OUTCOMES (Return Type - No Redesign Needed)

From 27-OUTCOMES.md lines 316-344:
- Structured sum types: `ok | proposed | nonconverged | cycle | needs | error`
- Monadic composition via `bindOutcome`
- Already language-level, propagates through computation

**This is fine as-is.** Outcomes are a data representation, not a kernel concern.

#### 28-SESSION (Wrapper Pattern - Partial Redesign Done)

From 28-SESSION.md lines 225-243:
```typescript
// EnforcedFFI wraps policy enforcement AROUND base FFI
class EnforcedFFI implements FFI {
  constructor(
    private base: FFI,
    private enforcer: SessionEnforcer,  // ← External
    private audit: AuditLog              // ← External
  ) {}

  call(name: string, args: Value[]): Value {
    this.enforcer.checkOperation(name, args);  // ← WRAPPER enforcement
    this.audit.log({ op: name, args, time: Date.now() });
    return this.base.call(name, args);
  }
}
```

**Kernel alternative**: 32's Oracle protocol addresses this - host controls via `Req.Apply`, `Req.Tool`, etc.

#### 29-EXPERTS (Application Layer - No Redesign Needed)

From 29-EXPERTS.md lines 278-306:
- Three-layer architecture: Tool Contract, Role Overlay, Task Envelope
- `compileIntent()` is LLM prompting, not kernel evaluation
- Output modes: REPORT, PLAN, PROGRAM, ANALYSIS

**This is application layer.** It uses the kernel but doesn't need kernel affordances.

### What "Redesign for CEKS" Means

For **21-SECURITY**, instead of:
```typescript
// OLD: External check
function evalExpr(expr, env, ..., counter) {
  counter.step();
  // ...
}
```

It should be:
```typescript
// NEW: Kernel enforcement
function step(s: State): StepOutcome {
  // FIRST THING: Check budget/security
  if (s.budget && !s.budget.hasRemaining()) {
    return { tag: "BudgetExceeded", ... };
  }
  if (s.sec && !s.sec.hasCapability(requiredCap(s.control))) {
    return { tag: "CapabilityDenied", ... };
  }
  // ... rest of step
}
```

For **22-PROVENANCE**, instead of:
```typescript
// OLD: External registry
const evidence = captureEvidence(world, ref);
registry.register(evidence);
```

It should be:
```typescript
// NEW: Automatic via Kont marks
function step(s: State): StepOutcome {
  // Every frame automatically records provenance
  const newKont = pushKont(s.kont, {
    ...frame,
    provenanceMark: {
      evidenceIds: [],
      worldFingerprint: s.store.worldFingerprint,
    }
  });
}
```

### Work Required - Complete Assessment

**DEFINITE REDESIGN (4 specs):**
1. **21-SECURITY**: Redesign as kernel enforcement (State.sec, checked in step())
2. **22-PROVENANCE**: Redesign with Kont marks + automatic evidence tracking
3. **23-FACTS**: Redesign with State.facts or effect handler (not Environment.factStore)
4. **25-BUDGET**: Redesign as kernel enforcement (State.budget, checked in step())

**CONSIDER REDESIGN (2 specs):**
5. **24-FIXPOINT**: Could become effect handler pattern - evaluate complexity vs benefit
6. **26-ARTIFACTS**: Could become Store-based effect - evaluate complexity vs benefit

**NO REDESIGN NEEDED (3 specs):**
7. **27-OUTCOMES**: Already language-level return types - works as-is
8. **28-SESSION**: Partially addressed by 32's Oracle protocol - finish Oracle implementation
9. **29-EXPERTS**: Application layer above kernel - fine as-is

This is DESIGN work, not just implementation. The 32-series introduced CEKS but **didn't redesign the enterprise specs to use CEKS affordances**.

---

## Conclusion

**Your hypothesis is confirmed across ALL 9 enterprise specs (21-29):**

Every spec from 21-29 was designed assuming recursive eval, and every one that interacts with evaluation uses the WRAPPER pattern. The ones that don't (27-OUTCOMES, 29-EXPERTS) are either return types or application layers.

The gap is:
- 32-series added `State.budget`, `State.sec`, `State.provenanceGraph` fields
- But 21-29 don't know these exist
- They still describe external wrappers around `evalExpr`

To unify: rewrite 21-26 to use kernel affordances, or mark them as "legacy LambdaLLM pattern, superseded by 32-series kernel enforcement."
