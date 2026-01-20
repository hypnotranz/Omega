# JOB-003: Comprehensive Feature Audit and Abstraction Layering

**Priority**: P0 - Strategic Planning
**Estimated Effort**: 4-8 hours
**Skills Required**: Architecture analysis, Lisp semantics, deep codebase reading
**Status**: ✅ DONE (2026-01-19)

> **Completed**: Full deep audit of LambdaRLM (29 Lisp files + Python runtime), LambdaLLM (71 spec files), and OmegaLLM (90+ files). Core comparison documented, portability assessed, visual abstraction layers created, integration roadmap written.
> See: [docs/LAMBDA-LLM--OMEGA-LLM-FEATURES.md](../docs/LAMBDA-LLM--OMEGA-LLM-FEATURES.md)

---

## Executive Summary

Perform a **complete, deep audit** of LambdaRLM, LambdaLLM, and OmegaLLM to:

1. **Inventory all features** - What actually exists (implementation vs spec)
2. **Compare the cores** - OmegaLLM's reentrancy vs LambdaLLM's capabilities
3. **Assess portability** - What can be "copied in" vs requires serious rework
4. **Design abstraction layers** - Proper layering for feature integration
5. **Identify magic** - Unique capabilities we must not lose

**Key Question**: What is the "magic" in each system's core that we must preserve?
- **OmegaLLM**: Reentrancy, CEKS machine introspection, receipts/provenance
- **LambdaLLM**: ??? (to be determined)
- **LambdaRLM**: Domain algebra, composable solvers, repair loops

---

## Audit Scope

### Systems to Audit

| System | Location | Language | Type |
|--------|----------|----------|------|
| **LambdaRLM** | `../LambdaRLM/` | Lisp + Python | Implementation |
| **LambdaLLM** | `../LambdaLLM/ARCHITECTURE/` | Spec (TypeScript examples) | Specification |
| **OmegaLLM** | Current repo | TypeScript | Implementation |
| **FrameLisp** | Reference docs | Algebraic spec | Specification |

### Audit Deliverables

1. **Feature Matrix** - Complete inventory with implementation status
2. **Core Comparison** - What makes each core unique
3. **Portability Assessment** - Effort estimates for each feature
4. **Abstraction Layer Diagram** - Visual layering proposal
5. **Integration Roadmap** - Priority-ordered feature migration plan

---

## Audit Methodology

### Phase 1: Deep Inventory

For each system, document:
- **File-by-file analysis** (not just grep)
- **Actual implementation** vs **spec/design**
- **Dependencies** - what each feature requires
- **API surface** - how it's called/used

### Phase 2: Core Comparison

Answer these questions:
1. What is OmegaLLM's CEKS machine capable of that others aren't?
2. What can LambdaRLM's evaluator do that OmegaLLM can't?
3. What does LambdaLLM spec that neither implements?
4. Where are the semantic gaps?

### Phase 3: Portability Analysis

For each feature, categorize:
- **Trivial**: Copy-paste with name changes
- **Moderate**: Requires adapter/wrapper
- **Significant**: Requires core changes
- **Impossible**: Semantic mismatch, can't port

### Phase 4: Layering Design

Design abstraction layers:
```
Layer 5: Domain-Specific (solvers, validators, experts)
Layer 4: Search & Strategy (meta-search, repair loops)
Layer 3: Effects & Control (amb, streams, budget)
Layer 2: Oracle & Provenance (receipts, inference)
Layer 1: Evaluation Core (CEKS, environments, values)
Layer 0: Primitives (cons, car, cdr, arithmetic)
```

---

## Feature Categories to Audit

### Category A: Core Evaluation
- [ ] Evaluator architecture (CEKS vs other)
- [ ] Environment model
- [ ] Value representations
- [ ] Continuation handling
- [ ] Effect system

### Category B: Streams & Laziness
- [ ] Stream primitives
- [ ] Lazy evaluation support
- [ ] Stream combinators
- [ ] Interleaving/fair merge

### Category C: Nondeterminism
- [ ] `amb` primitive
- [ ] Failure/backtracking
- [ ] Search strategies
- [ ] Frontier management

### Category D: Budget & Resources
- [ ] Budget primitives
- [ ] Allocation strategies
- [ ] Cost tracking
- [ ] Resource limits

### Category E: Oracle/LLM Integration
- [ ] Oracle primitives
- [ ] Protocol handlers
- [ ] Structured output
- [ ] Retry/repair

### Category F: Provenance & Receipts
- [ ] Receipt generation
- [ ] Trace logging
- [ ] Replay capability
- [ ] Audit trails

### Category G: Search & Strategies
- [ ] Meta-search
- [ ] Strategy selection
- [ ] Composable solvers
- [ ] Repair loops

### Category H: Domain Modeling
- [ ] Domain algebra
- [ ] Type systems
- [ ] Constraints
- [ ] Validation

### Category I: Sessions & State
- [ ] Session management
- [ ] Facts/assertions
- [ ] Fixpoint iteration
- [ ] Expert roles

---

## Checklist

### Phase 1: Inventory
- [x] Audit LambdaRLM lib/*.lisp (29 files) ✅
- [x] Audit LambdaRLM src/lambdarlm/*.py ✅
- [x] Audit LambdaLLM ARCHITECTURE/*.md (71 files) ✅
- [x] Audit OmegaLLM src/core/** (90+ files) ✅ (from previous work)
- [x] Document all primitives in each system ✅
- [x] Document all special forms ✅
- [x] Document all macros ✅

### Phase 2: Core Comparison
- [x] Document OmegaLLM's reentrancy mechanism ✅ (CEKS machine stepping/forking)
- [x] Document LambdaRLM's evaluation model ✅ (875-line eval.py, World abstraction)
- [x] Document LambdaLLM's specified semantics ✅ (call/cc, evidence, conditions)
- [x] Identify semantic gaps/mismatches ✅ (continuations, evidence, monads, world)
- [x] Identify unique "magic" in each ✅

### Phase 3: Portability
- [x] Assess each LambdaRLM feature ✅
- [x] Assess each LambdaLLM spec ✅
- [x] Categorize by effort level ✅ (Trivial/Moderate/Significant/Cannot)
- [x] Identify blockers ✅ (call/cc requires core change)

### Phase 4: Layering
- [x] Design layer boundaries ✅ (Layer 0-5)
- [x] Assign features to layers ✅
- [x] Design inter-layer APIs ✅ (in features doc)
- [x] Create visual diagram ✅
- [x] Write integration roadmap ✅ (Phase 1-5)

---

## Output Format

### Feature Matrix Template

```
| Feature | LambdaRLM | LambdaLLM | OmegaLLM | Portable? | Layer |
|---------|-----------|-----------|----------|-----------|-------|
| streams | Yes (impl) | No | Yes (impl) | N/A | 3 |
| amb | Yes (impl) | No | Yes (impl) | N/A | 3 |
| oracle | Python | Spec | Yes (impl) | N/A | 2 |
| etc... | | | | | |
```

### Layering Diagram Template

```
┌─────────────────────────────────────────────────────────────┐
│ Layer 5: DOMAIN-SPECIFIC                                    │
│   Solvers, Validators, Experts, DSLs                        │
├─────────────────────────────────────────────────────────────┤
│ Layer 4: SEARCH & STRATEGY                                  │
│   Meta-search, Repair loops, Composable solvers             │
├─────────────────────────────────────────────────────────────┤
│ Layer 3: EFFECTS & CONTROL                                  │
│   Streams, Nondeterminism, Budget, Sessions                 │
├─────────────────────────────────────────────────────────────┤
│ Layer 2: ORACLE & PROVENANCE                                │
│   Oracle protocol, Receipts, Inference, Replay              │
├─────────────────────────────────────────────────────────────┤
│ Layer 1: EVALUATION CORE                                    │
│   CEKS machine, Environments, Continuations, Effects        │
├─────────────────────────────────────────────────────────────┤
│ Layer 0: PRIMITIVES                                         │
│   cons/car/cdr, arithmetic, strings, predicates             │
└─────────────────────────────────────────────────────────────┘
```

---

## Key Questions to Answer

1. **What is OmegaLLM's reentrancy?**
   - How does machine stepping work?
   - How does forking work?
   - What can you do that you can't in other Lisps?

2. **What is LambdaRLM's domain algebra?**
   - How does it model problems?
   - What operations does it support?
   - Can it be layered on OmegaLLM?

3. **What does LambdaLLM specify that's unique?**
   - Facts and assertions
   - Fixpoint iteration
   - Expert roles
   - Are these just spec or partially implemented?

4. **What are the semantic conflicts?**
   - Different evaluation orders?
   - Incompatible effect systems?
   - Naming/API conflicts?

---

## Related Documents

- [LAMBDA-LLM--OMEGA-LLM-FEATURES.md](../docs/LAMBDA-LLM--OMEGA-LLM-FEATURES.md) - Current feature comparison
- [REFERENCE-ALGEBRA.md](../docs/REFERENCE-ALGEBRA.md) - Formal specification
- [ARCHITECTURE-EXPLANATION.md](../docs/ARCHITECTURE-EXPLANATION.md) - OmegaLLM architecture
- [INTEGRATION-ARCHITECTURE.md](../INTEGRATION-ARCHITECTURE.md) - Integration patterns

---

*Created: 2026-01-19*
*Estimated completion: 4-8 hours*
