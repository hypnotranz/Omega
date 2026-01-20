# JOB-004: Implement Core Magic (call/cc + Evidence)

**Priority**: P0 - Foundation
**Estimated Effort**: 2-3 days (revised down from 1 week - machinery already exists)
**Skills Required**: TypeScript, CEKS machine understanding, Lisp semantics
**Status**: NOT STARTED

> **MUST READ**: [LOGISTICS.md](LOGISTICS.md) before starting - covers testing, file locations, and proof of completion requirements.

### Quick Start

```bash
cd c:\Users\Richa\parmenides-dev\agent-harness\OmegaLLM

# Run existing tests (verify baseline)
npm run test

# After implementation, run new tests
npx vitest run test/continuations/

# Watch mode during development
npm run test:watch
```

### Output Files

| File | Action | Package |
|------|--------|---------|
| `src/core/eval/values.ts` | Add `ContinuationVal` type | `@omega/core` |
| `src/core/prims.ts` | Add `call/cc`, `call-with-prompt`, `abort-to-prompt` | `@omega/core` |
| `src/core/eval/machineStep.ts` | Handle continuation application | `@omega/core` |
| `src/core/pipeline/compileText.ts` | Register new primitives | `@omega/core` |
| `test/continuations/call-cc.spec.ts` | **CREATE** - call/cc tests | test |
| `test/continuations/delimited.spec.ts` | **CREATE** - delimited continuation tests | test |
| `test/evidence/evidence.spec.ts` | **CREATE** - evidence primitive tests | test |

### LambdaRLM Reference

| LambdaRLM Job | Relevance |
|---------------|-----------|
| [06-YIELD.md](../../LambdaRLM/CLAUDE-JOBS/06-YIELD.md) | Yield/partial results - different approach but related concepts |

---

## Executive Summary

Implement the "Phase A: Core Magic" items from the feature audit:

1. **Expose call/cc** - The CEKS machine already has continuations (`kont: Frame[]`). We just need a primitive to reify them as first-class values.
2. **Evidence primitives** - `Evidence` types already exist in meaning.ts. We need primitives to access them.
3. **Delimited continuations API** - Effect handlers already support resume. Add Racket-style `call-with-prompt`/`abort-to-prompt`.

**Why These First**: Everything else (conditions, proper amb, generators) depends on having call/cc exposed.

---

## What Already Exists

### Continuations (90% done)

**Files**: [machine.ts](../src/core/eval/machine.ts), [machineStep.ts](../src/core/eval/machineStep.ts)

```typescript
// State already has continuation stack
export type State = {
  control: Control;
  env: Env;
  store: Store;
  kont: Frame[];           // THE CONTINUATION IS RIGHT HERE
  handlers: HandlerFrame[];
  // ...
};

// machine-fork already clones entire state including kont
// Effect handlers already have resumeTo for resumable effects
```

**What's Missing**: A `call/cc` primitive that captures `kont` as a callable value.

### Evidence (60% done)

**File**: [meaning.ts](../src/core/oracle/meaning.ts)

```typescript
// Types already exist
export type Evidence =
  | { tag: "TestEvidence"; passed: number; total: number; receipt?: string }
  | { tag: "NoMatchEvidence"; pattern: Expr; searched: number; found: number }
  | { tag: "EqExtEvidence"; tests: number; allPassed: boolean; ... };

// Storage already exists in MeaningVal
export type MeaningVal = {
  evidence?: Evidence[];   // Already here!
  // ...
};
```

**What's Missing**: Primitives to access evidence from user code.

### Delimited Continuations (80% done)

**File**: [machineStep.ts](../src/core/eval/machineStep.ts)

```typescript
// KHandleBoundary already supports resumeTo
| { tag: "KHandleBoundary"; hid: string; savedHandlersDepth: number;
    resumeTo?: { kont: Frame[]; handlersDepth: number } }

// KHandleReturn already has exit vs resume modes
| { tag: "KHandleReturn"; mode: "exit" | "resume"; ... }
```

**What's Missing**: Racket-style `call-with-prompt` / `abort-to-prompt` API.

---

## Implementation Plan

### Task 1: Expose call/cc Primitive (4 hours)

**Goal**: Add `call/cc` (or `call-with-current-continuation`) primitive

**Files to Modify**:
- `src/core/prims.ts` - Add primitive definition
- `src/core/pipeline/compileText.ts` - Add to primitives list
- `src/core/eval/values.ts` - Add `ContinuationVal` type

**Implementation**:

```typescript
// In values.ts - Add continuation as a value type
export type ContinuationVal = {
  tag: "Continuation";
  kont: Frame[];
  env: Env;
  store: Store;
  handlers: HandlerFrame[];
};

// In prims.ts - Add call/cc primitive
def("call/cc", {
  arity: 1,
  fn: (args, state) => {
    const f = args[0];
    if (f.tag !== "Closure") throw new Error("call/cc expects a procedure");

    // Capture current continuation
    const k: ContinuationVal = {
      tag: "Continuation",
      kont: state.kont,
      env: state.env,
      store: state.store,
      handlers: state.handlers,
    };

    // Apply f to the continuation
    // Returns StepOutcome that applies f to k
    return applyProcedure(f, [k], state);
  }
});

// When a continuation is applied, restore its state
// This needs special handling in machineStep.ts
```

**Verification**:
```lisp
;; Test 1: Simple escape
(call/cc (lambda (k) (+ 1 (k 5))))  ; => 5

;; Test 2: Store and call later
(define saved-k #f)
(+ 1 (call/cc (lambda (k) (set! saved-k k) 10)))  ; => 11
(saved-k 100)  ; => 101

;; Test 3: Multiple returns (generator-like)
(define (make-generator proc)
  (let ((return #f) (resume #f))
    (lambda ()
      (call/cc (lambda (k)
        (if resume
            (resume k)
            (begin
              (set! return k)
              (proc (lambda (v)
                (call/cc (lambda (r)
                  (set! resume r)
                  (return v))))))))))))
```

### Task 2: Add Evidence Primitives (4 hours)

**Goal**: Expose evidence access from Lisp code

**Files to Modify**:
- `src/core/prims.ts` - Add evidence primitives
- `src/core/pipeline/compileText.ts` - Add to primitives list

**New Primitives**:

```typescript
// evidence-id: Extract ID from evidence-bearing value
def("evidence-id", {
  arity: 1,
  fn: (args) => {
    const val = args[0];
    if (isMeaning(val) && val.evidence && val.evidence.length > 0) {
      // Generate ID from evidence (hash of content)
      const id = hashEvidence(val.evidence[0]);
      return { tag: "Str", s: id };
    }
    return { tag: "Bool", b: false };
  }
});

// verify-evidence: Check evidence is still valid
def("verify-evidence", {
  arity: 1,
  fn: (args) => {
    const val = args[0];
    if (!isMeaning(val) || !val.evidence) {
      return { tag: "Bool", b: false };
    }
    // For TestEvidence, check that tests still pass
    // For NoMatchEvidence, check pattern still doesn't match
    // For EqExtEvidence, re-run equivalence tests
    return verifyEvidenceChain(val.evidence);
  }
});

// evidence-stale?: Check if source has changed
def("evidence-stale?", {
  arity: 1,
  fn: (args) => {
    const val = args[0];
    if (!isMeaning(val) || !val.evidence) {
      return { tag: "Bool", b: true };  // No evidence = stale
    }
    // Check receipt timestamps, source hashes, etc.
    return checkEvidenceStaleness(val.evidence);
  }
});
```

**Verification**:
```lisp
;; Test evidence access
(define result (oracle-infer "What is 2+2?"))
(evidence-id result)           ; => "ev-abc123"
(verify-evidence result)       ; => #t
(evidence-stale? result)       ; => #f
```

### Task 3: Add Delimited Continuation API (4 hours)

**Goal**: Add Racket-style `call-with-prompt` / `abort-to-prompt`

**Why**: Effect handlers already support this, but the API isn't Racket-compatible.

**New Primitives**:

```typescript
// call-with-prompt: Establish a prompt (delimited continuation boundary)
// (call-with-prompt tag thunk handler)
def("call-with-prompt", {
  arity: 3,
  fn: (args, state) => {
    const [tag, thunk, handler] = args;
    // Push a prompt frame
    // When abort-to-prompt is called with this tag,
    // invoke handler with the captured continuation
    return installPrompt(tag, thunk, handler, state);
  }
});

// abort-to-prompt: Abort to a prompt, capturing continuation
// (abort-to-prompt tag value)
def("abort-to-prompt", {
  arity: 2,
  fn: (args, state) => {
    const [tag, value] = args;
    // Find the prompt frame with matching tag
    // Capture continuation up to that point
    // Call the handler with (k, value)
    return abortToPrompt(tag, value, state);
  }
});
```

**Verification**:
```lisp
;; Test delimited continuation
(call-with-prompt 'my-prompt
  (lambda () (+ 1 (abort-to-prompt 'my-prompt 5)))
  (lambda (k v) (* v 2)))  ; => 10 (5 * 2)

;; Test resumption
(call-with-prompt 'my-prompt
  (lambda () (+ 1 (abort-to-prompt 'my-prompt 5)))
  (lambda (k v) (k (* v 2))))  ; => 11 (1 + 10)
```

---

## Test Plan

### call/cc Tests (`test/continuations/call-cc.spec.ts`)

#### Happy Path Tests

```lisp
;; HP-1: Simple escape - continuation invoked immediately
(call/cc (lambda (k) (k 42)))  ; => 42

;; HP-2: Escape from nested computation
(call/cc (lambda (k) (+ 1 (+ 2 (k 5)))))  ; => 5 (skips additions)

;; HP-3: Store continuation and call later
(define saved-k #f)
(+ 1 (call/cc (lambda (k) (set! saved-k k) 10)))  ; => 11
(saved-k 100)  ; => 101

;; HP-4: Multiple invocations of same continuation
(define k-saved #f)
(define counter 0)
(+ 1 (call/cc (lambda (k) (set! k-saved k) 0)))
(set! counter (+ counter 1))
(if (< counter 3) (k-saved counter) counter)  ; => eventually 3

;; HP-5: Continuation as argument to higher-order function
(define (try-with-escape f)
  (call/cc (lambda (escape)
    (f escape))))
(try-with-escape (lambda (e) (+ 1 (e 10) 100)))  ; => 10
```

#### Edge Case Tests

```lisp
;; EC-1: Continuation not invoked - normal return
(call/cc (lambda (k) 42))  ; => 42 (k never called)

;; EC-2: Nested call/cc - inner escapes outer
(call/cc (lambda (outer)
  (call/cc (lambda (inner)
    (outer 'escaped)))))  ; => 'escaped

;; EC-3: Continuation invoked after original context returned
(define late-k #f)
(define (capture) (call/cc (lambda (k) (set! late-k k) 'first)))
(capture)  ; => 'first
(late-k 'second)  ; => 'second (jumps back!)

;; EC-4: Continuation invoked with wrong arity - should error or handle gracefully
;; (Depending on semantics: error or take first arg)

;; EC-5: Continuation and mutation interaction
(define x 0)
(call/cc (lambda (k)
  (set! x 1)
  (k 'done)
  (set! x 2)))  ; x should be 1, not 2
x  ; => 1

;; EC-6: Continuation within amb - interaction with nondeterminism
;; Should either: (a) capture amb state, or (b) error if incompatible
```

#### Error Cases

```lisp
;; ERR-1: call/cc with non-procedure argument
(call/cc 42)  ; => Error: call/cc expects a procedure

;; ERR-2: call/cc with wrong arity procedure
(call/cc (lambda () 42))  ; => Error: procedure must accept 1 argument
```

### Delimited Continuation Tests (`test/continuations/delimited.spec.ts`)

#### Happy Path Tests

```lisp
;; HP-1: Basic prompt/abort - handler receives value
(call-with-prompt 'p
  (lambda () (+ 1 (abort-to-prompt 'p 5)))
  (lambda (k v) (* v 2)))  ; => 10 (5 * 2, ignoring k)

;; HP-2: Resume continuation after abort
(call-with-prompt 'p
  (lambda () (+ 1 (abort-to-prompt 'p 5)))
  (lambda (k v) (k (* v 2))))  ; => 11 (1 + 10)

;; HP-3: Multiple aborts with resume
(call-with-prompt 'p
  (lambda ()
    (+ (abort-to-prompt 'p 1)
       (abort-to-prompt 'p 2)))
  (lambda (k v) (k (* v 10))))  ; => 30 (10 + 20)

;; HP-4: Nested prompts with different tags
(call-with-prompt 'outer
  (lambda ()
    (call-with-prompt 'inner
      (lambda () (abort-to-prompt 'inner 5))
      (lambda (k v) (* v 2))))
  (lambda (k v) (+ v 100)))  ; => 10 (inner handles it)

;; HP-5: Abort to outer prompt, skipping inner
(call-with-prompt 'outer
  (lambda ()
    (call-with-prompt 'inner
      (lambda () (abort-to-prompt 'outer 5))
      (lambda (k v) (* v 2))))
  (lambda (k v) (+ v 100)))  ; => 105 (outer handles it)
```

#### Edge Case Tests

```lisp
;; EC-1: No abort - normal completion
(call-with-prompt 'p
  (lambda () 42)
  (lambda (k v) 'never-called))  ; => 42

;; EC-2: Abort with no matching prompt
(call-with-prompt 'wrong
  (lambda () (abort-to-prompt 'p 5))
  (lambda (k v) 'wrong))  ; => Error: No prompt 'p found

;; EC-3: Captured continuation used multiple times
(define k-delim #f)
(call-with-prompt 'p
  (lambda () (+ 1 (abort-to-prompt 'p 'capture)))
  (lambda (k v) (set! k-delim k) 'captured))
(k-delim 10)  ; => 11
(k-delim 20)  ; => 21

;; EC-4: Interaction with call/cc inside prompt
(call-with-prompt 'p
  (lambda ()
    (call/cc (lambda (k)
      (abort-to-prompt 'p k))))
  (lambda (k v) (v 42)))  ; Behavior depends on semantics
```

### Evidence Primitive Tests (`test/evidence/evidence.spec.ts`)

#### Happy Path Tests

```lisp
;; HP-1: evidence-id returns ID for oracle result
(define result (oracle-infer "What is 2+2?"))
(evidence-id result)  ; => "ev-abc123..." (string)

;; HP-2: verify-evidence returns #t for valid evidence
(define result (oracle-infer "test"))
(verify-evidence result)  ; => #t

;; HP-3: evidence-stale? returns #f for fresh evidence
(define result (oracle-infer "test"))
(evidence-stale? result)  ; => #f
```

#### Edge Case Tests

```lisp
;; EC-1: evidence-id on value without evidence
(evidence-id 42)  ; => #f (no evidence)
(evidence-id '(1 2 3))  ; => #f

;; EC-2: verify-evidence on value without evidence
(verify-evidence 42)  ; => #f

;; EC-3: evidence-stale? on value without evidence
(evidence-stale? 42)  ; => #t (no evidence = stale)

;; EC-4: evidence-id on derived value (should propagate?)
(define a (oracle-infer "x"))
(define b (oracle-infer "y"))
(define c (+ a b))  ; Does c have evidence?
(evidence-id c)  ; Depends on design: #f or combined evidence
```

### Integration Tests

1. **Generator implementation using call/cc** - Full working generator
2. **Coroutine implementation using prompts** - Producer/consumer
3. **Exception-like control flow using abort** - Try/catch pattern
4. **amb + call/cc interaction** - Verify they compose correctly
5. **Evidence propagation through computations** - Multi-step derivation

---

## Checklist

### Task 1: call/cc
- [ ] Add `ContinuationVal` type to values.ts
- [ ] Add `call/cc` primitive to prims.ts
- [ ] Add continuation application handling to machineStep.ts
- [ ] Add `call/cc` to compileText.ts primitives list
- [ ] Write unit tests
- [ ] Test with REPL

### Task 2: Evidence Primitives
- [ ] Add `evidence-id` primitive
- [ ] Add `verify-evidence` primitive
- [ ] Add `evidence-stale?` primitive
- [ ] Add hash function for evidence
- [ ] Write unit tests

### Task 3: Delimited Continuations
- [ ] Add `call-with-prompt` primitive
- [ ] Add `abort-to-prompt` primitive
- [ ] Test interaction with existing effect handlers
- [ ] Write unit tests

### Verification
- [ ] All existing tests still pass (1124)
- [ ] New continuation tests pass
- [ ] REPL examples work

---

## Package Allocation

This job spans **two packages**:

| Task | Package | Rationale |
|------|---------|-----------|
| Task 1: call/cc | `@omega/core` | Essential for CEKS machine - everything else builds on this |
| Task 2: Evidence primitives | `@omega/provenance` | Optional - only needed for LLM audit trails |
| Task 3: Delimited continuations | `@omega/core` | Essential for effect handlers |

**Note**: Task 2 can be deferred if provenance is not needed. Tasks 1 and 3 are **required** for the core language.

---

## Dependencies

- **Depends on**: Jobs 001-003 (primitives working, naming fixed, audit complete)
- **Blocks**: Job 005 (Non-unwinding Conditions), Job 007 (Full Provenance - needs Task 2)

---

## Notes

### Why call/cc is Lower Effort Than Expected

The CEKS machine design is *perfect* for call/cc:

1. **State is already a value** - `State` is just data, easily cloned
2. **Continuation is explicit** - `kont: Frame[]` is the K in CEKS
3. **machine-fork exists** - Already clones full state
4. **Effect handlers have resume** - Already capture partial continuations

We're not building call/cc - we're just *exposing* what's already there.

### Evidence Model Design Decision

The existing `Evidence` type focuses on test results (TestEvidence, EqExtEvidence). For full LLM provenance, we may want to add:

- `OracleEvidence` - Record of oracle call with request/response
- `TransformEvidence` - Record of code transformation with before/after

But that's for a future job. This job focuses on exposing what exists.

---

---

## Proof of Completion

When marking this job DONE:

1. **Build passes**: `npm run build` - no TypeScript errors
2. **Baseline tests pass**: `npm run test` - 1124+ tests still green
3. **New tests pass**:
   - `npx vitest run test/continuations/call-cc.spec.ts`
   - `npx vitest run test/continuations/delimited.spec.ts`
   - `npx vitest run test/evidence/evidence.spec.ts` (if Task 2 done)
4. **REPL verification**: Test examples from Verification section manually
5. **Update status**: Change `NOT STARTED` → `DONE` in this file
6. **Update README**: Mark Job 004 as DONE in [README.md](README.md)

---

*Created: 2026-01-19*
*Related: [003-COMPREHENSIVE-FEATURE-AUDIT-AND-LAYERING.md](./003-COMPREHENSIVE-FEATURE-AUDIT-AND-LAYERING.md)*
*See: [LAMBDA-LLM--OMEGA-LLM-FEATURES.md](../docs/LAMBDA-LLM--OMEGA-LLM-FEATURES.md) for full audit*
