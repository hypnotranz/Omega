# JOB-006: Monadic Primitives (unit/mzero/mplus/bind)

**Priority**: P1 - Important (Phase B)
**Estimated Effort**: 4-6 hours
**Skills Required**: TypeScript, Monad concepts, Lisp
**Status**: NOT STARTED

> **MUST READ**: [LOGISTICS.md](LOGISTICS.md) before starting - covers testing, file locations, and proof of completion requirements.

### Quick Start

```bash
cd c:\Users\Richa\parmenides-dev\agent-harness\OmegaLLM

# This job can run in parallel with Job 004 (no dependency)
# Run existing tests (verify baseline)
npm run test

# After implementation, run new tests
npx vitest run test/monad/

# Watch mode during development
npm run test:watch
```

### Output Files

| File | Action | Package |
|------|--------|---------|
| `src/core/prims.ts` | Add `unit`, `mzero`, `mplus`, `bind` primitives | `@omega/core` |
| `src/core/eval/machine.ts` | Add `KBind` frame type | `@omega/core` |
| `src/core/eval/machineStep.ts` | Handle `KBind` frame | `@omega/core` |
| `src/core/pipeline/compileText.ts` | Register new primitives | `@omega/core` |
| `lib/monad.lisp` | **CREATE** - mdo macro, guard, msum, mfilter | `@omega/core` |
| `test/monad/primitives.spec.ts` | **CREATE** - unit/mzero/mplus/bind tests | test |
| `test/monad/laws.spec.ts` | **CREATE** - monad law verification | test |

### LambdaRLM Reference

| LambdaRLM Job | Relevance |
|---------------|-----------|
| [08-NONDET.md](../../LambdaRLM/CLAUDE-JOBS/08-NONDET.md) | Nondeterminism - OmegaLLM already has amb, this adds clean interface |

---

## Executive Summary

Add **explicit monadic interface** for nondeterminism and composition:

- `unit` - Lift a value into a computation
- `mzero` - The failing/empty computation
- `mplus` - Combine computations (choice/alternatives)
- `bind` (or `>>=`) - Sequence computations

OmegaLLM already has `amb` for nondeterminism, but lacks the clean monadic interface that LambdaRLM uses for composable search and result handling.

**Why This Matters**: Without explicit monadic composition, combining search strategies is awkward. With it, you can write:

```lisp
;; Clean monadic composition
(bind (search-space-1)
  (lambda (x)
    (bind (search-space-2 x)
      (lambda (y)
        (unit (combine x y))))))

;; vs. nested amb mess
(let ((x (amb ...)))
  (let ((y (amb ...)))
    (combine x y)))
```

---

## Package

**Package**: `@omega/core`

This belongs in the **core package** - essential for clean amb composition.

```
@omega/core/
├── eval/           # CEKS machine (add KBind frame here)
├── prims/          # Add unit, mzero, mplus, bind here
└── lib/monad.lisp  # mdo macro, guard, msum, mfilter
```

**Rationale**: Without explicit monadic composition, `amb` is awkward to use. These primitives make nondeterministic search composable and are used by higher-level packages like `@omega/solver`.

---

## Dependencies

- **REQUIRES**: None (can be done in **parallel** with [Job 004](004-IMPLEMENT-CORE-MAGIC.md))
- **Builds on**: Existing `amb` effect system in `src/core/effects/nondet/`
- **Blocks**: [Job 008](008-SEARCH-PATTERNS-SOLVERS.md) (Solver Patterns use monadic composition for `compose-*` combinators)

---

## What Already Exists

### amb Effect System

**Files**: [src/core/nondet/](../src/core/nondet/), [src/core/prims.ts](../src/core/prims.ts)

```typescript
// amb exists and works
def("amb", { ... });  // Nondeterministic choice

// Effect handlers for amb
// Frontier kinds: DFS, BFS, Beam
```

**Gap**: No explicit `unit`, `mzero`, `mplus`, `bind` API. Users must use raw `amb`.

### Stream System

**Files**: [src/core/stream/](../src/core/stream/)

Streams are also monadic (lazy list monad). We could unify the interface.

---

## Design: List Monad Interface

### Core Primitives

| Primitive | Semantics | Implementation |
|-----------|-----------|----------------|
| `unit` | `(unit x)` → computation returning `x` | Already exists (returns value) |
| `mzero` | Empty computation (failure) | `(amb)` with no args |
| `mplus` | `(mplus m1 m2)` → try m1, then m2 | `(amb m1 m2)` under the hood |
| `bind` | `(bind m f)` → for each x in m, run (f x) | flatMap / concatMap |

### Type Signatures (Conceptual)

```
unit  : a -> M a
mzero : M a
mplus : M a -> M a -> M a
bind  : M a -> (a -> M b) -> M b
```

Where `M` is the nondeterminism monad (backed by amb + frontiers).

---

## Implementation Plan

### Task 1: Add Monadic Primitives (2 hours)

**File to Modify**: `src/core/prims.ts`

```typescript
// unit: Lift a value into the monad
// (unit x) => returns x
// This is essentially identity for our implicit monad
def("unit", {
  arity: 1,
  pure: true,
  fn: (args) => args[0],
});

// mzero: The failing computation
// (mzero) => fails the current branch (no results)
def("mzero", {
  arity: 0,
  fn: (args, state) => {
    // Signal amb failure - equivalent to (amb) with no choices
    return {
      tag: "Effect",
      effect: { tag: "Amb", choices: [] },
      state,
    };
  }
});

// mplus: Combine two computations (choice)
// (mplus m1 m2) => try m1, if it fails try m2
def("mplus", {
  arity: 2,
  fn: (args, state) => {
    const [m1, m2] = args;
    // Create a choice point between two thunks
    // Need to evaluate m1 and m2 as computations
    return {
      tag: "Effect",
      effect: {
        tag: "Amb",
        choices: [
          { thunk: m1 },
          { thunk: m2 }
        ]
      },
      state,
    };
  }
});

// bind: Sequence computations (flatMap)
// (bind m f) => for each result x of m, run (f x)
def("bind", {
  arity: 2,
  fn: (args, state) => {
    const [m, f] = args;

    // m is a thunk that may produce multiple results via amb
    // f is a function a -> M b

    // We need to:
    // 1. Run m to get results
    // 2. For each result x, run (f x)
    // 3. Combine all results

    // This is essentially: run m, then apply f to each result
    // The amb effect handler naturally does this for us

    // Step 1: Evaluate m (it's a thunk)
    // Step 2: Push a continuation that applies f
    return {
      control: { tag: "Apply", fn: m, args: [] },
      env: state.env,
      store: state.store,
      kont: [
        { tag: "KBind", f, next: state.kont }
      ],
      handlers: state.handlers,
    };
  }
});
```

### Task 2: Add KBind Frame (1 hour)

**File to Modify**: `src/core/eval/machine.ts`

```typescript
export type Frame =
  | ... // existing frames
  | { tag: "KBind"; f: ClosureVal; next: Frame[]; }
```

**File to Modify**: `src/core/eval/machineStep.ts`

```typescript
case "KBind": {
  // m has returned a value, now apply f to it
  const f = frame.f;
  const x = value;

  // Apply f to x, continue with rest of continuation
  return {
    control: { tag: "Apply", fn: f, args: [x] },
    env: state.env,
    store: state.store,
    kont: frame.next,
    handlers: state.handlers,
  };
}
```

### Task 3: Add Convenience Macros (1 hour)

**File to Create**: `lib/monad.lisp` (or add to stdlib)

```lisp
;; do-notation style macro
;; (mdo (x <- m1)
;;      (y <- m2)
;;      (unit (+ x y)))
;; =>
;; (bind m1 (lambda (x)
;;   (bind m2 (lambda (y)
;;     (unit (+ x y))))))

(define-syntax mdo
  (syntax-rules (<-)
    [(_ expr) expr]
    [(_ (var <- m) rest ...)
     (bind m (lambda (var) (mdo rest ...)))]
    [(_ m rest ...)
     (bind m (lambda (_) (mdo rest ...)))]))

;; guard: Filter results
;; (guard pred) => (if pred (unit #t) (mzero))
(define (guard pred)
  (if pred (unit #t) (mzero)))

;; msum: Combine list of computations
;; (msum (list m1 m2 m3)) => (mplus m1 (mplus m2 m3))
(define (msum ms)
  (if (null? ms)
      (mzero)
      (mplus (car ms) (msum (cdr ms)))))

;; mfilter: Filter results of a computation
;; (mfilter pred m) => results of m that satisfy pred
(define (mfilter pred m)
  (bind m (lambda (x)
    (if (pred x) (unit x) (mzero)))))

;; mconcatMap: map then flatten
;; (mconcatMap f xs) => combine all (f x) for x in xs
(define (mconcatMap f xs)
  (if (null? xs)
      (mzero)
      (mplus (f (car xs)) (mconcatMap f (cdr xs)))))
```

### Task 4: Add to Primitive List (30 min)

**File to Modify**: `src/core/pipeline/compileText.ts`

```typescript
const prims = [
  // ... existing
  // Monadic primitives
  "unit", "mzero", "mplus", "bind",
];
```

---

## Verification

### Test 1: Basic Monad Laws

```lisp
;; Left identity: (bind (unit x) f) = (f x)
(define (test-left-identity)
  (let ((f (lambda (x) (unit (* x 2)))))
    (equal?
      (collect-all (bind (unit 5) f))
      (collect-all (f 5)))))  ; => #t

;; Right identity: (bind m unit) = m
(define (test-right-identity)
  (let ((m (mplus (unit 1) (unit 2))))
    (equal?
      (collect-all (bind m unit))
      (collect-all m))))  ; => #t

;; Associativity: (bind (bind m f) g) = (bind m (lambda (x) (bind (f x) g)))
(define (test-associativity)
  (let ((m (mplus (unit 1) (unit 2)))
        (f (lambda (x) (unit (* x 2))))
        (g (lambda (x) (unit (+ x 1)))))
    (equal?
      (collect-all (bind (bind m f) g))
      (collect-all (bind m (lambda (x) (bind (f x) g)))))))  ; => #t
```

### Test 2: Nondeterministic Search

```lisp
;; Find all pairs (x, y) where x + y = 5
(define (pairs-summing-to-5)
  (mdo (x <- (msum (map unit '(1 2 3 4))))
       (y <- (msum (map unit '(1 2 3 4))))
       (guard (= (+ x y) 5))
       (unit (list x y))))

(collect-all (pairs-summing-to-5))
;; => ((1 4) (2 3) (3 2) (4 1))
```

### Test 3: Composing Searches

```lisp
;; Search for valid configs
(define (valid-configs)
  (mdo (db <- (choose-db))
       (cache <- (choose-cache))
       (guard (compatible? db cache))
       (unit (make-config db cache))))

(define (choose-db)
  (msum (map unit '(postgres mysql sqlite))))

(define (choose-cache)
  (msum (map unit '(redis memcached none))))

(first-result (valid-configs))  ; => first compatible config
```

---

## Test Plan

### Unit Tests (`test/monad/primitives.spec.ts`)

#### unit Tests

```lisp
;; HP-1: unit with simple value
(unit 42)
;; => 42

;; HP-2: unit with compound value
(unit (list 1 2 3))
;; => (1 2 3)

;; HP-3: unit with closure
(unit (lambda (x) x))
;; => <closure>

;; EC-1: unit with #f (falsy value)
(unit #f)
;; => #f (not failure!)

;; EC-2: unit with ()
(unit '())
;; => ()
```

#### mzero Tests

```lisp
;; HP-1: mzero causes branch failure
(collect-all (mzero))
;; => () (empty list - no results)

;; HP-2: mzero in mdo stops computation
(collect-all (mdo (_ <- (mzero)) (unit 42)))
;; => () (never reaches unit)

;; EC-1: mzero after successful computation
(collect-all (mdo (x <- (unit 1)) (_ <- (mzero)) (unit x)))
;; => () (mzero discards earlier work)
```

#### mplus Tests

```lisp
;; HP-1: mplus of two units
(collect-all (mplus (unit 1) (unit 2)))
;; => (1 2)

;; HP-2: mplus with three alternatives
(collect-all (mplus (unit 'a) (mplus (unit 'b) (unit 'c))))
;; => (a b c)

;; HP-3: mplus associativity
(equal?
  (collect-all (mplus (mplus (unit 1) (unit 2)) (unit 3)))
  (collect-all (mplus (unit 1) (mplus (unit 2) (unit 3)))))
;; => #t

;; EC-1: mplus with mzero (left identity of mzero)
(collect-all (mplus (mzero) (unit 42)))
;; => (42)

;; EC-2: mplus with mzero (right identity of mzero)
(collect-all (mplus (unit 42) (mzero)))
;; => (42)

;; EC-3: mplus of two mzeros
(collect-all (mplus (mzero) (mzero)))
;; => ()

;; EC-4: mplus with nested computations that fail
(collect-all (mplus
               (mdo (x <- (unit 1)) (_ <- (mzero)) (unit x))
               (unit 2)))
;; => (2) (first branch fails, second succeeds)
```

#### bind Tests

```lisp
;; HP-1: bind with identity function
(collect-all (bind (unit 42) (lambda (x) (unit x))))
;; => (42)

;; HP-2: bind chains computations
(collect-all (bind (unit 5) (lambda (x) (unit (* x 2)))))
;; => (10)

;; HP-3: bind with multiple results
(collect-all (bind (mplus (unit 1) (unit 2))
                   (lambda (x) (unit (* x 10)))))
;; => (10 20)

;; HP-4: bind flattens nested nondeterminism
(collect-all (bind (mplus (unit 1) (unit 2))
                   (lambda (x) (mplus (unit x) (unit (- x))))))
;; => (1 -1 2 -2)

;; EC-1: bind with mzero producer
(collect-all (bind (mzero) (lambda (x) (unit x))))
;; => ()

;; EC-2: bind with function that always fails
(collect-all (bind (mplus (unit 1) (unit 2)) (lambda (x) (mzero))))
;; => ()

;; EC-3: bind where some branches fail
(collect-all (bind (mplus (unit 1) (unit 2) (unit 3))
                   (lambda (x) (if (even? x) (unit x) (mzero)))))
;; => (2)

;; ERR-1: bind with non-procedure second argument
(bind (unit 1) 42)
;; => Error: bind expects a procedure as second argument
```

### Monad Laws Tests (`test/monad/laws.spec.ts`)

```lisp
;; LAW-1: Left identity - (bind (unit x) f) = (f x)
(define (test-left-identity)
  (let ((x 5)
        (f (lambda (n) (unit (* n 2)))))
    (equal?
      (collect-all (bind (unit x) f))
      (collect-all (f x)))))
;; => #t

;; LAW-2: Right identity - (bind m unit) = m
(define (test-right-identity)
  (let ((m (mplus (unit 1) (unit 2) (unit 3))))
    (equal?
      (collect-all (bind m unit))
      (collect-all m))))
;; => #t

;; LAW-3: Associativity - (bind (bind m f) g) = (bind m (λ (x) (bind (f x) g)))
(define (test-associativity)
  (let ((m (mplus (unit 1) (unit 2)))
        (f (lambda (x) (mplus (unit x) (unit (* x 10)))))
        (g (lambda (x) (unit (+ x 1)))))
    (equal?
      (collect-all (bind (bind m f) g))
      (collect-all (bind m (lambda (x) (bind (f x) g)))))))
;; => #t

;; LAW-4: mzero is left zero for bind - (bind mzero f) = mzero
(define (test-left-zero)
  (let ((f (lambda (x) (unit (* x 2)))))
    (equal?
      (collect-all (bind (mzero) f))
      (collect-all (mzero)))))
;; => #t

;; LAW-5: mzero is right zero for bind - (bind m (λ (_) mzero)) = mzero
(define (test-right-zero)
  (let ((m (mplus (unit 1) (unit 2))))
    (equal?
      (collect-all (bind m (lambda (_) (mzero))))
      (collect-all (mzero)))))
;; => #t
```

### guard Tests

```lisp
;; HP-1: guard with true condition
(collect-all (mdo (_ <- (guard #t)) (unit 'ok)))
;; => (ok)

;; HP-2: guard with false condition
(collect-all (mdo (_ <- (guard #f)) (unit 'ok)))
;; => ()

;; HP-3: guard filters results
(collect-all (mdo (x <- (msum (map unit '(1 2 3 4 5))))
                  (_ <- (guard (even? x)))
                  (unit x)))
;; => (2 4)

;; EC-1: guard with computed predicate
(collect-all (mdo (x <- (mplus (unit 5) (unit 10)))
                  (y <- (mplus (unit 1) (unit 2)))
                  (_ <- (guard (> x (* y 3))))
                  (unit (list x y))))
;; => ((5 1) (10 1) (10 2) (10 3))

;; EC-2: guard as first action
(collect-all (mdo (_ <- (guard (= 1 1))) (unit 'yes)))
;; => (yes)
```

### msum Tests

```lisp
;; HP-1: msum of list of units
(collect-all (msum (list (unit 1) (unit 2) (unit 3))))
;; => (1 2 3)

;; HP-2: msum with map for enumeration
(collect-all (msum (map unit '(a b c))))
;; => (a b c)

;; EC-1: msum of empty list
(collect-all (msum '()))
;; => ()

;; EC-2: msum of single element
(collect-all (msum (list (unit 42))))
;; => (42)

;; EC-3: msum with some failing branches
(collect-all (msum (list (mzero) (unit 1) (mzero) (unit 2))))
;; => (1 2)
```

### mdo Macro Tests

```lisp
;; HP-1: mdo with single binding
(collect-all (mdo (x <- (unit 5)) (unit (* x 2))))
;; => (10)

;; HP-2: mdo with multiple bindings
(collect-all (mdo (x <- (unit 2))
                  (y <- (unit 3))
                  (unit (* x y))))
;; => (6)

;; HP-3: mdo with nondeterminism
(collect-all (mdo (x <- (mplus (unit 1) (unit 2)))
                  (y <- (mplus (unit 10) (unit 20)))
                  (unit (+ x y))))
;; => (11 21 12 22)

;; EC-1: mdo with action without binding
(collect-all (mdo (display "side effect")
                  (unit 42)))
;; => (42) and prints "side effect"

;; EC-2: mdo referencing earlier bindings
(collect-all (mdo (x <- (unit 5))
                  (y <- (unit (* x 2)))
                  (z <- (unit (+ x y)))
                  (unit z)))
;; => (15)
```

### Integration Tests

1. Search problems using mdo notation
2. Combining with existing amb-based code
3. Performance comparison vs raw amb
4. Interaction with effects and handlers

---

## Checklist

### Task 1: Core Primitives
- [ ] Implement `unit` primitive
- [ ] Implement `mzero` primitive
- [ ] Implement `mplus` primitive
- [ ] Implement `bind` primitive

### Task 2: CEKS Support
- [ ] Add `KBind` frame to machine.ts
- [ ] Add KBind handling to machineStep.ts

### Task 3: Convenience Library
- [ ] Create `lib/monad.lisp` or add to stdlib
- [ ] Implement `mdo` macro
- [ ] Implement `guard` function
- [ ] Implement `msum` function
- [ ] Implement `mfilter` function
- [ ] Implement `mconcatMap` function

### Task 4: Registration
- [ ] Add primitives to compileText.ts

### Verification
- [ ] Monad laws pass
- [ ] All existing tests pass (1124+)
- [ ] REPL examples work
- [ ] Integration with amb works

---

## Notes

### Why Explicit Monadic Interface?

1. **Cleaner composition** - `bind` chains read better than nested `let`+`amb`
2. **Abstraction** - Can swap monad implementations (list, maybe, either)
3. **LambdaRLM compatibility** - Uses these primitives extensively
4. **Academic foundation** - Well-understood semantics

### Relationship to Streams

Streams are also a monad (the lazy list monad). We could potentially unify:

```lisp
;; Stream monad
(define stream-unit stream-cons)
(define stream-mzero the-empty-stream)
(define stream-mplus stream-append)
(define stream-bind stream-flatmap)
```

This would allow the same `mdo` notation for lazy and eager search.

### Performance Consideration

`bind` creates a continuation frame per call. For deep compositions:
- Consider tail-call optimization
- Consider batching results before continuing
- Profile against raw amb for hot paths

---

---

## Proof of Completion

When marking this job DONE:

1. **Build passes**: `npm run build` - no TypeScript errors
2. **Baseline tests pass**: `npm run test` - 1124+ tests still green
3. **New tests pass**:
   - `npx vitest run test/monad/primitives.spec.ts`
   - `npx vitest run test/monad/laws.spec.ts`
4. **Monad laws verified**: All three laws (left identity, right identity, associativity) pass
5. **REPL verification**: Test examples from Verification section manually
6. **Update status**: Change `NOT STARTED` → `DONE` in this file
7. **Update README**: Mark Job 006 as DONE in [README.md](README.md)
8. **Unblock Job 008**: Notify that monadic primitives are ready

---

*Created: 2026-01-19*
*Related: [004-IMPLEMENT-CORE-MAGIC.md](./004-IMPLEMENT-CORE-MAGIC.md)*
*Can run in parallel with Job 004*
