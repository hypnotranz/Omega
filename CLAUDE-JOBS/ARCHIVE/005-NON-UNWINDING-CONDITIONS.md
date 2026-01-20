# JOB-005: Non-Unwinding Conditions (Resumable Error Handling)

**Priority**: P1 - Important (Phase B)
**Estimated Effort**: 8-12 hours
**Skills Required**: TypeScript, CEKS machine, Lisp condition systems
**Status**: DONE

> **MUST READ**: [LOGISTICS.md](LOGISTICS.md) before starting - covers testing, file locations, and proof of completion requirements.

### Quick Start

```bash
cd c:\Users\Richa\parmenides-dev\agent-harness\OmegaLLM

# Verify Job 004 is complete first (this job depends on it)
npx vitest run test/continuations/

# Run existing tests (verify baseline)
npm run test

# After implementation, run new tests
npx vitest run test/conditions/

# Watch mode during development
npm run test:watch
```

### Output Files

| File | Action | Package |
|------|--------|---------|
| `src/core/conditions/types.ts` | **CREATE** - ConditionVal, RestartPoint, etc. | `@omega/conditions` |
| `src/core/conditions/frames.ts` | **CREATE** - KHandlerBind, KRestartBind frames | `@omega/conditions` |
| `src/core/conditions/prims.ts` | **CREATE** - signal, error, invoke-restart, etc. | `@omega/conditions` |
| `src/core/eval/machine.ts` | Add new frame types | `@omega/core` |
| `src/core/eval/machineStep.ts` | Handle condition frames | `@omega/core` |
| `src/core/pipeline/compileText.ts` | Add handler-bind, restart-bind special forms | `@omega/core` |
| `test/conditions/signal.spec.ts` | **CREATE** - signal/handler tests | test |
| `test/conditions/restart.spec.ts` | **CREATE** - restart tests | test |
| `test/conditions/error.spec.ts` | **CREATE** - error handling tests | test |

### LambdaRLM Reference

| LambdaRLM Job | Relevance |
|---------------|-----------|
| [03-FAILURE.md](../../LambdaRLM/CLAUDE-JOBS/03-FAILURE.md) | Structured failure - simpler version of what we're building |

---

## Executive Summary

Implement a **non-unwinding condition system** that allows error handlers to fix problems and **resume execution at the error site**, rather than unwinding the stack like exceptions.

This is distinct from OmegaLLM's existing actor supervisors (which restart/stop actors) - this is about **in-place repair** within a single computation.

**Why This Matters**: LLM-driven code often encounters recoverable errors (parsing failures, validation mismatches). Instead of failing and starting over, a condition system lets handlers provide fixes that resume the computation seamlessly.

---

## Package

**Package**: `@omega/conditions` (optional)

This is an **optional package** - not required for basic OmegaLLM operation.

```
@omega/conditions/
├── types.ts        # ConditionVal, RestartPoint, ConditionHandler, RestartBinding
├── frames.ts       # KHandlerBind, KRestartBind, KSignaling frames
└── prims.ts        # signal, error, handler-bind, restart-bind, invoke-restart, etc.
```

**Use Cases**:
- LLM-driven code repair
- Graceful degradation in agentic workflows
- Debugger integration

**Dependency**: Requires `@omega/core` (specifically call/cc from Job 004)

---

## Dependencies

- **REQUIRES**: [Job 004](004-IMPLEMENT-CORE-MAGIC.md) **MUST BE COMPLETE FIRST**
  - `call/cc` (from Task 1) needed to capture continuation at error site
  - `call-with-prompt`/`abort-to-prompt` (from Task 3) needed for structured control flow
  - Verify: `npx vitest run test/continuations/` must pass before starting this job
- **Blocks**: [Job 008](008-SEARCH-PATTERNS-SOLVERS.md) (Solver Patterns - `repair-until-valid` needs conditions)

---

## What Already Exists

### Actor Supervisors (Different Thing)

**File**: [src/core/concurrency/supervisors.ts](../src/core/concurrency/supervisors.ts)

```typescript
// Actor supervisors can restart or stop failed actors
type SupervisorStrategy = "one-for-one" | "all-for-one" | "rest-for-one";
// But they can't RESUME a computation in-place
```

**Not What We Need**: Supervisors restart entire actors, not individual computations.

### Effect Handlers (Partial Foundation)

**File**: [src/core/eval/machineStep.ts](../src/core/eval/machineStep.ts)

```typescript
// Effect handlers can already resume
| { tag: "KHandleReturn"; mode: "exit" | "resume"; ... }
```

**Can Build On**: Effect handlers have resume machinery we can repurpose.

---

## Design: Common Lisp-Style Conditions

### The Model

```
┌─────────────────────────────────────────────────────────────┐
│ (handler-bind ((condition-type handler-fn))                  │
│   body...)                                                   │
│                                                              │
│   When condition-type is signaled:                          │
│   1. handler-fn is called with condition object             │
│   2. handler-fn can:                                        │
│      - Return normally (decline to handle)                  │
│      - Invoke a restart to resume at error site             │
│      - Signal another condition                             │
│      - Abort entirely                                       │
│                                                              │
│ (restart-bind ((restart-name restart-fn))                   │
│   body...)                                                   │
│                                                              │
│   Establishes named restarts that handlers can invoke       │
└─────────────────────────────────────────────────────────────┘
```

### Key Primitives

| Primitive | Purpose |
|-----------|---------|
| `signal` | Signal a condition (non-fatal by default) |
| `error` | Signal a condition and require handling (fatal if unhandled) |
| `handler-bind` | Establish condition handlers |
| `restart-bind` | Establish named restart points |
| `invoke-restart` | Jump to a restart point with a value |
| `find-restart` | Check if a restart exists |
| `compute-restarts` | List all active restarts |

---

## Implementation Plan

### Task 1: Add Condition Types (2 hours)

**File to Create**: `src/core/conditions/types.ts`

```typescript
// Condition value type
export type ConditionVal = {
  tag: "Condition";
  type: symbol;           // e.g., 'parse-error, 'validation-failed
  message: string;
  data: Val;              // Arbitrary payload
  restarts: RestartPoint[];  // Available restarts at signal site
};

// Restart point - a named continuation
export type RestartPoint = {
  name: symbol;
  description?: string;
  kont: Frame[];          // Continuation to resume
  env: Env;
  store: Store;
  handlers: HandlerFrame[];
};

// Handler binding
export type ConditionHandler = {
  type: symbol | "*";     // "*" = catch all
  handler: ClosureVal;    // (condition) -> void or invokes restart
};

// Restart binding
export type RestartBinding = {
  name: symbol;
  fn: ClosureVal;         // (value) -> result
};
```

### Task 2: Add Condition Frames to CEKS (2 hours)

**File to Modify**: `src/core/eval/machine.ts`

```typescript
// Add new frame types
export type Frame =
  | ... // existing frames
  | { tag: "KHandlerBind"; handlers: ConditionHandler[]; }
  | { tag: "KRestartBind"; restarts: RestartBinding[]; savedKont: Frame[]; }
  | { tag: "KSignaling"; condition: ConditionVal; }
```

### Task 3: Implement Core Primitives (4 hours)

**File to Modify**: `src/core/prims.ts`

```typescript
// signal: Signal a condition, may be handled or may return
// (signal 'condition-type message data)
def("signal", {
  arity: 3,
  fn: (args, state) => {
    const [type, message, data] = args;

    // Create condition with available restarts
    const condition: ConditionVal = {
      tag: "Condition",
      type: asSymbol(type),
      message: asString(message),
      data,
      restarts: collectActiveRestarts(state),
    };

    // Find matching handler by searching handler frames
    const handler = findConditionHandler(condition.type, state.kont);
    if (!handler) {
      // No handler - return normally (condition not handled)
      return stepReturn(state, { tag: "Unit" });
    }

    // Call handler with condition (non-destructively!)
    return stepApply(handler.handler, [condition], state);
  }
});

// error: Signal a condition that MUST be handled
// (error 'condition-type message data)
def("error", {
  arity: 3,
  fn: (args, state) => {
    const [type, message, data] = args;

    const condition: ConditionVal = {
      tag: "Condition",
      type: asSymbol(type),
      message: asString(message),
      data,
      restarts: collectActiveRestarts(state),
    };

    const handler = findConditionHandler(condition.type, state.kont);
    if (!handler) {
      // No handler - this is a fatal error
      throw new UnhandledConditionError(condition);
    }

    return stepApply(handler.handler, [condition], state);
  }
});

// invoke-restart: Jump to a named restart with a value
// (invoke-restart 'restart-name value)
def("invoke-restart", {
  arity: 2,
  fn: (args, state) => {
    const [name, value] = args;

    // Find the restart binding
    const restart = findRestart(asSymbol(name), state.kont);
    if (!restart) {
      throw new Error(`No restart named ${asSymbol(name)}`);
    }

    // CRITICAL: This is a non-local jump!
    // We restore the continuation captured when restart-bind was entered
    // and apply the restart function with the provided value
    return {
      control: { tag: "Apply", fn: restart.fn, args: [value] },
      env: restart.env,
      store: restart.store,
      kont: restart.savedKont,  // Jump back!
      handlers: restart.handlers,
    };
  }
});

// find-restart: Check if a restart exists
// (find-restart 'restart-name) => #t or #f
def("find-restart", {
  arity: 1,
  fn: (args, state) => {
    const name = asSymbol(args[0]);
    const restart = findRestart(name, state.kont);
    return { tag: "Bool", b: restart !== null };
  }
});
```

### Task 4: Implement handler-bind and restart-bind (4 hours)

These need to be **special forms** because they establish dynamic scope.

**File to Modify**: `src/core/pipeline/compileText.ts`

Add `handler-bind` and `restart-bind` to special forms:

```typescript
// (handler-bind ((type handler) ...) body ...)
// Compiles to:
// 1. Evaluate handlers
// 2. Push KHandlerBind frame
// 3. Evaluate body
// 4. Pop frame, return body result

// (restart-bind ((name fn) ...) body ...)
// Compiles to:
// 1. Evaluate restart functions
// 2. Push KRestartBind frame with captured continuation
// 3. Evaluate body
// 4. Pop frame, return body result
```

**File to Modify**: `src/core/eval/machineStep.ts`

Add handling for new frames:

```typescript
case "KHandlerBind": {
  // When body returns, pop the handler frame
  return stepContinue(state, value, frame.next);
}

case "KRestartBind": {
  // When body returns, pop the restart frame
  return stepContinue(state, value, frame.next);
}
```

---

## Verification

### Test 1: Basic Signal and Handle

```lisp
(handler-bind ((parse-error
                (lambda (c)
                  (display "Caught parse error")
                  (invoke-restart 'use-default 42))))
  (restart-bind ((use-default (lambda (v) v)))
    (signal 'parse-error "bad input" '())
    (display "This runs after restart!")))
;; Output: "Caught parse error" then "This runs after restart!"
;; Returns: 42
```

### Test 2: Multiple Restarts

```lisp
(define (parse-int str)
  (restart-bind
    ((use-value (lambda (v) v))
     (retry-with (lambda (new-str) (parse-int new-str))))
    (if (valid-int? str)
        (string->number str)
        (error 'parse-error "Not an integer" str))))

(handler-bind ((parse-error
                (lambda (c)
                  (invoke-restart 'use-value 0))))  ; Default to 0
  (parse-int "abc"))  ; => 0
```

### Test 3: Handler Decline (Return Normally)

```lisp
(handler-bind ((warning (lambda (c)
                          (display "Warning noted")
                          ;; Return normally = decline to handle
                          )))
  (signal 'warning "Something odd" '())
  (display "Continuing..."))
;; Output: "Warning noted" then "Continuing..."
```

---

## Test Plan

### Signal/Handler Tests (`test/conditions/signal.spec.ts`)

#### Happy Path Tests

```lisp
;; HP-1: Handler catches and invokes restart
(handler-bind ((parse-error
                (lambda (c) (invoke-restart 'use-default 0))))
  (restart-bind ((use-default (lambda (v) v)))
    (signal 'parse-error "bad input" '())))
;; => 0

;; HP-2: Handler runs, body continues (non-unwinding)
(define log '())
(handler-bind ((warning (lambda (c) (set! log (cons 'handled log)))))
  (signal 'warning "minor issue" '())
  (set! log (cons 'continued log)))
log  ;; => '(continued handled)

;; HP-3: Multiple handlers, first match wins
(handler-bind ((type-a (lambda (c) 'a-handler))
               (type-b (lambda (c) 'b-handler)))
  (restart-bind ((return-val (lambda (v) v)))
    (invoke-restart 'return-val
      (signal 'type-b "test" '()))))
;; => 'b-handler

;; HP-4: Nested handler-bind, inner shadows outer
(handler-bind ((error (lambda (c) 'outer)))
  (handler-bind ((error (lambda (c) 'inner)))
    (restart-bind ((r (lambda (v) v)))
      (invoke-restart 'r (signal 'error "test" '())))))
;; => 'inner
```

#### Edge Case Tests

```lisp
;; EC-1: Signal with no matching handler - unhandled (for signal, not error)
(signal 'unknown-type "no handler" '())
;; => continues normally (signal is ignorable)

;; EC-2: Handler declines (returns normally without invoking restart)
(handler-bind ((warning (lambda (c) 'declined)))
  (signal 'warning "test" '())
  'body-result)
;; => 'body-result (handler ran but body continued)

;; EC-3: Restart not found
(handler-bind ((e (lambda (c) (invoke-restart 'nonexistent 0))))
  (restart-bind ((other (lambda (v) v)))
    (signal 'e "test" '())))
;; => Error: Unknown restart 'nonexistent

;; EC-4: Handler itself signals a condition
(handler-bind ((inner (lambda (c) 'inner-handled)))
  (handler-bind ((outer (lambda (c)
                          (signal 'inner "from handler" '())
                          'outer-handled)))
    (restart-bind ((r (lambda (v) v)))
      (invoke-restart 'r (signal 'outer "test" '())))))
;; => 'inner-handled (nested signaling works)

;; EC-5: Restart with multiple arguments
(handler-bind ((e (lambda (c) (invoke-restart 'multi 1 2 3))))
  (restart-bind ((multi (lambda (a b c) (+ a b c))))
    (signal 'e "test" '())))
;; => 6
```

### Error Tests (`test/conditions/error.spec.ts`)

#### Happy Path Tests

```lisp
;; HP-1: error with handler
(handler-bind ((fatal (lambda (c) (invoke-restart 'recover 'saved))))
  (restart-bind ((recover (lambda (v) v)))
    (error 'fatal "Something broke" '())))
;; => 'saved

;; HP-2: error requires handling - handler must invoke restart or will propagate
```

#### Error Cases

```lisp
;; ERR-1: error with no handler - fatal
(error 'unhandled "no handler" '())
;; => UnhandledConditionError

;; ERR-2: error with handler that doesn't invoke restart - fatal
(handler-bind ((e (lambda (c) 'did-nothing)))
  (error 'e "test" '()))
;; => UnhandledConditionError (handler didn't resolve it)
```

### Restart Tests (`test/conditions/restart.spec.ts`)

#### Happy Path Tests

```lisp
;; HP-1: find-restart returns #t when restart exists
(restart-bind ((my-restart (lambda (v) v)))
  (find-restart 'my-restart))
;; => #t

;; HP-2: compute-restarts returns list
(restart-bind ((r1 (lambda () 1))
               (r2 (lambda () 2)))
  (length (compute-restarts)))
;; => 2

;; HP-3: Nested restart-bind accumulates restarts
(restart-bind ((outer (lambda () 'outer)))
  (restart-bind ((inner (lambda () 'inner)))
    (length (compute-restarts))))
;; => 2
```

#### Edge Cases

```lisp
;; EC-1: find-restart returns #f when not found
(find-restart 'nonexistent)
;; => #f

;; EC-2: invoke-restart in nested context - finds correct restart
(restart-bind ((name (lambda () 'outer)))
  (restart-bind ((name (lambda () 'inner)))
    (invoke-restart 'name)))
;; => 'inner (most recent)

;; EC-3: Restart invoked outside its dynamic extent
(define saved-restart #f)
(restart-bind ((r (lambda () (set! saved-restart #t) 'ok)))
  (set! saved-restart (lambda () (invoke-restart 'r))))
(saved-restart)
;; => Error: Restart 'r not available (out of scope)
```

### Integration Tests

1. **Parse-and-repair loop** - Parser signals errors, handler auto-fixes
2. **Validation cascade** - Multiple validators, each can offer restarts
3. **LLM with retry** - Oracle call fails, handler retries with modified prompt
4. **Multi-layer handling** - Application > Library > Core condition layers

---

## Checklist

### Task 1: Condition Types
- [ ] Create `src/core/conditions/types.ts`
- [ ] Define `ConditionVal` type
- [ ] Define `RestartPoint` type
- [ ] Define `ConditionHandler` type
- [ ] Define `RestartBinding` type

### Task 2: CEKS Frames
- [ ] Add `KHandlerBind` frame to machine.ts
- [ ] Add `KRestartBind` frame to machine.ts
- [ ] Add `KSignaling` frame to machine.ts

### Task 3: Core Primitives
- [ ] Implement `signal` primitive
- [ ] Implement `error` primitive
- [ ] Implement `invoke-restart` primitive
- [ ] Implement `find-restart` primitive
- [ ] Implement `compute-restarts` primitive
- [ ] Add all to compileText.ts primitives list

### Task 4: Special Forms
- [ ] Add `handler-bind` special form compilation
- [ ] Add `restart-bind` special form compilation
- [ ] Add frame handling to machineStep.ts

### Verification
- [ ] All existing tests still pass (1124+)
- [ ] New condition tests pass
- [ ] REPL examples work
- [ ] Test interaction with effect handlers

---

## Notes

### Why Not Just Use Effect Handlers?

Effect handlers and conditions serve different purposes:

| Effect Handlers | Condition System |
|-----------------|------------------|
| Structured effects (amb, async) | Error recovery |
| Always resume or abort | May or may not resume |
| Implicit in effect operations | Explicit signal/restart |
| Typically one handler per effect | Multiple handlers can try |

They can coexist and complement each other.

### Interaction with call/cc

Conditions build on call/cc:
- `restart-bind` captures a continuation
- `invoke-restart` jumps to that continuation

Without call/cc from Job 004, we can't implement this.

### Future Enhancement: Debugger Integration

Once conditions work, we can build:
- `(break)` - signal a breakpoint condition
- A debugger handler that lets user inspect and choose restarts
- Integration with `machine-step` for step-debugging

---

---

## Proof of Completion

When marking this job DONE:

1. **Prerequisite check**: `npx vitest run test/continuations/` - Job 004 tests pass
2. **Build passes**: `npm run build` - no TypeScript errors
3. **Baseline tests pass**: `npm run test` - 1124+ tests still green
4. **New tests pass**:
   - `npx vitest run test/conditions/signal.spec.ts`
   - `npx vitest run test/conditions/restart.spec.ts`
   - `npx vitest run test/conditions/error.spec.ts`
5. **REPL verification**: Test examples from Verification section manually
6. **Update status**: Change `NOT STARTED` → `DONE` in this file
7. **Update README**: Mark Job 005 as DONE in [README.md](README.md)
8. **Unblock Job 008**: Notify that `@omega/conditions` is ready

---

*Created: 2026-01-19*
*Related: [004-IMPLEMENT-CORE-MAGIC.md](./004-IMPLEMENT-CORE-MAGIC.md)*
*Depends on: Job 004 (call/cc must be done first)*
