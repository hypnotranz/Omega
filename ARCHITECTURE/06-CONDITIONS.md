# 06: Conditions (Non-Unwinding Error Handling)

## The Problem with Exceptions

```
┌─────────────────────────────────────────────────────────────────┐
│  Traditional Exceptions (throw/catch)                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  try {                                                          │
│    readFile("config.json")  // throws if not found             │
│  } catch (e) {                                                  │
│    // Stack is UNWOUND - context is LOST                       │
│    // Can't inspect what readFile was doing                    │
│    // Can't say "try a different file"                         │
│    // Can only give up or retry from scratch                   │
│  }                                                              │
│                                                                 │
│  Problem: The error handler has NO CONTEXT.                    │
│  The stack unwound before we could decide what to do.          │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

## The Solution: Conditions & Restarts

```
┌─────────────────────────────────────────────────────────────────┐
│  Condition System (signal/handler/restart)                      │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  1. Error occurs → SIGNAL condition (don't unwind!)            │
│  2. Handler sees condition WITH full stack context             │
│  3. Handler can:                                                │
│     - INVOKE a restart (fix the problem, continue)             │
│     - DECLINE (let next handler try)                           │
│     - TRANSFER (like throw, unwinds)                           │
│  4. If restart invoked, execution continues normally           │
│                                                                 │
│  Key insight: Handler runs with stack INTACT.                  │
│  Can inspect, modify, and RESUME.                              │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

## Example in Lisp

```lisp
;; Define function that signals condition with restarts
(define (read-config path)
  (restart-case
    (if (file-exists? path)
        (world.read path)
        (signal 'file-not-found :path path))
    ;; Available restarts:
    (use-value (value)
      "Return a specific value instead"
      value)
    (try-another (new-path)
      "Try reading a different file"
      (read-config new-path))
    (create-default ()
      "Create default config and return it"
      (let ((default "{}"))
        (world.write path default)
        default))))

;; Handler that chooses what to do
(handler-bind
  ((file-not-found
    (lambda (c)
      ;; Can inspect condition
      (print (str "File not found: " (condition-path c)))
      ;; Can choose a restart
      (invoke-restart 'create-default))))
  (read-config "config.json"))
```

## TypeScript Implementation

### Condition Types

```typescript
// condition.ts

interface Condition {
  type: symbol;           // Condition type (e.g., 'file-not-found)
  message: string;        // Human-readable description
  data: Record<string, Value>;  // Condition-specific data
  restarts: Restart[];    // Available recovery options
}

interface Restart {
  name: symbol;           // Restart identifier
  description: string;    // What this restart does
  handler: (...args: Value[]) => Value;  // How to execute it
}

function makeCondition(
  type: string,
  message: string,
  data: Record<string, Value> = {},
  restarts: Restart[] = []
): Condition {
  return {
    type: Symbol.for(type),
    message,
    data,
    restarts
  };
}
```

### Signaling Conditions

```typescript
// Signal a condition (does NOT unwind stack)
function signalCondition(
  condition: Condition,
  cont: Continuation,
  ffi: FFI
): Value {
  // Walk up handler stack
  for (const handler of cont.getHandlers()) {
    if (handler.handles(condition.type)) {
      // Call handler with condition
      // Handler runs with CURRENT continuation (stack intact!)
      const result = handler.fn(condition);

      // Handler returned - it declined to handle
      if (result === DECLINE) continue;

      // Handler wants to transfer control
      if (result instanceof Transfer) {
        return applyTransfer(result, cont);
      }

      // Handler wants to invoke a restart
      if (result instanceof InvokeRestart) {
        return invokeRestart(result.name, result.args, condition, cont, ffi);
      }
    }
  }

  // No handler handled it - signal unhandled condition
  throw new UnhandledConditionError(condition);
}

// Invoke a restart
function invokeRestart(
  name: symbol,
  args: Value[],
  condition: Condition,
  cont: Continuation,
  ffi: FFI
): Value {
  // Find restart in condition
  const restart = condition.restarts.find(r => r.name === name);
  if (!restart) {
    throw new Error(`Unknown restart: ${Symbol.keyFor(name)}`);
  }

  // Execute restart handler
  const result = restart.handler(...args);

  // Continue from where signal was called
  return applyCont(cont, result);
}
```

### Handler Binding

```typescript
// Install handlers for a scope
function withHandlers(
  handlers: Map<symbol, HandlerFn>,
  body: () => Value,
  cont: Continuation
): Value {
  // Push handlers onto continuation's handler stack
  const withHandlers = cont.pushHandlers(handlers);

  // Evaluate body
  return body();  // Uses withHandlers continuation
}

// In Lisp:
// (handler-bind
//   ((file-not-found (lambda (c) ...))
//    (network-error (lambda (c) ...)))
//   body...)
```

### Restart Case Macro

```lisp
;; restart-case is a macro that expands to:
(restart-case
  body
  (restart-name (args...) "description" handler-body))

;; Expands to:
(let ((restarts (list
        (make-restart 'restart-name "description"
          (lambda (args...) handler-body)))))
  (with-restarts restarts
    body))
```

## Real-World Use Cases

### 1. File Not Found

```lisp
;; Low-level: signal with restarts
(define (safe-read path)
  (restart-case
    (world.read path)
    (use-default (default)
      "Return default value"
      default)
    (skip ()
      "Skip this file"
      nil)))

;; High-level: choose how to handle
(handler-bind
  ((file-not-found
    (lambda (c)
      (invoke-restart 'use-default "{}"))))
  (safe-read "config.json"))
```

### 2. LLM Response Parsing

```lisp
;; LLM returns unparseable response
(define (llm-eval prompt)
  (restart-case
    (let ((response (llm.complete prompt)))
      (parse-sexp response))
    (retry ()
      "Ask LLM to try again"
      (llm-eval (str prompt "\n\nPlease format as S-expression.")))
    (use-fallback (value)
      "Use a fallback value"
      value)))

;; Handler decides
(handler-bind
  ((parse-error
    (lambda (c)
      ;; First try, retry. Second try, fallback.
      (if (< (condition-attempts c) 2)
          (invoke-restart 'retry)
          (invoke-restart 'use-fallback '(error "parse failed"))))))
  (llm-eval "What is 2+2?"))
```

### 3. Debugging / Time-Travel

```lisp
;; When entering debugger, signal condition with restarts
(define (debug-break message)
  (restart-case
    (signal 'debug-break :message message)
    (continue ()
      "Continue execution"
      nil)
    (abort ()
      "Abort current evaluation"
      (signal 'abort))
    (eval-and-continue (expr)
      "Evaluate expression and continue"
      (eval expr))))

;; Debugger UI handles:
(handler-bind
  ((debug-break
    (lambda (c)
      ;; Show REPL, let user interact
      (let ((choice (debugger-repl c)))
        (invoke-restart choice)))))
  (my-computation))
```

## Integration with Time-Travel

The condition system enables powerful debugging:

```typescript
// When condition is signaled, we can:
// 1. Snapshot the current state (continuation + env + world)
// 2. Let user inspect/modify
// 3. Choose a restart or provide custom value
// 4. Resume execution

function handleConditionWithDebugger(
  condition: Condition,
  cont: Continuation,
  env: Environment,
  world: World
): Value {
  // Capture snapshot
  const snapshot = captureSnapshot(cont, env, world);
  snapshot.condition = condition;

  // Store for debugger access
  saveSnapshot(snapshot);

  // Let debugger (UI/CLI) handle
  const choice = await waitForDebuggerChoice(snapshot.id);

  if (choice.type === 'invoke-restart') {
    return invokeRestart(choice.restart, choice.args, condition, cont);
  }
  if (choice.type === 'use-value') {
    return applyCont(cont, choice.value);
  }
  if (choice.type === 'abort') {
    throw new AbortError();
  }
}
```

## Comparison

| Feature | Exceptions | Conditions |
|---------|------------|------------|
| Stack on error | Unwound (lost) | Intact (inspectable) |
| Recovery options | None (must retry) | Restarts |
| Handler context | Error only | Full stack + error |
| Intervention | Not possible | Natural |
| Time-travel | Must rebuild | Snapshot + resume |

## Summary

The condition system gives us:

1. **Non-unwinding errors**: Stack remains intact
2. **Restarts**: Multiple recovery options defined at error site
3. **Handler context**: Handler sees full state, can inspect
4. **Debugger hook**: Natural place to intervene
5. **Time-travel**: Snapshot state when condition occurs

This is how Common Lisp and Racket enable their powerful debuggers - the condition system IS the debugger interface.
