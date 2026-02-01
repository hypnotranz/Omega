# 500: Conditions Manager

## Status: COMPLETE ✅
- **Completed**: 2026-01-29
- **Location**: OmegaLLM-3/src/runtime/subsystems/ConditionsManager.ts (446 lines)

## Purpose
Implements Common Lisp-style condition system for sophisticated error handling with restarts and handlers.

## Dependencies
- 100-types.md ✅
- 110-events.md ✅

## Source References
- ARCHITECTURE/06-CONDITIONS.md
- Common Lisp Condition System (CLHS)
- Beyond Exception Handling (Kent Pitman)

---

## Deliverables

```
src/runtime/subsystems/
├── ConditionsManager.ts     # Main conditions manager
└── conditions/
    ├── Condition.ts         # Condition types
    ├── Restart.ts           # Restart types
    ├── Handler.ts           # Handler binding
    └── ConditionStack.ts    # Dynamic handler stack
```

---

## Key Types

```typescript
export interface Condition {
  type: symbol;              // Condition type (symbol for hierarchy)
  message: string;
  data?: Record<string, Val>;
  cause?: Condition;         // Chain of conditions
}

export interface Restart {
  name: symbol;
  description: string;
  interactive?: () => Val[]; // Prompt user for args
  test?: (condition: Condition) => boolean;
  action: (...args: Val[]) => Val;
}

export interface Handler {
  conditionType: symbol;     // What condition to handle
  handler: (condition: Condition) => Val | undefined;
  // If handler returns undefined, decline and try next handler
}

export interface RestartCase {
  condition: Condition;
  availableRestarts: Restart[];
}
```

---

## Key Interface

```typescript
export interface ConditionsManager {
  // ─── Signaling ───

  /**
   * Signal a condition.
   * If unhandled, becomes an error.
   */
  signal(condition: Condition): Val;

  /**
   * Signal an error (must be handled).
   */
  error(condition: Condition): never;

  /**
   * Signal a warning (can be ignored).
   */
  warn(condition: Condition): void;

  // ─── Handling ───

  /**
   * Establish condition handlers for body.
   */
  handlerBind<T>(
    handlers: Handler[],
    body: () => T
  ): T;

  /**
   * Handle condition and replace with value.
   */
  handlerCase<T>(
    body: () => T,
    cases: { type: symbol; handler: (c: Condition) => T }[]
  ): T;

  // ─── Restarts ───

  /**
   * Establish restarts for body.
   */
  restartBind<T>(
    restarts: Restart[],
    body: () => T
  ): T;

  /**
   * Find restart by name.
   */
  findRestart(name: symbol): Restart | undefined;

  /**
   * Get all active restarts.
   */
  computeRestarts(condition?: Condition): Restart[];

  /**
   * Invoke a restart.
   */
  invokeRestart(name: symbol, ...args: Val[]): Val;

  /**
   * Invoke restart interactively.
   */
  invokeRestartInteractively(name: symbol): Val;

  // ─── Built-in Restarts ───

  /**
   * Use a replacement value.
   */
  useValue(value: Val): Val;

  /**
   * Store and use a replacement value.
   */
  storeValue(value: Val): Val;

  /**
   * Abort operation.
   */
  abort(): never;

  /**
   * Continue without action.
   */
  continue(): void;

  /**
   * Retry operation.
   */
  retry(): Val;
}
```

---

## Condition Hierarchy

```typescript
// Base condition types (symbols for inheritance checking)
const SimpleCondition = Symbol('simple-condition');
const SeriousCondition = Symbol('serious-condition');
const Error = Symbol('error');
const Warning = Symbol('warning');

// Built-in conditions
const TypeError = Symbol('type-error');
const ArithmeticError = Symbol('arithmetic-error');
const DivisionByZero = Symbol('division-by-zero');
const UnboundVariable = Symbol('unbound-variable');
const FileError = Symbol('file-error');
const LLMError = Symbol('llm-error');
const BudgetExceeded = Symbol('budget-exceeded');

// Hierarchy (child -> parent)
const conditionHierarchy = new Map<symbol, symbol>([
  [TypeError, Error],
  [ArithmeticError, Error],
  [DivisionByZero, ArithmeticError],
  [UnboundVariable, Error],
  [FileError, Error],
  [LLMError, Error],
  [BudgetExceeded, SeriousCondition],
  [Error, SeriousCondition],
  [Warning, SimpleCondition],
  [SeriousCondition, SimpleCondition],
]);

function isSubtypeOf(type: symbol, supertype: symbol): boolean {
  if (type === supertype) return true;
  const parent = conditionHierarchy.get(type);
  if (!parent) return false;
  return isSubtypeOf(parent, supertype);
}
```

---

## Handler Stack Implementation

```typescript
class ConditionsManagerImpl implements ConditionsManager {
  private handlerStack: Handler[][] = [];
  private restartStack: Restart[][] = [];

  signal(condition: Condition): Val {
    // Try each handler in stack order (innermost first)
    for (let i = this.handlerStack.length - 1; i >= 0; i--) {
      for (const handler of this.handlerStack[i]) {
        if (isSubtypeOf(condition.type, handler.conditionType)) {
          const result = handler.handler(condition);
          if (result !== undefined) {
            return result;
          }
          // Handler declined, try next
        }
      }
    }

    // No handler found - if error type, throw
    if (isSubtypeOf(condition.type, Error)) {
      throw new ConditionError(condition, this.computeRestarts(condition));
    }

    // Non-error conditions return nil if unhandled
    return null;
  }

  handlerBind<T>(handlers: Handler[], body: () => T): T {
    this.handlerStack.push(handlers);
    try {
      return body();
    } finally {
      this.handlerStack.pop();
    }
  }
}
```

---

## Lisp Interface

```lisp
;; Signal and handle
(handler-case
  (progn
    (when (= x 0)
      (error 'division-by-zero :dividend y :divisor x))
    (/ y x))
  (division-by-zero (c)
    (println "Caught division by zero!")
    0))

;; Restarts
(restart-case
  (let ((result (parse-number input)))
    (if (not result)
        (error 'parse-error :input input)
        result))
  (use-value (v)
    :description "Use a specific value"
    :interactive (lambda () (list (read "Enter value: ")))
    v)
  (retry ()
    :description "Try parsing again"
    (parse-number (read "Enter new input: "))))

;; Invoke restart from handler
(handler-bind
  ((parse-error
    (lambda (c)
      (invoke-restart 'use-value 0))))
  (risky-parse input))
```

---

## Test Requirements

### Unit Tests: `tests/runtime/subsystems/ConditionsManager.test.ts`
- [ ] signal() invokes matching handler
- [ ] signal() declines to next handler if undefined
- [ ] error() throws if unhandled
- [ ] warn() continues if unhandled
- [ ] handlerBind() establishes handler scope
- [ ] handlerCase() catches and returns value
- [ ] restartBind() establishes restart scope
- [ ] findRestart() finds by name
- [ ] computeRestarts() returns all active
- [ ] invokeRestart() calls restart action
- [ ] Condition hierarchy matching works
- [ ] Nested handlers work correctly

### Integration Tests
- [ ] Division by zero with restart
- [ ] LLM error with retry restart
- [ ] Budget exceeded with abort restart
- [ ] Interactive restart prompts user
- [ ] Conditions integrate with debugger

---

## Acceptance Criteria
1. Conditions can be signaled and handled
2. Restarts allow recovery without stack unwinding
3. Condition hierarchy enables generic handlers
4. Handlers can decline to next handler
5. Interactive restarts prompt for input
6. Debugger shows available restarts
