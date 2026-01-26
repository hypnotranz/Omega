# ⛔ OBSOLETE - REPLACED BY CEKS MACHINE

> **THIS ENTIRE SPEC IS SUPERSEDED BY 32-6 (CEKS MACHINE)**
>
> ## Why This Is Obsolete
>
> This spec describes a **recursive evaluator** (~200 lines of `evalExpr` calling itself).
> The 32-series introduced a **CEKS machine** (Control, Environment, Kontinuation, Store)
> which is a fundamentally different architecture:
>
> ```typescript
> // THIS SPEC (RECURSIVE EVAL):
> function evalExpr(expr: Value, env: Env, cont: Cont, ffi: FFI): Value {
>   if (isSymbol(expr)) return envLookup(env, expr);
>   if (isAtom(expr)) return expr;
>   // ... recursively calls evalExpr
> }
>
> // 32-6 (CEKS MACHINE):
> function step(s: State): StepOutcome {
>   // Explicit state machine - no recursion
>   // State contains: control, env, store, kont, handlers
>   // Plus: budget, sec, provenanceGraph (governance fields)
> }
> ```
>
> ## Critical Difference
>
> - **Recursive eval**: Governance (budget, security, provenance) must be passed as
>   parameters or wrapped externally - CAN BE BYPASSED
> - **CEKS machine**: Governance is IN the State, checked on EVERY step - CANNOT BE BYPASSED
>
> ## What To Use Instead
>
> - See **32-6** for CEKS machine specification
> - See **32-1** for kernel forms (what replaces special forms)
> - The ~200 line recursive eval is replaced by `step()` function + explicit State
>
> ## References
> - [32-6 CEKS Machine](32-6-CEKS.md)
> - [32-1 Foundations](32-1-FOUNDATIONS.md)
> - [ARCHITECTURE-REDESIGN-ASSESSMENT.md](../docs/ARCHITECTURE-REDESIGN-ASSESSMENT.md)

---

# 04: Evaluator (Pure Interpreter)

## Design Goal

The evaluator should be **~200 lines of pure code**:
- No I/O
- No LLM calls
- No file system
- Just special forms + function application

Everything else goes through FFI.

## Special Forms

These are the only things the evaluator "knows about":

| Form | Purpose |
|------|---------|
| `quote` | Return unevaluated |
| `if` | Conditional |
| `lambda` | Create closure |
| `define` | Bind in current scope |
| `set!` | Mutate binding |
| `begin` | Sequence |
| `let` / `let*` / `letrec` | Local bindings |
| `call/cc` | Capture continuation |
| `signal` | Signal condition |
| `handler-bind` | Establish handlers |

That's it. Everything else (including `+`, `map`, `world.read`) is either:
- A primitive in the base environment
- An FFI call

## TypeScript Implementation

```typescript
// eval.ts - THE ENTIRE EVALUATOR (~200 lines)

import { Value, List, Closure, isSymbol, isList, isAtom } from './types';
import { Environment } from './env';
import { Continuation, Frame, applyCont, pushFrame } from './continuation';
import { FFI } from './ffi';

// Check for special forms
const SPECIAL_FORMS = new Set([
  'quote', 'if', 'lambda', 'define', 'set!',
  'begin', 'let', 'let*', 'letrec',
  'call/cc', 'signal', 'handler-bind'
]);

function isSpecialForm(expr: Value): boolean {
  return isList(expr) && expr.length > 0 &&
         isSymbol(expr[0]) && SPECIAL_FORMS.has(symbolName(expr[0]));
}

// The evaluator
export function evalExpr(
  expr: Value,
  env: Environment,
  cont: Continuation,
  ffi: FFI
): Value {
  // Self-evaluating atoms
  if (isAtom(expr)) {
    return applyCont(cont, expr);
  }

  // Variable lookup
  if (isSymbol(expr)) {
    const value = env.lookup(symbolName(expr));
    return applyCont(cont, value);
  }

  // Must be a list (special form or application)
  if (!isList(expr) || expr.length === 0) {
    throw new EvalError(`Invalid expression: ${print(expr)}`);
  }

  const head = expr[0];

  // ========== SPECIAL FORMS ==========

  // (quote <datum>)
  if (isSymbol(head) && symbolName(head) === 'quote') {
    return applyCont(cont, expr[1]);
  }

  // (if <test> <then> <else?>)
  if (isSymbol(head) && symbolName(head) === 'if') {
    const [_, test, thenBranch, elseBranch] = expr;

    // Create continuation for after test
    const afterTest = pushFrame(cont, {
      type: 'if-test',
      expr,
      data: { thenBranch, elseBranch, env },
      handler: (testResult, c) => {
        const branch = truthy(testResult) ? thenBranch : (elseBranch ?? null);
        if (branch === null) return applyCont(c, null);
        return evalExpr(branch, env, c, ffi);
      }
    });

    return evalExpr(test, env, afterTest, ffi);
  }

  // (lambda <params> <body>)
  if (isSymbol(head) && symbolName(head) === 'lambda') {
    const params = (expr[1] as List).map(p => symbolName(p as symbol));
    const body = expr.length === 3 ? expr[2] : [sym('begin'), ...expr.slice(2)];

    const closure: Closure = {
      type: 'closure',
      params,
      body,
      envSnapshot: env.snapshot(),
      source: print(expr),
    };

    return applyCont(cont, closure);
  }

  // (define <name> <value>)
  if (isSymbol(head) && symbolName(head) === 'define') {
    const name = symbolName(expr[1] as symbol);
    const valueExpr = expr[2];

    const afterValue = pushFrame(cont, {
      type: 'define',
      expr,
      data: { name, env },
      handler: (value, c) => {
        env.define(name, value);
        return applyCont(c, value);
      }
    });

    return evalExpr(valueExpr, env, afterValue, ffi);
  }

  // (set! <name> <value>)
  if (isSymbol(head) && symbolName(head) === 'set!') {
    const name = symbolName(expr[1] as symbol);
    const valueExpr = expr[2];

    const afterValue = pushFrame(cont, {
      type: 'set!',
      expr,
      data: { name, env },
      handler: (value, c) => {
        env.set(name, value);
        return applyCont(c, value);
      }
    });

    return evalExpr(valueExpr, env, afterValue, ffi);
  }

  // (begin <expr>...)
  if (isSymbol(head) && symbolName(head) === 'begin') {
    return evalSequence(expr.slice(1), env, cont, ffi);
  }

  // (let ((<name> <value>)...) <body>...)
  if (isSymbol(head) && symbolName(head) === 'let') {
    const bindings = expr[1] as List;
    const body = expr.slice(2);

    // Create new environment
    const letEnv = env.extend();

    // Evaluate bindings sequentially
    return evalLetBindings(bindings, 0, letEnv, body, cont, ffi);
  }

  // (call/cc <proc>) - Capture current continuation
  if (isSymbol(head) && symbolName(head) === 'call/cc') {
    const procExpr = expr[1];

    const afterProc = pushFrame(cont, {
      type: 'call/cc',
      expr,
      handler: (proc, c) => {
        // Capture continuation as a procedure
        const captured: ContinuationProc = {
          type: 'continuation',
          frames: c.frames.slice(),
          envSnapshot: env.snapshot(),
        };

        // Call proc with captured continuation
        return apply(proc, [captured], c, ffi);
      }
    });

    return evalExpr(procExpr, env, afterProc, ffi);
  }

  // (signal <condition>) - Signal a condition
  if (isSymbol(head) && symbolName(head) === 'signal') {
    const condExpr = expr[1];

    const afterCond = pushFrame(cont, {
      type: 'signal',
      expr,
      handler: (condition, c) => {
        return signalCondition(condition, c, ffi);
      }
    });

    return evalExpr(condExpr, env, afterCond, ffi);
  }

  // (handler-bind <handlers> <body>)
  if (isSymbol(head) && symbolName(head) === 'handler-bind') {
    const handlersExpr = expr[1] as List;
    const body = expr.slice(2);

    // Parse handlers: ((type handler-fn) ...)
    const handlers: Map<string, Value> = new Map();
    for (const h of handlersExpr) {
      const [type, fn] = h as List;
      handlers.set(symbolName(type as symbol), fn);
    }

    // Install handlers and evaluate body
    const withHandlers = cont.installHandlers(handlers);
    return evalSequence(body, env, withHandlers, ffi);
  }

  // ========== FUNCTION APPLICATION ==========

  // Evaluate operator
  const afterOp = pushFrame(cont, {
    type: 'apply-op',
    expr,
    data: { args: expr.slice(1), env },
    handler: (op, c) => {
      // Evaluate arguments
      return evalArgs(expr.slice(1), [], op, env, c, ffi);
    }
  });

  return evalExpr(head, env, afterOp, ffi);
}

// Evaluate argument list
function evalArgs(
  remaining: List,
  evaluated: Value[],
  op: Value,
  env: Environment,
  cont: Continuation,
  ffi: FFI
): Value {
  if (remaining.length === 0) {
    return apply(op, evaluated, cont, ffi);
  }

  const [first, ...rest] = remaining;

  const afterArg = pushFrame(cont, {
    type: 'apply-arg',
    data: { remaining: rest, evaluated, op, env },
    handler: (arg, c) => {
      return evalArgs(rest, [...evaluated, arg], op, env, c, ffi);
    }
  });

  return evalExpr(first, env, afterArg, ffi);
}

// Apply a procedure
function apply(
  proc: Value,
  args: Value[],
  cont: Continuation,
  ffi: FFI
): Value {
  // Continuation invocation (abandons current continuation!)
  if (isContinuation(proc)) {
    if (args.length !== 1) {
      throw new EvalError('Continuation requires exactly 1 argument');
    }
    // Jump to captured continuation with value
    return applyCont(proc.toContinuation(), args[0]);
  }

  // Primitive (TypeScript function)
  if (isPrimitive(proc)) {
    const result = proc.fn(...args);
    return applyCont(cont, result);
  }

  // Closure (user-defined)
  if (isClosure(proc)) {
    // Restore captured environment
    const closureEnv = Environment.fromSnapshot(proc.envSnapshot);

    // Extend with parameters
    const extendedEnv = closureEnv.extend(proc.params, args);

    // Evaluate body
    return evalExpr(proc.body, extendedEnv, cont, ffi);
  }

  // FFI call (external function)
  if (isFFI(proc)) {
    const result = ffi.call(proc.name, args);
    return applyCont(cont, result);
  }

  throw new EvalError(`Not a procedure: ${print(proc)}`);
}

// Evaluate sequence of expressions
function evalSequence(
  exprs: List,
  env: Environment,
  cont: Continuation,
  ffi: FFI
): Value {
  if (exprs.length === 0) {
    return applyCont(cont, null);
  }

  if (exprs.length === 1) {
    return evalExpr(exprs[0], env, cont, ffi);
  }

  const [first, ...rest] = exprs;

  const afterFirst = pushFrame(cont, {
    type: 'begin-seq',
    data: { remaining: rest, env },
    handler: (_, c) => {
      return evalSequence(rest, env, c, ffi);
    }
  });

  return evalExpr(first, env, afterFirst, ffi);
}

// Helper
function truthy(value: Value): boolean {
  return value !== false && value !== null;
}
```

## What's NOT in the Evaluator

Everything else is:
1. **Primitives** in base environment (`+`, `-`, `*`, `car`, `cdr`, etc.)
2. **FFI calls** (`world.read`, `llm.complete`, etc.)
3. **Macros** expanded before evaluation

### Primitives (in stdlib/base.ts)

```typescript
// stdlib/base.ts
export const basePrimitives: Map<string, Primitive> = new Map([
  ['+', { type: 'primitive', name: '+', fn: (...args) => args.reduce((a, b) => a + b, 0) }],
  ['-', { type: 'primitive', name: '-', fn: (a, b) => a - b }],
  ['*', { type: 'primitive', name: '*', fn: (...args) => args.reduce((a, b) => a * b, 1) }],
  ['/', { type: 'primitive', name: '/', fn: (a, b) => a / b }],
  ['=', { type: 'primitive', name: '=', fn: (a, b) => a === b }],
  ['<', { type: 'primitive', name: '<', fn: (a, b) => a < b }],
  ['>', { type: 'primitive', name: '>', fn: (a, b) => a > b }],
  ['car', { type: 'primitive', name: 'car', fn: (l) => l[0] }],
  ['cdr', { type: 'primitive', name: 'cdr', fn: (l) => l.slice(1) }],
  ['cons', { type: 'primitive', name: 'cons', fn: (h, t) => [h, ...t] }],
  ['list', { type: 'primitive', name: 'list', fn: (...args) => args }],
  ['null?', { type: 'primitive', name: 'null?', fn: (l) => l === null || (Array.isArray(l) && l.length === 0) }],
  // ... more
]);
```

### FFI Calls (registered externally)

```typescript
// runtime/setup.ts
export function setupFFI(ffi: FFI, world: World, llm: LLMAdapter) {
  // World operations
  ffi.register('world.read', (path: string) => world.read(path));
  ffi.register('world.write', (path: string, content: string) => world.write(path, content));
  ffi.register('world.list', (pattern: string) => world.list(pattern));

  // LLM operations
  ffi.register('llm.complete', (prompt: string, system?: string) => llm.complete(prompt, system));

  // Time
  ffi.register('now', () => Date.now());
}
```

## Why This Design?

| Benefit | How |
|---------|-----|
| **Testable** | No I/O in evaluator, mock FFI |
| **Portable** | Same eval.ts runs everywhere |
| **Extensible** | New features = new FFI registrations |
| **Debuggable** | All external calls go through FFI (can log) |
| **Serializable** | No hidden state in evaluator |

## Comparison to Python LambdaRLM

| Aspect | Python eval.py | TypeScript eval.ts |
|--------|---------------|-------------------|
| Lines | ~875 | ~200 |
| Knows about World | Yes (imports) | No (FFI) |
| Knows about LLM | Yes (imports) | No (FFI) |
| Knows about artifacts | Yes (imports) | No (FFI) |
| Continuations | No | Yes (call/cc) |
| Conditions | No | Yes (signal/handler-bind) |

The Python evaluator is a **monolith**. The TypeScript evaluator is a **pure kernel**.
