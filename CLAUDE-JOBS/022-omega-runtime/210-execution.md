# 210: Execution Engine

## Status: COMPLETE ✅
- **Completed**: 2026-01-29
- **Location**: OmegaLLM-3/src/runtime/subsystems/ExecutionEngine.ts (374 lines)

## Purpose
Core CESK machine implementation that evaluates expressions with proper continuation handling, stepping support, and event emission.

## Dependencies
- 100-types.md ✅
- 110-events.md ✅
- 120-providers.md ✅
- 200-macros.md ✅

## Source References
- ARCHITECTURE/02-CESK.md
- ARCHITECTURE/03-CONTINUATIONS.md
- SICP Chapter 4 (evaluator) & Chapter 5 (register machine)

---

## Deliverables

```
src/runtime/subsystems/
├── ExecutionEngine.ts       # Main execution engine
└── execution/
    ├── CESK.ts              # CESK machine state
    ├── Evaluator.ts         # Expression evaluator
    ├── Kontinuations.ts     # Continuation types
    ├── Primitives.ts        # Built-in functions
    └── StepController.ts    # Step-by-step execution
```

---

## Key Types

```typescript
/** Control: current expression being evaluated */
export type Control = Val;

/** Environment: variable bindings */
export type Environment = Map<string, Val>;

/** Store: mutable cells for set! */
export type Store = Map<number, Val>;

/** Kontinuation: what to do next */
export type Kontinuation =
  | { type: 'halt' }
  | { type: 'apply-cont'; fn: Val; args: Val[]; env: Environment; k: Kontinuation }
  | { type: 'arg-cont'; remaining: Val[]; evaluated: Val[]; env: Environment; k: Kontinuation }
  | { type: 'if-cont'; consequent: Val; alternative: Val; env: Environment; k: Kontinuation }
  | { type: 'let-cont'; bindings: LetBinding[]; body: Val[]; env: Environment; k: Kontinuation }
  | { type: 'begin-cont'; remaining: Val[]; env: Environment; k: Kontinuation }
  | { type: 'set!-cont'; name: string; env: Environment; k: Kontinuation }
  | { type: 'define-cont'; name: string; env: Environment; k: Kontinuation };

/** Complete CESK state */
export interface CEKState {
  control: Control;
  environment: Environment;
  kontinuation: Kontinuation;
  store: Store;
}

/** Execution result */
export interface EvalResult extends Outcome<Val> {
  state: CEKState;
  stepCount: number;
  emissions: RuntimeEvent[];
}
```

---

## Key Interface

```typescript
export interface ExecutionEngine {
  /**
   * Evaluate expression to completion.
   */
  eval(expr: Val, env?: Environment): Promise<EvalResult>;

  /**
   * Take a single evaluation step.
   * Returns new state after one reduction.
   */
  step(state: CEKState): Promise<CEKState>;

  /**
   * Check if state is terminal (halt continuation).
   */
  isTerminal(state: CEKState): boolean;

  /**
   * Get current CESK state for debugging.
   */
  getState(): CEKState;

  /**
   * Inject state (for restoration/debugging).
   */
  setState(state: CEKState): void;

  /**
   * Create initial CESK state from expression.
   */
  initState(expr: Val, env?: Environment): CEKState;

  /**
   * Run until breakpoint or completion.
   */
  runUntilBreakOrDone(
    state: CEKState,
    isBreakpoint: (state: CEKState) => boolean
  ): Promise<EvalResult>;

  /**
   * Access macro manager for expansion.
   */
  getMacroManager(): MacroManager;

  /**
   * Access emitter for events.
   */
  getEmitter(): RuntimeEventEmitter;
}
```

---

## CESK Transition Rules

```
┌────────────────────────────────────────────────────────────────┐
│ CESK TRANSITIONS                                                │
├────────────────────────────────────────────────────────────────┤
│ ⟨number, ρ, κ, σ⟩ → ⟨κ(number), σ⟩                             │
│ ⟨symbol, ρ, κ, σ⟩ → ⟨κ(ρ(symbol)), σ⟩                          │
│ ⟨(if test c a), ρ, κ, σ⟩ → ⟨test, ρ, if-κ(c,a,ρ,κ), σ⟩        │
│ ⟨(lambda ...), ρ, κ, σ⟩ → ⟨κ(closure(λ, ρ)), σ⟩                │
│ ⟨(f args...), ρ, κ, σ⟩ → ⟨f, ρ, arg-κ(args,ρ,κ), σ⟩           │
└────────────────────────────────────────────────────────────────┘
```

---

## Step Implementation

```typescript
async function step(state: CEKState): Promise<CEKState> {
  const { control, environment, kontinuation, store } = state;

  // Self-evaluating forms
  if (isNumber(control) || isString(control) || isBoolean(control)) {
    return applyKontinuation(kontinuation, control, store);
  }

  // Variable lookup
  if (isSymbol(control)) {
    const value = environment.get(symbolName(control));
    if (value === undefined) {
      throw new RuntimeError(`Unbound variable: ${symbolName(control)}`);
    }
    return applyKontinuation(kontinuation, value, store);
  }

  // Special forms and applications
  if (isList(control) && control.length > 0) {
    const head = control[0];

    // Quote
    if (isSymbolNamed(head, 'quote')) {
      return applyKontinuation(kontinuation, control[1], store);
    }

    // If
    if (isSymbolNamed(head, 'if')) {
      const [_, test, consequent, alternative] = control;
      return {
        control: test,
        environment,
        kontinuation: {
          type: 'if-cont',
          consequent,
          alternative: alternative ?? null,
          env: environment,
          k: kontinuation
        },
        store
      };
    }

    // Lambda
    if (isSymbolNamed(head, 'lambda')) {
      const [_, params, ...body] = control;
      const closure: Closure = {
        type: 'closure',
        params: parseParams(params),
        body: body.length === 1 ? body[0] : ['begin', ...body],
        env: environment
      };
      return applyKontinuation(kontinuation, closure, store);
    }

    // Application: evaluate operator first
    return {
      control: head,
      environment,
      kontinuation: {
        type: 'arg-cont',
        remaining: control.slice(1),
        evaluated: [],
        env: environment,
        k: kontinuation
      },
      store
    };
  }

  throw new RuntimeError(`Cannot evaluate: ${stringify(control)}`);
}
```

---

## Event Emission

The execution engine emits events at key points:

```typescript
// Before evaluation starts
emitter.emit('eval-start', { expr, env: environment });

// After each step
emitter.emit('step', { state, stepNumber });

// When value is produced
emitter.emit('eval-complete', { result, stepCount });

// On error
emitter.emit('eval-error', { error, state });
```

---

## Test Requirements

### Unit Tests: `tests/runtime/subsystems/ExecutionEngine.test.ts`
- [ ] eval() handles self-evaluating forms (numbers, strings, booleans)
- [ ] eval() handles variable lookup
- [ ] eval() handles if expressions
- [ ] eval() handles lambda creation and application
- [ ] eval() handles let, let*, letrec
- [ ] eval() handles begin (sequencing)
- [ ] eval() handles define
- [ ] eval() handles set!
- [ ] step() advances state correctly
- [ ] isTerminal() detects halt continuation
- [ ] initState() creates valid initial CESK state
- [ ] Macro expansion happens before evaluation
- [ ] Events are emitted at correct points

### Integration Tests
- [ ] Recursive functions work (factorial, fibonacci)
- [ ] Mutual recursion works (even?/odd?)
- [ ] Closures capture environment correctly
- [ ] Tail call optimization (if implemented)
- [ ] Stepping through evaluation works for debugging

---

## Acceptance Criteria
1. All core Scheme forms evaluate correctly
2. CESK state is inspectable at any point
3. Single-stepping works for debugging
4. Events fire at documented points
5. Error messages include source location when available
6. Performance: <100ms for typical expressions
