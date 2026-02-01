# 1210: Unit Tests

## Status: NOT_STARTED ❌

## Purpose
Comprehensive unit tests for all OmegaRuntime subsystems.

## Dependencies
- 1000-runtime-assembly.md ✅
- 1200-mocks.md ❌

## Source References
- Test requirements from each subsystem job file

---

## Test Structure

```
tests/runtime/
├── subsystems/
│   ├── ExecutionEngine.test.ts
│   ├── MacroManager.test.ts
│   ├── BreakpointManager.test.ts
│   ├── StateInspector.test.ts
│   ├── HistoryManager.test.ts
│   ├── SnapshotManager.test.ts
│   ├── SessionManager.test.ts
│   ├── ArtifactManager.test.ts
│   ├── FactsManager.test.ts
│   ├── FixpointManager.test.ts
│   ├── ConditionsManager.test.ts
│   ├── TransactionManager.test.ts
│   ├── ProvenanceManager.test.ts
│   ├── SecurityManager.test.ts
│   ├── BudgetManager.test.ts
│   ├── ConcurrencyManager.test.ts
│   ├── LLMIntegration.test.ts
│   ├── ExpertsManager.test.ts
│   ├── AmbManager.test.ts
│   ├── StreamsManager.test.ts
│   └── LogicManager.test.ts
├── internal/
│   ├── StateCoordinator.test.ts
│   └── BudgetAwareLLMAdapter.test.ts
├── protocol/
│   └── ProtocolServer.test.ts
├── OmegaRuntime.test.ts
└── setup.ts
```

---

## Test Template

```typescript
// tests/runtime/subsystems/ExecutionEngine.test.ts

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { TestHelpers, TestFixtures, MockLLMProvider } from '../../testing';
import { OmegaRuntime } from '../../../src/runtime';

describe('ExecutionEngine', () => {
  let runtime: OmegaRuntime;

  beforeEach(async () => {
    runtime = await TestHelpers.createTestRuntime();
  });

  afterEach(async () => {
    await runtime.dispose();
  });

  describe('eval()', () => {
    it('evaluates literals', async () => {
      const result = await runtime.eval('42');
      expect(result.value).toBe(42);
    });

    it('evaluates arithmetic', async () => {
      const result = await runtime.eval('(+ 1 2)');
      expect(result.value).toBe(3);
    });

    it('evaluates nested expressions', async () => {
      const result = await runtime.eval('(+ (* 2 3) (- 10 5))');
      expect(result.value).toBe(11);
    });

    it('evaluates lambda applications', async () => {
      const result = await runtime.eval('((lambda (x) (* x x)) 5)');
      expect(result.value).toBe(25);
    });

    it('evaluates if expressions', async () => {
      expect((await runtime.eval('(if #t 1 2)')).value).toBe(1);
      expect((await runtime.eval('(if #f 1 2)')).value).toBe(2);
    });

    it('evaluates let expressions', async () => {
      const result = await runtime.eval('(let ((x 10)) (+ x 5))');
      expect(result.value).toBe(15);
    });

    it('handles recursive functions', async () => {
      await runtime.eval('(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))');
      const result = await runtime.eval('(fact 5)');
      expect(result.value).toBe(120);
    });

    it('throws on undefined variable', async () => {
      await expect(runtime.eval('undefined-var')).rejects.toThrow(/unbound/i);
    });
  });

  describe('step()', () => {
    it('advances CESK state', async () => {
      const expr = runtime.parse('(+ 1 2)');
      const initialState = runtime.execution.initState(expr);

      const nextState = await runtime.execution.step(initialState);
      expect(nextState).not.toEqual(initialState);
    });

    it('eventually reaches terminal state', async () => {
      const expr = runtime.parse('(+ 1 2)');
      let state = runtime.execution.initState(expr);

      let steps = 0;
      while (!runtime.execution.isTerminal(state) && steps < 100) {
        state = await runtime.execution.step(state);
        steps++;
      }

      expect(runtime.execution.isTerminal(state)).toBe(true);
    });
  });
});
```

---

## Test Categories

### Core Evaluation
- Literals (numbers, strings, booleans)
- Arithmetic operations
- Variable binding and lookup
- Lambda and closure
- Conditionals (if)
- Let bindings (let, let*, letrec)
- Define
- Set!
- Begin (sequencing)
- Recursion

### Macros
- defmacro
- Quasiquote/unquote/splice
- gensym
- Macro expansion

### State Management
- StateCoordinator checkpoint/restore
- Context tracking (amb/transaction/fixpoint)
- Conflict detection

### Debugging
- Breakpoint add/remove
- Conditional breakpoints
- Hit conditions
- Stack trace
- Variable inspection
- Stepping

### Data Management
- Artifact caching
- Fact assertion/query
- Fixpoint computation

### Control Flow
- Conditions/restarts
- Transactions

### Governance
- Provenance tracking
- Security capabilities
- Budget tracking

### Semantic
- AMB choice/fail/require
- Streams (lazy evaluation)
- Logic programming

---

## Coverage Goals

| Subsystem | Target Coverage |
|-----------|-----------------|
| ExecutionEngine | 95% |
| MacroManager | 90% |
| StateCoordinator | 95% |
| All others | 85% |

---

## Test Commands

```bash
# Run all unit tests
npm run test:unit

# Run specific subsystem
npm run test:unit -- --grep "ExecutionEngine"

# Run with coverage
npm run test:unit -- --coverage

# Watch mode
npm run test:unit -- --watch
```

---

## Acceptance Criteria
1. All subsystem unit tests pass
2. Coverage meets targets
3. Tests are isolated (use mocks)
4. Tests are deterministic
5. Tests run quickly (<30s total)
