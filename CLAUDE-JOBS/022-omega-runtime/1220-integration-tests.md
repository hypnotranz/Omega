# 1220: Integration Tests

## Status: COMPLETE ✅

## Purpose
Integration tests verifying subsystems work together correctly.

## Dependencies
- 1000-runtime-assembly.md ✅
- 1200-mocks.md ✅

## Implementation Notes
Tests implemented in `test/runtime/integration.test.ts` (30 tests) and `test/runtime/e2e.test.ts` (18 tests).
All cross-subsystem scenarios pass, tests complete in <30s.

## Source References
- Integration test requirements from subsystem jobs
- Cross-subsystem interaction points

---

## Test Structure

```
tests/integration/
├── evaluation-flow.test.ts    # Full evaluation pipeline
├── debug-flow.test.ts         # Debug session lifecycle
├── amb-transaction.test.ts    # AMB + Transaction integration
├── llm-budget.test.ts         # LLM + Budget integration
├── history-snapshot.test.ts   # History + Snapshot integration
├── event-flow.test.ts         # Event propagation
├── provider-flow.test.ts      # Provider integration
└── setup.ts
```

---

## Key Integration Scenarios

### 1. Full Evaluation Flow

```typescript
describe('Evaluation Flow', () => {
  it('processes code through entire pipeline', async () => {
    const runtime = await TestHelpers.createTestRuntime();
    const events = TestHelpers.collectEvents(runtime, 'eval-complete');

    // Parse -> Macro expand -> Evaluate -> Record history -> Return
    const result = await runtime.eval('(define (square x) (* x x))');

    expect(result.status).toBe('success');
    expect(events.events.length).toBe(1);

    // Subsequent use
    const result2 = await runtime.eval('(square 5)');
    expect(result2.value).toBe(25);

    events.stop();
    await runtime.dispose();
  });

  it('macros expand before evaluation', async () => {
    const runtime = await TestHelpers.createTestRuntime();

    // Define macro
    await runtime.eval('(defmacro when (test body) `(if ,test ,body nil))');

    // Use macro
    const result = await runtime.eval('(when #t 42)');
    expect(result.value).toBe(42);

    await runtime.dispose();
  });
});
```

### 2. AMB + Transaction Integration

```typescript
describe('AMB + Transaction', () => {
  it('rolls back transaction on AMB backtrack', async () => {
    const runtime = await TestHelpers.createTestRuntime();

    // Define mutable state
    await runtime.eval('(define x 0)');

    // AMB search with transaction
    const result = await runtime.eval(`
      (amb-search
        (with-transaction
          (let ((n (amb '(1 2 3))))
            (set! x n)
            (require (even? n))
            n)))
    `);

    // x should be 2 (the successful path), not 1 (rolled back)
    expect(result.value).toBe(2);
    const xValue = await runtime.eval('x');
    expect(xValue.value).toBe(2);

    await runtime.dispose();
  });

  it('blocks snapshot restore during AMB', async () => {
    const runtime = await TestHelpers.createTestRuntime();

    await runtime.snapshots.save('checkpoint');

    await expect(
      runtime.eval(`
        (let ((x (amb '(1 2 3))))
          (snapshots.restore "checkpoint")
          x)
      `)
    ).rejects.toThrow(/AMB search in progress/);

    await runtime.dispose();
  });
});
```

### 3. LLM + Budget Integration

```typescript
describe('LLM + Budget', () => {
  it('tracks token usage across LLM calls', async () => {
    const mockLLM = new MockLLMProvider({
      defaultResponse: 'test',
      tokenUsage: { prompt: 10, completion: 20 }
    });

    const runtime = await TestHelpers.createTestRuntime({
      providers: { llmProvider: mockLLM }
    });

    // Make LLM calls
    await runtime.eval('(effect infer.op (list "question 1"))');
    await runtime.eval('(effect infer.op (list "question 2"))');

    // Check budget
    const budget = runtime.budget.getState();
    expect(budget.tokens.used).toBe(60); // 30 per call
    expect(budget.operations.llmCalls).toBe(2);

    await runtime.dispose();
  });

  it('stops on budget exceeded', async () => {
    const runtime = await TestHelpers.createTestRuntime();
    runtime.budget.configure({ tokens: { max: 10 } });

    await expect(
      runtime.eval('(effect infer.op (list "question"))')
    ).rejects.toThrow(/budget exceeded/i);

    await runtime.dispose();
  });
});
```

### 4. Event Flow

```typescript
describe('Event Flow', () => {
  it('propagates events between subsystems', async () => {
    const runtime = await TestHelpers.createTestRuntime();

    const evalEvents = TestHelpers.collectEvents(runtime, 'eval-complete');
    const llmEvents = TestHelpers.collectEvents(runtime, 'llm-call-complete');
    const budgetEvents = TestHelpers.collectEvents(runtime, 'budget-consumed');

    await runtime.eval('(effect infer.op (list "test"))');

    expect(evalEvents.events.length).toBe(1);
    expect(llmEvents.events.length).toBe(1);
    expect(budgetEvents.events.length).toBeGreaterThan(0);

    evalEvents.stop();
    llmEvents.stop();
    budgetEvents.stop();
    await runtime.dispose();
  });
});
```

### 5. Debug Flow

```typescript
describe('Debug Flow', () => {
  it('stops at breakpoints and allows inspection', async () => {
    const runtime = await TestHelpers.createTestRuntime({ preset: 'debug' });

    // Set breakpoint
    const bp = runtime.breakpoints.addLineBreakpoint('test.lisp', 2);

    // Start debugging
    await runtime.debug.launch({
      program: `
        (define x 10)
        (define y 20)  ; Line 2 - breakpoint here
        (+ x y)
      `,
      stopOnEntry: false
    });

    // Wait for breakpoint hit
    const stoppedEvent = await TestHelpers.waitForEvent(runtime, 'stopped');
    expect(stoppedEvent.reason).toBe('breakpoint');

    // Inspect variables
    const scopes = runtime.debug.getScopes(0);
    const locals = runtime.debug.getVariables(scopes[0].variablesReference);

    expect(locals.find(v => v.name === 'x')?.value).toBe('10');

    // Continue
    await runtime.debug.continue();

    await runtime.dispose();
  });
});
```

---

## Cross-Subsystem Test Matrix

| Subsystem A | Subsystem B | Test Scenario |
|-------------|-------------|---------------|
| AMB | Transaction | Rollback on backtrack |
| AMB | StateCoordinator | Conflict detection |
| LLM | Budget | Token tracking |
| LLM | Artifacts | Response caching |
| LLM | Provenance | Call tracking |
| History | Snapshots | State restore |
| Debugger | Inspector | Variable viewing |
| Debugger | Breakpoints | Stop execution |
| Streams | LLM | Lazy LLM calls |
| Logic | Facts | Query integration |
| Conditions | Debugger | Restart selection |

---

## Test Commands

```bash
# Run all integration tests
npm run test:integration

# Run specific scenario
npm run test:integration -- --grep "AMB + Transaction"

# With verbose output
npm run test:integration -- --verbose
```

---

## Acceptance Criteria
1. All cross-subsystem scenarios pass
2. Event propagation is correct
3. State coordination works
4. No race conditions
5. Tests complete in <60s
