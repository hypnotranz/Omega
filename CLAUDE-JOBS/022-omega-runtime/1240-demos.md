# 1240: Demo Coverage

## Status: COMPLETE ✅

## Purpose
Verify all documented demos work with the refactored runtime.

## Dependencies
- 1000-runtime-assembly.md ✅
- 1200-mocks.md ✅

## Implementation Notes
Demo tests implemented in three test files (49 total tests):

**`test/demos/basic.test.ts`** (7 tests):
- Basic evaluation, arithmetic expressions
- Lambda/function application via ExecutionEngine
- Session management (init, terminate, reset)
- History and snapshot integration

**`test/demos/semantic.test.ts`** (16 tests):
- AMB nondeterminism: manager creation, search state, reset, stats, events
- Streams: fromList, take, toList, isNull/empty detection
- Logic programming: fact assertion, ID retrieval, clearing, queries
- Combined operations: streams with facts

**`test/demos/debugging.test.ts`** (26 tests):
- Breakpoints: line, step, removal, clearing
- Stepping: stopOnEntry, continue, step/next/stepOut/runToStep
- Inspection: stack traces, scopes, variables, expression evaluation
- History: recording, retrieval, time travel navigation
- Snapshots: save, list, exists, delete, diff

## Source References
- docs/USER-MANUAL chapters
- Demo scripts in demos/

---

## Demo Categories

### 1. Basic Evaluation Demos

```typescript
// tests/demos/basic.test.ts

describe('Basic Evaluation Demos', () => {
  it('Chapter 01: Lisp basics', async () => {
    const runtime = await createTestRuntime();

    // Arithmetic
    await TestHelpers.evalExpect(runtime, '(+ 1 2 3)', 6);
    await TestHelpers.evalExpect(runtime, '(* 2 (+ 3 4))', 14);

    // Lists
    await TestHelpers.evalExpect(runtime, '(car \'(1 2 3))', 1);
    await TestHelpers.evalExpect(runtime, '(cdr \'(1 2 3))', [2, 3]);

    // Functions
    await runtime.eval('(define (square x) (* x x))');
    await TestHelpers.evalExpect(runtime, '(square 5)', 25);

    await runtime.dispose();
  });

  it('Chapter 03: Higher-order functions', async () => {
    const runtime = await createTestRuntime();

    await runtime.eval('(define (apply-twice f x) (f (f x)))');
    await runtime.eval('(define (add1 x) (+ x 1))');
    await TestHelpers.evalExpect(runtime, '(apply-twice add1 5)', 7);

    await runtime.eval('(define (compose f g) (lambda (x) (f (g x))))');
    await runtime.eval('(define add2 (compose add1 add1))');
    await TestHelpers.evalExpect(runtime, '(add2 5)', 7);

    await runtime.dispose();
  });
});
```

### 2. Semantic Effects Demos

```typescript
describe('Semantic Effects Demos', () => {
  it('Chapter 02: infer.op basic usage', async () => {
    const mockLLM = new MockLLMProvider({
      responses: new Map([
        ['capital of France', 'Paris'],
        ['translate hello', 'bonjour']
      ])
    });
    const runtime = await createTestRuntime({ providers: { llmProvider: mockLLM } });

    const result = await runtime.eval('(effect infer.op (list "What is the capital of France?"))');
    expect(result.value).toContain('Paris');

    await runtime.dispose();
  });

  it('Chapter 04: search.op distribution', async () => {
    const mockLLM = new MockLLMProvider({
      defaultResponse: 'translation'
    });
    const runtime = await createTestRuntime({ providers: { llmProvider: mockLLM } });

    // search.op returns distribution
    const result = await runtime.eval('(effect search.op (list "Translate hello") :num-samples 3)');
    expect(result.value).toBeDefined();

    await runtime.dispose();
  });
});
```

### 3. AMB Demos

```typescript
describe('AMB Demos', () => {
  it('Chapter 05: Basic nondeterminism', async () => {
    const runtime = await createTestRuntime();

    // Simple choice
    const result = await runtime.eval(`
      (let ((x (amb '(1 2 3))))
        (require (> x 1))
        x)
    `);
    expect(result.value).toBe(2);

    await runtime.dispose();
  });

  it('Chapter 05: Multiple AMB', async () => {
    const runtime = await createTestRuntime();

    const result = await runtime.eval(`
      (amb-search-all
        (let ((x (amb '(1 2 3)))
              (y (amb '(a b))))
          (list x y))
        6)
    `);

    // Should find all 6 combinations
    expect(result.value.length).toBe(6);

    await runtime.dispose();
  });

  it('Chapter 26: AMB as inference engine', async () => {
    const runtime = await createTestRuntime();

    // Pythagorean triples
    const result = await runtime.eval(`
      (amb-search-all
        (let ((a (amb '(1 2 3 4 5)))
              (b (amb '(1 2 3 4 5)))
              (c (amb '(1 2 3 4 5))))
          (require (= (+ (* a a) (* b b)) (* c c)))
          (list a b c))
        10)
    `);

    expect(result.value).toContainEqual([3, 4, 5]);

    await runtime.dispose();
  });
});
```

### 4. Streams Demos

```typescript
describe('Streams Demos', () => {
  it('Chapter 07: Basic streams', async () => {
    const runtime = await createTestRuntime();

    await runtime.eval('(define s (list->stream \'(1 2 3 4 5)))');

    const car = await runtime.eval('(stream-car s)');
    expect(car.value).toBe(1);

    const take = await runtime.eval('(stream->list (stream-take s 3) 3)');
    expect(take.value).toEqual([1, 2, 3]);

    await runtime.dispose();
  });

  it('Chapter 23: Streams of inference', async () => {
    const mockLLM = new MockLLMProvider({
      defaultResponse: 'elaborated'
    });
    const runtime = await createTestRuntime({ providers: { llmProvider: mockLLM } });

    // Lazy elaboration stream
    await runtime.eval(`
      (define (elaborate text)
        (effect infer.op (list "Elaborate: " text)))
    `);

    await runtime.eval(`
      (define elaborations
        (stream-iterate elaborate "trees"))
    `);

    // Only force what we need
    const result = await runtime.eval('(stream->list elaborations 2)');
    expect(result.value.length).toBe(2);

    // Should have made only 1 LLM call (first element is seed)
    expect(mockLLM.getCallCount()).toBe(1);

    await runtime.dispose();
  });
});
```

### 5. Logic Programming Demos

```typescript
describe('Logic Programming Demos', () => {
  it('Chapter 27: Semantic facts', async () => {
    const mockLLM = new MockLLMProvider({
      responses: new Map([
        ['Nobel Prize', 'Two - Physics and Chemistry']
      ])
    });
    const runtime = await createTestRuntime({ providers: { llmProvider: mockLLM } });

    await runtime.eval('(fact! "Marie Curie won the Nobel Prize in Physics in 1903")');
    await runtime.eval('(fact! "Marie Curie won the Nobel Prize in Chemistry in 1911")');

    const result = await runtime.eval('(facts.ask "How many Nobel Prizes did Marie Curie win?")');
    expect(result.value).toContain('Two');

    await runtime.dispose();
  });

  it('Chapter 27: Pattern matching', async () => {
    const runtime = await createTestRuntime();

    await runtime.eval('(fact! "Alice is the mother of Bob")');
    await runtime.eval('(fact! "Bob is the father of Charlie")');

    const result = await runtime.eval('(facts.query :pattern "mother")');
    expect(result.value.length).toBeGreaterThan(0);

    await runtime.dispose();
  });
});
```

### 6. Debugging Demos

```typescript
describe('Debugging Demos', () => {
  it('Chapter 08: Breakpoints', async () => {
    const runtime = await createTestRuntime({ preset: 'debug' });

    runtime.breakpoints.addLineBreakpoint('test', 2);

    const events = TestHelpers.collectEvents(runtime, 'stopped');

    // Evaluation should stop at breakpoint
    const evalPromise = runtime.eval(`
      (define x 10)
      (define y 20)
      (+ x y)
    `);

    await TestHelpers.waitForEvent(runtime, 'stopped', 5000);
    expect(events.events[0].reason).toBe('breakpoint');

    await runtime.debug.continue();

    events.stop();
    await runtime.dispose();
  });

  it('Chapter 09: History and snapshots', async () => {
    const runtime = await createTestRuntime();

    await runtime.eval('(define x 1)');
    await runtime.eval('(define y 2)');
    await runtime.eval('(define z 3)');

    const history = runtime.history.getLast(3);
    expect(history.length).toBe(3);

    // Save and restore
    await runtime.snapshots.save('checkpoint');
    await runtime.eval('(set! x 100)');

    const before = await runtime.eval('x');
    expect(before.value).toBe(100);

    await runtime.snapshots.restore('checkpoint');

    const after = await runtime.eval('x');
    expect(after.value).toBe(1);

    await runtime.dispose();
  });
});
```

---

## Demo Validation Script

```typescript
// scripts/validate-demos.ts

import { readdirSync, readFileSync } from 'fs';
import { createRuntime } from '../src/runtime';

async function validateDemos() {
  const demoFiles = readdirSync('demos').filter(f => f.endsWith('.lisp'));

  for (const file of demoFiles) {
    console.log(`Validating ${file}...`);

    const runtime = await createRuntime({ preset: 'testing' });
    const code = readFileSync(`demos/${file}`, 'utf-8');

    try {
      await runtime.eval(code);
      console.log(`  ✓ ${file} passed`);
    } catch (e) {
      console.error(`  ✗ ${file} failed: ${e.message}`);
    }

    await runtime.dispose();
  }
}

validateDemos();
```

---

## Test Commands

```bash
# Run demo tests
npm run test:demos

# Validate all demo files
npm run validate:demos
```

---

## Acceptance Criteria
1. All USER-MANUAL examples work
2. All demo/ files execute without error
3. AMB examples find correct solutions
4. Stream examples are lazy
5. Logic examples query correctly
6. Debug examples stop at breakpoints
