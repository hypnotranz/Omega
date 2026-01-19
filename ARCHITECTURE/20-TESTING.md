# 20: Testing (Verification Strategy)

## Testing Philosophy

LambdaLLM follows a layered testing approach:

```
┌─────────────────────────────────────────────────────────────────┐
│                        Testing Pyramid                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│                        ┌─────────┐                              │
│                        │   E2E   │  ← Few, slow, high value     │
│                      ┌─┴─────────┴─┐                            │
│                      │ Integration │  ← Medium count            │
│                    ┌─┴─────────────┴─┐                          │
│                    │   Unit Tests    │  ← Many, fast            │
│                  ┌─┴─────────────────┴─┐                        │
│                  │   Property Tests    │  ← Thousands           │
│                ┌─┴─────────────────────┴─┐                      │
│                │   Static Analysis       │  ← TypeScript types  │
│                └─────────────────────────┘                      │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Unit Tests

### TypeScript Layer (Bun Test)

```typescript
// src/reader.test.ts
import { describe, test, expect } from 'bun:test';
import { read, readAll } from './reader';

describe('Reader', () => {
  describe('atoms', () => {
    test('parses numbers', () => {
      expect(read('42')).toBe(42);
      expect(read('-3.14')).toBe(-3.14);
      expect(read('1e10')).toBe(1e10);
    });

    test('parses strings', () => {
      expect(read('"hello"')).toBe('hello');
      expect(read('"with\\"quote"')).toBe('with"quote');
      expect(read('"with\\nnewline"')).toBe('with\nnewline');
    });

    test('parses symbols', () => {
      expect(read('foo')).toEqual(Symbol.for('foo'));
      expect(read('+')).toEqual(Symbol.for('+'));
      expect(read('foo-bar')).toEqual(Symbol.for('foo-bar'));
    });

    test('parses booleans', () => {
      expect(read('#t')).toBe(true);
      expect(read('#f')).toBe(false);
    });
  });

  describe('lists', () => {
    test('parses empty list', () => {
      expect(read('()')).toEqual([]);
    });

    test('parses simple list', () => {
      expect(read('(1 2 3)')).toEqual([1, 2, 3]);
    });

    test('parses nested list', () => {
      expect(read('((1 2) (3 4))')).toEqual([[1, 2], [3, 4]]);
    });

    test('parses mixed list', () => {
      const result = read('(+ 1 "hello")');
      expect(result).toEqual([Symbol.for('+'), 1, 'hello']);
    });
  });

  describe('quote', () => {
    test('expands single quote', () => {
      expect(read("'x")).toEqual([Symbol.for('quote'), Symbol.for('x')]);
    });

    test('expands quasiquote', () => {
      expect(read('`x')).toEqual([Symbol.for('quasiquote'), Symbol.for('x')]);
    });

    test('expands unquote', () => {
      expect(read(',x')).toEqual([Symbol.for('unquote'), Symbol.for('x')]);
    });
  });

  describe('source locations', () => {
    test('tracks line and column', () => {
      const result = read('(+ 1 2)', { trackLocations: true });
      expect(result._loc).toEqual({
        file: '<string>',
        line: 1,
        column: 1,
        offset: 0,
      });
    });
  });
});
```

### Evaluator Tests

```typescript
// src/eval.test.ts
import { describe, test, expect } from 'bun:test';
import { createTestRuntime } from './test-utils';

describe('Evaluator', () => {
  describe('special forms', () => {
    test('quote returns unevaluated', async () => {
      const rt = createTestRuntime();
      expect(await rt.eval("'(1 2 3)")).toEqual([1, 2, 3]);
    });

    test('if evaluates correct branch', async () => {
      const rt = createTestRuntime();
      expect(await rt.eval('(if #t 1 2)')).toBe(1);
      expect(await rt.eval('(if #f 1 2)')).toBe(2);
    });

    test('lambda creates closure', async () => {
      const rt = createTestRuntime();
      await rt.eval('(define add1 (lambda (x) (+ x 1)))');
      expect(await rt.eval('(add1 5)')).toBe(6);
    });

    test('define binds value', async () => {
      const rt = createTestRuntime();
      await rt.eval('(define x 42)');
      expect(await rt.eval('x')).toBe(42);
    });

    test('set! mutates binding', async () => {
      const rt = createTestRuntime();
      await rt.eval('(define x 1)');
      await rt.eval('(set! x 2)');
      expect(await rt.eval('x')).toBe(2);
    });
  });

  describe('closures', () => {
    test('captures environment', async () => {
      const rt = createTestRuntime();
      await rt.eval('(define make-adder (lambda (n) (lambda (x) (+ x n))))');
      await rt.eval('(define add5 (make-adder 5))');
      expect(await rt.eval('(add5 10)')).toBe(15);
    });

    test('lexical scope', async () => {
      const rt = createTestRuntime();
      await rt.eval('(define x 1)');
      await rt.eval('(define f (lambda () x))');
      await rt.eval('(define x 2)');  // Rebind x
      expect(await rt.eval('(f)')).toBe(1);  // f sees original binding
    });
  });

  describe('recursion', () => {
    test('tail call optimization', async () => {
      const rt = createTestRuntime();
      await rt.eval(`
        (define (sum-to n acc)
          (if (= n 0)
              acc
              (sum-to (- n 1) (+ acc n))))
      `);
      // Should not stack overflow
      expect(await rt.eval('(sum-to 10000 0)')).toBe(50005000);
    });
  });
});
```

---

## Lisp-Level Tests

```lisp
;; test/core.test.lisp
(ns test.core
  (:require [lambdallm.test :refer [deftest is are testing]]))

(deftest test-arithmetic
  (testing "basic operations"
    (is (= (+ 1 2) 3))
    (is (= (- 5 3) 2))
    (is (= (* 4 3) 12))
    (is (= (/ 10 2) 5)))

  (testing "nested operations"
    (is (= (+ 1 (* 2 3)) 7))
    (is (= (* (+ 1 2) (+ 3 4)) 21))))

(deftest test-list-operations
  (testing "basic list functions"
    (is (= (car '(1 2 3)) 1))
    (is (= (cdr '(1 2 3)) '(2 3)))
    (is (= (cons 1 '(2 3)) '(1 2 3))))

  (testing "higher-order functions"
    (is (= (map (lambda (x) (* x 2)) '(1 2 3)) '(2 4 6)))
    (is (= (filter (lambda (x) (> x 2)) '(1 2 3 4)) '(3 4)))
    (is (= (reduce + 0 '(1 2 3 4)) 10))))

(deftest test-closures
  (let ((make-counter (lambda ()
                        (let ((count 0))
                          (lambda ()
                            (set! count (+ count 1))
                            count)))))
    (let ((c1 (make-counter))
          (c2 (make-counter)))
      (is (= (c1) 1))
      (is (= (c1) 2))
      (is (= (c2) 1))  ; Independent counter
      (is (= (c1) 3)))))

(deftest test-conditions
  (testing "signal and handler"
    (is (= (handler-bind
             ((test-condition (lambda (c) 'handled)))
             (signal 'test-condition))
           'handled)))

  (testing "restarts"
    (is (= (handler-bind
             ((test-error (lambda (c) (invoke-restart 'use-value 42))))
             (restart-case
               (signal 'test-error)
               (use-value (v) v)))
           42))))
```

---

## Property-Based Tests

```typescript
// src/property.test.ts
import { describe, test } from 'bun:test';
import fc from 'fast-check';
import { read, print } from './reader';
import { createTestRuntime } from './test-utils';

describe('Property Tests', () => {
  describe('Reader/Printer round-trip', () => {
    test('numbers round-trip', () => {
      fc.assert(fc.property(
        fc.double({ noNaN: true, noDefaultInfinity: true }),
        (n) => {
          const printed = print(n);
          const parsed = read(printed);
          return parsed === n || (isNaN(n) && isNaN(parsed));
        }
      ));
    });

    test('strings round-trip', () => {
      fc.assert(fc.property(
        fc.string(),
        (s) => {
          const printed = print(s);
          const parsed = read(printed);
          return parsed === s;
        }
      ));
    });

    test('lists round-trip', () => {
      const arbValue = fc.oneof(
        fc.integer(),
        fc.string(),
        fc.constant(true),
        fc.constant(false)
      );

      fc.assert(fc.property(
        fc.array(arbValue, { maxLength: 10 }),
        (arr) => {
          const printed = print(arr);
          const parsed = read(printed);
          return JSON.stringify(parsed) === JSON.stringify(arr);
        }
      ));
    });
  });

  describe('Evaluator properties', () => {
    test('arithmetic is associative', async () => {
      const rt = createTestRuntime();

      await fc.assert(fc.asyncProperty(
        fc.integer(),
        fc.integer(),
        fc.integer(),
        async (a, b, c) => {
          const left = await rt.eval(`(+ (+ ${a} ${b}) ${c})`);
          const right = await rt.eval(`(+ ${a} (+ ${b} ${c}))`);
          return left === right;
        }
      ));
    });

    test('map length preserved', async () => {
      const rt = createTestRuntime();

      await fc.assert(fc.asyncProperty(
        fc.array(fc.integer(), { maxLength: 100 }),
        async (arr) => {
          const listStr = `'(${arr.join(' ')})`;
          const original = await rt.eval(`(length ${listStr})`);
          const mapped = await rt.eval(`(length (map (lambda (x) (* x 2)) ${listStr}))`);
          return original === mapped;
        }
      ));
    });
  });

  describe('Continuation properties', () => {
    test('captured continuation can be invoked multiple times', async () => {
      const rt = createTestRuntime();

      await rt.eval(`
        (define saved nil)
        (define result (+ 1 (call/cc (lambda (k)
                                       (set! saved k)
                                       10))))
      `);

      const result1 = await rt.eval('result');
      const result2 = await rt.eval('(saved 20)');
      const result3 = await rt.eval('(saved 30)');

      expect(result1).toBe(11);
      expect(result2).toBe(21);
      expect(result3).toBe(31);
    });
  });
});
```

---

## Integration Tests

```typescript
// test/integration/protocol.test.ts
import { describe, test, expect, beforeAll, afterAll } from 'bun:test';
import { createServer, Client } from '../helpers';

describe('Protocol Integration', () => {
  let server: Server;
  let client: Client;

  beforeAll(async () => {
    server = await createServer({ port: 0 });
    client = new Client(`ws://localhost:${server.port}`);
    await client.connect();
  });

  afterAll(async () => {
    await client.close();
    await server.close();
  });

  test('eval returns result', async () => {
    const response = await client.send({
      op: 'eval',
      code: '(+ 1 2)',
    });

    expect(response.status).toEqual(['done']);
    expect(response.value).toBe('3');
  });

  test('session state persists', async () => {
    await client.send({
      op: 'eval',
      code: '(define x 42)',
    });

    const response = await client.send({
      op: 'eval',
      code: 'x',
    });

    expect(response.value).toBe('42');
  });

  test('snapshot and restore', async () => {
    // Set up state
    await client.send({ op: 'eval', code: '(define counter 0)' });
    await client.send({ op: 'eval', code: '(set! counter 1)' });

    // Take snapshot
    const { snapshotId } = await client.send({ op: 'snapshot' });

    // Modify state
    await client.send({ op: 'eval', code: '(set! counter 100)' });

    // Restore
    await client.send({ op: 'restore', snapshotId });

    // Verify restored
    const response = await client.send({ op: 'eval', code: 'counter' });
    expect(response.value).toBe('1');
  });
});
```

---

## E2E Tests

```typescript
// test/e2e/repl.test.ts
import { describe, test, expect } from 'bun:test';
import { spawn } from 'bun';

describe('REPL E2E', () => {
  test('interactive session', async () => {
    const proc = spawn({
      cmd: ['bun', 'run', 'src/cli.ts'],
      stdin: 'pipe',
      stdout: 'pipe',
    });

    const writer = proc.stdin.getWriter();
    const reader = proc.stdout.getReader();

    // Send input
    await writer.write(new TextEncoder().encode('(+ 1 2)\n'));

    // Read output
    const { value } = await reader.read();
    const output = new TextDecoder().decode(value);

    expect(output).toContain('3');

    await writer.write(new TextEncoder().encode('(exit)\n'));
    await proc.exited;
  });

  test('file execution', async () => {
    const proc = spawn({
      cmd: ['bun', 'run', 'src/cli.ts', 'test/fixtures/simple.lisp'],
      stdout: 'pipe',
    });

    const output = await new Response(proc.stdout).text();
    expect(output).toContain('success');
  });
});
```

---

## Test Utilities

```typescript
// src/test-utils.ts

export function createTestRuntime(config?: Partial<RuntimeConfig>): TestRuntime {
  return new TestRuntime({
    world: new InMemoryWorld(),
    llm: new MockLLM(),
    ...config,
  });
}

export class MockLLM implements LLMAdapter {
  private responses: Map<string, string> = new Map();

  setResponse(prompt: string, response: string): void {
    this.responses.set(prompt, response);
  }

  async complete(prompt: string): Promise<string> {
    return this.responses.get(prompt) ?? `Mock response for: ${prompt}`;
  }
}

export class InMemoryWorld implements World {
  private files: Map<string, string> = new Map();

  read(path: string): string {
    if (!this.files.has(path)) {
      throw new Error(`File not found: ${path}`);
    }
    return this.files.get(path)!;
  }

  write(path: string, content: string): void {
    this.files.set(path, content);
  }

  list(pattern: string): string[] {
    const regex = globToRegex(pattern);
    return Array.from(this.files.keys()).filter(k => regex.test(k));
  }

  fingerprint(): string {
    const entries = Array.from(this.files.entries()).sort();
    return hash(JSON.stringify(entries));
  }
}
```

---

## Coverage Requirements

```typescript
// bunfig.toml
[test]
coverage = true
coverageThreshold = {
  lines = 80,
  functions = 80,
  branches = 75,
  statements = 80
}

# Critical paths require higher coverage
[test.coverage.thresholds]
"src/eval.ts" = { lines = 95, branches = 90 }
"src/continuation.ts" = { lines = 95, branches = 90 }
"src/condition.ts" = { lines = 90, branches = 85 }
```

---

## CI Pipeline

```yaml
# .github/workflows/test.yml
name: Test

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - uses: oven-sh/setup-bun@v1
        with:
          bun-version: latest

      - name: Install dependencies
        run: bun install

      - name: Type check
        run: bun run typecheck

      - name: Unit tests
        run: bun test --coverage

      - name: Lisp tests
        run: bun run test:lisp

      - name: Integration tests
        run: bun run test:integration

      - name: E2E tests
        run: bun run test:e2e

      - name: Upload coverage
        uses: codecov/codecov-action@v3
```

---

## Test Organization

```
test/
├── unit/
│   ├── reader.test.ts
│   ├── eval.test.ts
│   ├── continuation.test.ts
│   └── condition.test.ts
├── property/
│   ├── roundtrip.test.ts
│   └── invariants.test.ts
├── integration/
│   ├── protocol.test.ts
│   └── persistence.test.ts
├── e2e/
│   ├── repl.test.ts
│   └── cli.test.ts
├── fixtures/
│   ├── simple.lisp
│   └── complex.lisp
└── helpers/
    ├── server.ts
    └── client.ts

lib/test/
├── core.test.lisp
├── list.test.lisp
├── condition.test.lisp
└── macro.test.lisp
```
