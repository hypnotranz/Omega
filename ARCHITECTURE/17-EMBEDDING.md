# 17: Embedding API

## Overview

LambdaLLM can be embedded in:
- Node.js applications
- Browser applications
- Deno/Bun applications
- Other TypeScript/JavaScript projects

---

## Quick Start

### Node.js

```typescript
import { Runtime, InMemoryWorld } from 'lambdallm';

// Create runtime
const runtime = new Runtime({
  world: new InMemoryWorld(),
});

// Evaluate expression
const result = await runtime.eval('(+ 1 2)');
console.log(result);  // 3

// Define function and call it
await runtime.eval('(define (square x) (* x x))');
const squared = await runtime.eval('(square 5)');
console.log(squared);  // 25
```

### Browser

```typescript
import { Runtime, VirtualWorld } from 'lambdallm/browser';

const runtime = new Runtime({
  world: new VirtualWorld(),  // IndexedDB-backed
});

const result = await runtime.eval('(+ 1 2)');
```

---

## Runtime Configuration

```typescript
interface RuntimeConfig {
  // World implementation
  world?: World;

  // LLM adapter
  llm?: LLMAdapter;

  // Initial namespace
  namespace?: string;

  // Preloaded modules
  modules?: string[];

  // FFI extensions
  ffi?: FFIExtension[];

  // Memory limits
  maxMemory?: number;

  // Execution limits
  maxSteps?: number;
  timeout?: number;

  // Debugging
  traceEnabled?: boolean;
  snapshotEnabled?: boolean;

  // Security
  sandboxed?: boolean;
  allowedFFI?: string[];
}

// Example
const runtime = new Runtime({
  world: new FileWorld('./workspace'),
  llm: new OpenAIAdapter({ apiKey: process.env.OPENAI_API_KEY }),
  namespace: 'myapp.main',
  modules: ['myapp.utils', 'myapp.data'],
  timeout: 30000,
  sandboxed: true,
  allowedFFI: ['world.read', 'llm.complete'],
});
```

---

## API Reference

### Runtime Class

```typescript
class Runtime {
  constructor(config?: RuntimeConfig);

  // Evaluation
  eval(code: string): Promise<Value>;
  evalFile(path: string): Promise<Value>;

  // Environment access
  define(name: string, value: Value): void;
  lookup(name: string): Value;
  call(name: string, ...args: Value[]): Promise<Value>;

  // Namespace management
  inNamespace(name: string): void;
  require(name: string): Promise<Namespace>;

  // State management
  snapshot(): Snapshot;
  restore(snapshot: Snapshot): void;
  reset(): void;

  // Image operations
  saveImage(path: string): Promise<void>;
  loadImage(path: string): Promise<void>;

  // Lifecycle
  dispose(): void;
}
```

### Value Conversion

```typescript
// TypeScript → Lisp
function toLisp(value: any): Value {
  if (value === null || value === undefined) return null;
  if (typeof value === 'number') return value;
  if (typeof value === 'string') return value;
  if (typeof value === 'boolean') return value;
  if (Array.isArray(value)) return value.map(toLisp);
  if (typeof value === 'object') {
    // Convert to Lisp map
    const pairs: Value[] = [];
    for (const [k, v] of Object.entries(value)) {
      pairs.push(Symbol.for(`:${k}`), toLisp(v));
    }
    return ['hash-map', ...pairs];
  }
  throw new Error(`Cannot convert ${typeof value} to Lisp`);
}

// Lisp → TypeScript
function fromLisp(value: Value): any {
  if (value === null) return null;
  if (typeof value === 'number') return value;
  if (typeof value === 'string') return value;
  if (typeof value === 'boolean') return value;
  if (typeof value === 'symbol') return Symbol.keyFor(value);
  if (Array.isArray(value)) return value.map(fromLisp);
  // Handle Lisp maps
  if (isMap(value)) {
    const obj: any = {};
    for (const [k, v] of mapEntries(value)) {
      obj[fromLisp(k)] = fromLisp(v);
    }
    return obj;
  }
  return value;
}
```

---

## Extending with FFI

### Registering Functions

```typescript
const runtime = new Runtime();

// Simple function
runtime.ffi.register('my.add', (a: number, b: number) => a + b);

// Async function
runtime.ffi.register('my.fetch', async (url: string) => {
  const response = await fetch(url);
  return response.text();
});

// With condition handling
runtime.ffi.register('my.read-file', (path: string, cont: Continuation) => {
  try {
    return fs.readFileSync(path, 'utf-8');
  } catch (e) {
    return cont.signal({
      type: Symbol.for('file-error'),
      message: e.message,
      data: { path },
      restarts: [
        { name: Symbol.for('use-default'), handler: (v) => v },
        { name: Symbol.for('retry'), handler: (p) => fs.readFileSync(p, 'utf-8') },
      ]
    });
  }
});

// Usage in Lisp
await runtime.eval('(my.add 1 2)');  // 3
await runtime.eval('(await (my.fetch "https://api.example.com"))');
```

### Custom Types

```typescript
// Register custom type
class Point {
  constructor(public x: number, public y: number) {}
}

runtime.types.register('Point', {
  // How to recognize
  predicate: (v) => v instanceof Point,

  // How to print
  print: (p: Point) => `#<Point ${p.x},${p.y}>`,

  // How to serialize
  serialize: (p: Point) => ({ type: 'Point', x: p.x, y: p.y }),

  // How to deserialize
  deserialize: (data) => new Point(data.x, data.y),
});

// Usage
runtime.define('origin', new Point(0, 0));
await runtime.eval('(point-x origin)');  // 0
```

---

## Event Hooks

```typescript
const runtime = new Runtime();

// Before evaluation
runtime.on('beforeEval', (event) => {
  console.log('Evaluating:', event.code);
});

// After evaluation
runtime.on('afterEval', (event) => {
  console.log('Result:', event.result);
});

// On error
runtime.on('error', (event) => {
  console.error('Error:', event.error);
});

// On FFI call
runtime.on('ffiCall', (event) => {
  console.log(`FFI: ${event.name}(${event.args.join(', ')})`);
});

// On condition
runtime.on('condition', (event) => {
  console.log('Condition:', event.condition.message);
});

// On snapshot
runtime.on('snapshot', (event) => {
  console.log('Snapshot taken:', event.snapshot.id);
});
```

---

## Multiple Runtimes

```typescript
// Create isolated runtimes
const runtime1 = new Runtime({ namespace: 'app1' });
const runtime2 = new Runtime({ namespace: 'app2' });

// Each has independent state
await runtime1.eval('(define x 10)');
await runtime2.eval('(define x 20)');

console.log(await runtime1.eval('x'));  // 10
console.log(await runtime2.eval('x'));  // 20

// Share world if needed
const sharedWorld = new FileWorld('./shared');
const runtime3 = new Runtime({ world: sharedWorld });
const runtime4 = new Runtime({ world: sharedWorld });
```

---

## React Integration Example

```tsx
import { Runtime, InMemoryWorld } from 'lambdallm';
import { useState, useEffect, useCallback } from 'react';

function useLambdaLLM() {
  const [runtime, setRuntime] = useState<Runtime | null>(null);

  useEffect(() => {
    const rt = new Runtime({
      world: new InMemoryWorld({
        'config.json': '{"theme": "dark"}'
      })
    });
    setRuntime(rt);
    return () => rt.dispose();
  }, []);

  const evaluate = useCallback(async (code: string) => {
    if (!runtime) throw new Error('Runtime not initialized');
    return runtime.eval(code);
  }, [runtime]);

  return { runtime, evaluate };
}

function App() {
  const { evaluate } = useLambdaLLM();
  const [result, setResult] = useState<string>('');

  const handleEval = async () => {
    const value = await evaluate('(+ 1 2)');
    setResult(String(value));
  };

  return (
    <div>
      <button onClick={handleEval}>Evaluate (+ 1 2)</button>
      <div>Result: {result}</div>
    </div>
  );
}
```

---

## Bundle Size

| Bundle | Size (gzip) |
|--------|-------------|
| Core (eval only) | ~15 KB |
| + stdlib | ~25 KB |
| + LLM adapters | ~35 KB |
| + debugger | ~50 KB |
| Full bundle | ~80 KB |

### Tree Shaking

```typescript
// Import only what you need
import { Runtime, InMemoryWorld } from 'lambdallm/core';
import { OpenAIAdapter } from 'lambdallm/llm/openai';
```
