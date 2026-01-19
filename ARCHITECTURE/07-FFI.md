# 07: FFI and World (External Effects)

## The Big Picture

```
┌─────────────────────────────────────────────────────────────────┐
│                                                                 │
│  PURE INTERPRETER          │         IMPURE WORLD              │
│  (deterministic)           │         (side effects)            │
│                            │                                    │
│  ┌──────────────────┐      │      ┌──────────────────┐         │
│  │                  │      │      │                  │         │
│  │  Environment     │      │      │  File System     │         │
│  │  (bindings)      │      │      │  Network/LLM     │         │
│  │                  │ FFI  │      │  Database        │         │
│  │  Evaluator       │◀────▶│      │  Clock/Random    │         │
│  │  (computation)   │      │      │                  │         │
│  │                  │      │      │  = "The World"   │         │
│  └──────────────────┘      │      └──────────────────┘         │
│                            │                                    │
│  Same input → Same output  │  Same input → Different output    │
│                            │                                    │
└─────────────────────────────────────────────────────────────────┘
```

## What Is World?

**World** is the external universe that the program can interact with. It includes anything that:

1. Can change between calls (non-deterministic)
2. Has side effects (changes external state)
3. Communicates outside the program

### Examples of World Operations

```lisp
;; File system
(world.read "config.json")           ; Read file
(world.write "output.txt" content)   ; Write file
(world.list "src/*.ts")              ; List files

;; Network / LLM
(llm.complete "What is 2+2?")        ; Call LLM API
(http.get "https://api.example.com") ; HTTP request

;; Time
(world.now)                          ; Current timestamp
(world.sleep 1000)                   ; Wait

;; Random
(world.random)                       ; Random number
```

## Why Separate World from Environment?

| Aspect | Environment | World |
|--------|-------------|-------|
| Contains | Variable bindings | External resources |
| Mutability | Lexical (controlled) | Global (uncontrolled) |
| Determinism | Same lookup → same value | Same call → different result |
| Scope | Per-expression | Global/session |
| Serialization | Easy (it's data) | Hard (external state) |

### Benefits of Separation

1. **Testing**: Use `InMemoryWorld` for deterministic tests
2. **Browser**: Use `VirtualWorld` with IndexedDB
3. **Sandboxing**: Restrict what `World` operations are allowed
4. **Replay**: Log all `World` operations, replay later
5. **Proposals**: Buffer writes in `StagedWorld`

## FFI: Foreign Function Interface

FFI is how the pure interpreter calls into World:

```typescript
// The interpreter only knows about FFI, not specific implementations
interface FFI {
  call(name: string, args: Value[]): Value | Promise<Value>;
  isAsync(name: string): boolean;
}

// FFI registration
const ffi = new FFIRegistry();

// Register World operations
ffi.register('world.read', async (path: string) => {
  return world.read(path);
});

ffi.register('world.write', async (path: string, content: string) => {
  return world.write(path, content);
});

// Register LLM operations
ffi.register('llm.complete', async (prompt: string, system?: string) => {
  return llmAdapter.complete(prompt, system);
});
```

### FFI in Evaluator

```typescript
// eval.ts - pure interpreter
function evalExpr(expr: Expr, env: Environment, ffi: FFI): Value {
  // ... pure special forms ...

  // Function call
  if (isList(expr)) {
    const op = first(expr);
    const args = rest(expr).map(a => evalExpr(a, env, ffi));

    // Check if it's an FFI call
    if (isSymbol(op) && ffi.has(op.name)) {
      return ffi.call(op.name, args);  // Delegate to FFI
    }

    // Otherwise, apply as closure
    const proc = evalExpr(op, env, ffi);
    return apply(proc, args, ffi);
  }
}
```

The evaluator has **no imports** for world, LLM, etc. It only knows about FFI.

## World Interface

```typescript
// world/interface.ts
interface World {
  // Identity
  fingerprint(): string;  // Hash of current state (for change detection)

  // Read operations
  read(path: string): string;
  list(pattern: string): string[];
  exists(path: string): boolean;
  stat(path: string): FileStat;

  // Write operations
  write(path: string, content: string): void;
  delete(path: string): void;
  mkdir(path: string): void;

  // Search
  search(pattern: string, query: string): SearchResult[];

  // Execute (optional, for shell commands)
  run?(command: string, args: string[]): RunResult;
}
```

## World Implementations

### 1. InMemoryWorld (Testing)

```typescript
class InMemoryWorld implements World {
  private files: Map<string, string> = new Map();

  read(path: string): string {
    if (!this.files.has(path)) {
      throw new FileNotFoundError(path);
    }
    return this.files.get(path)!;
  }

  write(path: string, content: string): void {
    this.files.set(path, content);
  }

  fingerprint(): string {
    // Hash of all file contents
    return hash(JSON.stringify(Object.fromEntries(this.files)));
  }
}
```

### 2. FileWorld (Node.js CLI)

```typescript
class FileWorld implements World {
  constructor(private root: string) {}

  read(path: string): string {
    const fullPath = join(this.root, path);
    return fs.readFileSync(fullPath, 'utf-8');
  }

  write(path: string, content: string): void {
    const fullPath = join(this.root, path);
    fs.writeFileSync(fullPath, content, 'utf-8');
  }
}
```

### 3. VirtualWorld (Browser)

```typescript
class VirtualWorld implements World {
  private db: IDBDatabase;  // IndexedDB

  async read(path: string): Promise<string> {
    const tx = this.db.transaction('files', 'readonly');
    const store = tx.objectStore('files');
    const result = await store.get(path);
    if (!result) throw new FileNotFoundError(path);
    return result.content;
  }
}
```

### 4. StagedWorld (Proposals)

```typescript
class StagedWorld implements World {
  private base: World;
  private staged: Map<string, string> = new Map();
  private deleted: Set<string> = new Set();

  constructor(base: World) {
    this.base = base;
  }

  read(path: string): string {
    // Check staged first
    if (this.deleted.has(path)) {
      throw new FileNotFoundError(path);
    }
    if (this.staged.has(path)) {
      return this.staged.get(path)!;
    }
    return this.base.read(path);
  }

  write(path: string, content: string): void {
    // Don't write to base, just stage
    this.staged.set(path, content);
    this.deleted.delete(path);
  }

  // Get all staged changes
  getChangeset(): Change[] {
    const changes: Change[] = [];
    for (const [path, content] of this.staged) {
      changes.push({ type: 'write', path, content });
    }
    for (const path of this.deleted) {
      changes.push({ type: 'delete', path });
    }
    return changes;
  }

  // Apply staged changes to base
  commit(): void {
    for (const [path, content] of this.staged) {
      this.base.write(path, content);
    }
    for (const path of this.deleted) {
      this.base.delete(path);
    }
    this.staged.clear();
    this.deleted.clear();
  }
}
```

### 5. ReplayWorld (Time-Travel)

```typescript
class ReplayWorld implements World {
  private log: WorldOperation[] = [];
  private base: World;

  read(path: string): string {
    const result = this.base.read(path);
    this.log.push({ op: 'read', path, result });
    return result;
  }

  write(path: string, content: string): void {
    this.log.push({ op: 'write', path, content });
    this.base.write(path, content);
  }

  // Get operation log for replay
  getLog(): WorldOperation[] {
    return [...this.log];
  }

  // Replay from log (for debugging)
  static fromLog(log: WorldOperation[]): MockWorld {
    const mock = new MockWorld();
    for (const op of log) {
      if (op.op === 'read') {
        mock.setReadResult(op.path, op.result);
      }
    }
    return mock;
  }
}
```

## LLM as World Operation

LLM calls are also World operations (they're non-deterministic external calls):

```typescript
// Register LLM operations in FFI
ffi.register('llm.complete', async (prompt: string, system?: string) => {
  return llmAdapter.complete(prompt, system);
});

ffi.register('llm.eval', async (expr: Expr) => {
  const code = await llmAdapter.compileIntent(exprToString(expr));
  return evalExpr(code, env, ffi);
});

// Different adapters for different LLMs
interface LLMAdapter {
  complete(prompt: string, system?: string): Promise<string>;
  compileIntent(text: string): Promise<Expr>;
}

class OpenAIAdapter implements LLMAdapter { ... }
class AnthropicAdapter implements LLMAdapter { ... }
class MockLLMAdapter implements LLMAdapter { ... }
```

## Condition System Integration

World operations can signal conditions instead of throwing:

```typescript
ffi.register('world.read', (path: string, cont: Continuation) => {
  try {
    return world.read(path);
  } catch (e) {
    if (e instanceof FileNotFoundError) {
      // Signal condition with restarts
      return cont.signal({
        type: 'file-not-found',
        message: `File not found: ${path}`,
        data: { path },
        restarts: [
          { name: 'use-value', handler: (v) => v },
          { name: 'retry', handler: (p) => world.read(p) },
          { name: 'create-empty', handler: () => { world.write(path, ''); return ''; } },
        ]
      });
    }
    throw e;
  }
});
```

In Lisp:

```lisp
;; Handle missing file by creating it
(handler-bind
  ((file-not-found
    (lambda (c)
      (invoke-restart 'create-empty))))
  (world.read "config.json"))
```

## Summary

| Concept | Purpose | Examples |
|---------|---------|----------|
| **Environment** | Lexical scope, bindings | `(let ((x 5)) ...)` |
| **World** | External mutable state | File system, network |
| **FFI** | Bridge from pure to impure | `ffi.call('world.read', [path])` |
| **Condition** | Non-unwinding errors | `(signal 'file-not-found)` |

The key insight: **The interpreter is pure. All side effects go through FFI to World.** This makes:
- Testing trivial (mock World)
- Browser possible (virtual World)
- Replay possible (logged World)
- Sandboxing possible (restricted World)
