# 03: Environment (Namespaces & Bindings)

## What Is Environment?

The Environment is the **lexical scope** - a mapping from names to values that tells the interpreter what each variable means.

```lisp
(let ((x 10))           ; x = 10 in this scope
  (let ((y 20))         ; y = 20 in nested scope
    (+ x y)))           ; Environment: {y: 20} → {x: 10} → {globals}
```

## Key Concepts

### 1. Lexical Scope (Static Scoping)

Variables are resolved based on WHERE they appear in the source code, not when they execute.

```lisp
(define (make-adder n)
  (lambda (x) (+ x n)))   ; n is captured from enclosing scope

(define add5 (make-adder 5))
(add5 10)  ; → 15, because n=5 was captured when lambda was created
```

### 2. Environment as Chain of Frames

```
┌─────────────────────────────────────────────────────────────────┐
│  Environment Structure                                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Current Frame ──▶ Parent Frame ──▶ ... ──▶ Global Frame       │
│                                                                 │
│  Lookup: Start at current, walk up until found                 │
│  Define: Add to current frame                                   │
│  Set!:   Find frame containing binding, modify it              │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 3. Closures Capture Environment

When a lambda is created, it captures its environment (not just variable values):

```lisp
(define (counter start)
  (let ((n start))
    (lambda ()
      (set! n (+ n 1))
      n)))

(define c (counter 0))
(c)  ; → 1
(c)  ; → 2
(c)  ; → 3
```

The lambda captures the frame containing `n`, so mutations persist.

## TypeScript Design

```typescript
// types.ts
interface Frame {
  id: string;
  bindings: Map<string, Value>;
  parent: Frame | null;

  // For debugging/serialization
  sourceLocation?: SourceLoc;
  name?: string;  // "let", "lambda", "define", etc.
}

// env.ts
class Environment {
  private current: Frame;

  constructor(parent?: Environment) {
    this.current = {
      id: generateId(),
      bindings: new Map(),
      parent: parent?.current ?? null,
    };
  }

  // Lookup: walk up chain
  lookup(name: string): Value {
    let frame: Frame | null = this.current;
    while (frame !== null) {
      if (frame.bindings.has(name)) {
        return frame.bindings.get(name)!;
      }
      frame = frame.parent;
    }
    throw new UnboundVariableError(name);
  }

  // Define: add to current frame
  define(name: string, value: Value): void {
    this.current.bindings.set(name, value);
  }

  // Set!: modify existing binding
  set(name: string, value: Value): void {
    let frame: Frame | null = this.current;
    while (frame !== null) {
      if (frame.bindings.has(name)) {
        frame.bindings.set(name, value);
        return;
      }
      frame = frame.parent;
    }
    throw new UnboundVariableError(name);
  }

  // Extend: create child environment
  extend(params: string[], args: Value[]): Environment {
    const child = new Environment(this);
    for (let i = 0; i < params.length; i++) {
      child.define(params[i], args[i]);
    }
    return child;
  }

  // For serialization
  toJSON(): object {
    const frames: object[] = [];
    let frame: Frame | null = this.current;
    while (frame !== null) {
      frames.push({
        id: frame.id,
        bindings: Object.fromEntries(frame.bindings),
        parentId: frame.parent?.id ?? null,
      });
      frame = frame.parent;
    }
    return { frames, currentId: this.current.id };
  }
}
```

## Namespaces (Beyond Basic Scoping)

Real Lisps have **namespaces** - named containers for related definitions:

```clojure
;; Clojure-style namespaces
(ns myapp.core
  (:require [myapp.utils :as u]))

(defn greet [name]
  (u/format-string "Hello, ~a!" name))
```

### Namespace Design

```typescript
interface Namespace {
  name: string;
  bindings: Map<string, Value>;
  imports: Map<string, Namespace>;  // aliased imports
  exports: Set<string>;             // what's public
}

class NamespaceRegistry {
  private namespaces: Map<string, Namespace> = new Map();
  private current: Namespace;

  // Switch to namespace (create if needed)
  inNamespace(name: string): void {
    if (!this.namespaces.has(name)) {
      this.namespaces.set(name, {
        name,
        bindings: new Map(),
        imports: new Map(),
        exports: new Set(),
      });
    }
    this.current = this.namespaces.get(name)!;
  }

  // Import another namespace with alias
  require(nsName: string, alias: string): void {
    const ns = this.namespaces.get(nsName);
    if (!ns) throw new Error(`Unknown namespace: ${nsName}`);
    this.current.imports.set(alias, ns);
  }

  // Lookup with namespace resolution
  resolve(symbol: string): Value {
    // Check for qualified name: "alias/name"
    if (symbol.includes('/')) {
      const [alias, name] = symbol.split('/');
      const ns = this.current.imports.get(alias);
      if (!ns) throw new Error(`Unknown alias: ${alias}`);
      if (!ns.exports.has(name)) throw new Error(`${name} not exported from ${ns.name}`);
      return ns.bindings.get(name)!;
    }

    // Unqualified: check current namespace
    if (this.current.bindings.has(symbol)) {
      return this.current.bindings.get(symbol)!;
    }

    throw new UnboundVariableError(symbol);
  }
}
```

## Facts (LambdaRLM Extension)

LambdaRLM adds **facts** to the environment - monotonic assertions that can only be added, never removed:

```lisp
(assert-if-absent '(file-analyzed "main.py"))
(fact? '(file-analyzed "main.py"))  ; → #t
(facts)  ; → ((file-analyzed "main.py"))
```

### Facts Design

```typescript
interface EnvironmentWithFacts extends Environment {
  facts: Set<string>;  // Serialized fact representations
  factEvidence: Map<string, Evidence>;  // Why each fact was asserted

  assertFact(fact: Value, evidence?: Evidence): boolean;
  hasFact(fact: Value): boolean;
  getAllFacts(): Value[];
}
```

Facts are useful for:
1. **Fixpoint detection**: Know when computation has stabilized
2. **Reasoning**: Track what the agent has learned
3. **Debugging**: See what conclusions were reached

## Serialization for Time-Travel

To support snapshots, the environment must be serializable:

```typescript
interface SerializedEnvironment {
  frames: SerializedFrame[];
  currentFrameId: string;
  facts: string[];
  factEvidence: Record<string, Evidence>;
}

interface SerializedFrame {
  id: string;
  parentId: string | null;
  bindings: Record<string, SerializedValue>;
  sourceLocation?: SourceLoc;
}

// Special handling for closures
interface SerializedClosure {
  type: 'closure';
  params: string[];
  body: SerializedExpr;  // The source AST
  envId: string;         // Reference to captured environment
}
```

The key insight: **closures store their source**, not just a function pointer. This makes them serializable.

## Comparison to Python LambdaRLM

| Feature | Python (env.py) | TypeScript (env.ts) |
|---------|-----------------|---------------------|
| Frame chain | ✅ Has it | ✅ Same |
| Lookup | ✅ Has it | ✅ Same |
| Define/Set | ✅ Has it | ✅ Same |
| Facts | ✅ Has it | ✅ Same |
| Namespaces | ❌ Missing | ✅ Added |
| Serialization | ⚠️ Partial | ✅ Full |
| Closure capture | ⚠️ Live reference | ✅ Snapshot + source |

## Key Differences from Standard Lisp

1. **Facts**: LambdaRLM adds monotonic fact storage
2. **Evidence**: Facts track WHY they were asserted
3. **Serialization**: Environment designed for snapshots
4. **Immutable option**: Can create "frozen" snapshots for time-travel
