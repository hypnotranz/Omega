# ⚠️ PARTIALLY SUPERSEDED BY 32-6

> **THIS SPEC IS EXTENDED BY 32-6 WITH NEW VALUE TYPES**
>
> ## What's Still Valid
>
> The basic type hierarchy (Atom, Symbol, List, Procedure) is still correct.
> These are the foundational Lisp types that don't change.
>
> ## What 32-6 Adds
>
> The 32-series adds ~10 new value types for the dual-plane semantics:
>
> ```typescript
> // THIS SPEC (basic Lisp types):
> type Value = Atom | Symbol | List | Procedure
>
> // 32-6 ADDS:
> type Value = ... |
>   Meaning |      // Semantic interpretation with denotation, residual, confidence
>   Dist |         // Probability distributions
>   Engine |       // LLM adapter handle
>   Prompt |       // Structured LLM prompt
>   Policy |       // Governance policy
>   Ctx |          // Context-as-value (sealing)
>   Receipt |      // Proof of computation
>   Obligation |   // Deferred verification
>   Stream |       // Lazy sequences (SICP)
>   // ...
> ```
>
> ## Recommendation
>
> Read this spec for foundational types, then see 32-6 for the extended type system
> that supports inference, governance, and SICP primitives.
>
> ## References
> - [32-6 CEKS Machine - Value Types](32-6-CEKS.md)
> - [ARCHITECTURE-REDESIGN-ASSESSMENT.md](../docs/ARCHITECTURE-REDESIGN-ASSESSMENT.md)

---

# 02: Types (Core Data Structures)

## The Type Hierarchy

```
┌─────────────────────────────────────────────────────────────────┐
│  LambdaLLM Type Hierarchy                                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Value (anything the interpreter can work with)                 │
│  ├── Atom (self-evaluating)                                     │
│  │   ├── Number    (42, 3.14)                                  │
│  │   ├── String    ("hello")                                   │
│  │   ├── Boolean   (#t, #f)                                    │
│  │   └── Nil       (nil, null)                                 │
│  │                                                              │
│  ├── Symbol        (foo, +, define)                            │
│  │                                                              │
│  ├── List          (cons cells or arrays)                       │
│  │   └── Pair      (a . b) - optional dotted pairs             │
│  │                                                              │
│  └── Procedure                                                  │
│      ├── Primitive     (built-in, TypeScript function)         │
│      ├── Closure       (user-defined lambda)                   │
│      ├── Macro         (compile-time transformation)           │
│      └── Continuation  (captured call stack)                   │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

## Key Design Goals

1. **Serializable**: Every value can be converted to JSON and back
2. **Inspectable**: Every value has a readable string representation
3. **Comparable**: Values can be compared for equality
4. **Portable**: No host-language-specific features

## TypeScript Implementation

```typescript
// types.ts

// ============================================================
// Atoms (self-evaluating values)
// ============================================================

type Atom = number | string | boolean | null;

// ============================================================
// Symbols
// ============================================================

// Use JavaScript's Symbol.for() for interning
// This gives us: Symbol.for('foo') === Symbol.for('foo')
type LispSymbol = symbol;

function sym(name: string): LispSymbol {
  return Symbol.for(name);
}

function symbolName(s: LispSymbol): string {
  return Symbol.keyFor(s) ?? s.toString();
}

// ============================================================
// Lists
// ============================================================

// We use arrays for lists (efficient, serializable)
type List = Value[];

function isList(v: Value): v is List {
  return Array.isArray(v);
}

function cons(head: Value, tail: List): List {
  return [head, ...tail];
}

function car(list: List): Value {
  if (list.length === 0) throw new TypeError('car of empty list');
  return list[0];
}

function cdr(list: List): List {
  if (list.length === 0) throw new TypeError('cdr of empty list');
  return list.slice(1);
}

// ============================================================
// Procedures
// ============================================================

// Base procedure interface
interface Procedure {
  type: 'primitive' | 'closure' | 'macro' | 'continuation';
  name?: string;
  arity?: number | { min: number; max?: number };
}

// Built-in primitive
interface Primitive extends Procedure {
  type: 'primitive';
  fn: (...args: Value[]) => Value;
}

// User-defined closure (SERIALIZABLE!)
interface Closure extends Procedure {
  type: 'closure';
  params: string[];          // Parameter names
  body: Value;               // Body expression (the AST)
  envSnapshot: EnvSnapshot;  // Captured environment (serializable)

  // For debugging/serialization
  source?: string;           // Original source text
  sourceLoc?: SourceLoc;     // Where it was defined
}

// Macro (compile-time transformation)
interface Macro extends Procedure {
  type: 'macro';
  params: string[];
  body: Value;
  envSnapshot: EnvSnapshot;
}

// First-class continuation
interface ContinuationProc extends Procedure {
  type: 'continuation';
  frames: Frame[];           // Captured stack
  envSnapshot: EnvSnapshot;  // Environment at capture point
}

// ============================================================
// The Value union type
// ============================================================

type Value =
  | Atom
  | LispSymbol
  | List
  | Primitive
  | Closure
  | Macro
  | ContinuationProc;

// ============================================================
// Type predicates
// ============================================================

function isAtom(v: Value): v is Atom {
  return v === null || typeof v === 'number' || typeof v === 'string' || typeof v === 'boolean';
}

function isSymbol(v: Value): v is LispSymbol {
  return typeof v === 'symbol';
}

function isProcedure(v: Value): v is Procedure {
  return v !== null && typeof v === 'object' && 'type' in v &&
    ['primitive', 'closure', 'macro', 'continuation'].includes((v as any).type);
}

function isClosure(v: Value): v is Closure {
  return isProcedure(v) && v.type === 'closure';
}

function isMacro(v: Value): v is Macro {
  return isProcedure(v) && v.type === 'macro';
}

function isContinuation(v: Value): v is ContinuationProc {
  return isProcedure(v) && v.type === 'continuation';
}

// ============================================================
// Equality
// ============================================================

function equal(a: Value, b: Value): boolean {
  // Same reference
  if (a === b) return true;

  // Atoms
  if (isAtom(a) && isAtom(b)) return a === b;

  // Symbols
  if (isSymbol(a) && isSymbol(b)) return a === b;

  // Lists (deep equality)
  if (isList(a) && isList(b)) {
    if (a.length !== b.length) return false;
    return a.every((elem, i) => equal(elem, b[i]));
  }

  // Procedures (reference equality only)
  return false;
}

// ============================================================
// Serialization (critical for time-travel!)
// ============================================================

interface SerializedValue {
  type: 'atom' | 'symbol' | 'list' | 'closure' | 'macro' | 'continuation' | 'primitive-ref';
  value?: any;
}

function serialize(v: Value): SerializedValue {
  if (v === null) return { type: 'atom', value: null };
  if (typeof v === 'number') return { type: 'atom', value: v };
  if (typeof v === 'string') return { type: 'atom', value: v };
  if (typeof v === 'boolean') return { type: 'atom', value: v };

  if (isSymbol(v)) {
    return { type: 'symbol', value: symbolName(v) };
  }

  if (isList(v)) {
    return { type: 'list', value: v.map(serialize) };
  }

  if (isClosure(v)) {
    return {
      type: 'closure',
      value: {
        params: v.params,
        body: serialize(v.body),
        envSnapshot: v.envSnapshot,  // Already serializable
        source: v.source,
        sourceLoc: v.sourceLoc,
      }
    };
  }

  if (isMacro(v)) {
    return {
      type: 'macro',
      value: {
        params: v.params,
        body: serialize(v.body),
        envSnapshot: v.envSnapshot,
      }
    };
  }

  if (isContinuation(v)) {
    return {
      type: 'continuation',
      value: {
        frames: v.frames,
        envSnapshot: v.envSnapshot,
      }
    };
  }

  // Primitives can't be fully serialized, but we can reference them by name
  if (isProcedure(v) && v.type === 'primitive') {
    return { type: 'primitive-ref', value: v.name };
  }

  throw new Error(`Cannot serialize: ${v}`);
}

function deserialize(s: SerializedValue, primitives: Map<string, Primitive>): Value {
  switch (s.type) {
    case 'atom':
      return s.value;

    case 'symbol':
      return sym(s.value);

    case 'list':
      return s.value.map((elem: SerializedValue) => deserialize(elem, primitives));

    case 'closure':
      return {
        type: 'closure',
        params: s.value.params,
        body: deserialize(s.value.body, primitives),
        envSnapshot: s.value.envSnapshot,
        source: s.value.source,
        sourceLoc: s.value.sourceLoc,
      };

    case 'macro':
      return {
        type: 'macro',
        params: s.value.params,
        body: deserialize(s.value.body, primitives),
        envSnapshot: s.value.envSnapshot,
      };

    case 'continuation':
      return {
        type: 'continuation',
        frames: s.value.frames,
        envSnapshot: s.value.envSnapshot,
      };

    case 'primitive-ref':
      const prim = primitives.get(s.value);
      if (!prim) throw new Error(`Unknown primitive: ${s.value}`);
      return prim;
  }
}
```

## Why Closures Store Body AST

The key to serialization is that closures store their **body as data** (the AST), not as a function:

```typescript
// WRONG: Not serializable
interface BadClosure {
  fn: Function;  // Can't serialize JavaScript functions!
}

// RIGHT: Serializable
interface GoodClosure {
  params: string[];
  body: Value;           // The AST - it's just data!
  envSnapshot: EnvSnapshot;
}
```

When we apply a closure:
1. Create new environment extending captured env
2. Bind parameters to arguments
3. Evaluate body in new environment

```typescript
function applyClosure(closure: Closure, args: Value[], ffi: FFI): Value {
  // Reconstruct environment from snapshot
  const env = Environment.fromSnapshot(closure.envSnapshot);

  // Extend with parameters
  const extended = env.extend(closure.params, args);

  // Evaluate body
  return evalExpr(closure.body, extended, ffi);
}
```

## Environment Snapshots

```typescript
// Serializable environment snapshot
interface EnvSnapshot {
  frames: FrameSnapshot[];
  currentFrameId: string;
}

interface FrameSnapshot {
  id: string;
  parentId: string | null;
  bindings: Record<string, SerializedValue>;
}

// Creating a snapshot
function snapshotEnv(env: Environment): EnvSnapshot {
  const frames: FrameSnapshot[] = [];
  let frame = env.currentFrame;

  while (frame) {
    frames.push({
      id: frame.id,
      parentId: frame.parent?.id ?? null,
      bindings: Object.fromEntries(
        Array.from(frame.bindings.entries()).map(([k, v]) => [k, serialize(v)])
      ),
    });
    frame = frame.parent;
  }

  return { frames, currentFrameId: env.currentFrame.id };
}
```

## Comparison to Python LambdaRLM

| Feature | Python | TypeScript |
|---------|--------|------------|
| Symbols | Custom `Symbol` class | JS `Symbol.for()` |
| Lists | Python lists | JS arrays |
| Closures | Store live env reference | Store env snapshot |
| Serialization | Partial (closures break) | Full |
| Continuations | None | First-class |
| Type safety | Runtime only | Static types |

## Benefits of This Design

1. **Time-travel**: Serialize any value, restore later
2. **Debugging**: Inspect closure's captured environment
3. **Persistence**: Save entire runtime state to disk
4. **Network**: Send closures over the wire
5. **Testing**: Compare values structurally
