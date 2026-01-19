# 13: Memory Model

## Overview

LambdaLLM runs on JavaScript engines (V8, SpiderMonkey, JSC), so memory management is handled by the host garbage collector. This document describes:
- Value representation
- Object lifecycle
- Memory considerations for time-travel

---

## Value Representation

### Immediate Values

Small values stored directly (no allocation):

| Type | Representation |
|------|----------------|
| Small integers | SMI (tagged pointer) |
| Booleans | `true` / `false` |
| Null | `null` |
| Undefined | `undefined` |

### Heap Objects

Larger values allocated on heap:

| Type | Structure |
|------|-----------|
| String | V8 String (interned when possible) |
| Symbol | JS Symbol (always interned) |
| List | JS Array |
| Closure | Object with params, body, env |
| Continuation | Object with frames array |

---

## Object Layout

### Closure

```typescript
interface Closure {
  type: 'closure';
  params: string[];           // 1 array
  body: Value;                // 1 reference to AST
  envSnapshot: EnvSnapshot;   // Serializable env
  source?: string;            // Optional source text
  sourceLoc?: SourceLoc;      // Optional location
}

// Memory: ~100-500 bytes depending on body/env size
```

### Environment Frame

```typescript
interface Frame {
  id: string;                 // ~36 bytes (UUID)
  bindings: Map<string, Value>; // Variable size
  parent: Frame | null;       // 8 bytes (pointer)
}

// Memory: ~100 bytes + bindings
```

### Continuation

```typescript
interface Continuation {
  frames: Frame[];            // Variable size
  currentFrameId: string;     // 36 bytes
}

// Memory: ~50 bytes + frames * 100 bytes
```

---

## Garbage Collection

### What Gets Collected

- Closures no longer reachable
- Environment frames no longer reachable
- Intermediate values from evaluation
- Old continuations (unless captured)

### What Stays Alive

- Global namespace bindings
- Captured continuations (time-travel)
- Cached artifacts
- Snapshot store entries

### GC Considerations for Time-Travel

Time-travel stores snapshots, which keep references alive:

```
Problem: 1000 snapshots → memory grows
Solution: Incremental snapshots + LRU eviction
```

```typescript
class SnapshotStore {
  private maxSnapshots = 100;
  private snapshots: Map<string, Snapshot> = new Map();

  save(snapshot: Snapshot): void {
    if (this.snapshots.size >= this.maxSnapshots) {
      // Evict oldest (LRU)
      const oldest = this.findOldest();
      this.snapshots.delete(oldest);
    }
    this.snapshots.set(snapshot.id, snapshot);
  }
}
```

---

## Memory Limits

### Default Limits

| Environment | Default Heap |
|-------------|--------------|
| Node.js | ~1.5GB (can increase) |
| Browser | Tab-specific, ~2-4GB |
| Bun | ~4GB |
| Deno | ~4GB |

### Increasing Limits

```bash
# Node.js
node --max-old-space-size=4096 lambdallm.js

# Bun
BUN_JSC_heapSize=4096 bun lambdallm.ts
```

---

## String Interning

Symbols are interned (only one copy per name):

```typescript
// Uses JavaScript's built-in symbol interning
Symbol.for('foo') === Symbol.for('foo')  // true

// Same symbol, no extra memory
const s1 = sym('my-long-symbol-name');
const s2 = sym('my-long-symbol-name');
// s1 === s2, single allocation
```

### Benefits

- Symbol comparison is O(1)
- Reduced memory for repeated symbols
- Better cache locality

---

## WeakMap for Metadata

Attach metadata without preventing GC:

```typescript
// Metadata that doesn't prevent GC
const sourceLocations = new WeakMap<Value, SourceLoc>();
const macroExpansions = new WeakMap<Value, Value>();

// When value is GC'd, metadata is automatically removed
```

---

## Memory Profiling

### Built-in Commands

```lisp
(memory-stats)
;; → {:heap-used 50000000
;;    :heap-total 100000000
;;    :external 1000000
;;    :snapshots 42
;;    :namespaces 5}

(gc)  ; Force garbage collection (if available)
```

### Heap Snapshots

```bash
# Node.js
node --inspect lambdallm.js
# Then use Chrome DevTools

# Bun
bun --inspect lambdallm.ts
```

---

## Avoiding Memory Leaks

### Common Pitfalls

1. **Unbounded snapshot growth**
   - Solution: LRU eviction

2. **Closure over large data**
   ```lisp
   ;; BAD: closure captures entire 'big-data'
   (let ((big-data (load-huge-file)))
     (lambda () (first big-data)))

   ;; GOOD: capture only what's needed
   (let ((first-item (first (load-huge-file))))
     (lambda () first-item))
   ```

3. **Global bindings accumulation**
   - Solution: Use namespaces, clear unused

4. **Event handler accumulation**
   - Solution: Remove handlers when done

### Best Practices

```lisp
;; Clear large values when done
(define big-data nil)

;; Use with-resource patterns
(with-open-file (f "huge.txt")
  (process-line-by-line f))
;; File handle released automatically

;; Limit recursion depth
(define *max-recursion* 1000)
```

---

## Browser-Specific Concerns

### IndexedDB Storage

For persistence in browser:

```typescript
// Store snapshots in IndexedDB, not memory
class BrowserSnapshotStore {
  private memoryCache: LRUCache<string, Snapshot>;
  private db: IDBDatabase;

  async get(id: string): Promise<Snapshot> {
    // Check memory cache first
    if (this.memoryCache.has(id)) {
      return this.memoryCache.get(id)!;
    }
    // Load from IndexedDB
    const snapshot = await this.loadFromDB(id);
    this.memoryCache.set(id, snapshot);
    return snapshot;
  }
}
```

### Web Worker Memory

Each Web Worker has separate heap:

```
Main Thread Heap  |  Worker 1 Heap  |  Worker 2 Heap
      2GB         |      2GB        |      2GB
```

Use workers for memory-intensive operations.

---

## Summary

| Concern | Solution |
|---------|----------|
| GC pauses | JS GC is generational, usually fast |
| Memory growth | LRU eviction for snapshots |
| Large closures | Capture minimal data |
| Symbol memory | Built-in interning |
| Browser limits | IndexedDB + workers |
| Leak detection | Heap snapshots + profiling |
