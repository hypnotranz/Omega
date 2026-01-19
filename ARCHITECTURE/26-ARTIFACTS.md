# 26: Artifacts (Content-Addressed Memoization)

## The Problem

AI agent computations are expensive:
- World reads hit disk/network
- LLM calls cost money and time
- Same analysis repeated wastefully

**Artifacts solve this** with content-addressed caching that tracks dependencies.

---

## Core Concept

An artifact is a cached result identified by:
1. **Expression key** - What computation produced it
2. **Dependencies fingerprint** - State of inputs when computed
3. **Parameters** - Any additional inputs

```typescript
interface ArtifactKey {
  exprKey: CanonicalKey;     // Canonicalized AST
  depsFingerprint: string;   // Hash of dependency fingerprints
  paramsKey?: string;        // Additional parameters
}

interface Artifact {
  key: ArtifactKey;
  value: Value;              // Cached result
  metadata: {
    createdAt: number;
    accessedAt: number;
    accessCount: number;
    computeTimeMs: number;
    deps: DependencyRecord[];
  };
}

interface DependencyRecord {
  ref: string;               // File path or resource
  fingerprint: string;       // Content hash at cache time
}
```

---

## Artifact Store

```typescript
class ArtifactStore {
  private artifacts: Map<string, Artifact> = new Map();
  private byExpr: Map<CanonicalKey, Set<string>> = new Map();
  private capacity: number | null;

  constructor(options: { capacity?: number } = {}) {
    this.capacity = options.capacity ?? null;
  }

  // === Storage ===

  put(key: ArtifactKey, value: Value, metadata: Partial<Artifact['metadata']>): void {
    const keyStr = this.keyToString(key);

    // Evict if at capacity
    if (this.capacity !== null && this.artifacts.size >= this.capacity) {
      this.evictLRU();
    }

    const artifact: Artifact = {
      key,
      value,
      metadata: {
        createdAt: Date.now(),
        accessedAt: Date.now(),
        accessCount: 0,
        computeTimeMs: metadata.computeTimeMs ?? 0,
        deps: metadata.deps ?? [],
      },
    };

    this.artifacts.set(keyStr, artifact);

    // Secondary index by expression
    if (!this.byExpr.has(key.exprKey)) {
      this.byExpr.set(key.exprKey, new Set());
    }
    this.byExpr.get(key.exprKey)!.add(keyStr);
  }

  // === Retrieval ===

  // Direct lookup by full key
  get(key: ArtifactKey): Value | undefined {
    const keyStr = this.keyToString(key);
    const artifact = this.artifacts.get(keyStr);

    if (artifact) {
      // Update access metadata
      artifact.metadata.accessedAt = Date.now();
      artifact.metadata.accessCount++;
      return artifact.value;
    }

    return undefined;
  }

  // Lookup by expression, validate fingerprints
  getByExpr(exprKey: CanonicalKey, world: World): Value | undefined {
    const keyStrs = this.byExpr.get(exprKey);
    if (!keyStrs) return undefined;

    for (const keyStr of keyStrs) {
      const artifact = this.artifacts.get(keyStr);
      if (!artifact) continue;

      // Validate all dependencies still match
      const stillValid = artifact.metadata.deps.every(dep => {
        const currentFingerprint = world.fingerprint(dep.ref);
        return currentFingerprint === dep.fingerprint;
      });

      if (stillValid) {
        artifact.metadata.accessedAt = Date.now();
        artifact.metadata.accessCount++;
        return artifact.value;
      }
    }

    return undefined;
  }

  // === Eviction ===

  private evictLRU(): void {
    let oldest: { keyStr: string; accessedAt: number } | null = null;

    for (const [keyStr, artifact] of this.artifacts) {
      if (!oldest || artifact.metadata.accessedAt < oldest.accessedAt) {
        oldest = { keyStr, accessedAt: artifact.metadata.accessedAt };
      }
    }

    if (oldest) {
      this.remove(oldest.keyStr);
    }
  }

  private remove(keyStr: string): void {
    const artifact = this.artifacts.get(keyStr);
    if (artifact) {
      this.artifacts.delete(keyStr);
      const exprSet = this.byExpr.get(artifact.key.exprKey);
      exprSet?.delete(keyStr);
    }
  }

  // === Utilities ===

  private keyToString(key: ArtifactKey): string {
    return sha256(JSON.stringify([key.exprKey, key.depsFingerprint, key.paramsKey]));
  }

  stats(): ArtifactStats {
    return {
      count: this.artifacts.size,
      capacity: this.capacity,
      totalAccessCount: Array.from(this.artifacts.values())
        .reduce((sum, a) => sum + a.metadata.accessCount, 0),
    };
  }

  signature(): string {
    // For fixpoint: hash of artifact keys
    const keys = Array.from(this.artifacts.keys()).sort();
    return sha256(JSON.stringify(keys));
  }
}
```

---

## Two Memoization Modes

### Mode 1: Explicit Dependencies (memo)

```lisp
;; Explicit dependency list
(memo (list "src/auth.ts" "src/types.ts")
  (expensive-analysis))
```

```typescript
function evalMemo(
  deps: string[],        // Explicit dependency refs
  body: Value,
  env: Environment,
  world: World,
  artifacts: ArtifactStore,
  cont: Continuation,
  ffi: FFI
): Value {
  const exprKey = toCanonicalKey(body);

  // Compute dependency fingerprint
  const depsFingerprint = sha256(
    deps.map(ref => `${ref}:${world.fingerprint(ref)}`).join('|')
  );

  const key: ArtifactKey = { exprKey, depsFingerprint };

  // Try cache
  const cached = artifacts.get(key);
  if (cached !== undefined) {
    metrics.increment('memo_hits');
    return applyCont(cont, cached);
  }

  // Cache miss - compute
  metrics.increment('memo_misses');
  const start = Date.now();
  const value = evalExpr(body, env, cont, ffi);
  const computeTimeMs = Date.now() - start;

  // Store result
  artifacts.put(key, value, {
    computeTimeMs,
    deps: deps.map(ref => ({ ref, fingerprint: world.fingerprint(ref) })),
  });

  return value;
}
```

### Mode 2: Auto-Dependency Tracking (memo/auto)

```lisp
;; Automatic dependency tracking
(memo/auto
  (begin
    (let ((content (world.read "src/auth.ts")))  ;; Tracked!
      (analyze content))))
```

```typescript
function evalMemoAuto(
  body: Value,
  env: Environment,
  world: World,
  artifacts: ArtifactStore,
  cont: Continuation,
  ffi: FFI
): Value {
  const exprKey = toCanonicalKey(body);

  // First, try to find valid cached result by expression
  const cached = artifacts.getByExpr(exprKey, world);
  if (cached !== undefined) {
    metrics.increment('memo_hits');
    return applyCont(cont, cached);
  }

  // Cache miss - compute with tracking
  metrics.increment('memo_misses');

  // Create tracking world wrapper
  const tracker = new DependencyTracker(world);
  const trackedWorld = tracker.wrap(world);

  // Evaluate with tracked world
  const start = Date.now();
  const value = evalExpr(body, env.withWorld(trackedWorld), cont, ffi);
  const computeTimeMs = Date.now() - start;

  // Get tracked dependencies
  const deps = tracker.getDependencies();
  const depsFingerprint = sha256(
    deps.map(d => `${d.ref}:${d.fingerprint}`).join('|')
  );

  // Store result
  const key: ArtifactKey = { exprKey, depsFingerprint };
  artifacts.put(key, value, { computeTimeMs, deps });

  return value;
}

class DependencyTracker {
  private deps: Map<string, string> = new Map();

  constructor(private world: World) {}

  wrap(world: World): World {
    return new Proxy(world, {
      get: (target, prop) => {
        if (prop === 'read') {
          return (ref: string) => {
            // Track the read
            this.deps.set(ref, target.fingerprint(ref));
            return target.read(ref);
          };
        }
        if (prop === 'fingerprint') {
          return (ref: string) => {
            // Track fingerprint calls too
            const fp = target.fingerprint(ref);
            this.deps.set(ref, fp);
            return fp;
          };
        }
        return (target as any)[prop];
      },
    });
  }

  getDependencies(): DependencyRecord[] {
    return Array.from(this.deps.entries()).map(([ref, fingerprint]) => ({
      ref,
      fingerprint,
    }));
  }
}
```

---

## Lisp API

```lisp
;; Explicit dependencies
(memo (list "file1.ts" "file2.ts")
  (analyze-files))

;; Auto-tracking
(memo/auto
  (let ((data (world.read "data.json")))
    (process data)))

;; Check if cached
(memo/cached? '(analyze-code "file.ts"))  ;; => #t or #f

;; Clear cache
(memo/clear!)          ;; Clear all
(memo/clear! expr)     ;; Clear specific

;; Cache statistics
(memo/stats)
;; => {:count 42 :hits 150 :misses 30 :hitRate 0.83}

;; Inspect dependencies for cached value
(memo/deps '(analyze-code "file.ts"))
;; => (("file.ts" "sha256:abc...") ("utils.ts" "sha256:def..."))
```

---

## Cache Invalidation

Artifacts automatically invalidate when dependencies change:

```lisp
;; First call - computes and caches
(memo/auto (analyze (world.read "src/auth.ts")))

;; Second call - cache hit (if file unchanged)
(memo/auto (analyze (world.read "src/auth.ts")))

;; After file modification - cache miss, recomputes
(world.write "src/auth.ts" "// modified")
(memo/auto (analyze (world.read "src/auth.ts")))  ;; Recomputes!
```

---

## Use Cases

### 1. Expensive Analysis

```lisp
;; AST parsing is expensive - cache it
(define (get-ast file)
  (memo (list file)
    (parse-typescript (world.read file))))

;; Multiple calls reuse cache
(get-ast "auth.ts")  ;; Parses
(get-ast "auth.ts")  ;; Cache hit!
(get-ast "auth.ts")  ;; Cache hit!
```

### 2. LLM Results with File Context

```lisp
;; LLM analysis cached by file content
(define (analyze-security file)
  (memo (list file)
    (llm.complete
      (str "Analyze security of:\n" (world.read file)))))

;; Re-analyze only if file changed
(analyze-security "auth.ts")  ;; LLM call
(analyze-security "auth.ts")  ;; Cache hit (saves $$$)
```

### 3. Multi-File Computations

```lisp
;; Cache result that depends on multiple files
(define (analyze-module module-dir)
  (let ((files (world.list (str module-dir "/**/*.ts"))))
    (memo files
      (let ((contents (map world.read files)))
        (analyze-all contents)))))
```

---

## Persistence

Artifacts can be persisted for cross-session caching:

```typescript
interface SerializedArtifactStore {
  version: number;
  artifacts: Array<{
    key: ArtifactKey;
    value: SerializedValue;
    metadata: Artifact['metadata'];
  }>;
}

class ArtifactStore {
  serialize(): SerializedArtifactStore {
    return {
      version: 1,
      artifacts: Array.from(this.artifacts.values()).map(a => ({
        key: a.key,
        value: serialize(a.value),
        metadata: a.metadata,
      })),
    };
  }

  static deserialize(
    data: SerializedArtifactStore,
    world: World
  ): ArtifactStore {
    const store = new ArtifactStore();

    for (const item of data.artifacts) {
      // Validate dependencies before restoring
      const stillValid = item.metadata.deps.every(dep => {
        try {
          return world.fingerprint(dep.ref) === dep.fingerprint;
        } catch {
          return false;  // File no longer exists
        }
      });

      if (stillValid) {
        store.artifacts.set(
          store.keyToString(item.key),
          {
            key: item.key,
            value: deserialize(item.value),
            metadata: item.metadata,
          }
        );
      }
    }

    return store;
  }
}
```

---

## Summary

Artifacts provide:

1. **Content-addressed caching** - Results keyed by computation + dependencies
2. **Automatic invalidation** - Stale when dependencies change
3. **Two modes** - Explicit deps or auto-tracking
4. **LRU eviction** - Memory-bounded cache
5. **Persistence** - Cross-session caching
6. **Metrics** - Hit/miss rates for tuning

Artifacts are the incremental computation engine that makes repeated analysis efficient.
