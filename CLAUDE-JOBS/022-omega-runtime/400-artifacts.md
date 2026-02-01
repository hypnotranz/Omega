# 400: Artifact Manager

## Status: COMPLETE ✅
- **Completed**: 2026-01-29
- **Location**: OmegaLLM-3/src/runtime/subsystems/ArtifactManager.ts (295 lines)

## Purpose
Manages semantic memoization - caching LLM results for deterministic replay and cost reduction.

## Dependencies
- 100-types.md ✅
- 110-events.md ✅
- 120-providers.md ✅

## Source References
- ARCHITECTURE/26-ARTIFACTS.md
- Content-addressable storage patterns
- Nix/Bazel deterministic build concepts

---

## Deliverables

```
src/runtime/subsystems/
├── ArtifactManager.ts       # Main artifact manager
└── artifacts/
    ├── ArtifactStore.ts     # Storage implementation
    ├── ArtifactKey.ts       # Key generation
    └── ArtifactEviction.ts  # Cache eviction policies
```

---

## Key Types

```typescript
export interface Artifact {
  key: string;               // Content-addressed hash
  input: ArtifactInput;      // What generated this
  output: Val;               // The cached result
  metadata: ArtifactMetadata;
}

export interface ArtifactInput {
  operation: string;         // 'infer.op', 'search.op', etc.
  prompt: string;            // Full prompt text
  parameters?: {             // LLM parameters
    temperature?: number;
    maxTokens?: number;
    model?: string;
  };
}

export interface ArtifactMetadata {
  createdAt: number;
  accessedAt: number;
  accessCount: number;
  size: number;              // Bytes
  tokensUsed?: number;
  latencyMs?: number;
  provenance?: string;       // Source tracking
}

export interface ArtifactStats {
  totalArtifacts: number;
  totalSize: number;
  hitCount: number;
  missCount: number;
  hitRate: number;
  savings: {
    tokens: number;
    cost: number;
    latencyMs: number;
  };
}

export type EvictionPolicy = 'lru' | 'lfu' | 'fifo' | 'size';
```

---

## Key Interface

```typescript
export interface ArtifactManager {
  /**
   * Get artifact by key, or undefined if not cached.
   */
  get(key: string): Artifact | undefined;

  /**
   * Get artifact by input (computes key).
   */
  getByInput(input: ArtifactInput): Artifact | undefined;

  /**
   * Store a new artifact.
   */
  store(input: ArtifactInput, output: Val, metadata?: Partial<ArtifactMetadata>): Artifact;

  /**
   * Check if artifact exists.
   */
  has(key: string): boolean;

  /**
   * Invalidate artifact(s).
   */
  invalidate(key: string): boolean;
  invalidateByPattern(pattern: string): number;

  /**
   * Clear all artifacts.
   */
  clear(): void;

  /**
   * Get cache statistics.
   */
  getStats(): ArtifactStats;

  /**
   * Set eviction policy.
   */
  setEvictionPolicy(policy: EvictionPolicy): void;

  /**
   * Set maximum cache size.
   */
  setMaxSize(bytes: number): void;

  /**
   * Export artifacts for persistence.
   */
  export(): SerializedArtifacts;

  /**
   * Import artifacts.
   */
  import(data: SerializedArtifacts): void;

  /**
   * Get all artifacts for iteration.
   */
  entries(): IterableIterator<[string, Artifact]>;
}
```

---

## Key Generation

```typescript
import { createHash } from 'crypto';

/**
 * Generate content-addressed key from input.
 * Same input → same key, regardless of when called.
 */
function generateKey(input: ArtifactInput): string {
  const normalized = normalizeInput(input);
  const hash = createHash('sha256');
  hash.update(JSON.stringify(normalized));
  return hash.digest('hex').slice(0, 16); // First 16 chars
}

/**
 * Normalize input for consistent hashing.
 * - Sort object keys
 * - Trim whitespace
 * - Normalize temperature precision
 */
function normalizeInput(input: ArtifactInput): ArtifactInput {
  return {
    operation: input.operation,
    prompt: input.prompt.trim(),
    parameters: input.parameters ? {
      temperature: Math.round((input.parameters.temperature ?? 0) * 100) / 100,
      maxTokens: input.parameters.maxTokens,
      model: input.parameters.model
    } : undefined
  };
}
```

---

## Integration Pattern

```typescript
// In LLM integration code
async function inferWithCache(prompt: string, params: LLMParams): Promise<Val> {
  const input: ArtifactInput = {
    operation: 'infer.op',
    prompt,
    parameters: params
  };

  // Check cache first
  const cached = artifactManager.getByInput(input);
  if (cached) {
    // Record stats but don't call LLM
    emitter.emit('artifact-hit', { key: cached.key, savings: cached.metadata });
    return cached.output;
  }

  // Call LLM
  const startTime = Date.now();
  const result = await llmAdapter.wrapCall('infer', estimatedTokens, () =>
    provider.complete(prompt, params)
  );
  const latencyMs = Date.now() - startTime;

  // Cache result
  artifactManager.store(input, result.content, {
    tokensUsed: result.usage?.totalTokens,
    latencyMs
  });

  return result.content;
}
```

---

## Eviction Policies

```typescript
interface EvictionStrategy {
  shouldEvict(artifact: Artifact, stats: ArtifactStats): boolean;
  selectForEviction(artifacts: Artifact[], targetSize: number): Artifact[];
}

const LRU: EvictionStrategy = {
  shouldEvict(artifact, stats) {
    return stats.totalSize > this.maxSize;
  },
  selectForEviction(artifacts, targetSize) {
    return [...artifacts]
      .sort((a, b) => a.metadata.accessedAt - b.metadata.accessedAt)
      .slice(0, Math.ceil(artifacts.length * 0.1)); // Evict oldest 10%
  }
};

const LFU: EvictionStrategy = {
  selectForEviction(artifacts, targetSize) {
    return [...artifacts]
      .sort((a, b) => a.metadata.accessCount - b.metadata.accessCount)
      .slice(0, Math.ceil(artifacts.length * 0.1)); // Evict least used 10%
  }
};
```

---

## Lisp Interface

```lisp
;; Check if result is cached
(artifacts.has? "translate" "hello" "french")

;; Get cached result (or nil)
(artifacts.get "translate" "hello" "french")

;; Manually store artifact
(artifacts.store "translate" "hello" "french" "bonjour")

;; Clear cache
(artifacts.clear)

;; Get statistics
(artifacts.stats)
; => (:hit-rate 0.85 :total-size 1048576 :savings (:tokens 50000 :cost 0.50))
```

---

## Test Requirements

### Unit Tests: `tests/runtime/subsystems/ArtifactManager.test.ts`
- [ ] store() creates artifact with correct key
- [ ] get() retrieves stored artifact
- [ ] getByInput() computes key correctly
- [ ] has() detects existence
- [ ] invalidate() removes artifact
- [ ] invalidateByPattern() removes matching
- [ ] clear() removes all
- [ ] Key generation is deterministic
- [ ] Same input always produces same key
- [ ] LRU eviction works correctly
- [ ] LFU eviction works correctly
- [ ] Size limits are enforced
- [ ] export()/import() round-trip

### Integration Tests
- [ ] LLM calls are cached
- [ ] Cache hit skips LLM call
- [ ] Statistics track savings
- [ ] Persistence works across sessions
- [ ] Temperature affects cache key

---

## Acceptance Criteria
1. Same prompt + params always returns same cached result
2. Cache significantly reduces LLM calls
3. Eviction prevents unbounded memory growth
4. Statistics accurately track hit/miss/savings
5. Content-addressed keys are collision-resistant
6. Performance: <1ms cache lookup
