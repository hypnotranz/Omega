# 120: Provider Interfaces

## Status: COMPLETE ✅
- **Completed**: 2026-01-29
- **Location**: OmegaLLM-3/src/runtime/providers/
- **Note**: 14 provider files. Mocks are in separate job (1200-mocks.md)

## Purpose
Define pluggable backend interfaces for all I/O and storage operations.
Providers enable testing (mocks) and flexibility (different storage backends).

## Dependencies
- 100-types.md ✅

## Source References
- Design pattern: Strategy (GoF)
- Design pattern: Dependency Injection

---

## Deliverables

### Provider Interfaces
```
src/runtime/providers/
├── index.ts                       # Re-exports all providers
│
├── # Core Providers
├── StateProvider.ts               # CESK state storage
├── SessionProvider.ts             # Session persistence
├── SnapshotProvider.ts            # Checkpoint storage
├── ReceiptProvider.ts             # OPR receipt storage
├── TraceProvider.ts               # Execution trace storage
├── EffectHandlerRegistry.ts       # Custom effect handlers
├── GovernanceProvider.ts          # Caps/budget checking
│
├── # Data Providers
├── ArtifactProvider.ts            # Memoization cache
├── LogicProvider.ts               # Semantic fact database
├── StreamProvider.ts              # Stream state
│
├── # Governance Providers
├── SecurityProvider.ts            # Security/audit
├── ProvenanceProvider.ts          # Evidence tracking
│
├── # Expert Provider
└── ExpertProvider.ts              # Expert role registry
```

### Default Implementations
```
src/runtime/providers/
├── MemoryStateProvider.ts
├── FilesystemStateProvider.ts
├── FilesystemSessionProvider.ts
├── MemorySnapshotProvider.ts
├── FilesystemSnapshotProvider.ts
├── InMemoryReceiptProvider.ts
├── FilesystemReceiptProvider.ts
├── MemoryTraceProvider.ts
├── StreamingTraceProvider.ts
├── LocalGovernanceProvider.ts
├── RemoteGovernanceProvider.ts
├── MemoryArtifactProvider.ts
├── FilesystemArtifactProvider.ts
├── MemoryLogicProvider.ts
├── MemoryStreamProvider.ts
├── LocalSecurityProvider.ts
├── MemoryProvenanceProvider.ts
└── DefaultExpertProvider.ts
```

### Mock Providers (for testing)
```
src/runtime/providers/mocks/
├── index.ts
├── MockStateProvider.ts
├── MockSessionProvider.ts
├── MockSnapshotProvider.ts
├── MockReceiptProvider.ts
├── MockTraceProvider.ts
├── MockGovernanceProvider.ts
├── MockEffectHandlerRegistry.ts
├── MockEventEmitter.ts
├── MockArtifactProvider.ts
├── MockLogicProvider.ts
├── MockExpertProvider.ts
└── MockStreamProvider.ts
```

---

## Key Interfaces

### StateProvider
```typescript
export interface StateProvider {
  /** Create fresh CESK state */
  createState(): State;

  /** Load state by ID */
  loadState(id: string): Promise<State | null>;

  /** Save state with ID */
  saveState(id: string, state: State): Promise<void>;

  /** Clone state (deep copy for snapshots) */
  cloneState(state: State): State;
}
```

### SessionProvider
```typescript
export interface SessionProvider {
  /** List all sessions */
  list(): Promise<SessionInfo[]>;

  /** Load session by name */
  load(name: string): Promise<SessionData | null>;

  /** Save session */
  save(name: string, data: SessionData): Promise<void>;

  /** Delete session */
  delete(name: string): Promise<void>;

  /** Check if session exists */
  exists(name: string): Promise<boolean>;
}
```

### SnapshotProvider
```typescript
export interface SnapshotProvider {
  /** List all snapshots */
  list(): Promise<SnapshotInfo[]>;

  /** Save snapshot */
  save(name: string, snapshot: MachineSnapshot): Promise<void>;

  /** Load snapshot */
  load(name: string): Promise<MachineSnapshot | null>;

  /** Export snapshot to file */
  export(name: string, path: string): Promise<void>;

  /** Import snapshot from file */
  import(path: string): Promise<{ name: string; snapshot: MachineSnapshot }>;

  /** Delete snapshot */
  delete(name: string): Promise<void>;
}
```

### ArtifactProvider (26-ARTIFACTS.md)
```typescript
export interface ArtifactKey {
  exprKey: string;           // Hash of expression
  depsFingerprint: string;   // Hash of dependencies
  paramsKey?: string;        // Hash of parameters
}

export interface Artifact<T = Val> {
  key: ArtifactKey;
  value: T;
  metadata: {
    createdAt: number;
    accessedAt: number;
    accessCount: number;
    computeTimeMs: number;
    deps: Array<{ ref: string; fingerprint: string }>;
  };
}

export interface ArtifactProvider {
  /** Get cached value by exact key */
  get(key: ArtifactKey): Val | undefined;

  /** Get by expression with fingerprint validation */
  getByExpr(exprKey: string, currentFingerprints: Map<string, string>): Val | undefined;

  /** Store value */
  put(key: ArtifactKey, value: Val, metadata: Partial<Artifact['metadata']>): void;

  /** Clear all cache */
  clear(): void;

  /** Clear by expression */
  clearByExpr(exprKey: string): void;

  /** Get cache stats */
  stats(): { count: number; capacity: number | null; hitCount: number; missCount: number };

  /** Get cache signature (for fixpoint) */
  signature(): string;
}
```

### GovernanceProvider (25-BUDGET.md)
```typescript
export interface GovernanceProvider {
  /** Get current profile */
  getProfile(): Profile;

  /** Get capability set */
  getCaps(): CapSet;

  /** Check if capability granted */
  checkCap(cap: string): boolean;

  /** Get budget tracker */
  getBudget(): Budget;

  /** Consume budget (returns false if exceeded) */
  consumeBudget(type: 'tokens' | 'cost' | 'time' | 'iterations', amount: number): boolean;

  /** Check if budget remains */
  hasRemaining(type: 'tokens' | 'cost' | 'time' | 'iterations'): boolean;

  /** Get remaining budget */
  remaining(type: 'tokens' | 'cost' | 'time' | 'iterations'): number | null;

  /** Get all remaining */
  remainingAll(): Record<'tokens' | 'cost' | 'time' | 'iterations', number | null>;

  /** Get full budget report */
  report(): BudgetReport;

  /** Reset budget to initial */
  reset(): void;
}
```

### LogicProvider (USER-MANUAL--27)
```typescript
export interface LogicProvider {
  /** Assert a fact */
  assertFact(fact: string): void;

  /** Add a rule */
  addRule(condition: string, conclusion: string): void;

  /** Get all facts */
  getFacts(): string[];

  /** Get all rules */
  getRules(): Array<{ condition: string; conclusion: string }>;

  /** Clear all */
  clear(): void;

  /** Get fact count */
  count(): number;
}
```

### EffectHandlerRegistry
```typescript
export type EffectHandler = (
  context: EffectContext,
  ...args: Val[]
) => Promise<EffectResult>;

export interface EffectHandlerRegistry {
  /** Register handler for effect operation */
  register(op: string, handler: EffectHandler): void;

  /** Get handler for operation */
  get(op: string): EffectHandler | undefined;

  /** List registered operations */
  list(): string[];

  /** Check if operation registered */
  has(op: string): boolean;
}
```

---

## Provider Relationship to Core

Providers WRAP existing `src/core/` modules:
- `FilesystemSessionProvider` wraps `src/core/session/writer.ts`, `reader.ts`
- `LocalGovernanceProvider` wraps `src/core/governance/profile.ts`
- `MemoryStateProvider` wraps `src/core/eval/store.ts`

---

## Test Requirements

### Unit Tests: `tests/runtime/providers/`
Each provider implementation needs tests:
- [ ] MemoryStateProvider - create, save, load, clone
- [ ] FilesystemSessionProvider - list, save, load, delete
- [ ] MemorySnapshotProvider - save, restore, export
- [ ] MemoryArtifactProvider - get, put, stats, clear
- [ ] All Mock providers - verify mock behavior

---

## Acceptance Criteria
1. All interfaces have at least one implementation
2. All implementations have corresponding mock
3. Providers are stateless (inject via constructor)
4. Async operations return Promises
5. Clear error handling for missing resources
