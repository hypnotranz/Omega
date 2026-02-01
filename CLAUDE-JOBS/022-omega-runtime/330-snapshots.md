# 330: Snapshot Manager

## Status: COMPLETE ✅
- **Completed**: 2026-01-29
- **Location**: OmegaLLM-3/src/runtime/subsystems/SnapshotManager.ts (454 lines)

## Purpose
Manages named checkpoints of machine state for user-controlled save/restore, distinct from automatic history tracking.

## Dependencies
- 100-types.md ✅
- 110-events.md ✅
- 120-providers.md ✅
- 220-state-coordinator.md ✅

## Source References
- ARCHITECTURE/32-HISTORY.md (checkpoint section)
- docs/USER-MANUAL--09--Image-Persistence.md
- Emacs undo tree concept

---

## Deliverables

```
src/runtime/subsystems/
├── SnapshotManager.ts       # Main snapshot manager
└── snapshots/
    ├── MachineSnapshot.ts   # Snapshot type definition
    ├── SnapshotDiff.ts      # Compare snapshots
    └── SnapshotExporter.ts  # Export/import formats
```

---

## Key Types

```typescript
export interface MachineSnapshot {
  id: string;
  name: string;
  timestamp: number;
  description?: string;

  // Core CESK state
  control: Control;
  environment: SerializedEnvironment;
  kontinuation: SerializedKontinuation;
  store: SerializedStore;

  // Additional runtime state
  macros: Map<string, Macro>;
  facts: Fact[];
  artifacts: Map<string, Artifact>;

  // Semantic state
  ambState?: AmbSnapshot;
  transactionState?: TransactionSnapshot;

  // Metadata
  size: number;  // Approximate bytes
  tags?: string[];
}

export interface SnapshotDiff {
  added: {
    environment: string[];
    facts: string[];
    artifacts: string[];
  };
  removed: {
    environment: string[];
    facts: string[];
    artifacts: string[];
  };
  modified: {
    environment: { key: string; oldValue: Val; newValue: Val }[];
    store: { address: number; oldValue: Val; newValue: Val }[];
  };
}
```

---

## Key Interface

```typescript
export interface SnapshotManager {
  /**
   * Save current state as named snapshot.
   */
  save(name: string, description?: string): MachineSnapshot;

  /**
   * Restore to a named snapshot.
   * Throws StateConflictError if in AMB/transaction.
   */
  restore(name: string): MachineSnapshot;

  /**
   * Check if snapshot exists.
   */
  has(name: string): boolean;

  /**
   * Get snapshot without restoring.
   */
  get(name: string): MachineSnapshot | undefined;

  /**
   * List all snapshots.
   */
  list(): MachineSnapshot[];

  /**
   * Delete a snapshot.
   */
  delete(name: string): boolean;

  /**
   * Rename a snapshot.
   */
  rename(oldName: string, newName: string): void;

  /**
   * Compare two snapshots.
   */
  diff(name1: string, name2: string): SnapshotDiff;

  /**
   * Compare snapshot with current state.
   */
  diffCurrent(name: string): SnapshotDiff;

  /**
   * Export snapshot to portable format.
   */
  export(name: string, format: 'json' | 'binary'): Uint8Array;

  /**
   * Import snapshot from portable format.
   */
  import(data: Uint8Array, name?: string): MachineSnapshot;

  /**
   * Get approximate memory usage.
   */
  getTotalSize(): number;

  /**
   * Limit total snapshot storage.
   * Removes oldest snapshots if over limit.
   */
  setMaxSize(bytes: number): void;
}
```

---

## Serialization

```typescript
/**
 * Environment can contain closures, which contain environments.
 * Need careful serialization to handle cycles.
 */
function serializeEnvironment(env: Environment): SerializedEnvironment {
  const seen = new WeakMap<object, string>();
  let refId = 0;

  function serialize(value: Val, path: string): SerializedVal {
    if (seen.has(value)) {
      return { type: 'ref', id: seen.get(value)! };
    }

    if (isClosure(value)) {
      const id = `closure-${refId++}`;
      seen.set(value, id);
      return {
        type: 'closure',
        id,
        params: value.params,
        body: serialize(value.body, `${path}.body`),
        env: serialize(value.env, `${path}.env`)
      };
    }

    if (isList(value)) {
      return {
        type: 'list',
        elements: value.map((v, i) => serialize(v, `${path}[${i}]`))
      };
    }

    // Primitives
    return { type: 'primitive', value };
  }

  const result: SerializedEnvironment = {};
  for (const [name, value] of env) {
    result[name] = serialize(value, name);
  }
  return result;
}
```

---

## Integration with StateCoordinator

```typescript
class SnapshotManagerImpl implements SnapshotManager {
  constructor(
    private coordinator: StateCoordinator,
    private provider: StateProvider,
    private emitter: RuntimeEventEmitter
  ) {}

  restore(name: string): MachineSnapshot {
    // Validate through coordinator
    const error = this.coordinator.canRestore('snapshot');
    if (error) {
      throw new StateConflictError('restore snapshot', error,
        this.coordinator.getContextDepth());
    }

    const snapshot = this.snapshots.get(name);
    if (!snapshot) {
      throw new Error(`Snapshot not found: ${name}`);
    }

    // Restore through coordinator
    this.coordinator.restore(snapshot.id, 'snapshot');

    // Emit event
    this.emitter.emit('snapshot-restored', {
      name,
      snapshot
    });

    return snapshot;
  }
}
```

---

## Lisp Interface

```lisp
;; Save current state
(snapshots.save "before-experiment")

;; Later, restore
(snapshots.restore "before-experiment")

;; List all snapshots
(snapshots.list)
; => (("before-experiment" :timestamp 1234567890)
;     ("working-version" :timestamp 1234567800))

;; Compare with current
(snapshots.diff "before-experiment")
; => (:added (x y) :removed (z) :modified ((a 1 -> 2)))

;; Delete
(snapshots.delete "before-experiment")
```

---

## Test Requirements

### Unit Tests: `tests/runtime/subsystems/SnapshotManager.test.ts`
- [ ] save() creates valid snapshot
- [ ] restore() returns to exact state
- [ ] has() correctly detects existence
- [ ] list() returns all snapshots
- [ ] delete() removes snapshot
- [ ] rename() updates name
- [ ] diff() shows correct differences
- [ ] export()/import() round-trip correctly
- [ ] Closures are serialized/deserialized correctly
- [ ] Circular references handled
- [ ] setMaxSize() enforces limit

### Integration Tests
- [ ] Snapshot blocked during AMB search
- [ ] Snapshot captures all semantic state
- [ ] Restore reverts all state correctly
- [ ] Large environments serialize efficiently
- [ ] Provider persistence works

---

## Acceptance Criteria
1. Snapshots capture complete machine state
2. Restore returns to exact saved state
3. StateCoordinator conflicts are respected
4. Serialization handles all value types
5. Memory limits are enforced
6. Export format is portable across sessions
