# 220: State Coordinator (FLAW F2 Resolution)

## Status: COMPLETE ✅
- **Completed**: 2026-01-29
- **Location**: OmegaLLM-3/src/runtime/subsystems/StateCoordinator.ts (10357 bytes)

## Purpose
Mediates all state-modifying operations to prevent conflicts between
Amb (backtracking), Snapshot (checkpoints), and History (time travel).

## Dependencies
- 100-types.md ✅
- 110-events.md ✅

## Source References
- Design pattern: Mediator (GoF)
- FLAW F2: State ownership conflict resolution
- FLAW F7: Transaction ↔ Amb circular dependency resolution

---

## Problem Solved

**Without StateCoordinator:**
```
1. User evaluates: (amb '(1 2 3))
   → AmbManager saves state S0, returns 1

2. User runs: (snapshots.save "checkpoint")
   → SnapshotManager saves state S1 (includes amb in progress)

3. User evaluates: (require (> x 1))
   → Fails! AmbManager backtracks to S0, tries 2

4. User runs: (snapshots.restore "checkpoint")
   → CONFLICT! S1 has stale backtrack stack
   → AmbManager's state is corrupted
```

**With StateCoordinator:**
- All state modifications go through coordinator
- Coordinator tracks context (in amb? in transaction? in fixpoint?)
- Conflicting operations are prevented with clear error messages

---

## Deliverables

```
src/runtime/internal/
├── StateCoordinator.ts          # Main coordinator class
├── StateContext.ts              # Context tracking
└── CheckpointRegistry.ts        # Checkpoint ID management
```

---

## Key Interface

```typescript
export type CheckpointSource = 'amb' | 'snapshot' | 'history' | 'transaction';

export interface CheckpointInfo {
  id: string;
  source: CheckpointSource;
  state: MachineSnapshot;
  timestamp: number;
  parentId?: string;           // For nested contexts
}

export interface StateCoordinator {
  // ─── Checkpoint Management ───

  /**
   * Create checkpoint, returns ID.
   * Validates context allows checkpointing.
   */
  checkpoint(source: CheckpointSource): string;

  /**
   * Restore to checkpoint.
   * Validates restoration is allowed in current context.
   * Throws StateConflictError if not allowed.
   */
  restore(checkpointId: string, source: CheckpointSource): MachineSnapshot;

  /**
   * Discard checkpoint (cleanup).
   */
  discard(checkpointId: string): void;

  // ─── Context Queries ───

  /** Is an AMB search in progress? */
  isInAmbSearch(): boolean;

  /** Is a transaction in progress? */
  isInTransaction(): boolean;

  /** Is a fixpoint computation running? */
  isInFixpoint(): boolean;

  /** Get nesting depths */
  getContextDepth(): {
    amb: number;
    transaction: number;
    fixpoint: number;
  };

  // ─── Conflict Prevention ───

  /**
   * Check if restore is allowed.
   * Returns error message if not, null if OK.
   */
  canRestore(source: CheckpointSource): string | null;

  /**
   * Check if starting AMB is allowed.
   * Returns error message if not, null if OK.
   */
  canStartAmb(): string | null;

  /**
   * Check if starting transaction is allowed.
   * Returns error message if not, null if OK.
   */
  canStartTransaction(): string | null;

  /**
   * Check if commit is allowed.
   * Returns error message if not, null if OK.
   */
  canCommit(): string | null;

  // ─── Context Lifecycle ───

  /** Enter AMB context */
  enterAmb(checkpointId: string): void;

  /** Exit AMB context (success or exhausted) */
  exitAmb(): void;

  /** Notify AMB backtrack (for transaction rollback) */
  notifyAmbBacktrack(checkpointId: string): void;

  /** Enter transaction context */
  enterTransaction(transactionId: string): void;

  /** Exit transaction (commit or rollback) */
  exitTransaction(): void;

  /** Enter fixpoint context */
  enterFixpoint(): void;

  /** Exit fixpoint */
  exitFixpoint(): void;
}
```

---

## Conflict Rules

### Restore Conflicts
| Current Context | Snapshot Restore | History Back | AMB Backtrack |
|-----------------|------------------|--------------|---------------|
| In AMB search   | BLOCKED          | BLOCKED      | OK            |
| In transaction  | BLOCKED          | BLOCKED      | OK (rollback) |
| In fixpoint     | BLOCKED          | BLOCKED      | BLOCKED       |
| Clean           | OK               | OK           | N/A           |

### Start Conflicts
| Current Context | Start AMB | Start Transaction | Commit |
|-----------------|-----------|-------------------|--------|
| In AMB search   | OK (nest) | OK (scoped)       | BLOCKED |
| In transaction  | OK        | OK (nest)         | OK (inner) |
| In fixpoint     | OK        | BLOCKED           | BLOCKED |

---

## Error Messages

```typescript
export class StateConflictError extends Error {
  constructor(
    public readonly operation: string,
    public readonly reason: string,
    public readonly context: { amb: number; transaction: number; fixpoint: number }
  ) {
    super(`Cannot ${operation}: ${reason}`);
  }
}

// Example errors:
// "Cannot restore snapshot: AMB search in progress. Call amb.resetSearch() first."
// "Cannot commit transaction: Inside AMB search. Transactions inside AMB are auto-committed on success."
// "Cannot start transaction: Inside fixpoint computation."
```

---

## Integration with Subsystems

### SnapshotManager uses coordinator:
```typescript
class SnapshotManager {
  restore(name: string): void {
    const error = this.coordinator.canRestore('snapshot');
    if (error) {
      throw new StateConflictError('restore snapshot', error,
        this.coordinator.getContextDepth());
    }
    // proceed...
  }
}
```

### AmbManager uses coordinator:
```typescript
class AmbManager {
  choose(alternatives: Thunk[]): Val {
    const error = this.coordinator.canStartAmb();
    if (error) {
      throw new StateConflictError('start AMB', error,
        this.coordinator.getContextDepth());
    }

    const checkpointId = this.coordinator.checkpoint('amb');
    this.coordinator.enterAmb(checkpointId);
    // ...
  }

  fail(): never {
    this.coordinator.notifyAmbBacktrack(this.currentCheckpointId);
    // TransactionManager will receive event and rollback
    // ...
  }
}
```

### TransactionManager listens to coordinator:
```typescript
class TransactionManager {
  constructor(coordinator: StateCoordinator, emitter: RuntimeEventEmitter) {
    // When AMB backtracks, rollback any proposals made in that choice
    emitter.on('amb-backtrack', (event) => {
      this.rollbackToCheckpoint(event.checkpointId);
    });
  }
}
```

---

## Test Requirements

### Unit Tests: `tests/runtime/internal/StateCoordinator.test.ts`
- [ ] checkpoint() creates unique IDs
- [ ] restore() returns correct state
- [ ] isInAmbSearch() tracks AMB context correctly
- [ ] canRestore() blocks during AMB
- [ ] canStartAmb() allows nesting
- [ ] canCommit() blocks during AMB
- [ ] notifyAmbBacktrack() emits event
- [ ] Context depths track correctly with nesting
- [ ] StateConflictError has correct message

### Integration Tests
- [ ] AMB + Snapshot interaction blocked correctly
- [ ] Transaction rollback on AMB fail
- [ ] Nested AMB contexts work
- [ ] Fixpoint blocks transactions

---

## Acceptance Criteria
1. All state conflicts are detected and prevented
2. Clear error messages guide user to resolution
3. No state corruption possible through any sequence of operations
4. TransactionManager correctly rolls back on AMB backtrack
5. Performance: <1ms overhead per checkpoint operation
