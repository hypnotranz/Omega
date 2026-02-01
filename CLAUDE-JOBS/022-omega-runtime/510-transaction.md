# 510: Transaction Manager

## Status: COMPLETE ✅
- **Completed**: 2026-01-29
- **Location**: OmegaLLM-3/src/runtime/subsystems/TransactionManager.ts (560 lines)

## Purpose
Manages atomic state modifications with commit/rollback semantics, integrated with AMB for automatic rollback on backtrack.

## Dependencies
- 100-types.md ✅
- 110-events.md ✅
- 220-state-coordinator.md ✅

## Source References
- Software Transactional Memory (STM) patterns
- FLAW F7: Transaction ↔ Amb circular dependency resolution
- Database ACID properties

---

## Deliverables

```
src/runtime/subsystems/
├── TransactionManager.ts    # Main transaction manager
└── transaction/
    ├── Transaction.ts       # Transaction types
    ├── Proposal.ts          # Change proposal types
    └── ConflictDetector.ts  # Optimistic locking
```

---

## Key Types

```typescript
export type TransactionState = 'active' | 'committed' | 'aborted';

export interface Transaction {
  id: string;
  state: TransactionState;
  parentId?: string;         // For nested transactions
  startTime: number;
  proposals: Proposal[];
  checkpointId: string;      // For rollback
}

export interface Proposal {
  id: string;
  transactionId: string;
  type: 'set' | 'delete' | 'assert' | 'retract';
  target: string;            // Variable name or fact ID
  oldValue?: Val;
  newValue?: Val;
  timestamp: number;
}

export interface TransactionResult {
  transactionId: string;
  committed: boolean;
  duration: number;
  proposalCount: number;
  conflictsWith?: string;    // If aborted due to conflict
}
```

---

## Key Interface

```typescript
export interface TransactionManager {
  /**
   * Begin a new transaction.
   */
  begin(): Transaction;

  /**
   * Commit the current transaction.
   * Throws if conflicts detected.
   */
  commit(): TransactionResult;

  /**
   * Abort/rollback the current transaction.
   */
  rollback(): void;

  /**
   * Propose a change (deferred until commit).
   */
  propose(proposal: Omit<Proposal, 'id' | 'transactionId' | 'timestamp'>): Proposal;

  /**
   * Get current transaction.
   */
  current(): Transaction | undefined;

  /**
   * Check if in a transaction.
   */
  isInTransaction(): boolean;

  /**
   * Get transaction depth (for nesting).
   */
  getDepth(): number;

  /**
   * Run body in transaction, auto-commit on success.
   */
  withTransaction<T>(body: () => T): T;

  /**
   * Rollback to specific checkpoint (for AMB integration).
   */
  rollbackToCheckpoint(checkpointId: string): void;

  /**
   * Get all proposals in current transaction.
   */
  getProposals(): Proposal[];
}
```

---

## Integration with StateCoordinator

The transaction manager listens to StateCoordinator events for AMB integration:

```typescript
class TransactionManagerImpl implements TransactionManager {
  constructor(
    private coordinator: StateCoordinator,
    private emitter: RuntimeEventEmitter
  ) {
    // Listen for AMB backtrack events
    emitter.on('amb-backtrack', (event) => {
      this.rollbackToCheckpoint(event.checkpointId);
    });
  }

  begin(): Transaction {
    // Check with coordinator
    const error = this.coordinator.canStartTransaction();
    if (error) {
      throw new StateConflictError('begin transaction', error,
        this.coordinator.getContextDepth());
    }

    const checkpointId = this.coordinator.checkpoint('transaction');
    const transaction: Transaction = {
      id: generateId(),
      state: 'active',
      parentId: this.current()?.id,
      startTime: Date.now(),
      proposals: [],
      checkpointId
    };

    this.stack.push(transaction);
    this.coordinator.enterTransaction(transaction.id);

    this.emitter.emit('transaction-start', { transaction });
    return transaction;
  }

  commit(): TransactionResult {
    // Check with coordinator
    const error = this.coordinator.canCommit();
    if (error) {
      throw new StateConflictError('commit transaction', error,
        this.coordinator.getContextDepth());
    }

    const transaction = this.current();
    if (!transaction) {
      throw new Error('No active transaction');
    }

    // Apply all proposals
    for (const proposal of transaction.proposals) {
      this.applyProposal(proposal);
    }

    transaction.state = 'committed';
    this.stack.pop();
    this.coordinator.exitTransaction();
    this.coordinator.discard(transaction.checkpointId);

    const result: TransactionResult = {
      transactionId: transaction.id,
      committed: true,
      duration: Date.now() - transaction.startTime,
      proposalCount: transaction.proposals.length
    };

    this.emitter.emit('transaction-commit', { result });
    return result;
  }

  rollbackToCheckpoint(checkpointId: string): void {
    // Find all transactions at or after this checkpoint
    while (this.stack.length > 0) {
      const tx = this.stack[this.stack.length - 1];
      if (tx.checkpointId === checkpointId) {
        // Found the checkpoint, rollback this transaction
        this.rollback();
        break;
      }
      // This transaction was created after the checkpoint, discard it
      this.rollback();
    }
  }
}
```

---

## Proposal Application

```typescript
private applyProposal(proposal: Proposal): void {
  switch (proposal.type) {
    case 'set':
      this.environment.set(proposal.target, proposal.newValue!);
      break;
    case 'delete':
      this.environment.delete(proposal.target);
      break;
    case 'assert':
      this.factsManager.assert(proposal.newValue as string);
      break;
    case 'retract':
      this.factsManager.retract(proposal.target);
      break;
  }
}
```

---

## Lisp Interface

```lisp
;; Simple transaction
(transaction
  (set! x 10)
  (fact! "x was set to 10")
  (if (> x 5)
      (commit)
      (rollback)))

;; Automatic commit
(with-transaction
  (set! balance (- balance amount))
  (set! recipient-balance (+ recipient-balance amount)))
;; Automatically commits if no error

;; Manual control
(begin-transaction)
(set! x 1)
(set! y 2)
(if (valid? x y)
    (commit-transaction)
    (rollback-transaction))

;; Nested transactions (savepoints)
(with-transaction
  (set! a 1)
  (with-transaction
    (set! b 2)
    (rollback))  ; Only rolls back b
  ;; a is still pending
  (commit))  ; Commits a
```

---

## AMB Integration

```lisp
;; Transactions inside AMB are auto-rolled-back on fail
(amb-search
  (with-transaction
    (let ((x (amb '(1 2 3))))
      (set! result x)
      (require (even? x))
      (commit)))
  ;; If require fails, transaction is rolled back
  ;; before trying next alternative
  result)
; => 2
```

---

## Test Requirements

### Unit Tests: `tests/runtime/subsystems/TransactionManager.test.ts`
- [ ] begin() creates transaction
- [ ] commit() applies proposals
- [ ] rollback() discards proposals
- [ ] propose() records proposal
- [ ] isInTransaction() tracks state
- [ ] getDepth() tracks nesting
- [ ] withTransaction() auto-commits
- [ ] withTransaction() rolls back on error
- [ ] Nested transactions work correctly
- [ ] rollbackToCheckpoint() finds correct transaction

### Integration Tests
- [ ] AMB backtrack rolls back transaction
- [ ] StateCoordinator blocks commit during AMB
- [ ] Transaction blocked during fixpoint
- [ ] Proposals affect correct state
- [ ] Events fire at correct points

---

## Acceptance Criteria
1. Transactions provide atomicity (all or nothing)
2. Proposals are deferred until commit
3. Rollback restores previous state
4. AMB integration auto-rollbacks on backtrack
5. Nested transactions work as savepoints
6. StateCoordinator conflicts are respected
