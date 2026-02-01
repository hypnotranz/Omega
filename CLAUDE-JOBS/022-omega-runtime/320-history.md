# 320: History Manager (FLAW F4 Resolution)

## Status: COMPLETE ✅
- **Completed**: 2026-01-29
- **Location**: OmegaLLM-3/src/runtime/subsystems/HistoryManager.ts (443 lines)

## Purpose
Tracks evaluation history for time-travel debugging - recording states, navigating backward/forward, and replaying execution.

## Dependencies
- 100-types.md ✅
- 110-events.md ✅
- 120-providers.md ✅

## Source References
- ARCHITECTURE/32-HISTORY.md
- Redux DevTools time-travel inspiration
- docs/USER-MANUAL--09--Image-Persistence.md

---

## Deliverables

```
src/runtime/subsystems/
├── HistoryManager.ts        # Main history manager
└── history/
    ├── HistoryEntry.ts      # Entry types
    ├── HistoryNavigator.ts  # Navigation logic
    └── HistoryStorage.ts    # Persistence adapter
```

---

## Key Types

```typescript
export interface HistoryEntry {
  id: string;
  index: number;
  timestamp: number;

  // What was evaluated
  input: string;
  parsedExpr: Val;

  // Result
  result: EvalResult;

  // State snapshot (for time travel)
  stateSnapshot?: MachineSnapshot;

  // Metadata
  duration: number;
  stepCount: number;
  llmCalls: number;
  tags?: string[];
}

export interface HistoryFilter {
  startTime?: number;
  endTime?: number;
  tags?: string[];
  hasError?: boolean;
  searchText?: string;
}

export interface HistoryStats {
  totalEntries: number;
  totalDuration: number;
  totalLLMCalls: number;
  errorCount: number;
  dateRange: { start: number; end: number };
}
```

---

## Key Interface

```typescript
export interface HistoryManager {
  /**
   * Record a new history entry.
   */
  record(entry: Omit<HistoryEntry, 'id' | 'index'>): HistoryEntry;

  /**
   * Get entry by index (0 = oldest).
   */
  get(index: number): HistoryEntry | undefined;

  /**
   * Get entry by ID.
   */
  getById(id: string): HistoryEntry | undefined;

  /**
   * Get last N entries.
   */
  getLast(n: number): HistoryEntry[];

  /**
   * Search/filter history.
   */
  search(filter: HistoryFilter): HistoryEntry[];

  /**
   * Get total entry count.
   */
  size(): number;

  /**
   * Clear all history.
   */
  clear(): void;

  // ─── Time Travel ───

  /**
   * Get current position in history (-1 if at end/live).
   */
  getCurrentPosition(): number;

  /**
   * Go to specific history entry.
   * Returns the state to restore.
   */
  goto(index: number): MachineSnapshot;

  /**
   * Go back one entry.
   */
  back(): MachineSnapshot | null;

  /**
   * Go forward one entry.
   */
  forward(): MachineSnapshot | null;

  /**
   * Return to live (current) state.
   */
  goLive(): void;

  /**
   * Check if viewing historical state.
   */
  isViewingHistory(): boolean;

  // ─── Persistence ───

  /**
   * Save history to storage.
   */
  save(name: string): Promise<void>;

  /**
   * Load history from storage.
   */
  load(name: string): Promise<void>;

  /**
   * Export history to JSON.
   */
  export(): string;

  /**
   * Import history from JSON.
   */
  import(json: string): void;

  /**
   * Get history statistics.
   */
  getStats(): HistoryStats;
}
```

---

## Time Travel Implementation

```typescript
class HistoryManagerImpl implements HistoryManager {
  private entries: HistoryEntry[] = [];
  private currentPosition: number = -1; // -1 = live

  goto(index: number): MachineSnapshot {
    if (index < 0 || index >= this.entries.length) {
      throw new Error(`History index ${index} out of range`);
    }

    const entry = this.entries[index];
    if (!entry.stateSnapshot) {
      throw new Error('No snapshot available for this entry');
    }

    this.currentPosition = index;

    // Emit event
    this.emitter.emit('history-navigate', {
      index,
      entry,
      direction: 'jump'
    });

    return entry.stateSnapshot;
  }

  back(): MachineSnapshot | null {
    const targetIndex = this.currentPosition === -1
      ? this.entries.length - 1
      : this.currentPosition - 1;

    if (targetIndex < 0) return null;

    return this.goto(targetIndex);
  }

  forward(): MachineSnapshot | null {
    if (this.currentPosition === -1) return null;

    const targetIndex = this.currentPosition + 1;
    if (targetIndex >= this.entries.length) {
      this.goLive();
      return null; // Return to live
    }

    return this.goto(targetIndex);
  }

  goLive(): void {
    this.currentPosition = -1;
    this.emitter.emit('history-navigate', {
      index: -1,
      entry: null,
      direction: 'live'
    });
  }
}
```

---

## Snapshot Strategy

```typescript
/**
 * Not every entry needs a full snapshot.
 * Strategy: snapshot every N entries or on significant state changes.
 */
interface SnapshotStrategy {
  shouldSnapshot(entry: HistoryEntry, lastSnapshot: number): boolean;
}

const defaultStrategy: SnapshotStrategy = {
  shouldSnapshot(entry, lastSnapshot) {
    // Snapshot every 10 entries
    if (entry.index - lastSnapshot >= 10) return true;

    // Snapshot if LLM was called (expensive to replay)
    if (entry.llmCalls > 0) return true;

    // Snapshot if error occurred
    if (entry.result.status === 'error') return true;

    return false;
  }
};
```

---

## Integration with StateCoordinator

```typescript
// History restore must go through StateCoordinator
class HistoryManager {
  constructor(
    private coordinator: StateCoordinator,
    private emitter: RuntimeEventEmitter
  ) {}

  goto(index: number): MachineSnapshot {
    // Check if restore is allowed
    const error = this.coordinator.canRestore('history');
    if (error) {
      throw new StateConflictError('history goto', error,
        this.coordinator.getContextDepth());
    }

    const entry = this.entries[index];
    this.currentPosition = index;

    // Restore through coordinator
    return this.coordinator.restore(entry.checkpointId, 'history');
  }
}
```

---

## Test Requirements

### Unit Tests: `tests/runtime/subsystems/HistoryManager.test.ts`
- [ ] record() adds entry with correct index
- [ ] get() retrieves by index
- [ ] getById() retrieves by ID
- [ ] getLast() returns correct entries
- [ ] search() filters correctly
- [ ] back() navigates correctly
- [ ] forward() navigates correctly
- [ ] goto() jumps to specific entry
- [ ] goLive() returns to current state
- [ ] isViewingHistory() tracks mode correctly
- [ ] save()/load() persist correctly
- [ ] export()/import() round-trip correctly

### Integration Tests
- [ ] History records all evaluations
- [ ] Time travel restores correct state
- [ ] History blocked during AMB
- [ ] Snapshots captured at correct intervals
- [ ] Large history handles memory efficiently

---

## Acceptance Criteria
1. All evaluations are recorded in history
2. Time travel restores exact state
3. Navigation (back/forward/goto) works correctly
4. History respects StateCoordinator conflicts
5. Large histories don't cause memory issues
6. History can be saved/loaded across sessions
