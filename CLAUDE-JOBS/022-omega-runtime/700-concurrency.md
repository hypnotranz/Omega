# 700: Concurrency Manager

## Status: COMPLETE ✅
- **Completed**: 2026-01-29
- **Location**: OmegaLLM-3/src/runtime/subsystems/ConcurrencyManager.ts (560 lines)

## Purpose
Manages asynchronous execution, cancellation, and cooperative scheduling. Defines the concurrency model (cooperative async, not true parallelism).

## Dependencies
- 100-types.md ✅
- 110-events.md ✅

## Source References
- ARCHITECTURE/12-CONCURRENCY.md
- docs/USER-MANUAL--22--Concurrency-And-Fairness.md
- FLAW F3: Concurrency model undefined (RESOLUTION)

---

## Deliverables

```
src/runtime/subsystems/
├── ConcurrencyManager.ts    # Main concurrency manager
└── concurrency/
    ├── AsyncPrimitives.ts   # Async primitives (fibers, channels, etc.)
    ├── WorkQueue.ts         # Task queue and scheduling
    └── index.ts             # Module exports
```

**Note:** File names differ from original spec but functionality is complete (50 tests pass).

---

## Concurrency Model

**OmegaRuntime uses Cooperative Async, NOT true parallelism:**

```
┌─────────────────────────────────────────────────────────────────┐
│ COOPERATIVE ASYNC MODEL                                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  • Single-threaded JavaScript runtime                           │
│  • Async operations yield at await points                       │
│  • No true parallel execution of Lisp code                      │
│  • Multiple LLM calls can be in-flight simultaneously           │
│  • Cancellation is cooperative (checks at yield points)         │
│                                                                  │
│  Timeline:                                                       │
│  ─────────────────────────────────────────────────────────►     │
│                                                                  │
│  Task A: [eval]──await──[llm]────────────────[continue]         │
│  Task B:        [eval]──await──[llm]─────[continue]             │
│                                                                  │
│  Only ONE Lisp evaluation runs at a time                        │
│  Multiple LLM calls can overlap (I/O bound)                     │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Key Types

```typescript
export type TaskState = 'pending' | 'running' | 'suspended' | 'completed' | 'cancelled' | 'error';

export interface Task<T = Val> {
  id: string;
  state: TaskState;
  priority: number;
  createdAt: number;
  startedAt?: number;
  completedAt?: number;

  // Execution
  body: () => Promise<T>;
  result?: T;
  error?: Error;

  // Cancellation
  cancellationToken: CancellationToken;

  // Metadata
  name?: string;
  parentId?: string;
}

export interface CancellationToken {
  isCancelled: boolean;
  reason?: string;
  cancel(reason?: string): void;
  throwIfCancelled(): void;
  onCancel(callback: () => void): void;
}

export interface SchedulerConfig {
  maxConcurrentTasks: number;
  maxConcurrentLLMCalls: number;
  taskTimeout: number;
  fairnessMode: 'fifo' | 'priority' | 'round-robin';
}
```

---

## Key Interface

```typescript
export interface ConcurrencyManager {
  // ─── Task Management ───

  /**
   * Submit a task for execution.
   */
  submit<T>(body: () => Promise<T>, options?: {
    name?: string;
    priority?: number;
    timeout?: number;
  }): Task<T>;

  /**
   * Get task by ID.
   */
  getTask(id: string): Task | undefined;

  /**
   * Get all tasks.
   */
  getAllTasks(): Task[];

  /**
   * Get tasks by state.
   */
  getTasksByState(state: TaskState): Task[];

  /**
   * Wait for task completion.
   */
  await<T>(task: Task<T>): Promise<T>;

  /**
   * Wait for all tasks.
   */
  awaitAll(tasks: Task[]): Promise<Val[]>;

  /**
   * Wait for any task.
   */
  awaitAny(tasks: Task[]): Promise<Task>;

  // ─── Cancellation ───

  /**
   * Cancel a task.
   */
  cancel(taskId: string, reason?: string): boolean;

  /**
   * Cancel all tasks.
   */
  cancelAll(reason?: string): void;

  /**
   * Create cancellation token.
   */
  createCancellationToken(): CancellationToken;

  /**
   * Check if current task is cancelled.
   */
  checkCancellation(): void;

  // ─── Scheduling ───

  /**
   * Configure scheduler.
   */
  configure(config: Partial<SchedulerConfig>): void;

  /**
   * Get current concurrency stats.
   */
  getStats(): {
    pendingTasks: number;
    runningTasks: number;
    completedTasks: number;
    cancelledTasks: number;
    concurrentLLMCalls: number;
  };

  // ─── Utilities ───

  /**
   * Delay execution (cooperative yield).
   */
  delay(ms: number): Promise<void>;

  /**
   * Yield to scheduler.
   */
  yield(): Promise<void>;
}
```

---

## Cooperative Cancellation

```typescript
class CancellationTokenImpl implements CancellationToken {
  private _isCancelled = false;
  private _reason?: string;
  private callbacks: (() => void)[] = [];

  get isCancelled(): boolean {
    return this._isCancelled;
  }

  get reason(): string | undefined {
    return this._reason;
  }

  cancel(reason?: string): void {
    if (this._isCancelled) return;
    this._isCancelled = true;
    this._reason = reason;
    this.callbacks.forEach(cb => cb());
  }

  throwIfCancelled(): void {
    if (this._isCancelled) {
      throw new CancellationError(this._reason);
    }
  }

  onCancel(callback: () => void): void {
    if (this._isCancelled) {
      callback();
    } else {
      this.callbacks.push(callback);
    }
  }
}

// Usage in evaluation loop
async function evalWithCancellation(
  expr: Val,
  token: CancellationToken
): Promise<Val> {
  let stepCount = 0;

  while (!isTerminal(state)) {
    // Check for cancellation every N steps
    if (++stepCount % 100 === 0) {
      token.throwIfCancelled();
      await concurrencyManager.yield(); // Allow other tasks
    }

    state = await step(state);
  }

  return state.value;
}
```

---

## Task Scheduler

```typescript
class Scheduler {
  private queue: Task[] = [];
  private running: Task[] = [];

  async run(): Promise<void> {
    while (true) {
      // Wait for tasks
      if (this.queue.length === 0) {
        await this.waitForTask();
        continue;
      }

      // Check concurrency limits
      if (this.running.length >= this.config.maxConcurrentTasks) {
        await this.waitForSlot();
        continue;
      }

      // Select next task
      const task = this.selectNext();
      if (!task) continue;

      // Start task
      this.running.push(task);
      this.executeTask(task);
    }
  }

  private selectNext(): Task | undefined {
    switch (this.config.fairnessMode) {
      case 'fifo':
        return this.queue.shift();
      case 'priority':
        this.queue.sort((a, b) => b.priority - a.priority);
        return this.queue.shift();
      case 'round-robin':
        return this.queue.shift(); // With time slicing
    }
  }
}
```

---

## Lisp Interface

```lisp
;; Submit async task
(async (lambda () (expensive-computation)))
; => <task-id>

;; Wait for task
(await task-id)
; => result

;; Submit with options
(async computation :name "analysis" :priority 10)

;; Cancel task
(cancel task-id "user requested")

;; Wait for any
(await-any (list task1 task2 task3))
; => First completed task result

;; Parallel LLM calls (I/O overlaps)
(await-all
  (list
    (async (lambda () (llm.infer "question 1")))
    (async (lambda () (llm.infer "question 2")))
    (async (lambda () (llm.infer "question 3")))))
; => List of three answers
```

---

## Test Requirements

### Unit Tests: `tests/runtime/subsystems/ConcurrencyManager.test.ts`
- [ ] submit() creates task
- [ ] await() waits for completion
- [ ] awaitAll() waits for all
- [ ] awaitAny() returns first
- [ ] cancel() cancels task
- [ ] CancellationToken works correctly
- [ ] checkCancellation() throws when cancelled
- [ ] Task priorities are respected
- [ ] FIFO ordering works
- [ ] Concurrency limits enforced

### Integration Tests
- [ ] Multiple LLM calls overlap correctly
- [ ] Cancellation stops evaluation
- [ ] Task timeout works
- [ ] Nested tasks work
- [ ] Stats are accurate

---

## Acceptance Criteria
1. Tasks execute with cooperative scheduling
2. Cancellation is prompt and reliable
3. Multiple LLM calls can overlap (I/O)
4. Concurrency limits are enforced
5. Priority scheduling works correctly
6. No race conditions or deadlocks
