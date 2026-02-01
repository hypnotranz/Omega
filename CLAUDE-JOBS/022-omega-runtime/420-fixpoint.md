# 420: Fixpoint Manager

## Status: COMPLETE ✅
- **Completed**: 2026-01-29
- **Location**: OmegaLLM-3/src/runtime/subsystems/FixpointManager.ts (285 lines)

## Purpose
Manages iterative convergence computations where facts/values are updated until stable.

## Dependencies
- 100-types.md ✅
- 110-events.md ✅
- 400-artifacts.md ✅
- 410-facts.md ✅

## Source References
- ARCHITECTURE/24-FIXPOINT.md
- Datalog fixpoint semantics
- Denotational semantics fixpoint theory

---

## Deliverables

```
src/runtime/subsystems/
├── FixpointManager.ts       # Main fixpoint manager
└── fixpoint/
    ├── Lattice.ts           # Lattice operations
    ├── FixpointIterator.ts  # Iteration logic
    └── ConvergenceChecker.ts # Check for stability
```

---

## Key Types

```typescript
export interface FixpointConfig {
  maxIterations: number;     // Safety limit
  convergenceThreshold?: number; // For numeric convergence
  timeout?: number;          // Milliseconds
  onChange?: (key: string, oldVal: Val, newVal: Val) => void;
}

export interface FixpointState {
  iteration: number;
  values: Map<string, Val>;
  changed: Set<string>;      // Keys that changed this iteration
  converged: boolean;
  startTime: number;
}

export interface FixpointResult {
  values: Map<string, Val>;
  iterations: number;
  converged: boolean;
  duration: number;
  changedKeys: string[];     // Keys that ever changed
}

export interface LatticeDef<T> {
  bottom: T;                 // Minimum element
  top?: T;                   // Maximum element (if bounded)
  join: (a: T, b: T) => T;   // Least upper bound
  leq: (a: T, b: T) => boolean; // Less than or equal
  equal: (a: T, b: T) => boolean;
}
```

---

## Key Interface

```typescript
export interface FixpointManager {
  /**
   * Run fixpoint computation.
   */
  compute<T>(
    initial: Map<string, T>,
    update: (current: Map<string, T>, iteration: number) => Map<string, T>,
    config?: FixpointConfig
  ): Promise<FixpointResult>;

  /**
   * Run fixpoint with lattice operations.
   */
  computeWithLattice<T>(
    initial: Map<string, T>,
    update: (current: Map<string, T>) => Map<string, T>,
    lattice: LatticeDef<T>,
    config?: FixpointConfig
  ): Promise<FixpointResult>;

  /**
   * Get current fixpoint state (if computation in progress).
   */
  getState(): FixpointState | null;

  /**
   * Check if fixpoint computation is in progress.
   */
  isComputing(): boolean;

  /**
   * Cancel running computation.
   */
  cancel(): void;

  /**
   * Common lattice definitions.
   */
  lattices: {
    boolean: LatticeDef<boolean>;     // false < true
    set: <T>() => LatticeDef<Set<T>>; // Subset lattice
    number: LatticeDef<number>;       // Standard ordering
    interval: LatticeDef<[number, number]>; // Interval lattice
  };
}
```

---

## Fixpoint Algorithm

```typescript
async function compute<T>(
  initial: Map<string, T>,
  update: (current: Map<string, T>, iteration: number) => Map<string, T>,
  config: FixpointConfig
): Promise<FixpointResult> {
  let current = new Map(initial);
  let iteration = 0;
  const startTime = Date.now();
  const allChangedKeys = new Set<string>();

  while (iteration < config.maxIterations) {
    // Check timeout
    if (config.timeout && Date.now() - startTime > config.timeout) {
      throw new FixpointTimeoutError(iteration, current);
    }

    // Compute next iteration
    const next = update(current, iteration);

    // Check for changes
    const changedKeys = findChangedKeys(current, next, config.convergenceThreshold);

    if (changedKeys.size === 0) {
      // Converged!
      return {
        values: next,
        iterations: iteration + 1,
        converged: true,
        duration: Date.now() - startTime,
        changedKeys: Array.from(allChangedKeys)
      };
    }

    // Record changes
    changedKeys.forEach(k => allChangedKeys.add(k));

    // Emit progress
    emitter.emit('fixpoint-iteration', {
      iteration,
      changedCount: changedKeys.size,
      changedKeys: Array.from(changedKeys)
    });

    // Notify callbacks
    if (config.onChange) {
      for (const key of changedKeys) {
        config.onChange(key, current.get(key), next.get(key));
      }
    }

    current = next;
    iteration++;
  }

  // Did not converge
  return {
    values: current,
    iterations: iteration,
    converged: false,
    duration: Date.now() - startTime,
    changedKeys: Array.from(allChangedKeys)
  };
}

function findChangedKeys<T>(
  prev: Map<string, T>,
  next: Map<string, T>,
  threshold?: number
): Set<string> {
  const changed = new Set<string>();

  for (const [key, value] of next) {
    const oldValue = prev.get(key);
    if (!isEqual(oldValue, value, threshold)) {
      changed.add(key);
    }
  }

  return changed;
}
```

---

## Common Lattices

```typescript
const booleanLattice: LatticeDef<boolean> = {
  bottom: false,
  top: true,
  join: (a, b) => a || b,
  leq: (a, b) => !a || b,
  equal: (a, b) => a === b
};

function setLattice<T>(): LatticeDef<Set<T>> {
  return {
    bottom: new Set(),
    join: (a, b) => new Set([...a, ...b]),
    leq: (a, b) => [...a].every(x => b.has(x)),
    equal: (a, b) => a.size === b.size && [...a].every(x => b.has(x))
  };
}

const intervalLattice: LatticeDef<[number, number]> = {
  bottom: [Infinity, -Infinity], // Empty interval
  join: ([a1, b1], [a2, b2]) => [Math.min(a1, a2), Math.max(b1, b2)],
  leq: ([a1, b1], [a2, b2]) => a2 <= a1 && b1 <= b2,
  equal: ([a1, b1], [a2, b2]) => a1 === a2 && b1 === b2
};
```

---

## Integration with StateCoordinator

```typescript
class FixpointManagerImpl implements FixpointManager {
  async compute<T>(...): Promise<FixpointResult> {
    // Fixpoint blocks transactions
    if (this.coordinator.isInTransaction()) {
      throw new StateConflictError('start fixpoint',
        'Cannot start fixpoint inside transaction',
        this.coordinator.getContextDepth());
    }

    this.coordinator.enterFixpoint();
    try {
      return await this.doCompute(...);
    } finally {
      this.coordinator.exitFixpoint();
    }
  }
}
```

---

## Lisp Interface

```lisp
;; Simple fixpoint
(fixpoint
  (lambda (facts)
    (if (facts.has? "A implies B")
        (facts.with "B")
        facts))
  :initial (facts.from "A implies B" "A"))
; => facts containing "A implies B", "A", "B"

;; With iteration limit
(fixpoint update-fn
  :initial init
  :max-iterations 100
  :on-change (lambda (k old new) (println k ":" old "->" new)))
```

---

## Test Requirements

### Unit Tests: `tests/runtime/subsystems/FixpointManager.test.ts`
- [ ] compute() converges on simple case
- [ ] compute() respects maxIterations
- [ ] compute() respects timeout
- [ ] computeWithLattice() uses lattice operations
- [ ] isComputing() tracks state
- [ ] cancel() stops computation
- [ ] Boolean lattice works correctly
- [ ] Set lattice works correctly
- [ ] onChange callback fires correctly
- [ ] Events emit on each iteration

### Integration Tests
- [ ] Fixpoint blocked during transaction
- [ ] Fixpoint with facts integration
- [ ] Fixpoint with artifacts caching
- [ ] Large fixpoint completes reasonably
- [ ] Monotonic progress is maintained

---

## Acceptance Criteria
1. Fixpoint converges when stable
2. Max iterations prevent infinite loops
3. Timeout prevents hung computations
4. Lattice operations enable monotonic convergence
5. StateCoordinator integration prevents conflicts
6. Events enable progress monitoring
