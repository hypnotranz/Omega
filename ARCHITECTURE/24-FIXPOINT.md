# 24: Fixpoint (Convergence Detection)

## The Problem

AI agents often need iterative refinement:

```
1. Analyze code → find issues
2. Fix issues → analyze again
3. Repeat until stable
```

But when do you stop? Without convergence detection:
- Risk infinite loops
- Waste compute/tokens
- Miss oscillations

**Fixpoint solves this** by detecting when state stabilizes.

---

## Core Concept

Fixpoint evaluates an expression repeatedly until the **state signature** stops changing:

```
┌─────────────────────────────────────────────────────────────────┐
│                    Fixpoint Execution                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Iteration 1: eval(body) → sig_1 = hash(facts, bindings, ...)│
│   Iteration 2: eval(body) → sig_2 = hash(facts, bindings, ...)│
│   Iteration 3: eval(body) → sig_3 = hash(facts, bindings, ...)│
│                                                                 │
│   if sig_n == sig_{n-1}:  CONVERGED (stable)                   │
│   if sig_n in {sig_1..sig_{n-2}}:  CYCLE (oscillating)         │
│   if n > max_iters:  NONCONVERGED (budget exceeded)            │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## State Projections

Different convergence criteria for different use cases:

```typescript
type SignatureMode =
  | 'facts'                    // Only fact accumulator
  | 'facts+bindings'           // Facts + scalar env bindings
  | 'facts+world'              // Facts + world fingerprint
  | 'facts+bindings+world'     // All three (default)
  | 'facts+bindings+world+artifacts';  // Also artifact cache stats

interface SignatureComponents {
  facts: string;           // Hash of fact set
  bindings?: string;       // Hash of scalar bindings
  world?: string;          // World fingerprint
  artifacts?: string;      // Artifact cache signature
}

function computeStateSignature(
  env: Environment,
  world: World,
  artifacts: ArtifactStore,
  mode: SignatureMode
): string {
  const components: SignatureComponents = {
    facts: env.facts.signature(),
  };

  if (mode.includes('bindings')) {
    components.bindings = computeBindingsSignature(env);
  }

  if (mode.includes('world')) {
    components.world = world.fingerprint();
  }

  if (mode.includes('artifacts')) {
    components.artifacts = artifacts.signature();
  }

  return sha256(JSON.stringify(components));
}
```

### When to Use Each Mode

| Mode | Use Case |
|------|----------|
| `facts` | Pure knowledge accumulation, no side effects |
| `facts+bindings` | Iterative computation with local state |
| `facts+world` | Code analysis/modification loops |
| `facts+bindings+world` | General agent loops (default) |
| `+artifacts` | When memo invalidation matters |

---

## TypeScript Implementation

```typescript
interface FixpointOptions {
  maxIterations: number;
  mode: SignatureMode;
  detectCycles: boolean;
  returnOutcome: boolean;  // Return structured outcome vs throw
}

interface FixpointResult {
  status: 'converged' | 'cycle' | 'nonconverged';
  value: Value;
  iterations: number;
  finalSignature: string;
  cycleLength?: number;  // For cycle detection
}

function evalFixpoint(
  body: Value,
  env: Environment,
  world: World,
  artifacts: ArtifactStore,
  options: FixpointOptions,
  cont: Continuation,
  ffi: FFI
): Value {
  const { maxIterations, mode, detectCycles, returnOutcome } = options;

  // Track signatures for convergence/cycle detection
  const signatures: string[] = [];
  const signatureToIteration: Map<string, number> = new Map();

  let lastValue: Value = null;
  let status: FixpointResult['status'] = 'nonconverged';
  let cycleLength: number | undefined;

  for (let i = 0; i < maxIterations; i++) {
    // Evaluate body
    lastValue = evalExpr(body, env, cont, ffi);

    // Compute current state signature
    const sig = computeStateSignature(env, world, artifacts, mode);

    // Check for convergence (same as previous)
    if (signatures.length > 0 && sig === signatures[signatures.length - 1]) {
      status = 'converged';
      break;
    }

    // Check for cycles (seen before, but not immediately previous)
    if (detectCycles && signatureToIteration.has(sig)) {
      const cycleStart = signatureToIteration.get(sig)!;
      cycleLength = i - cycleStart;
      status = 'cycle';
      break;
    }

    // Record signature
    signatures.push(sig);
    signatureToIteration.set(sig, i);
  }

  const result: FixpointResult = {
    status,
    value: lastValue,
    iterations: signatures.length,
    finalSignature: signatures[signatures.length - 1],
    cycleLength,
  };

  if (returnOutcome) {
    // Return structured outcome
    return [Symbol.for(status), lastValue, {
      iterations: result.iterations,
      signature: result.finalSignature,
      cycleLength: result.cycleLength,
    }];
  } else {
    // Legacy behavior: return value or throw
    if (status === 'converged') {
      return lastValue;
    }
    throw new FixpointError(`Fixpoint ${status} after ${result.iterations} iterations`);
  }
}
```

---

## Lisp API

```lisp
;; Basic fixpoint (throws on non-convergence)
(fixpoint
  (begin
    (let ((issues (find-issues)))
      (for-each (lambda (i) (assert `(issue ,i))) issues))
    (facts/count))
  :max-iters 10)

;; Fixpoint with outcome (structured return)
(fixpoint/outcome
  body
  :max-iters 10
  :mode 'facts+world)
;; => (ok value meta) | (cycle value meta) | (nonconverged value meta)

;; With explicit mode
(fixpoint body
  :max-iters 20
  :mode 'facts+bindings+world)

;; Pattern: handle outcomes
(match (fixpoint/outcome body :max-iters 10)
  [(ok value _) value]
  [(cycle value meta)
   (warn (str "Oscillation detected, cycle length: " (meta :cycleLength)))
   value]
  [(nonconverged value _)
   (error "Failed to converge")])
```

---

## Convergence Patterns

### Pattern 1: Knowledge Accumulation

```lisp
;; Keep finding issues until no new ones
(fixpoint
  (let ((current-count (facts/count)))
    (analyze-and-assert-findings)
    current-count)  ;; Return count for debugging
  :max-iters 100
  :mode 'facts)  ;; Only facts matter
```

### Pattern 2: Code Modification Loop

```lisp
;; Apply fixes until code is stable
(fixpoint
  (begin
    (let ((issues (find-issues)))
      (when (not (null? issues))
        (apply-fix (car issues))))
    (world.fingerprint "src/"))  ;; World fingerprint tracks changes
  :max-iters 10
  :mode 'facts+world)
```

### Pattern 3: Multi-Phase Pipeline

```lisp
;; Each phase runs to fixpoint, then triggers next
(fixpoint
  (begin
    ;; Phase 1: Analysis (run until stable)
    (unless (fact? '(phase-complete analysis))
      (when (analyze-step)
        (assert '(phase-complete analysis))))

    ;; Phase 2: Planning (after analysis)
    (when (and (fact? '(phase-complete analysis))
               (not (fact? '(phase-complete planning))))
      (when (plan-step)
        (assert '(phase-complete planning))))

    ;; Phase 3: Execution (after planning)
    (when (and (fact? '(phase-complete planning))
               (not (fact? '(phase-complete execution))))
      (when (execute-step)
        (assert '(phase-complete execution))))

    ;; Return overall status
    (fact? '(phase-complete execution)))
  :max-iters 50)
```

---

## Cycle Detection

Cycles indicate oscillating state:

```
sig_1 → sig_2 → sig_3 → sig_2 → sig_3 → ...
                 ↑___________|
                    CYCLE!
```

```typescript
function detectCycle(
  signatures: string[],
  current: string
): { isCycle: boolean; cycleLength?: number; cycleStart?: number } {
  for (let i = 0; i < signatures.length - 1; i++) {
    if (signatures[i] === current) {
      return {
        isCycle: true,
        cycleLength: signatures.length - i,
        cycleStart: i,
      };
    }
  }
  return { isCycle: false };
}
```

### Handling Cycles

```lisp
;; Detect and break cycles
(let ((result (fixpoint/outcome body :max-iters 20)))
  (match result
    [(cycle value meta)
     ;; Log cycle for debugging
     (log/warn (str "Cycle detected: length=" (meta :cycleLength)))

     ;; Strategy 1: Accept oscillating value
     value

     ;; Strategy 2: Inject randomness to break cycle
     ;; (set! some-binding (random))

     ;; Strategy 3: Signal for human intervention
     ;; (signal 'cycle-detected :meta meta)
     ]
    [_ (result 1)]))  ;; Extract value from ok/nonconverged
```

---

## Budget Integration

Fixpoint respects budget limits:

```typescript
function evalFixpoint(
  body: Value,
  options: FixpointOptions,
  budget: Budget,
  ...
): Value {
  for (let i = 0; i < options.maxIterations; i++) {
    // Check iteration budget
    if (!budget.hasRemaining('iterations')) {
      return makeOutcome('nonconverged', lastValue, { reason: 'budget' });
    }
    budget.consumeIteration();

    // Evaluate (this may consume tokens, time, etc.)
    lastValue = evalExpr(body, ...);

    // ... convergence checks
  }
}
```

---

## Metrics

Fixpoint tracks execution metrics:

```typescript
interface FixpointMetrics {
  totalIterations: number;
  convergedCount: number;
  cycleCount: number;
  nonconvergedCount: number;
  averageIterationsToConverge: number;
}

// In metrics collection
metrics.increment('fixpoint_iterations');
metrics.record('fixpoint_outcome', status);
```

---

## Debugging Fixpoint

```lisp
;; Verbose mode: log each iteration
(fixpoint/debug
  body
  :max-iters 10
  :on-iteration (lambda (i sig value)
                  (print (str "Iteration " i ": sig=" sig " value=" value))))

;; Get full trace
(let ((trace (fixpoint/trace body :max-iters 10)))
  (for-each (lambda (step)
              (print (str "Step " (step :iteration)
                         " sig=" (step :signature)
                         " facts=" (step :facts-count))))
            trace))
```

---

## Summary

Fixpoint provides:

1. **Convergence detection** - Know when to stop
2. **Pluggable state projections** - Customize what "stable" means
3. **Cycle detection** - Catch oscillations
4. **Structured outcomes** - Handle all termination cases
5. **Budget integration** - Prevent runaway loops
6. **Debugging support** - Trace and inspect iterations

Fixpoint is the foundation for iterative AI agent loops.
