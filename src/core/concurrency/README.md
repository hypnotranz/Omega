# Concurrency Module

This module implements SICP-style concurrency as explicit evaluator semantics - fibers, schedulers, and synchronization primitives for semantic pipelines.

## Overview

Concurrency in OmegaLLM is not "OS threads" but explicit interleaving of machine states. This enables:
- Deterministic replay of interleavings
- Governed concurrent operations (caps/budgets/profiles)
- Inference-driven concurrency analysis and repair
- Parallelizing semantic tasks (classification, redaction, verification)

## Architecture

```
concurrency/
  types.ts      - Core types (FiberState, SchedulerState, SchedulePolicy, etc.)
  scheduler.ts  - Fiber scheduler with deterministic policies
  sync.ts       - Synchronization primitives (Mutex, IVar, Channel)
  singleflight.ts - Deduplicated concurrent memoization
  actor.ts      - Actor model implementation
  critic.ts     - Deadlock detection and race analysis
  index.ts      - Public exports
```

## Key Concepts

### Fibers
Lightweight concurrent execution units. Each fiber has its own CEKS state but shares the store.

```lisp
(fiber/spawn (lambda () (classify text)))  ; Create fiber
(fiber/yield)                               ; Yield control
(fiber/join fiber)                          ; Wait for completion
```

### Scheduling Policies
Deterministic scheduling enables replay:
- **RoundRobin**: Simple fair scheduling
- **FairRR**: Round-robin with quantum
- **Random**: Seeded random selection (reproducible)
- **Replay**: Follow recorded decision sequence

### Serializers (SICP Pattern)
Wrap procedures for mutual exclusion:

```lisp
(define safe-put! ((make-serializer) put!))
```

### Singleflight Memoization
Deduplicate concurrent calls to semantic functions:
- First caller creates IVar and starts computation
- Other callers block on IVar
- Result shared with all callers
- Only ONE oracle call per key

### Actors
Message-passing concurrency avoiding shared state:

```lisp
(actor/spawn (lambda (msg) (handle msg)))
(actor/send actor message)
```

## Governance Integration

All concurrency operations are:
- **Profile-allowed**: Check `allowedOps` for fiber.*, mutex.*, etc.
- **Cap-checked**: Require `cap.fiber.spawn`, `cap.mutex`, etc.
- **Budgeted**: Track `spawnsLeft`, `yieldsLeft`, `stepsLeft`
- **Ledgered**: Record all scheduling decisions for replay

## Deadlock Detection

The critic detects:
- Lock cycles (circular wait)
- Budget exhaustion with no progress
- Duplicate oracle calls (race in cache)

And proposes repairs:
- Impose lock ordering
- Wrap with serializer
- Actorize shared state
- Introduce singleflight

## Deterministic Replay

Every scheduling decision is recorded:
```ts
Event(ScheduleChoose, { readySetHash, chosenFiberId, decisionIndex, step })
```

Replay mode consumes the recorded sequence to reproduce exact interleavings.
