# Prompt 13: SICP-Style Concurrency

This directory contains tests for SICP-style concurrency with fibers, deterministic scheduling, and synchronization primitives.

## Overview

Prompt 13 implements the concurrency layer based on SICP Chapter 5's explicit-control evaluator concepts:
- Fibers as lightweight cooperative execution contexts
- Deterministic scheduling with replay capability
- Serializers (mutexes) for mutual exclusion
- Singleflight memoization for deduplicating concurrent oracle calls
- Deadlock detection and diagnosis

## Test Coverage

### Test 13.1: Basic Fiber Spawn and Join
Tests that fibers can be spawned and joined correctly.
- Spawn two fibers computing different values
- Join waits for fiber completion
- Verifies: correct results, parent-child relationships

### Test 13.2: Deterministic Round-Robin Scheduling
Tests that round-robin scheduling produces consistent behavior.
- Multiple fibers with interleaved execution
- Verifies: deterministic ordering, step counts

### Test 13.3: Serializer Mutual Exclusion
Tests that serializers correctly protect shared state.
- Bank account transfer example from SICP
- Multiple concurrent transfers
- Verifies: no race conditions, correct final balance

### Test 13.4: Singleflight Oracle Deduplication
Tests that concurrent identical oracle calls are deduplicated.
- Multiple fibers request same oracle call
- Only one actual call is made
- Verifies: deduplication, all waiters get result

### Test 13.5: Deadlock Detection and Diagnosis
Tests that deadlocks are detected and explained.
- Two fibers each waiting for a mutex held by the other
- Verifies: deadlock detected, cycle identified, explanation generated

### Test 13.6: Random Scheduling with Seed Reproducibility
Tests that random scheduling is deterministic given a seed.
- Run with Random policy and seed
- Replay same schedule
- Verifies: identical decisions, identical outcomes

### Test 13.7: IVar Single-Assignment Coordination
Tests IVar semantics for single-assignment variables.
- Multiple readers waiting on empty IVar
- Single writer fills the IVar
- Verifies: all readers unblocked, correct value

### Test 13.8: Channel Communication
Tests buffered and unbuffered channel communication.
- Producer-consumer pattern
- Verifies: correct message passing, blocking behavior

## Key Types

- `FiberVal`: Reference to a fiber (lightweight thread)
- `MutexVal`: Reference to a mutex (serializer)
- `IVarVal`: Reference to a single-assignment variable
- `ChannelVal`: Reference to a communication channel
- `ActorVal`: Reference to an actor with mailbox
- `SchedulerView`: Snapshot of scheduler state for introspection

## Architecture

```
omega.concurrency/
  types.ts       - Core types (FiberState, SchedulerState, etc.)
  scheduler.ts   - Fiber scheduler with deterministic policies
  sync.ts        - Synchronization primitives (Mutex, IVar, Channel)
  singleflight.ts - Deduplicated concurrent memoization
  actor.ts       - Actor model with mailboxes
  critic.ts      - Deadlock detection and race analysis
```

## Scheduling Policies

- **RoundRobin**: Simple first-in-first-out scheduling
- **FairRR**: Round-robin with time quantum for fairness
- **Random**: Deterministic random selection with LCG
- **Replay**: Replay recorded scheduling decisions
