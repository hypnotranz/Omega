# 910: Streams Manager

## Status: COMPLETE ✅
- **Completed**: 2026-01-29
- **Location**: OmegaLLM-3/src/runtime/subsystems/StreamsManager.ts (576 lines)

## Purpose
Implements lazy streams for on-demand computation, enabling infinite sequences and efficient LLM result streaming.

## Dependencies
- 100-types.md ✅
- 110-events.md ✅
- 210-execution.md ✅

## Source References
- SICP Chapter 3.5 (Streams)
- docs/USER-MANUAL--23--Streams-Of-Inference.md
- docs/USER-MANUAL--07--Streaming-Computation.md

---

## Deliverables

```
src/runtime/subsystems/
├── StreamsManager.ts        # Main streams manager
└── streams/
    ├── Stream.ts            # Stream type definition
    ├── StreamOps.ts         # Stream operations
    └── StreamCache.ts       # Memoization for forced values
```

---

## Key Types

```typescript
export interface Stream<T = Val> {
  type: 'stream';
  head: () => T;              // Thunk for first element
  tail: () => Stream<T>;      // Thunk for rest of stream
  isEmpty: boolean;
}

export interface StreamState {
  activeStreams: number;
  totalForced: number;
  cachedValues: number;
}

// Empty stream singleton
export const THE_EMPTY_STREAM: Stream = {
  type: 'stream',
  head: () => { throw new Error('Empty stream has no head'); },
  tail: () => THE_EMPTY_STREAM,
  isEmpty: true
};
```

---

## Key Interface

```typescript
export interface StreamsManager {
  // ─── Creation ───

  /**
   * Create stream from head and tail thunks.
   */
  cons(head: () => Val, tail: () => Stream): Stream;

  /**
   * Create stream from list.
   */
  fromList(list: Val[]): Stream;

  /**
   * Create stream from generator function.
   */
  fromGenerator(generator: () => Generator<Val>): Stream;

  /**
   * Create infinite stream from seed and step function.
   */
  iterate(seed: Val, step: (v: Val) => Val): Stream;

  /**
   * Create infinite stream of repeated value.
   */
  repeat(value: Val): Stream;

  /**
   * Create stream of integers from n.
   */
  integersFrom(n: number): Stream;

  // ─── Access ───

  /**
   * Get first element (forces head).
   */
  car(stream: Stream): Val;

  /**
   * Get rest of stream (forces tail).
   */
  cdr(stream: Stream): Stream;

  /**
   * Check if stream is empty.
   */
  isNull(stream: Stream): boolean;

  // ─── Transformation ───

  /**
   * Map function over stream (lazy).
   */
  map(fn: (v: Val) => Val, stream: Stream): Stream;

  /**
   * Filter stream by predicate (lazy).
   */
  filter(pred: (v: Val) => boolean, stream: Stream): Stream;

  /**
   * Flat map (lazy).
   */
  flatMap(fn: (v: Val) => Stream, stream: Stream): Stream;

  /**
   * Take first n elements.
   */
  take(stream: Stream, n: number): Stream;

  /**
   * Drop first n elements.
   */
  drop(stream: Stream, n: number): Stream;

  /**
   * Combine two streams.
   */
  zip(s1: Stream, s2: Stream, combine?: (a: Val, b: Val) => Val): Stream;

  /**
   * Interleave two streams.
   */
  interleave(s1: Stream, s2: Stream): Stream;

  // ─── Forcing ───

  /**
   * Force stream to list (up to n elements).
   */
  toList(stream: Stream, n: number): Val[];

  /**
   * Reduce stream (forces elements as needed).
   */
  reduce(fn: (acc: Val, v: Val) => Val, init: Val, stream: Stream, limit?: number): Val;

  /**
   * Find first matching element.
   */
  find(pred: (v: Val) => boolean, stream: Stream): Val | undefined;

  // ─── State ───

  /**
   * Get streams state.
   */
  getState(): StreamState;

  /**
   * Clear cached values.
   */
  clearCache(): void;
}
```

---

## Implementation with Memoization

```typescript
function cons(headThunk: () => Val, tailThunk: () => Stream): Stream {
  let headCached: Val | undefined;
  let tailCached: Stream | undefined;
  let headForced = false;
  let tailForced = false;

  return {
    type: 'stream',
    head: () => {
      if (!headForced) {
        headCached = headThunk();
        headForced = true;
        this.state.totalForced++;
        this.state.cachedValues++;
      }
      return headCached!;
    },
    tail: () => {
      if (!tailForced) {
        tailCached = tailThunk();
        tailForced = true;
      }
      return tailCached!;
    },
    isEmpty: false
  };
}
```

---

## Lazy Operations

```typescript
function map(fn: (v: Val) => Val, stream: Stream): Stream {
  if (stream.isEmpty) return THE_EMPTY_STREAM;

  return this.cons(
    () => fn(stream.head()),
    () => this.map(fn, stream.tail())
  );
}

function filter(pred: (v: Val) => boolean, stream: Stream): Stream {
  if (stream.isEmpty) return THE_EMPTY_STREAM;

  // Need to force head to check predicate
  const head = stream.head();

  if (pred(head)) {
    return this.cons(
      () => head,
      () => this.filter(pred, stream.tail())
    );
  } else {
    // Skip this element
    return this.filter(pred, stream.tail());
  }
}

function take(stream: Stream, n: number): Stream {
  if (n <= 0 || stream.isEmpty) return THE_EMPTY_STREAM;

  return this.cons(
    () => stream.head(),
    () => this.take(stream.tail(), n - 1)
  );
}
```

---

## Infinite Streams

```typescript
// Infinite stream of natural numbers
const naturals = this.integersFrom(1);
// naturals = 1, 2, 3, 4, 5, ...

// Infinite stream of squares
const squares = this.map(x => x * x, naturals);
// squares = 1, 4, 9, 16, 25, ...

// Filtered infinite stream
const evenSquares = this.filter(x => x % 2 === 0, squares);
// evenSquares = 4, 16, 36, 64, ...

// Force only what we need
this.toList(evenSquares, 5);
// => [4, 16, 36, 64, 100]
```

---

## Lisp Interface

```lisp
;; Create from list
(list->stream '(1 2 3 4 5))

;; Force to list
(stream->list (some-stream) 10)

;; Lazy operations
(stream-map (lambda (x) (* x x)) numbers)
(stream-filter even? numbers)
(stream-take numbers 5)

;; Infinite streams
(define naturals (integers-from 1))
(define squares (stream-map (lambda (x) (* x x)) naturals))

;; Access
(stream-car stream)
(stream-cdr stream)
(stream-null? stream)

;; Useful infinite streams
(stream-repeat 42)        ; 42, 42, 42, ...
(stream-iterate f seed)   ; seed, f(seed), f(f(seed)), ...

;; LLM streams
(define elaborations
  (stream-iterate
    (lambda (text) (effect infer.op (list "Elaborate: " text)))
    "trees"))

(stream->list elaborations 3)
; => ("trees"
;     "Trees are vital organisms..."
;     "Trees serve as the lungs of our planet...")
```

---

## Event Emission

```typescript
// When element is forced
emitter.emit('stream-force', {
  streamId,
  elementIndex,
  value
});

// When stream is created
emitter.emit('stream-created', {
  streamId,
  isInfinite
});
```

---

## Test Requirements

### Unit Tests: `tests/runtime/subsystems/StreamsManager.test.ts`
- [ ] cons() creates stream
- [ ] car() forces and returns head
- [ ] cdr() returns tail stream
- [ ] isNull() detects empty stream
- [ ] fromList() converts list
- [ ] map() is lazy
- [ ] filter() is lazy
- [ ] take() limits elements
- [ ] drop() skips elements
- [ ] toList() forces elements
- [ ] Memoization caches forced values
- [ ] integersFrom() creates infinite stream

### Integration Tests
- [ ] Infinite streams work correctly
- [ ] LLM-based streams work
- [ ] Large streams handle memory well
- [ ] Complex stream pipelines work
- [ ] Events track forcing

---

## Acceptance Criteria
1. Streams are truly lazy (elements computed on demand)
2. Infinite streams don't consume unbounded memory
3. Memoization prevents recomputation
4. All standard stream operations work
5. LLM calls can be streamed lazily
6. Performance: forcing is O(1) after first force
