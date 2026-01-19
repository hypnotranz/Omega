# Stream Module (Prompt 16)

Streams + Laziness as a Derived Language following SICP stream model.

## Overview

This module provides:
- **Memoized Promises**: First-class promises with force-once semantics
- **Stream Primitives**: SICP-style lazy streams (cons-stream, stream-car, stream-cdr)
- **Stream Combinators**: map, filter, take, drop, append, flatMap, zip, fold
- **Receipt-backed Staging**: Hydration of stream segments via receipts
- **Intensional Analysis**: Strictness, productivity, fusion, space leak detection

## Architecture

```
stream/
├── types.ts      # Type definitions for promises, streams, and analysis
├── promise.ts    # Memoized promise implementation with singleflight
├── stream.ts     # Stream primitives and combinators
├── analysis.ts   # Strictness, productivity, fusion, leak analysis
└── index.ts      # Module exports
```

## Key Concepts

### Memoized Promises

Promises have three states:
- **Unforced**: Contains a thunk waiting to be evaluated
- **Forcing**: Currently being evaluated (singleflight coordination)
- **Forced**: Evaluation complete, value memoized

```typescript
const ctx = createStreamContext();
const promise = createPromise(ctx, thunk);
const result = forceSync(ctx, promise, evaluator);
// Second force returns cached value
const cached = forceSync(ctx, promise, evaluator);
```

### Lazy Streams

Streams are either empty or a head with a lazy tail:

```typescript
const s = consStream(
  ctx,
  { tag: "Int", value: 1n },
  () => consStream(ctx, { tag: "Int", value: 2n }, () => emptyStream())
);

const head = streamCar(s);  // { tag: "Int", value: 1n }
const tail = streamCdr(ctx, s);  // Forces tail promise
```

### Stream Combinators

```typescript
// Transform stream elements
const doubled = streamMap(ctx, s, x => ({ tag: "Int", value: x.value * 2n }));

// Filter stream elements
const evens = streamFilter(ctx, s, x => x.value % 2n === 0n);

// Take first n elements
const first5 = streamTake(ctx, s, 5);

// Infinite streams
const ones = streamRepeat(ctx, { tag: "Int", value: 1n });
const nats = streamIterate(ctx, { tag: "Int", value: 0n }, n => ({
  tag: "Int",
  value: n.value + 1n
}));
```

### Receipt-backed Staging

Materialize stream segments for persistence:

```typescript
const segment = materializeSegment(ctx, stream, 10);
const receipt = createStreamReceipt(segment);
// Later: hydrate from receipt
const restored = hydrateFromReceipt(ctx, receipt, segment, () => continuation);
```

### Analysis

```typescript
const analysis = analyzeStream(ctx, stream, {
  strictness: true,
  productivity: true,
  fusion: true,
  spaceLeaks: true
});

// Strictness: How many elements were demanded vs forced?
analysis.strictness?.forcedAhead  // true if forced beyond demand

// Productivity: Does stream terminate under bounded fuel?
analysis.productivity?.productive  // true if productive

// Fusion: Are there optimization opportunities?
analysis.fusionCandidates  // CSE, map-map, filter-map candidates

// Space leaks: Is memory growing unexpectedly?
analysis.spaceLeaks?.leakSuspected  // true if leak detected
```

## Event System

All operations emit events for debugging and replay:

```typescript
const ctx = createStreamContext({ logging: true });

// After operations...
ctx.events  // Array of PromiseEvent

// Event types:
// - PromiseCreated: New promise registered
// - PromiseForceStart: Force began
// - PromiseForceHit: Cache hit (memoization working)
// - PromiseForceDone: Force completed with result
// - PromiseForceJoin: Another fiber joined singleflight
```

## Tests

See `test/prompt16-stream/stream.spec.ts` for:
- Test 16.1: Laziness (no force until needed)
- Test 16.2: Memoization (force-once semantics)
- Test 16.3: Receipt hydration
- Test 16.4: Strictness analysis
- Test 16.5: Productivity analysis
- Test 16.6: Stream fusion / CSE
- Test 16.7: Space leak detection
- Test 16.8: Event replay

## Configuration

```typescript
const config: StreamConfig = {
  maxFuel: 1000,        // Fuel for bounded execution
  logging: true,        // Enable event logging
  memoization: true,    // Enable promise memoization
  singleflight: true,   // Enable concurrent force coordination
};

const ctx = createStreamContext(config);
```

## Performance Considerations

1. **Memoization**: Promises are forced at most once
2. **Singleflight**: Concurrent forces share computation
3. **Fusion**: CSE and map-map fusion reduce oracle calls
4. **Fuel**: Bounded execution prevents infinite loops
