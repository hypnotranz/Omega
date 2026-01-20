# JOB-009: Implement stream-interleave (Fair Stream Merge)

**Priority**: P2 - Important (Phase B)
**Estimated Effort**: 2 hours
**Skills Required**: TypeScript, Streams
**Status**: NOT STARTED

> **MUST READ**: [LOGISTICS.md](LOGISTICS.md) before starting - covers testing, file locations, and proof of completion requirements.

### Quick Start

```bash
cd c:\Users\Richa\parmenides-dev\agent-harness\OmegaLLM

# Run existing stream tests
npx vitest run test/streams/

# After implementation, run full test suite
npm test
```

---

## Source Documents

| Document | Section | Relevance |
|----------|---------|-----------|
| [LambdaRLM/lib/streams.lisp](../../LambdaRLM/lib/streams.lisp) | lines 44-59 | **PRIMARY** - Reference implementation |
| [LambdaRLM/CLAUDE-JOBS/07-STREAMS.md](../../LambdaRLM/CLAUDE-JOBS/07-STREAMS.md) | stream-interleave | Spec and test cases |
| [LambdaRLM/tests/test_streams.py](../../LambdaRLM/tests/test_streams.py) | TestStreamInterleave | Python test cases to port |

---

## Goal

Implement `stream-interleave` primitive for fair merging of two streams. This is critical for fair search - without it, one branch can starve another.

## Why This Matters

From SICP 4.3 and LambdaRLM design:
- **DFS Problem**: Without fair interleaving, stream operations can get stuck exploring one infinite branch forever
- **Fair Search**: `stream-interleave` alternates between streams, ensuring both make progress
- **Nondeterminism**: `mplus` in the list monad should use `stream-interleave` for fairness

## Requirements

### Primary: `stream-interleave`

```lisp
;; Fair merge: interleave two streams to avoid DFS-starvation
(define (stream-interleave s1 s2)
  (if (stream-null? s1)
      s2
      (stream-cons
        (stream-car s1)
        (lambda () (stream-interleave s2 (stream-cdr s1))))))
```

**Behavior**:
- Takes two streams `s1` and `s2`
- Returns a stream that alternates elements: s1[0], s2[0], s1[1], s2[1], ...
- If one stream is empty, returns the other
- Works with infinite streams (doesn't force them)

### Secondary: `stream-interleave-lazy`

```lisp
;; Fair merge with lazy second argument (takes a thunk)
(define (stream-interleave-lazy s1 s2-thunk)
  (if (stream-null? s1)
      (s2-thunk)
      (stream-cons
        (stream-car s1)
        (lambda () (stream-interleave-lazy (s2-thunk) (lambda () (stream-cdr s1)))))))
```

**Behavior**:
- Same as `stream-interleave` but second argument is a thunk
- Useful when second stream is expensive to compute

---

## Implementation

### File: `src/core/prims.ts`

Add after existing stream primitives (around line 1430):

```typescript
// stream-interleave: Fair merge of two streams
def("stream-interleave", {
  tag: "Native",
  name: "stream-interleave",
  arity: 2,
  fn: (args, s) => {
    const s1 = args[0];
    const s2 = args[1];

    // If s1 is empty, return s2
    if (s1.tag === "Unit" || (s1.tag === "Vector" && s1.items.length === 0)) {
      return { ...s, control: { tag: "Val", v: s2 } };
    }

    // s1 must be a stream pair (Vector with 2 elements: head and tail-thunk)
    if (s1.tag !== "Vector" || s1.items.length !== 2) {
      throw new Error("stream-interleave: expected stream pair");
    }

    const head = s1.items[0];
    const tail1 = s1.items[1];

    // Create new stream: (head, (lambda () (stream-interleave s2 (tail1))))
    // The interleave swaps s2 and (stream-cdr s1) for fairness
    const newTail: Val = {
      tag: "Native",
      name: "stream-interleave-thunk",
      arity: 0,
      fn: (_, st) => {
        // Force tail1 to get rest of s1
        if (tail1.tag === "Native" && tail1.arity === 0) {
          const forcedTail = tail1.fn([], st);
          if (forcedTail.control.tag === "Val") {
            const restS1 = forcedTail.control.v;
            // Recursively interleave with swapped arguments
            const interleaved = interleaveStreams(s2, restS1, st);
            return { ...st, control: { tag: "Val", v: interleaved } };
          }
        }
        // If tail is already a value
        const interleaved = interleaveStreams(s2, tail1, st);
        return { ...st, control: { tag: "Val", v: interleaved } };
      }
    };

    const result: Val = { tag: "Vector", items: [head, newTail] };
    return { ...s, control: { tag: "Val", v: result } };
  }
});

// Helper function for recursive interleave
function interleaveStreams(s1: Val, s2: Val, state: State): Val {
  if (s1.tag === "Unit" || (s1.tag === "Vector" && s1.items.length === 0)) {
    return s2;
  }
  if (s1.tag !== "Vector" || s1.items.length !== 2) {
    return s2; // fallback
  }

  const head = s1.items[0];
  const tail1 = s1.items[1];

  const newTail: Val = {
    tag: "Native",
    name: "stream-interleave-thunk",
    arity: 0,
    fn: (_, st) => {
      if (tail1.tag === "Native" && tail1.arity === 0) {
        const forcedTail = tail1.fn([], st);
        if (forcedTail.control.tag === "Val") {
          const restS1 = forcedTail.control.v;
          const interleaved = interleaveStreams(s2, restS1, st);
          return { ...st, control: { tag: "Val", v: interleaved } };
        }
      }
      const interleaved = interleaveStreams(s2, tail1, st);
      return { ...st, control: { tag: "Val", v: interleaved } };
    }
  };

  return { tag: "Vector", items: [head, newTail] };
}
```

### File: `src/core/pipeline/compileText.ts`

Add `"stream-interleave"` to the primitives list.

---

## Test Cases

### Create: `test/streams/stream-interleave.spec.ts`

```typescript
import { describe, it, expect } from "vitest";
import { evalText } from "../helpers/evalText";

describe("stream-interleave", () => {
  // HP-1: Alternates elements from two streams
  it("alternates elements from two finite streams", () => {
    const result = evalText(`
      (define s1 (stream-cons 1 (lambda () (stream-cons 2 (lambda () empty-stream)))))
      (define s2 (stream-cons 'a (lambda () (stream-cons 'b (lambda () empty-stream)))))
      (stream->list (stream-interleave s1 s2) 4)
    `);
    expect(result).toEqual([1, "a", 2, "b"]);
  });

  // HP-2: Returns other stream if one is empty
  it("returns s2 if s1 is empty", () => {
    const result = evalText(`
      (define s2 (stream-cons 1 (lambda () (stream-cons 2 (lambda () empty-stream)))))
      (stream->list (stream-interleave empty-stream s2) 2)
    `);
    expect(result).toEqual([1, 2]);
  });

  // HP-3: Returns s1 if s2 is empty
  it("returns s1 if s2 is empty", () => {
    const result = evalText(`
      (define s1 (stream-cons 1 (lambda () (stream-cons 2 (lambda () empty-stream)))))
      (stream->list (stream-interleave s1 empty-stream) 2)
    `);
    expect(result).toEqual([1, 2]);
  });

  // HP-4: Works with infinite streams
  it("works with infinite streams (takes first 6)", () => {
    const result = evalText(`
      (define (ones) (stream-cons 1 ones))
      (define (twos) (stream-cons 2 twos))
      (stream->list (stream-interleave (ones) (twos)) 6)
    `);
    expect(result).toEqual([1, 2, 1, 2, 1, 2]);
  });

  // HP-5: Handles different length streams
  it("handles streams of different lengths", () => {
    const result = evalText(`
      (define s1 (stream-cons 1 (lambda () empty-stream)))
      (define s2 (stream-cons 'a (lambda () (stream-cons 'b (lambda () (stream-cons 'c (lambda () empty-stream)))))))
      (stream->list (stream-interleave s1 s2) 4)
    `);
    expect(result).toEqual([1, "a", "b", "c"]);
  });

  // EC-1: Empty with empty
  it("returns empty when both streams are empty", () => {
    const result = evalText(`
      (stream-null? (stream-interleave empty-stream empty-stream))
    `);
    expect(result).toBe(true);
  });
});
```

---

## Acceptance Criteria

- [ ] `stream-interleave` primitive exists and is registered
- [ ] Alternates elements fairly between two streams
- [ ] Works with infinite streams (doesn't block)
- [ ] Returns other stream when one is empty
- [ ] All new tests pass
- [ ] All existing tests still pass
- [ ] TypeScript builds clean

---

## Verification

After implementation, verify with:

```bash
# Build
npm run build

# Run stream tests
npx vitest run test/streams/

# Run full test suite
npm test
```

Expected: All tests pass, including new stream-interleave tests.

---

## Completion Checklist

1. [ ] Read this document fully
2. [ ] Implement `stream-interleave` in prims.ts
3. [ ] Add to compileText.ts primitives list
4. [ ] Create test file with all test cases
5. [ ] Run tests: `npx vitest run test/streams/`
6. [ ] Run full suite: `npm test`
7. [ ] Update status: Change `NOT STARTED` → `DONE` in this file

---

*Created: 2026-01-19*
*Source: LambdaRLM/lib/streams.lisp*
*Related: [007-FULL-PROVENANCE-SYSTEM.md](./007-FULL-PROVENANCE-SYSTEM.md), [008-SEARCH-PATTERNS-SOLVERS.md](./008-SEARCH-PATTERNS-SOLVERS.md)*
