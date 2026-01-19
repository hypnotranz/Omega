# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 22: Concurrent Inference
*Corresponding to SICP Section 3.4: Concurrency: Time Is of the Essence*

### 22.1 The Nature of Time in Semantic Programming

SICP Chapter 3.4 addresses a fundamental challenge: when multiple processes share state, the *order* of events matters. The same operations, interleaved differently, produce different results. SICP introduces serializers, mutexes, and careful protocols to manage concurrent access.

In semantic programming, concurrency is not merely useful—it is **essential**. LLM calls are slow, often taking seconds each. If we need to analyze 100 documents, sequential processing is prohibitive:

```
100 documents × 2 seconds per call = 200 seconds (3+ minutes)
```

With parallelism:

```
100 documents / 10 parallel workers = 20 seconds
```

But concurrent LLM access raises unique challenges. What if two calls about the same topic give inconsistent answers? How do we merge results? What does "consistency" even mean for semantic data?

### 22.2 Parallel Map for Bulk Processing

The simplest concurrency pattern: parallel map over a collection:

```lisp
; Sequential - painfully slow
(define (classify-all-seq documents)
  (map classify documents))  ; 100 docs × 2s = 200s

; Parallel - acceptably fast
(define (classify-all-par documents)
  (parallel-map classify documents))  ; 100 docs / 10 parallel = 20s
```

OmegaLLM's fiber system enables this. Fibers are lightweight concurrent tasks:

```lisp
(define (parallel-map f lst)
  (let ((fibers (map (lambda (x)
                       (effect fiber.spawn (lambda () (f x))))
                     lst)))
    (map (lambda (fib) (effect fiber.join fib)) fibers)))

; Use it
Ω> (parallel-map summarize (list "Document A..." "Document B..." "Document C..."))
=> ("Summary of A" "Summary of B" "Summary of C")
```

All three LLM calls execute concurrently. Total wall-clock time: the longest single call, not the sum.

### 22.3 The Serialization Problem

SICP emphasizes **serialization**: ensuring certain operations don't interleave dangerously. Consider concurrent updates to a knowledge base:

```lisp
; Thread 1: "Alice is Bob's manager"
; Thread 2: "Bob is Alice's manager"

; If both succeed, we have a contradiction: circular management!
```

Without protection, the interleaving might be:

```
1. Thread 1 checks: no contradiction ✓
2. Thread 2 checks: no contradiction ✓
3. Thread 1 adds: "Alice is Bob's manager"
4. Thread 2 adds: "Bob is Alice's manager"
5. Knowledge base now contradicts itself!
```

### 22.4 Serializers for Semantic Consistency

SICP introduces **serializers**: mechanisms that ensure certain operations don't overlap. The semantic equivalent:

```lisp
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (proc)
      (lambda args
        (effect mutex.acquire mutex)
        (let ((result (apply proc args)))
          (effect mutex.release mutex)
          result)))))

(define (make-serialized-kb)
  (let ((kb (make-knowledge-base))
        (protect (make-serializer)))
    (define (protected-assert fact)
      ((protect (lambda ()
        (if (contradicts? fact (kb 'facts))
            (error "Contradiction detected")
            ((kb 'assert) fact))))))
    (lambda (msg)
      (cond
        ((eq? msg 'assert) protected-assert)
        ((eq? msg 'query) (kb 'query))  ; Queries can run in parallel
        (else (kb msg))))))

(define kb (make-serialized-kb))
```

Now concurrent assertions are serialized—only one runs at a time, preventing the race condition.

### 22.5 Singleflight: Preventing Duplicate Work

When processing overlapping content, we might call the LLM multiple times for the same question. The **singleflight** pattern prevents this:

```lisp
(define (make-singleflight)
  (let ((in-flight (make-table))
        (results (make-table)))
    (lambda (key compute)
      (cond
        ; Already have result
        ((get results key) => identity)
        ; Computation in progress - wait for it
        ((get in-flight key) =>
         (lambda (fib) (effect fiber.join fib)))
        ; Start new computation
        (else
          (let ((fib (effect fiber.spawn compute)))
            (put! in-flight key fib)
            (let ((result (effect fiber.join fib)))
              (del! in-flight key)
              (put! results key result)
              result)))))))

(define sf (make-singleflight))

; Even if called concurrently, only one LLM call happens
(parallel-do
  (sf "What is the capital of France?" classify-france)
  (sf "What is the capital of France?" classify-france))
; Second call waits for and reuses first call's result
```

This is especially valuable when multiple analyses might redundantly ask similar questions.

### 22.6 Consensus and Voting

LLMs are non-deterministic—the same prompt might yield different responses. For critical decisions, use **voting**:

```lisp
(define (classify-with-consensus text n)
  (let ((votes (parallel-map
                 (lambda (_)
                   (effect infer.op
                     (list "Classify sentiment as positive/negative/neutral: " text)))
                 (range n))))
    (majority-vote votes)))

(define (majority-vote lst)
  (car (sort (group-by identity lst)
             (lambda (a b) (> (length a) (length b))))))

Ω> (classify-with-consensus "The product is okay I guess" 5)
; LLM returns: ("neutral" "positive" "neutral" "neutral" "negative")
=> "neutral"  ; 3/5 majority
```

Voting improves reliability for borderline cases at the cost of multiple LLM calls.

### 22.7 Fan-Out/Fan-In for Complex Analysis

Complex semantic tasks can be parallelized by decomposing into independent subtasks:

```lisp
(define (comprehensive-analysis document)
  ; Fan out: spawn parallel independent analyses
  (let ((sentiment-fib (effect fiber.spawn
                         (lambda () (analyze-sentiment document))))
        (topics-fib (effect fiber.spawn
                      (lambda () (extract-topics document))))
        (entities-fib (effect fiber.spawn
                        (lambda () (extract-entities document))))
        (summary-fib (effect fiber.spawn
                       (lambda () (summarize document)))))
    ; Fan in: wait for all, combine results
    (make-analysis-result
      (effect fiber.join sentiment-fib)
      (effect fiber.join topics-fib)
      (effect fiber.join entities-fib)
      (effect fiber.join summary-fib))))

Ω> (comprehensive-analysis long-article)
=> (analysis
     (sentiment . "positive")
     (topics . ("technology" "innovation" "AI"))
     (entities . ("Apple" "Tim Cook" "Silicon Valley"))
     (summary . "Apple announces new AI research initiative..."))
```

Four LLM calls execute in parallel. Total latency: approximately one call's duration.

### 22.8 Semantic Deadlock

SICP discusses **deadlock**: when processes wait for each other indefinitely. Can semantic systems deadlock? Consider two conversational agents:

```lisp
; Agent A: "What did Agent B say about the topic?"
; Agent B: "What did Agent A say about the topic?"

; Each waits for the other - deadlock!
```

Or with knowledge bases requiring cross-validation:

```lisp
(define (add-if-consistent-with kb1 kb2 fact)
  ; Acquire lock on kb1
  ; Check fact against kb2 (requires lock on kb2)
  ; If consistent, add to kb1
  ...)

; Thread 1: add-if-consistent-with kb-A kb-B "fact1"
; Thread 2: add-if-consistent-with kb-B kb-A "fact2"

; Thread 1 holds kb-A, waits for kb-B
; Thread 2 holds kb-B, waits for kb-A
; DEADLOCK
```

**Solution**: Acquire locks in a consistent global order, or use timeout-based retry:

```lisp
(define (add-with-timeout kb1 kb2 fact timeout)
  (let ((result (effect mutex.try-acquire kb1 timeout)))
    (if (not result)
        'retry
        (let ((result2 (effect mutex.try-acquire kb2 timeout)))
          (if (not result2)
              (begin (effect mutex.release kb1) 'retry)
              (begin
                ; Do the work
                (effect mutex.release kb2)
                (effect mutex.release kb1)
                'ok))))))
```

### 22.9 Semantic Speculation

Sometimes we don't know which approach will succeed. **Speculative execution** tries multiple approaches in parallel:

```lisp
(define (translate-best text target-lang)
  (let ((literal-fib (effect fiber.spawn
                       (lambda () (translate-literal text target-lang))))
        (literary-fib (effect fiber.spawn
                        (lambda () (translate-literary text target-lang))))
        (casual-fib (effect fiber.spawn
                      (lambda () (translate-casual text target-lang)))))
    ; Return first successful result
    (effect fiber.select
      (list literal-fib literary-fib casual-fib))))

; Or: return all, let user choose
(define (translate-options text target-lang)
  (parallel-map
    (lambda (style)
      (cons style (translate-with-style text target-lang style)))
    '(literal literary casual)))
```

### 22.10 Consistency Models for Semantic Data

Traditional databases have ACID guarantees. What consistency model applies to semantic data?

**Eventual consistency**: Different LLM calls might give slightly different answers, but they converge to "roughly the same meaning."

**Semantic consistency**: Facts asserted don't contradict each other, even under concurrent access.

**Causal consistency**: If fact B depends on fact A, B is only visible after A.

```lisp
(define (make-causally-consistent-kb)
  (let ((facts '())
        (version 0)
        (dependencies (make-table)))
    (lambda (msg)
      (cond
        ((eq? msg 'assert-with-deps)
         (lambda (fact required-version)
           (if (< version required-version)
               (error "Causal violation: fact depends on unseen data")
               (begin
                 (set! version (+ version 1))
                 (set! facts (cons (list version fact) facts))))))
        ((eq? msg 'query)
         (lambda (question)
           (list version
                 (effect infer.op
                   (list "Given facts: " (format-facts facts)
                         "\n" question)))))))))
```

### 22.11 Exercises

**Exercise 22.1:** Implement `(parallel-filter pred lst)` that tests all predicates concurrently. What's the speedup for filtering 50 items with a 2-second predicate?

**Exercise 22.2:** Build a "speculative translation" system that tries literal, literary, and colloquial translations in parallel and returns the first one that passes a quality check.

**Exercise 22.3:** Design a "semantic merge" operation that reconciles potentially contradictory parallel results. If one call says "positive" and another says "neutral", how do you merge? What if it's "positive" vs "negative"?

**Exercise 22.4:** Implement a knowledge base with optimistic concurrency: allow parallel additions, but detect conflicts at commit time and require resolution.

**Exercise 22.5:** Design a scenario where two conversational agents create a deadlock. Then implement a solution using timeouts.

**Exercise 22.6:** Build a "semantic barrier": a synchronization point where multiple parallel analyses must all complete before the combined result is computed. Use it to ensure a summary incorporates all extracted entities.