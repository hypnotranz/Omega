# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 12: Inference Processes — Recursion and Iteration in Semantic Space
### 12.1 The Shape of Semantic Processes

SICP Chapter 1.2 reveals that procedures generate *processes*—patterns of execution that unfold over time. A recursive procedure may generate a recursive process (building up deferred operations) or an iterative process (carrying state forward). Understanding this distinction is crucial for writing efficient programs.

In semantic programming, the same distinction applies, but with a twist: **the processes we generate involve LLM calls**, and each call has cost—latency, money, and context window consumption. Understanding how inference processes unfold is essential for building systems that scale.

### 12.2 Linear Recursive Inference

Consider a procedure that "chains" understanding—each step builds on the previous:

```lisp
(define (explain-chain concept depth)
  (if (= depth 0)
      concept
      (effect infer.op
        (list "Explain this more simply: "
              (explain-chain concept (- depth 1))))))

; This generates a LINEAR RECURSIVE process:
; (explain-chain "quantum entanglement" 3)
;   → (explain (explain (explain "quantum entanglement")))
;   → Each explain waits for inner result
```

The process shape:
```
explain-chain(concept, 3)
  ↓ waits for...
  explain-chain(concept, 2)
    ↓ waits for...
    explain-chain(concept, 1)
      ↓ waits for...
      explain-chain(concept, 0) → returns concept
    ← simplifies result
  ← simplifies result
← simplifies result
```

Three LLM calls, but they're **sequential**—each waits for the previous. Total time = 3 × (LLM latency).

### 12.3 Linear Iterative Inference

Now consider an iterative version that carries state forward:

```lisp
(define (explain-iter concept depth result)
  (if (= depth 0)
      result
      (explain-iter concept
                    (- depth 1)
                    (effect infer.op
                      (list "Simplify: " result)))))

(define (explain-iterative concept depth)
  (explain-iter concept depth concept))
```

The process shape looks similar in total work, but there's a key difference: **no deferred operations**. At each step, we have the complete state. This matters for error recovery—if an LLM call fails, we can resume from the current state.

### 12.4 Tree Recursive Inference

Consider analyzing a text by breaking it into parts:

```lisp
(define (analyze-tree text)
  (if (short-enough? text)
      (effect infer.op (list "Analyze: " text))
      (let ((parts (split-in-half text)))
        (combine-analyses
          (analyze-tree (car parts))
          (analyze-tree (cadr parts))))))
```

This generates a **tree recursive process**:
```
              analyze(full-text)
             /                  \
    analyze(first-half)    analyze(second-half)
       /      \               /        \
    ...       ...           ...        ...
```

The key insight: **the branches are independent**. Unlike linear recursion, we can parallelize tree recursion. With proper concurrency (Chapter 22), tree recursive inference can be dramatically faster than linear.

### 12.5 Orders of Growth in LLM Calls

SICP introduces order of growth—how resource consumption scales with input size. For semantic processes:

| Process Type | LLM Calls | Time (sequential) | Time (parallel) |
|-------------|-----------|-------------------|-----------------|
| Linear recursive | O(n) | O(n) | O(n) |
| Linear iterative | O(n) | O(n) | O(n) |
| Tree recursive | O(2^n) | O(2^n) | O(n) |

Tree recursion seems expensive, but parallelization transforms O(2^n) time into O(n) depth. This is why understanding process shape matters.

### 12.6 Memoization for Semantic Efficiency

SICP shows how memoization can transform tree recursion (Fibonacci) from exponential to linear. In semantic programming, memoization is critical because LLM calls are expensive:

```lisp
(define semantic-cache (make-table))

(define (memoized-analyze text)
  (let ((cached (get semantic-cache text)))
    (if cached
        cached
        (let ((result (effect infer.op (list "Analyze: " text))))
          (put! semantic-cache text result)
          result))))
```

When analyzing overlapping text segments, memoization prevents redundant LLM calls.

### 12.7 The Cost Model for Inference

In traditional computing, we count operations. In inference programming, we count:

1. **LLM calls**: Each call has latency (~1-5 seconds) and cost (~$0.01-0.10)
2. **Token consumption**: Input and output tokens affect cost
3. **Context utilization**: Long contexts are expensive

A well-designed inference process minimizes all three while maximizing semantic value.

### 12.8 Exercises

**Exercise 12.1:** Write two versions of `(summarize-deeply text n)`—one linear recursive, one linear iterative. Trace the process shapes. Which is easier to debug if an LLM call fails at step 3 of 5?

**Exercise 12.2:** Implement tree recursive document analysis that splits a document into paragraphs, analyzes each, and combines results. Then add parallelization (using concepts from Chapter 22) and measure the speedup.

**Exercise 12.3:** The Fibonacci function has famously poor tree recursive performance. Design a "semantic Fibonacci"—where fib(n) is "the nth elaboration of an idea, building on the two previous elaborations." How do you memoize semantic content?

**Exercise 12.4:** SICP discusses the difference between a procedure and the process it generates. Can two different inference procedures generate identical semantic processes? Design an experiment to test this.

---