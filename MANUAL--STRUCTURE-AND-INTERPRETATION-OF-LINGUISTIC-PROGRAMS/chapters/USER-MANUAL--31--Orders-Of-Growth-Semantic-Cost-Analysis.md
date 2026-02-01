# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 31: Orders of Growth: Semantic Cost Analysis
*Corresponding to SICP Section 1.2.3: Orders of Growth*

### 31.1 Computational Complexity Revisited

SICP introduces **orders of growth** to analyze algorithm efficiency. For computing n!, we compare:

- **Recursive**: O(n) space, O(n) time
- **Iterative**: O(1) space, O(n) time

The notation O(n) means "grows proportionally to n." As n doubles, resource usage doubles.

This framework applies universally—but for inference programming, we measure **different resources**.

### 31.2 The Cost Model for Semantic Operations

Traditional algorithms use **time** and **space** as resources. For LLM operations, the critical resources are:

1. **Token costs** (money): Each API call costs based on input/output tokens
2. **Latency** (time): Network round-trips take seconds, not microseconds
3. **Rate limits**: Maximum calls per minute/hour

**Token costs dominate**. They're:
- **Predictable**: Proportional to prompt length
- **Cumulative**: Each call adds to total cost
- **Optimizable**: Reduce calls = reduce cost

We analyze algorithms by **counting LLM calls** and **measuring token usage**.

### 31.3 Example: Linear vs. Quadratic Costs

Consider two approaches to comparing items:

**Approach 1: Pairwise comparison (O(n²))**

```lisp
(define (analyze-all-pairs items)
  (map (lambda (item1)
         (map (lambda (item2)
                (effect infer.op
                  (list "Compare: " item1 " vs " item2)))
              items))
       items))
```

For n=10 items:
- LLM calls: 10 × 10 = **100 calls**
- If each call costs $0.01: **$1.00 total**

For n=100 items:
- LLM calls: 100 × 100 = **10,000 calls**
- Cost: **$100.00**

**Approach 2: Batch with summary (O(n))**

```lisp
(define (analyze-with-summary items)
  (let ((analyses
         (map (lambda (item)
                (effect infer.op (list "Analyze: " item)))
              items)))
    (effect infer.op
      (list "Summarize these analyses: " analyses))))
```

For n=10 items:
- LLM calls: 10 + 1 = **11 calls**
- Cost: **$0.11**

For n=100 items:
- LLM calls: 100 + 1 = **101 calls**
- Cost: **$1.01**

**The difference is dramatic**: O(n²) vs O(n) means $100 vs $1 at n=100!

### 31.4 Orders of Growth for Common Patterns

| Pattern | LLM Calls | Example |
|---------|-----------|---------|
| **O(1)** — Constant | 1 call | Single question |
| **O(n)** — Linear | n calls | Map over list |
| **O(n + k)** — Linear with summary | n + k calls | Map + k consolidations |
| **O(n log n)** — Divide & conquer | n log n calls | Hierarchical summarization |
| **O(n²)** — Quadratic | n² calls | All pairs comparison |
| **O(b^d)** — Exponential | b^d calls | Tree recursion (branch b, depth d) |

**Example: Summarizing 1000 documents**

- **O(n)**: 1000 calls (summarize each individually) = $10
- **O(n + log n)**: 1000 + 10 calls (tree summary) = $10.10
- **O(n²)**: 1,000,000 calls (compare all pairs) = $10,000

The last approach is **1000× more expensive**!

### 31.5 Optimizing: From O(n²) to O(n log n)

SICP shows how merge sort improves on bubble sort. Similarly, we can optimize semantic operations.

**Problem**: Cluster 100 documents by similarity.

**Naive approach** (O(n²)):
```lisp
(define (cluster-naive docs)
  ;; Compare every document to every other document
  (let ((similarity-matrix
         (map (lambda (doc1)
                (map (lambda (doc2)
                       (effect infer.op
                         (list "Similarity of " doc1 " and " doc2 " (0-1)")))
                     docs))
              docs)))
    (find-clusters similarity-matrix)))

;; Cost: 100² = 10,000 LLM calls
```

**Optimized approach** (O(n log n)):
```lisp
(define (cluster-hierarchical docs)
  ;; First, summarize each document
  (let ((summaries
         (map (lambda (doc)
                (effect infer.op (list "Summarize key themes: " doc)))
              docs)))
    ;; Then cluster summaries (cheaper than full docs)
    (let ((clusters
           (effect infer.op
             (list "Group these summaries into 5 clusters: " summaries))))
      clusters)))

;; Cost: 100 + 1 = 101 LLM calls (99× cheaper!)
```

The optimization: **summarize first, then group**. Summaries are shorter, and we ask the LLM to cluster in one call instead of computing all pairwise similarities.

### 31.6 Token Complexity: Prompt Length Matters

Beyond counting calls, we must consider **token counts**:

```lisp
(define (bad-summary items)
  ;; Concatenate everything into one huge prompt
  (effect infer.op
    (list "Summarize all of these: " items)))
;; Calls: O(1)
;; Tokens: O(n × item-length) — could hit context limits!
```

vs.

```lisp
(define (good-summary items)
  ;; Hierarchical: summarize in groups, then summarize summaries
  (let* ((groups (partition items 10))  ; Groups of 10
         (group-summaries
          (map (lambda (group)
                 (effect infer.op (list "Summarize: " group)))
               groups))
         (final-summary
          (effect infer.op
            (list "Summarize: " group-summaries))))
    final-summary))
;; Calls: O(n/k + 1)
;; Tokens: O(n) but distributed, stays under limits
```

The hierarchical approach both **reduces token counts per call** and **scales to arbitrary n**.

### 31.7 Practical Example: Sentiment Analysis at Scale

**Task**: Analyze sentiment of 1000 customer reviews.

**Approach 1: Naive sequential** — O(n) calls, O(n × review-length) tokens
```lisp
(map (lambda (review)
       (effect infer.op (list "Sentiment (positive/negative/neutral): " review)))
     reviews)
;; 1000 calls, ~500 tokens each = 500k tokens
;; Cost: ~$5
```

**Approach 2: Batch API** — O(1) call, O(n × review-length) tokens
```lisp
(effect infer.op
  (list "For each review, output sentiment:\n" reviews))
;; 1 call, 500k tokens in one request
;; Cost: ~$5 (same tokens, fewer API overhead)
;; But: hits context limit if reviews are long!
```

**Approach 3: Sample + infer** — O(k) calls, O(k × review-length) tokens
```lisp
(let* ((sample (take 100 reviews))  ; Sample 10%
       (sentiments (map analyze-sentiment sample))
       (overall-sentiment
        (effect infer.op
          (list "Overall sentiment from sample: " sentiments))))
  overall-sentiment)
;; 100 + 1 = 101 calls, 50k tokens
;; Cost: ~$0.50 (10× cheaper, reasonable approximation)
```

**Approach 4: Hierarchical clustering** — O(n + √n) calls
```lisp
(let* ((clusters
        (cluster-by-keywords reviews 10))  ; Group by keywords (cheap)
       (cluster-sentiments
        (map (lambda (cluster)
               (effect infer.op
                 (list "Sentiment of cluster: " (take 10 cluster))))
             clusters)))
  cluster-sentiments)
;; 10 cluster calls + keyword extraction
;; Cost: depends on implementation, likely < $1
```

### 31.8 The Trade-off: Accuracy vs. Cost

Lower complexity often means **approximation**:

| Approach | Complexity | Cost | Accuracy |
|----------|-----------|------|----------|
| All reviews | O(n) | $5 | 100% |
| Sample | O(k) | $0.50 | ~95% |
| Clusters | O(√n) | $1 | ~98% |

This is a fundamental trade-off in semantic computing: **exact answers are expensive**.

SICP emphasizes that **different problems have different acceptable costs**. For inference programming, the question becomes: *How much accuracy do you need, and what will you pay for it?*

### 31.9 Asymptotic Analysis: When Does It Matter?

For small n (< 10), complexity doesn't matter much:
- O(n²) with n=10: 100 calls ≈ $1
- O(n) with n=10: 10 calls ≈ $0.10

But complexity **dominates at scale**:

| n | O(n) | O(n log n) | O(n²) | O(2^n) |
|---|------|------------|-------|---------|
| 10 | 10 | 33 | 100 | 1,024 |
| 100 | 100 | 664 | 10,000 | ∞ (infeasible) |
| 1,000 | 1,000 | 9,966 | 1,000,000 | ∞ |
| 10,000 | 10,000 | 132,877 | 100,000,000 | ∞ |

At n=10,000:
- O(n): $100
- O(n log n): $1,329
- O(n²): $1,000,000
- O(2^n): Not computable

**The lesson**: For prototype-scale (n < 100), don't worry. For production-scale (n > 1000), **complexity is everything**.

### 31.10 Measuring Real Costs

Theory is great, but measure real costs:

```lisp
(define (with-cost-tracking thunk)
  (let ((start-tokens (get-total-tokens)))
    (let ((result (thunk)))
      (let ((end-tokens (get-total-tokens)))
        (begin
          (display (list "Tokens used:" (- end-tokens start-tokens)))
          (newline)
          result)))))

(with-cost-tracking
  (lambda ()
    (analyze-all-pairs items)))
;; Output: Tokens used: 127,450
```

Or use the budget system:

```lisp
(with-budget 10000  ; 10k token budget
  (lambda ()
    (expensive-operation)))
;; Throws error if budget exceeded
```

### 31.11 Advanced: Amortized Analysis

Some operations have variable cost that **amortizes** over time.

**Semantic caching**:
```lisp
(define cache (make-hash))

(define (cached-infer prompt)
  (or (hash-ref cache prompt #f)
      (let ((result (effect infer.op prompt)))
        (hash-set! cache prompt result)
        result)))
```

**First call**: O(1) LLM call (expensive)
**Subsequent calls**: O(1) cache lookup (free)

**Amortized cost**: If we call the same prompt k times, total cost is 1 LLM call across k invocations, so **amortized O(1/k) → O(0)**.

Caching makes repeated operations effectively free!

### 31.12 Exercises

**Exercise 31.1:** Analyze the complexity of:
```lisp
(map (lambda (doc)
       (effect infer.op (list "Keywords from: " doc)))
     documents)
```
If each document is k tokens, what's the total token count? What if we batch all documents into one call?

**Exercise 31.2:** You have 500 customer emails to route (support/sales/technical). Compare:
1. Individual classification (O(n))
2. Batch classification (O(1) but large prompt)
3. Rule-based filtering + LLM for edge cases

Estimate costs and accuracy for each.

**Exercise 31.3:** Implement hierarchical summarization that reduces 1000 documents to a single summary. Target O(n) calls but O(log n) depth. What's the grouping factor at each level?

**Exercise 31.4:** A naive implementation of document clustering is O(n²). Design an O(n log n) algorithm using:
- Initial embedding/hashing (cheap)
- Locality-sensitive hashing
- LLM only for final cluster refinement

**Exercise 31.5:** Create `(profile-complexity f inputs)` that runs `f` on inputs of increasing size and plots LLM calls vs. input size. Use it to determine empirically whether an algorithm is O(n), O(n log n), or O(n²).

**Exercise 31.6:** Tree exploration from Chapter 30 is O(b^d). Design a cost-limited version that stops when a budget is reached. What strategy should it use—breadth-first, depth-first, or adaptive?

---

**Key Insights:**
- Token costs are the primary resource metric for semantic operations
- O(n²) algorithms become prohibitively expensive at scale
- Hierarchical and sampling strategies reduce complexity
- Trade accuracy for cost when appropriate
- Caching amortizes repeated operations to near-zero cost
- Measure real costs, don't just analyze asymptotically
- Complexity matters at production scale (n > 1000)

**Next:** Chapter 32 explores general problem-solving methods like fixpoint iteration and golden ratio search in semantic space!
