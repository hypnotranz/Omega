# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 49: Semantic Caching Strategies
*OmegaLLM-Specific: Intelligent Result Caching*

### 49.1 Beyond Exact Matching

Traditional caching: exact key match. **Semantic caching**: match by meaning.

```lisp
;; Exact cache: MISS
(cache "What's 2+2?")  → stores result
(cache "What is 2+2?")  → MISS (different string)

;; Semantic cache: HIT
(semantic-cache "What's 2+2?")  → stores result
(semantic-cache "What is 2+2?")  → HIT (semantically equivalent)
(semantic-cache "Tell me the sum of 2 and 2")  → HIT (same meaning)
```

### 49.2 Similarity-Based Cache Lookup

```lisp
(define semantic-cache (make-table))

(define (semantic-cached-infer prompt)
  (let ((similar-prompt (find-similar-cached prompt semantic-cache)))
    (if (and similar-prompt
             (semantically-equivalent? prompt similar-prompt))
        (begin
          (display "Cache HIT (semantic match)")
          (lookup similar-prompt semantic-cache))
        (begin
          (display "Cache MISS - calling LLM")
          (let ((result (effect infer.op prompt)))
            (insert! prompt result semantic-cache)
            result)))))

(define (semantically-equivalent? p1 p2)
  (effect infer.op
    (list "Are these semantically equivalent? yes/no:\n"
          "A: " p1 "\n"
          "B: " p2)))
```

### 49.3 Cache with Similarity Threshold

```lisp
(define (similarity-score p1 p2)
  (effect infer.op
    (list "Similarity score 0-1:\n"
          "A: " p1 "\n"
          "B: " p2)))

(define (find-cached-similar prompt cache threshold)
  (let ((candidates (all-cached-prompts cache)))
    (find-first
      (lambda (cached-prompt)
        (>= (similarity-score prompt cached-prompt) threshold))
      candidates)))

;; Usage
Ω> (cached-infer "What is the capital of France?" cache 0.9)
;; Caches result

Ω> (cached-infer "What's France's capital city?" cache 0.9)
;; Similarity score 0.95 → Cache HIT!
```

### 49.4 Embedding-Based Caching

For efficiency, use embeddings instead of LLM comparisons:

```lisp
(define (embed-prompt prompt)
  (effect embed.op prompt))  ; Returns vector

(define (cosine-similarity v1 v2)
  (/ (dot-product v1 v2)
     (* (magnitude v1) (magnitude v2))))

(define (fast-similarity-cache prompt cache threshold)
  (let* ((prompt-embedding (embed-prompt prompt))
         (cached-entries (all-cache-entries cache))
         (similar-entry
           (find-first
             (lambda (entry)
               (>= (cosine-similarity prompt-embedding (entry-embedding entry))
                   threshold))
             cached-entries)))
    (if similar-entry
        (entry-result similar-entry)
        (let ((result (effect infer.op prompt)))
          (insert-with-embedding! prompt prompt-embedding result cache)
          result))))
```

### 49.5 Time-Based Invalidation

Cached results may become stale:

```lisp
(define (cached-with-ttl prompt cache ttl-seconds)
  (let ((entry (lookup prompt cache)))
    (if (and entry (< (entry-age entry) ttl-seconds))
        (entry-result entry)  ; Fresh cache hit
        (let ((result (effect infer.op prompt)))
          (insert-with-timestamp! prompt result (current-time) cache)
          result))))

;; News queries: short TTL
(cached-with-ttl "What's the latest tech news?" cache 3600)  ; 1 hour

;; Factual queries: long TTL
(cached-with-ttl "Who wrote Hamlet?" cache 86400)  ; 24 hours

;; Timeless facts: infinite TTL
(cached-with-ttl "What is 2+2?" cache infinity)
```

### 49.6 Context-Aware Caching

Same prompt, different context → different answers:

```lisp
(define (contextual-cache prompt context cache)
  (let ((cache-key (list prompt context)))  ; Composite key
    (or (lookup cache-key cache)
        (let ((result (effect infer.op (list prompt "Context: " context))))
          (insert! cache-key result cache)
          result))))

Ω> (contextual-cache "What does it mean?" "discussing variables" cache)
=> "In programming, 'it' refers to a variable..."

Ω> (contextual-cache "What does it mean?" "discussing movies" cache)
=> "In film, 'it' refers to the movie's theme..."
;; Different context → different cache entry
```

### 49.7 Adaptive Cache Size

```lisp
(define (adaptive-cache-policy cache)
  (cond
    ((cache-hit-rate cache < 0.3)
      ;; Low hit rate: reduce cache size, keep only frequent
      (evict-least-frequent! cache)
      (reduce-cache-size! cache))

    ((cache-hit-rate cache > 0.8)
      ;; High hit rate: increase cache size
      (increase-cache-size! cache))

    (else
      ;; Moderate hit rate: maintain current size
      (evict-lru! cache))))  ; Least Recently Used
```

### 49.8 Multi-Level Caching

```lisp
(define (multi-level-cache prompt)
  ;; Level 1: Exact match (fastest)
  (or (exact-cache-lookup prompt)

      ;; Level 2: Embedding similarity (fast)
      (embedding-cache-lookup prompt 0.95)

      ;; Level 3: LLM similarity (slower)
      (semantic-cache-lookup prompt 0.9)

      ;; Level 4: Cache miss - call LLM
      (let ((result (effect infer.op prompt)))
        (insert-all-levels! prompt result)
        result)))
```

### 49.9 Cache Statistics

```lisp
(define (cache-report cache)
  (let ((total-queries (cache-total-queries cache))
        (hits (cache-hits cache))
        (misses (cache-misses cache))
        (hit-rate (/ hits total-queries)))
    (list
      'total-queries: total-queries
      'hits: hits
      'misses: misses
      'hit-rate: hit-rate
      'cost-savings: (* hits (avg-llm-call-cost))
      'semantic-hits: (semantic-match-hits cache)
      'exact-hits: (exact-match-hits cache))))

Ω> (cache-report semantic-cache)
=> (total-queries: 1000
    hits: 750
    misses: 250
    hit-rate: 0.75
    cost-savings: $75.00
    semantic-hits: 200   ; 20% semantic matches
    exact-hits: 550)     ; 55% exact matches
```

### 49.10 Key Insights

- Semantic caching matches by meaning, not exact strings
- Similarity thresholds control cache hit criteria
- Embeddings enable fast similarity computation
- Time-based invalidation handles stale results
- Context-aware caching distinguishes ambiguous prompts
- Adaptive policies optimize cache size
- Multi-level caching balances speed and accuracy
- Dramatic cost savings in production systems

---

## Epilogue: The Future of Semantic Programming

We've journeyed from basic LLM calls (Chapter 1) to sophisticated caching strategies (Chapter 49). Along the way, we've adapted every major concept from SICP to work in semantic space:

- **Abstraction**: Semantic procedures as black boxes
- **Composition**: Combining meaning-making operations
- **State**: Conversational memory and mutable semantic structures
- **Metalinguistic abstraction**: Evaluators that invoke understanding
- **Compilation**: Optimizing semantic programs for efficiency

The principles remain timeless. The primitives—semantic effects that invoke LLM understanding—are new. Together, they create a new paradigm: **inference programming**.

Where computation once manipulated numbers and symbols, we now manipulate *meanings*. Where algorithms once processed data, we now process *understanding*. The programs we write are not just instructions for machines—they are **invocations of comprehension**.

SICP taught us how to think about computation. This manual shows how to think about **semantic computation**—programming with language models as the foundation.

**Welcome to the structure and interpretation of inference programs.**

*— End of Manual —*
