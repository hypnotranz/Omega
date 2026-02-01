# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 43: The Analyzing Evaluator
*Corresponding to SICP Section 4.1.7: Separating Syntactic Analysis from Execution*

### 43.1 Analysis vs. Execution

SICP shows: **analyze once, execute many times**. For semantic programs, analysis discovers optimization opportunities:

```lisp
(define (analyze prog)
  (list
    'parallel-ops: (find-independent-ops prog)
    'cacheable: (find-constant-prompts prog)
    'fuseable: (find-composable-ops prog)
    'dependencies: (build-dependency-graph prog)))
```

### 43.2 Dependency Analysis

```lisp
(define prog
  '(let ((trans (effect infer.op (list "Translate: " text)))
         (summ (effect infer.op (list "Summarize: " doc)))
         (trans2 (effect infer.op (list "Translate: " text))))
     (list trans summ trans2)))

Ω> (analyze-dependencies prog)

=> (parallel-ops: ((trans summ))        ; Independent, can run parallel
    cacheable: ((trans trans2))         ; Same prompt, cache
    dependencies: ((text → trans trans2)
                   (doc → summ)))
```

### 43.3 Execution with Analysis

```lisp
(define (execute-analyzed analyzed-prog env)
  (let ((parallel-pairs (get-field 'parallel-ops analyzed-prog))
        (cache-groups (get-field 'cacheable analyzed-prog)))

    ;; Run parallel ops concurrently
    (for-each (lambda (pair)
                (parallel-execute (car pair) (cadr pair) env))
              parallel-pairs)

    ;; Cache and reuse
    (for-each (lambda (group)
                (let ((result (execute-first group env)))
                  (reuse-for-rest group result env)))
              cache-groups)))
```

### 43.4 Constant Folding Analysis

```lisp
(define prog
  '(effect infer.op (list "Translate to " "French" ": " "Hello")))

Ω> (analyze prog)

=> (analysis:
     constants: ("French" "Hello")
     can-fold: #t
     folded: (effect infer.op "Translate to French: Hello"))
```

Analysis discovers all parts are constant → fold at compile time.

### 43.5 Common Subexpression Elimination

```lisp
(define prog
  '(let ((s1 (effect infer.op "Sentiment: I love it"))
         (s2 (effect infer.op "Sentiment: I love it")))
     (and (positive? s1) (positive? s2))))

Ω> (analyze prog)

=> (cse: ((effect infer.op "Sentiment: I love it") appears 2 times)
    optimized:
      (let ((s1 (effect infer.op "Sentiment: I love it")))
        (and (positive? s1) (positive? s1))))
```

### 43.6 Key Insights

- Separate analysis from execution
- Analysis discovers optimizations
- Parallelization, caching, CSE all from analysis
- Execute optimized plan

**Next:** Chapter 44 shows compiler optimizations for semantic programs!
