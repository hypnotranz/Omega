# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 25: Lazy Semantic Evaluation
### 25.1 The Promise of Delayed Understanding

SICP Chapter 4.2 introduces lazy evaluation—a paradigm where expressions are not evaluated until their values are needed. This enables working with infinite structures and avoiding unnecessary computation.

In semantic programming, lazy evaluation is **economically essential**. LLM calls are expensive. Why call the LLM for a result that might never be used?

```lisp
; EAGER: Both branches evaluated (wasteful!)
(if condition
    (effect infer.op "Expensive analysis A")   ; Called even if condition is false
    (effect infer.op "Expensive analysis B"))  ; Called even if condition is true

; LAZY: Only the needed branch evaluated
(if condition
    (delay (effect infer.op "Expensive analysis A"))   ; Thunk created, not called
    (delay (effect infer.op "Expensive analysis B")))  ; Thunk created, not called
; Only one thunk forced based on condition
```

### 25.2 Thunks: Packaged Semantic Computations

A thunk is a zero-argument function that packages a computation:

```lisp
; Create a thunk (delay evaluation)
(define (delay-inference prompt)
  (lambda () (effect infer.op prompt)))

; Force a thunk (demand evaluation)
(define (force thunk)
  (thunk))

(define lazy-summary (delay-inference "Summarize this 100-page document"))
; No LLM call yet!

Ω> (force lazy-summary)
; NOW the LLM is called
=> "This document covers..."
```

### 25.3 Memoized Thunks: Compute Once, Use Many

SICP introduces memoized thunks—once forced, they cache their result:

```lisp
(define (memo-delay computation)
  (let ((already-run? #f)
        (result #f))
    (lambda ()
      (if already-run?
          result
          (begin
            (set! result (computation))
            (set! already-run? #t)
            result)))))

(define expensive-analysis
  (memo-delay (lambda () (effect infer.op "Deep analysis of corpus"))))

Ω> (force expensive-analysis)
; LLM call happens
=> "The corpus reveals..."

Ω> (force expensive-analysis)
; NO LLM call - cached!
=> "The corpus reveals..."
```

### 25.4 Lazy Semantic Structures

We can build data structures where semantic content is computed on demand:

```lisp
(define (make-lazy-document sections)
  ; Store section texts, but summaries are computed lazily
  (map (lambda (section)
         (list section
               (memo-delay (lambda ()
                 (effect infer.op (list "Summarize: " section))))))
       sections))

(define doc (make-lazy-document
  '("Introduction text..." "Methods text..." "Results text...")))

; No summaries computed yet!

Ω> (force (cadar doc))  ; Force intro summary
=> "This paper introduces..."
; Only one LLM call, for the intro

Ω> (force (cadar (cdr doc)))  ; Force methods summary
=> "The methodology involves..."
; Second LLM call, for methods
; Results summary still not computed!
```

### 25.5 Normal Order vs Applicative Order

SICP distinguishes:
- **Applicative order**: Evaluate arguments before applying function
- **Normal order**: Evaluate arguments only when needed

For semantic programming:

```lisp
; Applicative order (eager)
(define (summarize-if-long text threshold)
  (if (> (string-length text) threshold)
      (effect infer.op (list "Summarize: " text))  ; Always evaluated!
      text))

; Normal order (lazy)
(define (summarize-if-long-lazy text threshold)
  (if (> (string-length text) threshold)
      (force (delay (effect infer.op (list "Summarize: " text))))
      text))
```

Wait—that's not quite right. The delay/force in the lazy version still gets evaluated. True normal order requires the *language itself* to be lazy:

```lisp
; In a lazy OmegaLLM dialect:
(define (summarize-if-long text threshold)
  (if (> (string-length text) threshold)
      (effect infer.op (list "Summarize: " text))
      text))
; The infer.op is only evaluated if the text is actually long
```

### 25.6 Infinite Semantic Possibilities

Laziness enables representing infinite semantic structures:

```lisp
(define (infinite-elaborations seed)
  (cons seed
        (memo-delay (lambda ()
          (infinite-elaborations
            (effect infer.op (list "Elaborate on: " seed)))))))

(define thoughts (infinite-elaborations "consciousness"))

; Take only what we need
Ω> (car thoughts)
=> "consciousness"

Ω> (car (force (cdr thoughts)))
=> "Consciousness is the state of being aware of one's surroundings..."

Ω> (car (force (cdr (force (cdr thoughts)))))
=> "Consciousness involves complex neural processes that integrate sensory information..."

; We could continue forever, but we only compute what we use
```

### 25.7 The Substitution Model Revisited

SICP's substitution model works for applicative order but breaks for lazy evaluation. Similarly, reasoning about lazy semantic programs requires thinking about *when* effects happen:

```lisp
(define a (delay (effect infer.op "Question A")))
(define b (delay (effect infer.op "Question B")))

; This determines order of LLM calls:
(begin
  (force b)   ; B called first
  (force a))  ; A called second

; vs
(begin
  (force a)   ; A called first
  (force b))  ; B called second
```

Order matters because:
1. LLM responses may vary over time
2. Rate limits may be hit
3. Context from earlier calls might influence later ones

### 25.8 Lazy Evaluation as Resource Management

The deepest insight: **lazy evaluation is about managing scarce resources**. In traditional computing, the resource is time/space. In semantic computing, the resources are:

- LLM API calls (rate-limited, costly)
- Context window tokens (finite)
- Human attention (for reviewing results)

Laziness ensures we spend these resources only when needed.

### 25.9 Exercises

**Exercise 25.1:** Implement `lazy-map` that creates a lazy list where each element's computation is deferred. Use it to create a lazy list of translations without calling the LLM until elements are accessed.

**Exercise 25.2:** Build a "semantic cache" that wraps any inference function with memoization. Test that repeated calls with the same prompt don't trigger new LLM calls.

**Exercise 25.3:** Create a lazy decision tree for text classification. Each node asks a question; children are computed lazily. Only the path actually taken triggers LLM calls.

**Exercise 25.4:** SICP shows that some programs only work with lazy evaluation (e.g., `(cons 1 (cons 2 (cons 3 ...)))` for infinite lists). Design a semantic program that *requires* laziness to terminate—one that would make infinite LLM calls under eager evaluation.

**Exercise 25.5:** Implement a "speculative" evaluator that eagerly starts computing likely-needed values in the background, but only uses them if actually needed. Compare with pure lazy evaluation.

---