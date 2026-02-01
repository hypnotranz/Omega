# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 32: General Methods: Fixpoint and Root-Finding
*Corresponding to SICP Section 1.3.3: Procedures as General Methods*

### 32.1 From Specific to General

SICP introduces a profound idea: many apparently different computational problems share the same **underlying structure**. Square roots, cube roots, and fixed points all use the same iterative pattern—only the improvement function changes.

This abstraction lets us write **general-purpose solvers** that work across domains. In inference programming, these general methods become even more powerful because the LLM can guide convergence semantically.

### 32.2 Fixpoint Computation

A **fixpoint** of a function f is a value x where f(x) = x—applying f doesn't change it.

SICP shows how to find fixpoints numerically:

```scheme
(define (fixpoint f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
```

For semantic operations, "close enough" isn't about numerical tolerance—it's about **semantic equivalence**:

```lisp
(define (semantic-fixpoint f start max-iterations)
  (define (close-enough? v1 v2)
    (equal? "yes"
      (effect infer.op
        (list "Are these semantically equivalent? yes or no:\n"
              "A: " v1 "\n"
              "B: " v2))))

  (define (iter guess count)
    (let ((next (f guess)))
      (if (or (close-enough? guess next)
              (= count max-iterations))
          next
          (iter next (+ count 1)))))

  (iter start 0))
```

Now we can find fixpoints in **meaning space**.

### 32.3 Example: Converging to Stable Phrasing

Consider repeatedly making text more concise:

```lisp
(define (make-concise text)
  (effect infer.op (list "Make this more concise while preserving meaning: " text)))

Ω> (semantic-fixpoint make-concise
     "The quick brown fox jumped over the extremely lazy dog"
     5)

; Iteration 1: "The quick brown fox jumped over the extremely lazy dog"
;           → "The quick fox jumped over the lazy dog"
; Iteration 2: "The quick fox jumped over the lazy dog"
;           → "Quick fox jumped over lazy dog"
; Iteration 3: "Quick fox jumped over lazy dog"
;           → "Fox jumped over dog"
; Iteration 4: "Fox jumped over dog"
;           → "Fox jumped over dog"
; Converged! (semantically equivalent to previous)

=> "Fox jumped over dog"
```

The text **converges** to a minimal form where further simplification produces no semantic change.

### 32.4 The Golden Ratio via Fixpoint

SICP famously shows computing the golden ratio as a fixpoint of f(x) = 1 + 1/x.

We can do this semantically:

```lisp
(define (semantic-golden-ratio)
  (semantic-fixpoint
    (lambda (x)
      (effect infer.op
        (list "What is 1 + 1/" x "? Give numeric answer.")))
    "1.0"
    10))

Ω> (semantic-golden-ratio)
; Iteration 1: "1.0" → "2.0"
; Iteration 2: "2.0" → "1.5"
; Iteration 3: "1.5" → "1.666..."
; Iteration 4: "1.666..." → "1.6"
; Iteration 5: "1.6" → "1.625"
; Iteration 6: "1.625" → "1.615..."
; Converged to approximately 1.618 (golden ratio)

=> "1.618"
```

The LLM performs arithmetic iteratively until the value stabilizes!

### 32.5 Damping for Convergence

SICP shows that some functions need **damping** to converge. Instead of f(x), we use:

average-damp(f)(x) = average(x, f(x)) = (x + f(x)) / 2

This slows down change, preventing oscillation.

Semantic version:

```lisp
(define (average-damp f)
  (lambda (x)
    (effect infer.op
      (list "Give a value halfway between: " x
            " and: " (f x)))))

(define (damped-fixpoint f start max-iter)
  (semantic-fixpoint (average-damp f) start max-iter))
```

Example use:

```lisp
; Without damping: might oscillate between extremes
(semantic-fixpoint too-aggressive "start" 10)

; With damping: smooth convergence
(damped-fixpoint too-aggressive "start" 10)
```

### 32.6 Newton's Method for Semantic Roots

SICP shows Newton's method for finding roots: given f, find x where f(x) = 0.

Newton's formula: x_new = x - f(x)/f'(x)

In semantic space, we can approximate this:

```lisp
(define (semantic-root f initial-guess target)
  (define (improvement guess)
    (effect infer.op
      (list "Given that f(" guess ") = " (f guess)
            ", and we want f(x) = " target
            ", what should we try next for x?")))

  (semantic-fixpoint improvement initial-guess 10))
```

The LLM acts as the derivative—it suggests how to adjust the guess!

Example: Finding the right tone:

```lisp
(define (tone-mismatch text)
  ; "Distance" from target tone
  (effect infer.op
    (list "How far is this from professional tone (0-10)? " text)))

(define (find-professional-tone initial-text)
  (semantic-root tone-mismatch initial-text "0"))

Ω> (find-professional-tone "yo dude whats up")
; LLM suggests adjustments to reduce tone mismatch
; Converges to professionally-toned text

=> "Hello, how may I assist you?"
```

### 32.7 Abstractions Over Iterative Methods

All these methods share a pattern:

```lisp
(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (define (iter current)
      (if (good-enough? current)
          current
          (iter (improve current))))
    (iter guess)))
```

Now we can define fixpoint in terms of this abstraction:

```lisp
(define (fixpoint-via-improve f start)
  ((iterative-improve
     (lambda (guess) (close-enough? guess (f guess)))
     f)
   start))
```

And square root:

```lisp
(define (sqrt-via-improve x)
  ((iterative-improve
     (lambda (guess) (good-enough? guess x))
     (lambda (guess) (average guess (/ x guess))))
   1.0))
```

**This is the power of abstraction**: one pattern, infinitely many uses.

### 32.8 Semantic Iterative Improvement

For inference programming, we have a semantic version:

```lisp
(define (semantic-improve good-enough? improve max-iter)
  (lambda (start)
    (define (iter guess count)
      (if (or (good-enough? guess) (= count max-iter))
          guess
          (iter (improve guess) (+ count 1))))
    (iter start 0)))
```

Now define specific solvers:

```lisp
; Fixpoint finder
(define fixpoint-semantic
  (semantic-improve
    (lambda (g) (semantically-equivalent? g (improve g)))
    improve
    10))

; Quality improver (from Chapter 29)
(define quality-improver
  (semantic-improve
    (lambda (g) (meets-criteria? g criteria))
    (lambda (g) (improve-based-on-feedback g))
    5))

; Tone optimizer
(define tone-optimizer
  (semantic-improve
    (lambda (g) (has-target-tone? g))
    (lambda (g) (adjust-tone g))
    7))
```

All use the same underlying pattern, just with different `good-enough?` and `improve` functions!

### 32.9 Combining Methods: Semantic Search

We can combine fixpoint finding with search:

```lisp
(define (semantic-search space goal max-attempts)
  ; Use fixpoint to find stable search direction
  (define (search-direction current)
    (effect infer.op
      (list "To get from " current " to " goal
            ", what's the next step?")))

  (define (moving-toward-goal? current next)
    (effect infer.op
      (list "Is " next " closer to " goal " than " current "? yes/no")))

  (define (iter current count)
    (let ((next (search-direction current)))
      (if (or (equal? next goal)
              (not (moving-toward-goal? current next))
              (= count max-attempts))
          next
          (iter next (+ count 1)))))

  (iter (first space) 0))
```

This combines:
- Fixpoint iteration (the search loop)
- Semantic guidance (LLM suggests next step)
- Goal checking (LLM evaluates progress)

### 32.10 Practical Example: API Design Refinement

```lisp
(define (refine-api-design initial-design constraints)
  (define (evaluate-design design)
    (effect infer.op
      (list "Rate this API design (0-10) against constraints:\n"
            "Design: " design "\n"
            "Constraints: " constraints)))

  (define (good-design? design)
    (>= (evaluate-design design) 8))

  (define (improve-design design)
    (let ((critique
           (effect infer.op
             (list "What's the biggest flaw in this API design?\n"
                   design "\n"
                   "Constraints: " constraints))))
      (effect infer.op
        (list "Improve this API design to address: " critique "\n"
              "Current design: " design))))

  ((semantic-improve good-design? improve-design 10)
   initial-design))
```

Usage:

```lisp
Ω> (refine-api-design
     "GET /users, POST /users, PUT /users/:id"
     "RESTful, consistent naming, support pagination")

; Iteratively improves until design scores 8+/10
=> "GET /users?page=1&limit=10,
    POST /users,
    PATCH /users/:id,
    DELETE /users/:id"
```

### 32.11 Convergence Guarantees

SICP discusses when fixpoint iteration converges. For semantic methods:

**Converges when:**
- The improvement function is **contractive** (brings values closer)
- The semantic space has a natural **metric** (distance measure)
- The LLM's judgments are **consistent** across iterations

**May not converge when:**
- Improvement oscillates between alternatives
- Semantic equivalence is too strict (nothing ever "close enough")
- LLM suggestions are random/contradictory

**Mitigation:**
- Add damping
- Loosen semantic equivalence check
- Set maximum iterations

### 32.12 Exercises

**Exercise 32.1:** Implement `(continued-fraction n d k)` where n and d are functions that produce numerator and denominator sequences. Use it to compute e (Euler's number) semantically by having the LLM generate the sequence.

**Exercise 32.2:** SICP shows that sqrt(x) = fixpoint(y ↦ average(y, x/y)). Implement this in semantic space where the LLM performs the averaging and division.

**Exercise 32.3:** Create `(semantic-bisection f a b)` that finds a root of f between a and b using LLM-guided bisection. The LLM evaluates f and suggests which half to search.

**Exercise 32.4:** Implement `(semantic-gradient-descent f start)` where the LLM acts as the gradient—it suggests which direction moves toward a minimum of f.

**Exercise 32.5:** Design `(multi-objective-improve goals improve)` that finds a fixpoint satisfying multiple semantic criteria simultaneously. How do you handle trade-offs?

**Exercise 32.6:** Create `(adaptive-damping f start)` that dynamically adjusts damping factor based on whether the sequence is oscillating or converging smoothly.

**Exercise 32.7:** Implement `(semantic-attractor f starts)` that tries multiple starting points and finds which attractors (stable fixpoints) they converge to. Use this to explore a semantic space's "basins of attraction."

---

**Key Insights:**
- Fixpoints exist in semantic space: f(x) = x for meanings
- Semantic equivalence replaces numerical tolerance
- Damping prevents oscillation in iterative semantic improvement
- Newton's method works with LLM as derivative
- All iterative methods share one abstract pattern
- Combining general methods creates powerful semantic search
- Convergence requires consistency in LLM judgments

**Next:** Chapter 33 explores hierarchical semantic structures—trees of meaning!
