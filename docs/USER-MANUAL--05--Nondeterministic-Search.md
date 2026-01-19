# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 5: Nondeterministic Search
### The AMB Operator

OmegaLLM includes `amb` (ambiguous choice), a powerful operator from the Lisp tradition. It lets you explore multiple possibilities with automatic backtracking.

```lisp
; Choose from alternatives
Ω> (effect amb.choose (list (lambda () 1) (lambda () 2) (lambda () 3)))
=> 1
```

By itself, `amb.choose` just picks the first option. The magic happens when you add constraints.

### Constraints with Require

Define a `require` function that fails if a condition isn't met:

```lisp
(define (require condition)
  (if condition #t (effect amb.fail "constraint failed")))
```

Now combine them:

```lisp
Ω> (begin
     (define x (effect amb.choose (list (lambda () 1)
                                        (lambda () 2)
                                        (lambda () 3))))
     (require (> x 1))
     x)
=> 2
```

What happened:
1. `amb.choose` tried x=1
2. `(require (> x 1))` failed (1 is not > 1)
3. System backtracked, tried x=2
4. `(require (> x 1))` passed
5. Returned 2

### Semantic Constraint Satisfaction

Here's the killer feature: combine AMB with LLM predicates for *semantic search*:

```lisp
Ω> (begin
     (define x (effect amb.choose (list (lambda () "car")
                                        (lambda () "laptop")
                                        (lambda () "pizza"))))
     (require (is-edible? x))
     x)
=> "pizza"
```

The system:
1. Tried "car" → LLM said not edible → backtracked
2. Tried "laptop" → LLM said not edible → backtracked
3. Tried "pizza" → LLM said edible → returned "pizza"

This is **constraint satisfaction with semantic predicates**—something no other LLM framework provides out of the box.

### Multiple Constraints

Chain multiple semantic requirements:

```lisp
(define (is-french-word? x)
  (eq? "yes" (effect infer.op (list "Is '" x "' a French word? Answer only yes or no."))))

Ω> (begin
     (define word (effect amb.choose (list (lambda () "apple")
                                           (lambda () "pomme")
                                           (lambda () "car")
                                           (lambda () "voiture"))))
     (require (is-edible? word))
     (require (is-french-word? word))
     word)
=> "pomme"
```

Found a word that is BOTH edible AND French.