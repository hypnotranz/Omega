# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 3: Functional Composition
### Higher-Order Functions with LLMs

Since LLM functions are ordinary functions, they work with `map`, `filter`, and other higher-order functions.

First, define these utilities (they're not built-in):

```lisp
(define (map f lst)
  (if (null? lst)
      (list)
      (cons (f (car lst)) (map f (cdr lst)))))

(define (filter pred lst)
  (if (null? lst)
      (list)
      (if (pred (car lst))
          (cons (car lst) (filter pred (cdr lst)))
          (filter pred (cdr lst)))))
```

### Mapping LLM Functions

Now you can apply an LLM function to every element of a list:

```lisp
Ω> (define (to-french word)
     (effect infer.op (list "Translate to French (one word only): " word)))
=> to-french

Ω> (map to-french (list "hello" "goodbye" "thanks"))
=> ("bonjour" "au revoir" "merci")
```

Each element triggered a separate LLM call. The results came back as a list.

### Filtering with LLM Predicates

Combine `filter` with an LLM predicate:

```lisp
Ω> (define (is-food? x)
     (eq? "yes" (effect infer.op (list "Is " x " a food? Answer only yes or no."))))
=> is-food?

Ω> (filter is-food? (list "apple" "chair" "pizza" "laptop" "banana"))
=> ("apple" "pizza" "banana")
```

The LLM evaluated each item and kept only the foods.

### Pipelines

Chain operations together:

```lisp
; Filter to foods, then translate to French
Ω> (map to-french (filter is-food? (list "apple" "car" "bread" "computer")))
=> ("pomme" "pain")
```

This pipeline:
1. Filters the list, keeping only edible items (apple, bread)
2. Translates each to French

### Function Composition

Create new functions by composing existing ones:

```lisp
(define (compose f g)
  (lambda (x) (f (g x))))

(define (to-spanish text)
  (effect infer.op (list "Translate to Spanish (one word only): " text)))

; English -> French -> Spanish
(define french-to-spanish (compose to-spanish to-french))

Ω> (french-to-spanish "hello")
=> "hola"
```

The word "hello" became "bonjour" (French), then "hola" (Spanish).