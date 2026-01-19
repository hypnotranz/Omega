# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 2: LLM Calls as Functions
### The Basic LLM Call

In OmegaLLM, calling a language model is an *effect*—a controlled side-effect that the runtime manages. The simplest form is `infer.op`:

```lisp
Ω> (effect infer.op "What is the capital of France?")
=> "Paris"
```

That's it. The string is sent to the LLM, and the response comes back as a value.

### Building Prompts with Lists

For dynamic prompts, use `list` to concatenate strings:

```lisp
Ω> (define city "Tokyo")
=> city

Ω> (effect infer.op (list "What country is " city " in?"))
=> "Japan"
```

When the payload is a list of strings, OmegaLLM automatically concatenates them into a single prompt.

### Defining LLM Functions

Here's where OmegaLLM shines. You can wrap LLM calls in ordinary functions:

```lisp
(define (translate-to-french text)
  (effect infer.op (list "Translate to French: " text)))
```

Now `translate-to-french` is a regular function that happens to call an LLM:

```lisp
Ω> (translate-to-french "hello")
=> "bonjour"

Ω> (translate-to-french "goodbye")
=> "au revoir"
```

### LLM Predicates

Functions that return yes/no answers are particularly useful:

```lisp
(define (is-edible? thing)
  (eq? "yes" (effect infer.op (list "Is " thing " edible? Answer only yes or no."))))
```

Now you have a semantic predicate:

```lisp
Ω> (is-edible? "apple")
=> #t

Ω> (is-edible? "laptop")
=> #f
```

### Practical Note: Handling Output Variations

LLMs may return their answers in slightly different formats. When asking for "yes or no", the response might be `"yes"` (a string), `"Yes"`, or even `#t` (parsed as a boolean). For robust predicates, define a helper:

```lisp
(define (truthy? x)
  (if (eq? x #t) #t (eq? x "yes")))

(define (is-edible? thing)
  (truthy? (effect infer.op (list "Is " thing " edible? Answer yes or no."))))
```

This handles both string and boolean responses consistently.