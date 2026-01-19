# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 4: Higher-Order LLM Functions
### Classifier Factories

You can write functions that *return* LLM functions:

```lisp
(define (make-classifier category)
  (lambda (text)
    (effect infer.op (list "Is " text " " category "? Answer yes or no."))))
```

Now create specialized classifiers:

```lisp
Ω> (define is-positive (make-classifier "positive sentiment"))
=> is-positive

Ω> (define is-technical (make-classifier "technical content"))
=> is-technical

Ω> (is-positive "I love this product!")
=> "yes"

Ω> (is-positive "This is terrible")
=> "no"

Ω> (is-technical "The API uses REST endpoints")
=> "yes"
```

### Parameterized Prompts

This pattern lets you create families of related LLM functions:

```lisp
(define (make-translator target-language)
  (lambda (text)
    (effect infer.op (list "Translate to " target-language ": " text))))

(define to-german (make-translator "German"))
(define to-japanese (make-translator "Japanese"))
(define to-italian (make-translator "Italian"))

Ω> (to-german "good morning")
=> "guten Morgen"

Ω> (to-japanese "thank you")
=> "ありがとう"
```