# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 40: Data-Directed Evaluation and Method Synthesis
*Corresponding to SICP Section 4.1 (Meta-level) and Data-Directed Programming*

### 40.1 LLM as Meta-Programmer

The ultimate meta-level: **the LLM writes code at runtime**.

When the evaluator encounters an unknown operation, it asks the LLM to synthesize a handler:

```lisp
;; User writes:
(eval '(effect custom.politeness-check "Hey dude") env)

;; Evaluator doesn't know "custom.politeness-check"
;; ASK LLM TO SYNTHESIZE IT!

(define (handle-unknown-effect op args)
  (effect infer.op
    (list "Generate a Lisp function that implements: " op
          "\nIt should take arguments: " args
          "\nReturn valid Lisp code as a string.")))

Ω> (handle-unknown-effect 'custom.politeness-check '("text"))

=> "(lambda (text)
      (let ((rating (effect infer.op
                      (list \"Rate politeness 0-10: \" text))))
        (>= rating 7)))"

;; Evaluator INSTALLS this synthesized handler
(install-operation! 'custom.politeness-check
  (eval (read-from-string synthesized-code) env))

;; Now it works!
(eval '(effect custom.politeness-check "Hey dude") env)
=> #f  ; Rating is low

(eval '(effect custom.politeness-check "Good morning, sir") env)
=> #t  ; Rating is high
```

### 40.2 Dynamic Dispatch via LLM

The LLM decides which handler to invoke based on **semantic content**:

```lisp
(define (semantic-dispatch input handlers)
  (let ((chosen-handler
         (effect infer.op
           (list "Which handler should process this input?\n"
                 "Input: " input "\n"
                 "Handlers: " (map car handlers)))))
    ((assoc chosen-handler handlers))))

(define handlers
  '((complaint handle-complaint)
    (question handle-question)
    (praise handle-praise)
    (request handle-request)))

Ω> (semantic-dispatch "Why was I charged twice?" handlers)
;; LLM selects "question" or "complaint"
=> (handle-complaint "Why was I charged twice?")
```

### 40.3 Self-Modifying Semantic Interpreter

The interpreter **extends itself**:

```lisp
(define operation-table (make-table))

(define (eval-effect expr env)
  (let ((op (effect-op expr))
        (args (effect-args expr)))
    (or (lookup op operation-table)
        ;; Unknown op: synthesize handler
        (let ((handler (synthesize-handler op args)))
          (install-operation! op handler operation-table)
          handler)
        (apply-handler handler args env))))

;; Example: User defines custom operation
Ω> (effect custom.extract-entities "Alice met Bob in Boston")

;; First time: LLM synthesizes entity extractor
;; Installed in operation-table
;; Returns: ((person "Alice") (person "Bob") (location "Boston"))

;; Second time: Uses installed handler (no synthesis)
Ω> (effect custom.extract-entities "Carol flew to Paris")
=> ((person "Carol") (location "Paris"))
```

### 40.4 Method Synthesis from Specification

Give LLM a spec, it generates the implementation:

```lisp
(define (generate-operation spec)
  (effect infer.op
    (list "Generate Lisp function from specification:\n" spec
          "\nReturn complete lambda expression.")))

Ω> (generate-operation
     "Takes a product review and returns (rating, aspects) where rating is 1-5 and aspects is list of mentioned features")

=> "(lambda (review)
      (let ((rating (extract-rating review))
            (aspects (extract-aspects review)))
        (list rating aspects)))"

;; Install and use
(install-operation! 'review.analyze (eval generated-code env))

Ω> (effect review.analyze "Great product! Love the battery life and screen.")
=> (5 ("battery life" "screen"))
```

### 40.5 Adaptive Dispatch Tables

Operations table evolves based on usage:

```lisp
(define (adaptive-eval expr env)
  (let ((op (car expr)))
    (if (lookup op operation-table)
        ;; Known operation
        (apply-operation op (cdr expr) env)
        ;; Unknown: ask LLM what to do
        (let ((strategy
               (effect infer.op
                 (list "What should I do with unknown operation: " op
                       "\nOptions: synthesize, delegate, error, or suggest-alternative"))))
          (case strategy
            ((synthesize) (synthesize-and-install op))
            ((delegate) (delegate-to-fallback op))
            ((suggest-alternative) (suggest-similar-ops op))
            (else (error "Unknown operation: " op)))))))
```

### 40.6 Key Insights

- LLM synthesizes operation handlers at runtime
- Evaluator extends itself dynamically
- Semantic dispatch: LLM chooses handler based on meaning
- Specifications become implementations via LLM
- Ultimate meta-programming: code that writes code

**Next:** Chapter 41 covers unification and semantic pattern matching!
