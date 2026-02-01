# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 28: The Substitution Model for Semantic Evaluation
*Corresponding to SICP Section 1.1.5: The Substitution Model for Procedure Application*

### 28.1 Understanding Evaluation Through Substitution

SICP introduces the **substitution model** as a mental framework for understanding how procedure application works. The idea is simple: to apply a procedure to arguments, substitute the argument values for the parameters throughout the body of the procedure, then evaluate the resulting expression.

This model is not how interpreters actually work (that's the environment model, which we'll see in Chapter 20), but it's a powerful pedagogical tool. It builds intuition about evaluation order and helps us reason about what our programs mean.

In inference programming, the substitution model becomes even more enlightening because we're substituting not just values, but **semantic expressions**—prompts that will be sent to the LLM.

### 28.2 Basic Substitution with Semantic Effects

Consider this simple procedure:

```lisp
(define (sentiment text)
  (effect infer.op (list "Sentiment of: " text)))
```

When we apply it:

```lisp
(sentiment "I love this!")
```

The substitution model says: substitute `"I love this!"` for `text` in the procedure body:

```lisp
;; Step 1: Substitute argument for parameter
(effect infer.op (list "Sentiment of: " "I love this!"))
```

Now evaluate the `list` expression:

```lisp
;; Step 2: Evaluate the list combination
(effect infer.op "Sentiment of: I love this!")
```

Finally, execute the effect:

```lisp
;; Step 3: Execute the semantic effect
=> "positive"
```

This trace shows us exactly what prompt gets constructed and sent to the LLM. The substitution model makes the prompt-building process transparent.

### 28.3 Nested Substitution: Compositions

Things get interesting when procedures call other procedures. Consider:

```lisp
(define (sentiment text)
  (effect infer.op (list "Sentiment of: " text)))

(define (analyze-sentiment-length text)
  (list (sentiment text) (length text)))

(analyze-sentiment-length "Great!")
```

Let's trace the substitution step by step:

```lisp
;; Application: (analyze-sentiment-length "Great!")
;; Substitute "Great!" for text in analyze-sentiment-length body:

(list (sentiment "Great!") (length "Great!"))

;; Now we have two subexpressions to evaluate:
;; First: (sentiment "Great!")
;;   Substitute "Great!" for text in sentiment body:
;;   (effect infer.op (list "Sentiment of: " "Great!"))
;;   Evaluate list:
;;   (effect infer.op "Sentiment of: Great!")
;;   Execute effect:
;;   => "positive"

;; Second: (length "Great!")
;;   Built-in, returns immediately:
;;   => 6

;; Finally, combine results:
(list "positive" 6)
=> ("positive" 6)
```

The substitution model reveals the evaluation order: inner expressions are evaluated before outer ones. The sentiment analysis happens **before** the list is constructed.

### 28.4 Substitution Order Matters

With pure functions, the order of substitution doesn't affect the final result. But semantic effects can produce different results each time. Consider:

```lisp
(define (creative-greeting)
  (effect infer.op "Generate a unique greeting"))

(define (double-greeting)
  (list (creative-greeting) (creative-greeting)))
```

Trace the substitution:

```lisp
;; (double-greeting)
;; Substitute into body:
(list (creative-greeting) (creative-greeting))

;; Evaluate first (creative-greeting):
;; (effect infer.op "Generate a unique greeting")
;; => "Hello, friend!"

;; Evaluate second (creative-greeting):
;; (effect infer.op "Generate a unique greeting")
;; => "Greetings and salutations!"

;; Combine:
(list "Hello, friend!" "Greetings and salutations!")
=> ("Hello, friend!" "Greetings and salutations!")
```

The two calls to `creative-greeting` happen at different times and may produce different results. The substitution model makes this explicit: we see two distinct effect evaluations, each invoking the LLM independently.

### 28.5 Applicative vs Normal Order with Semantic Effects

SICP discusses two evaluation orders:

1. **Applicative order**: Evaluate arguments before applying procedure (what most languages do)
2. **Normal order**: Substitute first, evaluate later ("fully expand, then reduce")

OmegaLLM uses applicative order by default. Let's see what difference this makes:

```lisp
(define (verbose-sentiment text)
  (effect infer.op (list "Sentiment of: " text)))

(define (if-positive text action)
  (if (equal? (verbose-sentiment text) "positive")
      action
      "neutral"))
```

**Applicative order** (actual evaluation):
```lisp
(if-positive "I love this!" (effect infer.op "Celebrate!"))

;; Step 1: Evaluate arguments first
;; - Evaluate "I love this!" => "I love this!"
;; - Evaluate (effect infer.op "Celebrate!") => "Hooray!" (EVALUATED IMMEDIATELY)
;;
;; Step 2: Substitute and apply
;; (if (equal? (verbose-sentiment "I love this!") "positive")
;;     "Hooray!"
;;     "neutral")
;;
;; Step 3: Evaluate verbose-sentiment
;; => "positive"
;;
;; Step 4: Test passes, return action
;; => "Hooray!"
```

**Normal order** (hypothetical):
```lisp
(if-positive "I love this!" (effect infer.op "Celebrate!"))

;; Step 1: Substitute without evaluating arguments
;; (if (equal? (verbose-sentiment "I love this!")
;;             "positive")
;;     (effect infer.op "Celebrate!")   ;; NOT YET EVALUATED
;;     "neutral")
;;
;; Step 2: Evaluate verbose-sentiment
;; => "positive"
;;
;; Step 3: Test passes, evaluate action NOW
;; (effect infer.op "Celebrate!")
;; => "Hooray!"
```

The difference: In applicative order, `"Celebrate!"` is evaluated **even if the test fails**. In normal order, it's only evaluated **if needed**. For semantic effects with costs, this distinction matters!

Chapter 25 explores lazy evaluation, which implements a form of normal order for efficiency.

### 28.6 Substitution and Prompt Construction

The substitution model is especially valuable for understanding **how prompts get built**. Consider:

```lisp
(define (translate-to lang text)
  (effect infer.op (list "Translate to " lang ": " text)))

(define (friendly-translate lang greeting-name)
  (translate-to lang
    (effect infer.op (list "Say hello to " greeting-name))))

(friendly-translate "French" "Alice")
```

Full substitution trace:

```lisp
;; Substitute "French" for lang, "Alice" for greeting-name:
(translate-to "French"
  (effect infer.op (list "Say hello to " "Alice")))

;; Evaluate inner effect argument:
(effect infer.op (list "Say hello to " "Alice"))
=> (effect infer.op "Say hello to Alice")
=> "Hello, Alice!"

;; Now substitute into translate-to:
(effect infer.op (list "Translate to " "French" ": " "Hello, Alice!"))

;; Evaluate list:
(effect infer.op "Translate to French: Hello, Alice!")

;; Execute effect:
=> "Bonjour, Alice!"
```

The substitution model shows us that prompts are built **inside-out**: inner LLM calls complete first, their results are substituted into outer prompts, and outer calls execute with those results.

This is the key to understanding prompt composition: each layer of function abstraction adds a layer of prompt construction.

### 28.7 When Substitution Breaks Down

The substitution model is a simplification. It fails to capture several realities:

**1. Mutable state**: If a procedure modifies a variable, substitution doesn't show those side effects:

```lisp
(define counter 0)
(define (increment!)
  (set! counter (+ counter 1))
  counter)

;; Substitution model can't explain this:
(+ (increment!) (increment!))
;; => 3, not 2
```

**2. Lazy evaluation**: Streams and delayed values don't fit pure substitution (see Chapter 25).

**3. Continuations and control flow**: Exception handlers, AMB, and prompts/shifts involve control effects that substitution can't model.

**4. Environment capture**: Closures capture environments, not just values. The substitution model doesn't show this.

For these cases, we need the **environment model** (Chapter 20), which shows how the interpreter actually works with environments, stores, and continuations.

### 28.8 Why the Substitution Model Matters

If the substitution model is inaccurate, why teach it?

Because it's the right mental model for **reasoning about meaning**. When you write:

```lisp
(effect infer.op (list "Summarize: " (effect infer.op "Generate report")))
```

You don't think about environments and continuation frames. You think: "First generate a report, then summarize it." The substitution model captures that intuition perfectly.

SICP puts it beautifully: *"The purpose of a model is not to reproduce reality in every detail, but to provide a framework for understanding."*

For inference programming, the substitution model provides exactly the right framework: it shows how semantic expressions compose, how prompts are built step-by-step, and how arguments flow through chains of abstraction to become parts of LLM queries.

### 28.9 Practical Example: Debugging Prompts

When an LLM call doesn't work as expected, the substitution model helps debug it. Consider:

```lisp
(define (classify category text)
  (effect infer.op (list "Is this " category "? yes or no: " text)))

(define (is-urgent? email)
  (equal? "yes" (classify "urgent" email)))

;; Not working as expected
(is-urgent? "Meeting moved to tomorrow")
=> #f  ;; Expected #t
```

Use the substitution model to trace what prompt is actually being sent:

```lisp
;; (is-urgent? "Meeting moved to tomorrow")
;; Substitute:
(equal? "yes" (classify "urgent" "Meeting moved to tomorrow"))

;; Substitute into classify:
(equal? "yes" (effect infer.op (list "Is this " "urgent" "? yes or no: " "Meeting moved to tomorrow")))

;; Evaluate list:
(equal? "yes" (effect infer.op "Is this urgent? yes or no: Meeting moved to tomorrow"))
```

Aha! The prompt is: *"Is this urgent? yes or no: Meeting moved to tomorrow"*

The problem is clear: "urgent" is too vague. Let's refine:

```lisp
(define (is-urgent? email)
  (equal? "yes" (classify "urgent requiring immediate response" email)))

;; Trace again:
;; Prompt becomes: "Is this urgent requiring immediate response? yes or no: Meeting moved to tomorrow"
;; Now the LLM has better context!
```

The substitution model turned abstract code into concrete prompts, revealing the issue.

### 28.10 Exercises

**Exercise 28.1:** Trace the substitution for this expression:
```lisp
(define (double x) (+ x x))
(define (square x) (* x x))
(double (square 3))
```
Show every substitution step until you reach the final value.

**Exercise 28.2:** Now trace this semantic version:
```lisp
(define (emphasize text)
  (effect infer.op (list "Add emphasis to: " text)))
(define (translate text)
  (effect infer.op (list "Translate to Spanish: " text)))
(emphasize (translate "Hello"))
```
At what point does each LLM call happen? What are the exact prompts sent?

**Exercise 28.3:** Consider:
```lisp
(define (creative)
  (effect infer.op "Generate a random number"))
(+ (creative) (creative))
```
If the LLM returns different values for each call (e.g., "7" then "42"), what does the expression evaluate to? Trace the substitution to show why.

**Exercise 28.4:** The substitution model assumes applicative order. Rewrite the trace from Exercise 28.2 using normal order (substitute first, evaluate later). Would the final result differ?

**Exercise 28.5:** You have:
```lisp
(define (sentiment text)
  (effect infer.op (list "Sentiment of: " text)))

(define (process texts)
  (map sentiment texts))

(process (list "Good" "Bad" "Ugly"))
```
Use the substitution model to show what prompts get generated. How many LLM calls occur?

**Exercise 28.6:** Debug this code using substitution tracing:
```lisp
(define (summarize-if-long text max-len)
  (if (> (length text) max-len)
      (effect infer.op (list "Summarize: " text))
      text))

(summarize-if-long "Hi" (effect infer.op "Pick a number between 5 and 10"))
```
What happens? Is there a wasted LLM call? How would lazy evaluation help?

---

**Key Insights:**
- The substitution model shows how arguments flow through procedures
- For semantic effects, it reveals exactly what prompts get constructed
- Evaluation order (applicative vs normal) matters for effects with costs
- The model is pedagogical, not literal—but it's the right mental model
- Use it to debug unexpected prompts and understand composition

**Next:** Chapter 29 explores iterative refinement using LLM feedback loops—Newton's method for semantic space!
