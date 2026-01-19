# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 17: Multiple Representations of Meaning
*Corresponding to SICP Section 2.4: Multiple Representations for Abstract Data*

### 17.1 The Problem of Multiple Representations

SICP Chapter 2.4 confronts a practical problem: the same abstract data can have multiple concrete representations. Complex numbers can be rectangular (real + imaginary) or polar (magnitude + angle). Both are legitimate; neither is canonical. The challenge is building systems that accommodate multiple representations without chaos.

In semantic programming, this problem is **fundamental and unavoidable**. The same meaning routinely appears in different forms:

| Representation | Expression |
|----------------|------------|
| Formal | "The precipitation probability exceeds 70%" |
| Casual | "It's probably gonna rain" |
| Poetic | "The heavens prepare their tears" |
| Technical | "P(rain) > 0.7" |
| Telegraphic | "Rain likely" |

These are not paraphrases—they're *different representations of identical semantic content*, each appropriate for different contexts, audiences, and purposes. A weather service API might emit the technical form; a poet crafting verse needs the lyrical form; a text message demands brevity.

### 17.2 The Importance of Representation Choice

Why does representation matter? Consider communicating to different audiences:

```lisp
(define core-meaning "The quarterly revenue decreased by 15%")

; To the board of directors
(define for-board
  (effect infer.op
    (list "Express for a board meeting: " core-meaning)))
=> "The company experienced a 15% revenue decline this quarter, requiring strategic review."

; To employees
(define for-employees
  (effect infer.op
    (list "Express reassuringly for employees: " core-meaning)))
=> "We had a challenging quarter, but we're adapting our approach."

; To investors
(define for-investors
  (effect infer.op
    (list "Express with analytical framing for investors: " core-meaning)))
=> "Q3 revenue contracted 15% QoQ, reflecting market headwinds."
```

The underlying fact is identical. The representations differ vastly. A system that can only produce one representation is brittle; a system that can produce any representation, on demand, is powerful.

### 17.3 Tagged Semantic Data

SICP's solution begins with **tagging**: attach a type marker to every datum. We know not just *what* the data is, but *which representation* it uses:

```lisp
; Constructors create tagged semantic data
(define (make-formal-text meaning)
  (list 'formal (effect infer.op
    (list "Express formally: " meaning))))

(define (make-casual-text meaning)
  (list 'casual (effect infer.op
    (list "Express casually: " meaning))))

(define (make-poetic-text meaning)
  (list 'poetic (effect infer.op
    (list "Express poetically: " meaning))))

; Selectors extract components
(define (text-register tagged) (car tagged))
(define (text-content tagged) (cadr tagged))

; Usage
Ω> (define weather-formal (make-formal-text "rain is likely"))
=> (formal "There is a high probability of precipitation.")

Ω> (define weather-casual (make-casual-text "rain is likely"))
=> (casual "Looks like it's gonna rain!")

Ω> (text-register weather-formal)
=> formal

Ω> (text-content weather-casual)
=> "Looks like it's gonna rain!"
```

The tag travels with the data. Any procedure receiving a tagged datum can inspect its representation and act accordingly.

### 17.4 Generic Semantic Operations

With tagged data, we can define **generic operations**—procedures that work correctly regardless of which representation they receive:

```lisp
(define (negate-meaning tagged-text)
  (let ((register (text-register tagged-text))
        (content (text-content tagged-text)))
    (list register
          (effect infer.op
            (list "Negate this, keeping the " (symbol->string register) " tone: " content)))))

Ω> (negate-meaning weather-formal)
=> (formal "There is a low probability of precipitation.")

Ω> (negate-meaning weather-casual)
=> (casual "Nah, probably won't rain.")

Ω> (negate-meaning (make-poetic-text "rain is likely"))
=> (poetic "The clouds disperse, withholding their silver benediction.")
```

The operation is *generic*: it preserves whatever register the input had. One procedure handles all representations.

More operations:

```lisp
(define (intensify-meaning tagged-text)
  (let ((register (text-register tagged-text))
        (content (text-content tagged-text)))
    (list register
          (effect infer.op
            (list "Make this more emphatic, keeping " (symbol->string register) " style: " content)))))

(define (soften-meaning tagged-text)
  (let ((register (text-register tagged-text))
        (content (text-content tagged-text)))
    (list register
          (effect infer.op
            (list "Make this gentler, keeping " (symbol->string register) " style: " content)))))

Ω> (intensify-meaning weather-formal)
=> (formal "Precipitation is virtually certain.")

Ω> (soften-meaning weather-formal)
=> (formal "There may be some possibility of precipitation.")
```

### 17.5 Register Conversion

Beyond operations that preserve register, we need operations that **convert** between registers—the semantic analog of converting rectangular to polar:

```lisp
(define (convert-register tagged-text new-register)
  (let ((content (text-content tagged-text)))
    (list new-register
          (effect infer.op
            (list "Rewrite in a " (symbol->string new-register) " style: " content)))))

Ω> (convert-register weather-formal 'casual)
=> (casual "Rain's coming!")

Ω> (convert-register weather-casual 'formal)
=> (formal "Precipitation is anticipated in the near term.")

Ω> (convert-register weather-casual 'poetic)
=> (poetic "Soon the sky shall weep.")
```

Conversion enables adapting content for different contexts on the fly.

### 17.6 Data-Directed Programming

SICP's key insight is **data-directed programming**: instead of embedding type checks in every operation, maintain a table that maps (operation, type) pairs to implementations:

```
             |  email       |  legal       |  technical
-------------|--------------|--------------|-------------
  summarize  |  email-sum   |  legal-sum   |  tech-sum
  expand     |  email-exp   |  legal-exp   |  tech-exp
  critique   |  email-crit  |  legal-crit  |  tech-crit
```

In OmegaLLM:

```lisp
; Create the operation table
(define semantic-ops (make-table))

; Install operations for email domain
(put! semantic-ops 'summarize 'email
  (lambda (text)
    (effect infer.op (list "Summarize this email in one line: " text))))

(put! semantic-ops 'expand 'email
  (lambda (text)
    (effect infer.op (list "Expand this into a full professional email: " text))))

; Install operations for legal domain
(put! semantic-ops 'summarize 'legal
  (lambda (text)
    (effect infer.op (list "Summarize legal implications and obligations: " text))))

(put! semantic-ops 'expand 'legal
  (lambda (text)
    (effect infer.op (list "Expand with standard legal clauses and qualifications: " text))))

; Install operations for technical domain
(put! semantic-ops 'summarize 'technical
  (lambda (text)
    (effect infer.op (list "Summarize technical content, preserving key specifications: " text))))

; Generic dispatch
(define (semantic-apply op domain text)
  ((get semantic-ops op domain) text))

; Use it
Ω> (semantic-apply 'summarize 'email "Dear Team, Please review the attached Q3 budget...")
=> "Budget review request for Q3"

Ω> (semantic-apply 'summarize 'legal "The party of the first part hereby agrees...")
=> "Binding agreement with mutual obligations"

Ω> (semantic-apply 'expand 'email "meeting tomorrow 2pm")
=> "Dear Colleague, I wanted to confirm our meeting scheduled for tomorrow at 2:00 PM..."
```

The generic `semantic-apply` doesn't know about emails or legal documents—it simply looks up the appropriate handler.

### 17.7 Additivity: The Power of the Table

The dispatch table enables **additivity**: adding new representations without modifying existing code. Anyone can install new domains:

```lisp
; Add medical domain later
(put! semantic-ops 'summarize 'medical
  (lambda (text)
    (effect infer.op (list "Summarize for a medical chart note: " text))))

; Add marketing domain
(put! semantic-ops 'summarize 'marketing
  (lambda (text)
    (effect infer.op (list "Summarize for marketing copy: " text))))

; Existing code works unchanged
Ω> (semantic-apply 'summarize 'medical "Patient presents with acute onset...")
=> "Chief complaint: acute presentation. Assessment pending."
```

No conditionals to update. No giant `cond` expression to extend. The table grows independently of the dispatch mechanism.

### 17.8 Message-Passing Style

SICP presents an alternative: **message passing**. Instead of external dispatch, the object itself handles operations:

```lisp
(define (make-semantic-object meaning register)
  (lambda (message)
    (cond
      ((eq? message 'content)
       (effect infer.op (list "Express in " (symbol->string register) " style: " meaning)))
      ((eq? message 'register) register)
      ((eq? message 'negate)
       (make-semantic-object
         (effect infer.op (list "Express the negation of: " meaning))
         register))
      ((eq? message 'intensify)
       (make-semantic-object
         (effect infer.op (list "Express more emphatically: " meaning))
         register))
      ((eq? message 'convert)
       (lambda (new-register)
         (make-semantic-object meaning new-register)))
      (else (error "Unknown message" message)))))

; Create an object
(define weather-obj (make-semantic-object "rain is likely" 'formal))

Ω> (weather-obj 'content)
=> "There is a high probability of precipitation."

Ω> (weather-obj 'register)
=> formal

Ω> ((weather-obj 'negate) 'content)
=> "There is a low probability of precipitation."

Ω> (((weather-obj 'convert) 'casual) 'content)
=> "Probably gonna rain!"
```

The object *is* its behavior. Operations are messages sent to objects, not functions applied to data.

### 17.9 Comparing Approaches

Data-directed and message-passing represent different decompositions:

**Data-directed** organizes by operation—all "summarize" implementations together, all "expand" implementations together. Adding new operations is easy (add a row to the table); adding new types requires updating all operations.

**Message-passing** organizes by type—all operations for "email" together. Adding new types is easy (define a new object); adding new operations requires updating all types.

For semantic programming, data-directed often wins: we frequently add new domains (email, legal, medical, marketing...) but rarely add fundamentally new operations. The table approach scales to many domains.

### 17.10 Coercion Between Semantic Domains

SICP introduces **coercion**: converting one type to another to enable operations. Semantic coercion converts between domains:

```lisp
; Define coercion procedures
(define (technical->layperson text)
  (effect infer.op
    (list "Explain to a non-technical person: " text)))

(define (layperson->technical text)
  (effect infer.op
    (list "Restate using technical terminology: " text)))

(define (formal->casual text)
  (effect infer.op
    (list "Rewrite in a casual, friendly tone: " text)))

(define (casual->formal text)
  (effect infer.op
    (list "Rewrite in formal business language: " text)))

; Coercion table
(define coercions (make-table))
(put! coercions 'technical 'layperson technical->layperson)
(put! coercions 'layperson 'technical layperson->technical)
(put! coercions 'formal 'casual formal->casual)
(put! coercions 'casual 'formal casual->formal)

(define (coerce text from-domain to-domain)
  ((get coercions from-domain to-domain) text))

Ω> (coerce "The API uses OAuth2 with JWT bearer tokens" 'technical 'layperson)
=> "The system uses a secure login method where you get a digital pass"

Ω> (coerce "The computer got confused and stopped" 'layperson 'technical)
=> "The system encountered an unhandled exception causing process termination"
```

Coercion enables cross-domain operations: if you have a technical operation but casual input, coerce first.

### 17.11 Exercises

**Exercise 17.1:** Implement a "semantic tower" analogous to SICP's numeric tower: casual → standard → formal → legal. Define coercion procedures between adjacent levels. Can you automatically coerce casual text to legal language through the chain?

**Exercise 17.2:** Create domain-specific summarizers for: news articles, scientific papers, social media posts, poetry. Test that identical content produces appropriately different summaries per domain.

**Exercise 17.3:** Build a "perspective converter" that restates arguments from different viewpoints: `(restate-as 'optimist text)`, `(restate-as 'pessimist text)`, `(restate-as 'neutral text)`. How does this relate to semantic representations?

**Exercise 17.4:** Implement the message-passing semantic object with additional messages: `'paraphrase`, `'summarize`, `'questions` (generates questions about the content). Test that objects properly maintain their register across transformations.

**Exercise 17.5:** SICP discusses the "tyranny of data types"—being forced to classify everything into rigid categories. Is there a "tyranny of registers" in semantic programming? Design a `'hybrid` register that blends properties of formal and casual. What challenges arise?

**Exercise 17.6:** Build an additive semantic system where new domains can be installed at runtime via configuration. Define a procedure `(install-domain! name summarize-prompt expand-prompt)` that registers a new domain in the dispatch table.