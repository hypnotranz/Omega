# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 14: Semantic Data Abstraction
*Corresponding to SICP Section 2.1: Introduction to Data Abstraction*

### 14.1 The Discipline of Data Abstraction

SICP Chapter 2 introduces one of programming's most powerful ideas: **data abstraction**. We separate *how* data is represented from *how* it is used. A rational number might be stored as a pair of integers, but users of the abstraction see only `(make-rat n d)`, `(numer r)`, and `(denom r)`.

This discipline has three components:
1. **Constructors** — procedures that create data objects
2. **Selectors** — procedures that extract components
3. **Abstraction barriers** — the principle that code should only use the agreed interface, never reach into representations

In semantic programming, this discipline becomes essential because meanings are complex, multifaceted objects. We need structured ways to create, access, and validate them.

### 14.2 Wishful Thinking: Designing Top-Down

SICP advocates "wishful thinking" — designing with procedures you wish existed, then implementing them. This top-down approach clarifies what operations you truly need.

Let's design a **semantic document** abstraction. We'll wish for operations, then implement them:

```lisp
; Wishful thinking: assume these exist
(make-semantic-doc title content)    ; Constructor
(doc-title doc)                      ; Selector
(doc-content doc)                    ; Selector
(doc-summary doc)                    ; Computed property
(doc-sentiment doc)                  ; Computed property
(valid-doc? doc)                     ; Validator
```

Now we can write code that *uses* documents before deciding how to represent them:

```lisp
(define (summarize-document doc)
  (list (doc-title doc)
        (doc-summary doc)
        (doc-sentiment doc)))

(define (filter-positive-docs docs)
  (filter (lambda (d) (eq? 'positive (doc-sentiment d))) docs))
```

This code doesn't know whether documents are lists, records, or something else. It only knows the interface.

### 14.3 Implementing the Abstraction

Now we implement our wished-for operations:

```lisp
; Representation: a document is a list (title content)
(define (make-semantic-doc title content)
  (list title content))

(define (doc-title doc) (car doc))
(define (doc-content doc) (cadr doc))

; Computed via LLM
(define (doc-summary doc)
  (effect infer.op (list "Summarize in one sentence: " (doc-content doc))))

(define (doc-sentiment doc)
  (let ((result (effect infer.op
                  (list "Is the sentiment positive, negative, or neutral? " (doc-content doc)))))
    (cond
      ((string-contains? result "positive") 'positive)
      ((string-contains? result "negative") 'negative)
      (else 'neutral))))
```

The calling code doesn't change. We've implemented the abstraction, and all uses continue to work.

### 14.4 Abstraction Barriers

SICP emphasizes abstraction barriers: layers of code should only communicate through defined interfaces. Violating barriers creates brittle systems.

```
┌────────────────────────────────────────────┐
│     Programs that USE documents            │
│  (summarize-document, filter-positive)     │
├────────────────────────────────────────────┤ ← Barrier: document interface
│     Document operations                    │
│  (make-doc, doc-title, doc-summary...)     │
├────────────────────────────────────────────┤ ← Barrier: representation
│     Underlying representation              │
│  (lists, cons cells)                       │
├────────────────────────────────────────────┤ ← Barrier: semantic primitives
│     LLM effects                            │
│  (effect infer.op ...)                     │
└────────────────────────────────────────────┘
```

A well-designed system respects these barriers. If we change document representation from lists to vectors, only the middle layer changes. If we change LLM providers, only the bottom layer changes.

### 14.5 Semantic Types as Contracts

Traditional type systems ask: "Is this data the right *shape*?" Semantic type systems ask: "Is this data the right *meaning*?"

We can define semantic type validators:

```lisp
(define (semantic-type? type-name value)
  (eq? "yes" (effect infer.op
    (list "Is this a valid " type-name "? Answer yes or no: " value))))

Ω> (semantic-type? "professional business greeting"
                   "Dear Mr. Smith, I hope this finds you well.")
=> #t

Ω> (semantic-type? "haiku"
                   "An old silent pond / A frog jumps into the pond / Splash! Silence again")
=> #t

Ω> (semantic-type? "polite refusal"
                   "No way, forget it!")
=> #f
```

The LLM is our type checker—determining membership based on *meaning*, not structure.

### 14.6 Constructors with Semantic Validation

SICP shows constructors that enforce data integrity. We can enforce *semantic* integrity:

```lisp
(define (make-professional-email greeting body closing)
  (if (and (semantic-type? "professional greeting" greeting)
           (semantic-type? "professional closing" closing))
      (list 'email greeting body closing)
      (error "Invalid email format: components must be professional")))

Ω> (make-professional-email "Dear Team," "Here's the report" "Best regards,")
=> (email "Dear Team," "Here's the report" "Best regards,")

Ω> (make-professional-email "yo" "Here's the report" "later dude")
=> ERROR: Invalid email format: components must be professional
```

The constructor refuses semantically malformed data—not based on syntax, but on meaning.

### 14.7 Rich Semantic Data Types

We can define elaborate semantic types as data structures:

```lisp
(define (make-semantic-type name validator description)
  (list 'semantic-type name validator description))

(define (type-name t) (cadr t))
(define (type-validator t) (caddr t))
(define (type-description t) (cadddr t))

(define (type-check type value)
  ((type-validator type) value))

; Define a type for valid arguments
(define valid-argument-type
  (make-semantic-type
    'valid-argument
    (lambda (text)
      (eq? "yes" (effect infer.op
        (list "Does this text make a logical argument with premises and conclusion? yes or no: " text))))
    "A text that presents premises leading to a conclusion"))

Ω> (type-check valid-argument-type
     "All humans are mortal. Socrates is human. Therefore, Socrates is mortal.")
=> #t

Ω> (type-check valid-argument-type "I like pizza")
=> #f
```

### 14.8 Levels of Semantic Abstraction

Just as SICP builds layers (pairs → lists → trees), we can build semantic abstraction layers:

```lisp
; Level 0: Raw LLM calls
(effect infer.op "...")

; Level 1: Semantic primitives
(define (classify text) ...)
(define (summarize text) ...)
(define (translate text lang) ...)

; Level 2: Semantic data types
(define (make-document title content) ...)
(define (make-review product rating text) ...)

; Level 3: Domain objects
(define (make-customer-ticket issue priority customer) ...)
(define (make-research-paper abstract methods results) ...)

; Level 4: Applications
(define (customer-support-bot) ...)
(define (paper-reviewer) ...)
```

Each level builds on the one below, never reaching through barriers.

### 14.9 The Power of Semantic Abstraction

Consider what we can now express: a "professional email" is a semantic type. A "valid argument" is a semantic type. A "haiku" is a semantic type. A "grammatically correct sentence" is a semantic type.

```lisp
Ω> (semantic-type? "grammatically correct English sentence"
                   "Me want cookie")
=> #f

Ω> (semantic-type? "sentence with passive voice"
                   "The ball was thrown by John")
=> #t

Ω> (semantic-type? "culturally sensitive communication for Japanese audience"
                   "We demand immediate action!")
=> #f
```

Traditional type systems could never express these constraints. Semantic abstraction gives us types defined by understanding, not structure.

### 14.10 Exercises

**Exercise 14.1:** Design a `semantic-record` abstraction with constructor, selectors, and validators. A semantic record should have named fields where each field can have a semantic type constraint.

**Exercise 14.2:** Define `(make-haiku line1 line2 line3)` that validates syllable counts (5-7-5) using semantic type checking. What happens with edge cases like compound words?

**Exercise 14.3:** Create a "business proposal" type with sections (problem, solution, timeline, cost). Each section should have semantic constraints. Build a constructor that validates all constraints.

**Exercise 14.4:** SICP discusses how abstraction barriers enable change. Imagine changing `make-semantic-doc` to store documents differently. What code would change? What wouldn't?

**Exercise 14.5:** Can semantic types replace traditional types entirely? What are the tradeoffs in reliability, performance, and determinism? When would you prefer structural types?

**Exercise 14.6:** Design a semantic type hierarchy: `formal-text` ⊃ `business-letter` ⊃ `legal-contract`. How do you express that one type is a subtype of another?