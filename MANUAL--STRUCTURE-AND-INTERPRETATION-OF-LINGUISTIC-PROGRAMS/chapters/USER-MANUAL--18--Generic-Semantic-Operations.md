# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 18: Generic Semantic Operations
*Corresponding to SICP Section 2.5: Systems with Generic Operations*

### 18.1 The Challenge of Generic Operations

SICP Chapter 2.5 develops **generic arithmetic**: operations like `add` that work seamlessly across integers, rationals, reals, and complex numbers. The user writes `(add x y)` without knowing—or caring—what types `x` and `y` are. The system dispatches to the appropriate implementation.

In semantic programming, generic operations face a richer challenge. We want operations that work across **any textual domain**:

```lisp
(define (summarize text) ...)      ; Works on emails, poems, contracts, recipes
(define (expand text) ...)         ; Works on tweets, abstracts, notes
(define (translate text lang) ...) ; Works on prose, dialogue, technical docs
(define (sentiment text) ...)      ; Works on reviews, articles, messages
```

These are **domain-transcending** operations. They don't know if the input is Shakespeare or a shopping list, yet they must produce sensible results for both.

### 18.2 Installing Operations in a Package

SICP organizes generic operations into **packages**—modules that install their implementations into a global table. For semantic programming:

```lisp
; The global operation table
(define semantic-op-table (make-table))

; Package for email domain
(define (install-email-package)
  (define (email-summarize text)
    (effect infer.op (list "Summarize this email in one line: " text)))
  (define (email-expand text)
    (effect infer.op (list "Expand into a full professional email: " text)))
  (define (email-formalize text)
    (effect infer.op (list "Make this email more formal: " text)))

  (put! semantic-op-table 'summarize 'email email-summarize)
  (put! semantic-op-table 'expand 'email email-expand)
  (put! semantic-op-table 'formalize 'email email-formalize)
  'email-package-installed)

; Package for legal domain
(define (install-legal-package)
  (define (legal-summarize text)
    (effect infer.op (list "Summarize legal implications and obligations: " text)))
  (define (legal-expand text)
    (effect infer.op (list "Expand with standard clauses and qualifications: " text)))
  (define (legal-formalize text)
    (effect infer.op (list "Express in formal legal language: " text)))

  (put! semantic-op-table 'summarize 'legal legal-summarize)
  (put! semantic-op-table 'expand 'legal legal-expand)
  (put! semantic-op-table 'formalize 'legal legal-formalize)
  'legal-package-installed)

; Install packages
(install-email-package)
(install-legal-package)
```

Each domain defines its own implementations; the table unifies them under common names.

### 18.3 Generic Dispatch

With operations installed, we define generic procedures that look up and dispatch:

```lisp
(define (apply-generic op domain . args)
  (let ((proc (get semantic-op-table op domain)))
    (if proc
        (apply proc args)
        (error "No method for" op domain))))

; Convenience wrappers
(define (summarize domain text)
  (apply-generic 'summarize domain text))

(define (expand domain text)
  (apply-generic 'expand domain text))

(define (formalize domain text)
  (apply-generic 'formalize domain text))

; Usage
Ω> (summarize 'email "Dear Team, Please review the Q3 budget projections...")
=> "Budget review request"

Ω> (summarize 'legal "The party of the first part hereby agrees...")
=> "Mutual obligations under contract"

Ω> (formalize 'email "hey can we chat tmrw?")
=> "Dear Colleague, I would like to schedule a meeting for tomorrow..."
```

The caller specifies *what* operation on *which* domain. The system finds the right implementation.

### 18.4 The Semantic Algebra

SICP notes that arithmetic operations obey algebraic laws: addition is commutative and associative; multiplication distributes over addition. Semantic operations have their own **quasi-algebraic** properties:

```lisp
; Summarize then expand approximates identity (lossy)
(define text "The quarterly sales report shows a 15% increase in revenue.")
Ω> (expand 'general (summarize 'general text))
=> "The sales report indicates revenue grew by approximately 15% over the quarter."

; Translate then translate back preserves meaning (approximately)
Ω> (same-meaning? text (translate (translate text "French") "English"))
=> #t  ; usually

; Sentiment is invariant under paraphrase
Ω> (eq? (sentiment text) (sentiment (paraphrase text)))
=> #t  ; positive remains positive

; Formality increases monotonically
Ω> (more-formal? (formalize 'general text) text)
=> #t  ; always
```

Unlike arithmetic, these properties hold **approximately** and **usually**, not **always** and **exactly**. This is the nature of semantic computation.

### 18.5 Cross-Domain Operations

What happens when we need to operate across domains? Consider combining an email with a legal document. We need **coercion**:

```lisp
; Coercion table
(define coercion-table (make-table))

(define (install-coercions)
  (put! coercion-table 'email 'formal
    (lambda (text) (effect infer.op (list "Rewrite as formal prose: " text))))
  (put! coercion-table 'casual 'formal
    (lambda (text) (effect infer.op (list "Rewrite in formal style: " text))))
  (put! coercion-table 'technical 'layperson
    (lambda (text) (effect infer.op (list "Explain for a general audience: " text))))
  (put! coercion-table 'layperson 'technical
    (lambda (text) (effect infer.op (list "Restate with technical precision: " text))))
  'coercions-installed)

(define (coerce text from-domain to-domain)
  (if (eq? from-domain to-domain)
      text
      (let ((coercer (get coercion-table from-domain to-domain)))
        (if coercer
            (coercer text)
            (error "No coercion" from-domain to-domain)))))

Ω> (coerce "The server's busted" 'casual 'formal)
=> "The server is experiencing a malfunction."

Ω> (coerce "API latency exceeds SLA thresholds" 'technical 'layperson)
=> "The system is responding slower than promised."
```

### 18.6 The Semantic Type Tower

SICP's **type tower** orders types by generality: integer ⊂ rational ⊂ real ⊂ complex. Higher types can represent lower types, but not vice versa.

Semantic domains form multiple towers. One tower orders by **formality**:

```
     Legal
       ↑
    Formal
       ↑
   Standard
       ↑
    Casual
       ↑
     Slang
```

Another orders by **abstraction**:

```
    Abstract ("Something happened")
         ↑
    General ("There was a problem")
         ↑
    Specific ("The server crashed")
         ↑
    Detailed ("Server 7 crashed at 14:32 due to OOM")
```

We can define raising operations that move up the tower:

```lisp
; Raise formality
(define (raise-formality text)
  (effect infer.op (list "Make this more formal: " text)))

; Raise abstraction
(define (raise-abstraction text)
  (effect infer.op (list "Make this more abstract and general: " text)))

; Lower operations (go down the tower)
(define (lower-formality text)
  (effect infer.op (list "Make this more casual: " text)))

(define (lower-abstraction text context)
  (effect infer.op (list "Make this more specific and detailed, given context: " context ". Text: " text)))

; Multi-level raise
(define (raise-formality-n text n)
  (if (= n 0) text
      (raise-formality-n (raise-formality text) (- n 1))))

Ω> (raise-formality-n "hey wanna grab lunch?" 2)
=> "Would you be available to join me for a meal?"
```

### 18.7 Operations Across the Tower

SICP shows how operations can work across the type tower by coercing operands to a common level. For semantic operations:

```lisp
; Combine two texts at their highest common formality
(define (semantic-combine text1 domain1 text2 domain2)
  (let ((target-domain (higher-formality domain1 domain2)))
    (let ((t1 (coerce-to-formality text1 domain1 target-domain))
          (t2 (coerce-to-formality text2 domain2 target-domain)))
      (effect infer.op (list "Combine these coherently: " t1 " AND " t2)))))

; Determine which domain is more formal
(define (higher-formality d1 d2)
  (let ((order '(slang casual standard formal legal)))
    (if (> (position d1 order) (position d2 order)) d1 d2)))

Ω> (semantic-combine "yo meeting cancelled" 'casual "pursuant to earlier discussions" 'formal)
=> "The meeting has been cancelled in accordance with prior discussions."
```

The system automatically raises the casual text to match the formal text before combining.

### 18.8 The Polynomial Analog: Structured Semantic Data

SICP culminates with polynomials—structured data that can be added, multiplied, and composed. The semantic analog: **structured arguments** that can be combined, contrasted, and synthesized:

```lisp
; A semantic argument has claims and evidence
(define (make-argument claim evidence)
  (list 'argument claim evidence))

(define (argument-claim arg) (cadr arg))
(define (argument-evidence arg) (caddr arg))

; Combine arguments (like polynomial addition)
(define (combine-arguments arg1 arg2)
  (make-argument
    (effect infer.op (list "Synthesize these claims: "
                           (argument-claim arg1) " AND " (argument-claim arg2)))
    (append (argument-evidence arg1) (argument-evidence arg2))))

; Strengthen argument (like polynomial multiplication)
(define (strengthen-argument arg additional-evidence)
  (make-argument
    (argument-claim arg)
    (cons additional-evidence (argument-evidence arg))))

; Counter an argument (like polynomial negation)
(define (counter-argument arg)
  (make-argument
    (effect infer.op (list "State the opposite position: " (argument-claim arg)))
    (map (lambda (e)
           (effect infer.op (list "Counter this evidence: " e)))
         (argument-evidence arg))))

Ω> (define arg1 (make-argument "Remote work improves productivity"
                                (list "Studies show fewer distractions"
                                      "Employees report better focus")))

Ω> (argument-claim (counter-argument arg1))
=> "In-office work improves productivity"
```

### 18.9 Generic Operations as Semantic Abstractions

The deepest insight from SICP 2.5: generic operations create **abstraction barriers** that separate interface from implementation. For semantic operations:

```
          User Code
              |
    (summarize domain text)
              |
     ┌────────┴────────┐
     |  Generic Layer  |  ← Knows only operation names
     └────────┬────────┘
              |
    dispatch table lookup
              |
     ┌────────┴────────┐
     | Implementation  |  ← Knows domain-specific prompts
     └────────┬────────┘
              |
         LLM call
```

The user never sees domain-specific prompts. The implementations never see the dispatch mechanism. Each layer is independent.

### 18.10 Combining Independent Semantic Systems

SICP shows how packages from different sources can coexist. Semantic packages can similarly be combined:

```lisp
; Install third-party packages
(install-medical-semantic-package)  ; From medical NLP team
(install-legal-semantic-package)    ; From legal tech vendor
(install-marketing-package)         ; From marketing tools

; All work through the same generic interface
Ω> (summarize 'medical "Patient presents with acute onset of dyspnea...")
=> "Acute respiratory distress, evaluation needed"

Ω> (summarize 'marketing "Our Q3 campaign achieved 150% of target impressions...")
=> "Q3 campaign exceeded goals"

; They don't interfere with each other
; They compose with standard operations
Ω> (translate (summarize 'medical report) "Spanish")
=> "Dificultad respiratoria aguda, se necesita evaluación"
```

### 18.11 Exercises

**Exercise 18.1:** Implement `(semantic-add text1 text2)` that combines two pieces of information. What algebraic properties should it have? Is it commutative? Associative? Test empirically.

**Exercise 18.2:** Define a "semantic inverse" operation. If `summarize` compresses, what's its inverse? Implement `expand` as the inverse and verify: `(same-meaning? text (expand (summarize text)))`. When does this break down?

**Exercise 18.3:** SICP discusses efficiency of generic operations. Define a "cost model" for semantic operations: how do you measure the efficiency of `(summarize (translate (expand text)))`? Consider token counts, latency, and meaning preservation.

**Exercise 18.4:** Build a complete "semantic polynomial" system: arguments with claims and evidence that can be added, multiplied (strengthened), negated (countered), and composed. Define `semantic-zero` (an empty argument) and `semantic-one` (a tautology).

**Exercise 18.5:** Implement the formality tower with automatic coercion. When combining texts of different formality levels, automatically raise both to the higher level before combining.

**Exercise 18.6:** Design a package system where new semantic domains can be installed at runtime from configuration files. Each domain specifies its prompts for summarize, expand, formalize, etc. Test by installing a "poetry" domain and a "code-review" domain.