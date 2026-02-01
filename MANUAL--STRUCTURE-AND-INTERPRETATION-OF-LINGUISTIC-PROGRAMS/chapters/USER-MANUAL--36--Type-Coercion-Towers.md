# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 36: Type Coercion Towers
*Corresponding to SICP Section 2.5.2: Combining Data of Different Types*

### 36.1 The Type Tower

SICP shows a tower of numeric types: integer → rational → real → complex. Coercion moves values up the tower automatically.

In semantic space, we have **style towers** and **specificity towers**:

**Formality Tower:**
```
casual → neutral → formal → legal
```

**Specificity Tower:**
```
vague → general → specific → precise → quantified
```

### 36.2 Formality Coercion

```lisp
(define (casual->neutral text)
  (effect infer.op (list "Rephrase in neutral tone: " text)))

(define (neutral->formal text)
  (effect infer.op (list "Rephrase in formal tone: " text)))

(define (formal->legal text)
  (effect infer.op (list "Rephrase in legal language: " text)))

;; Automatic path finding
(define formality-tower
  (make-coercion-tower
    '((casual neutral casual->neutral)
      (neutral formal neutral->formal)
      (formal legal formal->legal))))

(define (coerce text from-level to-level)
  (let ((path (find-coercion-path from-level to-level formality-tower)))
    (apply-coercion-path text path)))

Ω> (coerce "hey what's up" 'casual 'legal)
;; Automatically applies: casual→neutral→formal→legal
=> "Greetings. I hereby inquire as to your current status."
```

The system finds the **shortest path** through the tower automatically (using the existing `src/core/generic/coercion.ts` implementation).

### 36.3 Specificity Tower

```lisp
(define (vague->general text)
  (effect infer.op (list "Make more general but less vague: " text)))

(define (general->specific text)
  (effect infer.op (list "Add specific details: " text)))

(define (specific->precise text)
  (effect infer.op (list "Add precise measurements/data: " text)))

(define (precise->quantified text)
  (effect infer.op (list "Add exact numbers and statistics: " text)))

(define specificity-tower
  (make-coercion-tower
    '((vague general vague->general)
      (general specific general->specific)
      (specific precise specific->precise)
      (precise quantified precise->quantified))))

Ω> (coerce "some people" 'vague 'quantified)
;; Path: vague→general→specific→precise→quantified
=> "Approximately 37.2% of adults aged 25-54 (n=1,247, 95% CI: 34.1%-40.3%)"
```

### 36.4 Multi-Dimensional Coercion

We can coerce along **multiple dimensions** simultaneously, navigating a 2D semantic space:

```lisp
;; Formality axis: casual ↔ neutral ↔ formal ↔ legal
(define (step-formality direction text)
  (effect infer.op
    (list (if (eq? direction 'up)
              "Make more formal: "
              "Make more casual: ")
          text)))

;; Specificity axis: vague ↔ general ↔ specific ↔ precise
(define (step-specificity direction text)
  (effect infer.op
    (list (if (eq? direction 'up)
              "Add more detail: "
              "Make more general: ")
          text)))

;; Navigate both dimensions simultaneously
(define (coerce-2d text formality-steps specificity-steps)
  (let* ((after-formality
           (cond
             ((> formality-steps 0) (step-formality 'up text))
             ((< formality-steps 0) (step-formality 'down text))
             (else text)))
         (after-specificity
           (cond
             ((> specificity-steps 0) (step-specificity 'up after-formality))
             ((< specificity-steps 0) (step-specificity 'down after-formality))
             (else after-formality))))
    after-specificity))
```

**Example runs:**

```lisp
Ω> (coerce-2d "lots of folks are worried" 2 2)
;; +2 formality, +2 specificity
=> "A substantial proportion of individuals (approximately 60-70%) express significant concern regarding this matter."

Ω> (coerce-2d "We hereby inform you of issues" -2 1)
;; -2 formality (more casual), +1 specificity (more detail)
=> "Hey, there are some problems with the login flow and email notifications."

Ω> (coerce-2d "Hey, the server at 192.168.1.1 crashed" 2 -1)
;; +2 formality, -1 specificity (more general)
=> "We regret to inform you that a critical infrastructure component has experienced a failure."
```

This creates a **2D semantic space** where text can move along orthogonal axes of formality and specificity.

### 36.5 Bidirectional Coercion

Towers work in **both directions**:

```lisp
(define (formal->neutral text)
  (effect infer.op (list "Simplify to neutral tone: " text)))

(define (neutral->casual text)
  (effect infer.op (list "Make more casual: " text)))

;; Add reverse coercions
(extend-tower! formality-tower
  '((formal neutral formal->neutral)
    (neutral casual neutral->casual)))

Ω> (coerce "I hereby request your assistance" 'legal 'casual)
;; Path: legal→formal→neutral→casual
=> "Hey, can you help me out?"
```

### 36.6 Coercion Graph

The implementation (in `src/core/generic/coercion.ts`) uses a **coercion graph** with pathfinding:

```lisp
;; Graph allows multiple paths
(define rich-tower
  (make-coercion-graph
    '((casual neutral casual->neutral)
      (casual colloquial casual->colloquial)  ; Alternative path
      (colloquial neutral colloquial->neutral)
      (neutral formal neutral->formal)
      (formal legal formal->legal))))

;; Shortest path algorithm finds optimal route
(find-coercion-path 'casual 'formal rich-tower)
=> (casual neutral formal)  ; Direct path, not via colloquial
```

### 36.7 Practical Applications

**Email tone adjustment:**
```lisp
(define (adjust-email email recipient-type)
  (let ((current-tone (detect-tone email))
        (target-tone (appropriate-tone recipient-type)))
    (coerce email current-tone target-tone formality-tower)))

Ω> (adjust-email
     "hey can u send me the report"
     'executive)
=> "Good morning. Would you please send me the report at your earliest convenience?"
```

**Content localization:**
```lisp
(define (localize content target-culture)
  (let ((formality (cultural-formality target-culture))
        (directness (cultural-directness target-culture)))
    (multi-coerce content
      (list (list 'neutral formality formality-tower)
            (list 'direct directness directness-tower)))))
```

### 36.8 Coercion Failures

Not all coercions are possible:

```lisp
(find-coercion-path 'casual 'medical-jargon formality-tower)
=> #f  ; No path exists

;; Handle gracefully
(define (safe-coerce text from to tower)
  (let ((path (find-coercion-path from to tower)))
    (if path
        (apply-coercion-path text path)
        (begin
          (display "Warning: no coercion path from " from " to " to)
          text))))  ; Return unchanged
```

### 36.9 Key Insights

- Semantic towers organize related transformations
- Automatic pathfinding finds coercion sequences
- Multiple towers handle different semantic dimensions
- Bidirectional coercion enables flexible transformations
- Graph structure allows alternative paths
- Uses existing `coercion.ts` implementation (476 lines, BFS/Dijkstra)

**Next:** Chapter 37 explores mutable queues and tables for conversation management!
