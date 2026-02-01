# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 38: Constraint Propagation Networks
*Corresponding to SICP Section 3.3.5: Propagation of Constraints*

### 38.1 Constraints vs. One-Way Computation

SICP shows constraint propagation: set one variable, constraints propagate to determine others. Unlike one-way functions, constraints are **bidirectional**.

For semantic content, constraints ensure **coherence**: if we commit to a stance in paragraph 1, that constrains what we can say in paragraph 2.

### 38.2 Semantic Coherence Constraints

First, we create **connectors**—value holders that can propagate changes:

```lisp
(define (make-connector name)
  (list 'connector name 'unset))

(define (get-value c) (caddr c))
(define (set-value! c val)
  (set-car! (cddr c) val))
```

Now we build a **bidirectional constraint** linking stance, tone, and vocabulary:

```lisp
(define (stance-tone-vocabulary-constraint stance-c tone-c vocab-c)
  (define (propagate-from-stance)
    (let ((s (get-value stance-c)))
      (cond
        ((equal? s 'critical)
         (set-value! tone-c 'formal)
         (set-value! vocab-c 'technical))
        ((equal? s 'supportive)
         (set-value! tone-c 'casual)
         (set-value! vocab-c 'simple)))))

  (define (propagate-from-tone)
    (let ((t (get-value tone-c)))
      (cond
        ((equal? t 'casual)
         (set-value! stance-c 'supportive)
         (set-value! vocab-c 'simple))
        ((equal? t 'formal)
         (set-value! stance-c 'critical)
         (set-value! vocab-c 'technical)))))

  (list 'constraint propagate-from-stance propagate-from-tone))

;; Create connectors
(define stance-c (make-connector 'stance))
(define tone-c (make-connector 'tone))
(define vocab-c (make-connector 'vocabulary))

;; Install constraint
(define constraint (stance-tone-vocabulary-constraint stance-c tone-c vocab-c))
```

**Key property: bidirectionality**. Setting stance propagates to tone and vocabulary, OR setting tone propagates to stance and vocabulary:

```lisp
Ω> (set-value! stance-c 'critical)
Ω> ((cadr constraint))  ; propagate-from-stance
Ω> (get-value tone-c)
=> 'formal
Ω> (get-value vocab-c)
=> 'technical

;; Now reset and try the reverse direction
Ω> (set-value! tone-c 'casual)
Ω> ((caddr constraint))  ; propagate-from-tone
Ω> (get-value stance-c)
=> 'supportive
Ω> (get-value vocab-c)
=> 'simple
```

This is fundamentally different from one-way functions—we can derive stance from tone just as easily as deriving tone from stance.

### 38.3 Consistency Constraint

```lisp
(define (consistency-constraint overall para1 para2)
  (define (check-consistency)
    (let ((stance (get-value overall))
          (claim1 (get-value para1))
          (claim2 (get-value para2)))
      (if (and claim1 claim2)
          (let ((consistent?
                 (effect infer.op
                   (list "Are these consistent with " stance " stance?\n"
                         "Claim 1: " claim1 "\n"
                         "Claim 2: " claim2))))
            (if (not consistent?)
                (signal-violation "Inconsistent claims"))))))

  (on-change overall check-consistency)
  (on-change para1 check-consistency)
  (on-change para2 check-consistency))
```

**Bidirectional**: Changing any variable triggers consistency check.

### 38.4 Generating Under Constraints

```lisp
(define (generate-paragraph topic network)
  (let ((stance (get-value (network 'overall-stance)))
        (prev-claims (get-value (network 'para1-claim)))
        (terms (get-value (network 'terminology))))
    (effect infer.op
      (list "Write paragraph about " topic
            "\nMaintaining stance: " stance
            "\nConsistent with: " prev-claims
            "\nUsing terminology: " terms))))

;; Set constraints
(set-value! (network 'overall-stance) "pro-renewable-energy")
(set-value! (network 'terminology)
  '("clean energy" "sustainable" "carbon-neutral"))

;; Generate constrained content
Ω> (generate-paragraph "solar power" network)
=> "Solar power represents a crucial element of our clean energy future. This sustainable technology enables carbon-neutral electricity generation..."
```

The network **guides generation** to satisfy constraints.

### 38.5 Propagation Example

```lisp
;; Set audience constraint
(set-value! (network 'audience) 'child)

;; Propagates to:
;; - formality: casual
;; - vocabulary: simple
;; - sentence-length: short
;; - use-jargon: false

;; All paragraphs now constrained by these derived values

Ω> (generate-paragraph "photosynthesis" network)
=> "Plants make food from sunlight! They use special parts called chloroplasts. The sun's energy helps turn water and air into sugar."
;; Automatically uses simple words, short sentences
```

### 38.6 Conflict Detection

```lisp
(define (detect-conflicts network)
  (let ((constraints (network 'all-constraints)))
    (filter (lambda (c)
              (not (satisfied? c)))
            constraints)))

;; Conflicting constraints
(set-value! (network 'audience) 'expert)  ; Implies jargon OK
(set-value! (network 'avoid-jargon) #t)   ; Contradicts!

Ω> (detect-conflicts network)
=> ((audience-jargon-conflict
      "audience='expert implies use-jargon, but avoid-jargon=#t"))
```

### 38.7 Practical Application: Style Guide Enforcement

```lisp
(define style-guide-network
  (make-network
    '((if oxford-comma then serial-comma)
      (if formal-tone then no-contractions)
      (if audience=technical then use-jargon)
      (if brand-voice=friendly then casual-tone))))

;; Set brand voice
(set-value! (network 'brand-voice) 'friendly)

;; Propagates to:
;; - casual-tone: true
;; - no-contractions: false (casual allows contractions)

;; Generate constrained content
Ω> (generate-email network "service update")
=> "Hey there! We've updated our service..."
;; Uses contractions (we've), casual tone
```

### 38.8 Key Insights

- Constraints are bidirectional, not one-way functions
- Setting one variable propagates to others
- Semantic coherence maintained via constraint networks
- Conflicts detected automatically
- Generation guided by constraints
- Style guides as constraint networks

**Next:** Chapter 39 covers serializers for concurrent LLM operations!
