# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 41: Unification and Semantic Pattern Matching
*Corresponding to SICP Section 4.4.2: How the Query System Works (Unification)*

### 41.1 Beyond Syntactic Matching

SICP's unification matches patterns syntactically. **Semantic unification** matches by meaning:

```lisp
;; Pattern: (book-flight from: ?origin to: ?dest)

;; Syntactic matching would fail on variations
;; Semantic matching succeeds:

(unify-semantic
  '(book-flight from: ?origin to: ?dest)
  "I want to fly from Boston to Seattle")
=> ((?origin "Boston") (?dest "Seattle"))

(unify-semantic
  '(book-flight from: ?origin to: ?dest)
  "Book me BOS → SEA please")
=> ((?origin "Boston") (?dest "Seattle"))
;; LLM handles "BOS" = "Boston", arrow notation, etc.
```

### 41.2 Frame-Based Unification

Linguistic **frames** have slots that get filled:

```lisp
(define commercial-transaction-frame
  '(frame: buying
    buyer: ?buyer
    seller: ?seller
    goods: ?goods
    money: ?money
    time: ?when))

(define (unify-frame frame input)
  (effect infer.op
    (list "Extract frame slots from input:\n"
          "Frame: " frame "\n"
          "Input: " input "\n"
          "Return bindings as ((slot value) ...)")))

Ω> (unify-frame commercial-transaction-frame
     "Alice bought a laptop from Bob for $1000 yesterday")

=> ((buyer: "Alice")
    (seller: "Bob")
    (goods: "laptop")
    (money: "$1000")
    (time: "yesterday"))

Ω> (unify-frame commercial-transaction-frame
     "Bob sold Alice a laptop yesterday for $1000")

;; Same unification even though syntax differs!
=> ((buyer: "Alice")
    (seller: "Bob")
    (goods: "laptop")
    (money: "$1000")
    (time: "yesterday"))
```

The LLM understands "Bob sold Alice" means Bob is seller, Alice is buyer.

### 41.3 Partial Unification

```lisp
Ω> (unify-frame commercial-transaction-frame
     "Alice bought something expensive yesterday")

=> ((buyer: "Alice")
    (goods: ?unknown)
    (money: "expensive")  ; Qualitative, not quantitative
    (time: "yesterday")
    (seller: ?unknown))
```

Unification succeeds even with missing information.

### 41.4 Intent Pattern Matching

```lisp
(define intent-patterns
  '((book-flight (from ?origin) (to ?destination) (on ?date))
    (check-weather (in ?location) (on ?date))
    (cancel-reservation (id ?reservation-id))
    (complaint (about ?product) (issue ?problem))))

(define (match-intent input)
  (let ((matches
         (map (lambda (pattern)
                (let ((bindings (unify-semantic pattern input)))
                  (if bindings
                      (list (pattern-name pattern) bindings)
                      #f)))
              intent-patterns)))
    (filter identity matches)))

Ω> (match-intent "I need to fly to NYC next Tuesday")

=> ((book-flight ((destination "NYC") (date "next Tuesday"))))
;; Binds ?destination and ?date, ?origin unbound

Ω> (match-intent "What's the weather in Paris tomorrow?")

=> ((check-weather ((location "Paris") (date "tomorrow"))))
```

### 41.5 Unification with Constraints

```lisp
(define (unify-with-constraints pattern input constraints)
  (let ((bindings (unify-semantic pattern input)))
    (if (all (lambda (c) (satisfied? c bindings))
             constraints)
        bindings
        #f)))

;; Constraint: destination must be in Europe
(define europe-constraint
  (lambda (bindings)
    (let ((dest (lookup '?destination bindings)))
      (effect infer.op
        (list "Is " dest " in Europe? yes/no")))))

Ω> (unify-with-constraints
     '(book-flight to: ?destination)
     "Book flight to Tokyo"
     (list europe-constraint))

=> #f  ; Tokyo not in Europe, unification fails with constraint
```

### 41.6 Response Template Filling

Unification works backwards to **generate** from templates:

```lisp
(define apology-template
  '(template: "I apologize for {issue}. We'll {remedy}."
    slots: (?issue ?remedy)))

(define (fill-template template context)
  (let* ((slots (template-slots template))
         (bindings
          (effect infer.op
            (list "Fill template slots from context:\n"
                  "Template: " (template-text template) "\n"
                  "Slots: " slots "\n"
                  "Context: " context))))
    (instantiate (template-text template) bindings)))

Ω> (fill-template apology-template
     "Customer complains their order arrived late")

=> "I apologize for the delayed delivery. We'll process a refund and ensure faster shipping next time."
```

### 41.7 Key Insights

- Semantic unification matches by meaning, not syntax
- Frame-based unification fills structured slots
- Partial unification handles incomplete information
- Constraints filter unifications
- Templates + unification = generation

**Next:** Chapter 42 builds query systems with semantic facts!
