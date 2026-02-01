# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 21: Mutable Semantic Structures
### 21.1 The Costs of Change

SICP Chapter 3.3 explores mutable data structures—queues, tables, and circuits that change over time. Mutation introduces complexity: we must think about *when* things happen, not just *what* they compute.

In semantic programming, mutable structures are essential for:
- Building up knowledge bases
- Maintaining conversation history
- Caching expensive LLM results
- Tracking entity state across dialogue

But semantic mutation has unique challenges: **how do you maintain consistency when meanings can conflict?**

### 21.2 Mutable Semantic Tables

The simplest mutable structure: a table mapping keys to meanings:

```lisp
(define (make-semantic-table)
  (let ((bindings '()))
    (lambda (msg)
      (cond
        ((eq? msg 'put)
         (lambda (key value)
           (set! bindings (cons (cons key value) bindings))))
        ((eq? msg 'get)
         (lambda (key)
           (let ((found (assoc key bindings)))
             (if found (cdr found) #f))))
        ((eq? msg 'update)
         (lambda (key updater)
           (let ((old ((self 'get) key)))
             ((self 'put) key (updater old)))))))))

(define memory (make-semantic-table))

Ω> ((memory 'put) 'user-name "Alice")
Ω> ((memory 'put) 'user-preference "formal")
Ω> ((memory 'get) 'user-name)
=> "Alice"
```

### 21.3 Semantic Queues: Ordering Inference

Queues are essential for processing work in order. A semantic queue holds inference tasks:

```lisp
(define (make-inference-queue)
  (let ((front '()) (rear '()))
    (lambda (msg)
      (cond
        ((eq? msg 'enqueue)
         (lambda (task)
           (set! rear (cons task rear))))
        ((eq? msg 'dequeue)
         (if (null? front)
             (if (null? rear)
                 (error "Queue empty")
                 (begin
                   (set! front (reverse rear))
                   (set! rear '())
                   ((self 'dequeue))))
             (let ((item (car front)))
               (set! front (cdr front))
               item)))
        ((eq? msg 'empty?) (and (null? front) (null? rear)))))))

; Queue of analysis tasks
(define tasks (make-inference-queue))
((tasks 'enqueue) "Analyze sentiment of document A")
((tasks 'enqueue) "Extract entities from document B")
((tasks 'enqueue) "Summarize document C")

; Process in order
(define (process-queue q)
  (if ((q 'empty?))
      'done
      (begin
        (effect infer.op ((q 'dequeue)))
        (process-queue q))))
```

### 21.4 Semantic Graphs: Entities and Relations

Real-world knowledge forms graphs—entities connected by relationships:

```lisp
(define (make-knowledge-graph)
  (let ((entities (make-semantic-table))
        (relations '()))
    (lambda (msg)
      (cond
        ((eq? msg 'add-entity)
         (lambda (id properties)
           ((entities 'put) id properties)))
        ((eq? msg 'add-relation)
         (lambda (from rel to)
           (set! relations (cons (list from rel to) relations))))
        ((eq? msg 'query)
         (lambda (question)
           (effect infer.op
             (list "Given this knowledge graph:\n"
                   "Entities: " (format-entities entities)
                   "\nRelations: " (format-relations relations)
                   "\n\nAnswer: " question))))))))

(define kg (make-knowledge-graph))

Ω> ((kg 'add-entity) 'alice '((type . person) (role . engineer)))
Ω> ((kg 'add-entity) 'acme '((type . company) (industry . tech)))
Ω> ((kg 'add-relation) 'alice 'works-at 'acme)
Ω> ((kg 'query) "Where does Alice work?")
=> "Alice works at Acme, a tech company"
```

### 21.5 Constraint Propagation in Semantic Networks

SICP describes constraint propagation networks where values flow between cells. In semantic networks, meanings propagate:

```lisp
(define (make-semantic-constraint source target constraint-fn)
  ; When source changes, update target through constraint
  (lambda (new-source-value)
    (let ((derived (constraint-fn new-source-value)))
      (set-target! target derived))))

; Example: formality constraint
; If user-preference changes, tone should update
(define (update-tone preference)
  (effect infer.op
    (list "What tone matches preference: " preference)))

Ω> (set-preference! 'formal)
; Propagates → tone becomes "Use complete sentences, avoid contractions"

Ω> (set-preference! 'casual)
; Propagates → tone becomes "Relaxed, conversational, contractions OK"
```

### 21.6 The Challenge of Semantic Consistency

Unlike numeric constraints, semantic constraints can be subtle:

```lisp
(define (add-fact-with-check kb fact)
  (let ((existing ((kb 'facts))))
    (let ((contradiction? (effect infer.op
           (list "Does this new fact contradict any existing facts?\n"
                 "Existing: " (string-join existing "\n")
                 "\nNew: " fact
                 "\nAnswer yes or no with explanation."))))
      (if (starts-with? contradiction? "yes")
          (error (string-append "Contradiction: " contradiction?))
          ((kb 'add) fact)))))

Ω> (add-fact-with-check kb "Alice is 30 years old")
=> ok

Ω> (add-fact-with-check kb "Alice is a teenager")
=> ERROR: Contradiction: yes, a 30-year-old cannot be a teenager
```

### 21.7 Temporal Semantics: State Over Time

Semantic state changes over time. We can model this:

```lisp
(define (make-temporal-entity id)
  (let ((history '()))
    (lambda (msg)
      (cond
        ((eq? msg 'update)
         (lambda (timestamp state)
           (set! history (cons (cons timestamp state) history))))
        ((eq? msg 'at)
         (lambda (timestamp)
           ; Find state at given time
           (find-state-at timestamp history)))
        ((eq? msg 'current)
         (if (null? history)
             #f
             (cdar history)))))))

(define project (make-temporal-entity 'project-x))

((project 'update) "2024-01" '((status . "planning") (team . 3)))
((project 'update) "2024-03" '((status . "development") (team . 5)))
((project 'update) "2024-06" '((status . "testing") (team . 4)))

Ω> ((project 'at) "2024-02")
=> ((status . "planning") (team . 3))

Ω> ((project 'current))
=> ((status . "testing") (team . 4))
```

### 21.8 Exercises

**Exercise 21.1:** Implement a semantic table with *versioned* values—every `put` creates a new version, and you can query historical values. Use it to track how understanding of a concept evolved.

**Exercise 21.2:** Build a circular semantic structure: two entities that reference each other. For example, "Alice manages Bob" and "Bob reports to Alice" should be represented without infinite regress.

**Exercise 21.3:** SICP builds a digital circuit simulator. Design a "semantic circuit" where "signals" are meanings that propagate through "gates" that transform them (e.g., a "formalize" gate, a "summarize" gate).

**Exercise 21.4:** Implement undo/redo for a knowledge base. After adding facts, allow rolling back to previous states while maintaining consistency.

---