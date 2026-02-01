# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 27: Logic Programming with Semantic Facts
### 27.1 A New Way of Thinking

SICP Chapter 4.4 introduces logic programming—a paradigm where you state *what* is true rather than *how* to compute. The query system finds answers by searching a database of facts and rules.

In semantic programming, this becomes extraordinarily powerful. Instead of pattern-matching on symbols, we can query over *meanings*:

```lisp
; Traditional logic programming:
(fact (parent alice bob))
(fact (parent bob charlie))
(rule (grandparent ?x ?z) (parent ?x ?y) (parent ?y ?z))
(query (grandparent alice ?who))
=> charlie

; SEMANTIC logic programming:
(semantic-fact "Alice gave birth to Bob in 1960")
(semantic-fact "Bob is the father of Charlie")
(semantic-query "Who is Alice's grandchild?")
=> "Charlie"
```

The system doesn't match patterns—it *understands* relationships.

### 27.2 Asserting Semantic Facts

Facts in semantic logic programming are natural language statements:

```lisp
(define (make-semantic-db)
  (let ((facts '()))
    (lambda (msg)
      (cond
        ((eq? msg 'assert)
         (lambda (fact)
           (set! facts (cons fact facts))))
        ((eq? msg 'query)
         (lambda (question)
           (effect infer.op
             (list "Based ONLY on these facts:\n"
                   (string-join facts "\n")
                   "\n\nAnswer: " question
                   "\nIf the answer cannot be determined from the facts, say 'unknown'."))))
        ((eq? msg 'facts) facts)))))

(define db (make-semantic-db))

Ω> ((db 'assert) "Marie Curie won the Nobel Prize in Physics in 1903")
Ω> ((db 'assert) "Marie Curie won the Nobel Prize in Chemistry in 1911")
Ω> ((db 'assert) "Marie Curie was born in Poland")

Ω> ((db 'query) "How many Nobel Prizes did Marie Curie win?")
=> "Two - one in Physics and one in Chemistry"

Ω> ((db 'query) "What year did Marie Curie die?")
=> "unknown"  ; Not in the facts!
```

### 27.3 Semantic Rules

Rules in traditional logic programming define derived relationships. In semantic logic programming, rules are *natural language patterns*:

```lisp
(define (add-semantic-rule db condition conclusion)
  ((db 'assert)
   (string-append "RULE: If " condition ", then " conclusion)))

Ω> (add-semantic-rule db
     "someone wins multiple Nobel Prizes"
     "they are exceptionally accomplished")

Ω> ((db 'query) "Is Marie Curie exceptionally accomplished?")
=> "Yes - she won multiple Nobel Prizes, which according to the rules indicates exceptional accomplishment"
```

### 27.4 Backward Chaining

Logic programming uses backward chaining: to prove a goal, find rules whose conclusions match and try to prove their conditions:

```lisp
(define (semantic-prove db goal)
  (let ((direct ((db 'query) goal)))
    (if (not (eq? direct "unknown"))
        direct
        ; Try to find a rule that concludes this
        (let ((applicable-rule ((db 'query)
                (string-append "Is there a rule whose conclusion relates to: " goal "?"))))
          (if (not (eq? applicable-rule "unknown"))
              ; Extract condition and recursively prove
              (let ((condition (extract-condition applicable-rule)))
                (semantic-prove db condition))
              "Cannot prove")))))
```

### 27.5 Semantic Unification

Traditional unification matches patterns like `(parent ?x bob)` with `(parent alice bob)`, binding `?x` to `alice`.

Semantic unification matches *meanings*:

```lisp
(define (semantic-unify pattern text)
  (effect infer.op
    (list "Extract values for the variables in this pattern from the text.\n"
          "Pattern: " pattern "\n"
          "Text: " text "\n"
          "Return variable bindings as JSON or 'no match'.")))

Ω> (semantic-unify
     "?person won a ?award in ?field"
     "Marie Curie received the Nobel Prize in Chemistry")
=> {"person": "Marie Curie", "award": "Nobel Prize", "field": "Chemistry"}

Ω> (semantic-unify
     "?person won a ?award in ?field"
     "The weather is nice today")
=> "no match"
```

### 27.6 A Semantic Query Language

Putting it together, we get a query language over meaning:

```lisp
(define (semantic-sql db query-template)
  (let ((facts ((db 'facts))))
    (effect infer.op
      (list "Given these facts:\n"
            (string-join facts "\n")
            "\n\nAnswer this query: " query-template
            "\nReturn results as a list."))))

Ω> (semantic-sql db "SELECT all people who won awards in science fields")
=> ("Marie Curie")

Ω> (semantic-sql db "SELECT awards WHERE winner was born in Poland")
=> ("Nobel Prize in Physics", "Nobel Prize in Chemistry")
```

### 27.7 Frame-Based Queries

SICP's query system manipulates *frames*—bindings of pattern variables to values. In semantic queries, frames bind variables to *meanings*:

```lisp
(define (make-frame) '())

(define (extend-frame var val frame)
  (cons (cons var val) frame))

(define (lookup-in-frame var frame)
  (let ((binding (assoc var frame)))
    (if binding (cdr binding) #f)))

; Semantic frame extension via unification
(define (semantic-extend pattern text frame)
  (let ((bindings (semantic-unify pattern text)))
    (if (eq? bindings "no match")
        #f
        (fold-left (lambda (f binding)
                     (extend-frame (car binding) (cdr binding) f))
                   frame
                   (json->list bindings)))))

Ω> (semantic-extend "?person studies ?subject"
                     "Alice is learning quantum mechanics"
                     (make-frame))
=> ((person . "Alice") (subject . "quantum mechanics"))
```

### 27.8 Compound Queries

SICP supports `and`, `or`, and `not` in queries. Semantic queries can use these too:

```lisp
(define (semantic-and db q1 q2)
  (let ((r1 ((db 'query) q1)))
    (if (eq? r1 "unknown")
        "unknown"
        (let ((r2 ((db 'query) q2)))
          (if (eq? r2 "unknown")
              "unknown"
              (effect infer.op
                (list "Combine these answers: " r1 " AND " r2)))))))

(define (semantic-or db q1 q2)
  (let ((r1 ((db 'query) q1)))
    (if (not (eq? r1 "unknown"))
        r1
        ((db 'query) q2))))

(define (semantic-not db q)
  (let ((r ((db 'query) q)))
    (if (eq? r "unknown")
        "yes"  ; Can't prove it, so negation-as-failure says it's false
        (effect infer.op (list "Negate this: " r)))))

Ω> (semantic-and db "Who won Nobel Prizes?" "In what fields?")
=> "Marie Curie won Nobel Prizes in Physics and Chemistry"

Ω> (semantic-not db "Did Marie Curie win a Nobel Prize in Literature?")
=> "yes"  ; Cannot be proved from facts
```

### 27.9 The Closed World Assumption

SICP's query system operates under the **closed world assumption**: what cannot be proved is false. In semantic logic programming, this becomes subtle:

```lisp
(define (closed-world-query db question)
  (let ((answer ((db 'query) question)))
    (cond
      ((string-contains? answer "unknown") #f)
      ((string-contains? answer "cannot determine") #f)
      ((string-contains? answer "not stated") #f)
      (else answer))))

Ω> ((db 'assert) "All the planets in our solar system orbit the Sun")
Ω> ((db 'assert) "Earth is a planet in our solar system")

Ω> (closed-world-query db "Does Earth orbit the Sun?")
=> "Yes, Earth orbits the Sun"

Ω> (closed-world-query db "Does Mars orbit the Sun?")
=> #f  ; Mars not mentioned in facts, so closed world says unknown
```

### 27.10 Inference vs. Retrieval

A crucial distinction: **retrieval** finds facts that match; **inference** derives new facts from existing ones. The LLM enables both:

```lisp
; Pure retrieval - find facts that mention something
(define (retrieve db term)
  (filter (lambda (f) (string-contains? f term))
          ((db 'facts))))

; Inference - derive new information
(define (infer db question)
  (effect infer.op
    (list "Using logical reasoning on these facts:\n"
          (string-join ((db 'facts)) "\n")
          "\n\nWhat can you infer about: " question)))

Ω> ((db 'assert) "Socrates is a man")
Ω> ((db 'assert) "All men are mortal")

Ω> (retrieve db "Socrates")
=> ("Socrates is a man")

Ω> (infer db "Is Socrates mortal?")
=> "Yes. Socrates is a man, and all men are mortal, therefore Socrates is mortal."
```

The LLM performs the syllogism automatically—no explicit "mortal(?x) :- man(?x)" rule needed.

### 27.11 The Power and Limits of Semantic Logic

What can semantic logic programming do that traditional logic programming cannot?

**Powers:**
- Handle vague concepts: "Is this text professional?"
- Perform analogical reasoning: "X is to Y as A is to ?"
- Leverage world knowledge not in the database
- Understand synonyms, paraphrases, implications

**Limits:**
- Non-deterministic: same query may give different answers
- Unreliable for precise logical deduction
- May hallucinate connections not supported by facts
- Expensive: each "inference step" is an LLM call

```lisp
; The power: understanding synonyms
Ω> ((db 'assert) "Marie Curie received the Nobel Prize")
Ω> ((db 'query) "Did Marie Curie win a Nobel?")
=> "Yes"  ; LLM understands "received" ≈ "won"

; The danger: hallucination
Ω> ((db 'query) "What year did Marie Curie win her first Nobel?")
=> "1903"  ; Correct, but where did this come from?
           ; It's world knowledge, not in our facts!
```

### 27.12 Exercises

**Exercise 27.1:** Build a family tree in the semantic database using natural language facts. Query for grandparents, cousins, and siblings without explicitly defining these relationships.

**Exercise 27.2:** Implement semantic negation: `(semantic-not-provable? db statement)` that returns true only if the statement cannot be derived from the facts.

**Exercise 27.3:** Create a "semantic Prolog" with cut (!). How does commitment to a proof path work when proofs are semantic rather than syntactic?

**Exercise 27.4:** SICP's query system can loop infinitely on certain recursive rules. Can semantic query systems loop? Design a rule set that might cause the LLM to go in circles.

**Exercise 27.5:** Implement `(semantic-all db pattern)` that returns ALL bindings matching a pattern, not just one. How do you ensure completeness when the LLM might miss some?

**Exercise 27.6:** Build a semantic database for a domain you know (recipes, movies, history). Test whether the LLM can answer questions requiring multi-step inference.

**Exercise 27.7:** Implement a "confidence-aware" query system that returns not just answers but confidence levels. When should low-confidence answers be treated as "unknown"?

**Exercise 27.8:** SICP discusses the frame problem in AI. How does the frame problem manifest in semantic logic programming? Can the LLM help solve it?