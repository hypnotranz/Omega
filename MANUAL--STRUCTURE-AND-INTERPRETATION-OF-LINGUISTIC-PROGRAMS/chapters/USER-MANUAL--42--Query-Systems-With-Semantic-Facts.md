# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 42: Query Systems with Semantic Facts
*Corresponding to SICP Section 4.4: Logic Programming*

### 42.1 Conversational Memory as Fact Base

Instead of static knowledge, build a **dynamic fact base** from conversation:

```lisp
(define conversation-memory (make-fact-base))

(define (process-utterance utterance)
  (let ((facts (effect infer.op
                 (list "Extract factual assertions from: " utterance
                       "\nReturn as list of declarative sentences."))))
    (for-each (lambda (fact) (assert! fact conversation-memory))
              facts)))

;; User: "I live in Boston and prefer email communication"
Ω> (process-utterance "I live in Boston and prefer email communication")
;; Asserts:
;; - "User lives in Boston"
;; - "User prefers email communication"

;; User: "My budget is $50k"
Ω> (process-utterance "My budget is $50k")
;; Asserts: "User's budget is $50,000"
```

### 42.2 Natural Language Queries

Query with natural language, not formal logic:

```lisp
(define (query question fact-base)
  (effect infer.op
    (list "Answer using only these facts:\n"
          "Facts: " (get-all-facts fact-base) "\n"
          "Question: " question)))

Ω> (query "What city is the user in?" conversation-memory)
=> "Boston"

Ω> (query "How should I contact the user?" conversation-memory)
=> "Email (user stated preference for email communication)"

Ω> (query "Can the user afford a $40k purchase?" conversation-memory)
=> "Yes (user's budget is $50k, which exceeds $40k)"
```

The LLM performs **semantic search and inference**.

### 42.3 Inference Chains

```lisp
;; Add relational facts
(assert! "Boston is in Massachusetts" conversation-memory)
(assert! "Massachusetts is in the United States" conversation-memory)
(assert! "The United States is in North America" conversation-memory)

Ω> (query "What continent is the user in?" conversation-memory)

=> "North America
    Reasoning: User lives in Boston
    → Boston is in Massachusetts
    → Massachusetts is in the United States
    → The United States is in North America
    Therefore: North America"
```

The LLM **chains facts** to derive answers.

### 42.4 Rules as Facts

```lisp
;; Assert rules
(assert! "If X is-in Y and Y is-in Z, then X is-in Z" conversation-memory)
(assert! "If someone's budget exceeds a price, they can afford it" conversation-memory)

(define (query-with-rules question facts rules)
  (effect infer.op
    (list "Answer using facts and inference rules:\n"
          "Facts: " facts "\n"
          "Rules: " rules "\n"
          "Question: " question
          "\nShow reasoning.")))

Ω> (query-with-rules
     "Can the user afford a Tesla Model 3?"
     (get-facts conversation-memory)
     (get-rules conversation-memory))

=> "Yes.
    Reasoning:
    - Tesla Model 3 base price is $40k (general knowledge)
    - User's budget is $50k (from facts)
    - Rule: budget exceeds price → can afford
    - $50k > $40k → user can afford it"
```

### 42.5 Temporal Queries

```lisp
(assert! "User mentioned headache on 2024-01-15" conversation-memory)
(assert! "User mentioned headache on 2024-01-20" conversation-memory)
(assert! "User mentioned headache on 2024-01-25" conversation-memory)

Ω> (query "How many times did the user mention headaches in January?" conversation-memory)
=> "3 times (Jan 15, 20, 25)"

Ω> (query "Is this a recurring issue?" conversation-memory)
=> "Yes (user mentioned headaches 3 times over 10-day period, suggesting recurrence)"
```

### 42.6 Contextual Fact Retrieval

```lisp
(define (relevant-facts-for question fact-base)
  (effect infer.op
    (list "Which facts are relevant to: " question
          "\nAll facts: " (get-all-facts fact-base)
          "\nReturn list of relevant facts only.")))

Ω> (relevant-facts-for
     "What should I recommend for the user's commute?"
     conversation-memory)

=> ("User lives in Boston"
    "User prefers email communication"  ; Irrelevant!
    "User's budget is $50k")

;; Better: filter first
(define filtered (relevant-facts-for "commute recommendation" conversation-memory))
;; Then query using only relevant facts
(query "What should I recommend?" filtered)
```

### 42.7 Contradictions and Updates

```lisp
(assert! "User lives in Boston" conversation-memory)
;; Later...
(assert! "User lives in Seattle" conversation-memory)

;; Detect contradiction
Ω> (detect-contradictions conversation-memory)
=> ((contradiction
      ("User lives in Boston" "User lives in Seattle")
      reason: "Cannot live in two cities simultaneously"))

;; Resolve: newer fact supersedes
(retract! "User lives in Boston" conversation-memory)
(assert! "User moved from Boston to Seattle" conversation-memory)
```

### 42.8 Practical: Personalized Assistant

```lisp
(define assistant
  (make-query-system conversation-memory))

;; Build memory from conversation
(process-utterance "I'm vegetarian")
(process-utterance "I love Italian food")
(process-utterance "I'm allergic to nuts")

;; Query for recommendations
Ω> (query "What restaurant should I recommend?" assistant)
=> "An Italian restaurant with vegetarian options and nut-free menu
    (user is vegetarian, loves Italian food, allergic to nuts)"

;; Memory-based personalization
Ω> (query "Should I offer them the chef's special (contains walnuts)?" assistant)
=> "No (user is allergic to nuts, chef's special contains walnuts)"
```

### 42.9 Key Insights

- Facts extracted from natural language conversation
- Queries in natural language, not formal logic
- Inference chains derive new knowledge
- Rules enable general reasoning
- Temporal and contextual queries
- Contradictions detected and resolved
- Enables personalized, memory-based assistants

**Next:** Chapter 43 explores analyzing evaluators for optimization!
