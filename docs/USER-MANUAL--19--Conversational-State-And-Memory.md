# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 19: Conversational State and Memory
*Corresponding to SICP Section 3.1: Assignment and Local State*

### 19.1 The Need for Local State

SICP Chapter 3.1 introduces a fundamental shift: from pure functional programming to programming with **assignment and local state**. Until now, we could model computation as the evaluation of expressions. With state, objects have *history*—their behavior depends not just on inputs, but on what happened before.

In semantic programming, state is not optional—it's essential. Consider dialogue:

```
Turn 1: "My name is Alex"
Turn 2: "Nice to meet you, Alex"  ← Requires remembering Turn 1
Turn 3: "How old are you?"
Turn 4: "I'm 25"
Turn 5: "So Alex is 25 years old" ← Requires remembering Turns 1 and 4
```

Without state, each turn would be isolated. The system couldn't remember "Alex" from Turn 1 when generating Turn 2. **Conversational memory** is the semantic analog of SICP's bank account balance.

### 19.2 Local State Variables

SICP's bank account uses a local variable `balance` modified by `set!`:

```lisp
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  ...)
```

The semantic analog: a **conversation object** with local history:

```lisp
(define (make-conversation)
  (let ((history '()))  ; Local state variable
    (define (add-turn speaker text)
      (set! history (cons (list speaker text) history)))

    (define (format-history)
      (string-join
        (reverse (map (lambda (turn)
                        (string-append (car turn) ": " (cadr turn)))
                      history))
        "\n"))

    (define (respond input)
      (let ((response (effect infer.op
                        (list "Given this conversation:\n"
                              (format-history)
                              "\n\nUser: " input
                              "\nAssistant:"))))
        (add-turn "User" input)
        (add-turn "Assistant" response)
        response))

    (define (dispatch msg)
      (cond
        ((eq? msg 'respond) respond)
        ((eq? msg 'history) (format-history))
        ((eq? msg 'clear) (lambda () (set! history '())))
        (else (error "Unknown message" msg))))

    dispatch))
```

Using the conversation:

```lisp
(define conv (make-conversation))

Ω> ((conv 'respond) "My favorite color is blue")
=> "That's a lovely choice! Blue is often associated with calm and trust."

Ω> ((conv 'respond) "What's my favorite color?")
=> "You mentioned your favorite color is blue."

Ω> ((conv 'respond) "Why do you think I like it?")
=> "You might appreciate blue for its calming qualities."

Ω> (conv 'history)
=> "User: My favorite color is blue
    Assistant: That's a lovely choice! Blue is often associated with calm and trust.
    User: What's my favorite color?
    Assistant: You mentioned your favorite color is blue.
    ..."
```

The object *remembers*. Each call to `respond` modifies `history`, and future responses depend on that accumulated history.

### 19.3 The Costs of Introducing Assignment

SICP devotes significant attention to the **costs** of assignment. Before assignment, we had referential transparency: an expression always evaluates to the same value. With assignment, this breaks:

```lisp
; Without assignment: (square 5) always returns 25
; With assignment: (withdraw 50) returns different values!
```

In semantic programming, the costs are analogous:

```lisp
; Pure function - always same output for same input
(define (translate-pure text lang)
  (effect infer.op (list "Translate to " lang ": " text)))

; Stateful version - output depends on history
(define (make-contextual-translator)
  (let ((previous-translations '()))
    (lambda (text lang)
      (let ((result (effect infer.op
                      (list "Given previous translations:\n"
                            (format previous-translations)
                            "\nTranslate consistently to " lang ": " text))))
        (set! previous-translations
              (cons (list text result) previous-translations))
        result))))
```

The pure version is simple to reason about. The stateful version produces more consistent translations across a document—but we can no longer predict output from input alone.

### 19.4 Identity and Change

SICP asks: what does it mean for two things to be "the same"? Before assignment, sameness was easy—two expressions are the same if they have the same value. With assignment, we need to distinguish:

- **Same value**: two accounts with $100 each
- **Same object**: two references to the same account

In semantic programming:

```lisp
(define conv1 (make-conversation))
(define conv2 (make-conversation))
(define conv3 conv1)  ; Same object as conv1

; conv1 and conv2 have the same structure, but different identity
((conv1 'respond) "Hello, my name is Alice")
((conv2 'respond) "Hello, my name is Bob")

; Now conv1 and conv2 have different histories
; conv1 and conv3 are the same object - conv3's history changed too!
Ω> (conv3 'history)
=> "User: Hello, my name is Alice..."  ; Same as conv1
```

When we ask "what's my name?" to `conv1`, it says "Alice". To `conv2`, it says "Bob". They started identical but diverged through mutation.

### 19.5 Semantic Objects with Local State

SICP builds objects with local state via message passing. Here's a semantic **persona** that maintains and evolves context:

```lisp
(define (make-persona base-personality)
  (let ((personality base-personality)
        (learned-facts '())
        (conversation-count 0))

    (define (add-context new-context)
      (set! personality (string-append personality "\n" new-context)))

    (define (learn fact)
      (set! learned-facts (cons fact learned-facts)))

    (define (ask question)
      (set! conversation-count (+ conversation-count 1))
      (effect infer.op
        (list personality
              "\n\nFacts you know: " (string-join learned-facts "; ")
              "\n\nConversation #" (number->string conversation-count)
              "\nQuestion: " question)))

    (lambda (msg)
      (cond
        ((eq? msg 'add-context) add-context)
        ((eq? msg 'learn) learn)
        ((eq? msg 'ask) ask)
        ((eq? msg 'facts) learned-facts)
        ((eq? msg 'count) conversation-count)))))

(define scientist (make-persona "You are a scientist who explains with precision."))
(define poet (make-persona "You are a poet who speaks in metaphor."))

Ω> ((scientist 'ask) "What is rain?")
=> "Rain is precipitation in the form of liquid water droplets..."

Ω> ((poet 'ask) "What is rain?")
=> "Rain is the sky's gentle weeping, a silver curtain..."

; Teach the scientist something
((scientist 'learn) "The user prefers simple explanations")

Ω> ((scientist 'ask) "What is photosynthesis?")
=> "Plants use sunlight to make food from air and water."  ; Simpler now
```

### 19.6 Mutation and Semantic Consistency

SICP warns about the dangers of mutation. In semantic programming, careless mutation can create **logical contradictions**:

```lisp
(define (make-knowledge-base)
  (let ((facts '()))

    (define (contradicts? new-fact)
      (eq? "yes" (effect infer.op
        (list "Do any of these facts contradict '" new-fact "'? "
              (string-join facts "; ") " Answer yes or no."))))

    (define (assert fact)
      (if (and (not (null? facts)) (contradicts? fact))
          (error "Contradiction detected: " fact)
          (begin (set! facts (cons fact facts)) 'ok)))

    (define (query question)
      (effect infer.op
        (list "Given these facts: " (string-join facts "; ")
              "\nAnswer: " question)))

    (lambda (msg)
      (cond
        ((eq? msg 'assert) assert)
        ((eq? msg 'query) query)
        ((eq? msg 'facts) facts)))))

(define kb (make-knowledge-base))

Ω> ((kb 'assert) "Alice is Bob's mother")
=> ok

Ω> ((kb 'assert) "Bob was born in 1950")
=> ok

Ω> ((kb 'assert) "Alice was born in 1980")
=> ERROR: Contradiction detected: Alice was born in 1980
```

The knowledge base **protects its consistency** by checking for contradictions before mutation.

### 19.7 Benefits of Assignment in Semantic Programming

Despite its costs, assignment enables powerful patterns:

**1. Caching (Memoization):** LLM calls are expensive. Cache results:

```lisp
(define (make-memoized-classifier)
  (let ((cache (make-table)))
    (lambda (text)
      (let ((cached (get cache text)))
        (if cached
            (begin (display "[cache hit] ") cached)
            (let ((result (effect infer.op (list "Classify as positive/negative: " text))))
              (put! cache text result)
              result))))))

(define classify (make-memoized-classifier))

Ω> (classify "Great product!")
=> "positive"           ; LLM call

Ω> (classify "Great product!")
[cache hit] => "positive"  ; No LLM call
```

**2. Accumulation:** Build up context over time:

```lisp
(define (make-summarizer)
  (let ((documents '()))
    (lambda (msg)
      (cond
        ((eq? msg 'add) (lambda (doc) (set! documents (cons doc documents))))
        ((eq? msg 'summarize-all)
         (effect infer.op
           (list "Summarize all these documents together:\n"
                 (string-join documents "\n---\n"))))))))

(define sum (make-summarizer))
((sum 'add) "First document about climate change...")
((sum 'add) "Second document about renewable energy...")
((sum 'add) "Third document about carbon capture...")

Ω> (sum 'summarize-all)
=> "Three documents discuss climate solutions: impacts, renewables, and carbon capture."
```

**3. Learning:** Adapt based on feedback:

```lisp
(define (make-adaptive-assistant)
  (let ((feedback-history '()))
    (lambda (msg)
      (cond
        ((eq? msg 'respond)
         (lambda (input)
           (effect infer.op
             (list "Previous feedback on your responses:\n"
                   (format-feedback feedback-history)
                   "\n\nRespond to: " input))))
        ((eq? msg 'feedback)
         (lambda (rating comment)
           (set! feedback-history
                 (cons (list rating comment) feedback-history))))))))
```

### 19.8 The Imperative Semantic Style

With assignment, we can write **imperative** semantic programs:

```lisp
(define (process-documents-imperatively docs)
  (let ((result ""))
    (for-each
      (lambda (doc)
        (let ((summary (effect infer.op (list "Summarize: " doc))))
          (set! result (string-append result summary "\n"))))
      docs)
    result))
```

Compare to the functional style:

```lisp
(define (process-documents-functionally docs)
  (string-join
    (map (lambda (doc) (effect infer.op (list "Summarize: " doc))) docs)
    "\n"))
```

Both produce the same result, but the functional version is easier to reason about and parallelize.

### 19.9 Exercises

**Exercise 19.1:** Implement a "therapist" object that maintains patient history, tracks emotional themes across sessions, and responds with awareness of the patient's journey. Test with a multi-turn dialogue spanning several "sessions."

**Exercise 19.2:** Build a `make-tutor` that tracks what a student has learned, what they've struggled with, and adapts explanations accordingly. If they struggled with concept A, explicitly connect it when teaching related concept B.

**Exercise 19.3:** SICP discusses "sameness" in the presence of mutation. Implement `semantic-equal?` that determines if two semantic objects have equivalent knowledge/state, even if they're different objects. When should this return true?

**Exercise 19.4:** Design a "semantic transaction" system that batches assertions to a knowledge base, validates the entire batch for consistency, and rolls back all changes if any contradiction is detected.

**Exercise 19.5:** Implement a conversation object that can "fork" into two independent conversations sharing initial history but diverging afterward. How does this relate to SICP's discussion of object identity?

**Exercise 19.6:** Build a memoized semantic function with an LRU (least recently used) cache that limits memory usage. When the cache is full, evict the oldest entries.