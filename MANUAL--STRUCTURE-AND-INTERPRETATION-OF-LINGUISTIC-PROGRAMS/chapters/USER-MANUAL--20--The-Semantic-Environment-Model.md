# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 20: The Semantic Environment Model
### 20.1 How Meaning Acquires Context

SICP Chapter 3.2 introduces the environment model of evaluation—the mechanism by which variables acquire values. An environment is a sequence of frames, each containing bindings. When we evaluate a variable, we search frames from innermost to outermost.

In semantic programming, we have an analogous concept: **the context in which meaning is interpreted**. The same words mean different things in different contexts:

```lisp
; In a medical context
(effect infer.op "What does 'positive' mean in this context: The test came back positive")
=> "The test detected the presence of the condition being tested for"

; In an emotional context
(effect infer.op "What does 'positive' mean in this context: Stay positive during difficult times")
=> "Maintaining an optimistic or hopeful attitude"
```

The word "positive" has different meanings depending on the **semantic environment**.

### 20.2 Frames of Understanding

Just as SICP's environments have frames, semantic environments have layers of context:

```lisp
(define (make-semantic-environment)
  (let ((frames '()))
    (lambda (msg)
      (cond
        ((eq? msg 'extend)
         (lambda (context)
           (set! frames (cons context frames))))
        ((eq? msg 'frames) frames)
        ((eq? msg 'interpret)
         (lambda (text)
           (effect infer.op
             (list "Given these contextual layers (most specific first):\n"
                   (string-join frames "\n")
                   "\n\nInterpret: " text))))))))

(define env (make-semantic-environment))

Ω> ((env 'extend) "We are discussing software development")
Ω> ((env 'extend) "Specifically, we are in a code review")
Ω> ((env 'interpret) "This looks buggy")
=> "The code under review appears to contain defects or errors"
```

### 20.3 Lexical Scope for Prompts

SICP explains lexical scoping: a procedure's free variables are looked up in the environment where the procedure was *defined*, not where it's *called*. This is crucial for closures.

In semantic programming, we have **prompt closures**—functions that capture their semantic context:

```lisp
(define (make-domain-expert domain)
  (let ((context (string-append "You are an expert in " domain)))
    (lambda (question)
      ; The 'context' is captured from definition time
      (effect infer.op (list context "\nQuestion: " question)))))

(define legal-expert (make-domain-expert "contract law"))
(define medical-expert (make-domain-expert "cardiology"))

; Each expert "closes over" its domain context
Ω> (legal-expert "What is consideration?")
=> "Consideration is something of value exchanged between parties to form a valid contract"

Ω> (medical-expert "What is consideration?")
=> "I'm not sure what you mean by 'consideration' in a cardiology context. Could you clarify?"
```

The legal expert interprets "consideration" legally; the medical expert is confused because it's not a cardiology term. **Same question, different semantic environments, different interpretations.**

### 20.4 Dynamic Scope and Conversation

While lexical scope is usually preferred, SICP mentions dynamic scope—where variables are looked up in the *calling* environment. In semantic programming, conversation history creates dynamic context:

```lisp
(define conversation-context '())

(define (with-conversation-context utterance thunk)
  (let ((old-context conversation-context))
    (set! conversation-context (cons utterance conversation-context))
    (let ((result (thunk)))
      (set! conversation-context old-context)
      result)))

(define (interpret text)
  (effect infer.op
    (list "Conversation so far:\n"
          (string-join (reverse conversation-context) "\n")
          "\nInterpret: " text)))

Ω> (with-conversation-context "We're planning a surprise party"
     (lambda ()
       (interpret "Don't tell anyone!")))
=> "Keep the party plans secret from the person being surprised"
```

The interpretation depends on the *dynamic* conversation context at call time.

### 20.5 Environment Diagrams for Semantic Computation

SICP uses environment diagrams to visualize computation. For semantic computation:

```
Global Environment:
┌─────────────────────────────────┐
│ domain: "healthcare"            │
│ audience: "patients"            │
└─────────────────────────────────┘
           ↑
           │ (enclosing)
           │
Local Frame (procedure call):
┌─────────────────────────────────┐
│ topic: "diabetes management"    │
│ tone: "reassuring"              │
└─────────────────────────────────┘
           ↑
           │ (enclosing)
           │
Innermost Frame (current context):
┌─────────────────────────────────┐
│ question: "What about sugar?"   │
└─────────────────────────────────┘

Interpretation: Combine all frames to form the complete semantic context
=> "In healthcare, explaining diabetes management to patients in a reassuring tone,
    answering their question about sugar intake"
```

### 20.6 The Frame Problem in Semantics

SICP's environment model solves the problem of "where do variables come from?" In AI, there's a famous "frame problem": how do you know what context is relevant?

Semantic environments face this too:

```lisp
; Too little context - ambiguous
(effect infer.op "Is it safe?")
=> "I need more context to answer this question"

; Too much context - confused
(effect infer.op "Given: global economics, Renaissance art, quantum physics,
                  cooking techniques, and sports statistics. Is it safe?")
=> "I'm not sure which domain you're asking about"

; Just right - focused
(effect infer.op "In the context of food handling, is leaving milk out overnight safe?")
=> "No, milk should not be left at room temperature for more than 2 hours"
```

The art of semantic programming is constructing environments that provide **sufficient context without noise**.

### 20.7 Exercises

**Exercise 20.1:** Implement a semantic environment with `lookup` that searches frames for relevant context. Given a question about "inheritance," it should find whether we're in a legal, programming, or biological context.

**Exercise 20.2:** Create a "semantic closure" that captures conversational context at definition time. Compare with a dynamic version. Show an example where they produce different results.

**Exercise 20.3:** SICP's environment model enables recursive procedures. Design a recursive semantic procedure where inner calls see the accumulated context of outer calls.

**Exercise 20.4:** The frame problem asks: what changes and what stays the same? Implement a `semantic-update` that modifies only relevant parts of context while preserving the rest.

---