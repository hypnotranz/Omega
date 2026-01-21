# OmegaLLM Quick Reference

A Lisp where LLM calls are expressions. Use `(effect infer.op "prompt")` to call an LLM.

This reference has one section per chapter of the full manual. Each shows the key pattern with a minimal example.

---

## 1. Getting Started

**Warm-up REPL steps with simple definitions and evaluation.**

```lisp
(define greeting "Welcome to OmegaLLM. Describe what you want in everyday language.")
(define (echo text) text)
(echo greeting)
```

ðŸ“˜ [Full Chapter](./USER-MANUAL--01--Getting-Started.md) | ðŸŽ® [Demo](../demo/by-chapter/ch01-getting-started.ts)

---

## 2. LLM Calls as Functions

**Call infer.op inside reusable procedures.**

`effect infer.op` invokes the oracle and returns the modelâ€™s string answer so you can treat it like any other value.

```lisp
(define (analyze-sentiment text)
  (effect infer.op
    (list "What is the sentiment (positive/negative/neutral) of: " text)))
(analyze-sentiment "I love how carefully you explained the migration steps.")
```

ðŸ“˜ [Full Chapter](./USER-MANUAL--02--Llm-Calls-As-Functions.md) | ðŸŽ® [Demo](../demo/by-chapter/ch02-llm-calls.ts)

---

## 3. Functional Composition

**Map and filter with semantic predicates.**

```lisp
(define (is-complaint? text)
  (equal? "yes"
    (effect infer.op
      (list "Is this customer note a complaint? yes/no: " text))))

(define messages
  (list
    "Your latest release fixed the crash immediately."
    "My data export failed again and I'm getting frustrated."
    "Could you clarify the compliance statement for healthcare customers?"
    "This delay in refund approval feels unfair."))

(filter is-complaint? messages)
```

ðŸ“˜ [Full Chapter](./USER-MANUAL--03--Functional-Composition.md) | ðŸŽ® [Demo](../demo/by-chapter/ch03-composition.ts)

---

## 4. Higher-Order LLM Functions

**Factories that return semantic classifiers.**

`make-classifier` returns a new procedure that bakes a topic into the prompt and reuses it for each call.

```lisp
(define (make-classifier topic)
  (lambda (snippet)
    (effect infer.op
      (list "Classify this text into a " topic " bucket: " snippet))))

(define classify-risk (make-classifier "risk level (high/medium/low)"))

(list
  (classify-risk "Credentials leaked on a public git repo with customer secrets.")
  (classify-risk "Routine maintenance window notification with no user impact."))
```

ðŸ“˜ [Full Chapter](./USER-MANUAL--04--Higher-Order-Llm-Functions.md) | ðŸŽ® [Demo](../demo/by-chapter/ch04-higher-order.ts)

---

## 5. Nondeterministic Search (AMB)

**Backtrack across tone options until a semantic predicate passes.**

`amb` chooses one candidate; if `require` later fails, evaluation backtracks to try the next option automatically.

```lisp
(define tones (list "formal" "friendly" "apologetic"))

(define (matches-tone? reply desired)
  (equal? "yes"
    (effect infer.op
      (list "Does this reply use a " desired " tone? yes/no: " reply))))

(let ((tone (amb "formal" "friendly" "apologetic")))
  (let ((reply (effect infer.op
                  (list "Write a " tone " response acknowledging a delayed shipment."))))
    (require (matches-tone? reply "apologetic"))
    reply))
```

ðŸ“˜ [Full Chapter](./USER-MANUAL--05--Nondeterministic-Search.md) | ðŸŽ® [Demo](../demo/by-chapter/ch05-nondeterministic.ts)

---

## 6. Multi-Shot Sampling

**Use search.op to gather multiple semantic candidates.**

`search.op` runs several oracle samples and returns a distribution so you can see a spread of plausible answers.

```lisp
(define request "Please provide an update on the audit timeline and risk posture.")
(effect search.op
  (list "Rewrite this status update in three distinct tones: warm, concise, and executive:" request))
```

ðŸ“˜ [Full Chapter](./USER-MANUAL--06--Multi-Shot-Sampling.md) | ðŸŽ® [Demo](../demo/by-chapter/ch06-multi-shot.ts)

---

## 7. Lazy Streams

**Generate follow-up questions lazily and force only what you need.**

`list->stream` lifts a list into a lazy stream, `stream-map` applies a function lazily, and `stream->list` forces the first `n` elements.

```lisp
(define notes
  (list
    "The incident response runbook feels outdated."
    "Our healthcare customers need clearer assurances about data residency."
    "The onboarding emails sound too robotic."))

(define (follow-up note)
  (effect infer.op
    (list "Suggest one follow-up question to clarify this note. Keep it empathetic: " note)))

(define s (list->stream notes))
(define queued (stream-map follow-up s))
(stream->list queued 2)
```

ðŸ“˜ [Full Chapter](./USER-MANUAL--07--Lazy-Streams.md) | ðŸŽ® [Demo](../demo/by-chapter/ch07-lazy-streams.ts)

---

## 8. The Debugger

**Trace semantic steps with oracle explanations.**

```lisp
(define (explain step)
  (effect infer.op
    (list "Explain this debugging step in one sentence: " step)))

(list
  (explain "Check whether the classifier treated the note as a complaint.")
  (explain "Confirm the tone matcher backtracked to the apologetic branch."))
```

ðŸ“˜ [Full Chapter](./USER-MANUAL--08--The-Debugger.md) | ðŸŽ® [Demo](../demo/by-chapter/ch08-debugger.ts)

---

## 9. The Agentic REPL

**LLM asks the runtime for facts before replying.**

`oracle-lambda` creates an `OracleProc` the oracle can call like a normal function to inspect runtime data.

```lisp
(define active-tickets (list "Auth outage" "Export stalled" "Payment retry loop" "Stale cache"))
(define ask-runtime (oracle-lambda (question) "agentic-query"))
(ask-runtime "How many urgent tickets are active? Call (length active-tickets) before answering.")
```

ðŸ“˜ [Full Chapter](./USER-MANUAL--09--The-Agentic-Repl.md) | ðŸŽ® [Demo](../demo/by-chapter/ch09-agentic-repl.ts)

---

## 10. Full API Reference

**Combine infer, search, and amb in one small scenario.**

```lisp
(define (classify ticket)
  (effect infer.op
    (list "Classify this support ticket (bug/feature-request/question/complaint): " ticket)))

(define candidate (amb
  "The mobile app crashes when uploading receipts."
  "Could you add a calmer tone to the payment reminders?"
  "How do I export my audit logs to CSV?"))

(define label (classify candidate))
(define rewrites (effect search.op (list "Rewrite the ticket for an executive summary: " candidate)))
(list label rewrites)
```

ðŸ“˜ [Full Chapter](./USER-MANUAL--10--Full-Api-Reference.md) | ðŸŽ® [Demo](../demo/by-chapter/ch10-api-reference.ts)

---

## 11. Semantic Procedures as Black Boxes

**Encapsulate semantic judgment behind a predicate.**

`is-professional?` packages the prompt inside the predicate so callers only see a boolean-style result, not the prompt engineering.

```lisp
(define (is-professional? email)
  (equal? "yes"
    (effect infer.op
      (list "Is this email draft professional and calm? yes/no: " email))))

(is-professional?
  "Team, let's present findings with clarity and keep the tone reassuring for regulators.")
```

ðŸ“˜ [Full Chapter](./USER-MANUAL--11--Semantic-Procedures-As-Black-Boxes.md) | ðŸŽ® [Demo](../demo/by-chapter/ch11-semantic-procedures.ts)

---

## 12. Inference Processes

**Contrast recursive vs iterative summarization.**

```lisp
(define report
  "Customer anger escalated because the refund workflow failed twice. They also praised the clarity of the troubleshooting steps once resolved.")

(define (recursive-summarize text depth)
  (if (= depth 0)
      (effect infer.op (list "Summarize in one tight sentence: " text))
      (recursive-summarize
        (effect infer.op (list "Summarize the core issue: " text))
        (- depth 1))))

(list
  (recursive-summarize report 1)
  (effect infer.op (list "Summarize iteratively with cost awareness: " report)))
```

ðŸ“˜ [Full Chapter](./USER-MANUAL--12--Inference-Processes-Recursion-And-Iteration-In-Semantic-Space.md) | ðŸŽ® [Demo](../demo/by-chapter/ch12-inference-processes.ts)

---

## 13. Higher-Order Inference

**Fold stakeholder opinions using infer.op as the combiner.**

`fold-left` threads an accumulator through the list, letting the oracle-driven combiner synthesize consensus step by step.

```lisp
(define opinions
  (list
    "Engineering wants fewer meetings and clearer acceptance criteria."
    "Support needs a calmer tone in outage updates."
    "Legal wants explicit mention of data residency obligations."))

(define (merge consensus opinion)
  (effect infer.op
    (list "Merge this opinion into the current consensus. Keep it concise and empathetic."
          "Consensus: " consensus
          "Opinion: " opinion)))

(fold-left merge "Start with a balanced plan." opinions)
```

ðŸ“˜ [Full Chapter](./USER-MANUAL--13--Higher-Order-Inference.md) | ðŸŽ® [Demo](../demo/by-chapter/ch13-higher-order-inference.ts)

---

## 14. Semantic Data Abstraction

**Validators for natural-language structures.**

```lisp
(define (is-haiku? poem)
  (equal? "yes" (effect infer.op (list "Does this read like a calming haiku? yes/no: " poem))))

(define (has-greeting? email)
  (equal? "yes" (effect infer.op (list "Does this email open with a courteous greeting? yes/no: " email))))

(list
  (is-haiku? "Quiet dashboards hum / Incidents fall back asleep / Teams breathe evenly")
  (has-greeting? "Hello team, thank you for the latest buildâ€”can we add a changelog?"))
```

ðŸ“˜ [Full Chapter](./USER-MANUAL--14--Semantic-Data-Abstraction.md) | ðŸŽ® [Demo](../demo/by-chapter/ch14-semantic-data.ts)

---

## 15. Sequences as Semantic Interfaces

**Pipeline complaints â†' issues â†' prioritization.**

```lisp
(define complaints
  (list
    "The new security banner sounds alarming to clinicians."
    "I cannot export my case notes; the button feels hidden."
    "Payment reminders sound harsh and transactional.")) 

(define (extract-issue text)
  (effect infer.op
    (list "Extract the core issue in 6 words: " text)))

(define (prioritize issue)
  (effect infer.op
    (list "Label this issue urgency (high/medium/low): " issue)))

(map prioritize (map extract-issue complaints))
```

ðŸ“˜ [Full Chapter](./USER-MANUAL--15--Sequences-As-Semantic-Interfaces.md) | ðŸŽ® [Demo](../demo/by-chapter/ch15-sequences.ts)

---

## 16. Symbolic Semantic Data

**Meaning equivalence checks on emotionally worded phrases.**

`same-meaning?` is a semantic predicate backed by the oracle; it returns `"true"` when two phrases convey the same intent.

```lisp
(define (same-meaning? a b)
  (equal? "true"
    (effect infer.op (list "Do these mean the same thing? true/false: " a " | " b))))

(list
  (same-meaning? "I feel upset about the delay" "I am dissatisfied with how long this is taking")
  (same-meaning? "This is delightful" "This is unacceptable"))
```

ðŸ“˜ [Full Chapter](./USER-MANUAL--16--Symbolic-Semantic-Data.md) | ðŸŽ® [Demo](../demo/by-chapter/ch16-symbolic-semantic.ts)

---

## 17. Multiple Representations of Meaning

**Convert register across styles.**

```lisp
(define complaint "Your incident updates sound robotic and uncaring.")
(list
  (effect infer.op (list "Rewrite in a formal yet empathetic register: " complaint))
  (effect infer.op (list "Rewrite in a candid peer-to-peer register: " complaint)))
```

ðŸ“˜ [Full Chapter](./USER-MANUAL--17--Multiple-Representations-Of-Meaning.md) | ðŸŽ® [Demo](../demo/by-chapter/ch17-multiple-representations.ts)

---

## 18. Generic Semantic Operations

**Domain-aware summarization.**

```lisp
(define issue "Customer shared medical data while requesting a refund and asked for SOC2 proof.")

(define (summarize-legal text)
  (effect infer.op (list "Provide a legal summary highlighting duties: " text)))

(define (summarize-support text)
  (effect infer.op (list "Provide a support summary with calming reassurance: " text)))

(list (summarize-legal issue) (summarize-support issue))
```

ðŸ“˜ [Full Chapter](./USER-MANUAL--18--Generic-Semantic-Operations.md) | ðŸŽ® [Demo](../demo/by-chapter/ch18-generic-semantic.ts)

---

## 19. Conversational State and Memory

**Use prior turns as context for follow-up answers.**

```lisp
(define history
  (list
    "User: I am worried about the outage timeline."
    "Assistant: I will keep you updated every hour with calm language."
    "User: Please avoid sounding scripted in the next update."))

(effect infer.op
  (list "Given this conversation, craft the next reply that remembers prior concerns: " history))
```

ðŸ“˜ [Full Chapter](./USER-MANUAL--19--Conversational-State-And-Memory.md) | ðŸŽ® [Demo](../demo/by-chapter/ch19-conversational-state.ts)

---

## 20. The Semantic Environment Model

**Show how context shapes interpretation.**

```lisp
(define (interpret term env-note)
  (effect infer.op
    (list "Interpret the word 'bank' given this environment: " env-note ". Respond with river or finance.")))

(list
  (interpret "bank" "We studied erosion patterns near the river bank.")
  (interpret "bank" "The finance team asked the bank to extend credit."))
```

ðŸ“˜ [Full Chapter](./USER-MANUAL--20--The-Semantic-Environment-Model.md) | ðŸŽ® [Demo](../demo/by-chapter/ch20-semantic-environment.ts)

---

## 21. Mutable Semantic Structures

**Evolve a simple relation list with semantic checks.**

`set!` mutates the relation list in place so downstream semantic calls summarize the updated state.

```lisp
(define relations (list "login -> error pages" "refund -> frustration"))
(set! relations (cons "healthcare -> compliance questions" relations))

(effect infer.op
  (list "Summarize these relations in one sentence, keeping causal tone: " relations))
```

ðŸ“˜ [Full Chapter](./USER-MANUAL--21--Mutable-Semantic-Structures.md) | ðŸŽ® [Demo](../demo/by-chapter/ch21-mutable-semantic.ts)

---

## 22. Concurrent Inference

**Parallel-map sketch using semantic tasks.**

`parallel-map` applies a function to every element concurrently using fibers; here it is a sequential stand-in to show the interface.

```lisp
(define tickets
  (list
    "Cannot login to my account after password reset."
    "When will the new analytics feature be available?"
    "The app crashed and deleted my draft report."
    "How do I export my reports to PDF?"))

(define (classify ticket)
  (effect infer.op
    (list "Classify this support ticket. Return bug/feature-request/question/complaint: " ticket)))

(define (parallel-map f xs) (map f xs)) ; sequential stand-in for demo
(parallel-map classify tickets)
```

ðŸ“˜ [Full Chapter](./USER-MANUAL--22--Concurrent-Inference.md) | ðŸŽ® [Demo](../demo/by-chapter/ch22-concurrent-inference.ts)

---

## 23. Streams of Inference

**Potentially infinite semantic expansion, truncated on demand.**

`iterate` keeps expanding an idea with the oracle while decrementing a counter so you can stop after a bounded number of refinements.

```lisp
(define (expand idea)
  (effect infer.op
    (list "Generate a richer explanation building on this idea: " idea)))

(define (iterate n seed)
  (if (= n 0)
      (list seed)
      (cons seed (iterate (- n 1) (expand seed)))))

(iterate 3 "Calmly communicate risk to non-technical stakeholders.")
```

ðŸ“˜ [Full Chapter](./USER-MANUAL--23--Streams-Of-Inference.md) | ðŸŽ® [Demo](../demo/by-chapter/ch23-streams-of-inference.ts)

---

## 24. Metalinguistic Abstraction

**Oracle asks to evaluate a helper expression before answering.**

```lisp
(define helper "sanitize-and-trim")
(define explain (oracle-lambda (hint) "explain-macro"))
(explain helper)
```

ðŸ“˜ [Full Chapter](./USER-MANUAL--24--Metalinguistic-Abstraction-The-Oracle-In-The-Evaluator.md) | ðŸŽ® [Demo](../demo/by-chapter/ch24-metacircular.ts)

---

## 25. Lazy Semantic Evaluation

**Memoize a semantic result and reuse it.**

The first `analyze` call runs the oracle; subsequent calls reuse the cached value so no new LLM work is triggered.

```lisp
(define cached #f)

(define (analyze text)
  (if cached
      cached
      (begin
        (set! cached (effect infer.op (list "Assess emotional temperature (calm/tense): " text)))
        cached)))

(list
  (analyze "The outage update felt tense and robotic.")
  (analyze "Reusing the memoized tone analysis to avoid another call."))
```

ðŸ“˜ [Full Chapter](./USER-MANUAL--25--Lazy-Semantic-Evaluation.md) | ðŸŽ® [Demo](../demo/by-chapter/ch25-lazy-semantic.ts)

---

## 26. The AMB Inference Engine

**Constraint satisfaction with semantic predicates.**

```lisp
(define tones (list "formal" "empathetic" "playful"))
(define intents (list "explain risk" "apologize" "upsell"))

(define (fits? tone intent)
  (equal? "yes"
    (effect infer.op
      (list "Is this tone appropriate for the intent? yes/no: " tone " -> " intent))))

(let ((tone (amb "formal" "empathetic" "playful")))
  (let ((intent (amb "explain risk" "apologize")))
    (require (fits? tone intent))
    (list tone intent)))
```

ðŸ“˜ [Full Chapter](./USER-MANUAL--26--The-Amb-Inference-Engine.md) | ðŸŽ® [Demo](../demo/by-chapter/ch26-amb-inference.ts)

---

## 27. Logic Programming with Semantic Facts

**Query natural language facts with semantic matching.**

`is-grandparent?` asks the oracle to reason over the text facts instead of a structured table, returning yes/no.

```lisp
(define facts
  (list
    "Alice is Bob's parent."
    "Bob is Carol's parent."
    "Dana mentors Erin in compliance audits."))

(define (is-grandparent? a c)
  (equal? "yes"
    (effect infer.op
      (list "Given these facts, is " a " the grandparent of " c "? yes/no: " facts))))

(is-grandparent? "Alice" "Carol")
```

ðŸ“˜ [Full Chapter](./USER-MANUAL--27--Logic-Programming-With-Semantic-Facts.md) | ðŸŽ® [Demo](../demo/by-chapter/ch27-logic-programming.ts)

---

