# Work Items: 21 Missing Chapters

**Goal:** Write SICP-style chapters with explanations + runnable demos for all missing patterns

**Style Guide:**
- Follow SICP's pedagogical approach
- Start with simple example
- Build complexity gradually
- Show why it matters (not just how)
- Include exercises/variations
- Connect to bigger ideas

**Deliverables per chapter:**
1. Markdown chapter file (in `chapters/`)
2. Lisp demo file (in `code-examples/lisp/`)
3. TypeScript test config (in `code-examples/typescript/`)
4. Entry in DEMO-GALLERY.md

---

## PART I: Procedures Patterns (6 chapters)

### Ch28: The Substitution Model for Semantic Evaluation

**SICP Reference:** Section 1.1.5 - The Substitution Model for Procedure Application

**What to Write:**
- Explain substitution model with LLM twist
- Show how `(effect infer.op ...)` gets substituted
- Contrast with environment model (Ch20)
- Build intuition for evaluation order

**Example to Create:**
```lisp
;; Show step-by-step substitution of semantic expressions
(define (sentiment text)
  (effect infer.op (list "Sentiment of: " text)))

(sentiment "I love this!")
;; Step 1: Substitute argument
;; => (effect infer.op (list "Sentiment of: " "I love this!"))
;; Step 2: Evaluate list
;; => (effect infer.op "Sentiment of: I love this!")
;; Step 3: Execute effect
;; => "positive"
```

**Code to Reference:**
- `src/core/eval/machine.ts` - Actual evaluator (NOT substitution, but shows real model)
- Contrast with how it ACTUALLY works vs pedagogical model

**Deliverables:**
- [ ] `chapters/USER-MANUAL--28--The-Substitution-Model-For-Semantic-Evaluation.md`
- [ ] `code-examples/lisp/ch28-substitution-model.lisp`
- [ ] `code-examples/typescript/ch28-substitution-model.ts`
- [ ] Update DEMO-GALLERY.md

---

### Ch29: Iterative Semantic Refinement (Newton's Method)

**SICP Reference:** Section 1.1.7 - Example: Square Roots by Newton's Method

**What to Write:**
- Iterative refinement via LLM feedback
- Show "good enough?" predicate with semantic judgment
- Demonstrate convergence of semantic quality
- Connect to general iterative improvement patterns

**Example to Create:**
```lisp
;; Iteratively refine a response until it meets criteria
(define (good-enough? response criteria)
  (equal? "yes"
    (effect infer.op
      (list "Does this response meet the criteria? yes/no"
            "Response: " response
            "Criteria: " criteria))))

(define (improve response feedback)
  (effect infer.op
    (list "Improve this response based on feedback:"
          "Response: " response
          "Feedback: " feedback)))

(define (refine initial-response criteria max-iterations)
  (define (iter response count)
    (if (or (good-enough? response criteria)
            (= count max-iterations))
        response
        (iter (improve response
                      (effect infer.op
                        (list "Provide feedback on: " response
                              "Criteria: " criteria)))
              (+ count 1))))
  (iter initial-response 0))

;; Use it:
(refine "Hello" "Make it more professional and empathetic" 3)
```

**Code to Reference:**
- `src/core/solver/fixpoint.ts` - Fixpoint iteration
- `src/core/solver/repair.ts` - Repair strategies

**Deliverables:**
- [ ] `chapters/USER-MANUAL--29--Iterative-Semantic-Refinement.md`
- [ ] `code-examples/lisp/ch29-iterative-refinement.lisp`
- [ ] `code-examples/typescript/ch29-iterative-refinement.ts`
- [ ] Update DEMO-GALLERY.md

---

### Ch30: Tree Recursion with Semantic Branching

**SICP Reference:** Section 1.2.2 - Tree Recursion

**What to Write:**
- Tree recursion where LLM decides branch paths
- Show exponential growth of semantic possibilities
- Contrast with linear recursion (Ch12)
- Demonstrate semantic decision trees

**Example to Create:**
```lisp
;; Tree recursion: LLM decides how to branch
(define (semantic-tree-explore topic depth)
  (if (= depth 0)
      (list topic)
      (let ((subtopics
             (effect infer.op
               (list "List 2-3 key subtopics of: " topic))))
        (cons topic
              (map (lambda (subtopic)
                     (semantic-tree-explore subtopic (- depth 1)))
                   subtopics)))))

;; Explore "AI Safety" with depth 2
;; Creates tree: AI Safety -> [Alignment, Robustness] -> [[Value Learning, ...], ...]
(semantic-tree-explore "AI Safety" 2)
```

**Code to Reference:**
- Current recursion examples in Ch12
- `src/core/eval/machine.ts` - Recursive evaluation

**Deliverables:**
- [ ] `chapters/USER-MANUAL--30--Tree-Recursion-With-Semantic-Branching.md`
- [ ] `code-examples/lisp/ch30-tree-recursion.lisp`
- [ ] `code-examples/typescript/ch30-tree-recursion.ts`
- [ ] Update DEMO-GALLERY.md

---

### Ch31: Orders of Growth: Semantic Cost Analysis

**SICP Reference:** Section 1.2.3 - Orders of Growth

**What to Write:**
- Analyze token costs as complexity measure
- Show O(n) vs O(n²) in LLM calls
- Demonstrate cost vs quality tradeoffs
- Budget-aware algorithm design

**Example to Create:**
```lisp
;; Compare costs of different approaches
(define (analyze-all-pairs items)
  ;; O(n²) - compares every pair
  (map (lambda (item1)
         (map (lambda (item2)
                (effect infer.op
                  (list "Compare: " item1 " vs " item2)))
              items))
       items))
;; Cost: n² LLM calls

(define (analyze-with-summary items)
  ;; O(n) - one call per item + one summary
  (let ((analyses
         (map (lambda (item)
                (effect infer.op (list "Analyze: " item)))
              items)))
    (effect infer.op
      (list "Summarize these analyses: " analyses))))
;; Cost: n+1 LLM calls

;; For 10 items: 100 calls vs 11 calls!
```

**Code to Reference:**
- `src/core/solver/budget.ts` - Budget tracking
- `src/core/oracle/receipts.ts` - Cost tracking

**Deliverables:**
- [ ] `chapters/USER-MANUAL--31--Orders-Of-Growth-Semantic-Cost-Analysis.md`
- [ ] `code-examples/lisp/ch31-cost-analysis.lisp`
- [ ] `code-examples/typescript/ch31-cost-analysis.ts`
- [ ] Update DEMO-GALLERY.md

---

### Ch32: General Methods: Fixpoint and Root-Finding

**SICP Reference:** Section 1.3.3 - Procedures as General Methods

**What to Write:**
- Fixpoint computation with semantic convergence
- Root-finding via semantic search
- General iterative improvement
- Abstract patterns for semantic refinement

**Example to Create:**
```lisp
;; Fixpoint: Keep improving until stable
(define (fixpoint f start tolerance)
  (define (close-enough? v1 v2)
    (equal? "yes"
      (effect infer.op
        (list "Are these semantically equivalent? yes/no: "
              v1 " vs " v2))))

  (define (iter guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (iter next))))

  (iter start))

;; Use it to find stable phrasing
(fixpoint
  (lambda (text)
    (effect infer.op (list "Make this more concise: " text)))
  "The quick brown fox jumped over the extremely lazy dog")
;; Converges to stable concise form
```

**Code to Reference:**
- `src/core/solver/fixpoint.ts` - **FULL IMPLEMENTATION**
- `src/core/solver/combinators.ts` - Solver combinators

**Deliverables:**
- [ ] `chapters/USER-MANUAL--32--General-Methods-Fixpoint-And-Root-Finding.md`
- [ ] `code-examples/lisp/ch32-general-methods.lisp`
- [ ] `code-examples/typescript/ch32-general-methods.ts`
- [ ] Update DEMO-GALLERY.md

---

## PART II: Data Patterns (4 chapters)

### Ch33: Hierarchical Semantic Structures

**SICP Reference:** Section 2.2.2 - Hierarchical Structures

**What to Write:**
- Nested meaning representations
- Trees of semantic content
- Recursive operations on semantic hierarchies
- Closure property for semantic structures

**Example to Create:**
```lisp
;; Hierarchical structure: document -> sections -> paragraphs
(define doc
  (list "Report"
        (list "Introduction"
              "This report covers AI safety concerns."
              "We examine three key areas.")
        (list "Alignment"
              "Value alignment remains challenging."
              "Current approaches include RLHF.")))

;; Recursively summarize hierarchy
(define (summarize-tree structure)
  (if (string? structure)
      (effect infer.op (list "Summarize in 5 words: " structure))
      (cons (car structure)  ; keep heading
            (map summarize-tree (cdr structure)))))

(summarize-tree doc)
;; => Nested summary preserving structure
```

**Code to Reference:**
- Current list operations in Ch15
- Tree operations in evaluator

**Deliverables:**
- [ ] `chapters/USER-MANUAL--33--Hierarchical-Semantic-Structures.md`
- [ ] `code-examples/lisp/ch33-hierarchical-structures.lisp`
- [ ] `code-examples/typescript/ch33-hierarchical-structures.ts`
- [ ] Update DEMO-GALLERY.md

---

### Ch34: Symbolic Semantic Differentiation

**SICP Reference:** Section 2.3.2 - Example: Symbolic Differentiation

**What to Write:**
- Analyzing semantic changes/differences
- Symbolic manipulation of meaning
- Diff-ing semantic representations
- Change detection via LLM

**Example to Create:**
```lisp
;; Semantic differentiation: analyze what changed
(define (semantic-diff original modified)
  (effect infer.op
    (list "Describe what changed (additions, deletions, modifications):"
          "Original: " original
          "Modified: " modified)))

(define (apply-semantic-diff text operation)
  (effect infer.op
    (list "Apply this change to the text:"
          "Text: " text
          "Operation: " operation)))

;; Use it
(define v1 "We offer competitive pricing and fast delivery.")
(define v2 "We offer market-leading prices and rapid shipping.")
(define diff (semantic-diff v1 v2))
;; => "Changed 'competitive pricing' to 'market-leading prices',
;;     'fast delivery' to 'rapid shipping'"

;; Apply to different text
(apply-semantic-diff
  "Our service has competitive pricing."
  diff)
;; => "Our service has market-leading prices."
```

**Code to Reference:**
- `src/core/compiler/differential.ts` - **DIFFERENTIAL COMPILATION**
- Symbolic processing in evaluator

**Deliverables:**
- [ ] `chapters/USER-MANUAL--34--Symbolic-Semantic-Differentiation.md`
- [ ] `code-examples/lisp/ch34-symbolic-differentiation.lisp`
- [ ] `code-examples/typescript/ch34-symbolic-differentiation.ts`
- [ ] Update DEMO-GALLERY.md

---

### Ch35: Tagged Semantic Values

**SICP Reference:** Section 2.4.2 - Tagged Data

**What to Write:**
- Explicit type tags for semantic values
- Dispatch based on semantic types
- Confidence scores as metadata
- Structured semantic data

**Example to Create:**
```lisp
;; Tagged semantic values with confidence
(define (make-tagged-sentiment text)
  (let ((result
         (effect infer.op
           (list "Classify sentiment (positive/negative/neutral) with confidence 0-1: "
                 text))))
    (list 'sentiment
          (car result)      ; sentiment label
          (cadr result))))  ; confidence score

(define (tagged-type x) (car x))
(define (tagged-value x) (cadr x))
(define (tagged-confidence x) (caddr x))

;; Type-specific operations
(define (combine-sentiments s1 s2)
  (if (eq? (tagged-type s1) 'sentiment)
      (effect infer.op
        (list "Combine these sentiments, weighted by confidence:"
              "S1: " (tagged-value s1) " (" (tagged-confidence s1) ")"
              "S2: " (tagged-value s2) " (" (tagged-confidence s2) ")"))
      (error "Not sentiment types")))

(define sent1 (make-tagged-sentiment "I love this!"))
(define sent2 (make-tagged-sentiment "It's okay I guess."))
(combine-sentiments sent1 sent2)
```

**Code to Reference:**
- `src/core/eval/values.ts` - **ALL VALUES ARE TAGGED**
- Tagged union types throughout codebase

**Deliverables:**
- [ ] `chapters/USER-MANUAL--35--Tagged-Semantic-Values.md`
- [ ] `code-examples/lisp/ch35-tagged-values.lisp`
- [ ] `code-examples/typescript/ch35-tagged-values.ts`
- [ ] Update DEMO-GALLERY.md

---

### Ch36: Semantic Type Coercion

**SICP Reference:** Section 2.5.2 - Combining Data of Different Types

**What to Write:**
- Type towers for semantic data
- Automatic coercion (string → meaning → schema)
- Coercion paths and ambiguity
- Type hierarchies

**Example to Create:**
```lisp
;; Type coercion tower: string → meaning → structured-data
(define (coerce-to-meaning text)
  (effect infer.op
    (list "Extract the core meaning of: " text)))

(define (coerce-to-schema meaning)
  (effect infer.op
    (list "Convert to structured JSON: " meaning)))

;; Automatic coercion
(define (ensure-type value target-type)
  (cond
    ((eq? target-type 'string) (to-string value))
    ((eq? target-type 'meaning) (coerce-to-meaning value))
    ((eq? target-type 'schema) (coerce-to-schema (ensure-type value 'meaning)))))

;; Use it
(define raw "Customer complained about slow shipping on Jan 15")
(define meaning (ensure-type raw 'meaning))
;; => "Customer dissatisfaction with delivery speed"
(define structured (ensure-type raw 'schema))
;; => {"type": "complaint", "topic": "shipping", "sentiment": "negative", "date": "2024-01-15"}
```

**Code to Reference:**
- `src/core/generic/coercion.ts` - **FULL COERCION SYSTEM (476 lines!)**
- `src/core/generic/types.ts` - Type system

**Deliverables:**
- [ ] `chapters/USER-MANUAL--36--Semantic-Type-Coercion.md`
- [ ] `code-examples/lisp/ch36-type-coercion.lisp`
- [ ] `code-examples/typescript/ch36-type-coercion.ts`
- [ ] Update DEMO-GALLERY.md

---

## PART III: State Patterns (3 chapters)

### Ch37: Implementing Semantic Queues and Tables

**SICP Reference:** Section 3.3.2 & 3.3.3 - Representing Queues and Tables

**What to Write:**
- Semantic priority queue (LLM ranks urgency)
- Table with semantic keys (fuzzy lookup)
- Mutable data structure patterns
- Queue operations with semantic ordering

**Example to Create:**
```lisp
;; Semantic priority queue
(define (make-priority-queue)
  (list))

(define (insert-with-priority queue item)
  (let ((priority
         (effect infer.op
           (list "Rate urgency 1-10: " item))))
    ;; Insert maintaining priority order
    (define (insert-sorted lst)
      (cond
        ((null? lst) (list (cons item priority)))
        ((> priority (cdr (car lst)))
         (cons (cons item priority) lst))
        (else
         (cons (car lst) (insert-sorted (cdr lst))))))
    (insert-sorted queue)))

(define (pop-highest queue)
  (if (null? queue)
      '()
      (values (car (car queue))  ; item
              (cdr queue))))      ; rest

;; Use it
(define q (make-priority-queue))
(set! q (insert-with-priority q "Reply to customer inquiry"))
(set! q (insert-with-priority q "Fix critical security bug"))
(set! q (insert-with-priority q "Update documentation"))
;; Pops in LLM-determined urgency order
```

**Code to Reference:**
- `src/core/concurrency/scheduler.ts` - Task queues
- `src/core/effects/nondet/frontier.ts` - Search frontier queue

**Deliverables:**
- [ ] `chapters/USER-MANUAL--37--Implementing-Semantic-Queues-And-Tables.md`
- [ ] `code-examples/lisp/ch37-queues-and-tables.lisp`
- [ ] `code-examples/typescript/ch37-queues-and-tables.ts`
- [ ] Update DEMO-GALLERY.md

---

### Ch38: Semantic State Machines and Constraint Propagation

**SICP Reference:** Section 3.3.4 & 3.3.5 - Digital Circuits & Constraint Propagation

**What to Write:**
- State machines with semantic transitions
- Constraint networks for semantic entailment
- Propagation of semantic constraints
- Workflow state machines

**Example to Create:**
```lisp
;; Semantic state machine for support ticket workflow
(define (make-ticket-fsm initial-state)
  (let ((state initial-state))
    (lambda (event ticket-content)
      (let ((next-state
             (effect infer.op
               (list "Given current state and event, what's next state?"
                     "Current: " state
                     "Event: " event
                     "Content: " ticket-content
                     "Options: new, assigned, in-progress, resolved, closed"))))
        (set! state next-state)
        state))))

(define ticket-fsm (make-ticket-fsm "new"))
(ticket-fsm "agent-assigned" "Customer reports login issue")
;; => "assigned"
(ticket-fsm "agent-started" "Agent investigating database logs")
;; => "in-progress"

;; Constraint propagation example
(define (make-connector)
  (let ((value #f) (constraints '()))
    (lambda (msg . args)
      (cond
        ((eq? msg 'set-value)
         (set! value (car args))
         ;; Propagate to constraints
         (for-each (lambda (c) (c 'inform)) constraints))
        ((eq? msg 'get-value) value)
        ((eq? msg 'add-constraint)
         (set! constraints (cons (car args) constraints)))))))
```

**Code to Reference:**
- `src/core/constraints/network.ts` - **CONSTRAINT NETWORK (14KB!)**
- `src/core/constraints/engine.ts` - **CONSTRAINT ENGINE (13KB!)**
- `src/core/eval/machine.ts` - State machine

**Deliverables:**
- [ ] `chapters/USER-MANUAL--38--Semantic-State-Machines-And-Constraint-Propagation.md`
- [ ] `code-examples/lisp/ch38-state-machines-constraints.lisp`
- [ ] `code-examples/typescript/ch38-state-machines-constraints.ts`
- [ ] Update DEMO-GALLERY.md

---

### Ch39: Serializers and Concurrent LLM Calls

**SICP Reference:** Section 3.4.2 - Mechanisms for Controlling Concurrency

**What to Write:**
- Serializers for concurrent LLM calls
- Rate limiting with semaphores
- Budget-aware concurrency
- Shared resource protection

**Example to Create:**
```lisp
;; Serializer for LLM calls (rate limiting)
(define (make-serializer max-concurrent)
  (let ((count 0)
        (queue '()))
    (lambda (proc)
      (lambda args
        (if (< count max-concurrent)
            (begin
              (set! count (+ count 1))
              (let ((result (apply proc args)))
                (set! count (- count 1))
                ;; Process queue
                (if (not (null? queue))
                    (let ((next (car queue)))
                      (set! queue (cdr queue))
                      (next)))
                result))
            ;; Queue it
            (begin
              (set! queue (append queue (list (lambda () (apply proc args)))))
              'queued))))))

;; Use it
(define llm-serializer (make-serializer 3))  ; Max 3 concurrent

(define safe-infer
  (llm-serializer
    (lambda (prompt)
      (effect infer.op prompt))))

;; These will be rate-limited to 3 concurrent
(map safe-infer (list "Q1" "Q2" "Q3" "Q4" "Q5" "Q6"))
```

**Code to Reference:**
- `src/core/concurrency/sync.ts` - **SYNCHRONIZATION PRIMITIVES**
- `src/core/solver/budget.ts` - Budget tracking

**Deliverables:**
- [ ] `chapters/USER-MANUAL--39--Serializers-And-Concurrent-Llm-Calls.md`
- [ ] `code-examples/lisp/ch39-serializers-concurrency.lisp`
- [ ] `code-examples/typescript/ch39-serializers-concurrency.ts`
- [ ] Update DEMO-GALLERY.md

---

## PART IV: Metalinguistic Patterns (3 chapters)

### Ch40: The Analyzing Semantic Evaluator

**SICP Reference:** Section 4.1.7 - Separating Syntactic Analysis from Execution

**What to Write:**
- Separate analysis from execution
- Cache LLM prompt templates
- Compile semantic expressions once, execute many times
- Performance optimization

**Example to Create:**
```lisp
;; Analyzing evaluator: pre-process prompts
(define (analyze expr)
  (cond
    ((self-evaluating? expr) (lambda (env) expr))
    ((variable? expr) (lambda (env) (lookup env expr)))
    ((quoted? expr) (lambda (env) (text-of-quotation expr)))
    ((semantic-call? expr)
     ;; PRE-ANALYZE: Build template once
     (let ((template (extract-template expr)))
       ;; Return execution procedure
       (lambda (env)
         (let ((filled (fill-template template env)))
           (effect infer.op filled)))))
    ...))

;; Use analyzed version
(define analyzed-sentiment (analyze '(sentiment text)))
;; Template built once: "Sentiment of: {text}"

;; Execute many times (fast - no re-analysis)
(analyzed-sentiment (extend-env 'text "Great!" base-env))
(analyzed-sentiment (extend-env 'text "Terrible!" base-env))
;; Each execution just fills template + calls LLM
```

**Code to Reference:**
- `src/core/compiler/anf.ts` - **ANF CONVERSION (analysis phase)**
- `src/core/compiler/optimize.ts` - **OPTIMIZATION**

**Deliverables:**
- [ ] `chapters/USER-MANUAL--40--The-Analyzing-Semantic-Evaluator.md`
- [ ] `code-examples/lisp/ch40-analyzing-evaluator.lisp`
- [ ] `code-examples/typescript/ch40-analyzing-evaluator.ts`
- [ ] Update DEMO-GALLERY.md

---

### Ch41: Implementing the Semantic Query System

**SICP Reference:** Section 4.4.2 & 4.4.4 - How the Query System Works & Implementation

**What to Write:**
- Query system internals
- Pattern matching with LLM
- Database of semantic facts
- Unification and resolution

**Example to Create:**
```lisp
;; Semantic query system
(define (make-fact-base)
  (list))

(define (assert! fact-base fact)
  (cons fact fact-base))

(define (query fact-base pattern)
  (define (matches? fact)
    (equal? "yes"
      (effect infer.op
        (list "Does this fact match the pattern? yes/no"
              "Fact: " fact
              "Pattern: " pattern))))
  (filter matches? fact-base))

;; Use it
(define facts (make-fact-base))
(set! facts (assert! facts "Alice manages the engineering team"))
(set! facts (assert! facts "Bob reports to Alice"))
(set! facts (assert! facts "Carol leads the design team"))

(query facts "Who manages engineering?")
;; => ("Alice manages the engineering team")

(query facts "Who reports to Alice?")
;; => ("Bob reports to Alice")
```

**Code to Reference:**
- `src/core/opr/kernels/logic.ts` - **LOGIC KERNEL**
- `src/core/solver/facts.ts` - Fact database

**Deliverables:**
- [ ] `chapters/USER-MANUAL--41--Implementing-The-Semantic-Query-System.md`
- [ ] `code-examples/lisp/ch41-query-system.lisp`
- [ ] `code-examples/typescript/ch41-query-system.ts`
- [ ] Update DEMO-GALLERY.md

---

### Ch42: Semantic Unification

**SICP Reference:** Section 4.4.4.4 - Rules and Unification

**What to Write:**
- Pattern matching with variables
- Fuzzy semantic unification
- Binding variables to semantic matches
- LLM-guided unification

**Example to Create:**
```lisp
;; Semantic unification with variables
(define (unify pattern fact)
  (effect infer.op
    (list "Unify pattern with fact, extracting variable bindings:"
          "Pattern: " pattern  ; e.g., "?person manages ?department"
          "Fact: " fact        ; e.g., "Alice manages engineering"
          "Return bindings as JSON: {\"person\": \"Alice\", \"department\": \"engineering\"}")))

;; Use it
(define bindings (unify "?person manages ?department"
                        "Alice manages the engineering team"))
;; => {"person": "Alice", "department": "engineering"}

;; Apply bindings to new pattern
(define (apply-bindings pattern bindings)
  (effect infer.op
    (list "Substitute variables in pattern with bindings:"
          "Pattern: " pattern
          "Bindings: " bindings)))

(apply-bindings "?person is responsible for ?department"
                bindings)
;; => "Alice is responsible for engineering"
```

**Code to Reference:**
- `src/core/opr/kernels/logic.ts` - Unification logic
- `src/core/expand/syntaxRules.ts` - Pattern matching

**Deliverables:**
- [ ] `chapters/USER-MANUAL--42--Semantic-Unification.md`
- [ ] `code-examples/lisp/ch42-unification.lisp`
- [ ] `code-examples/typescript/ch42-unification.ts`
- [ ] Update DEMO-GALLERY.md

---

## PART V: Machine Patterns (3 chapters)

### Ch43: Agentic Workflow State Machines

**SICP Reference:** Section 5.1 - Designing Register Machines

**What to Write:**
- Explicit state machine design for workflows
- State transition diagrams
- Event-driven agent coordination
- Multi-step agentic flows

**Example to Create:**
```lisp
;; Workflow state machine
(define (make-workflow-machine states transitions initial)
  (let ((current-state initial)
        (history '()))
    (lambda (event context)
      (let ((next
             (effect infer.op
               (list "Given state machine, current state, and event, return next state:"
                     "States: " states
                     "Transitions: " transitions
                     "Current: " current-state
                     "Event: " event
                     "Context: " context))))
        (set! history (cons (list current-state event next) history))
        (set! current-state next)
        (values next history)))))

;; Customer onboarding workflow
(define onboarding
  (make-workflow-machine
    '(new email-sent verified profile-completed active)
    '((new email-request email-sent)
      (email-sent verify verified)
      (verified complete-profile profile-completed)
      (profile-completed activate active))
    'new))

(onboarding 'email-request "user@example.com")  ; => email-sent
(onboarding 'verify "clicked verification link") ; => verified
```

**Code to Reference:**
- `src/core/eval/machine.ts` - **CEK MACHINE (state machine!)**
- `src/core/eval/stepper.ts` - Step-by-step execution

**Deliverables:**
- [ ] `chapters/USER-MANUAL--43--Agentic-Workflow-State-Machines.md`
- [ ] `code-examples/lisp/ch43-workflow-state-machines.lisp`
- [ ] `code-examples/typescript/ch43-workflow-state-machines.ts`
- [ ] Update DEMO-GALLERY.md

---

### Ch44: The Explicit-Control Semantic Evaluator

**SICP Reference:** Section 5.4 - The Explicit-Control Evaluator

**What to Write:**
- Evaluator as explicit state machine
- Stepping through evaluation
- Debugger integration
- Reified control flow

**Example to Create:**
```lisp
;; Explicit-control evaluator with breakpoints
(define (make-stepping-evaluator)
  (let ((pc 0)           ; program counter
        (expr-stack '()) ; expression stack
        (env-stack '())  ; environment stack
        (mode 'eval))    ; eval or apply

    (lambda (command . args)
      (cond
        ((eq? command 'step)
         ;; Execute one step
         (cond
           ((eq? mode 'eval)
            ;; Eval step
            (let ((expr (car expr-stack))
                  (env (car env-stack)))
              (if (self-evaluating? expr)
                  (begin
                    (set! mode 'return)
                    expr)
                  ;; Continue evaluation...
                  )))
           ((eq? mode 'apply)
            ;; Apply step
            ...)
           ((eq? mode 'return)
            ;; Return value
            (pop-stack!))))

        ((eq? command 'run-until-breakpoint)
         ;; Run until condition
         (let loop ()
           (let ((result (step)))
             (if (breakpoint? result)
                 result
                 (loop)))))

        ((eq? command 'inspect)
         ;; Inspect current state
         (list 'pc pc
               'mode mode
               'expr (car expr-stack)
               'env (car env-stack)))))))

;; Use it
(define stepper (make-stepping-evaluator))
(stepper 'load '(+ 1 2))
(stepper 'step)  ; => evaluating +
(stepper 'step)  ; => evaluating 1
(stepper 'inspect) ; => (pc 2 mode eval ...)
```

**Code to Reference:**
- `src/core/eval/stepper.ts` - **STEPPER (explicit control)**
- `src/core/eval/machine.ts` - CEK machine

**Deliverables:**
- [ ] `chapters/USER-MANUAL--44--The-Explicit-Control-Semantic-Evaluator.md`
- [ ] `code-examples/lisp/ch44-explicit-control-evaluator.lisp`
- [ ] `code-examples/typescript/ch44-explicit-control-evaluator.ts`
- [ ] Update DEMO-GALLERY.md

---

### Ch45: Semantic Compilation: From Goals to Optimized LLM Calls

**SICP Reference:** Section 5.5 - Compilation

**What to Write:**
- Compiling semantic expressions
- Optimization passes (merge prompts, batch calls)
- Instruction sequences for LLM operations
- Ahead-of-time vs runtime

**Example to Create:**
```lisp
;; Semantic compiler
(define (compile-semantic expr)
  (cond
    ;; Constant folding
    ((all-constants? expr)
     (list (make-instruction 'call-llm (fold-constants expr))))

    ;; Batch optimization: merge adjacent LLM calls
    ((sequential-llm-calls? expr)
     (let ((batched (batch-llm-calls expr)))
       (map (lambda (batch)
              (make-instruction 'batch-call-llm batch))
            batched)))

    ;; Parallel optimization: independent calls
    ((parallel-llm-calls? expr)
     (list (make-instruction 'parallel-call-llm
                            (extract-parallel-calls expr))))

    ;; General case
    (else
     (compile-sequence expr))))

;; Example: optimize this
(define original
  '(let ((s1 (sentiment text1))
         (s2 (sentiment text2))
         (s3 (sentiment text3)))
     (combine s1 s2 s3)))

;; Compiled version batches the 3 sentiment calls
(compile-semantic original)
;; => [(batch-call-llm [(sentiment text1) (sentiment text2) (sentiment text3)])
;;     (call-llm (combine s1 s2 s3))]
;; Reduced from 4 LLM calls to 2!
```

**Code to Reference:**
- `src/core/compiler/` - **FULL COMPILER (17 files!)**
- `src/core/compiler/optimize.ts` - Optimization passes
- `src/core/compiler/bytecode.ts` - Instruction generation

**Deliverables:**
- [ ] `chapters/USER-MANUAL--45--Semantic-Compilation.md`
- [ ] `code-examples/lisp/ch45-semantic-compilation.lisp`
- [ ] `code-examples/typescript/ch45-semantic-compilation.ts`
- [ ] Update DEMO-GALLERY.md

---

## PART VI: Core LLM Extensions (4 chapters)

### Ch46: The OPR Multi-Kernel Runtime

**SICP Reference:** (Beyond SICP)

**What to Write:**
- Multi-kernel execution model
- Different inference kernels (dataflow, saga, logic)
- Kernel selection and routing
- Advanced runtime features

**Example to Create:**
```lisp
;; OPR kernel selection
(define (run-with-kernel kernel-type goal context)
  (case kernel-type
    ((dataflow)
     ;; Dataflow kernel: dependency-based execution
     (opr-dataflow goal context))
    ((saga)
     ;; Saga kernel: transaction with rollback
     (opr-saga goal context))
    ((logic)
     ;; Logic kernel: unification and resolution
     (opr-logic goal context))
    ((router)
     ;; Router kernel: LLM picks best kernel
     (opr-router goal context))))

;; Use it
(run-with-kernel 'dataflow
                 '(analyze-pipeline documents)
                 (make-context ...))
```

**Code to Reference:**
- `src/core/opr/` - **OPR RUNTIME**
- `src/core/opr/kernels/` - Kernel implementations
- `src/core/opr/runtime.ts` - Runtime system

**Deliverables:**
- [ ] `chapters/USER-MANUAL--46--The-Opr-Multi-Kernel-Runtime.md`
- [ ] `code-examples/lisp/ch46-opr-runtime.lisp`
- [ ] `code-examples/typescript/ch46-opr-runtime.ts`
- [ ] Update DEMO-GALLERY.md

---

### Ch47: Provenance: Evidence Chains and Derivations

**SICP Reference:** (Beyond SICP)

**What to Write:**
- Tracking semantic derivations
- Evidence chains
- Provenance queries
- Audit trails for LLM decisions

**Example to Create:**
```lisp
;; Track provenance of decisions
(define (with-provenance expr)
  (let ((result (eval expr))
        (evidence (collect-evidence expr)))
    (list 'result result
          'evidence evidence)))

;; Query provenance
(define (why? result)
  (get-evidence-chain result))

;; Use it
(define analysis
  (with-provenance
    '(classify "Customer is frustrated with shipping")))

(why? analysis)
;; => Evidence chain:
;;    1. Input: "Customer is frustrated with shipping"
;;    2. LLM call: classify -> "complaint"
;;    3. Confidence: 0.95
;;    4. Model: gpt-4o
;;    5. Timestamp: 2026-01-31T...
```

**Code to Reference:**
- `src/core/provenance/` - **PROVENANCE SYSTEM**
- `src/core/oracle/receipts.ts` - Receipt tracking

**Deliverables:**
- [ ] `chapters/USER-MANUAL--47--Provenance-Evidence-Chains.md`
- [ ] `code-examples/lisp/ch47-provenance.lisp`
- [ ] `code-examples/typescript/ch47-provenance.ts`
- [ ] Update DEMO-GALLERY.md

---

### Ch48: Budget Management and Cost Control

**SICP Reference:** (Beyond SICP)

**What to Write:**
- Token budgets
- Cost tracking
- Budget-aware execution
- Quota enforcement

**Example to Create:**
```lisp
;; Budget-aware execution
(define (with-budget max-tokens expr)
  (let ((budget (make-budget max-tokens)))
    (with-budget-context budget
      (eval expr))))

;; Check budget before expensive ops
(define (budget-aware-map f items budget)
  (define (process-item item)
    (if (budget-available? budget)
        (let ((result (f item)))
          (charge-budget! budget (cost-of result))
          result)
        (error "Budget exceeded")))
  (map process-item items))

;; Use it
(with-budget 10000  ; 10k tokens max
  (map sentiment-analysis large-document-list))
;; Stops when budget exhausted
```

**Code to Reference:**
- `src/core/solver/budget.ts` - **BUDGET TRACKING**
- Budget enforcement in OPR

**Deliverables:**
- [ ] `chapters/USER-MANUAL--48--Budget-Management-And-Cost-Control.md`
- [ ] `code-examples/lisp/ch48-budget-management.lisp`
- [ ] `code-examples/typescript/ch48-budget-management.ts`
- [ ] Update DEMO-GALLERY.md

---

### Ch49: Advanced Semantic Caching Strategies

**SICP Reference:** (Beyond SICP - extends Ch25 memoization)

**What to Write:**
- Beyond simple memoization
- Fuzzy cache matching
- Semantic equivalence caching
- Cache invalidation strategies

**Example to Create:**
```lisp
;; Semantic cache: similar inputs → cached results
(define (make-semantic-cache)
  (let ((cache '()))
    (lambda (key compute-fn)
      ;; Check if semantically similar key exists
      (let ((match (find-similar-key cache key)))
        (if match
            (cdr match)  ; Return cached value
            (let ((result (compute-fn)))
              (set! cache (cons (cons key result) cache))
              result))))))

(define (find-similar-key cache key)
  (define (similar? cached-key)
    (equal? "yes"
      (effect infer.op
        (list "Are these semantically equivalent? yes/no: "
              cached-key " vs " key))))
  (find similar? (map car cache)))

;; Use it
(define cache (make-semantic-cache))
(cache "What's the weather?"
       (lambda () (effect infer.op "Get weather")))
;; First call: expensive

(cache "How's the weather today?"
       (lambda () (effect infer.op "Get weather")))
;; Second call: CACHED (semantically similar)
```

**Code to Reference:**
- Ch25 basic memoization
- `src/core/artifacts/` - Content-addressed storage

**Deliverables:**
- [ ] `chapters/USER-MANUAL--49--Advanced-Semantic-Caching.md`
- [ ] `code-examples/lisp/ch49-advanced-caching.lisp`
- [ ] `code-examples/typescript/ch49-advanced-caching.ts`
- [ ] Update DEMO-GALLERY.md

---

## SUMMARY: 21 Work Items

### By Part:
- **Part I (Procedures):** 6 chapters (Ch28-Ch32 + 1 duplicate)
- **Part II (Data):** 4 chapters (Ch33-36)
- **Part III (State):** 3 chapters (Ch37-39)
- **Part IV (Meta):** 3 chapters (Ch40-42)
- **Part V (Machines):** 3 chapters (Ch43-45)
- **Part VI (LLM):** 4 chapters (Ch46-49)

### Total Deliverables:
- **21 Markdown chapters**
- **21 Lisp demo files**
- **21 TypeScript test configs**
- **21 entries in DEMO-GALLERY.md**

### Effort Estimate:
- ~2-3 hours per chapter (explanation + example + test)
- **Total: 42-63 hours** for all 21 chapters

### Approach:
1. Start with chapters that use existing code (Ch36, Ch38, Ch40, Ch45, Ch46, Ch47, Ch48)
2. Then do pedagogical chapters (Ch28-32, Ch33-35)
3. Then integration chapters (Ch37, Ch39, Ch41-44, Ch49)

---

**Next Step:** Begin writing chapters in order, starting with Ch28!
