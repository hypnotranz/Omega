;; ========================================================================
;; OMEGALLM ULTIMATE SHOWCASE - Every Killer Feature That Actually Works
;; ========================================================================
;; This demo shows:
;;   1. Higher-order functions (map/filter) over LLM operations
;;   2. Backtracking search with amb + semantic validation
;;   3. Lazy streams - infinite sequences on demand
;;   4. Composability - all these primitives work together
;;
;; Run: npm run demo
;; ========================================================================

;; ========================================================================
;; PART 1: Higher-Order Functions - Map LLM Over Data
;; ========================================================================
;; Treat LLM calls like any other function - map them over lists!

(define customer-feedback
  (list
    "Great product, love it!"
    "Broken on arrival, want refund"
    "Fast shipping, works as described"
    "TERRIBLE experience, never again"))

;; Map sentiment analysis over ALL items with a single LLM call each
(define sentiments
  (map (lambda (text)
         (effect infer.op
           (list "One word sentiment (positive/negative/neutral): " text)))
       customer-feedback))

;; Result: List of sentiments for each feedback item
sentiments

;; ========================================================================
;; PART 2: Filter Using LLM-Backed Predicates
;; ========================================================================
;; Use LLM as a predicate function inside filter!

(define (is-complaint? text)
  (equal? "yes"
    (effect infer.op
      (list "Is this a complaint? yes/no only: " text))))

;; Filter to find only complaints - LLM decides for each item
(define complaints
  (filter is-complaint? customer-feedback))

;; Result: Only the negative items
complaints

;; ========================================================================
;; PART 3: Backtracking Search with Semantic Validation
;; ========================================================================
;; amb tries options; require validates; auto-backtrack on failure!

(define (sounds-empathetic? text)
  (equal? "yes"
    (effect infer.op
      (list "Does this sound empathetic and caring? yes/no: " text))))

;; Try different tones until we find one the LLM validates as empathetic
(define empathetic-response
  (let ((tone (amb "blunt" "technical" "empathetic" "formal")))
    (let ((reply (effect infer.op
                    (list "Write a " tone " 1-sentence response to: 'My order never arrived'"))))
      (require (sounds-empathetic? reply))
      (list 'winner: tone 'response: reply))))

;; Result: The first tone that passed validation + the response
empathetic-response

;; ========================================================================
;; PART 4: Lazy Streams - Infinite Sequences On Demand
;; ========================================================================
;; Generate infinite content, only compute what you actually need!

(define (idea-stream n)
  (stream-cons
    (effect infer.op
      (list "Invent startup idea #" (number->string n) " in 5 words or less"))
    (idea-stream (+ n 1))))

;; Create infinite stream of startup ideas
(define infinite-ideas (idea-stream 1))

;; Only force the first 3 - the rest never get computed!
(stream->list infinite-ideas 3)

;; ========================================================================
;; PART 5: Compose Everything - Real Pipeline
;; ========================================================================
;; Chain multiple LLM operations together

(define raw-issues
  (list
    "Login broken since update"
    "Love the new dark mode!"
    "Payment failed 3 times"
    "Can't export my data"))

;; Pipeline: Filter complaints → Generate responses → Validate tone
(define issue-responses
  (map (lambda (issue)
         (let ((response (effect infer.op
                           (list "Write apologetic 1-sentence response to: " issue))))
           (list 'issue: issue 'response: response)))
       (filter is-complaint? raw-issues)))

;; Result: Responses only for the complaints
issue-responses

;; ========================================================================
;; FINALE - What else can you do?
;; ========================================================================
;; In the REPL, try these commands:
;;
;;   :help              - See ALL available commands
;;   :debug (+ 1 2)     - Step-through debugger
;;   :goto 5            - Time travel to step 5
;;   :session save foo  - Persist state to disk
;;   :opr-list          - 10 structured inference kernels
;;   :ask "question"    - Agentic LLM that evals code
;;
;; This was OMEGALLM: Structured computation over semantic primitives!

(list
  'demo-complete!
  'features-shown:
  (list 'map-over-llm 'filter-with-llm-predicate 'amb-backtracking 'lazy-streams 'pipelines)
  'try: ':help 'in 'the 'REPL!)
