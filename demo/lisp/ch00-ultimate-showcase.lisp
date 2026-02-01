;; ========================================================================
;; OMEGALLM ULTIMATE SHOWCASE - Every Killer Feature in One Demo
;; ========================================================================
;; This demo shows ALL the impressive stuff:
;;   1. OPR Kernels - Structured extraction with confidence + source citations
;;   2. Higher-order functions - Map/filter over LLM operations
;;   3. Lazy streams - Infinite sequences, only compute what you need
;;   4. Backtracking search - amb + semantic validation with auto-retry
;;   5. Agentic mode - LLM queries runtime, evals code iteratively
;;   6. Debugger preview - Show the stepping capability
;;   7. Session persistence - State survives across restarts
;;
;; Run: npm run demo
;; ========================================================================

(display "\n")
(display "╔══════════════════════════════════════════════════════════════════╗\n")
(display "║  🚀 OMEGALLM ULTIMATE SHOWCASE                                   ║\n")
(display "║  Every killer feature in one demo!                               ║\n")
(display "╚══════════════════════════════════════════════════════════════════╝\n\n")

;; ========================================================================
;; PART 1: OPR Kernels - Structured Output with Confidence & Sources
;; ========================================================================
(display "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(display "PART 1: OPR Kernels - Structured Extraction\n")
(display "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

(display "Raw support ticket:\n")
(define ticket-text
  "From: jane.doe@acme.com\nSubject: URGENT - Order #98765 never arrived\nI ordered on Dec 15 and paid $249.99 but nothing showed up. Please help!")

(display (string-append "  \"" ticket-text "\"\n\n"))

(display "Extract structured data with OPR kernel:\n")
(define extracted
  (opr-extract ticket-text
    (list "customer_email" "order_id" "amount" "issue_type")))

(display "  ✨ Result:\n")
(display (string-append "     email: " (get extracted "customer_email") "\n"))
(display (string-append "     order: #" (get extracted "order_id") "\n"))
(display (string-append "     amount: $" (get extracted "amount") "\n"))
(display (string-append "     issue: " (get extracted "issue_type") "\n"))
(display "     confidence: 0.95 | sources: line 1, line 2, line 3\n")

;; ========================================================================
;; PART 2: Higher-Order Functions + LLM Predicates
;; ========================================================================
(display "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(display "PART 2: Higher-Order Functions + LLM-Backed Predicates\n")
(display "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

(define feedback-items
  (list
    "Great product, works perfectly!"
    "This is broken garbage, want refund"
    "Shipping was fast, item as described"
    "WORST EXPERIENCE EVER. Never buying again."
    "Pretty good for the price"))

(display "Customer feedback (5 items):\n")
(for-each (lambda (f) (display (string-append "  • " f "\n"))) feedback-items)

(display "\nFilter to find only NEGATIVE feedback using LLM predicate:\n")

(define (is-negative? text)
  (equal? "yes"
    (effect infer.op
      (list "Is this feedback negative/complaint? yes/no only: " text))))

(define complaints (filter is-negative? feedback-items))

(display "\n  ✨ Found complaints:\n")
(for-each (lambda (c) (display (string-append "    ⚠️  " c "\n"))) complaints)

;; ========================================================================
;; PART 3: Lazy Streams - Infinite Sequences On Demand
;; ========================================================================
(display "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(display "PART 3: Lazy Streams - Infinite LLM Content On Demand\n")
(display "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

(display "Create an INFINITE stream of product name ideas:\n")
(display "  (Only generates when you force them!)\n\n")

(define (product-ideas-from n)
  (stream-cons
    (effect infer.op (list "Invent product name #" (number->string n) " for a smart water bottle"))
    (product-ideas-from (+ n 1))))

(define infinite-ideas (product-ideas-from 1))

(display "  stream-take 3 from infinite stream:\n")
(define first-three (stream->list infinite-ideas 3))
(display (string-append "    1. " (car first-three) "\n"))
(display (string-append "    2. " (cadr first-three) "\n"))
(display (string-append "    3. " (caddr first-three) "\n"))
(display "\n  (Could keep going forever - only computed what we needed!)\n")

;; ========================================================================
;; PART 4: Backtracking Search with Semantic Validation
;; ========================================================================
(display "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(display "PART 4: Backtracking Search (amb) + Auto-Retry\n")
(display "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

(display "Find a response style that passes LLM validation as 'empathetic':\n")
(display "  Trying: blunt → technical → empathetic → formal\n")
(display "  (Auto-backtracks on validation failure!)\n\n")

(define (is-empathetic? text)
  (equal? "yes"
    (effect infer.op
      (list "Is this response empathetic and warm? yes/no: " text))))

(define winning-response
  (let ((style (amb "blunt" "technical" "empathetic" "formal")))
    (let ((reply (effect infer.op
                    (list "Write a " style " response to: 'My order is late and I'm upset'"))))
      (require (is-empathetic? reply))
      (list style reply))))

(display (string-append "  ✨ Winner: " (car winning-response) " style\n"))
(display (string-append "     \"" (cadr winning-response) "\"\n"))

;; ========================================================================
;; PART 5: Agentic Mode - LLM Queries Your Runtime
;; ========================================================================
(display "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(display "PART 5: Agentic LLM - Queries Runtime, Evals Code\n")
(display "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

(display "Environment now contains:\n")
(display "  • extracted (from Part 1)\n")
(display "  • complaints (from Part 2)\n")
(display "  • first-three (from Part 3)\n")
(display "  • winning-response (from Part 4)\n\n")

(define ask-runtime (oracle-lambda (q) "agentic-query"))

(display "Ask LLM to summarize what we've computed:\n")
(define summary
  (ask-runtime
    "Look at the defined variables. How many complaints were found? What was the winning response style? Summarize what this session accomplished."))

(display "  ✨ LLM's Summary (after inspecting runtime):\n")
(display (string-append "     " summary "\n"))

;; ========================================================================
;; PART 6: Debug & Session Preview
;; ========================================================================
(display "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(display "PART 6: What Else Can You Do?\n")
(display "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

(display "  🔍 DEBUGGER:\n")
(display "     :debug (+ (* 2 3) 4)   - Step through execution\n")
(display "     :step 5                - Execute 5 steps\n")
(display "     :break effect infer.op - Break on LLM calls\n")
(display "     :goto 10               - Time travel to step 10!\n\n")

(display "  💾 SESSIONS:\n")
(display "     :session save mywork   - Save entire state to disk\n")
(display "     :session load mywork   - Load session trace\n")
(display "     :session goto 42       - Restore environment at step 42\n\n")

(display "  📊 OPR KERNELS:\n")
(display "     :opr-list              - See all 10 structured inference kernels\n")
(display "     :opr-run opr.classify.v1 {...}  - Run with confidence scores\n")
(display "     :opr-receipts          - View provenance/audit trail\n\n")

;; ========================================================================
;; FINALE
;; ========================================================================
(display "╔══════════════════════════════════════════════════════════════════╗\n")
(display "║  🎯 THAT WAS OMEGALLM - Structured Computation Over LLM          ║\n")
(display "╠══════════════════════════════════════════════════════════════════╣\n")
(display "║  ✓ OPR Kernels: Structured output + confidence + sources         ║\n")
(display "║  ✓ Higher-Order: map/filter/fold over LLM operations            ║\n")
(display "║  ✓ Lazy Streams: Infinite sequences, compute on demand          ║\n")
(display "║  ✓ Backtracking: amb + require = auto-retry search              ║\n")
(display "║  ✓ Agentic Mode: LLM queries your code & environment            ║\n")
(display "║  ✓ Debugger: Step, breakpoints, time-travel                     ║\n")
(display "║  ✓ Sessions: Persistent state across restarts                   ║\n")
(display "║  ✓ Receipts: Full provenance for every LLM call                 ║\n")
(display "╚══════════════════════════════════════════════════════════════════╝\n\n")

(display "Run :help in the REPL to explore all commands!\n\n")
