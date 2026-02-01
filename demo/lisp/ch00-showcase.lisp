;; ========================================================================
;; OMEGALLM SHOWCASE - The Most Impressive Features in One Demo
;; ========================================================================
;; This demo shows:
;;   1. Higher-order functions (map) over LLM operations
;;   2. Backtracking search with `amb` + semantic validation
;;   3. Agentic mode - LLM queries the runtime to see what you defined
;;   4. Composability - all these primitives work together seamlessly
;;
;; Run: npm run demo
;; ========================================================================

(display "\n🚀 OMEGALLM SHOWCASE - Watch multiple killer features in action!\n\n")

;; ========================================================================
;; PART 1: Higher-Order Functions - Map LLM Over Data
;; ========================================================================
(display "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(display "PART 1: Higher-Order Functions Over LLM Operations\n")
(display "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

(display "Define some customer issues:\n")
(define issues
  (list
    "User can't login - 500 error"
    "Payment declined but charged anyway"
    "Export button does nothing"))

(display "  ✓ Defined: issues\n\n")

(display "Map sentiment analysis over all issues with LLM:\n")
(define sentiments
  (map (lambda (issue)
         (effect infer.op
           (list "Classify sentiment as positive/neutral/negative: " issue)))
       issues))

(display "\n✨ Results:\n")
(display (string-append "  • " (car issues) " → " (car sentiments) "\n"))
(display (string-append "  • " (cadr issues) " → " (cadr sentiments) "\n"))
(display (string-append "  • " (caddr issues) " → " (caddr sentiments) "\n"))

;; ========================================================================
;; PART 2: Backtracking Search with Semantic Validation
;; ========================================================================
(display "\n\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(display "PART 2: Backtracking Search (amb) + LLM Validation\n")
(display "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

(display "Find a response tone that the LLM validates as 'professional':\n")
(display "  (trying: casual, friendly, professional, formal...)\n\n")

(define (is-professional? text)
  (equal? "yes"
    (effect infer.op
      (list "Is this text professional in tone? Answer only yes/no: " text))))

(define professional-response
  (let ((tone (amb "casual" "friendly" "professional" "formal")))
    (let ((reply (effect infer.op
                    (list "Write a " tone " email apologizing for a service outage."))))
      (require (is-professional? reply))
      (list tone reply))))

(display (string-append "✨ Found valid tone: " (car professional-response) "\n"))
(display (string-append "   Response: " (cadr professional-response) "\n"))

;; ========================================================================
;; PART 3: Agentic Query - LLM Queries The Runtime
;; ========================================================================
(display "\n\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(display "PART 3: Agentic Mode - LLM Sees Your Definitions & Calls Code\n")
(display "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

(display "Define some data in the environment:\n")
(define active-tickets
  (list "Auth outage" "Payment stuck" "Export timeout" "Cache stale"))

(define resolved-tickets
  (list "UI bug fixed" "Typo corrected"))

(display "  ✓ active-tickets (4 items)\n")
(display "  ✓ resolved-tickets (2 items)\n\n")

(display "Now ask the LLM - it will query the runtime to find the answer:\n\n")
(define ask-runtime (oracle-lambda (question) "agentic-query"))

(define answer
  (ask-runtime "How many active vs resolved tickets are there? Use (length active-tickets) and (length resolved-tickets) to find out before answering."))

(display "✨ LLM's Answer (after calling length on both lists):\n")
(display (string-append "   " answer "\n"))

;; ========================================================================
;; FINALE
;; ========================================================================
(display "\n\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(display "🎯 THAT'S OMEGALLM:\n")
(display "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
(display "  ✓ Higher-order functions (map/filter/fold) over LLM operations\n")
(display "  ✓ Backtracking search with automatic retry (amb + require)\n")
(display "  ✓ Agentic LLM that queries your code & environment\n")
(display "  ✓ All primitives compose seamlessly\n\n")
(display "Type :help in the REPL to see debugger, time-travel, sessions, and more!\n\n")
