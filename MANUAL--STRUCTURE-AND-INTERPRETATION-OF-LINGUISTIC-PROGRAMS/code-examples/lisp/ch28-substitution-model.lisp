;; Chapter 28: The Substitution Model for Semantic Evaluation
;; Demonstrates step-by-step substitution with semantic effects

;; ============================================================
;; 28.2: Basic Substitution with Semantic Effects
;; ============================================================

(display "28.2: Basic substitution example")
(newline)

(define (sentiment text)
  (effect infer.op (list "Sentiment of: " text)))

;; Trace the substitution:
;; (sentiment "I love this!")
;; => (effect infer.op (list "Sentiment of: " "I love this!"))
;; => (effect infer.op "Sentiment of: I love this!")
;; => "positive"

(display "Sentiment analysis: ")
(display (sentiment "I love this!"))
(newline)
(newline)

;; ============================================================
;; 28.3: Nested Substitution - Compositions
;; ============================================================

(display "28.3: Nested substitution with compositions")
(newline)

(define (analyze-sentiment-length text)
  (list (sentiment text) (length text)))

;; Trace:
;; (analyze-sentiment-length "Great!")
;; => (list (sentiment "Great!") (length "Great!"))
;; => (list "positive" 6)

(display "Analysis: ")
(display (analyze-sentiment-length "Great!"))
(newline)
(newline)

;; ============================================================
;; 28.4: Substitution Order Matters
;; ============================================================

(display "28.4: Multiple calls may produce different results")
(newline)

(define (creative-greeting)
  (effect infer.op "Generate a unique greeting in 3-5 words"))

(define (double-greeting)
  (list (creative-greeting) (creative-greeting)))

;; Each call to creative-greeting is evaluated independently
;; May produce different results

(display "Two greetings: ")
(display (double-greeting))
(newline)
(newline)

;; ============================================================
;; 28.5: Applicative Order Evaluation
;; ============================================================

(display "28.5: Applicative order - arguments evaluated first")
(newline)

(define (verbose-sentiment text)
  (let ((result (effect infer.op (list "Sentiment of: " text))))
    (begin
      (display "  [Sentiment evaluated]: ")
      (display result)
      (newline)
      result)))

(define (if-positive text action)
  (if (equal? (verbose-sentiment text) "positive")
      action
      "neutral"))

;; In applicative order, 'action' is evaluated BEFORE if-positive is entered
;; Watch the output to see evaluation order

(display "Testing if-positive:")
(newline)
(display (if-positive "I love this!" "SUCCESS"))
(newline)
(newline)

;; ============================================================
;; 28.6: Substitution and Prompt Construction
;; ============================================================

(display "28.6: How prompts get built inside-out")
(newline)

(define (translate-to lang text)
  (effect infer.op (list "Translate to " lang ": " text)))

(define (friendly-translate lang greeting-name)
  (translate-to lang
    (effect infer.op (list "Say hello to " greeting-name))))

;; Trace:
;; (friendly-translate "French" "Alice")
;; => (translate-to "French" (effect infer.op (list "Say hello to " "Alice")))
;; => (translate-to "French" "Hello, Alice!")
;; => (effect infer.op (list "Translate to " "French" ": " "Hello, Alice!"))
;; => "Bonjour, Alice!"

(display "Nested translation: ")
(display (friendly-translate "French" "Alice"))
(newline)
(newline)

;; ============================================================
;; 28.9: Practical Example - Debugging Prompts
;; ============================================================

(display "28.9: Using substitution to debug prompts")
(newline)

;; Original version - too vague
(define (classify category text)
  (effect infer.op (list "Is this " category "? yes or no: " text)))

(define (is-urgent-v1? email)
  (equal? "yes" (classify "urgent" email)))

;; Trace what prompt is sent:
;; (is-urgent-v1? "Meeting moved to tomorrow")
;; => (equal? "yes" (classify "urgent" "Meeting moved to tomorrow"))
;; => (equal? "yes" (effect infer.op "Is this urgent? yes or no: Meeting moved to tomorrow"))
;; Prompt is too vague!

(display "Version 1 (vague): ")
(display (is-urgent-v1? "Meeting moved to tomorrow"))
(newline)

;; Improved version - more context
(define (is-urgent-v2? email)
  (equal? "yes" (classify "urgent requiring immediate response" email)))

;; Now the prompt is clearer
(display "Version 2 (clear): ")
(display (is-urgent-v2? "Meeting moved to tomorrow"))
(newline)
(newline)

;; ============================================================
;; Demonstration Complete
;; ============================================================

(display "Chapter 28 demonstration complete!")
(newline)
(display "Key insight: Substitution model shows how prompts are built step-by-step")
(newline)
