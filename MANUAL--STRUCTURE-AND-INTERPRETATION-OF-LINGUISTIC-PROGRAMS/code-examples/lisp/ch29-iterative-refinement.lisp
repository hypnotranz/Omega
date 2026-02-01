;; Chapter 29: Iterative Semantic Refinement
;; Demonstrates Newton's method adapted for semantic quality improvement

;; ============================================================
;; 29.2: Basic Semantic Refinement Pattern
;; ============================================================

(display "29.2: Iterative semantic refinement")
(newline)

(define (good-enough? response criteria)
  (equal? "yes"
    (effect infer.op
      (list "Does this response meet the criteria? Answer yes or no.\n"
            "Response: " response "\n"
            "Criteria: " criteria))))

(define (improve response feedback)
  (effect infer.op
    (list "Improve this response based on feedback:\n"
          "Response: " response "\n"
          "Feedback: " feedback)))

(define (get-feedback response criteria)
  (effect infer.op
    (list "What's wrong with this response?\n"
          "Response: " response "\n"
          "Criteria: " criteria)))

(define (refine initial-response criteria max-iterations)
  (define (iter response count)
    (begin
      (display (list "Iteration" count ": " response))
      (newline)
      (if (or (good-enough? response criteria)
              (= count max-iterations))
          response
          (iter (improve response (get-feedback response criteria))
                (+ count 1)))))
  (iter initial-response 0))

;; Test basic refinement
(display "Refining greeting to be more professional:")
(newline)
(define refined-greeting
  (refine "Hello"
          "Make it more professional and empathetic"
          3))

(display "Final result: ")
(display refined-greeting)
(newline)
(newline)

;; ============================================================
;; 29.4: Example - Refining Email Tone
;; ============================================================

(display "29.4: Refining email tone")
(newline)

(define initial-draft
  "We apologize for the inconvenience. This was not our fault.")

(define criteria
  "Tone should be genuinely apologetic without being defensive or making excuses.")

(display "Initial draft: ")
(display initial-draft)
(newline)
(display "Refining to meet criteria...")
(newline)

(define refined-email (refine initial-draft criteria 4))

(display "Final email: ")
(display refined-email)
(newline)
(newline)

;; ============================================================
;; 29.5: Convergence with Scoring
;; ============================================================

(display "29.5: Tracking convergence with scores")
(newline)

(define (get-score response criteria)
  (effect infer.op
    (list "Rate how well this meets criteria (0-10, respond with just the number):\n"
          "Response: " response "\n"
          "Criteria: " criteria)))

(define (refine-with-tracking initial criteria max-iter target-score)
  (define (iter response count)
    (let ((score (get-score response criteria)))
      (begin
        (display (list "Iteration" count "- Score:" score))
        (newline)
        (if (or (>= score target-score) (= count max-iter))
            response
            (iter (improve response (get-feedback response criteria))
                  (+ count 1))))))
  (iter initial 0))

(display "Refining with score tracking (target 8/10):")
(newline)
(define scored-result
  (refine-with-tracking
    "Thanks"
    "Should be warm and professional"
    4
    8))

(display "Final: ")
(display scored-result)
(newline)
(newline)

;; ============================================================
;; 29.6: Multi-Criteria Refinement
;; ============================================================

(display "29.6: Multi-criteria refinement")
(newline)

(define (is-professional? response)
  (equal? "yes" (effect infer.op
    (list "Is this professional? yes or no: " response))))

(define (is-empathetic? response)
  (equal? "yes" (effect infer.op
    (list "Is this empathetic? yes or no: " response))))

(define (is-concise? response max-len)
  (<= (length response) max-len))

(define (multi-criteria-good-enough? response max-len)
  (and
    (is-professional? response)
    (is-empathetic? response)
    (is-concise? response max-len)))

(define (gather-all-feedback response max-len)
  (list
    (if (not (is-professional? response))
        "Make more professional. "
        "")
    (if (not (is-empathetic? response))
        "Add more empathy. "
        "")
    (if (> (length response) max-len)
        "Make more concise. "
        "")))

(define (refine-multi initial max-len max-iter)
  (define (iter response count)
    (begin
      (display (list "Multi-criteria iteration" count))
      (newline)
      (if (or (multi-criteria-good-enough? response max-len)
              (= count max-iter))
          response
          (let ((feedback (gather-all-feedback response max-len)))
            (iter (improve response feedback) (+ count 1))))))
  (iter initial 0))

(display "Refining for professional + empathetic + concise:")
(newline)
(define multi-result
  (refine-multi
    "Hey, sorry about that problem you had"
    80
    3))

(display "Final: ")
(display multi-result)
(newline)
(newline)

;; ============================================================
;; 29.8: Cost-Aware Refinement
;; ============================================================

(display "29.8: Cost-aware refinement - try direct first")
(newline)

(define (smart-refine criteria max-iter)
  ;; Try direct generation first
  (let ((direct-attempt
         (effect infer.op (list "Generate response meeting: " criteria))))
    (begin
      (display "Direct attempt: ")
      (display direct-attempt)
      (newline)
      (if (good-enough? direct-attempt criteria)
          (begin
            (display "Direct attempt was good enough!")
            (newline)
            direct-attempt)
          (begin
            (display "Refining direct attempt...")
            (newline)
            (refine direct-attempt criteria max-iter))))))

(define smart-result
  (smart-refine "Brief, friendly greeting" 2))

(display "Final: ")
(display smart-result)
(newline)
(newline)

;; ============================================================
;; Demonstration Complete
;; ============================================================

(display "Chapter 29 demonstration complete!")
(newline)
(display "Key insight: Iterative refinement brings responses closer to semantic criteria")
(newline)
