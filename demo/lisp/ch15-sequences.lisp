;; Chapter 15: Sequences as Semantic Interfaces
;; Pipeline complaints -> issues -> prioritization.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch15-sequences

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
