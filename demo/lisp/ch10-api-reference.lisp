;; Chapter 10: Full API Reference
;; Combine infer, search, and amb in one small scenario.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch10-api-reference

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
