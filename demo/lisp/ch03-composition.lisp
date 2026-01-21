;; Chapter 3: Functional Composition
;; Map and filter with semantic predicates.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch03-composition

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
