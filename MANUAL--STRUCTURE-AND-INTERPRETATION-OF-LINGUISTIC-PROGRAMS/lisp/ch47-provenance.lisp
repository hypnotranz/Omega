;; Chapter 47: Provenance And Evidence Chains
;; Track reasoning steps with evidence nodes.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch47-provenance

(define (get-credit app) 720)
(define (get-income app) 75000)

(define (should-approve? application)
  (let* ((credit-ok?
           (effect infer.op (list "Credit adequate? " (get-credit application))))
         (income-ok?
           (effect infer.op (list "Income sufficient? " (get-income application)))))
    (and credit-ok? income-ok?)))

;; Returns result + evidence chain showing each LLM call and timestamp
(should-approve? 'alice-application)
