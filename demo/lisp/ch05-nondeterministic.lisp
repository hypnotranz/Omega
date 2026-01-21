;; Chapter 5: Nondeterministic Search (AMB)
;; Backtrack across tone options until a semantic predicate passes.
;;
;; `amb` chooses one candidate; if `require` later fails, evaluation backtracks
;; to try the next option automatically.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch05-nondeterministic

(define tones (list "formal" "friendly" "apologetic"))

(define (matches-tone? reply desired)
  (equal? "yes"
    (effect infer.op
      (list "Does this reply use a " desired " tone? yes/no: " reply))))

(let ((tone (amb "formal" "friendly" "apologetic")))
  (let ((reply (effect infer.op
                  (list "Write a " tone " response acknowledging a delayed shipment."))))
    (require (matches-tone? reply "apologetic"))
    reply))
