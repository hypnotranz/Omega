;; Chapter 26: The AMB Inference Engine
;; Constraint satisfaction with semantic predicates.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch26-amb-inference

(define tones (list "formal" "empathetic" "playful"))
(define intents (list "explain risk" "apologize" "upsell"))

(define (fits? tone intent)
  (equal? "yes"
    (effect infer.op
      (list "Is this tone appropriate for the intent? yes/no: " tone " -> " intent))))

(let ((tone (amb "formal" "empathetic" "playful")))
  (let ((intent (amb "explain risk" "apologize")))
    (require (fits? tone intent))
    (list tone intent)))
