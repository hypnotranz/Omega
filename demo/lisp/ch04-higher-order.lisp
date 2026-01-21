;; Chapter 4: Higher-Order LLM Functions
;; Factories that return semantic classifiers.
;;
;; `make-classifier` returns a new procedure that bakes a topic into the prompt
;; and reuses it for each call.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch04-higher-order

(define (make-classifier topic)
  (lambda (snippet)
    (effect infer.op
      (list "Classify this text into a " topic " bucket: " snippet))))

(define classify-risk (make-classifier "risk level (high/medium/low)"))

(list
  (classify-risk "Credentials leaked on a public git repo with customer secrets.")
  (classify-risk "Routine maintenance window notification with no user impact."))
