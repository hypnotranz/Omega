;; Chapter 21: Mutable Semantic Structures
;; Evolve a simple relation list with semantic checks.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch21-mutable-semantic

(define relations (list "login -> error pages" "refund -> frustration"))
(set! relations (cons "healthcare -> compliance questions" relations))

(effect infer.op
  (list "Summarize these relations in one sentence, keeping causal tone: " relations))
