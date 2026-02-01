;; Chapter 25: Lazy Semantic Evaluation
;; Memoize a semantic result and reuse it.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch25-lazy-semantic

(define cached #f)

(define (analyze text)
  (if (not (equal? cached #f))
      cached
      (begin
        (set! cached (effect infer.op (list "Assess emotional temperature (calm/tense): " text)))
        cached)))

(list
  (analyze "The outage update felt tense and robotic.")
  (analyze "Reusing the memoized tone analysis to avoid another call."))
