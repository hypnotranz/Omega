;; Chapter 23: Streams of Inference
;; Potentially infinite semantic expansion, truncated on demand.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch23-streams-of-inference

(define (expand idea)
  (effect infer.op
    (list "Generate a richer explanation building on this idea: " idea)))

(define (iterate n seed)
  (if (= n 0)
      (list seed)
      (cons seed (iterate (- n 1) (expand seed)))))

(iterate 3 "Calmly communicate risk to non-technical stakeholders.")
