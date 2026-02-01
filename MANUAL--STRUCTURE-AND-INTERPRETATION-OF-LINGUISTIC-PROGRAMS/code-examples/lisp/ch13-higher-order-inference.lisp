;; Chapter 13: Higher-Order Inference
;; Fold stakeholder opinions using infer.op as the combiner.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch13-higher-order-inference

(define opinions
  (list
    "Engineering wants fewer meetings and clearer acceptance criteria."
    "Support needs a calmer tone in outage updates."
    "Legal wants explicit mention of data residency obligations."))

(define (merge consensus opinion)
  (effect infer.op
    (list "Merge this opinion into the current consensus. Keep it concise and empathetic."
          "Consensus: " consensus
          "Opinion: " opinion)))

(fold-left merge "Start with a balanced plan." opinions)
