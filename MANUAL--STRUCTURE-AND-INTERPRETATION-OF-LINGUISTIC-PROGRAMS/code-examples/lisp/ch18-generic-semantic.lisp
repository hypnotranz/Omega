;; Chapter 18: Generic Semantic Operations
;; Domain-aware summarization.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch18-generic-semantic

(define issue "Customer shared medical data while requesting a refund and asked for SOC2 proof.")

(define (summarize-legal text)
  (effect infer.op (list "Provide a legal summary highlighting duties: " text)))

(define (summarize-support text)
  (effect infer.op (list "Provide a support summary with calming reassurance: " text)))

(list (summarize-legal issue) (summarize-support issue))
