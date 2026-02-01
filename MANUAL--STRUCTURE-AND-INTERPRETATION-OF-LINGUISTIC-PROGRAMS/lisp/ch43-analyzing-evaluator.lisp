;; Chapter 43: Analyzing Evaluator
;; Dependency analysis discovers optimization opportunities.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch43-analyzing-evaluator

(define program
  '(begin
     (define summary1 (effect infer.op "Summarize doc1"))
     (define summary2 (effect infer.op "Summarize doc2"))
     (define synthesis (effect infer.op (list "Combine: " summary1 summary2)))))

(define (analyze-dependencies prog)
  (effect infer.op
    (list "Identify parallelizable operations: " prog)))

(analyze-dependencies program)
