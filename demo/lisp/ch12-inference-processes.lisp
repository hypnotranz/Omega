;; Chapter 12: Inference Processes
;; Contrast recursive vs iterative summarization.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch12-inference-processes

(define report
  "Customer anger escalated because the refund workflow failed twice. They also praised the clarity of the troubleshooting steps once resolved.")

(define (recursive-summarize text depth)
  (if (= depth 0)
      (effect infer.op (list "Summarize in one tight sentence: " text))
      (recursive-summarize
        (effect infer.op (list "Summarize the core issue: " text))
        (- depth 1))))

(list
  (recursive-summarize report 1)
  (effect infer.op (list "Summarize iteratively with cost awareness: " report)))
