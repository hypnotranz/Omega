;; Chapter 22: Concurrent Inference
;; Parallel-map sketch using semantic tasks.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch22-concurrent-inference

(define tickets
  (list
    "Cannot login to my account after password reset."
    "When will the new analytics feature be available?"
    "The app crashed and deleted my draft report."
    "How do I export my reports to PDF?"))

(define (classify ticket)
  (effect infer.op
    (list "Classify this support ticket. Return bug/feature-request/question/complaint: " ticket)))

(define (parallel-map f xs) (map f xs)) ; sequential stand-in for demo
(parallel-map classify tickets)
