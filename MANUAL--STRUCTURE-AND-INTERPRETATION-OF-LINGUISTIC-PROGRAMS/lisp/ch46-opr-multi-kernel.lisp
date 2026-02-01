;; Chapter 46: OPR Multi-Kernel Execution
;; Route tasks to specialized kernels.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch46-opr-multi-kernel

(define (select-kernel task)
  (effect infer.op
    (list "Which kernel? reasoning/code/creative/fast: " task)))

(list
  (select-kernel "Explain quantum entanglement")
  (select-kernel "Write a Python function for merge sort")
  (select-kernel "Brainstorm blog post ideas")
  (select-kernel "What is 2+2?"))
