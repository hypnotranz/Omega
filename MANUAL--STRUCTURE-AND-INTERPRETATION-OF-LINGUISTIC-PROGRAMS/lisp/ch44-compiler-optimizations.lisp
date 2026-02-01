;; Chapter 44: Compiler Optimizations
;; Call batching and deduplication.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch44-compiler-optimizations

(define program
  '(begin
     (sentiment "Great product")
     (sentiment "Great product")  ; Duplicate
     (sentiment "Poor service")))

(define (optimize prog)
  (effect infer.op
    (list "Optimize by removing duplicates and batching: " prog)))

(optimize program)
