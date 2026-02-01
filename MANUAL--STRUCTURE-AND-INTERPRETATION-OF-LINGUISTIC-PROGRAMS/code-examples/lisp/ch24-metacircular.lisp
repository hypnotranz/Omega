;; Chapter 24: Metalinguistic Abstraction
;; Oracle asks to evaluate a helper expression before answering.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch24-metacircular

(define helper "sanitize-and-trim")
(define explain (oracle-lambda (hint) "explain-macro"))
(explain helper)
