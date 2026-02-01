;; Chapter 16: Symbolic Semantic Data
;; Meaning equivalence checks on emotionally worded phrases.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch16-symbolic-semantic

(define (same-meaning? a b)
  (equal? "true"
    (effect infer.op (list "Do these mean the same thing? true/false: " a " | " b))))

(list
  (same-meaning? "I feel upset about the delay" "I am dissatisfied with how long this is taking")
  (same-meaning? "This is delightful" "This is unacceptable"))
