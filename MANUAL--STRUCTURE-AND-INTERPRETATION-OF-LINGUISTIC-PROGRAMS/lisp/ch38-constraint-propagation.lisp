;; Chapter 38: Constraint Propagation Networks
;; Pattern: Constraints maintain consistency across connected values
;; LLM validates constraint satisfaction

;; Helper: Check if temperature values satisfy C = (F - 32) * 5/9
(define (check-temp-constraint celsius fahrenheit)
  (effect infer.op (list "Are these temperatures equivalent? "
                         (string-append "C=" (number->string celsius))
                         (string-append "F=" (number->string fahrenheit))
                         " Answer yes or no")))

;; Helper: Convert yes/no to boolean
(define (yesno->bool s)
  (equal? s "yes"))

;; Constraint propagation test
;; Given: C=0, F=32 (freezing point of water)
;; Constraint: C = (F - 32) * 5/9
;; Expected: LLM recognizes these are equivalent
(let ((celsius 0)
      (fahrenheit 32))
  (let ((check-result (check-temp-constraint celsius fahrenheit)))
    (yesno->bool check-result)))
