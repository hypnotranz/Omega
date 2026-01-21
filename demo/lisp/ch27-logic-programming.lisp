;; Chapter 27: Logic Programming with Semantic Facts
;; Query natural language facts with semantic matching.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch27-logic-programming

(define facts
  (list
    "Alice is Bob's parent."
    "Bob is Carol's parent."
    "Dana mentors Erin in compliance audits."))

(define (is-grandparent? a c)
  (equal? "yes"
    (effect infer.op
      (list "Given these facts, is " a " the grandparent of " c "? yes/no: " facts))))

(is-grandparent? "Alice" "Carol")
