;; Chapter 27: Logic Programming with Semantic Facts
;; Pattern: LLM perception → deterministic logic
;; LLM parses natural language facts, then pure logic operates on structured data

;; Helper: Parse natural language fact into boolean
(define (parse-fact fact)
  (effect infer.op (list "Does this statement describe a parent-child relationship? Answer yes/no: " fact)))

;; Helper: Convert LLM yes/no response to boolean
(define (yesno->bool response)
  (if (equal? response "yes") #t #f))

;; Deterministic logic: grandparent rule
;; If A is parent of B and B is parent of C, then A is grandparent of C
(define (is-grandparent? fact1-is-parent fact2-is-parent)
  (if (and fact1-is-parent fact2-is-parent)
      #t
      #f))

;; Main: LLM parses NL facts, then deterministic logic combines them
(let ((fact1 "Alice is Bob's mother")
      (fact2 "Bob is Charlie's father"))
  (let ((f1-parsed (parse-fact fact1))
        (f2-parsed (parse-fact fact2)))
    (let ((f1-bool (yesno->bool f1-parsed))
          (f2-bool (yesno->bool f2-parsed)))
      ;; Deterministic logic: if both are parent relationships, Alice is Charlie's grandparent
      (is-grandparent? f1-bool f2-bool))))
