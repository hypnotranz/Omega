;; Chapter 30: Tree Recursion with Semantic Branching
;; Demonstrates tree recursion where LLM decides branching structure

;; ============================================================
;; 30.2: Basic Semantic Tree Exploration
;; ============================================================

(display "30.2: Semantic tree exploration")
(newline)

(define (semantic-tree-explore topic depth)
  (if (= depth 0)
      (list topic)
      (let ((subtopics
             (effect infer.op
               (list "List 2-3 key subtopics of: " topic))))
        (cons topic
              (map (lambda (subtopic)
                     (semantic-tree-explore subtopic (- depth 1)))
                   subtopics)))))

(display "Exploring 'Artificial Intelligence' to depth 2:")
(newline)
(define ai-tree (semantic-tree-explore "Artificial Intelligence" 2))
(display ai-tree)
(newline)
(newline)

;; ============================================================
;; 30.4: Controlling Branching
;; ============================================================

(display "30.4: Controlled branching with max limit")
(newline)

(define (take n lst)
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

(define (explore-with-limit topic depth max-branches)
  (if (= depth 0)
      (list topic)
      (let ((all-subtopics
             (effect infer.op
               (list "List up to " max-branches " subtopics of: " topic))))
        (cons topic
              (map (lambda (subtopic)
                     (explore-with-limit subtopic
                                        (- depth 1)
                                        max-branches))
                   (take max-branches all-subtopics))))))

(display "Exploring 'Programming' with max 2 branches per node:")
(newline)
(define limited-tree (explore-with-limit "Programming" 2 2))
(display limited-tree)
(newline)
(newline)

;; ============================================================
;; 30.5: Argument Decomposition Example
;; ============================================================

(display "30.5: Decomposing an argument into premises")
(newline)

(define (decompose-argument claim depth)
  (if (= depth 0)
      claim
      (let ((premises
             (effect infer.op
               (list "What are 2 key premises supporting: " claim))))
        (list claim
              (map (lambda (premise)
                     (decompose-argument premise (- depth 1)))
                   premises)))))

(display "Decomposing: 'Software testing improves quality'")
(newline)
(define argument-tree
  (decompose-argument "Software testing improves quality" 2))
(display argument-tree)
(newline)
(newline)

;; ============================================================
;; 30.6: Pruning - Avoiding Duplicates
;; ============================================================

(display "30.6: Exploration with duplicate detection")
(newline)

(define (member? item lst)
  (cond
    ((null? lst) #f)
    ((equal? item (car lst)) #t)
    (else (member? item (cdr lst)))))

(define (explore-no-duplicates topic depth visited)
  (if (or (= depth 0) (member? topic visited))
      (list topic)
      (let ((subtopics
             (effect infer.op
               (list "List 2 subtopics of: " topic))))
        (cons topic
              (map (lambda (subtopic)
                     (explore-no-duplicates subtopic
                                           (- depth 1)
                                           (cons topic visited)))
                   (filter (lambda (st) (not (member? st visited)))
                           subtopics))))))

(display "Exploring without duplicates:")
(newline)
(define no-dup-tree (explore-no-duplicates "Technology" 2 '()))
(display no-dup-tree)
(newline)
(newline)

;; ============================================================
;; 30.7: Flattening Tree Results
;; ============================================================

(display "30.7: Flattening tree to list of all topics")
(newline)

(define (flatten tree)
  (cond
    ((null? tree) '())
    ((pair? tree)
     (append (flatten (car tree))
             (flatten (cdr tree))))
    (else (list tree))))

(define (explore-and-flatten topic depth)
  (flatten (semantic-tree-explore topic depth)))

(display "Flattened exploration of 'Machine Learning':")
(newline)
(define flat-topics (explore-and-flatten "Machine Learning" 2))
(display flat-topics)
(newline)
(newline)

;; ============================================================
;; 30.8: Decision Trees with Semantic Predicates
;; ============================================================

(display "30.8: Decision tree with LLM-guided branching")
(newline)

(define (semantic-decision-tree question depth)
  (if (= depth 0)
      (effect infer.op (list "Brief answer for: " question))
      (let* ((should-split
              (effect infer.op
                (list "Should we break down '" question "' further? yes or no")))
             (answer
              (effect infer.op (list "Brief answer: " question))))
        (if (equal? should-split "no")
            answer
            (let ((subquestions
                   (effect infer.op
                     (list "Break '" question "' into 2 simpler subquestions"))))
              (list question
                    (map (lambda (sq)
                           (semantic-decision-tree sq (- depth 1)))
                         subquestions)))))))

(display "Decision tree for: 'How to learn programming?'")
(newline)
(define decision-tree
  (semantic-decision-tree "How to learn programming?" 2))
(display decision-tree)
(newline)
(newline)

;; ============================================================
;; Helper: Count Nodes in Tree
;; ============================================================

(display "Exercise: Counting nodes in a tree")
(newline)

(define (count-nodes tree)
  (cond
    ((null? tree) 0)
    ((pair? tree)
     (+ (count-nodes (car tree))
        (count-nodes (cdr tree))))
    (else 1)))

(define test-tree '("root" ("a" ("a1") ("a2")) ("b" ("b1"))))
(display "Test tree: ")
(display test-tree)
(newline)
(display "Node count: ")
(display (count-nodes test-tree))
(newline)
(newline)

;; ============================================================
;; Demonstration Complete
;; ============================================================

(display "Chapter 30 demonstration complete!")
(newline)
(display "Key insight: Tree structure emerges from LLM's semantic understanding")
(newline)
