;; Chapter 33: Hierarchical Semantic Structures
;; Composite dialogue tree + recursive interpretation.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch33-hierarchical-structures

;; Constructors
(define (node assistant branches) (list 'node assistant branches))
(define (branch label child) (list 'branch label child))

;; Accessors
(define (node-text n) (cadr n))
(define (node-branches n) (caddr n))
(define (branch-label b) (cadr b))
(define (branch-child b) (caddr b))

(define support-tree
  (node
    "Hi — what can I help you with today?"
    (list
      (branch "login trouble"
        (node
          "Got it. Are you seeing an error message, or is it just not accepting your password?"
          (list
            (branch "error message"
              (node "Please paste the exact error text (remove secrets)." '()))
            (branch "password rejected"
              (node "Have you tried a password reset in the last 24 hours?" '())))))
      (branch "billing question"
        (node
          "Happy to help with billing. Is this about an unexpected charge or updating your plan?"
          (list
            (branch "unexpected charge"
              (node "Understood. What is the invoice date and the last 4 of the card on file?" '()))
            (branch "update plan"
              (node "Which plan are you moving to, and is this for monthly or annual billing?" '()))))))))

;; Structural recursion over the dialogue tree (Visitor pattern)
(define (tree-map f tree)
  (let ((txt (node-text tree))
        (bs (node-branches tree)))
    (node
      (f txt)
      (map
        (lambda (b)
          (branch (branch-label b)
                  (tree-map f (branch-child b))))
        bs))))

(define (rewrite-in-tone tone sentence)
  (effect infer.op
    (list "Rewrite in a " tone " tone, preserving meaning:\n" sentence)))

(define rewritten
  (tree-map (lambda (s) (rewrite-in-tone "warm-professional" s))
            support-tree))

;; Summarize each subtree into an operator-facing "flow summary"
(define (summarize-subtree tree)
  (let ((txt (node-text tree))
        (bs (node-branches tree)))
    (if (equal? bs '())
        (effect infer.op (list "Summarize this step in 6 words:\n" txt))
        (let ((child-summaries
               (map (lambda (b) (summarize-subtree (branch-child b))) bs)))
          (effect infer.op
            (list "Summarize this dialogue node and its options.\n"
                  "Node: " txt "\n"
                  "Options: " child-summaries "\n"
                  "Return one sentence."))))))

(list
  "FLOW-SUMMARY:"
  (summarize-subtree support-tree))
