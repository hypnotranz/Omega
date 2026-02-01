;; Chapter 34: Symbolic Semantic Data
;; Discourse relations as symbolic expressions that can be linearized into coherent text.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch34-symbolic-semantic

;; Symbolic rhetoric structure using discourse operators
(define rhetoric-tree
  '(contrast
     (elaboration
       (claim "AI is powerful")
       (evidence "GPT-4 writes code"))
     (cause
       (claim "Risks exist")
       (claim "Hallucinations occur"))))

;; Helper to check if something is an atom
(define (atom? x)
  (not (pair? x)))

;; Linearize rhetorical operators → coherent paragraph
;; Recursively converts symbolic discourse structure into natural language
(define (linearize tree)
  (cond
    ((atom? tree) tree)
    ((eq? (car tree) 'claim)
     (cadr tree))  ; Extract claim text
    ((eq? (car tree) 'evidence)
     (cadr tree))  ; Extract evidence text
    ((eq? (car tree) 'elaboration)
     (effect infer.op
       (list "Combine with 'specifically': "
             (linearize (cadr tree)) ", " (linearize (caddr tree)))))
    ((eq? (car tree) 'contrast)
     (effect infer.op
       (list "Combine with 'however': "
             (linearize (cadr tree)) " vs " (linearize (caddr tree)))))
    ((eq? (car tree) 'cause)
     (effect infer.op
       (list "Combine with 'because': "
             (linearize (cadr tree)) " + " (linearize (caddr tree)))))
    (else tree)))

;; Example: More complex rhetoric tree
(define support-article
  '(claim "Customer support quality matters"
    (elaboration
      (claim "Quick responses increase satisfaction")
      (evidence "Studies show 80% prefer fast replies"))
    (contrast
      (claim "Automation helps scale")
      (cause
        (claim "Human touch still needed")
        (evidence "Complex issues require empathy")))))

(list
  "LINEARIZED-SIMPLE:"
  (linearize rhetoric-tree)
  "LINEARIZED-COMPLEX:"
  (linearize support-article))
