;; Chapter 33: Hierarchical Semantic Structures
;; Demonstrates closure property with semantic hierarchies

(display "Chapter 33: Hierarchical Semantic Structures")
(newline)
(newline)

;; ============================================================
;; 33.2: Dialogue Tree Structure
;; ============================================================

(display "33.2: Dialogue tree with semantic branching")
(newline)

(define greeting-dialogue
  '(bot: "What brings you here today?"
    branches:
    ((user: "I have technical issues")
     (bot: "Tell me about the issue"
       branches:
       ((user: "Login doesn't work") (bot: "Let me help with login"))
       ((user: "App crashes") (bot: "Let me investigate crashes"))))

    ((user: "I have billing questions")
     (bot: "What's your billing question?"
       branches:
       ((user: "Why was I charged?") (bot: "Let me check billing"))
       ((user: "How do I cancel?") (bot: "I can help you cancel")))))))

(display "Dialogue tree structure created")
(newline)
(newline)

;; ============================================================
;; 33.3: Recursive Operations on Trees
;; ============================================================

(display "33.3: Counting utterances in dialogue tree")
(newline)

(define (count-utterances tree)
  (cond
    ((null? tree) 0)
    ((string? tree) 1)
    ((pair? tree)
     (+ (count-utterances (car tree))
        (count-utterances (cdr tree))))
    (else 0)))

(define utterance-count (count-utterances greeting-dialogue))
(display (list "Total utterances:" utterance-count))
(newline)
(newline)

;; ============================================================
;; 33.4: Generating Dialogue Trees with LLM
;; ============================================================

(display "33.4: LLM-generated dialogue expansion")
(newline)

(define (generate-dialogue-branch prompt depth)
  (if (= depth 0)
      (list 'bot: prompt)
      (let ((user-responses
             (effect infer.op
               (list "Given bot says: '" prompt
                     "', list 2 likely user responses (brief)"))))
        (list 'bot: prompt
              'branches:
              (map (lambda (user-resp)
                     (list 'user: user-resp
                           (generate-dialogue-branch
                             (effect infer.op
                               (list "Brief bot response to: " user-resp))
                             (- depth 1))))
                   user-responses)))))

(display "Generating dialogue tree:")
(newline)
(define generated-tree
  (generate-dialogue-branch "Welcome! How can I help?" 2))

(display generated-tree)
(newline)
(newline)

;; ============================================================
;; 33.5: Document Hierarchy with Recursive Summarization
;; ============================================================

(display "33.5: Document hierarchy")
(newline)

(define simple-doc
  '(title: "AI Safety"
    sections:
    ("AI systems are becoming more powerful each year"
     "This power raises important safety questions"
     "Alignment research addresses these challenges")))

(define (summarize-section text)
  (effect infer.op (list "Summarize in 7 words: " text)))

(define (summarize-document doc)
  (let* ((title (car doc))
         (sections (cdr doc))
         (summaries (map summarize-section sections)))
    (list 'title: title
          'summary: (effect infer.op
                     (list "Overall summary from: " summaries)))))

(display "Document summary:")
(newline)
(display (summarize-document simple-doc))
(newline)
(newline)

;; ============================================================
;; 33.7: Flattening Hierarchies
;; ============================================================

(display "33.7: Flattening dialogue tree")
(newline)

(define (flatten-tree tree)
  (cond
    ((null? tree) '())
    ((string? tree) (list tree))
    ((pair? tree)
     (append (flatten-tree (car tree))
             (flatten-tree (cdr tree))))
    (else '())))

(define flat-dialogue (flatten-tree greeting-dialogue))
(display "Flattened utterances (first 5):")
(newline)
(display (take 5 flat-dialogue))
(newline)
(newline)

;; ============================================================
;; 33.10: Knowledge Hierarchy Exploration
;; ============================================================

(display "33.10: Knowledge tree exploration")
(newline)

(define (explore-knowledge topic depth)
  (if (= depth 0)
      (list 'concept: topic)
      (let ((subtopics
             (effect infer.op
               (list "List 2 key subtopics of '" topic "' (brief)"))))
        (list 'concept: topic
              'subtopics:
              (map (lambda (sub)
                     (explore-knowledge sub (- depth 1)))
                   subtopics)))))

(display "Exploring 'Programming' to depth 2:")
(newline)
(define knowledge-tree (explore-knowledge "Programming" 2))
(display knowledge-tree)
(newline)
(newline)

;; ============================================================
;; Helper: Take first n elements
;; ============================================================

(define (take n lst)
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

;; ============================================================
;; Demonstration Complete
;; ============================================================

(display "Chapter 33 complete!")
(newline)
(display "Key insight: Closure property enables arbitrary semantic hierarchies")
(newline)
