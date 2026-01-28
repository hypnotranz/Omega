;; usecase-research-agent.lisp
;;
;; RESEARCH AGENT that searches, reads, and synthesizes information
;; Maintains state across multiple queries and builds knowledge
;;
;; Run: omega --file demo/lisp/usecase-research-agent.lisp --caps shell,file.read,file.write,infer

;; ============================================================================
;; KNOWLEDGE BASE (stateful)
;; ============================================================================

(define knowledge-base '())
(define sources '())
(define queries-made '())

;; Add fact to knowledge base
(define (learn! fact source)
  (set! knowledge-base (cons (cons fact source) knowledge-base))
  (set! sources (cons source sources))
  fact)

;; Query knowledge base
(define (recall topic)
  (filter (lambda (entry)
            (string-contains? (car entry) topic))
          knowledge-base))

;; Get all knowledge as context
(define (get-context)
  (if (null? knowledge-base)
      "No prior knowledge."
      (string-join
        (map (lambda (entry)
               (string-append "- " (car entry) " [from: " (cdr entry) "]"))
             knowledge-base)
        "\n")))

;; ============================================================================
;; SEARCH & EXTRACTION
;; ============================================================================

;; Search local codebase for a pattern
(define (search-code pattern)
  (let ((result (effect shell.op
                  (string-append "grep -r '" pattern "' --include='*.ts' --include='*.js' . 2>/dev/null | head -20"))))
    (set! queries-made (cons (cons 'code-search pattern) queries-made))
    result))

;; Search for files by name
(define (find-files pattern)
  (let ((result (effect shell.op
                  (string-append "find . -name '" pattern "' -type f 2>/dev/null"))))
    (filter (lambda (f) (not (equal? f "")))
            (string-split result "\n"))))

;; Read and extract key info from a file
(define (read-and-extract filepath question)
  (let* ((content (effect file.read.op filepath))
         (answer (effect infer.op
                   (list "From this file (" filepath "):\n```\n"
                         (substring content 0 (min 3000 (string-length content)))
                         "\n```\n\n" question))))
    (learn! answer filepath)
    answer))

;; ============================================================================
;; REASONING
;; ============================================================================

;; Ask a question using accumulated knowledge
(define (reason question)
  (let ((context (get-context)))
    (effect infer.op
      (list "Based on what you know:\n" context
            "\n\nAnswer: " question
            "\n\nIf you don't have enough information, say what's missing."))))

;; Generate hypotheses
(define (hypothesize question)
  (effect infer.op
    (list "Given this context:\n" (get-context)
          "\n\nGenerate 3 hypotheses about: " question
          "\n\nFormat as numbered list.")))

;; ============================================================================
;; RESEARCH WORKFLOWS
;; ============================================================================

;; Research a topic by exploring files
(define (research-topic topic)
  (let* (;; Step 1: Find relevant files
         (files (find-files (string-append "*" topic "*")))
         ;; Step 2: Search for mentions in code
         (code-refs (search-code topic))
         ;; Step 3: Analyze top results
         (analysis (if (> (length files) 0)
                      (map (lambda (f)
                             (read-and-extract f
                               (string-append "What does this tell us about " topic "?")))
                           (take files 3))
                      '()))
         ;; Step 4: Synthesize findings
         (synthesis (reason (string-append "What do we now understand about " topic "?"))))
    (list
      (cons 'files-found (length files))
      (cons 'code-refs code-refs)
      (cons 'analysis analysis)
      (cons 'synthesis synthesis))))

;; Deep dive into a specific question
(define (deep-dive question)
  (define (dive depth)
    (if (> depth 3)
        (reason question)
        (let* ((current-knowledge (get-context))
               (next-query (effect infer.op
                            (list "To answer: " question
                                  "\n\nI currently know:\n" current-knowledge
                                  "\n\nWhat ONE specific thing should I search for next? "
                                  "Give a grep pattern or filename.")))
               (search-result (search-code next-query))
               (_ (learn! (string-append "Search for '" next-query "': "
                                        (substring search-result 0 (min 500 (string-length search-result))))
                          "code-search")))
          (dive (+ depth 1)))))
  (dive 0))

;; ============================================================================
;; MULTI-SHOT EXPLORATION
;; ============================================================================

;; Explore multiple angles on a topic
(define (explore-angles topic)
  (let ((angles (list "implementation" "usage" "testing" "documentation")))
    (map (lambda (angle)
           (let* ((query (string-append topic " " angle))
                  (results (search-code query))
                  (insight (effect infer.op
                             (list "What insight about " topic
                                   " do we get from " angle " perspective?\n"
                                   results))))
             (learn! insight (string-append angle "-analysis"))
             (cons angle insight)))
         angles)))

;; ============================================================================
;; REPORT GENERATION
;; ============================================================================

(define (generate-report topic)
  (let ((findings (research-topic topic)))
    (string-append
      "# Research Report: " topic "\n\n"
      "## Files Found\n" (number->string (cdr (assoc 'files-found findings))) " files\n\n"
      "## Code References\n```\n" (cdr (assoc 'code-refs findings)) "\n```\n\n"
      "## Synthesis\n" (cdr (assoc 'synthesis findings)) "\n\n"
      "## Sources\n" (string-join sources "\n"))))

;; ============================================================================
;; USAGE
;; ============================================================================

;; Research a topic:
;; (research-topic "debugSession")

;; Deep dive into a question:
;; (deep-dive "How does the CESK machine handle effects?")

;; Generate report:
;; (effect file.write.op "research-report.md" (generate-report "effects"))
