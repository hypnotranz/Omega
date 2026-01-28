;; usecase-code-review.lisp
;;
;; AUTOMATED CODE REVIEW that actually reads the code and runs checks
;; Demonstrates: file I/O + shell + LLM reasoning + state
;;
;; Run: omega --file demo/lisp/usecase-code-review.lisp --caps shell,file.read,infer

;; ============================================================================
;; CODE REVIEW SYSTEM
;; ============================================================================

;; Get changed files from git
(define (get-changed-files)
  (let ((output (effect shell.op "git diff --name-only HEAD~1 2>/dev/null || git diff --name-only")))
    (filter (lambda (f) (and (not (equal? f ""))
                             (or (string-contains? f ".ts")
                                 (string-contains? f ".js")
                                 (string-contains? f ".py"))))
            (string-split output "\n"))))

;; Get the diff for a specific file
(define (get-file-diff filepath)
  (effect shell.op (string-append "git diff HEAD~1 -- " filepath " 2>/dev/null || git diff -- " filepath)))

;; Read file content
(define (read-source filepath)
  (effect file.read.op filepath))

;; ============================================================================
;; REVIEW CHECKS (LLM-powered)
;; ============================================================================

;; Check for security issues
(define (check-security code filepath)
  (effect infer.op
    (list "Review this code for security vulnerabilities (injection, XSS, auth issues, secrets):\n"
          "File: " filepath "\n```\n" code "\n```\n\n"
          "List any security concerns. If none, say 'No security issues found.'")))

;; Check for performance issues
(define (check-performance code filepath)
  (effect infer.op
    (list "Review this code for performance issues (N+1 queries, memory leaks, inefficient loops):\n"
          "File: " filepath "\n```\n" code "\n```\n\n"
          "List any performance concerns. If none, say 'No performance issues found.'")))

;; Check for code style/best practices
(define (check-style code filepath)
  (effect infer.op
    (list "Review this code for style and best practices:\n"
          "File: " filepath "\n```\n" code "\n```\n\n"
          "Suggest improvements for readability, naming, or structure. Be concise.")))

;; Summarize the diff
(define (summarize-diff diff filepath)
  (effect infer.op
    (list "Summarize what changed in this diff in 2-3 sentences:\n"
          "File: " filepath "\n```diff\n" diff "\n```")))

;; ============================================================================
;; REVIEW ORCHESTRATION
;; ============================================================================

;; Review a single file
(define (review-file filepath)
  (let* ((content (read-source filepath))
         (diff (get-file-diff filepath))
         (summary (summarize-diff diff filepath))
         (security (check-security content filepath))
         (performance (check-performance content filepath))
         (style (check-style content filepath)))
    (list
      (cons 'file filepath)
      (cons 'summary summary)
      (cons 'security security)
      (cons 'performance performance)
      (cons 'style style))))

;; Review all changed files
(define (review-all-changes)
  (let ((files (get-changed-files)))
    (if (null? files)
        "No code files changed."
        (map review-file files))))

;; ============================================================================
;; OUTPUT FORMATTING
;; ============================================================================

(define (format-review review)
  (string-append
    "\n## " (cdr (assoc 'file review)) "\n\n"
    "### Summary\n" (cdr (assoc 'summary review)) "\n\n"
    "### Security\n" (cdr (assoc 'security review)) "\n\n"
    "### Performance\n" (cdr (assoc 'performance review)) "\n\n"
    "### Style\n" (cdr (assoc 'style review)) "\n"))

(define (format-all-reviews reviews)
  (string-append
    "# Code Review Report\n\n"
    (string-join (map format-review reviews) "\n---\n")))

;; ============================================================================
;; NONDETERMINISTIC REVIEW (with constraints)
;; ============================================================================

;; Sometimes we want to explore different review focuses
(define (focused-review filepath focus)
  (let* ((content (read-source filepath))
         (focus-type (amb "security" "performance" "maintainability" "testing")))
    (require (equal? focus-type focus))
    (effect infer.op
      (list "Review this code focusing ONLY on " focus-type ":\n"
            "File: " filepath "\n```\n" content "\n```"))))

;; ============================================================================
;; MAIN ENTRY POINT
;; ============================================================================

;; Run full review
;; (format-all-reviews (review-all-changes))

;; Or review specific file
;; (format-review (review-file "src/server/debugSession.ts"))
