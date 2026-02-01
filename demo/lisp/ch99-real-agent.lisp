;; ============================================================================
;; REAL LLM-POWERED CODE REVIEW AGENT
;; Actually uses effect infer.op to make LLM calls
;; This is the REAL DEAL - not simulation!
;; ============================================================================

;; ============================================================================
;; CODEBASE TO REVIEW
;; ============================================================================

(define code-samples (list
  (list "auth.ts"
        "function login(user, pass) { if(user=='admin'&&pass=='123') return true; }")

  (list "database.ts"
        "const query = 'SELECT * FROM users WHERE id=' + userId; db.exec(query);")

  (list "api.ts"
        "async function handle(req) { try { return await process(req); } catch(e) { } }")))

(define (file-name pair) (car pair))
(define (file-code pair) (cadr pair))

;; ============================================================================
;; AGENT 1: SECURITY REVIEWER (Real LLM!)
;; ============================================================================

;; Use LLM to detect security issues
(define (agent-security-review code)
  (effect infer.op
    (list "Review this code for security vulnerabilities. "
          "List any issues found, or say 'SECURE' if none: "
          code)))

;; Run security review on all files (REAL LLM CALLS!)
(define security-results
  (map (lambda (file)
         (list (file-name file)
               (agent-security-review (file-code file))))
       code-samples))

;; ============================================================================
;; AGENT 2: QUALITY CHECKER (Real LLM!)
;; ============================================================================

;; Use LLM to assess code quality
(define (agent-quality-check code)
  (effect infer.op
    (list "Rate this code's quality on a scale of 1-10. "
          "Consider: error handling, naming, readability. "
          "Respond with just the number: "
          code)))

;; Run quality checks (REAL LLM CALLS!)
(define quality-scores
  (map (lambda (file)
         (list (file-name file)
               (agent-quality-check (file-code file))))
       code-samples))

;; ============================================================================
;; AGENT 3: REFACTORING SUGGESTER (Real LLM!)
;; ============================================================================

;; Use LLM to suggest improvements
(define (agent-suggest-refactor code)
  (effect infer.op
    (list "Suggest ONE concrete refactoring for this code. "
          "Be specific and brief: "
          code)))

;; Get refactoring suggestions (REAL LLM CALLS!)
(define refactor-suggestions
  (map (lambda (file)
         (list (file-name file)
               (agent-suggest-refactor (file-code file))))
       code-samples))

;; ============================================================================
;; AGENT 4: COORDINATOR (Real LLM aggregation!)
;; ============================================================================

;; Use LLM to synthesize all findings into a report
(define (agent-synthesize-report security quality refactors)
  (effect infer.op
    (list "Create a brief executive summary from these code review findings. "
          "Security issues: " security " "
          "Quality scores: " quality " "
          "Refactoring suggestions: " refactors " "
          "Format: 2-3 sentences highlighting critical items.")))

;; Generate final report (REAL LLM CALL!)
(define executive-summary
  (agent-synthesize-report
    security-results
    quality-scores
    refactor-suggestions))

;; ============================================================================
;; RESULTS
;; ============================================================================

(list
  (list "security-review" security-results)
  (list "quality-scores" quality-scores)
  (list "refactoring-suggestions" refactor-suggestions)
  (list "executive-summary" executive-summary))
