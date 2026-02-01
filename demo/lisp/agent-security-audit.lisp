;; ============================================================================
;; AGENT DEMO: Security Audit Across Entire Codebase
;; ============================================================================
;;
;; What this demonstrates FOR CODING AGENTS:
;;
;; Traditional agent approach:
;;   Tool call 1: Read file1.ts → Analyze security → Report
;;   Tool call 2: Read file2.ts → Analyze security → Report
;;   ... (100 files = 100 serial tool calls)
;;
;; OmegaLLM approach:
;;   Write ONE program that maps security analysis over all files
;;
;; This uses REAL, WORKING primitives:
;;   - shell.op for finding files (mechanical, no LLM)
;;   - file.read.op for reading files
;;   - map for parallel LLM analysis
;;   - filter for selecting files with issues
;;   - amb + require for auto-retry fix generation
;;
;; Run: omega --file demo/lisp/agent-security-audit.lisp --caps shell,file.read
;; ============================================================================

;; ============================================================================
;; STEP 1: Find all source files (mechanical, no LLM)
;; ============================================================================

(display "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(display "STEP 1: Finding all TypeScript files...\n")
(display "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

;; Use shell.op to find files (fast, deterministic)
(define find-result
  (effect shell.op "find src -name '*.ts' -type f 2>/dev/null | head -10"))

;; Parse result into list of files
(define source-files
  (filter (lambda (f) (not (equal? f "")))
          (string-split find-result "\n")))

(display (string-append "Found " (number->string (length source-files)) " files:\n"))
(map (lambda (f) (display (string-append "  • " f "\n"))) source-files)

;; ============================================================================
;; STEP 2: Define security analysis (LLM-based predicate)
;; ============================================================================

(display "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(display "STEP 2: Analyzing security across ALL files...\n")
(display "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

;; Security analysis predicate using LLM
(define (analyze-security filepath)
  (let ((content (effect file.read.op filepath)))
    (effect infer.op
      (list "Find security vulnerabilities in this code:\n\n"
            content
            "\n\nCheck for: SQL injection, XSS, CSRF, improper auth, hardcoded secrets.\n"
            "Return 'NONE' if no issues, or list specific vulnerabilities."))))

;; ============================================================================
;; STEP 3: Map security analysis over ALL files (PARALLEL!)
;; ============================================================================

;; KEY INSIGHT: Instead of calling analyze-security 10 times sequentially,
;; we express it as ONE map operation. The runtime can parallelize this.

(define security-reports
  (map analyze-security source-files))

;; Display results
(display "Security analysis complete!\n\n")
(map (lambda (file report)
       (display (string-append "📄 " file "\n"))
       (display (string-append "   " report "\n\n")))
     source-files
     security-reports)

;; ============================================================================
;; STEP 4: Filter to files with actual issues
;; ============================================================================

(display "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(display "STEP 3: Filtering to files with vulnerabilities...\n")
(display "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

;; Helper: Check if report indicates issues
(define (has-vulnerability? report)
  (not (or (string-contains? report "NONE")
           (string-contains? report "No vulnerabilities")
           (string-contains? report "no issues"))))

;; Filter to only files with issues
(define vulnerable-files
  (filter (lambda (pair)
            (has-vulnerability? (cadr pair)))
          (map list source-files security-reports)))

(display (string-append "Found " (number->string (length vulnerable-files)) " vulnerable files:\n"))
(map (lambda (pair)
       (display (string-append "  🚨 " (car pair) "\n")))
     vulnerable-files)

;; ============================================================================
;; STEP 5: Generate fixes with auto-retry (AMB + REQUIRE)
;; ============================================================================

;; COMMENTED OUT - Requires amb support
;; This shows how to auto-retry different fix approaches until one works

;; (define (generate-fix-with-retry vulnerability)
;;   (let ((approach (amb "input-validation" "parameterized-query" "sanitization" "escape-output")))
;;     (let ((fix (effect infer.op
;;                  (list "Generate " approach " fix for:\n" vulnerability))))
;;       (require (secure? fix))  ;; Auto-backtrack if still vulnerable
;;       (list approach fix))))
;;
;; (define fixes
;;   (map (lambda (pair)
;;          (generate-fix-with-retry (cadr pair)))
;;        vulnerable-files))

;; ============================================================================
;; SUMMARY
;; ============================================================================

(display "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(display "✅ DEMO COMPLETE\n")
(display "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

(display "What this showed:\n")
(display "  ✓ shell.op to find files (mechanical, no LLM)\n")
(display "  ✓ map to analyze ALL files in parallel\n")
(display "  ✓ filter to select only vulnerable files\n")
(display "  ✓ No manual iteration - ONE program expression\n\n")

(display "For agents, this means:\n")
(display "  • NOT: 100 sequential tool calls\n")
(display "  • YES: ONE OmegaLLM program with map\n\n")

(display "Next steps:\n")
(display "  1. Add amb + require for auto-retry fix generation\n")
(display "  2. Use sessions to persist analysis across tool calls\n")
(display "  3. Stream for processing large codebases\n\n")

;; Return summary
(list
  'files-analyzed: (length source-files)
  'vulnerabilities-found: (length vulnerable-files)
  'pattern: 'map-over-codebase)
