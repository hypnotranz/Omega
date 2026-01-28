;; usecase-coding-agent.lisp
;;
;; A CODING AGENT that can read, analyze, modify code, and run tests
;; ALL FROM WITHIN A LISP PROGRAM with full state management.
;;
;; This demonstrates OmegaLLM's unique power: embedding an LLM coding agent
;; inside a program that maintains state, loops, and makes decisions.
;;
;; Run: omega --file demo/lisp/usecase-coding-agent.lisp --caps shell,file.read,file.write,infer

;; ============================================================================
;; THE CODING AGENT
;; ============================================================================

;; Agent state - tracks what we've done
(define agent-state
  (list
    (cons 'files-read '())
    (cons 'changes-made '())
    (cons 'tests-run 0)
    (cons 'errors '())))

;; Update state helper
(define (update-state! key value)
  (set! agent-state
    (map (lambda (pair)
           (if (equal? (car pair) key)
               (cons key value)
               pair))
         agent-state)))

(define (get-state key)
  (let ((pair (assoc key agent-state)))
    (if pair (cdr pair) #f)))

;; ============================================================================
;; FILE OPERATIONS
;; ============================================================================

;; Read a file and track it
(define (read-file path)
  (let ((content (effect file.read.op path)))
    (update-state! 'files-read (cons path (get-state 'files-read)))
    content))

;; Write a file and track the change
(define (write-file path content reason)
  (effect file.write.op path content)
  (update-state! 'changes-made
    (cons (list path reason) (get-state 'changes-made)))
  (string-append "Wrote " path))

;; ============================================================================
;; CODE ANALYSIS (LLM-powered)
;; ============================================================================

;; Ask LLM to analyze code
(define (analyze-code code question)
  (effect infer.op
    (list "Given this code:\n```\n" code "\n```\n\n" question)))

;; Find bugs in code
(define (find-bugs code)
  (analyze-code code "List any bugs, issues, or improvements. Be specific with line references."))

;; Suggest a fix
(define (suggest-fix code issue)
  (effect infer.op
    (list "Given this code:\n```\n" code "\n```\n\nFix this issue: " issue
          "\n\nReturn ONLY the fixed code, no explanation.")))

;; ============================================================================
;; SHELL OPERATIONS
;; ============================================================================

;; Run tests and parse results
(define (run-tests)
  (update-state! 'tests-run (+ 1 (get-state 'tests-run)))
  (effect shell.op "npm test 2>&1 || true"))

;; Run a specific test file
(define (run-test-file path)
  (effect shell.op (string-append "npx jest " path " 2>&1 || true")))

;; Get git status
(define (git-status)
  (effect shell.op "git status --short"))

;; Find files matching a pattern
(define (find-files pattern)
  (let ((result (effect shell.op (string-append "find . -name '" pattern "' -type f 2>/dev/null"))))
    (filter (lambda (f) (not (equal? f "")))
            (string-split result "\n"))))

;; ============================================================================
;; DECISION MAKING (LLM-powered)
;; ============================================================================

;; Decide next action based on context
(define (decide-next-action context)
  (effect infer.op
    (list "You are a coding agent. Based on this context:\n" context
          "\n\nWhat should be the next action? Choose one:\n"
          "- READ <filepath> - read a file\n"
          "- ANALYZE <filepath> - analyze code for issues\n"
          "- FIX <filepath> <issue> - fix an issue\n"
          "- TEST - run tests\n"
          "- DONE - task complete\n\n"
          "Respond with just the action.")))

;; ============================================================================
;; THE AGENT LOOP
;; ============================================================================

;; Main agent loop - continues until done or max iterations
(define (agent-loop task max-iterations)
  (define (loop iteration context)
    (if (>= iteration max-iterations)
        (list 'max-iterations-reached context)
        (let ((action (decide-next-action context)))
          (cond
            ((string-contains? action "DONE")
             (list 'completed (get-state 'changes-made)))

            ((string-contains? action "READ")
             (let* ((filepath (extract-filepath action))
                    (content (read-file filepath))
                    (new-context (string-append context "\n\nRead " filepath ":\n" (substring content 0 500) "...")))
               (loop (+ iteration 1) new-context)))

            ((string-contains? action "ANALYZE")
             (let* ((filepath (extract-filepath action))
                    (content (read-file filepath))
                    (analysis (find-bugs content))
                    (new-context (string-append context "\n\nAnalysis of " filepath ":\n" analysis)))
               (loop (+ iteration 1) new-context)))

            ((string-contains? action "TEST")
             (let* ((test-output (run-tests))
                    (new-context (string-append context "\n\nTest results:\n" test-output)))
               (loop (+ iteration 1) new-context)))

            (else
             (loop (+ iteration 1) (string-append context "\n\nUnknown action: " action)))))))

  (loop 0 (string-append "Task: " task "\n\nAgent state: " (format-state))))

;; Helper to extract filepath from action
(define (extract-filepath action)
  (let ((parts (string-split action " ")))
    (if (> (length parts) 1)
        (cadr parts)
        "unknown")))

;; Format state for context
(define (format-state)
  (string-append
    "Files read: " (number->string (length (get-state 'files-read)))
    ", Changes: " (number->string (length (get-state 'changes-made)))
    ", Test runs: " (number->string (get-state 'tests-run))))

;; ============================================================================
;; EXAMPLE USAGE
;; ============================================================================

;; Run the agent on a task
;; (agent-loop "Find and fix any type errors in src/utils.ts" 10)

;; Or run specific operations:
;; (analyze-code (read-file "src/index.ts") "What does this module export?")
;; (run-tests)
;; (git-status)
