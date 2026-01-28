;; usecase-workflow-orchestrator.lisp
;;
;; WORKFLOW ORCHESTRATOR - coordinate multi-step tasks with state
;; Like a CI/CD pipeline but with LLM decision-making at each step
;;
;; Run: omega --file demo/lisp/usecase-workflow-orchestrator.lisp --caps shell,file.read,file.write,infer

;; ============================================================================
;; WORKFLOW STATE
;; ============================================================================

(define workflow-state
  (list
    (cons 'status 'pending)
    (cons 'current-step 0)
    (cons 'steps-completed '())
    (cons 'errors '())
    (cons 'outputs '())))

(define (set-state! key value)
  (set! workflow-state
    (map (lambda (pair)
           (if (equal? (car pair) key)
               (cons key value)
               pair))
         workflow-state)))

(define (get-state key)
  (cdr (assoc key workflow-state)))

(define (add-output! name value)
  (set-state! 'outputs (cons (cons name value) (get-state 'outputs))))

(define (add-error! msg)
  (set-state! 'errors (cons msg (get-state 'errors))))

;; ============================================================================
;; STEP DEFINITIONS
;; ============================================================================

;; A step is: (name, condition-fn, action-fn)
(define (make-step name condition action)
  (list name condition action))

(define (step-name step) (car step))
(define (step-condition step) (cadr step))
(define (step-action step) (caddr step))

;; ============================================================================
;; BUILT-IN STEP ACTIONS
;; ============================================================================

;; Run shell command
(define (shell-step cmd)
  (lambda ()
    (let ((result (effect shell.op cmd)))
      (add-output! 'shell-output result)
      (if (string-contains? result "error")
          (begin (add-error! result) #f)
          #t))))

;; Run tests
(define (test-step)
  (lambda ()
    (let ((result (effect shell.op "npm test 2>&1 || true")))
      (add-output! 'test-output result)
      (not (string-contains? result "FAIL")))))

;; Build step
(define (build-step)
  (lambda ()
    (let ((result (effect shell.op "npm run build 2>&1 || true")))
      (add-output! 'build-output result)
      (not (string-contains? result "error")))))

;; LLM decision step
(define (llm-decision-step question options)
  (lambda ()
    (let ((decision (effect infer.op
                      (list question "\n\nOptions: " (string-join options ", ")
                            "\n\nBased on the workflow context:\n"
                            (format-context)
                            "\n\nChoose one option. Respond with just the option."))))
      (add-output! 'llm-decision decision)
      (member decision options))))

;; File check step
(define (file-exists-step filepath)
  (lambda ()
    (let ((result (effect shell.op (string-append "test -f " filepath " && echo yes || echo no"))))
      (equal? (string-trim result) "yes"))))

;; LLM validation step
(define (llm-validate-step validation-prompt)
  (lambda ()
    (let ((result (effect infer.op
                    (list validation-prompt "\n\nContext:\n" (format-context)
                          "\n\nRespond with PASS or FAIL."))))
      (add-output! 'validation result)
      (string-contains? result "PASS"))))

;; ============================================================================
;; WORKFLOW EXECUTION
;; ============================================================================

(define (format-context)
  (string-append
    "Status: " (symbol->string (get-state 'status)) "\n"
    "Steps completed: " (number->string (length (get-state 'steps-completed))) "\n"
    "Errors: " (number->string (length (get-state 'errors))) "\n"
    "Outputs: " (number->string (length (get-state 'outputs)))))

;; Run a single step
(define (run-step step)
  (let* ((name (step-name step))
         (condition (step-condition step))
         (action (step-action step)))
    (if (condition)
        (let ((result (action)))
          (if result
              (begin
                (set-state! 'steps-completed
                  (cons name (get-state 'steps-completed)))
                (list 'success name))
              (begin
                (add-error! (string-append "Step failed: " (symbol->string name)))
                (list 'failed name))))
        (list 'skipped name))))

;; Run entire workflow
(define (run-workflow steps)
  (set-state! 'status 'running)
  (define (run-remaining remaining-steps)
    (if (null? remaining-steps)
        (begin
          (set-state! 'status 'completed)
          (list 'workflow-completed (get-state 'steps-completed)))
        (let ((result (run-step (car remaining-steps))))
          (if (equal? (car result) 'failed)
              (begin
                (set-state! 'status 'failed)
                (list 'workflow-failed (cadr result) (get-state 'errors)))
              (run-remaining (cdr remaining-steps))))))
  (run-remaining steps))

;; ============================================================================
;; EXAMPLE WORKFLOWS
;; ============================================================================

;; CI/CD Pipeline
(define ci-pipeline
  (list
    (make-step 'lint
      (lambda () #t)
      (shell-step "npm run lint 2>&1 || true"))

    (make-step 'test
      (lambda () #t)
      (test-step))

    (make-step 'build
      (lambda () (null? (get-state 'errors)))
      (build-step))

    (make-step 'quality-check
      (lambda () (null? (get-state 'errors)))
      (llm-validate-step "Review the test and build output. Is the code quality acceptable for deployment?"))

    (make-step 'deploy-decision
      (lambda () (null? (get-state 'errors)))
      (llm-decision-step "Should we deploy this build?" (list "deploy-prod" "deploy-staging" "hold")))))

;; Code Review Pipeline
(define review-pipeline
  (list
    (make-step 'get-changes
      (lambda () #t)
      (shell-step "git diff --stat HEAD~1"))

    (make-step 'security-review
      (lambda () #t)
      (llm-validate-step "Review the git diff for security issues. Any concerns?"))

    (make-step 'approve-or-request-changes
      (lambda () #t)
      (llm-decision-step "Based on the review, what's the verdict?"
                         (list "approve" "request-changes" "needs-discussion")))))

;; ============================================================================
;; RECOVERY & RETRY
;; ============================================================================

;; Retry a failed step with LLM guidance
(define (retry-with-guidance failed-step)
  (let* ((error-context (string-join (get-state 'errors) "\n"))
         (fix-suggestion (effect infer.op
                           (list "The step '" (symbol->string failed-step) "' failed with:\n"
                                 error-context
                                 "\n\nSuggest a fix or workaround."))))
    (add-output! 'fix-suggestion fix-suggestion)
    fix-suggestion))

;; ============================================================================
;; USAGE
;; ============================================================================

;; Run CI pipeline:
;; (run-workflow ci-pipeline)

;; Run code review:
;; (run-workflow review-pipeline)

;; After failure, get guidance:
;; (retry-with-guidance 'test)
