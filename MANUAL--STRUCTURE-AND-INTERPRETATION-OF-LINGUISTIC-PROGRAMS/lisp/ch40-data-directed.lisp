;; Chapter 40: Data-Directed Evaluation
;; Pattern: LLM synthesizes SPECS for handlers (not executable code)
;; Safe: Returns specification string, not code to eval

;; Helper: Generate handler specification for unknown operation
(define (synthesize-handler-spec operation)
  (effect infer.op (list "Describe what a handler for '" operation "' should do. "
                         "Return a brief specification string, not code.")))

;; Helper: Check if spec is non-empty
(define (valid-spec? spec)
  (and (string? spec)
       (> (string-length spec) 10)))

;; Data-directed dispatch: synthesize spec for unknown operation
;; Safe: Returns SPEC string describing handler, not executable code
(let ((operation "extract-entities"))
  (let ((spec (synthesize-handler-spec operation)))
    (valid-spec? spec)))
