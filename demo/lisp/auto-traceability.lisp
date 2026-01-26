;; auto-traceability.lisp
;;
;; PURE MECHANICAL traceability matrix generation - NO LLM REQUIRED
;;
;; This demonstrates:
;; - shell.op with grep for regex extraction (what TypeScript uses regex for)
;; - file.read.op for file access
;; - file.write.op for output
;; - Streams for lazy iteration over large codebases
;; - map/filter for transformation
;;
;; Key insight: Everything TypeScript does, OmegaLLM can do with shell.op
;; The only thing we NEED TypeScript (or another host) for is BOOTSTRAPPING.
;;
;; Run: omega --file demo/lisp/auto-traceability.lisp --caps shell,file.read,file.write

;; ============================================================================
;; CONFIGURATION
;; ============================================================================

(define source-files
  (list "src/core/prims.ts"
        "src/core/conditions/prims.ts"
        "src/core/provenance/prims.ts"
        "src/core/solver/prims.ts"))

;; ============================================================================
;; MECHANICAL EXTRACTION (no LLM)
;; ============================================================================

;; Extract primitive names from a file using grep
;; Returns raw grep output: each line is like:  123:  def("name",
;; Note: Uses double quotes for Windows compatibility
(define (grep-primitives filepath)
  (effect shell.op
    (string-append "grep -n \"def(\" " filepath " 2>/dev/null || true")))

;; Parse one grep line into (name line-num)
;; Input:  "123:  def(\"car\","
;; Output: ("car" 123)
(define (parse-grep-line line)
  (if (string-contains? line "def(")
      (let* ((parts (string-split line ":"))
             (line-num (car parts))
             (rest (string-join (cdr parts) ":"))
             ;; Extract name between def(" and next "
             (after-def (if (string-contains? rest "def(\"")
                           (cadr (string-split rest "def(\""))
                           #f)))
        (if after-def
            (list (car (string-split after-def "\"")) (string->number line-num))
            #f))
      #f))  ;; Not a valid line

;; string->number is now a native primitive - no need to define it here

;; Extract all primitives from a file -> list of (name line-num)
(define (extract-primitives filepath)
  (let* ((grep-output (grep-primitives filepath))
         (lines (string-split grep-output "\n")))
    (filter identity
      (map parse-grep-line
           (filter (lambda (l) (not (equal? l ""))) lines)))))

;; ============================================================================
;; TEST FILE DISCOVERY (mechanical)
;; ============================================================================

;; Find all test files
(define (find-all-test-files)
  (let ((result (effect shell.op "find test -name '*.spec.ts' 2>/dev/null")))
    (filter (lambda (f) (not (equal? f "")))
            (string-split result "\n"))))

;; Check if a test file mentions a primitive name
(define (test-file-mentions? test-file prim-name)
  (let ((result (effect shell.op
                  (string-append "grep -l '" prim-name "' " test-file " 2>/dev/null || true"))))
    (not (equal? (string-trim result) ""))))

;; Find test files for a primitive
(define (find-tests-for-primitive prim-name test-files)
  (filter (lambda (tf) (test-file-mentions? tf prim-name)) test-files))

;; ============================================================================
;; MARKDOWN GENERATION (mechanical)
;; ============================================================================

(define (format-row prim-info filepath test-files)
  (let* ((name (car prim-info))
         (line (cadr prim-info))
         (tests (find-tests-for-primitive name test-files))
         (test-str (if (null? tests)
                       "❌"
                       (string-join (map (lambda (t) (string-replace-all t "test/" ""))
                                         tests) ", "))))
    (string-append "| `" name "` | " filepath ":" (if (number? line) (number->string line) line) " | " test-str " |")))

;; number->string is now a native primitive - no need to define it here

;; ============================================================================
;; STREAM-BASED PROCESSING (lazy - handles large codebases)
;; ============================================================================

;; Create stream of all primitives with their source files
(define (primitives-stream files)
  (if (null? files)
      the-empty-stream
      (let* ((file (car files))
             (rest (cdr files))
             (prims (extract-primitives file)))
        (stream-append
          (list->stream (map (lambda (p) (cons p file)) prims))
          (primitives-stream rest)))))

;; ============================================================================
;; MAIN ENTRY POINTS
;; ============================================================================

;; Helper: process files recursively
(define (process-files-helper files acc test-files)
  (if (null? files)
      acc
      (let* ((file (car files))
             (prims (extract-primitives file))
             (rows (map (lambda (p) (format-row p file test-files)) prims)))
        (process-files-helper (cdr files) (append acc rows) test-files))))

;; Generate full matrix (eager - loads all into memory)
(define (generate-traceability-matrix output-file)
  (let* ((test-files (find-all-test-files))
         (header "# OmegaLLM Traceability Matrix\n\n> Generated by auto-traceability.lisp (pure OmegaLLM, no external tools)\n\n| Primitive | Location | Tests |\n|-----------|----------|-------|\n")
         (all-rows (process-files-helper source-files '() test-files)))
    (effect file.write.op output-file
      (string-append header (string-join all-rows "\n")))
    (string-append "Written " (number->string (length all-rows)) " primitives to " output-file)))

;; Helper: process stream recursively
(define (process-stream-helper s count output-file test-files)
  (if (stream-null? s)
      (string-append "Streamed " (number->string count) " primitives")
      (let* ((item (stream-car s))
             (prim-info (car item))
             (filepath (cdr item))
             (row (format-row prim-info filepath test-files)))
        ;; Append row to file
        (effect shell.op
          (string-append "echo '" row "' >> " output-file))
        (process-stream-helper (stream-cdr s) (+ count 1) output-file test-files))))

;; Streaming version (lazy - memory efficient for huge codebases)
(define (stream-traceability output-file)
  (let ((test-files (find-all-test-files)))
    ;; Write header
    (effect file.write.op output-file
      "# OmegaLLM Traceability Matrix (Streaming)\n\n| Primitive | Location | Tests |\n|-----------|----------|-------|\n")
    ;; Stream each primitive
    (process-stream-helper (primitives-stream source-files) 0 output-file test-files)))

;; ============================================================================
;; OPTIONAL: LLM-ENRICHED VERSION (adds descriptions)
;; ============================================================================

;; If you want descriptions, this adds LLM calls
(define (describe-primitive name)
  (effect infer.op
    (list "In one sentence, describe the Lisp primitive '" name "'.")))

(define (format-row-with-description prim-info filepath test-files)
  (let* ((name (car prim-info))
         (line (cadr prim-info))
         (tests (find-tests-for-primitive name test-files))
         (desc (describe-primitive name))
         (test-str (if (null? tests) "❌" (car tests))))
    (string-append "| `" name "` | " filepath ":" line " | " desc " | " test-str " |")))

;; ============================================================================
;; RUN
;; ============================================================================

;; Uncomment to run:
;; (generate-traceability-matrix "docs/TRACEABILITY-OMEGA.md")
;; (stream-traceability "docs/TRACEABILITY-OMEGA-STREAM.md")
