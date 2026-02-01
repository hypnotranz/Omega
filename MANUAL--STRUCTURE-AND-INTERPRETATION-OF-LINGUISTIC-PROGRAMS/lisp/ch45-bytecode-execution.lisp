;; Chapter 45: Bytecode Execution
;; Semantic bytecode VM with INFER instruction.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch45-bytecode-execution

;; High-level code
(define translate-fn
  (lambda (text)
    (effect infer.op (list "Translate to French: " text))))

;; Compiles to bytecode:
;; LOAD_VAR text
;; LOAD_CONST "Translate to French: "
;; CONCAT
;; INFER translate
;; RETURN

(translate-fn "Hello world")
