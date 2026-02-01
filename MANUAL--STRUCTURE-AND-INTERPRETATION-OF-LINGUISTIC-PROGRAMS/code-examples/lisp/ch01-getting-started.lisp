;; Chapter 1: Getting Started
;; Warm-up REPL steps with simple definitions and evaluation.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch01-getting-started

(define greeting "Welcome to OmegaLLM. Describe what you want in everyday language.")
(define (echo text) text)
(echo greeting)
