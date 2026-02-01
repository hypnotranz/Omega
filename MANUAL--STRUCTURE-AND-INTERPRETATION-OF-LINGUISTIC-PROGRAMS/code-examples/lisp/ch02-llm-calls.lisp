;; Chapter 2: LLM Calls as Functions
;; Call infer.op inside reusable procedures.
;;
;; `effect infer.op` invokes the oracle and returns the model's string answer
;; so you can treat it like any other value.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch02-llm-calls

(define (analyze-sentiment text)
  (effect infer.op
    (list "What is the sentiment (positive/negative/neutral) of: " text)))

(analyze-sentiment "I love how carefully you explained the migration steps.")
