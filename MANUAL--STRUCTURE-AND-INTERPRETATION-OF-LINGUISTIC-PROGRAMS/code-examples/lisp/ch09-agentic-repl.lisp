;; Chapter 9: Agentic REPL
;; LLM asks the runtime for facts before replying.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch09-agentic-repl

(define active-tickets (list "Auth outage" "Export stalled" "Payment retry loop" "Stale cache"))
(define ask-runtime (oracle-lambda (question) "agentic-query"))
(ask-runtime "How many urgent tickets are active? Call (length active-tickets) before answering.")
