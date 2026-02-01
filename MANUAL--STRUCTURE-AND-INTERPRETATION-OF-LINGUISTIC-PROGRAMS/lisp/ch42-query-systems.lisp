;; Chapter 42: Query Systems With Semantic Facts
;; Conversational memory query system.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch42-query-systems

(define conversation-facts
  (list
    "User mentioned frustration with refund delays."
    "User praised customer support responsiveness."
    "User requested priority handling."))

(define (query-facts question facts)
  (effect infer.op
    (list "Answer based on facts. Question: " question " Facts: " facts)))

(query-facts "What is the user's overall sentiment?" conversation-facts)
