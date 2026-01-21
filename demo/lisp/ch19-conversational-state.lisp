;; Chapter 19: Conversational State and Memory
;; Use prior turns as context for follow-up answers.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch19-conversational-state

(define history
  (list
    "User: I am worried about the outage timeline."
    "Assistant: I will keep you updated every hour with calm language."
    "User: Please avoid sounding scripted in the next update."))

(effect infer.op
  (list "Given this conversation, craft the next reply that remembers prior concerns: " history))
