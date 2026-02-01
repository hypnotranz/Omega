;; Chapter 37: Mutable Queues And Tables
;; Conversation history as FIFO queue.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch37-mutable-queues

(define history (make-queue))

(enqueue! history "User: What's the refund policy?")
(enqueue! history "Bot: Refunds within 30 days.")
(enqueue! history "User: What about damaged items?")

(effect infer.op
  (list "Based on conversation history, answer: "
        (queue->list history)
        " Latest: What about damaged items?"))
