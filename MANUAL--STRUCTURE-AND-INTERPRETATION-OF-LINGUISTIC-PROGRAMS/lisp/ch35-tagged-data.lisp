;; Chapter 35: Tagged Data With Type Dispatch
;; Response strategies with tagged dispatch for chatbot responses.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch35-tagged-data

;; Constructor for tagged response
(define (tag-response strategy content)
  (list 'response-type strategy 'content content))

;; Classify user query → response strategy
(define (classify-query query)
  (effect infer.op
    (list "Classify intent: direct-answer / clarification / hedged-answer / refusal: " query)))

;; Dispatch based on tag
(define (handle-response response)
  (let ((tag (cadr (assoc 'response-type response)))
        (content (cadr (assoc 'content response))))
    (case tag
      ((direct-answer)
       (effect infer.op (list "State confidently: " content)))
      ((clarification)
       (effect infer.op (list "Ask for more details: " content)))
      ((hedged-answer)
       (effect infer.op (list "Answer with caveats: " content)))
      ((refusal)
       (effect infer.op (list "Politely decline: " content))))))

;; Full flow example with multiple queries
(define queries
  (list
    "What's the weather tomorrow?"
    "Tell me about quantum computing"
    "How do I hack into someone's account?"
    "What does photosynthesis mean?"))

(define (process-query query)
  (let* ((strategy (classify-query query))
         (content (effect infer.op (list "Generate response for: " query)))
         (response (tag-response (string->symbol strategy) content)))
    (list
      "QUERY:" query
      "STRATEGY:" strategy
      "FINAL:" (handle-response response))))

(map process-query queries)
