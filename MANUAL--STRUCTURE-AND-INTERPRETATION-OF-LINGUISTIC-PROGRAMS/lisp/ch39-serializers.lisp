;; Chapter 39: Serializers For Concurrent LLM Calls
;; Parallel document processing with shared glossary.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch39-serializers

(define glossary (make-table))

(define (add-term! term definition)
  (insert! term definition glossary))

(define (process-document doc serializer)
  (serializer
    (lambda ()
      (let ((terms (effect infer.op (list "Extract key terms: " doc))))
        (add-term! terms doc)))))

;; Multiple documents processed concurrently, glossary updates serialized
(parallel-map (lambda (doc) (process-document doc glossary-serializer))
              (list "Doc A about AI" "Doc B about ML" "Doc C about NLP"))
