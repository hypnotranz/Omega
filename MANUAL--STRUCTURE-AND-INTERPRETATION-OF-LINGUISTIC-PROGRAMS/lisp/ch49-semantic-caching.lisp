;; Chapter 49: Semantic Caching with Validation Gate
;; Pattern: Cache + LLM validation gate to check if cached result still valid

;; Helper: Simulate cache lookup
(define (get-cached-sentiment text)
  ;; Simulated cache: returns cached sentiment if available
  (if (string-contains? text "product")
      "positive"  ;; cached result
      #f))  ;; cache miss

;; Helper: Validation gate - LLM checks if cached result still valid
(define (validate-cached-result text cached-sentiment)
  (effect infer.op (list "Is sentiment '" cached-sentiment "' still valid for: " text
                         "? Answer yes/no")))

;; Helper: Convert yes/no to boolean
(define (yesno->bool s)
  (equal? s "yes"))

;; Semantic caching with validation
;; 1. Check cache
;; 2. If hit, use LLM to validate cached result is still accurate
;; 3. Return validation result
(let ((text "I love this product!"))
  (let ((cached (get-cached-sentiment text)))
    ;; Check if cache hit (not #f)
    (let ((hit? (not (equal? cached #f))))
      (if hit?
          (let ((validation (validate-cached-result text cached)))
            (yesno->bool validation))
          #f))))  ;; cache miss
