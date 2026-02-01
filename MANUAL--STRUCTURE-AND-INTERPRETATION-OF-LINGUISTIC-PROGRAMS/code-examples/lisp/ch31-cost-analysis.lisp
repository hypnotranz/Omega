;; Chapter 31: Orders of Growth - Semantic Cost Analysis
;; Demonstrates analyzing token costs as complexity measure

;; ============================================================
;; 31.3: Linear vs. Quadratic Costs
;; ============================================================

(display "31.3: Comparing O(n) vs O(n²) approaches")
(newline)

;; O(n²) approach - compare all pairs
(define (analyze-all-pairs items)
  (display "  Pairwise comparison (O(n²)):")
  (newline)
  (let ((call-count 0))
    (map (lambda (item1)
           (map (lambda (item2)
                  (begin
                    (set! call-count (+ call-count 1))
                    (effect infer.op
                      (list "Compare: " item1 " vs " item2))))
                items))
         items)
    (display (list "    LLM calls:" call-count))
    (newline)))

;; O(n) approach - analyze each then summarize
(define (analyze-with-summary items)
  (display "  Batch with summary (O(n)):")
  (newline)
  (let* ((call-count 0)
         (analyses
          (map (lambda (item)
                 (begin
                   (set! call-count (+ call-count 1))
                   (effect infer.op (list "Analyze: " item))))
               items)))
    (begin
      (set! call-count (+ call-count 1))
      (let ((summary
             (effect infer.op
               (list "Summarize these analyses: " analyses))))
        (begin
          (display (list "    LLM calls:" call-count))
          (newline)
          summary)))))

;; Test with small dataset
(define test-items (list "item1" "item2" "item3"))

(display "With 3 items:")
(newline)
(analyze-all-pairs test-items)  ; Should show 9 calls
(analyze-with-summary test-items)  ; Should show 4 calls
(newline)

;; ============================================================
;; 31.5: Optimizing from O(n²) to O(n)
;; ============================================================

(display "31.5: Clustering optimization")
(newline)

;; Naive O(n²) clustering
(define (cluster-naive docs)
  (display "  Naive pairwise (O(n²)):")
  (newline)
  (let ((call-count 0))
    (map (lambda (doc1)
           (map (lambda (doc2)
                  (begin
                    (set! call-count (+ call-count 1))
                    (effect infer.op
                      (list "Similarity: " doc1 " vs " doc2))))
                docs))
         docs)
    (display (list "    Calls:" call-count))
    (newline)))

;; Optimized O(n) clustering
(define (cluster-hierarchical docs)
  (display "  Hierarchical (O(n)):")
  (newline)
  (let* ((call-count (length docs))
         (summaries
          (map (lambda (doc)
                 (effect infer.op (list "Summarize themes: " doc)))
               docs)))
    (begin
      (set! call-count (+ call-count 1))
      (let ((clusters
             (effect infer.op
               (list "Group these into clusters: " summaries))))
        (begin
          (display (list "    Calls:" call-count))
          (newline)
          clusters)))))

(define test-docs (list "doc1" "doc2" "doc3" "doc4"))

(display "Clustering 4 documents:")
(newline)
(cluster-naive test-docs)  ; 16 calls
(cluster-hierarchical test-docs)  ; 5 calls
(newline)

;; ============================================================
;; 31.7: Sentiment Analysis - Different Strategies
;; ============================================================

(display "31.7: Sentiment analysis strategies")
(newline)

;; Strategy 1: Analyze each review (O(n))
(define (sentiment-individual reviews)
  (display "  Individual analysis (O(n)):")
  (newline)
  (let ((results
         (map (lambda (review)
                (effect infer.op
                  (list "Sentiment (positive/negative/neutral): " review)))
              reviews)))
    (begin
      (display (list "    Calls:" (length reviews)))
      (newline)
      results)))

;; Strategy 2: Sample and infer (O(k) where k << n)
(define (take n lst)
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

(define (sentiment-sample reviews sample-size)
  (display "  Sample-based (O(k)):")
  (newline)
  (let* ((sample (take sample-size reviews))
         (sentiments
          (map (lambda (review)
                 (effect infer.op
                   (list "Sentiment: " review)))
               sample))
         (overall
          (effect infer.op
            (list "Overall sentiment from sample: " sentiments))))
    (begin
      (display (list "    Calls:" (+ sample-size 1)))
      (newline)
      overall)))

(define test-reviews
  (list "Great product!" "Terrible experience" "It's okay"
        "Love it!" "Hate it" "Neutral feelings"
        "Best ever" "Worst ever" "Fine" "Good"))

(display "Analyzing 10 reviews:")
(newline)
(sentiment-individual test-reviews)  ; 10 calls
(sentiment-sample test-reviews 3)  ; 4 calls (sample of 3 + 1 summary)
(newline)

;; ============================================================
;; 31.9: Asymptotic Scaling Demonstration
;; ============================================================

(display "31.9: Growth rates at different scales")
(newline)

(define (count-calls-for-size n approach)
  (cond
    ((equal? approach "O(n)") n)
    ((equal? approach "O(n-log-n)") (* n (ceiling (/ (log n) (log 2)))))
    ((equal? approach "O(n^2)") (* n n))
    (else 0)))

(define (show-scaling n)
  (begin
    (display (list "n=" n ":"))
    (newline)
    (display (list "  O(n):" (count-calls-for-size n "O(n)")))
    (newline)
    (display (list "  O(n log n):" (count-calls-for-size n "O(n-log-n)")))
    (newline)
    (display (list "  O(n²):" (count-calls-for-size n "O(n^2)")))
    (newline)))

(show-scaling 10)
(show-scaling 100)
(show-scaling 1000)
(newline)

;; ============================================================
;; 31.11: Semantic Caching for Amortized Cost
;; ============================================================

(display "31.11: Caching for amortized O(1/k) cost")
(newline)

(define cache (make-hash))
(define cache-hits 0)
(define cache-misses 0)

(define (cached-infer prompt)
  (let ((cached-result (hash-ref cache prompt #f)))
    (if cached-result
        (begin
          (set! cache-hits (+ cache-hits 1))
          (display "  [Cache HIT]")
          (newline)
          cached-result)
        (begin
          (set! cache-misses (+ cache-misses 1))
          (display "  [Cache MISS - calling LLM]")
          (newline)
          (let ((result (effect infer.op prompt)))
            (hash-set! cache prompt result)
            result)))))

;; Call same prompt multiple times
(display "Calling same prompt 3 times:")
(newline)
(cached-infer "What is 2+2?")
(cached-infer "What is 2+2?")
(cached-infer "What is 2+2?")

(display (list "Cache stats - Hits:" cache-hits "Misses:" cache-misses))
(newline)
(display "Amortized cost: 1 LLM call / 3 invocations = 0.33 calls per invocation")
(newline)
(newline)

;; ============================================================
;; Demonstration Complete
;; ============================================================

(display "Chapter 31 demonstration complete!")
(newline)
(display "Key insight: Token costs drive algorithmic decisions - O(n²) is prohibitive at scale")
(newline)
