;; Chapter 48: Budget Management
;; Adaptive strategies based on remaining token budget.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch48-budget-management

(define (adaptive-summarize documents remaining-budget)
  (cond
    ((> remaining-budget 50000)
      (map (lambda (doc)
             (effect infer.op (list "Detailed summary: " doc)))
           documents))
    ((> remaining-budget 10000)
      (map (lambda (doc)
             (effect infer.op (list "Brief summary (50 words): " doc)))
           documents))
    (else
      (map (lambda (doc)
             (effect infer.op (list "Title only: " doc)))
           documents))))

(adaptive-summarize (list "Doc1" "Doc2" "Doc3") 15000)
