# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 48: Budget Management for Token Costs
*OmegaLLM-Specific: Resource-Aware Execution*

### 48.1 Token Budgets

Every LLM call costs tokens. **Budget management** prevents cost overruns:

```lisp
(define (with-budget budget-tokens thunk)
  (let ((spent 0))
    (define (track-spend tokens)
      (set! spent (+ spent tokens))
      (if (> spent budget-tokens)
          (error "Budget exceeded")))

    (with-tracking track-spend thunk)))

Ω> (with-budget 10000  ; 10k token budget
     (lambda ()
       (expensive-operation)))
;; Throws error if exceeds budget
```

### 48.2 Cost-Aware Execution

Choose strategies based on remaining budget:

```lisp
(define (adaptive-summarize documents remaining-budget)
  (cond
    ((> remaining-budget 50000)
      ;; Plenty of budget: detailed summaries
      (map (lambda (doc)
             (effect infer.op (list "Detailed summary: " doc)))
           documents))

    ((> remaining-budget 10000)
      ;; Moderate budget: brief summaries
      (map (lambda (doc)
             (effect infer.op (list "Brief summary (50 words): " doc)))
           documents))

    (else
      ;; Low budget: titles only
      (map (lambda (doc)
             (effect infer.op (list "Title only: " doc)))
           documents))))
```

### 48.3 Budget Allocation

Distribute budget across subtasks:

```lisp
(define (allocate-budget total-budget tasks)
  (let* ((num-tasks (length tasks))
         (priority-weights (map get-priority tasks))
         (total-weight (sum priority-weights))
         (allocations
           (map (lambda (task weight)
                  (cons task (* total-budget (/ weight total-weight))))
                tasks
                priority-weights)))
    allocations))

;; Example
Ω> (allocate-budget 100000
     '((research priority: 3)
       (writing priority: 2)
       (editing priority: 1)))

=> ((research: 50000)   ; 3/6 of budget
    (writing: 33333)    ; 2/6 of budget
    (editing: 16667))   ; 1/6 of budget
```

### 48.4 Quality vs. Cost Trade-offs

```lisp
(define (generate-with-quality-target text quality-level budget)
  (case quality-level
    ((high)
      ;; Use expensive model, multiple passes
      (if (>= budget 5000)
          (multi-pass-refinement text 'gpt-4 3)
          (error "Insufficient budget for high quality")))

    ((medium)
      ;; Single pass with good model
      (if (>= budget 1000)
          (single-pass text 'gpt-4)
          (single-pass text 'gpt-3.5)))  ; Downgrade if needed

    ((low)
      ;; Fast model, no refinement
      (single-pass text 'gpt-3.5))))
```

### 48.5 Budget Monitoring

```lisp
(define (monitor-budget-usage task)
  (let ((initial-budget (get-budget))
        (start-time (current-time)))

    (define (report-usage)
      (let ((spent (- initial-budget (get-budget)))
            (duration (- (current-time) start-time)))
        (display (list "Budget used:" spent "tokens in" duration "seconds"))
        (display (list "Rate:" (/ spent duration) "tokens/sec"))))

    (on-completion report-usage)
    (execute task)))
```

### 48.6 Emergency Budget Strategies

When budget runs low:

```lisp
(define (emergency-mode remaining-budget)
  (cond
    ((< remaining-budget 1000)
      ;; Critical: cache everything, use shortest prompts
      (enable-aggressive-caching!)
      (use-minimal-prompts!))

    ((< remaining-budget 5000)
      ;; Warning: reduce quality, increase caching
      (reduce-quality-to-medium!)
      (enable-caching!))

    (else
      ;; Normal operation
      (standard-mode!))))
```

### 48.7 Budget Forecasting

Predict future costs:

```lisp
(define (forecast-budget-needs task)
  (let* ((similar-tasks (find-similar-historical-tasks task))
         (avg-cost (average (map get-cost similar-tasks)))
         (std-dev (standard-deviation (map get-cost similar-tasks)))
         (safety-margin 1.5))
    (* avg-cost safety-margin)))  ; 50% safety margin

Ω> (forecast-budget-needs '(summarize 100 documents))
=> 15000  ; Based on historical data
```

### 48.8 Key Insights

- Token budgets prevent cost overruns
- Adaptive strategies adjust to remaining budget
- Budget allocation distributes resources by priority
- Quality/cost trade-offs based on budget constraints
- Monitoring tracks usage patterns
- Emergency modes handle low-budget situations
- Forecasting predicts future costs

**Next:** Chapter 49 covers semantic caching strategies - the final chapter!
