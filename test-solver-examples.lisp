;; ═══════════════════════════════════════════════════════════════════════════
;; Solver System Verification Examples
;; ═══════════════════════════════════════════════════════════════════════════

;; 1. Budget Split and Allocation
(define budget (make-budget 100 10 1000))
(budget-split budget 4)  ;; Should create 4 budgets with 25 tokens each
(budget-allocate budget (list 1 2 1))  ;; Should create budgets with 25, 50, 25 tokens

;; 2. Create a Simple Solver
(define identity-solver 
  (make-solver "identity"
    (lambda (p b) (unit (make-result 'success p 0)))
    (lambda (p) (make-estimate 1 1 1 1.0))))

(solver? identity-solver)  ;; Should be #t
(solver-name identity-solver)  ;; Should be "identity"

;; 3. Compose Solvers Sequentially
(define add1 
  (make-solver "add1"
    (lambda (p b) (unit (make-result 'success (+ p 1) 1)))
    (lambda (p) (make-estimate 1 1 1 1.0))))

(define mult2 
  (make-solver "mult2"
    (lambda (p b) (unit (make-result 'success (* p 2) 1)))
    (lambda (p) (make-estimate 1 1 1 1.0))))

(define pipeline (compose-sequential (list add1 mult2)))
(solver-solve pipeline 5 (make-budget 100 10 1000))
;; Should return result with solution 12 (5+1)*2

;; 4. Repair Loop
(repair-until-valid
  0
  (lambda (x) (>= x 3))    ;; Valid when >= 3
  (lambda (x) (+ x 1))      ;; Repair by adding 1
  10)
;; Should return success with solution 3 after 3 iterations

;; 5. Fact Store
(define fs (make-fact-store))
(define fs2 (assert-fact fs "x.type" "integer"))
(define fs3 (assert-fact fs2 "x.value" 42))
(query-fact fs3 "x.type")   ;; Should return "integer"
(query-facts fs3 "x\..*")  ;; Should return both facts

;; 6. Fixpoint Iteration
(fixpoint
  1
  (lambda (x) (if (< x 10) (+ x 1) x))  ;; Increment until 10
  (lambda (a b) (equal? a b))            ;; Check equality
  100)
;; Should converge to 10

;; 7. Compose Fallback
(define fail-solver 
  (make-solver "fail"
    (lambda (p b) (unit (make-result 'failure #f "nope" 1)))
    (lambda (p) (make-estimate 1 1 1 1.0))))

(define success-solver 
  (make-solver "success"
    (lambda (p b) (unit (make-result 'success 'ok 1)))
    (lambda (p) (make-estimate 1 1 1 1.0))))

(define fallback (compose-fallback (list fail-solver success-solver)))
(solver-solve fallback 'x (make-budget 100 10 1000))
;; Should succeed with 'ok

;; 8. Compose Retry
(define attempt-counter 0)
(define eventual-success
  (make-solver "eventual"
    (lambda (p b)
      (set! attempt-counter (+ attempt-counter 1))
      (if (>= attempt-counter 3)
          (unit (make-result 'success 'finally attempt-counter))
          (unit (make-result 'failure #f "not yet" attempt-counter))))
    (lambda (p) (make-estimate 1 1 1 1.0))))

(define retry-solver (compose-retry eventual-success 5))
(solver-solve retry-solver 'x (make-budget 100 10 1000))
;; Should succeed on third attempt

;; ═══════════════════════════════════════════════════════════════════════════
;; End of Verification Examples
;; ═══════════════════════════════════════════════════════════════════════════
