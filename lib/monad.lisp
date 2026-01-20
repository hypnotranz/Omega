;; Monadic helpers for nondeterminism
;; Provides do-notation (mdo), guard, and helpers for list-style composition.

(define-syntax mdo
  (syntax-rules (<-)
    ((_ expr) expr)
    ((_ (var <- m) rest ...)
     (bind m (lambda (var) (mdo rest ...))))
    ((_ m rest ...)
     (bind m (lambda (_) (mdo rest ...))))))

(define (guard pred)
  (if pred (unit #t) (mzero)))

(define-syntax msum
  (syntax-rules ()
    ;; Empty list literal
    ((_ (list)) (mzero))
    ;; List literal: expand to variadic form to avoid eager evaluation of elements
    ((_ (list e ...)) (msum e ...))
    ;; No args -> mzero
    ((_ ) (mzero))
    ;; Single arg -> return computation
    ((_ e) e)
    ;; Fold remaining computations with mplus
    ((_ e1 e2 ...) (mplus e1 (msum e2 ...)))))

(define (mfilter pred m)
  (bind m (lambda (x)
    (if (pred x) (unit x) (mzero)))))

(define (mconcatMap f xs)
  (if (null? xs)
      (mzero)
      (mplus (f (car xs)) (mconcatMap f (cdr xs)))))
