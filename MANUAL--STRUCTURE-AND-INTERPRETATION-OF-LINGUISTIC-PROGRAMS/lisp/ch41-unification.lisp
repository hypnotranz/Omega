;; Chapter 41: Unification And Pattern Matching
;; Frame-based semantic unification.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch41-unification

(define transaction-frame
  '(buyer: ?buyer
    seller: ?seller
    goods: ?goods
    price: ?price))

(define (unify pattern text)
  (effect infer.op
    (list "Extract frame slots from text: " pattern " Text: " text)))

(unify transaction-frame "Alice bought a laptop from Bob for $1200")
