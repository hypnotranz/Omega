;; Chapter 20: The Semantic Environment Model
;; Show how context shapes interpretation.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch20-semantic-environment

(define (interpret term env-note)
  (effect infer.op
    (list "Interpret the word 'bank' given this environment: " env-note ". Respond with river or finance.")))

(list
  (interpret "bank" "We studied erosion patterns near the river bank.")
  (interpret "bank" "The finance team asked the bank to extend credit."))
