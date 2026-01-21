;; Chapter 14: Semantic Data Abstraction
;; Validators for natural-language structures.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch14-semantic-data

(define (is-haiku? poem)
  (equal? "yes" (effect infer.op (list "Does this read like a calming haiku? yes/no: " poem))))

(define (has-greeting? email)
  (equal? "yes" (effect infer.op (list "Does this email open with a courteous greeting? yes/no: " email))))

(list
  (is-haiku? "Quiet dashboards hum / Incidents fall back asleep / Teams breathe evenly")
  (has-greeting? "Hello team, thank you for the latest build - can we add a changelog?"))
