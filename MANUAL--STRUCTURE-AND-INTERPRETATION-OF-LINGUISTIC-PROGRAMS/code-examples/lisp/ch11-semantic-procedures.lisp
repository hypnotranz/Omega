;; Chapter 11: Semantic Procedures as Black Boxes
;; Encapsulate semantic judgment behind a predicate.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch11-semantic-procedures

(define (is-professional? email)
  (equal? "yes"
    (effect infer.op
      (list "Is this email draft professional and calm? yes/no: " email))))

(is-professional?
  "Team, let's present findings with clarity and keep the tone reassuring for regulators.")
