;; Chapter 36: Type Coercion Towers
;; Dual-dimension coercion: formality × specificity towers.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch36-type-coercion

;; Formality axis: casual ↔ neutral ↔ formal ↔ legal
(define (step-formality direction text)
  (effect infer.op
    (list (if (eq? direction 'up)
              "Make more formal: "
              "Make more casual: ")
          text)))

;; Specificity axis: vague ↔ general ↔ specific ↔ precise
(define (step-specificity direction text)
  (effect infer.op
    (list (if (eq? direction 'up)
              "Add more detail: "
              "Make more general: ")
          text)))

;; Navigate both dimensions simultaneously
;; formality-steps: positive = more formal, negative = more casual
;; specificity-steps: positive = more specific, negative = more vague
(define (coerce-2d text formality-steps specificity-steps)
  (let* ((after-formality
           (cond
             ((> formality-steps 0)
              (step-formality 'up text))
             ((< formality-steps 0)
              (step-formality 'down text))
             (else text)))
         (after-specificity
           (cond
             ((> specificity-steps 0)
              (step-specificity 'up after-formality))
             ((< specificity-steps 0)
              (step-specificity 'down after-formality))
             (else after-formality))))
    after-specificity))

;; Examples navigating the 2D semantic space
(list
  "EXAMPLE-1: Increase formality and specificity"
  (coerce-2d "lots of folks are worried" 2 2)

  "EXAMPLE-2: Decrease formality, increase specificity"
  (coerce-2d "We hereby inform you of issues" -2 1)

  "EXAMPLE-3: Increase formality, decrease specificity"
  (coerce-2d "Hey, the server at 192.168.1.1 crashed" 2 -1))
