;; Chapter 8: The Debugger
;; Trace semantic steps with oracle explanations.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch08-debugger

(define (explain step)
  (effect infer.op
    (list "Explain this debugging step in one sentence: " step)))

(list
  (explain "Check whether the classifier treated the note as a complaint.")
  (explain "Confirm the tone matcher backtracked to the apologetic branch."))
