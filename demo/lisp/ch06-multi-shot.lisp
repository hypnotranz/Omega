;; Chapter 6: Multi-Shot Sampling
;; Use search.op to gather multiple semantic candidates.
;;
;; `search.op` runs several oracle samples and returns a distribution so you can
;; see a spread of plausible answers.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch06-multi-shot

(define request "Please provide an update on the audit timeline and risk posture.")
(effect search.op
  (list "Rewrite this status update in three distinct tones: warm, concise, and executive:" request))
