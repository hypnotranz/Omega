;; Chapter 17: Multiple Representations of Meaning
;; Convert register across styles.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch17-multiple-representations

(define complaint "Your incident updates sound robotic and uncaring.")
(list
  (effect infer.op (list "Rewrite in a formal yet empathetic register: " complaint))
  (effect infer.op (list "Rewrite in a candid peer-to-peer register: " complaint)))
