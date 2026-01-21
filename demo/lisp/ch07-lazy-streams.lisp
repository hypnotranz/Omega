;; Chapter 7: Lazy Streams
;; Generate follow-up questions lazily and force only what you need.
;;
;; `list->stream` lifts a list into a lazy stream, `stream-map` applies a function
;; lazily, and `stream->list` forces the first `n` elements.
;;
;; Run: npx tsx demo/by-chapter/index.ts --chapter ch07-lazy-streams

(define notes
  (list
    "The incident response runbook feels outdated."
    "Our healthcare customers need clearer assurances about data residency."
    "The onboarding emails sound too robotic."))

(define (follow-up note)
  (effect infer.op
    (list "Suggest one follow-up question to clarify this note. Keep it empathetic: " note)))

(define s (list->stream notes))
(define queued (stream-map follow-up s))
(stream->list queued 2)
