# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 7: Lazy Streams
### Infinite Data Structures

OmegaLLM supports lazy streams—potentially infinite sequences that compute values on demand.

```lisp
; Convert a list to a stream
Ω> (define s (list->stream (list 1 2 3 4 5)))
=> s

; Take first 3 elements as a list
Ω> (stream->list s 3)
=> (1 2 3)
```

### Stream Operations

Streams support map and filter (lazily):

```lisp
; Stream of numbers
(define nums (list->stream (list 1 2 3 4 5 6 7 8 9 10)))

; Lazily square each
(define squared (stream-map (lambda (x) (* x x)) nums))

; Take first 5 squares
Ω> (stream->list squared 5)
=> (1 4 9 16 25)
```

### Combining Streams with LLMs

You can create streams where each element involves an LLM call:

```lisp
; A stream of translations (computed lazily)
(define words (list->stream (list "one" "two" "three" "four" "five")))
(define french-words (stream-map to-french words))

; Only compute translations as needed
Ω> (stream->list french-words 2)
=> ("un" "deux")
```

Only the first two translations were computed.