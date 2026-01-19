# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 23: Streams of Inference
### 23.1 The Allure of Infinity

SICP Chapter 3.5 contains some of the most mind-expanding material in all of computer science: the treatment of infinite data structures through lazy evaluation. A stream is a sequence whose elements are computed on demand, allowing us to work with sequences too large—or even infinite—to fit in memory.

The key insight is *delayed evaluation*. Instead of computing all elements upfront, we compute them only when needed. The tail of a stream is a *promise* to compute more, not the computation itself.

Why does this matter for inference programming? Because LLM calls are expensive. Each call takes time, costs money, and consumes resources. We don't want to call the LLM for elements we'll never use. Streams give us *semantic computation on demand*.

### 23.2 The Stream Abstraction

In OmegaLLM, streams provide these primitives:

```lisp
(list->stream lst)        ; Convert a list to a stream
(stream->list s n)        ; Force n elements to a list
(stream-car s)            ; First element (forced)
(stream-cdr s)            ; Rest of stream (lazy)
(stream-map f s)          ; Lazy map
(stream-filter p s)       ; Lazy filter
(stream-take s n)         ; Take first n elements
(stream-null? s)          ; Check if empty
the-empty-stream          ; The empty stream
```

The crucial property: `stream-cdr` returns a stream, not a list. The elements beyond the first remain unevaluated until requested.

### 23.3 Lazy Semantic Processing

Consider processing a list of topics. With eager evaluation, every element gets processed immediately:

```lisp
; EAGER: All 4 LLM calls happen now
(map summarize (list "AI" "climate" "quantum" "energy"))
```

With streams, we control when computation happens:

```lisp
(define topics (list->stream
  (list "artificial intelligence"
        "climate change"
        "quantum computing"
        "renewable energy")))

; Convert to stream first - no LLM calls yet
(define topic-stream topics)

; Only take what we need - just 2 LLM calls
Ω> (stream->list topic-stream 2)
=> ("artificial intelligence" "climate change")
```

The third and fourth topics were never processed. Their LLM calls never happened. This is the power of laziness.

### 23.4 Stream Pipelines

The true elegance emerges when we chain stream operations:

```lisp
; Define our semantic operations
(define (is-technical? text)
  (eq? "yes" (effect infer.op
    (list "Is this technical content? yes or no: " text))))

(define (simplify text)
  (effect infer.op (list "Explain in simple terms: " text)))

; Build a lazy pipeline
(define documents (list->stream
  (list "The TCP/IP stack handles network packets"
        "My cat is fluffy"
        "Quantum entanglement enables secure communication"
        "I like pizza"
        "Machine learning uses gradient descent")))

; Filter to technical, then simplify - ALL LAZY
(define simplified-technical
  (stream-map simplify
    (stream-filter is-technical? documents)))

; Only compute what we need
Ω> (stream->list simplified-technical 2)
=> ("Networks send information in small chunks called packets"
    "Special physics lets us send secret messages")
```

How many LLM calls happened? Let's trace:
1. `is-technical?` called on "TCP/IP..." → yes → `simplify` called → result 1
2. `is-technical?` called on "cat" → no
3. `is-technical?` called on "quantum" → yes → `simplify` called → result 2
4. We have 2 results → STOP

Five potential documents. Only four `is-technical?` calls. Only two `simplify` calls. The last document was never examined.

### 23.5 The Concept of Infinite Semantic Streams

Here is where streams become philosophically interesting. Consider a stream representing "all possible elaborations of an idea":

```lisp
(define (elaborate text)
  (effect infer.op (list "Elaborate on this idea: " text)))

(define (infinite-elaboration seed)
  (cons-stream seed
    (infinite-elaboration (elaborate seed))))
```

This stream is *conceptually infinite*. Each element is the elaboration of the previous. You could take 1 element, or 10, or 100. The stream doesn't care—it produces on demand.

```lisp
; Start with a simple idea
(define thought-stream (infinite-elaboration "trees"))

; Take just 3 elaborations
Ω> (stream->list thought-stream 3)
=> ("trees"
    "Trees are vital organisms that produce oxygen and provide habitat"
    "Trees serve as the lungs of our planet, converting CO2 to oxygen through
     photosynthesis while simultaneously providing shelter, food, and nesting
     sites for countless species...")
```

Each element is richer than the last. We could continue indefinitely—but we don't have to.

### 23.6 Streams and Multi-Shot Sampling

Streams combine naturally with the `search.op` effect for probabilistic inference:

```lisp
; A stream of sampled translations
(define (translation-samples text target-lang)
  (list->stream
    (map (lambda (m) (m 'denotation))
         (dist-support
           (effect search.op
             (list "Translate to " target-lang ": " text))))))

; Get several possible translations
Ω> (stream->list (translation-samples "hello" "Japanese") 3)
=> ("こんにちは" "やあ" "もしもし")
```

Each element of the stream is a different sampled translation. The LLM's uncertainty becomes a stream of possibilities.

### 23.7 The Wisdom of Laziness

Abelson and Sussman conclude their streams chapter with a meditation on the difference between "what" and "when." Lazy evaluation separates the *description* of a computation from its *execution*. We describe an infinite sequence; we execute only what we need.

For inference programming, this separation is not merely elegant—it is economic. LLM calls have real costs. Laziness ensures we pay only for what we use.

### 23.8 Exercises

**Exercise 23.1:** Create a "stream of increasingly formal rewrites" that takes casual text and returns a stream where each element is more formal than the last. Test with "hey, wanna grab lunch?"

**Exercise 23.2:** Implement `stream-flatmap` for streams. Use it to expand each word in a sentence into a stream of its synonyms, flattened into a single stream.

**Exercise 23.3:** Consider a stream of "story continuations" where each element continues the narrative from the previous. Write `(continue-story seed n)` that returns n continuations. What happens to narrative coherence as n grows?

**Exercise 23.4:** Streams in SICP are used to solve the problem of signal processing. Design a "semantic signal processor" that takes a stream of customer messages and outputs a stream of (sentiment, urgency, suggested-action) tuples, computed lazily.

**Exercise 23.5:** The sieve of Eratosthenes can be expressed as stream operations. Can you express a "semantic sieve" that filters a stream of texts, removing any that are "too similar" to previously seen texts?