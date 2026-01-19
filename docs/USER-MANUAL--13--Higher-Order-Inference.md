# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 13: Higher-Order Inference
### 13.1 Procedures as Arguments

SICP's Chapter 1.3 opens with a profound observation: the same pattern of computation appears again and again across different domains. The sum of integers, the sum of squares, the sum of cubes—all share a common structure. Abstraction over this pattern gives us higher-order procedures.

The same insight applies to semantic operations. Consider how often we need to:
- Transform each item in a list (translation, summarization, reformatting)
- Select items meeting some criterion (filtering by topic, sentiment, relevance)
- Combine items into a synthesis (merging ideas, building narratives)

These are the eternal patterns of `map`, `filter`, and `fold`—now operating on meaning.

### 13.2 Fold as Semantic Synthesis

The `fold` operation (also called `reduce`) combines elements of a list using a binary operation. In traditional programming, fold might sum numbers or concatenate strings. In inference programming, fold performs *semantic synthesis*:

```lisp
(define (fold-left op init lst)
  (if (null? lst) init (fold-left op (op init (car lst)) (cdr lst))))

(define (merge-ideas a b)
  (effect infer.op (list "Merge these ideas into one sentence: '" a "' and '" b "'")))
```

Watch what happens when we fold over text fragments:

```lisp
Ω> (fold-left merge-ideas
              "The economy grew"
              (list "unemployment fell" "inflation remained stable"))
=> "The economy grew as unemployment fell and inflation remained stable."
```

Observe carefully: the LLM didn't merely concatenate. It *synthesized*. It chose the connective "as" to express causation, then "and" for the final conjunction. It produced grammatically correct, semantically coherent prose.

This is fold operating on the level of meaning.

### 13.3 Map as Bulk Transformation

When you map a traditional function over a list, each element undergoes the same transformation. When you map an LLM function, each element undergoes *semantic* transformation:

```lisp
(define (to-french word)
  (effect infer.op (list "Translate to French (one word only): " word)))

Ω> (map to-french (list "hello" "goodbye" "thanks"))
=> ("bonjour" "au revoir" "merci")
```

Three LLM calls, three semantic judgments, one elegant expression. The pattern is familiar; the power is new.

### 13.4 Filter as Semantic Selection

Perhaps the most striking application is filter with semantic predicates:

```lisp
(define (is-food? thing)
  (eq? "yes" (effect infer.op (list "Is " thing " a food? Answer only yes or no."))))

Ω> (filter is-food? (list "apple" "chair" "pizza" "laptop" "banana"))
=> ("apple" "pizza" "banana")
```

Five items. Five LLM calls. Five semantic judgments about edibility. The filter pattern hides this complexity behind a single, declarative expression.

Consider what we've achieved: selection based on *meaning*, not structure. No regex could do this. No rule system could enumerate all foods. The LLM knows.

### 13.5 Procedures as Returned Values — Prompt Factories

SICP 1.3 culminates with procedures that return procedures. This lets us create *procedure generators*—factories that manufacture specialized procedures on demand.

In inference programming, this pattern becomes **prompt factories**: functions that manufacture specialized LLM functions. A single factory can produce infinite variations, each tailored to a specific task.

### 13.6 Tone Adjustment Factories

Consider the task of rewriting text in different tones. We could write separate functions for formal, casual, and poetic transformations. But observe the repetition:

```lisp
(define (make-formal text)
  (effect infer.op (list "Rewrite in a formal tone: " text)))

(define (make-casual text)
  (effect infer.op (list "Rewrite in a casual tone: " text)))

(define (make-poetic text)
  (effect infer.op (list "Rewrite in a poetic tone: " text)))
```

The pattern cries out for abstraction. Extract the varying part—the tone—as a parameter:

```lisp
(define (make-tone-adjuster tone)
  (lambda (text)
    (effect infer.op (list "Rewrite in a " tone " tone: " text))))
```

Now we have a factory. Feed it a tone, receive a specialized transformer:

```lisp
(define make-formal (make-tone-adjuster "formal"))
(define make-casual (make-tone-adjuster "casual"))
(define make-poetic (make-tone-adjuster "poetic"))
```

Each returned procedure closes over its tone parameter, remembering what kind of transformation it performs:

```lisp
Ω> (make-formal "hey whats up, wanna grab lunch?")
=> "Hello, how are you? Would you like to join me for lunch?"

Ω> (make-poetic "The sun set behind the mountains.")
=> "'Twas the sun's grand descent beyond the mountain's crest,"
```

One factory. Infinite specialized transformers. This is the expressive power of procedures as first-class values.

### 13.7 Translator Factories

The same pattern applies to translation:

```lisp
(define (make-translator target-language)
  (lambda (text)
    (effect infer.op (list "Translate to " target-language ": " text))))

(define to-german (make-translator "German"))
(define to-japanese (make-translator "Japanese"))
(define to-swahili (make-translator "Swahili"))
```

We haven't written three translation functions. We've written a *translation function generator*, and used it to instantiate three specific translators. The distinction matters: adding a fourth language requires no new code, only a new invocation of the factory.

### 13.8 Classifier Factories

Perhaps most useful are classifier factories—generators of semantic predicates:

```lisp
(define (make-classifier category)
  (lambda (text)
    (eq? "yes" (effect infer.op
      (list "Is this " category "? Answer yes or no: " text)))))

(define is-question? (make-classifier "a question"))
(define is-complaint? (make-classifier "a complaint"))
(define is-urgent? (make-classifier "urgent"))
(define is-actionable? (make-classifier "actionable feedback"))
```

Now we can filter and classify at will:

```lisp
Ω> (filter is-urgent? inbox-messages)
Ω> (filter is-complaint? customer-feedback)
```

### 13.9 Exercises

**Exercise 13.1:** Define `(summarize-all texts)` that takes a list of paragraphs and produces a single summary using fold.

**Exercise 13.2:** Use map and filter together to process a list of customer reviews: filter to negative reviews, then map each to a one-sentence summary.

**Exercise 13.3:** The fold in 13.2 processed items left-to-right. Does the order matter for semantic synthesis? Experiment with `fold-right` and compare results.

**Exercise 13.4:** Define `(make-summarizer length)` that returns a procedure summarizing text to approximately `length` words.

**Exercise 13.5:** Define `(make-style-transfer source-style target-style)` that returns a procedure converting text from one writing style to another. Test with "academic" to "journalistic".

**Exercise 13.6:** Compose factories: use `compose` to create a procedure that first translates to French, then adjusts to a formal tone.