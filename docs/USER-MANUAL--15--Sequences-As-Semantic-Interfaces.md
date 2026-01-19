# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 15: Sequences as Semantic Interfaces
*Corresponding to SICP Section 2.2: Hierarchical Data and the Closure Property*

### 15.1 The Closure Property

SICP Section 2.2 introduces a profound concept: the **closure property**. An operation satisfies closure if applying it to members of a set produces members of that same set. Cons satisfies closure: consing two pairs gives you another pair. This lets us build hierarchical structures of arbitrary depth.

In semantic programming, closure enables powerful composition. Consider: if `summarize` takes text and returns text, and `translate` takes text and returns text, then we can compose them indefinitely:

```lisp
(translate (summarize (translate (summarize text))))
```

Each operation preserves the interface—text in, text out. This is the semantic closure property.

### 15.2 Hierarchical Semantic Structures

SICP shows how closure enables trees: lists of lists of lists... In semantic programming, we build **semantic hierarchies**:

```lisp
; A document with chapters, each with sections
(define doc
  (list "Introduction to AI"
        (list "Chapter 1: History"
              (list "Section 1.1: Early Days" "Section 1.2: Winter"))
        (list "Chapter 2: Methods"
              (list "Section 2.1: Logic" "Section 2.2: Learning"))))

; Summarize every leaf in the tree
(define (summarize-tree tree)
  (if (string? tree)
      (effect infer.op (list "Summarize: " tree))
      (map summarize-tree tree)))
```

The tree structure is preserved. Each leaf is summarized. The hierarchy remains intact. This is closure in action.

### 15.3 Conventional Interfaces

SICP presents a vision: complex processing as pipelines of simple operations. Rather than nested loops and conditionals, we compose `map`, `filter`, and `fold`:

```
enumerate → filter → map → accumulate
```

With inference programs, this becomes *semantic signal flow*:

```
documents → filter(is-relevant?) → map(summarize) → fold(combine)
    |              |                    |                |
  input       semantically           condensed        unified
  texts       filtered               versions         output
```

### 15.4 Semantic Signal Flow in Practice

Consider processing customer feedback:

```lisp
(define reviews (list "Product broke after one day!"
                      "Great service, very happy"
                      "Never buying again, terrible quality"
                      "Exceeded expectations"))

(define (is-complaint? text)
  (eq? "yes" (effect infer.op
    (list "Is this a complaint? yes or no: " text))))

(define (summarize text)
  (effect infer.op (list "Summarize in 10 words: " text)))

; The pipeline
Ω> (map summarize (filter is-complaint? reviews))
=> ("Product failed quickly." "Very poor quality, customer lost.")
```

Four reviews. Two filtered as complaints. Each summarized. The pipeline reads like a description of what we want.

### 15.5 Flatmap: The Essential Operation

SICP emphasizes `flatmap` (or `append-map`): map a function that returns lists, then flatten. This is essential when each input produces multiple outputs:

```lisp
(define (flatmap f lst)
  (apply append (map f lst)))

(define (expand-to-synonyms word)
  (string-split
    (effect infer.op (list "List 3 synonyms of '" word "', comma-separated: "))
    ","))

Ω> (flatmap expand-to-synonyms (list "happy" "sad"))
=> ("joyful" "content" "pleased" "unhappy" "melancholy" "sorrowful")
```

One word becomes many synonyms. Flatmap collects them all into a single list.

### 15.6 Nested Mappings and Semantic Combinations

SICP uses nested mappings to generate combinations. The semantic analog generates *all semantic variations*:

```lisp
; Generate all tone/language combinations
(define tones (list "formal" "casual" "poetic"))
(define languages (list "French" "German" "Spanish"))

(define (make-variation text tone lang)
  (effect infer.op
    (list "Translate to " lang " in a " tone " tone: " text)))

; All 9 combinations
(define (all-variations text)
  (flatmap
    (lambda (tone)
      (map (lambda (lang) (make-variation text tone lang))
           languages))
    tones))

Ω> (all-variations "Hello, how are you?")
=> ("Bonjour, comment allez-vous?"      ; formal French
    "Guten Tag, wie geht es Ihnen?"     ; formal German
    "Buenos días, ¿cómo está usted?"    ; formal Spanish
    "Salut, ça va?"                     ; casual French
    ...)
```

Nine LLM calls. Nine semantic variations. The nested mapping expresses the combinatorial structure clearly.

### 15.7 Tree Accumulation

SICP shows `accumulate` (fold) operating on sequences. For trees, we need recursive accumulation:

```lisp
(define (tree-accumulate op init tree)
  (if (string? tree)
      (op tree init)
      (fold-right (lambda (subtree result)
                    (tree-accumulate op result subtree))
                  init
                  tree)))

; Count all semantic units in a document
(define (count-topics doc)
  (tree-accumulate
    (lambda (text count)
      (+ count (length (string-split
        (effect infer.op (list "List main topics: " text)) ","))))
    0
    doc))
```

### 15.8 Mapping Over Trees

The power of closure lets us transform entire hierarchies:

```lisp
(define (tree-map f tree)
  (if (string? tree)
      (f tree)
      (map (lambda (subtree) (tree-map f subtree)) tree)))

; Translate an entire document hierarchy
(define (translate-document doc lang)
  (tree-map
    (lambda (text) (effect infer.op (list "Translate to " lang ": " text)))
    doc))

Ω> (translate-document doc "French")
=> ("Introduction à l'IA"
    ("Chapitre 1: Histoire"
     ("Section 1.1: Débuts" "Section 1.2: Hiver"))
    ...)
```

The entire structure translates. Headers become French headers. Sections become French sections. Hierarchy preserved.

### 15.9 The Eight Queens of Semantics

SICP's famous eight queens example uses nested mappings to generate and filter solutions. Here's a semantic analog: find valid *argument structures*:

```lisp
(define premises (list "All birds can fly" "Penguins are birds" "Fish live in water"))
(define conclusions (list "Penguins can fly" "Fish are birds" "Birds exist"))

(define (valid-argument? premise conclusion)
  (eq? "yes" (effect infer.op
    (list "Does the premise '" premise "' logically support the conclusion '" conclusion "'? yes or no"))))

; Find all valid premise-conclusion pairs
(define (find-valid-arguments)
  (filter
    (lambda (pair) (valid-argument? (car pair) (cadr pair)))
    (flatmap
      (lambda (p) (map (lambda (c) (list p c)) conclusions))
      premises)))

Ω> (find-valid-arguments)
=> (("All birds can fly" "Penguins can fly")
    ("Penguins are birds" "Birds exist"))
```

We generate all premise-conclusion combinations, then filter to logically valid ones. The LLM evaluates logical validity.

### 15.10 The Abstraction Barrier of Pipelines

The pipeline pattern creates an abstraction barrier: *what* we want versus *how* it's computed. The caller specifies transformations declaratively; the implementation handles LLM calls, iteration, and combination.

```lisp
; What we want: negative reviews, summarized, in French
(define (french-complaint-summary reviews)
  (map to-french
       (map summarize
            (filter is-complaint? reviews))))
```

The pipeline reads like English: "French summaries of complaints." The mechanics are hidden.

### 15.11 Exercises

**Exercise 15.1:** Build a pipeline that processes news articles: filter to technology topics, extract key claims, and combine into a briefing.

**Exercise 15.2:** Implement `(tree-filter pred tree)` that keeps only leaves satisfying a semantic predicate. Use it to extract all "controversial statements" from a document.

**Exercise 15.3:** Design a pipeline for sentiment analysis that categorizes reviews as positive/negative/neutral, groups by category, and summarizes each group.

**Exercise 15.4:** SICP's picture language composes transformations. Design a "text transformation language" where operations like `beside`, `above`, and `flip` combine text blocks semantically.

**Exercise 15.5:** The eight queens problem generates and tests combinations. Design a "coherent story generator" that generates possible plot points, filters for narrative coherence, and assembles them.

**Exercise 15.6:** Implement `(semantic-permutations items)` that generates all orderings of items, then `(best-ordering items criterion)` that uses the LLM to select the most coherent ordering.

---