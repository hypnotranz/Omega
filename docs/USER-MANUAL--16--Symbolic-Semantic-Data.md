# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 16: Symbolic Semantic Data
### 16.1 The Essence of Symbolic Processing

SICP Chapter 2.3 introduces symbolic data—data that represents things other than numbers. The ability to manipulate symbols, to treat `'x` differently than the value of `x`, is what makes Lisp powerful for AI. The chapter explores symbolic differentiation, set representations, and Huffman encoding.

But consider: what are symbols really *for*? They're for representing *meaning* in a form programs can manipulate. With LLMs, we can work with meaning directly. We don't need symbols as proxies—we have the thing itself.

This chapter explores **semantic symbolic data**: treating meanings as first-class objects that can be quoted, compared, transformed, and composed.

### 16.2 Quoting Meaning

In traditional Lisp, `(quote x)` or `'x` gives you the symbol rather than its value. In semantic programming, we need a similar distinction: the *text* of an expression versus its *meaning*.

```lisp
; The text "The cat sat on the mat"
(define text1 "The cat sat on the mat")

; The meaning of that text (extracted by LLM)
(define meaning1 (effect infer.op
  (list "What is the core meaning of: " text1)))

Ω> meaning1
=> "A cat was sitting on a mat"
```

The text and the meaning are different objects. The text is syntax; the meaning is semantics. We can now manipulate meanings as data:

```lisp
; Check if two different texts have the same meaning
(define (same-meaning? text1 text2)
  (eq? "yes" (effect infer.op
    (list "Do these have the same meaning? yes or no:\n1. "
          text1 "\n2. " text2))))

Ω> (same-meaning? "The cat sat on the mat" "A feline rested upon the rug")
=> #t

Ω> (same-meaning? "The cat sat on the mat" "Dogs are loyal pets")
=> #f
```

### 16.3 Semantic Differentiation

SICP's symbolic differentiation takes an algebraic expression and produces its derivative. The semantic analog: take a statement and produce its *implications*, *prerequisites*, or *contradictions*.

```lisp
(define (implies? statement)
  (effect infer.op
    (list "What does this statement logically imply? " statement)))

(define (prerequisites statement)
  (effect infer.op
    (list "What must be true for this to be true? " statement)))

(define (contradicts? s1 s2)
  (eq? "yes" (effect infer.op
    (list "Do these statements contradict each other? yes or no:\n1. "
          s1 "\n2. " s2))))

Ω> (implies? "All employees must attend the meeting")
=> "At least one meeting will occur. The company has employees."

Ω> (prerequisites "The project was completed on time")
=> "A project existed. A deadline was set. Work was done."

Ω> (contradicts? "The store is open 24 hours" "The store closes at 9pm")
=> #t
```

This is **semantic calculus**: operating on meanings the way traditional symbolic computing operates on algebraic expressions.

### 16.4 Semantic Sets

SICP explores different representations of sets. In semantic programming, sets can be sets of *concepts*:

```lisp
(define (concept-in-set? concept set-description)
  (eq? "yes" (effect infer.op
    (list "Is '" concept "' a member of the category: " set-description "? yes or no"))))

(define (concept-union desc1 desc2)
  (effect infer.op
    (list "Describe a category that includes both: " desc1 " AND " desc2)))

(define (concept-intersection desc1 desc2)
  (effect infer.op
    (list "Describe what is common to both: " desc1 " AND " desc2)))

Ω> (concept-in-set? "penguin" "birds")
=> #t

Ω> (concept-in-set? "penguin" "things that can fly")
=> #f

Ω> (concept-union "fruits" "vegetables")
=> "produce" or "edible plants"

Ω> (concept-intersection "things that are red" "fruits")
=> "red fruits like apples, strawberries, cherries"
```

### 16.5 Semantic Trees

Just as SICP builds expression trees, we can build **semantic trees**—hierarchies of meaning:

```lisp
(define (semantic-children concept)
  (effect infer.op
    (list "List 3-5 subcategories of: " concept ". Return as comma-separated list.")))

(define (semantic-parent concept)
  (effect infer.op
    (list "What broader category contains: " concept "? One word.")))

Ω> (semantic-children "furniture")
=> "chairs, tables, beds, sofas, desks"

Ω> (semantic-parent "penguin")
=> "birds"

Ω> (semantic-parent "birds")
=> "animals"
```

We're building an ontology—a semantic tree of concepts—using the LLM's world knowledge.

### 16.6 Exercises

**Exercise 16.1:** Implement `(paraphrases text n)` that returns n different ways to say the same thing. Verify they all have `same-meaning?` with the original.

**Exercise 16.2:** Build a semantic analogy solver: `(analogy "king" "queen" "man")` should return "woman". Use the pattern "A is to B as C is to ?"

**Exercise 16.3:** Implement `(semantic-distance concept1 concept2)` that estimates how many semantic-parent steps separate two concepts. Is "apple" closer to "banana" or to "computer"?

**Exercise 16.4:** SICP builds Huffman trees for data compression. Design a "semantic compression" scheme that represents common concepts briefly. How would you compress "The meeting was rescheduled to next Tuesday" in a domain-specific context?