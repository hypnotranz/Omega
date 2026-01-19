# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 26: The AMB Inference Engine
### 26.1 The Nature of Nondeterminism

SICP Chapter 4.3 introduces one of computing's most elegant ideas: nondeterministic programming with the `amb` operator. The idea is deceptively simple: instead of telling the computer *what* value to use, you tell it to *choose* from possibilities. If a choice leads to failure, the system automatically backtracks and tries another.

In traditional nondeterministic programming, this is used for puzzles like the eight queens problem or map coloring—domains where constraints are mathematical or logical. But with LLMs, we enter new territory: **semantic nondeterminism**, where the constraints are about *meaning*.

Consider what happens when we combine `amb` with LLM predicates. The search space becomes a space of *meanings*, and the constraints become questions of comprehension.

### 26.2 The Mechanics of Choice and Failure

The `amb` system in OmegaLLM has two key effects:

```lisp
(effect amb.choose alternatives)  ; Choose one of the alternatives
(effect amb.fail reason)          ; Signal that current path failed
```

The `amb.choose` effect takes a list of *thunks* (zero-argument functions), and returns the result of calling one of them. If subsequent computation fails, the system backtracks and tries another.

We typically wrap failure in a `require` function:

```lisp
(define (require condition)
  (if condition #t (effect amb.fail "constraint failed")))
```

Now we can express constraints declaratively:

```lisp
Ω> (begin
     (define x (effect amb.choose (list (lambda () 1)
                                        (lambda () 2)
                                        (lambda () 3))))
     (require (> x 1))
     x)
=> 2
```

What happened here? The system tried x=1, but `(> 1 1)` is false, so `require` failed. The system backtracked, tried x=2, and `(> 2 1)` succeeded. The search terminated with x=2.

### 26.3 Semantic Constraint Satisfaction

Here is where the magic happens. Traditional constraints ask: "Is x > 1?" Semantic constraints ask: "Is x edible?" "Is x professional?" "Does x rhyme with 'sing'?"

```lisp
(define (is-edible? thing)
  (eq? "yes" (effect infer.op
    (list "Is " thing " edible? Answer only yes or no."))))

(define (is-french-word? word)
  (eq? "yes" (effect infer.op
    (list "Is '" word "' a French word? Answer only yes or no."))))
```

Now combine them:

```lisp
; Find something that is BOTH edible AND French
Ω> (begin
     (define word (effect amb.choose
       (list (lambda () "apple")
             (lambda () "pomme")
             (lambda () "car")
             (lambda () "voiture"))))
     (require (is-edible? word))
     (require (is-french-word? word))
     word)
=> "pomme"
```

Trace the execution:
1. System tries "apple" → LLM says edible ✓, but LLM says not French ✗ → backtrack
2. System tries "pomme" → LLM says edible ✓, LLM says French ✓ → success!

The search explored a space of linguistic possibilities, guided by semantic constraints that no traditional type system could express.

### 26.4 Linguistic Puzzle Solving

The combination of `amb` and semantic predicates creates a puzzle-solving engine for language:

```lisp
(define (rhymes-with? w1 w2)
  (eq? "yes" (effect infer.op
    (list "Does '" w1 "' rhyme with '" w2 "'? Answer yes or no."))))

(define (is-verb? word)
  (eq? "yes" (effect infer.op
    (list "Is '" word "' a verb? Answer yes or no."))))

; Find a word that rhymes with "sing" AND is a verb
Ω> (begin
     (define word (effect amb.choose
       (list (lambda () "cat")
             (lambda () "bring")
             (lambda () "ring")
             (lambda () "table"))))
     (require (is-verb? word))
     (require (rhymes-with? word "sing"))
     word)
=> "bring"
```

The system found "bring"—a verb that rhymes with "sing". It rejected "cat" (doesn't rhyme), "ring" if checked as noun first, and "table" (neither rhymes nor is primarily a verb).

### 26.5 Multi-Constraint Poetry

Consider generating constrained poetry:

```lisp
(define (is-noun? word)
  (eq? "yes" (effect infer.op (list "Is '" word "' a noun? yes or no."))))

(define (has-two-syllables? word)
  (eq? "yes" (effect infer.op (list "Does '" word "' have exactly two syllables? yes or no."))))

(define (evokes-nature? word)
  (eq? "yes" (effect infer.op (list "Does '" word "' evoke nature? yes or no."))))

; Find a two-syllable noun evoking nature
Ω> (begin
     (define word (effect amb.choose
       (list (lambda () "computer")
             (lambda () "flower")
             (lambda () "car")
             (lambda () "river")
             (lambda () "table"))))
     (require (is-noun? word))
     (require (has-two-syllables? word))
     (require (evokes-nature? word))
     word)
=> "flower"  ; or "river" - both satisfy all three constraints
```

Three semantic constraints. Automatic backtracking. A word that satisfies all meaning-based requirements.

### 26.6 The Multiple Dwelling Problem: Semantic Edition

SICP presents the classic "multiple dwelling" puzzle: five people live in a building, constrained by various requirements. The semantic analog: **constrained narrative generation**.

```lisp
; Generate a story where characters have specific traits
(define (generate-constrained-story)
  (let ((protagonist (effect amb.choose
          (list (lambda () "a brave knight")
                (lambda () "a cunning merchant")
                (lambda () "a wise scholar"))))
        (setting (effect amb.choose
          (list (lambda () "a medieval castle")
                (lambda () "a bustling port city")
                (lambda () "an ancient library"))))
        (conflict (effect amb.choose
          (list (lambda () "a dragon threatens the land")
                (lambda () "a valuable artifact is stolen")
                (lambda () "forbidden knowledge is discovered")))))

    ; Semantic constraints
    (require (coherent-pairing? protagonist setting))
    (require (fitting-conflict? conflict setting))
    (require (protagonist-can-resolve? protagonist conflict))

    (list protagonist setting conflict)))

(define (coherent-pairing? char setting)
  (eq? "yes" (effect infer.op
    (list "Does " char " naturally belong in " setting "? yes or no"))))

Ω> (generate-constrained-story)
=> ("a wise scholar" "an ancient library" "forbidden knowledge is discovered")
```

The system found a coherent combination: a scholar in a library discovering forbidden knowledge. It rejected incoherent combinations like a knight in a library facing a dragon.

### 26.7 Parsing Natural Language with AMB

SICP uses `amb` to build a natural language parser. The semantic version doesn't just parse—it interprets:

```lisp
(define (parse-intent sentence)
  (let ((intent (effect amb.choose
          (list (lambda () 'request)
                (lambda () 'question)
                (lambda () 'statement)
                (lambda () 'command)))))
    (require (matches-intent? sentence intent))
    intent))

(define (matches-intent? sentence intent)
  (eq? "yes" (effect infer.op
    (list "Is this sentence a " (symbol->string intent) "? "
          "Sentence: " sentence " Answer yes or no."))))

Ω> (parse-intent "Could you please help me with this?")
=> request

Ω> (parse-intent "The sky is blue.")
=> statement

Ω> (parse-intent "What time is it?")
=> question
```

### 26.8 Semantic Sudoku: Constraint Propagation with Meaning

SICP discusses how `amb` can solve constraint satisfaction problems. Here's a semantic analog—filling in a story with consistent details:

```lisp
(define (fill-story-blanks)
  (let ((weather (effect amb.choose
          (list (lambda () "sunny") (lambda () "rainy")
                (lambda () "snowy") (lambda () "foggy"))))
        (mood (effect amb.choose
          (list (lambda () "cheerful") (lambda () "melancholic")
                (lambda () "tense") (lambda () "peaceful"))))
        (activity (effect amb.choose
          (list (lambda () "having a picnic")
                (lambda () "reading by the fire")
                (lambda () "walking through the woods")
                (lambda () "waiting for someone")))))

    ; All three must form a coherent scene
    (require (atmosphere-matches? weather mood))
    (require (activity-suits-weather? activity weather))
    (require (activity-expresses-mood? activity mood))

    (format-scene weather mood activity)))

(define (atmosphere-matches? weather mood)
  (eq? "yes" (effect infer.op
    (list "Does " weather " weather naturally evoke a " mood " mood? yes or no"))))

Ω> (fill-story-blanks)
=> "On a rainy day with a melancholic mood, they were reading by the fire."
```

### 26.9 Exhaustive Search vs. Directed Search

Traditional `amb` exhaustively searches all combinations. With semantic constraints, we can do **directed search**—letting the LLM suggest promising candidates:

```lisp
(define (directed-amb prompt constraint)
  ; Instead of trying all options, ask LLM for likely candidates
  (let ((candidates (effect infer.op
          (list "List 5 options for: " prompt ". Return as comma-separated."))))
    (let ((options (string-split candidates ",")))
      (let ((choice (effect amb.choose
              (map (lambda (o) (lambda () (string-trim o))) options))))
        (require (constraint choice))
        choice))))

; Find an animal that is both cute and dangerous
Ω> (directed-amb "animals that could be cute"
                  (lambda (x) (is-dangerous? x)))
=> "polar bear"  ; LLM suggested cute animals, backtracking found one that's dangerous
```

### 26.10 The Liars Puzzle: Semantic Version

SICP's liars puzzle involves statements that may be true or false. Here's a semantic version—determining which claims are credible:

```lisp
(define (evaluate-claims claims)
  ; For each claim, decide if it's true or false
  (let ((verdicts (map (lambda (claim)
                         (let ((v (effect amb.choose
                                    (list (lambda () 'true) (lambda () 'false)))))
                           (cons claim v)))
                       claims)))
    ; Check for internal consistency
    (require (semantically-consistent? verdicts))
    verdicts))

(define (semantically-consistent? verdicts)
  (eq? "yes" (effect infer.op
    (list "Are these verdicts logically consistent with each other? "
          (format-verdicts verdicts) " Answer yes or no."))))

Ω> (evaluate-claims
     (list "Alice says she was home all day"
           "Bob says he saw Alice at the store"
           "Alice says she never met Bob"))
=> (("Alice says she was home all day" . false)
    ("Bob says he saw Alice at the store" . true)
    ("Alice says she never met Bob" . false))
```

### 26.11 The Philosophical Significance

What we have built is extraordinary: **a constraint solver over the space of meanings**. Traditional SAT solvers work with boolean variables. Our semantic AMB works with linguistic concepts—concepts that no formal logic could capture.

The LLM serves as an oracle in the technical sense: when the system needs to know whether a constraint is satisfied, it asks the oracle. The oracle's answer guides the search through possibility space.

This is, in a very real sense, reasoning about meaning—not through rules we write, but through understanding the LLM has absorbed.

### 26.12 Exercises

**Exercise 26.1:** Define a procedure that uses `amb` to find, from a list of words, one that is both a color AND an emotion (e.g., "blue" can be both).

**Exercise 26.2:** Create a word puzzle solver that finds a word matching: "a verb that starts with 's' and relates to water." Provide a candidate list and use semantic constraints.

**Exercise 26.3:** Implement the "who owns the zebra" puzzle using semantic constraints instead of positional ones. Use natural language facts and let the LLM evaluate consistency.

**Exercise 26.4:** Build a "story coherence checker" that uses `amb` to fill in missing story elements while maintaining narrative consistency across setting, characters, and plot.

**Exercise 26.5:** SICP shows how `amb` can be used for parsing. Build a semantic parser that classifies sentences into intent categories and extracts parameters using backtracking.

**Exercise 26.6:** Implement `(all-solutions thunk)` that collects ALL satisfying assignments, not just the first. How do you know when you've found them all?

**Exercise 26.7:** The current `amb` explores choices in order. Design an experiment to see if reordering candidates affects which answer is returned. What does this tell us about determinism in semantic search?

**Exercise 26.8:** Can `amb` with semantic constraints solve actual crossword puzzle clues? Try: "Four-letter word meaning 'to look intently' that rhymes with 'care'."