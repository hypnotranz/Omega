---

## Part I: Foundations

## Chapter 1: Getting Started

### Running the REPL

The simplest way to interact with OmegaLLM is through its REPL (Read-Eval-Print Loop):

```bash
cd OmegaLLM
npx tsx bin/omega-repl.ts
```

You'll see:
```
═══════════════════════════════════════════════════════════
Ω REPL — Omega Lisp with Oracle Protocol & Debugger
═══════════════════════════════════════════════════════════
Type Lisp expressions to evaluate.
Commands: :help :debug :step :run :goto :trace :break :quit

Ω>
```

### Your First Expression

Try some basic Lisp:

```lisp
Ω> (+ 1 2 3)
=> 6

Ω> (define (square x) (* x x))
=> square

Ω> (square 5)
=> 25
```

### Sessions: Persistent State Across Calls

For scripting and automation, use sessions. A session preserves your definitions across separate invocations:

```bash
# Define a function
npx tsx bin/omega-repl.ts --session mywork --cmd "(define (double x) (* x 2))"
=> double

# Use it later (in a separate call)
npx tsx bin/omega-repl.ts --session mywork --cmd "(double 21)"
=> 42

# Check what's defined
npx tsx bin/omega-repl.ts --session mywork --cmd ":defs"
(define (double x) (* x 2))
```

Sessions are stored in `~/.omega-sessions/` and persist until you delete them.

## Chapter 2: LLM Calls as Functions

### The Basic LLM Call

In OmegaLLM, calling a language model is an *effect*—a controlled side-effect that the runtime manages. The simplest form is `infer.op`:

```lisp
Ω> (effect infer.op "What is the capital of France?")
=> "Paris"
```

That's it. The string is sent to the LLM, and the response comes back as a value.

### Building Prompts with Lists

For dynamic prompts, use `list` to concatenate strings:

```lisp
Ω> (define city "Tokyo")
=> city

Ω> (effect infer.op (list "What country is " city " in?"))
=> "Japan"
```

When the payload is a list of strings, OmegaLLM automatically concatenates them into a single prompt.

### Defining LLM Functions

Here's where OmegaLLM shines. You can wrap LLM calls in ordinary functions:

```lisp
(define (translate-to-french text)
  (effect infer.op (list "Translate to French: " text)))
```

Now `translate-to-french` is a regular function that happens to call an LLM:

```lisp
Ω> (translate-to-french "hello")
=> "bonjour"

Ω> (translate-to-french "goodbye")
=> "au revoir"
```

### LLM Predicates

Functions that return yes/no answers are particularly useful:

```lisp
(define (is-edible? thing)
  (eq? "yes" (effect infer.op (list "Is " thing " edible? Answer only yes or no."))))
```

Now you have a semantic predicate:

```lisp
Ω> (is-edible? "apple")
=> #t

Ω> (is-edible? "laptop")
=> #f
```

### Practical Note: Handling Output Variations

LLMs may return their answers in slightly different formats. When asking for "yes or no", the response might be `"yes"` (a string), `"Yes"`, or even `#t` (parsed as a boolean). For robust predicates, define a helper:

```lisp
(define (truthy? x)
  (if (eq? x #t) #t (eq? x "yes")))

(define (is-edible? thing)
  (truthy? (effect infer.op (list "Is " thing " edible? Answer yes or no."))))
```

This handles both string and boolean responses consistently.

## Chapter 3: Functional Composition

### Higher-Order Functions with LLMs

Since LLM functions are ordinary functions, they work with `map`, `filter`, and other higher-order functions.

First, define these utilities (they're not built-in):

```lisp
(define (map f lst)
  (if (null? lst)
      (list)
      (cons (f (car lst)) (map f (cdr lst)))))

(define (filter pred lst)
  (if (null? lst)
      (list)
      (if (pred (car lst))
          (cons (car lst) (filter pred (cdr lst)))
          (filter pred (cdr lst)))))
```

### Mapping LLM Functions

Now you can apply an LLM function to every element of a list:

```lisp
Ω> (define (to-french word)
     (effect infer.op (list "Translate to French (one word only): " word)))
=> to-french

Ω> (map to-french (list "hello" "goodbye" "thanks"))
=> ("bonjour" "au revoir" "merci")
```

Each element triggered a separate LLM call. The results came back as a list.

### Filtering with LLM Predicates

Combine `filter` with an LLM predicate:

```lisp
Ω> (define (is-food? x)
     (eq? "yes" (effect infer.op (list "Is " x " a food? Answer only yes or no."))))
=> is-food?

Ω> (filter is-food? (list "apple" "chair" "pizza" "laptop" "banana"))
=> ("apple" "pizza" "banana")
```

The LLM evaluated each item and kept only the foods.

### Pipelines

Chain operations together:

```lisp
; Filter to foods, then translate to French
Ω> (map to-french (filter is-food? (list "apple" "car" "bread" "computer")))
=> ("pomme" "pain")
```

This pipeline:
1. Filters the list, keeping only edible items (apple, bread)
2. Translates each to French

### Function Composition

Create new functions by composing existing ones:

```lisp
(define (compose f g)
  (lambda (x) (f (g x))))

(define (to-spanish text)
  (effect infer.op (list "Translate to Spanish (one word only): " text)))

; English -> French -> Spanish
(define french-to-spanish (compose to-spanish to-french))

Ω> (french-to-spanish "hello")
=> "hola"
```

The word "hello" became "bonjour" (French), then "hola" (Spanish).

## Chapter 4: Higher-Order LLM Functions

### Classifier Factories

You can write functions that *return* LLM functions:

```lisp
(define (make-classifier category)
  (lambda (text)
    (effect infer.op (list "Is " text " " category "? Answer yes or no."))))
```

Now create specialized classifiers:

```lisp
Ω> (define is-positive (make-classifier "positive sentiment"))
=> is-positive

Ω> (define is-technical (make-classifier "technical content"))
=> is-technical

Ω> (is-positive "I love this product!")
=> "yes"

Ω> (is-positive "This is terrible")
=> "no"

Ω> (is-technical "The API uses REST endpoints")
=> "yes"
```

### Parameterized Prompts

This pattern lets you create families of related LLM functions:

```lisp
(define (make-translator target-language)
  (lambda (text)
    (effect infer.op (list "Translate to " target-language ": " text))))

(define to-german (make-translator "German"))
(define to-japanese (make-translator "Japanese"))
(define to-italian (make-translator "Italian"))

Ω> (to-german "good morning")
=> "guten Morgen"

Ω> (to-japanese "thank you")
=> "ありがとう"
```

## Chapter 5: Nondeterministic Search

### The AMB Operator

OmegaLLM includes `amb` (ambiguous choice), a powerful operator from the Lisp tradition. It lets you explore multiple possibilities with automatic backtracking.

```lisp
; Choose from alternatives
Ω> (effect amb.choose (list (lambda () 1) (lambda () 2) (lambda () 3)))
=> 1
```

By itself, `amb.choose` just picks the first option. The magic happens when you add constraints.

### Constraints with Require

Define a `require` function that fails if a condition isn't met:

```lisp
(define (require condition)
  (if condition #t (effect amb.fail "constraint failed")))
```

Now combine them:

```lisp
Ω> (begin
     (define x (effect amb.choose (list (lambda () 1)
                                        (lambda () 2)
                                        (lambda () 3))))
     (require (> x 1))
     x)
=> 2
```

What happened:
1. `amb.choose` tried x=1
2. `(require (> x 1))` failed (1 is not > 1)
3. System backtracked, tried x=2
4. `(require (> x 1))` passed
5. Returned 2

### Semantic Constraint Satisfaction

Here's the killer feature: combine AMB with LLM predicates for *semantic search*:

```lisp
Ω> (begin
     (define x (effect amb.choose (list (lambda () "car")
                                        (lambda () "laptop")
                                        (lambda () "pizza"))))
     (require (is-edible? x))
     x)
=> "pizza"
```

The system:
1. Tried "car" → LLM said not edible → backtracked
2. Tried "laptop" → LLM said not edible → backtracked
3. Tried "pizza" → LLM said edible → returned "pizza"

This is **constraint satisfaction with semantic predicates**—something no other LLM framework provides out of the box.

### Multiple Constraints

Chain multiple semantic requirements:

```lisp
(define (is-french-word? x)
  (eq? "yes" (effect infer.op (list "Is '" x "' a French word? Answer only yes or no."))))

Ω> (begin
     (define word (effect amb.choose (list (lambda () "apple")
                                           (lambda () "pomme")
                                           (lambda () "car")
                                           (lambda () "voiture"))))
     (require (is-edible? word))
     (require (is-french-word? word))
     word)
=> "pomme"
```

Found a word that is BOTH edible AND French.

## Chapter 6: Multi-Shot Sampling

### The search.op Effect

For probabilistic reasoning, use `search.op` to sample multiple responses:

```lisp
Ω> (effect search.op "Pick a random fruit: apple, banana, or cherry")
```

This returns a *distribution*—a collection of samples with weights:

```json
{
  "tag": "Dist",
  "support": [
    {"v": {"denotation": "apple"}, "w": 1},
    {"v": {"denotation": "apple"}, "w": 1},
    {"v": {"denotation": "banana"}, "w": 1},
    {"v": {"denotation": "cherry"}, "w": 1},
    ...
  ],
  "meta": {"kind": "search", "note": "n=8"}
}
```

By default, it takes 8 samples. You can see which answers the LLM favors.

### Controlling Sample Count

Pass a map with an "n" key to control sampling:

```lisp
; This is the payload format for specifying sample count
(effect search.op (map ("n" 16) ("prompt" "Pick a color")))
```

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

## Chapter 8: The Debugger

### Time-Travel Debugging

OmegaLLM includes a step debugger with time-travel capabilities.

### Loading an Expression

```lisp
Ω> :debug (+ (* 2 3) (* 4 5))
Debug session started. Use :step to begin stepping.

─── Step 0 ───
Control: Expr: Begin(1 exprs)
Stack depth: 0
```

### Stepping Through Execution

```lisp
Ω> :step 5
Step 5: Expr: App(...)
Stack depth: 2

Ω> :step 10
Step 15: Value: 26
DONE at step 15: 26
```

### Viewing the Trace

```lisp
Ω> :trace
Trace (16 steps recorded):
  [0] Expr: Begin(1 exprs) | stack=0
  [1] Expr: App(...) | stack=1
  [2] Expr: App(...) | stack=2
  ...
  [15] Value: 26 | stack=0 <-- current
```

### Time Travel

Jump to any previous state:

```lisp
Ω> :goto 5
Jumped to step 5.

─── Step 5 ───
Control: Expr: App(...)
Stack depth: 2
```

### Breakpoints

Set breakpoints on steps, expression types, or effects:

```lisp
Ω> :break step 10
Breakpoint 1 added: step = 10

Ω> :break expr If
Breakpoint 2 added: expr = If

Ω> :break effect infer.op
Breakpoint 3 added: effect = infer.op

Ω> :breaks
Breakpoints:
  1: step = 10 [enabled]
  2: expr = If [enabled]
  3: effect = infer.op [enabled]
```

## Chapter 9: The Agentic REPL

### Asking Questions

The `:ask` command lets an LLM agent interact with the REPL, using tools to evaluate code:

```lisp
Ω> :ask What is the factorial of 10?
; --- agentic session begins
```

---

## Chapter 10: Full API Reference

### Core Effects

| Effect | Description | Example |
|--------|-------------|---------|
| `infer.op` | Single LLM call, returns answer | `(effect infer.op "question")` |
| `int.op` | LLM call, returns full Meaning | `(effect int.op "question")` |
| `search.op` | Multi-shot sampling, returns Dist | `(effect search.op "question")` |
| `amb.choose` | Nondeterministic choice | `(effect amb.choose (list thunks...))` |
| `amb.fail` | Trigger backtracking | `(effect amb.fail "reason")` |

### Stream Primitives

| Function | Description |
|----------|-------------|
| `list->stream` | Convert list to lazy stream |
| `stream->list` | Force N elements to a list |
| `stream-car` | Get first element |
| `stream-cdr` | Get rest (forced) |
| `stream-map` | Lazy map |
| `stream-filter` | Lazy filter |
| `stream-take` | Take first N elements |
| `stream-null?` | Check if empty |
| `the-empty-stream` | Empty stream constant |

### REPL Commands

| Command | Description |
|---------|-------------|
| `:help` | Show all commands |
| `:defs` | List definitions |
| `:env` | Show environment |
| `:debug (expr)` | Start debugging |
| `:step [N]` | Step N times |
| `:run` | Run to completion/breakpoint |
| `:goto N` | Jump to step N |
| `:trace` | Show execution trace |
| `:break TYPE COND` | Set breakpoint |
| `:breaks` | List breakpoints |
| `:ask QUESTION` | Ask LLM agent |
| `:traces` | List agent traces |
| `:quit` | Exit |

---

## Part II: Structure and Interpretation of Inference Programs

*In the spirit of Abelson and Sussman's masterwork, we now explore how the principles that govern computation also govern understanding.*

## Chapter 11: Semantic Procedures as Black Boxes

*Corresponding to SICP Section 1.1: The Elements of Programming*

### 11.1 Expressions and Their Meanings

SICP begins with a simple observation: programming languages provide means for combining simple ideas to form complex ideas. Every powerful language has three mechanisms for this:

1. **Primitive expressions** — the simplest entities
2. **Means of combination** — building compound elements from simpler ones
3. **Means of abstraction** — naming compound elements and manipulating them as units

In traditional programming, primitives are numbers, strings, and basic operations. In inference programming, we add a new primitive: the **semantic effect**.

```lisp
; A primitive semantic expression
(effect infer.op "What is the capital of France?")
=> "Paris"
```

This expression doesn't compute; it *asks*. The LLM—our oracle—provides an answer drawn from its understanding of the world. This is profoundly different from any other primitive. Numbers and strings are inert data; semantic effects invoke understanding.

### 11.2 Naming Semantic Operations

SICP emphasizes the critical importance of naming. Names let us refer to computational objects, building complex programs from simple, named pieces.

In inference programming, naming lets us capture *semantic capabilities*:

```lisp
(define capital-of
  (lambda (country)
    (effect infer.op (list "What is the capital of " country "?"))))

Ω> (capital-of "Japan")
=> "Tokyo"

Ω> (capital-of "Brazil")
=> "Brasília"
```

We have named something remarkable: the ability to answer geographic questions. The procedure `capital-of` is not a lookup table—it contains no data about capitals. It's a *semantic capability* that invokes understanding on demand.

Consider what we haven't had to specify: the format of the answer, handling of ambiguous cases (does Sudan have two capitals?), or what happens with fictional countries. The LLM handles all of this implicitly.

### 11.3 Evaluating Combinations

SICP describes how the interpreter evaluates combinations by:
1. Evaluating the subexpressions
2. Applying the operator to the operands

For semantic combinations, this process becomes fascinating:

```lisp
(effect infer.op (list "Translate to French: " (effect infer.op "Say hello")))
```

What happens here? First, the inner effect asks the LLM to generate a greeting. The LLM might respond "Hello!" or "Hi there!" or "Greetings!". Then the outer effect takes whatever greeting emerged and translates it.

The evaluation order matters. Unlike pure functions, semantic effects can produce different results on different evaluations. The combination `(list "Translate: " (effect infer.op "..."))` doesn't just combine strings—it orchestrates a conversation with the oracle.

### 11.4 Compound Semantic Procedures

SICP introduces compound procedures—user-defined procedures built from primitives. The same power applies to semantic procedures:

```lisp
(define (sentiment text)
  (effect infer.op (list "What is the sentiment of this text? Answer positive, negative, or neutral: " text)))

(define (summarize text)
  (effect infer.op (list "Summarize in one sentence: " text)))

(define (summarize-with-sentiment text)
  (list (summarize text) (sentiment text)))

Ω> (summarize-with-sentiment "I love this product! It exceeded all my expectations!")
=> ("The writer expresses enthusiasm about a product that surpassed their expectations." "positive")
```

Notice how `summarize-with-sentiment` composes two semantic operations. Each invokes the oracle independently, and their results are combined. We're building complex semantic capabilities from simpler ones—the essence of abstraction.

### 11.5 The Substitution Model for Semantic Computation

SICP introduces the substitution model: to apply a procedure, substitute the arguments for the parameters in the body. This model helps us understand how procedures work.

For semantic procedures, substitution works the same way:

```lisp
(define (formal-translation text lang)
  (effect infer.op (list "Translate to formal " lang ": " text)))

; Applying (formal-translation "hey" "German"):
; Substitute "hey" for text, "German" for lang:
; => (effect infer.op (list "Translate to formal " "German" ": " "hey"))
; => (effect infer.op "Translate to formal German: hey")
; => "Guten Tag"
```

The substitution model clarifies what happens with the prompts we construct. Arguments are substituted into the prompt template, forming the actual question posed to the oracle.

### 11.6 Conditional Semantic Logic

SICP covers conditional expressions: `if`, `cond`, and predicates. These allow programs to make decisions based on test results.

In inference programming, conditionals combine with semantic predicates to create intelligent routing:

```lisp
(define (is-question? text)
  (eq? "yes" (effect infer.op (list "Is this a question? yes or no: " text))))

(define (handle-input text)
  (if (is-question? text)
      (effect infer.op (list "Answer this question: " text))
      (effect infer.op (list "Acknowledge this statement: " text))))

Ω> (handle-input "What time is it?")
=> "I don't have access to the current time, but you can check your device."

Ω> (handle-input "I'm feeling tired today.")
=> "I hear you—fatigue can be tough. Take care of yourself."
```

The conditional doesn't just branch on syntax (looking for question marks). It branches on *meaning*—the LLM determines whether the input is semantically a question. This is decision-making at the level of understanding.

### 11.7 Example: Building a Semantic Classifier

SICP's extended example shows building square-root computation through iterative refinement. Let's build a semantic classifier through similar principled construction.

**Goal:** Classify customer feedback into categories: complaint, question, praise, or suggestion.

**Step 1:** Define the primitive classification:

```lisp
(define (classify-feedback text)
  (effect infer.op
    (list "Classify this customer feedback as exactly one of: complaint, question, praise, or suggestion: " text)))
```

**Step 2:** Improve robustness by normalizing output:

```lisp
(define (normalize-category cat)
  (cond
    ((string-contains? cat "complaint") 'complaint)
    ((string-contains? cat "question") 'question)
    ((string-contains? cat "praise") 'praise)
    ((string-contains? cat "suggestion") 'suggestion)
    (else 'unknown)))

(define (classify-feedback-robust text)
  (normalize-category (classify-feedback text)))
```

**Step 3:** Add confidence checking:

```lisp
(define (classify-with-confidence text)
  (let ((result (effect int.op  ; int.op returns full Meaning with confidence
                  (list "Classify: " text))))
    (if (> (meaning-confidence result) 0.8)
        (normalize-category (meaning-denotation result))
        'uncertain)))
```

We've built up a classifier through layers of abstraction, each improving on the previous. This is the SICP methodology applied to semantic computation.

### 11.8 Procedures as Semantic Black Boxes

The deepest insight of SICP 1.1 is that procedures are "black boxes." You use them without knowing how they work internally. The square root function takes a number and returns its root; whether it uses Newton's method or binary search is irrelevant.

With LLMs, this principle reaches its ultimate expression:

```lisp
(define (is-professional-greeting? text)
  (eq? "yes" (effect infer.op
    (list "Is this a professional business greeting? yes or no: " text))))
```

How does this function work? We cannot say. The LLM's process is opaque—billions of parameters, attention mechanisms, emergent behaviors. Yet it *works*:

```lisp
Ω> (is-professional-greeting? "Dear Mr. Smith, I hope this finds you well.")
=> #t

Ω> (is-professional-greeting? "yo dude whats crackin")
=> #f
```

No rules. No regex. No explicit encoding of what "professional" means. The LLM simply... knows.

Traditional abstraction barriers hide *algorithmic complexity*. Semantic abstraction barriers hide something deeper: they hide *conceptual complexity*. The concept of "professional" encompasses thousands of subtle social conventions, cultural contexts, and linguistic patterns. No programmer could enumerate them all. Yet the LLM has absorbed them from training, and our procedure harnesses that knowledge.

**This is the key insight: LLM procedures are black boxes that encapsulate understanding, not just algorithms.**

### 11.9 Exercises

**Exercise 11.1:** Define a procedure `(is-sarcastic? text)` that determines whether text is sarcastic. Test it on: "Oh great, another meeting" and "I'm excited about the meeting." How does the procedure handle edge cases like "I'm *so* excited about the meeting"?

**Exercise 11.2:** The procedure `is-professional-greeting?` uses English-centric notions of professionalism. Define `(is-professional-in? culture text)` that takes a culture parameter. Test with Japanese and American business conventions. What differences emerge?

**Exercise 11.3:** Can you define `is-professional-greeting?` without using an LLM? What rules would you need? How many edge cases would remain? This exercise illuminates what the LLM is actually providing.

**Exercise 11.4:** Using the substitution model, trace the evaluation of:
```lisp
(summarize-with-sentiment "This product is terrible and broke immediately.")
```
Show each step of substitution until you reach the oracle calls.

**Exercise 11.5:** Design a `(classify-email email-text)` procedure that routes emails to: urgent, routine, spam, or personal. Build it up through layers of abstraction as shown in section 11.7.

**Exercise 11.6:** SICP asks whether `(sqrt x)` or `(good-enough? guess x)` is the "real" square root procedure. In your classifier, which procedure is the "real" classifier—the one that calls the LLM, or the one that normalizes outputs? Discuss.

## Chapter 12: Inference Processes — Recursion and Iteration in Semantic Space

### 12.1 The Shape of Semantic Processes

SICP Chapter 1.2 reveals that procedures generate *processes*—patterns of execution that unfold over time. A recursive procedure may generate a recursive process (building up deferred operations) or an iterative process (carrying state forward). Understanding this distinction is crucial for writing efficient programs.

In semantic programming, the same distinction applies, but with a twist: **the processes we generate involve LLM calls**, and each call has cost—latency, money, and context window consumption. Understanding how inference processes unfold is essential for building systems that scale.

### 12.2 Linear Recursive Inference

Consider a procedure that "chains" understanding—each step builds on the previous:

```lisp
(define (explain-chain concept depth)
  (if (= depth 0)
      concept
      (effect infer.op
        (list "Explain this more simply: "
              (explain-chain concept (- depth 1))))))

; This generates a LINEAR RECURSIVE process:
; (explain-chain "quantum entanglement" 3)
;   → (explain (explain (explain "quantum entanglement")))
;   → Each explain waits for inner result
```

The process shape:
```
explain-chain(concept, 3)
  ↓ waits for...
  explain-chain(concept, 2)
    ↓ waits for...
    explain-chain(concept, 1)
      ↓ waits for...
      explain-chain(concept, 0) → returns concept
    ← simplifies result
  ← simplifies result
← simplifies result
```

Three LLM calls, but they're **sequential**—each waits for the previous. Total time = 3 × (LLM latency).

### 12.3 Linear Iterative Inference

Now consider an iterative version that carries state forward:

```lisp
(define (explain-iter concept depth result)
  (if (= depth 0)
      result
      (explain-iter concept
                    (- depth 1)
                    (effect infer.op
                      (list "Simplify: " result)))))

(define (explain-iterative concept depth)
  (explain-iter concept depth concept))
```

The process shape looks similar in total work, but there's a key difference: **no deferred operations**. At each step, we have the complete state. This matters for error recovery—if an LLM call fails, we can resume from the current state.

### 12.4 Tree Recursive Inference

Consider analyzing a text by breaking it into parts:

```lisp
(define (analyze-tree text)
  (if (short-enough? text)
      (effect infer.op (list "Analyze: " text))
      (let ((parts (split-in-half text)))
        (combine-analyses
          (analyze-tree (car parts))
          (analyze-tree (cadr parts))))))
```

This generates a **tree recursive process**:
```
              analyze(full-text)
             /                  \
    analyze(first-half)    analyze(second-half)
       /      \               /        \
    ...       ...           ...        ...
```

The key insight: **the branches are independent**. Unlike linear recursion, we can parallelize tree recursion. With proper concurrency (Chapter 22), tree recursive inference can be dramatically faster than linear.

### 12.5 Orders of Growth in LLM Calls

SICP introduces order of growth—how resource consumption scales with input size. For semantic processes:

| Process Type | LLM Calls | Time (sequential) | Time (parallel) |
|-------------|-----------|-------------------|-----------------|
| Linear recursive | O(n) | O(n) | O(n) |
| Linear iterative | O(n) | O(n) | O(n) |
| Tree recursive | O(2^n) | O(2^n) | O(n) |

Tree recursion seems expensive, but parallelization transforms O(2^n) time into O(n) depth. This is why understanding process shape matters.

### 12.6 Memoization for Semantic Efficiency

SICP shows how memoization can transform tree recursion (Fibonacci) from exponential to linear. In semantic programming, memoization is critical because LLM calls are expensive:

```lisp
(define semantic-cache (make-table))

(define (memoized-analyze text)
  (let ((cached (get semantic-cache text)))
    (if cached
        cached
        (let ((result (effect infer.op (list "Analyze: " text))))
          (put! semantic-cache text result)
          result))))
```

When analyzing overlapping text segments, memoization prevents redundant LLM calls.

### 12.7 The Cost Model for Inference

In traditional computing, we count operations. In inference programming, we count:

1. **LLM calls**: Each call has latency (~1-5 seconds) and cost (~$0.01-0.10)
2. **Token consumption**: Input and output tokens affect cost
3. **Context utilization**: Long contexts are expensive

A well-designed inference process minimizes all three while maximizing semantic value.

### 12.8 Exercises

**Exercise 12.1:** Write two versions of `(summarize-deeply text n)`—one linear recursive, one linear iterative. Trace the process shapes. Which is easier to debug if an LLM call fails at step 3 of 5?

**Exercise 12.2:** Implement tree recursive document analysis that splits a document into paragraphs, analyzes each, and combines results. Then add parallelization (using concepts from Chapter 22) and measure the speedup.

**Exercise 12.3:** The Fibonacci function has famously poor tree recursive performance. Design a "semantic Fibonacci"—where fib(n) is "the nth elaboration of an idea, building on the two previous elaborations." How do you memoize semantic content?

**Exercise 12.4:** SICP discusses the difference between a procedure and the process it generates. Can two different inference procedures generate identical semantic processes? Design an experiment to test this.

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

## Chapter 14: Semantic Data Abstraction

*Corresponding to SICP Section 2.1: Introduction to Data Abstraction*

### 14.1 The Discipline of Data Abstraction

SICP Chapter 2 introduces one of programming's most powerful ideas: **data abstraction**. We separate *how* data is represented from *how* it is used. A rational number might be stored as a pair of integers, but users of the abstraction see only `(make-rat n d)`, `(numer r)`, and `(denom r)`.

This discipline has three components:
1. **Constructors** — procedures that create data objects
2. **Selectors** — procedures that extract components
3. **Abstraction barriers** — the principle that code should only use the agreed interface, never reach into representations

In semantic programming, this discipline becomes essential because meanings are complex, multifaceted objects. We need structured ways to create, access, and validate them.

### 14.2 Wishful Thinking: Designing Top-Down

SICP advocates "wishful thinking" — designing with procedures you wish existed, then implementing them. This top-down approach clarifies what operations you truly need.

Let's design a **semantic document** abstraction. We'll wish for operations, then implement them:

```lisp
; Wishful thinking: assume these exist
(make-semantic-doc title content)    ; Constructor
(doc-title doc)                      ; Selector
(doc-content doc)                    ; Selector
(doc-summary doc)                    ; Computed property
(doc-sentiment doc)                  ; Computed property
(valid-doc? doc)                     ; Validator
```

Now we can write code that *uses* documents before deciding how to represent them:

```lisp
(define (summarize-document doc)
  (list (doc-title doc)
        (doc-summary doc)
        (doc-sentiment doc)))

(define (filter-positive-docs docs)
  (filter (lambda (d) (eq? 'positive (doc-sentiment d))) docs))
```

This code doesn't know whether documents are lists, records, or something else. It only knows the interface.

### 14.3 Implementing the Abstraction

Now we implement our wished-for operations:

```lisp
; Representation: a document is a list (title content)
(define (make-semantic-doc title content)
  (list title content))

(define (doc-title doc) (car doc))
(define (doc-content doc) (cadr doc))

; Computed via LLM
(define (doc-summary doc)
  (effect infer.op (list "Summarize in one sentence: " (doc-content doc))))

(define (doc-sentiment doc)
  (let ((result (effect infer.op
                  (list "Is the sentiment positive, negative, or neutral? " (doc-content doc)))))
    (cond
      ((string-contains? result "positive") 'positive)
      ((string-contains? result "negative") 'negative)
      (else 'neutral))))
```

The calling code doesn't change. We've implemented the abstraction, and all uses continue to work.

### 14.4 Abstraction Barriers

SICP emphasizes abstraction barriers: layers of code should only communicate through defined interfaces. Violating barriers creates brittle systems.

```
┌────────────────────────────────────────────┐
│     Programs that USE documents            │
│  (summarize-document, filter-positive)     │
├────────────────────────────────────────────┤ ← Barrier: document interface
│     Document operations                    │
│  (make-doc, doc-title, doc-summary...)     │
├────────────────────────────────────────────┤ ← Barrier: representation
│     Underlying representation              │
│  (lists, cons cells)                       │
├────────────────────────────────────────────┤ ← Barrier: semantic primitives
│     LLM effects                            │
│  (effect infer.op ...)                     │
└────────────────────────────────────────────┘
```

A well-designed system respects these barriers. If we change document representation from lists to vectors, only the middle layer changes. If we change LLM providers, only the bottom layer changes.

### 14.5 Semantic Types as Contracts

Traditional type systems ask: "Is this data the right *shape*?" Semantic type systems ask: "Is this data the right *meaning*?"

We can define semantic type validators:

```lisp
(define (semantic-type? type-name value)
  (eq? "yes" (effect infer.op
    (list "Is this a valid " type-name "? Answer yes or no: " value))))

Ω> (semantic-type? "professional business greeting"
                   "Dear Mr. Smith, I hope this finds you well.")
=> #t

Ω> (semantic-type? "haiku"
                   "An old silent pond / A frog jumps into the pond / Splash! Silence again")
=> #t

Ω> (semantic-type? "polite refusal"
                   "No way, forget it!")
=> #f
```

The LLM is our type checker—determining membership based on *meaning*, not structure.

### 14.6 Constructors with Semantic Validation

SICP shows constructors that enforce data integrity. We can enforce *semantic* integrity:

```lisp
(define (make-professional-email greeting body closing)
  (if (and (semantic-type? "professional greeting" greeting)
           (semantic-type? "professional closing" closing))
      (list 'email greeting body closing)
      (error "Invalid email format: components must be professional")))

Ω> (make-professional-email "Dear Team," "Here's the report" "Best regards,")
=> (email "Dear Team," "Here's the report" "Best regards,")

Ω> (make-professional-email "yo" "Here's the report" "later dude")
=> ERROR: Invalid email format: components must be professional
```

The constructor refuses semantically malformed data—not based on syntax, but on meaning.

### 14.7 Rich Semantic Data Types

We can define elaborate semantic types as data structures:

```lisp
(define (make-semantic-type name validator description)
  (list 'semantic-type name validator description))

(define (type-name t) (cadr t))
(define (type-validator t) (caddr t))
(define (type-description t) (cadddr t))

(define (type-check type value)
  ((type-validator type) value))

; Define a type for valid arguments
(define valid-argument-type
  (make-semantic-type
    'valid-argument
    (lambda (text)
      (eq? "yes" (effect infer.op
        (list "Does this text make a logical argument with premises and conclusion? yes or no: " text))))
    "A text that presents premises leading to a conclusion"))

Ω> (type-check valid-argument-type
     "All humans are mortal. Socrates is human. Therefore, Socrates is mortal.")
=> #t

Ω> (type-check valid-argument-type "I like pizza")
=> #f
```

### 14.8 Levels of Semantic Abstraction

Just as SICP builds layers (pairs → lists → trees), we can build semantic abstraction layers:

```lisp
; Level 0: Raw LLM calls
(effect infer.op "...")

; Level 1: Semantic primitives
(define (classify text) ...)
(define (summarize text) ...)
(define (translate text lang) ...)

; Level 2: Semantic data types
(define (make-document title content) ...)
(define (make-review product rating text) ...)

; Level 3: Domain objects
(define (make-customer-ticket issue priority customer) ...)
(define (make-research-paper abstract methods results) ...)

; Level 4: Applications
(define (customer-support-bot) ...)
(define (paper-reviewer) ...)
```

Each level builds on the one below, never reaching through barriers.

### 14.9 The Power of Semantic Abstraction

Consider what we can now express: a "professional email" is a semantic type. A "valid argument" is a semantic type. A "haiku" is a semantic type. A "grammatically correct sentence" is a semantic type.

```lisp
Ω> (semantic-type? "grammatically correct English sentence"
                   "Me want cookie")
=> #f

Ω> (semantic-type? "sentence with passive voice"
                   "The ball was thrown by John")
=> #t

Ω> (semantic-type? "culturally sensitive communication for Japanese audience"
                   "We demand immediate action!")
=> #f
```

Traditional type systems could never express these constraints. Semantic abstraction gives us types defined by understanding, not structure.

### 14.10 Exercises

**Exercise 14.1:** Design a `semantic-record` abstraction with constructor, selectors, and validators. A semantic record should have named fields where each field can have a semantic type constraint.

**Exercise 14.2:** Define `(make-haiku line1 line2 line3)` that validates syllable counts (5-7-5) using semantic type checking. What happens with edge cases like compound words?

**Exercise 14.3:** Create a "business proposal" type with sections (problem, solution, timeline, cost). Each section should have semantic constraints. Build a constructor that validates all constraints.

**Exercise 14.4:** SICP discusses how abstraction barriers enable change. Imagine changing `make-semantic-doc` to store documents differently. What code would change? What wouldn't?

**Exercise 14.5:** Can semantic types replace traditional types entirely? What are the tradeoffs in reliability, performance, and determinism? When would you prefer structural types?

**Exercise 14.6:** Design a semantic type hierarchy: `formal-text` ⊃ `business-letter` ⊃ `legal-contract`. How do you express that one type is a subtype of another?

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

## Chapter 17: Multiple Representations of Meaning

*Corresponding to SICP Section 2.4: Multiple Representations for Abstract Data*

### 17.1 The Problem of Multiple Representations

SICP Chapter 2.4 confronts a practical problem: the same abstract data can have multiple concrete representations. Complex numbers can be rectangular (real + imaginary) or polar (magnitude + angle). Both are legitimate; neither is canonical. The challenge is building systems that accommodate multiple representations without chaos.

In semantic programming, this problem is **fundamental and unavoidable**. The same meaning routinely appears in different forms:

| Representation | Expression |
|----------------|------------|
| Formal | "The precipitation probability exceeds 70%" |
| Casual | "It's probably gonna rain" |
| Poetic | "The heavens prepare their tears" |
| Technical | "P(rain) > 0.7" |
| Telegraphic | "Rain likely" |

These are not paraphrases—they're *different representations of identical semantic content*, each appropriate for different contexts, audiences, and purposes. A weather service API might emit the technical form; a poet crafting verse needs the lyrical form; a text message demands brevity.

### 17.2 The Importance of Representation Choice

Why does representation matter? Consider communicating to different audiences:

```lisp
(define core-meaning "The quarterly revenue decreased by 15%")

; To the board of directors
(define for-board
  (effect infer.op
    (list "Express for a board meeting: " core-meaning)))
=> "The company experienced a 15% revenue decline this quarter, requiring strategic review."

; To employees
(define for-employees
  (effect infer.op
    (list "Express reassuringly for employees: " core-meaning)))
=> "We had a challenging quarter, but we're adapting our approach."

; To investors
(define for-investors
  (effect infer.op
    (list "Express with analytical framing for investors: " core-meaning)))
=> "Q3 revenue contracted 15% QoQ, reflecting market headwinds."
```

The underlying fact is identical. The representations differ vastly. A system that can only produce one representation is brittle; a system that can produce any representation, on demand, is powerful.

### 17.3 Tagged Semantic Data

SICP's solution begins with **tagging**: attach a type marker to every datum. We know not just *what* the data is, but *which representation* it uses:

```lisp
; Constructors create tagged semantic data
(define (make-formal-text meaning)
  (list 'formal (effect infer.op
    (list "Express formally: " meaning))))

(define (make-casual-text meaning)
  (list 'casual (effect infer.op
    (list "Express casually: " meaning))))

(define (make-poetic-text meaning)
  (list 'poetic (effect infer.op
    (list "Express poetically: " meaning))))

; Selectors extract components
(define (text-register tagged) (car tagged))
(define (text-content tagged) (cadr tagged))

; Usage
Ω> (define weather-formal (make-formal-text "rain is likely"))
=> (formal "There is a high probability of precipitation.")

Ω> (define weather-casual (make-casual-text "rain is likely"))
=> (casual "Looks like it's gonna rain!")

Ω> (text-register weather-formal)
=> formal

Ω> (text-content weather-casual)
=> "Looks like it's gonna rain!"
```

The tag travels with the data. Any procedure receiving a tagged datum can inspect its representation and act accordingly.

### 17.4 Generic Semantic Operations

With tagged data, we can define **generic operations**—procedures that work correctly regardless of which representation they receive:

```lisp
(define (negate-meaning tagged-text)
  (let ((register (text-register tagged-text))
        (content (text-content tagged-text)))
    (list register
          (effect infer.op
            (list "Negate this, keeping the " (symbol->string register) " tone: " content)))))

Ω> (negate-meaning weather-formal)
=> (formal "There is a low probability of precipitation.")

Ω> (negate-meaning weather-casual)
=> (casual "Nah, probably won't rain.")

Ω> (negate-meaning (make-poetic-text "rain is likely"))
=> (poetic "The clouds disperse, withholding their silver benediction.")
```

The operation is *generic*: it preserves whatever register the input had. One procedure handles all representations.

More operations:

```lisp
(define (intensify-meaning tagged-text)
  (let ((register (text-register tagged-text))
        (content (text-content tagged-text)))
    (list register
          (effect infer.op
            (list "Make this more emphatic, keeping " (symbol->string register) " style: " content)))))

(define (soften-meaning tagged-text)
  (let ((register (text-register tagged-text))
        (content (text-content tagged-text)))
    (list register
          (effect infer.op
            (list "Make this gentler, keeping " (symbol->string register) " style: " content)))))

Ω> (intensify-meaning weather-formal)
=> (formal "Precipitation is virtually certain.")

Ω> (soften-meaning weather-formal)
=> (formal "There may be some possibility of precipitation.")
```

### 17.5 Register Conversion

Beyond operations that preserve register, we need operations that **convert** between registers—the semantic analog of converting rectangular to polar:

```lisp
(define (convert-register tagged-text new-register)
  (let ((content (text-content tagged-text)))
    (list new-register
          (effect infer.op
            (list "Rewrite in a " (symbol->string new-register) " style: " content)))))

Ω> (convert-register weather-formal 'casual)
=> (casual "Rain's coming!")

Ω> (convert-register weather-casual 'formal)
=> (formal "Precipitation is anticipated in the near term.")

Ω> (convert-register weather-casual 'poetic)
=> (poetic "Soon the sky shall weep.")
```

Conversion enables adapting content for different contexts on the fly.

### 17.6 Data-Directed Programming

SICP's key insight is **data-directed programming**: instead of embedding type checks in every operation, maintain a table that maps (operation, type) pairs to implementations:

```
             |  email       |  legal       |  technical
-------------|--------------|--------------|-------------
  summarize  |  email-sum   |  legal-sum   |  tech-sum
  expand     |  email-exp   |  legal-exp   |  tech-exp
  critique   |  email-crit  |  legal-crit  |  tech-crit
```

In OmegaLLM:

```lisp
; Create the operation table
(define semantic-ops (make-table))

; Install operations for email domain
(put! semantic-ops 'summarize 'email
  (lambda (text)
    (effect infer.op (list "Summarize this email in one line: " text))))

(put! semantic-ops 'expand 'email
  (lambda (text)
    (effect infer.op (list "Expand this into a full professional email: " text))))

; Install operations for legal domain
(put! semantic-ops 'summarize 'legal
  (lambda (text)
    (effect infer.op (list "Summarize legal implications and obligations: " text))))

(put! semantic-ops 'expand 'legal
  (lambda (text)
    (effect infer.op (list "Expand with standard legal clauses and qualifications: " text))))

; Install operations for technical domain
(put! semantic-ops 'summarize 'technical
  (lambda (text)
    (effect infer.op (list "Summarize technical content, preserving key specifications: " text))))

; Generic dispatch
(define (semantic-apply op domain text)
  ((get semantic-ops op domain) text))

; Use it
Ω> (semantic-apply 'summarize 'email "Dear Team, Please review the attached Q3 budget...")
=> "Budget review request for Q3"

Ω> (semantic-apply 'summarize 'legal "The party of the first part hereby agrees...")
=> "Binding agreement with mutual obligations"

Ω> (semantic-apply 'expand 'email "meeting tomorrow 2pm")
=> "Dear Colleague, I wanted to confirm our meeting scheduled for tomorrow at 2:00 PM..."
```

The generic `semantic-apply` doesn't know about emails or legal documents—it simply looks up the appropriate handler.

### 17.7 Additivity: The Power of the Table

The dispatch table enables **additivity**: adding new representations without modifying existing code. Anyone can install new domains:

```lisp
; Add medical domain later
(put! semantic-ops 'summarize 'medical
  (lambda (text)
    (effect infer.op (list "Summarize for a medical chart note: " text))))

; Add marketing domain
(put! semantic-ops 'summarize 'marketing
  (lambda (text)
    (effect infer.op (list "Summarize for marketing copy: " text))))

; Existing code works unchanged
Ω> (semantic-apply 'summarize 'medical "Patient presents with acute onset...")
=> "Chief complaint: acute presentation. Assessment pending."
```

No conditionals to update. No giant `cond` expression to extend. The table grows independently of the dispatch mechanism.

### 17.8 Message-Passing Style

SICP presents an alternative: **message passing**. Instead of external dispatch, the object itself handles operations:

```lisp
(define (make-semantic-object meaning register)
  (lambda (message)
    (cond
      ((eq? message 'content)
       (effect infer.op (list "Express in " (symbol->string register) " style: " meaning)))
      ((eq? message 'register) register)
      ((eq? message 'negate)
       (make-semantic-object
         (effect infer.op (list "Express the negation of: " meaning))
         register))
      ((eq? message 'intensify)
       (make-semantic-object
         (effect infer.op (list "Express more emphatically: " meaning))
         register))
      ((eq? message 'convert)
       (lambda (new-register)
         (make-semantic-object meaning new-register)))
      (else (error "Unknown message" message)))))

; Create an object
(define weather-obj (make-semantic-object "rain is likely" 'formal))

Ω> (weather-obj 'content)
=> "There is a high probability of precipitation."

Ω> (weather-obj 'register)
=> formal

Ω> ((weather-obj 'negate) 'content)
=> "There is a low probability of precipitation."

Ω> (((weather-obj 'convert) 'casual) 'content)
=> "Probably gonna rain!"
```

The object *is* its behavior. Operations are messages sent to objects, not functions applied to data.

### 17.9 Comparing Approaches

Data-directed and message-passing represent different decompositions:

**Data-directed** organizes by operation—all "summarize" implementations together, all "expand" implementations together. Adding new operations is easy (add a row to the table); adding new types requires updating all operations.

**Message-passing** organizes by type—all operations for "email" together. Adding new types is easy (define a new object); adding new operations requires updating all types.

For semantic programming, data-directed often wins: we frequently add new domains (email, legal, medical, marketing...) but rarely add fundamentally new operations. The table approach scales to many domains.

### 17.10 Coercion Between Semantic Domains

SICP introduces **coercion**: converting one type to another to enable operations. Semantic coercion converts between domains:

```lisp
; Define coercion procedures
(define (technical->layperson text)
  (effect infer.op
    (list "Explain to a non-technical person: " text)))

(define (layperson->technical text)
  (effect infer.op
    (list "Restate using technical terminology: " text)))

(define (formal->casual text)
  (effect infer.op
    (list "Rewrite in a casual, friendly tone: " text)))

(define (casual->formal text)
  (effect infer.op
    (list "Rewrite in formal business language: " text)))

; Coercion table
(define coercions (make-table))
(put! coercions 'technical 'layperson technical->layperson)
(put! coercions 'layperson 'technical layperson->technical)
(put! coercions 'formal 'casual formal->casual)
(put! coercions 'casual 'formal casual->formal)

(define (coerce text from-domain to-domain)
  ((get coercions from-domain to-domain) text))

Ω> (coerce "The API uses OAuth2 with JWT bearer tokens" 'technical 'layperson)
=> "The system uses a secure login method where you get a digital pass"

Ω> (coerce "The computer got confused and stopped" 'layperson 'technical)
=> "The system encountered an unhandled exception causing process termination"
```

Coercion enables cross-domain operations: if you have a technical operation but casual input, coerce first.

### 17.11 Exercises

**Exercise 17.1:** Implement a "semantic tower" analogous to SICP's numeric tower: casual → standard → formal → legal. Define coercion procedures between adjacent levels. Can you automatically coerce casual text to legal language through the chain?

**Exercise 17.2:** Create domain-specific summarizers for: news articles, scientific papers, social media posts, poetry. Test that identical content produces appropriately different summaries per domain.

**Exercise 17.3:** Build a "perspective converter" that restates arguments from different viewpoints: `(restate-as 'optimist text)`, `(restate-as 'pessimist text)`, `(restate-as 'neutral text)`. How does this relate to semantic representations?

**Exercise 17.4:** Implement the message-passing semantic object with additional messages: `'paraphrase`, `'summarize`, `'questions` (generates questions about the content). Test that objects properly maintain their register across transformations.

**Exercise 17.5:** SICP discusses the "tyranny of data types"—being forced to classify everything into rigid categories. Is there a "tyranny of registers" in semantic programming? Design a `'hybrid` register that blends properties of formal and casual. What challenges arise?

**Exercise 17.6:** Build an additive semantic system where new domains can be installed at runtime via configuration. Define a procedure `(install-domain! name summarize-prompt expand-prompt)` that registers a new domain in the dispatch table.

## Chapter 18: Generic Semantic Operations

*Corresponding to SICP Section 2.5: Systems with Generic Operations*

### 18.1 The Challenge of Generic Operations

SICP Chapter 2.5 develops **generic arithmetic**: operations like `add` that work seamlessly across integers, rationals, reals, and complex numbers. The user writes `(add x y)` without knowing—or caring—what types `x` and `y` are. The system dispatches to the appropriate implementation.

In semantic programming, generic operations face a richer challenge. We want operations that work across **any textual domain**:

```lisp
(define (summarize text) ...)      ; Works on emails, poems, contracts, recipes
(define (expand text) ...)         ; Works on tweets, abstracts, notes
(define (translate text lang) ...) ; Works on prose, dialogue, technical docs
(define (sentiment text) ...)      ; Works on reviews, articles, messages
```

These are **domain-transcending** operations. They don't know if the input is Shakespeare or a shopping list, yet they must produce sensible results for both.

### 18.2 Installing Operations in a Package

SICP organizes generic operations into **packages**—modules that install their implementations into a global table. For semantic programming:

```lisp
; The global operation table
(define semantic-op-table (make-table))

; Package for email domain
(define (install-email-package)
  (define (email-summarize text)
    (effect infer.op (list "Summarize this email in one line: " text)))
  (define (email-expand text)
    (effect infer.op (list "Expand into a full professional email: " text)))
  (define (email-formalize text)
    (effect infer.op (list "Make this email more formal: " text)))

  (put! semantic-op-table 'summarize 'email email-summarize)
  (put! semantic-op-table 'expand 'email email-expand)
  (put! semantic-op-table 'formalize 'email email-formalize)
  'email-package-installed)

; Package for legal domain
(define (install-legal-package)
  (define (legal-summarize text)
    (effect infer.op (list "Summarize legal implications and obligations: " text)))
  (define (legal-expand text)
    (effect infer.op (list "Expand with standard clauses and qualifications: " text)))
  (define (legal-formalize text)
    (effect infer.op (list "Express in formal legal language: " text)))

  (put! semantic-op-table 'summarize 'legal legal-summarize)
  (put! semantic-op-table 'expand 'legal legal-expand)
  (put! semantic-op-table 'formalize 'legal legal-formalize)
  'legal-package-installed)

; Install packages
(install-email-package)
(install-legal-package)
```

Each domain defines its own implementations; the table unifies them under common names.

### 18.3 Generic Dispatch

With operations installed, we define generic procedures that look up and dispatch:

```lisp
(define (apply-generic op domain . args)
  (let ((proc (get semantic-op-table op domain)))
    (if proc
        (apply proc args)
        (error "No method for" op domain))))

; Convenience wrappers
(define (summarize domain text)
  (apply-generic 'summarize domain text))

(define (expand domain text)
  (apply-generic 'expand domain text))

(define (formalize domain text)
  (apply-generic 'formalize domain text))

; Usage
Ω> (summarize 'email "Dear Team, Please review the Q3 budget projections...")
=> "Budget review request"

Ω> (summarize 'legal "The party of the first part hereby agrees...")
=> "Mutual obligations under contract"

Ω> (formalize 'email "hey can we chat tmrw?")
=> "Dear Colleague, I would like to schedule a meeting for tomorrow..."
```

The caller specifies *what* operation on *which* domain. The system finds the right implementation.

### 18.4 The Semantic Algebra

SICP notes that arithmetic operations obey algebraic laws: addition is commutative and associative; multiplication distributes over addition. Semantic operations have their own **quasi-algebraic** properties:

```lisp
; Summarize then expand approximates identity (lossy)
(define text "The quarterly sales report shows a 15% increase in revenue.")
Ω> (expand 'general (summarize 'general text))
=> "The sales report indicates revenue grew by approximately 15% over the quarter."

; Translate then translate back preserves meaning (approximately)
Ω> (same-meaning? text (translate (translate text "French") "English"))
=> #t  ; usually

; Sentiment is invariant under paraphrase
Ω> (eq? (sentiment text) (sentiment (paraphrase text)))
=> #t  ; positive remains positive

; Formality increases monotonically
Ω> (more-formal? (formalize 'general text) text)
=> #t  ; always
```

Unlike arithmetic, these properties hold **approximately** and **usually**, not **always** and **exactly**. This is the nature of semantic computation.

### 18.5 Cross-Domain Operations

What happens when we need to operate across domains? Consider combining an email with a legal document. We need **coercion**:

```lisp
; Coercion table
(define coercion-table (make-table))

(define (install-coercions)
  (put! coercion-table 'email 'formal
    (lambda (text) (effect infer.op (list "Rewrite as formal prose: " text))))
  (put! coercion-table 'casual 'formal
    (lambda (text) (effect infer.op (list "Rewrite in formal style: " text))))
  (put! coercion-table 'technical 'layperson
    (lambda (text) (effect infer.op (list "Explain for a general audience: " text))))
  (put! coercion-table 'layperson 'technical
    (lambda (text) (effect infer.op (list "Restate with technical precision: " text))))
  'coercions-installed)

(define (coerce text from-domain to-domain)
  (if (eq? from-domain to-domain)
      text
      (let ((coercer (get coercion-table from-domain to-domain)))
        (if coercer
            (coercer text)
            (error "No coercion" from-domain to-domain)))))

Ω> (coerce "The server's busted" 'casual 'formal)
=> "The server is experiencing a malfunction."

Ω> (coerce "API latency exceeds SLA thresholds" 'technical 'layperson)
=> "The system is responding slower than promised."
```

### 18.6 The Semantic Type Tower

SICP's **type tower** orders types by generality: integer ⊂ rational ⊂ real ⊂ complex. Higher types can represent lower types, but not vice versa.

Semantic domains form multiple towers. One tower orders by **formality**:

```
     Legal
       ↑
    Formal
       ↑
   Standard
       ↑
    Casual
       ↑
     Slang
```

Another orders by **abstraction**:

```
    Abstract ("Something happened")
         ↑
    General ("There was a problem")
         ↑
    Specific ("The server crashed")
         ↑
    Detailed ("Server 7 crashed at 14:32 due to OOM")
```

We can define raising operations that move up the tower:

```lisp
; Raise formality
(define (raise-formality text)
  (effect infer.op (list "Make this more formal: " text)))

; Raise abstraction
(define (raise-abstraction text)
  (effect infer.op (list "Make this more abstract and general: " text)))

; Lower operations (go down the tower)
(define (lower-formality text)
  (effect infer.op (list "Make this more casual: " text)))

(define (lower-abstraction text context)
  (effect infer.op (list "Make this more specific and detailed, given context: " context ". Text: " text)))

; Multi-level raise
(define (raise-formality-n text n)
  (if (= n 0) text
      (raise-formality-n (raise-formality text) (- n 1))))

Ω> (raise-formality-n "hey wanna grab lunch?" 2)
=> "Would you be available to join me for a meal?"
```

### 18.7 Operations Across the Tower

SICP shows how operations can work across the type tower by coercing operands to a common level. For semantic operations:

```lisp
; Combine two texts at their highest common formality
(define (semantic-combine text1 domain1 text2 domain2)
  (let ((target-domain (higher-formality domain1 domain2)))
    (let ((t1 (coerce-to-formality text1 domain1 target-domain))
          (t2 (coerce-to-formality text2 domain2 target-domain)))
      (effect infer.op (list "Combine these coherently: " t1 " AND " t2)))))

; Determine which domain is more formal
(define (higher-formality d1 d2)
  (let ((order '(slang casual standard formal legal)))
    (if (> (position d1 order) (position d2 order)) d1 d2)))

Ω> (semantic-combine "yo meeting cancelled" 'casual "pursuant to earlier discussions" 'formal)
=> "The meeting has been cancelled in accordance with prior discussions."
```

The system automatically raises the casual text to match the formal text before combining.

### 18.8 The Polynomial Analog: Structured Semantic Data

SICP culminates with polynomials—structured data that can be added, multiplied, and composed. The semantic analog: **structured arguments** that can be combined, contrasted, and synthesized:

```lisp
; A semantic argument has claims and evidence
(define (make-argument claim evidence)
  (list 'argument claim evidence))

(define (argument-claim arg) (cadr arg))
(define (argument-evidence arg) (caddr arg))

; Combine arguments (like polynomial addition)
(define (combine-arguments arg1 arg2)
  (make-argument
    (effect infer.op (list "Synthesize these claims: "
                           (argument-claim arg1) " AND " (argument-claim arg2)))
    (append (argument-evidence arg1) (argument-evidence arg2))))

; Strengthen argument (like polynomial multiplication)
(define (strengthen-argument arg additional-evidence)
  (make-argument
    (argument-claim arg)
    (cons additional-evidence (argument-evidence arg))))

; Counter an argument (like polynomial negation)
(define (counter-argument arg)
  (make-argument
    (effect infer.op (list "State the opposite position: " (argument-claim arg)))
    (map (lambda (e)
           (effect infer.op (list "Counter this evidence: " e)))
         (argument-evidence arg))))

Ω> (define arg1 (make-argument "Remote work improves productivity"
                                (list "Studies show fewer distractions"
                                      "Employees report better focus")))

Ω> (argument-claim (counter-argument arg1))
=> "In-office work improves productivity"
```

### 18.9 Generic Operations as Semantic Abstractions

The deepest insight from SICP 2.5: generic operations create **abstraction barriers** that separate interface from implementation. For semantic operations:

```
          User Code
              |
    (summarize domain text)
              |
     ┌────────┴────────┐
     |  Generic Layer  |  ← Knows only operation names
     └────────┬────────┘
              |
    dispatch table lookup
              |
     ┌────────┴────────┐
     | Implementation  |  ← Knows domain-specific prompts
     └────────┬────────┘
              |
         LLM call
```

The user never sees domain-specific prompts. The implementations never see the dispatch mechanism. Each layer is independent.

### 18.10 Combining Independent Semantic Systems

SICP shows how packages from different sources can coexist. Semantic packages can similarly be combined:

```lisp
; Install third-party packages
(install-medical-semantic-package)  ; From medical NLP team
(install-legal-semantic-package)    ; From legal tech vendor
(install-marketing-package)         ; From marketing tools

; All work through the same generic interface
Ω> (summarize 'medical "Patient presents with acute onset of dyspnea...")
=> "Acute respiratory distress, evaluation needed"

Ω> (summarize 'marketing "Our Q3 campaign achieved 150% of target impressions...")
=> "Q3 campaign exceeded goals"

; They don't interfere with each other
; They compose with standard operations
Ω> (translate (summarize 'medical report) "Spanish")
=> "Dificultad respiratoria aguda, se necesita evaluación"
```

### 18.11 Exercises

**Exercise 18.1:** Implement `(semantic-add text1 text2)` that combines two pieces of information. What algebraic properties should it have? Is it commutative? Associative? Test empirically.

**Exercise 18.2:** Define a "semantic inverse" operation. If `summarize` compresses, what's its inverse? Implement `expand` as the inverse and verify: `(same-meaning? text (expand (summarize text)))`. When does this break down?

**Exercise 18.3:** SICP discusses efficiency of generic operations. Define a "cost model" for semantic operations: how do you measure the efficiency of `(summarize (translate (expand text)))`? Consider token counts, latency, and meaning preservation.

**Exercise 18.4:** Build a complete "semantic polynomial" system: arguments with claims and evidence that can be added, multiplied (strengthened), negated (countered), and composed. Define `semantic-zero` (an empty argument) and `semantic-one` (a tautology).

**Exercise 18.5:** Implement the formality tower with automatic coercion. When combining texts of different formality levels, automatically raise both to the higher level before combining.

**Exercise 18.6:** Design a package system where new semantic domains can be installed at runtime from configuration files. Each domain specifies its prompts for summarize, expand, formalize, etc. Test by installing a "poetry" domain and a "code-review" domain.

## Chapter 19: Conversational State and Memory

*Corresponding to SICP Section 3.1: Assignment and Local State*

### 19.1 The Need for Local State

SICP Chapter 3.1 introduces a fundamental shift: from pure functional programming to programming with **assignment and local state**. Until now, we could model computation as the evaluation of expressions. With state, objects have *history*—their behavior depends not just on inputs, but on what happened before.

In semantic programming, state is not optional—it's essential. Consider dialogue:

```
Turn 1: "My name is Alex"
Turn 2: "Nice to meet you, Alex"  ← Requires remembering Turn 1
Turn 3: "How old are you?"
Turn 4: "I'm 25"
Turn 5: "So Alex is 25 years old" ← Requires remembering Turns 1 and 4
```

Without state, each turn would be isolated. The system couldn't remember "Alex" from Turn 1 when generating Turn 2. **Conversational memory** is the semantic analog of SICP's bank account balance.

### 19.2 Local State Variables

SICP's bank account uses a local variable `balance` modified by `set!`:

```lisp
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  ...)
```

The semantic analog: a **conversation object** with local history:

```lisp
(define (make-conversation)
  (let ((history '()))  ; Local state variable
    (define (add-turn speaker text)
      (set! history (cons (list speaker text) history)))

    (define (format-history)
      (string-join
        (reverse (map (lambda (turn)
                        (string-append (car turn) ": " (cadr turn)))
                      history))
        "\n"))

    (define (respond input)
      (let ((response (effect infer.op
                        (list "Given this conversation:\n"
                              (format-history)
                              "\n\nUser: " input
                              "\nAssistant:"))))
        (add-turn "User" input)
        (add-turn "Assistant" response)
        response))

    (define (dispatch msg)
      (cond
        ((eq? msg 'respond) respond)
        ((eq? msg 'history) (format-history))
        ((eq? msg 'clear) (lambda () (set! history '())))
        (else (error "Unknown message" msg))))

    dispatch))
```

Using the conversation:

```lisp
(define conv (make-conversation))

Ω> ((conv 'respond) "My favorite color is blue")
=> "That's a lovely choice! Blue is often associated with calm and trust."

Ω> ((conv 'respond) "What's my favorite color?")
=> "You mentioned your favorite color is blue."

Ω> ((conv 'respond) "Why do you think I like it?")
=> "You might appreciate blue for its calming qualities."

Ω> (conv 'history)
=> "User: My favorite color is blue
    Assistant: That's a lovely choice! Blue is often associated with calm and trust.
    User: What's my favorite color?
    Assistant: You mentioned your favorite color is blue.
    ..."
```

The object *remembers*. Each call to `respond` modifies `history`, and future responses depend on that accumulated history.

### 19.3 The Costs of Introducing Assignment

SICP devotes significant attention to the **costs** of assignment. Before assignment, we had referential transparency: an expression always evaluates to the same value. With assignment, this breaks:

```lisp
; Without assignment: (square 5) always returns 25
; With assignment: (withdraw 50) returns different values!
```

In semantic programming, the costs are analogous:

```lisp
; Pure function - always same output for same input
(define (translate-pure text lang)
  (effect infer.op (list "Translate to " lang ": " text)))

; Stateful version - output depends on history
(define (make-contextual-translator)
  (let ((previous-translations '()))
    (lambda (text lang)
      (let ((result (effect infer.op
                      (list "Given previous translations:\n"
                            (format previous-translations)
                            "\nTranslate consistently to " lang ": " text))))
        (set! previous-translations
              (cons (list text result) previous-translations))
        result))))
```

The pure version is simple to reason about. The stateful version produces more consistent translations across a document—but we can no longer predict output from input alone.

### 19.4 Identity and Change

SICP asks: what does it mean for two things to be "the same"? Before assignment, sameness was easy—two expressions are the same if they have the same value. With assignment, we need to distinguish:

- **Same value**: two accounts with $100 each
- **Same object**: two references to the same account

In semantic programming:

```lisp
(define conv1 (make-conversation))
(define conv2 (make-conversation))
(define conv3 conv1)  ; Same object as conv1

; conv1 and conv2 have the same structure, but different identity
((conv1 'respond) "Hello, my name is Alice")
((conv2 'respond) "Hello, my name is Bob")

; Now conv1 and conv2 have different histories
; conv1 and conv3 are the same object - conv3's history changed too!
Ω> (conv3 'history)
=> "User: Hello, my name is Alice..."  ; Same as conv1
```

When we ask "what's my name?" to `conv1`, it says "Alice". To `conv2`, it says "Bob". They started identical but diverged through mutation.

### 19.5 Semantic Objects with Local State

SICP builds objects with local state via message passing. Here's a semantic **persona** that maintains and evolves context:

```lisp
(define (make-persona base-personality)
  (let ((personality base-personality)
        (learned-facts '())
        (conversation-count 0))

    (define (add-context new-context)
      (set! personality (string-append personality "\n" new-context)))

    (define (learn fact)
      (set! learned-facts (cons fact learned-facts)))

    (define (ask question)
      (set! conversation-count (+ conversation-count 1))
      (effect infer.op
        (list personality
              "\n\nFacts you know: " (string-join learned-facts "; ")
              "\n\nConversation #" (number->string conversation-count)
              "\nQuestion: " question)))

    (lambda (msg)
      (cond
        ((eq? msg 'add-context) add-context)
        ((eq? msg 'learn) learn)
        ((eq? msg 'ask) ask)
        ((eq? msg 'facts) learned-facts)
        ((eq? msg 'count) conversation-count)))))

(define scientist (make-persona "You are a scientist who explains with precision."))
(define poet (make-persona "You are a poet who speaks in metaphor."))

Ω> ((scientist 'ask) "What is rain?")
=> "Rain is precipitation in the form of liquid water droplets..."

Ω> ((poet 'ask) "What is rain?")
=> "Rain is the sky's gentle weeping, a silver curtain..."

; Teach the scientist something
((scientist 'learn) "The user prefers simple explanations")

Ω> ((scientist 'ask) "What is photosynthesis?")
=> "Plants use sunlight to make food from air and water."  ; Simpler now
```

### 19.6 Mutation and Semantic Consistency

SICP warns about the dangers of mutation. In semantic programming, careless mutation can create **logical contradictions**:

```lisp
(define (make-knowledge-base)
  (let ((facts '()))

    (define (contradicts? new-fact)
      (eq? "yes" (effect infer.op
        (list "Do any of these facts contradict '" new-fact "'? "
              (string-join facts "; ") " Answer yes or no."))))

    (define (assert fact)
      (if (and (not (null? facts)) (contradicts? fact))
          (error "Contradiction detected: " fact)
          (begin (set! facts (cons fact facts)) 'ok)))

    (define (query question)
      (effect infer.op
        (list "Given these facts: " (string-join facts "; ")
              "\nAnswer: " question)))

    (lambda (msg)
      (cond
        ((eq? msg 'assert) assert)
        ((eq? msg 'query) query)
        ((eq? msg 'facts) facts)))))

(define kb (make-knowledge-base))

Ω> ((kb 'assert) "Alice is Bob's mother")
=> ok

Ω> ((kb 'assert) "Bob was born in 1950")
=> ok

Ω> ((kb 'assert) "Alice was born in 1980")
=> ERROR: Contradiction detected: Alice was born in 1980
```

The knowledge base **protects its consistency** by checking for contradictions before mutation.

### 19.7 Benefits of Assignment in Semantic Programming

Despite its costs, assignment enables powerful patterns:

**1. Caching (Memoization):** LLM calls are expensive. Cache results:

```lisp
(define (make-memoized-classifier)
  (let ((cache (make-table)))
    (lambda (text)
      (let ((cached (get cache text)))
        (if cached
            (begin (display "[cache hit] ") cached)
            (let ((result (effect infer.op (list "Classify as positive/negative: " text))))
              (put! cache text result)
              result))))))

(define classify (make-memoized-classifier))

Ω> (classify "Great product!")
=> "positive"           ; LLM call

Ω> (classify "Great product!")
[cache hit] => "positive"  ; No LLM call
```

**2. Accumulation:** Build up context over time:

```lisp
(define (make-summarizer)
  (let ((documents '()))
    (lambda (msg)
      (cond
        ((eq? msg 'add) (lambda (doc) (set! documents (cons doc documents))))
        ((eq? msg 'summarize-all)
         (effect infer.op
           (list "Summarize all these documents together:\n"
                 (string-join documents "\n---\n"))))))))

(define sum (make-summarizer))
((sum 'add) "First document about climate change...")
((sum 'add) "Second document about renewable energy...")
((sum 'add) "Third document about carbon capture...")

Ω> (sum 'summarize-all)
=> "Three documents discuss climate solutions: impacts, renewables, and carbon capture."
```

**3. Learning:** Adapt based on feedback:

```lisp
(define (make-adaptive-assistant)
  (let ((feedback-history '()))
    (lambda (msg)
      (cond
        ((eq? msg 'respond)
         (lambda (input)
           (effect infer.op
             (list "Previous feedback on your responses:\n"
                   (format-feedback feedback-history)
                   "\n\nRespond to: " input))))
        ((eq? msg 'feedback)
         (lambda (rating comment)
           (set! feedback-history
                 (cons (list rating comment) feedback-history))))))))
```

### 19.8 The Imperative Semantic Style

With assignment, we can write **imperative** semantic programs:

```lisp
(define (process-documents-imperatively docs)
  (let ((result ""))
    (for-each
      (lambda (doc)
        (let ((summary (effect infer.op (list "Summarize: " doc))))
          (set! result (string-append result summary "\n"))))
      docs)
    result))
```

Compare to the functional style:

```lisp
(define (process-documents-functionally docs)
  (string-join
    (map (lambda (doc) (effect infer.op (list "Summarize: " doc))) docs)
    "\n"))
```

Both produce the same result, but the functional version is easier to reason about and parallelize.

### 19.9 Exercises

**Exercise 19.1:** Implement a "therapist" object that maintains patient history, tracks emotional themes across sessions, and responds with awareness of the patient's journey. Test with a multi-turn dialogue spanning several "sessions."

**Exercise 19.2:** Build a `make-tutor` that tracks what a student has learned, what they've struggled with, and adapts explanations accordingly. If they struggled with concept A, explicitly connect it when teaching related concept B.

**Exercise 19.3:** SICP discusses "sameness" in the presence of mutation. Implement `semantic-equal?` that determines if two semantic objects have equivalent knowledge/state, even if they're different objects. When should this return true?

**Exercise 19.4:** Design a "semantic transaction" system that batches assertions to a knowledge base, validates the entire batch for consistency, and rolls back all changes if any contradiction is detected.

**Exercise 19.5:** Implement a conversation object that can "fork" into two independent conversations sharing initial history but diverging afterward. How does this relate to SICP's discussion of object identity?

**Exercise 19.6:** Build a memoized semantic function with an LRU (least recently used) cache that limits memory usage. When the cache is full, evict the oldest entries.

## Chapter 20: The Semantic Environment Model

### 20.1 How Meaning Acquires Context

SICP Chapter 3.2 introduces the environment model of evaluation—the mechanism by which variables acquire values. An environment is a sequence of frames, each containing bindings. When we evaluate a variable, we search frames from innermost to outermost.

In semantic programming, we have an analogous concept: **the context in which meaning is interpreted**. The same words mean different things in different contexts:

```lisp
; In a medical context
(effect infer.op "What does 'positive' mean in this context: The test came back positive")
=> "The test detected the presence of the condition being tested for"

; In an emotional context
(effect infer.op "What does 'positive' mean in this context: Stay positive during difficult times")
=> "Maintaining an optimistic or hopeful attitude"
```

The word "positive" has different meanings depending on the **semantic environment**.

### 20.2 Frames of Understanding

Just as SICP's environments have frames, semantic environments have layers of context:

```lisp
(define (make-semantic-environment)
  (let ((frames '()))
    (lambda (msg)
      (cond
        ((eq? msg 'extend)
         (lambda (context)
           (set! frames (cons context frames))))
        ((eq? msg 'frames) frames)
        ((eq? msg 'interpret)
         (lambda (text)
           (effect infer.op
             (list "Given these contextual layers (most specific first):\n"
                   (string-join frames "\n")
                   "\n\nInterpret: " text))))))))

(define env (make-semantic-environment))

Ω> ((env 'extend) "We are discussing software development")
Ω> ((env 'extend) "Specifically, we are in a code review")
Ω> ((env 'interpret) "This looks buggy")
=> "The code under review appears to contain defects or errors"
```

### 20.3 Lexical Scope for Prompts

SICP explains lexical scoping: a procedure's free variables are looked up in the environment where the procedure was *defined*, not where it's *called*. This is crucial for closures.

In semantic programming, we have **prompt closures**—functions that capture their semantic context:

```lisp
(define (make-domain-expert domain)
  (let ((context (string-append "You are an expert in " domain)))
    (lambda (question)
      ; The 'context' is captured from definition time
      (effect infer.op (list context "\nQuestion: " question)))))

(define legal-expert (make-domain-expert "contract law"))
(define medical-expert (make-domain-expert "cardiology"))

; Each expert "closes over" its domain context
Ω> (legal-expert "What is consideration?")
=> "Consideration is something of value exchanged between parties to form a valid contract"

Ω> (medical-expert "What is consideration?")
=> "I'm not sure what you mean by 'consideration' in a cardiology context. Could you clarify?"
```

The legal expert interprets "consideration" legally; the medical expert is confused because it's not a cardiology term. **Same question, different semantic environments, different interpretations.**

### 20.4 Dynamic Scope and Conversation

While lexical scope is usually preferred, SICP mentions dynamic scope—where variables are looked up in the *calling* environment. In semantic programming, conversation history creates dynamic context:

```lisp
(define conversation-context '())

(define (with-conversation-context utterance thunk)
  (let ((old-context conversation-context))
    (set! conversation-context (cons utterance conversation-context))
    (let ((result (thunk)))
      (set! conversation-context old-context)
      result)))

(define (interpret text)
  (effect infer.op
    (list "Conversation so far:\n"
          (string-join (reverse conversation-context) "\n")
          "\nInterpret: " text)))

Ω> (with-conversation-context "We're planning a surprise party"
     (lambda ()
       (interpret "Don't tell anyone!")))
=> "Keep the party plans secret from the person being surprised"
```

The interpretation depends on the *dynamic* conversation context at call time.

### 20.5 Environment Diagrams for Semantic Computation

SICP uses environment diagrams to visualize computation. For semantic computation:

```
Global Environment:
┌─────────────────────────────────┐
│ domain: "healthcare"            │
│ audience: "patients"            │
└─────────────────────────────────┘
           ↑
           │ (enclosing)
           │
Local Frame (procedure call):
┌─────────────────────────────────┐
│ topic: "diabetes management"    │
│ tone: "reassuring"              │
└─────────────────────────────────┘
           ↑
           │ (enclosing)
           │
Innermost Frame (current context):
┌─────────────────────────────────┐
│ question: "What about sugar?"   │
└─────────────────────────────────┘

Interpretation: Combine all frames to form the complete semantic context
=> "In healthcare, explaining diabetes management to patients in a reassuring tone,
    answering their question about sugar intake"
```

### 20.6 The Frame Problem in Semantics

SICP's environment model solves the problem of "where do variables come from?" In AI, there's a famous "frame problem": how do you know what context is relevant?

Semantic environments face this too:

```lisp
; Too little context - ambiguous
(effect infer.op "Is it safe?")
=> "I need more context to answer this question"

; Too much context - confused
(effect infer.op "Given: global economics, Renaissance art, quantum physics,
                  cooking techniques, and sports statistics. Is it safe?")
=> "I'm not sure which domain you're asking about"

; Just right - focused
(effect infer.op "In the context of food handling, is leaving milk out overnight safe?")
=> "No, milk should not be left at room temperature for more than 2 hours"
```

The art of semantic programming is constructing environments that provide **sufficient context without noise**.

### 20.7 Exercises

**Exercise 20.1:** Implement a semantic environment with `lookup` that searches frames for relevant context. Given a question about "inheritance," it should find whether we're in a legal, programming, or biological context.

**Exercise 20.2:** Create a "semantic closure" that captures conversational context at definition time. Compare with a dynamic version. Show an example where they produce different results.

**Exercise 20.3:** SICP's environment model enables recursive procedures. Design a recursive semantic procedure where inner calls see the accumulated context of outer calls.

**Exercise 20.4:** The frame problem asks: what changes and what stays the same? Implement a `semantic-update` that modifies only relevant parts of context while preserving the rest.

---

## Chapter 21: Mutable Semantic Structures

### 21.1 The Costs of Change

SICP Chapter 3.3 explores mutable data structures—queues, tables, and circuits that change over time. Mutation introduces complexity: we must think about *when* things happen, not just *what* they compute.

In semantic programming, mutable structures are essential for:
- Building up knowledge bases
- Maintaining conversation history
- Caching expensive LLM results
- Tracking entity state across dialogue

But semantic mutation has unique challenges: **how do you maintain consistency when meanings can conflict?**

### 21.2 Mutable Semantic Tables

The simplest mutable structure: a table mapping keys to meanings:

```lisp
(define (make-semantic-table)
  (let ((bindings '()))
    (lambda (msg)
      (cond
        ((eq? msg 'put)
         (lambda (key value)
           (set! bindings (cons (cons key value) bindings))))
        ((eq? msg 'get)
         (lambda (key)
           (let ((found (assoc key bindings)))
             (if found (cdr found) #f))))
        ((eq? msg 'update)
         (lambda (key updater)
           (let ((old ((self 'get) key)))
             ((self 'put) key (updater old)))))))))

(define memory (make-semantic-table))

Ω> ((memory 'put) 'user-name "Alice")
Ω> ((memory 'put) 'user-preference "formal")
Ω> ((memory 'get) 'user-name)
=> "Alice"
```

### 21.3 Semantic Queues: Ordering Inference

Queues are essential for processing work in order. A semantic queue holds inference tasks:

```lisp
(define (make-inference-queue)
  (let ((front '()) (rear '()))
    (lambda (msg)
      (cond
        ((eq? msg 'enqueue)
         (lambda (task)
           (set! rear (cons task rear))))
        ((eq? msg 'dequeue)
         (if (null? front)
             (if (null? rear)
                 (error "Queue empty")
                 (begin
                   (set! front (reverse rear))
                   (set! rear '())
                   ((self 'dequeue))))
             (let ((item (car front)))
               (set! front (cdr front))
               item)))
        ((eq? msg 'empty?) (and (null? front) (null? rear)))))))

; Queue of analysis tasks
(define tasks (make-inference-queue))
((tasks 'enqueue) "Analyze sentiment of document A")
((tasks 'enqueue) "Extract entities from document B")
((tasks 'enqueue) "Summarize document C")

; Process in order
(define (process-queue q)
  (if ((q 'empty?))
      'done
      (begin
        (effect infer.op ((q 'dequeue)))
        (process-queue q))))
```

### 21.4 Semantic Graphs: Entities and Relations

Real-world knowledge forms graphs—entities connected by relationships:

```lisp
(define (make-knowledge-graph)
  (let ((entities (make-semantic-table))
        (relations '()))
    (lambda (msg)
      (cond
        ((eq? msg 'add-entity)
         (lambda (id properties)
           ((entities 'put) id properties)))
        ((eq? msg 'add-relation)
         (lambda (from rel to)
           (set! relations (cons (list from rel to) relations))))
        ((eq? msg 'query)
         (lambda (question)
           (effect infer.op
             (list "Given this knowledge graph:\n"
                   "Entities: " (format-entities entities)
                   "\nRelations: " (format-relations relations)
                   "\n\nAnswer: " question))))))))

(define kg (make-knowledge-graph))

Ω> ((kg 'add-entity) 'alice '((type . person) (role . engineer)))
Ω> ((kg 'add-entity) 'acme '((type . company) (industry . tech)))
Ω> ((kg 'add-relation) 'alice 'works-at 'acme)
Ω> ((kg 'query) "Where does Alice work?")
=> "Alice works at Acme, a tech company"
```

### 21.5 Constraint Propagation in Semantic Networks

SICP describes constraint propagation networks where values flow between cells. In semantic networks, meanings propagate:

```lisp
(define (make-semantic-constraint source target constraint-fn)
  ; When source changes, update target through constraint
  (lambda (new-source-value)
    (let ((derived (constraint-fn new-source-value)))
      (set-target! target derived))))

; Example: formality constraint
; If user-preference changes, tone should update
(define (update-tone preference)
  (effect infer.op
    (list "What tone matches preference: " preference)))

Ω> (set-preference! 'formal)
; Propagates → tone becomes "Use complete sentences, avoid contractions"

Ω> (set-preference! 'casual)
; Propagates → tone becomes "Relaxed, conversational, contractions OK"
```

### 21.6 The Challenge of Semantic Consistency

Unlike numeric constraints, semantic constraints can be subtle:

```lisp
(define (add-fact-with-check kb fact)
  (let ((existing ((kb 'facts))))
    (let ((contradiction? (effect infer.op
           (list "Does this new fact contradict any existing facts?\n"
                 "Existing: " (string-join existing "\n")
                 "\nNew: " fact
                 "\nAnswer yes or no with explanation."))))
      (if (starts-with? contradiction? "yes")
          (error (string-append "Contradiction: " contradiction?))
          ((kb 'add) fact)))))

Ω> (add-fact-with-check kb "Alice is 30 years old")
=> ok

Ω> (add-fact-with-check kb "Alice is a teenager")
=> ERROR: Contradiction: yes, a 30-year-old cannot be a teenager
```

### 21.7 Temporal Semantics: State Over Time

Semantic state changes over time. We can model this:

```lisp
(define (make-temporal-entity id)
  (let ((history '()))
    (lambda (msg)
      (cond
        ((eq? msg 'update)
         (lambda (timestamp state)
           (set! history (cons (cons timestamp state) history))))
        ((eq? msg 'at)
         (lambda (timestamp)
           ; Find state at given time
           (find-state-at timestamp history)))
        ((eq? msg 'current)
         (if (null? history)
             #f
             (cdar history)))))))

(define project (make-temporal-entity 'project-x))

((project 'update) "2024-01" '((status . "planning") (team . 3)))
((project 'update) "2024-03" '((status . "development") (team . 5)))
((project 'update) "2024-06" '((status . "testing") (team . 4)))

Ω> ((project 'at) "2024-02")
=> ((status . "planning") (team . 3))

Ω> ((project 'current))
=> ((status . "testing") (team . 4))
```

### 21.8 Exercises

**Exercise 21.1:** Implement a semantic table with *versioned* values—every `put` creates a new version, and you can query historical values. Use it to track how understanding of a concept evolved.

**Exercise 21.2:** Build a circular semantic structure: two entities that reference each other. For example, "Alice manages Bob" and "Bob reports to Alice" should be represented without infinite regress.

**Exercise 21.3:** SICP builds a digital circuit simulator. Design a "semantic circuit" where "signals" are meanings that propagate through "gates" that transform them (e.g., a "formalize" gate, a "summarize" gate).

**Exercise 21.4:** Implement undo/redo for a knowledge base. After adding facts, allow rolling back to previous states while maintaining consistency.

---

## Chapter 22: Concurrent Inference

*Corresponding to SICP Section 3.4: Concurrency: Time Is of the Essence*

### 22.1 The Nature of Time in Semantic Programming

SICP Chapter 3.4 addresses a fundamental challenge: when multiple processes share state, the *order* of events matters. The same operations, interleaved differently, produce different results. SICP introduces serializers, mutexes, and careful protocols to manage concurrent access.

In semantic programming, concurrency is not merely useful—it is **essential**. LLM calls are slow, often taking seconds each. If we need to analyze 100 documents, sequential processing is prohibitive:

```
100 documents × 2 seconds per call = 200 seconds (3+ minutes)
```

With parallelism:

```
100 documents / 10 parallel workers = 20 seconds
```

But concurrent LLM access raises unique challenges. What if two calls about the same topic give inconsistent answers? How do we merge results? What does "consistency" even mean for semantic data?

### 22.2 Parallel Map for Bulk Processing

The simplest concurrency pattern: parallel map over a collection:

```lisp
; Sequential - painfully slow
(define (classify-all-seq documents)
  (map classify documents))  ; 100 docs × 2s = 200s

; Parallel - acceptably fast
(define (classify-all-par documents)
  (parallel-map classify documents))  ; 100 docs / 10 parallel = 20s
```

OmegaLLM's fiber system enables this. Fibers are lightweight concurrent tasks:

```lisp
(define (parallel-map f lst)
  (let ((fibers (map (lambda (x)
                       (effect fiber.spawn (lambda () (f x))))
                     lst)))
    (map (lambda (fib) (effect fiber.join fib)) fibers)))

; Use it
Ω> (parallel-map summarize (list "Document A..." "Document B..." "Document C..."))
=> ("Summary of A" "Summary of B" "Summary of C")
```

All three LLM calls execute concurrently. Total wall-clock time: the longest single call, not the sum.

### 22.3 The Serialization Problem

SICP emphasizes **serialization**: ensuring certain operations don't interleave dangerously. Consider concurrent updates to a knowledge base:

```lisp
; Thread 1: "Alice is Bob's manager"
; Thread 2: "Bob is Alice's manager"

; If both succeed, we have a contradiction: circular management!
```

Without protection, the interleaving might be:

```
1. Thread 1 checks: no contradiction ✓
2. Thread 2 checks: no contradiction ✓
3. Thread 1 adds: "Alice is Bob's manager"
4. Thread 2 adds: "Bob is Alice's manager"
5. Knowledge base now contradicts itself!
```

### 22.4 Serializers for Semantic Consistency

SICP introduces **serializers**: mechanisms that ensure certain operations don't overlap. The semantic equivalent:

```lisp
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (proc)
      (lambda args
        (effect mutex.acquire mutex)
        (let ((result (apply proc args)))
          (effect mutex.release mutex)
          result)))))

(define (make-serialized-kb)
  (let ((kb (make-knowledge-base))
        (protect (make-serializer)))
    (define (protected-assert fact)
      ((protect (lambda ()
        (if (contradicts? fact (kb 'facts))
            (error "Contradiction detected")
            ((kb 'assert) fact))))))
    (lambda (msg)
      (cond
        ((eq? msg 'assert) protected-assert)
        ((eq? msg 'query) (kb 'query))  ; Queries can run in parallel
        (else (kb msg))))))

(define kb (make-serialized-kb))
```

Now concurrent assertions are serialized—only one runs at a time, preventing the race condition.

### 22.5 Singleflight: Preventing Duplicate Work

When processing overlapping content, we might call the LLM multiple times for the same question. The **singleflight** pattern prevents this:

```lisp
(define (make-singleflight)
  (let ((in-flight (make-table))
        (results (make-table)))
    (lambda (key compute)
      (cond
        ; Already have result
        ((get results key) => identity)
        ; Computation in progress - wait for it
        ((get in-flight key) =>
         (lambda (fib) (effect fiber.join fib)))
        ; Start new computation
        (else
          (let ((fib (effect fiber.spawn compute)))
            (put! in-flight key fib)
            (let ((result (effect fiber.join fib)))
              (del! in-flight key)
              (put! results key result)
              result)))))))

(define sf (make-singleflight))

; Even if called concurrently, only one LLM call happens
(parallel-do
  (sf "What is the capital of France?" classify-france)
  (sf "What is the capital of France?" classify-france))
; Second call waits for and reuses first call's result
```

This is especially valuable when multiple analyses might redundantly ask similar questions.

### 22.6 Consensus and Voting

LLMs are non-deterministic—the same prompt might yield different responses. For critical decisions, use **voting**:

```lisp
(define (classify-with-consensus text n)
  (let ((votes (parallel-map
                 (lambda (_)
                   (effect infer.op
                     (list "Classify sentiment as positive/negative/neutral: " text)))
                 (range n))))
    (majority-vote votes)))

(define (majority-vote lst)
  (car (sort (group-by identity lst)
             (lambda (a b) (> (length a) (length b))))))

Ω> (classify-with-consensus "The product is okay I guess" 5)
; LLM returns: ("neutral" "positive" "neutral" "neutral" "negative")
=> "neutral"  ; 3/5 majority
```

Voting improves reliability for borderline cases at the cost of multiple LLM calls.

### 22.7 Fan-Out/Fan-In for Complex Analysis

Complex semantic tasks can be parallelized by decomposing into independent subtasks:

```lisp
(define (comprehensive-analysis document)
  ; Fan out: spawn parallel independent analyses
  (let ((sentiment-fib (effect fiber.spawn
                         (lambda () (analyze-sentiment document))))
        (topics-fib (effect fiber.spawn
                      (lambda () (extract-topics document))))
        (entities-fib (effect fiber.spawn
                        (lambda () (extract-entities document))))
        (summary-fib (effect fiber.spawn
                       (lambda () (summarize document)))))
    ; Fan in: wait for all, combine results
    (make-analysis-result
      (effect fiber.join sentiment-fib)
      (effect fiber.join topics-fib)
      (effect fiber.join entities-fib)
      (effect fiber.join summary-fib))))

Ω> (comprehensive-analysis long-article)
=> (analysis
     (sentiment . "positive")
     (topics . ("technology" "innovation" "AI"))
     (entities . ("Apple" "Tim Cook" "Silicon Valley"))
     (summary . "Apple announces new AI research initiative..."))
```

Four LLM calls execute in parallel. Total latency: approximately one call's duration.

### 22.8 Semantic Deadlock

SICP discusses **deadlock**: when processes wait for each other indefinitely. Can semantic systems deadlock? Consider two conversational agents:

```lisp
; Agent A: "What did Agent B say about the topic?"
; Agent B: "What did Agent A say about the topic?"

; Each waits for the other - deadlock!
```

Or with knowledge bases requiring cross-validation:

```lisp
(define (add-if-consistent-with kb1 kb2 fact)
  ; Acquire lock on kb1
  ; Check fact against kb2 (requires lock on kb2)
  ; If consistent, add to kb1
  ...)

; Thread 1: add-if-consistent-with kb-A kb-B "fact1"
; Thread 2: add-if-consistent-with kb-B kb-A "fact2"

; Thread 1 holds kb-A, waits for kb-B
; Thread 2 holds kb-B, waits for kb-A
; DEADLOCK
```

**Solution**: Acquire locks in a consistent global order, or use timeout-based retry:

```lisp
(define (add-with-timeout kb1 kb2 fact timeout)
  (let ((result (effect mutex.try-acquire kb1 timeout)))
    (if (not result)
        'retry
        (let ((result2 (effect mutex.try-acquire kb2 timeout)))
          (if (not result2)
              (begin (effect mutex.release kb1) 'retry)
              (begin
                ; Do the work
                (effect mutex.release kb2)
                (effect mutex.release kb1)
                'ok))))))
```

### 22.9 Semantic Speculation

Sometimes we don't know which approach will succeed. **Speculative execution** tries multiple approaches in parallel:

```lisp
(define (translate-best text target-lang)
  (let ((literal-fib (effect fiber.spawn
                       (lambda () (translate-literal text target-lang))))
        (literary-fib (effect fiber.spawn
                        (lambda () (translate-literary text target-lang))))
        (casual-fib (effect fiber.spawn
                      (lambda () (translate-casual text target-lang)))))
    ; Return first successful result
    (effect fiber.select
      (list literal-fib literary-fib casual-fib))))

; Or: return all, let user choose
(define (translate-options text target-lang)
  (parallel-map
    (lambda (style)
      (cons style (translate-with-style text target-lang style)))
    '(literal literary casual)))
```

### 22.10 Consistency Models for Semantic Data

Traditional databases have ACID guarantees. What consistency model applies to semantic data?

**Eventual consistency**: Different LLM calls might give slightly different answers, but they converge to "roughly the same meaning."

**Semantic consistency**: Facts asserted don't contradict each other, even under concurrent access.

**Causal consistency**: If fact B depends on fact A, B is only visible after A.

```lisp
(define (make-causally-consistent-kb)
  (let ((facts '())
        (version 0)
        (dependencies (make-table)))
    (lambda (msg)
      (cond
        ((eq? msg 'assert-with-deps)
         (lambda (fact required-version)
           (if (< version required-version)
               (error "Causal violation: fact depends on unseen data")
               (begin
                 (set! version (+ version 1))
                 (set! facts (cons (list version fact) facts))))))
        ((eq? msg 'query)
         (lambda (question)
           (list version
                 (effect infer.op
                   (list "Given facts: " (format-facts facts)
                         "\n" question)))))))))
```

### 22.11 Exercises

**Exercise 22.1:** Implement `(parallel-filter pred lst)` that tests all predicates concurrently. What's the speedup for filtering 50 items with a 2-second predicate?

**Exercise 22.2:** Build a "speculative translation" system that tries literal, literary, and colloquial translations in parallel and returns the first one that passes a quality check.

**Exercise 22.3:** Design a "semantic merge" operation that reconciles potentially contradictory parallel results. If one call says "positive" and another says "neutral", how do you merge? What if it's "positive" vs "negative"?

**Exercise 22.4:** Implement a knowledge base with optimistic concurrency: allow parallel additions, but detect conflicts at commit time and require resolution.

**Exercise 22.5:** Design a scenario where two conversational agents create a deadlock. Then implement a solution using timeouts.

**Exercise 22.6:** Build a "semantic barrier": a synchronization point where multiple parallel analyses must all complete before the combined result is computed. Use it to ensure a summary incorporates all extracted entities.

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

## Chapter 24: Metalinguistic Abstraction — The Oracle in the Evaluator

### 24.1 The Metacircular Revelation

SICP Chapter 4 opens with one of the most profound ideas in computer science: the metacircular evaluator. Abelson and Sussman show us that a Lisp interpreter can be written *in Lisp*—the language describes its own meaning. This isn't a trick; it's a revelation about the nature of language and computation.

The metacircular evaluator has two core procedures:
- `eval` takes an expression and environment, and produces a value
- `apply` takes a procedure and arguments, and produces a value

These call each other recursively. `eval` may need to `apply`; `apply` may need to `eval`. The dance between them *is* computation.

Now we add something new: the **oracle**. An oracle is a computational entity that can answer questions we cannot answer algorithmically. In OmegaLLM, the LLM is our oracle.

### 24.2 The Oracle Effect

When the evaluator encounters an effect expression:

```lisp
(effect infer.op "What is the capital of France?")
```

It doesn't try to compute the answer. How could it? The answer requires *understanding*—something algorithms don't have. Instead, the evaluator *suspends*, packages the question, and consults the oracle.

The oracle—the LLM—returns: "Paris"

The evaluator *resumes* with this answer, incorporating it into ongoing computation.

This is the key insight: **the oracle is part of the eval/apply cycle**. It's not an external service called from Lisp; it's an extension of evaluation itself.

```
Traditional:
  eval(expr, env) → value
  apply(proc, args) → value

With Oracle:
  eval(expr, env) → may suspend for oracle → value
  apply(proc, args) → may suspend for oracle → value
  oracle(question) → semantic answer (from LLM)
```

### 24.3 The Oracle Can Evaluate Code

Here is where the architecture becomes recursive in a deeper sense. The oracle—the LLM—has access to the Lisp runtime. When the LLM needs to answer a question, it can:

- Evaluate Lisp expressions
- Observe the environment
- Examine the call stack
- Run computations to inform its reasoning

This creates a fascinating loop: the evaluator asks the oracle a question, and the oracle may ask the evaluator to run code.

```
User: (effect infer.op "What is factorial of 10?")
       ↓
Evaluator: "I have a semantic question" → suspends
       ↓
Oracle (LLM): "I should compute this" → calls eval("(factorial 10)")
       ↓
Evaluator: Runs factorial computation → returns 3628800
       ↓
Oracle (LLM): "The answer is 3628800" → returns to evaluator
       ↓
Evaluator: Resumes with "3628800"
```

The oracle is not passive. It is an active participant in evaluation.

### 24.4 Tools as Reified Effects

In OmegaLLM's agentic mode (`:ask`), the oracle's ability to call back into the evaluator is made explicit through *tools*. The LLM sees:

```
Available tools:
- eval(expr): Evaluate a Lisp expression
- observe(what): Examine runtime state
- apply(fn, args): Apply a function
- return(value): Provide final answer
```

These tools are reified effect operations. When the LLM calls `eval("(+ 2 3)")`, it triggers a `ReqEval` effect that the runtime handles.

The conversation between oracle and evaluator becomes visible:

```
Oracle: I'll compute this.
Oracle: [calls eval("(define (fact n) (if (< n 2) 1 (* n (fact (- n 1)))))")]
Runtime: (returns: fact defined)
Oracle: [calls eval("(fact 10)")]
Runtime: (returns: 3628800)
Oracle: [calls return("3628800")]
```

### 24.5 The Oracle Protocol

OmegaLLM defines a formal protocol for oracle communication:

**Requests (from evaluator to oracle):**
- `ReqEval`: "Evaluate this expression"
- `ReqApply`: "Apply this function to these arguments"
- `ReqObserve`: "Tell me about the runtime state"
- `ReqMatch`: "Match this expression against a pattern"
- `ReqAssert`: "Verify this condition holds"
- `ReqReturn`: "Here is the final answer"

**Responses (from oracle to evaluator):**
- `RespVal`: A computed value
- `RespObs`: Observed state
- `RespAck`: Acknowledgment
- `RespError`: Something went wrong

This protocol is the interface between computation and comprehension.

### 24.6 Meaning as Value

The oracle doesn't return raw strings—it returns *meanings*. A `Meaning` is a value that carries:

```typescript
interface Meaning {
  denotation: Val;      // The semantic content
  confidence: number;    // How certain (0.0 to 1.0)
  trace: Val;           // Provenance information
}
```

When you write `(effect infer.op "Is this text professional?")`, the returned meaning might be:

```
{
  denotation: "yes",
  confidence: 0.92,
  trace: "inferred by gpt-4o, 1 turn"
}
```

The meaning captures not just *what* the oracle said, but *how confident* it was and *how* it arrived at the answer.

### 24.7 The Evaluator as Conversation

Traditional evaluators are monologues: they reduce expressions silently until a value emerges. OmegaLLM's evaluator is a dialogue. It talks to the oracle, and the oracle talks back.

This dialogue is recorded. Every question asked, every answer received, every tool call made—all preserved in a trace. You can replay, debug, and inspect the conversation between algorithm and understanding.

### 24.8 The Philosophical Implications

What does it mean for an evaluator to have an oracle?

In classical computation, the evaluator is complete—it can reduce any expression to a value using only its rules. But semantic questions ("Is this professional?") have no algorithmic answer. They require judgment, context, knowledge of human conventions.

By adding an oracle, we acknowledge that some questions require *understanding*, and understanding is not (currently) algorithmic. The oracle is a window into a form of intelligence different from the evaluator's mechanical reduction.

The evaluator is logic. The oracle is intuition. Together, they form something new: a system that can both compute and comprehend.

### 24.9 Exercises

**Exercise 24.1:** The oracle protocol includes `ReqObserve` for inspecting runtime state. Implement a semantic function that asks the LLM to suggest optimizations by observing the current call stack.

**Exercise 24.2:** The `Meaning` type includes a confidence score. Write a wrapper function `(confident-infer threshold prompt)` that retries the inference if confidence is below the threshold, up to 3 attempts.

**Exercise 24.3:** The oracle can evaluate code. Design a "self-improving prompt" system where the oracle evaluates a test case, and if it fails, asks the oracle to revise the prompt and try again.

**Exercise 24.4:** Consider the philosophical question: Is the oracle part of the language, or external to it? Write an essay arguing each position. What are the implications for program semantics?

**Exercise 24.5:** The metacircular evaluator in SICP can interpret itself. Could OmegaLLM's oracle-augmented evaluator theoretically describe its own meaning? What would happen if the oracle were asked to explain how it works?

## Chapter 25: Lazy Semantic Evaluation

### 25.1 The Promise of Delayed Understanding

SICP Chapter 4.2 introduces lazy evaluation—a paradigm where expressions are not evaluated until their values are needed. This enables working with infinite structures and avoiding unnecessary computation.

In semantic programming, lazy evaluation is **economically essential**. LLM calls are expensive. Why call the LLM for a result that might never be used?

```lisp
; EAGER: Both branches evaluated (wasteful!)
(if condition
    (effect infer.op "Expensive analysis A")   ; Called even if condition is false
    (effect infer.op "Expensive analysis B"))  ; Called even if condition is true

; LAZY: Only the needed branch evaluated
(if condition
    (delay (effect infer.op "Expensive analysis A"))   ; Thunk created, not called
    (delay (effect infer.op "Expensive analysis B")))  ; Thunk created, not called
; Only one thunk forced based on condition
```

### 25.2 Thunks: Packaged Semantic Computations

A thunk is a zero-argument function that packages a computation:

```lisp
; Create a thunk (delay evaluation)
(define (delay-inference prompt)
  (lambda () (effect infer.op prompt)))

; Force a thunk (demand evaluation)
(define (force thunk)
  (thunk))

(define lazy-summary (delay-inference "Summarize this 100-page document"))
; No LLM call yet!

Ω> (force lazy-summary)
; NOW the LLM is called
=> "This document covers..."
```

### 25.3 Memoized Thunks: Compute Once, Use Many

SICP introduces memoized thunks—once forced, they cache their result:

```lisp
(define (memo-delay computation)
  (let ((already-run? #f)
        (result #f))
    (lambda ()
      (if already-run?
          result
          (begin
            (set! result (computation))
            (set! already-run? #t)
            result)))))

(define expensive-analysis
  (memo-delay (lambda () (effect infer.op "Deep analysis of corpus"))))

Ω> (force expensive-analysis)
; LLM call happens
=> "The corpus reveals..."

Ω> (force expensive-analysis)
; NO LLM call - cached!
=> "The corpus reveals..."
```

### 25.4 Lazy Semantic Structures

We can build data structures where semantic content is computed on demand:

```lisp
(define (make-lazy-document sections)
  ; Store section texts, but summaries are computed lazily
  (map (lambda (section)
         (list section
               (memo-delay (lambda ()
                 (effect infer.op (list "Summarize: " section))))))
       sections))

(define doc (make-lazy-document
  '("Introduction text..." "Methods text..." "Results text...")))

; No summaries computed yet!

Ω> (force (cadar doc))  ; Force intro summary
=> "This paper introduces..."
; Only one LLM call, for the intro

Ω> (force (cadar (cdr doc)))  ; Force methods summary
=> "The methodology involves..."
; Second LLM call, for methods
; Results summary still not computed!
```

### 25.5 Normal Order vs Applicative Order

SICP distinguishes:
- **Applicative order**: Evaluate arguments before applying function
- **Normal order**: Evaluate arguments only when needed

For semantic programming:

```lisp
; Applicative order (eager)
(define (summarize-if-long text threshold)
  (if (> (string-length text) threshold)
      (effect infer.op (list "Summarize: " text))  ; Always evaluated!
      text))

; Normal order (lazy)
(define (summarize-if-long-lazy text threshold)
  (if (> (string-length text) threshold)
      (force (delay (effect infer.op (list "Summarize: " text))))
      text))
```

Wait—that's not quite right. The delay/force in the lazy version still gets evaluated. True normal order requires the *language itself* to be lazy:

```lisp
; In a lazy OmegaLLM dialect:
(define (summarize-if-long text threshold)
  (if (> (string-length text) threshold)
      (effect infer.op (list "Summarize: " text))
      text))
; The infer.op is only evaluated if the text is actually long
```

### 25.6 Infinite Semantic Possibilities

Laziness enables representing infinite semantic structures:

```lisp
(define (infinite-elaborations seed)
  (cons seed
        (memo-delay (lambda ()
          (infinite-elaborations
            (effect infer.op (list "Elaborate on: " seed)))))))

(define thoughts (infinite-elaborations "consciousness"))

; Take only what we need
Ω> (car thoughts)
=> "consciousness"

Ω> (car (force (cdr thoughts)))
=> "Consciousness is the state of being aware of one's surroundings..."

Ω> (car (force (cdr (force (cdr thoughts)))))
=> "Consciousness involves complex neural processes that integrate sensory information..."

; We could continue forever, but we only compute what we use
```

### 25.7 The Substitution Model Revisited

SICP's substitution model works for applicative order but breaks for lazy evaluation. Similarly, reasoning about lazy semantic programs requires thinking about *when* effects happen:

```lisp
(define a (delay (effect infer.op "Question A")))
(define b (delay (effect infer.op "Question B")))

; This determines order of LLM calls:
(begin
  (force b)   ; B called first
  (force a))  ; A called second

; vs
(begin
  (force a)   ; A called first
  (force b))  ; B called second
```

Order matters because:
1. LLM responses may vary over time
2. Rate limits may be hit
3. Context from earlier calls might influence later ones

### 25.8 Lazy Evaluation as Resource Management

The deepest insight: **lazy evaluation is about managing scarce resources**. In traditional computing, the resource is time/space. In semantic computing, the resources are:

- LLM API calls (rate-limited, costly)
- Context window tokens (finite)
- Human attention (for reviewing results)

Laziness ensures we spend these resources only when needed.

### 25.9 Exercises

**Exercise 25.1:** Implement `lazy-map` that creates a lazy list where each element's computation is deferred. Use it to create a lazy list of translations without calling the LLM until elements are accessed.

**Exercise 25.2:** Build a "semantic cache" that wraps any inference function with memoization. Test that repeated calls with the same prompt don't trigger new LLM calls.

**Exercise 25.3:** Create a lazy decision tree for text classification. Each node asks a question; children are computed lazily. Only the path actually taken triggers LLM calls.

**Exercise 25.4:** SICP shows that some programs only work with lazy evaluation (e.g., `(cons 1 (cons 2 (cons 3 ...)))` for infinite lists). Design a semantic program that *requires* laziness to terminate—one that would make infinite LLM calls under eager evaluation.

**Exercise 25.5:** Implement a "speculative" evaluator that eagerly starts computing likely-needed values in the background, but only uses them if actually needed. Compare with pure lazy evaluation.

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

## Chapter 27: Logic Programming with Semantic Facts

### 27.1 A New Way of Thinking

SICP Chapter 4.4 introduces logic programming—a paradigm where you state *what* is true rather than *how* to compute. The query system finds answers by searching a database of facts and rules.

In semantic programming, this becomes extraordinarily powerful. Instead of pattern-matching on symbols, we can query over *meanings*:

```lisp
; Traditional logic programming:
(fact (parent alice bob))
(fact (parent bob charlie))
(rule (grandparent ?x ?z) (parent ?x ?y) (parent ?y ?z))
(query (grandparent alice ?who))
=> charlie

; SEMANTIC logic programming:
(semantic-fact "Alice gave birth to Bob in 1960")
(semantic-fact "Bob is the father of Charlie")
(semantic-query "Who is Alice's grandchild?")
=> "Charlie"
```

The system doesn't match patterns—it *understands* relationships.

### 27.2 Asserting Semantic Facts

Facts in semantic logic programming are natural language statements:

```lisp
(define (make-semantic-db)
  (let ((facts '()))
    (lambda (msg)
      (cond
        ((eq? msg 'assert)
         (lambda (fact)
           (set! facts (cons fact facts))))
        ((eq? msg 'query)
         (lambda (question)
           (effect infer.op
             (list "Based ONLY on these facts:\n"
                   (string-join facts "\n")
                   "\n\nAnswer: " question
                   "\nIf the answer cannot be determined from the facts, say 'unknown'."))))
        ((eq? msg 'facts) facts)))))

(define db (make-semantic-db))

Ω> ((db 'assert) "Marie Curie won the Nobel Prize in Physics in 1903")
Ω> ((db 'assert) "Marie Curie won the Nobel Prize in Chemistry in 1911")
Ω> ((db 'assert) "Marie Curie was born in Poland")

Ω> ((db 'query) "How many Nobel Prizes did Marie Curie win?")
=> "Two - one in Physics and one in Chemistry"

Ω> ((db 'query) "What year did Marie Curie die?")
=> "unknown"  ; Not in the facts!
```

### 27.3 Semantic Rules

Rules in traditional logic programming define derived relationships. In semantic logic programming, rules are *natural language patterns*:

```lisp
(define (add-semantic-rule db condition conclusion)
  ((db 'assert)
   (string-append "RULE: If " condition ", then " conclusion)))

Ω> (add-semantic-rule db
     "someone wins multiple Nobel Prizes"
     "they are exceptionally accomplished")

Ω> ((db 'query) "Is Marie Curie exceptionally accomplished?")
=> "Yes - she won multiple Nobel Prizes, which according to the rules indicates exceptional accomplishment"
```

### 27.4 Backward Chaining

Logic programming uses backward chaining: to prove a goal, find rules whose conclusions match and try to prove their conditions:

```lisp
(define (semantic-prove db goal)
  (let ((direct ((db 'query) goal)))
    (if (not (eq? direct "unknown"))
        direct
        ; Try to find a rule that concludes this
        (let ((applicable-rule ((db 'query)
                (string-append "Is there a rule whose conclusion relates to: " goal "?"))))
          (if (not (eq? applicable-rule "unknown"))
              ; Extract condition and recursively prove
              (let ((condition (extract-condition applicable-rule)))
                (semantic-prove db condition))
              "Cannot prove")))))
```

### 27.5 Semantic Unification

Traditional unification matches patterns like `(parent ?x bob)` with `(parent alice bob)`, binding `?x` to `alice`.

Semantic unification matches *meanings*:

```lisp
(define (semantic-unify pattern text)
  (effect infer.op
    (list "Extract values for the variables in this pattern from the text.\n"
          "Pattern: " pattern "\n"
          "Text: " text "\n"
          "Return variable bindings as JSON or 'no match'.")))

Ω> (semantic-unify
     "?person won a ?award in ?field"
     "Marie Curie received the Nobel Prize in Chemistry")
=> {"person": "Marie Curie", "award": "Nobel Prize", "field": "Chemistry"}

Ω> (semantic-unify
     "?person won a ?award in ?field"
     "The weather is nice today")
=> "no match"
```

### 27.6 A Semantic Query Language

Putting it together, we get a query language over meaning:

```lisp
(define (semantic-sql db query-template)
  (let ((facts ((db 'facts))))
    (effect infer.op
      (list "Given these facts:\n"
            (string-join facts "\n")
            "\n\nAnswer this query: " query-template
            "\nReturn results as a list."))))

Ω> (semantic-sql db "SELECT all people who won awards in science fields")
=> ("Marie Curie")

Ω> (semantic-sql db "SELECT awards WHERE winner was born in Poland")
=> ("Nobel Prize in Physics", "Nobel Prize in Chemistry")
```

### 27.7 Frame-Based Queries

SICP's query system manipulates *frames*—bindings of pattern variables to values. In semantic queries, frames bind variables to *meanings*:

```lisp
(define (make-frame) '())

(define (extend-frame var val frame)
  (cons (cons var val) frame))

(define (lookup-in-frame var frame)
  (let ((binding (assoc var frame)))
    (if binding (cdr binding) #f)))

; Semantic frame extension via unification
(define (semantic-extend pattern text frame)
  (let ((bindings (semantic-unify pattern text)))
    (if (eq? bindings "no match")
        #f
        (fold-left (lambda (f binding)
                     (extend-frame (car binding) (cdr binding) f))
                   frame
                   (json->list bindings)))))

Ω> (semantic-extend "?person studies ?subject"
                     "Alice is learning quantum mechanics"
                     (make-frame))
=> ((person . "Alice") (subject . "quantum mechanics"))
```

### 27.8 Compound Queries

SICP supports `and`, `or`, and `not` in queries. Semantic queries can use these too:

```lisp
(define (semantic-and db q1 q2)
  (let ((r1 ((db 'query) q1)))
    (if (eq? r1 "unknown")
        "unknown"
        (let ((r2 ((db 'query) q2)))
          (if (eq? r2 "unknown")
              "unknown"
              (effect infer.op
                (list "Combine these answers: " r1 " AND " r2)))))))

(define (semantic-or db q1 q2)
  (let ((r1 ((db 'query) q1)))
    (if (not (eq? r1 "unknown"))
        r1
        ((db 'query) q2))))

(define (semantic-not db q)
  (let ((r ((db 'query) q)))
    (if (eq? r "unknown")
        "yes"  ; Can't prove it, so negation-as-failure says it's false
        (effect infer.op (list "Negate this: " r)))))

Ω> (semantic-and db "Who won Nobel Prizes?" "In what fields?")
=> "Marie Curie won Nobel Prizes in Physics and Chemistry"

Ω> (semantic-not db "Did Marie Curie win a Nobel Prize in Literature?")
=> "yes"  ; Cannot be proved from facts
```

### 27.9 The Closed World Assumption

SICP's query system operates under the **closed world assumption**: what cannot be proved is false. In semantic logic programming, this becomes subtle:

```lisp
(define (closed-world-query db question)
  (let ((answer ((db 'query) question)))
    (cond
      ((string-contains? answer "unknown") #f)
      ((string-contains? answer "cannot determine") #f)
      ((string-contains? answer "not stated") #f)
      (else answer))))

Ω> ((db 'assert) "All the planets in our solar system orbit the Sun")
Ω> ((db 'assert) "Earth is a planet in our solar system")

Ω> (closed-world-query db "Does Earth orbit the Sun?")
=> "Yes, Earth orbits the Sun"

Ω> (closed-world-query db "Does Mars orbit the Sun?")
=> #f  ; Mars not mentioned in facts, so closed world says unknown
```

### 27.10 Inference vs. Retrieval

A crucial distinction: **retrieval** finds facts that match; **inference** derives new facts from existing ones. The LLM enables both:

```lisp
; Pure retrieval - find facts that mention something
(define (retrieve db term)
  (filter (lambda (f) (string-contains? f term))
          ((db 'facts))))

; Inference - derive new information
(define (infer db question)
  (effect infer.op
    (list "Using logical reasoning on these facts:\n"
          (string-join ((db 'facts)) "\n")
          "\n\nWhat can you infer about: " question)))

Ω> ((db 'assert) "Socrates is a man")
Ω> ((db 'assert) "All men are mortal")

Ω> (retrieve db "Socrates")
=> ("Socrates is a man")

Ω> (infer db "Is Socrates mortal?")
=> "Yes. Socrates is a man, and all men are mortal, therefore Socrates is mortal."
```

The LLM performs the syllogism automatically—no explicit "mortal(?x) :- man(?x)" rule needed.

### 27.11 The Power and Limits of Semantic Logic

What can semantic logic programming do that traditional logic programming cannot?

**Powers:**
- Handle vague concepts: "Is this text professional?"
- Perform analogical reasoning: "X is to Y as A is to ?"
- Leverage world knowledge not in the database
- Understand synonyms, paraphrases, implications

**Limits:**
- Non-deterministic: same query may give different answers
- Unreliable for precise logical deduction
- May hallucinate connections not supported by facts
- Expensive: each "inference step" is an LLM call

```lisp
; The power: understanding synonyms
Ω> ((db 'assert) "Marie Curie received the Nobel Prize")
Ω> ((db 'query) "Did Marie Curie win a Nobel?")
=> "Yes"  ; LLM understands "received" ≈ "won"

; The danger: hallucination
Ω> ((db 'query) "What year did Marie Curie win her first Nobel?")
=> "1903"  ; Correct, but where did this come from?
           ; It's world knowledge, not in our facts!
```

### 27.12 Exercises

**Exercise 27.1:** Build a family tree in the semantic database using natural language facts. Query for grandparents, cousins, and siblings without explicitly defining these relationships.

**Exercise 27.2:** Implement semantic negation: `(semantic-not-provable? db statement)` that returns true only if the statement cannot be derived from the facts.

**Exercise 27.3:** Create a "semantic Prolog" with cut (!). How does commitment to a proof path work when proofs are semantic rather than syntactic?

**Exercise 27.4:** SICP's query system can loop infinitely on certain recursive rules. Can semantic query systems loop? Design a rule set that might cause the LLM to go in circles.

**Exercise 27.5:** Implement `(semantic-all db pattern)` that returns ALL bindings matching a pattern, not just one. How do you ensure completeness when the LLM might miss some?

**Exercise 27.6:** Build a semantic database for a domain you know (recipes, movies, history). Test whether the LLM can answer questions requiring multi-step inference.

**Exercise 27.7:** Implement a "confidence-aware" query system that returns not just answers but confidence levels. When should low-confidence answers be treated as "unknown"?

**Exercise 27.8:** SICP discusses the frame problem in AI. How does the frame problem manifest in semantic logic programming? Can the LLM help solve it?

## Appendix A: Configuration

### API Keys

OmegaLLM looks for API keys in this order:

1. `OPENAI_API_KEY` environment variable
2. `ANTHROPIC_API_KEY` environment variable
3. `../LambdaRLM/config.yaml` file

### The .env File

Create a `.env` file in the OmegaLLM directory:

```
OPENAI_API_KEY=sk-proj-your-key-here
```

The REPL loads this automatically.

---

## Appendix B: Design Philosophy

### Why Lisp?

Lisp's homoiconicity (code as data) means LLM prompts can be constructed, transformed, and inspected as ordinary data structures. The language doesn't distinguish between "prompt templates" and "code"—they're the same thing.

### Why Effects?

Algebraic effects make LLM calls explicit and controllable. The runtime can:
- Intercept calls for caching/mocking
- Manage budgets and rate limits
- Record provenance for auditing
- Enable time-travel debugging

### Why AMB?

Nondeterministic search with backtracking is a natural fit for LLM-guided exploration. When the LLM's judgment acts as a constraint, AMB automatically explores the solution space until all semantic requirements are satisfied.

---

# Part II: Structure and Interpretation of Inference Programs

*In the spirit of Abelson and Sussman's classic, adapted for the age of language models.*

---

## Preface to Part II

In 1985, Harold Abelson and Gerald Jay Sussman published *Structure and Interpretation of Computer Programs*, a book that changed how we think about programming. Its central insight was that programs are not merely instructions for machines—they are expressions of ideas, and the act of programming is the act of building languages to express those ideas.

Four decades later, we find ourselves at a similar inflection point. Language models have given computers something they never had before: the ability to understand meaning. Not perfectly, not reliably, but genuinely. When you ask a language model "Is this text professional?", it doesn't pattern-match against rules—it *comprehends*.

This section explores what happens when we take the principles of SICP—abstraction, composition, higher-order programming, streams, nondeterminism—and apply them to programs that can invoke semantic understanding. The result is a new kind of programming, one where the distinction between computation and comprehension dissolves.

We call it the **Structure and Interpretation of Inference Programs**.

---

## Epilogue: The Structure of Understanding

We began this journey with a simple observation: language models can understand meaning. We end with a new kind of programming—one where computation and comprehension intertwine.

The lessons of SICP—abstraction, composition, laziness, nondeterminism, metalinguistic power—all apply, but transformed. Where Abelson and Sussman built evaluators for algorithms, we build evaluators for inference. Where they abstracted over procedures, we abstract over understanding.

The programs in this book are not just instructions for computation. They are invocations of meaning. They ask questions that no algorithm could answer, and they compose those answers into new knowledge.

This is programming for the age of AI: precise where precision matters, open to understanding where understanding matters, and always, always compositional.

The journey continues. Every new capability of language models becomes a new primitive for inference programming. Every insight into evaluation becomes a way to structure semantic computation.

Welcome to the structure and interpretation of inference programs.

*—The Authors, in the spirit of Abelson and Sussman*

---

