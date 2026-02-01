# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 30: Tree Recursion with Semantic Branching
*Corresponding to SICP Section 1.2.2: Tree Recursion*

### 30.1 From Linear to Tree Recursion

SICP introduces tree recursion through the classic Fibonacci sequence:

```scheme
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))
```

The computation forms a **tree**: each call spawns two more calls, which spawn two more each, creating exponential branching.

```
         fib(5)
        /      \
     fib(4)   fib(3)
     /   \     /   \
  fib(3) fib(2) ...
  /   \
...  ...
```

In contrast, **linear recursion** (like factorial) has one recursive call per step, forming a straight line.

### 30.2 Semantic Tree Recursion

In inference programming, tree recursion becomes even more powerful: **the LLM decides how to branch**. Instead of fixed recursive structure, the branching pattern emerges from semantic understanding.

```lisp
(define (semantic-tree-explore topic depth)
  (if (= depth 0)
      (list topic)  ; Leaf: just the topic itself
      (let ((subtopics
             (effect infer.op
               (list "List 2-3 key subtopics of: " topic))))
        (cons topic
              (map (lambda (subtopic)
                     (semantic-tree-explore subtopic (- depth 1)))
                   subtopics)))))
```

Example exploration:

```lisp
Ω> (semantic-tree-explore "AI Safety" 2)

; Depth 0: "AI Safety"
; LLM generates subtopics: ["Alignment" "Robustness" "Transparency"]
;
; Depth 1: Explore each
;   "Alignment" → ["Value Learning" "Reward Modeling"]
;   "Robustness" → ["Adversarial Examples" "Distribution Shift"]
;   "Transparency" → ["Interpretability" "Explainability"]
;
; Result (tree structure):
=> ("AI Safety"
    ("Alignment"
      ("Value Learning")
      ("Reward Modeling"))
    ("Robustness"
      ("Adversarial Examples")
      ("Distribution Shift"))
    ("Transparency"
      ("Interpretability")
      ("Explainability")))
```

The LLM determines the branching factor (2-3 subtopics) and the branch content (which subtopics). The recursion structure emerges from understanding, not from hardcoded rules.

### 30.3 The Exponential Nature of Semantic Trees

Like Fibonacci, semantic tree recursion has exponential growth:

- **Depth 0:** 1 node
- **Depth 1:** 1 + 3 = 4 nodes (assuming 3 branches)
- **Depth 2:** 1 + 3 + 9 = 13 nodes
- **Depth 3:** 1 + 3 + 9 + 27 = 40 nodes

Each level multiplies the number of LLM calls. At depth 3 with branching factor 3:
- Total nodes: 1 + 3 + 9 + 27 = **40 LLM calls**
- Cost grows as O(b^d) where b = branching factor, d = depth

**SICP's warning applies doubly here**: tree recursion is expensive. For semantic trees, it's not just computation time—it's **API costs**. Use wisely!

### 30.4 Controlling Branching with Semantic Constraints

We can constrain the LLM's branching decisions:

```lisp
(define (explore-with-limit topic depth max-branches)
  (if (= depth 0)
      (list topic)
      (let ((all-subtopics
             (effect infer.op
               (list "List up to " max-branches " subtopics of: " topic))))
        ;; LLM might return fewer than max-branches
        (cons topic
              (map (lambda (subtopic)
                     (explore-with-limit subtopic
                                        (- depth 1)
                                        max-branches))
                   (take max-branches all-subtopics))))))
```

Or dynamically adjust branching based on importance:

```lisp
(define (adaptive-explore topic depth)
  (if (= depth 0)
      (list topic)
      (let* ((importance
              (effect infer.op
                (list "How important is '" topic "' (1-3)?")))
             (num-branches
              (case importance
                ((3) 4)  ; Very important: explore 4 branches
                ((2) 2)  ; Moderate: 2 branches
                (else 1)))) ; Low: 1 branch
        (let ((subtopics
               (effect infer.op
                 (list "List " num-branches " subtopics of: " topic))))
          (cons topic
                (map (lambda (st)
                       (adaptive-explore st (- depth 1)))
                     subtopics))))))
```

The tree shape adapts to semantic importance!

### 30.5 Example: Argument Decomposition

Tree recursion naturally models argument breakdown:

```lisp
(define (decompose-argument claim depth)
  (if (= depth 0)
      claim  ; Base case: just the claim itself
      (let ((premises
             (effect infer.op
               (list "What are the 2-3 key premises supporting: " claim))))
        (list claim
              (map (lambda (premise)
                     (decompose-argument premise (- depth 1)))
                   premises)))))

Ω> (decompose-argument "AI will transform society" 2)

; Tree of arguments:
=> ("AI will transform society"
    (("AI will automate many jobs"
      (("Automation technology is advancing rapidly")
       ("Many jobs involve routine tasks")))
     ("AI will change how we communicate"
      (("Language models enable new interfaces")
       ("AI can generate and understand text")))
     ("AI will raise new ethical questions"
      (("AI systems can be biased")
       ("AI impacts privacy and autonomy")))))
```

Each claim is supported by premises, which are themselves supported by sub-premises. The LLM constructs a tree of reasoning.

### 30.6 Pruning: Avoiding Redundancy

Naive tree recursion can explore the same concept multiple times:

```lisp
; Problem: might explore "Machine Learning" under both "AI" and "Data Science"
(semantic-tree-explore "Computer Science" 3)
```

Solution: Track visited nodes:

```lisp
(define (explore-no-duplicates topic depth visited)
  (if (or (= depth 0) (member topic visited))
      (list topic)
      (let ((subtopics
             (effect infer.op
               (list "List 2-3 subtopics of: " topic))))
        (cons topic
              (map (lambda (subtopic)
                     (explore-no-duplicates subtopic
                                           (- depth 1)
                                           (cons topic visited)))
                   (filter (lambda (st) (not (member st visited)))
                           subtopics))))))

Ω> (explore-no-duplicates "Computer Science" 3 '())
; Won't re-explore "Machine Learning" if already visited
```

### 30.7 Flattening Tree Results

Sometimes we want a flat list of all explored topics, not a tree:

```lisp
(define (flatten tree)
  (cond
    ((null? tree) '())
    ((pair? tree)
     (append (flatten (car tree))
             (flatten (cdr tree))))
    (else (list tree))))

(define (explore-and-flatten topic depth)
  (flatten (semantic-tree-explore topic depth)))

Ω> (explore-and-flatten "AI Safety" 2)
=> ("AI Safety" "Alignment" "Value Learning" "Reward Modeling"
    "Robustness" "Adversarial Examples" "Distribution Shift"
    "Transparency" "Interpretability" "Explainability")
```

This gives us all nodes visited during exploration, useful for generating tag lists or knowledge maps.

### 30.8 Decision Trees with Semantic Predicates

Tree recursion models decision-making where the LLM evaluates branches:

```lisp
(define (semantic-decision-tree question depth)
  (if (= depth 0)
      (effect infer.op (list "Final answer for: " question))
      (let* ((should-split?
              (effect infer.op
                (list "Should we break down '" question "' further? yes/no")))
             (answer
              (effect infer.op (list "Answer: " question))))
        (if (equal? should-split? "no")
            answer  ; Leaf node: direct answer
            (let ((subquestions
                   (effect infer.op
                     (list "Break '" question "' into 2 subquestions"))))
              ;; Recursive case: solve subquestions
              (list question
                    (map (lambda (sq)
                           (semantic-decision-tree sq (- depth 1)))
                         subquestions)))))))

Ω> (semantic-decision-tree "How can we reduce carbon emissions?" 2)

=> ("How can we reduce carbon emissions?"
    (("How can transportation be made cleaner?"
      ("Electric vehicles" "Public transit"))
     ("How can energy production be decarbonized?"
      ("Renewable energy" "Nuclear power"))))
```

The LLM decides both **whether** to branch (should-split?) and **how** to branch (what subquestions).

### 30.9 Comparison: Fixed vs. Semantic Branching

| Aspect | Fixed Tree (Fibonacci) | Semantic Tree |
|--------|----------------------|---------------|
| **Branching** | Hardcoded (always 2) | LLM decides (varies) |
| **Termination** | Simple base case (n < 2) | Semantic judgment |
| **Structure** | Predictable | Emergent from meaning |
| **Cost** | Predictable (O(2^n)) | Harder to predict |
| **Value** | Demonstrates pattern | Generates knowledge |

Semantic trees trade predictability for **intelligence**—the structure reflects understanding.

### 30.10 Practical Considerations

**1. Cost explosion:**
```lisp
; This could make HUNDREDS of LLM calls:
(semantic-tree-explore "Philosophy" 5)  ; DON'T DO THIS!

; Be conservative with depth:
(semantic-tree-explore "Philosophy" 2)  ; Much more reasonable
```

**2. Timeout risks:**
With large trees, some branches may time out. Build robustness:

```lisp
(define (explore-with-timeout topic depth max-time)
  (with-timeout max-time
    (semantic-tree-explore topic depth)
    (lambda () (list topic "TIMEOUT"))))
```

**3. Parallel exploration:**
If branches are independent, we can parallelize (Chapter 22):

```lisp
(define (parallel-explore topic depth)
  (if (= depth 0)
      (list topic)
      (let ((subtopics (get-subtopics topic)))
        (cons topic
              (parallel-map
                (lambda (st) (parallel-explore st (- depth 1)))
                subtopics)))))
```

**4. Caching:**
If the same topic appears in multiple branches, cache results:

```lisp
(define exploration-cache (make-hash))

(define (cached-explore topic depth)
  (let ((cache-key (list topic depth)))
    (or (hash-ref exploration-cache cache-key)
        (let ((result (semantic-tree-explore topic depth)))
          (hash-set! exploration-cache cache-key result)
          result))))
```

### 30.11 Exercises

**Exercise 30.1:** Implement `(count-nodes tree)` that counts total nodes in a semantic tree. Use it to measure tree size for different depths.

**Exercise 30.2:** Write `(max-depth tree)` that finds the deepest path in a semantic tree. How does this relate to the `depth` parameter you passed in?

**Exercise 30.3:** Create `(breadth-first-explore topic levels)` that explores level-by-level (all depth-1 topics before any depth-2) instead of depth-first. Compare to `semantic-tree-explore`.

**Exercise 30.4:** Implement `(symmetric-explore topics depth)` that explores multiple root topics and returns a forest (list of trees). How does cost scale?

**Exercise 30.5:** Design `(explore-until-cost topic max-cost)` that explores as deeply as possible without exceeding a token budget. Track costs and terminate when budget is reached.

**Exercise 30.6:** SICP shows memoization dramatically improves Fibonacci. Implement `(memo-explore topic depth)` with memoization. Test on topics with overlapping subtopics (like "CS" and "Engineering" both having "Algorithms").

**Exercise 30.7:** Create `(comparative-tree topic1 topic2 depth)` that explores both topics and highlights where their subtopic trees overlap.

---

**Key Insights:**
- Tree recursion with LLMs: structure emerges from understanding
- Branching is semantic—LLM decides how many branches and what they contain
- Exponential cost growth demands depth limits and pruning
- Decision trees, argument decomposition, knowledge exploration all use this pattern
- Memoization, parallelization, and caching mitigate costs
- The tree shape itself carries information about the domain

**Next:** Chapter 31 analyzes the orders of growth for semantic operations—token costs as complexity measure!
