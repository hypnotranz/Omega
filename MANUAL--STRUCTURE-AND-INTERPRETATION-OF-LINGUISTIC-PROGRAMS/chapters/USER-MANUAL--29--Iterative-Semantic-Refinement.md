# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 29: Iterative Semantic Refinement
*Corresponding to SICP Section 1.1.7: Example: Square Roots by Newton's Method*

### 29.1 Newton's Method: Iterative Approximation

SICP uses square-root computation to demonstrate a powerful idea: **iterative refinement**. Instead of computing the answer directly, we start with a guess and improve it repeatedly until it's "good enough."

```scheme
; SICP's sqrt algorithm
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
```

The process:
1. Start with a guess
2. Test if it's good enough
3. If not, improve it
4. Repeat until satisfied

This pattern transcends mathematics. It's a **general strategy for problem-solving**: successive approximation toward a goal.

### 29.2 Semantic Refinement: Improving Responses

In inference programming, we can apply this same pattern to **semantic quality**. Instead of improving numerical accuracy, we improve semantic fitness—how well a response meets criteria.

```lisp
(define (good-enough? response criteria)
  (equal? "yes"
    (effect infer.op
      (list "Does this response meet the criteria? Answer yes or no.\n"
            "Response: " response "\n"
            "Criteria: " criteria))))

(define (improve response feedback)
  (effect infer.op
    (list "Improve this response based on feedback:\n"
          "Response: " response "\n"
          "Feedback: " feedback)))

(define (refine initial-response criteria max-iterations)
  (define (iter response count)
    (if (or (good-enough? response criteria)
            (= count max-iterations))
        response
        (iter (improve response
                      (effect infer.op
                        (list "What's wrong with this response?\n"
                              "Response: " response "\n"
                              "Criteria: " criteria)))
              (+ count 1))))
  (iter initial-response 0))
```

Now we can refine responses iteratively:

```lisp
Ω> (refine "Hello" "Make it more professional and empathetic" 3)
; Iteration 1: "Hello" → Feedback: "Too casual, lacks empathy"
;           → Improve → "Hello, how can I assist you today?"
; Iteration 2: Check criteria → Still not quite right
;           → Improve → "Hello! I'm here to help. How can I support you?"
; Iteration 3: Check criteria → Good enough!
=> "Hello! I'm here to help. How can I support you?"
```

Each iteration brings the response closer to meeting the criteria.

### 29.3 The Structure of Iterative Refinement

Let's break down the pattern:

**1. The Termination Test** (`good-enough?`)
- Asks: "Is this response adequate?"
- Uses LLM judgment, not rule-based logic
- Can express complex, nuanced criteria

**2. The Improvement Step** (`improve`)
- Takes current attempt + feedback
- Generates better version
- Guided by specific critique

**3. The Iteration Loop** (`iter`)
- Manages the refinement cycle
- Counts iterations to prevent infinite loops
- Returns final result

This structure mirrors SICP's sqrt-iter but operates in **semantic space** instead of numerical space.

### 29.4 Example: Refining Email Tone

Suppose we're writing an apology email. The initial draft is too defensive:

```lisp
(define initial-draft
  "We apologize for the inconvenience. This was not our fault.")

(define criteria
  "Tone should be genuinely apologetic without being defensive or making excuses.")

(refine initial-draft criteria 5)
```

**Iteration 1:**
```
Check: Does "We apologize... not our fault" meet criteria?
LLM: "No - contains defensive language ('not our fault')"
Feedback: "Remove defensive phrases, add genuine empathy"
Improve: "We sincerely apologize for the inconvenience this has caused you."
```

**Iteration 2:**
```
Check: Does new version meet criteria?
LLM: "Almost - could show more understanding of impact"
Feedback: "Acknowledge specific impact on customer"
Improve: "We sincerely apologize for the inconvenience this has caused you. We understand how frustrating this must be."
```

**Iteration 3:**
```
Check: Does this meet criteria?
LLM: "Yes"
Return: "We sincerely apologize for the inconvenience this has caused you. We understand how frustrating this must be."
```

The LLM evaluates semantic qualities—tone, defensiveness, empathy—that would be impossible to encode in rules.

### 29.5 Convergence and Termination

SICP emphasizes that iterative methods must converge. For Newton's method, mathematical properties guarantee convergence. For semantic refinement, we have different considerations:

**Guaranteed termination:**
```lisp
(define (refine initial criteria max-iterations)
  (define (iter response count)
    (if (or (good-enough? response criteria)
            (= count max-iterations))  ; <-- Safety bound
        response
        ...)))
```

Without `max-iterations`, we could loop forever if the criteria are impossible to meet.

**Early convergence:**
```lisp
(define (good-enough? response criteria tolerance)
  (let ((score (effect infer.op
                 (list "Rate how well this meets criteria (0-10):\n"
                       "Response: " response "\n"
                       "Criteria: " criteria))))
    (>= score tolerance)))

; Accept "good enough" rather than waiting for perfect
(refine initial criteria 5 8.0)  ; Accept 8/10 or better
```

**Tracking progress:**
```lisp
(define (refine-with-tracking initial criteria max-iter)
  (define (iter response count history)
    (let ((score (get-score response criteria)))
      (display (list "Iteration" count "Score:" score))
      (newline)
      (if (or (>= score 8.0) (= count max-iter))
          (list 'result response 'history history)
          (iter (improve response (get-feedback response criteria))
                (+ count 1)
                (cons score history))))))
  (iter initial 0 '()))
```

This version shows scores over time, letting us see if we're converging or oscillating.

### 29.6 Multi-Objective Refinement

SICP's square root has one objective: minimize error. Semantic refinement often has multiple goals:

```lisp
(define (multi-criteria-good-enough? response)
  (and
    (equal? "yes" (effect infer.op
      (list "Is this professional? yes/no: " response)))
    (equal? "yes" (effect infer.op
      (list "Is this empathetic? yes/no: " response)))
    (<= (length response) 100)))  ; Also check length

(define (refine-multi initial max-iter)
  (define (iter response count)
    (if (or (multi-criteria-good-enough? response)
            (= count max-iter))
        response
        (let ((feedback (gather-all-feedback response)))
          (iter (improve response feedback) (+ count 1)))))
  (iter initial 0))

(define (gather-all-feedback response)
  (list
    (if (not-professional? response)
        "Make more professional"
        "")
    (if (not-empathetic? response)
        "Add more empathy"
        "")
    (if (> (length response) 100)
        "Make more concise"
        "")))
```

Multiple criteria create a **multi-dimensional optimization space**. The LLM navigates this space, balancing trade-offs (e.g., conciseness vs. empathy).

### 29.7 Comparison with Numerical Iteration

| Aspect | Numerical (sqrt) | Semantic (refinement) |
|--------|-----------------|---------------------|
| **Space** | Real numbers | Natural language |
| **Distance** | Absolute difference | Semantic similarity |
| **Improvement** | Mathematical formula | LLM generation |
| **Convergence** | Guaranteed (for sqrt) | Depends on criteria |
| **Cost** | Arithmetic operations | LLM calls (expensive!) |
| **Termination** | Error threshold | Semantic judgment |

The key difference: semantic refinement operates in a **fuzzy, high-dimensional space** where "better" is defined by understanding, not arithmetic.

### 29.8 When to Use Iterative Refinement

**Good for:**
- Quality improvement (tone, style, clarity)
- Meeting complex criteria hard to specify directly
- Exploring variations until one satisfies constraints
- Self-improving outputs based on feedback

**Not good for:**
- When direct generation works fine
- When iterations are too expensive (token costs)
- When criteria are impossible or contradictory
- When you need deterministic results

**Cost awareness:**
```lisp
; This could cost 10-20 LLM calls:
(refine initial criteria 10)

; More economical: try direct generation first
(let ((direct-attempt (effect infer.op
                        (list "Generate response meeting: " criteria))))
  (if (good-enough? direct-attempt criteria)
      direct-attempt
      (refine direct-attempt criteria 3)))  ; Only refine if needed
```

### 29.9 Advanced Pattern: Fixpoint Refinement

SICP's more advanced iterations find **fixpoints**—values where f(x) = x. We can adapt this:

```lisp
(define (fixpoint-refine f initial tolerance max-iter)
  (define (close-enough? v1 v2)
    (< (semantic-distance v1 v2) tolerance))

  (define (iter value count)
    (let ((next (f value)))
      (if (or (close-enough? value next) (= count max-iter))
          next
          (iter next (+ count 1)))))

  (iter initial 0))

(define (semantic-distance text1 text2)
  ; Use LLM to judge similarity
  (effect infer.op
    (list "How similar are these (0-1)?\n"
          "A: " text1 "\n"
          "B: " text2)))

; Example: stabilize tone
(define (normalize-tone text)
  (effect infer.op (list "Normalize tone to neutral: " text)))

(fixpoint-refine normalize-tone "I'm REALLY upset!!!" 0.1 5)
; Iteration 1: "I'm REALLY upset!!!" → "I'm quite upset"
; Iteration 2: "I'm quite upset" → "I'm upset"
; Iteration 3: "I'm upset" → "I'm upset"
; Converged! (f(x) ≈ x)
```

The tone stabilizes when further applications don't change it significantly.

### 29.10 Practical Example: Code Review Comments

```lisp
(define (refine-code-review-comment initial-comment code)
  (define (good-enough? comment)
    (and
      (equal? "yes" (effect infer.op
        (list "Is this constructive (not harsh)? yes/no: " comment)))
      (equal? "yes" (effect infer.op
        (list "Does this suggest specific improvements? yes/no: " comment)))
      (< (length comment) 200)))

  (define (improve comment)
    (effect infer.op
      (list "Improve this code review comment to be more constructive and specific:\n"
            "Comment: " comment "\n"
            "Code: " code)))

  (define (iter comment count)
    (display (list "Attempt" count ":" comment))
    (newline)
    (if (or (good-enough? comment) (= count 5))
        comment
        (iter (improve comment) (+ count 1))))

  (iter initial-comment 0))

; Usage:
(refine-code-review-comment
  "This code is terrible"
  "(define (foo x) (+ x 1))")

; Iteration 1: "This code is terrible"
;   → Not constructive, harsh
;   → Improve
; Iteration 2: "The function name 'foo' is unclear"
;   → Better, but could be more specific
;   → Improve
; Iteration 3: "Consider renaming 'foo' to describe what it does (e.g., 'increment')"
;   → Constructive, specific ✓
;   → Done!
```

### 29.11 Exercises

**Exercise 29.1:** Implement `(refine-until-concise text max-length max-iterations)` that repeatedly shortens text until it fits `max-length` while preserving meaning. Show the iterations for a 200-word paragraph compressed to 50 words.

**Exercise 29.2:** Create a `(balanced-refine text)` that iteratively balances formality and friendliness, converging to a "just right" tone. How do you define the fixpoint?

**Exercise 29.3:** Compare costs: Generate 10 responses directly vs. generate 2 and refine each 4 times. Which produces better results? Which costs more tokens?

**Exercise 29.4:** Implement `(adversarial-refine text attack defense max-iter)` where each iteration tries to "attack" the text (find flaws) then "defend" (fix them). Does this converge? Why or why not?

**Exercise 29.5:** SICP shows that sqrt converges quadratically (error halves each iteration). Can you measure convergence rate for semantic refinement? Design an experiment that tracks semantic quality score over iterations.

**Exercise 29.6:** Build a `(repair-until-valid code spec max-iter)` that iteratively fixes code until it passes tests. This is the foundation of agentic coding loops!

---

**Key Insights:**
- Iterative refinement adapts Newton's method to semantic space
- LLM evaluates "good enough" and generates improvements
- Multiple criteria create multi-dimensional optimization
- Cost awareness is critical—each iteration calls the LLM
- Fixpoints emerge when further refinement produces no change
- This pattern underlies many agentic workflows

**Next:** Chapter 30 explores tree recursion where the LLM decides how to branch!
