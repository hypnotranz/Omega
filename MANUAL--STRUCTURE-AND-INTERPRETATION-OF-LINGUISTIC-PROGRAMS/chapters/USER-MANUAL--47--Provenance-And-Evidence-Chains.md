# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 47: Provenance and Evidence Chains
*OmegaLLM-Specific: Tracking Semantic Reasoning*

### 47.1 Why Track Provenance?

For critical decisions, we need to know: **How did we arrive at this conclusion?**

Provenance tracking records:
- Which LLM calls produced which results
- How results combined to form final answer
- Evidence chain supporting the conclusion

### 47.2 Evidence Graphs

```lisp
(define (track-provenance expr)
  (case (expr-type expr)
    ((effect)
      (let* ((result (execute-effect expr))
             (evidence-node
               (make-evidence-node
                 'type: 'llm-call
                 'input: (effect-args expr)
                 'output: result
                 'timestamp: (current-time)
                 'model: (current-model))))
        (list result evidence-node)))

    ((combination)
      (let* ((sub-results (map track-provenance (expr-parts expr)))
             (results (map car sub-results))
             (evidence-nodes (map cadr sub-results))
             (combined (combine-results results))
             (combination-node
               (make-evidence-node
                 'type: 'combination
                 'inputs: evidence-nodes
                 'output: combined)))
        (list combined combination-node)))))
```

### 47.3 Example: Tracing a Decision

```lisp
(define (should-approve? application)
  (let* ((credit-ok?
           (effect infer.op
             (list "Is credit score adequate? " (get-credit-score application))))
         (income-ok?
           (effect infer.op
             (list "Is income sufficient? " (get-income application))))
         (decision
           (and credit-ok? income-ok?)))
    decision))

;; With provenance tracking
Ω> (track-provenance '(should-approve? alice-application))

=> (result: #t
    evidence-chain:
      (step-1:
        (llm-call
          input: "Is credit score adequate? 720"
          output: #t
          timestamp: "2024-01-15T10:30:00"
          model: "gpt-4"))
      (step-2:
        (llm-call
          input: "Is income sufficient? $75,000"
          output: #t
          timestamp: "2024-01-15T10:30:01"
          model: "gpt-4"))
      (step-3:
        (combination
          operation: AND
          inputs: (step-1 step-2)
          output: #t)))
```

### 47.4 Audit Trails

```lisp
(define (explain-decision decision-id)
  (let ((evidence (lookup-evidence decision-id)))
    (effect infer.op
      (list "Explain this decision to a non-technical user:\n"
            "Evidence chain: " evidence))))

Ω> (explain-decision 'application-approval-12345)

=> "The application was approved because:
    1. Credit score (720) meets our minimum requirement (680)
    2. Annual income ($75,000) exceeds 3x the requested amount ($20,000)
    Both criteria were evaluated using our standard assessment model."
```

### 47.5 Source Attribution

Track which sources contributed to the final answer:

```lisp
(define (research-question question documents)
  (let* ((relevant-excerpts
           (map (lambda (doc)
                  (let ((excerpt
                         (effect infer.op
                           (list "Extract relevant excerpt from: " doc
                                 " for question: " question))))
                    (track-source excerpt doc)))
                documents))
         (synthesis
           (effect infer.op
             (list "Synthesize answer from excerpts: " relevant-excerpts))))

    (attach-sources synthesis
      (map get-source relevant-excerpts))))

Ω> (research-question "What causes inflation?" economics-papers)

=> (answer: "Inflation occurs when money supply grows faster than economic output..."
    sources:
      ("Smith 2020, p.45: discussed monetary policy"
       "Jones 2021, p.12: analyzed supply-side factors"
       "Lee 2019, p.78: examined demand-pull inflation"))
```

### 47.6 Confidence Propagation

Track confidence through reasoning chain:

```lisp
(define (confident-enough? evidence-chain threshold)
  (let ((confidences (map get-confidence evidence-chain)))
    (>= (minimum confidences) threshold)))

;; Low confidence in one step → low confidence in conclusion
(if (not (confident-enough? evidence-chain 0.7))
    (warn "Low confidence - consider additional verification"))
```

### 47.7 Key Insights

- Provenance tracks how conclusions were reached
- Evidence chains show reasoning steps
- Audit trails enable explainability
- Source attribution credits original sources
- Confidence propagates through chains
- Critical for trustworthy AI systems

**Next:** Chapter 48 covers budget management for token costs!
