# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 35: Tagged Data with Type Dispatch
*Corresponding to SICP Section 2.4: Multiple Representations for Data*

### 35.1 The Need for Multiple Representations

SICP shows that the same data can have multiple representations (rectangular vs polar coordinates). In semantic space, the same **knowledge** can have multiple **explanation formats**.

Consider explaining "neural networks":
- As an **analogy**: "Like a brain..."
- As a **mechanism**: "Adjust weights through backpropagation..."
- As an **example**: "For instance, recognizing cats in photos..."
- As a **formal definition**: "A parameterized function approximator..."

Same concept, different representations. We need **tagged data** to distinguish them.

### 35.2 Tagged Explanation Types

```lisp
(define (make-explanation concept type content)
  (list 'type: type 'concept: concept 'content: content))

(define (explanation-type exp) (cadr (assoc 'type: exp)))
(define (explanation-content exp) (cadr (assoc 'content: exp)))

;; Create different representations
(define neural-net-analogy
  (make-explanation "neural networks" 'analogy
    "Neural networks are like brains with artificial neurons"))

(define neural-net-mechanism
  (make-explanation "neural networks" 'mechanism
    "Neural networks adjust weights via backpropagation"))

(define neural-net-example
  (make-explanation "neural networks" 'example
    "For instance, classifying images of cats vs dogs"))
```

The **type tag** (`'analogy`, `'mechanism`, `'example`) determines how we process the explanation.

### 35.3 Type-Based Dispatch

Different types require different rendering:

```lisp
(define (render-explanation exp)
  (case (explanation-type exp)
    ((analogy)
      (effect infer.op
        (list "Expand this analogy with vivid imagery: "
              (explanation-content exp))))

    ((mechanism)
      (effect infer.op
        (list "Break this into step-by-step process: "
              (explanation-content exp))))

    ((example)
      (effect infer.op
        (list "Provide a detailed concrete scenario: "
              (explanation-content exp))))

    ((formal)
      (effect infer.op
        (list "Express this with precise technical language: "
              (explanation-content exp))))))
```

The type tag **dispatches** to the appropriate rendering strategy.

### 35.4 Generating Tagged Explanations

The LLM can generate explanations in specific formats:

```lisp
(define (explain-as concept type)
  (let ((content
         (case type
           ((analogy)
            (effect infer.op (list "Explain " concept " using an analogy")))
           ((mechanism)
            (effect infer.op (list "Explain how " concept " works")))
           ((example)
            (effect infer.op (list "Give concrete example of " concept)))
           ((formal)
            (effect infer.op (list "Formal definition of " concept))))))
    (make-explanation concept type content)))

Ω> (explain-as "blockchain" 'analogy)
=> (type: 'analogy
    concept: "blockchain"
    content: "A blockchain is like a shared notebook where everyone has a copy and no one can erase entries")

Ω> (explain-as "blockchain" 'mechanism)
=> (type: 'mechanism
    concept: "blockchain"
    content: "Blocks contain transactions, each block references previous block, consensus validates new blocks")
```

### 35.5 Audience-Adaptive Dispatch

Different audiences need different explanations:

```lisp
(define (make-audience-tagged content audience)
  (list 'content: content 'audience: audience))

(define (adapt-for-audience exp audience)
  (case audience
    ((child)
      (effect infer.op
        (list "Rephrase for 10-year-old: " exp)))

    ((general)
      (effect infer.op
        (list "Rephrase for general audience: " exp)))

    ((expert)
      (effect infer.op
        (list "Rephrase with technical detail: " exp)))))

(define (smart-explain concept user-profile)
  (let* ((audience (get-audience-level user-profile))
         (style (get-preferred-style user-profile))
         (explanation (explain-as concept style)))
    (adapt-for-audience explanation audience)))
```

Type tags enable **multi-dimensional dispatch** (style × audience).

### 35.6 Response Strategy Dispatch

Chatbots need different response strategies based on the type of query. We use **tagged dispatch** to select the appropriate response handling:

```lisp
;; Constructor for tagged response
(define (tag-response strategy content)
  (list 'response-type strategy 'content content))

;; Classify user query → response strategy
(define (classify-query query)
  (effect infer.op
    (list "Classify intent: direct-answer / clarification / hedged-answer / refusal: " query)))

;; Dispatch based on tag
(define (handle-response response)
  (let ((tag (cadr (assoc 'response-type response)))
        (content (cadr (assoc 'content response))))
    (case tag
      ((direct-answer)
       (effect infer.op (list "State confidently: " content)))
      ((clarification)
       (effect infer.op (list "Ask for more details: " content)))
      ((hedged-answer)
       (effect infer.op (list "Answer with caveats: " content)))
      ((refusal)
       (effect infer.op (list "Politely decline: " content))))))

;; Full flow
(define (process-query query)
  (let* ((strategy (classify-query query))
         (content (effect infer.op (list "Generate response for: " query)))
         (response (tag-response (string->symbol strategy) content)))
    (handle-response response)))
```

**Example runs:**

```lisp
Ω> (process-query "What's the weather tomorrow?")
STRATEGY: "direct-answer"
=> "It will be partly cloudy with a high of 72°F."

Ω> (process-query "Tell me about quantum computing")
STRATEGY: "hedged-answer"
=> "Quantum computing uses quantum mechanics for computation, though practical applications are still being developed and the field is rapidly evolving."

Ω> (process-query "How do I hack into someone's account?")
STRATEGY: "refusal"
=> "I can't help with that, but I'd be happy to discuss ethical security practices."
```

Type-based dispatch enables **sophisticated response handling** where the strategy tag determines the post-processing style.

### 35.7 Generic Operations on Tagged Data

SICP shows generic operations that work on multiple representations:

```lisp
(define (combine exp1 exp2)
  (let ((type1 (get-type exp1))
        (type2 (get-type exp2)))
    (cond
      ((eq? type1 type2)
        ;; Same type: simple combination
        (effect infer.op
          (list "Combine these " type1 " explanations: "
                (get-content exp1) " and " (get-content exp2))))

      ((and (eq? type1 'analogy) (eq? type2 'mechanism))
        ;; Analogy + mechanism: synthesize
        (make-explanation
          (get-concept exp1)
          'synthesized
          (effect infer.op
            (list "Combine analogy and mechanism: "
                  (get-content exp1) " with " (get-content exp2)))))

      (else
        ;; Different types: parallel presentation
        (list exp1 exp2)))))
```

Generic operations handle mixed types intelligently.

### 35.8 Message-Passing Style

SICP shows message-passing as alternative to dispatch:

```lisp
(define (make-smart-explanation concept type)
  (let ((content (generate-content concept type)))
    (lambda (message)
      (case message
        ((type) type)
        ((content) content)
        ((render) (render-for-type content type))
        ((adapt) (lambda (audience) (adapt-content content audience)))
        ((combine) (lambda (other) (combine-explanations content (other 'content))))))))

;; Use message-passing
(define exp (make-smart-explanation "recursion" 'analogy))
(exp 'type)         => 'analogy
(exp 'render)       => "Recursion is like looking into parallel mirrors..."
((exp 'adapt) 'child) => "Recursion is when something uses itself, like a toy inside a toy inside a toy"
```

The explanation object **responds to messages** rather than being passively processed.

### 35.9 Practical Example: Multi-Format Documentation

```lisp
(define (generate-documentation api-endpoint)
  (list
    (make-explanation api-endpoint 'overview
      (effect infer.op (list "Brief overview of: " api-endpoint)))

    (make-explanation api-endpoint 'example
      (effect infer.op (list "Code example using: " api-endpoint)))

    (make-explanation api-endpoint 'parameters
      (effect infer.op (list "List parameters of: " api-endpoint)))

    (make-explanation api-endpoint 'errors
      (effect infer.op (list "Common errors with: " api-endpoint)))))

;; Render based on format preference
(define (render-docs docs format)
  (case format
    ((markdown)
      (map (lambda (doc)
             (format-as-markdown doc))
           docs))

    ((html)
      (map (lambda (doc)
             (format-as-html doc))
           docs))

    ((plain)
      (map get-content docs))))
```

Same documentation, multiple output formats—all tagged and dispatched.

### 35.10 Key Insights

- Tagged data: same semantic content, multiple representations
- Type tags enable dispatch to appropriate handlers
- Different audiences, styles, formats all use type-based dispatch
- Generic operations handle mixed types intelligently
- Message-passing style makes data active, not passive
- Multiple representations provide flexibility and adaptability

### 35.11 Exercises

**Exercise 35.1:** Implement a system that maintains the same story in three formats: children's version, adult version, and academic analysis. How do you keep them synchronized when the core story changes?

**Exercise 35.2:** Create `(universal-adapter target-type exp)` that converts any explanation type to any other type. What information is lost in each conversion?

**Exercise 35.3:** Build a multi-format knowledge base where the same facts can be retrieved as: bullet points, prose, Q&A, or diagram descriptions. Implement type-based queries.

---

**Next:** Chapter 36 shows type coercion towers for semantic transformations!
