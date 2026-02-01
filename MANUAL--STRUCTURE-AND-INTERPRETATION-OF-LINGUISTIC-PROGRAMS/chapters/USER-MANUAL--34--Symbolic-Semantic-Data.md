# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 34: Symbolic Semantic Data
*Corresponding to SICP Section 2.3: Symbolic Data*

### 34.1 Symbols vs. Strings

SICP introduces **symbols**—atomic data that stand for themselves. The symbol `'apple` is different from the string `"apple"`. Symbols enable **quotation**: you can talk *about* data rather than just using it.

In inference programming, symbols represent **semantic categories**: discourse relations, speech acts, semantic roles. The symbol `'cause` represents the causal relationship itself, distinct from any particular causal statement.

### 34.2 Discourse Relations as Symbols

Natural language is structured by **discourse relations**—how utterances connect:

```lisp
;; Discourse relation symbols
'elaboration  ; "Specifically, ..."
'contrast     ; "However, ..."
'cause        ; "Because ..."
'result       ; "Therefore, ..."
'example      ; "For instance, ..."
```

These symbols let us build **symbolic representations of discourse structure**:

```lisp
(define discourse
  '(claim: "AI is advancing rapidly"
    (elaboration
      (claim: "GPT-4 can write code")
      (claim: "DALL-E creates images"))
    (contrast
      (claim: "Challenges remain")
      (cause
        (claim: "Hallucinations are problematic")
        (result
          (claim: "Users must verify outputs"))))))
```

This is **symbolic** because `elaboration`, `contrast`, `cause` are symbols representing relationships, not the relationships themselves.

### 34.3 Symbolic Manipulation of Discourse

SICP shows symbolic differentiation. We can show symbolic discourse transformation by **linearizing** symbolic rhetoric structures into coherent text.

First, we build a symbolic discourse tree using relation operators:

```lisp
(define rhetoric-tree
  '(contrast
     (elaboration
       (claim "AI is powerful")
       (evidence "GPT-4 writes code"))
     (cause
       (claim "Risks exist")
       (claim "Hallucinations occur"))))
```

Now we recursively linearize it by pattern-matching on symbolic operators:

```lisp
(define (linearize tree)
  (cond
    ((atom? tree) tree)
    ((eq? (car tree) 'claim)
     (cadr tree))  ; Extract claim text
    ((eq? (car tree) 'evidence)
     (cadr tree))  ; Extract evidence text
    ((eq? (car tree) 'elaboration)
     (effect infer.op
       (list "Combine with 'specifically': "
             (linearize (cadr tree)) ", " (linearize (caddr tree)))))
    ((eq? (car tree) 'contrast)
     (effect infer.op
       (list "Combine with 'however': "
             (linearize (cadr tree)) " vs " (linearize (caddr tree)))))
    ((eq? (car tree) 'cause)
     (effect infer.op
       (list "Combine with 'because': "
             (linearize (cadr tree)) " + " (linearize (caddr tree)))))
    (else tree)))
```

We're **transforming symbolic structure** into text:

```lisp
Ω> (linearize rhetoric-tree)

=> "AI is powerful—specifically, GPT-4 writes code. However, risks exist because hallucinations occur."
```

The symbolic operators (`'elaboration`, `'contrast`, `'cause`) drive the linearization strategy. Each operator tells the LLM *how* to combine the child clauses.

### 34.4 Quotation: Talking About Meaning

Quotation lets us distinguish:

```lisp
(effect infer.op "What is the sentiment?")
;; USES the effect

'(effect infer.op "What is the sentiment?")
;; MENTIONS the effect (quoted, not evaluated)
```

This enables **meta-level reasoning**:

```lisp
(define prompt '(effect infer.op "Classify this email"))

(define (analyze-prompt p)
  (effect infer.op
    (list "What semantic operation does this perform? " p)))

Ω> (analyze-prompt prompt)
=> "This prompt performs email classification"
```

We're analyzing the *structure* of the prompt, not executing it.

### 34.5 Speech Acts as Symbols

Speech act theory: utterances perform actions (`'request`, `'promise`, `'assert`, `'apologize`).

```lisp
(define (classify-speech-act text)
  (let ((act (effect infer.op
               (list "Classify as: request, promise, assertion, apology, or question: " text))))
    (string->symbol act)))  ; Convert to symbol

Ω> (classify-speech-act "Please help me with this")
=> 'request

Ω> (classify-speech-act "I'll finish it by tomorrow")
=> 'promise

Ω> (classify-speech-act "Sorry for the delay")
=> 'apology
```

Now we can **dispatch based on the symbol**:

```lisp
(define (respond-to-speech-act text)
  (let ((act (classify-speech-act text)))
    (case act
      ((request) (effect infer.op "Generate helpful response"))
      ((promise) (effect infer.op "Acknowledge the commitment"))
      ((apology) (effect infer.op "Accept the apology graciously"))
      ((question) (effect infer.op "Provide informative answer"))
      (else (effect infer.op "Generate neutral acknowledgment")))))
```

The symbol `'request` triggers request-handling logic. The symbol *represents* the category.

### 34.6 Semantic Role Labeling

Linguistic theory: predicates have semantic roles (`'agent`, `'patient`, `'instrument`, `'location`).

```lisp
(define (extract-roles sentence)
  (effect infer.op
    (list "Extract semantic roles from: " sentence
          "\nFormat: ((agent X) (action Y) (patient Z))")))

Ω> (extract-roles "Alice cut the bread with a knife")
=> '((agent: "Alice")
     (action: "cut")
     (patient: "the bread")
     (instrument: "a knife"))
```

The symbols `'agent`, `'patient`, `'instrument` are **semantic primitives**—atomic categories of meaning.

We can symbolically manipulate these:

```lisp
(define (passivize roles)
  ;; Swap agent and patient, mark patient as new subject
  (let ((agent (assoc 'agent: roles))
        (patient (assoc 'patient: roles))
        (action (assoc 'action: roles)))
    (list
      (list 'subject: (cadr patient))  ; patient becomes subject
      (list 'action: (list "was" (cadr action) "by"))
      (list 'agent: (cadr agent)))))  ; agent becomes "by X"

Ω> (passivize '((agent: "Alice") (action: "cut") (patient: "the bread")))
=> ((subject: "the bread")
    (action: ("was" "cut" "by"))
    (agent: "Alice"))

;; Generate from symbolic representation
(effect infer.op "Render as sentence: The bread was cut by Alice")
```

**Symbolic transformation** of meaning structure!

### 34.7 Building Symbolic Structures

SICP shows building symbolic expressions. We build symbolic discourse:

```lisp
(define (build-discourse-tree claims relations)
  (if (null? relations)
      (car claims)
      (cons (car relations)
            (cons (car claims)
                  (build-discourse-tree (cdr claims) (cdr relations))))))

Ω> (build-discourse-tree
     '("The economy is growing" "Unemployment is falling" "Wages are rising")
     '(cause result))

=> (cause "The economy is growing"
      (result "Unemployment is falling"
        "Wages are rising"))
```

Symbolic structure built programmatically from claims and relations.

### 34.8 Symbolic Differentiation: Discourse Edition

SICP's famous symbolic differentiation. Our version: **discourse simplification**:

```lisp
(define (simplify-discourse tree)
  (cond
    ((leaf? tree) tree)  ; Base case

    ;; Simplification rules
    ((and (eq? (car tree) 'elaboration)
          (eq? (caadr tree) 'elaboration))
     ;; Nested elaboration → flatten
     (cons 'elaboration
           (append (cdr (cadr tree)) (cddr tree))))

    ((and (eq? (car tree) 'contrast)
          (eq? (caadr tree) 'contrast))
     ;; Double contrast cancels
     (caddr tree))

    (else
      (cons (car tree)
            (map simplify-discourse (cdr tree))))))
```

Like symbolic differentiation, this **rewrites symbolic expressions** using transformation rules.

### 34.9 The Power of Symbolic Semantics

Symbols enable:

1. **Abstraction**: `'cause` represents all causal relationships
2. **Manipulation**: Transform symbolic discourse structures
3. **Meta-reasoning**: Analyze structure without executing
4. **Dispatch**: Different handling for different symbols

This is semantic computing at the **symbolic level**—manipulating meaning as structure.

### 34.10 Practical Example: Dialogue Act Tagging

```lisp
(define dialogue-acts
  '(greeting opening question answer clarification closing))

(define (tag-dialogue-turns turns)
  (map (lambda (turn)
         (let ((act (effect infer.op
                      (list "Classify as: " dialogue-acts ": " turn))))
           (list 'turn: turn
                 'act: (string->symbol act))))
       turns))

Ω> (tag-dialogue-turns
     '("Hello, how can I help you?"
       "I'm having trouble logging in"
       "Can you tell me your username?"
       "It's alice@example.com"
       "Thank you, let me check"
       "All set, you're logged in"))

=> ((turn: "Hello, how can I help you?" act: 'greeting)
    (turn: "I'm having trouble logging in" act: 'opening)
    (turn: "Can you tell me your username?" act: 'question)
    (turn: "It's alice@example.com" act: 'answer)
    (turn: "Thank you, let me check" act: 'clarification)
    (turn: "All set, you're logged in" act: 'closing))
```

Now we can **analyze dialogue flow symbolically**:

```lisp
(define (valid-dialogue-sequence? tagged-turns)
  ;; Check if acts follow valid patterns
  ;; E.g., question should be followed by answer or clarification
  (define (valid-next? act1 act2)
    (case act1
      ((question) (or (eq? act2 'answer) (eq? act2 'clarification)))
      ((answer) (or (eq? act2 'question) (eq? act2 'closing)))
      (else #t)))

  (all-pairs valid-next? (map get-act tagged-turns)))
```

Symbolic dialogue structure enables structural analysis.

### 34.11 Exercises

**Exercise 34.1:** Implement `(discourse-depth tree)` that returns the maximum nesting depth of discourse relations. What does depth tell you about discourse complexity?

**Exercise 34.2:** Create `(normalize-discourse tree)` that converts all discourse relations to a canonical form (e.g., convert `'however` and `'but` both to `'contrast`).

**Exercise 34.3:** Design symbolic rewrite rules for discourse that simplify redundancy. For example, `(cause A (result B))` might be rewritten as `(therefore A B)`.

**Exercise 34.4:** Build `(extract-discourse-structure text)` that takes a multi-sentence text and returns a symbolic discourse tree. The LLM identifies relations between sentences.

**Exercise 34.5:** Implement `(render-with-style tree style)` that takes a symbolic discourse tree and renders it in different styles (formal, casual, academic) while preserving structure.

**Exercise 34.6:** Create a symbolic dialogue grammar that defines valid sequences of speech acts. Use it to generate or validate dialogues symbolically.

---

**Key Insights:**
- Symbols represent semantic categories abstractly
- Quotation enables meta-level reasoning about meaning
- Discourse relations form symbolic structures
- Symbolic manipulation transforms meaning structures
- Speech acts and semantic roles are symbolic primitives
- Symbols enable dispatch and structural analysis
- Semantic computing works at symbolic level

**Next:** Chapter 35 explores tagged data with type dispatch—multiple representations with semantic types!
