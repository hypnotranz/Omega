# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 33: Hierarchical Semantic Structures
*Corresponding to SICP Section 2.2.2: Hierarchical Data and the Closure Property*

### 33.1 The Closure Property

SICP introduces a profound idea: **closure**. A means of combination satisfies the closure property if the results of combination can themselves be combined using the same operation.

Lists satisfy closure: you can put lists inside lists. This simple property enables arbitrary hierarchical structure.

For inference programming, closure means: **semantic structures can contain semantic structures**. A conversation contains utterances. An utterance can contain sub-conversations (clarifications, elaborations). This nesting creates hierarchies of meaning.

### 33.2 Dialogue Trees: Hierarchical Conversations

The most dynamic semantic hierarchy is a **dialogue tree**—a conversation that branches based on semantic possibilities.

We use a **Composite pattern** with explicit constructors:

```lisp
;; Constructors
(define (node assistant branches) (list 'node assistant branches))
(define (branch label child) (list 'branch label child))

;; Accessors
(define (node-text n) (cadr n))
(define (node-branches n) (caddr n))
(define (branch-label b) (cadr b))
(define (branch-child b) (caddr b))
```

Now we build a support dialogue tree:

```lisp
(define support-tree
  (node
    "Hi — what can I help you with today?"
    (list
      (branch "login trouble"
        (node
          "Got it. Are you seeing an error message, or is it just not accepting your password?"
          (list
            (branch "error message"
              (node "Please paste the exact error text (remove secrets)." '()))
            (branch "password rejected"
              (node "Have you tried a password reset in the last 24 hours?" '())))))
      (branch "billing question"
        (node
          "Happy to help with billing. Is this about an unexpected charge or updating your plan?"
          (list
            (branch "unexpected charge"
              (node "Understood. What is the invoice date and the last 4 of the card on file?" '()))
            (branch "update plan"
              (node "Which plan are you moving to, and is this for monthly or annual billing?" '()))))))))
```

This structure is:
- **Hierarchical**: Conversations contain sub-conversations
- **Closed**: Each branch is itself a node (Composite pattern)
- **Non-linear**: One utterance leads to multiple possible continuations
- **Semantic**: Branch labels describe user intent, not exact phrasing

### 33.3 Structural Recursion: Tree-Map Pattern

SICP's `map-tree` applies a function to every node. For dialogue trees, we use structural recursion:

```lisp
(define (tree-map f tree)
  (let ((txt (node-text tree))
        (bs (node-branches tree)))
    (node
      (f txt)  ; Transform the text at this node
      (map
        (lambda (b)
          (branch (branch-label b)
                  (tree-map f (branch-child b))))  ; Recurse on children
        bs))))
```

**Example: Rewrite all utterances in a different tone**

```lisp
(define (rewrite-in-tone tone sentence)
  (effect infer.op
    (list "Rewrite in a " tone " tone, preserving meaning:\n" sentence)))

(define warm-tree
  (tree-map (lambda (s) (rewrite-in-tone "warm-professional" s))
            support-tree))

Ω> (node-text warm-tree)
=> "Hello! I'm here to help — what brings you by today?"

Ω> (node-text (branch-child (car (node-branches warm-tree))))
=> "I understand — let's figure this out together. Are you seeing an error message, or is your password just not being accepted?"
```

Every utterance transformed, structure preserved. This is the **Visitor pattern** in action.

### 33.4 Recursive Summarization of Dialogue Trees

We can summarize each subtree into an operator-facing "flow summary":

```lisp
(define (summarize-subtree tree)
  (let ((txt (node-text tree))
        (bs (node-branches tree)))
    (if (equal? bs '())
        ; Leaf: summarize just this step
        (effect infer.op (list "Summarize this step in 6 words:\n" txt))
        ; Internal node: summarize this node + recursively summarize children
        (let ((child-summaries
               (map (lambda (b) (summarize-subtree (branch-child b))) bs)))
          (effect infer.op
            (list "Summarize this dialogue node and its options.\n"
                  "Node: " txt "\n"
                  "Options: " child-summaries "\n"
                  "Return one sentence."))))))

Ω> (summarize-subtree support-tree)
=> "We greet the user and route them to login or billing, then ask targeted clarifying questions based on their issue."
```

This creates a **hierarchy of summaries**: leaves summarize themselves, internal nodes summarize their children's summaries. The root summarization captures the entire conversation flow.

### 33.5 Generating Dialogue Trees with LLM

The power of semantic hierarchies: **the LLM can generate the structure**:

```lisp
(define (generate-dialogue-tree prompt depth)
  (if (= depth 0)
      (list 'bot: prompt)  ; Leaf
      (let ((response prompt)
            (user-responses
             (effect infer.op
               (list "Given bot says: " prompt
                     ", list 2-3 likely user responses"))))
        (list 'bot: response
              'branches:
              (map (lambda (user-resp)
                     (list 'user: user-resp
                           (generate-dialogue-tree
                             (effect infer.op
                               (list "Bot response to: " user-resp))
                             (- depth 1))))
                   user-responses)))))
```

Now we can grow dialogue trees organically:

```lisp
Ω> (generate-dialogue-tree "Welcome! How can I help?" 2)

=> (bot: "Welcome! How can I help?"
    branches:
    ((user: "I need help with my account"
      (bot: "What account issue are you having?"
        branches:
        ((user: "I forgot my password") (bot: "I can send a reset link"))
        ((user: "I can't log in") (bot: "Let me check your account status"))))
     (user: "I have a question about your service"
      (bot: "What would you like to know?"
        branches:
        ((user: "What are your hours?") (bot: "We're open 24/7"))
        ((user: "Do you offer refunds?") (bot: "Yes, within 30 days"))))))
```

The LLM generates both the structure and content!

### 33.5 Document Hierarchies: Nested Summaries

A second hierarchical structure: documents with sections, paragraphs, sentences.

```lisp
(define research-paper
  '(title: "AI Safety"
    sections:
    ((section: "Introduction"
      paragraphs:
      ((para: "AI systems are becoming more powerful...")
       (para: "This raises important safety concerns...")))
     (section: "Alignment"
      paragraphs:
      ((para: "Value alignment is the problem of...")
       (para: "Current approaches include RLHF..."))))))
```

Recursive summarization:

```lisp
(define (summarize-tree structure)
  (cond
    ((string? structure)
     ; Leaf: actual text
     (effect infer.op (list "Summarize in 5 words: " structure)))

    ((has-field? 'paragraphs structure)
     ; Section: summarize paragraphs
     (let ((para-summaries
            (map summarize-tree (get-field 'paragraphs structure))))
       (list 'section: (get-field 'section structure)
             'summary: (effect infer.op
                        (list "Synthesize: " para-summaries)))))

    ((has-field? 'sections structure)
     ; Document: summarize sections
     (let ((section-summaries
            (map summarize-tree (get-field 'sections structure))))
       (list 'title: (get-field 'title structure)
             'summary: (effect infer.op
                        (list "Overall summary: " section-summaries)))))))
```

This produces a **hierarchy of summaries**:

```lisp
Ω> (summarize-tree research-paper)

=> (title: "AI Safety"
    summary: "AI safety addresses risks from powerful AI systems"
    sections:
    ((section: "Introduction"
      summary: "AI power raises safety concerns")
     (section: "Alignment"
      summary: "Value alignment via RLHF and related methods")))
```

Each level summarizes the level below, creating a semantic pyramid.

### 33.6 Mapping Over Hierarchies

SICP's `map-tree` applies a function to every leaf. For semantic trees:

```lisp
(define (map-utterances f tree)
  (cond
    ((string? tree) (f tree))  ; Apply to leaf
    ((pair? tree)
     (cons (map-utterances f (car tree))
           (map-utterances f (cdr tree))))
    (else tree)))

; Translate every utterance
(define (translate-dialogue tree target-lang)
  (map-utterances
    (lambda (utt)
      (effect infer.op (list "Translate to " target-lang ": " utt)))
    tree))

; Assess sentiment at every node
(define (sentiment-tree tree)
  (map-utterances
    (lambda (utt)
      (list utt 'sentiment: (effect infer.op (list "Sentiment: " utt))))
    tree))
```

**Example:**

```lisp
Ω> (translate-dialogue greeting-dialogue "Spanish")
; Every utterance translated, structure preserved

Ω> (sentiment-tree greeting-dialogue)
; Each utterance tagged with sentiment, structure preserved
```

### 33.7 Flattening Hierarchies

Sometimes we want to flatten a tree into a sequence:

```lisp
(define (flatten-dialogue tree)
  (cond
    ((null? tree) '())
    ((string? tree) (list tree))
    ((pair? tree)
     (append (flatten-dialogue (car tree))
             (flatten-dialogue (cdr tree))))
    (else '())))

Ω> (flatten-dialogue greeting-dialogue)
=> ("What brings you here today?"
    "I have technical issues"
    "Tell me about the issue"
    "Login doesn't work"
    "Let me help with login..."
    ...)
```

This gives us all utterances in traversal order—useful for generating conversation transcripts.

### 33.8 Argument Structures: Hierarchies of Justification

A third hierarchical structure: arguments with premises that themselves have supporting premises.

```lisp
(define argument
  '(claim: "AI will transform society"
    premises:
    ((claim: "AI will automate many jobs"
      premises:
      ((claim: "Automation technology is advancing rapidly")
       (claim: "Many jobs involve routine tasks")))
     (claim: "AI will change communication"
      premises:
      ((claim: "Language models enable new interfaces")
       (claim: "AI can generate and understand text"))))))

(define (validate-argument-tree tree)
  (if (null? (get-field 'premises tree))
      ; Leaf claim: ask LLM if it's plausible
      (effect infer.op
        (list "Is this claim plausible? yes/no: " (get-field 'claim tree)))
      ; Internal node: validate premises, then check if they support claim
      (let ((premises-valid?
             (map validate-argument-tree (get-field 'premises tree)))
            (claim (get-field 'claim tree))
            (premises (get-field 'premises tree)))
        (if (all premises-valid?)
            (effect infer.op
              (list "Do these premises support this claim?\n"
                    "Claim: " claim "\n"
                    "Premises: " (map get-claim premises)))
            #f))))  ; Invalid if any premise invalid
```

This recursively validates an argument from bottom-up: leaf claims must be plausible, and premises must support their claims.

### 33.9 The Power of Semantic Closure

The closure property—structures containing structures—enables:

1. **Arbitrary depth**: Conversations can nest indefinitely, documents can have sub-sub-sections, arguments can have premises of premises
2. **Recursive algorithms**: One algorithm handles all levels (summarize-tree, validate-argument-tree)
3. **Compositional meaning**: The meaning of the whole emerges from the meanings of the parts
4. **Fractal semantics**: Same patterns repeat at different scales

This is the essence of hierarchical thinking in semantic space.

### 33.10 Practical Example: Knowledge Hierarchy Exploration

```lisp
(define (explore-knowledge-tree topic depth)
  (if (= depth 0)
      (list 'concept: topic)
      (let ((subtopics
             (effect infer.op
               (list "List 2-3 key subtopics of: " topic))))
        (list 'concept: topic
              'subtopics:
              (map (lambda (sub)
                     (explore-knowledge-tree sub (- depth 1)))
                   subtopics)))))

Ω> (explore-knowledge-tree "Machine Learning" 3)

=> (concept: "Machine Learning"
    subtopics:
    ((concept: "Supervised Learning"
      subtopics:
      ((concept: "Classification"
        subtopics: ((concept: "Decision Trees") (concept: "Neural Networks")))
       (concept: "Regression"
        subtopics: ((concept: "Linear Regression") (concept: "Polynomial Regression")))))
     (concept: "Unsupervised Learning"
      subtopics:
      ((concept: "Clustering"
        subtopics: ((concept: "K-Means") (concept: "Hierarchical Clustering")))
       (concept: "Dimensionality Reduction"
        subtopics: ((concept: "PCA") (concept: "t-SNE")))))))
```

A knowledge tree grows organically, limited only by depth and the LLM's understanding.

### 33.11 Exercises

**Exercise 33.1:** Implement `(prune-dialogue-tree tree predicate)` that removes branches where the predicate (an LLM-judged condition) is false. Use it to filter out off-topic branches.

**Exercise 33.2:** Create `(merge-dialogue-trees tree1 tree2)` that combines two dialogue trees when they have overlapping paths. How do you handle conflicts?

**Exercise 33.3:** Design `(dialogue-tree->flowchart tree)` that converts a dialogue tree into a textual flowchart representation. How do you handle branching?

**Exercise 33.4:** Implement `(detect-loops tree)` that finds circular references in a dialogue tree (e.g., a clarification that returns to the original question). Should loops be allowed?

**Exercise 33.5:** Build `(adaptive-depth-exploration topic budget)` that explores a knowledge tree but adapts depth based on importance (more important topics get deeper exploration within fixed budget).

**Exercise 33.6:** Create `(diff-trees tree1 tree2)` that shows differences between two hierarchical structures (e.g., two versions of a document). What does "difference" mean semantically?

---

**Key Insights:**
- Closure property: structures contain structures of same type
- Enables arbitrary hierarchical nesting
- Recursive algorithms handle all levels uniformly
- Dialogue trees, document hierarchies, argument structures all show closure
- LLM can generate both structure and content
- Flattening and mapping preserve/transform hierarchies
- Compositional meaning: whole emerges from parts

**Next:** Chapter 34 explores symbolic semantic data—symbols representing meanings!
