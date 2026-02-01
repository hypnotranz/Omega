# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 37: Mutable Queues and Tables
*Corresponding to SICP Sections 3.3.2-3.3.3: Representing Queues and Tables*

### 37.1 Conversation History as a Queue

A **queue** is FIFO: first in, first out. Perfect for conversation history where oldest messages drop off when context fills:

```lisp
(define history (make-queue))

(define (add-turn speaker utterance)
  (enqueue! history (list speaker utterance))
  (if (> (queue-length history) 10)  ; Max 10 turns
      (dequeue! history)))  ; Remove oldest

(define (get-context)
  (queue->list history))

;; Usage
(add-turn 'user "What's the weather?")
(add-turn 'assistant "It's sunny, 72°F")
(add-turn 'user "And tomorrow?")

Ω> (get-context)
=> ((user "What's the weather?")
    (assistant "It's sunny, 72°F")
    (user "And tomorrow?"))
```

The queue **maintains conversation flow** while managing context limits.

### 37.2 Queue Implementation

```lisp
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty?) (null? front-ptr))
    (define (enqueue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair))
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)))))
    (define (dequeue!)
      (cond ((empty?) (error "DEQUEUE! called on empty queue"))
            (else
             (let ((item (car front-ptr)))
               (set! front-ptr (cdr front-ptr))
               item))))
    (define (dispatch m)
      (case m
        ((enqueue!) enqueue!)
        ((dequeue!) dequeue!)
        ((empty?) (empty?))
        ((length) (length front-ptr))))
    dispatch))
```

**Mutation** is key—we modify the queue in place, not creating new ones.

### 37.3 Response Cache as a Table

A **table** maps keys to values. Perfect for caching LLM responses:

```lisp
(define response-cache (make-table))

(define (cached-infer prompt)
  (or (lookup prompt response-cache)
      (let ((result (effect infer.op prompt)))
        (insert! prompt result response-cache)
        result)))

;; First call: cache miss
Ω> (cached-infer "What is 2+2?")
=> "4"  ; LLM called

;; Second call: cache hit
Ω> (cached-infer "What is 2+2?")
=> "4"  ; From cache, no LLM call!
```

Tables enable **memoization**—remember results to avoid redundant work.

### 37.4 Multi-Dimensional Tables

SICP shows multi-dimensional tables. For LLM use:

```lisp
(define (make-2d-table)
  (let ((table (make-table)))
    (define (lookup key1 key2)
      (let ((subtable (lookup key1 table)))
        (if subtable
            (lookup key2 subtable)
            #f)))
    (define (insert! key1 key2 value)
      (let ((subtable (lookup key1 table)))
        (if (not subtable)
            (begin
              (set! subtable (make-table))
              (insert! key1 subtable table)))
        (insert! key2 value subtable)))
    (lambda (m)
      (case m
        ((lookup) lookup)
        ((insert!) insert!)))))

;; Cache by (language, text)
(define translation-cache (make-2d-table))

(define (translate text lang)
  (or ((translation-cache 'lookup) lang text)
      (let ((result (effect infer.op
                      (list "Translate to " lang ": " text))))
        ((translation-cache 'insert!) lang text result)
        result)))
```

### 37.5 User Preferences Table

```lisp
(define user-prefs (make-table))

(define (set-preference! user pref value)
  (let ((user-table (lookup user user-prefs)))
    (if (not user-table)
        (begin
          (set! user-table (make-table))
          (insert! user user-table user-prefs)))
    (insert! pref value user-table)))

(define (get-preference user pref default)
  (let ((user-table (lookup user user-prefs)))
    (if user-table
        (or (lookup pref user-table) default)
        default)))

;; Usage
(set-preference! 'alice 'tone 'formal)
(set-preference! 'alice 'verbosity 'concise)

Ω> (get-preference 'alice 'tone 'neutral)
=> 'formal

Ω> (get-preference 'bob 'tone 'neutral)
=> 'neutral  ; Default, Bob has no prefs
```

### 37.6 Priority Queue for Prompts

Sometimes urgent prompts should jump ahead:

```lisp
(define (make-priority-queue)
  (let ((items '()))
    (define (enqueue! item priority)
      (set! items
        (merge-sorted
          items
          (list (cons priority item))
          (lambda (a b) (> (car a) (car b))))))  ; Higher priority first
    (define (dequeue!)
      (if (null? items)
          (error "Empty queue")
          (let ((item (cdar items)))
            (set! items (cdr items))
            item)))
    ...))

(define prompt-queue (make-priority-queue))

((prompt-queue 'enqueue!) "Regular question" 1)
((prompt-queue 'enqueue!) "URGENT: system error!" 10)
((prompt-queue 'enqueue!) "Another regular question" 1)

Ω> ((prompt-queue 'dequeue!))
=> "URGENT: system error!"  ; Highest priority processed first
```

### 37.7 Conversation Context Window Management

Practical implementation combining queues and tables:

```lisp
(define (make-conversation-manager max-turns)
  (let ((history (make-queue))
        (summaries (make-table)))

    (define (add-turn! turn)
      (enqueue! history turn)
      (when (> (queue-length history) max-turns)
        ;; Summarize old turn before dropping
        (let ((old-turn (dequeue! history)))
          (insert! (turn-id old-turn)
                   (summarize-turn old-turn)
                   summaries))))

    (define (get-full-context)
      (append
        (map (lambda (id) (lookup id summaries))
             (get-summarized-ids))
        (queue->list history)))

    (lambda (m)
      (case m
        ((add!) add-turn!)
        ((context) (get-full-context))))))
```

This maintains recent turns in full, older turns as summaries.

### 37.8 Key Insights

- Queues: FIFO, perfect for conversation history
- Tables: key-value, perfect for caching
- Mutation enables in-place updates
- Multi-dimensional tables for complex caching
- Priority queues for importance-based ordering
- Conversation managers combine queues and tables

**Next:** Chapter 38 explores constraint propagation for semantic consistency!
