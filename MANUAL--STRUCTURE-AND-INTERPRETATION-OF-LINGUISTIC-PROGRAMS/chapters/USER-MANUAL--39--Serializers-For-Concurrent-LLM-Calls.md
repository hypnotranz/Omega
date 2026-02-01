# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 39: Serializers for Concurrent LLM Calls
*Corresponding to SICP Sections 3.4.1-3.4.2: The Nature of Time in Concurrent Systems and Serialization*

### 39.1 The Race Condition Problem

When multiple LLM agents process documents in parallel and update a shared glossary, **race conditions** occur:

```lisp
;; WITHOUT serialization - RACE CONDITION
(define shared-glossary (make-table))

(parallel
  ;; Agent A translating chapter 1
  (lambda ()
    (let ((gloss (read-table shared-glossary)))
      (add-entry gloss "API" "Application Programming Interface")
      (write-table! shared-glossary gloss)))

  ;; Agent B translating chapter 2
  (lambda ()
    (let ((gloss (read-table shared-glossary)))
      (add-entry gloss "API" "American Petroleum Institute")
      (write-table! shared-glossary gloss))))

;; BOTH read empty glossary
;; BOTH add their definition
;; BOTH write → ONE OVERWRITES THE OTHER!
```

**Data corruption**: one definition is lost.

### 39.2 Serializers Prevent Interference

A **serializer** ensures operations complete atomically:

```lisp
(define glossary-serializer (make-serializer))

(parallel
  ((glossary-serializer
     (lambda ()
       (let ((gloss (read-table shared-glossary)))
         (detect-conflict "API" gloss shared-glossary)
         (add-entry gloss "API" "Application Programming Interface")
         (write-table! shared-glossary gloss)))))

  ((glossary-serializer
     (lambda ()
       (let ((gloss (read-table shared-glossary)))
         (detect-conflict "API" gloss shared-glossary)
         (add-entry gloss "API" "American Petroleum Institute")
         (write-table! shared-glossary gloss))))))

;; Agent A runs COMPLETELY before Agent B starts (or vice versa)
;; No interleaving → no corruption
```

### 39.3 Multi-Agent Dialogue Coordination

```lisp
(define conversation-state (make-mutable-context))
(define context-serializer (make-serializer))

(define (safe-agent-respond agent)
  ((context-serializer
     (lambda ()
       (let* ((ctx (read-context conversation-state))
              (response (effect infer.op
                          (list agent " responds to: " ctx))))
         (write-context! conversation-state
                        (append ctx (list (cons agent response)))))))))

;; Agents safely take turns
(parallel
  (safe-agent-respond 'AgentA)
  (safe-agent-respond 'AgentB)
  (safe-agent-respond 'AgentC))
;; Serialized: One completes fully before next starts
```

### 39.4 Granularity of Serialization

Serializers can protect different granularities:

```lisp
;; Fine-grained: One serializer per glossary entry
(define (make-entry-serializer term)
  (make-serializer))

(define api-serializer (make-entry-serializer "API"))
(define llm-serializer (make-entry-serializer "LLM"))

;; Agents updating DIFFERENT terms can run in parallel
(parallel
  ((api-serializer (lambda () (update-entry "API" ...))))
  ((llm-serializer (lambda () (update-entry "LLM" ...)))))
;; These run concurrently - no conflict

;; But updates to SAME term are serialized
(parallel
  ((api-serializer (lambda () (update-entry "API" ...))))
  ((api-serializer (lambda () (update-entry "API" ...)))))
;; These are serialized
```

### 39.5 Deadlock Avoidance

Improper serialization causes **deadlock**:

```lisp
;; DEADLOCK EXAMPLE
(define term-a-serializer (make-serializer))
(define term-b-serializer (make-serializer))

(parallel
  ;; Agent 1: acquires A, then needs B
  ((term-a-serializer
     (lambda ()
       ...
       ((term-b-serializer
          (lambda () ...))))))

  ;; Agent 2: acquires B, then needs A
  ((term-b-serializer
     (lambda ()
       ...
       ((term-a-serializer
          (lambda () ...)))))))
;; DEADLOCK: Each waits for the other's resource!
```

**Solution**: Acquire serializers in consistent order:

```lisp
(define (update-both term1 term2 ...)
  (let ((s1 (get-serializer term1))
        (s2 (get-serializer term2)))
    ;; Always acquire in alphabetical order
    (if (string<? term1 term2)
        ((s1 (lambda () ((s2 (lambda () ...))))))
        ((s2 (lambda () ((s1 (lambda () ...)))))))))
```

### 39.6 Practical: Streaming LLM Response Assembly

```lisp
(define response-buffer (make-buffer))
(define buffer-serializer (make-serializer))

(define (assemble-response-chunk chunk position)
  ((buffer-serializer
     (lambda ()
       (insert-at! response-buffer chunk position)))))

;; Multiple parallel LLM calls generate different parts
(parallel
  (lambda ()
    (let ((intro (effect infer.op "Generate introduction")))
      (assemble-response-chunk intro 0)))

  (lambda ()
    (let ((body (effect infer.op "Generate body")))
      (assemble-response-chunk body 1)))

  (lambda ()
    (let ((conclusion (effect infer.op "Generate conclusion")))
      (assemble-response-chunk conclusion 2))))

;; Serializer ensures buffer updates don't corrupt
Ω> (get-buffer response-buffer)
=> "Introduction... Body content... Conclusion."
```

### 39.7 Key Insights

- Concurrent updates to shared state cause race conditions
- Serializers ensure atomic operations
- Fine-grained serializers enable more parallelism
- Deadlock avoided by consistent acquisition order
- Essential for multi-agent LLM systems

**Next:** Chapter 40 explores data-directed evaluation where LLM synthesizes handlers!
