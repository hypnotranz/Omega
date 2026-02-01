# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 50: Language Building — The Sussman Lattice
*Creating DSLs and Language Games On-The-Fly*

> "Languages are not defined by their syntax, but by their evaluators."
> — Structure and Interpretation of Computer Programs

### 50.1 The Seven Mechanisms of Language Building

Gerald Sussman identified seven fundamental mechanisms for building new languages on top of existing ones. OmegaLLM provides **first-class primitives** for all seven, making language construction trivial:

| Mechanism | Primitive | Purpose |
|-----------|-----------|---------|
| **Syntactic Extension** | `register-macro` | Add new syntax forms |
| **Symbol Generation** | `gensym` | Hygienic macro support |
| **Custom Evaluators** | `make-evaluator` | Create DSL evaluators |
| **Evaluator Composition** | `eval-in` | Evaluate in custom context |
| **Machine Reification** | `machine-new` | Turn eval into inspectable data |
| **Continuation Control** | `call/cc` | First-class continuations |
| **Syntax Conversion** | `syntax->datum` | Manipulate code as data |

### 50.2 Symbol Generation with gensym

**Hygienic symbols** prevent variable capture in macros:

```lisp
;; Generate unique symbols for macro hygiene
(define temp1 (gensym))           ; => g42
(define temp2 (gensym))           ; => g43
(equal? temp1 temp2)              ; => #f

;; With custom prefix for readability
(define loop-var (gensym "loop")) ; => loop44
(symbol? loop-var)                ; => #t
```

### 50.3 Defining Macros with register-macro

**Syntactic extension** lets you add new language forms:

```lisp
;; Define the 'unless' macro
(register-macro 'unless
  (lambda (form)
    (let ((condition (cadr form))
          (body (caddr form)))
      (list 'if (list 'not condition) body))))

;; Check if registered
(macro? 'unless)  ; => #t
(macro? 'foo)     ; => #f

;; Define the 'when' macro
(register-macro 'when
  (lambda (form)
    (let ((cond (cadr form))
          (body (caddr form)))
      (list 'if cond body))))
```

### 50.4 Custom Evaluators with make-evaluator

**Create domain-specific languages** by defining custom evaluators:

```lisp
;; Define DSL operations
(define double (lambda (x) (* x 2)))
(define square (lambda (x) (* x x)))
(define inc (lambda (x) (+ x 1)))

;; Create a math DSL evaluator
(define math-dsl (make-evaluator))

;; Verify it's an evaluator
(evaluator? math-dsl)  ; => #t
(evaluator? 42)        ; => #f
```

### 50.5 Machine Reification — Eval as Data

**Reify evaluation** into inspectable, steppable machines:

```lisp
;; Create a machine from an expression
(define m (machine-new '(+ 1 2)))
(machine? m)  ; => #t

;; Step through execution
(define m1 (machine-step m))
(machine-step-count m1)  ; => 1

;; Run to completion
(define m-done (machine-run m))
(machine-done? m-done)  ; => #t

;; Fork machines for exploration
(define m2 (machine-fork m))
(machine? m2)  ; => #t
```

### 50.6 First-Class Continuations with call/cc

**Control operators** give you the full power of delimited continuations:

```lisp
;; Capture and invoke a continuation
(+ 1 (call/cc (lambda (k) (k 5))))
;; => 6

;; Early escape from nested computation
(+ 1 (call/cc (lambda (k) (* 10 (k 5)))))
;; => 6 (the multiplication never happens)

;; Implement coroutines, generators, exceptions...
(define (make-generator proc)
  (call/cc (lambda (return)
    (proc (lambda (value)
      (call/cc (lambda (continue)
        (return (cons value continue)))))))))
```

### 50.7 Syntax Conversion

**Manipulate code as data** and back:

```lisp
;; Convert syntax to datum (code → data)
(define datum (syntax->datum '(+ 1 2)))

;; Convert datum to syntax (data → code)
(define syntax (datum->syntax (list '+ 1 2)))

;; Create macro transformers
(define t (make-transformer (lambda (x) x)))
```

### 50.8 Why Language Building Matters for LLMs

LLMs can use these primitives to **coin new language games** on-the-fly:

```lisp
;; LLM-driven DSL construction
(define (create-domain-dsl domain-description)
  (let* (;; Ask LLM to suggest operations for this domain
         (ops (effect infer.op
                (list "Suggest 5 key operations for: " domain-description)))

         ;; Parse the response and create primitives
         (primitives (parse-operations ops))

         ;; Build the evaluator
         (dsl (make-evaluator)))

    ;; Register domain-specific macros
    (for-each
      (lambda (op)
        (register-macro (car op) (cdr op)))
      primitives)

    dsl))

;; Create a finance DSL
(define finance-dsl
  (create-domain-dsl "financial calculations and risk assessment"))
```

### 50.9 Integration: Building a Complete Mini-DSL

Combining all mechanisms to build a working DSL:

```lisp
;; Step 1: Define DSL primitives
(define double (lambda (x) (* x 2)))
(define square (lambda (x) (* x x)))
(define inc (lambda (x) (+ x 1)))

;; Step 2: Create the evaluator
(define math-dsl (make-evaluator))

;; Step 3: Register syntactic extensions
(register-macro 'double-it
  (lambda (form)
    (list 'double (cadr form))))

(register-macro 'square-it
  (lambda (form)
    (list 'square (cadr form))))

;; Step 4: Verify everything is set up
(and (evaluator? math-dsl)
     (macro? 'double-it)
     (macro? 'square-it))
;; => #t
```

### 50.10 The Power of Meta-Linguistic Abstraction

With these seven mechanisms, you can:

1. **Extend syntax** — Add new forms without modifying the evaluator
2. **Create DSLs** — Build languages tailored to specific domains
3. **Debug evaluation** — Step through and inspect execution
4. **Implement control flow** — Exceptions, coroutines, generators
5. **Transform code** — Optimize, instrument, or translate programs
6. **Build towers** — Evaluators that evaluate evaluators

This is the **Sussman Lattice** — the complete space of language building techniques, all available as first-class primitives that LLMs can invoke at runtime.

---

### Summary

| Primitive | Type Check | Purpose |
|-----------|------------|---------|
| `gensym` | `symbol?` | Generate unique symbols |
| `register-macro` | `macro?` | Register syntax transformers |
| `make-evaluator` | `evaluator?` | Create custom evaluators |
| `machine-new` | `machine?` | Reify evaluation |
| `machine-step` | — | Single-step execution |
| `machine-run` | — | Run to completion |
| `machine-fork` | — | Clone for exploration |
| `machine-done?` | — | Check completion |
| `call/cc` | — | Capture continuations |
| `syntax->datum` | — | Code to data |
| `datum->syntax` | — | Data to code |
| `make-transformer` | — | Create transformers |

---

[← Chapter 49: Semantic Caching Strategies](USER-MANUAL--49--Semantic-Caching-Strategies.md) | [Epilogue →](USER-MANUAL--99--Epilogue-The-Structure-Of-Understanding.md)
