# OmegaLLM Integration Architecture

> **The Big Idea:** Control `eval` and `apply`, and you control everything.
> Any Lisp compiles to the same 7 forms. Inject semantics at that layer.
> Data-directed dispatch means the same code works for computation OR inference.

---

## Part 1: The Pitch

What if you could make **any Lisp dialect** semantically-aware with zero translation?

```
Common Lisp  ─┐
Scheme       ─┼──→  Core 7 Forms  ──→  YOUR eval/apply  ──→  Effects, Oracle, Governance
Clojure      ─┤                              ↑
Racket       ─┘                     (inject semantics here)
```

OmegaLLM sits at the **eval/apply boundary** - the universal chokepoint where all Lisp code executes. By controlling this layer, we inject semantic capabilities into ANY Lisp that compiles down to our core.

This isn't "Scheme with an LLM bolted on."
This is a **semantic execution substrate** that any Lisp can target.

---

## Part 2: The Core 7 Forms

Everything in Lisp reduces to these forms:

```
┌─────────────────────────────────────────────────────────────┐
│                        THE CORE 7                           │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   quote     '(1 2 3)               Literal data (INERT)     │
│   if        (if test then else)    Conditional branch       │
│   lambda    (lambda (x) body)      Procedure formation      │
│   begin     (begin e1 e2 ...)      Sequencing               │
│   define    (define x value)       Binding introduction     │
│   set!      (set! x value)         Assignment               │
│   app       (f arg1 arg2 ...)      Application (call)       │
│                                                             │
├─────────────────────────────────────────────────────────────┤
│   PRIMITIVES (the "hardware" - can't be written in Lisp)    │
│   + - * / < > = <= >=              Arithmetic               │
│   cons car cdr                     Pairs                    │
│   eq? equal?                       Equality                 │
│   null? pair? number? string?      Type predicates          │
└─────────────────────────────────────────────────────────────┘
```

**Critical invariants:**
- **`quote` is INERT** - always returns its argument unchanged
- **`lambda` is STABLE** - string body returns string, not SemanticProc
- **`if` is PURE** - no implicit semantic dispatch

**Everything else is derived:**

```scheme
;; let is just lambda
(let ((x 1)) body)  →  ((lambda (x) body) 1)

;; and is a macro
(and a b)  →  (if a b #f)

;; map is a function
(define (map f lst)
  (if (null? lst) '()
      (cons (f (car lst)) (map f (cdr lst)))))
```

50 lines of Scheme builds a complete standard library.
The Core 7 is the **universal Lisp IR**.

---

## Part 3: Semantic Lifting (The Key Insight)

### 3.1 The Principle

Semantic computation must be:
- **Compositional** - first-class values, higher-order, map/filter/compose
- **Governable** - effects + profiles + budgets
- **Replayable** - ledger + receipts
- **Homoiconic** - code as data (quote must be inert!)
- **Non-surprising** - no "strings sometimes run the model"

**So we introduce semantic intent via EXPLICIT constructors, not string magic.**

### 3.2 Semantic Intent via Tagged Values

These are first-class Ω values (tagged records). They are **data**, so they can be quoted, stored, passed, transformed, and macro-expanded without side effects.

```scheme
;; Prompt: data used to drive intensional evaluation
(prompt
  :text  "Decide if the email contains sensitive info."
  :style 'concise
  :rules '(no-new-facts cite-evidence)
  :schema 'boolean)

;; Query: a prompt with an explicitly declared result schema
(query
  :text  "Does this email contain PII?"
  :as    'boolean
  :evidence (list receipt:policy receipt:pii-spec)
  :engine  engine:solver
  :policy  policy:pragmatic)

;; Goal: synthesis / search target (for infer/rewrite)
(goal
  :kind 'synthesize
  :name 'redact
  :contract contract:redact
  :prefer '(pure deterministic))
```

**Important invariant:** a plain string `"..."` is NEVER implicitly treated as a `Prompt` or `Query`.

### 3.3 Convenience Sugar (Reader Level Only)

If you want shorthand, add a reader macro:

```scheme
#?("is this spam?" :as boolean)  →  (query :text "is this spam?" :as 'boolean)
```

The **reader** expands it. The **evaluator** sees only tagged `Query` data.

### 3.4 Three Intensional Portal Operators

Semantic work enters computation via **explicit** operations:

| Operator | Purpose | Returns |
|----------|---------|---------|
| `(int qexpr)` | Interpret query/prompt | `Meaning` |
| `(infer goal)` | Synthesize/search | `Dist<Meaning>` or `Dist<Val>` |
| `(rewrite expr :goal g)` | Transform with goal | `Meaning` with rewrite field |

These lower to `(effect infer.op ...)` and are the ONLY places LLM calls originate.

### 3.5 Semantic Procedures: Explicit `oracle-lambda`

You want "LLM functions" that participate in `apply` like closures. Use an explicit constructor:

```scheme
(define redact
  (oracle-lambda (text)
    (prompt
      :text "Redact sensitive information. Return redacted text only."
      :schema 'string)
    :policy policy:pragmatic))
```

This returns an `OracleProc` / `SemanticProc` value - a first-class procedure.

**Now SICP higher-order programming works:**

```scheme
(map redact emails)                    ;; works
(filter (compose not spam?) emails)    ;; works
((compose summarize translate) doc)    ;; works
(fold merge-summaries "" (map summarize chapters))  ;; works
```

**`lambda` stays `lambda`. `oracle-lambda` is explicitly different.**

---

## Part 4: Data-Directed Dispatch (At the Value Level)

### 4.1 Keep Core Forms Pure

We do NOT dispatch inside `if`, `lambda`, `quote`, etc.
Instead, dispatch happens at **value interpretation** via generic operations.

### 4.2 The Key Generic Operation: `truthy`

```scheme
(define (truthy v)
  (apply-generic 'truthy v))
```

Dispatch table:

```scheme
(put 'truthy 'boolean (lambda (b) b))

(put 'truthy 'query
  (lambda (q)
    ;; Explicit intensional portal
    (effect infer.op (list :kind 'boolean-query :query q))))

(put 'truthy 'dist
  (lambda (d)
    ;; Policy decides: sample? maximize expected utility?
    (effect infer.op (list :kind 'choose-boolean :dist d))))

(put 'truthy 'meaning
  (lambda (m)
    ;; Interpret meaning's denotation
    (truthy (meaning->denotation m))))
```

### 4.3 Semantic Conditionals via `truthy`

Instead of overloading `if`, we use `truthy` explicitly:

```scheme
;; Pure if - works exactly as standard Scheme
(if (> age 18) "adult" "minor")

;; Semantic conditional - explicit truthy call
(if (truthy (query :text "Is this email sensitive?" :as 'boolean))
    (redact email)
    email)

;; Or with convenience macro:
(define-syntax if*
  (syntax-rules ()
    ((_ cond a b) (if (truthy cond) a b))))

(if* (query :text "Is this sensitive?") (redact email) email)
```

**`if` stays pure. Semantic dispatch happens through `truthy`.**

### 4.4 Another Generic Operation: `denote`

The more general version for schema-directed evaluation:

```scheme
(define (denote schema v)
  (apply-generic 'denote schema v))

;; Usage:
(denote 'boolean (query ...))           ;; inference → boolean
(denote '(list string) (prompt ...))    ;; inference → list of strings
(denote 'string (oracle-proc-call ...)) ;; inference → coerce to string
```

Convenience helpers:

```scheme
(define (ask? q) (denote 'boolean q))
(define (gen schema p) (denote schema p))
```

### 4.5 Apply Dispatch: The Other Switching Point

```
┌─────────────────────────────────────────────────────────────┐
│                        APPLY                                │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   Closure       →  eval body in extended env (pure)         │
│   Native        →  call primitive (pure)                    │
│   OracleProc    →  emit (effect infer.op ...)              │◄── HERE
│   Continuation  →  resume suspended computation             │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

**This is the only place in apply that's semantic-aware.**

---

## Part 5: Where LLM-Awareness Lives

Only **2 loci** know about semantic operations:

### 1. The Intensional Portal Operators

```scheme
(int qexpr)           ;; → effect infer.op
(infer goal)          ;; → effect infer.op
(rewrite expr :goal)  ;; → effect infer.op
```

### 2. Apply Case for OracleProc

```scheme
;; In apply:
(match proc
  [(OracleProc params spec env policy)
   (effect infer.op ...)]   ;; ◄── HERE
  ...)
```

### 3. Library-Level Generic Helpers

```scheme
(truthy v)    ;; May emit effect if v is Query/Dist/Meaning
(denote s v)  ;; May emit effect for semantic types
```

**Everything else is standard Lisp:**

```
┌─────────────────────────────────────────────────────────────┐
│                         EVAL                                │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   Lit, Var       →  pure (no LLM)                          │
│   Quote          →  INERT - returns datum unchanged         │
│   If             →  PURE - evaluates test, branches         │
│   Lambda         →  STABLE - creates Closure                │
│   Begin          →  sequences expressions                   │
│   Define, Set!   →  binds/mutates                          │
│   App            →  calls apply (dispatch there)            │
│   Effect         →  emits effect directly                   │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

**Primitives remain pure:** `cons`, `car`, `cdr`, `+`, `-`, `map`, `filter` - no LLM awareness.

---

## Part 6: The Aspect Stack

Below the Core 7, we inject **cross-cutting concerns**:

```
┌─────────────────────────────────────────────────────────────┐
│                     THE ASPECT STACK                        │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   EFFECTS         Typed interrupts that suspend execution   │
│                   (effect infer.op ...)                     │
│                   (effect tool.op ...)                      │
│                   (effect commit.op ...)                    │
│                                                             │
│   HANDLERS        Intercept effects, provide implementation │
│                   (handle body (on infer.op (x k) ...))    │
│                                                             │
│   PROFILES        Named bundles of policies                 │
│                   strict / pragmatic / explore / yolo       │
│                                                             │
│   BUDGETS         Resource limits (oracle calls, steps)     │
│                   { maxOracleTurns: 100, maxSteps: 500000 } │
│                                                             │
│   LEDGER          Audit trail for replay & measurement      │
│                   EffectEmit, OracleReq, OracleResp, Commit │
│                                                             │
│   MACHINE         Reified execution state (Prompt 8)        │
│                   machine/step, machine/fork, breakpoints   │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

**Same program, different profile = different behavior:**

```scheme
(if* (query :text "is this spam?") (delete msg) (deliver msg))

;; Profile: strict
;;   → ERROR: infer.op not allowed

;; Profile: pragmatic
;;   → LLM called, result cached, budget decremented

;; Profile: explore
;;   → LLM called, speculative (not committed)

;; Profile: mock
;;   → Scripted response returned (testing)
```

The program doesn't know about profiles. The aspect stack enforces policy.

---

## Part 7: The Full Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    ANY LISP DIALECT                         │
│         Common Lisp  /  Scheme  /  Clojure  /  DSL          │
└────────────────────────────┬────────────────────────────────┘
                             │
                             │  macro expansion
                             │  desugaring
                             ▼
┌─────────────────────────────────────────────────────────────┐
│                       CORE 7 FORMS                          │
│        quote / if / lambda / begin / define / set! / app    │
│                     + primitives                            │
└────────────────────────────┬────────────────────────────────┘
                             │
                             │  eval/apply (forms stay pure)
                             │  semantic dispatch at value level
                             ▼
┌─────────────────────────────────────────────────────────────┐
│                    EVAL / APPLY                             │
│                                                             │
│    ┌─────────────┐     ┌─────────────┐     ┌─────────────┐ │
│    │ Computation │     │ OracleProc  │     │   Effect    │ │
│    │   (pure)    │     │   Apply     │     │  Emission   │ │
│    └─────────────┘     └─────────────┘     └─────────────┘ │
│                                                             │
└────────────────────────────┬────────────────────────────────┘
                             │
                             │  effects bubble up
                             ▼
┌─────────────────────────────────────────────────────────────┐
│                     HANDLER STACK                           │
│                                                             │
│    ┌─────────────┐     ┌─────────────┐     ┌─────────────┐ │
│    │   Profile   │     │   Budget    │     │   Ledger    │ │
│    │   Handler   │     │   Handler   │     │   Handler   │ │
│    └─────────────┘     └─────────────┘     └─────────────┘ │
│                                                             │
└────────────────────────────┬────────────────────────────────┘
                             │
                             │  external calls (if allowed)
                             ▼
┌─────────────────────────────────────────────────────────────┐
│                    EXTERNAL WORLD                           │
│              LLM APIs  /  Tools  /  Storage                 │
└─────────────────────────────────────────────────────────────┘
```

---

## Part 8: Why This Architecture Wins

### 1. Universal Backend
Any Lisp dialect compiles to Core 7. No translation between dialects.

### 2. Semantic Functions as First-Class
OracleProcs are just procedures. Compose, map, memoize - it all works.

```scheme
(define classify (oracle-lambda (t) ...))
(map classify sentences)           ;; works
(filter is-actionable? emails)     ;; works
(compose summarize translate)      ;; works
```

### 3. Explicit Intent, No Magic
Semantic operations are explicit. No "string sometimes calls LLM".

```scheme
(if #t a b)                                  ;; computational
(if (truthy (query :text "x?")) a b)         ;; semantic - EXPLICIT
```

### 4. Homoiconicity Preserved
Quote is inert. Lambda is stable. Macros work correctly.

```scheme
'(query :text "x")           ;; Returns the datum, no inference
((lambda (x) x) "hello")     ;; Returns "hello", not SemanticProc
```

### 5. Optimization is Measurable
Effects are explicit. Ledger records everything.

```
Before: 15 infer.op emissions
After:   3 infer.op emissions
Output:  identical
```

### 6. Governance Without Code Changes
Same program behaves differently under different profiles.
Policy is configuration, not code modification.

### 7. Hermetic Replay
Receipts + snapshots = deterministic replay.

```typescript
transcript = run(program, realOracle)
result = replay(program, transcript)  // no oracle calls
assert(result == originalResult)
```

---

## Part 9: Comparison to Alternatives

### vs. "Just Call the LLM API"

| Aspect | Raw API | OmegaLLM |
|--------|---------|----------|
| Composition | Manual prompt chains | First-class functions |
| Caching | DIY | Effect handlers |
| Debugging | Print statements | Time-travel, breakpoints |
| Testing | Mock HTTP | Hermetic replay |
| Governance | Hope | Profile-enforced |
| Optimization | None | Compiler passes |

### vs. LangChain / LlamaIndex

| Aspect | LangChain | OmegaLLM |
|--------|-----------|----------|
| Paradigm | Object graphs | Lambda calculus + effects |
| Composability | Limited | Unlimited (it's Lisp) |
| Inspection | Callbacks | Full machine state |
| Formal semantics | No | Yes (CESK machine) |

### vs. DSPy

| Aspect | DSPy | OmegaLLM |
|--------|------|----------|
| Optimization | Gradient-based | Symbolic rewriting |
| Transparency | Black box | White box |
| Language | Python embedded | First-class language |

---

## Part 10: The Prompt Arc

| Prompt | Capability Added |
|--------|------------------|
| 1-2 | Core eval/apply, basic primitives |
| 3 | Effect system, algebraic handlers |
| 4 | Oracle protocol, semantic procedures |
| 5 | Generic operations, data-directed dispatch |
| 6 | Streams, lazy evaluation, rewriting |
| 7 | Metacircular evaluator, debug harness |
| **8** | **Machine reification, ANF, optimizer** |
| 9 | Profiles as handler stacks, full governance |

---

## Part 11: Summary

**OmegaLLM is:**

1. **A minimal core** - 7 forms (quote/if/lambda/begin/define/set!/app)
2. **Explicit semantic values** - Prompt/Query/Goal as tagged data
3. **Value-level dispatch** - truthy/denote for semantic interpretation
4. **An aspect stack** - effects, handlers, profiles, budgets, ledger
5. **A universal Lisp backend** - any dialect can target it
6. **A measurable, optimizable substrate** - compiler for semantic computation

**The Core 7 gives you the language.**
**Explicit semantic values give you intent.**
**Value-level dispatch gives you interpretation.**
**The aspect stack gives you governance.**

Any Lisp that targets Core 7 gets all of it for free.

---

## Part 12: Multi-Dialect Support

All major Lisps are open source. We don't need their runtimes - just parsers.

### Available Implementations

| Dialect | Implementation | License |
|---------|----------------|---------|
| Scheme | Chez Scheme | Apache 2.0 |
| | Guile | GPL |
| | Chicken | BSD |
| Common Lisp | SBCL | Public domain |
| | CCL | LGPL |
| Clojure | Clojure | EPL |
| Racket | Racket | Apache 2.0 / MIT |

### The Insight: 90% Shared Parsing

All Lisps are S-expressions:

```
(keyword arg1 arg2 ...)
```

We already have an S-expression parser. Dialect differences are just keyword mappings:

| Dialect | Function def | Boolean | Let syntax |
|---------|--------------|---------|------------|
| Scheme | `(define (f x) ...)` | `#t` `#f` | `(let ((x 1)) ...)` |
| Common Lisp | `(defun f (x) ...)` | `T` `NIL` | `(let ((x 1)) ...)` |
| Clojure | `(defn f [x] ...)` | `true` `false` | `(let [x 1] ...)` |

### Implementation Strategy

```
┌─────────────────────────────────────────────────────────────┐
│                     DIALECT FRONTENDS                       │
│                                                             │
│   scheme.ts        common-lisp.ts        clojure.ts        │
│   ~200 lines       ~200 lines            ~300 lines        │
│                                                             │
│   - keyword map    - keyword map         - keyword map     │
│   - #t/#f          - T/NIL               - true/false      │
│   - define         - defun/defvar        - defn/def        │
│   - reader macros  - reader macros       - [vectors]       │
│                                                             │
└─────────────────────────────┬───────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                   SHARED S-EXPR PARSER                      │
│                      (already exists)                       │
└─────────────────────────────┬───────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                        CORE 7 IR                            │
│     quote / if / lambda / begin / define / set! / app       │
└─────────────────────────────┬───────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                  OMEGALLM EVAL / APPLY                      │
│         (semantic dispatch at value level via truthy)       │
└─────────────────────────────────────────────────────────────┘
```

### CLI Usage

```bash
$ omega run program.scm              # Scheme
$ omega run program.lisp             # Common Lisp
$ omega run program.clj              # Clojure
$ omega run program.rkt              # Racket
$ omega run program.omega            # Native OmegaLLM
```

Same engine. Different frontends. Full semantic capabilities for all.

---

## Part 13: What Changes vs. What Stays Pure

### Primitives: STAY PURE (no changes needed)

The ~100 primitives we already have are computational "hardware":

```
Arithmetic:    + - * / < > = <= >= modulo
Pairs:         cons car cdr
Lists:         list append reverse length map filter fold
Strings:       string=? string-append substring string-split
Predicates:    null? pair? number? string? boolean? procedure?
Streams:       stream-car stream-cdr stream-map stream-filter
Generic:       make-op-table op-table-put apply-generic
Rewriting:     make-rule rewrite/once rewrite/fixpoint
```

**These don't know about LLM. They never will. They're pure functions.**

### Core Forms: STAY PURE

| Form | Behavior | Semantic Equivalent |
|------|----------|---------------------|
| `quote` | INERT - returns datum | Use `(infer (goal ...))` explicitly |
| `if` | PURE - evaluates test | Use `(if (truthy q) ...)` |
| `lambda` | STABLE - creates Closure | Use `oracle-lambda` for semantic procs |
| `begin` | Sequences expressions | No change needed |
| `define` | Binds value | Bind result of `(infer ...)` |
| `set!` | Mutates binding | Assign result of `(infer ...)` |
| `app` | Dispatches on proc type | OracleProc → emits effect |

### The Boundary

```
┌─────────────────────────────────────────────────────────────┐
│  PRIMITIVES (pure, no LLM)                                  │
│  +, cons, map, filter, string-append, ...                   │
│                                                             │
│  These work on VALUES. Always computational.                │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│  CORE 7 FORMS (ALL STAY PURE)                               │
│  quote, if, lambda, begin, define, set!, app                │
│                                                             │
│  NO implicit semantic dispatch here.                        │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│  VALUE-LEVEL DISPATCH (truthy, denote)                      │
│  Query/Prompt/Goal → emit effect                            │
│  Boolean/Number/String → pure                               │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│  APPLY DISPATCH                                             │
│  Closure → eval body (pure)                                 │
│  OracleProc → emit effect (semantic)                        │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│  EFFECT HANDLERS (policy layer)                             │
│  Intercept effects, apply profiles, enforce budgets         │
└─────────────────────────────────────────────────────────────┘
```

### Example: Same Program, Explicit Semantic Intent

```scheme
;; Path 1: All computational (no LLM calls)
(define age 25)
(if (> age 18) "adult" "minor")
;; age is Number, (> age 18) is Boolean → pure computation

;; Path 2: Semantic (LLM calls) - EXPLICIT
(define age (infer (goal :kind 'synthesize
                         :prompt "typical age of college senior"
                         :schema 'number)))
(if (truthy (query :text "is this person an adult?" :as 'boolean))
    "adult"
    "minor")
;; Semantic intent is EXPLICIT via query/goal constructors
```

**No string magic. Explicit semantic values. Primitives untouched.**

---

## Part 14: Library Compatibility (The Shim Strategy)

### The Problem: I/O Escape Hatches

When existing Lisp code calls I/O libraries, we don't automatically get governance:

```scheme
;; Their existing code
(drakma:http-request "https://api.example.com/data")

;; To our eval/apply, this is just:
(apply some-function some-args)

;; We don't know it's HTTP. We can't:
;;   - Cache it
;;   - Budget it
;;   - Log it
;;   - Deny it based on profile
```

### The Split: What's Automatic vs. What Needs Shims

| Category | Examples | Automatic? | Governance? |
|----------|----------|------------|-------------|
| **Pure computation** | map, filter, fold, math | ✅ Yes | ✅ Full |
| **Semantic operations** | query, goal, oracle-lambda | ✅ Yes | ✅ Full |
| **Their I/O libraries** | HTTP, threads, files | ❌ No | ❌ None |
| **Their I/O with shims** | HTTP, threads, files | ✅ Yes | ✅ Full |

### The Solution: Compatibility Shims

We provide shim libraries that redirect their I/O to our effect system:

```
┌─────────────────────────────────────────────────────────────┐
│  THEIR CODE (unchanged)                                     │
│  (drakma:http-request url)                                  │
│  (bordeaux-threads:make-thread fn)                          │
│  (cl-json:decode response)                                  │
└─────────────────────────┬───────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│  COMPATIBILITY SHIMS (we provide)                           │
│                                                             │
│  ;; HTTP: redirect to effect                                │
│  (define drakma:http-request                                │
│    (lambda (url . opts)                                     │
│      (effect http.request url opts)))                       │
│                                                             │
│  ;; Threads: redirect to effect                             │
│  (define bordeaux-threads:make-thread                       │
│    (lambda (fn)                                             │
│      (effect thread.spawn fn)))                             │
│                                                             │
│  ;; JSON: pure, no shim needed                              │
│  (define cl-json:decode json-parse)  ; already pure         │
│                                                             │
└─────────────────────────┬───────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│  OUR EFFECT HANDLERS                                        │
│                                                             │
│  (handle program                                            │
│    (on http.request (url opts k)                            │
│      (if (profile-allows? 'http)                            │
│          (k (do-http-request url opts))                     │
│          (error "HTTP denied by profile")))                 │
│                                                             │
│    (on thread.spawn (fn k)                                  │
│      (if (profile-allows? 'threads)                         │
│          (k (do-spawn-thread fn))                           │
│          (error "Threads denied by profile"))))             │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

### Shim Libraries Per Dialect

```
shims/
├── common-lisp/
│   ├── drakma.scm          ;; HTTP client
│   ├── bordeaux-threads.scm ;; Threading
│   ├── cl-json.scm         ;; JSON (mostly pure)
│   ├── cl-ppcre.scm        ;; Regex (pure)
│   ├── usocket.scm         ;; Sockets
│   └── cl-fad.scm          ;; File utilities
│
├── scheme/
│   ├── srfi-18.scm         ;; Threading
│   ├── http-client.scm     ;; HTTP
│   └── json.scm            ;; JSON (pure)
│
└── clojure/
    ├── clj-http.scm        ;; HTTP
    ├── core-async.scm      ;; Async/channels
    └── cheshire.scm        ;; JSON (pure)
```

### What Each Shim Does

| Library | I/O Type | Shim Strategy |
|---------|----------|---------------|
| HTTP clients | Network | `→ (effect http.request ...)` |
| Threading | Concurrency | `→ (effect thread.spawn ...)` |
| File I/O | Filesystem | `→ (effect fs.read ...)`, `(effect fs.write ...)` |
| Sockets | Network | `→ (effect socket.connect ...)` |
| Database | Network + State | `→ (effect db.query ...)` |
| JSON/XML | Pure parsing | No shim needed (already pure) |
| Regex | Pure matching | No shim needed (already pure) |
| Crypto | Pure computation | No shim needed (already pure) |

### Benefits of Shims

1. **Their code unchanged** - `(drakma:http-request url)` still works
2. **Full governance** - Profile can allow/deny, budget can limit
3. **Caching** - Handler can cache HTTP responses
4. **Logging** - Every I/O recorded in ledger
5. **Testing** - Mock handlers for hermetic tests

### Example: Governed HTTP

```scheme
;; Their original Common Lisp code
(defun fetch-user (id)
  (let ((response (drakma:http-request
                    (format nil "https://api.example.com/users/~a" id))))
    (cl-json:decode response)))

;; Runs on OmegaLLM with shims:
;; 1. drakma:http-request → (effect http.request ...)
;; 2. Handler checks profile: HTTP allowed?
;; 3. Handler checks budget: Under limit?
;; 4. Handler logs to ledger
;; 5. Handler does actual HTTP (or returns cached)
;; 6. cl-json:decode runs pure (no shim needed)
```

### The Escape Hatch (When They Really Need It)

For cases where they MUST bypass governance (debugging, legacy, etc.):

```scheme
;; Explicit escape hatch - not recommended but available
(unsafe:raw-http-request url)  ;; Bypasses effect system

;; This is logged as a warning in the ledger
;; Profiles can forbid unsafe:* entirely
```

### Shim Size Estimates

| Dialect | Core Shims | Lines | Coverage |
|---------|------------|-------|----------|
| Common Lisp | ~10 libraries | ~500 | 80% of I/O |
| Scheme (R7RS) | ~5 libraries | ~300 | 70% of I/O |
| Clojure | ~8 libraries | ~400 | 75% of I/O |

Pure libraries (JSON, regex, crypto, math) need no shims - they just work.

---

## Part 15: The Complete Interop Story

### What "Just Works" (Zero Effort)

```scheme
;; ALL of this works automatically:

;; Pure computation
(+ 1 2 3)
(map square '(1 2 3 4 5))
(filter even? numbers)
(fold + 0 values)

;; Data structures
(cons 'a '(b c))
(assoc 'key alist)
(hash-ref table key)

;; String processing
(string-append "hello" " " "world")
(regexp-match pattern text)
(json-parse response)

;; Semantic operations (EXPLICIT)
(if (truthy (query :text "is this spam?")) delete-it keep-it)
(define summary (infer (goal :kind 'summarize :input doc)))
(define redact (oracle-lambda (x) (prompt :text "redact PII")))
```

### What Needs Shims (I/O)

```scheme
;; These need shims to be governed:

;; Network
(http-request url)        ;; → effect http.request
(socket-connect host port) ;; → effect socket.connect

;; Concurrency
(make-thread fn)          ;; → effect thread.spawn
(channel-put ch val)      ;; → effect channel.send

;; Filesystem
(read-file path)          ;; → effect fs.read
(write-file path content) ;; → effect fs.write

;; Database
(db-query sql)            ;; → effect db.query
(db-execute sql)          ;; → effect db.execute
```

### What's Forbidden (By Design)

```scheme
;; These are blocked or heavily restricted:

;; Raw FFI (escape hatch to host language)
(cffi:foreign-funcall ...)  ;; Blocked by default

;; Arbitrary eval (security risk)
(eval user-input)           ;; Requires explicit capability

;; Raw system calls
(sb-ext:run-program ...)    ;; → effect system.exec (governed)
```

### The Full Stack

```
┌─────────────────────────────────────────────────────────────┐
│                    USER'S LISP CODE                         │
│                                                             │
│  (defun process-data (url)                                  │
│    (let ((data (http-get url)))          ; I/O - shimmed    │
│      (if (truthy (query :text "valid?")) ; Semantic - explicit│
│          (transform data)                ; Pure - auto      │
│          (error "invalid"))))                               │
└─────────────────────────┬───────────────────────────────────┘
                          │
          ┌───────────────┼───────────────┐
          │               │               │
          ▼               ▼               ▼
┌─────────────┐   ┌─────────────┐   ┌─────────────┐
│    PURE     │   │  SEMANTIC   │   │    I/O      │
│             │   │             │   │   SHIMS     │
│ transform   │   │ (truthy q)  │   │ http-get    │
│ (just runs) │   │ → effect    │   │ → effect    │
└─────────────┘   └─────────────┘   └──────┬──────┘
                          │                │
                          ▼                ▼
                  ┌─────────────────────────────┐
                  │      EFFECT HANDLERS        │
                  │                             │
                  │  infer.op → Oracle          │
                  │  http.request → HTTP client │
                  │  fs.read → File system      │
                  │                             │
                  │  All governed by profile    │
                  │  All logged to ledger       │
                  │  All under budget           │
                  └─────────────────────────────┘
```

---

## Appendix A: Quick Reference

### Semantic Value Constructors

```scheme
;; Query: boolean question
(query :text "Is this spam?" :as 'boolean)

;; Prompt: generation request
(prompt :text "Summarize this" :schema 'string)

;; Goal: synthesis target
(goal :kind 'synthesize :contract spec :prefer '(pure))
```

### Generic Interpretation Operations

```scheme
;; truthy: semantic boolean interpretation
(put 'truthy 'boolean (lambda (b) b))
(put 'truthy 'query   (lambda (q) (effect infer.op ...)))
(put 'truthy 'dist    (lambda (d) (effect infer.op ...)))

;; denote: schema-directed interpretation
(put 'denote 'boolean (lambda (v) (truthy v)))
(put 'denote 'string  (lambda (v) ...))
(put 'denote 'list    (lambda (v) ...))
```

### Semantic Procedure Constructor

```scheme
;; oracle-lambda: creates OracleProc (not Closure)
(define redact
  (oracle-lambda (text)
    (prompt :text "Redact PII" :schema 'string)
    :policy policy:pragmatic))
```

### Effect Types

```scheme
(effect infer.op payload)      ;; Oracle inference
(effect tool.op name args)     ;; Tool invocation
(effect commit.op artifact)    ;; Commit to ledger
(effect amb.choose options)    ;; Nondeterministic choice
```

### Profile Capabilities

| Profile | Oracle | Tools | Commits | Semantic Ops |
|---------|--------|-------|---------|--------------|
| `strict` | ✗ | ✗ | ✗ | ✗ |
| `pragmatic` | ✓ | ✓ | test-certified | ✓ |
| `explore` | ✓ | ✓ | speculative | ✓ |
| `yolo` | ✓ | ✓ | ✓ | ✓ |

---

## Appendix B: Invariant Tests

These tests MUST pass to ensure the architecture is correct:

### Test A: Quote is Inert

```scheme
(evalOmega "'(query :text \"x\" :as 'boolean)")
;; Returns the datum unchanged, NO infer.op emitted
```

### Test B: Lambda Returns String When Body is String

```scheme
(evalOmega "((lambda (x) \"double it\") 10)")
;; => "double it"
;; NOT a SemanticProc, just a string
```

### Test C: Semantic Conditional Uses truthy

```scheme
(evalOmega "(if (truthy (query :text \"is 2+2=4?\" :as 'boolean)) 1 0)")
;; Must emit infer.op through truthy, not through if
```

### Test D: OracleProc is First-Class

```scheme
(evalOmega "((compose redact normalize) email)")
;; compose must work on OracleProc values
```

---

## Part 16: Feature Comparison (vs. Existing Lisps)

### What OmegaLLM Has That Others DON'T

| Feature | OmegaLLM | SBCL/CCL | Chez/Guile | Racket | Clojure |
|---------|----------|----------|------------|--------|---------|
| **Algebraic Effects** | ✅ Full | ❌ | ❌ | ⚠️ Partial | ❌ |
| **Explicit Semantic Values** | ✅ Query/Prompt/Goal | ❌ | ❌ | ❌ | ❌ |
| **Session Context** | ✅ Built-in | ❌ | ❌ | ❌ | ❌ |
| **Effect Ledger** | ✅ Full replay | ❌ | ❌ | ❌ | ❌ |
| **Profiles/Governance** | ✅ Native | ❌ | ❌ | ❌ | ❌ |
| **OracleProc (LLM)** | ✅ First-class | ❌ | ❌ | ❌ | ❌ |
| **Budget Tracking** | ✅ Built-in | ❌ | ❌ | ❌ | ❌ |
| **Delimited Continuations** | ✅ CESK | ❌ | ✅ | ✅ | ❌ |
| **Meaning Values** | ✅ Structured | ❌ | ❌ | ❌ | ❌ |
| **Distributions** | ✅ Nondeterminism | ❌ | ❌ | ❌ | ❌ |

### What They Have (That We Can Adopt)

| Feature | Source | Our Strategy |
|---------|--------|--------------|
| **CLOS (Object System)** | Common Lisp | Add `make-class`, `defmethod` via macros |
| **Condition System** | Common Lisp | Already covered by effect handlers |
| **Hygienic Macros** | Scheme/Racket | Already have `syntax-case` |
| **call/cc** | Scheme | Delimited continuations safer, sufficient |
| **Contracts** | Racket | Add as library (pure Scheme) |
| **Immutable Data** | Clojure | Add persistent structures as primitives |
| **core.async (CSP)** | Clojure | Expressible via effects + handlers |
| **Transducers** | Clojure | Already have streams + rewriting |

### The Key Insight

**Our effect system + sessions + ledger + explicit semantic values is UNIQUE.**

No existing Lisp has:
- Built-in LLM semantics with explicit value constructors
- Automatic governance of semantic operations
- Hermetic replay via receipts
- Profile-based policy enforcement
- Budget tracking for inference

**We're not replacing Lisps. We're making them AI-native.**

When code runs on OmegaLLM:
1. Their pure computation works unchanged
2. Their I/O gets governed via shims
3. They GAIN semantic capabilities through explicit constructs

### Complete Feature Matrix

```
┌──────────────────────────────────────────────────────────────────────────┐
│                        FEATURE COMPARISON                                 │
├──────────────────┬───────────┬───────┬───────┬────────┬─────────────────┤
│ Feature          │ OmegaLLM  │ CL    │ Scheme│ Racket │ Clojure         │
├──────────────────┼───────────┼───────┼───────┼────────┼─────────────────┤
│ SEMANTICS        │           │       │       │        │                 │
│ LLM integration  │ Native    │ None  │ None  │ None   │ None            │
│ Oracle protocol  │ ✓         │ -     │ -     │ -      │ -               │
│ Explicit values  │ ✓         │ -     │ -     │ -      │ -               │
│ truthy/denote    │ ✓         │ -     │ -     │ -      │ -               │
├──────────────────┼───────────┼───────┼───────┼────────┼─────────────────┤
│ PURITY           │           │       │       │        │                 │
│ Quote inert      │ ✓         │ ✓     │ ✓     │ ✓      │ ✓               │
│ Lambda stable    │ ✓         │ ✓     │ ✓     │ ✓      │ ✓               │
│ If pure          │ ✓         │ ✓     │ ✓     │ ✓      │ ✓               │
├──────────────────┼───────────┼───────┼───────┼────────┼─────────────────┤
│ EFFECTS          │           │       │       │        │                 │
│ Algebraic effects│ ✓         │ -     │ -     │ Partial│ -               │
│ Effect handlers  │ ✓         │ -     │ -     │ ✓      │ -               │
│ Delimited conts  │ ✓         │ -     │ ✓     │ ✓      │ -               │
│ Full call/cc     │ ✓ shift   │ -     │ ✓     │ ✓      │ -               │
├──────────────────┼───────────┼───────┼───────┼────────┼─────────────────┤
│ GOVERNANCE       │           │       │       │        │                 │
│ Sessions         │ ✓         │ -     │ -     │ -      │ -               │
│ Profiles         │ ✓         │ -     │ -     │ -      │ -               │
│ Budgets          │ ✓         │ -     │ -     │ -      │ -               │
│ Ledger/Audit     │ ✓         │ -     │ -     │ -      │ -               │
│ Hermetic replay  │ ✓         │ -     │ -     │ -      │ -               │
├──────────────────┼───────────┼───────┼───────┼────────┼─────────────────┤
│ OOP              │           │       │       │        │                 │
│ Object system    │ Library   │ CLOS  │ SRFI  │ Classes│ Protocols       │
│ Generic functions│ ✓         │ ✓     │ SRFI  │ ✓      │ Multimethods    │
│ Multiple dispatch│ Planned   │ ✓     │ -     │ -      │ ✓               │
├──────────────────┼───────────┼───────┼───────┼────────┼─────────────────┤
│ METAPROGRAMMING  │           │       │       │        │                 │
│ Macros           │ ✓         │ ✓     │ ✓     │ ✓      │ ✓               │
│ Hygienic macros  │ ✓         │ -     │ ✓     │ ✓      │ -               │
│ Reader macros    │ Planned   │ ✓     │ ✓     │ ✓      │ Limited         │
│ syntax-case      │ ✓         │ -     │ ✓     │ ✓      │ -               │
├──────────────────┼───────────┼───────┼───────┼────────┼─────────────────┤
│ CONCURRENCY      │           │       │       │        │                 │
│ Threads          │ Effect    │ ✓     │ SRFI  │ ✓      │ ✓               │
│ Channels/CSP     │ Effect    │ -     │ -     │ ✓      │ core.async      │
│ Futures          │ Effect    │ ✓     │ -     │ ✓      │ ✓               │
│ STM              │ Effect    │ -     │ -     │ -      │ ✓               │
├──────────────────┼───────────┼───────┼───────┼────────┼─────────────────┤
│ DATA             │           │       │       │        │                 │
│ Persistent data  │ Planned   │ -     │ -     │ -      │ ✓ Native        │
│ Streams/Lazy     │ ✓         │ Series│ SRFI  │ ✓      │ ✓               │
│ Transducers      │ Streams   │ -     │ -     │ -      │ ✓               │
├──────────────────┼───────────┼───────┼───────┼────────┼─────────────────┤
│ TOOLING          │           │       │       │        │                 │
│ REPL             │ ✓         │ ✓     │ ✓     │ ✓      │ ✓               │
│ Debugger         │ Machine   │ ✓     │ ✓     │ ✓      │ ✓               │
│ Time-travel      │ ✓         │ -     │ -     │ -      │ -               │
│ Breakpoints      │ ✓ Effect  │ ✓     │ ✓     │ ✓      │ ✓               │
└──────────────────┴───────────┴───────┴───────┴────────┴─────────────────┘

Legend: ✓ = Native support, - = Not available, Partial = Limited support
        Effect = Implemented via effect handlers
        Library = Available as add-on library
        Planned = On roadmap
```

### Why This Matters

1. **We preserve homoiconicity** - Quote is inert, macros work correctly
2. **We preserve referential transparency** - Same expression → same value
3. **We ADD to dialects** - Any Lisp gains semantic capabilities
4. **Governance is novel** - No one else has profile-enforced LLM access
5. **Replay is unique** - Hermetic testing of semantic code is ours alone
6. **The effect system unifies** - I/O, LLM, concurrency all become effects

**OmegaLLM is the first homoiconic, semantically-aware Lisp execution substrate.**
