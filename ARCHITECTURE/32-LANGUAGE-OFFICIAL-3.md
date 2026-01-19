---

## High-Level Goal

Omega is a **homoiconic metacircular language** for inference. It is not an LLM wrapper or API abstraction—it is a programming language whose `eval` and `apply` operate on **semantic functions** exactly as Lisp's `eval` and `apply` operate on syntactic procedures.

**Semantic functions are first-class values.** You define them with prompts:

```lisp
(define-semantic (find-slurs text)
  "Examine {text} and return any slurs found as a list")
```

And use them exactly like normal functions:

```lisp
(find-slurs my-text)                    ; → invokes inference
(map find-slurs list-of-texts)          ; → HOF works
(define find-in (curry find-slurs))     ; → currying works
```

**Composition happens via the call stack**, not as an escape hatch:

```lisp
(remove text (find-slurs text))
```

Here, the system determines that `find-slurs` requires inference, evaluates it semantically, then passes the result to `remove`—which may itself decide whether to operate syntactically (string manipulation) or semantically (grammar-preserving removal). This routing is transparent to the programmer.

**All SICP principles apply to inference:**
- Currying and partial application
- Higher-order functions
- Recursion and self-reference
- Data abstraction and closures
- Streams and lazy evaluation
- Metacircular interpretation

**Undefined functions may be inferred from their names.** If `(summarize document)` is called and `summarize` is not defined, the system may attempt to infer its meaning from the symbol name and argument types.

This is the essential contract: **inference is not a tool you call—it is a plane of computation you program in**, with the full power of functional programming and SICP methodology.

---

## Ω Language Specification v1.0

### Part 3 — The SICP Reconstruction Kit: Re-deriving the Whole Tower (with an Inference Plane Everywhere)

This part is where Ω stops being “a kernel with a protocol” and becomes a **full language ecosystem** in the SICP sense:

* a small, principled kernel
* a tower of derived languages (embedded, compiled, meta-circular)
* a repertoire of “tricks” (streams, amb, generic ops, constraints, compilers)
* and now, for each trick: an **intensional dual** (analysis / simulation / rewrite / synthesis / certification)

I’ll do this as a **standard library + language-making toolkit**, because that is the SICP move: the language becomes powerful by **deriving** everything from a small evaluator and a few special forms, then making up languages as needed.

I’m going to define the “SICP kit” as a set of Ω modules. Each module will have:

* the **extensional** definitions (E-plane): runnable semantics
* the **intensional** counterparts (I-plane): `Meaning`-producing analyzers/rewriters
* the **governance layer** (truth/uncertainty/obligation hooks) so it never collapses into “LLM said so”

---

# 18. The Ω Standard Library Layout

The “balls-to-the-wall” way is not to hardwire features into the kernel; it’s to provide **a coherent module system** whose derived features are implemented as:

* macros (syntactic language-making)
* interpreters (semantic language-making)
* effect handlers (evaluation strategy language-making)
* certified rewrites (refactoring-to-patterns with obligations)
* receipts + VOI policies (context economics)

### Core modules

1. `omega.kernel` — raw primitives and canonical handlers
2. `omega.meta` — meta-circular evaluators + evaluator instrumentation
3. `omega.intension` — Meaning algebra, obligations, certification, inference strategies
4. `omega.identity` — semantic identity, equivalence relations, hashing, α-equivalence
5. `omega.test` — property/metamorphic tests, adversarial generators, regression harness
6. `omega.stream` — streams, laziness, memoized thunks, fusion + strictness analysis
7. `omega.nondet` — `amb`, backtracking, distributions, search strategies (DFS/BFS/beam/MCTS)
8. `omega.generic` — generic ops, coercion towers, data-directed dispatch + inferred glue
9. `omega.constraints` — propagators/constraints + diagnosis and repair suggestions
10. `omega.concurrent` — serializers/STM/actors + race/deadlock inference
11. `omega.compiler` — explicit-control evaluator, CEK machine, CPS, defunctionalization, register machines
12. `omega.match` — pattern matching, compilation to decision trees + exhaustiveness inference
13. `omega.tool` — structured “lisp-bash”, cached tool effects, receipts
14. `omega.ml` — models as values, training as effects, distillation, RL episodes

You don’t “add features.”
You add *libraries that are languages*.

That’s the Sussman move taken to the inference era.

---

# 19. `omega.identity`: Semantic Identity and Equivalence

This is foundational. Without it:

* receipts can’t be trusted
* caching is unsound
* rewrites drift
* memoization becomes folklore

Ω therefore defines **multiple equivalence relations** (each with a different strength), and treats them as first-class.

## 19.1 Equivalence relations

### 19.1.1 Syntactic equality

`eq-syn(e1,e2)` — literal structural equality of Expr ASTs.

### 19.1.2 α-equivalence

`eq-alpha(e1,e2)` — equal up to renaming of bound variables.

### 19.1.3 Extensional equivalence (behavioral)

`eq-ext(e1,e2, ρ, Envelope)` holds if extensional evaluation agrees within an envelope:

* on a domain restriction,
* under sealed dependencies,
* across regression/property tests,
* or via proof.

### 19.1.4 Intensional equivalence (meaning-level)

`eq-int(e1,e2, ρ, Policy)` holds if their `Meaning` objects are equivalent under a policy (same invariants/cost/effects/denotation distribution up to tolerance).

### 19.1.5 Approximate equivalence (ε-equivalence)

`eq-ε` for probabilistic/approximate computations:

* distributional similarity (`KL`, `TV`, Wasserstein)
* numerical tolerance
* confidence thresholds

## 19.2 Canonical obligations for rewrites

When `rewrite` returns a candidate `e'`, the default obligation is:

* either a proof certificate, **or**
* a test certificate (regression + property + metamorphic), **and**
* an environment digest constraint (so the equivalence is scoped)

So Ω makes “refactor” a **Command** with an attached **Proof Obligation**.

That’s “Refactoring to Patterns,” but enforceable.

---

# 20. `omega.intension`: Meaning Algebra + Inference Strategy as a Language

You asked “how far can we get” and “don’t let guards stop us.”
To do that cleanly, **inference strategies must be programmable**, not hardcoded.

So Ω defines a combinator language for inference, which compiles down to handler behavior for `infer.op`.

## 20.1 Meaning combinators (monoidal / lattice structure)

`Meaning` fields are combined by structural rules:

* `combine.denotation`: combine distributions (mixture / intersection depending on strategy)
* `combine.invariants`: set union with conflict detection
* `combine.effects`: join in effect lattice
* `combine.cost`: max/expected/capped, policy-controlled
* `combine.confidence`: policy algebra (min/logit-add/DS theory)
* `combine.obligation`: conjunction of required checks

These combinators are used to implement:

* self-consistency
* multi-engine triangulation
* speculative+verify pipelines

## 20.2 Inference strategy DSL (core)

An inference strategy is a value:

```lisp
(strategy
  (speculate :n 8 :temp 0.3)
  (triangulate :engines [llm smt profiler])
  (rewrite-if :improves {time 1.2} :preserve extensional)
  (verify :tests suite:regression :props suite:quickcheck)
  (escalate-if :confidence< 0.98 (verify :proof smt))
  (commit-if :obligations-satisfied true))
```

This is a **Pipes-and-Filters** architecture pattern, but semantic: each stage transforms `Meaning` and accumulates obligations/evidence.

### Design patterns embodied

* **Strategy**: choose inference/search/verification policy at runtime
* **Chain of Responsibility**: handler chain resolves `infer.op` and subrequests
* **Template Method**: “job skeleton” with overridable phases (plan/fetch/think/act/verify/pack)
* **Decorator**: wrap strategies with budgets, critics, auditors
* **Command**: each rewrite/verification step is a replayable command in Σ
* **Memento**: receipts snapshot states between stages

## 20.3 The “escalation ladder” (your rocket staging, formal)

A canonical strategy:

1. `int` cheap (pure intensional)
2. If insufficient confidence → `simulate` (intensional with REPL requests)
3. If still uncertain → `eval` on sampled inputs (extensional validation)
4. If still uncertain → `proof` or exhaustive tests
5. `seal` & `compress` once verified

This is literally a **Progressive Disclosure** / **Circuit Breaker** pattern applied to inference: don’t spend expensive resources until needed.

---

# 21. `omega.test`: The Adversarial & Metamorphic Test Arsenal

If inference is first-class, test generation is not optional; it’s part of the language’s truth model.

Ω provides three test paradigms:

1. **Example-based** unit tests (classic)
2. **Property-based** tests (QuickCheck style)
3. **Metamorphic** tests (critical for nondeterministic / LLM-ish systems)

## 21.1 Metamorphic tests

A metamorphic test asserts relations like:

* scaling invariance
* permutation invariance
* idempotence
* commutativity
* monotonicity

For rewrite validation, metamorphic relations are often stronger than random examples.

## 21.2 Adversarial test generation (red-team mode)

Ω provides:

```lisp
(fuzz expr :goal "break rewrite" :engine llm :budget 200)
```

and:

```lisp
(counterexample (eq-ext e e') :engine llm)
```

This is your “don’t let nondeterminism stop us” in practice: you defeat nondeterminism with adversarial harnesses and obligations.

---

# 22. `omega.meta`: Meta-circular Evaluators (Extensional + Intensional)

SICP’s central “magic trick” is the metacircular evaluator: implement the language in itself, then mutate the evaluator to create new languages.

Ω does that twice.

## 22.1 An object-language subset Ω₀

Define Ω₀ as Ω without:

* `int`, `infer`, `rewrite`, learning ops
  so we can write a clean metacircular extensional evaluator.

### 22.1.1 Expression classifier

```lisp
(define (self-evaluating? e) (or (number? e) (string? e) (bool? e)))
(define (variable? e) (symbol? e))
(define (quoted? e) (and (pair? e) (eq? (car e) 'quote)))
(define (lambda? e) (and (pair? e) (eq? (car e) 'lambda)))
(define (if? e) (and (pair? e) (eq? (car e) 'if)))
(define (begin? e) (and (pair? e) (eq? (car e) 'begin)))
(define (define? e) (and (pair? e) (eq? (car e) 'define)))
(define (set!? e) (and (pair? e) (eq? (car e) 'set!)))
(define (application? e) (pair? e))
```

### 22.1.2 Metacircular `eval0`

```lisp
(define (eval0 e env)
  (cond
    ((self-evaluating? e) e)
    ((variable? e) (lookup env e))
    ((quoted? e) (cadr e))
    ((lambda? e) (make-closure (cadr e) (caddr e) env))
    ((if? e) (if (truthy? (eval0 (cadr e) env))
                 (eval0 (caddr e) env)
                 (eval0 (cadddr e) env)))
    ((begin? e) (eval-seq0 (cdr e) env))
    ((define? e) (define0 e env))
    ((set!? e) (set0 e env))
    ((application? e)
      (apply0 (eval0 (car e) env)
              (map (lambda (a) (eval0 a env)) (cdr e))))
    (else (error "unknown expression"))))
```

This is classic SICP structure: classify, then interpret.

## 22.2 The intensional meta-evaluator: `int0`

Now the new move: a meta-level intensional evaluator that returns `Meaning`, not `Val`.

Its job is not to run the program, but to produce:

* residualization
* cost estimates
* invariants
* rewrite candidates
* simulated traces

In Ω, `int0` is not a single algorithm. It is a dispatcher over engines and strategies.

```lisp
(define (int0 qexpr env engine policy)
  (handle
    (effect infer.op {kind "int" expr qexpr env env engine engine policy policy})
    (on infer.op (req k) (resume k (oracle-handler req)))))
```

That looks like “just call the engine,” but it’s not: it’s *the portal into the oracle protocol* that can REPL against `eval0` under caps and budgets.

## 22.3 Meta-circularity payoff

Once you have both:

* a metacircular extensional evaluator
* a metacircular intensional evaluator interface

You can build:

* language variants (new special forms)
* optimized evaluators (CPS, lazy, strict, hybrid)
* and **intensional advisors** that can refactor those evaluators themselves.

That is “SICP, but the language understands itself.”

---

# 23. `omega.stream`: Streams + Inference (Strictness, Productivity, Fusion)

SICP uses streams to:

* make time explicit
* separate specification from control
* implement infinite structures

Ω reproduces streams but adds:

* **intensional strictness analysis**
* **productivity prediction**
* **fusion / deforestation as rewrite**
* **space-leak detection as Meaning**

## 23.1 Extensional streams (classic)

Define delayed thunks with memoization (a FP memo-thunk is literally a controlled `set!` inside a closure).

```lisp
(define (delay e)
  (let ((forced? false) (val #f))
    (lambda ()
      (if forced? val
          (begin (set! val e)
                 (set! forced? true)
                 val)))))

(define (force thunk) (thunk))

(define (cons-stream a b-thunk)
  (cons a b-thunk))

(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))
```

Then `stream-map`, `stream-filter`, etc.

## 23.2 Intensional stream tools

Now the inference-plane additions:

### 23.2.1 Strictness analyzer

```lisp
(define (int-strictness qexpr)
  (int qexpr :policy (strategy (speculate) (infer-invariants))))
```

It returns Meaning with inferred invariants such as:

* “argument 2 is lazy”
* “stream-cdr is demanded only under predicate P”
* “this consumer forces entire stream (bad)”

### 23.2.2 Productivity predictor

Productivity: will `stream-cdr` produce the next cell without diverging?

`int` can predict it; `infer` can search for a forcing pattern that avoids divergence.

### 23.2.3 Stream fusion as rewrite

Fusion is a rewrite goal:

```lisp
(rewrite '(stream-map f (stream-map g s))
         :goal { preserve extensional optimize allocations }
         :policy (strategy (rewrite-if ...) (verify ...)))
```

Here the rewrite is a semantic macro: a meaning-preserving refactoring with obligations.

---

# 24. `omega.nondet`: `amb`, Backtracking, Distributions, and Search Policies

SICP’s `amb` introduces nondeterministic computing by modifying the evaluator.
Ω already has nondeterminism as a plane (`Dist`) and effects/handlers.

## 24.1 Extensional `amb` via delimited continuations or effects

In Ω, define:

```lisp
(define (amb . choices)
  (effect amb.op choices))
```

Handler chooses a branch, with backtracking by resuming alternative continuations.

Policies implement:

* DFS
* BFS
* best-first
* beam
* probabilistic sampling

## 24.2 Intensional augmentation: heuristic branch ordering

The intensional plane can propose which branch to try first, reducing search explosion.

This is a huge “LLM understands code” win:

* it can infer constraints from problem structure,
* it can reorder choices,
* it can propose pruning predicates.

This turns classical `amb` into a modern heuristic solver while keeping semantics explicit.

## 24.3 Distributional view: unify `amb` and `infer`

You can interpret `amb` as:

* enumeration of a finite support distribution
* and then `infer` provides posterior scoring/selection based on goals

This gives you a probabilistic programming substrate for free.

---

# 25. `omega.generic`: Generic Operations + Inferred Coercions and Glue

SICP’s generic arithmetic uses:

* data-directed dispatch
* coercion towers
* type tags

Ω reproduces it, then adds intensional glue synthesis.

## 25.1 Extensional generic ops (classic)

* operation table: `op × type-tags → procedure`
* `apply-generic` dispatches and coerces

## 25.2 Intensional method synthesis

When an operation is missing, Ω can attempt:

* infer a coercion path,
* synthesize adapter methods,
* propose canonical representation.

But **not as silent magic**: as a `rewrite`/`infer` result with obligations:

```lisp
(infer {goal "define method for op" op 'add types '(matrix scalar)}
      :policy (strategy (speculate) (rewrite-if) (verify :tests ...)))
```

Then:

* `commit/method` installs the method into the generic op table (Command)
* receipts record why/how

This is “generic arithmetic evolves” as a language process.

Also: you can enforce coherence constraints (“no ambiguous dispatch”), making the system mathematically sane.

---

# 26. `omega.constraints`: Propagators + Intensional Diagnosis/Repair

SICP’s constraint systems are about local propagation and consistency.
Ω adds:

* diagnosis (why inconsistent)
* repair proposals (minimal edits)
* inferred missing constraints
* symbolic explanation graphs

## 26.1 Extensional propagators

Implement connectors/constraints (propagator network) as classic.

## 26.2 Intensional tools

* `int` over a network produces Meaning:

  * suspected contradictions
  * minimal unsatisfiable core (approx)
  * suggested constraint additions
  * explanation graphs (proof sketches)

Then `rewrite` can propose refactoring the network:

* factor constraints
* isolate subsystems
* add guards

Again: obligations are required before committing changes.

---

# 27. `omega.concurrent`: Concurrency + Race/Deadlock Inference

SICP’s concurrency chapter exposes the hazards of interleaving and motivates serializers.

Ω provides multiple concurrency models (POSA style: **Active Object**, **Reactor**, **Half-Sync/Half-Async**, **Actor**), and adds intensional analyzers.

## 27.1 Extensional concurrency primitives

* fibers: `spawn`, `await`
* STM: `atomically`
* locks/serializers: `make-serializer`

## 27.2 Intensional concurrency analysis

The intensional plane can infer:

* effect footprints (reads/writes sets)
* potential races (non-commuting writes)
* deadlock risks (lock order cycles)
* linearizability candidates
* recommended synchronization patterns

Then it can propose:

* lock ordering (global partial order)
* converting to STM
* actorization (move state into isolated processes)
* message passing with idempotent handlers

These are exactly POSA and GoF patterns, but driven by Meaning.

The key is: **analysis is a first-class intensional computation**, not a comment.

---

# 28. `omega.compiler`: Explicit-Control Evaluator, CPS, Defunctionalization, Register Machines

SICP’s final chapters show:

* explicit-control evaluator
* register machines
* compilation

Ω goes further: the intensional plane can **drive compilation** and verify transformations.

## 28.1 Extensional explicit-control evaluator (ECE)

We implement a CEK/CESK machine as a library interpreter for Ω₀ or a subset, where:

* `control` is explicit,
* continuations are data,
* evaluation steps are explicit.

This is the same move SICP makes: “turn evaluator into a machine.”

## 28.2 Intensional compilation pipeline

Now the new plane:

* `rewrite` can transform programs:

  * CPS transform
  * ANF (A-normal form)
  * SSA-like representation
  * defunctionalization
  * closure conversion
* `int` can estimate:

  * allocation counts
  * call overhead
  * likely hotspots

### The crucial part: certification

Every transformation is:

* a rewrite with an obligation
* discharged by:

  * equivalence tests,
  * metamorphic tests,
  * or proof (optional)

This is how you get a compiler that is:

* *learned-assisted* (intensional)
* *semantics-governed* (extensional verification)

This is the “LLM understands code” capability turned into compiler technology.

---

# 29. `omega.match`: Pattern Matching + Exhaustiveness Inference + Pattern Synthesis

You explicitly demanded this: pattern matching should have an inference-plane counterpart.

## 29.1 Extensional match (compilable)

Ω offers:

```lisp
(match e
  ((list 'add a b) (+ a b))
  ((list 'mul a b) (* a b))
  (else (error "no match")))
```

Implementation options:

* kernel `match` special form, or
* macro expansion to nested conditionals, or
* compilation to decision trees (better asymptotics)

## 29.2 Intensional match: exhaustiveness & redundancy

`int` over a match yields Meaning with:

* uncovered pattern space (non-exhaustive)
* redundant clauses (shadowed patterns)
* suggested clause ordering (performance)
* example witnesses (counterexamples)

## 29.3 Pattern synthesis from examples (the new power)

Given examples, `infer` can propose patterns:

```lisp
(infer {goal "synthesize match clauses"
        examples [ {in '(add 1 2) out 3}
                   {in '(mul 2 3) out 6} ]}
      :policy (strategy (speculate) (rewrite-if) (verify :tests ...)))
```

This is not “AI magic.” It’s a derived language facility:

* pattern synthesis is search in program space,
* validated by extensional tests,
* committed with obligations.

That’s exactly how you keep it a language.

---

# 30. `omega.tool`: Structured Tooling, Caching, Receipts, Write-through

Your “lisp bash” idea is a key ergonomics win.

Ω provides:

* a command AST
* a pipeline DSL
* typed streams
* cached execution
* content-addressed artifacts
* receipts and replay plans

This is Enterprise Integration Patterns:

* **Pipes-and-Filters** (pipelines)
* **Message Channel** (artifact streams)
* **Aggregator** (combine outputs)
* **Resequencer** (reorder tasks)
  …and CI/CD patterns:
* **Build Pipeline**
* **Artifact Repository**
* **Immutable Infrastructure** (content addressing)

All expressed as language-level effects, not shell strings.

---

# 31. `omega.ml`: Models, Training, Distillation, and RL — as First-Class Language Objects

You asked for DNN abstractions and training: Ω can do this without embedding PyTorch semantics into the kernel by treating ML as:

* **models as values** (architecture graphs + parameters)
* **training as effects** (producing new versions + receipts)
* **prediction as effect or pure** (depending on engine)
* **distillation as rewrite/training** (teacher→student)

## 31.1 Model DSL (architecture as data)

```lisp
(define-model m:cnn
  (model
    (conv :k 3 :c 64)
    (relu)
    (pool :k 2)
    (dense :n 128)
    (softmax)))
```

This is an embedded DSL. It compiles to:

* tool effects to a backend (PyTorch/JAX/ONNX/etc.)
* receipts store training provenance

## 31.2 Training as explicit versioned transformation

```lisp
(define-model m:cnn/v2
  (train m:cnn/v1 dataset:images
        :objective "accuracy"
        :policy policy:train
        :requires { eval-suite pass }))
```

Training returns:

* new model version
* receipt (dataset hashes, hyperparams, metrics)
* evidence (logs, eval results)

## 31.3 RL for policy learning (especially inference policies)

This is the high-leverage use: learning **when to speculate vs eval**, **how to compress context**, **how to order search**, **how to call tools**.

Ω provides:

* `episode`
* `reward`
* `emit-example`
* `train/policy`

That yields an end-to-end self-improving evaluator configuration *without hidden mutation*.

---

# 32. The “SICP Reconstruction” Proper: Every Chapter Gets an Intensional Dual

Now, to satisfy your “every trick” demand, here is the canonical mapping.

### 32.1 Procedures and processes

* E-plane: recursion, iteration, environment model
* I-plane: cost inference, termination prediction, tail-call refactor

### 32.2 Higher-order procedures

* E-plane: map/fold/accumulate
* I-plane: fusion, algebraic law discovery, parallelization conditions

### 32.3 Data abstraction

* E-plane: constructors/selectors, representation independence
* I-plane: barrier violation detection, ADT extraction, invariant inference

### 32.4 Symbolic data

* E-plane: symbolic differentiation, algebra as lists
* I-plane: rewrite engine with certificates, simplification strategies

### 32.5 Generic operations

* E-plane: apply-generic, coercion tower
* I-plane: adapter synthesis, coercion inference, coherence checking

### 32.6 State

* E-plane: assignment, objects via closures
* I-plane: effect footprint inference, rollback plans, purity refactor

### 32.7 Concurrency

* E-plane: serializers, interleavings
* I-plane: race/deadlock inference, protocol synthesis

### 32.8 Streams

* E-plane: laziness, infinite data
* I-plane: productivity/strictness/space leak analyzers, fusion rewrites

### 32.9 Nondeterminism / amb

* E-plane: backtracking evaluator
* I-plane: heuristic pruning, probabilistic scoring, search strategy learning

### 32.10 Meta-circular evaluator

* E-plane: interpreter in itself
* I-plane: interpreter-understanding, self-optimization, proof obligations

### 32.11 Compiler / explicit-control evaluator

* E-plane: machine model, compilation
* I-plane: learned optimization proposals + verified transforms

That is “SICP, completed” under the new semantic plane.

---

# 33. What’s still left for Part 4 (the final pass)

We now have the kernel and the entire derived tower.

**Part 4** is the “final pass” you asked for:

1. A complete *reference* set of canonical Ω programs that demonstrate:

   * program synthesis with certification
   * automated refactoring to patterns
   * context-economic multi-stage jobs (plan→fetch→act→verify→compress)
   * policy learning for inference/tool/context decisions
2. A complete catalog of **inference-plane counterparts** for every core construct:

   * `lambda-int`, `apply-int`, `if-int`, `match-int`, etc. as standard policies
3. The final “language-making” chapter:

   * how to construct new DSLs in Ω (syntactic + semantic)
   * how to ship them as packages with proofs/receipts
4. A full “governance profile” system:

   * strict / pragmatic / exploratory modes
   * promotion pipelines for prompts/policies/engines/models
   * semantic versioning rules (what is a breaking change)

Reply **continue** and I’ll deliver Part 4 as the final “balls-to-the-wall” pass, with canonical programs, formal profiles, and the full catalog of dual-plane constructs so nothing is left implicit.
