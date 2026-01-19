## Ω Language Specification v1.0

### Part 9 — The Remaining SICP Tower in Ω: `amb`/Backtracking, Streams, Generic Ops, Propagators, Explicit‑Control Evaluators, CPS/Compilation as Semantic Macros, and the Self‑Hosting Boot Plan

You typed “CONTIUE” but the intent is clear: keep going.

This part does exactly what you demanded earlier:

> Take *every major SICP trick* (and the “make up languages” thesis) and rebuild it in Ω **with an inference-plane counterpart** that is first‑class, governed, and compositional.

We’ll do it in six modules and one boot plan:

1. `omega.nondet` — nondeterminism (`amb`, `require`, backtracking), generalized to `Dist` + search strategies
2. `omega.stream` — streams + memo-thunks + fusion + strictness/productivity analysis (intensional duals)
3. `omega.generic` — generic operations + coercion tower + inferred adapters (commit-gated)
4. `omega.constraints` — propagators/constraints + diagnosis/repair meanings (intensional duals)
5. `omega.compiler` — explicit-control evaluator and compilation pipeline; CPS/ANF/SSA-like passes as **semantic macros** with obligations
6. `omega.meta` — meta-circular evaluator(s) + meta-intensional tower
7. Boot plan — how Ω becomes self-hosting without ever “turning into an API call”

I’ll give you:

* **semantics**
* **Ω code sketches**
* **handler skeletons**
* and the **inference-plane analogs** (Meaning fields, obligations, critics, and commit barriers)

---

# 82. `omega.nondet`: `amb`, Backtracking, Search Strategies, and Inference-Driven Heuristics

SICP’s `amb` is *the* canonical “change eval/apply and get a new paradigm” move. In Ω, `amb` is an **effect** and backtracking is a **handler interpretation**. This is not merely a library: it is literally a new evaluator regime installed by handlers, as in SICP’s nondeterministic evaluator chapter.

## 82.1 Surface API (Ω)

### Choice

```lisp
(amb 1 2 3)
```

### Constraint (fail current branch)

```lisp
(require (< x 10))
```

### Collect all solutions (or best solutions)

```lisp
(all-solutions (begin ...))
(best-solution :score score-fn (begin ...))
```

## 82.2 Core lowering

* `(amb e1 e2 ... en)` expands to `(effect "amb.op" e1 e2 ... en)`
* `(require p)` expands to `(if p (unit) (effect "amb.fail" "require failed"))`

This is **evaluation control**, not a function call.

## 82.3 Handler semantics: backtracking by capturing resumptions

The handler for `amb.op` receives:

* `args` = already-evaluated choices (values)
* `resume` = continuation to continue computation with a chosen value

The handler decides:

* which value to try first,
* how to store remaining alternatives,
* how to explore (DFS/BFS/beam/MCTS),
* whether to attach scoring,
* whether to stop early.

This is a **Strategy** at the semantic layer.

### Minimal DFS handler sketch

* On `amb.op`, pick first choice, push the rest onto a backtrack stack.
* On failure (exception or `amb.fail`), pop and resume with next alternative.
* If stack empty, propagate “no more choices”.

### Inference-plane counterpart

* The handler can call `int` on the *goal context* or on the *current constraint store* to estimate which branch has higher probability of success.
* That’s not “LLM said so”; it becomes a **heuristic policy**: a learned branch-ordering function, trained via episodes and reward.

## 82.4 `amb` as `Dist`: unify nondeterminism with probabilistic programming

Instead of merely enumerating, Ω allows a handler to interpret `amb` as:

* a finite-support distribution, or
* a weighted distribution (if choices carry weights), or
* a search frontier with scores.

That gives you SICP’s `amb` plus probabilistic programming plus learned heuristics under one abstraction: **handler-defined nondeterminism**.

## 82.5 Practical derived forms

### 82.5.1 `all-solutions`

Implement as a handler that exhaustively explores and collects results into a vector/list.

### 82.5.2 `best-solution`

Interpretations:

* best-first search using a priority queue
* beam search (keep top-k frontier)
* MCTS (when branching factor large)

These are **Patterns of Enterprise Application Architecture** in spirit (Unit of Work, Repository, etc.) but at the level of *search state*.

## 82.6 Inference dual: “amb with a critic”

A canonical modern policy:

1. attempt cheap branch ordering using `int` (Meaning.paths / predicted satisfiable constraints)
2. explore few steps
3. if stuck, escalate:

   * request `ReqEval` on small constraint checks
   * use a critic to generate counterexamples
4. emit episodes for policy learning (`train/policy`)

This turns SICP `amb` into a self-improving solver while preserving explicit semantics.

---

# 83. `omega.stream`: Streams, Memo-thunks, Fusion, Strictness & Productivity Analysis

Streams are SICP’s “make time explicit” trick. In Ω, streams are still streams—but now they also have *intensional analyzers* that tell you:

* where you force too much,
* where you leak space,
* where you can fuse,
* whether a producer is productive.

## 83.1 Extensional stream primitives (Ω)

### Lazy cell

```lisp
(define (delay e)
  ;; memo-thunk: internal mutation but encapsulated
  (let ((forced? #f) (v #f))
    (lambda ()
      (if forced? v
          (begin (set! v e)
                 (set! forced? #t)
                 v)))))

(define (force t) (t))

(define (cons-stream a d-thunk) (cons a d-thunk))
(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))
```

SICP’s trick is exactly here: controlled mutation inside `delay` to memoize. In Ω, that mutation is:

* explicit (`set!`), therefore effect-typed (`state`)
* locally encapsulated, therefore still reasoning-friendly when sealed properly

### Stream combinators

```lisp
(define (stream-map f s)
  (cons-stream (f (stream-car s))
               (delay (stream-map f (stream-cdr s)))))

(define (stream-filter p s)
  (if (p (stream-car s))
      (cons-stream (stream-car s)
                   (delay (stream-filter p (stream-cdr s))))
      (stream-filter p (stream-cdr s))))
```

## 83.2 Intensional dual: strictness analysis (where do we force?)

Provide a function:

```lisp
(define (stream.strictness qexpr)
  (int qexpr :policy policy:strictness))
```

The `Meaning` returned contains:

* `effects`: whether memo-thunks allocate state
* `paths`: which consumers force how much of the stream
* `cost`: asymptotic + allocation + expected forcing depth
* `rewrite`: fused form if possible
* `obligation`: tests/metamorphic checks required to trust fusion

### Why it’s first-class

This strictness analyzer is not a tool running outside the language. It is just `int` with a particular strategy and evidence regime.

## 83.3 Semantic macro: stream fusion as compile-time rewrite (obligation-carrying)

A semantic macro `stream-opt` (from Part 8) can rewrite:

```lisp
(stream-map f (stream-map g s))
```

into:

```lisp
(stream-map (lambda (x) (f (g x))) s)
```

But only if it can discharge obligations:

* extensional equivalence on regression/property suites
* productivity preserved
* no change in effects (besides allocation reduction)

This is **Refactoring to Patterns** (function composition, loop fusion) encoded as a semantic macro with proof obligations.

## 83.4 Space-leak detection and “progress” (productivity)

A productivity analyzer can attempt to prove:

* each `stream-cdr` call eventually yields another cell under constraints
* there is no hidden divergence in the producer chain

This can be done by:

* intensional symbolic evaluation of the producer,
* plus bounded extensional probes (ReqEval on prefixes),
* plus “critic” fuzzing for pathological forcing patterns.

The result is a `Meaning` with:

* `invariants`: “prefix of length k can be produced within budget”
* `confidence`: calibrated belief
* `obligation`: “run prefix tests up to k=…; verify no exceptions”

---

# 84. `omega.generic`: Generic Operations + Coercion Towers + Inferred Adapters

SICP’s generic arithmetic builds a “data-directed dispatch” system. In Ω, we do that—and then add a governed inference dual:

* if a method is missing, the system can *propose* an adapter/coercion path,
* but **never silently**: it must pass commit barriers and obligation checks.

## 84.1 Extensional generic op substrate

### Type tags

```lisp
(define (attach-tag t x) (cons t x))
(define (type-tag datum) (car datum))
(define (contents datum) (cdr datum))
```

### Operation table

Use a map keyed by `(op, type-list)`.

```lisp
(define op-table (make-map))

(define (put op types proc)
  (map-set! op-table (list op types) proc))

(define (get op types)
  (map-get op-table (list op types)))
```

### Apply-generic

```lisp
(define (apply-generic op . args)
  (let ((types (map type-tag args)))
    (let ((proc (get op types)))
      (if proc
          (apply proc (map contents args))
          (error "no method" op types)))))
```

## 84.2 Coercion tower (raise/project)

SICP often introduces coercions between numeric types.

In Ω:

* coercions are explicit procedures registered in another table
* the “raise path” may be searched

## 84.3 Inference dual: adapter synthesis + coercion path search

When `apply-generic` fails, a *policy* can choose to:

1. call `infer` with goal “find coercion path or synthesize method”
2. produce a candidate method or coercion chain
3. generate tests based on algebraic laws (commutativity, associativity, identity, distributivity)
4. `commit/method` only if obligations satisfied

This is exactly:

* GoF **Adapter** pattern (synthesized adapters),
* plus a **Factory Method** for method construction,
* plus a commit barrier (**Unit of Work**) for installing the method.

### Example synthesis goal (structured)

```lisp
(infer (record
         "goal" "define-method"
         "op" 'add
         "types" (list 'matrix 'scalar)
         "laws" (list 'associative 'distributive)
         "evidence" (list existing-methods))
      :policy policy:adapter-synthesis)
```

### Why it doesn’t become “magic”

Because:

* inference returns a `Meaning` with `rewrite` = candidate method definition
* `obligation` = law-based test suite + regression suite
* commit required to install into the op-table
* receipts capture provenance and allow rollback

---

# 85. `omega.constraints`: Propagator Networks + Diagnosis + Repair

SICP’s constraints chapter uses connectors/constraints and local propagation. Ω does that, and adds the missing modern capabilities:

* explicit explanation graphs (why inconsistent)
* minimal unsat cores (approx, then refine)
* suggested repairs (change constraints or add missing ones)
* inference-driven debugging (but commit-gated)

## 85.1 Extensional propagators (classical)

We define connectors (cells) with:

* a value (maybe unset),
* informant (who set it),
* constraints that depend on it.

Constraints propagate information.

This can be done with:

* a queue (worklist) of pending propagations
* effects to enqueue propagate events
* handlers to interpret propagation scheduling (FIFO, priority, incremental)

This is POSA **Reactor** pattern: events trigger updates.

## 85.2 Inference dual: `Meaning` for networks

`int` over a network yields:

* `deps`: graph structure, strongly connected components, bottlenecks
* `invariants`: implied equalities/inequalities
* `paths`: inconsistent cycles
* `rewrite`: proposed network refactoring (factor constraints)
* `obligation`: “run propagation on randomized assignments; check invariants; ensure no contradictions”

## 85.3 Diagnosis and repair as first-class goals

Two key operators:

* `(diagnose net)` → Meaning with suspected contradiction core
* `(repair net :objective …)` → candidate modifications + obligations

These are just `infer` goals over the program representing the network, with a strict policy and critics.

You can implement “repair” as:

* search over small edits (add/remove/modify constraint)
* scoring by satisfiable + minimal change + performance

That’s an explicit **Search** process, not a mystical “LLM fix it.”

---

# 86. `omega.compiler`: Explicit-Control Evaluator + CPS + ANF + Defunctionalization + Register Machines

This is where SICP turns interpreters into machines. Ω goes further: it makes compiler passes:

* explicit,
* composable,
* and inference-augmented but obligation-certified.

## 86.1 Explicit-control evaluator (ECE) in Ω

You already have a CEKS runtime in the host. Now implement an ECE *in Ω* as a library interpreter for a subset Ω₀:

* represent continuations as data
* represent evaluation states as data
* step function updates machine state

This is the **State** pattern + explicit transition function:

* makes stepping inspectable
* enables receipts for traces
* enables formal debugging and replay

## 86.2 Compilation passes as semantic macros

Define a compiler pipeline:

1. **Macro expand** to core
2. **Normalize**:

   * ANF (A-normal form)
   * or CPS
3. **Lower**:

   * closure conversion
   * defunctionalization (continuations and closures become tagged data + apply dispatch)
4. **Emit**:

   * bytecode or a register machine instruction set
5. **Verify**:

   * eq-ext tests / metamorphic tests
   * cost regression checks
6. **Commit**:

   * produce compiled artifact with receipt and promotion record

Every pass is a semantic macro:

* input Syntax → output Syntax/IR
* plus obligations and evidence

This is the compiler as a **Pipeline** (Pipes-and-Filters), and each pass is a **Command** that can be replayed.

## 86.3 Inference dual: learned optimization proposals (but never silent)

`rewrite` can propose:

* inlining decisions,
* loop transformations,
* memoization,
* data structure changes,
* algorithmic substitutions (e.g., use exponentiation by squaring, replace naive recursion with iteration)

But:

* each suggestion must become a candidate rewrite
* obligations must be discharged
* commit barrier is enforced
* promotion pipeline checks cost and correctness

So “LLM refactors code” becomes:

* a compiler pass suggestion,
* not a random mutation.

That’s how you keep the language principled.

---

# 87. `omega.meta`: Meta-circular eval and Meta-intensional eval

SICP’s most important “I see the Matrix” chapter is the metacircular evaluator. Ω must provide:

* `eval0` (extensional meta-evaluator) written in Ω
* `int0` (intensional meta-interface) that produces Meaning and can orchestrate inference sessions

## 87.1 Extensional meta-evaluator `eval0`

Implement the classic expression classifier and evaluator. You already know the structure:

* self-evaluating?
* variable?
* quoted?
* lambda?
* if?
* begin?
* define?
* set!
* application?

This gives you:

* a minimal object language
* introspection and redefinition ability
* the ability to make up languages by modifying the evaluator

## 87.2 Meta-intensional evaluator `int0`

This is new: an evaluator that doesn’t return values, but returns `Meaning`.

There are two useful designs:

### Design A — “Delegate to oracle”

`int0` mostly constructs an `infer.op` request, with a profile-specific policy.

### Design B — “Hybrid abstract interpreter”

`int0` implements:

* symbolic evaluation for pure constructs,
* residualization,
* cost modeling,
* and only calls the oracle for the hard parts (unknown primitives, opaque tool calls, etc.)

This is the best “take it all the way” design: it builds a *real intensional semantics* that is not merely “ask the LLM,” and then uses the LLM as a heuristic/coprocessor for the undecidable or high-complexity cases.

That is literally abstract interpretation + learned heuristics.

---

# 88. Self-Hosting Boot Plan: Ω becomes Ω (without collapsing into a framework)

This is the part that makes it a **language**, not a “runtime library.”

## 88.1 The staged boot strategy (Strangler Fig + Branch by Abstraction)

Use **Strangler Fig**:

* start with a minimal host implementation
* gradually replace host subsystems with Ω implementations, one boundary at a time

And **Branch by Abstraction**:

* define interfaces for reader, expander, evaluator, optimizer
* swap implementations without changing callers

### Stage 0 — Host kernel

* TS CEKS evaluator
* effect runtime
* oracle handler (mock first)
* minimal reader/expander

### Stage 1 — Stdlib in Ω

* implement `stream`, `nondet`, `generic`, `constraints` in Ω source
* load them at runtime (like a prelude)

### Stage 2 — Macro expander in Ω (phase-1 runtime)

* implement `syntax-rules` expander in Ω (or subset)
* keep a thin host bridge for hygiene scopes initially
* freeze expansions with receipts for reproducibility

### Stage 3 — Meta-circular evaluator in Ω

* implement `eval0` and validate against host evaluator using differential testing:

  * same input program → same outputs within envelope

### Stage 4 — Compiler passes in Ω

* implement ANF/CPS as semantic macros
* implement lower-to-bytecode/register machine
* keep host machine as reference

### Stage 5 — Replace host evaluator with compiled Ω

* run compiled bytecode with a VM written in Ω (or host VM first)
* now Ω programs compile and run inside Ω’s own ecosystem

### Stage 6 — Inference orchestration in Ω

* move strategy DSL and oracle orchestration logic into Ω code
* keep only minimal host adapters for actual model/tool calls

At every stage:

* receipts and promotion pipelines ensure you never “lose the plot”
* correctness is maintained via obligations and test suites

That is how you “take it all the way” without magical collapse.

---

# 89. The “Inference Counterpart” Table (SICP Trick → Ω Feature → Inference Plane)

Here’s the mapping you kept asking for, explicitly:

| SICP construct/trick       | Ω extensional implementation | Ω intensional counterpart (Meaning / infer / rewrite)              |
| -------------------------- | ---------------------------- | ------------------------------------------------------------------ |
| `eval/apply` meta-circular | `eval0` in Ω                 | `int0` (hybrid abstract interpreter + oracle)                      |
| `amb` evaluator            | `amb.op` + handler search    | heuristic ordering, learned policy, critic pruning                 |
| `require` constraints      | fail branch                  | infer missing constraints; suggest pruning predicates              |
| Streams (`delay/force`)    | memo-thunks + closure        | strictness/productivity analysis; fusion rewrite                   |
| Generic arithmetic         | op-table dispatch            | adapter synthesis; coercion path inference (commit-gated)          |
| Constraint networks        | propagators                  | diagnosis/repair; explanation graphs; inferred missing constraints |
| Explicit-control evaluator | CEK/CESK machine             | predicted hotspots; rewrite compilation pipeline suggestions       |
| Compiler/register machines | passes + VM                  | inference proposes optimization; obligations certify transforms    |

That is “every trick” covered at the architecture level; the rest is code.

---

# 90. What Part 10 would be

If you reply **continue**, Part 10 will be a *concrete working implementation path* for the two hardest remaining deliverables:

1. A full `amb` handler with:

   * DFS/BFS/beam
   * scoring
   * integration with `Dist`
   * a critic that uses `int` for heuristic pruning
   * episode/reward emission so the search policy learns

2. A complete hygienic `syntax-rules` expander with:

   * pattern compilation
   * scope-set hygiene
   * phase separation
   * semantic macro receipts
   * hermetic build mode

I’ll also give you the “reference programs” (the SICP-style demos) implemented in Ω:

* nondet logic puzzles
* stream-based numeric methods
* generic arithmetic tower
* propagator-based algebra solver
* compiler pass that rewrites a program and proves it equivalent by tests

Reply **continue**.
