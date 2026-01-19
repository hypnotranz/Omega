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

Alright — I'm going to **restart from scratch** and lay down a **coherent, unified, "balls‑to‑the‑wall" language** whose *primary axiom* is:

> **Inference is a first-class plane of computation** — co-equal with extensional evaluation — and must permeate *every construct* the way `eval`/`apply` permeate SICP.

I’m going to define the language as if it were a real PL spec: **ontology → kernel → static semantics → dynamic semantics → inference semantics → training semantics → governance → derived tower**.

I will not reduce this to “an API call.” That’s explicitly one of the non‑goals.

I’ll call the language **Ω** (Omega). Rename later.

---

# Ω Language Specification v1.0

## Part 1 — Foundations, Kernel, and Dual-Plane Semantics

---

## 0. Terminology

Ω is built on *four distinct semantic notions* that people often conflate:

1. **Extensional evaluation** (E): “run it”
2. **Intensional evaluation** (I): “understand it / simulate it / transform it”
3. **Search** (S): “explore a space of meanings / programs”
4. **Learning** (L): “update the intensional evaluator (prompt/memory/policy/weights)”

Classical languages model only (1) well. SICP gave us the methodology to make (1) explicit and malleable via `eval`/`apply`. Ω extends the same methodology to (2–4).

---

## 1. Language classification: what *type* of language is Ω?

Ω is **not** inherently a DSL. It is a **reflective homoiconic general-purpose metalanguage** (Lisp family in spirit), whose defining feature is a **dual-plane semantics**.

It can be deployed in three “SICP-adjacent” implementation modes (the common triad):

* **Standalone language** (its own reader/runtime)
* **Embedded language** (EDSL inside Scheme/Racket/CL/etc. via macros + interpreter)
* **Compiled/lowered** (Ω → IR/bytecode)

But this is about *implementation strategy*, not semantics. Semantically, Ω is a first-class language because:

* it has syntax and evaluation rules,
* inference is a **special form / effect** in the evaluator,
* it produces structured semantic artifacts (`Meaning`),
* it has a governance model (effects, capabilities, budgets, truth relations),
* learning is a first-class, versioned transformation (no hidden drift).

That is how it avoids degenerating into “just an API call.”

---

## 2. Design Goals (requirements distilled from our conversation)

Ω must support:

### 2.1 Dual-plane computation everywhere

Every construct has:

* an **extensional** meaning (“what happens if executed”)
* an **intensional** meaning (“what it implies / predicts / can be rewritten to”)

### 2.2 Quoted programs as a *primary substrate*

Quoted code is not merely data; it is the **bridge** between planes. Inference operates *natively* over quoted programs.

### 2.3 REPL re-entrancy under discipline

The intensional engine may:

* “run code in its head” (symbolic/speculative)
* *or* re-enter the extensional evaluator (“REPL against env”)
* *or* do hybrid (simulate then validate)

But always mediated by:

* capabilities,
* budgets,
* ledgered traces,
* and certification regimes.

### 2.4 Truth governance

Inference can be wrong. Ω must *represent and control*:

* uncertainty,
* confidence,
* evidence,
* certificates,
* and when substitution is legal.

### 2.5 Learning as a first-class plane

Two broad learning modalities must be representable:

* **Weight-level training** (changes the intensional evaluator implementation)
* **In-context / prompt / memory / policy adaptation** (changes evaluator configuration and strategy)

Both must be:

* explicit,
* versioned,
* replayable,
* test-gated.

### 2.6 Context economics (your “rocket staging”)

Context is a resource. Ω must support:

* snapshots,
* receipts,
* compression/hydration,
* VOI-driven retention,
* memoized research and tool results.

### 2.7 “Make up languages” to the limit

Ω must be a *language workbench*:

* hygienic macros (syntactic embedding)
* semantic macros (meaning-preserving rewrites with obligations)
* interpreter kits (meta-circular evaluators)
* MOP for evaluation strategy and inference strategy

---

## 3. Ontology: Core runtime entities

### 3.1 Context (lexical environment as persistent object)

A **Context** is a persistent frame chain (tree/DAG), not a mutable map.

```
Ctx = ⟨
  cid         : Hash?              ; content address (optional but recommended)
  id          : Sym
  parent      : Ctx | ⊥
  frame       : Map<Sym, Val>
  constraints : Set<Inv>
  caps        : CapSet
  sealed      : Bool
  evidence    : Seq<Evidence>
  receipt     : Receipt | ⊥
⟩
```

Properties:

* lexical scoping via parent chain
* persistence: `extend` returns a new `Ctx`
* sealed contexts restore referential transparency locally
* constraints and receipts implement context economics

### 3.2 Values

Ω is dynamically typed at the core (like Scheme), but can be layered with gradual/refinement typing.

```
Val ::=
    Atom
  | Pair(Val, Val)
  | Vector(Seq<Val>)
  | Map(Map<Val, Val>)
  | Ctx
  | Closure
  | Meaning
  | Dist<Val>
  | Engine
  | Prompt
  | Policy
  | Receipt
  | Evidence
  | Capability
  | Effect
  | Contract
  | Continuation
```

### 3.3 Closures

Closures are lexically scoped and carry effect/capability claims.

```
Closure = ⟨
  params    : Seq<Sym>
  body      : Expr
  env       : Ctx
  eff       : EffRow
  caps      : CapSet
  contract  : Contract | ⊥
⟩
```

### 3.4 Meaning (intensional semantic object)

This is the *canonical* intensional product; not a string.

```
Meaning = ⟨
  denotation    : Dist<Val> | Val | ⊥      ; predicted results
  residual      : Expr | ⊥                 ; partially evaluated residual program
  rewrite       : Expr | ⊥                 ; meaning-preserving transform candidate
  invariants    : Set<Inv>                 ; inferred properties
  effects       : EffSummary               ; inferred effect footprint
  cost          : CostModel                ; predicted cost (time/space/tool/token)
  paths         : PathSummary              ; symbolic path conditions/branch probs
  deps          : DepGraph                 ; call graph, dataflow, aliasing
  memo          : MemoTable | ⊥            ; memoization hints or computed table
  evidence      : Seq<Evidence>            ; citations/tests/tool outputs
  obligation    : Obligation | ⊥           ; what must be proven/checked to trust
  confidence    : Real in [0,1]            ; calibrated belief
  trace         : TraceSketch | ⊥          ; simulated evaluation trace
⟩
```

This makes “LLM understands code” a formal artifact: the engine returns Meaning.

### 3.5 Engine / Prompt / Policy (three levers of “learning”)

Your key insight: “there are really two types of training” (weights vs in-context). Ω formalizes *three* levers:

* **Engine**: the semantic machine (weights / executable model)
* **Prompt**: the structured in-context semantics program
* **Policy**: the inference strategy (search/verification/budget discipline)

```
Engine = ⟨ weightsRef, promptRef, memoryRef, policyRef, version, provenance ⟩
Prompt = PromptAST  ; typed structure, not raw string
Policy = Strategy object (search + verification + budgeting + tool rules)
```

Learning may update any of these explicitly, producing new versions.

---

## 4. Kernel Syntax (from scratch)

Ω is homoiconic; syntax is an s-expression surface. Everything else is derived.

### 4.1 Kernel expressions

Ω’s kernel is the smallest set that supports:

* SICP computation
* intensional semantics
* governance
* learning hooks

**Kernel forms**:

1. `quote`
2. `var` (symbol reference; implicit in surface syntax)
3. `lambda`
4. `apply`
5. `if`
6. `define`
7. `set!` (optional but included; effect-typed)
8. `begin`
9. `ctx` / `extend` / `seal`
10. `eval` (extensional reflective eval)
11. `int` (intensional evaluation → Meaning)
12. `infer` (search over meanings / programs → Dist<Meaning/Val>)
13. `rewrite` (meaning-preserving transformation with obligations)
14. `handle` / `effect` (algebraic effects and handlers; control)
15. `match` (pattern matching; we’ll also show how it can be derived via macros, but we keep it as a kernel convenience because it is practically evaluation control)

Everything else—macros, modules, objects, streams, amb, constraint systems, training DSLs—derives from these.

### 4.2 Concrete surface sketch

Examples:

```lisp
(define (f x) (+ (* x 2) 1))

(eval '(f 10))                         ; extensional run → 21
(int  '(f 10) :engine engine:solver)   ; intensional meaning → Meaning
(rewrite '(fib n) :goal {opt time})    ; suggested refactor + obligation
(infer {goal "synthesize function"} )  ; search over programs/meanings
```

---

## 5. Static Semantics: Effects, Capabilities, Epistemics

If you “take it all the way,” you must type-check not only values, but also:

* side effects,
* authority,
* uncertainty.

### 5.1 Effect rows (algebraic effects)

Judgment form:

[
\Gamma \vdash e : \tau ; ! ; \epsilon
]

Effects include at minimum:

* `pure` — referentially transparent
* `state` — mutation (`set!`)
* `tool` — external I/O / tools
* `infer` — intensional evaluation
* `nondet` — distributions/search
* `train` — learning emission/training
* `time` — scheduling/time
* `any` — unconstrained

Handlers can interpret or eliminate effects (Strategy pattern at the semantic level).

### 5.2 Capability discipline (object-capability security)

Inference must not become ambient authority. Every sensitive act requires a capability token.

* `cap.eval` (re-enter extensional evaluator)
* `cap.tool.fs.read`, `cap.tool.git.write`, etc.
* `cap.train.emit`, `cap.train.weights`, `cap.train.prompt`
* `cap.rewrite.commit`

Judgment:

[
\Gamma; \mathit{Caps} \vdash e ;\mathrm{ok}
]

`infer` is capability-guarded; tool use is capability-guarded; training is capability-guarded.

### 5.3 Epistemic typing (uncertainty as a tracked quantity)

This is what makes inference a plane, not an API. We attach *epistemic metadata* to expressions and values:

* `Conf(e)` confidence
* `Ent(e)` entropy
* `Risk(e)` risk / worst-case loss
* `Obl(e)` obligations (tests/proofs required)

Ω treats these as compositional “epistemic effects.” (We’ll define algebra later; for now: they exist and are carried in `Meaning` and `Dist`.)

---

## 6. Dynamic Semantics: Dual evaluators

This is the central formal pivot: Ω has two evaluation relations.

### 6.1 Extensional evaluation (E)

Big-step judgment:

[
\rho \vdash e \Downarrow v ;; \mathrm{with};\Sigma
]

* `ρ` is a context
* `Σ` is the ledger (event log / trace)

This is “SICP eval/apply,” plus effects and handlers.

### 6.2 Intensional evaluation (I)

Big-step judgment:

[
\rho \vdash e \Downarrow^\sim m ;; \mathrm{with};\Sigma
]

* returns `Meaning` `m`
* may request re-entrant extensional steps, but only via mediated operations
* may return distributions

### 6.3 Search (S)

Search is not ad hoc; it is a semantic operator that returns distributions:

[
\rho \vdash \mathrm{infer}(g,,engine,,policy) \Downarrow \mathcal{D}(m)
]

`infer` returns a `Dist<Meaning>` (or `Dist<Val>` depending on goal), and the policy determines:

* sampling vs enumeration vs beam vs MCTS
* verification regime
* budget enforcement
* tool use policy
* certificate requirements

---

## 7. Why `int` / `infer` / `rewrite` are not “API calls”

Because they are **special forms** / **effect operations** whose semantics include:

* construction of a `Meaning` value (structured semantic artifact)
* ledgered interaction protocol (requests/responses)
* capability checks
* budget enforcement
* obligation generation
* optional certification and promotion

A library call can’t do this without becoming a second evaluator. Ω makes that evaluator explicit and governed.

---

## 8. Learning plane: from scratch primitives (preview)

You asked: should the language have a way to send training cases back? Yes. But it must be explicit and versioned.

Ω introduces learning as first-class effects and definitional forms:

* `define-engine`, `define-prompt`, `define-policy`
* `specialize` (lexically scoped in-context adaptation)
* `emit-example`, `episode`, `reward` (for RL / dataset building)
* `train/prompt`, `train/weights`, `train/policy` producing new versions
* `promote` / `deprecate` (semantic version governance)

I’m not going to fully formalize those yet in Part 1; that’s Part 2.

---

# 9. SICP 7 primitives revisited (now properly “from scratch”)

SICP’s canonical special forms exist because they **control evaluation**. In Ω, each has a dual-plane counterpart (E and I). The important design point:

> **Every extensional construct has an intensional “lens”** that produces Meaning, can speculate, can simulate, can propose rewrites, and can generate obligations.

For kernel forms:

* `quote` is the bridge between planes
* `define` binds denotations (E-plane)
* `define-engine/prompt/policy` bind semantic machinery (I-plane)
* `set!` mutates E-plane; intensional analysis of state transitions is first-class

This yields your “augment define” instinct in a disciplined way:

* you *don’t* overload `define`
* you create a definitional family for denotations vs semantic machines

---

# 10. What comes next in Part 2

To complete the language “for real,” Part 2 must formalize:

1. **The oracle protocol** (interactive intensional evaluator that can REPL against extensional evaluator)
2. **Truth governance / soundness envelopes**

   * when you can substitute intensional predictions for extensional execution
   * certified vs uncertified meaning
3. **Uncertainty algebra** (confidence/entropy propagation operators)
4. **Training semantics**

   * prompt evolution (versioned, test-gated)
   * weight training (new engine versions)
   * RL for policies (context economics, tool-use, speculate-vs-eval)
5. **Context economics as semantics**

   * receipts, compression/hydration, VOI selection
6. **A “SICP reconstruction library”**

   * streams, amb, generic ops, constraint networks, meta-circular evaluator, etc.
   * each with an intensional counterpart

And then we can do the “final pass” where every SICP trick gets its inference-plane mirror.

---

If you want me to proceed, reply **continue** and I’ll deliver **Part 2**:
**Oracle protocol + truth/uncertainty governance + training/prompt evolution as definitional semantics**, all still from scratch and fully integrated.
