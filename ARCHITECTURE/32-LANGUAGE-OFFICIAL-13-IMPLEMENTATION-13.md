# Omega Language Specification (Parts 1-5)

This document contains the core language specification including the dual-plane semantics and Oracle protocol.

---

<!-- SOURCE: 32-LANGUAGE-OFFICIAL-1.md -->

Alright — I’m going to **restart from scratch** and lay down a **coherent, unified, “balls‑to‑the‑wall” language** whose *primary axiom* is:

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


---

<!-- SOURCE: 32-LANGUAGE-OFFICIAL-2.md -->

## Ω Language Specification v1.0

### Part 2 — Oracle Protocol, Truth Governance, Uncertainty Algebra, Learning & Training Semantics, Context Economics

This part is where Ω stops being “a Lisp with `infer`” and becomes a **dual‑plane programming language** whose semantics, optimization, and evolution *explicitly* incorporate inference *without collapsing into an API call*.

You asked for “take it all the way.” The only way to do that is to make the following **first-class and formal**:

1. **How intensional evaluation interacts with extensional evaluation** (the REPL / re‑entrancy story)
2. **What it means for intensional results to be “trustworthy enough”** (soundness envelopes)
3. **How uncertainty propagates compositionally**
4. **How “training” exists in the language** (prompt/ICL adaptation, policy learning, weight training)
5. **How contexts evolve, compress, hydrate, and stay economical** (“rocket staging”)

---

# 11. Oracle Protocol: Intensional Evaluation as a Resumable, Re-entrant Effect

If inference is “in the language,” it cannot be a plain function call. It must be:

* **a special form** (i.e., evaluator-level semantics), *or*
* **an algebraic effect operation** handled by the evaluator’s control layer.

Ω chooses the latter because it yields maximum compositionality.

## 11.1 Core forms (kernel level)

We have three intensional entry points:

* `int` — “understand/simulate/analyze this expression” → `Meaning`
* `infer` — “search/synthesize over meanings/programs” → `Dist<Meaning>` (or `Dist<Val>`)
* `rewrite` — “propose a meaning-preserving rewrite under obligations” → `Meaning` (with `rewrite` field populated)

All three are desugared to effect emissions:

```lisp
(int e :engine E :policy P :env ρ)
≡ (effect infer.op {kind "int" expr e engine E policy P env ρ})

(infer g :engine E :policy P :env ρ)
≡ (effect infer.op {kind "search" goal g engine E policy P env ρ})

(rewrite e :goal G :engine E :policy P :env ρ)
≡ (effect infer.op {kind "rewrite" expr e goal G engine E policy P env ρ})
```

## 11.2 The infer-op handler is the “semantic coprocessor boundary”

A handler of `infer.op` implements the oracle protocol. It is responsible for:

* interacting with an engine (LLM/classifier/simulator/etc.)
* mediating **re-entrant evaluator requests**
* enforcing:

  * capabilities
  * budgets
  * constraints (no-new-facts, audited, deterministic envelope)
* logging every step to the ledger Σ
* producing a `Meaning` (or distribution of them) as the *semantic artifact*.

This is **Interpreter** (GoF) for the intensional plane, with **Strategy** controlling search/verification, and **Chain of Responsibility** via handler nesting.

---

## 11.3 Re-entrant requests: the “REPL against an environment” right

During an intensional session, the engine may request:

### Request algebra (normative)

```
Req ::=
    Req.Eval   (qexpr, envRef)                 ; extensional eval of quoted expr
  | Req.Apply  (procVal, args, envRef)         ; extensional apply
  | Req.Observe(ctxRef, projectionSchema)      ; summarize / project context safely
  | Req.Match  (qexpr, pattern, envRef)        ; structural match
  | Req.Tool   (toolCall)                      ; external tool call (capability-guarded)
  | Req.Test   (testSpec)                      ; run tests / property checks
  | Req.Assert (predicate, msg, severity)      ; enforce invariants
  | Req.Snapshot(ctxRef, meta)                 ; snapshot boundary
  | Req.Compress(ctxRef, policy)               ; compress / receipt
  | Req.Hydrate(receiptRef)                    ; hydrate receipt
  | Req.EmitExample(exampleRecord)             ; training data emission (capability-guarded)
  | Req.Return (meaningOrDist)                 ; finish
  | Req.Fail   (reason)                        ; abort
```

### Response algebra

```
Resp ::=
    Resp.Val(val)
  | Resp.Meaning(meaning)
  | Resp.Ctx(ctx)
  | Resp.Evidence(evidence)
  | Resp.Error(err)
```

**Important:** the engine never mutates the environment directly. It only **requests** operations. The evaluator owns all state transitions.

That is what prevents the “API call” collapse.

---

## 11.4 Capability and constraint enforcement (non-negotiable)

Each `Req.*` is checked:

1. **Capability check**

   * `Req.Eval` requires `cap.eval`
   * `Req.Tool` requires tool-specific caps
   * `Req.EmitExample` requires `cap.train.emit`
   * `Req.Compress/Hydrate` requires `cap.ctx.manage`

2. **Constraint check**
   Examples:

   * `no-new-facts`: engine cannot assert novel facts without evidence
   * `budget(tokens≤N, tools≤M)`: hard budget enforcement
   * `audited`: must emit receipt/evidence for key phases
   * `sealed`: cannot `extend`/mutate within sealed contexts

3. **Ledger append**
   Every request and response is event-sourced:

   * `Σ := Σ ⋅ Event(oracle.req, ...) ⋅ Event(oracle.resp, ...)`

This gives you:

* replayability
* debugging
* regression comparison
* training dataset extraction

---

## 11.5 Determinism envelopes for a nondeterministic engine

Inference is nondeterministic. Ω treats determinism as a **handler-level policy**, not a property of the engine.

A policy might specify:

* sampling regime: `n`, `temperature`, `top_p`
* search regime: `beam`, `best_first`, `mcts`, `dfs`, `iterative_deepening`
* consensus regime: `self_consistency`, `n_of_m`, `cross_engine_consensus`
* verification regime: `tests`, `proofs`, `metamorphic tests`, `oracle-check`
* acceptance criteria: confidence threshold, obligations satisfied

So “deterministic mode” is:

```lisp
(with-policy policy:det-envelope
  (int '(f 10)))
```

Semantically: it is `infer.op` handled by a `det-envelope` strategy that forces bounded variability and validation.

---

# 12. Truth Governance: How Intensional Results Become Usable

You’re designing a language where an evaluator can “understand code” and “simulate it.” That is powerful but epistemically unstable unless the language has **first-class truth management**.

Ω introduces **a lattice of trust** and a calculus for when intensional outputs can substitute extensional evaluation.

## 12.1 The core distinction: **Claim vs Certificate**

A `Meaning` contains:

* **claims** (predicted denotation, inferred invariants, rewrite candidates)
* **certificates / evidence** (tests passed, proofs, tool citations)

We define:

```
Claim    ::= proposition about program behavior/structure
Cert     ::= artifact that discharges an obligation
Obligation ::= requirement that must be satisfied to treat a claim as trusted
```

### Example obligation

* “rewrite must be extensional-equivalent on test suite T”
* “no-new-facts must cite evidence”
* “complexity claim must be supported by static proof or benchmark evidence”
* “memoization table must match extensional eval for sampled inputs”

## 12.2 Certification regimes (levels of semantic authority)

Ω defines four standard “trust modes” (handlers can implement additional modes):

1. **Speculative**

   * `Meaning` is advisory only
   * not substitutable for extensional evaluation
   * used for search, planning, refactoring suggestions

2. **Test-certified**

   * claims are trusted only within the *tested envelope*
   * obligations include regression/property/metamorphic tests
   * strong practical mode for software work

3. **Proof-certified**

   * claims are trusted under a proof system (SMT/coq/lean/etc.)
   * expensive, but yields genuine substitutability

4. **Consensus-certified**

   * claims validated by multi-engine agreement + minimal tests
   * useful when proofs are unavailable

These are **semantic patterns**. You do not want to bury them in application logic; they must be language-level because they govern substitution.

---

## 12.3 The substitution barrier: `commit` and `trust`

To prevent accidental “LLM said so” bugs, Ω introduces a barrier construct:

```lisp
(commit meaning :requires obligation-set)
```

* `commit` is *not* about writing to disk only.
* It is a **semantic promotion**: converting intensional artifacts into trusted program state (rewrites, memo tables, derived invariants).

### Promotion variants

* `commit/rewrite` — replace code with rewrite candidate (refactoring to patterns)
* `commit/memo` — install memo table
* `commit/invariants` — attach invariants/typing info
* `commit/prompt` — promote a tuned prompt version
* `commit/policy` — promote a learned policy

Each requires obligations.

This is how you keep “power” while avoiding “just an API call.”

---

## 12.4 A formal relation between planes: soundness envelopes

To be maximally rigorous, treat intensional evaluation as an **abstract interpretation** of extensional evaluation.

Let:

* Concrete semantics: ( \llbracket e \rrbracket_\rho ) (extensional)
* Abstract semantics: ( \llbracket e \rrbracket^#_\rho ) (intensional / Meaning)

Define:

* ( \alpha ) : concretization → abstraction
* ( \gamma ) : abstraction → concretization set

A Meaning `m` is **sound** for `e` in `ρ` if:

[
\llbracket e \rrbracket_\rho \in \gamma(m)
]

But: LLMs are not inherently sound. So Ω distinguishes:

* **Untrusted abstract result**: no guarantee
* **Certified abstract result**: obligations prove or test that soundness holds *within an envelope*

Thus soundness is not global; it’s **conditional**:

[
\text{Sound}(m, e, \rho \mid \mathcal{E})
]

where ( \mathcal{E} ) is an envelope (tested inputs, proof assumptions, bounded domain, sealed dependencies).

This matches your real-world intent: “not 100% but damn close — guarded.”

---

# 13. Uncertainty Algebra: How Belief Propagates Compositionally

If inference is a plane, uncertainty cannot be hand-waved. It must be:

* compositional (closed under language constructs),
* governable (handlers can tighten it),
* reducible (via verification),
* optimizable (VOI-driven context selection).

Ω models uncertainty via two coupled objects:

1. `Dist<Val>` — distribution over possible denotations
2. `Confidence` — belief that a claim is correct (calibrated)

## 13.1 Dist as the primary nondeterminism monad

`infer` returns distributions. Standard monadic operators exist:

* `map-dist`
* `bind-dist`
* `support` (finite or approximate)
* `sample`
* `score` (optional likelihood)

This is the basis for:

* `amb`-like nondeterminism
* probabilistic programming
* best-first search
* self-consistency

## 13.2 Confidence propagation (epistemic effect)

Ω attaches confidence to Meaning, and defines default propagation laws. These are not “true probability” in the philosophical sense; they’re an **epistemic semiring** used for governance.

### Core operators

* sequential composition: `conf_seq(a,b)`
* conjunction: `conf_and(a,b)`
* disjunction: `conf_or(a,b)`
* negation: `conf_not(a)`

A conservative default is:

* `conf_and(a,b) = min(a,b)`
* `conf_seq(a,b) = min(a,b)`

A less conservative but still monotone variant uses log-odds:

* `logit(c) = ln(c/(1-c))`
* combine independent evidence additively in logit space

Ω does not hard-wire one rule; it provides a policy hook:

```lisp
(policy :confidence-algebra "min" | "logit-add" | "ds-theory" | custom)
```

The important thing is: the language makes confidence **first-class and programmable**.

## 13.3 Risk and utility (so “optimize” is principled)

You were thinking in terms of efficiency and expected values. Ω therefore pairs confidence with:

* `CostModel` (tokens, time, tool calls)
* `RiskModel` (expected loss if wrong)

Then a default decision rule:

[
\text{Act if } \mathbb{E}[U] = P(\text{correct})\cdot U_{ok} + (1-P(\text{correct}))\cdot U_{bad} - C ;>; 0
]

This gives you a principled “when do we trust intensional vs re-enter extensional evaluator” policy.

And *that* is how the “rocket staging” optimization becomes something you can reason about formally.

---

# 14. Learning Semantics: Prompt/ICL, Policy RL, Weight Training — as Language Constructs

This is where your `define` intuition becomes fully coherent.

## 14.1 Three definitional families

### Extensional denotations

```lisp
(define (f x) ...)
```

### Intensional configuration programs

```lisp
(define-prompt prompt:solver ...)
(define-policy policy:strict ...)
```

### Engines (semantic machines)

```lisp
(define-engine engine:solver
  (engine :weights base:gpt :prompt prompt:solver :policy policy:strict :memory mem:default))
```

This avoids overloading `define`, but preserves your gestalt: “training feels definitional.”

---

## 14.2 In-context specialization (ICL) as lexical scoping

ICL must be represented as a **scoped specialization** of an engine configuration:

```lisp
(with-engine (specialize engine:solver
               :examples ex:domain
               :rules    [no-new-facts prefer-SSA]
               :memory   (retrieve mem:default :query q))
  (int '(f 10)))
```

Semantics:

* `specialize` returns a new engine value `engine'` (persistent/functional update)
* scope is lexical (like `let`)
* nothing mutates globally unless you `commit`/`promote`

This is crucial: it preserves reproducibility and prevents silent semantic drift.

---

## 14.3 Emitting training data as effects (never implicit)

Ω introduces effectful emissions:

* `emit-example`
* `episode`
* `reward`

These are not random logs; they are structured, typed, and ledgered.

### Example record schema (normative)

```
Example = ⟨
  engineVersion,
  promptVersion,
  policyVersion,
  envDigest,
  quotedProgram,
  goal,
  meaningPredicted,
  extensionalChecks,
  toolEvidence,
  reward,
  costProfile
⟩
```

This is the substrate for:

* prompt tuning datasets
* policy RL datasets
* fine-tuning datasets

---

## 14.4 Training operators: persistent updates with receipts

### Prompt tuning (fast)

```lisp
(train/prompt prompt:solver dataset :objective obj :policy pol)
  => (prompt:solver/v2, receipt)
```

### Policy training (RL / bandits)

```lisp
(train/policy policy:strict episodes :algorithm "RL" :objective obj)
  => (policy:strict/v2, receipt)
```

### Weight training (slow, privileged)

```lisp
(train/weights engine:solver dataset :algorithm "SFT|RLHF|DPO|..." :objective obj)
  => (engine:solver/W2, receipt)
```

All training returns:

* a **new versioned artifact**
* a **receipt** containing:

  * dataset hashes
  * replay plan
  * evaluation suite results
  * governance approvals

This is **Memento + Command + Event Sourcing** again, but now for learning.

---

## 14.5 Promotion and semantic versioning governance

To keep the system sane, Ω introduces lifecycle states:

* `experimental`
* `candidate`
* `trusted`
* `deprecated`

Promotion is explicit and obligation-gated:

```lisp
(promote prompt:solver/v2
  :requires { regressions pass, hallucination-rate < 0.1%, cost <= baseline*1.1 })
```

This is how “evolve the prompt to be more successful” becomes a **language-governed, test-driven** activity.

---

# 15. Context Economics: Snapshot, Receipt, Compression, Hydration, VOI

Now we formalize your rocket-stage idea as language semantics.

## 15.1 The ledger Σ is the backbone (Event Sourcing)

Σ records:

* extensional steps (optionally)
* tool calls and outputs (evidence)
* inference sessions (requests/responses)
* training emissions
* validation results
* commits and promotions

Σ allows:

* replay
* regression
* provenance
* dataset extraction

This is Fowler **Event Sourcing** as a runtime semantic substrate.

## 15.2 Snapshot and receipt

### Snapshot

```lisp
(snapshot ctx meta) -> Receipt
```

A receipt is a *compressed read model* of a context subtree:

```
Receipt = ⟨
  rid        : Hash,
  summary    : String,
  schema     : Shape,
  deps       : Seq<Hash>,        ; tool outputs / evidence hashes
  replay     : ReplayPlan,       ; command log
  checks     : Seq<CheckResult>, ; tests/proofs/constraints
  cost       : CostProfile,
  voi        : VOIProfile
⟩
```

Receipts let you drop large histories without losing auditability.

GoF patterns:

* **Memento** (state snapshot)
* **Command** (replay plan)
  Enterprise:
* **CQRS** (receipt is a read model)
* **Event Sourcing** (Σ is the source of truth)

## 15.3 Compress / hydrate

```lisp
(compress ctx :policy P) -> ctx'
(hydrate ctx receipt)    -> ctx''
```

* `compress` replaces subtrees with receipts
* `hydrate` restores them on demand

This is precisely your “drop rocket stages, keep placeholder receipts.”

## 15.4 VOI-driven retention (formal decision rule)

Represent context segments (s_i) with:

* length ( \ell_i ) (tokens)
* utility ( u_i ) (expected improvement in success/verification)
* retrieval cost ( r_i ) (hydrate/tool latency)
* staleness ( h_i )

Choose subset (S) to keep:

[
\max_S \sum_{i \in S} u_i - \lambda \sum_{i \in S} \ell_i - \mu \sum_{i \in S} r_i
\quad\text{s.t.}\quad \sum_{i \in S} \ell_i \le B
]

`compress` may implement a greedy approximation or DP under bounded sizes.

Critically: **the intensional engine can estimate (u_i)** (VOI), and the extensional evaluator enforces (B).

That yields a principled feedback loop:

* infer estimates VOI
* compress applies VOI policy
* future inference cost shrinks

---

# 16. Tooling is not UNIX, but can feel like UNIX (without inheriting its hazards)

Your “lisp bash” idea is dead-on: the model has priors about shell workflows. But we do not want stringly shell semantics.

Ω therefore provides a *structured command DSL* that compiles to tool effects:

```lisp
(sh
  (rg "pattern" "src/")
  (pipe (sed "s/foo/bar/g"))
  (tee "out.txt"))
```

Semantics:

* each stage is an effect with structured args
* outputs are typed artifacts (streams, blobs)
* cached by content addressing
* receipts store hashes and provenance
* write-through caching supported for codebase writes

This yields the “trained-on-unix” ergonomics without surrendering language discipline.

---

# 17. What remains for Part 3

At this point we have:

* the dual-plane kernel
* the oracle protocol (REPL re-entrancy)
* truth governance (obligations, certification, commit barrier)
* uncertainty algebra (Dist + confidence + risk + utility)
* learning semantics (prompt/ICL, policy RL, weights training as versioned definitional artifacts)
* context economics (receipts, VOI compression/hydration)
* tool DSL grounding

**Part 3** is where we re-derive the full SICP tower and give *every trick* an intensional counterpart in code, as libraries/DSLs/macros:

1. **Meta-circular evaluator** (E) and meta-intensional evaluator (I)
2. **Streams** + intensional strictness/progress analyzers
3. **amb / nondeterminism** + distributional search strategies
4. **Generic arithmetic** + inferred coercions / rewrite laws
5. **Constraint propagation** + inferred missing constraints / diagnosis
6. **Concurrency** + inferred race/deadlock analysis and synthesis of protocols
7. **Explicit-control evaluator / compiler pipeline** + rewrite/verify staging
8. Pattern matching DSL + inferred exhaustiveness and synthesis of patterns
9. DNN/ML abstractions as first-class DSLs (models, training, distillation) integrated with the learning plane

Reply **continue** and I’ll deliver **Part 3**: the “SICP reconstruction kit” for Ω, including the intensional duals, and show how the entire language ecosystem can be derived from the kernel and handlers without collapsing into an API wrapper.


---

<!-- SOURCE: 32-LANGUAGE-OFFICIAL-3.md -->

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


---

<!-- SOURCE: 32-LANGUAGE-OFFICIAL-4.md -->

## Ω Language Specification v1.0

### Part 4 — Final Pass: Profiles, Promotion Pipelines, Semantic Versioning, Complete Dual‑Plane Catalog, and Canonical “Omega Programs”

This part turns the spec into a **complete language** in the SICP sense: not just a kernel plus ideas, but a **discipline** and a **workbench** where inference is irreducible, governable, and compositional—never an “API call.”

I’m going to do five things, exhaustively:

1. Define **governance profiles** (strict/pragmatic/exploratory/etc.) as *semantic regimes*.
2. Define **semantic versioning + promotion pipelines** for prompts/policies/engines/models.
3. Provide the **complete dual-plane catalog**: for every kernel construct, the intensional counterpart (Meaning rules + obligations).
4. Define **semantic memory** (ICL/prompt/memory/policy/weights) as first-class, versioned artifacts with decay/invalidations.
5. Provide canonical **Omega Programs** demonstrating the whole thing end-to-end (synthesis, refactoring-to-patterns, recursive jobs with context economics, self-improving policies, DSL fabrication).

---

# 34. Governance Profiles as First‑Class Semantic Regimes

A “profile” is a **semantic envelope**: it configures evaluation, inference, verification, budgeting, capabilities, and learning emission. It is not a runtime flag; it is an object that *changes the rules of the game* by installing handlers and constraints.

## 34.1 `Profile` datatype

```
Profile = ⟨
  name            : Sym,
  allowedEffects  : EffRow,
  allowedCaps     : CapSet,
  budgets         : BudgetProfile,          ; tokens/tools/time/steps
  truthRegime     : TruthRegime,            ; speculative|test|proof|consensus
  inferenceRegime : InferenceRegime,        ; strategy pipeline
  rewriteRegime   : RewriteRegime,          ; allowed rewrite classes + obligations
  learningRegime  : LearningRegime,         ; emit-only|prompt-tune|policy-rl|weights
  memoryRegime    : MemoryRegime,           ; ttl/decay/invalidation policy
  auditRegime     : AuditRegime             ; ledger granularity + receipt requirements
⟩
```

### `TruthRegime`

* `speculative` (no substitutability)
* `test-certified` (within envelope)
* `proof-certified` (strong substitutability)
* `consensus-certified` (ensemble agreement + minimal tests)

### `AuditRegime`

* `minimal`: log only boundaries (infer/tool/commit)
* `full`: log request/response, test results, hashes, costs
* `forensic`: log evaluation steps (explicit-control trace) + provenance

This is exactly the “don’t let nondeterminism stop us; enforce invariants” requirement made concrete.

---

## 34.2 Canonical profiles (standard library)

### 34.2.1 `profile:explore`

* goal: maximum creativity and speed
* allows inference everywhere, tool use by default
* truth regime: speculative
* rewrite regime: allowed, but not auto-committed
* learning regime: emit examples enabled

Use-case: brainstorming, early exploration, rapid prototyping.

### 34.2.2 `profile:pragmatic`

* goal: real work, moderate safety
* truth regime: test-certified
* rewrites allowed only if regression/property tests pass
* “escalation ladder” enabled (speculate → test → proof optionally)
* tool calls cached, receipts required for external actions

Use-case: engineering tasks, code changes, CI-like discipline.

### 34.2.3 `profile:strict`

* goal: correctness and reproducibility
* truth regime: proof-certified where possible; otherwise heavy testing + consensus
* forbids “no-new-facts” violations
* forbids un-certified commit
* requires receipts for phase boundaries
* budgets enforced hard; deterministic envelope strategy installed

Use-case: production changes, security-sensitive transformations, scientific workflows.

### 34.2.4 `profile:airgap`

* goal: no external I/O
* forbids `tool`, forbids network, may forbid learning emission
* allows `int` but only over sealed contexts and local evidence
* very useful for reproducibility and privacy

Use-case: confidential codebases, offline reasoning.

---

## 34.3 `with-profile` is a semantic operator

```
(with-profile profile e)
```

Semantics:

* installs a handler stack:

  * `infer.op` handler configured by profile.inferenceRegime
  * `tool.op` handler configured by profile.budgets + cache
  * `commit.op` handler configured by profile.truthRegime
  * `train.*` handlers per profile.learningRegime
* sets capability scope (object-capability discipline)
* installs constraints into the current context (budget, audited, no-new-facts, etc.)

This is a **Template Method** (phased job skeleton) + **Strategy** (pluggable regimes) + **Decorator** (wrap evaluation with auditing).

---

# 35. Semantic Versioning for Prompts, Policies, Engines, and Models

If learning exists, semantics can drift. Ω must make drift explicit and manageable.

## 35.1 Versioned artifacts

Every semantic artifact is versioned:

* `Prompt@v`
* `Policy@v`
* `Engine@v` (weightsRef + promptRef + memoryRef + policyRef)
* `Model@v` (for ML models)
* `DSL@v` (macro + interpreter + obligations + tests)

Each version has:

* `hash` (content address)
* `provenance` (ledger references to how built)
* `baseline` (metrics, tests)
* `trust-level` (experimental/candidate/trusted/deprecated)

---

## 35.2 Semantic versioning rules (normative)

For artifact `A`:

* **PATCH**: changes that do not change accepted obligations or external behavior envelope

  * e.g., prompt wording change that preserves regression suite pass/fail and does not widen authority
  * policy tweak that reduces cost without changing outputs beyond tolerated ε

* **MINOR**: additive capability without breaking envelopes

  * new rewrite rule guarded by tests
  * new inference strategy stage that only activates under explicit opt-in

* **MAJOR**: any of:

  * changes the truth regime or obligations
  * changes effect footprint (new tool access, new authority)
  * changes equivalence notion
  * changes distributional semantics beyond ε
  * changes certification requirements

This is the semantic analog of API versioning; it prevents “prompt evolution” from silently breaking everything.

---

## 35.3 Promotion pipeline (CI/CD for semantics)

Promotion is the enterprise pattern you want: **Build Pipeline + Stage Gate + Promotion**.

### Stages (canonical)

1. **Build**: produce candidate prompt/policy/engine/model
2. **Evaluate**: run regression suite + property suite + metamorphic suite
3. **Red-team**: adversarial counterexample search
4. **Benchmark**: cost profile comparison (tokens/tools/time)
5. **Approve**: human or policy gate
6. **Promote**: mark artifact trusted; write receipt

This is “Continuous Integration patterns” but for semantic machines.

In Ω:

```lisp
(promote engine:solver/v7
  :requires { suite:regression pass
              suite:properties pass
              redteam:fuzz no-fails
              cost <= baseline*1.05
              authority unchanged })
```

Promotion produces a **Receipt** that is itself a first-class certificate.

---

# 36. Semantic Memory: ICL, Prompt Evolution, and Weight Training Unified

You proposed “two kinds of training”: weights and in-context. Ω formalizes **four levers**:

1. **Weights** `W` (engine implementation)
2. **Prompt program** `P` (structured conditioning)
3. **Semantic memory** `M` (retrieval store of receipts, laws, proofs, experiences)
4. **Policy** `Π` (search + verification + budgeting + tool-use decisions)

An Engine is:

[
Engine = \langle W, P, M, \Pi, version \rangle
]

And Ω exposes explicit, versioned operations for each.

---

## 36.1 Prompt is AST, not string

Prompt is a typed AST:

```lisp
(prompt
  (system "You are Ω’s intensional evaluator.")
  (rules  [no-new-facts cite-evidence])
  (schema Meaning)
  (tools  [eval apply observe test])
  (examples ...))
```

That makes “prompt refactoring” a normal `rewrite` problem.

---

## 36.2 Semantic memory store `M` (with decay and invalidation)

Memory is **not** “chat history.” It is a content-addressed store of:

* receipts (tool outputs, proofs, verified rewrites)
* learned rewrite laws with obligations
* failure cases (counterexamples)
* successful episodes for RL
* domain ontologies (optional)

### Memory regime (policy)

* TTL decay
* staleness weighting
* invalidation on dependency changes
* scoping: local/module/org-wide

Operations are persistent updates:

```lisp
(remember engine:solver mem-entry) -> engine'
(forget engine:solver key)         -> engine'
(prune engine:solver :policy ...)  -> engine'
```

This prevents “in-context learning” from turning into untracked mutation.

---

## 36.3 Learning operators

### Prompt tuning

```lisp
(train/prompt prompt:solver/v1 dataset
  :objective { minimize hallucination maximize test-pass-rate minimize cost }
  :policy policy:prompt-tune)
=> prompt:solver/v2 + receipt
```

### Policy learning (RL / bandits)

```lisp
(train/policy policy:solver/v1 episodes
  :objective { maximize success - λ*tokenCost - μ*toolCost }
  :algorithm "RL|bandit|offline-RL")
=> policy:solver/v2 + receipt
```

### Weight training (privileged)

```lisp
(train/weights engine:solver/W1 dataset
  :algorithm "SFT|DPO|RLHF|..."
  :requires { privacy ok, eval-suite pass })
=> engine:solver/W2 + receipt
```

All of these are **explicit and versioned**; nothing silently mutates.

---

# 37. The Complete Dual‑Plane Catalog for Kernel Forms

This is the “no stone unturned” part: for each kernel construct, define the intensional counterpart as a **Meaning transformer** plus obligations.

Important: Ω does **not** necessarily introduce new syntax like `if-int`. Instead, `int` produces the Meaning, and the Meaning rules are defined *per construct*. That’s how it remains a language, not a pile of APIs.

I’ll specify each as:

* **E-plane rule**: extensional behavior
* **I-plane rule**: intensional Meaning fields (denotation/residual/rewrite/invariants/effects/cost/paths/deps/memo/obligation/confidence)

---

## 37.1 Literals

* E: evaluate to itself.
* I: invariants derive trivially; cost O(1); confidence high.

Meaning highlights:

* `denotation = literal`
* `effects = ∅`
* `rewrite = ⊥`
* `confidence ≈ 1`

---

## 37.2 Variables

* E: lookup in `Ctx`.
* I:

  * infer type/refinement from binding evidence and usage
  * infer aliasing (if state/concurrency present)
  * infer whether stable under sealing

Obligations:

* if inferred type is promoted: require either proof (typing) or runtime contract checks.

---

## 37.3 `quote`

* E: returns AST.
* I:

  * produces structural Meaning: AST shape, free vars, binder structure, α-normal form
  * can compute “semantic digest” keys for caching and equivalence classes

---

## 37.4 `lambda`

* E: closure formation.
* I:

  * infer contract shape (pre/post), effect footprint, possible purity
  * suggest refactor to eliminate captured mutable state
  * compute “closure specialization potential” (partial eval opportunities)

Obligations:

* contracts must be validated by tests/proofs before `commit/contract`.

---

## 37.5 `apply`

* E: evaluate operator and args, extend env, evaluate body.
* I:

  * if callee and args are known, attempt partial evaluation/inlining
  * produce residual program if not fully reducible
  * simulate call stack sketch; compute cost estimate; detect recursion patterns
  * propose memo table if function is pure and arguments hashable
  * propose algorithm replacement (with strict obligations)

Obligations:

* any rewrite must discharge `eq-ext` within an envelope or proof.
* memo table commit requires sampled equivalence check.

---

## 37.6 `if`

* E: evaluate predicate, then branch.
* I:

  * produce path conditions; branch probabilities (if heuristic)
  * detect dead branch / redundant predicate
  * predicate abstraction: suggest moving checks outward or simplifying guard
  * can split Meaning into mixture distribution (if nondet)

Obligations:

* dead-code elimination requires proof or tests.

---

## 37.7 `begin`

* E: sequence.
* I:

  * accumulate effect summary with ordering constraints
  * detect commutativity; suggest reordering for performance if safe
  * propose phase boundaries for snapshot/compress (rocket staging)

Obligations:

* reordering effects requires commutativity proof or transactional boundary.

---

## 37.8 `define`

* E: bind name → value/closure in context.
* I:

  * infer specification; derive tests; infer invariants; generate doc contracts
  * propose module boundary; detect cohesion/coupling metrics
  * can propose renaming or API shaping (Refactoring patterns: Extract Function, Inline Function, Introduce Parameter Object)

Obligations:

* API changes require migration plan + regression suite.

---

## 37.9 `set!`

* E: mutation (state effect).
* I:

  * compute effect footprint (writes set)
  * detect referential transparency breakpoints
  * suggest refactoring to pure style: accumulator, state monad, object encapsulation
  * suggest transactional boundaries (STM, Unit of Work) and compensating actions (Saga)

Obligations:

* refactoring away mutation requires behavior equivalence validation.

---

## 37.10 `ctx/extend/seal`

* E: context creation/extension/sealing.
* I:

  * infer which bindings are VOI-critical
  * suggest compression points; compute env digest for caching
  * sealing suggestions: “freeze here to restore substitution”
  * detect authority leaks: caps that should not cross boundary

Obligations:

* sealing may require ensuring no future code expects mutation of that subtree.

---

## 37.11 `eval` (reflective extensional eval)

* E: evaluate quoted expression in a chosen env.
* I:

  * reason about safety of reflective eval
  * propose staging: partial evaluate before reflective eval
  * attach obligations about injection safety (capability constraints)

Obligations:

* reflective evaluation in strict profiles requires capability gate + audit receipt.

---

## 37.12 `int` / `infer` / `rewrite`

* E: these are effects; extensional meaning is “produce Meaning / Dist”.
* I: this is the core; already defined by oracle protocol and strategy DSL.

Key intensional obligations:

* `no-new-facts` enforcement
* evidence citations
* deterministic envelope when required
* substitution only after certification

---

## 37.13 `handle/effect`

* E: algebraic effects.
* I:

  * infer effect footprints
  * propose handler placement (where to intercept)
  * verify that “inference is avoided” in forbidden regions by checking effect rows

Obligations:

* handler modifications require regression tests (behavioral equivalence of effect interpretations).

---

## 37.14 `match`

* E: pattern match evaluation.
* I:

  * exhaustiveness and redundancy analysis
  * decision tree compilation suggestions
  * pattern synthesis from examples (with obligations)
  * witness generation for uncovered cases

Obligations:

* any auto-generated patterns require test coverage and counterexample search.

---

# 38. Inference Avoidance as a First‑Class Construct

This is essential: “a language with inference must have ways not to use inference.”

Ω supports this in three mutually reinforcing ways:

## 38.1 Effect typing / row exclusion

A function can be annotated `! [pure]` meaning it cannot call `infer` or `tool`. That’s the static wall.

## 38.2 Dynamic semantic walls

Forms:

```lisp
(ext-only e)     ; disallow infer/rewrite/train effects inside
(int-only e)     ; disallow eval/tool/state inside (analysis sandbox)
(no-rewrite e)   ; allow int but forbid rewrite suggestions/commit
(no-tools e)     ; airgap region
```

These are implemented as handlers that raise errors if forbidden ops occur. It’s the semantic analog of “pure subset” in SICP.

## 38.3 Capability walls

Even if inference is allowed, the engine may not have `cap.eval`, `cap.tool`, etc., so it cannot REPL or act.

This prevents accidental escalation.

---

# 39. Adversarial Semantics and Failure-Mode Discipline

Inference can hallucinate and overfit. Ω makes defense a language feature.

## 39.1 “Critic” pipelines (Decorator + Chain of Responsibility)

A `critic` is a Meaning transformer that attempts to falsify claims:

* contradiction detection (self-inconsistency)
* counterexample search (property-based)
* differential testing (cross-engine disagreement)
* mutation testing (does test suite actually detect changes?)
* fuzzing for edge cases

A strict profile installs a critic stack automatically:

```lisp
(strategy
  (speculate ...)
  (critic :self-contradiction)
  (critic :counterexample :budget 200)
  (critic :cross-engine [llm smt])
  (verify :tests suite:regression)
  (commit-if ...))
```

This is “Defense in Depth” applied to semantics.

## 39.2 Disagreement is first-class

If engines disagree, Ω records `Event(disagreement, ...)` in Σ and may:

* escalate verification,
* block commit,
* or mark Meaning as low-confidence.

That makes “hallucination” an observable runtime phenomenon.

---

# 40. Canonical Omega Programs

Now I’ll give you canonical programs that demonstrate the entire language as a coherent system. These are not toy examples; they correspond to the use cases you’ve been circling: code synthesis, refactoring, recursive jobs, context economics, training, and DSL fabrication.

---

## 40.1 Program A — Synthesize a function from a contract, certify, commit

**Intent**: show inference that “understands code,” generates a program, verifies extensional behavior, then commits.

```lisp
(with-profile profile:pragmatic
  (begin
    (define contract:sum
      (contract
        :requires (lambda (xs) (and (list? xs) (all number? xs)))
        :ensures  (lambda (xs r) (= r (fold + 0 xs)))))

    (define goal
      { kind "synthesize"
        name 'sum
        contract contract:sum
        language "Ω"
        prefer "pure"
        forbid-effects [tool state] })

    (define distM
      (infer goal :engine engine:solver :policy policy:synthesize))

    (define m (sample (map-dist Meaning.rewrite distM) :seed 7))
    (define cand (Meaning.rewrite m))

    ;; verification obligations
    (define tests (infer {goal "generate property tests" program cand contract contract:sum}
                         :engine engine:solver :policy policy:testgen))

    (assert (run-tests tests) "synthesized program failed tests")

    (commit/rewrite cand :requires { tests pass })

    (define sum (eval 'sum))   ; now available
    sum))
```

Patterns:

* **Specification** pattern (contract)
* **Strategy** (synthesize/testgen)
* **Command** (`commit/rewrite`)
* **Memento/Receipt** (profile will snapshot and compress if configured)

---

## 40.2 Program B — Refactor mutation to pure FP (Refactoring to Patterns)

**Intent**: take SICP’s mutation hazards and force them into a disciplined refactor.

```lisp
(with-profile profile:strict
  (begin
    (define qbad
      '(define (count-occurrences xs)
         (let ((c 0))
           (for-each (lambda (x) (when (= x 0) (set! c (+ c 1)))) xs)
           c)))

    (define m
      (rewrite qbad
        :goal { preserve extensional
                eliminate-effects [state]
                prefer "fold"
                introduce "accumulator" }
        :engine engine:solver
        :policy policy:rewrite+verify))

    (define qgood (Meaning.rewrite m))

    (define suite (infer {goal "derive regression suite" original qbad candidate qgood}
                         :engine engine:solver :policy policy:testgen))

    (assert (run-tests suite) "rewrite not equivalent")

    (commit/rewrite qgood :requires { suite pass }))
```

Patterns:

* Fowler refactorings: **Replace Loop with Pipeline**, **Introduce Explaining Variable**, **Replace Mutable Accumulator with Fold**
* GoF: **Strategy**, **Decorator** (critics), **Command**

---

## 40.3 Program C — Recursive jobs with rocket-staging and VOI compression

**Intent**: your “jobs recursive, drop rocket stages, receipts, caching” requirement as a first-class idiom.

We define a reusable combinator `job` (Template Method + Pipeline):

```lisp
(define (job spec)
  (begin
    (phase 'plan   (plan spec))
    (snapshot (current-context) {phase 'plan})
    (compress (current-context) :policy {objective "voi" budgetTokens 2500})

    (phase 'fetch  (fetch spec))
    (snapshot (current-context) {phase 'fetch})
    (compress (current-context) :policy {objective "voi" budgetTokens 2500})

    (phase 'act    (act spec))
    (snapshot (current-context) {phase 'act})

    (phase 'verify (verify spec))
    (snapshot (current-context) {phase 'verify})

    (phase 'pack   (pack spec))))
```

Now a recursive job:

```lisp
(with-profile profile:pragmatic
  (job
    { task "refactor module"
      subtasks [
        { task "locate hotspots" }
        { task "rewrite critical path" }
        { task "run tests" }
      ]}))
```

The inference plane can help compute VOI:

* which receipts to hydrate for each subtask,
* which tool outputs to reuse,
* what to discard safely.

Patterns:

* **Template Method** (job phases)
* **Pipes-and-Filters** (phases)
* **Event Sourcing** (ledger)
* **CQRS** (receipts as read models)

---

## 40.4 Program D — Learn a better “speculate vs eval” policy via RL

**Intent**: bake in reinforcement learning in a language-governed way, training the *policy* rather than silently drifting the model.

```lisp
(with-profile profile:pragmatic
  (begin
    (define (episode-run qexpr)
      (episode
        (define m (int qexpr :engine engine:solver :policy policy:baseline))

        ;; policy action: speculate or call eval?
        (define action (policy:choose m))  ; action in {speculate, eval}

        (define v
          (if (eq? action 'eval)
              (eval qexpr)
              (choose-from (Meaning.denotation m) :seed 1)))

        (define ok? (validate-result qexpr v)) ; oracle / tests / rubric
        (reward (if ok? 1.0 -1.0)
                { cost (usage) action action }))
      ))

    (define episodes (map episode-run dataset:queries))

    (define policy:baseline/v2
      (train/policy policy:baseline/v1 episodes
        :objective { maximize reward - 0.001*tokens - 0.1*toolCalls }
        :algorithm "offline-RL"))

    (promote policy:baseline/v2 :requires { regressions pass })))
```

This is “training as define,” but properly:

* returns a new versioned Policy artifact
* promotion is test-gated
* semantics never mutate silently

Patterns:

* **Strategy** (policy)
* **Memento/Receipt** (episodes as artifacts)
* **Build Pipeline** (promotion)
* **Reinforcement learning** as a first-class meta-process

---

## 40.5 Program E — Fabricate a DSL (Sussman “make up languages”) with intensional duals

**Intent**: show Ω as a metalanguage. We create a DSL for an Enterprise Integration Pattern: message routing.

### 40.5.1 DSL syntax (macro layer)

```lisp
(define-syntax route
  (syntax-rules ()
    ((_ (from ch) (when pred) (to out))
      (route.core {from ch pred pred to out}))))
```

### 40.5.2 DSL semantics (interpreter layer)

`route.core` compiles to a pipes-and-filters graph of tool/effect operations.

### 40.5.3 Intensional companion

Now the inference plane can:

* infer missing predicates,
* propose normalization,
* detect routing loops,
* generate test messages,
* estimate throughput and backpressure issues,
* propose circuit breakers and retries.

Example:

```lisp
(int '(route (from in) (when pred) (to out))
     :policy (strategy (speculate) (critic :loop-detect) (rewrite-if :improves {...})))
```

This proves the point: inference is not a library call; it’s a *semantic plane* available to every made-up language.

Patterns:

* Enterprise Integration Patterns: **Message Router**, **Content-Based Router**, **Filter**, **Aggregator**, **Splitter**
* Resilience: **Circuit Breaker**, **Retry**, **Bulkhead**
* GoF: **Interpreter** (DSL interpreter), **Builder** (construct pipeline graph)

---

# 41. “Final Pass” Checklist: Requirements You Wanted, Now Explicitly in the Language

Let’s explicitly map your core concerns to language constructs:

### “Not just an API call”

* `int/infer/rewrite` are effect ops handled by evaluator-level handlers
* outputs are structured `Meaning`, not strings
* authority is capability-scoped
* actions/rewrites require `commit` with obligations

### “LLM understands code; can run it in head or REPL”

* oracle protocol includes `Req.Eval`/`Req.Apply`/`Req.Test` and pure simulation
* strategy decides speculate vs eval using cost/risk/VOI

### “Every SICP trick gets an inference-plane counterpart”

* implemented as Meaning rules + derived modules (`stream`, `nondet`, `generic`, `constraints`, `compiler`, etc.)
* plus critics, obligations, certification

### “Training should be first-class”

* explicit `train/prompt`, `train/policy`, `train/weights`
* episodes and examples are emitted as effects
* everything versioned and promotion-gated

### “Context economics / drop rocket stages”

* `snapshot`, `receipt`, `compress`, `hydrate`, VOI selection built in
* job-phase Template Method is canonical library pattern

### “Inference avoidance boundaries”

* `ext-only`, effect rows, caps

### “Adversarial/failure semantics”

* critic pipelines, fuzz/counterexample, disagreement events, mutation testing

---

# 42. Where we go next (if you want)

At this point, the **language definition is complete** at the semantic/spec level:

* kernel
* dual-plane evaluation
* oracle protocol
* truth/uncertainty governance
* learning semantics
* context economics
* full derived SICP tower
* DSL fabrication
* canonical programs

If you say **continue**, the next (optional) part is **Part 5: Implementation Blueprint**:

* a reference interpreter architecture (CEK/CEKS + effect handlers)
* the oracle coroutine protocol as concrete data structures
* receipt store (content-addressed)
* caching/tool framework
* test harness + promotion pipeline automation
* minimal stdlib that boots the SICP kit modules

Just reply **continue**.


---

<!-- SOURCE: 32-LANGUAGE-OFFICIAL-5.md -->

## Ω Language Specification v1.0

### Part 5 — Implementation Blueprint: Reference Interpreter, Effect Runtime, Oracle Protocol, Receipts, Caching, and Training Pipelines

You now have enough requirements that we can describe an implementation that is **architecturally inevitable** if you want:

* inference as a **semantic plane**, not an API call
* re‑entrant REPL against the extensional evaluator
* governance (truth/uncertainty, capabilities, budgets)
* learning (prompt/policy/weights) as **versioned definitional artifacts**
* receipts / compression / hydration (context economics)
* “make up languages” (macros + interpreter kits) *at scale*

What follows is a **full engineering blueprint**: components, interfaces, runtime model, data structures, algorithms, and the design patterns they embody.

I’ll write this language‑neutral, then show concrete type sketches in a **TypeScript‑ish** style (because it’s readable) and note where Rust/OCaml/Haskell would differ.

---

# 0. System Architecture (Layered + Hexagonal)

Treat Ω as a **language runtime product**, not a script.

### Ports & Adapters (Hexagonal Architecture)

Core is deterministic and testable; adapters handle LLM APIs, tools, storage.

**Core (pure-ish)**

* Reader / AST
* Macro expander (phase separation)
* Extensional evaluator (CEK/CEKS machine)
* Effect runtime (algebraic effects + handlers)
* Intensional session driver (oracle protocol)
* Trust / obligations / commit semantics
* Receipts / compression/hydration logic
* Artifact registry logic (versions, promotion)

**Adapters (impure)**

* LLM adapter(s): OpenAI/other
* Tool adapters: bash, git, web, filesystem, compilers
* Stores: ledger Σ store, receipt store, artifact registry store, dataset store
* Tokenizer/cost estimator adapter

This is classic **Onion Architecture**: dependencies point inward.

---

# 1. The Core Data Model (Concrete Types)

You must implement the ontology exactly, or inference will leak into “just text.”

## 1.1 Expr / Datum representation (homoiconicity)

Two viable representations:

### Option A — AST Nodes (explicit)

Good for optimization and tooling.

```ts
type Expr =
  | { tag: "Lit"; value: Atom; loc?: Loc }
  | { tag: "Sym"; name: string; loc?: Loc }
  | { tag: "Pair"; car: Expr; cdr: Expr; loc?: Loc }     // Lisp lists
  | { tag: "Quote"; datum: Expr; loc?: Loc }
  | { tag: "Lambda"; params: string[]; body: Expr; ann?: Ann; loc?: Loc }
  | { tag: "If"; test: Expr; conseq: Expr; alt: Expr; loc?: Loc }
  | { tag: "Begin"; exprs: Expr[]; loc?: Loc }
  | { tag: "Define"; name: string; rhs: Expr; loc?: Loc }
  | { tag: "Set"; name: string; rhs: Expr; loc?: Loc }
  | { tag: "Apply"; fn: Expr; args: Expr[]; loc?: Loc }
  | { tag: "Effect"; op: string; args: Expr[]; loc?: Loc }
  | { tag: "Handle"; body: Expr; handler: HandlerExpr; loc?: Loc }
  | { tag: "Ctx"; fields: CtxField[]; loc?: Loc }
  | { tag: "Extend"; base: Expr; binds: [string, Expr][]; loc?: Loc }
  | { tag: "Seal"; base: Expr; loc?: Loc }
  | { tag: "Eval"; qexpr: Expr; env?: Expr; loc?: Loc }
  | { tag: "Int"; qexpr: Expr; engine?: Expr; policy?: Expr; env?: Expr; loc?: Loc }
  | { tag: "Infer"; goal: Expr; engine?: Expr; policy?: Expr; env?: Expr; loc?: Loc }
  | { tag: "Rewrite"; qexpr: Expr; goal: Expr; engine?: Expr; policy?: Expr; env?: Expr; loc?: Loc }
  | { tag: "Match"; scrut: Expr; clauses: MatchClause[]; loc?: Loc };
```

### Option B — “Everything is pairs”

Good for true Lisp minimalism; special forms recognized at eval time.
Still add `loc` metadata via wrapper cells.

In practice: start with Option A; it pays off for tracing, macro hygiene, and inference protocol schemas.

## 1.2 Values

```ts
type Val =
  | Atom
  | PairVal
  | VectorVal
  | MapVal
  | CtxVal
  | ClosureVal
  | MeaningVal
  | DistVal
  | EngineVal
  | PromptVal
  | PolicyVal
  | ReceiptVal
  | EvidenceVal
  | CapVal
  | ContinuationVal
  | EffectVal; // usually internal
```

Closures capture:

* params
* body
* env (Ctx)
* effect row
* caps
* contract

## 1.3 Context (persistent environment)

Use persistent maps (HAMT) for production; a parent-linked chain of JS Maps is fine for a reference interpreter.

```ts
type Ctx = {
  cid: Hash;                    // content address (hash of parent + frame + constraints)
  id: string;
  parent?: Ctx;
  frame: PersistentMap<string, Val>;
  constraints: InvSet;
  caps: CapSet;
  sealed: boolean;
  evidence: Evidence[];
  receipt?: Receipt;
};
```

### Implementation note

* If `extend` returns a new `Ctx`, you get functional persistence.
* `seal` sets `sealed=true` (and optionally drops write caps).

## 1.4 Ledger Σ (event store)

An append-only log:

```ts
type Event =
  | { tag: "EvalEnter"; exprHash: Hash; ctx: Hash; time: number }
  | { tag: "EvalExit"; exprHash: Hash; ctx: Hash; valueHash: Hash; time: number }
  | { tag: "EffectEmit"; op: string; argsHash: Hash; ctx: Hash; time: number }
  | { tag: "EffectHandle"; op: string; handlerId: string; time: number }
  | { tag: "OracleStart"; req: InferRequest; sessionId: string; time: number }
  | { tag: "OracleReq"; sessionId: string; req: OracleReq; time: number }
  | { tag: "OracleResp"; sessionId: string; resp: OracleResp; time: number }
  | { tag: "ToolCall"; call: ToolCall; callId: string; time: number }
  | { tag: "ToolResult"; callId: string; resultHash: Hash; time: number }
  | { tag: "Snapshot"; ctx: Hash; receiptId: Hash; time: number }
  | { tag: "Commit"; kind: string; payloadHash: Hash; time: number }
  | { tag: "Promote"; artifact: ArtifactRef; receiptId: Hash; time: number }
  | { tag: "TrainEmit"; exampleHash: Hash; time: number };
```

**Event sourcing** is not optional: it is the substrate for receipts, replay, training datasets, and semantic version provenance.

---

# 2. The Extensional Evaluator (CEK/CEKS + Algebraic Effects)

If you want:

* deterministic semantics
* explicit control
* effect handlers
* re-entrancy for oracle requests
* debuggable traces
  you should implement a **small-step abstract machine** (CEK family), not naive recursion.

## 2.1 Machine state

```ts
type State = {
  expr: Expr;
  ctx: Ctx;
  kont: Kont;
  store: Store;      // optional; only if you implement explicit store (CESK)
  handlers: HandlerStack;
  ledger: Ledger;
};
```

Where `Kont` is a sum type of continuation frames:

* `IfK(conseq, alt, ctx, next)`
* `BeginK(rest, ctx, next)`
* `ApplyFnK(args, ctx, next)` / `ApplyArgsK(fnVal, doneArgs, remainingArgs, ctx, next)`
* `DefineK(name, ctx, next)`
* `SetK(name, ctx, next)`
* `HandleK(handler, ctx, next)` (delimited resumption point)
* etc.

This is the **Interpreter pattern** realized as a machine.

## 2.2 Algebraic effects implementation

When evaluation reaches `(effect op args...)`, it does not “do” the op; it **yields**:

```ts
type OpCall = { op: string; args: Val[]; k: Resume; ctx: Ctx };
type Resume = (v: Val) => State; // or a continuation object
```

The runtime finds the nearest handler that can handle `op` and invokes it:

```ts
type Handler = {
  id: string;
  canHandle(op: string): boolean;
  handle(opCall: OpCall, runtime: Runtime): Val | OpCall; // may re-emit ops
  onReturn?(v: Val): Val;
  onFinally?(): void;
};
```

That gives you **semantic control**:

* `infer.op` is handled by the oracle handler
* `tool.op` by tool handler
* `commit.op` by commit handler
* `train.emit` by dataset handler
* `amb.op` by nondet handler
* etc.

This is **Chain of Responsibility** (handler stack), and **Strategy** (different handler policies).

---

# 3. Oracle Protocol Runtime (Interactive Intensional Sessions)

This is the crux: inference is not “call model once.” It is an interactive coroutine where the engine can:

* speculate purely (simulate execution)
* re-enter evaluator (Req.Eval / Req.Apply)
* request projections (Req.Observe)
* call tools (Req.Tool)
* request tests (Req.Test)
* emit training examples (Req.EmitExample)
* return Meaning or a distribution

## 3.1 Engine adapter interface

The adapter must support an interactive session:

```ts
type InferRequest = {
  kind: "int" | "search" | "rewrite";
  expr?: Expr;          // quoted program
  goal?: Val;           // structured goal object
  envDigest: Hash;      // context digest (not raw context)
  prompt: Prompt;       // prompt AST
  policy: Policy;
  caps: CapSet;
  budgets: BudgetProfile;
  evidenceRefs: Hash[]; // receipts/evidence references
  schema: "Meaning" | "Dist<Meaning>" | "Dist<Val>";
};

type OracleSession = AsyncGenerator<OracleReq, OracleResp, OracleReturn>;

type OracleReq =
  | { tag: "ReqEval"; qexpr: Expr; envRef: Hash }
  | { tag: "ReqApply"; fn: Val; args: Val[]; envRef: Hash }
  | { tag: "ReqObserve"; ctxRef: Hash; schema: ProjectionSchema }
  | { tag: "ReqTool"; call: ToolCall }
  | { tag: "ReqTest"; spec: TestSpec }
  | { tag: "ReqSnapshot"; ctxRef: Hash; meta: any }
  | { tag: "ReqCompress"; ctxRef: Hash; policy: CompressPolicy }
  | { tag: "ReqHydrate"; receiptId: Hash }
  | { tag: "ReqEmitExample"; ex: Example }
  | { tag: "ReqReturn"; result: Meaning | DistVal | Val }
  | { tag: "ReqFail"; reason: string };

type OracleResp =
  | { tag: "RespVal"; v: Val }
  | { tag: "RespMeaning"; m: Meaning }
  | { tag: "RespEvidence"; ev: Evidence }
  | { tag: "RespError"; err: RuntimeError };

type OracleReturn = { tag: "Return"; value: Meaning | DistVal | Val };
```

The interactive protocol is **Mediator**: the oracle doesn’t talk to tools or the evaluator directly; the runtime mediates.

## 3.2 Oracle handler semantics

The `infer.op` handler:

1. Constructs `InferRequest` (policy + budgets + env digest + prompt AST + evidence refs)
2. Starts the engine session
3. While session yields `OracleReq`, do:

   * capability check
   * budget check
   * perform the requested operation (often by calling the extensional evaluator in a controlled sub-run)
   * ledger events
   * send back `OracleResp`
4. On `Return`, produce `Meaning`/`Dist` as the handled effect result
5. Attach obligations/evidence/confidence into the Meaning

This is not an API call. It is a controlled evaluator sub-language: **“make up inference languages” via handlers**.

---

# 4. Prompt/Policy/Engine as First-Class Artifacts (Not Strings)

If you want prompt evolution to be “like define,” you must store prompts as typed ASTs and version them.

## 4.1 Prompt AST representation

```ts
type Prompt =
  | { tag: "System"; text: string }
  | { tag: "Rules"; rules: Rule[] }
  | { tag: "Schema"; schema: Schema }
  | { tag: "Tools"; tools: ToolSig[] }
  | { tag: "Examples"; examples: ExampleIO[] }
  | { tag: "Compose"; parts: Prompt[] };
```

Then `define-prompt` just binds a `PromptVal`.

## 4.2 Policy as strategy pipeline (composable)

Policies compile to:

* handler configuration
* engine request parameters
* verification obligations
* critic pipeline settings

```ts
type Policy = {
  id: string;
  inferenceStrategy: StrategyPipeline;
  truthRegime: TruthRegime;
  confidenceAlgebra: "min" | "logit-add" | "custom";
  budgets: BudgetProfile;
  critics: CriticSpec[];
  escalation: EscalationRule[];
};
```

## 4.3 Engine version is a content-addressed object

An engine value is immutable; training returns a new engine version.

```ts
type Engine = {
  weightsRef: ArtifactRef;  // “W”
  promptRef: ArtifactRef;   // “P”
  memoryRef: ArtifactRef;   // “M”
  policyRef: ArtifactRef;   // “Π”
  version: SemVer;
  digest: Hash;
  trustLevel: "experimental" | "candidate" | "trusted" | "deprecated";
};
```

This is the linchpin for reproducibility and “no silent drift.”

---

# 5. Commit Semantics (Truth Promotion Barriers)

Never auto-apply rewrites. Always go through a commit barrier with obligations.

## 5.1 Commit operation family

* `commit/rewrite`
* `commit/memo`
* `commit/invariants`
* `commit/prompt`
* `commit/policy`
* `commit/engine`

Each is an effect `commit.op` with payload. The commit handler enforces:

* obligations satisfied (tests/proofs/consensus)
* capability present
* artifact registry update is versioned and ledgered

This is **Unit of Work** for semantic changes: you can batch and roll back at the commit boundary.

---

# 6. Receipts, Compression, Hydration (Context Economics Implementation)

Receipts are content-addressed summaries + replay plans + certificates.

## 6.1 Receipt store

A receipt is stored as:

* structured metadata JSON
* plus references (hashes) to event ranges in Σ and evidence blobs

```ts
type Receipt = {
  rid: Hash;
  summary: string;
  schema: Shape;
  deps: Hash[];
  replay: ReplayPlan;
  checks: CheckResult[];
  cost: CostProfile;
  voi: VOIProfile;
};
```

## 6.2 Compression algorithm (VOI knapsack)

Implementation sketch:

* Segment context into “chunks” (bindings, evidence, tool results, reasoning trace segments)
* Each chunk has:

  * length estimate ℓ
  * retrieval cost r
  * VOI u (from a heuristic + optionally from `int` itself)
* Choose subset under budget via:

  * greedy by u/(ℓ + αr) or
  * DP for small sizes
* Replace discarded chunks with:

  * receipt references
  * digest placeholders

This is precisely **CQRS**: receipts are read models; ledger is write model.

---

# 7. Tool Subsystem (Structured “Lisp Bash” + Caching + Write-through)

This is a classical EIP / CI pipeline situation.

## 7.1 Tool calls as effects, not strings

A tool call is a structured object:

```ts
type ToolCall = {
  name: "bash" | "git" | "web" | "python" | string;
  argv: string[];
  cwd?: string;
  env?: Record<string,string>;
  stdin?: string;
  timeoutMs?: number;
  cacheKey?: Hash; // computed from args + env digest + tool version
};
```

The tool handler:

* checks caps
* checks budgets
* computes cache key
* returns cached result if present
* otherwise executes, stores evidence blob, returns hash

## 7.2 Write-through caching for codebase changes (Unit of Work + Repository)

If the tool performs writes:

* represent writes as a **Command log** (diffs, patches, file edits)
* apply changes atomically at commit boundary
* store patch receipts

This enables:

* “agent writes while thinking” safely
* replayable modifications
* rollback (Saga compensations) if verification fails

---

# 8. Training Pipelines Inside Ω (Prompt / Policy / Weights)

You explicitly want:

* prompt evolution (ICL/system prompt changes)
* weight training optional
* RL for policies

All must be explicit effects and versioned artifacts.

## 8.1 Emitting training data

`emit-example` is an effect that appends structured example records to a dataset store:

```ts
type Example = {
  engineDigest: Hash;
  promptDigest: Hash;
  policyDigest: Hash;
  envDigest: Hash;
  qexprHash: Hash;
  goal: any;
  predictedMeaning: Meaning;
  extensionalChecks: CheckResult[];
  reward?: number;
  cost: CostProfile;
  timestamp: number;
};
```

This is the “write side.” Later, you compile datasets from these.

## 8.2 Training as versioned transformations

* `train/prompt` returns a new prompt artifact + receipt
* `train/policy` returns a new policy artifact + receipt
* `train/weights` returns a new engine/weights artifact + receipt

All go through the promotion pipeline.

## 8.3 RL: what you actually train

High-value targets in Ω:

* speculate vs eval decision
* context selection (VOI)
* tool-use decision
* rewrite acceptance thresholds
* search strategy parameters (beam width, stop rules)

These are policies (Strategy objects), and training produces new policy versions.

This is **Policy Object** + **Strategy** + **Feedback Control**.

---

# 9. Macro System and “Make Up Languages” (SICP’s Language-Making, Industrialized)

You can’t call it “SICP to the limit” unless macros and interpreter kits are first-class.

## 9.1 Macro expander phases

Implement at least:

* `define-syntax` and a hygienic macro system (syntax-rules tier)
* phase separation: macro env vs runtime env

Pipeline:

1. **read**: text → Expr with locations
2. **expand**: Expr → core-expanded Expr (macroexpanded + desugared)
3. **eval**: core Expr → Val

This is the standard Scheme pipeline.

## 9.2 Semantic macros (refactoring to patterns)

Add a second kind of macro:

* syntactic macro: Expr → Expr (no semantic obligation)
* semantic macro: Expr → (Expr, Obligation)

Semantic macros are built on `rewrite` and must pass through commit barriers.

This is what upgrades “macros” into “automated refactoring with correctness.”

---

# 10. Testing, Certification, and CI-Style Promotion

You’re effectively building a language + a continuous semantic integration system.

## 10.1 Test suites as first-class

* regression suite (golden master)
* property suite (QuickCheck)
* metamorphic suite
* adversarial fuzz suite
* mutation testing (test quality)

## 10.2 Promotion as pipeline (Build Pipeline pattern)

A `promote` operation triggers:

* run suites
* run red-team fuzz
* compare cost profiles
* check authority deltas
* write promotion receipt
* mark artifact trusted

This is exactly CI/CD, but for prompts/policies/engines/rewrites.

---

# 11. Bootstrapping Strategy (How Ω Becomes Self-Hosting)

This is how you avoid building an “LLM framework” and instead build a language.

### Stage 0 — Host kernel interpreter

Implement kernel in host language with:

* reader
* evaluator
* effect runtime
* oracle handler stub

### Stage 1 — Implement Ω standard library in Ω

Write `omega.stream`, `omega.nondet`, etc. in Ω itself.

### Stage 2 — Meta-circular evaluator in Ω

Implement `eval0` in Ω (as in SICP), and test it against host evaluator.

### Stage 3 — Replace host parts with Ω parts

Gradually shift:

* macro expander
* optimizer
* compiler passes
  into Ω itself

That’s the “tower of interpreters” realized.

---

# 12. Why This Implementation Does Not Collapse into “API Call”

Because:

* inference is an **effect op** with handler-defined semantics
* the engine runs an **interactive protocol** (requests/responses), not a single call
* outputs are **Meaning** + obligations + evidence, not text
* re-entry to `eval` is capability-guarded and ledgered
* training and prompt evolution are versioned transformations, not silent state
* commits are obligation-gated and event-sourced
* receipts compress history while preserving provenance

This is the difference between “calling an LLM” and “a programming language with a second evaluator.”

---

# 13. What I’ll do next if you say continue

Next I can give you a concrete **reference implementation skeleton** in a specific host language (your choice):

* **TypeScript/Node**: easiest for rapid prototype + async oracle sessions
* **Rust**: strongest for safety, performance, and sandboxing
* **Python**: fastest to hack, but harder to keep strict invariants

Part 6 would include:

1. The core CEK machine types and step function
2. The effect runtime / handler stack
3. The oracle session loop (Req/Resp)
4. The receipt store + content-addressing
5. A minimal `infer.op` handler that can drive a mock engine (for testing)
6. A minimal standard library (streams + amb + match)

Reply **continue** and tell me which host you want first (**TypeScript** or **Rust**).
