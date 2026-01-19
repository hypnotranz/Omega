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
