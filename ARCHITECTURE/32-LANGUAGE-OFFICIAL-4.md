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

### Part 4 — Final Pass: Profiles, Promotion Pipelines, Semantic Versioning, Complete Dual‑Plane Catalog, and Canonical "Omega Programs"

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
