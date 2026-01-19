## Ω Language Specification v1.0

### Part 8 — Hygienic Macros, Phase Separation, Semantic Macros, Module/Package System, Strategy DSL Runtime, Promotion Pipelines, and the Learning Loop (Policy + Prompt + Memory)

This part is where Ω becomes a **SICP‑grade language workbench** rather than “an interpreter with an oracle handler.”

SICP’s real power move was never “Lisp syntax.” It was the *combination* of:

* a tiny evaluator,
* homoiconicity,
* and the ability to **make up languages** (syntactic and semantic) as a normal act of programming.

Ω must do that too—**but** now “language-making” includes:

* **syntactic macros** (classic),
* **semantic macros** (meaning‑preserving, obligation‑carrying rewrites),
* compile‑time *inference* (ICL/prompt/policy/engine as first-class),
* and governance (reproducible builds, receipts, promotion).

This part is therefore the industrial-grade “macro/packaging/CI” chapter that SICP didn’t fully spell out.

---

# 70. The Expansion Pipeline: Read → Expand → Evaluate

Ω must have an explicit compilation pipeline:

1. `read` (text → syntax objects)
2. `expand` (syntax → core AST; hygienic macro expansion; lowering; staging)
3. `eval` (core AST → value; CEKS + effects)

This is not cosmetic. It is the **separation of concerns** that makes the language:

* extensible,
* analyzable,
* and reproducible.

In Ω, “inference” is allowed in expansion *only if explicitly enabled by profile/capabilities*.

---

# 71. Hygiene: Syntax Objects, Scopes, and Phase Levels

If you implement macros without hygiene, you get capture bugs, accidental shadowing, and “language-making” becomes a minefield. Ω must support hygienic expansion as a primitive capability.

## 71.1 Syntax objects

A syntax object is not a datum. It is:

[
\text{Syntax} = \langle \text{datum}, \text{scopes}, \text{srcloc}, \text{phase} \rangle
]

### Concrete representation

```ts
type Scope = string; // fresh unique IDs
type Scopes = Set<Scope>;

type Ident = {
  tag: "Ident";
  name: string;
  scopes: Scopes;
  loc?: Loc;
};

type Syntax =
  | { tag: "SAtom"; atom: Atom; scopes: Scopes; loc?: Loc }
  | Ident
  | { tag: "SList"; items: Syntax[]; scopes: Scopes; loc?: Loc };
```

Key point: **identifiers carry scopes**. Expansion manipulates scopes; resolution uses scopes.

## 71.2 Binding resolution (the hygiene algorithm)

There are multiple known hygienic algorithms (marks, renaming, sets-of-scopes). The **sets-of-scopes** formulation (Racket-style) is the cleanest for implementation:

* Each binding introduces a **binding scope**.
* Each macro use introduces a **use-site scope**.
* Macro expansion introduces an **introducer scope** for identifiers created by the transformer.

Resolution rule (informal, but precise enough to implement):

> An identifier resolves to the binding whose scope-set has the “best match” with the identifier’s scope-set, i.e., maximal intersection under the language’s partial order.

In practice:

* Maintain an environment mapping `BindingKey = (name, scopeSignature)` → binding.
* When resolving an identifier:

  * collect all candidate bindings with same `name`
  * choose the one with maximal scope match
  * error on ambiguity (or require disambiguation syntax)

This gives you true hygiene without gensym everywhere.

---

# 72. Phase Separation: Compile-Time vs Run-Time (and why inference complicates it)

A macro system is a *two-language tower*:

* **Phase 0** (run-time): normal program evaluation
* **Phase 1** (expand-time): macro transformers execute here

Ω must support:

* `define` (phase 0 bindings)
* `define-syntax` (phase 1 transformer bindings)
* `begin-for-syntax` (execute code at expansion time)
* `require` and `provide` with phase levels

### Principle

**Phase 1 must be at least as disciplined as phase 0**, because it can mutate the program’s meaning before runtime even begins.

### Why inference makes this non-trivial

If compile-time macros can call `infer`, you have a “compiler” whose behavior can vary nondeterministically unless governed. Therefore:

* compile-time inference requires a **determinism envelope**, or
* compile-time inference must produce **receipts** that freeze results, or
* compilation is non-reproducible (unacceptable for serious systems)

Ω supports all three, as explicit profile choices.

---

# 73. Two Kinds of Macros: Syntactic Macros and Semantic Macros

This is the key Ω extension.

## 73.1 Syntactic macros (classic hygiene)

A syntactic macro is:

[
T : \text{Syntax} \to \text{Syntax}
]

It rewrites syntax, purely structurally, without claiming semantic properties.

### Forms

* `define-syntax`
* `syntax-rules` (pattern macros)
* optional `syntax-case` (procedural macros)

Example:

```lisp
(define-syntax let
  (syntax-rules ()
    ((_ ((x e) ...) body ...)
     ((lambda (x ...) (begin body ...)) e ...))))
```

This is pure SICP/Scheme language-making.

## 73.2 Semantic macros (Ω’s new thing)

A semantic macro is:

[
T^* : \text{Syntax} \to \langle \text{Syntax}, \text{Obligation}, \text{Evidence} \rangle
]

It is a **meaning-preserving transformation** (or meaning‑improving optimization) that must discharge obligations before it can be promoted/committed.

### Forms (recommended)

* `define-semantic-syntax` — defines a transformer allowed to:

  * call `rewrite` / `infer` at expansion time (if profile permits),
  * attach obligations,
  * produce a frozen receipt for reproducibility.

Example:

```lisp
(define-semantic-syntax fuse-streams
  (lambda (stx)
    (define q (syntax->datum stx))
    (define m (rewrite q :goal { preserve extensional optimize allocations }
                       :engine engine:optimizer
                       :policy policy:strict))
    (values (datum->syntax stx (Meaning.rewrite m))
            (Meaning.obligation m)
            (Meaning.evidence m))))
```

### The decisive point

Semantic macros do not *apply silently*. They expand into either:

* the transformed program **plus a latent `commit`** requirement; or
* the transformed program but with a **compile-time receipt** that pins the rewrite.

Both keep inference as a language feature, not an ambient magic.

---

# 74. Expansion-Time Effects and Compile-Time Governance

Macro transformers are programs. Therefore, in Ω, the expander is itself a CEKS evaluator with effects—**but under a different profile**.

## 74.1 Macro expansion runtime (“Phase 1 runtime”)

Expansion evaluates transformer code under:

* restricted capabilities by default:

  * usually no tools
  * often no eval re-entry into phase 0
  * inference allowed only in permitted profiles
* strict determinism envelope by default (for reproducible builds)

### Profiles for compilation

* `profile:compile-strict`

  * `infer` allowed only if it yields a receipt and uses deterministic envelope
* `profile:compile-airgap`

  * forbids tool/network; inference can only use pinned local engines/memory
* `profile:compile-explore` (only for prototyping)

  * may allow nondeterminism, but artifacts cannot be promoted without freezing

## 74.2 “Freezing” semantic macro results

When a semantic macro uses inference, it must emit a `macro.receipt`:

* digest of input syntax
* engine/policy/prompt versions
* output syntax
* obligations + evidence hashes
* deterministic seed/parameters (if applicable)

That receipt becomes part of the module artifact so builds are replayable.

This is **Build Reproducibility** as a language invariant.

---

# 75. Modules and Packages: Ω as a Language Ecosystem, Not a Script

SICP’s “make up languages” becomes practical only with a module system.

## 75.1 Module form

```lisp
(module omega.stream
  (provide delay force cons-stream stream-map stream-filter)
  (provide-for-syntax stream-syntax)     ; macro exports
  (require omega.kernel)
  (require-for-syntax omega.match)       ; macro-time imports

  ;; runtime definitions...
  (define (delay e) ...)
  ...
)
```

There are **two export sets**:

* phase 0 exports (`provide`)
* phase 1 exports (`provide-for-syntax`)

And two import sets:

* `require` (runtime)
* `require-for-syntax` (macro time)

This is essential phase hygiene.

## 75.2 Artifact registry and content addressing

A compiled module is an artifact:

* source hash
* expansion receipt(s)
* compiled core AST
* dependency digests (including macro deps)
* test suites and obligations
* provenance pointer into Σ ledger

This makes module compilation behave like a hermetic build system (Bazel/Nix philosophy), but inside language semantics.

---

# 76. The Inference Strategy DSL as a First-Class Program (Not a Config File)

You already have `Policy` values. Now we make them programmable in Ω, and give them *operational semantics* in the oracle handler.

## 76.1 Strategy pipeline IR

At runtime, a strategy is a list of stages:

```ts
type Stage =
  | { tag: "Speculate"; n: number; temp: number }
  | { tag: "Simulate"; maxEval: number }                     // allows REPL requests
  | { tag: "Triangulate"; engines: string[] }
  | { tag: "RewriteIf"; objective: any; threshold: number }
  | { tag: "Critic"; kind: "contradiction" | "fuzz" | "xcheck"; budget: number }
  | { tag: "VerifyTests"; suiteRef: string }
  | { tag: "VerifyEqExt"; samples: any }
  | { tag: "VerifyProof"; prover: string }
  | { tag: "EscalateIf"; predicate: any; then: Stage[] }
  | { tag: "CompressIf"; voiBudget: number }
  | { tag: "EmitExample"; mode: "always" | "on-fail" | "on-success" }
  | { tag: "ReturnBest" };
```

## 76.2 Strategy execution model (handler-level orchestration)

The oracle handler is upgraded:

* It no longer blindly “runs an engine session.”
* It becomes an **orchestrator**:

  * calling engines,
  * requesting eval/tests/tools,
  * assembling Meaning objects,
  * applying critics,
  * discharging obligations,
  * and producing a final Meaning/Dist.

That means `infer.op` handling is effectively a small *workflow engine*—but *semantic*, not business logic.

This is classic **Pipes-and-Filters**, but staged over Meaning, and governed by effects/caps.

## 76.3 Policy as a Strategy + Truth regime + Budgets + Capability envelope

A policy value determines:

* stage pipeline
* confidence algebra
* acceptance thresholds
* escalation ladder
* test suites and proof hooks
* budgets and tool caps
* whether to emit training data

So “policy learning” becomes meaningful: policies control computational economics, not just prompt words.

---

# 77. Promotion Pipeline Automation: CI/CD for Semantic Artifacts

You wanted “not just rewriting code—improve meaning”—but to do that safely you need an internal semantic CI.

## 77.1 Artifacts that need promotion

* prompts
* policies
* engines (weights+prompt+memory+policy)
* semantic macros (rewrites)
* compiled modules
* ML models (if you enable `omega.ml`)

## 77.2 Pipeline DSL

A pipeline is itself a DSL (language-made language):

```lisp
(pipeline promote-policy
  (stage build
    (action (train/policy policy:baseline episodes :algorithm "offline-RL")))
  (stage test
    (action (run-suite suite:regression)))
  (stage redteam
    (action (fuzz :goal "break policy behavior" :budget 500)))
  (stage benchmark
    (action (compare-cost :against policy:baseline :max-multiplier 1.05)))
  (stage gate
    (require (no-new-authority))
    (require (all-stages-pass)))
  (stage promote
    (action (promote policy:baseline/v2))))
```

Each `stage` emits:

* ledger events
* receipts with hashes and metrics

This is *literally* a Build Pipeline pattern rendered as language semantics.

## 77.3 Semantic versioning enforcement

Promotion checks:

* whether authority increased (caps widened)
* whether obligations changed
* whether behavioral envelopes changed beyond ε
* whether cost regressions exceed thresholds

Promotion is denied if:

* it widens authority in a strict profile
* it violates regression suite
* it increases hallucination rate beyond bounds
* it breaks reproducibility receipts

This is where Ω becomes “serious.”

---

# 78. The Learning Loop as a Language Mechanism (Policy + Prompt + Memory)

Now we fully answer your original intuition (“define-like evolution”) in concrete runtime terms.

## 78.1 Episodes and reward are effects

An episode delimits a trajectory. Rewards become first-class emissions.

```lisp
(episode
  (define m (int qexpr :engine E :policy Π))
  (define v (decide-speculate-or-eval m))
  (reward (score v ground-truth)
          (record "tokens" (usage.tokens) "tools" (usage.tools))))
```

This produces a structured dataset in the ledger.

## 78.2 Offline RL for policy learning (the practical default)

In most real environments you will not be updating base weights online; you will:

* collect trajectories,
* compute returns,
* and update **policy parameters** (search thresholds, escalation rules, VOI settings).

So `train/policy` is an effect whose adapter can be:

* a pure function (bandit tuning, heuristics),
* an external training job,
* or a learned policy model (small DNN) compiled into `PolicyVal`.

Either way, it produces a new versioned policy artifact with receipts.

## 78.3 Prompt evolution as “semantic refactoring” of Prompt AST

Prompt tuning is not “string hacking.” In Ω it is:

* rewrite Prompt AST
* validate on prompt regression suite
* promote only with receipts

Example:

```lisp
(define-prompt prompt:solver/v2
  (commit/prompt
    (rewrite prompt:solver/v1
      :goal { reduce hallucination preserve schema improve coverage }
      :policy policy:prompt-tune)
    :requires { suite:prompt-regressions pass }))
```

This realizes your “augmenting define” intuition in a disciplined way:

* `define-prompt` is definitional binding
* prompt evolution is `rewrite + commit + promote`, not mutation

## 78.4 Memory updates: “learning without training weights”

The most robust “learning” in production is often:

* semantic memory: cache receipts, counterexamples, verified rewrites
* retrieval-guided inference: engine consults memory via `ReqObserve` / `ReqHydrate`

Memory is updated by:

* `remember` effect (capability-gated)
* `prune` effect (VOI-based)
* invalidation on dependency change (artifact digests)

This gives you persistent improvement even with frozen base weights.

---

# 79. Hermeticity and Deterministic Builds (Non-optional for semantic macros)

If semantic macros can call inference at expansion time, you must enforce a build discipline, or you get “heisenbuilds.”

Ω supports **Hermetic Compilation Mode**:

* engine versions pinned
* deterministic envelope pinned (seed, sampling regime)
* tool access forbidden (unless tool outputs are content-addressed and cached)
* macro receipts required for any inference-derived expansion

In strict mode:

* compilation fails if a semantic macro tries to do nondeterministic inference without producing a receipt

This is essentially the “Nix/Bazel” worldview internalized into language semantics.

---

# 80. Putting It Together: A Full Example of a Semantic Macro Package

A package `omega.stream.opt` provides a semantic macro `stream-opt` that fuses maps/filters.

### 80.1 Module exports a semantic macro at phase 1

```lisp
(module omega.stream.opt
  (provide-for-syntax stream-opt)
  (require omega.stream)
  (require-for-syntax omega.kernel omega.intension)

  (define-semantic-syntax stream-opt
    (lambda (stx)
      (define q (syntax->datum stx))
      (define m (rewrite q
                 :goal { preserve extensional optimize allocations }
                 :engine engine:optimizer
                 :policy policy:compile-strict))
      (values (datum->syntax stx (Meaning.rewrite m))
              (Meaning.obligation m)
              (Meaning.evidence m)))))
```

### 80.2 User uses it at compile time

```lisp
(require omega.stream.opt)

(stream-opt
  (stream-map f (stream-map g s)))
```

Compilation:

* expands `stream-opt` via semantic macro
* emits macro receipt (input hash, output hash, obligations)
* if profile is strict, requires discharge of obligations before module can be promoted

This is “make up languages” with inference, but still governed.

---

# 81. What’s Next (Part 9)

At this point you have:

* a correct CEKS + effects interpreter,
* hygienic macros with phase separation,
* semantic macros with obligations and receipts,
* module/package artifacts,
* strategy DSL and handler-level orchestration,
* promotion pipelines,
* and a learning loop that updates policy/prompt/memory as first-class artifacts.

**Part 9** is where we “close the SICP loop” in the strongest possible way:

1. Implement `amb` via effects + backtracking handler (full SICP nondeterminism chapter)
2. Implement streams + memo-thunks + strictness/productivity inference tooling
3. Implement generic ops + inferred adapter synthesis with commit barriers
4. Implement propagator constraint networks + diagnosis/repair Meaning
5. Implement explicit-control evaluator + CPS compiler passes as semantic macros
6. Show a self-hosting story: Ω macro expander written in Ω, evaluator written in Ω (“meta-circular” + “meta-intensional” towers)

Reply **continue** and I’ll deliver Part 9 with concrete definitions (Ω code + runtime hooks) for the remaining SICP tower pieces and the self-hosting / bootstrapping plan.
