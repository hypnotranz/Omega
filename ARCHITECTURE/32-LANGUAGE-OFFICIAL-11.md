## Ω Language Specification v1.0

### Part 11 — Deep Hardening: A Complete `amb` Handler Suite, a Correct `syntax-rules` (Nested Ellipses + Hygiene) with Module/Phase Semantics, Hermetic Receipts + Promotion, and a Self‑Hosting Milestone Plan

This part is the “industrial strength” layer: we stop talking in sketches and we nail down the **operational semantics**, **data structures**, **algorithmic invariants**, and **conformance test regimes** for the two subsystems that make Ω genuinely SICP‑grade:

* **Nondeterminism**: `amb` is only “SICP magic” if backtracking is correct, delimited, compositional, and can be interpreted under multiple search regimes.
* **Macros**: `syntax-rules` is only “language-making” if hygiene is correct, ellipses are correct (including nesting), and phase separation + modules are coherent and reproducible.

Then we close with a self-hosting plan that is not hand-waving: it is a **Strangler Fig + Branch by Abstraction** roadmap with deterministic receipts and differential testing.

---

# 96. `omega.nondet` Hardening: Delimited Backtracking as an Effect Interpretation

## 96.1 The nondet effect signature (the algebra)

We standardize a minimal algebra for nondeterminism that is expressive enough to encode SICP `amb` and modern search/ppl:

* `amb.op : (ChoiceList) -> α`
* `amb.fail : (Reason) -> ⊥`
* `amb.cut : () -> Unit`
* `amb.observe : (Observation) -> Unit` *(optional; for scoring / constraints / telemetry)*
* `amb.score : (ScoreDelta) -> Unit` *(optional; for best-first / branch-and-bound)*

In pure SICP style, only `amb.op` and `amb.fail` are strictly required, but `cut` and score hooks become essential for “take it to the limit” without exponential blowups.

### Canonical lowering (call-by-value base)

* `(amb e1 … en)` expands to:
  `((lambda (v1 … vn) (effect "amb.op" v1 … vn)) e1 … en)`
* `(require p)` expands to:
  `(if p (unit) (effect "amb.fail" (record "reason" "require" "pred" 'p)))`
* `(cut)` expands to: `(effect "amb.cut")`

### Call-by-name alternative (recommended for serious search)

For the CBN variant, each choice is a thunk:

* `(amb/cbn e1 … en)` expands to:
  `(effect "amb.op" (delay e1) … (delay en))`
  and the handler forces the selected thunk only.

This is the same move as streams: **control evaluation** by staging.

---

## 96.2 The semantic object of search: Frontier + ChoicePoints + Delimited Regions

### 96.2.1 ChoicePoint

A choice point must be a replayable unit:

```
ChoicePoint = ⟨
  id        : UUID,
  state     : State,          ; CEKS machine snapshot after committing to one choice
  score     : Real,           ; optional, default 0
  bound     : Real | ⊥,        ; optional optimistic upper bound (for branch-and-bound)
  depth     : Nat,
  meta      : Map,            ; e.g., heuristic info, inferred satisfiable prob, VOI, etc.
  traceRef  : Hash | ⊥         ; pointer into Σ (for audit/debug)
⟩
```

This is **Command** + **Memento**: the state is a memento; the act of exploring it is a command.

### 96.2.2 Frontier

Frontier is the only thing that changes between DFS/BFS/beam/best-first/MCTS-ish policies. Therefore Frontier is a **Strategy interface**:

* `push(cp)`
* `pop()`
* `peek()` (optional)
* `clear()`
* `size()`

Concrete strategies:

* DFS: LIFO stack
* BFS: FIFO queue
* Best-first: priority queue by `score` (max-heap)
* Beam: bounded priority queue (keep top-k; drop the rest)
* Branch-and-bound: priority queue by `bound`, prune by global best
* Sampling: reservoir or alias method over weights (if interpreting as distribution)

This is GoF **Strategy** and **State** (frontier controls policy state).

---

## 96.3 Operational semantics of nondet handler: “search interpreter” around CEKS

Think of the nondet handler as a **meta-interpreter** (in the SICP sense) that interprets the `amb.*` effects and orchestrates the CEKS machine.

### 96.3.1 Fundamental invariant: delimited extent

A nondet run is delimited by a wrapper:

* `(first-solution e)`
* `(all-solutions e)`
* `(best-solution :score f e)`
* `(sample-solution :seed s e)`

Each wrapper installs a handler whose internal frontier must not leak outside the handler’s dynamic extent.

That’s the algebraic effects law: operations are handled by the nearest enclosing handler.

### 96.3.2 Canonical search loop (schematic)

Let `Run(State s0)` mean “run CEKS from s0 until it returns a value or emits an opcall.”

Pseudo:

1. Initialize `frontier := empty`.
2. Set `current := s0`.
3. Loop:

   * Evaluate `current` under CEKS until:

     * returns value `v`  → success
     * emits `amb.op`     → branch
     * emits `amb.fail`   → fail
     * emits `amb.cut`    → cut
   * Handle each case:

     * **success**:

       * `first-solution`: return `v`
       * `all-solutions`: collect `v`; set `current := frontier.pop()`, continue
       * `best-solution`: update best; continue exploring while bound permits
     * **amb.op choices**:

       * construct one ChoicePoint per choice:

         * cp_i.state = resume(choice_i)  (or resume(force(thunk_i)) in CBN)
         * cp_i.depth = currentDepth+1
       * order / score / prune cps (policy hook)
       * push all cps to frontier
       * set `current := frontier.pop()`
     * **amb.fail**:

       * set `current := frontier.pop()`
       * if none: return “no solution”
     * **amb.cut**:

       * frontier.clear() (or clear up to delimiter level if nested nondet regions)
       * continue with current branch

This is literally SICP’s failure continuation in a modern effect handler.

---

## 96.4 Completeness, fairness, and termination (formal properties)

Different frontiers imply different meta-properties:

* DFS is not fair: can diverge down one branch and starve others.
* BFS is fair over depth but can blow memory.
* Best-first can be incomplete if heuristic is not admissible and you prune too aggressively.
* Beam is intentionally incomplete (approximate).
* Sampling gives stochastic completeness only in the limit (if you keep sampling).

Ω must treat these as **explicit policy attributes**, so that a strict profile can forbid incomplete modes for certain tasks.

Define:

* `completeness`: if a solution exists, the strategy will find one (given unbounded resources).
* `soundness`: returned solutions satisfy all `require` constraints and any declared contracts (enforced by construction if `require` is implemented as failure).
* `fairness`: no infinite starvation for finite-depth solutions.

In Ω, these become fields in Policy:

```lisp
(policy
  (amb :strategy "bfs")
  (guarantees (list 'sound 'fair 'complete))
  (budgets ...))
```

This is the “truth governance” pattern applied to search.

---

## 96.5 Heuristic augmentation: inference-driven scoring and pruning

This is the inference plane counterpart.

### 96.5.1 Accumulating a constraint context

Every time `(require p)` occurs, record the predicate (as Syntax) in the nondet handler’s “constraint log”:

* for explainability,
* for contradiction detection,
* for heuristic scoring.

This is not mutation of the program semantics; it’s meta-data stored in handler state and ledgered.

### 96.5.2 Heuristic scoring hook

When `amb.op` occurs, call a heuristic function:

* extensional heuristic: user-supplied scoring closure
* intensional heuristic: `int` query that returns branch probabilities and cost

**Structured goal (example)**:

```lisp
(record
  "goal" "amb-branch-order"
  "constraints" constraintsVec
  "choices" choicesVec
  "envDigest" envDigest
  "objective" (record "maximize" "p_success" "minimize" "expected_cost"))
```

The oracle returns Meaning with `paths` and `cost`. Convert to score:

[
score_i = \alpha \cdot \widehat{P}_i - \beta \cdot \widehat{C}_i
]

Policy selects α, β, and how to treat uncertainty (confidence).

### 96.5.3 Critic pruning hook

Install critic stages, e.g.:

* `critic:contradiction`: run a lightweight symbolic simplifier over predicates
* `critic:probe`: request `ReqEval` on partially instantiated predicates where possible
* `critic:counterexample`: ask inference to produce a witness of contradiction

If a branch is pruned, you must log:

* which branch,
* why,
* confidence and evidence if any.

Strict profiles may require “pruning must be conservative” (only prune if contradiction is certain, or if you discharge an obligation via proof/test). Explore profiles can prune speculatively but must never auto-commit results as trusted.

This is **Circuit Breaker** + **Auditing Decorator** at the semantic layer.

---

## 96.6 “Disagreement logging” and triangulation (engine vs evaluator vs proofs)

When inference is used for scoring/pruning, record disagreements:

* inference says branch likely satisfiable but extensional probes fail
* inference says prune but extensional probes show satisfiable

These become explicit events in Σ and feed policy training.

This is **Event Sourcing** used for evaluator improvement.

---

## 96.7 RL integration: episodes, reward shaping, and policy updates

### 96.7.1 Episode schema for nondet search

A nondet run yields a trajectory:

* `(choice_id, choice_value, constraint_state_digest, heuristic_score, pruned?, reason)`
* step cost (CEKS steps)
* inference/tool cost (tokens/tools)
* terminal result (success/failure)
* reward

Reward shaping (canonical):

* +1 for valid solution found
* -1 for failure/no-solution
* -λ·(CEKS steps) - μ·(tokens) - ν·(toolCalls)

This supports:

* bandit tuning of heuristic parameters
* offline RL for branch ordering
* learning prune thresholds
* learning when to call inference (cost-benefit tradeoff)

### 96.7.2 Policy artifacts and promotion

A newly trained policy is a versioned artifact. Promote it only if:

* it doesn’t widen capabilities,
* it improves success rate or reduces cost under test corpora,
* it doesn’t regress correctness (solutions must still satisfy constraints).

That’s a semantic CI pipeline.

---

# 97. `syntax-rules` Hardening: Correct Nested Ellipses + Scope-Set Hygiene + Phase/Module Semantics

The macro expander is the *other evaluator* in a Lisp system. Ω’s expander must be:

* hygienic,
* deterministic under strict compilation profiles,
* module-aware,
* and capable of semantic macros (inference-assisted) **with hermetic receipts**.

## 97.1 Syntax objects: identifiers with scope sets

A syntax object is:

[
\text{Syntax} = \langle datum,\ scopes,\ srcloc,\ phase \rangle
]

Identifiers are special:

[
\text{Ident} = \langle name,\ scopes,\ srcloc,\ phase \rangle
]

### Scope operations (core)

* `addScope(stx, scope)` — union
* `flipScope(stx, scope)` — symmetric difference (optional, for certain algorithms)
* `stripScope(stx, scope)` — remove

We’ll use union-only for sets-of-scopes hygiene (Racket-style). Flip is useful for more advanced mark/cancel schemes, but not required.

## 97.2 Three scopes: use-site, introducer, binder

When expanding a macro call:

1. Add **use-site scope** `U` to the macro call’s syntax (to distinguish call site).
2. In the transformer output, identifiers that come from the macro template get an **introducer scope** `I` so they won’t capture user identifiers.
3. When the expansion introduces bindings (e.g., `let` → `lambda` binds variables), those binder identifiers get a **binder scope** `B` so that references in the introduced body resolve correctly.

This ensures:

* introduced binders bind introduced references,
* user references bind user binders,
* no accidental capture.

## 97.3 Resolution algorithm: “best match” scope selection

Maintain a binding environment keyed by `(name, scopeSignature)` where scopeSignature is the set of scopes present at binding site.

To resolve identifier `id(name, scopes)`:

* collect all bindings with that `name`
* choose binding `b` with maximal scope intersection size `|scopes ∩ b.scopes|`
* tie-break:

  * if two candidates have equal maximal intersection and neither is subset-dominant → **ambiguous** (macro hygiene error)
  * if one candidate’s scopes are a strict superset match under a defined partial order, pick it

This is the standard set-of-scopes hygiene behavior.

Strict mode should treat ambiguity as a compilation error. Explore mode might allow explicit disambiguation forms, but do not default to heuristics; that destroys hygiene.

---

# 97.4 `syntax-rules`: Pattern compilation and matching with nested ellipses

This is where most incorrect macro implementations break.

## 97.4.1 Pattern representation

A compiled pattern tree:

* `PVar(x)` — pattern variable
* `PLit(id)` — literal identifier (keyword)
* `PWild` — `_`
* `PAtom(atom)` — literal atoms
* `PList(items)` — list pattern
* `PEllipsis(subpat)` — repeated subpattern

But the tricky part is that `...` applies to the **preceding pattern element** in the list, and nesting introduces multi-dimensional binding shapes.

So represent list patterns as a sequence of elements where any element may have `repeat=true`:

```
PList([ (p1, repeat?), (p2, repeat?), ... ])
```

## 97.4.2 Substitution values with rank (ellipsis depth)

A binding for variable `x` is not always a single syntax object. Under ellipses it becomes a vector (or nested vectors).

Define:

```
SubstVal :=
    One(Syntax)
  | Many(List<SubstVal>)   ; nested Many corresponds to nested ellipses
Subst := Map<VarName, SubstVal>
```

The “rank” of a substitution value is the nesting depth of `Many`.

* no ellipses: rank 0 (`One`)
* one ellipsis: rank 1 (`Many(One(...), One(...))`)
* nested ellipses: rank > 1 (`Many(Many(...), Many(...))`)

## 97.4.3 Matching algorithm (key invariants)

### Invariant A: Shape consistency

If `x` is bound under an ellipsis, every occurrence of `x` under the same ellipsis nesting must produce the same number of repetitions (same shape), otherwise the pattern match fails.

### Invariant B: Rank consistency

If `x` appears at rank `r` in the pattern, it must be substituted at rank `r` in the template at corresponding ellipsis structure, otherwise template expansion fails.

### Invariant C: Literal identifiers compare by hygienic identity

Literal keywords must be compared by resolved binding identity, not string equality, in hygienic systems. In practice:

* compare identifier names + scope sets
* or compare resolved binding keys if available

## 97.4.4 Matching procedure sketch (deterministic)

`match(pat, stx, literals) -> Subst | null`

* If pat is `PWild`: succeed with empty subst
* If `PAtom`: succeed iff datum equal
* If `PLit`: succeed iff identifier equals literal under hygienic equality
* If `PVar(x)`:

  * bind x to `One(stx)` if unbound
  * if already bound, require hygienic-equal to existing binding
* If `PList(items)`:

  * require stx is list of syntax items
  * match element-by-element, with special handling for repeated elements:

    * for an element `(p, repeat=true)`, consume zero or more stx items, producing `Many` bindings
    * must decide how many items to consume; in syntax-rules, repetition consumes as many as needed to match the rest of the pattern (greedy with backtracking). Implement deterministically by:

      * compute minimal suffix match lengths
      * or do a bounded backtracking (still deterministic because input is finite)

Nested ellipses require recursion where a repeated element itself contains repeated subpatterns.

The easiest correct implementation is to compile patterns into an NFA-like matcher with explicit repetition nodes, but a recursive matcher with careful shape constraints is fine for a reference expander.

---

# 97.5 Template expansion with ellipses: “zip” semantics over Many bindings

Template expansion `expandTemplate(template, subst)` must replicate repeated template segments according to the shape of the bindings.

### Core rule: Ellipsis in templates iterates in lockstep

If template contains `( ... )` around a segment referencing repeated variables, replicate that segment once per element of the `Many` binding. If multiple variables appear, they must have the same repetition length at that ellipsis depth.

That is essentially a **zip** over vectors of substitutions.

For nested ellipses, you recursively zip at each rank.

This is the main source of subtle bugs; you must enforce shape/rank constraints or you’ll produce unsound expansions.

---

# 97.6 Module/phase interaction: require/provide with phase levels

A module has:

* runtime exports (phase 0)
* macro exports (phase 1)
* optional higher phase exports (if macros generate macros)

Operations:

* `(require M)` imports runtime bindings from module M into Env0
* `(require-for-syntax M)` imports macro bindings into Env1
* `(provide ...)` exports runtime bindings
* `(provide-for-syntax ...)` exports macro bindings

**Phase separation invariant**:

* macro expansion must not depend on runtime evaluation unless explicitly in `begin-for-syntax`, and even then it is phase 1 evaluation, not phase 0.

This is a two-tier interpreter tower; treat it as such.

---

# 97.7 Hermetic receipts for expansion: reproducible builds

Any semantic macro (or any macro-time inference/tool usage) must produce an expansion receipt that is stored in the module artifact.

### Expansion receipt fields (normative)

* input syntax hash
* macro transformer digest (module hash + macro binding key)
* macro-time environment digest (imports at phase 1)
* output syntax hash
* deterministic parameters (seed, sampling settings)
* engine/policy/prompt versions if inference used
* obligations + evidence hashes (if semantic macro)
* tool evidence hashes (if tools allowed)

Strict compilation rejects expansions that cannot be reproduced from receipts.

This is the **Hermetic Build** pattern internalized into language semantics.

---

# 97.8 Conformance test suite: macro hygiene torture tests + ellipsis correctness

You need an executable conformance suite. Minimum categories:

## Hygiene tests

* “introduced identifier does not capture user binding”
* “user binding does not capture introduced identifier”
* “macro introduces binder; references in template resolve to that binder, not user binder”
* “macro expansion across modules preserves hygiene”
* “macro used inside another macro (macro-generating macros) remains hygienic”
* “shadowing and renaming behave as expected”

## Ellipsis tests

* single ellipsis: binding and replication works
* multiple vars repeated: lockstep zip enforced
* nested ellipses: correct rank handling
* shape mismatch: expansion must fail deterministically with useful error
* templates where ellipsis repeats non-variable substructure correctly

## Phase tests

* runtime binding not visible at macro time unless explicitly imported for syntax
* macro time evaluation does not pollute runtime env
* cross-phase persistence rules behave correctly (if supported)

## Hermeticity tests

* compile twice with strict profile → identical expanded core AST hash
* macro-time inference allowed only with receipts → builds reproducible
* changing engine/policy version invalidates receipt and requires rebuild

This is “compiler QA” for the expander.

---

# 98. Self-Hosting Milestone Plan: Expander-in-Ω, Nondet-in-Ω, and Differential Testing Against Host

Now we put it all together into a credible self-hosting story.

## 98.1 Architectural boundary: “Kernel Host” vs “Boot Image”

Define an interface boundary (Ports-and-Adapters):

* Reader port: text → Syntax
* Expander port: Syntax → Core AST
* Evaluator port: Core AST → Value
* Artifact store port: save/load module artifacts, receipts, ledgers

Start with host implementations. Then gradually replace with Ω implementations.

This is **Strangler Fig**: new implementation grows around old, then takes over.

## 98.2 Differential testing harness (the non-negotiable safety net)

For any stage where you replace a subsystem, use differential testing:

* Host expander vs Ω expander: same input module → same expanded core AST (up to α-equivalence / scope normalization)
* Host evaluator vs Ω evaluator (meta-circular): same program → same outputs in an envelope
* Compiler passes: original vs compiled output equivalence under test suites

Add metamorphic tests:

* alpha-renaming invariance
* reordering of independent definitions
* macro expansion invariance under benign whitespace/comments (reader correctness)

This harness is your proof substitute when full formal proof is expensive.

## 98.3 Boot stages (practical, deterministic)

### Milestone A — Nondet handler in host, nondet library in Ω

* implement `amb` handler in host runtime
* provide `(amb ...)` and `(require ...)` macros in Ω stdlib

### Milestone B — Expander in host, macro libraries in Ω

* implement hygienic expander in host
* write macros (`let`, `cond`, `and`, `or`, `match`) in Ω itself

### Milestone C — Expander core in Ω (phase-1 evaluator)

* implement `syntax-rules` matcher and template engine in Ω
* host provides only minimal primitives for:

  * syntax object construction
  * scope set operations
  * hash/digest

### Milestone D — Module compiler in Ω

* module loader, artifact builder, receipt integration in Ω
* host remains adapter for filesystem/storage

### Milestone E — Meta-circular evaluator in Ω (Ω₀ subset)

* implement `eval0` in Ω
* run differential tests vs host CEKS evaluator

### Milestone F — Strategy/oracle orchestration moves into Ω

* policies and strategy pipelines become Ω programs
* host oracle handler becomes a thin adapter that runs a “strategy interpreter” written in Ω

At this point, Ω is not just self-hosted; it is self-directed: it can rewrite, verify, and promote its own macros/policies.

## 98.4 Guardrails to prevent semantic collapse during self-hosting

* strict profile for build artifacts
* receipts pinned everywhere
* promotion required for switching default expander/evaluator versions
* capability narrowing by default (no tools at compile time unless explicitly enabled)
* “no-new-facts” for compile-time inference unless evidence attached

These are not “nice to haves”; they are the only way to keep the system from degenerating into irreproducible behavior.

---

# 99. The “Take It To The Limit” Synthesis: What You Now Have

At this point—if you implement exactly what’s described—you have a language where:

* SICP’s “change `eval/apply` to get a new paradigm” becomes “install a new handler regime to reinterpret effects.”
* Nondeterminism is not a library; it is a semantic interpretation that can be **learned** and **certified**.
* Macro expansion is not “string rewriting”; it is a hygienic, phase-separated compiler with hermetic receipts.
* Inference is not an API call; it is a **second evaluator plane** (intensional) that is:

  * interactive (oracle protocol),
  * capability-scoped,
  * budgeted,
  * audited,
  * and promotion-gated.

This is SICP’s core methodology (language as a tool for controlling complexity) lifted into the inference era without abandoning correctness discipline.

---

# 100. If you want the next continuation

The next (and final) “maximal pass” would be **Part 12**: a complete “reference suite” of Ω code that implements:

* the `amb` wrappers (`first-solution`, `all-solutions`, `best-solution`, `sample-solution`)
* the full `syntax-rules` expander in runnable pseudo-code (including nested ellipses)
* module artifacts + receipt verification
* the standard SICP demos as executable Ω modules

If you say **continue**, I’ll write that suite out in Ω source form (with the minimal host primitives it assumes), so you can directly translate it into your reference interpreter.
