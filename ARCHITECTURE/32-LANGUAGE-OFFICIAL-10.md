## Ω Language Specification v1.0

### Part 10 — Concrete `amb`/Search Runtime + Full Hygienic `syntax-rules` Expander + Hermetic Semantic-Macro Receipts + Reference Ω Programs

This part is the “make it real” pass for the two hardest subsystems you called out:

1. **SICP nondeterminism** implemented *properly* as an algebraic-effect interpretation (`amb`, `require`, backtracking), plus modern search policies (BFS/beam/best‑first/MCTS‑ish), plus an inference-driven heuristic/critic plane, plus RL‑ready episode emission.
2. **A hygienic macro expander** with `syntax-rules`, phase separation, module integration, and **semantic macros** that can call inference at expansion time *without destroying reproducibility* (hermetic build receipts).

I’ll also provide canonical Ω programs (SICP demos) that run on top of these.

---

# 91. `omega.nondet`: `amb` as an Effect, Backtracking as a Handler, Search as a Policy, and Heuristics as Intensional Meaning

SICP’s nondeterministic evaluator works because it **reifies choice points** and **reinstalls continuations** on failure. In Ω, the *exact* analog is:

* choice points are stored resumptions (`resume : Val → State`)
* failure is an effect (`amb.fail`) interpreted by the handler
* the exploration order is a **Strategy** object (DFS/BFS/beam/best‑first/etc.)
* inference augments that Strategy with heuristic ordering, pruning, and scoring
* governance ensures we never “accept a solution” without whatever obligations the profile demands

## 91.1 Surface language forms (Ω)

### 91.1.1 Choice

```lisp
(amb e1 e2 ... en)
```

### 91.1.2 Constraint / branch filter

```lisp
(require predicate-expr)
```

### 91.1.3 Optional: Cut (commit to current branch; prune alternatives)

This is a classic Prolog move; it’s also useful to control explosion. Make it explicit:

```lisp
(cut)              ; discard all saved choice points in the current handler scope
```

### 91.1.4 Collectors

```lisp
(all-solutions expr)
(first-solution expr)
(best-solution :score score-fn expr)
(sample-solution :seed s expr)       ; probabilistic sampling if interpreted as Dist
```

These are not “library functions” in the ordinary sense: they install a handler regime (Template Method + Strategy) and run the body.

---

## 91.2 Lowering to effects (kernel-level desugaring)

### 91.2.1 `amb`

```lisp
(amb e1 e2 ... en)
;; expands to
(effect "amb.op" e1 e2 ... en)
```

Because argument evaluation matters, you have two legitimate semantics (choose one; both are SICP-consistent depending on evaluation order discipline):

* **Call-by-value choices** (evaluate all `ei` then choose among values): simpler, but may do wasted work.
* **Call-by-name choices** (store resumptions that evaluate each `ei` only if chosen): closer to the classic nondet evaluator and more efficient.

Ω should support both via policy:

* `amb/cbv` expands to evaluate `ei` first
* `amb/cbn` expands to choices as thunks (delays) to preserve laziness

A pragmatic implementation starts with CBV; then add CBN as an optimization pattern.

### 91.2.2 `require`

```lisp
(require p)
;; expands to
(if p (unit) (effect "amb.fail" (record "reason" "require" "expr" 'p)))
```

`amb.fail` is the semantic analog of “backtrack now.”

### 91.2.3 `cut`

```lisp
(cut)
;; expands to
(effect "amb.cut")
```

---

## 91.3 Handler semantics: a disciplined backtracking kernel

### 91.3.1 The key runtime object: ChoicePoint

A `ChoicePoint` must capture everything necessary to resume:

* the resumption (`resume`) applied to a particular choice value (or thunk)
* the continuation stack is already inside `State` (CEKS), so storing `State` is enough
* store and ctx are already in State (and in `OpCall`), so the state snapshot is self-contained

```ts
type ChoicePoint = {
  id: string;
  state: State;                 // state after selecting a specific choice
  score?: number;               // optional heuristic score
  meta?: Record<string,unknown>; // VOI, predicted satisfiable probability, etc.
};
```

### 91.3.2 The search frontier (policy-controlled)

A nondet handler maintains a frontier:

* stack → DFS
* queue → BFS
* priority queue → best-first
* bounded priority queue → beam search
* stochastic reservoir → sampling

```ts
interface Frontier {
  push(cp: ChoicePoint): void;
  pop(): ChoicePoint | undefined;
  size(): number;
  clear(): void;
}
```

### 91.3.3 Failure is a control signal (not a return value)

In algebraic-effect terms, failure is best represented as an exception-like *abort* that is caught by the nondet handler’s outer exploration loop.

Define a sentinel error:

```ts
class AmbFail extends Error {
  constructor(public reason: unknown) { super("amb.fail"); }
}
```

When the handler sees `amb.fail`, it throws `AmbFail`, and the exploration loop catches it and continues with the next frontier element.

This is morally identical to SICP’s “failure continuation.”

---

## 91.4 Concrete handler algorithm (DFS baseline)

Here is the **canonical DFS nondet handler** implementation shape. This is essentially the SICP nondet evaluator encoded as an effect handler + an explicit frontier.

### 91.4.1 DFS exploration loop

**Invariant**: the handler explores alternatives until one branch produces a value (first-solution semantics) or until frontier is exhausted.

Pseudo-code:

1. Evaluate `body`
2. If `amb.op` occurs:

   * create a choice point per alternative
   * push all but the first
   * explore first immediately (tail-call to runtime)
3. If `amb.fail` occurs:

   * pop next choice point and explore it
4. If no frontier left:

   * propagate “no solution”

### 91.4.2 Handler contract (delimited)

This handler only governs failures and choices inside its dynamic extent (`all-solutions`/`first-solution` wrapper). Outside, `amb.op` is unhandled.

That’s delimited control, exactly as algebraic effects intend.

---

## 91.5 “all solutions” vs “first solution” vs “best solution”

These are not syntactic sugar; they are **different handler interpretations** (Strategy + Template Method).

### 91.5.1 First solution

Stop at first successful completion.

### 91.5.2 All solutions

Continue after each success, collecting results, until frontier empty.

This requires a controlled “success continuation”:

* after producing a value, push a continuation that “collects it then continues exploring.”

### 91.5.3 Best solution

Two variants:

* **best-first search**: the frontier is a priority queue by score.
* **branch-and-bound**: maintain best score so far; prune any branch whose optimistic bound < best.

Ω can support both; the score function can be:

* a user-defined scoring closure (extensional)
* an inference-derived heuristic score (intensional)
* or a mixture (critic/triangulate)

---

## 91.6 Inference plane augmentation: heuristic ordering and pruning

This is the “LLM understands the code” counterpart to nondeterminism.

### 91.6.1 Heuristic scoring goal

When `amb.op` happens, before pushing choice points, the handler may call:

```lisp
(int (quote (record "amb-branch" ...)) :policy policy:amb-heuristic)
```

But in implementation terms, the handler can construct a structured request:

* current constraints (all `require` predicates encountered so far)
* current partial assignments (bindings)
* candidate choice values
* objective: maximize probability of satisfiable branch, minimize expected cost

The engine returns a `Meaning` containing:

* `paths`: predicted satisfiable probability per choice
* `cost`: predicted remaining search depth
* `confidence`: calibration
* `obligation`: optional “probe these constraints” requests

The handler uses this as a heuristic function:

[
score(choice) = \alpha \cdot P(\text{success} \mid choice) - \beta \cdot \mathbb{E}[\text{cost} \mid choice]
]

This is not “magic”—it is an explicit policy.

### 91.6.2 Critic pruning

A critic stage tries to falsify a branch cheaply:

* detect contradiction among accumulated `require` predicates (symbolic simplification)
* run a cheap extensional probe (evaluate predicate with partial assignment if possible)
* ask inference to generate a counterexample quickly

Branches predicted contradictory are pruned (not explored), but pruning must be **explainable**:

* ledger event: “pruned branch X because predicate set inconsistent”
* optional evidence: “counterexample input” or “proof sketch” if strict profile

This is **Abstract Interpretation** + **Heuristic Search** + **Auditability**.

---

## 91.7 RL-ready: Episode emission for policy learning

Every nondet run can emit:

* the sequence of choices
* pruned branches and reasons
* cost metrics (steps, tokens, tool calls)
* terminal outcome (success/failure)
* reward function

Reward examples:

* `+1` if a valid solution found
* `-1` if no solution
* subtract `λ * steps` and `μ * toolCalls`

These episodes become datasets for `train/policy`, improving:

* branch ordering
* pruning aggressiveness
* early stopping thresholds
* whether to call inference vs run more extensional checks

This is a closed loop: the language improves its own evaluator strategy.

---

# 92. `syntax-rules` Hygienic Macro Expander: Full Algorithm + Phase Separation + Hermetic Receipts

Now we implement the other big pillar: SICP’s language-making, but industrialized.

## 92.1 The two environments: runtime and macro-time

Ω must maintain:

* `Env0` (phase 0): runtime bindings (`define`)
* `Env1` (phase 1): macro bindings (`define-syntax`)

Macro expansion runs in phase 1 and produces phase 0 code.

This is a **two-level interpreter tower**.

### 92.1.1 Macro transformer values

A transformer is a phase‑1 value:

* either `syntax-rules` transformer (declarative)
* or procedural transformer (like `syntax-case`), optional

For this part, we implement `syntax-rules`.

---

## 92.2 Syntax objects with scope sets (hygiene)

Identifiers carry scope sets. A macro expansion introduces scopes in three roles:

1. **use-site scope**: marks the input syntax at the macro call site
2. **introducer scope**: marks identifiers produced by the macro
3. **binding scope**: marks the binding site identifiers in expanded output

### 92.2.1 Why this works (binding hygiene)

* identifiers originating from the macro’s template get introducer scopes, so they don’t accidentally capture user bindings
* identifiers originating from user input keep use-site scopes and resolve to user bindings
* when a macro introduces a binding (e.g., expanding `let` to `lambda`), binding identifiers get a new binding scope that only matches references created in the corresponding expansion template

This is essentially “lexical scoping for syntax.”

---

## 92.3 `syntax-rules` compilation: patterns and templates

A `syntax-rules` macro consists of:

* literal identifiers list (keywords)
* a sequence of rules: `(pattern template)`

### 92.3.1 Pattern language (core)

Support:

* identifiers (pattern variables)
* literals (keywords)
* lists
* ellipses `...` for repetition
* wildcards `_`

We compile patterns into a pattern IR:

```ts
type Pat =
  | { tag: "PVar"; name: string }            // pattern var
  | { tag: "PLit"; ident: string }           // literal keyword
  | { tag: "PWild" }
  | { tag: "PList"; items: Pat[]; ellipsis?: boolean } // handle `...` at list level
  | { tag: "PAtom"; atom: Atom };
```

### 92.3.2 Matching algorithm (deterministic)

`match(pat, stx) -> Subst | null`

Where substitution `Subst` maps pattern variables to syntax fragments, with support for repeated bindings under ellipses.

Implementation constraints:

* ellipsis introduces vector-of-bindings semantics
* nested ellipses require “ranked” substitutions (a classic subtlety)

For reference correctness, implement ellipsis binding as a tree:

```ts
type SubstVal =
  | { tag: "One"; stx: Syntax }
  | { tag: "Many"; items: SubstVal[] }; // nesting corresponds to ellipsis depth
type Subst = Map<string, SubstVal>;
```

### 92.3.3 Template expansion

Given a template Syntax and substitution Subst:

* replace pattern variables with their bound syntax
* replicate template segments under ellipses according to `Many` structure
* apply introducer scope to introduced identifiers
* preserve source locations if desired (useful for debugging)

---

## 92.4 Macro expansion algorithm (deterministic, hygienic)

Pseudo-algorithm:

1. If expression is not a list, return it (after recursively expanding subforms as appropriate).
2. If list head is an identifier:

   * resolve it in `Env1` (macro env) using scope-aware resolution
   * if it is a transformer:

     * apply transformer to the *original* syntax object
     * get output syntax
     * add introducer scope to introduced ids
     * recursively expand output (macro chaining)
3. Otherwise recursively expand list elements, then lower special forms.

Hygiene is preserved because:

* resolution is scope-based
* transformers handle syntax objects not raw datums
* introducer scopes isolate introduced bindings

---

## 92.5 Lowering special forms after macro expansion

After macro expansion, a second pass lowers surface constructs into core AST (as in Parts 6–7):

* `int/infer/rewrite` → `(effect "infer.op" (record ...))`
* `amb/require/cut` → effects
* `handle` → runtime handler objects or a core `Handle` node
* `match` → core `Match` node

This is **desugaring**—a compilation pass.

---

## 92.6 Hermetic semantic macros: receipts for reproducible expansion

Here is the hard rule that prevents “compiler nondeterminism”:

> Any macro expansion that depends on inference (or tools) must produce a **macro receipt** that pins the result.

### 92.6.1 Macro receipt content

* input syntax hash
* transformer identity (macro name + version)
* engine/policy/prompt versions if inference used
* deterministic envelope parameters (seed, sampling settings)
* output syntax hash
* obligations (if semantic macro)
* evidence hashes

### 92.6.2 Build modes

* `compile-strict`: inference allowed only if receipt emitted and deterministic envelope enforced
* `compile-airgap`: inference only from pinned engines/memory; no network/tools
* `compile-explore`: may allow nondeterminism; artifacts from this mode are not promotable without re-freezing

This imports CI/CD discipline into language semantics.

---

# 93. Canonical Ω Programs: SICP Demos Rebuilt (with Inference Counterparts)

Now the payoff: programs that exercise the subsystems.

## 93.1 Nondeterministic logic puzzle (SICP “multiple dwelling”)

Classic problem: five people in five floors with constraints.

### 93.1.1 Ω code (sketch)

```lisp
(define (distinct? xs)
  ;; simple check
  ...)

(define (multiple-dwelling)
  (with-policy policy:amb:dfs
    (let ((baker    (amb 1 2 3 4 5))
          (cooper   (amb 1 2 3 4 5))
          (fletcher (amb 1 2 3 4 5))
          (miller   (amb 1 2 3 4 5))
          (smith    (amb 1 2 3 4 5)))
      (require (distinct? (list baker cooper fletcher miller smith)))
      (require (not (= baker 5)))
      (require (not (= cooper 1)))
      (require (not (= fletcher 5)))
      (require (not (= fletcher 1)))
      (require (> miller cooper))
      (require (not (= (abs (- smith fletcher)) 1)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (list (list 'baker baker)
            (list 'cooper cooper)
            (list 'fletcher fletcher)
            (list 'miller miller)
            (list 'smith smith)))))

(first-solution (multiple-dwelling))
(all-solutions (multiple-dwelling))
```

### 93.1.2 Inference counterpart

Install a policy that uses inference to order floors and prune:

```lisp
(with-policy policy:amb:heuristic
  (first-solution (multiple-dwelling)))
```

The handler calls `int` on accumulated constraints to score choices; emits episodes to improve branch ordering.

---

## 93.2 Streams: sqrt stream (SICP numerical methods)

### 93.2.1 Extensional code (sketch)

```lisp
(define (average a b) (/ (+ a b) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
      (delay (stream-map (lambda (g) (sqrt-improve g x)) guesses))))
  guesses)
```

### 93.2.2 Inference dual

Ask for:

* fusion opportunities,
* forcing depth,
* productivity risk.

```lisp
(int '(sqrt-stream 2) :policy policy:stream-analysis)
```

Meaning might propose a rewrite that:

* factors the self-reference more cleanly
* or introduces memoization boundaries

Commit is obligation-gated: prefix tests and convergence checks.

---

## 93.3 Generic arithmetic: complex numbers (rectangular vs polar)

SICP canonical.

* register methods for `(add (complex complex))`, etc.
* implement coercions between polar/rect
* then show missing method synthesis:

```lisp
(apply-generic 'mul z1 z2)    ; fails if missing
(infer {goal "define-method" op 'mul types '(complex complex) ...})
(commit/method ...)
```

In strict profile: obligations are algebraic laws + regression suite.

---

## 93.4 Propagators: Celsius/Fahrenheit constraint network

Build a small constraint network:

[
9C = 5(F - 32)
]

Then:

* extensional propagation sets values
* intensional diagnosis explains contradictions and suggests repairs

```lisp
(diagnose net)
(repair net :objective {satisfy minimize-changes})
```

Obligation: randomized propagation checks and invariant validation.

---

## 93.5 Compiler pass: constant folding as semantic macro

Define a semantic macro `const-fold` that:

* rewrites `(+ 1 2)` → `3`
* rewrites nested arithmetic trees
* attaches obligation: eq-ext on sampled tests + metamorphic invariants (commutativity, etc., where applicable)

Then compile module under `compile-strict` profile:

* macro receipt freezes rewrite
* promotion checks regression suite

This is SICP’s compiler chapter meets Fowler’s refactoring discipline.

---

# 94. Integration Points: Where the Two Pillars Meet

This is the subtle “take it to the limit” insight:

* nondeterminism (`amb`) is a *control regime*
* macros (`syntax-rules`) are *language-making*
* inference is a *semantic plane* that can help both

### 94.1 Inference-guided nondet + compile-time partial evaluation

A semantic macro can rewrite an `amb` problem by:

* extracting constraints
* reordering choices
* introducing early `require` filters
* factoring common subexpressions
* and emitting obligations: “solution set preserved” (which is itself a quantified claim)

In strict mode, you approximate obligation discharge by:

* generating random trials and comparing solution sets up to bounded depth
* or using proof tooling if feasible

That is the closest you get to “compile-time solver optimization” while remaining honest about undecidability.

### 94.2 Macro-time inference is allowed, but only with receipts

This is the hermeticity rule again: *compile-time inference is fine if pinned*.

This gives you “in-context learning as define augmentation” in a disciplined form:

* macro-time engines and policies are versioned artifacts
* expansions are reproducible because receipts pin them
* prompt/policy evolution is done via promotion pipelines

---

# 95. What Part 11 will be

If you reply **continue**, Part 11 will be the “deep hardening” pass:

1. A fully specified `amb` handler suite:

   * DFS/BFS/beam/best-first
   * branch-and-bound
   * cut semantics
   * distributional sampling interpretation (`Dist`)
   * episodic telemetry + RL integration
   * critic pruning and disagreement logging

2. A complete `syntax-rules` implementation plan with:

   * nested ellipses correctness
   * scope-set resolution details (ambiguity resolution rules)
   * module system interactions (require/provide at phase levels)
   * macro expansion receipts integrated with artifact registry and promotion
   * conformance test suite (the macro hygiene “torture tests”)

3. A “self-hosting milestone plan”:

   * implementing the expander in Ω itself
   * implementing a subset compiler in Ω
   * differential testing harness against the host interpreter
   * a minimal standard library sufficient for bootstrapping

Reply **continue**.
