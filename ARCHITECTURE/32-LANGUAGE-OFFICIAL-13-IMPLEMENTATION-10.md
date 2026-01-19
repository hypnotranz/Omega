## Ω Language Specification v1.0

### Part 22 — Full Buildable System: Phase‑Tower Module Compiler Producing `initByPhase[p]`; Procedural Macro ABI + Errors + SrcLoc + Macro Stepper Events; `syntax-quasiquote` + `with-syntax` in Ω; Self‑Hosted Stdlib (`cond/and/or/let*/match`); and the Training Plane (`learn.op`) as Policy‑Artifact Production With Receipts + Obligations

This part finishes the “system builder” layer. Up to Part 21, you had the **conceptual** tower closure, strict builds, and promotion. Here we make it operationally complete:

* A concrete **module compiler** that emits *phase-indexed init programs* `initByPhase[p]` (at least p=0 and p=1, with a clear generalization to higher phases).
* A precise **procedural macro ABI** (call protocol, context, error signaling, metadata).
* A **macro stepper** event stream (instrumentation) that is purely derived from expansion and receipts (Event Sourcing pattern).
* A self-hostable **macro toolkit** (`syntax-case` mini) with `syntax-quasiquote` and `with-syntax` implemented in Ω itself.
* A self-hosted **stdlib macro layer** including `match`.
* A full **training plane** (`learn.op`) that yields *policy artifacts* rather than mutating “the model,” with the same receipt/obligation discipline as inference and commits.

I’ll keep this “reference-grade”: explicit invariants, payload schemas, and the minimal kernel you must implement in the host vs what lives in Ω.

---

# 208. Syntax Objects Gain Source Locations and Properties

Procedural macros, error reporting, and macro stepping become dramatically more usable once syntax carries **srcloc** and a small **property map**. This is not cosmetic; it’s how you build a real compiler front-end.

## 208.1 Extend Syntax representation

### Normative structure (language-level)

A `Syntax` value is:

* **shape**: Atom / Ident / List
* **lexical scope set**: `scopes : Scope[]`
* **source location**: optional `loc : SrcLoc`
* **properties**: optional `props : Map<string, Val>` (for expansion marks, stepper tags, inferred meaning pointers, etc.)

### TS patch (conceptual)

```ts
// src/core/syntax/syntax.ts (PATCH)
export type SrcLoc = {
  file?: string;
  line?: number;
  col?: number;
  span?: number;
};

export type Syntax =
  | { tag: "Atom"; value: any; scopes: string[]; loc?: SrcLoc; props?: Record<string, any> }
  | { tag: "Ident"; name: string; scopes: string[]; loc?: SrcLoc; props?: Record<string, any> }
  | { tag: "List"; items: Syntax[]; scopes: string[]; loc?: SrcLoc; props?: Record<string, any> };
```

## 208.2 Reader adds `loc`

Your reader already tokenizes; you now track (line,col,offset) per token and attach:

* `Ident.loc` for symbols
* `Atom.loc` for literals
* `List.loc` for open-paren position

**Design pattern**: **Builder** for syntax nodes; your tokenizer produces tokens with coordinates; parser builds AST nodes with loc.

---

# 209. Procedural Macro ABI: Transformers Are Protocol Objects, Not Just Functions

To keep the expander self-hostable, transformers must have a **uniform call protocol** and a **uniform result type**.

## 209.1 Transformer kinds (compile-time binding payload)

A transformer binding value is one of:

* **SyntaxRules**: data transformer
* **Proc**: closure transformer (Ω function evaluated at transformer phase)
* **SemanticProc**: procedural macro that uses inference (still a Proc; semantics are governed by receipts and obligations)
* **Compiled**: optional future optimization (a precompiled decision DAG for patterns)

### Canonical “TransformerVal” (serializable)

```ts
export type TransformerVal =
  | { tag: "SyntaxRules"; payload: any /* SRTransformer JSON */ }
  | { tag: "ProcRef"; originModule: string; phase: number; internal: string }
  | { tag: "SemanticProcRef"; originModule: string; phase: number; internal: string; policyDigest: string };
```

**Why `ProcRef`?** Closures are not serialized. The module’s `initByPhase[phase]` program reconstructs the closure upon instantiation. `ProcRef` is a stable *pointer* into that instance (Interpreter + Repository + Indirection).

---

## 209.2 Macro invocation signature

A procedural transformer is invoked with:

* `stx : SyntaxVal` — the whole use-site form
* `ctx : MacroCtxVal` — a structured context object
* returns `ExpansionResultVal`

### MacroCtx (Ω value)

`MacroCtx` must include at least:

* current module id, current phase
* the **compile-time env Γ** (or an opaque reference plus query functions)
* a scope allocator (fresh scope) — but in Ω, we pass explicit gensym state or provide it via an effect
* receipt / strictness mode flags
* policy pointer for inference (optional)

**Minimal MacroCtx** (baseline):

```scheme
(map
  ("moduleId" "B")
  ("phase" 0)
  ("mode" "strict" | "explore" | "replay")
  ("fresh-scope" <closure or proc-ref>)
  ("lookup-transformer" <closure>)
  ("raise-syntax-error" <closure>)
  ("record-event" <closure>)   ;; macro stepper
  ("record-receipt" <closure>) ;; receipts
)
```

This is a **Context Object** pattern (Fowler): a bundle of services for a subsystem that would otherwise require global variables.

### Transformer call protocol

```scheme
(transformer stx ctx)  ; -> ExpansionResult
```

### ExpansionResult sum type

Procedural macros should not “throw strings.” They return a structured result:

```scheme
(map
  ("tag" "ok")
  ("syntax" <Syntax> )
  ("receipts" (vector ...))
  ("obligations" (vector ...))
  ("events" (vector ...)))
```

or

```scheme
(map
  ("tag" "error")
  ("error" <SyntaxError>))
```

---

## 209.3 Error protocol: `raise-syntax-error` as an effect boundary

### SyntaxError schema

```scheme
(map
  ("tag" "SyntaxError")
  ("message" "...")
  ("who" "macro-name" | "expander" | ...)
  ("blame" <Syntax>)
  ("notes" (vector (map ("message" "...") ("stx" <Syntax>)) ...)))
```

### Operational semantics

A procedural transformer can:

* return `{tag:"error", error: ...}`
* or call `(raise-syntax-error ...)` which triggers an effect `syntax.error` caught by the compilation driver and reified.

**Why effect?** Because it preserves structured failure while allowing:

* macro stepping to record the failure event,
* the compiler to attach extra context (module stack, phase stack),
* and strict builds to convert it into a stable, hashable failure report.

This is **Exception as Effect** (algebraic effect style).

---

# 210. Macro Stepper: Expansion Events as an Event‑Sourced Trace

You want the equivalent of Racket’s macro stepper, but *built from the same semantics* (not ad-hoc debugging prints).

## 210.1 Expansion event schema

Events are pure data:

* `ExpandStart`: before expanding a form
* `ExpandMacro`: when a macro transformer is invoked
* `ExpandMacroResult`: output syntax
* `ExpandRuleMatch`: syntax-rules rule index chosen + match summary
* `ExpandEnd`: after expansion of a form
* `ExpandError`: error data

Example event record:

```scheme
(map
  ("tag" "ExpandMacro")
  ("module" "B")
  ("phase" 0)
  ("transformerBid" "bid::A::17")
  ("useStxHash" "H(...)")
  ("timeMs" 2))
```

## 210.2 Event storage vs receipts

Receipts are for reproducibility; events are for explainability.

Strict mode options:

* store **minimal events**: digests only
* store **full step trace**: expensive but excellent for debugging

This is a **Memento** pattern choice: receipts are minimal mementos, events are rich event sourcing.

---

# 211. The Module Compiler Emits `initByPhase[p]` Programs (p=0 and p=1 Baseline)

This is the core deliverable: concrete compilation output that enables both:

* runtime evaluation (phase 0)
* compile-time instantiation and procedural macro construction (phase 1)

## 211.1 Partition module content by phase

Given module body forms, classify:

### Phase‑agnostic directives (compile-only)

* `require`, `provide`, `require-for-syntax`

### Phase‑1 “compile-time body”

* `begin-for-syntax` forms (expand/eval at phase 1)
* procedural `define-syntax` RHS evaluation happens at phase 1
* syntax-rules `define-syntax` can be compiled to phase-1 installs without eval, but unifying them as “phase-1 init code” is simpler.

### Phase‑0 “runtime body”

* `define` (value defs)
* expressions (top-level effects)
* **expanded** macro uses become core runtime forms by the time they reach lowerer

## 211.2 Output programs

A compiled module artifact contains:

* `initByPhase[1] : Expr` — installs compile-time values and transformer values
* `initByPhase[0] : Expr` — runtime init: defines + top-level expressions

**Invariants**:

* `initByPhase[1]` must be executable with no dependence on phase 0 runtime state (except via `require-for-syntax`, which imports phase 0 values of dependencies into phase 1).
* `initByPhase[0]` must assume all compile-time expansion is already done (i.e., it is core code).

---

## 211.3 How to compile `define-syntax` into phase‑1 init

### Syntax‑rules case

`(define-syntax m (syntax-rules ...))`

Compile into a phase‑1 init expression roughly equivalent to:

1. construct `SRTransformer` data value (as a `Map` / `Vector` structure)
2. bind it in the phase‑1 runtime instance under internal name `m$bid::M::k`
3. also register it in Γ as a **syntax binding** at phase 1 (compile-time env), so later expansions in the module body see it

The key subtlety: once you have a phase‑1 runtime instance, you can store the transformer value there as a runtime value. Then macro lookup resolves to a `ProcRef` or to an in-memory transformer value. For persistence, you store either:

* the JSON payload in the artifact (`SyntaxRules`), or
* an internal binding name in phase‑1 init (`ProcRef`).

### Procedural case

`(define-syntax m <expr>)` where `<expr>` evaluates to a transformer procedure.

Compile into phase‑1 init:

* evaluate `<expr>` at phase 1 to a `Closure` value
* store it in the phase‑1 instance environment under internal name `m$bid...`
* register syntax binding that points to `{tag:"ProcRef", originModule:M, phase:1, internal:"m$bid..."}`

This is the **Abstract Factory** pattern:

* phase‑1 init is a factory program that *constructs* transformer procedures at instantiation time.

---

## 211.4 Compile `begin-for-syntax` into phase‑1 init blocks

`(begin-for-syntax f1 f2 ...)` becomes:

* expand each `fi` at phase 1 (macro lookup at phase 2 if you later add it)
* lower each expanded `fi` with `lowerSyntax(..., phase=1)`
* include those expressions in `initByPhase[1]` sequentially (as a `Begin`)

In other words, `begin-for-syntax` contributes executable code to phase‑1 init.

---

# 212. Phase Instantiation: How the Compiler “Runs” the Tower

Once you compile modules to `initByPhase`, you can instantiate them by phase to get transformer values and compile-time values.

## 212.1 Instantiation procedure (baseline)

`instantiate(M, phase p)`:

1. instantiate dependencies of M at phase p (via `require`)
2. instantiate dependencies of M at phase p+1 (via `require-for-syntax`)
3. create a fresh CEKS state:

   * env preloaded with phase-appropriate primitives
   * store empty
4. evaluate `M.initByPhase[p]` to completion
5. cache `(M,p)` instance (env + store)

**This is the Interpreter pattern** applied as a staging runtime.

## 212.2 Primitive sets by phase

* **phase 0 runtime**:

  * arithmetic primitives, list/vec/map operations, effects (`effect`, `handle`), nondet support if enabled
* **phase ≥ 1 compile-time**:

  * all phase 0 primitives (optional)
  * syntax API (`syntax->datum`, `datum->syntax`, etc.)
  * macro services (or closures in MacroCtx)
  * inference allowed only if build mode permits and receipts exist

This is **Policy Injection**: the primitive set is selected by build mode and phase.

---

# 213. `syntax-quasiquote` and `with-syntax` Implemented in Ω (Self-Hosted)

These are “macro authoring ergonomics” primitives. Without them, procedural macro writing is painful.

## 213.1 Define a `syntax` constructor macro

Surface syntax:

* `(syntax <datum-shaped-form>)` returns a Syntax object
* inside `syntax`, you can use:

  * `(unsyntax e)` to splice evaluated syntax/datum into the template
  * `(unsyntax-splicing e)` to splice a list of items

This is Racket’s `#'` + quasi-syntax.

### Minimal approach

Implement `syntax-quasiquote` as a procedural transformer that:

1. receives the whole syntax
2. computes a datum template
3. recursively walks it, evaluating `unsyntax` / `unsyntax-splicing` positions at compile-time
4. constructs the result with `(datum->syntax ctx <datum>)` while preserving lexical context of `ctx`

### Core library functions you need (compile-time):

* `syntax->datum`
* `datum->syntax`
* `vector?` / `vector-length` / `vector-ref` (or list equivalents)
* `append` / list constructors (you can write these in Ω)

### Sketch (Ω pseudo, not fully expanded)

```scheme
(define-syntax syntax
  (lambda (stx ctx)
    ;; stx = (syntax template)
    (let* ((parts (syntax-list-items stx))
           (template (vector-ref parts 1)))
      (syntax-quasiquote-expand template ctx))))
```

Where `syntax-quasiquote-expand`:

* if sees `(unsyntax e)`: evaluate `e` at phase 1 (already in transformer), expect Syntax or Datum, and inject
* if sees `(unsyntax-splicing e)`: evaluate `e`, expect vector/list, splice into surrounding list

This is exactly where your inference plane can later help: it can propose rewrite templates, but `syntax-quasiquote` remains deterministic.

---

## 213.2 `with-syntax` macro (pattern-driven substitution)

`with-syntax` binds pattern variables to syntax values and uses `syntax` to build outputs.

Example:

```scheme
(with-syntax ((x #'(+ 1 2)))
  #'(let ((y x)) y))
```

Minimal `with-syntax` can be implemented in terms of:

* a binding map σ
* substitution into a template (like syntax-rules template expansion but with explicit bindings)

This is a **Template Method** pattern: you reuse the same underlying substitution engine you already wrote for syntax-rules, but the binding acquisition is explicit.

---

# 214. Self‑Hosted Stdlib Macros: `cond`, `and`, `or`, `let*`, and a Real `match`

You already wrote `cond/and/or/let*` in syntax-rules form in Part 18. Here we add `match`, because `match` is the canonical “language-making” feature.

## 214.1 `match` as a derived form

We want a macro:

```scheme
(match v
  [pat1 e1]
  [pat2 e2]
  [else eN])
```

That expands to nested conditionals with bindings.

### Minimal pattern language (Phase 1)

Patterns:

* `_` wildcard
* identifier binds
* literal numbers/booleans/strings
* list patterns `(p1 p2 ...)`
* vector patterns `#(p1 p2 ...)` (or `(vector p1 p2 ...)`)
* predicate pattern `(? pred p)` meaning “if pred(v) then bind p to v”

### Expansion strategy

`match` expands into a `let` that binds the scrutinee once, then a chain of tests.

This is a classic **Compiler** pattern: pattern matching compilation to decision trees (you can start linear, then optimize).

### Example expansion (linear)

```scheme
(let ((tmp v))
  (if (match? tmp pat1) (let (bindings...) e1)
      (if (match? tmp pat2) (let (bindings...) e2)
          eN)))
```

Where `match?` and “extract bindings” can be generated inline by the macro, or delegated to a runtime helper function. For self-hosting simplicity, it’s often better to expand into:

* explicit nested `if` with primitive tests and destructuring.

**Key**: hygiene requires `tmp` introduced by macro be protected by introducer scope (which your system does).

---

# 215. The Training Plane: `learn.op` Produces Policy Artifacts, Not Mutable “Weights in Place”

Now we satisfy your earlier instinct: an inference language should be able to “send shit back for training” — but correctly.

The mistake would be treating training like `define` (mutating semantics). The right design is:

> Training is an effect that produces a **policy artifact** (immutable, content-addressed).
> Selection/promotion of a policy is a separate controlled operation.

This mirrors how you treat compilation artifacts and expander promotion. It is the same governance pattern.

## 215.1 `learn` as an effect

Surface derived form:

```scheme
(learn req)  =>  (effect learn.op req)
```

## 215.2 LearnRequest schema (normative)

A `LearnRequest` must be explicit and serializable:

* `basePolicyDigest` (what you are updating)
* `objective` (loss/reward spec)
* `dataRefs` (CAS hashes for datasets / logs / traces)
* `evaluation` (metrics + test harness refs)
* `constraints` (safety constraints, licensing, data provenance)
* `envelope` (budget, seeds, parallelism, determinism knobs)
* `artifactTargets` (where to write policy artifacts)

Example:

```scheme
(map
  ("kind" "prompt-tuning" | "rlhf" | "distill" | "finetune")
  ("basePolicy" "policy:abcd...")
  ("dataRefs" (vector "cas:..." "cas:..."))
  ("objective" (map ("tag" "reward-model") ("metric" "pass@k") ...))
  ("constraints" (vector ...))
  ("envelope" (map ("seed" 123) ("budgetMs" 600000) ("maxSteps" 1000) ...))
  ("evidence" (vector ...)))
```

## 215.3 LearnResult = PolicyArtifact (normative)

The result is an immutable artifact descriptor:

* `policyDigest` (content-addressable id)
* `kind` (prompt update vs weights reference)
* `payloadRef` (hash pointer; e.g., prompt text hash or weight blob hash)
* `metrics` (eval summary)
* `obligations` (must pass regression suite, safety suite, etc.)
* `provenance` (dataRefs used, base policy digest)
* `transcriptHash` (training log digest)

This is exactly the “artifact-based” discipline you were circling.

## 215.4 Receipts and strictness

* **strict build**: `learn.op` forbidden (training is not reproducible in the same sense); only previously produced policy artifacts may be selected.
* **explore mode**: `learn.op` allowed; it yields candidate policy artifacts with obligations.
* **replay**: training forbidden; only selection of already-digested policy artifacts allowed.

This prevents the build from becoming a stochastic moving target.

## 215.5 Policy selection (separate, promotable)

Add an effect:

```scheme
(select-policy digest) => (effect policy.select.op digest)
```

This updates the dynamic policy used by `infer` and `semantic macros` *within a controlled evaluator session*, not by mutating the language definition.

Promotion to trusted is again a gate:

```scheme
(effect policy.promote.op digest)
```

which requires discharged obligations.

**Enterprise Integration Patterns note**: this is literally **Command** + **Approval** + **Audit Log**; the policy store is a **Repository**; promotion is a **Process Manager**.

---

# 216. End‑to‑End Story: Compile-Time Semantic Macro + Strict Replay + Promotion

Here’s the complete narrative flow that makes your original “LLM understands code and can evaluate it” a first-class feature without sacrificing formalism:

1. Module `Semantics` exports a procedural macro `auto-when` implemented with `syntax-case` + `syntax-quasiquote`.
2. `auto-when` calls `(infer req)` to propose a rewrite.
3. The macro validates the proposal locally:

   * shape check: rewrite must be an `if`
   * hygiene check: introduced identifiers are allowed only from a whitelist (or must carry introducer scopes)
   * optional: generate obligations (unit tests / equivalence checks)
4. In explore mode:

   * compilation stores `MacroReceipt` keyed by request hash + policy digest + envelope
   * artifact is candidate
5. In strict replay:

   * the same compilation produces the same expanded/core hashes using receipts only (no oracle calls)
6. Promotion:

   * obligations are discharged (tests run, equivalence checked)
   * module artifact becomes trusted
   * registry pointer may be updated if this is an expander/policy upgrade

This is not “an API call.” It’s a **semantics-carrying effect** integrated into:

* the expander,
* the evaluator,
* the artifact store,
* the promotion pipeline.

---

# 217. What You Have Now (Why This Is “All the Way”)

At this point Ω has the full SICP-style closure property:

* **Kernel**: CEKS + quote + effects + syntax values + module instantiation
* **Derived language**: almost everything else is a macro or library (cond, match, let*, when, unless, syntax-quasiquote)
* **Meta-language**: the expander is an Ω program, and can be promoted via differential proof
* **Inference plane**: inference is an effect, receipted, replayable, obligation-bearing
* **Training plane**: training produces policy artifacts with the same governance discipline

This is precisely “eval/apply + representation as data” expanded to “infer/verify + artifacts as data.”

---

## Continue

If you reply **continue**, Part 23 will go even more “limit” in two directions:

1. A formal **small-step semantics** for the expander + phase tower (judgments, preservation of hygiene invariants, and determinism conditions under strict mode).
2. A concrete **optimization suite** for the “LLM context as data structure” idea you raised earlier:

   * context capsules and receipts as compression (Memento)
   * incremental compilation caches keyed by expanded/core hash
   * selective replay (only re-expand the dependency slice whose hashes changed)
   * inference memoization with semantic keys (request canonicalization + environment digests)
   * and how nondet search + inference scoring can be made replayable and convergent under budgets
