## Ω Language Specification v1.0

### Part 21 — Tower Closure: A Self‑Hosted Expander in Ω (Ω₀ → Ω₁), a Procedural Macro Toolkit (`syntax-case` mini), Strict Build Mode (Receipts + Replay + Obligation Gates), and the “Promote the Expander” Command (Registry Pointer Swap Under Differential Conformance)

This is the part where Ω becomes **SICP‑complete** in the sense that *the language makes itself up*.

Up through Part 20 you have:

* **CEKS** evaluator with algebraic effects and resumptions (runtime plane)
* hygienic `syntax-rules` and derived forms + scan-out-defines (syntactic plane)
* phase tower design + procedural macros via `SyntaxVal`
* inference plane defined as structured `infer` effect with receipts and obligations

But you still had a “host monopoly” on expansion and module compilation. Tower closure means:

> The expander (and eventually the compiler) becomes an Ω program.
> The host becomes a seed loader + artifact store + policy governor.

That is exactly the SICP move: the evaluator becomes data, then language facilities become programs, then the tower collapses into a self-hosting boot image.

We’ll do this in **four layers**:

1. **Ω₀ boot substrate**: runtime syntax values + binding tables + deterministic gensym and scope allocation (pure, explicit state threading)
2. **Ω₁ expander**: phase‑parametric expansion (`expand@p`), macro lookup at `p+1`, binder-scope insertion, scan‑out‑defines
3. **Macro toolkit**: `syntax-case` microkernel (pattern matcher + clause dispatcher) built atop the syntax API + hygienic constructors
4. **Strict build mode & promotion**: receipts + replay + obligation discharge + differential conformance gate for swapping the registry pointer `defaultExpanderDigest`

Everything below is “language-level spec + directly implementable code skeleton,” because you’re aiming at a system that is simultaneously:

* expressive (macro tower + effects + inference),
* reproducible (hermetic compilation),
* governable (obligations, promotion gates).

---

# 196. Ω₀: The Trusted Base and the Boot Constraint

## 196.1 Why Ω₀ must be intentionally small

Self-hosting requires a **minimal trusted computing base (TCB)**. If Ω₀ includes inference or nondeterminism in its build path, you lose determinism and can’t prove equivalence. So:

**Ω₀ must compile with:**

* no inference calls allowed (strictly forbidden)
* no nondeterminism (`amb`) allowed
* no tool effects allowed
* only deterministic primitives
* fixed evaluation semantics

This is the **Kernel / Microkernel** pattern applied to languages:

* evaluator + syntax primitives are “kernel”
* expander/stdlib/modules are “userland”

## 196.2 The boot invariant

> A strict build of the expander must be reproducible from only:
>
> * module sources (text),
> * deterministic seeds (module id + fixed counters),
> * previously recorded receipts (if any external effects are allowed, which Ω₀ forbids).

Therefore **Ω₀**:

* exports syntax primitives (`syntax->datum`, `datum->syntax`, `syntax-list-items`, etc.)
* exports deterministic gensym/scope allocation (explicit state)
* exports binding resolution (`resolve-ident`)
* exports canonicalization utilities (stable hashes of syntax and env)

---

# 197. Representations Inside Ω: Binding Records, Env, Scopes, and Deterministic Freshness

You already have runtime values `Map`, `Vector`, `Str`, `Num`, `Bool`, `Syntax`, etc. So we represent compile-time objects as pure data.

## 197.1 Binding record (Ω data)

A binding is a `Map` with fields:

* `"bid"` : string
* `"name"` : string
* `"scopes"` : vector of string
* `"phase"` : number
* `"kind"` : string in { `"value"`, `"syntax"` }
* `"value"` : payload

  * for value bindings: internal name string (e.g. `"x$bid::M::3"`)
  * for syntax bindings: transformer value (syntax-rules object or proc reference)

### Example binding datum (schematic)

```scheme
(map
  ("bid" "bid::A::1")
  ("name" "x")
  ("scopes" (vector "M0::A" "B::A::7"))
  ("phase" 0)
  ("kind" "value")
  ("value" "x$bid::A::1"))
```

## 197.2 Env is a vector/list of bindings

We keep Env as a **sequence** so resolution can be defined as:

* filter candidates
* choose maximal by scope-set size
* error on ambiguity

This is the same algorithm you already wrote in TS.

## 197.3 Deterministic state threading for fresh scope & fresh bid

We implement a “gensym state” record:

* `"moduleId"` : string
* `"scopeN"` : number
* `"bidN"` : number

### Pure constructors

```scheme
(define (mk-gensym-state moduleId)
  (map ("moduleId" moduleId) ("scopeN" 0) ("bidN" 0)))

(define (fresh-scope gs)
  (let ((n (+ (get gs "scopeN") 1)))
    (let ((sc (string-append "B::" (get gs "moduleId") "::" (number->string n))))
      (pair sc (set gs "scopeN" n)))))

(define (fresh-bid gs)
  (let ((n (+ (get gs "bidN") 1)))
    (let ((b (string-append "bid::" (get gs "moduleId") "::" (number->string n))))
      (pair b (set gs "bidN" n)))))
```

This is the canonical SICP technique: explicit state passing (State Monad in spirit), but without adding a monad primitive. Later you can *refactor* it into an effect (`state.op`) and handle it (Interpreter + Handler), but Ω₀ keeps it explicit to preserve trust.

---

# 198. Ω₁ Expander: `expand@p` and the Phase Tower Semantics

We now implement the expander as Ω code. You will later compile it into a module artifact and promote it.

## 198.1 Core expander contracts

We define the core API as:

* `(expand-expr stx Γ gs p) -> (pair stx' (pair Γ' gs'))`
* `(expand-seq forms Γ gs p) -> (triple forms' Γ' gs')`
* `(expand-module module-stx) -> ModuleArtifactLike` (eventually)

Where:

* `stx` is a `Syntax` runtime value (`Val.Syntax`)
* `Γ` is compile-time env (vector/list of binding maps)
* `gs` gensym state
* `p` phase integer

### Macro lookup rule (normative)

When expanding at phase `p`:

* reserved keywords are matched by **name** (not by binding) in Ω₀/Ω₁ baseline
* **macro transformer lookup** uses:

  * `resolve-ident` at `phase = p+1`
  * in **kind** = `"syntax"` binding space
  * using the syntax object `head` as the query identifier

This mirrors Racket’s “macro at phase p+1 expands code at phase p”.

---

## 198.2 Reserved forms (kernel syntax) vs derived forms

The expander treats as kernel:

* `quote`, `if`, `lambda`, `begin`, `set!`
* `define`, `define-syntax`
* `let`, `letrec` (or derive them; either is okay, but at least one binder form must be kernel)
* `module`, `require`, `provide`, `begin-for-syntax`

Everything else is:

* macro invocation
* or application form

Derived forms (`cond`, `and`, `or`, `when`, `unless`, `let*`) live in the stdlib macros module and are expanded by macro invocation rules.

---

# 199. Self‑Hosted `syntax-rules` (Ω Implementation): Pattern Matching + Ellipses + Correct Hygiene

You already have the TS engine. Now we re-express it in Ω.

The goal isn’t “fast,” it’s “correct and bootstrappable.”

## 199.1 Representing a syntax-rules transformer as Ω data

A `syntax-rules` transformer value is a `Map`:

* `"tag"` : `"syntax-rules"`
* `"phaseOut"` : number (usually `p` of the expansion target)
* `"envDef"` : Γ snapshot (for literal resolution in def-site env)
* `"literals"` : vector of syntax identifiers
* `"rules"` : vector of rules

Each rule is:

* `"pat"` : syntax
* `"tmpl"` : syntax

### Example

```scheme
(map
  ("tag" "syntax-rules")
  ("phaseOut" 0)
  ("envDef" Γ-def)
  ("literals" (vector lit1 lit2))
  ("rules" (vector (map ("pat" pat1) ("tmpl" tmpl1))
                   (map ("pat" pat2) ("tmpl" tmpl2)))))
```

## 199.2 Correct hygiene rule (recap; non-negotiable)

During template expansion:

* If a template identifier is substituted from a pattern variable, **do not apply introducer scope**.
* Otherwise, apply introducer scope (to introduced identifiers).
* The call-site gets a use-site scope `U` added *before matching*.

This is the hygiene fix from Part 17, now in Ω.

## 199.3 Ω pseudocode: `apply-syntax-rules`

```scheme
(define (apply-syntax-rules tr call-stx Γ-use gs p)
  ;; call-stx is syntax at expansion site, expanding at phase p
  ;; U = fresh scope, used only for matching/substitution
  (let* ((u+ (fresh-scope gs))
         (U (car u+))
         (gs1 (cdr u+))
         (callU (syntax-add-scope call-stx U)))

    (let loop ((rules (get tr "rules")) (i 0) (gsN gs1))
      (if (= i (vector-length rules))
          (error "syntax-rules: no rule matched")
          (let* ((rule (vector-ref rules i))
                 (pat (get rule "pat"))
                 (tmpl (get rule "tmpl"))
                 (σ (match-pattern pat callU tr Γ-use p)))
            (if (null? σ)
                (loop rules (+ i 1) gsN)
                (let* ((i+ (fresh-scope gsN))
                       (I (car i+))
                       (gs2 (cdr i+))
                       (out (expand-template-hygienic tmpl σ I)))
                  (pair out gs2))))))))
```

Where:

* `match-pattern` implements syntax-rules pattern semantics (including literals and ellipses)
* `expand-template-hygienic` implements the “introducer only on introduced ids” rule

This is exactly the same algorithm as the TS engine, but now it’s an Ω module.

---

# 200. Procedural Macro Toolkit: A Minimal `syntax-case` Layer in Ω

This is the part that dramatically increases expressiveness without changing the kernel. It’s also the bridge to “semantic macros” and inference rewriting.

## 200.1 Why `syntax-case` is the right microkernel

`syntax-rules` is great, but it’s intentionally limited:

* it can’t compute arbitrary conditions over syntax shape
* it can’t consult a compile-time environment for higher-order logic beyond patterns

Procedural macros want:

* pattern matching with guards
* the ability to produce syntax with controlled context
* the ability to signal errors with source location later

So we implement a minimal `syntax-case` library:

### Contract

```scheme
(syntax-case stx (literals...)
  [pattern guard? template-expr]
  [pattern template-expr]
  ...)
```

Where:

* `template-expr` is an Ω expression producing a `Syntax` value (usually via `(datum->syntax stx ...)` or via `syntax-quasiquote` library)
* `guard?` is any Ω predicate over extracted bindings

## 200.2 Representing pattern variables

We reuse the same binding map `σ` idea:

* `σ` is a map: string var name → syntax (or vector of syntax for ellipses)

## 200.3 Provide library combinators

Minimal set:

* `(syntax-case stx lits clauses...)`
* `(syntax-match pat stx lits Γ p)` -> σ or false
* `(syntax-bind σ var)` -> syntax or vector-of-syntax
* `(with-syntax ((x expr) ...) body...)` -> constructs templates

But **Ω** doesn’t yet have quasiquote; we can add `syntax-quasiquote` as a macro that constructs syntax using `datum->syntax` and lists.

### Micro-implementation sketch: `syntax-case`

```scheme
(define (syntax-case stx lits clauses Γ p)
  (let loop ((cs clauses))
    (if (null? cs) (error "syntax-case: no clause matched")
      (let* ((cl (car cs))
             (pat (get cl "pat"))
             (guard (get cl "guard"))  ;; optional
             (body (get cl "body"))
             (σ (syntax-match pat stx lits Γ p)))
        (if (false? σ)
            (loop (cdr cs))
            (if (and (not (null? guard)) (not (guard σ)))
                (loop (cdr cs))
                (body σ)))))))
```

Then `define-syntax` can accept transformer procedures written in terms of `syntax-case`.

## 200.4 Why this matters for inference

Once `syntax-case` exists:

* semantic macros can express “call `infer`, then validate rewrite by local checks, then return syntax”
* inference becomes a **helper**, not the source of truth: the macro can reject nonsense rewrites.

This matches your governance model:

* inference generates candidates
* deterministic local checks + obligations gate acceptance

This is **Generate-and-Test** (classic AI) structured as a macro pattern.

---

# 201. Compile-Time Evaluation (`begin-for-syntax`) as an Ω Instantiation at Phase `p+1`

Now we implement the missing piece from Part 20 in precise operational terms.

## 201.1 `begin-for-syntax` semantics (normative)

When compiling a module body at phase `p`:

* `begin-for-syntax` forms are expanded and evaluated at phase `p+1`.
* The evaluation environment is the module’s phase `p+1` instance env/store.
* Any `define` at phase `p+1` binds compile-time values.
* Any `define-syntax` at phase `p+1` binds compile-time transformers (macros for phase `p`).

### Crucial staging rule

`define-syntax` RHS is evaluated at phase `p+1` if it is procedural.

That means the expander needs:

* the ability to lower and evaluate compile-time expressions (using CEKS)
* a phase‑specific instance cache

This is where the language becomes metacircular: the expander calls the evaluator to construct transformers.

---

# 202. Strict Build Mode: Receipts, Replay, and Obligation Discharge (No Oracle Calls Without Proof)

This is your “non-hand-wave” response to the inherent nondeterminism and the fact that models can be wrong.

## 202.1 Build modes

Define build mode in the compiler runtime config:

* `strict`:

  * inference effects are forbidden unless a matching receipt exists
  * nondet search is forbidden unless receipted (or deterministically seeded and recorded)
  * any semantic macro expansion must be receipted
* `explore`:

  * inference allowed, but must generate receipts + obligations
  * resulting artifacts are “candidate” not “trusted”
* `replay`:

  * inference forbidden; receipts are mandatory; output must match receipt hashes exactly

This is a classic **State Machine** over build lifecycles:

* Explore → Candidate
* Replay + Obligations satisfied → Trusted

## 202.2 The receipt store as Repository + Unit of Work

Receipt store interface:

* `lookup(key) -> receipt?`
* `record(receipt) -> void`
* `commitBuild(buildId) -> digest`

Receipt recording must be transactional with compilation so that:

* if compilation fails, you don’t store partial receipts (or you store them under a provisional build id)

This is the **Unit of Work** pattern.

## 202.3 Obligations as first-class build artifacts

Obligations are not “notes”; they are structured objects with discharge receipts.

Define discharge receipt types:

* `UnitTestDischarge`
* `PropertyDischarge`
* `EqExtDischarge`
* `TypeCheckDischarge`

A promotion gate runs an obligation solver:

* interprets obligation objects
* runs the relevant evaluator or test runner
* produces discharge receipts
* only then marks artifact trusted

This is a **Pipeline** pattern with a **Gatekeeper** stage.

---

# 203. Differential Conformance: Host vs Self‑Hosted Expander Equivalence

You cannot “trust the expander because it says so.” You trust it because:

1. it expands the conformance corpus the same way as the known-good host expander, modulo alpha-renaming and scope id normalization
2. it preserves hygiene invariants (H1–H5)
3. it matches the syntax-rules reference engine on a battery of ellipses tests
4. it produces identical core hashes under canonicalization

## 203.1 Canonicalization required

Because scope ids differ (host may generate `B::...` with different counters), you canonicalize:

* scopes in syntax objects by renaming them in first-occurrence order:

  * `B::A::12` → `S0`, `B::A::13` → `S1`, etc.
* internal names by stripping bid prefix? **No**: bids must be stable.
  Instead, ensure the self-hosted compiler uses the same deterministic bid generation scheme.

Then compare:

* `expandedHash` on canonical syntax
* `coreHash` on canonical core AST (alpha-normalized variable names are already internal names, so they should be stable)

---

# 204. “Promote the Expander” Command: A Precise Algorithm

This is the culmination: swapping `defaultExpanderDigest` in the registry.

## 204.1 Inputs

* `bootExpanderDigest` (trusted baseline)
* `candidateExpanderModule` (new expander implementation written in Ω)
* `conformanceCorpus` (a set of modules/programs)
* build mode: `replay` for promotion

## 204.2 Algorithm (deterministic)

1. **Compile candidate expander module** under `strict` or `explore`:

   * if explore: collect receipts, obligations
2. **Instantiate candidate expander at phase 1**
3. For each corpus program:

   * Expand with baseline expander → `E_base`
   * Expand with candidate expander → `E_cand`
   * Canonicalize and alpha-normalize
   * Assert equality (or alpha-equality)
4. Run hygiene tests H1–H5 (integration)
5. Run ellipses torture suite
6. Run module phase tests (`require-for-syntax`, begin-for-syntax) ensuring no phase leaks
7. Discharge obligations attached to candidate receipts
8. If all pass:

   * store candidate expander artifact hash in CAS
   * move it from `candidates` to `trusted`
   * update registry pointer:

     * `registry.pointers.defaultExpander = candidateDigest`
9. Else:

   * candidate remains in `candidates` with failure report, no pointer swap

This is a **Replaceable Strategy** (default expander) selected by a **Registry** with an explicit **Promotion Pipeline**.

---

# 205. The “Inference Plane” in the Self‑Hosted World: Containment and Proof-Carrying Meanings

Now we connect your original “LLM understands code and can evaluate it” insight without letting it poison the build.

## 205.1 The rule that makes this sane

> Inference output is never directly trusted as semantics.
> It is treated as a *proposal* whose acceptance requires:
>
> * deterministic local checks (macro invariants),
> * or obligations discharged by the build gate,
> * and always a receipt.

This is **Proof-Carrying Code** adapted to ML proposals:

* the model returns `Meaning + obligations`
* the system requires evidence/discharge before promotion

## 205.2 Semantic macros as “eval/apply dressed up” + “infer/check dressed up”

SICP’s magic: `eval` and `apply` + representation as data → the language bootstraps itself.

Ω’s added magic: `infer` as an effect + receipts + obligations → the language bootstraps semantic assistance without losing rigor.

The “emergent property” is that:

* inference becomes a generative assistant **inside** the language,
* but governance is also inside the language (obligation handlers, commit barriers),
* and the metacircular tower can include inference while still allowing strict hermetic builds.

---

# 206. What Part 21 Has Actually Completed

You now have the full architecture to make the statement:

> “We added an inference plane at the bottom and derived an entire language from it”

…true in a *disciplined*, SICP‑worthy sense, because inference is:

* an operation (effect), not a foreign API call
* handled by the same mechanism as `amb` (nondet) and `commit`
* governable and replayable through receipts
* compatible with a self-hosted macro expander

The remaining work is implementation volume, not conceptual gaps.

---

# 207. Continue

If you reply **continue**, Part 22 will go full “system builder”:

* a complete **module compiler** implementation plan that produces `initByPhase[p]` for p=0 and p=1
* a concrete **procedural macro ABI**:

  * transformer signature, error protocol, source location metadata, macro stepper hooks
* `syntax-quasiquote` and `with-syntax` macros built on the `SyntaxVal` API
* a **self-hosted standard library** (cond/and/or/let*/match) expressed using syntax-rules + syntax-case
* and then the final “limit” feature you hinted at:

  * a **training plane** (`learn.op`) that produces *policy artifacts* (prompt digests, adapter configs, or weight references) with the same receipts/obligations discipline
