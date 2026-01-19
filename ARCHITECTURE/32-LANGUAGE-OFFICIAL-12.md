## Ω Language Specification v1.0

### Part 13 — Tightened Hygiene Semantics: Binding Identity, `free-identifier=?` Literals, Binder‑Scope Introduction, Hermetic Semantic‑Macro Receipts with Oracle Transcripts, and the Expander‑in‑Ω Boot Image (Ω₀)

This part eliminates the remaining “hand‑wavy edges” and pins down the places where real macro systems and real search semantics live or die:

1. **Binding identity** is an explicit, stable object (`bid`) rather than an implicit accident of scope sets.
2. `syntax-rules` literal matching uses **binding identity** (`free-identifier=?`) rather than symbol name equality.
3. Binder scope insertion is specified as a deterministic expander algorithm for **each binding form** (λ/let/let*/letrec/define/define-syntax/module), with correct region scoping.
4. Semantic macros integrate inference at expansion time **without non-reproducible builds** via hermetic receipts that include oracle transcript digests and pinned artifacts.
5. The expander is made **self-hostable**: a boot subset **Ω₀** can compile the expander written in Ω itself, then replace the host expander.

Everything here is written in a way that can be implemented *directly*.

---

# 110. Binding Identity as a First-Class Artifact

Scope sets are the mechanism; **binding identity** is the invariant. If you rely on “(name, scopes) happens to be unique,” you will eventually hit ambiguity, redefinition, or cross‑module collisions that can’t be repaired cleanly.

## 110.1 Binding entries carry a `bid`

Upgrade the binding record from Part 12:

```lisp
;; binding entry
;;  bid   : unique id (uuid or content hash)
;;  name  : symbol
;;  scopes: scope-set
;;  phase : integer
;;  kind  : 'value | 'syntax | 'module | ...
;;  value : binding payload (runtime value, transformer, module export ref, etc.)
(record "tag" "bind"
        "bid" bid
        "name" name
        "scopes" scopes
        "phase" phase
        "kind" kind
        "value" value)
```

### Why `bid` matters

* it makes `free-identifier=?` **structural** (compare IDs) rather than “best match and hope”
* it supports *rebinding* (same name, different binder) unambiguously
* it lets you store receipts and proofs against a stable referent

This is the *semantic* analog of pointer identity in compiler symbol tables.

---

# 111. Identifier Equality Predicates (The Hygiene Workhorses)

You need at least these two, in the precise spirit of hygienic macro systems:

## 111.1 `bound-identifier=?` (syntactic identity)

Two identifiers are `bound-identifier=?` if they are the *same syntactic binder*, typically used for “are these binders the same?” and alpha-equivalence checks.

A practical definition in scope-set systems:

* same symbol name
* same scope set (or same canonical scope signature)

```lisp
(define (bound-identifier=? a b)
  (and (ident? a) (ident? b)
       (eq? (get a "name") (get b "name"))
       (equal? (get a "scopes") (get b "scopes"))))
```

This is a strong predicate.

## 111.2 `free-identifier=?` (semantic/binding identity)

Two identifiers are `free-identifier=?` at phase `p` if they resolve to the **same binding** at phase `p` in their respective environments.

Let:

* `resolve-id(id, env, p) -> bind | unbound`

Then:

* if both resolve to `bind` records: compare `bid`
* if both unbound: fallback (typically name equality)
* else: false

```lisp
(define (free-identifier=? id1 env1 p id2 env2)
  (let ((r1 (resolve-id id1 env1 p))
        (r2 (resolve-id id2 env2 p)))
    (cond
      ((and (bind? r1) (bind? r2))
       (eq? (get r1 "bid") (get r2 "bid")))
      ((and (unbound? r1) (unbound? r2))
       (eq? (get id1 "name") (get id2 "name")))
      (else #f))))
```

This is the predicate you use for **literal keyword matching** in `syntax-rules`.

> The critical distinction: `bound-identifier=?` is *syntactic identity*; `free-identifier=?` is *binding identity*.

---

# 112. Tightened `resolve-id` Semantics (Subset Applicability + Maximal Specificity)

We keep the subset-based hygiene selection from Part 12, but we specify it rigorously:

## 112.1 Applicability

A binding `b` applies to identifier use `id` at phase `p` iff:

* `b.phase = p`
* `b.name = id.name`
* `b.scopes ⊆ id.scopes`

## 112.2 Selection

Among applicable bindings, choose the one with maximal `|b.scopes|`.
If there is a tie, it is an **ambiguity error** (expansion-time error in strict mode).

This maximality rule is what gives you lexical shadowing for nested binders (more scopes ⇒ more specific).

---

# 113. `syntax-rules` Literal Matching: Correct `free-identifier=?` Semantics Across Definition vs Use Contexts

This is the single biggest correctness fix to Part 12.

## 113.1 The phase-shift rule for transformers

General macro phase rule:

* a transformer that *runs* at phase `p_run` transforms syntax at phase `p_out = p_run - 1`.

So:

* normal macros run at phase 1, transform phase 0
* “macros-for-syntax” run at phase 2, transform phase 1
* etc.

Therefore literal matching must use `free-identifier=?` at **phase `p_out`**.

## 113.2 Definition environment capture

A `syntax-rules` transformer must capture the *definition environment* relevant to the output phase:

* `env_def_out` = the expander’s phase‑`p_out` environment at the macro definition site
* store it inside the transformer

You also store (or precompute) literal binding keys:

```lisp
(record "tag" "sr"
        "phase-out" p_out
        "env-def-out" env_def_out
        "literals" literals   ;; syntax list
        "litkeys" litkeys     ;; name -> bid or "unbound"
        "rules" rules)
```

### Precompute literal keys (recommended)

For each literal identifier `lit` in the literal list:

* resolve `lit` in `env_def_out` at phase `p_out`
* if bound: store `bid`
* else: store `"unbound"` (fallback to name match)

This yields constant-time literal comparison in the matcher.

## 113.3 Literal comparison rule (normative)

When pattern literal `L` must match input identifier `I` at call site:

* let `kL = litkeys[L.name]`
* resolve `I` in `env_use_out` at phase `p_out`, yielding binding `bI` or unbound
* match holds iff:

  * if `kL` is a `bid` and `bI` is bound: `(bI.bid == kL)`
  * if `kL` is `"unbound"` and `I` is unbound: `(I.name == L.name)`
  * otherwise: fail

This is precisely the `free-identifier=?` behavior adapted to a cached-key matcher.

---

# 114. Binder-Scope Introduction Rules (The Expander’s “Binding Forms” Discipline)

A hygienic expander is not just “macro substitution.” It is a compiler that recognizes binding forms and introduces binder scopes to make resolution well-defined.

We specify binder-scope insertion for:

* `lambda`
* `let`
* `let*`
* `letrec`
* internal `define` (desugared to letrec block)
* `define` at module top-level
* `define-syntax` at module top-level (phase 1 binding)
* `module` (module scope introduction)
* `begin-for-syntax` (phase shift and eval boundary)

## 114.1 Module scope

At module start, create module scopes:

* `M0` for phase 0
* `M1` for phase 1
* generally, `Mp` per phase used in the module

Add `Mp` to all forms that belong to that phase. For a first implementation:

* add `M0` to all module body syntax (phase 0 program)
* add `M1` to macro definitions / begin-for-syntax bodies

**Top-level bindings** (defines, define-syntax) should use module scope as their “ambient binder scope,” not a fresh lexical scope per define.

This makes top-level resolution stable without having to scope-shift the entire rest-of-module per define.

## 114.2 Lexical binder scope for `lambda` and `let`-family

For a binding form introducing lexical variables:

1. Generate a fresh binder scope `B`.

2. Add `B` to **each binder identifier** in the binder list.

3. Extend the appropriate environment with binding entries for these binders (each with a fresh `bid`).

4. Add `B` to the entire **lexical region** where these binders are in scope:

   * for `lambda`: the body
   * for `let`: the body only (not the initializer expressions)
   * for `letrec`: both initializer expressions and body
   * for `let*`: sequentially nest scopes

5. Recursively expand region forms in the extended environment.

### 114.2.1 Why the region matters (let vs letrec)

This is not cosmetic: it is the difference between non-recursive and recursive binding.

* In `let`, the RHS initializers cannot see the new binders.
* In `letrec`, they can.

So `B` must be applied differently.

## 114.3 Internal `define` (SICP’s “scan-out-defines”)

Within a lambda body, internal defines act like `letrec`:

```scheme
(lambda (...)
  (define x e1)
  (define y e2)
  body ...)
```

Expander desugars to:

```scheme
(lambda (...)
  (letrec ((x e1) (y e2))
    body ...))
```

Then apply the `letrec` binder-scope algorithm.

This is exactly the SICP transformation, now stated as a hygiene rule.

---

# 115. A Tightened Expander Algorithm (Macro Expansion + Binder Installation + Phase Discipline)

We now describe the expander as a total function:

[
\text{expand}_{p} : \text{Syntax} \times Env \to \text{Syntax}
]

parameterized by phase `p`, and returning syntax at the same phase.

## 115.1 Core expansion loop

For syntax `stx` at phase `p_out`:

1. If `stx` is atomic: return.
2. If `stx` is an identifier: return (resolution is deferred; expansion only installs bindings).
3. If `stx` is a list:

   * expand head
   * if head resolves (in `Env[p_run]`) to a transformer running at `p_run = p_out + 1`:

     * apply transformer to `stx` (with use-site scope addition `U`)
     * transformer produces output syntax at phase `p_out`
     * re-expand output (macro chaining)
   * else:

     * if list is a binding form at phase `p_out` (`lambda`, `let`, etc.): run binder-scope algorithm and expand subforms under extended env
     * otherwise: recursively expand all elements and return list

### 115.1.1 Use-site scope `U` on macro application

When expanding a macro use:

* generate fresh use-site scope `U`
* add `U` to the entire call form before matching
* this ensures that pattern variables carry call-site identity

This is the standard hygiene move.

### 115.1.2 Introducer scope `I` in transformer output

For `syntax-rules`, the transformer introduces fresh scope `I` that is applied to identifiers originating in the template (not substituted pattern variables).

This isolates introduced temporaries from user code.

---

# 116. Semantic Macros at Expansion Time: Hermetic Receipts with Oracle Transcript Digests

Now we finalize the inference integration in the expander.

## 116.1 Semantic macro contract

A semantic transformer (procedural macro) at phase `p_run` is allowed to call inference and must return:

* `outStx` : Syntax at `p_out`
* `obligation` : structured obligation object
* `evidence` : evidence references (hashes)
* `meta` : cost profile, confidence, etc.

We represent this as a `Meaning`-like record (or literally a `Meaning`):

```lisp
(record "tag" "meaning"
        "rewrite" outStx
        "obligation" obl
        "evidence" ev
        "confidence" conf
        "cost" cost
        "deps" deps)
```

## 116.2 Hermetic build discipline for expansion-time inference

**Strict compile mode** requires that any inference-dependent expansion:

* is *keyed* by `(inputHash, transformerDigest, pinnedArtifacts, envelope)`
* is *memoized* as a **MacroReceipt**
* can be replayed without re-running inference

### 116.2.1 Receipt key

Define:

* `H_in` = hash(input syntax datum + scopes canonicalization)
* `D_tr` = hash(transformer definition module digest + transformer binding bid)
* `D_art` = hash(engine/policy/prompt versions + any dependency digests)
* `E_env` = determinism envelope (seed, sampling spec, temperature regime, etc.)

Key:

[
K = H(H_{in} \parallel D_{tr} \parallel D_{art} \parallel E_{env})
]

The expander checks receipt store for `K`. If present, use `receipt.output` and skip inference.

## 116.3 Oracle transcript digest

To keep inference from becoming “black box textual,” you store an **oracle transcript digest** in the receipt:

* hash of the sequence of `OracleReq/OracleResp` events (or at least the final return plus evidence references)
* optionally store full transcript in Σ (ledger) and put pointer hash in receipt

Receipt fields:

```lisp
(record "tag" "macro-receipt"
        "key" K
        "input-hash" H_in
        "transformer-digest" D_tr
        "artifacts-digest" D_art
        "envelope" E_env
        "output-hash" H_out
        "output" outStx
        "obligation" obl
        "evidence" ev
        "oracle-transcript-hash" H_oracle
        "time" (time-ms))
```

This makes inference use auditable and replayable.

---

# 117. Commit Barriers for Semantic Expansion: “Compile” vs “Promote”

A semantic macro expansion may be correct only probabilistically (confidence < 1, or obligations not discharged). The language must not pretend otherwise.

Therefore the build pipeline is **two-phase** (CQRS + promotion):

## 117.1 Compile phase

* expands module
* produces macro receipts
* emits obligations as part of module artifact
* module artifact status: `candidate`

## 117.2 Promote phase

Promotion is a pipeline that:

* discharges obligations (tests, eq-ext, proofs)
* runs fuzz/critic suites if required
* checks authority deltas
* then marks artifact `trusted`

Only trusted artifacts can be used in strict runtime profiles.

This is a textbook **Stage Gate** + **Build Pipeline** design, and it is the only reliable way to govern inference-derived transformations.

---

# 118. Ω₀ — The Boot Subset That Can Compile Ω

To self-host the expander, you need a subset language that is:

* small enough to implement in the host once
* expressive enough to implement `syntax-rules`, environments, and the expander algorithm
* deterministic by default (so boot is reproducible)

## 118.1 Ω₀ grammar (kernel)

Ω₀ contains exactly the SICP/Scheme core (no macros):

* `quote`
* `if`
* `lambda`
* `begin`
* `define`
* `set!`
* application

Plus data primitives:

* pairs/lists
* vectors
* records/maps
* basic arithmetic and predicates
* `error`

And (optional but recommended):

* `effect` / `handle` (for receipts and controlled I/O), but boot can be done without it if you allow the host to write artifacts.

Critically: **Ω₀ does not require macros**. It is sufficient to *implement macros*.

---

# 119. The Expander-in-Ω Boot Image

We now specify a concrete boot artifact set.

## 119.1 Boot modules (minimal closure)

1. `omega.boot.syntax0`

   * syntax objects
   * scopes
   * bind records with `bid`
   * `resolve-id`
   * `free-identifier=?` and `bound-identifier=?`

2. `omega.boot.syntax_rules0`

   * `syntax-rules` compiler + matcher + template expander
   * literal matching by `free-identifier=?` using captured def env
   * nested ellipses and ranked substitutions

3. `omega.boot.expander0`

   * phase separation: env0/env1 with phase tags
   * binder-scope insertion for λ/let/let*/letrec/define/define-syntax/module
   * macro expansion loop
   * lowering to core AST (or directly to host AST encoding)

4. `omega.boot.build0`

   * artifact hashing
   * macro receipt store interface (in-memory or host-backed)
   * module artifact creation
   * strict-mode verification

These are all implementable in Ω₀ without macros.

## 119.2 Bootstrap compilation sequence (deterministic)

1. Host loads and evaluates `omega.boot.*0` modules under Ω₀.
2. Use `expander0` to compile:

   * `omega.base_macros` (syntax-rules definitions for `let`, `cond`, `and`, `or`, `when`, `unless`, etc.)
   * `omega.stdlib` modules (streams, nondet, generic ops, constraints)
3. Produce a **boot image artifact**:

   * expanded core AST hashes for these modules
   * receipts and module artifacts
4. Now the system can run normal Ω code with macros.

Then you proceed to the self-host milestone:

* recompile the expander itself using the expander (tower closure)
* differential-test host expander vs Ω expander
* promote the Ω expander artifact
* switch the default expander to the promoted one

This is the precise Strangler Fig path.

---

# 120. Tightened Code Updates (Reference-Level Ω Source)

Below are the key patches to the Part 12 reference suite, focusing on the essential correctness deltas.

## 120.1 Update `omega.syntax`: binding entries include `bid`, and add `free-identifier=?`

```lisp
;; add to omega.syntax:

(define (make-bind bid name scopes phase kind value)
  (record "tag" "bind" "bid" bid "name" name "scopes" scopes "phase" phase "kind" kind "value" value))

(define (bind? b) (and (record? b) (eq? (get b "tag") "bind")))

(define (unbound? r) (and (record? r) (eq? (get r "tag") "unbound")))

(define (resolve-id id env phase)
  ;; returns bind or (record "tag" "unbound" ...)
  ... ; same subset+maximality selection, but return the bind record

)

(define (free-identifier=? id1 env1 phase id2 env2)
  (let ((r1 (resolve-id id1 env1 phase))
        (r2 (resolve-id id2 env2 phase)))
    (cond
      ((and (bind? r1) (bind? r2)) (eq? (get r1 "bid") (get r2 "bid")))
      ((and (unbound? r1) (unbound? r2)) (eq? (get id1 "name") (get id2 "name")))
      (else #f))))
```

## 120.2 Update `omega.syntax_rules`: literal matching uses `litkeys` + `free-identifier=?`

Transformer now stores:

* `phase-out`
* `env-def-out`
* `litkeys` precomputed

Matching uses:

* input resolved in `env-use-out` at `phase-out`

This is the exact replacement for the name-based `literal-id=?`.

---

# 121. The Final Invariants (What Must Never Be Violated)

If you implement Part 13 exactly, you get a macro/inference/search substrate with the following invariants:

1. **Hygiene**: introduced temporaries never capture user references unless an explicit escape hatch is used.
2. **Lexical scoping**: binders install stable binding identities; resolution is deterministic and unambiguous in strict mode.
3. **Correct literal matching**: `syntax-rules` literals are matched by binding identity, not by spelling.
4. **Hermetic builds**: any inference/tool-dependent expansion is replayable via receipts; strict compilation is reproducible.
5. **Governed semantics**: semantic rewrites never silently mutate meaning; they produce obligations; promotion gates enforce them.
6. **Bootstrappability**: Ω₀ can compile the expander written in Ω; the system can transition to self-hosting via promoted artifacts.

These are the properties that separate a real language from a clever prompt wrapper.

---

# 122. Next Continuation (Part 14) — Only If You Want It

At this point, the spec is complete enough to implement. The next thing to do is not more prose; it’s **a concrete implementation plan** with:

* a directory-by-directory build of the TypeScript reference interpreter
* the Ω₀ boot modules executed end-to-end
* conformance tests:

  * hygiene torture suite
  * nested ellipses suite
  * nondet search correctness suite
  * hermetic receipt replay suite
* a differential testing harness between host and self-hosted expander
* and the first “promote the expander” milestone script

If you say **continue**, Part 14 will be that implementation playbook (including the test matrices and the exact data formats for receipts/artifacts/ledger events).
