## Ω Language Specification v1.0

### Part 14 — Implementation Playbook: TypeScript Reference Interpreter + Ω₀ Boot Image + Conformance & Differential Test Harness + “Promote the Expander” Milestone Script

You’ve now got enough specification to build Ω as a real system. This part is the **engineering playbook**: concrete repository structure, interfaces, invariants, test matrices, and the exact build/promotion workflow that keeps the whole thing from collapsing into “some API call.”

I’m going to assume **Node.js + TypeScript** for the reference implementation. The key reason is not convenience—it’s that TS makes **discriminated unions** ergonomic (AST/Values/Kont/OpCall), and it’s easy to wire a hermetic artifact store and a deterministic test runner.

---

# 123. Architectural “Ports & Adapters” Boundary (Non‑Negotiable)

Everything in Ω will go wrong if you don’t isolate:

* **Pure semantics** (expansion + evaluation)
  from
* **impure adapters** (oracle engines, tools, filesystem, time, randomness)

So we enforce a Hexagonal Architecture:

* Core packages: *pure-ish* (can run in-memory, deterministic)
* Adapters: plug-in implementations of ports

This is what lets you:

* run strict hermetic builds,
* do deterministic tests,
* replay receipts without calling an LLM,
* and prove that “inference is a semantic plane” rather than ambient behavior.

---

# 124. Repository Layout (Final Form)

```
omega/
  package.json
  tsconfig.json
  src/

    core/
      ast.ts
      sexp.ts
      reader.ts
      printer.ts

      syntax/
        syntax.ts        # Syntax objects, scopes, Ident, canonicalization
        scopes.ts
        binding.ts       # bid, binding records
        resolve.ts       # resolve-id, free-identifier=?, bound-identifier=?
        alpha.ts         # alpha-eq for expanded core (for differential testing)

      expand/
        expander.ts      # expand(p): Syntax -> Syntax
        binders.ts       # binder-scope insertion algorithms (lambda/let/letrec/define/module)
        syntaxRules.ts   # compiled syntax-rules, ellipses, ranked subst
        lowering.ts      # expand -> core AST lowering (int/infer/rewrite -> infer.op, amb -> effects, etc.)
        phase.ts         # phase environment tower Env[p]
        module.ts        # module forms, require/provide scaffolding

      eval/
        values.ts
        store.ts         # persistent store or snapshot discipline for multi-shot
        env.ts           # runtime env (ctx)
        kont.ts
        machine.ts       # CEKS stepper
        runtime.ts       # effect dispatch + handle semantics (delimited)
        builtins.ts      # NativeFns (record/get/vector/list ops, etc.)

      effects/
        handler.ts
        nondet.ts        # amb handler suite: dfs/bfs/best/beam/sample
        oracle.ts        # infer.op handler (orchestrator)
        commit.ts        # commit barriers & obligation checking
        receipts.ts      # ctx snapshot/compress/hydrate effects

      obligations/
        obligation.ts
        checks.ts        # tests, eq-ext sampling, metamorphic checks
        fuzz.ts          # property/fuzz harness hooks (deterministic RNG)
        proof.ts         # prover port (optional)

      artifacts/
        digest.ts
        cas.ts           # content-addressed store
        receipt.ts
        registry.ts      # promotion registry (trusted/candidate)
        pipeline.ts      # promotion pipelines (stages)

      boot/
        omega0/          # Ω₀ modules as plain .omega source
          syntax0.omega
          syntax_rules0.omega
          expander0.omega
          build0.omega

        loadBoot.ts      # loads Ω₀ modules into runtime, builds boot image

      cli/
        omega.ts         # CLI entry: compile, expand, eval, test, promote, replay
        commands/
          compile.ts
          expand.ts
          eval.ts
          test.ts
          promote.ts
          replay.ts

    adapters/
      oracle/
        engines/
          mock.ts
          openai.ts      # (optional) real engine adapter
        cache.ts         # memoization of infer sessions by request digest

      tools/
        shell.ts
        fs.ts
        http.ts
      time.ts
      rng.ts

  test/
    macros/
      hygiene.spec.ts
      ellipses.spec.ts
      phase.spec.ts
    nondet/
      amb.spec.ts
      fairness.spec.ts
      cut.spec.ts
      heuristics.spec.ts
    hermetic/
      receipts.spec.ts
      replay.spec.ts
    differential/
      expander_host_vs_boot.spec.ts
      expanded_ast_alpha_eq.spec.ts
```

Patterns embodied here:

* **Interpreter** (GoF): `eval/*` + `expand/*`
* **Strategy**: search frontiers in `effects/nondet.ts`, inference strategies in `effects/oracle.ts`
* **Template Method**: promotion pipeline stages in `artifacts/pipeline.ts`
* **Repository** + **Unit of Work**: artifact registry + commit barrier
* **Event Sourcing**: Σ ledger hooks (even if you start with in-memory)

---

# 125. Build System & Determinism Envelope

## 125.1 Package scripts

Use a deterministic test runner (`vitest` recommended; `jest` also fine).

```json
{
  "scripts": {
    "build": "tsc -p tsconfig.json",
    "test": "vitest run",
    "test:watch": "vitest",
    "omega": "node dist/src/cli/omega.js"
  }
}
```

## 125.2 Determinism envelope (core API)

Every stage that could be nondeterministic must accept an explicit envelope:

```ts
export type Envelope = {
  seed: number;            // PRNG seed
  maxSteps: number;        // CEKS step budget
  maxTokens: number;       // oracle token budget
  maxToolCalls: number;    // tool budget
  timeBudgetMs: number;    // wall clock budget (adapter-enforced)
  temperature: number;     // inference sampling (if used)
  mode: "strict" | "explore";
};
```

Strict mode must treat:

* time,
* randomness,
* oracle calls,
* tools,
  as **effectful** and therefore must be pinned or cached by receipts.

This is the system‑level equivalent of referential transparency boundaries.

---

# 126. Critical Semantic Implementation Choice: Multi‑Shot Resumptions Require Store Persistence

Your `amb` handler **requires multi-shot** resumptions.

In a CEKS machine with a mutable store, this is a landmine:

* If two resumed branches share the same store object and one writes to it, the other branch observes the mutation → **unsound backtracking**.

Therefore:

## 126.1 Rule: every captured resumption captures a store snapshot

Implement either:

### Option A — Persistent store (recommended)

Represent store as an immutable map with structural sharing:

```ts
type Store = { next: number; cells: PersistentMap<number, Val> };
```

Every write returns a new Store. Then a resumption can safely reuse the captured Store.

### Option B — Copy-on-resume snapshot

Keep a mutable Map for speed, but snapshot on capture:

* store serialization snapshot
* or `new Map(old.cells)` cloning plus copy next pointer

For a reference interpreter, Option B is acceptable but you must be explicit about O(n) per capture.

**If you do not do one of these, `amb` is incorrect.** This is the same reason call/cc implementations require careful store handling in the presence of mutation.

---

# 127. Core Execution Pipeline: `read → expand → lower → eval`

You implement a single “compile/eval pipeline” entry point so your CLI and tests use identical semantics.

```ts
type CompileOutput = {
  expanded: Syntax;        // macro-expanded surface syntax
  core: Expr;              // lowered AST
  receipts: MacroReceipt[]; // hermetic receipts produced during expansion
  digests: { source: Hash; expanded: Hash; core: Hash };
};

async function compile(
  sourceText: string,
  envs: PhaseEnvs,
  envelope: Envelope,
  artifactStore: ArtifactStore
): Promise<CompileOutput> { ... }

async function run(
  core: Expr,
  runtime: Runtime,
  envelope: Envelope
): Promise<Val> { ... }
```

This is a **Pipeline** (Pipes-and-Filters) architecture with well-defined artifacts at each stage.

---

# 128. Implementation Order: Milestones that Minimize Rework

You want to implement in an order that avoids catastrophic refactors:

## Milestone 1 — CEKS core + effects runtime

* `values.ts`, `kont.ts`, `machine.ts`, `runtime.ts`
* `effect` emission and handler dispatch
* delimited `handle` semantics
* builtins: `record`, `get`, `vector`, basic arithmetic

Deliverable:

* can run pure Ω₀ programs (no macros)
* can run `handle` as delimited effects (but not yet inference/amb)

## Milestone 2 — Persistent store or snapshot discipline

* commit to Option A or B
* write regression tests ensuring multi-shot resume doesn’t share mutation

Deliverable:

* safe resumptions

## Milestone 3 — `amb` handler suite (dfs/bfs/best/beam/sample)

* implement `amb.op`, `amb.fail`, `amb.cut`, `amb.observe`
* implement wrappers `first-solution`, `all-solutions`, `best-solution`, `sample-solution`
* fairness tests (BFS finds shallow solution if DFS diverges)

Deliverable:

* SICP nondet problems run

## Milestone 4 — Syntax objects + scope sets + binding identity

* `Syntax`, `Ident`, `ScopeSet`, `bid` generation
* `resolve-id`, `free-identifier=?`, `bound-identifier=?`

Deliverable:

* can represent hygienic syntax and resolve identifiers deterministically

## Milestone 5 — `syntax-rules` compiler (nested ellipses)

* ranked substitution `One/Many`
* ellipsis shape/rank constraints
* template replication (zip semantics)

Deliverable:

* pass ellipsis torture suite (below)

## Milestone 6 — Expander + binder-scope insertion + phase envs

* macro expansion loop
* binder installation algorithms:

  * `lambda`, `let`, `let*`, `letrec`, internal defines (scan-out)
* `define-syntax` and `require-for-syntax` skeleton

Deliverable:

* expand core macros like `let`, `cond`, `and`, `or` hygienically

## Milestone 7 — Hermetic receipts + artifact store + replay

* macro receipt keys
* store output syntax + oracle transcript hash (even if oracle isn’t used yet)
* strict compile = deterministic rebuild

Deliverable:

* compile strict twice yields identical expanded/core hashes

## Milestone 8 — Ω₀ boot image load + self-host expander differential harness

* load Ω₀ modules (`syntax0`, `syntax_rules0`, `expander0`, `build0`) into runtime
* run boot expander on test programs
* compare host expander output vs boot expander output (α-equivalence aware)

Deliverable:

* expander-in-Ω works and matches host on conformance suite

## Milestone 9 — Promotion pipeline: “Promote the expander”

* compile expander module artifact
* run macro torture suite & differential suite
* mark artifact as trusted in registry
* switch default expander pointer to trusted artifact digest

Deliverable:

* reproducible self-hosting transition without guessing

---

# 129. Conformance Test Matrices (Executable, Not Aspirational)

You need explicit test suites that make regressions impossible to miss.

## 129.1 Macro hygiene torture suite

### Test H1 — introduced temporary does not capture user binder

```scheme
(define-syntax m
  (syntax-rules ()
    ((_ x) (let ((t 10)) x))))

(let ((t 99))
  (m t))
;; must be 99
```

### Test H2 — user binder does not capture introduced temporary

Macro introduces `t` and also refers to `t` internally. Ensure user `t` doesn’t hijack it.

### Test H3 — binder introduced by macro binds references introduced by macro

Classic `let` expansion: `(let ((x e)) body)` expands to `((lambda (x) body) e)`; the references in `body` that originate from user remain user; binder in lambda binds only those occurrences intended by expansion scoping.

### Test H4 — nested macros preserve hygiene across expansion phases

A macro that expands to another macro invocation.

### Test H5 — macro across module boundaries

* module A defines macro
* module B imports macro and uses it
  Ensure literal matching uses `free-identifier=?` semantics relative to definition env and use env.

**Implementation note**: test H5 is where name-based literal equality will fail; it forces binding-identity correctness.

---

## 129.2 Ellipses correctness suite (nested ellipses)

### E1 — single ellipsis binds vectors correctly

```scheme
(define-syntax mylist
  (syntax-rules ()
    ((_ x ...) (list x ...))))

(mylist 1 2 3)  ;; => (list 1 2 3)
```

### E2 — multiple vars under same ellipsis zip lockstep

```scheme
(define-syntax zip
  (syntax-rules ()
    ((_ (a ...) (b ...)) (list (list a b) ...))))

(zip (1 2) (3 4))  ;; => ((1 3) (2 4))
```

### E3 — nested ellipses rank correctness

Patterns like:

```scheme
((_ ((x ...) ...) body ...)
 ...)
```

and templates that replicate accordingly.

### E4 — shape mismatch must be deterministic error

If `a...` has length 2 and `b...` length 3 at same depth, expansion must fail.

---

## 129.3 Phase separation suite

### P1 — runtime bindings not visible at macro time

Unless explicitly imported for syntax.

### P2 — define-syntax exports only at phase 1

`require` vs `require-for-syntax` differences.

### P3 — begin-for-syntax evaluation doesn’t pollute runtime env0

Cross-phase persistence rules tested explicitly.

---

## 129.4 Nondeterminism suite

### N1 — soundness: all solutions satisfy constraints

For a known puzzle, every returned solution must validate all `require` predicates.

### N2 — completeness (bounded): `all-solutions` enumerates expected set

For small finite search spaces.

### N3 — fairness: BFS finds shallow solution even if a deep branch diverges

Construct a program where DFS diverges but BFS should succeed.

### N4 — multi-shot correctness with mutation

Write a nondet program that mutates a cell differently in different branches; ensure branches don’t interfere.

### N5 — `cut` semantics is delimited

`cut` clears only the frontier within the handler’s region.

---

## 129.5 Hermetic receipts & replay suite

### R1 — strict compile twice produces identical hashes

`sourceHash`, `expandedHash`, `coreHash` stable.

### R2 — semantic macro inference requires receipt

If semantic macro calls inference under strict mode and receipt is absent: compilation rejects.

### R3 — receipt replay produces identical output without oracle calls

Force engine adapter to throw if called; replay must still compile.

---

## 129.6 Differential suite (host expander vs boot expander)

### D1 — α‑equivalence of expanded core AST

Because binder ids/scopes differ, compare via alpha-normalization.

Alpha-normalization algorithm:

* rename binders to canonical `x0, x1, ...` by traversal order
* rewrite all bound occurrences accordingly
* compare normalized trees structurally

### D2 — macro receipt equivalence

Ensure receipts produced by host expander and boot expander:

* have same input/output digests
* may differ in internal bid/scopes but must have equivalent canonicalized forms

---

# 130. Differential Harness: How to Compare Two Expanders Correctly

Naively comparing printed syntax will fail because:

* scope IDs differ
* binder IDs differ

So define a canonicalization function:

## 130.1 Syntax canonicalization

* strip source locations
* rename scope IDs by encounter order
* rename binder IDs by encounter order
* preserve structural shape and symbol names

Then compare canonicalized syntax trees.

## 130.2 Core AST alpha-normalization

As above: alpha-normalize binders.

This is exactly the method compilers use for IR equivalence tests.

---

# 131. “Promote the Expander” — The Milestone Script

This is the crucial stage-gate move: the expander becomes a *versioned trusted artifact*.

## 131.1 Artifact registry model

Registry stores:

* `candidate` artifacts (built but not trusted)
* `trusted` artifacts (promoted)
* a pointer `defaultExpanderDigest`

Implement as a simple JSON file in the CAS directory for the reference.

```ts
type Registry = {
  trusted: Record<Hash, { name: string; time: number; receipts: Hash[] }>;
  candidates: Record<Hash, { name: string; time: number }>;
  pointers: { defaultExpander?: Hash };
};
```

## 131.2 Promotion pipeline stages (Template Method)

A pipeline is:

1. **Compile** expander module under strict mode
2. **Verify hermeticity** (receipts required, deterministic envelope pinned)
3. **Run macro conformance** (hygiene + ellipses + phase tests)
4. **Run differential** (host expander vs boot expander)
5. **Run performance sanity** (optional; just step counts/budgets in strict mode)
6. **Promote**: mark artifact trusted and update pointer

This is literally Fowler’s “Deployment Pipeline,” but for semantics.

## 131.3 CLI command: `omega promote expander`

Pseudo-flow:

```ts
async function promoteExpander(args) {
  const envelope = strictEnvelope(args);
  const artifact = await compileModule("omega.boot.expander0.omega", envelope);

  // stage 1: verify receipts
  verifyHermetic(artifact);

  // stage 2: run conformance suites
  await runTestSuite("macros:hygiene", artifact);
  await runTestSuite("macros:ellipses", artifact);
  await runTestSuite("macros:phase", artifact);

  // stage 3: differential
  await runDifferentialSuite("expander_host_vs_boot", artifact);

  // stage 4: promote
  registry.promote(artifact.digest);
  registry.pointers.defaultExpander = artifact.digest;
  registry.save();
}
```

### Critical gate: “no new authority”

Promotion must check that the expander artifact does not request more capabilities than allowed for strict compilation.

In practice:

* expanders should not require tools or external inference in strict mode
* if semantic macros do inference at compile-time, they must be pinned and receipt’d (and usually restricted to local, deterministic engines)

---

# 132. Ω₀ Boot Image: End-to-End Execution Plan

You want a deterministic boot flow you can run in CI.

## 132.1 Boot steps (mechanical)

1. Start runtime with Ω₀ evaluator (no macros)
2. Load Ω₀ modules as plain source:

   * `syntax0.omega`
   * `syntax_rules0.omega`
   * `expander0.omega`
   * `build0.omega`
3. Evaluate them to install phase‑1 transformer constructors and expander functions into the runtime environment
4. Use `expander0` to expand `omega.base_macros.omega` (which defines `let`, `cond`, etc.)
5. Compile and store resulting module artifacts into CAS
6. Re-run expansion using the newly compiled expander to ensure tower closure
7. Run differential tests vs host expander to certify equivalence
8. Promote expander

This is the “boot image build” in compiler engineering terms.

---

# 133. The Minimal Set of Ω₀ Host Builtins Required for Boot

To self-host the expander in Ω₀, you must provide (or implement in Ω₀) primitives for:

* syntax object constructors (`make-ident`, `make-list`, `add-scope`)
* scope generation (`fresh-scope`)
* digest (`hash`)
* deterministic list/vector/record ops

Everything else can be built in Ω₀.

This keeps the trusted base tiny—exactly the SICP ethos.

---

# 134. Where Inference Fits in Part 14 (Without Destroying Reproducibility)

Even during implementation, you must keep inference disciplined:

* **Default**: compile-time inference disabled in strict mode
* **Allowed**: semantic macros can use inference at compile time **only** if:

  * request digest is pinned (engine/policy/prompt versions)
  * oracle transcript digest stored in macro receipt
  * expansion output pinned in receipt
  * obligations attached and discharged before promotion

At runtime:

* inference is an effect (`infer.op`) handled by `OracleHandler`
* results are `Meaning` objects, never raw text
* `commit` gates enforce obligations before artifacts mutate reality

This is the “not an API call” criterion embodied operationally.

---

# 135. Concrete Next Step

If you want to proceed like an engineer instead of continuing to write spec, the next thing to do is:

1. Implement `eval/` CEKS + delimited effects correctly with persistent store snapshots
2. Implement `effects/nondet.ts` and pass the nondet tests
3. Implement `syntax/` + `expand/syntaxRules.ts` and pass the macro torture suite
4. Implement `boot/loadBoot.ts` and pass the differential suite
5. Implement `artifacts/` and the `promote expander` pipeline

If you reply **continue**, I’ll give you **Part 15** as a “code-first” blueprint: exact TS interfaces and concrete skeleton code for:

* persistent store representation,
* multi-shot resumption capture,
* a correct `amb` handler in TS (DFS/BFS/best/beam),
* a correct `syntax-rules` matcher with nested ellipses,
* and the alpha-normalization comparator used by the differential harness.
