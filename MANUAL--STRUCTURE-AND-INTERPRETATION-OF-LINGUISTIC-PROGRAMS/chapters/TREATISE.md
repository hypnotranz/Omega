# Treatise: Cognitive Pattern Equivalents for OmegaLLM + Matrix

> **IMPORTANT: This is a REFERENCE VOCABULARY, not a specification to implement.**
>
> Use these terms (CDT, CPO, CA, etc.) in code comments and documentation to describe patterns as you discover them. Extract reusable abstractions only after 20+ examples prove their value through actual usage.
>
> This treatise defines shared terminology for talking about cognitive patterns. It is not a library to build upfront—it's a pattern catalog to consult when needed.

---

## Pattern Source References

This treatise maps OmegaLLM/Matrix patterns to canonical sources:

* **SICP** - Structure and Interpretation of Computer Programs
* **EIP** - Enterprise Integration Patterns
* **GoF** - Design Patterns: Elements of Reusable Object-Oriented Software
* **PoEAA** - Patterns of Enterprise Application Architecture
* **DDD** - Domain-Driven Design
* **POSA** - Pattern-Oriented Software Architecture

---

## 0) The Objective, Made Literal

We are designing **two interlocking systems**:

### OmegaLLM

A semantic programming language (SICP-class) where **inference is a first-class algebraic effect** and where programs are **re-entrant** between:

* deterministic compute (pure/total where possible),
* effectful inference,
* and effectful Matrix queries/commands.

OmegaLLM must support: higher-order procedures, recursion, streams, nondeterminism (AMB), analyzers/compilers, abstract machines.

### Matrix Protocol

A universal addressability layer where everything addressable is a **component** with:

* semantic URI,
* ports (query/command/event),
* local cached context,
* local deterministic compute,
* local semantic inference (when needed),
* hierarchical composition,
* streaming parallel scatter-gather,
* request-reply over pub-sub (correlation IDs).

### Thesis

Many LLM tasks have **latent algorithmic structure** that should be expressed as **program skeletons**: data structures + algorithms, not prompt chains. Once inference is a first-class effect and artifacts are addressable, we can systematically re-use FP/EIP/GoF/DDD/POSA/CI+test patterns as **cognitive equivalents**.

---

# 1) REQUIRED DEFINITIONS (Hard, Explicit)

Below each definition includes: (a) definition (b) boundary (c) examples (d) composition (e) verifiability.

---

## 1.1 Cognitive Data Type (CDT)

### (a) Definition

A **Cognitive Data Type (CDT)** is a first-class value type whose **denotation** is a semantic object (meaning-level structure) rather than raw text. A CDT has:

* a **canonical representation** (Meaning Normal Form; MNF),
* **invariants** (semantic + structural),
* and a **total interface** of operations (some pure, some effectful).

Formally: a CDT is a pair ⟨Rep, Laws⟩ where:

* Rep ∈ MNF (a canonical semantic form),
* Laws is a set of invariants checked by contracts and/or verifiers,
* operations are typed as `ENV -> CDT -> CE[CDT’]` or pure `CDT -> CDT’`.

### (b) Boundary / Not

Not “just JSON,” not “just a string,” not “whatever the model says.” JSON is a **carrier**. A CDT is a **semantic type** with invariants and typed operations.

### (c) Canonical Examples

* `Summary(level, scope, invariants_preserved, evidence_refs)`
* `SQLAST(select, from, where, …)`
* `ConstraintViolation(invariant_id, counterexample, severity)`

### (d) Composition

CDTs compose via:

* product composition (records/tuples),
* sum composition (tagged unions),
* graph composition (DAGs of evidence/provenance),
* and via **Kleisli composition** when operations are effectful.

### (e) Testability

A CDT is testable by:

* schema/contract validation (JSON Schema / enum),
* metamorphic tests (paraphrase invariance under ENV),
* golden masters tied to snapshot IDs,
* differential checks (before/after transformations),
* and provenance replay (re-run derivations).

---

## 1.2 Cognitive Primitive Operation (CPO)

### (a) Definition

A **Cognitive Primitive Operation** is a minimal, typed semantic instruction. It is the “ISA” of cognition: classification, extraction, judgment, rewriting, canonicalization, routing, citation, etc.

Formally: `op : (ENV × Inputs) -> CE[Output]` with a declared:

* **effect class** (pure vs effectful),
* **contract** for output,
* **evidence obligations** (if epistemic).

### (b) Not

Not an unstructured prompt. Not an opaque “agent step.” CPOs must be composable and contractible.

### (c) Examples

* `classify : ENV × ArtifactView -> CE[TagSet]`
* `extract : ENV × ArtifactView × ExtractSpec -> CE[ExtractedStructure]`
* `rewrite : ENV × ArtifactView × RewriteSpec -> CE[Patch]`

### (d) Composition

CPOs compose via:

* Kleisli (`>=>`) for effectful operations,
* applicative batching,
* stream fusion (map/filter/fold).

### (e) Verifiability

* contract tests on output schema,
* evidence presence checks,
* determinism harness for tier-0 operations.

---

## 1.3 Cognitive Algorithm (CA)

### (a) Definition

A **Cognitive Algorithm** is a reusable program skeleton that interleaves compute + cognitive effects to solve a task with explicit invariants and cost semantics.

Formally: `CA : ENV -> Inputs -> CE[Outputs]` built from:

* deterministic computation,
* CPO invocations,
* and explicit control structures (search, fixpoint, planning, compilation).

### (b) Not

Not “a workflow.” A CA has:

* a defined search space / state space,
* invariants,
* termination conditions,
* budget semantics,
* and failure modes.

### (c) Examples

* beam search over hypotheses (bounded),
* propagator fixpoint for architecture constraints,
* compilation pipeline from NL → IR → distributed query plan,
* provenance DAG minimization.

### (d) Composition

CAs compose by:

* higher-order algorithms (e.g., search parameterized by predicate),
* monoidal aggregation,
* staging (cheap filters then expensive inference),
* distribution over Matrix subtrees.

### (e) Verifiability

* cost invariants (budget never negative),
* replay receipts,
* property-based tests on algorithmic laws (e.g., monotonic fixpoint).

---

## 1.4 Cognitive Structure (CS)

### (a) Definition

A **Cognitive Structure** is a composite CDT that organizes cognition: trees, graphs, distributions, provenance DAGs, constraint networks.

### (b) Not

Not an ad hoc list of strings. Must have declared semantics and invariants.

### (c) Examples

* `EvidenceDAG`, `ProvenanceDAG`
* `ConstraintNetwork`
* `CallGraph`, `DependencyGraph`
* `Timeline`

### (d) Composition

CSs compose by graph merges, tree folds, and incremental updates keyed by snapshot IDs.

### (e) Testability

* structural invariants (acyclicity for DAG),
* minimality constraints (no dangling evidence refs),
* replayable derivations.

---

## 1.5 Cognitive Effect (CE)

### (a) Definition

A **Cognitive Effect** is an algebraic effect boundary representing interactions that are not pure deterministic compute:

* inference (LLM semantic transduction),
* retrieval/search,
* Matrix queries/commands,
* tool invocation.

We model a CE as a computation that can:

* call the outside world,
* and be **re-entrant** (inference can request compute; compute can call inference).

Type: `CE[T]` with handlers:

* `handle_infer`, `handle_matrix_query`, `handle_tool`, …

### (b) Not

Not “just calling a model.” CE includes:

* budget accounting,
* contracts,
* provenance emission,
* and policy enforcement.

### (c) Examples

* `(effect infer.op …)`
* `(effect matrix.query uri …)`
* `(effect matrix.command uri …)`

### (d) Composition

CE composes by Kleisli operators:

* `(bind ce f)` where `f : T -> CE[U]`.

### (e) Testability

* handler substitution (mock inference),
* deterministic replay with cached responses,
* contract assertion on effect results,
* budget exhaust tests.

---

## 1.6 Meaning Normal Form (MNF)

### (a) Definition

**Meaning Normal Form** is a canonical representation discipline that prevents stringly-typed brittleness. MNF is a normalization function `normalize : Raw -> MNF[T]` producing:

* canonical IDs,
* stable span references,
* normalized units,
* canonical schemas for structured outputs,
* and normalized language variants according to ENV.

### (b) Not

Not “pretty formatting.” MNF is canonicalization for correctness and caching.

### (c) Examples

* Code spans normalized to `(uri, snapshot, range)` rather than copied text.
* Table values normalized to `(table_uri, row_id, column, value, unit)`.

### (d) Composition

MNF allows safe memoization and equality up to canonicalization (≃).

### (e) Testability

* normalization idempotence: `normalize(normalize(x)) = normalize(x)`
* stable keys under paraphrase when semantics unchanged.

---

## 1.7 Semantic Contract

### (a) Definition

A **Semantic Contract** is a typed schema/enum/record constraint on CE results that is enforced at runtime and used for compilation, optimization, and testing.

### (b) Not

Not “hope the model outputs JSON.” A contract is checked; coercions are explicit adapters.

### (c) Examples

* enum: `Tag ∈ {pure_logic, side_effect, config, glue, business_rule}`
* schema: `SQLAST` JSON Schema.

### (d) Composition

Contracts compose via product/sum schemas; contract refinement via subtyping.

### (e) Testability

* contract tests,
* metamorphic tests ensuring stable contract outputs under paraphrase,
* schema evolution tests.

---

## 1.8 Semantic Environment / Pragmatics Parameter (ENV)

### (a) Definition

ENV is an explicit parameter capturing pragmatics:

* audience (expert/novice),
* domain ontology,
* policy constraints (security, compliance),
* style constraints (tone),
* risk tolerance,
* budget preferences.

ENV makes semantic operations explicit functions of context rather than hidden prompt-state.

### (b) Not

Not “the system prompt.” ENV is a typed record passed into computations.

### (c) Examples

* ENV sets taxonomy for `classify`.
* ENV sets citation requirements for `summarize`.

### (d) Composition

ENV composes by scoped override and lexical binding.

### (e) Testability

* environment-specific golden masters,
* cross-ENV consistency tests.

---

## 1.9 Addressable Artifact

### (a) Definition

An **Addressable Artifact** is a Matrix component with:

* a semantic URI,
* ports: query/command/event,
* local state (cached snapshot),
* capability discovery,
* and optional local inference/compute.

### (b) Not

Not a file path only. It’s a typed component with protocol semantics.

### (c) Examples

* `matrix://acme/project/src/AuthService.ts`
* `matrix://acme/db/users`
* `matrix://home/thermostat`

### (d) Composition

Artifacts compose hierarchically (Composite) and via views (projections).

### (e) Testability

* protocol conformance tests,
* snapshot consistency tests,
* idempotency tests on commands.

---

## 1.10 View Types

### (a) Definition

**View Types** are structured projections that bridge deterministic compute and inference: they provide the *shape* over which reasoning happens.

Examples: `RelationalView`, `ASTView`, `ClauseView`, `DialogueView`, `TimelineView`, `EvidenceView`.

### (b) Not

Not “a summary.” A View is a structured data model with stable addressing for elements.

### (c) Examples

* ASTView: nodes with stable IDs, spans, symbol links.
* ClauseView: numbered clauses with obligations/prohibitions extracted.

### (d) Composition

Views are materialized or virtual; can be cached and invalidated by snapshot.

### (e) Testability

* view determinism tests (same snapshot → same view),
* round-trip tests (view references map back to raw spans).

---

# 2) OmegaLLM + Matrix: Formal Interface Sketch

## 2.1 Matrix Component Protocol (Minimal IDL)

A component is identified by `URI` and exposes ports:

**Query Port** (pure-ish / read-only)

* `capabilities() -> CapabilitySet`
* `snapshot() -> SnapshotRef`
* `view(ViewSpec) -> ViewCDT`
* `query(QueryCDT, Contract) -> CE[ResultCDT]`

**Command Port** (state-changing)

* `command(CommandCDT, Preconditions, Contract) -> CE[CommandReceipt]`

**Event Port** (pub/sub)

* emits `ArtifactChanged`, `SnapshotInvalidated`, `CommandApplied`, etc.

Correlation IDs and request-reply:

* every request has `corr_id`
* every response includes `corr_id`, `snapshot_ref`, `provenance_ref`

---

## 2.2 OmegaLLM Effect Forms (Core)

Illustrative syntax (Lisp-ish):

```lisp
(effect infer.op
  :env ENV
  :contract Contract
  :budget Budget
  :prompt PromptCDT)

(effect matrix.query
  :uri "matrix://realm/mount/path"
  :env ENV
  :contract Contract
  :budget Budget
  :query QueryCDT)

(effect matrix.command
  :uri "matrix://realm/mount/path"
  :env ENV
  :contract Contract
  :budget Budget
  :command CommandCDT)
```

---

## 2.3 Type Notation (Compact)

* `CE[T]` cognitive effect returning `T`
* `Contract[T]` runtime-checkable schema for T
* `ENV` pragmatics record
* `SnapshotRef = {uri, snapshot_id, content_hash, ts}`
* `EvidenceRef = {uri, snapshot_id, span_id | row_id | clause_id}`

---

# 3) REQUIRED DELIVERABLE: CDT Catalog (64 CDTs)

**Template per CDT**

* **Definition**
* **Canonical Representation**: JSON-ish + Lisp-ish
* **Invariants**
* **Core CPOs / Typical CAs**
* **Worked Example** (prompt + return shape)
* **Matrix URI / View**
* **Test Strategy**

I’ll group CDTs by hierarchy.

---

## 3.A Core Semantic Atoms (CDT-01 … CDT-10)

### CDT-01: `EntityRef`

* **Definition:** Canonical reference to an entity (person/org/system/concept) within a domain ontology.
* **Canonical Representation:**

  * JSON: `{ "id":"ent:jwt", "label":"JWT", "ontology":"security", "aliases":["JSON Web Token"] }`
  * Lisp: `(entity-ref :id "ent:jwt" :label "JWT" :ontology "security")`
* **Invariants:** `id` stable; `ontology` declared; aliases non-authoritative.
* **Core CPOs / CAs:** `canonicalize-entity`, `link-entity`, CA: `OntologyLinking`.
* **Worked Example:** Prompt: “Normalize ‘json web tokens’ into canonical entities.”
  Return: `{entities:[{id,label,ontology}]}`.
* **Matrix URI / View:** `matrix://…/ontology/security#entity=jwt`
* **Test Strategy:** contract tests + synonym metamorphic tests.

### CDT-02: `SpanRef`

* **Definition:** Stable pointer to a region in an artifact snapshot (text/code/PDF).
* **Canonical Representation:**

  * JSON: `{ "uri": "...", "snapshot":"s123", "range": {"start":120,"end":188} }`
  * Lisp: `(span-ref :uri U :snapshot S :range (120 188))`
* **Invariants:** range valid in snapshot; immutable for snapshot.
* **Core CPOs / CAs:** `resolve-span`, `cite-span`, CA: `EvidenceWeaving`.
* **Worked Example:** “Cite the paragraph defining termination.” → `{span_refs:[…]}`.
* **Matrix:** `matrix://…/doc#span=…`
* **Tests:** round-trip: span resolves to same bytes.

### CDT-03: `Claim`

* **Definition:** A propositional statement with epistemic status.
* **Canonical:**

  * JSON: `{ "text":"JWT validation checks aud and iss", "status":"hypothesis|fact", "evidence":[SpanRef] }`
  * Lisp: `(claim :text ... :status 'hypothesis :evidence (list ...))`
* **Invariants:** facts must carry deterministic or cited evidence; hypotheses carry confidence.
* **CPOs/CAs:** `judge-claim`, `support-claim`, CA: `ClaimCheck`.
* **Example:** “Is issuer validated?” → `{claim:{status,confidence,evidence}}`
* **Matrix:** any artifact node.
* **Tests:** evidence obligation tests.

### CDT-04: `Question`

* **Definition:** A query with intent, scope, and expected answer contract.
* **Canonical:**

  * JSON: `{ "intent":"diagnose|summarize|extract", "text":"Where is JWT verified?", "scope":[URI] }`
  * Lisp: `(question :intent 'extract :text ... :scope (list U))`
* **Invariants:** intent ∈ enum; contract present for structured answers.
* **CPOs/CAs:** `compile-question->plan`, CA: `NL2IRCompilation`.
* **Example:** “Generate discriminative questions to verify auth flow.” → `{questions:[…]}`
* **Matrix:** orchestrator-level CDT.
* **Tests:** contract + intent classification tests.

### CDT-05: `Answer`

* **Definition:** Structured response to a Question with evidence and provenance.
* **Canonical:** `{ "value":..., "evidence":[…], "provenance":ProvRef }`
* **Invariants:** must cite snapshot; must satisfy contract.
* **CPOs/CAs:** `coerce-to-contract`, CA: `ReceiptReplay`.
* **Example:** “Return as enum.” → `{value:"jwt", evidence:[…]}`
* **Matrix:** response envelope.
* **Tests:** contract tests + provenance presence.

### CDT-06: `Summary`

* **Definition:** Compressive semantic representation preserving declared invariants.
* **Canonical:**

  * JSON: `{ "level":2, "scope":"module", "bullets":[...], "invariants_preserved":[...], "evidence":[...] }`
  * Lisp: `(summary :level 2 :scope 'module :bullets ... :invariants ...)`
* **Invariants:** must declare what it preserves; must be stable under paraphrase within ENV tolerance.
* **CPOs/CAs:** `summarize`, CA: `HierarchicalCompressionFold`.
* **Example:** “Summarize auth module at level 2.” → `{summary:{...}}`
* **Matrix:** `...#view=summary&level=2`
* **Tests:** metamorphic paraphrase invariance + golden master per snapshot.

### CDT-07: `TagSet`

* **Definition:** A set of typed tags with confidence and rationale.
* **Canonical:** `{ "taxonomy":"code-role", "tags":[{"tag":"side_effect","p":0.92,"evidence":[...]}] }`
* **Invariants:** tags from taxonomy; confidence in [0,1].
* **CPOs/CAs:** `classify`, CA: `TaggingPass`.
* **Example:** “Tag this file by role.” → TagSet.
* **Matrix:** `...#view=tags&taxonomy=...`
* **Tests:** contract tests; stability under whitespace changes.

### CDT-08: `Decision`

* **Definition:** A committed choice among alternatives with rationale and constraints.
* **Canonical:** `{ "choice":"jwt", "alternatives":[...], "rationale":..., "constraints":[...], "provenance":... }`
* **Invariants:** alternatives enumerated; rationale cites constraints.
* **CPOs/CAs:** `decide`, CA: `DesignSpaceSearchAMB`.
* **Example:** “Pick auth scheme satisfying constraints.” → Decision.
* **Matrix:** stored in `matrix://…/decisions/...`
* **Tests:** replay to validate choice still satisfies constraints.

### CDT-09: `RiskAssessment`

* **Definition:** Structured risk analysis (severity, likelihood, impact, mitigations).
* **Canonical:** `{ "risks":[{"id":"jwt-alg","severity":"high","mitigation":"pin alg"}], "evidence":[...] }`
* **Invariants:** severity ∈ enum; mitigations actionable.
* **CPOs/CAs:** `assess-risk`, CA: `SecurityReviewPass`.
* **Example:** “Assess JWT validation risks.” → RiskAssessment.
* **Matrix:** `...#view=risk`
* **Tests:** contract tests + checklist-based deterministic validators.

### CDT-10: `GlossaryEntry`

* **Definition:** Ubiquitous-language term definition with synonyms and references.
* **Canonical:** `{ "term":"session", "definition":"...", "domain":"auth", "refs":[SpanRef] }`
* **Invariants:** term unique within bounded context.
* **CPOs/CAs:** `extract-glossary`, CA: `OntologyInduction`.
* **Example:** “Extract glossary from ADR.” → entries.
* **Matrix:** `matrix://…/glossary/auth`
* **Tests:** diff against curated glossary; human-in-the-loop.

---

## 3.B Evidence, Provenance, Receipts (CDT-11 … CDT-20)

### CDT-11: `EvidenceSpan`

* **Definition:** Evidence anchored to a span/row/clause with interpretation label.
* **Canonical:** `{ "ref":SpanRef, "label":"issuer-check", "notes":"aud validated here" }`
* **Invariants:** ref resolvable; label from evidence taxonomy.
* **CPOs/CAs:** `extract-evidence`, CA: `EvidenceWeaving`.
* **Example:** “Return evidence for issuer validation.” → EvidenceSpan list.
* **Matrix:** `...#view=evidence`
* **Tests:** span validity + label contract.

### CDT-12: `Citation`

* **Definition:** A citation object suitable for inline rendering with stable anchors.
* **Canonical:** `{ "kind":"span|row|clause", "ref":..., "quote_hash":"..." }`
* **Invariants:** quote_hash matches content at snapshot.
* **CPOs/CAs:** `cite`, CA: `CitationSynthesis`.
* **Example:** “Cite the function verifying signature.” → Citation.
* **Matrix:** `...#cite=...`
* **Tests:** hash consistency checks.

### CDT-13: `ProvenanceNode`

* **Definition:** Node in provenance DAG (operation execution).
* **Canonical:** `{ "id":"p1", "op":"extract", "inputs":[...], "outputs":[...], "snapshot":"..." }`
* **Invariants:** DAG acyclic; inputs/outputs typed.
* **CPOs/CAs:** `emit-provenance`, CA: `ProvenanceDAGBuild`.
* **Example:** always emitted.
* **Matrix:** `matrix://…/prov/...`
* **Tests:** DAG integrity.

### CDT-14: `ProvenanceEdge`

* **Definition:** Edge indicating derivation dependency (dataflow/control).
* **Canonical:** `{ "from":"p1", "to":"p2", "kind":"data|control" }`
* **Invariants:** endpoints exist.
* **CPOs/CAs:** provenance ops.
* **Tests:** referential integrity.

### CDT-15: `ProvenanceDAG`

* **Definition:** Full derivation graph for an answer/decision.
* **Canonical:** `{ "nodes":[...], "edges":[...], "root":"p0" }`
* **Invariants:** connected from root; evidence references resolvable.
* **CPOs/CAs:** `minimize-provenance`, CA: `ProvenanceMinCut`.
* **Example:** “Explain why we chose JWT.” → DAG.
* **Matrix:** `...#view=prov`
* **Tests:** replay selected nodes; minimality heuristics.

### CDT-16: `Receipt`

* **Definition:** Replayable record of an effect call (query/command/infer) with inputs, contract, and outputs (or hash thereof).
* **Canonical:** `{ "corr":"c123", "op":"matrix.query", "uri":"...", "contract":"...", "output_hash":"..." }`
* **Invariants:** output_hash matches stored output.
* **CPOs/CAs:** `record-receipt`, CA: `ReceiptReplay`.
* **Example:** budgeted runs.
* **Matrix:** `matrix://…/receipts/...`
* **Tests:** deterministic replay.

### CDT-17: `Budget`

* **Definition:** Resource envelope: token budget, time budget, call limits, concurrency limits.
* **Canonical:** `{ "tokens":100000, "calls":200, "time_ms":300000, "max_parallel":256 }`
* **Invariants:** non-negative; decremented monotonically.
* **CPOs/CAs:** `with-budget`, CA: `BudgetedEvaluation`.
* **Example:** “Run under 20 inference calls.” → enforce.
* **Matrix:** `...#budget=...`
* **Tests:** exhaustion tests; graceful degradation checks.

### CDT-18: `SnapshotRef`

* **Definition:** Immutable reference to artifact content version.
* **Canonical:** `{ "uri":"...", "snapshot_id":"s123", "content_hash":"h...", "ts":"..." }`
* **Invariants:** content_hash consistent.
* **CPOs/CAs:** `snapshot`, CA: `SnapshotMaterialization`.
* **Example:** “Analyze latest snapshot.” → SnapshotRef.
* **Matrix:** `matrix://…#snapshot=s123`
* **Tests:** integrity checks.

### CDT-19: `SemanticKey`

* **Definition:** Canonical key for semantic caching (meaning-level fingerprint).
* **Canonical:** `{ "kind":"summary", "uri":"...", "snapshot":"s123", "env_hash":"e...", "shape":"level2" }`
* **Invariants:** deterministic given inputs + ENV.
* **CPOs/CAs:** `semantic-fingerprint`, CA: `SemanticMemoization`.
* **Example:** “Cache summary.” → key.
* **Matrix:** cache subsystem.
* **Tests:** idempotence + collision tests.

### CDT-20: `CacheEntry`

* **Definition:** Stored semantic artifact + validation gate.
* **Canonical:** `{ "key":SemanticKey, "value":CDT, "valid_until_snapshot":"s123", "validator":"..." }`
* **Invariants:** validator declared for semantic similarity.
* **CPOs/CAs:** cache ops, CA: `CacheWithValidationGate`.
* **Example:** reuse prior analyses.
* **Tests:** invalidation tests.

---

## 3.C Plans, Queries, Constraints (CDT-21 … CDT-30)

### CDT-21: `Intent`

* **Definition:** Canonical intent classification for a request (diagnose, summarize, transform…).
* **Canonical:** `{ "intent":"summarize", "confidence":0.93 }`
* **Invariants:** intent enum.
* **CPOs/CAs:** `classify-intent`, CA: `NLIntentRouting`.
* **Example:** “What does this file do?” → intent summarize/explain.
* **Matrix:** orchestrator routing.
* **Tests:** confusion-matrix tests on labeled dataset.

### CDT-22: `QueryPlan`

* **Definition:** Executable plan compiled from Omega IR (staged, costed, distributable).
* **Canonical:** `{ "stages":[{...}], "cost_estimate":{...}, "budgets":Budget }`
* **Invariants:** stages type-checked; budgets attached.
* **CPOs/CAs:** `compile-plan`, CA: `CostBasedPlanner`.
* **Example:** “Find auth handlers” → plan.
* **Matrix:** orchestrator executes over subtree.
* **Tests:** plan validity + cost monotonicity.

### CDT-23: `OmegaIR`

* **Definition:** Lisp-ish AST representing cognitive computation (first-class programs).
* **Canonical:** s-expressions + typed annotations.
* **Invariants:** well-formed; effect nodes carry contracts.
* **CPOs/CAs:** `optimize-ir`, CA: `PartialEvaluation`, `DCE`.
* **Example:** compile NL to OmegaIR.
* **Matrix:** stored as artifact `matrix://…/plans/...`
* **Tests:** IR round-trip + interpreter equivalence.

### CDT-24: `Constraint`

* **Definition:** Declarative invariant over artifact/view state (architecture/security/policy).
* **Canonical:** `{ "id":"auth.jwt.requires.stateless", "expr":"if auth=jwt then storage!=session", "severity":"high" }`
* **Invariants:** id unique; severity enum.
* **CPOs/CAs:** `check-constraint`, CA: `PropagatorFixpoint`.
* **Example:** “JWT implies stateless storage.” → constraint.
* **Matrix:** `matrix://…/constraints/...`
* **Tests:** satisfiable/unsatisfiable scenarios.

### CDT-25: `ConstraintNetwork`

* **Definition:** Propagator graph of connectors and constraints.
* **Canonical:** `{ "connectors":[...], "constraints":[...], "state":{...} }`
* **Invariants:** propagation terminates or bounded.
* **CPOs/CAs:** `propagate`, CA: `FixpointIteration`.
* **Example:** change auth strategy, propagate.
* **Matrix:** architecture node.
* **Tests:** convergence tests + contradiction detection.

### CDT-26: `ConstraintViolation`

* **Definition:** A violated constraint with counterexample evidence.
* **Canonical:** `{ "constraint_id":"...", "counterexample":{...}, "evidence":[...] }`
* **Invariants:** evidence required; severity inherited.
* **CPOs/CAs:** `explain-violation`, CA: `MinimalUnsatCore`.
* **Example:** “SQL sessions conflict with JWT.” → violation.
* **Matrix:** `...#view=violations`
* **Tests:** replay counterexample.

### CDT-27: `RewriteSpec`

* **Definition:** Contracted refactor/transform request specifying obligations.
* **Canonical:** `{ "pattern":"extract-function", "target":SpanRef, "obligations":["tests-pass","public-api-stable"] }`
* **Invariants:** obligations enum.
* **CPOs/CAs:** `rewrite`, CA: `RefactoringWithObligations`.
* **Example:** “Refactor to async/await.”
* **Matrix:** code node command.
* **Tests:** obligation check harness.

### CDT-28: `Patch`

* **Definition:** A transformation artifact (diff-like) with applicability conditions.
* **Canonical:** `{ "applies_to":SnapshotRef, "diff":"...", "touched_spans":[...] }`
* **Invariants:** applies_to matches; diff parses.
* **CPOs/CAs:** `apply-patch`, CA: `PatchValidation`.
* **Example:** “Generate fix + patch.”
* **Matrix:** `matrix://…/patches/...`
* **Tests:** apply/rollback; semantic diff tests.

### CDT-29: `ToolInvocation`

* **Definition:** Normalized tool call request (non-infer effect).
* **Canonical:** `{ "tool":"sql.exec", "args":{...}, "contract":"..." }`
* **Invariants:** args schema.
* **CPOs/CAs:** `invoke-tool`, CA: `ToolAdapter`.
* **Example:** execute SQL.
* **Matrix:** tool nodes.
* **Tests:** contract tests + stubbed tools.

### CDT-30: `PortContract`

* **Definition:** Capability signature set for a node port (query/command) including schemas.
* **Canonical:** `{ "capability":"view.ast", "input_schema":..., "output_schema":... }`
* **Invariants:** schemas valid; versioned.
* **CPOs/CAs:** `discover-capabilities`, CA: `CapabilityNegotiation`.
* **Example:** “Can you produce ASTView?”
* **Matrix:** component introspection.
* **Tests:** conformance tests.

---

## 3.D Code & Software Engineering CDTs (CDT-31 … CDT-44)

### CDT-31: `ASTView`

* **Definition:** Structured AST projection with stable node IDs and span refs.
* **Canonical:** `{ "language":"ts", "nodes":[{id,kind,children,span}], "symbols":[...] }`
* **Invariants:** node spans valid; tree acyclic.
* **CPOs/CAs:** `view(ast)`, `extract(symbols)`, CA: `StaticExtractionPass`.
* **Example:** “Create AST view for AuthService.ts.” → ASTView.
* **Matrix URI:** `matrix://…/AuthService.ts#view=ast`
* **Tests:** deterministic across runs; node count stable.

### CDT-32: `SymbolTable`

* **Definition:** Mapping of symbol IDs to declarations/uses with scope.
* **Canonical:** `{ "symbols":[{name,kind,decl_span,refs:[...] }] }`
* **Invariants:** references resolve; scopes nested.
* **CPOs/CAs:** `extract(symbols)`, CA: `NameResolution`.
* **Example:** “List exported functions.” → SymbolTable slice.
* **Matrix:** `...#view=symbols`
* **Tests:** compare to compiler output.

### CDT-33: `CallGraph`

* **Definition:** Directed graph of call edges with evidence spans.
* **Canonical:** `{ "nodes":[fnId], "edges":[{from,to,span}] }`
* **Invariants:** nodes exist; edges reference spans.
* **CPOs/CAs:** `extract(callgraph)`, CA: `GraphTraversalExplain`.
* **Example:** “Explain checkout flow.” → CallGraph + discourse.
* **Matrix:** `...#view=callgraph`
* **Tests:** deterministic on snapshot; graph sanity checks.

### CDT-34: `DependencyGraph`

* **Definition:** Module/package dependency graph with import edges.
* **Canonical:** `{ "modules":[...], "imports":[{from,to,kind}] }`
* **Invariants:** no missing modules.
* **CPOs/CAs:** `extract(imports)`, CA: `CycleDetection`.
* **Example:** “Find cycles.” → cycles list.
* **Matrix:** project node view.
* **Tests:** cycle detection regression.

### CDT-35: `APIContract`

* **Definition:** Request/response schema + auth + rate limits for endpoint/service.
* **Canonical:** `{ "method":"POST", "path":"/login", "auth":"bearer", "req_schema":..., "resp_schema":... }`
* **Invariants:** schemas valid; auth enum.
* **CPOs/CAs:** `extract(api)`, `check(contract)`, CA: `CompatibilityCheck`.
* **Example:** “What does /orders/list accept?” → APIContract.
* **Matrix:** `matrix://…/api/orders/list`
* **Tests:** contract tests vs implementation.

### CDT-36: `SecurityFinding`

* **Definition:** Structured security issue with CWE-like classification, evidence, severity.
* **Canonical:** `{ "id":"sf1", "kind":"jwt-alg-none", "severity":"high", "evidence":[...], "fix_suggestion":"..." }`
* **Invariants:** evidence required; severity enum.
* **CPOs/CAs:** `scan-security`, CA: `TaintLikeHeuristicSearch`.
* **Example:** “Check JWT verification correctness.” → findings.
* **Matrix:** `...#view=security`
* **Tests:** curated test corpus; false-positive budgeting.

### CDT-37: `RefactoringOpportunity`

* **Definition:** Recognized code smell + recommended refactoring pattern.
* **Canonical:** `{ "smell":"god-object", "target":SpanRef, "refactor":"extract-class", "severity":"med" }`
* **Invariants:** refactor from catalog.
* **CPOs/CAs:** `detect-smells`, CA: `PatternMatchScan`.
* **Example:** “Find nested callbacks >3 levels.” → opportunities.
* **Matrix:** `...#view=refactor-opps`
* **Tests:** snapshot-based regression.

### CDT-38: `TestSpec`

* **Definition:** Structured test intent including property/stateful scenarios.
* **Canonical:** `{ "kind":"property|unit|integration", "properties":[...], "fixtures":[...], "oracles":[...] }`
* **Invariants:** kind enum; properties typed.
* **CPOs/CAs:** `generate-tests`, CA: `PropertySynthesis`.
* **Example:** “Generate tests for payment invariants.” → TestSpec.
* **Matrix:** test generator node.
* **Tests:** compile/run harness; flakiness checks.

### CDT-39: `Property`

* **Definition:** Declarative invariant suitable for property-based testing.
* **Canonical:** `{ "forall":["amount","currency"], "implies":..., "oracle":"processPayment" }`
* **Invariants:** variables bound; oracle exists.
* **CPOs/CAs:** `extract-properties`, CA: `QuickCheckGenerator`.
* **Example:** “Successful transaction implies audit entry.” → Property.
* **Matrix:** `...#view=properties`
* **Tests:** falsification-driven; shrinker tests.

### CDT-40: `BuildGraph`

* **Definition:** Targets, dependencies, and build steps for CI/CD.
* **Canonical:** `{ "targets":[...], "deps":[...], "steps":[...] }`
* **Invariants:** acyclic or explicitly staged.
* **CPOs/CAs:** `extract(build)`, CA: `IncrementalBuildImpact`.
* **Example:** “What breaks if we change module X?” → impacted targets.
* **Matrix:** CI node.
* **Tests:** deterministic extraction.

### CDT-41: `CIResult`

* **Definition:** Typed record of CI run outcomes with logs and failing tests.
* **Canonical:** `{ "status":"fail", "failed_tests":[...], "log_refs":[...] }`
* **Invariants:** status enum; refs valid.
* **CPOs/CAs:** `diagnose-ci`, CA: `FailureTriage`.
* **Example:** “Summarize failing stage.” → CIResult summary.
* **Matrix:** `matrix://…/ci/runs/...`
* **Tests:** golden master on known failures.

### CDT-42: `ChangeSet`

* **Definition:** Set of modifications with rationale/provenance.
* **Canonical:** `{ "patches":[Patch], "rationale":..., "risks":[...] }`
* **Invariants:** patches apply to snapshots.
* **CPOs/CAs:** `plan-change`, CA: `SagaPlan`.
* **Example:** “Upgrade lodash across repos.” → ChangeSet.
* **Matrix:** workspace node.
* **Tests:** dry-run apply + rollback plan tests.

### CDT-43: `ArchitectureDecisionRecord` (ADRView)

* **Definition:** Structured ADR with decisions, alternatives, consequences.
* **Canonical:** `{ "context":..., "decision":Decision, "consequences":[...] }`
* **Invariants:** decision has provenance.
* **CPOs/CAs:** `extract-adr`, CA: `DecisionConsistencyCheck`.
* **Example:** “Extract ADR from doc.” → ADRView.
* **Matrix:** `...#view=adr`
* **Tests:** schema + divergence detection vs code.

### CDT-44: `ModulePurpose`

* **Definition:** Structured “purpose” object for code module with boundaries and responsibilities.
* **Canonical:** `{ "module":"AuthService", "responsibilities":[...], "non_responsibilities":[...], "interfaces":[...] }`
* **Invariants:** responsibilities non-overlapping under taxonomy.
* **CPOs/CAs:** `infer-purpose`, CA: `PurposeInduction`.
* **Example:** “What does reconciler.ts do?” → ModulePurpose.
* **Matrix:** `...#view=purpose`
* **Tests:** reviewer-validated goldens + drift alerts.

---

## 3.E Data / BI / Warehousing CDTs (CDT-45 … CDT-52)

### CDT-45: `RelationalView`

* **Definition:** Deterministic projection of an artifact into tables/rows/columns.
* **Canonical:** `{ "tables":[{name,columns:[{name,type,unit}], rows_ref:"..."}] }`
* **Invariants:** schema consistent; row IDs stable within snapshot.
* **CPOs/CAs:** `view(relational)`, CA: `ViewMaterialization`.
* **Example:** “Interpret CSV as table with inferred schema.” → RelationalView.
* **Matrix:** `...#view=relational`
* **Tests:** schema inference regression.

### CDT-46: `SQLAST`

* **Definition:** Typed SQL abstract syntax tree under a schema contract.
* **Canonical:** `{ "select":[...], "from":"t", "where":..., "limit":... }`
* **Invariants:** references valid in schema.
* **CPOs/CAs:** `nl->sqlast`, CA: `ConstrainedDecoding`.
* **Example:** “Top 10 by date desc.” → SQLAST.
* **Matrix:** query planner.
* **Tests:** parse/pretty-print round-trip.

### CDT-47: `MetricDefinition`

* **Definition:** KPI metric with formula, dimensions, time grain, owners.
* **Canonical:** `{ "name":"ARR", "formula":"sum(contract_value)", "grain":"month", "dimensions":["region"] }`
* **Invariants:** formula references known facts; grain enum.
* **CPOs/CAs:** `extract-metrics`, CA: `MetricLineage`.
* **Example:** “Define revenue impact metric.” → MetricDefinition.
* **Matrix:** `matrix://…/metrics/arr`
* **Tests:** lineage validation.

### CDT-48: `DimensionalModel`

* **Definition:** Fact/dimension model suggestion with conformance.
* **Canonical:** `{ "facts":[...], "dimensions":[...], "conformed_dims":[...] }`
* **Invariants:** dimension keys consistent.
* **CPOs/CAs:** `propose-dw-model`, CA: `DimModelSynthesis`.
* **Example:** “Model orders + customers.” → DimensionalModel.
* **Matrix:** DW node.
* **Tests:** schema conformance tests.

### CDT-49: `Anomaly`

* **Definition:** Anomaly event with detection method and explanation.
* **Canonical:** `{ "series":"temp", "start":"...", "delta":2.1, "method":"changepoint", "explanation":... }`
* **Invariants:** method enum; evidence to underlying points.
* **CPOs/CAs:** `detect-anomaly`, CA: `ChangePointSegmentation`.
* **Example:** “Warmer than usual this month.” → anomalies.
* **Matrix:** time-series node.
* **Tests:** synthetic anomalies.

### CDT-50: `ForecastNarrative`

* **Definition:** Narrative explanation of forecast with uncertainty bands.
* **Canonical:** `{ "forecast":[{t,y,p10,p90}], "story":..., "assumptions":[...] }`
* **Invariants:** bands consistent; assumptions listed.
* **CPOs/CAs:** `forecast+explain`, CA: `NarrativeSynthesisWithEvidence`.
* **Example:** “Revenue impact if Acme lost.” → forecast narrative.
* **Matrix:** finance node.
* **Tests:** backtesting harness + narrative consistency.

### CDT-51: `LineageGraph`

* **Definition:** Graph from source data to derived outputs (like semantic provenance for BI).
* **Canonical:** `{ "nodes":[...], "edges":[...] }`
* **Invariants:** DAG preferred; edges typed.
* **CPOs/CAs:** `compute-lineage`, CA: `LineageMinimization`.
* **Example:** “Where does ARR come from?” → lineage.
* **Matrix:** `...#view=lineage`
* **Tests:** lineage diff across schema changes.

### CDT-52: `RowEvidence`

* **Definition:** Evidence anchored to specific rows/cells.
* **Canonical:** `{ "table":"...", "row_id":"r42", "cells":{"amount":100}, "notes":"..." }`
* **Invariants:** row exists; cell types match schema.
* **CPOs/CAs:** `cite-rows`, CA: `EvidenceWeaving`.
* **Example:** “Cite top 3 customers by spend.” → RowEvidence.
* **Matrix:** `...#row=r42`
* **Tests:** row lookup determinism.

---

## 3.F Legal / Policy / Compliance CDTs (CDT-53 … CDT-58)

### CDT-53: `ClauseView`

* **Definition:** Structured clauses extracted from a document with numbering and spans.
* **Canonical:** `{ "clauses":[{id:"3.2", text:"...", span:SpanRef, type:"obligation"}] }`
* **Invariants:** clause ids unique; spans resolve.
* **CPOs/CAs:** `view(clauses)`, CA: `ClauseSegmentation`.
* **Example:** “Extract obligations from contract.” → ClauseView.
* **Matrix:** `matrix://…/contract.pdf#view=clauses`
* **Tests:** clause boundary regression.

### CDT-54: `Obligation`

* **Definition:** Duty statement with actor, action, deadline, conditions.
* **Canonical:** `{ "actor":"Vendor", "action":"provide support", "deadline":"30d", "conditions":[...] }`
* **Invariants:** actor resolvable; deadline normalized.
* **CPOs/CAs:** `extract-obligations`, CA: `DeonticExtraction`.
* **Example:** “List vendor obligations.” → obligations.
* **Matrix:** clause node.
* **Tests:** human-reviewed goldens.

### CDT-55: `Permission`

* **Definition:** Allowed action in policy with scope.
* **Canonical:** `{ "actor":"Customer", "may":"terminate", "conditions":[...] }`
* **Invariants:** deontic type.
* **CPOs/CAs:** `extract-permissions`, CA: `DeonticExtraction`.
* **Example:** “What may the customer do?” → permissions.
* **Tests:** contract tests.

### CDT-56: `Prohibition`

* **Definition:** Forbidden action in policy with scope.
* **Canonical:** `{ "actor":"Vendor", "must_not":"share data", "exceptions":[...] }`
* **Invariants:** exceptions explicit.
* **CPOs/CAs:** `extract-prohibitions`, CA: `PolicyConsistencyCheck`.
* **Example:** “Any prohibitions on data sharing?” → list.
* **Tests:** contradiction checks.

### CDT-57: `PolicyDecision`

* **Definition:** Yes/no/conditional decision with rationale and citations.
* **Canonical:** `{ "decision":"allow|deny|conditional", "rationale":..., "citations":[...] }`
* **Invariants:** citations required.
* **CPOs/CAs:** `decide-policy`, CA: `ConstraintSatisfaction`.
* **Example:** “Can we store PII in logs?” → decision.
* **Tests:** replay on policy snapshots.

### CDT-58: `RedlinePatch`

* **Definition:** Proposed doc edits with clause-level diffs and reasons.
* **Canonical:** `{ "applies_to":SnapshotRef, "edits":[{clause:"3.2", replace_with:"...", reason:"..."}] }`
* **Invariants:** clause exists; reasons cite risk/obligation.
* **CPOs/CAs:** `rewrite-doc`, CA: `TransformWithObligations`.
* **Example:** “Redline indemnity clause.” → patch.
* **Tests:** apply patch to doc model; review workflow.

---

## 3.G Ops / Logs / Incident CDTs (CDT-59 … CDT-64)

### CDT-59: `LogEvent`

* **Definition:** Normalized log entry with timestamp, service, severity, fields.
* **Canonical:** `{ "ts":"...", "service":"api", "level":"error", "msg":"...", "fields":{...} }`
* **Invariants:** ts normalized; level enum.
* **CPOs/CAs:** `parse-logs`, CA: `LogNormalization`.
* **Example:** “Parse last hour logs.” → LogEvents.
* **Matrix:** `matrix://…/logs/...#view=events`
* **Tests:** parser regression.

### CDT-60: `TraceView`

* **Definition:** Distributed trace graph of spans with causal ordering.
* **Canonical:** `{ "trace_id":"...", "spans":[{id,parent,service,ts,dur,tags}] }`
* **Invariants:** tree/graph consistent; parent exists.
* **CPOs/CAs:** `view(trace)`, CA: `CriticalPathAnalysis`.
* **Example:** “Why is latency high?” → trace analysis.
* **Matrix:** tracing node.
* **Tests:** synthetic traces.

### CDT-61: `TimelineView`

* **Definition:** Temporal ordering of events with narrative labels.
* **Canonical:** `{ "events":[{ts,label,ref,evidence}] }`
* **Invariants:** sorted by ts; refs resolvable.
* **CPOs/CAs:** `build-timeline`, CA: `CausalTimeline`.
* **Example:** “What happened during incident?” → timeline.
* **Matrix:** incident node.
* **Tests:** ordering tests; missing-event detection.

### CDT-62: `IncidentReport`

* **Definition:** Structured postmortem: impact, root cause, contributing factors, actions.
* **Canonical:** `{ "impact":..., "root_cause":Claim, "timeline":TimelineView, "actions":[...] }`
* **Invariants:** root_cause evidence-backed; actions tracked.
* **CPOs/CAs:** `generate-postmortem`, CA: `RootCauseTree`.
* **Example:** “Generate postmortem from logs.” → report.
* **Matrix:** `matrix://…/incidents/...`
* **Tests:** template completeness checks.

### CDT-63: `RunbookStep`

* **Definition:** Operational instruction with preconditions, commands, expected signals.
* **Canonical:** `{ "step":"...", "pre":[...], "commands":[...], "verify":[...] }`
* **Invariants:** verify signals typed.
* **CPOs/CAs:** `extract-runbook`, CA: `PlanSynthesis`.
* **Example:** “Create runbook for cache flush.” → steps.
* **Matrix:** ops node.
* **Tests:** dry-run in staging; idempotency.

### CDT-64: `SLOViolation`

* **Definition:** SLO breach description with metric evidence and suspected causes.
* **Canonical:** `{ "slo":"p95_latency", "breach_window":[...], "evidence":[RowEvidence], "hypotheses":[Claim] }`
* **Invariants:** evidence to metric points.
* **CPOs/CAs:** `detect-slo-violations`, CA: `Anomaly+Diagnosis`.
* **Example:** “SLO violated yesterday—why?” → violation object.
* **Matrix:** monitoring node.
* **Tests:** synthetic breach scenarios.

---

# 4) REQUIRED DELIVERABLE: Cognitive Primitive Instruction Set (CPO ISA)

## 4.1 CPO Categories

* **Pure structural** (deterministic): view materialization, parsing, indexing.
* **Effectful semantic**: inference, search, classification.
* **Effectful distributed**: Matrix queries/commands.
* **Meta**: budget, caching, provenance.

I’ll define ~28 primitives. Each includes signature + effect class + contract + snippet.

### Notation

* `⊢ op : (ENV × A) -> CE[B]` effectful
* `⊢ op : (A) -> B` pure

---

## 4.2 Core CPOs (with typed signatures)

### CPO-01 `capabilities`

* **Sig:** `capabilities : URI -> CE[CapabilitySet]`
* **Effect:** Matrix query (distributed)
* **Contract:** `CapabilitySet` schema
* **Snippet:**

```lisp
(effect matrix.query :uri U :query '(capabilities) :contract CapabilitySet)
```

### CPO-02 `snapshot`

* **Sig:** `snapshot : URI -> CE[SnapshotRef]`
* **Effect:** Matrix query
* **Contract:** SnapshotRef
* **Use:** determinism boundary + cache key.

### CPO-03 `view`

* **Sig:** `view : (ENV × URI × ViewSpec) -> CE[ViewCDT]`
* **Effect:** Matrix query (may be deterministic local)
* **Contract:** depends on ViewSpec (e.g., ASTView)

```lisp
(effect matrix.query :uri U :query (list 'view ViewSpec) :contract ASTView)
```

### CPO-04 `canonicalize` (MNF)

* **Sig:** `canonicalize : (ENV × CDT) -> CDT` (pure if no retrieval)
* **Effect:** pure (ideally) / else CE if requires ontology lookup
* **Contract:** same CDT.

### CPO-05 `classify`

* **Sig:** `classify : (ENV × ViewCDT × Taxonomy) -> CE[TagSet]`
* **Effect:** inference
* **Contract:** TagSet

```lisp
(effect infer.op :env ENV :contract TagSet :prompt (list 'classify View Taxonomy))
```

### CPO-06 `summarize`

* **Sig:** `summarize : (ENV × ViewCDT × SummarySpec) -> CE[Summary]`
* **Effect:** inference (may be deterministic if rule-based)
* **Contract:** Summary

### CPO-07 `extract`

* **Sig:** `extract : (ENV × ViewCDT × ExtractSpec) -> CE[ExtractedCDT]`
* **Effect:** mixed: deterministic extraction if possible; else inference
* **Contract:** depends (SymbolTable, ClauseView, MetricDefinition…)

### CPO-08 `judge`

* **Sig:** `judge : (ENV × Claim × EvidenceView) -> CE[Claim]` (refines status/confidence)
* **Effect:** inference + evidence checking
* **Contract:** Claim

### CPO-09 `questionize`

* **Sig:** `questionize : (ENV × Goal × ViewCDT) -> CE[List[Question]]`
* **Effect:** inference
* **Contract:** list of Question

### CPO-10 `route`

* **Sig:** `route : (ENV × Question × CandidateURIs) -> CE[List[URI]]`
* **Effect:** inference + capability discovery
* **Contract:** URI list

### CPO-11 `plan`

* **Sig:** `plan : (ENV × Question) -> CE[OmegaIR]`
* **Effect:** inference (constrained decoding)
* **Contract:** OmegaIR (well-formed)

### CPO-12 `compile`

* **Sig:** `compile : (ENV × OmegaIR × TopologyInfo) -> CE[QueryPlan]`
* **Effect:** compute (planner) + possible infer for heuristics
* **Contract:** QueryPlan

### CPO-13 `execute-plan`

* **Sig:** `execute-plan : (ENV × QueryPlan) -> CE[Answer]`
* **Effect:** distributed Matrix queries + inference at leaves
* **Contract:** Answer

### CPO-14 `rewrite`

* **Sig:** `rewrite : (ENV × URI × RewriteSpec) -> CE[Patch]`
* **Effect:** inference + command generation
* **Contract:** Patch

### CPO-15 `apply`

* **Sig:** `apply : (ENV × URI × Patch) -> CE[CommandReceipt]`
* **Effect:** Matrix command
* **Contract:** Receipt

### CPO-16 `verify`

* **Sig:** `verify : (ENV × URI × ObligationSpec) -> CE[VerificationReport]`
* **Effect:** tool calls + inference for explanation
* **Contract:** VerificationReport (define as record)

### CPO-17 `diff`

* **Sig:** `diff : (ENV × SnapshotRef × SnapshotRef × DiffSpec) -> CE[DiffCDT]`
* **Effect:** deterministic + optional semantic
* **Contract:** DiffCDT

### CPO-18 `propagate`

* **Sig:** `propagate : (ENV × ConstraintNetwork × Change) -> CE[ConstraintNetwork]`
* **Effect:** compute (fixpoint) + optional infer for mapping
* **Contract:** ConstraintNetwork

### CPO-19 `explain`

* **Sig:** `explain : (ENV × Answer|Decision|Violation) -> CE[ProvenanceDAG]`
* **Effect:** may be compute if provenance stored
* **Contract:** ProvenanceDAG

### CPO-20 `cite`

* **Sig:** `cite : (ENV × EvidenceSpan|RowEvidence) -> Citation`
* **Effect:** pure if evidence already resolved
* **Contract:** Citation

### CPO-21 `coerce-enum`

* **Sig:** `coerce-enum : (ENV × RawText × EnumSpec) -> CE[EnumValue]`
* **Effect:** inference under contract
* **Contract:** enum
* **Purpose:** eliminate brittle “yes/no” strings.

### CPO-22 `coerce-schema`

* **Sig:** `coerce-schema : (ENV × RawText|Any × JSONSchema) -> CE[CDT]`
* **Effect:** inference with strict JSON schema checking.

### CPO-23 `consensus`

* **Sig:** `consensus : (ENV × List[Answer] × ConsensusPolicy) -> CE[Answer]`
* **Effect:** inference + voting; may call judge.
* **Contract:** Answer

### CPO-24 `cache-get`

* **Sig:** `cache-get : SemanticKey -> CE[Maybe[CacheEntry]]`
* **Effect:** storage read
* **Contract:** CacheEntry

### CPO-25 `cache-put`

* **Sig:** `cache-put : CacheEntry -> CE[Unit]`
* **Effect:** storage write

### CPO-26 `budget-with`

* **Sig:** `budget-with : (Budget × CE[T]) -> CE[T]` (handler combinator)
* **Effect:** meta
* **Purpose:** enforce ceilings + graceful degradation.

### CPO-27 `record-receipt`

* **Sig:** `record-receipt : Receipt -> CE[Unit]`
* **Effect:** storage write

### CPO-28 `redact`

* **Sig:** `redact : (ENV × CDT × RedactionPolicy) -> CDT` (pure where possible)
* **Effect:** policy
* **Purpose:** least-privilege outputs.

---

## 4.3 Adapters (Safe Coercions)

### Yes/No coercion

```lisp
(define (yesno env text)
  (effect infer.op
    :env env
    :contract '(enum yes no unknown)
    :prompt (list 'coerce-yesno text)))
```

### Enum coercion

```lisp
(define (to-tag env text taxonomy)
  (effect infer.op
    :env env
    :contract TagSet
    :prompt (list 'classify text taxonomy)))
```

### Schema coercion

```lisp
(define (to-sqlast env nl schema)
  (effect infer.op
    :env env
    :contract SQLAST
    :prompt (list 'nl->sqlast nl schema)))
```

### Evidence extraction gate

```lisp
(define (must-cite env claim evidence-view)
  (let ((judged (effect infer.op :env env :contract Claim
                    :prompt (list 'judge claim evidence-view))))
    (assert (not (null? (get judged 'evidence))))
    judged))
```

---

## 4.4 Kleisli Composition (Because ENV + Effects)

Cognitive operators are generally `A -> CE[B]`, so composition is Kleisli:

```lisp
(define (kcompose f g)
  (lambda (x)
    (bind (f x) g)))

;; Example: summarize then extract decisions from summary
(define summarize>extract-decisions
  (kcompose (lambda (view) (summarize ENV view Spec))
            (lambda (sum)  (extract ENV sum DecisionSpec))))
```

This is exactly where FP design laws become “approximate” unless stabilized by MNF + contracts.

---

# 5) REQUIRED DELIVERABLE: Cognitive Algorithms Library (CA) — 30 Algorithms

Each CA includes: definition, invariants, cost model, failure modes, Omega sketch, distribution.

I’ll group by class and keep each “card” compact but explicit.

---

## 5.A Search / Nondeterminism / AMB

### CA-01 Generate-and-Test (bounded)

* **Definition:** Enumerate candidates, validate via constraints/judges.
* **Invariants:** candidates finite (or bounded); validator total under budget.
* **Cost:** `O(k * (validate_cost))` inference dominates.
* **Failure modes:** combinatorial explosion → mitigate with pruning + heuristics.
* **Omega sketch:**

```lisp
(define (gen-test env gen validate k)
  (let loop ((i 0) (acc '()))
    (if (or (= i k) (null? gen)) acc
      (let ((c (gen)))
        (if (validate c) (loop (+ i 1) (cons c acc))
            (loop (+ i 1) acc))))))
```

* **Distribution:** generator can be distributed to nodes; validator local.

### CA-02 Beam Search over Hypotheses

* **Invariant:** beam width `B` constant; score comparable.
* **Cost:** `O(depth * B * eval)` with eval as infer/judge.
* **Mitigation:** use deterministic features for scoring first.

### CA-03 Best-First over Evidence Graph

* **Invariant:** priority queue monotonic in score.
* **Use:** root-cause diagnosis; explore most plausible causes first.
* **Distribution:** evidence extraction local; queue global.

### CA-04 Branch-and-Bound

* **Invariant:** upper bound admissible.
* **Use:** search with cost budgets; prune unpromising branches.

### CA-05 AMB Nondeterminism with `require`

* **Invariant:** backtracking respects constraints.
* **Omega sketch:**

```lisp
(define (amb . choices) (choose-nondeterministically choices))
(define (require p) (if (not p) (fail)))
```

* **Mitigation:** budget-limited backtracking; memoize failed partials.

---

## 5.B Constraint Propagation / Fixpoints

### CA-06 Propagator Fixpoint

* **Definition:** Iterate constraint propagation until no new info.
* **Invariant:** monotone information lattice (preferred).
* **Cost:** `O(iterations * edges)`; ensure bounded iterations.
* **Failure:** oscillation/non-monotone constraints → stratify or cap.
* **Omega:**

```lisp
(define (fixpoint step state max)
  (let loop ((s state) (i 0))
    (let ((s2 (step s)))
      (if (or (= i max) (equal? s2 s)) s2
          (loop s2 (+ i 1))))))
```

* **Distribution:** propagate within architecture node; query subnodes for facts.

### CA-07 Contradiction Detection

* **Invariant:** detect unsat when connector has conflicting assignments.
* **Mitigation:** produce minimal unsat core (CA-08).

### CA-08 Minimal Unsat Core (approx)

* **Definition:** Find small set of constraints causing contradiction.
* **Cost:** potentially expensive; approximate via greedy deletion.
* **Output:** `ConstraintViolation` with counterexample.

---

## 5.C Normalization / Equivalence / Consensus

### CA-09 Meaning Normalization Pipeline

* **Invariant:** idempotent normalization.
* **Use:** stable caching + equality.
* **Failure:** ENV mismatch → include env_hash in key.

### CA-10 Bidirectional Entailment Check (approx)

* **Definition:** check A⇒B and B⇒A under evidence/judge.
* **Use:** semantic equivalence after refactor.
* **Mitigation:** property tests + golden master.

### CA-11 Self-Consistency Voting

* **Definition:** run inference multiple times or via multiple models; vote.
* **Invariant:** contract equality after normalization.
* **Cost:** multiplies inference calls; use only for high-stakes.
* **Output:** `consensus` answer with confidence.

### CA-12 Evidence-Minimizing Explanation

* **Definition:** build explanation with minimal evidence spans supporting claim.
* **Use:** compact proofs; reduce context.

---

## 5.D Planning / HTN / Preconditions

### CA-13 HTN-like Plan Synthesis

* **Definition:** decompose goal into methods until primitives.
* **Invariant:** methods cover goal; preconditions checkable.
* **Failure:** missing method → questionize to fill gaps.

### CA-14 Pre/Postcondition Checking

* **Definition:** verify obligations before applying patch.
* **Use:** refactor correctness gating.

### CA-15 Saga / Compensation Plan

* **Definition:** for multi-step changes, attach compensations.
* **Use:** cross-project dependency upgrades.
* **Failure:** partial failures → compensate; idempotent commands.

---

## 5.E Compilation / Optimization / Scheduling

### CA-16 NL → OmegaIR Compilation (constrained decoding)

* **Invariant:** output is well-formed OmegaIR with effect nodes carrying contracts.
* **Failure:** hallucinated ops → validate capability set.

### CA-17 Cost-Based Planner

* **Definition:** reorder predicates: cheap deterministic filters first.
* **Invariant:** semantics preserved (or explicitly approximate).
* **Use:** avoid 50k LLM calls.

### CA-18 Applicative Batching

* **Definition:** batch independent inference calls.
* **Use:** classify many files with one batched contract (if supported).
* **Failure:** context overflow in batch → split adaptively.

### CA-19 CSE (Common Subexpression Elimination) for Cognition

* **Definition:** reuse identical semantic subcalls via semantic keys.
* **Invariant:** same snapshot + env_hash.
* **Effect:** massive cost reduction.

### CA-20 DCE (Dead Cognitive Elimination)

* **Definition:** remove plan stages whose outputs are unused.
* **Use:** keep plans minimal.

### CA-21 Partial Evaluation

* **Definition:** precompute deterministic parts; specialize to current schema/topology.
* **Use:** stable compiled plans.

---

## 5.F Abstract Machine / Bytecode Execution

### CA-22 Cognitive Bytecode VM

* **Instructions:** `INFER`, `QUERY`, `BRANCH`, `FORK`, `JOIN`, `CACHE_GET`, `CACHE_PUT`, `ASSERT`, `EMIT_PROV`.
* **Invariant:** every instruction consumes budget and emits receipts.
* **Use:** reproducible execution and debugging.

### CA-23 Streaming Scatter–Gather Executor

* **Definition:** distribute queries to subtree nodes; stream partial results.
* **Invariant:** bounded parallelism; backpressure.
* **Failure:** thundering herd → concurrency limits + sampling.

---

## 5.G Provenance / Receipts / Replay

### CA-24 Provenance DAG Construction

* **Invariant:** DAG acyclic; nodes linked to receipts.
* **Use:** explainability, audits.

### CA-25 Provenance Minimization

* **Definition:** remove redundant nodes/evidence while preserving explainability.
* **Use:** compact reports.

### CA-26 Receipt Replay

* **Definition:** deterministic re-execution using recorded outputs for effects.
* **Invariant:** snapshot stable; receipts match.

---

## 5.H Caching / Similarity with Validation Gates

### CA-27 Semantic Memoization

* **Definition:** cache outputs keyed by SemanticKey (snapshot+env+shape).
* **Invariant:** cache hit must validate (or be treated as hypothesis).

### CA-28 Similarity Cache with Validation

* **Definition:** reuse “close” cache entries only after validator confirms equivalence.
* **Use:** similar code patterns across repos.
* **Failure:** false equivalence → strict validator + evidence.

---

## 5.I Concurrency / Reliability (EIP-grade)

### CA-29 Idempotent Receiver + Retry with Jitter

* **Invariant:** commands carry idempotency keys.
* **Use:** safe distributed commands (apply patch).
* **Failure:** duplicate apply → no-op.

### CA-30 Resequencing + Causal Ordering

* **Definition:** reorder events by causality/time before summarization.
* **Use:** incident timelines; avoid incoherent narratives.

---

# 6) REQUIRED DELIVERABLE: Pattern-Language Mapping Tables

I’ll do this in two layers:

1. a compact mapping table per family
2. pattern cards (each includes: classical definition, cognitive equivalent in CDT/CPO/CA, Omega skeleton, Matrix usage, use case)

---

## 6.A Functional Programming Patterns

### Mapping Table (FP → Cognitive Equivalent)

| Classical Pattern  | Cognitive Equivalent                           | Primary CDT/CPO/CA                                        |
| ------------------ | ---------------------------------------------- | --------------------------------------------------------- |
| Functor `map`      | broadcast apply CPO across nodes/views         | `TagSet`, `Summary`; CPO `classify/summarize`; CA-23      |
| Applicative        | batch independent inferences deterministically | `QueryPlan`; CPO `compile`; CA-18                         |
| Monad/Kleisli      | sequence effectful cognition with ENV          | `Answer`; CPO `bind`; CA-22                               |
| Fold/Monoid        | associative synthesis of partial meaning       | `Summary`, `IncidentReport`; CPO `aggregate`; CA-01/CA-23 |
| Streams            | lazy traversal over large artifact spaces      | `TimelineView`; CA-23                                     |
| Parser combinators | constrained decoding to ASTs/contracts         | `SQLAST`, `OmegaIR`; CA-16                                |
| Lenses/Zippers     | focus+edit substructures w/ invariants         | `ASTView`, `Patch`; CA-14                                 |
| Memoization        | semantic caching keyed by MNF+ENV              | `SemanticKey`; CA-27                                      |

#### FP Pattern Cards (selected)

**Functor/map**

1. Classical: apply f to each element.
2. Cognitive: apply `classify/summarize/extract` to each node in a subtree (scatter).
3. Omega:

```lisp
(define (map-nodes env uris f)
  (par-map (lambda (u) (f u)) uris))
```

4. Matrix: `matrix.query` per URI, bounded parallelism.
5. Use case: tag all files by role; summarize each module.

**Applicative/batching**

* Cognitive equivalent: batch N independent `infer` calls under a shared contract (or vectorized call).
* Use: classify 100 files → TagSet each, in one batched request if possible.

**Monad/Kleisli**

* Cognitive: `view -> extract -> judge -> decide -> emit provenance`, each step effectful.

**Fold/Monoid**

* Cognitive: hierarchical summarization that’s associative so you can stream partial summaries.

---

## 6.B Enterprise Integration Patterns (EIP/EAI)

### Mapping Table (EIP → Cognitive Equivalent)

| EIP Pattern          | Cognitive Equivalent                  | CDT/CPO/CA                                           |
| -------------------- | ------------------------------------- | ---------------------------------------------------- |
| Content-Based Router | route queries by capability + tags    | CDT `Intent`, `TagSet`; CPO `route`; CA-17           |
| Message Translator   | NL ↔ OmegaIR / schema coercion        | `OmegaIR`, `SQLAST`; CPO `plan/coerce-schema`; CA-16 |
| Splitter             | decompose question into subquestions  | `Question`; CPO `questionize`; CA-13                 |
| Aggregator           | fold answers into synthesis           | `Answer`, `Summary`; CPO `aggregate`; CA-23          |
| Resequencer          | reorder evidence/events for coherence | `TimelineView`; CA-30                                |
| Claim Check          | externalize big context, pass refs    | `SpanRef`, `SnapshotRef`; MNF discipline             |
| Idempotent Receiver  | safe repeated commands                | `Receipt`; CA-29                                     |
| Saga/Compensation    | reversible multi-step transformations | `ChangeSet`; CA-15                                   |
| Pub-Sub              | streaming results/events              | Matrix event port                                    |
| Scatter–Gather       | distributed semantic evaluation       | CA-23                                                |

#### EIP Pattern Cards (selected)

**Claim Check**

1. Classical: store payload, pass token.
2. Cognitive: store full artifact in node cache; orchestrator passes only `SpanRef/SnapshotRef`.
3. Omega:

```lisp
(let ((snap (effect matrix.query :uri U :query '(snapshot) :contract SnapshotRef)))
  (span-ref :uri U :snapshot (get snap 'snapshot_id) :range ...))
```

4. Matrix: `snapshot()` + `view()` used instead of moving data.
5. Use case: huge repo analysis without central context overflow.

**Scatter–Gather**

* Cognitive: broadcast semantic predicate to all descendants; gather answers as stream; stop early on top‑k.

---

## 6.C GoF Patterns

### Mapping Table (GoF → Cognitive Equivalent)

| GoF                     | Cognitive Equivalent                              | CDT/CPO/CA                                  |
| ----------------------- | ------------------------------------------------- | ------------------------------------------- |
| Strategy                | choose deterministic vs infer vs hybrid reasoning | ENV + CPO strategy; CA-17                   |
| Command                 | represent semantic action as replayable artifact  | `ToolInvocation`, `Patch`, `Receipt`; CA-26 |
| Observer                | invalidate views/caches on artifact changes       | events + `CacheEntry`; CA-27                |
| Chain of Responsibility | staged analysis: cheap→expensive                  | CA-17 planner + CPO escalation              |
| Composite               | artifact hierarchy (directories/projects)         | Matrix topology                             |
| Visitor                 | traverse node graph applying ops                  | CA-23 executor                              |
| Adapter                 | translate schemas/ontologies                      | CPO `coerce-schema`; CA-16                  |
| Facade                  | stable high-level cognitive API                   | “agent:compress-codebase” etc.              |
| Proxy                   | lazy remote artifact access + caching             | `SnapshotRef`, cache                        |
| Decorator               | wrap nodes with provenance/budget/redaction       | meta CPOs                                   |
| Interpreter             | OmegaIR evaluator / VM                            | CA-22                                       |
| Builder/AbstractFactory | construct node types + plans                      | CDT `PortContract`                          |

#### GoF Pattern Card (example: Strategy)

1. Classical: swap algorithm at runtime.
2. Cognitive: choose reasoning tier (static analysis, heuristic rules, infer).
3. Omega:

```lisp
(define (summarize* env view)
  (case (get env 'reasoning_tier)
    ((tier0) (deterministic-summary view))
    ((tier1) (rule-summary view))
    (else (summarize env view Spec))))
```

4. Matrix: capability discovery decides which tier is available locally.
5. Use case: security scan uses deterministic checks first, then infer only on suspicious sinks.

---

## 6.D DDD / Enterprise Application Patterns

### Mapping Table (DDD/EAA → Cognitive Equivalent)

| Pattern               | Cognitive Equivalent                               | CDT/CPO/CA                        |
| --------------------- | -------------------------------------------------- | --------------------------------- |
| Aggregate             | node as invariant boundary with local state        | Artifact node + ConstraintNetwork |
| Repository            | semantic store of CDTs keyed by snapshot           | CacheEntry + Provenance store     |
| Domain Event          | artifact-changed / snapshot-invalidated            | Matrix event port                 |
| Bounded Context       | ENV+Ontology scope; prevents meaning bleed         | ENV + GlossaryEntry               |
| Anti-Corruption Layer | schema/ontology adapter between systems            | CPO `coerce-schema`               |
| CQRS                  | separate query (views) from commands (patch/apply) | query/command ports               |
| Event Sourcing        | provenance as event log of cognition               | Receipts + ProvenanceDAG          |
| Specification         | constraints as executable specs                    | Constraint CDTs + CA-06           |
| Unit of Work          | ChangeSet + receipts for atomic operations         | ChangeSet + CA-15                 |

---

## 6.E POSA / Architectural Patterns

### Mapping Table (POSA → Cognitive Equivalent)

| POSA              | Cognitive Equivalent                      | CDT/CPO/CA                         |
| ----------------- | ----------------------------------------- | ---------------------------------- |
| Layers            | deterministic layer under semantic layer  | view materialization vs infer      |
| Pipes-and-Filters | *typed* cognitive combinators, not mush   | CPO ISA + CA-22                    |
| Blackboard        | shared hypothesis/evidence store          | ProvenanceDAG + Claim set          |
| Broker            | Matrix URI + port indirection             | Matrix protocol                    |
| Microkernel       | core runtime + pluggable node types       | node plugins (FileNode, TableNode) |
| Reactor           | event-driven invalidation + recomputation | Matrix events + cache              |
| Interpreter       | Omega IR executor                         | CA-22                              |

---

## 6.F CI / Testing Patterns

### Mapping Table (Testing → Cognitive Equivalent)

| Pattern              | Cognitive Equivalent               | CDTs/CAs                              |
| -------------------- | ---------------------------------- | ------------------------------------- |
| Golden Master        | snapshot-tied expected CDTs        | Summary, ClauseView; CA-26            |
| Contract Tests       | schema validation for all effects  | Semantic Contract                     |
| Metamorphic Tests    | paraphrase invariance under ENV    | MNF + CA-09                           |
| Replay Receipts      | deterministic run via receipts     | Receipt + CA-26                       |
| Determinism Harness  | tier-0 checks for stable views     | View tests                            |
| Differential Testing | before/after semantics comparisons | Diff + CA-10                          |
| Property-Based Tests | logic rules generate tests         | Property + CA-? (QuickCheckGenerator) |

---

# 7) Pragmatics Dependence (Why Semantic Ops Aren’t Associative)

## 7.1 Why non-associative / non-commutative happens

Semantic operators like summarization and explanation are **context sensitive**:

* ordering affects salience,
* audience affects abstraction level,
* policy affects redaction,
* domain ontology affects classification boundaries.

So naïvely:

* `summarize(summarize(x))` ≠ `summarize(x)` (lossy comp)
* `merge(summary(a), summary(b))` depends on order and framing

This breaks classic algebraic laws unless we add discipline.

## 7.2 How ENV + MNF disciplines semantics

We force each semantic operator to be:

* explicitly parameterized by ENV,
* and to output MNF-canonical CDTs with declared invariants preserved.

So “associativity” becomes conditional:

* associative **within a fixed ENV and SummarySpec**,
* when `⊕` is defined as a monoid over a specific CDT shape.

Example: define an associative “summary merge” as:

* union of responsibility sets,
* union of interfaces,
* max severity risk,
* concatenation of evidence refs,
* then normalize.

That gives you a monoid for `ModulePurpose` or `RiskAssessment`, even if free-form prose wouldn’t.

## 7.3 Kleisli, not pure

Composition is:

* `A -> CE[B]`, not `A -> B`
  so use Kleisli composition and explicit handlers:
* budget,
* provenance,
* contract checking,
* caching.

## 7.4 Reliability techniques

* strict contracts + coercion adapters (no raw “yes” strings)
* consensus for high-stakes (CA-11)
* evidence obligations (Claim must cite)
* receipts + replay (CA-26)
* determinism tiers (tier0 static → tier2 infer)

---

# 8) REQUIRED: Fully Worked Example — “SQL REPL for Any File”

We’ll build a complete program skeleton demonstrating compute/infer synergy and generalization.

## 8.1 Goal

User says: “Show the top 10 items by date descending, then summarize insights.”

Artifact: `matrix://acme/project/data/orders.csv`

We want:

1. deterministically create a `RelationalView`
2. infer NL → `SQLAST` under schema contract
3. execute SQL deterministically
4. return rows as typed CDT
5. infer explanation + summary with citations to rows
6. emit provenance DAG + enforce budget
7. generalize to PDFs (ClauseView), code (ASTView), logs (TimelineView)

---

## 8.2 Contracts

### Relational schema contract (simplified)

```json
{
  "type":"object",
  "properties":{
    "tables":{"type":"array"},
    "primary_table":{"type":"string"}
  },
  "required":["tables","primary_table"]
}
```

### SQLAST contract (simplified)

```json
{
  "type":"object",
  "properties":{
    "select":{"type":"array"},
    "from":{"type":"string"},
    "where":{"type":["object","null"]},
    "order_by":{"type":["array","null"]},
    "limit":{"type":["integer","null"]}
  },
  "required":["select","from"]
}
```

---

## 8.3 OmegaLLM Program Skeleton

```lisp
(define (sql-repl-for-artifact env uri nl-question)
  (budget-with (get env 'budget)
    (let* (
      ;; 1) snapshot for determinism + keys
      (snap (effect matrix.query :uri uri :env env
              :query '(snapshot) :contract SnapshotRef))

      ;; 2) deterministic view materialization (prefer tier0)
      (rv (effect matrix.query :uri uri :env env
            :query '(view (RelationalViewSpec)) :contract RelationalView))

      ;; 3) compile NL -> SQLAST under schema-aware contract
      (sql (effect infer.op :env env :contract SQLAST
             :prompt (list 'nl->sqlast nl-question (get rv 'tables))))

      ;; 4) execute SQL deterministically via tool (or local compute)
      (rows (effect matrix.query :uri uri :env env
              :query (list 'execute-sql sql) :contract '(RowsCDT)))

      ;; 5) cite rows and summarize with evidence
      (row-evidence (map (lambda (r) (make-row-evidence rv r)) rows))

      (insights (effect infer.op :env env :contract '(Summary)
                 :prompt (list 'summarize-rows nl-question row-evidence)))

      ;; 6) provenance
      (prov (effect matrix.query :uri "matrix://acme/provenance" :env env
              :query '(collect-current) :contract ProvenanceDAG))
    )
    (list :snapshot snap :relational rv :sql sql :rows rows :insights insights :provenance prov))))
```

### Notes on non-hand-wavy aspects

* **Determinism boundary:** snapshot + deterministic view materialization.
* **Inference constrained:** NL → SQLAST must satisfy contract and schema references.
* **Citations:** insights must reference row IDs/cells, not free-floating prose.
* **Provenance:** collected from receipts emitted by effect handlers.
* **Budget:** enforced by handler.

---

## 8.4 Evidence and Citation Mechanism (Row Evidence)

Row evidence is MNF-anchored:

* `(table_uri, snapshot, row_id, cells, notes)`

So the summarizer’s prompt is not “here are some rows,” it is “here are stable references,” enabling replay and redaction.

---

## 8.5 Generalization Pattern: View Types

### PDFs → ClauseView

* `view(ClauseViewSpec)` deterministically segments pages into clauses
* inference maps NL → clause queries (“find termination obligations”)
* outputs obligations with citations to clause spans

### Code → ASTView

* deterministic AST extraction
* inference maps NL → query over symbol/call graph
* outputs `SecurityFinding` / `ModulePurpose` with span citations

### Logs → TimelineView

* deterministic log normalization
* inference generates causal hypotheses, but timeline ordering is deterministic
* outputs `IncidentReport` with LogEvent evidence

This is the “SQL REPL for any file” idea: **(1) deterministically materialize a view; (2) infer a query AST under contract; (3) execute deterministically; (4) synthesize explanations with anchored evidence.**

---

# 9) Final Index

## 9.1 CDTs (64)

EntityRef, SpanRef, Claim, Question, Answer, Summary, TagSet, Decision, RiskAssessment, GlossaryEntry, EvidenceSpan, Citation, ProvenanceNode, ProvenanceEdge, ProvenanceDAG, Receipt, Budget, SnapshotRef, SemanticKey, CacheEntry, Intent, QueryPlan, OmegaIR, Constraint, ConstraintNetwork, ConstraintViolation, RewriteSpec, Patch, ToolInvocation, PortContract, ASTView, SymbolTable, CallGraph, DependencyGraph, APIContract, SecurityFinding, RefactoringOpportunity, TestSpec, Property, BuildGraph, CIResult, ChangeSet, ADRView, ModulePurpose, RelationalView, SQLAST, MetricDefinition, DimensionalModel, Anomaly, ForecastNarrative, LineageGraph, RowEvidence, ClauseView, Obligation, Permission, Prohibition, PolicyDecision, RedlinePatch, LogEvent, TraceView, TimelineView, IncidentReport, RunbookStep, SLOViolation.

## 9.2 CPOs (28)

capabilities, snapshot, view, canonicalize, classify, summarize, extract, judge, questionize, route, plan, compile, execute-plan, rewrite, apply, verify, diff, propagate, explain, cite, coerce-enum, coerce-schema, consensus, cache-get, cache-put, budget-with, record-receipt, redact.

## 9.3 CAs (30)

Generate-and-Test, Beam Search, Best-First, Branch-and-Bound, AMB, Propagator Fixpoint, Contradiction Detection, Minimal Unsat Core, Meaning Normalization, Bidirectional Entailment, Self-Consistency Voting, Evidence-Min Explanation, HTN Plan Synthesis, Pre/Post Checking, Saga/Compensation, NL→OmegaIR Compilation, Cost-Based Planner, Applicative Batching, CSE, DCE, Partial Evaluation, Cognitive VM, Streaming Scatter–Gather, Provenance DAG Build, Provenance Minimization, Receipt Replay, Semantic Memoization, Similarity Cache + Validation, Idempotent Receiver + Retry, Resequencing + Causal Ordering.

---

## Closing Synthesis (One clean statement)

**OmegaLLM** gives you a **typed deliberation calculus** (CDTs + CPO ISA + CA library) where inference is an explicit effect under contract, budget, and provenance.

**Matrix** gives you an **addressable world** where those programs can distribute over hierarchies, keep context local, and move only MNF-stabilized evidence-bearing artifacts.

Together they yield a pattern language where FP/EIP/GoF/DDD/POSA/testing patterns lift almost mechanically into **cognitive equivalents**—not as “workflow mush,” but as **program skeletons with invariants, costs, and replayability**.

If you want the “machine-ingestable” follow-up: I can output (1) JSON Schemas for every CDT, (2) an IDL for Matrix ports/capabilities, and (3) an OmegaIR grammar + bytecode spec for the cognitive VM.
------

Alright. I *actually* ran the prompt. 😄
You now have:

* **PART A**: a dense (but still navigable) treatise summary explaining the ontology + why it works + how the patterns map.
* **PART B**: a **strict JSON spec bundle** (100 CDTs, 50 CPOs, 50 CAs, 150 pattern mappings, Matrix views/ports, MNF rules, plus the required worked example). It’s too large to safely inline without truncation, so I generated it as downloadable files.

**Download PART B (Spec Bundle JSON):**

* [matrix_omega_spec_bundle.json (pretty)](sandbox:/mnt/data/matrix_omega_spec_bundle.json)
  SHA-256: `301fabba7179c5b7c051121033bd2036fbbcb3a12061a396dc7350c8e350a9de`
* [matrix_omega_spec_bundle.min.json (minified)](sandbox:/mnt/data/matrix_omega_spec_bundle.min.json)
  SHA-256: `af916fd6376cec06d723a8636416306124c28d6ee28371791b8e8815fb262709`

Counts in the bundle:

* CDTs: **100**
* CPOs: **50**
* CAs: **50**
* Pattern mappings: **150**
* Matrix views: **11**
* Matrix ports: **5**

---

## PART A — Human Treatise Summary

### 1) “Is JSON a universal data type?” — Yes, but only as a carrier

Your intuition is correct **in the narrow sense**: JSON is a *universal carrier/serialization* (a lowest-common-denominator interchange format). But the *whole thrust* of Matrix + OmegaLLM is: **“carrier ≠ semantic type.”**

If you only standardize on JSON, you get:

* stringly-typed semantics,
* brittle prompting interfaces,
* weak replayability,
* bad caching (hashes change under paraphrase),
* no strong contracts at effect boundaries,
* and no reliable composition laws.

So the architecture explicitly separates:

* **CDT (Cognitive Data Type)**: the semantic *type* with invariants + canonical representation + operations.
* **JSON**: one possible *encoding* of the CDT.

The “universal type” you actually want is not JSON; it’s **Meaning Normal Form (MNF)**: a canonicalized, contract-enforced semantic representation that is *stable* under irrelevant variation (formatting, paraphrase, reorderings in sets, etc.), and *explicit* about provenance and environment.

---

### 2) OmegaLLM + Matrix = “calculus + addressable world substrate”

This is the synergy in your thesis:

* **OmegaLLM** is the *calculus*: an SICP-like language for composing deterministic compute with inference as a first-class effect. The relevant mental model is: “programs are deterministic skeletons with **typed effect boundaries**.”
  (Think analyzers, abstract machines, streams, nondeterminism/AMB, higher-order procedures, etc., per Structure and Interpretation of Computer Programs.)

* **Matrix** is the *world substrate*: everything is an addressable component/agent with:

  * semantic URI
  * ports (query/command/event/view/state)
  * local cached context (SnapshotRef)
  * local deterministic compute + optional local semantic reasoning
  * hierarchical composition + scatter-gather streaming.

This yields the key inversion you described earlier:

> Instead of pushing raw artifacts into one global context window, you *route questions to the artifact nodes* and pull back **typed semantic outputs**.

That turns the “context window bottleneck” into a distributed compute + inference topology.

---

### 3) The foundational semantic equation: semantic ops are Env-parameterized effects

If you want design-pattern-level rigor, this is the core formal move:

A semantic operation is not (generally) `X -> Y`.
It’s closer to:

* `Env × X -> Effect<Y>`
* i.e. a **Kleisli arrow** `X -> Eff<Y>` where `Env` is threaded implicitly or explicitly.

Why? Because semantics are **pragmatics-dependent**:

* audience, domain ontology, safety policy, compliance rules,
* local conventions (“Ubiquitous Language” in the DDD sense),
* cost budgets (tokens/calls/time),
* and evidence requirements.

So OmegaLLM treats inference and routing as algebraic effects, and **ENV** is an explicit parameter (or Reader effect) so you can reason about composition.

---

### 4) The irreducible primitives: CPO ISA (Cognitive Primitive Operations)

The spec bundle defines **50** CPOs as the “instruction set” of cognition. The key property isn’t just “what verbs exist,” but:

* **typed signatures**
* **contract discipline** (schema/enum)
* **budget constraints**
* **provenance emission**
* **normalization discipline** (MNF)

A useful way to think about it:

* Tier-0: transport + deterministic projections (Matrix query/view/snapshot).
* Tier-1: normalization, constraint checks, patch generation (bounded but may call inference selectively).
* Tier-2: “semantic” operations (classify, judge, summarize, plan, route) that are explicitly effectful and require contracts.

Your earlier instinct (“summarization, enrichment, question generation”) is exactly right: those are not ad hoc “workflows” — they are **primitive semantic operators** that get embedded into larger deterministic skeletons.

---

### 5) Cognitive Data Types: you wanted categorization — here’s the point of the CDT catalog

The CDT catalog is the *concrete answer* to: “what kinds of data require cognition?”

A CDT represents a **meaning-bearing structure**. The spec includes **100 CDTs** spanning:

* Core semantic atoms: `URI`, `SnapshotRef`, `SemanticKey`, `Contract`, `Env`, `Policy`
* Epistemics: `Claim`, `Hypothesis`, `Decision`, `RiskAssessment`, `Summary`
* Evidence and provenance: `SpanRef`, `RowRef`, `ClauseRef`, `EvidenceSet`, `Receipt`, `ProvenanceDAG`
* Views (deterministic projections): `RelationalView`, `ASTView`, `ClauseView`, `TimelineView`, `GraphView`, etc.
* Code cognition: `SecurityFinding`, `RefactoringOpportunity`, `Patch`, `ConstraintViolation`, `TestSpec`
* Data/BI cognition: `SQLAST`, `QueryResultSet`, `MetricDefinition`, `LineageGraph`, `Anomaly`
* Legal/policy cognition: `Obligation`, `Prohibition`, `ComplianceRequirement`, `AccessControlRule`
* Ops cognition: `IncidentReport`, `Alert`, `RootCauseTree`
* UI/IoT cognition: `UIActionPlan`, `DeviceState`
* Science/medical exemplars (as data modeling, not advice): `ExperimentDesign`, `PatientSummary`

The important conceptual point: CDTs are not “documents.” They’re **semantic normal forms** you can pass around, cache, test, diff, replay, and compose.

---

### 6) Cognitive Algorithms (CA): the “program skeleton” library you actually wanted

The CA library defines **50** reusable algorithms that are *deterministic orchestration structures* with declared inference points.

They’re grouped into:

* search
* constraints
* normalization
* planning
* compilation
* machine/execution
* provenance
* caching
* concurrency

A CA is what gives you *non-hand-wavy* structure like:

* branch-and-bound over architectural alternatives
* fixpoint constraint propagation networks
* cost-based query planning
* applicative batching (to minimize inference overhead)
* scatter-gather streaming over Matrix hierarchies
* receipt replay and provenance minimization
* semantic memoization with snapshot invalidation
* determinism harnesses (goldens, metamorphic tests, contract tests)

This is *exactly* the thesis you stated: “anything expressible as a function inherits FP patterns,” and by extension a large portion of EAI structure.

---

### 7) Pattern equivalences: translating FP + EIP + GoF + DDD + POSA + CI into cognitive equivalents

You asked: “please analyze and try a few.” Here’s the crisp mapping principle:

> A classical pattern is a reusable *structure of computation and interaction*.
> A cognitive equivalent is that same structure, but where “computation” includes **typed inference effects** and “interaction” includes **Matrix-addressed nodes + views + receipts + provenance**.

Below are representative mappings (the spec bundle contains **150** mappings).

#### 7.1 Functional patterns → cognitive equivalents (key examples)

* **Functor / `map`** → *Scatter-Gather Map*
  Map a question across a set of nodes (or views), returning `Answer[]`, then optionally fold/aggregate.

* **Applicative** → *Batchable Inference*
  If you can structure N independent semantic calls, you batch them under one budget gate, preserving determinism at the orchestration level.

* **Monad / Kleisli composition** → *Env-parameterized effect sequencing*
  The canonical OmegaLLM “monadic pipeline” is:
  `view -> plan -> compile -> execute -> summarize -> cite -> emit_provenance`

* **Fold / Monoid** → *Evidence-preserving aggregation*
  Aggregation becomes nontrivial because you must retain evidence and provenance, and because semantics aren’t associative unless normalized (hence MNF + explicit aggregation semantics).

* **Streams** → *EventPort subscription with backpressure*
  Logs, traces, UI state transitions become streams; you process lazily with explicit resequencing and budget limits.

* **Lenses / Zippers** → *View + patch edit discipline*
  Navigate ASTView deterministically, generate Patch (semantic rewrite), apply_patch under optimistic concurrency, record receipts.

* **Fixed point / recursion schemes** → *Constraint propagation and summarization fixpoints*
  Compute until semantic convergence (or budget exhaustion) with explicit termination conditions.

* **Memoization** → *SemanticKey + snapshot invalidation*
  Cache results by semantic fingerprint, not raw text; invalidate on SnapshotRef changes.

These correspond strongly to both SICP composition and dataflow integration structure.

#### 7.2 EIP / EAI patterns → cognitive equivalents (key examples)

Classic EIP book reference: Enterprise Integration Patterns.

* **Content-Based Router** → `classify -> route -> query`
  Classification is semantic (not regex), routing is by capability sets and URI namespaces.

* **Message Translator** → `coerce_schema / coerce_enum / normalize`
  Strong contract coercion replaces “best effort prompting.”

* **Splitter** → `split(question)` into sub-questions with contracts

* **Aggregator** → `aggregate(answers)` with consensus rules + evidence merge

* **Resequencer** → timeline ordering, causal ordering, trace stitching

* **Claim Check** → store/retrieve large payloads via semantic cache keys, keep only references in main context

* **Idempotent Receiver** → receipt logs + replay semantics for commands

* **Saga / Compensation** → compensated command plans with rollback receipts

* **Scatter-Gather** → first-class Matrix pattern (broadcast query down subtree)

So yes: FP patterns and EIP patterns are *structurally isomorphic* here because both are fundamentally about composition, routing, and aggregation — except now the payloads are CDTs and the “processors” are effectful inference and node queries.

#### 7.3 GoF patterns → cognitive equivalents (key examples)

Canonical GoF reference: Design Patterns: Elements of Reusable Object-Oriented Software.

* **Strategy** → `Policy` object controlling routing, model choice, thresholds, voting rules.
* **Command** → Matrix CommandPort invocation returning a receipt.
* **Observer** → EventPort subscription; nodes publish state changes.
* **Mediator** → orchestrator node compiles and schedules a QueryPlan across subnodes.
* **Chain of Responsibility** → fallback routing chain: try deterministic view → then infer → then escalate.
* **Composite** → directory trees / component trees / graph views are composites; queries distribute recursively.
* **Visitor** → structured extraction across ASTs / clauses / rows.
* **Adapter** → schema coercion and view conversion.
* **Proxy** → caching proxies for expensive semantic ops.
* **Decorator** → wrap operations with policy enforcement, redaction, auditing.
* **Interpreter** → OmegaIR / SQLAST executed by a deterministic abstract machine.

#### 7.4 DDD patterns → cognitive equivalents (key examples)

Canonical DDD reference: Domain-Driven Design.
Also useful enterprise pattern context: Patterns of Enterprise Application Architecture.

* **Aggregate** → an addressable node as a consistency boundary with constraint checks.
* **Repository** → RelationalView query interface; deterministic projection + typed query.
* **Bounded Context** → URI namespace partitioning; routing respects context maps.
* **Anti-Corruption Layer** → translator/coercion boundary between view types + vocabularies.
* **CQRS** → QueryPort/ViewPort vs CommandPort separation.
* **Event Sourcing** → ProvenanceDAG/Receipt log is the event stream; state is a replayable projection.
* **Domain Events** → EventPort topics.

#### 7.5 POSA / architectural patterns → cognitive equivalents

Reference family: Pattern-Oriented Software Architecture.

* **Layers** → View stratification + effect tiers + compilation pipeline (IR→plan→execute) that is still typed and replayable.
* **Broker** → Matrix acts as the broker/rendezvous, routing queries to agents.
* **Blackboard** → EvidenceView + ProvenanceDAG as a shared knowledge structure.
* **Microkernel** → capabilities as plugins; runtime can discover new node types and operations.
* **Reactor** → event streams with backpressure and deterministic scheduling.

#### 7.6 CI/testing patterns → cognitive reliability discipline

* **Golden Masters** → snapshot canonical CDT outputs + replay comparisons.
* **Contract Tests** → schema validation at every effect boundary.
* **Metamorphic Tests** → invariances under paraphrase, reorder, and equivalent transformations.
* **Replay Receipts** → determinism harness: reproduce a run without re-invoking inference.
* **Shift-left security** → ASTView extraction + constraint checking emits SecurityFinding.

This is the piece that makes “programming with inference” not devolve into “prompt mush.”

---

### 8) Pragmatics dependence: why semantic composition is not associative, and how we make it reliable anyway

Semantic operators are usually **not associative** or **commutative** because:

* Summarizing then extracting ≠ extracting then summarizing.
* Classification depends on taxonomy and audience and policy.
* Routing decisions depend on capability sets and budgets.
* Two paraphrases can yield different raw model outputs.

So the system enforces reliability via:

1. **ENV as explicit parameter** (Reader discipline)
2. **MNF** (normalization to canonical forms)
3. **Contracts at every effect boundary** (schema/enum coercion + rejection)
4. **Evidence anchoring** (SpanRef/RowRef/ClauseRef)
5. **Provenance DAG** (auditable lineage)
6. **Receipts + replay** (determinism harness)
7. **Budgets** (bounded search/inference)
8. **Consensus operators** (self-consistency voting, aggregation policies)

This is what turns “semantic operations” into something you can **test**, **cache**, **diff**, and **compose**.

---

### 9) Worked example: “SQL REPL for any file” (compute + inference synergy)

The canonical pattern is:

1. **Address artifact**: `matrix://realm/files/x.csv`
2. **Snapshot**: create `SnapshotRef` and content hash
3. **Construct RelationalView** deterministically (parse schema+rows)
4. **Infer**: translate NL → SQLAST *under schema contract*
5. **Execute** SQL deterministically on the RelationalView
6. **Wrap results** into EvidenceView: row/cell refs not raw excerpts
7. **Summarize** with citations to evidence refs
8. **Emit provenance DAG**
9. **Cache** by SemanticKey(schema, nl, env) with snapshot invalidation

In OmegaLLM-ish pseudocode:

```lisp
(define (sql-repl env file-uri nl)
  (effect CPO.budget_guard env
    (lambda ()
      (let* ((snap  (effect CPO.snapshot file-uri))
             (rv    (effect CPO.view env file-uri 'RelationalView))
             (sql   (effect CPO.coerce_schema env
                      (effect CPO.plan env (list 'sql rv nl))
                      'SQLAST-Schema))
             (rows  (compute.sql.exec rv sql))
             (ev    (compute.evidence.wrap rows snap))
             (sum   (effect CPO.summarize env ev 'SummarySpec))
             (prov  (effect CPO.emit_provenance env (compute.receipt ...) sum)))
        (list sum prov)))))
```

Generalization is literally: **swap the View type**:

* PDF → ClauseView (NL → clause query/AST; execute deterministically; cite ClauseRefs)
* Code → ASTView (NL → ASTQuery; deterministic traversal; cite SpanRefs)
* Logs → TimelineView (NL → predicates; deterministic filter; cite EventRefs)

That’s why “views” are the bridge type between compute and inference: they make the artifact *queryable* without stuffing it into the orchestrator context.

---

## PART B — Spec Bundle in strict JSON (download)

The full JSON is too large to inline safely without truncation, so it’s provided as files:

* [Download pretty JSON](sandbox:/mnt/data/matrix_omega_spec_bundle.json)
* [Download minified JSON](sandbox:/mnt/data/matrix_omega_spec_bundle.min.json)

If you want, next we can do one of these (no extra “research,” just concrete engineering moves):

1. **Generate OmegaLLM standard library stubs** from the JSON (auto-emit `(define (CPO.xxx ...))` wrappers, plus contract schemas).
2. **Generate Matrix View interface definitions** (RelationalView/ASTView/ClauseView) and a minimal “View runtime” skeleton.
3. **Emit a “delta bundle”**: e.g., “add 50 more CDTs under legal/compliance + finance + security,” without perturbing existing names (backwards compatible evolution).

Say which direction you want, and I’ll materialize the next artifact.
