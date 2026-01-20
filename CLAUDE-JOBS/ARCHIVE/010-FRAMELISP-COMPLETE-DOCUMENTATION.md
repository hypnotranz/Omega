# JOB-010: Complete FrameLisp Documentation in Features File

**Status**: OPEN
**Priority**: P0 (Foundational - blocks all other LLM work)
**Source**: [REFERENCE-ALGEBRA.md](../docs/REFERENCE-ALGEBRA.md)
**Target**: [LAMBDA-LLM--OMEGA-LLM-FEATURES.md](../docs/LAMBDA-LLM--OMEGA-LLM-FEATURES.md)

---

## Objective

Document **100% of FrameLisp primitives** from REFERENCE-ALGEBRA.md in the features document, organized by semantic layer following SICP principles:

1. **Primitive expressions** - the simplest entities (data types, kernel ops)
2. **Means of combination** - compound elements (prompt+, bind, all/race)
3. **Means of abstraction** - naming/manipulation (defprompt, defop, defgraph)

Each primitive must be:
- Mapped to its source section in REFERENCE-ALGEBRA.md
- Described with signature and purpose
- Marked with OmegaLLM implementation status
- Cross-referenced to implementation file if it exists

---

## Layer Architecture (SICP-Aligned)

```
┌─────────────────────────────────────────────────────────────┐
│ Layer 5c: Runtime Contracts                                  │
│   Caching, Tracing, Safety Policies                         │
├─────────────────────────────────────────────────────────────┤
│ Layer 5b: Protocol Libraries                                 │
│   chat-turn, tool-loop, rag, complete, repl                 │
├─────────────────────────────────────────────────────────────┤
│ Layer 5a: Abstraction Mechanisms                             │
│   defprompt, deftransform, defop, deftool, defgraph         │
├─────────────────────────────────────────────────────────────┤
│ Layer 4d: Execution Algebra (Means of Combination)           │
│   bind, branch, retry-until, all/race/par-map, streaming    │
├─────────────────────────────────────────────────────────────┤
│ Layer 4c: Prompt Algebra (Means of Combination)              │
│   prompt+, transformers, templates, as-data                 │
├─────────────────────────────────────────────────────────────┤
│ Layer 4b: Kernel Primitives (Primitive Expressions)          │
│   infer, embed, retrieve, call-tool, validate, commit       │
├─────────────────────────────────────────────────────────────┤
│ Layer 4a: Data Model (Primitive Expressions)                 │
│   Message, Prompt, Artifact, Flow, ToolSpec, Provenance     │
├─────────────────────────────────────────────────────────────┤
│ Layer 3: Effect System (OmegaLLM provides)                   │
│   conditions, handlers, restarts, amb, oracle effects       │
├─────────────────────────────────────────────────────────────┤
│ Layer 2: Standard Library (OmegaLLM provides)                │
│   streams, nondet, concurrency, constraints, generic        │
├─────────────────────────────────────────────────────────────┤
│ Layer 1: Core Language (OmegaLLM provides)                   │
│   CEKS machine, lambda, define, if, macros                  │
└─────────────────────────────────────────────────────────────┘
```

---

## Complete Inventory (84 Items)

### Layer 4a: Data Model (§3) - 11 items

| # | Symbol | Source | Signature | Purpose |
|---|--------|--------|-----------|---------|
| 1 | `Message` | §3.1 | `{role, content, name?, tool-call-id?, mime-type?, timestamp?, tags?, provenance?}` | Role-tagged message unit |
| 2 | `Message.role` | §3.1 | `:system \| :developer \| :user \| :assistant \| :tool \| :context` | Message source/type |
| 3 | `Prompt` | §3.2 | `{messages: Message[], vars: Map, policy: PolicySpec, meta: Map}` | Immutable prompt container |
| 4 | `PromptTemplate` | §3.3 | `{render: Bindings -> Prompt, partial: Bindings -> PromptTemplate}` | Parameterized prompt factory |
| 5 | `Artifact[A]` | §3.4 | `{value: A, provenance: Provenance, diagnostics: List, metrics: Map}` | Value with provenance |
| 6 | `ToolSpec` | §3.5 | `{name, doc, input-schema, output-schema, fn, purity, timeout-ms?, retry-policy?, authz-policy?}` | Tool definition |
| 7 | `ToolSpec.purity` | §3.5 | `:pure \| :idempotent \| :effectful` | Tool effect classification |
| 8 | `Provenance` | §3.6 | `{source-ids, tool-lineage, retrieval-docs, model-id, hashes}` | Derivation tracking |
| 9 | `Flow[A]` | §4.1 | First-class program yielding `Artifact[A]` or `EventStream` | Effectful computation |
| 10 | `EventStream` | §4.2 | Bounded/unbounded stream with backpressure, cancellation, fanout | Async event sequence |
| 11 | `Event` | §4.2 | `:token \| :delta \| :tool-call \| :tool-result \| :retrieval \| :trace \| :error \| :done` | Stream event types |

### Layer 4b: Kernel Primitives (§5) - 9 items

These are the **ONLY** operations that require a runtime handler. Everything else is definable.

| # | Symbol | Source | Signature | Purpose | OmegaLLM |
|---|--------|--------|-----------|---------|----------|
| 12 | `infer` | §5.1 | `(infer prompt &key model decoding tools response-format policy) => Flow[Completion]` | Submit prompt to LLM | Partial (oracle/portal.ts) |
| 13 | `infer/stream` | §5.1 | `(infer/stream prompt &key ...) => Flow[Completion]` | Streaming inference | Partial |
| 14 | `embed` | §5.2 | `(embed items &key model) => Flow[VectorEmbeddings]` | Generate embeddings | MISSING |
| 15 | `retrieve` | §5.3 | `(retrieve query &key index top-k filter rerank) => Flow[DocSet]` | RAG retrieval | MISSING |
| 16 | `call-tool` | §5.4 | `(call-tool tool-name args &key timeout) => Flow[ToolResult]` | Invoke tool | Partial (ReqTool) |
| 17 | `emit` | §5.5 | `(emit sink item &key format) => Flow[Ok]` | Output to sink | MISSING |
| 18 | `observe` | §5.5 | `(observe source &key format) => Flow[Item]` | Input from source | MISSING |
| 19 | `validate` | §5.6 | `(validate spec value &key explain) => Flow[Validation]` | Schema/predicate validation | MISSING |
| 20 | `commit` | §5.7 | `(commit store key value &key ttl) => Flow[Ok]` | Persist to store | Partial (COWStore) |

### Layer 4c: Prompt Artifact Algebra (§6) - 17 items

Pure operations on prompts - **no LLM calls**. The "cons/car/cdr" of prompts.

| # | Symbol | Source | Signature | Purpose | OmegaLLM |
|---|--------|--------|-----------|---------|----------|
| 21 | `system` | §6.1 | `(system text &key name tags meta) => Message` | Create system message | MISSING |
| 22 | `user` | §6.1 | `(user text &key name tags meta) => Message` | Create user message | MISSING |
| 23 | `assistant` | §6.1 | `(assistant text &key name tags meta) => Message` | Create assistant message | MISSING |
| 24 | `tool` | §6.1 | `(tool name payload &key tool-call-id) => Message` | Create tool message | MISSING |
| 25 | `prompt` | §6.1 | `(prompt &rest messages-or-prompts) => Prompt` | Construct prompt | MISSING |
| 26 | `prompt+` | §6.2 | `(prompt+ p1 p2 &rest ps) => Prompt` | Concatenate prompts (associative, non-commutative) | MISSING |
| 27 | `with-system` | §6.3 | `(with-system text) => PromptTransformer` | Add system instruction | MISSING |
| 28 | `with-policy` | §6.3 | `(with-policy policy-spec) => PromptTransformer` | Add policy constraints | MISSING |
| 29 | `with-tools` | §6.3 | `(with-tools toolset) => PromptTransformer` | Add available tools | MISSING |
| 30 | `with-examples` | §6.3 | `(with-examples examples) => PromptTransformer` | Add few-shot examples | MISSING |
| 31 | `with-context` | §6.3 | `(with-context docs) => PromptTransformer` | Add RAG context | MISSING |
| 32 | `with-guard` | §6.3 | `(with-guard guard-spec) => PromptTransformer` | Add safety guard | MISSING |
| 33 | `compose-transformers` | §6.3 | `(compose-transformers t1 t2 ...) => PromptTransformer` | Compose transformers | MISSING |
| 34 | `defprompt` | §6.4 | `(defprompt name (vars...) template-body...)` | Define prompt template | MISSING |
| 35 | `render` | §6.4 | `(render template &key bindings) => Prompt` | Instantiate template | MISSING |
| 36 | `partial` | §6.4 | `(partial template &key bindings) => PromptTemplate` | Curry template (partial application!) | MISSING |
| 37 | `as-data` | §6.5 | `(as-data x &key fence mime) => string` | Quote data to prevent injection | MISSING |

### Layer 4d: Execution Algebra (§7) - 21 items

Means of combination for effectful operations. Maps to OmegaLLM's effect system.

| # | Symbol | Source | Signature | Purpose | OmegaLLM |
|---|--------|--------|-----------|---------|----------|
| 38 | `pure` | §7.1 | `(pure x) => Flow[X]` | Lift value into Flow | ✓ `unit` |
| 39 | `mapf` | §7.1 | `(mapf f flow) => Flow[Y]` | Transform result | Partial via bind |
| 40 | `bind` | §7.1 | `(bind flow f) => Flow[Y]` | Chain computations (Kleisli) | ✓ `bind` + KBind |
| 41 | `tap` | §7.1 | `(tap flow side-effect) => Flow[X]` | Side effect (logging) | MISSING |
| 42 | `flow` | §7.1 | `(flow (let* ((x (<! ...))) ...))` | Monadic do-notation macro | MISSING |
| 43 | `<!` | §7.1 | `(<! flow)` | Bind extract macro | MISSING |
| 44 | `branch` | §7.2 | `(branch flow (:when pred then) ... (:else else))` | Content-based routing | MISSING |
| 45 | `retry-until` | §7.3 | `(retry-until flow :validate :repair :max-tries :backoff)` | Generate-validate-repair loop | MISSING |
| 46 | `all` | §7.4 | `(all &rest flows) => Flow[(values ...)]` | Wait for all (parallel) | MISSING |
| 47 | `race` | §7.4 | `(race &rest flows) => Flow[A]` | First to complete wins | MISSING |
| 48 | `par-map` | §7.4 | `(par-map f items &key max-par) => Flow[list]` | Parallel map | MISSING |
| 49 | `par-flatmap` | §7.4 | `(par-flatmap f items &key max-par) => Flow[list]` | Parallel flatmap | MISSING |
| 50 | `with-timeout` | §7.5 | `(with-timeout ms flow) => Flow[A]` | Timeout wrapper | MISSING |
| 51 | `with-budget` | §7.5 | `(with-budget budget-spec flow) => Flow[A]` | Budget wrapper | Partial (budgets exist) |
| 52 | `cancel` | §7.5 | `(cancel flow) => Flow[Ok]` | Cancel in-progress | MISSING |
| 53 | `stream-map` | §7.6 | `(stream-map f es) => es` | Map over event stream | Build on streams |
| 54 | `stream-filter` | §7.6 | `(stream-filter pred es) => es` | Filter event stream | Build on streams |
| 55 | `stream-take` | §7.6 | `(stream-take n es) => es` | Take n from stream | Build on streams |
| 56 | `stream-merge` | §7.6 | `(stream-merge &rest es) => es` | Merge streams (fair) | MISSING (stream-interleave) |
| 57 | `stream-zip` | §7.6 | `(stream-zip es1 es2) => es` | Zip streams | Build on streams |
| 58 | `stream-reduce` | §7.6 | `(stream-reduce f init es) => Flow[result]` | Reduce stream to value | Build on streams |

### Layer 5a: Abstraction Mechanisms (§9) - 8 items

| # | Symbol | Source | Signature | Purpose | OmegaLLM |
|---|--------|--------|-----------|---------|----------|
| 59 | `defprompt` | §9.1 | `(defprompt name (vars) body)` | Define prompt template | MISSING |
| 60 | `deftransform` | §9.2 | `(deftransform name () (lambda (p) ...))` | Define prompt transformer | MISSING |
| 61 | `defop` | §9.3 | `(defop name (args) body)` | Define named Flow operator | MISSING |
| 62 | `deftool` | §9.3 | `(deftool name :input-schema :output-schema :purity fn)` | Define tool | MISSING |
| 63 | `defschema` | §9.4 | `(defschema name spec)` | Define validation schema | MISSING |
| 64 | `defgraph` | §9.5 | `(defgraph name (state) (node ...) (edge ...))` | Define state machine agent | MISSING |
| 65 | `workflow` | §9.4 | `(workflow ...)` | Workflow DSL macro | MISSING |
| 66 | `with-*` decorators | §9.4 | `with-runtime, with-model, with-policy, with-cache, with-trace` | Context decorators | MISSING |

### Layer 5b: Protocol Libraries (§8) - 8 items

Libraries built on the kernel - "magical subtypes" made explicit.

| # | Symbol | Source | Signature | Purpose | OmegaLLM |
|---|--------|--------|-----------|---------|----------|
| 67 | `Conversation` | §8.1 | `{history: Message[], memory: Map, policy: Policy}` | Chat state object | MISSING |
| 68 | `chat-turn` | §8.1 | `(chat-turn conv user-msg &key persona tools) => Flow[(reply, conv')]` | Multi-turn chat | MISSING |
| 69 | `tool-loop` | §8.2 | `(tool-loop prompt &key tools max-steps) => Flow[Completion]` | ReAct-style tool use | MISSING |
| 70 | `rag` | §8.3 | `(rag query :retrieve :compose-prompt :answer) => Flow[Answer]` | Retrieval-augmented generation | MISSING |
| 71 | `complete` | §8.4 | `(complete prefix &key system decoding) => Flow[string]` | Single-shot completion | MISSING |
| 72 | `repl-protocol` | §8.5 | Integration with Lisp REPL streams | Interactive inference | MISSING |
| 73 | `log-stream` | §8.6 | Stream processor with bounded inference | Event classification | MISSING |
| 74 | EIP patterns | §8.3 | Splitter, Aggregator, Content Enricher, Router | Enterprise integration | MISSING |

### Layer 5c: Runtime Contracts (§10-11) - 10 items

| # | Symbol | Source | Signature | Purpose | OmegaLLM |
|---|--------|--------|-----------|---------|----------|
| 75 | `Runtime` | §10.1 | `{model-backend, retriever-backend, tool-registry, cache, scheduler, tracer, policy-engine, clock}` | Execution context | Partial |
| 76 | Effect handlers | §10.2 | `handle-infer, handle-retrieve, handle-call-tool, ...` | Pluggable backends | Partial |
| 77 | Cache-key derivation | §10.3 | Normalized prompt + model + params → key | Deterministic caching | MISSING |
| 78 | Cache tiers | §10.3 | In-memory LRU + persistent store | Multi-level cache | Partial |
| 79 | Tracing spans | §10.4 | Start/end span, metrics, tool args (redacted) | Observability | Partial |
| 80 | Policy object | §11 | Tool allowlist, max calls, fencing, redaction, gates | Safety controls | MISSING |
| 81 | `inference-error` | §12.3 | Condition type | LLM call failure | MISSING |
| 82 | `tool-error` | §12.3 | Condition type | Tool invocation failure | MISSING |
| 83 | `validation-error` | §12.3 | Condition type | Schema validation failure | MISSING |
| 84 | Restarts | §12.3 | `retry-step, fallback, skip, use-value, abort-run, reduce-scope` | Error recovery | MISSING |

---

## Semantic Closure Principles

Each layer must maintain **closure under composition** (Henderson Escher principle):

### Layer 4a (Data):
- `Message` + `Message` → via `prompt` → `Prompt`
- `Prompt` + `Prompt` → via `prompt+` → `Prompt` (closed!)
- `Flow` + `Flow` → via `bind` → `Flow` (closed!)

### Layer 4c (Prompt Algebra):
- `PromptTransformer` ∘ `PromptTransformer` → via `compose-transformers` → `PromptTransformer` (closed!)
- `PromptTemplate` + partial bindings → via `partial` → `PromptTemplate` (closed!)

### Layer 4d (Execution Algebra):
- `Flow[A]` + `(A → Flow[B])` → via `bind` → `Flow[B]` (monad!)
- `Flow[A]` + `Flow[B]` → via `all` → `Flow[(A, B)]` (closed!)
- `Flow[A]` + `Flow[A]` → via `race` → `Flow[A]` (closed!)

### Layer 5 (Abstractions):
- Named operators compose because they return `Flow`
- Graph nodes compose because edges are explicit

---

## Acceptance Criteria

1. [ ] All 84 items documented in features file
2. [ ] Each item has: source section, signature, purpose, OmegaLLM status
3. [ ] Layer diagram shows semantic closure at each level
4. [ ] Cross-references to implementation files where they exist
5. [ ] Clear indication of what's MISSING vs PARTIAL vs COMPLETE
6. [ ] Summary statistics updated

---

## Implementation Notes

This is **documentation only** - no code changes. The goal is to have a complete map of:
1. What FrameLisp specifies
2. What OmegaLLM currently has
3. What gaps exist

This map will drive subsequent implementation work.
