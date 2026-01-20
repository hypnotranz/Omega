## 56) FrameIR v1: concrete node schemas (field-by-field) that can be canonicalized, hashed, linted, replayed

What you want next is *not* “more functions”; it’s a **precise, mechanically-checkable IR contract**. Everything else (runtime, language, solver library, docgen, lints, replay, cache, provenance) becomes a projection of this contract.

I’ll define this as if you were implementing `@frameir` (TypeScript package) with an isomorphic JSON encoding.

### 56.1 Core invariants

**Invariant A — Every IR node is a tagged record**
So you can do total pattern matching and safe decoding.

**Invariant B — Every IR node must be canonical-serializable**
So it can be hashed and cached deterministically.

**Invariant C — Spans are metadata, not semantics**
Spans must never change evaluation behavior; they should be droppable without changing meaning.

**Invariant D — “Policy” is data, but policy that changes semantics must be salted into hashes**
(Your “semantic salt” rule.)

---

### 56.2 Common node envelope

A pragmatic envelope that supports diagnostics/provenance without polluting semantic hashing too badly:

```ts
// @frameir/core

export type IRVersion = "frameir@1";

export interface Span {
  file?: string;        // logical source file id
  startLine?: number;
  startCol?: number;
  endLine?: number;
  endCol?: number;
}

export interface Meta {
  span?: Span;                  // non-semantic
  doc?: string;                 // non-semantic
  attrs?: Record<string, any>;  // non-semantic; must be excluded from canonical hashing unless explicitly included
}

export interface NodeBase {
  v: IRVersion;         // required
  tag: string;          // required discriminator
  meta?: Meta;          // optional
}
```

**Canonical hashing rule**: by default, `meta` is excluded from `hashNode()` unless you explicitly request “debug hash”.

That lets you:

* preserve spans for blame
* keep semantics stable across formatting/doc edits

---

### 56.3 ValueIR v1: pure data + references (minimal, but enough for closure conversion)

You can keep ValueIR quite small if you compile most pure Lisp into runtime values, but for your *Flow* binder lambdas / predicates you need something stable.

I recommend **closure conversion** early, which means FlowIR mostly references function definitions rather than embedding lambdas. Still, you need an expression language for:

* predicates (`retry-until pred`, `branch pred`)
* options expressions (`infer options`)
* basic derived calculations (budget allocations, counters)

#### 56.3.1 Value nodes

```ts
export type ValueIR =
  | VNil | VBool | VInt | VFloat | VStr | VSymbol | VKeyword
  | VList | VRecord
  | VRef
  | VExpr;

export interface VNil extends NodeBase { tag: "VNil" }
export interface VBool extends NodeBase { tag: "VBool"; value: boolean }
export interface VInt extends NodeBase { tag: "VInt"; value: string /* canonical decimal */ }
export interface VFloat extends NodeBase { tag: "VFloat"; value: string /* canonical decimal */ }
export interface VStr extends NodeBase { tag: "VStr"; value: string }
export interface VSymbol extends NodeBase { tag: "VSymbol"; name: string }   // 'foo
export interface VKeyword extends NodeBase { tag: "VKeyword"; name: string } // :foo

export interface VList extends NodeBase { tag: "VList"; items: ValueIR[] }
export interface VRecord extends NodeBase { tag: "VRecord"; entries: Array<{ k: ValueIR; v: ValueIR }> }

// Reference into a bundle (module symbol table) or external resource
export interface VRef extends NodeBase {
  tag: "VRef";
  ref: { kind: "Global" | "Fn" | "ToolContract" | "Schema" | "Store" | "Sink" | "Source";
         id: string;         // stable id (often sha256)
         name?: string; };   // human label; non-semantic unless you choose
}
```

#### 56.3.2 Value expressions (tiny expression language)

A deliberately tiny calculus—don’t recreate full Lisp here.

```ts
export type VExpr =
  | EIf | EEq | ENot | EAnd | EOr
  | EAdd | ESub | EMul | EDiv | EMod
  | EGet | EAssoc
  | ECallPrim;

export interface EIf extends NodeBase { tag: "EIf"; cond: ValueIR; then: ValueIR; else: ValueIR; }
export interface EEq extends NodeBase { tag: "EEq"; a: ValueIR; b: ValueIR; }
export interface ENot extends NodeBase { tag: "ENot"; x: ValueIR; }
export interface EAnd extends NodeBase { tag: "EAnd"; xs: ValueIR[]; } // short-circuit semantics
export interface EOr  extends NodeBase { tag: "EOr";  xs: ValueIR[]; } // short-circuit semantics

export interface EAdd extends NodeBase { tag: "EAdd"; xs: ValueIR[]; }
export interface ESub extends NodeBase { tag: "ESub"; xs: ValueIR[]; } // unary or n-ary
export interface EMul extends NodeBase { tag: "EMul"; xs: ValueIR[]; }
export interface EDiv extends NodeBase { tag: "EDiv"; a: ValueIR; b: ValueIR; }
export interface EMod extends NodeBase { tag: "EMod"; a: ValueIR; b: ValueIR; }

export interface EGet extends NodeBase { tag: "EGet"; map: ValueIR; key: ValueIR; default?: ValueIR; }
export interface EAssoc extends NodeBase { tag: "EAssoc"; map: ValueIR; key: ValueIR; val: ValueIR; }

// Call a registered pure primitive from the registry (see §57)
export interface ECallPrim extends NodeBase {
  tag: "ECallPrim";
  prim: string;     // e.g. "lambdallm.core/hash"
  args: ValueIR[];
}
```

This supports enough for budgets/options/predicates *without* forcing you to embed full Lisp.

---

## 56.4 PromptIR v1: document model, not “just strings”

Treat prompts as structured documents with attachments. This makes:

* canonicalization stable
* tool/schema wiring explicit
* lint rules tractable
* provenance attachable

```ts
export type PromptIR = PromptDoc;

export interface PromptDoc extends NodeBase {
  tag: "PromptDoc";
  parts: PromptPart[];
}

export type PromptPart =
  | PSystem | PUser | PAssistant
  | PFewShot
  | PData
  | PXml
  | PCodeBlock
  | PNumbered
  | PAttachTools
  | PAttachSchema
  | PAttachFormat
  | PTransform;

export interface PSystem extends NodeBase { tag: "PSystem"; text: string; }
export interface PUser extends NodeBase { tag: "PUser"; text: string; }
export interface PAssistant extends NodeBase { tag: "PAssistant"; text: string; }

export interface PFewShot extends NodeBase {
  tag: "PFewShot";
  examples: Array<{ user: string; assistant: string }>;
}

// Serializes ValueIR (not runtime Val) as stable data
export interface PData extends NodeBase { tag: "PData"; value: ValueIR; }

// Structure wrappers
export interface PXml extends NodeBase { tag: "PXml"; tagName: string; inner: PromptIR; }
export interface PCodeBlock extends NodeBase { tag: "PCodeBlock"; lang: string; code: string; }
export interface PNumbered extends NodeBase { tag: "PNumbered"; items: PromptIR[]; }

// Attachments (contracts are referenced by VRef for hashing/replay)
export interface PAttachTools extends NodeBase {
  tag: "PAttachTools";
  tools: VRef[]; // each VRef kind = ToolContract
  inner: PromptIR;
}

export interface PAttachSchema extends NodeBase {
  tag: "PAttachSchema";
  schema: VRef; // kind=Schema
  inner: PromptIR;
}

export interface PAttachFormat extends NodeBase {
  tag: "PAttachFormat";
  format: { kind: "json" | "xml" | "text"; details?: ValueIR };
  inner: PromptIR;
}

// Controlled transforms (don’t allow arbitrary code here)
export interface PTransform extends NodeBase {
  tag: "PTransform";
  transform: string; // registry id for transformer
  inner: PromptIR;
}
```

**How `prompt+` lowers:** concatenation is `PromptDoc(parts = [...])`, and a normalizer can flatten nested docs (associativity law).

**How `as-data` lowers:** `PData(value)` (then renderer decides how to serialize data).

---

## 56.5 FlowIR v1: operational algebra as an executable plan

This is the spine of your whole platform.

```ts
export type FlowIR =
  | FPure
  | FBind
  | FCatch
  | FFail
  | FWithBudget
  | FWithTimeout
  | FAll
  | FRace
  | FAny
  | FSequence
  | FBranch
  | FLoop
  | FInfer
  | FToolCall
  | FValidate
  | FCommit
  | FEmit
  | FObserve
  | FSuspend;

export interface FPure extends NodeBase { tag: "FPure"; value: ValueIR; }

// Binder is a reference to a compiled function (closure-converted)
export interface FBind extends NodeBase {
  tag: "FBind";
  flow: FlowIR;
  k: VRef;            // kind=Fn
}

// Handler receives Failure; also a Fn ref
export interface FCatch extends NodeBase {
  tag: "FCatch";
  flow: FlowIR;
  handler: VRef;      // kind=Fn
}

export interface FFail extends NodeBase {
  tag: "FFail";
  reason: ValueIR;     // typically keyword or symbol
  ctx?: ValueIR;       // structured context record
}

export interface FWithBudget extends NodeBase {
  tag: "FWithBudget";
  budget: ValueIR;     // record: {llmCalls, tokens, timeMs}
  flow: FlowIR;
}

export interface FWithTimeout extends NodeBase {
  tag: "FWithTimeout";
  ms: ValueIR;
  flow: FlowIR;
}

export interface FAll extends NodeBase { tag: "FAll"; flows: FlowIR[]; }
export interface FRace extends NodeBase { tag: "FRace"; flows: FlowIR[]; }
export interface FAny extends NodeBase { tag: "FAny"; flows: FlowIR[]; }
export interface FSequence extends NodeBase { tag: "FSequence"; flows: FlowIR[]; }

export interface FBranch extends NodeBase {
  tag: "FBranch";
  pred: ValueIR;
  then: FlowIR;
  else: FlowIR;
}

// Loop state is explicit; step/cond are Fn refs
export interface FLoop extends NodeBase {
  tag: "FLoop";
  init: ValueIR;
  step: VRef;   // Fn: state -> Flow[state]
  until: VRef;  // Fn: state -> Bool (or ValueIR truthy)
}

export interface FInfer extends NodeBase {
  tag: "FInfer";
  prompt: PromptIR;
  options?: ValueIR; // record: model, temperature, maxTokens, jsonMode, etc.
}

export interface FToolCall extends NodeBase {
  tag: "FToolCall";
  tool: ValueIR;        // typically string or symbol
  args: ValueIR;        // record or list; validated by contract
  contract?: VRef;      // kind=ToolContract (strongly recommended required)
}

export interface FValidate extends NodeBase {
  tag: "FValidate";
  schema: VRef;        // kind=Schema
  value: ValueIR;
}

export interface FCommit extends NodeBase {
  tag: "FCommit";
  store: VRef;         // kind=Store
  key: ValueIR;
  value: ValueIR;
}

export interface FEmit extends NodeBase {
  tag: "FEmit";
  sink: VRef;          // kind=Sink
  item: ValueIR;
}

export interface FObserve extends NodeBase {
  tag: "FObserve";
  source: VRef;        // kind=Source
  query?: ValueIR;
}

export interface FSuspend extends NodeBase {
  tag: "FSuspend";
  reason: ValueIR;
}
```

### 56.5.1 Why Fn references, not inline lambdas?

Because canonicalization and hashing become dramatically simpler:

* a `FnDef` can be hashed by canonicalizing its body and captured environment structure
* references are stable
* alpha-renaming problems disappear

So you store functions in a **bundle**:

```ts
export interface IRBundle {
  v: IRVersion;
  entry: FlowIR;                 // top-level plan
  fns: Record<string, FnDefIR>;  // keyed by stable fnId (sha256)
  schemas: Record<string, SchemaIR>;
  toolContracts: Record<string, ToolContractIR>;
  // optional: modules, exports, docs
}

export interface FnDefIR extends NodeBase {
  tag: "FnDef";
  fnId: string;
  params: string[];        // purely for debug; semantics uses positional
  body: FlowIR | ValueIR;  // depending on whether it returns Flow or pure Value
  captures?: ValueIR;      // record of captured values (closure env)
}
```

That’s closure conversion “as data.”

---

## 56.6 Canonicalization rules (the non-negotiable boring part)

If you get these wrong, everything (caching, replay, merkle hashes, evidence IDs) becomes unreliable.

### 56.6.1 Canonical JSON rules (repeat, but sharpened)

* Object keys sorted lexicographically by UTF-16 code units **or** by UTF-8 bytes — pick one and freeze it.
* No undefined; only explicit omission or `null` (choose one; I recommend omission for optional fields).
* Integers encoded as strings in base-10 without leading zeros (except `"0"`).
* Floats encoded as strings with a canonical decimal format (or forbid floats in semantic fields and use rationals).
* Arrays preserve order.
* `meta` excluded from canonical bytes used for semantic hashing, unless you explicitly ask for “debug hash”.

### 56.6.2 Normalization passes (safe rewrites)

Provide deterministic normalizers:

* `normalizePrompt`:

  * flatten nested `PromptDoc`
  * merge adjacent same-role segments (optional)
  * delete empty segments
* `normalizeFlow`:

  * flatten `FSequence` nested sequences
  * `FBind(FPure(x), k)` → (optional) *beta-step* into `k(x)` only if you can do it safely (often you can’t without evaluating `k`)
  * collapse `FAll([])` → `FPure([])` etc.

Keep **rewrite legality** explicit: never do an optimization unless it’s semantics-preserving.

Patterns:

* **Normalization** as a compiler phase
* **Visitor + Rewriter** (GoF Visitor, plus a Fowler-style “Replace Conditional with Polymorphism” via tag dispatch)

---

## 57) PrimitiveDescriptor Registry: one source of truth for docs, typing, effects, budgets, and lowering

You’re sitting on a combinatorial explosion of “function lists.” The way out is: **a single, queryable registry**.

Think of this as the platform’s **Reflection API** plus **Compiler Table**.

### 57.1 Descriptor shape

```ts
export type Effect =
  | "Pure"
  | "Oracle"
  | "Tool"
  | "Store"
  | "Sink"
  | "Source"
  | "Clock"
  | "Concurrency"
  | "Constraint"
  | "Nondet"
  | "Control";

export interface TypeSig {
  // Keep this minimal initially; upgrade later to full Hindley–Milner-ish
  params: Array<{ name: string; type: string }>;
  returns: string;
}

export interface CostModel {
  estimate: (args: any, ctx: any) => { llmCalls?: number; tokens?: number; timeMs?: number };
  // optionally: symbolic cost expression
}

export interface PrimitiveDescriptor {
  id: string;                 // canonical id, e.g. "framelisp/infer" or "lambdallm.core/+"
  layer: "FrameLisp" | "LambdaLLM" | "OmegaLLM" | "LambdaRLM";
  kind: "SpecialForm" | "Function" | "Macro" | "ProtocolMethod";

  signature: TypeSig;
  effects: Effect[];          // closed set for linting and capability calculus
  resources?: CostModel;      // estimator for budgets / planning

  doc: {
    summary: string;
    detail?: string;
    laws?: string[];          // equational laws / invariants
    examples?: Array<{ in: string; out: string }>;
  };

  // Lowering rule: LambdaLLM surface -> FrameIR node
  lowering?: {
    // purely declarative or a small compiler hook
    kind: "Intrinsic" | "MacroExpand" | "LowerHook";
    irTag?: string;           // e.g. "FInfer", "FBind", "PAttachSchema"
  };

  // Runtime rule: how Omega kernel executes a given IR tag (or primitive)
  runtime?: {
    // for IR tags, execution is part of evaluator; for primitives, implementer id
    implementer?: string;    // e.g. "omega-kernel/prim/add"
  };

  // Validation hooks for linting
  constraints?: {
    mustBeDominatedByBudget?: boolean;
    mustBeDominatedByTimeout?: boolean;
    requiresToolContract?: boolean;
  };

  // Versioning for evolution and replay compatibility
  version: string;            // semver, but internal; e.g. "1.2.0"
  deprecated?: { since: string; replacedBy?: string; note?: string };
}
```

This gives you a uniform place to answer:

* “What is this thing?”
* “What does it lower to?”
* “What effects does it require?”
* “What budget should it consume?”
* “How do we document it?”
* “How do we lint it?”

### 57.2 How this registry eliminates your doc drift

You can now generate:

* `REFERENCE-LIBRARIES.md` (like the one you pasted)
* `ARCHITECTURE/14-STDLIB.md` consistency checks
* a `stdlib index` command (`apropos` becomes registry search)
* contract tests automatically: “every ToolCall must reference a ToolContract in bundle”
* effect lints automatically: “every Oracle effect must be under WithBudget”

This is classic **Single Source of Truth** + **Code Generation**.

### 57.3 Registry as a plugin system (so Omega/LambdaRLM can extend safely)

Use a registration mechanism:

* `registerPrimitive(descriptor)`
* descriptor IDs are namespaced to packages
* the CLI can load registries from installed packages

This keeps the system extensible without turning into “import spaghetti”.

Patterns:

* **Plugin Architecture**
* **Inversion of Control** (registry injection)
* **Extension Objects** (GoF)

---

## 58) ToolContractIR and SchemaIR: unify validation, structured output, and tooling

You have “with-schema”, “structured-output”, “deftool” specs. Make them concrete as IR.

### 58.1 ToolContractIR

```ts
export interface ToolContractIR extends NodeBase {
  tag: "ToolContract";
  id: string;                 // stable hash id
  name: string;               // tool name used at runtime
  version: string;            // semver for the contract

  inputSchema: VRef;          // Schema ref (or embed SchemaIR directly)
  outputSchema: VRef;
  errorSchema?: VRef;

  idempotency: "idempotent" | "non-idempotent" | "unknown";
  capabilityTag: string;      // ties into object-capability policy
  quotaGroup?: string;

  resourceModel?: {
    typicalTimeMs?: number;
    worstTimeMs?: number;
    typicalTokens?: number;
  };

  provenancePolicy?: {
    mustAttachEvidence?: boolean;
    evidenceMode?: Array<"observed"|"measured"|"derived">;
    stalenessInputs?: Array<"toolContract"|"schema"|"sourceFingerprint"|"oracleConfig">;
  };
}
```

### 58.2 SchemaIR

You can do either:

* embed raw JSON Schema (pragmatic), or
* define a small structural schema calculus (more portable)

I’d do **both**:

```ts
export interface SchemaIR extends NodeBase {
  tag: "Schema";
  id: string;
  kind: "JsonSchema" | "FrameSchema";
  jsonSchema?: any;      // canonical JSON required if used for hashing
  frameSchema?: FrameSchemaNode;
}

export type FrameSchemaNode =
  | SAny | SString | SNumber | SInt | SBool | SNil
  | SList | SRecord
  | SUnion
  | SRef;

export interface SAny extends NodeBase { tag: "SAny" }
export interface SString extends NodeBase { tag: "SString" }
export interface SNumber extends NodeBase { tag: "SNumber" }
export interface SInt extends NodeBase { tag: "SInt" }
export interface SBool extends NodeBase { tag: "SBool" }
export interface SNil extends NodeBase { tag: "SNil" }

export interface SList extends NodeBase { tag: "SList"; item: FrameSchemaNode }
export interface SRecord extends NodeBase {
  tag: "SRecord";
  fields: Array<{ key: string; schema: FrameSchemaNode; optional?: boolean }>;
  closed?: boolean; // if true, no extra fields allowed
}

export interface SUnion extends NodeBase { tag: "SUnion"; options: FrameSchemaNode[] }
export interface SRef extends NodeBase { tag: "SRef"; schemaId: string }
```

Then you provide:

* `compileFrameSchemaToJsonSchema()`
* `validateValueAgainstFrameSchema()`

This creates a bridge where:

* language-level schemas can be structural
* tool-level schemas can still be JSON Schema for interoperability

---

## 59) The “IR-to-kernel” operational mapping: CEKS frames for each Flow node

If you want *optimal power*, the runtime must be:

* deterministic under replay
* precise about evaluation order
* able to interleave concurrency and nondet fairly
* instrumented

You’re already CEKS-ish with a scheduler. Make the mapping explicit.

### 59.1 Machine components (conceptual)

* **C**ontrol: current FlowIR node
* **E**nvironment: bindings + capability tokens + function table + contracts
* **K**ontinuation: stack of frames
* **S**tore: separate (port) or internal store

### 59.2 Frame types (suggested)

```ts
type KFrame =
  | KBind
  | KCatch
  | KAllGather
  | KRaceWait
  | KAnyWait
  | KSeq
  | KBranch
  | KLoop
  | KWithBudget
  | KWithTimeout
  | KInfer
  | KToolCall
  | KValidate
  | KCommit
  | KEmit
  | KObserve;

interface KBind { tag:"KBind"; kFn: FnClosure; }
interface KCatch { tag:"KCatch"; handlerFn: FnClosure; }
interface KSeq { tag:"KSeq"; rest: FlowIR[]; acc: ValueIR[]; } // if you choose to collect
// etc
```

**Execution sketch:**

* `FPure(v)`:

  * pop frame
  * if top is `KBind`, apply binder to `v` and continue
  * if top is `KSeq`, push result and continue with next
* `FBind(flow,k)`:

  * push `KBind(k)`
  * set control to `flow`
* `FCatch(flow, handler)`:

  * push `KCatch(handler)`
  * set control to `flow`
* `FFail(reason, ctx)`:

  * unwind K until you find `KCatch`
  * if found: call handler with Failure and continue
  * else: terminal failure

This yields structured error handling (like exceptions, but typed and replayable).

### 59.3 Concurrency nodes (`FAll`, `FRace`, `FAny`)

A clean approach: each child flow runs in a fiber:

* `FAll(flows)`:

  * spawn fibers for each flow
  * join all
  * if any fail: decide policy (fail-fast vs accumulate failures)
  * return list of results

* `FRace(flows)`:

  * spawn all fibers
  * return first success (or first completion depending on semantics)
  * cancel remaining fibers or leave them (but cancellation semantics must be explicit)

* `FAny(flows)`:

  * spawn sequentially or in parallel
  * return first non-failure

**Determinism requirement**: fiber scheduling decisions must be recorded in the ledger (you already have `extractDecisions` / replay policy).

Patterns:

* **Fork/Join**
* **Hedged Requests** (race)
* **Bulkhead** (with budgets per fiber)

---

## 60) Budget semantics: budgets as a first-class effect algebra, not a passive counter

You already have `Budget` in LambdaRLM. Make budget checks universal and structural.

### 60.1 Budget as capability token

Instead of “global mutable budget”, treat budget as:

* `BudgetToken { remaining: ... }` in the environment
* `WithBudget(budgetExpr, flow)` installs a *new* token (child budget), and when done, returns remainder to parent (or consumes as executed)

This prevents accidental “out of band” consumption.

### 60.2 Budget lints become structural dominator checks

At lint time:

* build a Flow graph
* compute dominators
* ensure every node tagged with effects in `{Oracle,Tool}` is dominated by `FWithBudget`

This is compiler-grade correctness enforcement.

### 60.3 Cost propagation via PrimitiveDescriptor resources

You compute static estimates by folding the IR:

* `estimate(FPure)` = 0
* `estimate(FInfer)` = descriptor for `infer` plus options-based multipliers
* `estimate(FAll(children))` = sum (or max time) depending on cost dimension
* `estimate(FLoop)` = multiply by loop bound estimate (or mark unknown)

Then:

* solver meta-search can allocate budgets
* CI can fail if plan exceeds known quotas

Pattern vocabulary:

* **Resource-Aware Planning**
* **Cost Semantics** / **Abstract Interpretation** (over resource domain)

---

## 61) Nondeterminism integration: unify “stream semantics” and “frontier semantics” cleanly

You currently have:

* Omega nondet types (jobs/frontier policies)
* LambdaRLM nondet stream monad (amb)
* stream libraries in both Omega and LambdaRLM

To avoid a split brain, define a **single conceptual interface**:

> Nondeterministic computation produces a (possibly infinite) **Stream of candidates** with attached **score + constraints + provenance**.

### 61.1 Candidate stream element type

```ts
export interface Candidate extends NodeBase {
  tag: "Candidate";
  value: ValueIR;
  score?: { value: number; confidence?: number; source?: string };
  constraints?: ValueIR;   // alist or record
  evidence?: ValueIR;      // evidence ref
}
```

### 61.2 Two engines, one output

* **Engine 1: Fair Stream Engine**

  * classic `stream-interleave`, `stream-flatmap-fair`
  * good for enumerations, relational programming, SICP amb

* **Engine 2: Frontier Engine**

  * BFS/DFS/Beam/Best/Sample
  * integrates scoring, budgets, early pruning
  * produces a stream whose tail is computed by running the frontier scheduler

Both output `Stream[Candidate]`.

### 61.3 How to represent nondet in FlowIR

Two options:

#### Option A — Keep nondet as library (ValueIR streams)

FlowIR stays unchanged; nondet is data-level.

Pros:

* minimal kernel changes

Cons:

* hard to attach spans and determinism for exploration policies

#### Option B — Add explicit nondet Flow nodes (recommended for “optimal power”)

```ts
export interface FNondet extends NodeBase {
  tag: "FNondet";
  policy: ValueIR;        // mode/frontier/beam width/etc
  generator: VRef;        // Fn: () -> Stream[Candidate]
  evaluator: VRef;        // Fn: Candidate -> Flow[Candidate] (score/validate/refine)
  limit?: ValueIR;        // optional bound
}
```

Then kernel:

* treats nondet exploration as a first-class execution mode
* logs frontier decisions into the scheduler ledger
* can replay it precisely

This gives you an “Agentic Search VM” but still in the same Flow algebra.

---

## 62) Constraint propagation as a first-class subsystem (optional, but high leverage)

You already have a Sussman-style constraint network in Omega. Don’t leave it as a “random internal thing”; elevate it into IR.

### 62.1 Introduce a ConstraintPort and ConstraintIR

You can keep it behind a port so it remains testable and replaceable:

* `ConstraintPort` supports:

  * create network
  * add variables/cells
  * add propagators
  * run propagation
  * query contradictions / explanations

Then in FlowIR you add nodes:

```ts
export interface FConstraintNewNet extends NodeBase { tag:"FConstraintNewNet"; config?: ValueIR }
export interface FConstraintAddProp extends NodeBase { tag:"FConstraintAddProp"; net: ValueIR; prop: ValueIR }
export interface FConstraintRun extends NodeBase { tag:"FConstraintRun"; net: ValueIR; config?: ValueIR }
export interface FConstraintQuery extends NodeBase { tag:"FConstraintQuery"; net: ValueIR; query: ValueIR }
```

**Why bother?** Because then:

* propagation runs produce spans
* contradictions become structured failures with explanations
* you can combine constraints + nondet + solvers coherently

Patterns:

* **Blackboard** / **Constraint Network**
* **Explainable Failure** (propagator explanations become provenance)

---

## 63) Replay, receipts, and claim-checks: make “streams” and “caches” compositional

You already have receipt-backed streams in Omega. Formalize it:

### 63.1 Receipt type as a ValueIR record

```ts
export interface ReceiptIR extends NodeBase {
  tag: "Receipt";
  kind: "StreamSegment" | "CacheEntry" | "ToolResult";
  id: string;          // content hash
  meta?: ValueIR;      // provenance pointers
}
```

### 63.2 Claim Check pattern baked into `commit` + `observe`

* `commit(store, key, value)` stores large payloads and returns `Receipt`
* downstream passes around the receipt instead of full payload
* `observe(source, receipt)` hydrates

This is how you keep large RAG contexts and tool outputs from ballooning prompts.

---

## 64) Lints as first-class “IR passes” (not just CLI rules)

Instead of ad-hoc lint scripts, define a `Pass` interface:

```ts
export interface PassResult {
  diagnostics: Array<{ code: string; severity: "error"|"warn"; message: string; span?: Span; data?: any }>;
  transformed?: IRBundle; // optional rewriting passes
}

export interface Pass {
  id: string;
  phase: "lowering" | "normalize" | "lint" | "optimize";
  run(bundle: IRBundle, registry: Registry): PassResult;
}
```

Then:

* `normalize` is a pass
* `budget dominator lint` is a pass
* `schema compatibility lint` is a pass
* `tool contract required lint` is a pass

This makes your tooling pipeline *composable*:

Pattern: **Pipeline** / **Chain of Responsibility**.

---

## 65) Docgen and binding generation: eliminate “REFERENCE-LIBRARIES.md drift” permanently

Now that you have:

* a registry
* an IR schema
* a pass pipeline

You can generate **all documentation artifacts**.

### 65.1 What gets generated

* `REFERENCE-LIBRARIES.md`

  * from registry descriptors + source refs
* `ARCHITECTURE/14-STDLIB.md`

  * from lowering rules + descriptor grouping
* `stdlib symbol index`

  * machine-readable JSON + CLI `apropos`
* `golden lowering tests`

  * by enumerating descriptors with examples

### 65.2 Language bindings

You can generate:

* LambdaLLM surface stubs (macros/intrinsics) from descriptors
* TypeScript host APIs from the same descriptors

This is where you get “multi-language coherence” without manual duplication.

Patterns:

* **Model-Driven Engineering**
* **Scaffolding** as a build product (not hand-maintained)

---

## 66) Versioning and compatibility: you need two separate version axes

### 66.1 IR version vs runtime version

* **IR version** (`frameir@1`) changes only when the node schemas / semantics change
* **runtime version** (`omega-kernel@x.y.z`) changes for implementation details

Replay logs should record:

* IR version
* runtime version
* registry versions (descriptor set hash)

### 66.2 Contract versioning

Tool contracts are *their own semver stream*.

Rule:

* If input/output schema changes in any breaking way, bump major version.
* FlowIR referencing a contract should reference `{name, major, hash}`.

### 66.3 Migration strategy

Provide migrations as passes:

* `migrateFrameIR_1_0_to_1_1(bundle)` etc.
* CLI can auto-migrate with an explicit flag

Pattern: **Strangler Fig** / **Expand-Contract** for gradual evolution.

---

## 67) “Optimal power” language stacking: concrete recommendations

You asked earlier: “what should be languages and how stacking in abstraction layers.”

With the above IR in place, here’s the crisp stack:

### 67.1 Host/runtime implementation language: TypeScript (Omega)

Keep:

* evaluator
* scheduler
* receipts/stream runtime
* ports/adapters

Why TS works:

* rapid iteration, good tooling
* easy JSON interop for IR
* can still be made deterministic if you treat ports as explicit effects

### 67.2 Language of user programs + solvers: LambdaLLM / LambdaRLM (Lisp)

Keep:

* solver combinators
* strategies
* repair loops
* provenance reasoning
* domain algebra rewriting

Because:

* Lisp macros are ideal for *language-oriented programming*
* solver composition becomes DSL-level, not framework-level

### 67.3 Canonical interchange + optimization substrate: FrameIR (data)

This is the “truth layer”:

* caching keys
* replay logs
* linting
* provenance binding
* doc generation

### 67.4 Additional language (optional) for “static spec”: a tiny declarative schema language

If JSON Schema becomes painful, a FrameSchema DSL (like §58.2) becomes your stable core.

So the **language set** is:

* TS for runtime
* Lisp for DSL + solver logic
* FrameIR (JSON) as interchange and audit substrate
* optional FrameSchema DSL as a type/contract kernel

This is textbook **Ports & Adapters** + **Compiler/VM split**.

---

## 68) Concrete next implementation steps (high ROI order)

If you want to maximize power per unit work, do it in this order:

1. **Implement `@frameir` package**

   * types + canonical codec + hashing + normalization

2. **Implement Registry**

   * primitive descriptors for all FlowIR nodes + key stdlib intrinsics

3. **Switch LambdaLLM lowering to emit FlowIR bundles**

   * even if runtime still executes “directly” initially, make the IR real

4. **Add kernel interpreter for FlowIR subset**

   * start with FPure, FBind, FFail, FCatch, FInfer, FToolCall, FValidate

5. **Add spans + replay logger**

   * wrap ports, record interactions, decisions

6. **Add lints as passes**

   * budget dominator lint is first

7. **Integrate LambdaRLM solvers**

   * make them compile to FlowIR
   * stop TS-side solver duplication

8. **Only then** expand nondet + constraints as first-class Flow nodes

   * because you’ll have the observability and replay foundation already

This sequence avoids the trap of building “cool nondet engines” before you can debug them.

---

If you want me to continue further, the next “continuation” that meaningfully advances the architecture is to write:

* an explicit **JSON Schema** for `IRBundle`, `FlowIR`, `PromptIR`, `ValueIR`
* a complete **Registry seed** for your top ~40 primitives (infer, tool-call, validate, bind, loop, all/race/any, stream ops)
* and a concrete **Port interface suite** (`OraclePort`, `ToolPort`, `StorePort`, `ClockPort`, `RngPort`) with replay proxies.

That is the point where your system stops being a set of documents and becomes a self-consistent platform specification.
