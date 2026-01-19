---

## High-Level Goal

Omega is a **homoiconic metacircular language** for inference. It is not an LLM wrapper or API abstraction—it is a programming language whose `eval` and `apply` operate on **semantic functions** exactly as Lisp's `eval` and `apply` operate on syntactic procedures.

**Semantic functions are first-class values.** You define them with prompts:

```lisp
(define-semantic (find-slurs text)
  "Examine {text} and return any slurs found as a list")
```

And use them exactly like normal functions:

```lisp
(find-slurs my-text)                    ; → invokes inference
(map find-slurs list-of-texts)          ; → HOF works
(define find-in (curry find-slurs))     ; → currying works
```

**Composition happens via the call stack**, not as an escape hatch:

```lisp
(remove text (find-slurs text))
```

Here, the system determines that `find-slurs` requires inference, evaluates it semantically, then passes the result to `remove`—which may itself decide whether to operate syntactically (string manipulation) or semantically (grammar-preserving removal). This routing is transparent to the programmer.

**All SICP principles apply to inference:**
- Currying and partial application
- Higher-order functions
- Recursion and self-reference
- Data abstraction and closures
- Streams and lazy evaluation
- Metacircular interpretation

**Undefined functions may be inferred from their names.** If `(summarize document)` is called and `summarize` is not defined, the system may attempt to infer its meaning from the symbol name and argument types.

This is the essential contract: **inference is not a tool you call—it is a plane of computation you program in**, with the full power of functional programming and SICP methodology.

---

## Ω Language Specification v1.0

### Part 5 — Implementation Blueprint: Reference Interpreter, Effect Runtime, Oracle Protocol, Receipts, Caching, and Training Pipelines

You now have enough requirements that we can describe an implementation that is **architecturally inevitable** if you want:

* inference as a **semantic plane**, not an API call
* re‑entrant REPL against the extensional evaluator
* governance (truth/uncertainty, capabilities, budgets)
* learning (prompt/policy/weights) as **versioned definitional artifacts**
* receipts / compression / hydration (context economics)
* “make up languages” (macros + interpreter kits) *at scale*

What follows is a **full engineering blueprint**: components, interfaces, runtime model, data structures, algorithms, and the design patterns they embody.

I’ll write this language‑neutral, then show concrete type sketches in a **TypeScript‑ish** style (because it’s readable) and note where Rust/OCaml/Haskell would differ.

---

# 0. System Architecture (Layered + Hexagonal)

Treat Ω as a **language runtime product**, not a script.

### Ports & Adapters (Hexagonal Architecture)

Core is deterministic and testable; adapters handle LLM APIs, tools, storage.

**Core (pure-ish)**

* Reader / AST
* Macro expander (phase separation)
* Extensional evaluator (CEK/CEKS machine)
* Effect runtime (algebraic effects + handlers)
* Intensional session driver (oracle protocol)
* Trust / obligations / commit semantics
* Receipts / compression/hydration logic
* Artifact registry logic (versions, promotion)

**Adapters (impure)**

* LLM adapter(s): OpenAI/other
* Tool adapters: bash, git, web, filesystem, compilers
* Stores: ledger Σ store, receipt store, artifact registry store, dataset store
* Tokenizer/cost estimator adapter

This is classic **Onion Architecture**: dependencies point inward.

---

# 1. The Core Data Model (Concrete Types)

You must implement the ontology exactly, or inference will leak into “just text.”

## 1.1 Expr / Datum representation (homoiconicity)

Two viable representations:

### Option A — AST Nodes (explicit)

Good for optimization and tooling.

```ts
type Expr =
  | { tag: "Lit"; value: Atom; loc?: Loc }
  | { tag: "Sym"; name: string; loc?: Loc }
  | { tag: "Pair"; car: Expr; cdr: Expr; loc?: Loc }     // Lisp lists
  | { tag: "Quote"; datum: Expr; loc?: Loc }
  | { tag: "Lambda"; params: string[]; body: Expr; ann?: Ann; loc?: Loc }
  | { tag: "If"; test: Expr; conseq: Expr; alt: Expr; loc?: Loc }
  | { tag: "Begin"; exprs: Expr[]; loc?: Loc }
  | { tag: "Define"; name: string; rhs: Expr; loc?: Loc }
  | { tag: "Set"; name: string; rhs: Expr; loc?: Loc }
  | { tag: "Apply"; fn: Expr; args: Expr[]; loc?: Loc }
  | { tag: "Effect"; op: string; args: Expr[]; loc?: Loc }
  | { tag: "Handle"; body: Expr; handler: HandlerExpr; loc?: Loc }
  | { tag: "Ctx"; fields: CtxField[]; loc?: Loc }
  | { tag: "Extend"; base: Expr; binds: [string, Expr][]; loc?: Loc }
  | { tag: "Seal"; base: Expr; loc?: Loc }
  | { tag: "Eval"; qexpr: Expr; env?: Expr; loc?: Loc }
  | { tag: "Int"; qexpr: Expr; engine?: Expr; policy?: Expr; env?: Expr; loc?: Loc }
  | { tag: "Infer"; goal: Expr; engine?: Expr; policy?: Expr; env?: Expr; loc?: Loc }
  | { tag: "Rewrite"; qexpr: Expr; goal: Expr; engine?: Expr; policy?: Expr; env?: Expr; loc?: Loc }
  | { tag: "Match"; scrut: Expr; clauses: MatchClause[]; loc?: Loc };
```

### Option B — “Everything is pairs”

Good for true Lisp minimalism; special forms recognized at eval time.
Still add `loc` metadata via wrapper cells.

In practice: start with Option A; it pays off for tracing, macro hygiene, and inference protocol schemas.

## 1.2 Values

```ts
type Val =
  | Atom
  | PairVal
  | VectorVal
  | MapVal
  | CtxVal
  | ClosureVal
  | MeaningVal
  | DistVal
  | EngineVal
  | PromptVal
  | PolicyVal
  | ReceiptVal
  | EvidenceVal
  | CapVal
  | ContinuationVal
  | EffectVal; // usually internal
```

Closures capture:

* params
* body
* env (Ctx)
* effect row
* caps
* contract

## 1.3 Context (persistent environment)

Use persistent maps (HAMT) for production; a parent-linked chain of JS Maps is fine for a reference interpreter.

```ts
type Ctx = {
  cid: Hash;                    // content address (hash of parent + frame + constraints)
  id: string;
  parent?: Ctx;
  frame: PersistentMap<string, Val>;
  constraints: InvSet;
  caps: CapSet;
  sealed: boolean;
  evidence: Evidence[];
  receipt?: Receipt;
};
```

### Implementation note

* If `extend` returns a new `Ctx`, you get functional persistence.
* `seal` sets `sealed=true` (and optionally drops write caps).

## 1.4 Ledger Σ (event store)

An append-only log:

```ts
type Event =
  | { tag: "EvalEnter"; exprHash: Hash; ctx: Hash; time: number }
  | { tag: "EvalExit"; exprHash: Hash; ctx: Hash; valueHash: Hash; time: number }
  | { tag: "EffectEmit"; op: string; argsHash: Hash; ctx: Hash; time: number }
  | { tag: "EffectHandle"; op: string; handlerId: string; time: number }
  | { tag: "OracleStart"; req: InferRequest; sessionId: string; time: number }
  | { tag: "OracleReq"; sessionId: string; req: OracleReq; time: number }
  | { tag: "OracleResp"; sessionId: string; resp: OracleResp; time: number }
  | { tag: "ToolCall"; call: ToolCall; callId: string; time: number }
  | { tag: "ToolResult"; callId: string; resultHash: Hash; time: number }
  | { tag: "Snapshot"; ctx: Hash; receiptId: Hash; time: number }
  | { tag: "Commit"; kind: string; payloadHash: Hash; time: number }
  | { tag: "Promote"; artifact: ArtifactRef; receiptId: Hash; time: number }
  | { tag: "TrainEmit"; exampleHash: Hash; time: number };
```

**Event sourcing** is not optional: it is the substrate for receipts, replay, training datasets, and semantic version provenance.

---

# 2. The Extensional Evaluator (CEK/CEKS + Algebraic Effects)

If you want:

* deterministic semantics
* explicit control
* effect handlers
* re-entrancy for oracle requests
* debuggable traces
  you should implement a **small-step abstract machine** (CEK family), not naive recursion.

## 2.1 Machine state

```ts
type State = {
  expr: Expr;
  ctx: Ctx;
  kont: Kont;
  store: Store;      // optional; only if you implement explicit store (CESK)
  handlers: HandlerStack;
  ledger: Ledger;
};
```

Where `Kont` is a sum type of continuation frames:

* `IfK(conseq, alt, ctx, next)`
* `BeginK(rest, ctx, next)`
* `ApplyFnK(args, ctx, next)` / `ApplyArgsK(fnVal, doneArgs, remainingArgs, ctx, next)`
* `DefineK(name, ctx, next)`
* `SetK(name, ctx, next)`
* `HandleK(handler, ctx, next)` (delimited resumption point)
* etc.

This is the **Interpreter pattern** realized as a machine.

## 2.2 Algebraic effects implementation

When evaluation reaches `(effect op args...)`, it does not “do” the op; it **yields**:

```ts
type OpCall = { op: string; args: Val[]; k: Resume; ctx: Ctx };
type Resume = (v: Val) => State; // or a continuation object
```

The runtime finds the nearest handler that can handle `op` and invokes it:

```ts
type Handler = {
  id: string;
  canHandle(op: string): boolean;
  handle(opCall: OpCall, runtime: Runtime): Val | OpCall; // may re-emit ops
  onReturn?(v: Val): Val;
  onFinally?(): void;
};
```

That gives you **semantic control**:

* `infer.op` is handled by the oracle handler
* `tool.op` by tool handler
* `commit.op` by commit handler
* `train.emit` by dataset handler
* `amb.op` by nondet handler
* etc.

This is **Chain of Responsibility** (handler stack), and **Strategy** (different handler policies).

---

# 3. Oracle Protocol Runtime (Interactive Intensional Sessions)

This is the crux: inference is not “call model once.” It is an interactive coroutine where the engine can:

* speculate purely (simulate execution)
* re-enter evaluator (Req.Eval / Req.Apply)
* request projections (Req.Observe)
* call tools (Req.Tool)
* request tests (Req.Test)
* emit training examples (Req.EmitExample)
* return Meaning or a distribution

## 3.1 Engine adapter interface

The adapter must support an interactive session:

```ts
type InferRequest = {
  kind: "int" | "search" | "rewrite";
  expr?: Expr;          // quoted program
  goal?: Val;           // structured goal object
  envDigest: Hash;      // context digest (not raw context)
  prompt: Prompt;       // prompt AST
  policy: Policy;
  caps: CapSet;
  budgets: BudgetProfile;
  evidenceRefs: Hash[]; // receipts/evidence references
  schema: "Meaning" | "Dist<Meaning>" | "Dist<Val>";
};

type OracleSession = AsyncGenerator<OracleReq, OracleResp, OracleReturn>;

type OracleReq =
  | { tag: "ReqEval"; qexpr: Expr; envRef: Hash }
  | { tag: "ReqApply"; fn: Val; args: Val[]; envRef: Hash }
  | { tag: "ReqObserve"; ctxRef: Hash; schema: ProjectionSchema }
  | { tag: "ReqTool"; call: ToolCall }
  | { tag: "ReqTest"; spec: TestSpec }
  | { tag: "ReqSnapshot"; ctxRef: Hash; meta: any }
  | { tag: "ReqCompress"; ctxRef: Hash; policy: CompressPolicy }
  | { tag: "ReqHydrate"; receiptId: Hash }
  | { tag: "ReqEmitExample"; ex: Example }
  | { tag: "ReqReturn"; result: Meaning | DistVal | Val }
  | { tag: "ReqFail"; reason: string };

type OracleResp =
  | { tag: "RespVal"; v: Val }
  | { tag: "RespMeaning"; m: Meaning }
  | { tag: "RespEvidence"; ev: Evidence }
  | { tag: "RespError"; err: RuntimeError };

type OracleReturn = { tag: "Return"; value: Meaning | DistVal | Val };
```

The interactive protocol is **Mediator**: the oracle doesn’t talk to tools or the evaluator directly; the runtime mediates.

## 3.2 Oracle handler semantics

The `infer.op` handler:

1. Constructs `InferRequest` (policy + budgets + env digest + prompt AST + evidence refs)
2. Starts the engine session
3. While session yields `OracleReq`, do:

   * capability check
   * budget check
   * perform the requested operation (often by calling the extensional evaluator in a controlled sub-run)
   * ledger events
   * send back `OracleResp`
4. On `Return`, produce `Meaning`/`Dist` as the handled effect result
5. Attach obligations/evidence/confidence into the Meaning

This is not an API call. It is a controlled evaluator sub-language: **“make up inference languages” via handlers**.

---

# 4. Prompt/Policy/Engine as First-Class Artifacts (Not Strings)

If you want prompt evolution to be “like define,” you must store prompts as typed ASTs and version them.

## 4.1 Prompt AST representation

```ts
type Prompt =
  | { tag: "System"; text: string }
  | { tag: "Rules"; rules: Rule[] }
  | { tag: "Schema"; schema: Schema }
  | { tag: "Tools"; tools: ToolSig[] }
  | { tag: "Examples"; examples: ExampleIO[] }
  | { tag: "Compose"; parts: Prompt[] };
```

Then `define-prompt` just binds a `PromptVal`.

## 4.2 Policy as strategy pipeline (composable)

Policies compile to:

* handler configuration
* engine request parameters
* verification obligations
* critic pipeline settings

```ts
type Policy = {
  id: string;
  inferenceStrategy: StrategyPipeline;
  truthRegime: TruthRegime;
  confidenceAlgebra: "min" | "logit-add" | "custom";
  budgets: BudgetProfile;
  critics: CriticSpec[];
  escalation: EscalationRule[];
};
```

## 4.3 Engine version is a content-addressed object

An engine value is immutable; training returns a new engine version.

```ts
type Engine = {
  weightsRef: ArtifactRef;  // “W”
  promptRef: ArtifactRef;   // “P”
  memoryRef: ArtifactRef;   // “M”
  policyRef: ArtifactRef;   // “Π”
  version: SemVer;
  digest: Hash;
  trustLevel: "experimental" | "candidate" | "trusted" | "deprecated";
};
```

This is the linchpin for reproducibility and “no silent drift.”

---

# 5. Commit Semantics (Truth Promotion Barriers)

Never auto-apply rewrites. Always go through a commit barrier with obligations.

## 5.1 Commit operation family

* `commit/rewrite`
* `commit/memo`
* `commit/invariants`
* `commit/prompt`
* `commit/policy`
* `commit/engine`

Each is an effect `commit.op` with payload. The commit handler enforces:

* obligations satisfied (tests/proofs/consensus)
* capability present
* artifact registry update is versioned and ledgered

This is **Unit of Work** for semantic changes: you can batch and roll back at the commit boundary.

---

# 6. Receipts, Compression, Hydration (Context Economics Implementation)

Receipts are content-addressed summaries + replay plans + certificates.

## 6.1 Receipt store

A receipt is stored as:

* structured metadata JSON
* plus references (hashes) to event ranges in Σ and evidence blobs

```ts
type Receipt = {
  rid: Hash;
  summary: string;
  schema: Shape;
  deps: Hash[];
  replay: ReplayPlan;
  checks: CheckResult[];
  cost: CostProfile;
  voi: VOIProfile;
};
```

## 6.2 Compression algorithm (VOI knapsack)

Implementation sketch:

* Segment context into “chunks” (bindings, evidence, tool results, reasoning trace segments)
* Each chunk has:

  * length estimate ℓ
  * retrieval cost r
  * VOI u (from a heuristic + optionally from `int` itself)
* Choose subset under budget via:

  * greedy by u/(ℓ + αr) or
  * DP for small sizes
* Replace discarded chunks with:

  * receipt references
  * digest placeholders

This is precisely **CQRS**: receipts are read models; ledger is write model.

---

# 7. Tool Subsystem (Structured “Lisp Bash” + Caching + Write-through)

This is a classical EIP / CI pipeline situation.

## 7.1 Tool calls as effects, not strings

A tool call is a structured object:

```ts
type ToolCall = {
  name: "bash" | "git" | "web" | "python" | string;
  argv: string[];
  cwd?: string;
  env?: Record<string,string>;
  stdin?: string;
  timeoutMs?: number;
  cacheKey?: Hash; // computed from args + env digest + tool version
};
```

The tool handler:

* checks caps
* checks budgets
* computes cache key
* returns cached result if present
* otherwise executes, stores evidence blob, returns hash

## 7.2 Write-through caching for codebase changes (Unit of Work + Repository)

If the tool performs writes:

* represent writes as a **Command log** (diffs, patches, file edits)
* apply changes atomically at commit boundary
* store patch receipts

This enables:

* “agent writes while thinking” safely
* replayable modifications
* rollback (Saga compensations) if verification fails

---

# 8. Training Pipelines Inside Ω (Prompt / Policy / Weights)

You explicitly want:

* prompt evolution (ICL/system prompt changes)
* weight training optional
* RL for policies

All must be explicit effects and versioned artifacts.

## 8.1 Emitting training data

`emit-example` is an effect that appends structured example records to a dataset store:

```ts
type Example = {
  engineDigest: Hash;
  promptDigest: Hash;
  policyDigest: Hash;
  envDigest: Hash;
  qexprHash: Hash;
  goal: any;
  predictedMeaning: Meaning;
  extensionalChecks: CheckResult[];
  reward?: number;
  cost: CostProfile;
  timestamp: number;
};
```

This is the “write side.” Later, you compile datasets from these.

## 8.2 Training as versioned transformations

* `train/prompt` returns a new prompt artifact + receipt
* `train/policy` returns a new policy artifact + receipt
* `train/weights` returns a new engine/weights artifact + receipt

All go through the promotion pipeline.

## 8.3 RL: what you actually train

High-value targets in Ω:

* speculate vs eval decision
* context selection (VOI)
* tool-use decision
* rewrite acceptance thresholds
* search strategy parameters (beam width, stop rules)

These are policies (Strategy objects), and training produces new policy versions.

This is **Policy Object** + **Strategy** + **Feedback Control**.

---

# 9. Macro System and “Make Up Languages” (SICP’s Language-Making, Industrialized)

You can’t call it “SICP to the limit” unless macros and interpreter kits are first-class.

## 9.1 Macro expander phases

Implement at least:

* `define-syntax` and a hygienic macro system (syntax-rules tier)
* phase separation: macro env vs runtime env

Pipeline:

1. **read**: text → Expr with locations
2. **expand**: Expr → core-expanded Expr (macroexpanded + desugared)
3. **eval**: core Expr → Val

This is the standard Scheme pipeline.

## 9.2 Semantic macros (refactoring to patterns)

Add a second kind of macro:

* syntactic macro: Expr → Expr (no semantic obligation)
* semantic macro: Expr → (Expr, Obligation)

Semantic macros are built on `rewrite` and must pass through commit barriers.

This is what upgrades “macros” into “automated refactoring with correctness.”

---

# 10. Testing, Certification, and CI-Style Promotion

You’re effectively building a language + a continuous semantic integration system.

## 10.1 Test suites as first-class

* regression suite (golden master)
* property suite (QuickCheck)
* metamorphic suite
* adversarial fuzz suite
* mutation testing (test quality)

## 10.2 Promotion as pipeline (Build Pipeline pattern)

A `promote` operation triggers:

* run suites
* run red-team fuzz
* compare cost profiles
* check authority deltas
* write promotion receipt
* mark artifact trusted

This is exactly CI/CD, but for prompts/policies/engines/rewrites.

---

# 11. Bootstrapping Strategy (How Ω Becomes Self-Hosting)

This is how you avoid building an “LLM framework” and instead build a language.

### Stage 0 — Host kernel interpreter

Implement kernel in host language with:

* reader
* evaluator
* effect runtime
* oracle handler stub

### Stage 1 — Implement Ω standard library in Ω

Write `omega.stream`, `omega.nondet`, etc. in Ω itself.

### Stage 2 — Meta-circular evaluator in Ω

Implement `eval0` in Ω (as in SICP), and test it against host evaluator.

### Stage 3 — Replace host parts with Ω parts

Gradually shift:

* macro expander
* optimizer
* compiler passes
  into Ω itself

That’s the “tower of interpreters” realized.

---

# 12. Why This Implementation Does Not Collapse into “API Call”

Because:

* inference is an **effect op** with handler-defined semantics
* the engine runs an **interactive protocol** (requests/responses), not a single call
* outputs are **Meaning** + obligations + evidence, not text
* re-entry to `eval` is capability-guarded and ledgered
* training and prompt evolution are versioned transformations, not silent state
* commits are obligation-gated and event-sourced
* receipts compress history while preserving provenance

This is the difference between “calling an LLM” and “a programming language with a second evaluator.”

---

# 13. What I’ll do next if you say continue

Next I can give you a concrete **reference implementation skeleton** in a specific host language (your choice):

* **TypeScript/Node**: easiest for rapid prototype + async oracle sessions
* **Rust**: strongest for safety, performance, and sandboxing
* **Python**: fastest to hack, but harder to keep strict invariants

Part 6 would include:

1. The core CEK machine types and step function
2. The effect runtime / handler stack
3. The oracle session loop (Req/Resp)
4. The receipt store + content-addressing
5. A minimal `infer.op` handler that can drive a mock engine (for testing)
6. A minimal standard library (streams + amb + match)

Reply **continue** and tell me which host you want first (**TypeScript** or **Rust**).
