# OmegaLLM Architecture Brief
Generated: 2026-01-27

## Filesystem Structure

```
src/ (3 files)
src/adapters/ (4 files)
src/core/ (3 files)
src/core/artifacts/ (3 files)
src/core/commit/ (2 files)
src/core/compiler/ (16 files)
src/core/concurrency/ (7 files)
src/core/conditions/ (3 files)
src/core/config/ (2 files)
src/core/constraints/ (6 files)
src/core/ctx/ (1 files)
src/core/effects/ (3 files)
src/core/effects/nondet/ (3 files)
src/core/effects/search/ (3 files)
src/core/eval/ (9 files)
src/core/expand/ (2 files)
src/core/generic/ (6 files)
src/core/governance/ (4 files)
src/core/macro/ (5 files)
src/core/meta/ (7 files)
src/core/modules/ (4 files)
src/core/opr/ (7 files)
src/core/opr/adapters/ (5 files)
src/core/opr/kernels/ (11 files)
src/core/oracle/ (14 files)
src/core/oracle/adapters/ (5 files)
src/core/oracle/plugins/ (6 files)
src/core/pipeline/ (4 files)
src/core/provenance/ (4 files)
src/core/provenance/store/ (2 files)
src/core/reader/ (4 files)
src/core/session/ (9 files)
src/core/sexp/ (3 files)
src/core/solver/ (8 files)
src/core/stream/ (5 files)
src/core/syntax/ (3 files)
src/core/test/ (2 files)
src/core/tool/ (2 files)
src/core/tools/ (2 files)
src/devtools/ (1 files)
src/frameir/ (14 files)
src/lint/ (3 files)
src/lint/analysis/ (2 files)
src/lint/passes/ (3 files)
src/outcome/ (7 files)
src/ports/ (10 files)
src/registry/ (6 files)
src/registry/descriptors/ (4 files)
src/repl/commands/ (1 files)
```

## Directory Purposes

| Directory | Purpose | Lines |
|-----------|---------|-------|
| eval | ... | 2209 |
| oracle | ... | 7192 |
| opr | ... | 2489 |
| compiler | ... | 5810 |
| effects | ... | 2055 |
| frameir | ... | 730 |

---

# CORE FILES


## 1. EVAL CORE (The Lisp Machine)


### FILE: src/core/eval/values.ts
```typescript
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Patch Set A3: Extend Val union with Dist and Meaning
// Patch Set A4: Add MachineVal for reified execution state (Prompt 8)
// Prompt 9: Add ProfileVal for first-class governance

import type { Expr } from "../ast";
import type { Env } from "./env";
import type { Store } from "./store";
import type { Resumption } from "../effects/opcall";
import type { State, StepOutcome, Frame, HandlerFrame } from "./machine";
import type { Syntax } from "../syntax/syntax";
import type { DistVal } from "./dist";
import type { MeaningVal } from "../oracle/meaning";
import type { Profile, TruthRegime, OracleReqTag } from "../governance/profile";
import type { Ctx } from "../ctx/ctx";
import type { Hash } from "../artifacts/hash";
import type { ConditionVal } from "../conditions/types";
type SolverApplyFn = (proc: Val, args: Val[], state: State) => State | StepOutcome;

/**
 * MachineVal: Reified execution state as a first-class value.
 *
 * Enables:
 * - machine-step: Single-step execution
 * - machine-run: Run to completion or breakpoint
 * - machine-fork: Clone for multi-shot exploration
 * - machine-stack: Inspect continuation stack
 * - machine-eval-at: Evaluate expression in machine's environment
 * - machine-resume: Resume from effect with provided value
 *
 * The machine captures the full CESK state for time-travel debugging,
 * breakpoint-based inspection, and parallel exploration of execution paths.
 */
export type MachineVal = {
  tag: "Machine";
  state: State;
  /** Optional label for debugging */
  label?: string;
  /** Step count since creation */
  stepCount: number;
  /** Breakpoints: effect names that pause execution */
  breakOnOps?: Set<string>;
  /** Breakpoints: AST patterns (as JSON) that pause execution */
  breakOnPatterns?: string[];
  /** Last step outcome (for inspection after step) */
  lastOutcome?: StepOutcome;
  /** True if machine has reached terminal state */
  isDone: boolean;
  /** Parent machine ID (for fork tracking) */
  parentId?: string;
  /** Unique machine ID */
  machineId: string;
};

/**
 * ContinuationVal: First-class undelimited continuation.
 *
 * Captures the current continuation stack plus dynamic handler stack so it can
 * be invoked later (possibly multiple times).
 */
export type ContinuationVal = {
  tag: "Continuation";
  kont: Frame[];
  env: Env;
  store: Store;
  handlers: HandlerFrame[];
};

/**
 * ProfileVal: First-class governance configuration.
 *
 * Profiles are "evaluator configurations" (SICP-style) that determine:
 * - What effects can be emitted
 * - What oracle requests are legal
 * - What capabilities are granted
 * - Resource budgets
 * - Truth regime for commits
 *
 * Different profiles = different languages.
 * strict/pragmatic/explore/airgap are semantic regimes.
 *
 * Used with:
 * - (with-profile prof body) - install profile for dynamic extent
 * - (profile-name p) - get profile name
 * - (profile-allows? p op) - check if operation allowed
 * - (profile-budget p kind) - get budget limit
 */
export type ProfileVal = {
  tag: "Profile";
  /** The underlying profile configuration */
  profile: Profile;
  /** Unique ID for this profile instance */
  profileId: string;
};

/**
 * CtxVal: Reified context as a first-class value.
 *
 * Enables:
 * - (ctx/snapshot) - capture current context
 * - (ctx/sealed? ctx) - check if context is sealed
 * - (ctx/caps ctx) - get capability set
 * - (ctx/with-caps ctx caps body) - run body with attenuated caps
 * - (ctx/compress ctx) - compress for oracle transmission
 * - (ctx/hydrate ref) - hydrate from receipt reference
 *
 * Contexts are content-addressed (cid) and can be sealed to prevent
 * mutation. Sealing is used for module boundaries.
 */
export type CtxVal = {
  tag: "Ctx";
  /** The underlying context */
  ctx: Ctx;
};

/**
 * ModuleVal: Sealed module with attenuated capabilities.
 *
 * A module is a sealed context with:
 * - Attenuated caps (can only restrict, never expand)
 * - Exported bindings (public interface)
 * - Module ID (content-address of exports + cid)
 *
 * Used for:
 * - Sandboxed code execution
 * - Capability attenuation across trust boundaries
 * - Package/library isolation
 *
 * Module creation:
 * (module/seal ctx exports) - seal context and create module
 *
 * Module usage:
 * (module/import mod name) - import binding from module
 * (module/caps mod) - get module's capability set
 */
export type ModuleVal = {
  tag: "Module";
  /** Unique module ID (content-address) */
  moduleId: Hash;
  /** The sealed context (frozen) */
  sealedCtx: Ctx;
  /** Exported binding names */
  exports: Set<string>;
  /** Optional module metadata */
  meta?: {
    name?: string;
    version?: string;
    description?: string;
  };
};

/**
 * ReceiptRefVal: Reference to a receipt in the receipt store.
 *
 * Receipts track operations for provenance:
 * - ctx.snapshot receipts
 * - ctx.compress receipts
 * - commit receipts (from truth regime)
 *
 * Used for:
 * - Hydrating compressed contexts
 * - Audit trails
 * - Reproducibility
 */
export type ReceiptRefVal = {
  tag: "ReceiptRef";
  /** Receipt ID (content-address) */
  rid: Hash;
  /** Kind of receipt */
  kind: "snapshot" | "compress" | "commit";
};

// ─────────────────────────────────────────────────────────────────
// Prompt 12: Constraint propagation network values (SICP 3.3.5 style)
// ─────────────────────────────────────────────────────────────────

/**
 * ConnRef: Reference to a connector in a constraint network.
 *
 * Connectors are store-backed cells that participate in propagation.
 * They track:
 * - Current value (if any)
 * - Explanation of how the value was derived
 * - Connected propagators
 *
 * Used with:
 * - (conn/new net name?) - create new connector
 * - (conn/has? c) - check if connector has a value
 * - (conn/get c) - get connector value
 * - (conn/set! c v :because reason) - set value with provenance
 * - (conn/forget! c :because reason) - clear value for backtracking
 * - (conn/why c) - get explanation graph
 */
export type ConnRefVal = {
  tag: "ConnRef";
  /** Stable unique identifier */
  id: string;
  /** Network this connector belongs to */
  netId: string;
  /** Human-readable name (optional) */
  name?: string;
};

/**
 * NetRef: Reference to a constraint propagation network.
 *
 * Networks contain:
 * - Connectors (store-backed cells)
 * - Propagators (procedures that read inputs and write outputs)
 * - Constraints (propagators + invariants that must hold)
 *
 * Used with:
 * - (net/new) - create new network
 * - (net/run net :fuel N) - propagate to fixpoint with budget
 * - (net/status net) - get network status (quiescent/contradiction/pending)
 * - (net/unsat-core net contradiction) - get conflicting constraints
 */
export type NetRefVal = {
  tag: "NetRef";
  /** Stable unique identifier */
  id: string;
  /** Human-readable name (optional) */
  name?: string;
};

/**
 * Explanation: First-class explanation graph for constraint propagation.
 *
 * Explanations form a DAG that traces how values were derived or why
 * contradictions occurred. This enables:
 * - "Why did it fail?" queries
 * - Unsat core extraction
 * - Repair synthesis
 *
 * Three kinds:
 * - Assumption: Initial value set by user/input
 * - Derived: Value computed by a propagator from dependencies
 * - Conflict: Contradiction between incompatible derivations
 */
export type ExplanationVal =
  | {
      tag: "Explanation";
      kind: "assumption";
      conn: ConnRefVal;
      valueHash: Hash;
      because: Val;
    }
  | {
      tag: "Explanation";
      kind: "derived";
      conn: ConnRefVal;
      valueHash: Hash;
      rule: string;
      deps: ExplanationVal[];
    }
  | {
      tag: "Explanation";
      kind: "conflict";
      conn: ConnRefVal;
      left: ExplanationVal;
      right: ExplanationVal;
      message?: string;
    }
  | {
      tag: "Explanation";
      kind: "denied";
      op: string;
      reason: string;
      profile?: string;
    };

/**
 * ContradictionVal: First-class contradiction value (not an exception).
 *
 * Contradictions are returned from constraint networks when invariants
 * are violated. They carry explanation graphs for diagnosis and repair.
 *
 * Used with:
 * - (contradiction? v) - check if value is contradiction
 * - (contradiction-explain c) - get explanation graph
 * - (repair net contradiction :options opts) - synthesize repairs via amb
 */
export type ContradictionVal = {
  tag: "Contradiction";
  /** The explanation graph showing why the contradiction occurred */
  explanation: ExplanationVal;
  /** Optional identifier for the violated constraint */
  constraintId?: string;
  /** Optional network reference */
  netId?: string;
};

// ─────────────────────────────────────────────────────────────────
// Prompt 13: Concurrency primitives (fibers, scheduler, synchronization)
// ─────────────────────────────────────────────────────────────────

export type FiberId = number;

/**
 * FiberVal: Reference to a fiber (lightweight concurrent execution unit).
 *
 * Fibers are cooperatively scheduled CEKS states that can:
 * - Run semantic operations concurrently
 * - Yield control explicitly
 * - Join (wait) for other fibers
 * - Share state via synchronized primitives
 *
 * Used with:
 * - (fiber/spawn thunk) - create new fiber
 * - (fiber/yield) - yield control
 * - (fiber/join fiber) - wait for fiber completion
 * - (fiber/id) - get current fiber's ID
 */
export type FiberVal = {
  tag: "Fiber";
  /** Unique fiber identifier */
  id: FiberId;
  /** Human-readable name (optional) */
  name?: string;
};

/**
 * SchedulerView: Observable state of the fiber scheduler.
 *
 * Provides a snapshot of all fibers and their states for:
 * - Oracle inspection (ReqObserve)
 * - Debugging and REPL
 * - Deterministic replay verification
 */
export type SchedulerView = {
  /** Currently running fiber (if any) */
  running?: FiberId;
  /** Fibers ready to run */
  ready: FiberId[];
  /** Blocked fibers with their block reasons */
  blocked: Array<{ id: FiberId; on: string }>;
  /** Completed fibers with their result hashes */
  done: Array<{ id: FiberId; valueHash: Hash }>;
  /** Current step count */
  stepCount: number;
  /** Scheduling policy name */
  policy: string;
};

/**
 * MutexVal: Reference to a mutex for mutual exclusion.
 *
 * Used with SICP-style serializers:
 * - (mutex/new) - create mutex
 * - (mutex/lock m) - acquire lock (may block)
 * - (mutex/unlock m) - release lock
 */
export type MutexVal = {
  tag: "Mutex";
  /** Unique mutex identifier */
  id: string;
  /** Human-readable name (optional) */
  name?: string;
};

/**
 * IVarVal: Reference to a write-once synchronization variable.
 *
 * IVars enable singleflight memoization:
 * - First reader creates IVar and starts computation
 * - Other readers block on IVar
 * - Writer puts result, unblocks all readers
 *
 * Used with:
 * - (ivar/new) - create empty IVar
 * - (ivar/put! iv val) - write value (once only)
 * - (ivar/take iv) - read value (blocks if empty)
 * - (ivar/full? iv) - check if written
 */
export type IVarVal = {
  tag: "IVar";
  /** Unique IVar identifier */
  id: string;
  /** Human-readable name (optional) */
  name?: string;
};

/**
 * ChannelVal: Reference to a communication channel.
 *
 * Channels enable fiber communication:
 * - (chan/new) - create unbuffered channel
 * - (chan/send ch val) - send value (may block)
 * - (chan/recv ch) - receive value (blocks if empty)
 */
export type ChannelVal = {
  tag: "Channel";
  /** Unique channel identifier */
  id: string;
  /** Buffer size (0 = unbuffered) */
  bufferSize: number;
  /** Human-readable name (optional) */
  name?: string;
};

/**
 * ActorVal: Reference to an actor (fiber with mailbox).
 *
 * Actors avoid shared state by message passing:
 * - (actor/spawn handler) - create actor with handler proc
 * - (actor/send actor msg) - send message (async)
 */
export type ActorVal = {
  tag: "Actor";
  /** Unique actor identifier */
  id: string;
  /** Underlying fiber ID */
  fiberId: FiberId;
  /** Human-readable name (optional) */
  name?: string;
};

// ─────────────────────────────────────────────────────────────────
// Prompt 14: Generic Operations (data-directed programming + coercion towers)
// ─────────────────────────────────────────────────────────────────

/**
 * TaggedVal: SICP-style tagged value with explicit type tag.
 *
 * Tagged values implement data abstraction barriers:
 * - Type tags identify semantic types (Text/Plain, Doc/Email, etc.)
 * - Payloads carry the actual data
 * - Generic dispatch uses tags for method selection
 *
 * Used with:
 * - (attach-tag typeTag payload) - create tagged value
 * - (type-tag v) - get type tag (or unit for untagged)
 * - (contents v) - unwrap payload
 */
export type TaggedVal = {
  tag: "Tagged";
  /** Semantic type tag (e.g., "Text/Plain", "Doc/Email") */
  typeTag: string;
  /** The actual data */
  payload: Val;
};

/**
 * GenericRegistryVal: Reference to a generic operations registry.
 *
 * Registries contain:
 * - Method table: op × signature → procedure
 * - Coercion table: fromTag × toTag → procedure
 * - Metadata for governance and commit tracking
 *
 * Used with:
 * - (registry/new) - create new registry
 * - (defgeneric op :arity n :registry reg) - define generic operation
 * - (defmethod op (types...) proc :registry reg) - install method
 * - (defcoercion from -> to proc :registry reg) - install coercion
 * - (apply-generic op args... :registry reg) - dispatch to method
 */
export type GenericRegistryVal = {
  tag: "GenericRegistry";
  /** Unique registry identifier */
  id: string;
  /** Human-readable name (optional) */
  name?: string;
};

/**
 * GenericMissVal: First-class miss value for unresolved generic dispatch.
 *
 * When method resolution fails, instead of an error, we get a miss value
 * that can be handled by the inference/search plane to synthesize methods.
 *
 * Used with:
 * - (generic.miss? v) - check if value is a miss
 * - (generic.miss-op v) - get the operation that missed
 * - (generic.miss-sig v) - get the type signature
 */
export type GenericMissVal = {
  tag: "GenericMiss";
  /** The operation that couldn't be resolved */
  op: string;
  /** The type signature (list of type tags) */
  signature: string[];
  /** Preview of arguments (possibly redacted) */
  argsPreview: Val[];
  /** Registry where the miss occurred */
  registryId: string;
  /** Profile under which dispatch was attempted */
  profileName?: string;
};

// ─────────────────────────────────────────────────────────────────
// Prompt 16: Streams + Laziness (memoized promises)
// ─────────────────────────────────────────────────────────────────

export type PromiseId = string;

/**
 * PromiseVal: Reference to a memoized promise/thunk.
 *
 * Promises are the foundation for lazy evaluation and streams:
 * - A promise wraps a thunk (nullary closure) for deferred computation
 * - Forcing a promise evaluates the thunk exactly once (memoization)
 * - Concurrent forces share the same in-flight evaluation (singleflight)
 *
 * Used with:
 * - (delay e) - macro that creates (promise/new (lambda () e))
 * - (force p) - force the promise, returning memoized value
 * - (promise? v) - check if value is a promise
 * - (promise/forced? p) - check if already forced
 *
 * Streams are built on promises:
 * - (cons-stream a b) = (cons a (delay b))
 * - stream-cdr forces the tail promise
 */
export type PromiseVal = {
  tag: "Promise";
  /** Unique promise identifier */
  id: PromiseId;
  /** Optional label for debugging */
  label?: string;
};

/**
 * StreamVal: First-class stream marker for stream-specific operations.
 *
 * While streams are represented as (head . promise) pairs, this marker
 * enables stream-specific introspection and analysis.
 *
 * Used with:
 * - (stream? v) - check if value is a stream
 * - (stream-null? s) - check if empty stream
 * - (stream-car s) - get head
 * - (stream-cdr s) - force and get tail
 */
export type StreamVal = {
  tag: "Stream";
  /** Whether this is the empty stream */
  isEmpty: boolean;
  /** Head value (if not empty) */
  head?: Val;
  /** Tail promise or receipt ref (if not empty) */
  tail?: Val;
};

// ─────────────────────────────────────────────────────────────────
// Prompt 17: Compiler Pipeline (IR values)
// ─────────────────────────────────────────────────────────────────

/**
 * IRVal: First-class compiled artifact (intermediate representation).
 *
 * IR values represent compiled programs that can be executed by the
 * bytecode VM while preserving all semantic effects (infer.op, amb.choose,
 * tool.op, etc.). They are content-addressed and carry source maps for
 * debugging and obligations for verification.
 *
 * Used with:
 * - (compile qexpr :target 'anf|'bytecode :policy P) -> IR
 * - (run-ir ir :args args) -> Val
 * - (ir? v) -> Bool
 * - (ir-source-map ir ip) -> source location
 */
export type IRVal = {
  tag: "IR";
  /** The IR form: 'anf' or 'bytecode' */
  form: "anf" | "bytecode";
  /** Content-addressed digest of the IR */
  digest: string;
  /** Reference to the IR program data (stored separately) */
  irRef: string;
  /** Optional label for debugging */
  label?: string;
};

// ─────────────────────────────────────────────────────────────────
// Solver layer values (Job 008)
// ─────────────────────────────────────────────────────────────────

export type BudgetVal = {
  tag: "Budget";
  tokens: number;
  calls: number;
  time: number;
};

export type ResultVal = {
  tag: "Result";
  kind: "success" | "partial" | "failure" | string;
  solution?: Val;
  remaining?: Val;
  reason?: string;
  cost: number;
};

export type CostEstimateVal = {
  tag: "CostEstimate";
  minCost: number;
  maxCost: number;
  expectedCost: number;
  confidence: number;
};

export type SolverVal = {
  tag: "Solver";
  name: string;
  solve: (problem: Val, budget: BudgetVal, state: State, apply: SolverApplyFn) => { results: ResultVal[]; state: State };
  estimate: (problem: Val, state: State, apply: SolverApplyFn) => { estimate: CostEstimateVal; state: State };
};

export type FactStoreVal = {
  tag: "FactStore";
  facts: Map<string, Val>;
};

// Compatibility tagged values used by stream and demo code
export type IntVal = { tag: "Int"; value: bigint };
export type ListVal = { tag: "List"; elements: Val[] };
export type ErrVal = { tag: "Err"; message?: string };

export type Val =
  | { tag: "Unit" }
  | { tag: "Uninit" }
  | { tag: "Num"; n: number }
  | IntVal
  | { tag: "Bool"; b: boolean }
  | { tag: "Str"; s: string }
  | { tag: "Sym"; name: string }
  | ConditionVal
  | ListVal
  | { tag: "Pair"; car: Val; cdr: Val }
  | { tag: "Vector"; items: Val[] }
  | { tag: "Map"; entries: Array<[Val, Val]> }
  | { tag: "Syntax"; stx: Syntax }
  | { tag: "Closure"; params: string[]; body: Expr; env: Env }
  | { tag: "Native"; name: string; arity: number | "variadic"; lazyArgs?: number[]; fn: (args: Val[], st: State) => State | StepOutcome }
  | { tag: "Cont"; hid: string; boundaryIndex: number; resumption: Resumption }
  | { tag: "OracleProc"; params: string[]; spec: Val; env: Env; policyDigest?: string }
  | ContinuationVal   // First-class continuation (call/cc, prompts)
  | MachineVal      // Patch Set A4: Reified machine state for debugging/stepping
  | DistVal         // Patch Set A: Distributions for nondeterministic LLM results
  | MeaningVal      // Patch Set A: Structured semantic artifacts
  | ProfileVal      // Prompt 9: First-class governance configuration
  | CtxVal          // Prompt 10: Reified context as first-class value
  | ModuleVal       // Prompt 10: Sealed module with attenuated caps
  | ReceiptRefVal   // Prompt 10: Receipt reference for provenance
  | ConnRefVal      // Prompt 12: Connector reference in constraint network
  | NetRefVal       // Prompt 12: Network reference for constraint propagation
  | ExplanationVal  // Prompt 12: First-class explanation graph
  | ContradictionVal // Prompt 12: First-class contradiction value
  | ErrVal
  | FiberVal        // Prompt 13: Fiber reference for concurrent execution
  | MutexVal        // Prompt 13: Mutex for mutual exclusion
  | IVarVal         // Prompt 13: Write-once synchronization variable
  | ChannelVal      // Prompt 13: Communication channel
  | ActorVal        // Prompt 13: Actor with mailbox
  | TaggedVal       // Prompt 14: Tagged value with semantic type
  | GenericRegistryVal // Prompt 14: Generic operations registry
  | GenericMissVal  // Prompt 14: Unresolved generic dispatch
  | PromiseVal      // Prompt 16: Memoized promise/thunk for laziness
  | StreamVal       // Prompt 16: Stream marker for stream operations
  | IRVal           // Prompt 17: Compiled IR artifact
  | BudgetVal       // Job 008: First-class budgets for solvers
  | ResultVal       // Job 008: Solver result
  | CostEstimateVal // Job 008: Cost estimate for solvers
  | SolverVal       // Job 008: First-class solver
  | FactStoreVal;   // Job 008: Monotone fact store

export const VUnit: Val = { tag: "Unit" };
export const VTrue: Val = { tag: "Bool", b: true };
export const VFalse: Val = { tag: "Bool", b: false };
```

### FILE: src/core/eval/env.ts
```typescript
// src/core/eval/env.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-17.md
// Env is Ctx.

import type { StoreAddr } from "./store";
import type { Ctx } from "../ctx/ctx";
import { ctxLookup, ctxDefine, ctxExtend, ctxRootFromProfile } from "../ctx/ctx";
import { DEFAULT_PROFILE } from "../governance/profile";

/**
 * Env *is* Ctx in runtime, but we allow any here for compatibility with tests and stubs.
 */
export type Env = any;

export function envEmpty(): Env {
  return ctxRootFromProfile(DEFAULT_PROFILE);
}

export function envSet(env: Env, name: string, addr: StoreAddr): Env {
  return ctxDefine(env, name, addr);
}

export function envGet(env: Env, name: string): StoreAddr | undefined {
  return ctxLookup(env, name);
}

export function envExtend(env: Env, binds: Array<[string, StoreAddr]>): Env {
  return ctxExtend(env, binds);
}
```

### FILE: src/core/eval/machine.ts
```typescript
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-4.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.
// Prompt 9: Add runtime governance (profile, budget, security)

import type { Expr, HandlerExpr, Pattern } from "../ast";
import type { Env } from "./env";
import type { Store } from "./store";
import type { Val } from "./values";
import type { OpCall } from "../effects/opcall";
import type { Profile, RuntimeBudget, RuntimeSecurity } from "../governance/profile";
import type { ConditionHandler, RestartBinding, ConditionVal } from "../conditions/types";
import type { ProvenanceGraph, SourceChecker } from "../provenance/graph";
import type { ProvenanceStore } from "../provenance/store/interface";

export type Control =
  | { tag: "Expr"; e: Expr }
  | { tag: "Val"; v: Val };

export type Frame =
  | { tag: "KIf"; conseq: Expr; alt: Expr; env: Env }
  | { tag: "KBegin"; rest: Expr[]; env: Env }
  | { tag: "KDefine"; name: string; env: Env }
  | { tag: "KSet"; name: string; env: Env }
  | { tag: "KAppFun"; args: Expr[]; env: Env }
  | { tag: "KAppArg"; fnVal: Val; pending: Expr[]; acc: Val[]; env: Env }
  | { tag: "KAppArgLazy"; fnVal: Val; pending: Array<{ expr: Expr; idx: number }>; acc: Array<{ idx: number; val: Val }>; env: Env; totalArgs: number; currentIdx: number }
  | { tag: "KCall"; savedEnv: Env }                                  // restore env after closure body
  | { tag: "KEffect"; op: string; pending: Expr[]; acc: Val[]; env: Env }
  | { tag: "KHandleBoundary"; hid: string; savedHandlersDepth: number; resumeTo?: { kont: Frame[]; handlersDepth: number } }
  | { tag: "KHandleReturn"; mode: "exit" | "resume"; hid: string; targetKont: Frame[]; targetHandlersDepth: number; savedHandlersDepth: number }
  | { tag: "KPrompt"; promptTag: Val; handler: Val; env: Env; savedKont: Frame[]; savedHandlersDepth: number }
  | { tag: "KMatch"; clauses: Array<{ pat: Pattern; body: Expr }>; env: Env }
  | { tag: "KOracleLambda"; params: string[]; env: Env }  // oracle-lambda: waiting for spec to evaluate
  | { tag: "KBind"; fn: Val; env: Env }
  | { tag: "KHandlerBind"; handlers: ConditionHandler[] }
  | { tag: "KRestartBind"; restarts: RestartBinding[]; savedKont: Frame[]; env: Env; store: Store; handlers: HandlerFrame[] }
  | { tag: "KSignaling"; condition: ConditionVal; required: boolean };

export type HandlerFrame = {
  hid: string;
  env: Env;
  on: Map<string, { op: string; params: string[]; k: string; body: Expr }>;
  ret?: { v: string; body: Expr };
  fin?: { body: Expr };
};

export type State = {
  control: Control;
  /** Legacy alias used in a few tests */
  ctrl?: Control;
  env: Env;
  store: Store;
  kont: Frame[];           // bottom->top, push/pop at end
  handlers: HandlerFrame[]; // bottom->top, push/pop at end

  // ─────────────────────────────────────────────────────────────────
  // Prompt 9: Runtime Governance
  // ─────────────────────────────────────────────────────────────────

  /** Active profile - determines what effects/ops/requests are allowed */
  profile?: Profile;

  /** Mutable budget counters - decremented on each operation */
  budget?: RuntimeBudget;

  /** Current security context - intersection of profile caps and context caps */
  sec?: RuntimeSecurity;

  // Provenance graph + receipt store (optional)
  provenanceGraph?: ProvenanceGraph;
  provenanceStore?: ProvenanceStore;
  provenanceSourceChecker?: SourceChecker;
};

export type StepOutcome =
  | { tag: "State"; state: State; value?: undefined; opcall?: undefined }
  | { tag: "Done"; value: Val; state: State; opcall?: undefined }
  | { tag: "Op"; opcall: OpCall; state: State; value?: Val };
```

### FILE: src/core/eval/machineStep.ts
```typescript
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-4.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.
// Prompt 9: Integrated governance enforcement at chokepoints

import type { Expr, HandlerExpr, Pattern } from "../ast";
import type { State, StepOutcome, Frame, HandlerFrame } from "./machine";
import type { Env } from "./env";
import { envGet, envSet } from "./env";
import type { Val } from "./values";
import { VUnit, VFalse, VTrue } from "./values";
import { captureValueResumption } from "../effects/capture";
import {
  checkEffectAllowed,
  debitEffect,
  checkStepBudget,
  debitStep,
} from "../governance/enforcement";
import { UnhandledConditionError } from "../conditions/prims";

// Compatibility export for tests that expected StepResult alias
export type StepResult = StepOutcome;

function uuid(): string {
  return Math.random().toString(16).slice(2) + "-" + Date.now().toString(16);
}

function ctxDigest(env: Env): string {
  // Ctx has a content-addressed cid - use it directly
  return env.cid;
}

function push(kont: Frame[], fr: Frame): Frame[] {
  const k2 = kont.slice();
  k2.push(fr);
  return k2;
}

function pop(kont: Frame[]): [Frame | undefined, Frame[]] {
  if (kont.length === 0) return [undefined, kont];
  const k2 = kont.slice();
  const fr = k2.pop();
  return [fr, k2];
}

function isBool(v: Val): v is { tag: "Bool"; b: boolean } {
  return v.tag === "Bool";
}

function findHandlerIndexByHid(handlers: HandlerFrame[], hid: string): number {
  for (let i = handlers.length - 1; i >= 0; i--) {
    if (handlers[i].hid === hid) return i;
  }
  return -1;
}

/** Convert quoted datum to runtime value (minimal). Extend as needed.
 * Arrays are converted to proper cons-cell lists (Vector with 2 items each)
 * for compatibility with the list primitives (car, cdr, length, etc.)
 */
function datumToVal(d: unknown): Val {
  if (d === null) return { tag: "Unit" };
  if (typeof d === "number") return { tag: "Num", n: d };
  if (typeof d === "string") return { tag: "Str", s: d };
  if (typeof d === "boolean") return { tag: "Bool", b: d };
  if (Array.isArray(d)) {
    // Build proper cons-cell list: (a b c) -> [a, [b, [c, Unit]]]
    let result: Val = { tag: "Unit" };
    for (let i = d.length - 1; i >= 0; i--) {
      result = { tag: "Vector", items: [datumToVal(d[i]), result] };
    }
    return result;
  }
  // symbols in datum: represent as {tag:"Sym"} if encoded as {sym:"x"}
  if (typeof d === "object" && d !== null && "sym" in d) {
    return { tag: "Sym", name: (d as { sym: string }).sym };
  }
  return { tag: "Str", s: JSON.stringify(d) };
}

function buildHandlerFrame(handler: HandlerExpr, env: Env): HandlerFrame {
  const hid = uuid();
  const on = new Map<string, { op: string; params: string[]; k: string; body: Expr }>();
  for (const c of handler.on) on.set(c.op, c);
  return { hid, env, on, ret: handler.ret, fin: handler.fin };
}

/** Pattern matching helper for Match expressions */
function matchPatternValue(p: Pattern, v: Val, env = new Map<string, Val>()): Map<string, Val> | null {
  switch (p.tag) {
    case "PWild":
      return env;

    case "PVar": {
      const prev = env.get(p.name);
      if (!prev) {
        const e2 = new Map(env);
        e2.set(p.name, v);
        return e2;
      }
      // Same var bound twice: check equality
      return valEq(prev, v) ? env : null;
    }

    case "PLit": {
      if (p.value === null) return v.tag === "Unit" ? env : null;
      if (typeof p.value === "number") return v.tag === "Num" && v.n === p.value ? env : null;
      if (typeof p.value === "boolean") return v.tag === "Bool" && v.b === p.value ? env : null;
      if (typeof p.value === "string") {
        // String pattern can match either Str or Sym
        if (v.tag === "Str" && v.s === p.value) return env;
        if (v.tag === "Sym" && v.name === p.value) return env;
        return null;
      }
      return null;
    }

    case "PVector": {
      if (v.tag !== "Vector") return null;

      // Direct vector match (flat vectors)
      if (v.items.length === p.items.length) {
        let ecur: Map<string, Val> | null = env;
        for (let i = 0; i < p.items.length; i++) {
          ecur = matchPatternValue(p.items[i], v.items[i], ecur!);
          if (!ecur) return null;
        }
        return ecur;
      }

      // Try cons-cell list match: Vector[a, Vector[b, Vector[c, Unit]]] matches pattern (a b c)
      // A cons-cell is a 2-element vector where items[1] is another cons or Unit
      if (p.items.length > 0 && v.items.length === 2) {
        // Convert cons-cell list to flat array for matching
        const flatList: Val[] = [];
        let cur: Val = v;
        while (cur.tag === "Vector" && cur.items.length === 2) {
          flatList.push(cur.items[0]);
          cur = cur.items[1];
        }
        // Check if terminated by Unit (proper list)
        if (cur.tag !== "Unit") return null;

        // Now match pattern items against flat list
        if (flatList.length !== p.items.length) return null;
        let ecur: Map<string, Val> | null = env;
        for (let i = 0; i < p.items.length; i++) {
          ecur = matchPatternValue(p.items[i], flatList[i], ecur!);
          if (!ecur) return null;
        }
        return ecur;
      }

      return null;
    }
  }
}

function valEq(a: Val, b: Val): boolean {
  if (a.tag !== b.tag) return false;
  switch (a.tag) {
    case "Unit": return true;
    case "Num": return (b as any).n === a.n;
    case "Bool": return (b as any).b === a.b;
    case "Str": return (b as any).s === a.s;
    case "Sym": return (b as any).name === a.name;
    case "Vector": {
      const bb = b as any;
      if (bb.items.length !== a.items.length) return false;
      for (let i = 0; i < a.items.length; i++) if (!valEq(a.items[i], bb.items[i])) return false;
      return true;
    }
    default:
      return JSON.stringify(a) === JSON.stringify(b);
  }
}

/**
 * Apply a value as a function (closure/native/cont/oracle-proc).
 * Returns StepOutcome: either a state transition or an effect operation.
 */
function applyVal(fnVal: Val, args: Val[], st: State): StepOutcome {
  // Cont: delimited resumption call
  if (fnVal.tag === "Cont") {
    if (args.length !== 1) throw new Error(`Cont apply arity mismatch: got ${args.length}`);
    const callerKont = st.kont;               // continuation expecting result of the (k arg) call
    const callerHandlersDepth = st.handlers.length;

    const baseResumed = fnVal.resumption.invoke(args[0]); // resumes at op site with effect result = args[0]

    // Patch the handle boundary in resumed kont to "resume mode" returning to callerKont.
    const k = baseResumed.kont.slice();
    const idx = fnVal.boundaryIndex;
    const fr = k[idx];
    if (!fr || fr.tag !== "KHandleBoundary" || fr.hid !== fnVal.hid) {
      throw new Error("Cont invoke: boundary frame mismatch");
    }

    k[idx] = {
      ...fr,
      resumeTo: { kont: callerKont, handlersDepth: callerHandlersDepth },
    };

    return {
      tag: "State",
      state: {
        ...baseResumed,
        kont: k,
        // handlers remain as captured at op site; boundary frame will truncate on resume-return
      },
    };
  }

  // First-class continuation (call/cc, prompts)
  if (fnVal.tag === "Continuation") {
    if (args.length !== 1) {
      throw new Error(`Continuation apply arity mismatch: expected 1, got ${args.length}`);
    }
    return {
      tag: "State",
      state: {
        ...st,
        control: { tag: "Val", v: args[0] },
        env: fnVal.env,
        // Use the current store to preserve mutations performed before invoking the continuation.
        store: st.store,
        kont: fnVal.kont.slice(),
        handlers: fnVal.handlers.slice(),
      },
    };
  }

  // Native function: host-implemented (primitives, etc.)
  if (fnVal.tag === "Native") {
    const result = fnVal.fn(args, st);
    if ((result as any).tag === "State" || (result as any).tag === "Op" || (result as any).tag === "Done") {
      return result as StepOutcome;
    }
    return { tag: "State", state: result as State };
  }

  // Closure
  if (fnVal.tag === "Closure") {
    if (args.length !== fnVal.params.length) {
      throw new Error(`Closure apply arity mismatch: expected ${fnVal.params.length}, got ${args.length}`);
    }
    // allocate params in store
    let store = st.store;
    let env = fnVal.env;
    for (let i = 0; i < fnVal.params.length; i++) {
      const [store2, addr] = store.alloc(args[i]);
      store = store2;
      env = envSet(env, fnVal.params[i], addr);
    }
    // push call-return frame to restore env after body
    const kont = push(st.kont, { tag: "KCall", savedEnv: st.env });
    return {
      tag: "State",
      state: {
        ...st,
        control: { tag: "Expr", e: fnVal.body },
        env,
        store,
        kont,
      },
    };
  }

  // OracleProc: LLM in apply position - emit oracle.apply.op
  if (fnVal.tag === "OracleProc") {
    const suspended: State = { ...st, control: { tag: "Val", v: VUnit } };
    const resumption = captureValueResumption(suspended);
    const opcall = {
      op: "oracle.apply.op",
      args: [fnVal, { tag: "Vector", items: args } as Val],
      ctxDigest: ctxDigest(st.env),
      resumption,
    };
    return { tag: "Op", opcall, state: suspended };
  }

  throw new Error(`Attempted to apply non-callable value: ${fnVal.tag}`);
}

function applyFrame(fr: Frame, v: Val, st: State): StepOutcome {
  switch (fr.tag) {
    case "KIf": {
      if (!isBool(v)) throw new Error("if test must be boolean");
      return {
        tag: "State",
        state: {
          ...st,
          control: { tag: "Expr", e: v.b ? fr.conseq : fr.alt },
          env: fr.env,
        },
      };
    }

    case "KBegin": {
      if (fr.rest.length === 0) {
        return { tag: "State", state: { ...st, control: { tag: "Val", v } } };
      }
      const [e0, ...rest] = fr.rest;
      // IMPORTANT: Use st.env (not fr.env) to preserve bindings from define
      return {
        tag: "State",
        state: {
          ...st,
          control: { tag: "Expr", e: e0 },
          env: st.env,
          kont: push(st.kont, { tag: "KBegin", rest, env: st.env }),
        },
      };
    }

    case "KDefine": {
      // define returns Unit
      const addr0 = envGet(fr.env, fr.name);
      if (addr0 !== undefined) {
        const store2 = st.store.write(addr0, v);
        return {
          tag: "State",
          state: { ...st, control: { tag: "Val", v: VUnit }, env: fr.env, store: store2 },
        };
      } else {
        const [store2, addr] = st.store.alloc(v);
        const env2 = envSet(fr.env, fr.name, addr);
        return {
          tag: "State",
          state: { ...st, control: { tag: "Val", v: VUnit }, env: env2, store: store2 },
        };
      }
    }

    case "KSet": {
      // Enforce seal: set! denied if context is sealed
      if (fr.env.sealed) {
        throw new Error(`set! denied: context is sealed (name=${fr.name})`);
      }
      const addr = envGet(fr.env, fr.name);
      if (addr === undefined) throw new Error(`set!: unbound var ${fr.name}`);
      const store2 = st.store.write(addr, v);
      return {
        tag: "State",
        state: { ...st, control: { tag: "Val", v: VUnit }, env: fr.env, store: store2 },
      };
    }

    case "KAppFun": {
      // Native with lazy argument indices: wrap specified args as zero-arg thunks (closures)
      if (v.tag === "Native" && Array.isArray((v as any).lazyArgs) && (v as any).lazyArgs.length > 0) {
        const lazyIdx: number[] = (v as any).lazyArgs;
        const totalArgs = fr.args.length;
        const acc: Array<{ idx: number; val: Val }> = [];
        const pending: Array<{ expr: Expr; idx: number }> = [];

        fr.args.forEach((arg, idx) => {
          if (lazyIdx.includes(idx)) {
            const thunk: Val = { tag: "Closure", params: [], body: arg, env: fr.env };
            acc.push({ idx, val: thunk });
          } else {
            pending.push({ expr: arg, idx });
          }
        });

        if (pending.length === 0) {
          const ordered: Val[] = new Array(totalArgs);
          for (const { idx, val } of acc) ordered[idx] = val;
          return applyVal(v, ordered, { ...st, env: fr.env });
        }

        const [next, ...rest] = pending;
        const kontFrame: Frame = {
          tag: "KAppArgLazy",
          fnVal: v,
          pending: rest,
          acc,
          env: fr.env,
          totalArgs,
          currentIdx: next.idx,
        };
        return {
          tag: "State",
          state: {
            ...st,
            control: { tag: "Expr", e: next.expr },
            env: fr.env,
            kont: push(st.kont, kontFrame),
          },
        };
      }

      if (fr.args.length === 0) {
        // apply immediately - applyVal returns StepOutcome directly
        return applyVal(v, [], { ...st, env: fr.env });
      }
      const [a0, ...rest] = fr.args;
      return {
        tag: "State",
        state: {
          ...st,
          control: { tag: "Expr", e: a0 },
          env: fr.env,
          kont: push(st.kont, { tag: "KAppArg", fnVal: v, pending: rest, acc: [], env: fr.env }),
        },
      };
    }

    case "KAppArg": {
      const acc2 = fr.acc.concat([v]);
      if (fr.pending.length === 0) {
        // apply fnVal to accumulated args - applyVal returns StepOutcome directly
        return applyVal(fr.fnVal, acc2, { ...st, env: fr.env });
      }
      const [a0, ...rest] = fr.pending;
      return {
        tag: "State",
        state: {
          ...st,
          control: { tag: "Expr", e: a0 },
          env: fr.env,
          kont: push(st.kont, { tag: "KAppArg", fnVal: fr.fnVal, pending: rest, acc: acc2, env: fr.env }),
        },
      };
    }

    case "KAppArgLazy": {
      const acc2 = fr.acc.concat([{ idx: fr.currentIdx, val: v }]);
      if (fr.pending.length === 0) {
        const ordered: Val[] = new Array(fr.totalArgs);
        for (const { idx, val } of acc2) ordered[idx] = val;
        return applyVal(fr.fnVal, ordered, { ...st, env: fr.env });
      }
      const [next, ...rest] = fr.pending;
      const kontFrame: Frame = {
        ...fr,
        pending: rest,
        acc: acc2,
        currentIdx: next.idx,
      };
      return {
        tag: "State",
        state: {
          ...st,
          control: { tag: "Expr", e: next.expr },
          env: fr.env,
          kont: push(st.kont, kontFrame),
        },
      };
    }

    case "KCall": {
      // restore environment after closure body returns
      return { tag: "State", state: { ...st, control: { tag: "Val", v }, env: fr.savedEnv } };
    }

    case "KEffect": {
      const acc2 = fr.acc.concat([v]);
      if (fr.pending.length === 0) {
        // Prompt 9 Chokepoint A: Debit the effect when emitting
        const stDebited = debitEffect(st, fr.op);
        // perform operation now: emit OpCall with a multi-shot resumption
        const suspended: State = { ...stDebited, control: { tag: "Val", v: VUnit }, env: fr.env }; // placeholder
        const resumption = captureValueResumption(suspended);
        const opcall = { op: fr.op, args: acc2, ctxDigest: ctxDigest(fr.env), resumption };
        return { tag: "Op", opcall, state: suspended };
      }
      const [e0, ...rest] = fr.pending;
      return {
        tag: "State",
        state: {
          ...st,
          control: { tag: "Expr", e: e0 },
          env: fr.env,
          kont: push(st.kont, { tag: "KEffect", op: fr.op, pending: rest, acc: acc2, env: fr.env }),
        },
      };
    }

    case "KHandleBoundary": {
      // Body (or clause) has returned a value v to the handler boundary.
      const hIdx = findHandlerIndexByHid(st.handlers, fr.hid);
      if (hIdx < 0) throw new Error("KHandleBoundary: handler not found");
      const hf = st.handlers[hIdx];

      // Determine whether we are exiting handle or resuming to a caller continuation.
      const mode: "exit" | "resume" = fr.resumeTo ? "resume" : "exit";
      const targetKont = mode === "resume" ? fr.resumeTo!.kont : st.kont; // st.kont already has outer continuation after pop
      const targetHandlersDepth = mode === "resume" ? fr.resumeTo!.handlersDepth : fr.savedHandlersDepth;

      // Apply return clause (if any) by evaluating it under handler env with v bound.
      if (hf.ret) {
        const param = hf.ret.v;
        const body = hf.ret.body;

        // allocate param in store
        let store = st.store;
        const [store2, addr] = store.alloc(v);
        store = store2;
        const env2 = envSet(hf.env, param, addr);

        // After ret body computes v2, KHandleReturn decides whether to exit or resume.
        return {
          tag: "State",
          state: {
            ...st,
            control: { tag: "Expr", e: body },
            env: env2,
            store,
            kont: push(targetKont, {
              tag: "KHandleReturn",
              mode,
              hid: fr.hid,
              targetKont,
              targetHandlersDepth,
              savedHandlersDepth: fr.savedHandlersDepth,
            }),
            // handlers remain as-is during return-clause evaluation; KHandleReturn will truncate.
          },
        };
      }

      // No return clause: direct value
      const st2: State = {
        ...st,
        control: { tag: "Val", v },
        kont: targetKont,
        handlers: st.handlers.slice(0, targetHandlersDepth),
      };
      return { tag: "State", state: st2 };
    }

    case "KHandleReturn": {
      // v is result of return clause evaluation
      const st2: State = {
        ...st,
        control: { tag: "Val", v },
        kont: fr.targetKont,
        handlers: st.handlers.slice(0, fr.targetHandlersDepth),
      };
      return { tag: "State", state: st2 };
    }

    case "KMatch": {
      // v is the scrutinee; try each clause
      for (const cl of fr.clauses) {
        const binds = matchPatternValue(cl.pat, v);
        if (!binds) continue;

        // Bind matched values in env
        let store = st.store;
        let env2 = fr.env;
        for (const [name, val] of binds.entries()) {
          const [s2, addr] = store.alloc(val);
          store = s2;
          env2 = envSet(env2, name, addr);
        }

        return {
          tag: "State",
          state: { ...st, control: { tag: "Expr", e: cl.body }, env: env2, store },
        };
      }
      throw new Error("match: no clause matched");
    }

    case "KOracleLambda": {
      // v is the evaluated spec; create an OracleProc capturing lexical env
      const oracleProc: Val = {
        tag: "OracleProc",
        params: fr.params,
        spec: v,
        env: fr.env,
      };
      return { tag: "State", state: { ...st, control: { tag: "Val", v: oracleProc } } };
    }

    case "KPrompt": {
      const handlersTrunc = st.handlers.slice(0, fr.savedHandlersDepth);
      return {
        tag: "State",
        state: {
          ...st,
          control: { tag: "Val", v },
          env: fr.env,
          kont: fr.savedKont,
          handlers: handlersTrunc,
        },
      };
    }

    case "KHandlerBind": {
      return { tag: "State", state: { ...st, control: { tag: "Val", v } } };
    }

    case "KRestartBind": {
      return { tag: "State", state: { ...st, control: { tag: "Val", v } } };
    }

    case "KSignaling": {
      if (fr.required) {
        throw new UnhandledConditionError(fr.condition);
      }
      return { tag: "State", state: { ...st, control: { tag: "Val", v } } };
    }

    case "KBind": {
      return applyVal(fr.fn, [v], { ...st, env: fr.env });
    }

    default: {
      const _exh: never = fr;
      return _exh;
    }
  }
}

export function stepOnce(st: State): StepOutcome {
  // Prompt 9: Check step budget before each step
  checkStepBudget(st);

  // Expr step
  if (st.control.tag === "Expr") {
    const e = st.control.e;

    switch (e.tag) {
      case "Lit": {
        const v: Val =
          e.value === null ? VUnit :
          typeof e.value === "number" ? { tag: "Num", n: e.value } :
          typeof e.value === "boolean" ? { tag: "Bool", b: e.value } :
          { tag: "Str", s: String(e.value) };
        return { tag: "State", state: { ...st, control: { tag: "Val", v } } };
      }

      case "Var": {
        const addr = envGet(st.env, e.name);
        if (addr === undefined) throw new Error(`unbound var ${e.name}`);
        const v = st.store.read(addr);
        return { tag: "State", state: { ...st, control: { tag: "Val", v } } };
      }

      case "Lambda": {
        const v: Val = { tag: "Closure", params: e.params, body: e.body, env: st.env };
        return { tag: "State", state: { ...st, control: { tag: "Val", v } } };
      }

      case "OracleLambda": {
        // oracle-lambda creates an OracleProc that captures the lexical environment
        // The spec is evaluated to a value that the oracle will use as its goal/prompt
        const kont = push(st.kont, { tag: "KOracleLambda", params: e.params, env: st.env });
        return { tag: "State", state: { ...st, control: { tag: "Expr", e: e.spec }, kont } };
      }

      case "If": {
        const kont = push(st.kont, { tag: "KIf", conseq: e.conseq, alt: e.alt, env: st.env });
        return { tag: "State", state: { ...st, control: { tag: "Expr", e: e.test }, kont } };
      }

      case "Begin": {
        if (e.exprs.length === 0) return { tag: "State", state: { ...st, control: { tag: "Val", v: VUnit } } };
        const [e0, ...rest] = e.exprs;
        const kont = push(st.kont, { tag: "KBegin", rest, env: st.env });
        return { tag: "State", state: { ...st, control: { tag: "Expr", e: e0 }, kont } };
      }

      case "Define": {
        // Pre-allocate binding with placeholder to support recursive definitions
        // This ensures lambda bodies capture an env that includes the binding
        const existingAddr = envGet(st.env, e.name);
        if (existingAddr !== undefined) {
          // Already allocated (re-definition) - just evaluate RHS and overwrite
          const kont = push(st.kont, { tag: "KDefine", name: e.name, env: st.env });
          return { tag: "State", state: { ...st, control: { tag: "Expr", e: e.rhs }, kont } };
        }
        // Allocate placeholder, add to env, then evaluate RHS
        const placeholder: Val = { tag: "Unit" }; // will be overwritten
        const [store2, addr] = st.store.alloc(placeholder);
        const env2 = envSet(st.env, e.name, addr);
        const kont = push(st.kont, { tag: "KDefine", name: e.name, env: env2 });
        return { tag: "State", state: { ...st, control: { tag: "Expr", e: e.rhs }, env: env2, store: store2, kont } };
      }

      case "Set": {
        const kont = push(st.kont, { tag: "KSet", name: e.name, env: st.env });
        return { tag: "State", state: { ...st, control: { tag: "Expr", e: e.rhs }, kont } };
      }

      case "App": {
        const kont = push(st.kont, { tag: "KAppFun", args: e.args, env: st.env });
        return { tag: "State", state: { ...st, control: { tag: "Expr", e: e.fn }, kont } };
      }

      case "Quote": {
        const v = datumToVal(e.datum);
        return { tag: "State", state: { ...st, control: { tag: "Val", v } } };
      }

      case "Effect": {
        // Prompt 9 Chokepoint A: Check effect is allowed before emission
        checkEffectAllowed(st, e.op);

        if (e.args.length === 0) {
          // Debit the effect before emitting
          const stDebited = debitEffect(st, e.op);
          const suspended: State = { ...stDebited, control: { tag: "Val", v: VUnit } };
          const resumption = captureValueResumption(suspended);
          const opcall = { op: e.op, args: [], ctxDigest: ctxDigest(st.env), resumption };
          return { tag: "Op", opcall, state: suspended };
        }
        const [a0, ...rest] = e.args;
        const kont = push(st.kont, { tag: "KEffect", op: e.op, pending: rest, acc: [], env: st.env });
        return { tag: "State", state: { ...st, control: { tag: "Expr", e: a0 }, kont } };
      }

      case "Handle": {
        const hf = buildHandlerFrame(e.handler, st.env);
        const savedHandlersDepth = st.handlers.length;
        const handlers2 = st.handlers.concat([hf]);
        const boundary: Frame = { tag: "KHandleBoundary", hid: hf.hid, savedHandlersDepth };
        const kont2 = push(st.kont, boundary);
        return { tag: "State", state: { ...st, control: { tag: "Expr", e: e.body }, handlers: handlers2, kont: kont2 } };
      }

      case "Match": {
        // Evaluate the scrutinee first, then apply pattern matching in KMatch frame
        const kont = push(st.kont, { tag: "KMatch", clauses: e.clauses, env: st.env });
        return { tag: "State", state: { ...st, control: { tag: "Expr", e: e.scrutinee }, kont } };
      }

      case "QuoteSyntax": {
        const v: Val = { tag: "Str", s: JSON.stringify(e.datum) };
        return { tag: "State", state: { ...st, control: { tag: "Val", v } } };
      }

      case "Let":
      case "Letrec": {
        // Minimal handling: evaluate the body assuming bindings already processed elsewhere.
        return { tag: "State", state: { ...st, control: { tag: "Expr", e: (e as any).body } } };
      }

      default: {
        return { tag: "State", state: st };
      }
    }
  }

  // Val step: apply continuation frame or finish
  const v = st.control.v;
  const [fr, kont2] = pop(st.kont);
  if (!fr) {
    return { tag: "Done", value: v, state: st };
  }

  const st2: State = { ...st, kont: kont2 };
  return applyFrame(fr, v, st2);
}
```

### FILE: src/core/eval/run.ts
```typescript
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-4.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import type { State } from "./machine";
import type { Runtime } from "./runtime";
import type { Val } from "./values";
import { stepOnce } from "./machineStep";
import type { BudgetTracker } from "../governance/budgets";

export type RunOptions = {
  maxSteps?: number;
  budget?: BudgetTracker;
};

export async function runToCompletion(
  runtime: Runtime,
  initial: State,
  optionsOrMaxSteps: RunOptions | number = {}
): Promise<Val> {
  // Backward compatibility: accept number as maxSteps
  const options: RunOptions = typeof optionsOrMaxSteps === "number"
    ? { maxSteps: optionsOrMaxSteps }
    : optionsOrMaxSteps;

  const maxSteps = options.maxSteps ?? 1_000_000;
  const budget = options.budget;

  let st = initial;
  for (let i = 0; i < maxSteps; i++) {
    // Consume one eval step if budget tracking is enabled
    budget?.consumeEvalStep();

    const out = stepOnce(st);

    if (out.tag === "State") {
      st = out.state;
      continue;
    }
    if (out.tag === "Done") {
      return out.value;
    }
    if (out.tag === "Op") {
      const handled = await runtime.dispatch(out.state, out.opcall);
      if (handled === "Uncaught") {
        throw new Error(`Uncaught op: ${out.opcall.op}`);
      }
      st = handled;
      continue;
    }
  }
  throw new Error("runToCompletion: step budget exceeded");
}

/**
 * Like runToCompletion but returns both value AND final state.
 * Required by PortalImpl for oracle REPL re-entry.
 */
export async function runToCompletionWithState(
  runtime: Runtime,
  st0: State,
  optionsOrMaxSteps: RunOptions | number = {}
): Promise<{ value: Val; state: State }> {
  // Backward compatibility: accept number as maxSteps
  const options: RunOptions = typeof optionsOrMaxSteps === "number"
    ? { maxSteps: optionsOrMaxSteps }
    : optionsOrMaxSteps;

  const maxSteps = options.maxSteps ?? 1_000_000;
  const budget = options.budget;

  let st = st0;

  for (let i = 0; i < maxSteps; i++) {
    // Consume one eval step if budget tracking is enabled
    budget?.consumeEvalStep();

    const out = stepOnce(st);

    if (out.tag === "State") {
      st = out.state;
      continue;
    }

    if (out.tag === "Done") {
      return { value: out.value, state: (out.state ?? st) as any };
    }

    if (out.tag === "Op") {
      const handled = await runtime.dispatch(out.state, out.opcall);
      if (handled === "Uncaught") throw new Error(`Uncaught op: ${out.opcall.op}`);
      st = handled;
      continue;
    }

    throw new Error(`unknown stepOnce output: ${(out as any).tag}`);
  }

  throw new Error(`runToCompletionWithState exceeded maxSteps=${maxSteps}`);
}

// ============================================================================
// TRACING INFRASTRUCTURE
// ============================================================================

export type TraceEntry = {
  step: number;
  controlTag: string;
  controlDetail: string;
  stackDepth: number;
  stackTags: string[];
  outcome: "state" | "done" | "op";
  opName?: string;
};

export type TraceOptions = RunOptions & {
  trace?: boolean;
  onStep?: (entry: TraceEntry) => void;
};

function controlToString(control: State["control"]): { tag: string; detail: string } {
  if (control.tag === "Val") {
    const v = control.v;
    if (v.tag === "Num") return { tag: "Val", detail: `Num(${v.n})` };
    if (v.tag === "Str") return { tag: "Val", detail: `Str("${v.s.slice(0, 20)}${v.s.length > 20 ? "..." : ""}")` };
    if (v.tag === "Bool") return { tag: "Val", detail: `Bool(${v.b})` };
    if (v.tag === "Unit") return { tag: "Val", detail: "Unit" };
    if (v.tag === "Closure") return { tag: "Val", detail: `Closure(${(v as any).params?.join(",") || "?"})` };
    if (v.tag === "Native") return { tag: "Val", detail: "Native" };
    return { tag: "Val", detail: v.tag };
  }
  if (control.tag === "Expr") {
    const e = control.e;
    if (e.tag === "Lit") {
      if (typeof e.value === "number") return { tag: "Expr", detail: `Lit(${e.value})` };
      if (typeof e.value === "string") return { tag: "Expr", detail: `Lit("${e.value.slice(0, 15)}")` };
      return { tag: "Expr", detail: `Lit(${e.value})` };
    }
    if (e.tag === "Var") return { tag: "Expr", detail: `Var(${e.name})` };
    if (e.tag === "App") return { tag: "Expr", detail: `App(${(e as any).fn?.tag || "?"})` };
    if (e.tag === "If") return { tag: "Expr", detail: "If" };
    if (e.tag === "Lambda") return { tag: "Expr", detail: `Lambda(${(e as any).params?.length || 0} params)` };
    if (e.tag === "Begin") return { tag: "Expr", detail: `Begin(${(e as any).exprs?.length || 0} exprs)` };
    if (e.tag === "Define") return { tag: "Expr", detail: `Define(${(e as any).name || "?"})` };
    if (e.tag === "Effect") return { tag: "Expr", detail: `Effect(${(e as any).op || "?"})` };
    return { tag: "Expr", detail: e.tag };
  }
  return { tag: (control as any).tag ?? "?", detail: "?" };
}

/**
 * Run to completion with full execution tracing.
 * Returns value, final state, and trace log.
 */
export async function runWithTrace(
  runtime: Runtime,
  initial: State,
  optionsOrMaxSteps: TraceOptions | number = {}
): Promise<{ value: Val; state: State; trace: TraceEntry[] }> {
  const options: TraceOptions = typeof optionsOrMaxSteps === "number"
    ? { maxSteps: optionsOrMaxSteps, trace: true }
    : { trace: true, ...optionsOrMaxSteps };

  const maxSteps = options.maxSteps ?? 1_000_000;
  const budget = options.budget;
  const trace: TraceEntry[] = [];

  let st = initial;

  for (let i = 0; i < maxSteps; i++) {
    budget?.consumeEvalStep();

    const { tag, detail } = controlToString(st.control);
    const stackTags = st.kont.map(fr => (fr as any).tag || "?");

    const out = stepOnce(st);

    const entry: TraceEntry = {
      step: i,
      controlTag: tag,
      controlDetail: detail,
      stackDepth: st.kont.length,
      stackTags,
      outcome: out.tag === "State" ? "state" : out.tag === "Done" ? "done" : "op",
      opName: out.tag === "Op" ? out.opcall.op : undefined,
    };

    trace.push(entry);
    options.onStep?.(entry);

    if (out.tag === "State") {
      st = out.state;
      continue;
    }

    if (out.tag === "Done") {
      return { value: out.value, state: (out.state ?? st) as any, trace };
    }

    if (out.tag === "Op") {
      const handled = await runtime.dispatch(out.state, out.opcall);
      if (handled === "Uncaught") throw new Error(`Uncaught op: ${out.opcall.op}`);
      st = handled;
      continue;
    }

    throw new Error(`unknown stepOnce output: ${(out as any).tag}`);
  }

  throw new Error(`runWithTrace exceeded maxSteps=${maxSteps}`);
}

/**
 * Format trace for human-readable output
 */
export function formatTrace(trace: TraceEntry[], options?: { compact?: boolean }): string {
  if (options?.compact) {
    return trace.map(e =>
      `[${e.step}] ${e.controlTag}:${e.controlDetail} | stack=${e.stackDepth}${e.opName ? ` | OP=${e.opName}` : ""}`
    ).join("\n");
  }

  const lines: string[] = [];
  for (const e of trace) {
    lines.push(`Step ${e.step}:`);
    lines.push(`  Control: ${e.controlTag} - ${e.controlDetail}`);
    lines.push(`  Stack depth: ${e.stackDepth}`);
    if (e.stackTags.length > 0) {
      lines.push(`  Stack frames: [${e.stackTags.join(" <- ")}]`);
    }
    if (e.opName) {
      lines.push(`  >>> EFFECT: ${e.opName}`);
    }
    lines.push("");
  }
  return lines.join("\n");
}
```

## 2. ORACLE SYSTEM (LLM Integration)


### FILE: src/core/oracle/protocol.ts
```typescript
// src/core/oracle/protocol.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Patch Set C: Extended Oracle protocol request algebra

import type { Expr } from "../ast";
import type { Val } from "../eval/values";
import type { Hash } from "../artifacts/hash";
import type { MeaningVal } from "./meaning";

export type EnvRef = Hash;
export type StateRef = Hash;

// Re-export MeaningVal as the protocol's Meaning type
export type Meaning = MeaningVal;

export type QExpr = string | { tag: "Text"; text: string } | Expr;

export type ObserveSpec =
  | { tag: "Stack"; limit?: number }
  | { tag: "Control" }
  | { tag: "Handlers" }
  | { tag: "FrameEnv"; frameIndex: number }
  | { tag: "StoreSummary"; maxCells?: number }
  | { tag: "Env"; envRef?: EnvRef }                     // List all bindings in environment
  | { tag: "EnvLookup"; name: string; envRef?: EnvRef } // Lookup specific binding by name
  | { tag: "Defs"; envRef?: EnvRef };                   // List top-level definitions

export type ToolCall = {
  name: string;
  argv: string[];
  cwd?: string;
  stdin?: string;
  timeoutMs?: number;
};

export type TestSpec =
  | { tag: "ExprEquals"; qexpr: Expr; expected: Val; envRef: EnvRef }
  | { tag: "Smoke"; qexpr: Expr; envRef: EnvRef }
  | { tag: "Tests"; tests: QExpr[]; envRef: EnvRef }  // Batch tests: each expr should return truthy
  | { tag: "TestSuite"; name: string; tests: Array<{ name: string; qexpr: QExpr }>; envRef: EnvRef };  // Named suite

export type TrainingExample = {
  tag: "Example";
  payload: unknown;
};

export type OracleReq =
  // Core REPL re-entry
  | { tag: "ReqEval"; qexpr: QExpr; envRef: EnvRef }
  | { tag: "ReqApply"; fn: Val; args: Val[]; envRef: EnvRef }
  | { tag: "ReqObserve"; what: ObserveSpec; stateRef: StateRef }

  // Patch Set C: Extended request algebra
  | { tag: "ReqMatch"; qexpr: QExpr; pattern: QExpr; envRef: EnvRef }
  | { tag: "ReqAssert"; predicate: QExpr | Val; msg: string; severity?: "warn" | "error"; envRef: EnvRef }
  | { tag: "ReqSnapshot"; envRef: EnvRef; stateRef?: StateRef; meta?: unknown }
  | { tag: "ReqCompress"; envRef: EnvRef; meta?: unknown }
  | { tag: "ReqHydrate"; receiptId: Hash }

  // Tools and testing
  | { tag: "ReqTool"; call: ToolCall; envRef?: EnvRef }
  | { tag: "ReqTest"; spec: TestSpec }
  | { tag: "ReqEmitExample"; ex: TrainingExample }

  // Adoption and termination
  | { tag: "ReqAdoptEnv"; envRef: EnvRef }
  | { tag: "ReqReturn"; result: Meaning }
  | { tag: "ReqFail"; reason: string };

export type OracleResp =
  | { tag: "RespVal"; value: Val; envRef?: EnvRef; stateRef?: StateRef }
  | { tag: "RespObs"; data: unknown }
  | { tag: "RespTool"; result: unknown }
  | { tag: "RespTest"; passed: boolean; report: unknown }
  | { tag: "RespAck" }
  | { tag: "RespError"; message: string; details?: unknown };

export type OracleReturn = Meaning;

/**
 * Interactive oracle session:
 *   - yields OracleReq
 *   - receives OracleResp
 *   - returns OracleReturn (Meaning)
 */
export type OracleSession = AsyncGenerator<OracleReq, OracleReturn, OracleResp>;
```

### FILE: src/core/oracle/adapter.ts
```typescript
// src/core/oracle/adapter.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-14.md

import type { Val } from "../eval/values";
import type { EnvRef, StateRef, OracleSession } from "./protocol";

export type OracleInit =
  | { tag: "Infer"; payload: Val; envRef: EnvRef; stateRef: StateRef; policyDigest?: string }
  | { tag: "Apply"; proc: Val; args: Val[]; envRef: EnvRef; stateRef: StateRef; policyDigest?: string };

export type InferInit = Extract<OracleInit, { tag: "Infer" }>;
export type ApplyInit = Extract<OracleInit, { tag: "Apply" }>;

export interface OracleAdapter {
  startSession(init: OracleInit): OracleSession;
}
```

### FILE: src/core/oracle/meaning.ts
```typescript
// src/core/oracle/meaning.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Patch Set A2: Meaning as structured first-class value
// Patch Set 6: Enhanced obligations for term rewriting

import type { Val } from "../eval/values";
import type { DistVal } from "../eval/dist";
import type { Expr } from "../ast";
import type { Evidence } from "../provenance/evidence";
import { evidenceToVal } from "../provenance/evidence";

export type { Evidence, OracleEvidence, TransformEvidence, DerivedEvidence } from "../provenance/evidence";

/**
 * Obligation types for governing program transformations.
 * These must be discharged before a commit can succeed.
 */
export type Obligation =
  | { tag: "OblTests"; tests: Expr[]; envRef: string }
  | { tag: "OblIdempotent"; f: Expr; domain?: Val }              // f(f(x)) = f(x)
  | { tag: "OblNoMatch"; pattern: Expr; scope: "output" | "all" } // pattern must not appear
  | { tag: "OblEqExt"; original: Expr; candidate: Expr; tests: Expr[]; envRef: string }; // extensional equivalence

/**
 * Rewrite trace step for debugging and audit.
 */
export type RewriteStep = {
  rule: string;         // rule name
  before: Val;          // AST before
  after: Val;           // AST after
  position?: string;    // path in AST where rewrite occurred
};

/**
 * Meaning is a VALUE in Omega.
 * We store "residual/rewrite" as Val so you can represent them as Syntax values (tag:"Syntax") or as quoted AST data.
 */
export type MeaningVal = {
  tag: "Meaning";

  // Denotation plane
  denotation?: Val | DistVal;

  // Program plane
  residual?: Val;   // typically {tag:"Syntax", stx: ...} or quoted AST (partially evaluated)
  rewrite?: Val;    // candidate transformed program

  // Analysis plane (optional for now; but the fields EXIST)
  invariants?: Val;
  effects?: Val;
  cost?: Val;
  paths?: Val;
  deps?: Val;
  memo?: Val;

  // Governance plane (Prompt 6 enhanced)
  obligation?: Val;        // legacy single obligation (backward compat)
  obligations?: Obligation[];  // structured obligations array
  evidence?: Evidence[];   // discharge evidence
  confidence?: number;     // 0..1
  trace?: Val | RewriteStep[];  // rewrite trace

  // Runtime surgery hook (optional but extremely powerful)
  adoptEnvRef?: string;
  adoptStateRef?: string;
};

export function meaning(partial: Omit<MeaningVal, "tag">): MeaningVal {
  return { tag: "Meaning", ...partial };
}

export function isMeaning(v: Val): v is MeaningVal {
  return typeof v === "object" && v !== null && (v as any).tag === "Meaning";
}

// Helper to convert old-style protocol Meaning to MeaningVal
function vStr(s: string): Val {
  return { tag: "Str", s };
}
function vNum(n: number): Val {
  return { tag: "Num", n };
}

/**
 * Convert MeaningVal to Map representation for backward compatibility.
 * Prefer using MeaningVal directly as it's now a first-class value.
 */
export function meaningToVal(m: MeaningVal): Val {
  const entries: Array<[Val, Val]> = [];
  entries.push([vStr("tag"), vStr("Meaning")]);

  if (m.denotation) entries.push([vStr("denotation"), m.denotation as Val]);
  if (m.confidence !== undefined) entries.push([vStr("confidence"), vNum(m.confidence)]);
  if (m.adoptEnvRef) entries.push([vStr("adoptEnvRef"), vStr(m.adoptEnvRef)]);
  if (m.residual) entries.push([vStr("residual"), m.residual]);
  if (m.rewrite) entries.push([vStr("rewrite"), m.rewrite]);
  if (m.obligation) entries.push([vStr("obligation"), m.obligation]);
  // Convert Evidence[] to Val (vector of maps) if present
  if (m.evidence) {
    const evidenceItems: Val[] = m.evidence.map(e => evidenceToVal(e));
    entries.push([vStr("evidence"), { tag: "Vector", items: evidenceItems }]);
  }
  if (m.trace) entries.push([vStr("trace"), m.trace as Val]);

  return { tag: "Map", entries };
}
```

### FILE: src/core/oracle/portal.ts
```typescript
// src/core/oracle/portal.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-14.md

import type { OracleReq, OracleResp } from "./protocol";

export interface OraclePortal {
  perform(req: OracleReq): Promise<OracleResp>;
}
```

## 3. OPR SYSTEM (Omega Protocol Runtime)


### FILE: src/core/opr/types.ts
```typescript
/**
 * ΩPR Type System
 */

// Kernel Output Contract
export interface KernelState {
  iteration?: number;
  facts?: string[];
  derived?: string[];
  done?: boolean;
  [key: string]: unknown;
}

export interface Diagnostics {
  invariants_checked?: string[];
  notes?: string[];
  errors?: string[];
}

export interface KernelOutput {
  kernel: string;
  op: string;
  ok: boolean;
  result: unknown;
  next_state: KernelState | null;
  effects: Effect[];
  diagnostics: Diagnostics;
}

// Effect Types
export type EffectType =
  | 'callback.eval_lisp'
  | 'callback.artifact.get'
  | 'callback.facts.query'
  | 'callback.hash';

export interface Precondition {
  type: 'fact_exists' | 'artifact_exists' | 'capability_held';
  value: unknown;
}

export interface Effect {
  type: EffectType;
  idempotency_key: string;
  correlation_id?: string;
  payload: unknown;
  preconditions?: Precondition[];
}

// Validation Types
export type ViolationCode =
  | 'NOT_JSON'
  | 'NOT_OBJECT'
  | 'MISSING_FIELD'
  | 'WRONG_TYPE'
  | 'INVALID_VALUE'
  | 'KERNEL_MISMATCH'
  | 'OP_MISMATCH';

export interface ValidationViolation {
  path: string;
  code: ViolationCode;
  message: string;
  expected?: string;
  actual?: string;
}

export interface ValidationResult {
  ok: boolean;
  parsed?: unknown;
  violations: ValidationViolation[];
}

// Receipt Types
export type Hash = `sha256:${string}`;
export type ReceiptId = `rct_${string}`;
export type EffectReceiptId = `effr_${string}`;
export type HashRef = Hash | null;
export type ReceiptStatus = 'OK' | 'ERROR' | 'TIMEOUT' | 'CALLBACK_ERROR';

export interface OprReceipt {
  receipt_version: 1;
  receipt_id: ReceiptId;
  created_at: string;
  prev_receipt_hash: HashRef;
  request_hash: Hash;
  response_hash: HashRef;
  kernel_id: string;
  op: string;
  attempt: number;
  status: ReceiptStatus;
  errors: string[];
  diagnostics?: Diagnostics;
  receipt_hash: Hash;
}

// Callback and Capabilities Types
export interface CallbackResult {
  correlation_id: string;
  ok: boolean;
  value?: unknown;
  error?: {
    code: string;
    message: string;
  };
}

export interface OprCapabilities {
  allowedCallbacks: Set<EffectType>;
  maxCallbacksPerStep: number;
  callbackTimeout: number;
}

// Budget Types
export interface OprBudgetConfig {
  maxAttempts: number;
  sessionBudget?: unknown;
}

// Progress Invariants
export interface ProgressInvariants {
  iterationMonotonic: boolean;
  derivedMonotonic: boolean;
  deltaTermination: boolean;
}

// Error Classes
export class OprError extends Error {
  constructor(message: string, public readonly code: string) {
    super(message);
    this.name = 'OprError';
  }
}

export class OprValidationError extends OprError {
  constructor(
    message: string,
    public readonly violations: ValidationViolation[]
  ) {
    super(message, 'VALIDATION_FAILED');
    this.name = 'OprValidationError';
  }
}

export class OprBudgetExhaustedError extends OprError {
  constructor(
    message: string,
    public readonly budgetType: 'attempts' | 'session-tokens' | 'session-cost'
  ) {
    super(message, 'BUDGET_EXHAUSTED');
    this.name = 'OprBudgetExhaustedError';
  }
}

export class OprCapabilityError extends OprError {
  constructor(
    message: string,
    public readonly requestedCapability: string
  ) {
    super(message, 'CAPABILITY_VIOLATION');
    this.name = 'OprCapabilityError';
  }
}

// Step Result - Discriminated Union
export interface OprStepResultBase {
  attempts: number;
  receipts: OprReceipt[];
}

export interface OprStepResultOk extends OprStepResultBase {
  tag: 'ok';
  ok: true;
  output: KernelOutput;
}

export interface OprStepResultBudgetExhausted extends OprStepResultBase {
  tag: 'budget-exhausted';
  ok: false;
  error: OprBudgetExhaustedError;
  lastOutput?: KernelOutput;
}

export interface OprStepResultValidationFailed extends OprStepResultBase {
  tag: 'validation-failed';
  ok: false;
  error: OprValidationError;
  violations: ValidationViolation[];
}

export interface OprStepResultCapabilityViolation extends OprStepResultBase {
  tag: 'capability-violation';
  ok: false;
  error: OprCapabilityError;
  requestedCapability: string;
}

export type OprStepResult =
  | OprStepResultOk
  | OprStepResultBudgetExhausted
  | OprStepResultValidationFailed
  | OprStepResultCapabilityViolation;

// Type Guards
export function isOprStepResultOk(r: OprStepResult): r is OprStepResultOk {
  return r.tag === 'ok';
}

export function isOprStepResultBudgetExhausted(
  r: OprStepResult
): r is OprStepResultBudgetExhausted {
  return r.tag === 'budget-exhausted';
}

export function isOprStepResultValidationFailed(
  r: OprStepResult
): r is OprStepResultValidationFailed {
  return r.tag === 'validation-failed';
}

export function isOprStepResultCapabilityViolation(
  r: OprStepResult
): r is OprStepResultCapabilityViolation {
  return r.tag === 'capability-violation';
}
```

### FILE: src/core/opr/hash.ts
```typescript
/**
 * OPR Hash Utilities
 *
 * Deterministic hashing for content-addressed receipts and audit trail integrity.
 */

import { createHash, randomBytes } from 'crypto';
import type { Hash } from './types';

/**
 * Compute SHA-256 hash of content
 * Returns branded Hash type: `sha256:${hex}`
 */
export function sha256Of(content: unknown): Hash {
  const canonical = canonicalJson(content);
  const hash = createHash('sha256').update(canonical).digest('hex');
  return `sha256:${hash}` as Hash;
}

/**
 * Generate a new unique ID (for receipt IDs, etc.)
 */
export function newId(): string {
  return randomBytes(16).toString('hex');
}

/**
 * Canonical JSON serialization for deterministic hashing
 * - Sorts object keys alphabetically
 * - No extra whitespace
 * - Handles undefined by omitting keys
 */
export function canonicalJson(value: unknown): string {
  return JSON.stringify(value, (_, v) => {
    if (v && typeof v === 'object' && !Array.isArray(v)) {
      // Sort object keys
      const sorted: Record<string, unknown> = {};
      for (const key of Object.keys(v).sort()) {
        if (v[key] !== undefined) {
          sorted[key] = v[key];
        }
      }
      return sorted;
    }
    return v;
  });
}

/**
 * Verify a hash matches content
 */
export function verifyHash(content: unknown, expectedHash: Hash): boolean {
  const computed = sha256Of(content);
  return computed === expectedHash;
}

/**
 * Extract the hex portion from a Hash
 */
export function hashToHex(hash: Hash): string {
  return hash.slice(7); // Remove "sha256:" prefix
}

/**
 * Create a Hash from a hex string
 */
export function hexToHash(hex: string): Hash {
  return `sha256:${hex}` as Hash;
}
```

### FILE: src/core/opr/receipts.ts
```typescript
/**
 * OPR Receipt Chain
 *
 * Hash-linked receipt chain for audit trail of all LLM attempts.
 * Each receipt links to the previous, forming a verifiable chain.
 */

import type { OprReceipt, Hash, HashRef, ReceiptId, ReceiptStatus, Diagnostics } from './types';
import { sha256Of, newId } from './hash';

/**
 * Parameters for creating a receipt
 */
export interface CreateReceiptParams {
  prevReceiptHash: HashRef;
  requestHash: Hash;
  responseHash: HashRef;
  kernelId: string;
  op: string;
  attempt: number;
  status: ReceiptStatus;
  errors: string[];
  diagnostics?: Diagnostics;
}

/**
 * Result of chain verification
 */
export interface ChainVerificationResult {
  valid: boolean;
  brokenAt?: number;
  error?: string;
}

/**
 * Interface for receipt storage
 */
export interface ReceiptStore {
  /** Add a receipt to the store */
  add(receipt: OprReceipt): void;

  /** Get all receipts for a kernel */
  getByKernel(kernelId: string): OprReceipt[];

  /** Get all receipts */
  getAll(): OprReceipt[];

  /** Get the last receipt (for chaining) */
  getLast(): OprReceipt | null;

  /** Clear all receipts */
  clear(): void;

  /** Get count */
  count(): number;
}

/**
 * Create a receipt with computed self-hash
 */
export function createReceipt(params: CreateReceiptParams): OprReceipt {
  const receiptWithoutHash: Omit<OprReceipt, 'receipt_hash'> = {
    receipt_version: 1,
    receipt_id: `rct_${newId()}` as ReceiptId,
    created_at: new Date().toISOString(),
    prev_receipt_hash: params.prevReceiptHash,
    request_hash: params.requestHash,
    response_hash: params.responseHash,
    kernel_id: params.kernelId,
    op: params.op,
    attempt: params.attempt,
    status: params.status,
    errors: params.errors,
    diagnostics: params.diagnostics,
  };

  // Compute self-hash
  const receipt_hash = computeReceiptHash(receiptWithoutHash);

  return { ...receiptWithoutHash, receipt_hash };
}

/**
 * Compute hash of a receipt (excluding the receipt_hash field)
 */
function computeReceiptHash(receipt: Omit<OprReceipt, 'receipt_hash'>): Hash {
  return sha256Of(receipt);
}

/**
 * Verify the integrity of a receipt chain
 */
export function verifyReceiptChain(receipts: OprReceipt[]): ChainVerificationResult {
  if (receipts.length === 0) {
    return { valid: true };
  }

  for (let i = 0; i < receipts.length; i++) {
    const receipt = receipts[i];

    // Verify self-hash
    const { receipt_hash: _, ...hashableFields } = receipt;
    const computedHash = sha256Of(hashableFields);
    if (computedHash !== receipt.receipt_hash) {
      return {
        valid: false,
        brokenAt: i,
        error: `Receipt ${i} self-hash mismatch: expected ${receipt.receipt_hash}, computed ${computedHash}`,
      };
    }

    // Verify chain link (except first)
    if (i > 0) {
      const prevReceipt = receipts[i - 1];
      if (receipt.prev_receipt_hash !== prevReceipt.receipt_hash) {
        return {
          valid: false,
          brokenAt: i,
          error: `Receipt ${i} chain link broken: prev_receipt_hash doesn't match previous receipt`,
        };
      }
    } else {
      // First receipt should have null prev_receipt_hash
      if (receipt.prev_receipt_hash !== null) {
        return {
          valid: false,
          brokenAt: 0,
          error: `First receipt should have null prev_receipt_hash`,
        };
      }
    }
  }

  return { valid: true };
}

/**
 * In-memory implementation of ReceiptStore
 */
export class InMemoryReceiptStore implements ReceiptStore {
  private receipts: OprReceipt[] = [];
  private byKernel = new Map<string, OprReceipt[]>();

  add(receipt: OprReceipt): void {
    this.receipts.push(receipt);

    const kernelReceipts = this.byKernel.get(receipt.kernel_id) ?? [];
    kernelReceipts.push(receipt);
    this.byKernel.set(receipt.kernel_id, kernelReceipts);
  }

  getByKernel(kernelId: string): OprReceipt[] {
    return this.byKernel.get(kernelId) ?? [];
  }

  getAll(): OprReceipt[] {
    return [...this.receipts];
  }

  getLast(): OprReceipt | null {
    return this.receipts.length > 0 ? this.receipts[this.receipts.length - 1] : null;
  }

  clear(): void {
    this.receipts = [];
    this.byKernel.clear();
  }

  count(): number {
    return this.receipts.length;
  }
}

/**
 * Fluent builder for creating receipts
 */
export class ReceiptBuilder {
  private store: ReceiptStore;
  private kernelId: string;
  private op: string;

  constructor(store: ReceiptStore, kernelId: string, op: string) {
    this.store = store;
    this.kernelId = kernelId;
    this.op = op;
  }

  /** Create and add a receipt for a successful attempt */
  success(
    attempt: number,
    requestHash: Hash,
    responseHash: Hash,
    diagnostics?: Diagnostics
  ): OprReceipt {
    const receipt = createReceipt({
      prevReceiptHash: this.store.getLast()?.receipt_hash ?? null,
      requestHash,
      responseHash,
      kernelId: this.kernelId,
      op: this.op,
      attempt,
      status: 'OK',
      errors: [],
      diagnostics,
    });
    this.store.add(receipt);
    return receipt;
  }

  /** Create and add a receipt for a failed attempt */
  error(
    attempt: number,
    requestHash: Hash,
    responseHash: HashRef,
    errors: string[]
  ): OprReceipt {
    const receipt = createReceipt({
      prevReceiptHash: this.store.getLast()?.receipt_hash ?? null,
      requestHash,
      responseHash,
      kernelId: this.kernelId,
      op: this.op,
      attempt,
      status: 'ERROR',
      errors,
    });
    this.store.add(receipt);
    return receipt;
  }

  /** Create and add a receipt for a timeout */
  timeout(attempt: number, requestHash: Hash): OprReceipt {
    const receipt = createReceipt({
      prevReceiptHash: this.store.getLast()?.receipt_hash ?? null,
      requestHash,
      responseHash: null,
      kernelId: this.kernelId,
      op: this.op,
      attempt,
      status: 'TIMEOUT',
      errors: ['Request timed out'],
    });
    this.store.add(receipt);
    return receipt;
  }

  /** Create and add a receipt for a callback error */
  callbackError(
    attempt: number,
    requestHash: Hash,
    responseHash: HashRef,
    errors: string[]
  ): OprReceipt {
    const receipt = createReceipt({
      prevReceiptHash: this.store.getLast()?.receipt_hash ?? null,
      requestHash,
      responseHash,
      kernelId: this.kernelId,
      op: this.op,
      attempt,
      status: 'CALLBACK_ERROR',
      errors,
    });
    this.store.add(receipt);
    return receipt;
  }
}
```

### FILE: src/core/opr/validate.ts
```typescript
/**
 * ΩPR Validation Pipeline
 *
 * Single-parse validation that returns structured violations usable for
 * counterexample-guided retry.
 */

import type {
  ValidationResult,
  ValidationViolation,
  KernelState,
  ProgressInvariants,
} from './types';

/**
 * Validates kernel output JSON response.
 *
 * Performs single-parse validation, reusing the parsed result for repair prompts.
 *
 * @param raw - Raw JSON string from kernel
 * @param expected - Expected kernel ID and operation for security validation
 * @returns ValidationResult with structured violations
 */
export function validateKernelOutput(
  raw: string,
  expected: { kernelId: string; op: string }
): ValidationResult {
  const violations: ValidationViolation[] = [];

  // 1. Parse JSON (single parse, reuse result)
  let parsed: unknown;
  try {
    parsed = JSON.parse(raw);
  } catch (e) {
    return {
      ok: false,
      violations: [
        {
          path: '$',
          code: 'NOT_JSON',
          message: `Invalid JSON: ${(e as Error).message}`,
        },
      ],
    };
  }

  // 2. Must be object
  if (typeof parsed !== 'object' || parsed === null || Array.isArray(parsed)) {
    return {
      ok: false,
      parsed,
      violations: [
        {
          path: '$',
          code: 'NOT_OBJECT',
          message: 'Response must be a JSON object',
          expected: 'object',
          actual: Array.isArray(parsed) ? 'array' : typeof parsed,
        },
      ],
    };
  }

  const obj = parsed as Record<string, unknown>;

  // 3. Required fields
  const required = ['kernel', 'op', 'ok', 'result', 'next_state', 'effects', 'diagnostics'];
  for (const field of required) {
    if (!(field in obj)) {
      violations.push({
        path: `$.${field}`,
        code: 'MISSING_FIELD',
        message: `Missing required field: ${field}`,
      });
    }
  }

  // Continue validation only if we have the basic structure
  if (violations.length > 0) {
    return { ok: false, parsed, violations };
  }

  // 4. Type checks
  validateFieldType(obj, 'kernel', 'string', violations);
  validateFieldType(obj, 'op', 'string', violations);
  validateFieldType(obj, 'ok', 'boolean', violations);

  // 5. Kernel/op match (critical for security)
  if (obj.kernel !== expected.kernelId) {
    violations.push({
      path: '$.kernel',
      code: 'KERNEL_MISMATCH',
      message: `Expected kernel "${expected.kernelId}", got "${obj.kernel}"`,
      expected: expected.kernelId,
      actual: String(obj.kernel),
    });
  }

  if (obj.op !== expected.op) {
    violations.push({
      path: '$.op',
      code: 'OP_MISMATCH',
      message: `Expected op "${expected.op}", got "${obj.op}"`,
      expected: expected.op,
      actual: String(obj.op),
    });
  }

  // 6. Effects array validation
  if ('effects' in obj) {
    if (!Array.isArray(obj.effects)) {
      violations.push({
        path: '$.effects',
        code: 'WRONG_TYPE',
        message: 'effects must be an array',
        expected: 'array',
        actual: typeof obj.effects,
      });
    } else {
      validateEffectsArray(obj.effects, violations);
    }
  }

  // 7. next_state must be object or null
  if ('next_state' in obj && obj.next_state !== null && typeof obj.next_state !== 'object') {
    violations.push({
      path: '$.next_state',
      code: 'WRONG_TYPE',
      message: 'next_state must be object or null',
      expected: 'object | null',
      actual: typeof obj.next_state,
    });
  }

  return {
    ok: violations.length === 0,
    parsed,
    violations,
  };
}

/**
 * Validates a field type within an object.
 *
 * @param obj - Object to validate
 * @param field - Field name
 * @param expectedType - Expected typeof result
 * @param violations - Array to accumulate violations
 */
export function validateFieldType(
  obj: Record<string, unknown>,
  field: string,
  expectedType: string,
  violations: ValidationViolation[]
): void {
  if (field in obj && typeof obj[field] !== expectedType) {
    violations.push({
      path: `$.${field}`,
      code: 'WRONG_TYPE',
      message: `${field} must be a ${expectedType}`,
      expected: expectedType,
      actual: typeof obj[field],
    });
  }
}

/**
 * Validates an array of effects.
 *
 * Each effect must have:
 * - type: string (effect type)
 * - idempotency_key: string (for deduplication)
 *
 * @param effects - Array of effects to validate
 * @param violations - Array to accumulate violations
 */
export function validateEffectsArray(
  effects: unknown[],
  violations: ValidationViolation[]
): void {
  for (let i = 0; i < effects.length; i++) {
    const effect = effects[i];
    if (typeof effect !== 'object' || effect === null) {
      violations.push({
        path: `$.effects[${i}]`,
        code: 'WRONG_TYPE',
        message: `Effect at index ${i} must be an object`,
        expected: 'object',
        actual: typeof effect,
      });
      continue;
    }

    const e = effect as Record<string, unknown>;

    // Required effect fields
    if (!('type' in e) || typeof e.type !== 'string') {
      violations.push({
        path: `$.effects[${i}].type`,
        code: 'MISSING_FIELD',
        message: `Effect at index ${i} missing required field: type`,
      });
    }

    if (!('idempotency_key' in e) || typeof e.idempotency_key !== 'string') {
      violations.push({
        path: `$.effects[${i}].idempotency_key`,
        code: 'MISSING_FIELD',
        message: `Effect at index ${i} missing required field: idempotency_key`,
      });
    }
  }
}

/**
 * Checks progress invariants between state transitions.
 *
 * Enforces monotonicity properties when enabled:
 * - iterationMonotonic: iteration number must increase
 * - derivedMonotonic: derived facts can only grow
 *
 * @param prevState - Previous kernel state (null if initial)
 * @param nextState - Next kernel state
 * @param invariants - Progress invariant configuration
 * @returns Array of violations
 */
export function checkProgressInvariants(
  prevState: KernelState | null,
  nextState: KernelState,
  invariants: ProgressInvariants
): ValidationViolation[] {
  const violations: ValidationViolation[] = [];

  if (prevState === null) return violations;

  if (invariants.iterationMonotonic) {
    const prevIter = prevState.iteration ?? 0;
    const nextIter = nextState.iteration ?? 0;
    if (nextIter <= prevIter) {
      violations.push({
        path: '$.next_state.iteration',
        code: 'INVALID_VALUE',
        message: `Iteration must increase: ${prevIter} -> ${nextIter}`,
      });
    }
  }

  if (invariants.derivedMonotonic) {
    const prevCount = prevState.derived?.length ?? 0;
    const nextCount = nextState.derived?.length ?? 0;
    if (nextCount < prevCount) {
      violations.push({
        path: '$.next_state.derived',
        code: 'INVALID_VALUE',
        message: `Derived facts must grow monotonically: ${prevCount} -> ${nextCount}`,
      });
    }
  }

  return violations;
}
```

### FILE: src/core/opr/retry.ts
```typescript
import type { ValidationViolation } from './types';

/**
 * Build a repair prompt from validation violations
 * This becomes the "counterexample" that guides the LLM to fix its output
 */
export function buildRepairPrompt(violations: ValidationViolation[]): string {
  const lines: string[] = [
    'YOUR PREVIOUS RESPONSE HAD VALIDATION ERRORS.',
    '',
    'VIOLATIONS:',
  ];

  for (const v of violations) {
    lines.push(`  - Path: ${v.path}`);
    lines.push(`    Code: ${v.code}`);
    lines.push(`    Error: ${v.message}`);
    if (v.expected) {
      lines.push(`    Expected: ${v.expected}`);
    }
    if (v.actual) {
      lines.push(`    Got: ${v.actual}`);
    }
    lines.push('');
  }

  lines.push('INSTRUCTIONS:');
  lines.push('1. Fix ALL violations listed above');
  lines.push('2. Return ONLY valid JSON matching the OUTPUT CONTRACT');
  lines.push('3. Do NOT include markdown code blocks');
  lines.push('4. Do NOT include any explanation or preamble');
  lines.push('');
  lines.push('Return the corrected JSON response now:');

  return lines.join('\n');
}

/**
 * Decide whether to retry based on violation type
 */
export function shouldRetry(violations: ValidationViolation[]): boolean {
  // Don't retry on kernel/op mismatch - indicates fundamental confusion
  const hasCriticalViolation = violations.some(v =>
    v.code === 'KERNEL_MISMATCH' || v.code === 'OP_MISMATCH'
  );

  if (hasCriticalViolation) {
    return false;
  }

  // Retry on fixable violations
  const fixableCodes: Set<string> = new Set([
    'NOT_JSON',
    'NOT_OBJECT',
    'MISSING_FIELD',
    'WRONG_TYPE',
    'INVALID_VALUE',
  ]);

  return violations.every(v => fixableCodes.has(v.code));
}

/**
 * Format violations for human-readable display
 */
export function formatViolationsForDisplay(violations: ValidationViolation[]): string {
  return violations.map(v => {
    let line = `[${v.code}] ${v.path}: ${v.message}`;
    if (v.expected && v.actual) {
      line += ` (expected ${v.expected}, got ${v.actual})`;
    }
    return line;
  }).join('\n');
}
```

### FILE: src/core/opr/runtime.ts
```typescript
/**
 * OPR Runtime
 *
 * The main execution engine for OPR kernel operations.
 * Handles validation, retry with counterexample feedback, and receipt generation.
 */

import type {
  OprStepResult,
  OprStepResultOk,
  OprStepResultBudgetExhausted,
  KernelOutput,
  KernelState,
  OprBudgetConfig,
  ProgressInvariants,
  ValidationViolation,
  Hash,
  OprReceipt,
} from './types';
import { OprBudgetExhaustedError } from './types';
import type { OprLLMAdapter, OprLLMRequest } from './adapters/types';
import type { ReceiptStore } from './receipts';
import { ReceiptBuilder } from './receipts';
import { validateKernelOutput, checkProgressInvariants } from './validate';
import { buildRepairPrompt } from './retry';
import { sha256Of } from './hash';
import type { PromptDoc } from '../../frameir/prompt';

/**
 * Kernel prompt configuration
 */
export interface KernelPromptConfig {
  /** Unique kernel identifier */
  id: string;

  /** Compiled prompt document */
  prompt: PromptDoc;

  /** Operation name (default: 'step') */
  op?: string;
}

/**
 * Session budget interface (optional)
 */
export interface SessionBudget {
  hasRemaining(type: 'tokens' | 'cost'): boolean;
  consumeTokens(tokens: number): void;
  consumeCost(cost: number): void;
}

/**
 * OPR Runtime configuration
 */
export interface OprRuntimeConfig {
  /** Kernel configuration with prompt and capabilities */
  kernel: KernelPromptConfig;

  /** LLM adapter for making calls */
  adapter: OprLLMAdapter;

  /** Receipt storage */
  receipts: ReceiptStore;

  /** Budget configuration */
  budget: OprBudgetConfig;

  /** Progress invariants to enforce (optional) */
  invariants?: ProgressInvariants;

  /** Session budget (optional) */
  sessionBudget?: SessionBudget;
}

/**
 * Parameters for executing a step
 */
export interface OprExecuteParams {
  /** Program/data to pass to kernel */
  program: unknown;

  /** Current state (null for first step, memento from previous step) */
  state: unknown | null;
}

/**
 * Internal request type
 */
export interface OprRequest {
  kernelId: string;
  prompt: PromptDoc;
  program: unknown;
  state: unknown | null;
  repairContext: string | null;
}

/**
 * Run result types
 */
export interface OprRunResultOk {
  tag: 'ok';
  results: OprStepResultOk[];
  finalState: KernelState | null;
  iterations: number;
  receipts: OprReceipt[];
}

export interface OprRunResultError {
  tag: 'error';
  error: OprStepResult;
  iterations: number;
  receipts: OprReceipt[];
}

export interface OprRunResultMaxIterations {
  tag: 'max-iterations';
  results: OprStepResultOk[];
  iterations: number;
  receipts: OprReceipt[];
}

export type OprRunResult = OprRunResultOk | OprRunResultError | OprRunResultMaxIterations;

interface ExtractOk {
  ok: true;
  json: unknown;
}

interface ExtractError {
  ok: false;
  error: string;
}

type ExtractResult = ExtractOk | ExtractError;

/**
 * Extract JSON object from LLM response
 * Handles markdown code blocks and extra text
 */
function extractJsonObject(response: string): ExtractResult {
  // Try to find JSON in code blocks first
  const codeBlockMatch = response.match(/```(?:json)?\s*([\s\S]*?)```/);
  const content = codeBlockMatch ? codeBlockMatch[1].trim() : response.trim();

  // Find the first { and matching }
  const start = content.indexOf('{');
  if (start === -1) {
    return { ok: false, error: 'No JSON object found in response' } as ExtractError;
  }

  let depth = 0;
  let end = -1;
  for (let i = start; i < content.length; i++) {
    if (content[i] === '{') depth++;
    else if (content[i] === '}') {
      depth--;
      if (depth === 0) {
        end = i;
        break;
      }
    }
  }

  if (end === -1) {
    return { ok: false, error: 'Unbalanced braces in JSON' } as ExtractError;
  }

  const jsonStr = content.slice(start, end + 1);
  try {
    const json = JSON.parse(jsonStr);
    return { ok: true, json } as ExtractOk;
  } catch (e) {
    return { ok: false, error: `Invalid JSON: ${(e as Error).message}` } as ExtractError;
  }
}

/**
 * OPR Runtime - the main execution engine
 */
export class OprRuntime {
  private config: OprRuntimeConfig;
  private receiptBuilder: ReceiptBuilder;

  constructor(config: OprRuntimeConfig) {
    this.config = config;
    this.receiptBuilder = new ReceiptBuilder(
      config.receipts,
      config.kernel.id,
      config.kernel.op ?? 'step'
    );
  }

  /**
   * Execute a single kernel step with validation and retry
   */
  async step(params: OprExecuteParams): Promise<OprStepResult> {
    const { kernel, adapter, budget, invariants, sessionBudget } = this.config;
    const { maxAttempts } = budget;

    let lastViolations: ValidationViolation[] = [];
    let repairContext: string | null = null;

    for (let attempt = 1; attempt <= maxAttempts; attempt++) {
      // Check session budget (if provided)
      if (sessionBudget) {
        if (!sessionBudget.hasRemaining('tokens')) {
          return this.makeBudgetExhaustedResult('session-tokens', attempt, lastViolations);
        }
        if (!sessionBudget.hasRemaining('cost')) {
          return this.makeBudgetExhaustedResult('session-cost', attempt, lastViolations);
        }
      }

      // Build request
      const userContent = this.formatUserContent(params, repairContext);
      const request: OprLLMRequest = {
        kernelId: kernel.id,
        prompt: kernel.prompt,
        userContent,
        repairContext: repairContext ?? undefined,
      };
      const requestHash = sha256Of(request);

      // Call LLM
      let response: string;
      try {
        response = await adapter.complete(request);
      } catch (e) {
        // Record timeout/error receipt
        this.receiptBuilder.timeout(attempt, requestHash);
        continue;
      }

      const responseHash = sha256Of(response);

      // Consume session budget (if provided)
      if (sessionBudget && adapter.getLastUsage) {
        const usage = adapter.getLastUsage();
        sessionBudget.consumeTokens(usage.totalTokens);
        sessionBudget.consumeCost(usage.estimatedCost);
      }

      // Extract JSON from response (handles markdown code blocks, etc.)
      const extracted = extractJsonObject(response);
      if (extracted.ok === false) {
        const errorMsg = (extracted as ExtractError).error;
        lastViolations = [
          {
            path: '$',
            code: 'NOT_JSON',
            message: errorMsg,
          },
        ];
        this.receiptBuilder.error(attempt, requestHash, responseHash, [errorMsg]);
        repairContext = buildRepairPrompt(lastViolations);
        continue;
      }

      // Validate response
      const validation = validateKernelOutput(JSON.stringify(extracted.json), {
        kernelId: kernel.id,
        op: kernel.op ?? 'step',
      });

      if (!validation.ok) {
        lastViolations = validation.violations;
        this.receiptBuilder.error(
          attempt,
          requestHash,
          responseHash,
          validation.violations.map((v) => v.message)
        );
        repairContext = buildRepairPrompt(validation.violations);
        continue;
      }

      const output = validation.parsed as KernelOutput;

      // Check progress invariants (if configured and we have previous state)
      if (invariants && params.state !== null && output.next_state !== null) {
        const invariantViolations = checkProgressInvariants(
          params.state as KernelState,
          output.next_state,
          invariants
        );
        if (invariantViolations.length > 0) {
          lastViolations = invariantViolations;
          this.receiptBuilder.error(
            attempt,
            requestHash,
            responseHash,
            invariantViolations.map((v) => v.message)
          );
          repairContext = buildRepairPrompt(invariantViolations);
          continue;
        }
      }

      // Success!
      this.receiptBuilder.success(attempt, requestHash, responseHash, output.diagnostics);

      return {
        tag: 'ok',
        ok: true,
        output,
        attempts: attempt,
        receipts: this.config.receipts.getAll(),
      };
    }

    // Exhausted local budget
    return this.makeBudgetExhaustedResult('attempts', maxAttempts, lastViolations);
  }

  /**
   * Run kernel to fixpoint (until next_state.done = true or null)
   */
  async runToFixpoint(params: OprExecuteParams): Promise<OprRunResult> {
    const results: OprStepResultOk[] = [];
    let currentState = params.state;
    let iterations = 0;
    const maxIterations = 100; // Safety limit

    while (iterations < maxIterations) {
      const result = await this.step({ program: params.program, state: currentState });

      if (result.tag !== 'ok') {
        return {
          tag: 'error',
          error: result,
          iterations,
          receipts: this.config.receipts.getAll(),
        };
      }

      results.push(result);
      iterations++;

      // Check termination
      const nextState = result.output.next_state;
      if (nextState === null || nextState.done === true) {
        return {
          tag: 'ok',
          results,
          finalState: nextState,
          iterations,
          receipts: this.config.receipts.getAll(),
        };
      }

      currentState = nextState;
    }

    // Max iterations reached
    return {
      tag: 'max-iterations',
      results,
      iterations,
      receipts: this.config.receipts.getAll(),
    };
  }

  /**
   * Format user content for the LLM request
   */
  private formatUserContent(params: OprExecuteParams, repairContext: string | null): string {
    const parts: string[] = [];

    if (repairContext) {
      parts.push(repairContext);
      parts.push('\n---\n');
    }

    parts.push('PROGRAM:');
    parts.push(JSON.stringify(params.program, null, 2));

    if (params.state !== null) {
      parts.push('\nCURRENT STATE:');
      parts.push(JSON.stringify(params.state, null, 2));
    }

    return parts.join('\n');
  }

  private makeBudgetExhaustedResult(
    budgetType: 'attempts' | 'session-tokens' | 'session-cost',
    attempts: number,
    lastViolations: ValidationViolation[]
  ): OprStepResultBudgetExhausted {
    return {
      tag: 'budget-exhausted',
      ok: false,
      error: new OprBudgetExhaustedError(`Budget exhausted: ${budgetType}`, budgetType),
      attempts,
      receipts: this.config.receipts.getAll(),
    };
  }
}
```

### FILE: src/core/opr/adapters/types.ts
```typescript
/**
 * OPR Adapter Interface
 *
 * Common interface for LLM adapters used by OprRuntime.
 * Adapters handle the actual LLM API calls.
 */

import type { PromptDoc } from '../../../frameir/prompt';

/**
 * Usage information from an LLM call
 */
export interface LLMUsage {
  promptTokens: number;
  completionTokens: number;
  totalTokens: number;
  estimatedCost: number; // in USD
}

/**
 * Request to send to LLM
 */
export interface OprLLMRequest {
  /** Kernel ID for context */
  kernelId: string;

  /** Compiled prompt (system + few-shot examples) */
  prompt: PromptDoc;

  /** User content (program, state, etc.) */
  userContent: string;

  /** Optional repair context from previous failed attempt */
  repairContext?: string;

  /** Max tokens for response */
  maxTokens?: number;

  /** Temperature (0-1) */
  temperature?: number;
}

/**
 * Interface for LLM adapters
 */
export interface OprLLMAdapter {
  /**
   * Send a completion request to the LLM
   * @returns The raw text response from the LLM
   */
  complete(request: OprLLMRequest): Promise<string>;

  /**
   * Get usage information from the last call (optional)
   */
  getLastUsage?(): LLMUsage;

  /**
   * Get the model identifier
   */
  getModel(): string;

  /**
   * Check if the adapter supports streaming
   */
  supportsStreaming(): boolean;
}

/**
 * Base configuration for all adapters
 */
export interface OprAdapterConfig {
  /** Model to use */
  model: string;

  /** Max tokens for response (default: 2000) */
  maxTokens?: number;

  /** Temperature (default: 0) */
  temperature?: number;

  /** Request timeout in ms (default: 60000) */
  timeout?: number;
}

/**
 * OpenAI-specific configuration
 */
export interface OpenAIAdapterConfig extends OprAdapterConfig {
  apiKey: string;
  baseURL?: string;
  organization?: string;
}

/**
 * Anthropic-specific configuration
 */
export interface AnthropicAdapterConfig extends OprAdapterConfig {
  apiKey: string;
  baseURL?: string;
}

/**
 * Scripted adapter configuration (for testing)
 */
export interface ScriptedAdapterConfig {
  /** Responses to return in sequence */
  responses: Array<string | { response: string; delay?: number }>;

  /** Whether to loop responses or throw on exhaustion */
  loop?: boolean;
}

/**
 * Abstract base class for LLM adapters
 * Provides common functionality like prompt compilation
 */
export abstract class BaseOprAdapter implements OprLLMAdapter {
  protected lastUsage: LLMUsage | null = null;

  abstract complete(request: OprLLMRequest): Promise<string>;
  abstract getModel(): string;

  getLastUsage(): LLMUsage {
    if (!this.lastUsage) {
      return {
        promptTokens: 0,
        completionTokens: 0,
        totalTokens: 0,
        estimatedCost: 0,
      };
    }
    return this.lastUsage;
  }

  supportsStreaming(): boolean {
    return false;
  }

  /**
   * Format the user message content
   */
  protected formatUserContent(request: OprLLMRequest): string {
    let content = request.userContent;

    if (request.repairContext) {
      content = `${request.repairContext}\n\n---\n\nORIGINAL REQUEST:\n${content}`;
    }

    return content;
  }

  /**
   * Estimate cost based on token usage and model
   */
  protected estimateCost(
    usage: { promptTokens: number; completionTokens: number },
    model: string
  ): number {
    // Rough estimates per 1M tokens
    const pricing: Record<string, { prompt: number; completion: number }> = {
      'gpt-4o': { prompt: 2.5, completion: 10.0 },
      'gpt-4o-mini': { prompt: 0.15, completion: 0.6 },
      'gpt-4-turbo': { prompt: 10.0, completion: 30.0 },
      'claude-3-opus': { prompt: 15.0, completion: 75.0 },
      'claude-3-sonnet': { prompt: 3.0, completion: 15.0 },
      'claude-3-haiku': { prompt: 0.25, completion: 1.25 },
    };

    const prices = pricing[model] ?? { prompt: 1.0, completion: 3.0 };

    return (
      (usage.promptTokens / 1_000_000) * prices.prompt +
      (usage.completionTokens / 1_000_000) * prices.completion
    );
  }
}
```

### FILE: src/core/opr/adapters/openai.ts
```typescript
/**
 * OpenAI OPR Adapter
 *
 * Makes real calls to OpenAI API
 */

import type { OprLLMRequest, OpenAIAdapterConfig } from './types';
import { BaseOprAdapter } from './types';

export class OpenAIOprAdapter extends BaseOprAdapter {
  private config: OpenAIAdapterConfig;

  constructor(config: OpenAIAdapterConfig) {
    super();
    this.config = config;
  }

  async complete(request: OprLLMRequest): Promise<string> {
    const { apiKey, baseURL, organization, model, maxTokens, temperature, timeout } = this.config;

    // Build messages from prompt
    const messages: Array<{ role: string; content: string }> = [];

    // Extract system message from prompt (supports PSystem from FrameIR)
    if (request.prompt && request.prompt.parts) {
      for (const part of request.prompt.parts) {
        if ((part as any).tag === 'PSystem') {
          messages.push({ role: 'system', content: (part as any).text });
        } else if ((part as any).tag === 'System') {
          messages.push({ role: 'system', content: (part as any).content });
        }
      }
    }

    // Add user content
    messages.push({ role: 'user', content: request.userContent });

    const body = {
      model,
      messages,
      max_tokens: request.maxTokens ?? maxTokens ?? 2000,
      temperature: request.temperature ?? temperature ?? 0,
    };

    const headers: Record<string, string> = {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${apiKey}`,
    };

    if (organization) {
      headers['OpenAI-Organization'] = organization;
    }

    const url = `${baseURL ?? 'https://api.openai.com/v1'}/chat/completions`;

    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), timeout ?? 60000);

    try {
      const response = await fetch(url, {
        method: 'POST',
        headers,
        body: JSON.stringify(body),
        signal: controller.signal,
      });

      clearTimeout(timeoutId);

      if (!response.ok) {
        const error = await response.text();
        throw new Error(`OpenAI API error ${response.status}: ${error}`);
      }

      const data = await response.json();

      // Track usage
      if (data.usage) {
        this.lastUsage = {
          promptTokens: data.usage.prompt_tokens,
          completionTokens: data.usage.completion_tokens,
          totalTokens: data.usage.total_tokens,
          estimatedCost: this.estimateCost(
            {
              promptTokens: data.usage.prompt_tokens,
              completionTokens: data.usage.completion_tokens,
            },
            model
          ),
        };
      }

      return data.choices[0]?.message?.content ?? '';
    } catch (e) {
      clearTimeout(timeoutId);
      if ((e as Error).name === 'AbortError') {
        throw new Error('OpenAI request timed out');
      }
      throw e;
    }
  }

  getModel(): string {
    return this.config.model;
  }
}
```

### FILE: src/core/opr/kernels/logic.ts
```typescript
/**
 * Logic Inference Kernel (opr.logic.v1)
 *
 * Performs forward-chaining inference over horn clauses.
 * Input: rules (horn clauses) + facts
 * Output: newly derived facts
 */

import type { KernelPromptConfig } from '../runtime';

export const LOGIC_KERNEL: KernelPromptConfig = {
  id: 'opr.logic.v1',
  op: 'infer',
  prompt: {
    tag: 'PromptDoc',
    v: 'frameir@1',
    parts: [{
      tag: 'PSystem',
      v: 'frameir@1',
      text: `You are a LOGIC INFERENCE kernel implementing forward-chaining deduction.

INPUT FORMAT:
{
  "rules": ["head :- body1, body2", ...],  // Horn clauses
  "facts": ["predicate(args)", ...]         // Ground facts
}

SEMANTICS:
- Rules are horn clauses: "conclusion :- premise1, premise2"
- Variables are UPPERCASE: X, Y, Person
- Constants are lowercase: socrates, 42, true
- Apply all rules whose premises are satisfied by current facts
- Derive new facts by unifying variables with constants

OUTPUT CONTRACT (strict JSON, no markdown):
{
  "kernel": "opr.logic.v1",
  "op": "infer",
  "ok": true,
  "result": {
    "delta": ["newly derived facts this iteration"]
  },
  "next_state": {
    "iteration": <int>,
    "derived": ["all derived facts so far"],
    "done": <true if delta is empty, false otherwise>
  },
  "effects": [],
  "diagnostics": {
    "rules_applied": ["which rules fired"],
    "bindings": [{"rule": "...", "unifier": {...}}]
  }
}

TERMINATION: Set done=true when no new facts can be derived (fixpoint).`
    }]
  },
};
```

### FILE: src/core/opr/kernels/index.ts
```typescript
/**
 * OPR Kernel Registry
 *
 * Kernels are prompts that define how an LLM processes structured input.
 * Each kernel specifies:
 *   - Input format (what the "program" looks like)
 *   - Processing rules
 *   - Output schema (the JSON contract)
 */

import type { KernelPromptConfig } from '../runtime';

// Re-export all kernels
export { LOGIC_KERNEL } from './logic';
export { ANALYZE_KERNEL } from './analyze';
export { SEMANTIC_KERNEL } from './semantic';
export { CODE_REVIEW_KERNEL } from './codeReview';
export { TRANSFORM_KERNEL } from './transform';
export { EXTRACT_KERNEL } from './extract';
export { CLASSIFY_KERNEL } from './classify';
export { SYNTHESIZE_KERNEL } from './synthesize';
export { VALIDATE_KERNEL } from './validate';
export { PLAN_KERNEL } from './plan';

// Kernel registry
const kernels = new Map<string, KernelPromptConfig>();

export function registerKernel(config: KernelPromptConfig): void {
  kernels.set(config.id, config);
}

export function getKernel(id: string): KernelPromptConfig | undefined {
  return kernels.get(id);
}

export function listKernels(): string[] {
  return Array.from(kernels.keys());
}

// Auto-register all kernels on import
import { LOGIC_KERNEL } from './logic';
import { ANALYZE_KERNEL } from './analyze';
import { SEMANTIC_KERNEL } from './semantic';
import { CODE_REVIEW_KERNEL } from './codeReview';
import { TRANSFORM_KERNEL } from './transform';
import { EXTRACT_KERNEL } from './extract';
import { CLASSIFY_KERNEL } from './classify';
import { SYNTHESIZE_KERNEL } from './synthesize';
import { VALIDATE_KERNEL } from './validate';
import { PLAN_KERNEL } from './plan';

[
  LOGIC_KERNEL,
  ANALYZE_KERNEL,
  SEMANTIC_KERNEL,
  CODE_REVIEW_KERNEL,
  TRANSFORM_KERNEL,
  EXTRACT_KERNEL,
  CLASSIFY_KERNEL,
  SYNTHESIZE_KERNEL,
  VALIDATE_KERNEL,
  PLAN_KERNEL,
].forEach(registerKernel);
```

## 4. EFFECTS SYSTEM


### FILE: src/core/effects/runtimeImpl.ts
```typescript
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Patch Set B: int.op/infer.op/rewrite.op as distinct kernel forms
// Patch Set D: Governance (caps, budgets, commit barrier)

import type { Runtime, DispatchResult } from "../eval/runtime";
import type { State, HandlerFrame } from "../eval/machine";
import type { OpCall } from "./opcall";
import type { Val } from "../eval/values";
import { VUnit } from "../eval/values";
import { envSet } from "../eval/env";
import type { Resumption } from "./opcall";

// AMB: Alternative tracking for backtracking search
type AmbAlternative = {
  thunk: Val;           // Closure to invoke
  resumption: Resumption; // Continuation to resume with thunk's result
};

export type AmbStrategy = "DFS" | "FAIR";

// Oracle Protocol imports
import type { OracleAdapter } from "../oracle/adapter";
import type { SnapshotRepo } from "../oracle/snapshots";
import type { ReceiptStore } from "../oracle/receipts";
import { PortalImpl } from "../oracle/portalImpl";
import { runOracleSession } from "../oracle/driver";
import { isMeaning, meaning as mkMeaning, type MeaningVal, type Obligation, type Evidence } from "../oracle/meaning";
import { matchAST } from "../oracle/match";
import { distFrom, type DistVal } from "../eval/dist";
import { computeSourceHash, type OracleEvidence } from "../provenance/evidence";
import type { StoredReceipt } from "../provenance/store/interface";
import { sha256JSON } from "../artifacts/hash";

// Governance imports
import type { Profile } from "../governance/profile";
import { DEFAULT_PROFILE } from "../governance/profile";
import { capRequire, capHas, type CapSet } from "../governance/caps";
import type { BudgetTracker } from "../governance/budgets";

// Pipeline imports for parsing text to expressions
import { compileTextToExpr } from "../pipeline/compileText";
import { runToCompletionWithState } from "../eval/run";

export interface CommitAdapter {
  commit(payload: Val, ctxDigest: string): Promise<Val>;
}

function requireCommitCapability(caps: CapSet, required: string, context: string): void {
  if (capHas(caps, "commit.*")) return;
  capRequire(caps, required, context);
}

function findBoundaryIndex(kont: State["kont"], hid: string): number {
  for (let i = kont.length - 1; i >= 0; i--) {
    const fr = kont[i];
    if (fr.tag === "KHandleBoundary" && fr.hid === hid) return i;
  }
  return -1;
}

type InferKind = "int" | "search" | "rewrite";

function intSamplesFromPayload(payload: Val, fallback: number): number {
  // Optional convention: payload may be (map (("n" . <num>)) ...)
  try {
    if (payload.tag === "Map") {
      for (const [k, v] of payload.entries) {
        if (k.tag === "Str" && k.s === "n" && v.tag === "Num") return Math.max(1, Math.floor(v.n));
      }
    }
  } catch {}
  return fallback;
}

function obligationSatisfied(m: MeaningVal): boolean {
  // Convention: obligation = {tag:"Obligation", status:"satisfied"} or Bool true.
  const o = m?.obligation;
  if (!o) return false;
  if ((o as any).tag === "Bool") return !!(o as any).b;
  if ((o as any).tag === "Obligation" && (o as any).status === "satisfied") return true;
  return false;
}

function needsProvenance(state: State): boolean {
  return !!state.provenanceGraph || !!state.provenanceStore;
}

async function attachOracleEvidence(meaning: MeaningVal, payload: Val, state: State): Promise<MeaningVal> {
  if (!needsProvenance(state)) return meaning;

  const timestamp = Date.now();
  const receiptId = sha256JSON({ payload, timestamp });
  const evidence: OracleEvidence = {
    tag: "OracleEvidence",
    receiptId,
    sourceHash: computeSourceHash(payload),
    timestamp,
  };

  state.provenanceGraph?.addNode(evidence);

  if (state.provenanceStore) {
    const receipt: StoredReceipt = { id: receiptId, timestamp, request: payload, response: meaning };
    await state.provenanceStore.storeReceipt(receipt);
  }

  const evidenceList = meaning.evidence ? meaning.evidence.concat([evidence]) : [evidence];
  return { ...meaning, evidence: evidenceList };
}

export class RuntimeImpl implements Runtime {
  private profile: Profile;
  private budget?: BudgetTracker;

  // AMB: Backtracking state
  private ambAlternatives: AmbAlternative[] = [];
  private ambStrategy: AmbStrategy = "DFS";

  constructor(
    private readonly oracle: OracleAdapter,
    private readonly snapshots: SnapshotRepo,
    private readonly receipts: ReceiptStore,
    private readonly commit: CommitAdapter,
    profile?: Profile,
    budget?: BudgetTracker
  ) {
    this.profile = profile ?? DEFAULT_PROFILE;
    this.budget = budget;
  }

  /** Set the budget tracker (allows sharing with run loop). */
  setBudget(budget: BudgetTracker): void {
    this.budget = budget;
  }

  /** Set the amb search strategy (DFS or FAIR). */
  setAmbStrategy(strategy: AmbStrategy): void {
    this.ambStrategy = strategy;
  }

  /** Reset amb state (clears all pending alternatives). */
  resetAmb(): void {
    this.ambAlternatives = [];
  }

  /** Check if there are pending amb alternatives. */
  hasAmbAlternatives(): boolean {
    return this.ambAlternatives.length > 0;
  }

  /** Extract test thunks from a list/vector value. */
  private extractTestThunks(v: Val): Val[] {
    const result: Val[] = [];
    // Handle cons-cell list
    let cur = v;
    while ((cur as any).tag === "Vector" && (cur as any).items?.length === 2) {
      result.push((cur as any).items[0]);
      cur = (cur as any).items[1];
    }
    // Handle flat vector
    if ((cur as any).tag === "Vector" && (cur as any).items?.length > 0 && result.length === 0) {
      return (cur as any).items;
    }
    return result;
  }

  async dispatch(state: State, opcall: OpCall): Promise<DispatchResult> {
    // 1) Try language-level handlers (deep handlers; nearest enclosing clause wins).
    for (let hi = state.handlers.length - 1; hi >= 0; hi--) {
      const hf = state.handlers[hi];
      const clause = hf.on.get(opcall.op);
      if (!clause) continue;

      // The handler for opcall.op is hf at index hi.
      // Clause executes *outside* any inner handlers above hf (those are inside the suspended continuation).
      const handlersTrunc = state.handlers.slice(0, hi + 1);

      // Compute delimiter boundary location in the suspended continuation.
      const boundaryIndex = findBoundaryIndex(state.kont, hf.hid);
      if (boundaryIndex < 0) throw new Error(`dispatch: boundary not found for handler ${hf.hid}`);

      // Truncate continuation to delimiter boundary (discard frames inside handle body).
      // Keep the boundary itself so clause return aborts to it and triggers return clause logic.
      const kontTrunc = state.kont.slice(0, boundaryIndex + 1);

      // Construct k as a ContVal.
      const kVal: Val = { tag: "Cont", hid: hf.hid, boundaryIndex, resumption: opcall.resumption };

      // Bind params and k into handler lexical env.
      let store = state.store;
      let env = hf.env;

      // Bind operation parameters
      if (clause.params.length !== opcall.args.length) {
        throw new Error(`dispatch: op arity mismatch for ${clause.op}`);
      }
      for (let i = 0; i < clause.params.length; i++) {
        const [store2, addr] = store.alloc(opcall.args[i]);
        store = store2;
        env = envSet(env, clause.params[i], addr);
      }

      // Bind k
      {
        const [store2, addr] = store.alloc(kVal);
        store = store2;
        env = envSet(env, clause.k, addr);
      }

      // Evaluate handler clause body.
      const st2: State = {
        ...state,
        control: { tag: "Expr", e: clause.body },
        env,
        store,
        kont: kontTrunc,
        handlers: handlersTrunc,
      };
      return st2;
    }

    // 2) Built-in fallback handlers using the Oracle Protocol.
    const opName = opcall.op;

    // Patch Set B: Three distinct intensional forms
    // - infer.op: backward compatible, returns denotation directly
    // - int.op: returns full Meaning
    // - rewrite.op: returns full Meaning (expects rewrite field)
    // - search.op: returns Dist<Meaning> for multi-shot search
    if (opName === "int.op" || opName === "infer.op" || opName === "rewrite.op" || opName === "search.op") {
      const kind: InferKind = opName === "search.op" ? "search" : opName === "rewrite.op" ? "rewrite" : "int";
      const needProv = needsProvenance(state);

      // Consume oracle turn budget
      this.budget?.consumeOracleTurn();

      // Minimal caps: oracle must be able to re-enter eval/apply/observe to be "first class".
      capRequire(this.profile.caps, "eval", `start ${opName}`);
      capRequire(this.profile.caps, "apply", `start ${opName}`);
      capRequire(this.profile.caps, "observe", `start ${opName}`);

      const payload: Val = opcall.args[0] ?? ({ tag: "Unit" } as Val);

      // Snapshot current state for oracle introspection
      const envRef = this.snapshots.putEnv({ env: state.env, store: state.store });
      const stateRef = this.snapshots.putState({ state });

      // Create portal for oracle to REPL back into evaluator
      const portal = new PortalImpl(this as any, this.snapshots, this.receipts, {
        maxEvalSteps: 100_000,
        parseText: (src: string) => compileTextToExpr(src),
      });

      // SEARCH: search.op - multi-shot oracle sampling returns Dist<Meaning>
      if (kind === "search") {
        const n = intSamplesFromPayload(payload, 8);
        const items: { v: Val; w: number }[] = [];

        for (let i = 0; i < n; i++) {
          const session = this.oracle.startSession({
            tag: "Infer",
            payload,
            envRef,
            stateRef,
          });

          const meaning = await runOracleSession(session, portal);
          const enriched = needProv ? await attachOracleEvidence(meaning as MeaningVal, payload, state) : meaning;
          items.push({ v: enriched as Val, w: 1 });
        }

        const d: DistVal = distFrom(items, { kind: "search", note: `n=${n}` });
        return opcall.resumption.invoke(d as Val);
      }

      // int.op, infer.op, rewrite.op: Run ONE oracle session
      const session = this.oracle.startSession({
        tag: "Infer",
        payload,
        envRef,
        stateRef,
      });

      const meaning = await runOracleSession(session, portal);
      let enrichedMeaning = meaning as MeaningVal;
      if (needProv) {
        enrichedMeaning = await attachOracleEvidence(enrichedMeaning, payload, state);
      }

      // Optional: if oracle asked to adopt a modified env, patch state
      const adoptEnv = portal.consumeAdoptEnvRef() ?? meaning.adoptEnvRef;
      let st2 = state;
      if (adoptEnv) {
        const snap = this.snapshots.getEnv(adoptEnv);
        st2 = { ...st2, env: snap.env, store: snap.store };
      }

      // int.op and rewrite.op: Return the full Meaning as a first-class value
      if (opName === "int.op" || opName === "rewrite.op") {
        // runOracleSession always returns Meaning (MeaningVal), which is a Val
        return opcall.resumption.invoke(enrichedMeaning as Val);
      }

      // infer.op: Return the denotation directly (backward compatible)
      const resultVal = enrichedMeaning.denotation ?? ({ tag: "Unit" } as Val);
      return opcall.resumption.invoke(resultVal);
    }

    // oracle.apply.op: LLM in apply position (unchanged)
    if (opcall.op === "oracle.apply.op") {
      // Consume oracle turn budget
      this.budget?.consumeOracleTurn();
      const needProv = needsProvenance(state);

      // Capability checks for oracle apply
      capRequire(this.profile.caps, "eval", "oracle.apply.op");
      capRequire(this.profile.caps, "apply", "oracle.apply.op");
      capRequire(this.profile.caps, "observe", "oracle.apply.op");

      const proc = opcall.args[0]; // OracleProc
      const argVec = opcall.args[1];
      if (argVec.tag !== "Vector") throw new Error("oracle.apply.op expects (OracleProc, Vector)");

      // Snapshot current state
      const envRef = this.snapshots.putEnv({ env: state.env, store: state.store });
      const stateRef = this.snapshots.putState({ state });

      // Create portal
      const portal = new PortalImpl(this as any, this.snapshots, this.receipts, {
        maxEvalSteps: 100_000,
        parseText: (src: string) => compileTextToExpr(src),
      });

      // Start apply session
      const session = this.oracle.startSession({
        tag: "Apply",
        proc,
        args: argVec.items,
        envRef,
        stateRef,
      });

      // Drive session
      const meaning = await runOracleSession(session, portal);

      let enrichedMeaning = meaning as MeaningVal;
      if (needProv) {
        const payload: Val = {
          tag: "Vector",
          items: [((proc as any).spec ?? ({ tag: "Unit" } as Val)) as Val, argVec as Val],
        };
        enrichedMeaning = await attachOracleEvidence(enrichedMeaning, payload, state);
      }

      const v = enrichedMeaning.denotation ?? ({ tag: "Unit" } as Val);

      // Optional adoption
      const adoptEnv = portal.consumeAdoptEnvRef() ?? meaning.adoptEnvRef;
      let st2 = state;
      if (adoptEnv) {
        const snap = this.snapshots.getEnv(adoptEnv);
        st2 = { ...st2, env: snap.env, store: snap.store };
      }

      return opcall.resumption.invoke(v);
    }

    // Patch Set D: commit.op with truth regime enforcement
    if (opcall.op === "commit.op") {
      requireCommitCapability(this.profile.caps, "commit.method", "commit");

      const kind = opcall.args[0]?.tag === "Str" ? (opcall.args[0] as any).s : "unknown";
      const payload = opcall.args[1] ?? opcall.args[0] ?? { tag: "Unit" };

      if (this.profile.truth === "speculative") {
        throw new Error(`commit rejected in speculative truth regime (kind=${kind})`);
      }

      if (this.profile.truth === "test-certified" || this.profile.truth === "proof-certified") {
        if (isMeaning(payload)) {
          if (!obligationSatisfied(payload)) {
            throw new Error("commit requires satisfied obligations under non-speculative regimes");
          }
        }
      }

      const res = await this.commit.commit(payload, opcall.ctxDigest);
      return opcall.resumption.invoke(res);
    }

    // Patch Set 5: commit-tested.op - commit with test barrier
    // Args: (kind: Str, payload: Val, tests: Vec<Closure>) or (payload, tests)
    // Runs each test thunk; all must return truthy for commit to proceed
    if (opcall.op === "commit-tested.op") {
      requireCommitCapability(this.profile.caps, "commit.method", "commit-tested");
      capRequire(this.profile.caps, "test", "commit-tested");

      // Parse arguments: (kind payload tests) or (payload tests)
      let kind = "tested";
      let payload: Val;
      let tests: Val[];

      if (opcall.args.length >= 3 && opcall.args[0]?.tag === "Str") {
        kind = (opcall.args[0] as any).s;
        payload = opcall.args[1];
        tests = this.extractTestThunks(opcall.args[2]);
      } else if (opcall.args.length >= 2) {
        payload = opcall.args[0];
        tests = this.extractTestThunks(opcall.args[1]);
      } else {
        throw new Error("commit-tested.op: expected (kind payload tests) or (payload tests)");
      }

      // Run each test thunk - they must be closures returning truthy values
      const envRef = this.snapshots.putEnv({ env: state.env, store: state.store });
      const portal = new PortalImpl(this as any, this.snapshots, this.receipts, {
        maxEvalSteps: 100_000,
        parseText: (src: string) => compileTextToExpr(src),
        caps: this.profile.caps,
      });

      // Evaluate each test
      for (let i = 0; i < tests.length; i++) {
        const testThunk = tests[i];
        if (testThunk.tag !== "Closure") {
          throw new Error(`commit-tested.op: test ${i} is not a closure`);
        }

        // Evaluate test thunk body in its closure environment
        const { env, store } = this.snapshots.getEnv(envRef);
        const testState: State = {
          control: { tag: "Expr", e: testThunk.body },
          env: testThunk.env,
          store,
          kont: [],
          handlers: [],
        };

        try {
          const { value } = await runToCompletionWithState(this as any, testState, 100_000);
          const passed = value.tag === "Bool" ? value.b : (value.tag !== "Unit");
          if (!passed) {
            throw new Error(`commit-tested.op: test ${i} failed (returned ${value.tag})`);
          }
        } catch (e: any) {
          throw new Error(`commit-tested.op: test ${i} threw: ${e?.message ?? e}`);
        }
      }

      // All tests passed, now check truth regime
      if (this.profile.truth === "speculative") {
        throw new Error(`commit rejected in speculative truth regime (kind=${kind})`);
      }

      // Mark obligation as satisfied since tests passed
      const result = await this.commit.commit(payload, opcall.ctxDigest);
      return opcall.resumption.invoke(result);
    }

    // Patch Set 6: commit/rewrite.op - commit a Meaning with obligation discharge
    // Args: (meaning: MeaningVal) where meaning has .rewrite and .obligations
    // Discharges obligations, records evidence, then commits if all pass
    if (opcall.op === "commit/rewrite.op") {
      requireCommitCapability(this.profile.caps, "commit.rewrite", "commit/rewrite");

      const meaningArg = opcall.args[0];
      if (!meaningArg || !isMeaning(meaningArg)) {
        throw new Error("commit/rewrite.op: expected Meaning value");
      }

      const meaning = meaningArg as MeaningVal;
      const obligations = meaning.obligations ?? [];
      const evidence: Evidence[] = [];

      // Set up portal for running tests
      const envRef = this.snapshots.putEnv({ env: state.env, store: state.store });
      const portal = new PortalImpl(this as any, this.snapshots, this.receipts, {
        maxEvalSteps: 100_000,
        parseText: (src: string) => compileTextToExpr(src),
        caps: this.profile.caps,
      });

      // Discharge each obligation
      for (const obl of obligations) {
        if (obl.tag === "OblTests") {
          // Run test expressions, all must return truthy
          let passed = 0;
          const total = obl.tests.length;

          for (const testExpr of obl.tests) {
            const { env, store } = this.snapshots.getEnv(obl.envRef ?? envRef);
            const testState: State = {
              control: { tag: "Expr", e: testExpr },
              env,
              store,
              kont: [],
              handlers: [],
            };

            try {
              const { value } = await runToCompletionWithState(this as any, testState, 100_000);
              const ok = value.tag === "Bool" ? value.b : (value.tag !== "Unit");
              if (ok) passed++;
              else throw new Error(`OblTests: test failed`);
            } catch (e: any) {
              throw new Error(`commit/rewrite.op: OblTests failed: ${e?.message ?? e}`);
            }
          }

          evidence.push({ tag: "TestEvidence", passed, total });
        }

        if (obl.tag === "OblNoMatch") {
          const targets: Val[] = [];
          const unwrap = (v: Val): unknown => {
            if ((v as any)?.tag === "Syntax" && "stx" in (v as any)) return (v as any).stx;
            return v;
          };
          if (obl.scope === "all") {
            if (meaning.rewrite) targets.push(meaning.rewrite);
            if (meaning.residual) targets.push(meaning.residual);
          } else if (meaning.rewrite) {
            targets.push(meaning.rewrite);
          }

          for (const target of targets) {
            const { ok } = matchAST(obl.pattern, unwrap(target));
            if (ok) {
              throw new Error(`commit/rewrite.op: OblNoMatch failed - pattern found in ${obl.scope}`);
            }
          }

          evidence.push({
            tag: "NoMatchEvidence",
            pattern: obl.pattern,
            searched: targets.length,
            found: 0,
          });
        }

        if (obl.tag === "OblEqExt") {
          // Run extensional equivalence tests: original(input) === candidate(input) for all tests
          let allPassed = true;
          const failures: Array<{ input: Val; expected: Val; got: Val }> = [];

          for (const testExpr of obl.tests) {
            const { env, store } = this.snapshots.getEnv(obl.envRef ?? envRef);

            // Evaluate original with test input
            const origState: State = {
              control: { tag: "Expr", e: { tag: "App", fn: obl.original, args: [testExpr] } },
              env,
              store,
              kont: [],
              handlers: [],
            };
            const { value: expected } = await runToCompletionWithState(this as any, origState, 100_000);

            // Evaluate candidate with same input
            const candState: State = {
              control: { tag: "Expr", e: { tag: "App", fn: obl.candidate, args: [testExpr] } },
              env,
              store,
              kont: [],
              handlers: [],
            };
            const { value: got } = await runToCompletionWithState(this as any, candState, 100_000);

            // Compare (simple structural equality for now)
            const eq = JSON.stringify(expected) === JSON.stringify(got);
            if (!eq) {
              allPassed = false;
              failures.push({ input: { tag: "Syntax", stx: testExpr } as any, expected, got });
            }
          }

          if (!allPassed) {
            throw new Error(`commit/rewrite.op: OblEqExt failed - ${failures.length} test(s) differ`);
          }
          evidence.push({ tag: "EqExtEvidence", tests: obl.tests.length, allPassed, failures: failures.length > 0 ? failures : undefined });
        }
      }

      // All obligations discharged - check truth regime
      if (this.profile.truth === "speculative") {
        throw new Error(`commit/rewrite rejected in speculative truth regime`);
      }

      // Create result Meaning with evidence
      const resultMeaning: MeaningVal = {
        ...meaning,
        evidence,
      };

      // Commit the rewrite (the actual binding update)
      const payload = meaning.rewrite ?? meaning.residual ?? meaning.denotation ?? { tag: "Unit" };
      const result = await this.commit.commit(payload as Val, opcall.ctxDigest);

      // Return the Meaning with evidence attached
      return opcall.resumption.invoke(resultMeaning as Val);
    }

    // AMB: amb.choose - pick first thunk, save rest as pending alternatives
    if (opcall.op === "amb.choose") {
      const thunks = opcall.args[0];
      if (!thunks || thunks.tag !== "Vector") {
        throw new Error("amb.choose: expected vector of thunks");
      }

      // Convert cons-cell list to flat array
      function consToArray(v: Val): Val[] {
        const result: Val[] = [];
        let cur = v;
        while (cur.tag === "Vector" && cur.items.length === 2) {
          result.push(cur.items[0]);
          cur = cur.items[1];
        }
        // Handle flat vectors too
        if (cur.tag === "Vector" && cur.items.length > 2) {
          return cur.items;
        }
        return result;
      }

      const items = consToArray(thunks);
      if (items.length === 0) {
        // No alternatives - immediately fail
        return this.dispatch(state, {
          op: "amb.fail",
          args: [{ tag: "Str", s: "amb: no alternatives" } as Val],
          resumption: opcall.resumption,
          ctxDigest: opcall.ctxDigest,
        });
      }

      // Save remaining alternatives (based on strategy)
      // DFS: push to stack (most recent first when we pop)
      // FAIR: add to queue (oldest first when we shift)
      for (let i = 1; i < items.length; i++) {
        const alt: AmbAlternative = {
          thunk: items[i],
          resumption: opcall.resumption,
        };
        if (this.ambStrategy === "DFS") {
          this.ambAlternatives.push(alt);
        } else {
          // FAIR: add to end of queue
          this.ambAlternatives.push(alt);
        }
      }

      // Debit budget for amb attempt
      this.budget?.consumeAmbAttempt?.();

      // Invoke first thunk: apply it with no arguments
      const firstThunk = items[0];
      if (firstThunk.tag !== "Closure") {
        throw new Error("amb.choose: thunk must be a closure");
      }

      // Build a state that evaluates the thunk with resumption's frames
      // The thunk is a zero-arg closure - inline its body evaluation
      const baseState: State | "Uncaught" | "OutOfBudget" =
        opcall.resumption.invoke({ tag: "Unit" } as Val);
      if (baseState === ("Uncaught" as any) || baseState === ("OutOfBudget" as any)) {
        return baseState;
      }

      // Inline the closure: evaluate body with closure's env
      // The kont from baseState has the continuation waiting for the result
      return {
        ...baseState,
        control: { tag: "Expr", e: firstThunk.body },
        env: firstThunk.env,
      } as State;
    }

    // AMB: amb.fail - backtrack to next pending alternative
    if (opcall.op === "amb.fail") {
      if (this.ambAlternatives.length === 0) {
        // No more alternatives - propagate failure
        throw new Error("amb: all alternatives exhausted");
      }

      // Debit budget for backtrack
      this.budget?.consumeAmbAttempt?.();

      // Pick next alternative based on strategy
      // Both DFS and FAIR try alternatives in order (first added = first tried)
      // The difference is in how fairness interleaves with continuation
      let alt: AmbAlternative;
      alt = this.ambAlternatives.shift()!;  // FIFO: try in order added

      // Resume with this alternative's thunk
      const thunk = alt.thunk;
      const resumption = alt.resumption;

      // Same as amb.choose: we need to evaluate the thunk and resume with result
      const baseState: State | "Uncaught" | "OutOfBudget" =
        resumption.invoke({ tag: "Unit" } as Val);
      if (baseState === ("Uncaught" as any) || baseState === ("OutOfBudget" as any)) {
        return baseState;
      }

      // We need to actually call the thunk. Let's inline it properly.
      // The thunk is a Closure. We want to evaluate its body with its env.
      if (thunk.tag === "Closure") {
        return {
          ...baseState,
          control: { tag: "Expr", e: thunk.body },
          env: thunk.env,
        } as State;
      } else if (thunk.tag === "Native") {
        // Native thunk - call it with empty args
        // This is tricky... for now assume thunks are Closures
        throw new Error("amb: Native thunks not yet supported");
      }

      throw new Error(`amb: invalid thunk type ${thunk.tag}`);
    }

    // amb.* passthrough: if not handled by a language handler or a dedicated nondet runner, it is uncaught.

    // shell.op: Execute a shell command and return the output
    // Args: (command: Str) -> Str (stdout) or throws on error
    if (opcall.op === "shell.op") {
      capRequire(this.profile.caps, "shell", "shell.op");

      const cmdArg = opcall.args[0];
      if (!cmdArg || cmdArg.tag !== "Str") {
        throw new Error("shell.op: expected string command");
      }

      const command = cmdArg.s;

      // Execute command using child_process
      const { execSync } = await import("child_process");
      try {
        const stdout = execSync(command, {
          encoding: "utf-8",
          maxBuffer: 10 * 1024 * 1024, // 10MB
          timeout: 300000, // 5 minutes
        });
        return opcall.resumption.invoke({ tag: "Str", s: stdout.trim() } as Val);
      } catch (e: any) {
        // Return error as a structured value rather than throwing
        const stderr = e.stderr?.toString?.() ?? "";
        const message = e.message ?? "command failed";
        throw new Error(`shell.op failed: ${message}\n${stderr}`);
      }
    }

    // file.read.op: Read a file and return its contents
    // Args: (path: Str) -> Str
    if (opcall.op === "file.read.op") {
      capRequire(this.profile.caps, "file.read", "file.read.op");

      const pathArg = opcall.args[0];
      if (!pathArg || pathArg.tag !== "Str") {
        throw new Error("file.read.op: expected string path");
      }

      const { readFileSync } = await import("fs");
      try {
        const content = readFileSync(pathArg.s, "utf-8");
        return opcall.resumption.invoke({ tag: "Str", s: content } as Val);
      } catch (e: any) {
        throw new Error(`file.read.op failed: ${e.message}`);
      }
    }

    // file.write.op: Write content to a file
    // Args: (path: Str, content: Str) -> Unit
    if (opcall.op === "file.write.op") {
      capRequire(this.profile.caps, "file.write", "file.write.op");

      const pathArg = opcall.args[0];
      const contentArg = opcall.args[1];
      if (!pathArg || pathArg.tag !== "Str") {
        throw new Error("file.write.op: expected string path as first arg");
      }
      if (!contentArg || contentArg.tag !== "Str") {
        throw new Error("file.write.op: expected string content as second arg");
      }

      const { writeFileSync } = await import("fs");
      try {
        writeFileSync(pathArg.s, contentArg.s, "utf-8");
        return opcall.resumption.invoke({ tag: "Unit" } as Val);
      } catch (e: any) {
        throw new Error(`file.write.op failed: ${e.message}`);
      }
    }

    return "Uncaught";
  }
}
```

### FILE: src/core/effects/opcall.ts
```typescript
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-4.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import type { Val } from "../eval/values";
import type { State } from "../eval/machine";

export type Resumption = {
  readonly rid: string;
  readonly base: State;             // store MUST be snapshotted/persistent
  invoke(v: Val): State;            // inject v as effect result
  digest(): string;
};

export type OpCall = {
  readonly op: string;
  readonly args: Val[];
  readonly ctxDigest: string;
  readonly resumption: Resumption;
};```

## 5. REPL (Key Sections Only)

### FILE: bin/omega-repl.ts (imports and structure)
```typescript
#!/usr/bin/env npx tsx
// bin/omega-repl.ts
// Interactive Omega REPL with dual-REPL oracle support
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-IMPLEMENTATION-20.md
//
// Run:  npx tsx bin/omega-repl.ts
//       npx tsx bin/omega-repl.ts --verbose   (show oracle transcript)

import * as readline from "readline";
import * as fs from "fs";
import * as path from "path";

// Load .env file if it exists
const envPath = path.join(process.cwd(), ".env");
if (fs.existsSync(envPath)) {
  const envContent = fs.readFileSync(envPath, "utf8");
  for (const line of envContent.split("\n")) {
    const match = line.match(/^([^=]+)=(.*)$/);
    if (match && !process.env[match[1]]) {
      process.env[match[1]] = match[2].trim();
    }
  }
}
import { COWStore, type Store } from "../src/core/eval/store";
import { RuntimeImpl } from "../src/core/effects/runtimeImpl";
import { SnapshotRepo } from "../src/core/oracle/snapshots";
import { InMemoryReceiptStore } from "../src/core/oracle/receipts";
import { installPrims } from "../test/helpers/prims";
import { createOpenAIAdapter, createAnthropicAdapter } from "../src/core/oracle/adapters";
import { DepthTrackingAdapter } from "../src/core/oracle/adapters/types";
import type { OracleAdapter } from "../src/core/oracle/adapter";
import type { State, Frame } from "../src/core/eval/machine";
import { runToCompletionWithState } from "../src/core/eval/run";
import { stepOnce } from "../src/core/eval/machineStep";
import type { StepOutcome } from "../src/core/eval/machine";
import { compileTextToExpr } from "../src/core/pipeline/compileText";
import type { Val } from "../src/core/eval/values";
import { VUnit } from "../src/core/eval/values";
import type { Env } from "../src/core/eval/env";
import { ScriptedOracleAdapter } from "../src/core/oracle/scriptedOracle";
import { SessionWriter, SessionReader, JumpController, renderTrace } from "../src/core/session";
import type { SessionIndex } from "../src/core/session";
import { buildNativeRegistry } from "../src/core/session/nativeRegistry";
import { buildSolverRegistry } from "../src/core/session/solverRegistry";

// OPR imports
import { OprRuntime } from "../src/core/opr/runtime";
import { InMemoryReceiptStore as OprReceiptStore } from "../src/core/opr/receipts";
import { OpenAIOprAdapter } from "../src/core/opr/adapters/openai";
import { AnthropicOprAdapter } from "../src/core/opr/adapters/anthropic";
import { listKernels, getKernel } from "../src/core/opr/kernels";

// ─────────────────────────────────────────────────────────────────
// LLM Integration
// ─────────────────────────────────────────────────────────────────
function loadApiKey(): string | undefined {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  if (process.env.ANTHROPIC_API_KEY) return process.env.ANTHROPIC_API_KEY;
  try {
    const configPath = path.join(process.cwd(), "../LambdaRLM/config.yaml");
    const content = fs.readFileSync(configPath, "utf8");
    const match = content.match(/api_key:\s*(\S+)/);
    if (match?.[1]) return match[1];
  } catch { /* ignore */ }
  return undefined;
}

async function askLLM(prompt: string, apiKey: string): Promise<string> {
  // Simple OpenAI API call (legacy, non-agentic)
  const response = await fetch("https://api.openai.com/v1/chat/completions", {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      "Authorization": `Bearer ${apiKey}`,
    },
    body: JSON.stringify({
      model: "gpt-4o-mini",
      messages: [{ role: "user", content: prompt }],
      max_tokens: 500,
    }),
  });
  const data = await response.json() as any;
  return data.choices?.[0]?.message?.content || "";
}

// ─────────────────────────────────────────────────────────────────
// LLM Adapter Interface (pluggable LLM support)
// ─────────────────────────────────────────────────────────────────

interface LLMToolCall {
  id: string;
  name: string;
  args: Record<string, any>;
}

interface LLMResponse {
  content?: string;
  toolCalls?: LLMToolCall[];
  finishReason?: string;
}

// ... (2700+ lines of command handling) ...

// === OPR COMMANDS (lines ~2678-2800) ===
  // OPR Commands
  if (trimmed === ":opr-list") {
    log("\nAvailable OPR Kernels:");
    log("======================");
    for (const id of listKernels()) {
      const kernel = getKernel(id);
      if (kernel) {
        log(`  ${id} (op: ${kernel.op})`);
      }
    }
    log("\nUse :opr-run <kernel-id> <program-json> to execute a kernel.");
    return { replState, output: output.join("\n"), shouldExit };
  }

  if (trimmed.startsWith(":opr-run ")) {
    const args = trimmed.slice(9).trim();
    const spaceIdx = args.indexOf(" ");
    if (spaceIdx === -1) {
      log("Usage: :opr-run <kernel-id> <program-json>");
      log("Example: :opr-run opr.classify.v1 {\"item\":\"error at line 42\",\"categories\":[\"bug\",\"feature\"]}");
      return { replState, output: output.join("\n"), shouldExit };
    }

    const kernelId = args.slice(0, spaceIdx);
    const programJson = args.slice(spaceIdx + 1).trim();

    const kernel = getKernel(kernelId);
    if (!kernel) {
      log(`Unknown kernel: ${kernelId}`);
      log("Use :opr-list to see available kernels.");
      return { replState, output: output.join("\n"), shouldExit };
    }

    let program: unknown;
    try {
      program = JSON.parse(programJson);
    } catch (e: any) {
      log(`Invalid JSON: ${e.message}`);
      return { replState, output: output.join("\n"), shouldExit };
    }

    // Get API key
    const openaiKey = process.env.OPENAI_API_KEY;
    const anthropicKey = process.env.ANTHROPIC_API_KEY;

    if (!openaiKey && !anthropicKey) {
      log("No API key found. Set OPENAI_API_KEY or ANTHROPIC_API_KEY.");
      return { replState, output: output.join("\n"), shouldExit };
    }

    // Create adapter
    const adapter = openaiKey
      ? new OpenAIOprAdapter({ apiKey: openaiKey, model: "gpt-4o-mini" })
      : new AnthropicOprAdapter({ apiKey: anthropicKey!, model: "claude-sonnet-4-20250514" });

    const receipts = new OprReceiptStore();
    const runtime = new OprRuntime({
      kernel,
      adapter,
      receipts,
      budget: { maxAttempts: 3 },
    });

    log(`\nRunning kernel: ${kernelId}`);
    log(`Program: ${JSON.stringify(program, null, 2)}`);
    log("\nCalling LLM...\n");

    try {
      const result = await runtime.step({ program, state: null });

      if (result.tag === "ok") {
        log("SUCCESS!");
        log(`\nResult: ${JSON.stringify(result.output.result, null, 2)}`);
        if (result.output.next_state) {
          log(`\nNext State: ${JSON.stringify(result.output.next_state, null, 2)}`);
        }
        if (result.output.effects && result.output.effects.length > 0) {
          log(`\nEffects: ${JSON.stringify(result.output.effects, null, 2)}`);
        }
        log(`\nAttempts: ${result.attempts}`);
      } else {
        log(`FAILED: ${result.tag}`);
        if ("error" in result) {
          log(`Error: ${(result.error as Error).message}`);
        }
      }

      // Store receipts in session
      (replState as any).oprReceipts = [
        ...((replState as any).oprReceipts || []),
        ...result.receipts,
      ];
      log(`\nReceipts: ${result.receipts.length} generated (use :opr-receipts to view)`);
    } catch (e: any) {
      log(`Error running kernel: ${e.message}`);
    }

    return { replState, output: output.join("\n"), shouldExit };
  }

  if (trimmed === ":opr-receipts") {
    const oprReceipts = (replState as any).oprReceipts;
    if (!oprReceipts || oprReceipts.length === 0) {
      log("No OPR receipts in current session.");
    } else {
      log("\nOPR Receipt Chain:");
      log("==================");
      for (let i = 0; i < oprReceipts.length; i++) {
        const r = oprReceipts[i];
        const statusIcon = r.status === 'OK' ? '[OK]' : '[X]';
        log(`${i + 1}. ${statusIcon} ${r.status} - ${r.kernel_id}:${r.op}`);
        log(`     Attempt: ${r.attempt}  Created: ${r.created_at}`);
        if (r.errors && r.errors.length > 0) {
          log(`     Errors: ${r.errors[0]}`);
        }
      }
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  if (trimmed === ":opr-verify" || trimmed.startsWith(":opr-verify ")) {
    const arg = trimmed.slice(12).trim();
    let receipts: any[];

    if (arg) {
      // Load from file
      try {
        const content = fs.readFileSync(arg, 'utf-8');
        receipts = JSON.parse(content);
      } catch (e: any) {
        log(`Error loading receipt file: ${e.message}`);
        return { replState, output: output.join("\n"), shouldExit };
      }
    } else {
      receipts = (replState as any).oprReceipts || [];
      if (receipts.length === 0) {
        log("No OPR receipts in session. Provide a file path to verify.");
        return { replState, output: output.join("\n"), shouldExit };
      }
    }

    // Simple chain verification
    let valid = true;
    let brokenAt = -1;
    let error = "";

    if (receipts.length > 0 && receipts[0].prev_receipt_hash !== null) {
      valid = false;
      brokenAt = 0;
      error = "First receipt should have null prev_receipt_hash";
    } else {
      for (let i = 1; i < receipts.length; i++) {
        if (receipts[i].prev_receipt_hash !== receipts[i - 1].receipt_hash) {
          valid = false;
          brokenAt = i;
          error = "Chain link broken";
          break;
        }
      }
    }

    if (valid) {
      log(`\nReceipt chain is VALID`);
      log(`  ${receipts.length} receipts verified`);
      if (receipts.length > 0) {
        log(`  Chain hash: ${receipts[receipts.length - 1].receipt_hash}`);
      }
    } else {
      log(`\nReceipt chain is BROKEN`);
      log(`  Broken at index: ${brokenAt}`);
      log(`  Error: ${error}`);
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  // Skip other commands starting with :
  if (trimmed.startsWith(":")) {
    log(`Unknown command: ${trimmed}. Type :help for commands.`);
    return { replState, output: output.join("\n"), shouldExit };
  }
```

## 6. FRAMEIR (Prompt IR)


### FILE: src/frameir/prompt.ts
```typescript
import type { NodeBase } from "./meta";
import type { ValueIR, VRef } from "./value";

// Role-based segments
export interface PSystem extends NodeBase { tag: "PSystem"; text: string }
export interface PUser extends NodeBase { tag: "PUser"; text: string }
export interface PAssistant extends NodeBase { tag: "PAssistant"; text: string }

// Few-shot examples
export interface PFewShot extends NodeBase {
  tag: "PFewShot";
  examples: Array<{ user: string; assistant: string }>;
}

// Data embedding
export interface PData extends NodeBase { tag: "PData"; value: ValueIR }

// Structure wrappers
export interface PXml extends NodeBase { tag: "PXml"; tagName: string; inner: PromptIR }
export interface PCodeBlock extends NodeBase { tag: "PCodeBlock"; lang: string; code: string }
export interface PNumbered extends NodeBase { tag: "PNumbered"; items: PromptIR[] }

// Attachments (for tools/schemas)
export interface PAttachTools extends NodeBase {
  tag: "PAttachTools";
  tools: VRef[];
  inner: PromptIR;
}

export interface PAttachSchema extends NodeBase {
  tag: "PAttachSchema";
  schema: VRef;
  inner: PromptIR;
}

export interface PAttachFormat extends NodeBase {
  tag: "PAttachFormat";
  format: { kind: "json" | "xml" | "text"; details?: ValueIR };
  inner: PromptIR;
}

// Transforms
export interface PTransform extends NodeBase {
  tag: "PTransform";
  transform: string;
  inner: PromptIR;
}

export type PromptPart =
  | PSystem | PUser | PAssistant
  | PFewShot
  | PData
  | PXml | PCodeBlock | PNumbered
  | PAttachTools | PAttachSchema | PAttachFormat
  | PTransform;

// Top-level prompt document
export interface PromptDoc extends NodeBase {
  tag: "PromptDoc";
  parts: PromptPart[];
}

export type PromptIR = PromptDoc;
```

### FILE: src/frameir/meta.ts
```typescript
import type { IRVersion } from "./version";

export interface Span {
  file?: string;
  startLine?: number;
  startCol?: number;
  endLine?: number;
  endCol?: number;
}

export interface Meta {
  span?: Span;
  doc?: string;
  attrs?: Record<string, unknown>;
}

export interface NodeBase {
  v: IRVersion;
  tag: string;
  meta?: Meta;
}
```

### FILE: src/frameir/version.ts
```typescript
export type IRVersion = "frameir@1";

export const CURRENT_IR_VERSION: IRVersion = "frameir@1";
```

---

## 7. ARCHITECTURE SUMMARY

### What Works

1. **Lisp Interpreter (CEKS Machine)**
   - `src/core/eval/` - Full interpreter with continuations
   - Values, environments, store, machine step

2. **Oracle System**
   - `src/core/oracle/` - Multi-turn LLM conversations
   - OpenAI/Anthropic/MCP adapters
   - Portal for effect handling

3. **OPR System (NEW - Not Integrated)**
   - `src/core/opr/` - Structured LLM output validation
   - 10 kernel definitions (classify, extract, review, etc.)
   - Receipt chain for audit
   - Retry with counterexample feedback
   - **PROBLEM: Completely separate from Oracle system**

4. **REPL**
   - Full interactive environment
   - Debug, breakpoints, snapshots
   - Session management
   - `:opr-run`, `:opr-list` commands

### Architecture Problem

```
┌─────────────────────────────────────────────────────────┐
│                    REPL                                  │
├─────────────────────────────────────────────────────────┤
│  Lisp Eval ──────► Oracle System ◄──── (effect ...)    │
│     │                   │                               │
│     │              LLM Adapters                         │
│     │                                                   │
│     └──────────────── ??? ─────────────────────────────┤
│                                                         │
│  :opr-run ──────► OPR System (DISCONNECTED!)           │
│                       │                                 │
│                  OPR Adapters (DUPLICATE!)              │
└─────────────────────────────────────────────────────────┘
```

OPR should be:
- An effect handler in the Oracle system
- Callable via `(effect infer.opr ...)`
- Using the same adapters as Oracle
- Reentrant (kernels can call other kernels)

### Files Not Included (but exist)

- `src/core/compiler/` - Full compiler pipeline (209K)
- `src/core/concurrency/` - Actors, scheduling (101K)
- `src/core/macro/` - Macro expansion (85K)
- `src/core/constraints/` - Constraint solving (73K)
- `src/core/solver/` - SAT/SMT (49K)
- `src/core/session/` - Session persistence (77K)
- Many more...

### Total Codebase

- 243 TypeScript files
- ~52,000 lines of code
- 1.9MB in src/core/ alone
