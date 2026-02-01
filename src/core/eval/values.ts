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
/**
 * PromiseVal supports two implementation strategies:
 *
 * 1. Inline memoization (used by primitives): thunk/forced/value fields
 *    - Direct evaluation and caching in the value itself
 *    - Simple, efficient for basic lazy evaluation
 *
 * 2. Store-based (used by stream subsystem): id field references external PromiseStore
 *    - Enables logging, events, receipts
 *    - More sophisticated tracking for debugging/provenance
 */
export type PromiseVal = {
  tag: "Promise";
  /** Inline: The thunk (Closure or Native) to evaluate when forced */
  thunk?: Val;
  /** Inline: Whether the promise has been forced */
  forced?: boolean;
  /** Inline: The cached value after forcing */
  value?: Val;
  /** Store-based: Promise ID for external PromiseStore lookup */
  id?: PromiseId;
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
  | FactStoreVal    // Job 008: Monotone fact store
  | EvaluatorVal    // Language building: Custom evaluator
  | MacroTransformerVal; // Language building: Macro transformer

/**
 * EvaluatorVal: Custom evaluator with extended primitives.
 * Enables: make-evaluator, eval-in, extend-evaluator
 */
export type EvaluatorVal = {
  tag: "Evaluator";
  base?: Val;
  extensions: Val;
  primitives: Map<string, Val>;
};

/**
 * MacroTransformerVal: Macro transformer procedure.
 * Enables: make-transformer, register-macro
 */
export type MacroTransformerVal = {
  tag: "MacroTransformer";
  proc: Val;
  name: string;
};

export const VUnit: Val = { tag: "Unit" };
export const VTrue: Val = { tag: "Bool", b: true };
export const VFalse: Val = { tag: "Bool", b: false };
