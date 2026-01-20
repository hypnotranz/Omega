# REFERENCE-LIBRARIES.md

# Unified Library Reference

This document catalogs ALL functions from the four interconnected projects:
1. **FrameLisp** - The LLM prompt/execution algebra specification
2. **OmegaLLM** - The Lisp implementation runtime
3. **LambdaLLM** - The language specification and standard library
4. **LambdaRLM** - The reasoning/solver library

Each entry includes: signature, parameters, return type, description, and source reference.

---

# 1. FrameLisp (Specification Only)

**Source**: [docs/REFERENCE-ALGEBRA.md](../docs/REFERENCE-ALGEBRA.md)

FrameLisp is a specification for an LLM-oriented prompt and execution algebra. It defines how to compose LLM calls, prompts, and execution flows.

## 1.1 Data Primitives

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `nil` | `nil` | `Nil` | The nil value representing absence or empty. Used as base case for recursion and empty sequences. |
| `unit` | `unit` | `Unit` | The unit value representing "nothing meaningful". Used when an operation completes but produces no value. |
| `true` | `true` | `Bool` | Boolean true literal. Used in predicates and conditional logic. |
| `false` | `false` | `Bool` | Boolean false literal. Used in predicates and conditional logic. |
| `int` | `42`, `-7` | `Int` | Integer literals. Used for counts, indices, and numeric operations. |
| `float` | `3.14`, `-0.5` | `Float` | Floating-point literals. Used for scores, probabilities, and continuous values. |
| `str` | `"hello"` | `Str` | String literals. Core data type for prompts and LLM interactions. |
| `symbol` | `'foo` | `Sym` | Symbol literals. Used for identifiers and named references. |
| `keyword` | `:key` | `Kw` | Keyword literals. Self-evaluating symbols for option keys and tagging. |
| `list` | `(list a b c)` | `[Val]` | Construct a list from values. Fundamental sequence type for collections. |
| `record` | `{:key val ...}` | `Record` | Construct a record (map) from key-value pairs. Used for structured data. |

## 1.2 Kernel Operations

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `infer` | `(infer prompt [options])` | `Str` | Core LLM inference primitive. Sends prompt to LLM and returns generated text. The fundamental operation for all LLM interactions. |
| `call-tool` | `(call-tool name args)` | `Val` | Invoke a registered tool by name with arguments. Enables LLM to call external functions and APIs. |
| `validate` | `(validate spec val)` | `Val \| Err` | Validate a value against a specification/schema. Returns the value if valid, error otherwise. |
| `commit` | `(commit store key val)` | `Unit` | Persist a key-value pair to a store. Used for state management and caching results. |
| `emit` | `(emit sink item)` | `Unit` | Emit an item to a sink (stream/channel). Used for incremental output and logging. |
| `observe` | `(observe source)` | `Val` | Observe/read from an external source. Used for polling and event handling. |
| `suspend` | `(suspend reason)` | `Suspended` | Suspend execution with a reason. Used for yielding control and human-in-the-loop. |
| `fail` | `(fail reason ctx)` | `Never` | Signal a failure with reason and context. Used for structured error handling. |
| `check` | `(check pred)` | `Unit \| Fail` | Assert a predicate, fail if false. Used for precondition validation. |

## 1.3 Prompt Algebra

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `prompt+` | `(prompt+ p1 p2 ...)` | `Prompt` | Concatenate prompts in sequence. The fundamental composition for building complex prompts from parts. |
| `as-data` | `(as-data val)` | `Prompt` | Convert any value to its prompt representation. Serializes structured data for LLM consumption. |
| `defprompt` | `(defprompt name template)` | `PromptDef` | Define a named prompt template. Enables reusable prompt patterns with parameter substitution. |
| `@template` | `(@template args)` | `Prompt` | Instantiate a defined template with arguments. Fills in template parameters. |
| `system-prompt` | `(system-prompt text)` | `Prompt` | Create a system prompt section. Sets the LLM's behavioral context. |
| `user-prompt` | `(user-prompt text)` | `Prompt` | Create a user prompt section. Represents user input in chat format. |
| `assistant-prompt` | `(assistant-prompt text)` | `Prompt` | Create an assistant prompt section. Represents prior assistant output. |
| `few-shot` | `(few-shot examples)` | `Prompt` | Create few-shot examples section. Provides in-context learning examples. |
| `with-format` | `(with-format spec prompt)` | `Prompt` | Attach output format specification. Instructs LLM on expected response structure. |
| `with-schema` | `(with-schema schema prompt)` | `Prompt` | Attach JSON schema constraint. Enables structured output parsing. |
| `with-tools` | `(with-tools tools prompt)` | `Prompt` | Attach available tools to prompt. Enables tool-use in chat completion. |
| `transform` | `(transform f prompt)` | `Prompt` | Transform prompt through a function. Pre/post-processing hook. |
| `compose-transformers` | `(compose-transformers t1 t2 ...)` | `Transformer` | Compose multiple transformers. Builds transformation pipelines. |
| `chain-of-thought` | `(chain-of-thought prompt)` | `Prompt` | Wrap prompt with CoT instruction. Encourages step-by-step reasoning. |
| `xml-tags` | `(xml-tags tag content)` | `Prompt` | Wrap content in XML tags. Structured markup for parsing. |
| `numbered-list` | `(numbered-list items)` | `Prompt` | Format items as numbered list. Structured enumeration. |
| `code-block` | `(code-block lang code)` | `Prompt` | Format code with language fence. Syntax-highlighted code blocks. |

## 1.4 Execution Algebra

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `bind` | `(bind m f)` | `Flow[B]` | Monadic bind - chain flows. Core composition: run m, pass result to f, run f's result. |
| `pure` | `(pure val)` | `Flow[A]` | Lift value into flow. Wraps a value in a trivial flow that just returns it. |
| `fail` | `(fail reason)` | `Flow[Never]` | Fail the flow. Signals failure that propagates unless handled. |
| `catch` | `(catch m handler)` | `Flow[A]` | Handle failures in flow. Provides error recovery mechanism. |
| `retry-until` | `(retry-until pred max-attempts flow)` | `Flow[A]` | Retry flow until predicate succeeds. Implements self-repair loops. |
| `with-timeout` | `(with-timeout ms flow)` | `Flow[A]` | Add timeout to flow. Bounds execution time. |
| `with-budget` | `(with-budget budget flow)` | `Flow[A]` | Add resource budget to flow. Bounds LLM calls, tokens, time. |
| `all` | `(all flows)` | `Flow[[A]]` | Run all flows, collect results. Parallel fan-out with join. |
| `race` | `(race flows)` | `Flow[A]` | Run flows, return first success. Parallel competition. |
| `any` | `(any flows)` | `Flow[A]` | Run flows, return first non-failure. Fallback pattern. |
| `sequence` | `(sequence flows)` | `Flow[[A]]` | Run flows in order, collect results. Sequential pipeline. |
| `branch` | `(branch pred then-flow else-flow)` | `Flow[A]` | Conditional branching. Choose flow based on predicate. |
| `loop` | `(loop init f until)` | `Flow[A]` | Iterate with state until condition. Stateful iteration. |
| `fold-flows` | `(fold-flows f init flows)` | `Flow[A]` | Fold over flows with accumulator. Reduce pattern for flows. |
| `map-flow` | `(map-flow f flow)` | `Flow[B]` | Transform flow result. Functor map for flows. |
| `filter-flow` | `(filter-flow pred flow)` | `Flow[A]` | Filter flow result by predicate. Conditional pass-through. |
| `stream` | `(stream src)` | `Stream[A]` | Create stream from source. Lazy sequence for incremental processing. |
| `stream-map` | `(stream-map f stream)` | `Stream[B]` | Map over stream lazily. Transform each element. |
| `stream-filter` | `(stream-filter p stream)` | `Stream[A]` | Filter stream by predicate. Select matching elements. |
| `stream-take` | `(stream-take n stream)` | `Stream[A]` | Take first n elements. Limit stream length. |
| `stream-flatmap` | `(stream-flatmap f stream)` | `Stream[B]` | Flatmap over stream. Expand each element to stream. |

## 1.5 Protocols

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `chat-turn` | `(chat-turn messages [tools])` | `Message` | Single chat turn with optional tools. Returns assistant message. Core chat interaction. |
| `tool-loop` | `(tool-loop messages tools max-iterations)` | `[Message]` | Run tool-calling loop until done. Implements agentic tool use. |
| `rag` | `(rag query retriever k)` | `[Doc]` | Retrieve k documents for query. Retrieval-augmented generation support. |
| `complete` | `(complete prompt [options])` | `Str` | High-level completion wrapper. Convenience over raw infer. |
| `structured-output` | `(structured-output prompt schema)` | `Val` | Get structured output matching schema. JSON-mode completion. |
| `multi-agent` | `(multi-agent agents task)` | `Val` | Coordinate multiple agents on task. Multi-agent orchestration. |
| `expert-session` | `(expert-session persona messages)` | `Flow[Messages]` | Create expert persona session. Role-playing interaction. |
| `debate` | `(debate proposition agents rounds)` | `[Message]` | Multi-agent debate format. Adversarial reasoning. |

## 1.6 Abstractions

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `defflow` | `(defflow name params body)` | `FlowDef` | Define a named flow. Reusable flow templates. |
| `defagent` | `(defagent name config)` | `AgentDef` | Define an agent with configuration. Agent blueprints. |
| `deftool` | `(deftool name schema handler)` | `ToolDef` | Define a tool with schema and handler. Tool registration. |
| `defvalidator` | `(defvalidator name spec)` | `ValidatorDef` | Define a validator. Reusable validation rules. |
| `defpipeline` | `(defpipeline name stages)` | `PipelineDef` | Define a multi-stage pipeline. Complex flow composition. |
| `defprotocol` | `(defprotocol name methods)` | `ProtocolDef` | Define a protocol interface. Polymorphic dispatch. |
| `extend-protocol` | `(extend-protocol proto type impl)` | `Unit` | Extend protocol for a type. Implementation registration. |

---

# 2. OmegaLLM (TypeScript Implementation)

**Source**: [src/core/](../src/core/)

OmegaLLM is the TypeScript runtime implementing a Lisp evaluator with effects, streams, constraints, and LLM integration.

## 2.1 Stream Library

**Source**: [src/core/stream/stream.ts](../src/core/stream/stream.ts)

SICP-style lazy streams with promise-based delayed evaluation.

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `emptyStream` | `() => Val` | `EmptyStream` | Create the empty stream. Base case for stream recursion. Returns the stream-null sentinel value. |
| `isStreamNull` | `(v: Val) => boolean` | `boolean` | Check if value is empty stream. Predicate for stream termination. |
| `isStreamEmpty` | `(v: Val) => boolean` | `boolean` | Alias for isStreamNull. Convenience predicate. |
| `consStream` | `(ctx, head, tailThunk, label?) => Val` | `StreamCell` | Create stream cons cell. Head is eager, tail is lazy thunk wrapped in promise. |
| `consStreamWithPromise` | `(head, tailPromise) => Val` | `StreamCell` | Create stream cons with existing promise. For pre-constructed promises. |
| `consStreamWithReceipt` | `(head, receiptRef) => Val` | `StreamCell` | Create stream cons with receipt ref. For receipt-backed streams. |
| `streamCar` | `(s: Val) => Val` | `Val` | Get head of stream. Throws on empty. |
| `streamCdrPromise` | `(s: Val) => Val` | `Promise` | Get tail promise without forcing. Access without evaluation. |
| `streamCdr` | `(ctx, s, evaluator?) => Val` | `Val` | Force tail and return. Evaluates the delayed tail thunk. |
| `streamMap` | `(ctx, s, f, evaluator?) => Val` | `Stream` | Map function over stream lazily. Transforms each element. |
| `streamFilter` | `(ctx, s, p, evaluator?) => Val` | `Stream` | Filter stream by predicate. Keeps matching elements. |
| `streamTake` | `(ctx, s, n, evaluator?) => Val` | `Stream` | Take first n elements. Bounds stream length. |
| `streamDrop` | `(ctx, s, n, evaluator?) => Val` | `Stream` | Drop first n elements. Skips prefix. |
| `streamAppend` | `(ctx, s1, s2Thunk, evaluator?) => Val` | `Stream` | Append two streams. Lazy concatenation. |
| `streamFlatMap` | `(ctx, s, f, evaluator?) => Val` | `Stream` | Flatmap over stream. Monadic bind for streams. |
| `streamZip` | `(ctx, s1, s2, evaluator?) => Val` | `Stream` | Zip two streams together. Pairs corresponding elements. |
| `streamRepeat` | `(ctx, value) => Val` | `Stream` | Infinite stream of constant value. Generator for constants. |
| `streamRange` | `(ctx, start, end) => Val` | `Stream` | Stream of integers in range. Bounded integer sequence. |
| `listToStream` | `(ctx, list) => Val` | `Stream` | Convert list to stream. Lifts list into lazy form. |
| `streamIterate` | `(ctx, seed, f) => Val` | `Stream` | Generate stream by iteration. Infinite sequence from function. |
| `streamToList` | `(ctx, s, maxElements?, evaluator?) => Val[]` | `Val[]` | Convert stream to list (forces). Materializes bounded prefix. |
| `forceN` | `(ctx, s, n, evaluator?) => Val[]` | `Val[]` | Force first n elements to array. Bounded materialization. |
| `forceHead` | `(ctx, s, evaluator?) => Val \| null` | `Val?` | Force just the head. Single element access. |
| `forceTail` | `(ctx, s, evaluator?) => Val \| null` | `Val?` | Force just the tail. Rest access. |
| `deepForce` | `(ctx, s, n, evaluator?) => Val[]` | `Val[]` | Deep force up to n elements. Alias for forceN. |
| `streamForEach` | `(ctx, s, f, maxElements?, evaluator?) => void` | `void` | Apply function to each element. Side-effecting iteration. |
| `streamFold` | `(ctx, s, init, f, evaluator?) => Val` | `Val` | Fold over stream from left. Accumulating reduction. |
| `streamReduce` | `(ctx, s, f, evaluator?) => Val` | `Val` | Reduce stream (fold with first as init). |
| `materializeSegment` | `(ctx, s, n, evaluator?) => StreamSegment` | `Segment` | Materialize prefix into segment. For receipt-backed staging. |
| `createStreamReceipt` | `(segment) => StreamReceipt` | `Receipt` | Create receipt for segment. Provenance tracking. |
| `hydrateFromReceipt` | `(ctx, receipt, segment, continuation) => Val` | `Stream` | Rebuild stream from receipt. Replay from checkpoint. |
| `streamLength` | `(ctx, s, evaluator?) => number` | `number` | Get length of finite stream. Full traversal. |
| `isStream` | `(v: Val) => boolean` | `boolean` | Check if value is a stream. Type predicate. |
| `streamTailRef` | `(s: Val) => Val \| null` | `Val?` | Get the ref in stream's cdr. Access promise/receipt. |

## 2.2 Nondeterminism Types

**Source**: [src/core/effects/nondet/types.ts](../src/core/effects/nondet/types.ts)

Types for nondeterministic computation (amb operator).

| Type | Definition | Description |
|------|------------|-------------|
| `NondetMode` | `"first" \| "all" \| "best" \| "sample"` | Mode for amb operator. Controls how many/which solutions to find. |
| `ChoiceVec` | `Val[]` | Vector of choices for amb. Already-evaluated values (call-by-value). |
| `ConstraintObs` | `Require \| Note` | Constraint observation during search. Tracks predicates and notes. |
| `Job` | `{jid, state, depth, score, constraints}` | A search job in the frontier. Represents a branch being explored. |
| `FrontierKind` | `"dfs" \| "bfs" \| "best" \| "beam" \| "sample"` | Type of frontier for scheduling. Controls search order. |
| `NondetPolicy` | `{mode, frontier, quantumSteps, ...}` | Full policy for nondeterministic search. Controls all search parameters. |
| `NondetResult` | `None \| One \| Many` | Result of nondeterministic computation. Zero, one, or many solutions. |

## 2.3 Constraint Propagation Engine

**Source**: [src/core/constraints/engine.ts](../src/core/constraints/engine.ts)

Sussman-style constraint propagation network.

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `runPropagation` | `(netRef, config?) => Promise<PropagationResult>` | `PropResult` | Run propagation until quiescence or contradiction. Main propagation loop. |
| `registerScriptedPropagator` | `(prop: ScriptedPropagator) => void` | `void` | Register a scripted propagator for testing. Adds custom propagator. |
| `clearScriptedPropagators` | `() => void` | `void` | Clear all scripted propagators. Reset for tests. |
| `getScriptedPropagator` | `(name: string) => ScriptedPropagator?` | `Propagator?` | Get a scripted propagator by name. Lookup for execution. |
| `hasContradiction` | `(netRef: NetRefVal) => boolean` | `boolean` | Check if network has contradiction. Detects conflicts. |
| `getContradiction` | `(netRef: NetRefVal) => ContradictionVal?` | `Contradiction?` | Get the latest contradiction. Retrieves conflict details. |
| `isQuiescent` | `(netRef: NetRefVal) => boolean` | `boolean` | Check if network is quiescent. No pending work. |
| `getPendingCount` | `(netRef: NetRefVal) => number` | `number` | Get number of pending propagators. Agenda size. |

| Type | Definition | Description |
|------|------------|-------------|
| `PropagationResult` | `{status, firings, sets, budgetExhausted, contradiction?}` | Result of propagation run. Summary of what happened. |
| `PropagatorContext` | `{inputs, inputExplanations, propagatorName, netRef}` | Context for executing a propagator. Environment for propagation. |
| `PropagatorOutput` | `values \| contradiction \| noChange \| suspend` | Output from propagator execution. What the propagator produced. |

## 2.4 Fiber Scheduler

**Source**: [src/core/concurrency/scheduler.ts](../src/core/concurrency/scheduler.ts)

Deterministic cooperative fiber scheduler for concurrency.

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `genFiberId` | `() => FiberId` | `FiberId` | Generate unique fiber ID. Sequential counter for fibers. |
| `resetSchedulerRegistry` | `() => void` | `void` | Reset scheduler registry. Clear all schedulers for testing. |
| `createScheduler` | `(policy?, name?) => SchedulerState` | `Scheduler` | Create new scheduler with policy. Initialize scheduler state. |
| `getScheduler` | `(id: string) => SchedulerState?` | `Scheduler?` | Get scheduler by ID. Lookup in registry. |
| `cloneScheduler` | `(state: SchedulerState) => SchedulerState` | `Scheduler` | Clone scheduler state. For speculation/backtracking. |
| `spawnFiber` | `(scheduler, machineState, options?) => FiberState` | `Fiber` | Spawn new fiber in scheduler. Creates and registers fiber. |
| `getFiber` | `(scheduler, id) => FiberState?` | `Fiber?` | Get fiber by ID. Lookup in scheduler. |
| `setFiberStatus` | `(scheduler, id, status, options?) => void` | `void` | Update fiber's status. Manages fiber lifecycle. |
| `blockFiber` | `(scheduler, id, reason) => void` | `void` | Mark fiber as blocked. Suspends fiber with reason. |
| `unblockFiber` | `(scheduler, id) => void` | `void` | Unblock a fiber. Makes it ready again. |
| `completeFiber` | `(scheduler, id, result) => void` | `void` | Complete fiber with result. Marks done and unblocks joiners. |
| `failFiber` | `(scheduler, id, error) => void` | `void` | Fail fiber with error. Error termination. |
| `selectNextFiber` | `(scheduler) => FiberId?` | `FiberId?` | Select next fiber based on policy. Scheduling decision. |
| `runScheduler` | `(scheduler, config) => SchedulerStatus` | `Status` | Run scheduler until completion/budget. Main execution loop. |
| `getSchedulerStatus` | `(scheduler) => SchedulerStatus` | `Status` | Get current scheduler status. Inspect state. |
| `createEventLedger` | `() => ConcurrencyEvent[]` | `Event[]` | Create event ledger. For recording events. |
| `recordEvent` | `(ledger, event) => void` | `void` | Record event to ledger. Append event. |
| `extractDecisions` | `(ledger) => number[]` | `number[]` | Extract schedule decisions from ledger. For replay. |
| `createReplayPolicy` | `(decisions) => SchedulePolicy` | `Policy` | Create replay policy from decisions. Deterministic replay. |
| `yieldFiber` | `(scheduler, fiberId) => void` | `void` | Yield current fiber. Cooperative yielding. |
| `joinFiber` | `(scheduler, waiterId, targetId) => Val?` | `Val?` | Join on another fiber. Wait for completion. |
| `getFiberResult` | `(scheduler, fiberId) => Val?` | `Val?` | Get result of completed fiber. Retrieve output. |

| Type | Definition | Description |
|------|------------|-------------|
| `SchedulePolicy` | `RoundRobin \| FairRR \| Random \| Replay` | Scheduling policy. Controls fiber selection order. |
| `SchedulerStatus` | `running \| idle \| done \| deadlock \| error` | Current scheduler state. Overall status. |
| `FiberStatus` | `ready \| running \| blocked \| done \| error` | Individual fiber state. Lifecycle stage. |
| `BlockReason` | `{tag: "join", fiberId} \| ...` | Why a fiber is blocked. Suspension cause. |

---

# 3. LambdaLLM (Language Specification)

**Source**: [ARCHITECTURE/](../../LambdaLLM/ARCHITECTURE/)

LambdaLLM is the language specification defining the Lisp dialect for LLM-integrated programming.

## 3.1 Core Module (lambdallm.core)

**Source**: [ARCHITECTURE/14-STDLIB.md](../../LambdaLLM/ARCHITECTURE/14-STDLIB.md)

Fundamental operations auto-imported into every module.

### Arithmetic

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `+` | `(+ n ...)` | `Number` | Sum of numbers. Variadic addition. |
| `-` | `(- n)` or `(- n m ...)` | `Number` | Negation or subtraction. Unary or binary minus. |
| `*` | `(* n ...)` | `Number` | Product of numbers. Variadic multiplication. |
| `/` | `(/ n m)` | `Number` | Division. Binary division. |
| `mod` | `(mod n m)` | `Number` | Modulo operation. Remainder after division. |
| `inc` | `(inc n)` | `Number` | Add 1. Increment. |
| `dec` | `(dec n)` | `Number` | Subtract 1. Decrement. |
| `abs` | `(abs n)` | `Number` | Absolute value. Non-negative magnitude. |
| `min` | `(min n ...)` | `Number` | Minimum of numbers. Smallest value. |
| `max` | `(max n ...)` | `Number` | Maximum of numbers. Largest value. |

### Comparison

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `=` | `(= a b)` | `Bool` | Value equality. Deep comparison. |
| `eq?` | `(eq? a b)` | `Bool` | Reference identity. Same object. |
| `<` | `(< a b)` | `Bool` | Less than. Ordering comparison. |
| `>` | `(> a b)` | `Bool` | Greater than. Ordering comparison. |
| `<=` | `(<= a b)` | `Bool` | Less or equal. Ordering comparison. |
| `>=` | `(>= a b)` | `Bool` | Greater or equal. Ordering comparison. |
| `zero?` | `(zero? n)` | `Bool` | Is zero? Numeric predicate. |
| `positive?` | `(positive? n)` | `Bool` | Is positive? Numeric predicate. |
| `negative?` | `(negative? n)` | `Bool` | Is negative? Numeric predicate. |

### Logic

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `not` | `(not x)` | `Bool` | Logical negation. Inverts boolean. |
| `and` | `(and x ...)` | `Bool` | Short-circuit and. Special form, evaluates left-to-right. |
| `or` | `(or x ...)` | `Bool` | Short-circuit or. Special form, evaluates left-to-right. |

### Type Predicates

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `number?` | `(number? x)` | `Bool` | Is number? Type predicate. |
| `string?` | `(string? x)` | `Bool` | Is string? Type predicate. |
| `symbol?` | `(symbol? x)` | `Bool` | Is symbol? Type predicate. |
| `keyword?` | `(keyword? x)` | `Bool` | Is keyword? Type predicate. |
| `list?` | `(list? x)` | `Bool` | Is list? Type predicate. |
| `null?` | `(null? x)` | `Bool` | Is null/nil? Type predicate. |
| `procedure?` | `(procedure? x)` | `Bool` | Is procedure? Type predicate. |
| `boolean?` | `(boolean? x)` | `Bool` | Is boolean? Type predicate. |
| `identical?` | `(identical? a b)` | `Bool` | Same object? Reference equality. |
| `equal?` | `(equal? a b)` | `Bool` | Deep equality? Structural comparison. |
| `hash` | `(hash x)` | `Number` | Hash code. For hash tables. |

## 3.2 List Module (lambdallm.list)

### Construction

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `list` | `(list x ...)` | `List` | Create list from values. Variadic constructor. |
| `cons` | `(cons h t)` | `List` | Prepend to list. Fundamental pair construction. |
| `list*` | `(list* x ... tail)` | `List` | Create list with explicit tail. Improper list support. |

### Access

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `car` | `(car l)` | `Val` | First element. Head of list. |
| `cdr` | `(cdr l)` | `List` | Rest of list. Tail after first. |
| `first` | `(first l)` | `Val` | Alias for car. Readable name. |
| `rest` | `(rest l)` | `List` | Alias for cdr. Readable name. |
| `second` | `(second l)` | `Val` | Second element. (car (cdr l)). |
| `third` | `(third l)` | `Val` | Third element. (car (cdr (cdr l))). |
| `nth` | `(nth l n)` | `Val` | Element at index. Random access. |
| `last` | `(last l)` | `Val` | Last element. End of list. |
| `butlast` | `(butlast l)` | `List` | All but last. Drop final element. |

### Query

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `length` | `(length l)` | `Number` | Number of elements. List size. |
| `empty?` | `(empty? l)` | `Bool` | Is empty? Null check. |
| `member` | `(member x l)` | `List \| #f` | Find element. Returns sublist or false. |
| `index-of` | `(index-of x l)` | `Number \| #f` | Find index. Position or false. |
| `contains?` | `(contains? l x)` | `Bool` | Element exists? Membership test. |

### Transformation

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `map` | `(map f l)` | `List` | Apply f to each. Transform elements. |
| `filter` | `(filter p l)` | `List` | Keep matching. Select by predicate. |
| `remove` | `(remove p l)` | `List` | Remove matching. Reject by predicate. |
| `reduce` | `(reduce f init l)` | `Val` | Fold left. Accumulating reduction. |
| `reduce-right` | `(reduce-right f init l)` | `Val` | Fold right. Right-associative fold. |
| `reverse` | `(reverse l)` | `List` | Reverse list. Flip order. |
| `append` | `(append l ...)` | `List` | Concatenate lists. Join sequences. |
| `flatten` | `(flatten l)` | `List` | Flatten nested lists. Single level. |
| `take` | `(take n l)` | `List` | First n elements. Prefix. |
| `drop` | `(drop n l)` | `List` | Skip n elements. Suffix. |
| `take-while` | `(take-while p l)` | `List` | While predicate holds. Conditional prefix. |
| `drop-while` | `(drop-while p l)` | `List` | Skip while predicate. Conditional skip. |
| `partition` | `(partition p l)` | `(List List)` | Split by predicate. Two groups. |
| `sort` | `(sort l cmp?)` | `List` | Sort list. Optional comparator. |
| `unique` | `(unique l)` | `List` | Remove duplicates. Distinct elements. |

### Combination

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `zip` | `(zip l1 l2)` | `List` | Pair elements. Combine by position. |
| `interleave` | `(interleave l1 l2)` | `List` | Alternate elements. Fair merge. |
| `cartesian` | `(cartesian l1 l2)` | `List` | All pairs. Cross product. |

## 3.3 String Module (lambdallm.string)

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `str` | `(str x ...)` | `String` | Concatenate to string. String building. |
| `string` | `(string c ...)` | `String` | From characters. Character assembly. |
| `format` | `(format fmt args ...)` | `String` | Format string. Printf-style formatting. |
| `string-length` | `(string-length s)` | `Number` | String length. Character count. |
| `string-empty?` | `(string-empty? s)` | `Bool` | Is empty string? Empty check. |
| `string-contains?` | `(string-contains? s sub)` | `Bool` | Contains substring? Search. |
| `string-starts-with?` | `(string-starts-with? s prefix)` | `Bool` | Starts with prefix? Prefix test. |
| `string-ends-with?` | `(string-ends-with? s suffix)` | `Bool` | Ends with suffix? Suffix test. |
| `string-index-of` | `(string-index-of s sub)` | `Number \| #f` | Find substring index. Position search. |
| `substring` | `(substring s start end?)` | `String` | Extract portion. Slice string. |
| `string-append` | `(string-append s ...)` | `String` | Concatenate strings. Join strings. |
| `string-join` | `(string-join l sep)` | `String` | Join with separator. Delimited join. |
| `string-split` | `(string-split s sep)` | `List` | Split on separator. Tokenize. |
| `string-trim` | `(string-trim s)` | `String` | Remove whitespace. Trim edges. |
| `string-upcase` | `(string-upcase s)` | `String` | Uppercase. Case conversion. |
| `string-downcase` | `(string-downcase s)` | `String` | Lowercase. Case conversion. |
| `string-replace` | `(string-replace s old new)` | `String` | Replace all occurrences. Substitution. |
| `regex-match` | `(regex-match pattern s)` | `Match \| #f` | Match regex. Pattern matching. |
| `regex-find` | `(regex-find pattern s)` | `List` | Find all matches. Global search. |
| `regex-replace` | `(regex-replace pattern s replacement)` | `String` | Replace matches. Pattern substitution. |

## 3.4 Map Module (lambdallm.map)

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `hash-map` | `(hash-map k1 v1 ...)` | `Map` | Create map. Key-value pairs. |
| `get` | `(get m k default?)` | `Val` | Get value. Lookup with optional default. |
| `assoc` | `(assoc m k v)` | `Map` | Add/update key. Immutable update. |
| `dissoc` | `(dissoc m k)` | `Map` | Remove key. Immutable removal. |
| `contains-key?` | `(contains-key? m k)` | `Bool` | Key exists? Membership test. |
| `keys` | `(keys m)` | `List` | Get all keys. Key enumeration. |
| `vals` | `(vals m)` | `List` | Get all values. Value enumeration. |
| `entries` | `(entries m)` | `List` | Get key-value pairs. Entry list. |
| `merge` | `(merge m1 m2 ...)` | `Map` | Merge maps. Combine dictionaries. |
| `select-keys` | `(select-keys m ks)` | `Map` | Subset of keys. Project columns. |
| `map-vals` | `(map-vals f m)` | `Map` | Map over values. Transform values. |
| `map-keys` | `(map-keys f m)` | `Map` | Map over keys. Transform keys. |

## 3.5 I/O Module (lambdallm.io)

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `print` | `(print x ...)` | `Unit` | Print without newline. Console output. |
| `println` | `(println x ...)` | `Unit` | Print with newline. Line output. |
| `read-line` | `(read-line)` | `String` | Read line from stdin. Console input. |
| `read-file` | `(read-file path)` | `String` | Read entire file. File input. |
| `write-file` | `(write-file path content)` | `Unit` | Write file. File output. |
| `append-file` | `(append-file path content)` | `Unit` | Append to file. File append. |
| `file-exists?` | `(file-exists? path)` | `Bool` | File exists? Existence check. |
| `list-files` | `(list-files pattern)` | `List` | Glob files. Directory listing. |
| `delete-file` | `(delete-file path)` | `Unit` | Delete file. File removal. |

## 3.6 LLM Module (lambdallm.llm)

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `complete` | `(complete prompt system?)` | `String` | Get completion. Single-turn LLM call. |
| `chat` | `(chat messages tools?)` | `Message` | Multi-turn chat. Conversation with optional tools. |
| `embed` | `(embed text)` | `Vector` | Get embedding. Text to vector. |
| `intent` | `(intent description)` | `Code` | Compile intent to code. Natural language programming. |

## 3.7 Test Module (lambdallm.test)

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `deftest` | `(deftest name body ...)` | `TestDef` | Define test. Test case definition. |
| `is` | `(is expr)` | `Unit` | Assert truthy. Basic assertion. |
| `is=` | `(is= expected actual)` | `Unit` | Assert equal. Equality assertion. |
| `is-not` | `(is-not expr)` | `Unit` | Assert falsy. Negation assertion. |
| `throws?` | `(throws? type body)` | `Bool` | Assert throws. Exception assertion. |
| `run-tests` | `(run-tests ns?)` | `Report` | Run all tests. Test execution. |

## 3.8 Debug Module (lambdallm.debug)

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `trace` | `(trace expr)` | `Val` | Print and return. Debug output. |
| `spy` | `(spy label expr)` | `Val` | Labeled trace. Named debug output. |
| `time` | `(time expr)` | `Val` | Measure time. Performance profiling. |
| `break` | `(break)` | `Unit` | Enter debugger. Breakpoint. |
| `apropos` | `(apropos pattern)` | `List` | Find matching symbols. Symbol search. |
| `doc` | `(doc symbol)` | `String` | Show documentation. Help lookup. |
| `source` | `(source symbol)` | `String` | Show source. Code inspection. |
| `macroexpand` | `(macroexpand form)` | `Form` | Expand macros. Macro debugging. |

---

# 4. LambdaRLM (Reasoning/Solver Library)

**Source**: [lib/](../../LambdaRLM/lib/)

LambdaRLM provides composable solvers, search strategies, and reasoning patterns for LLM-based problem solving.

## 4.1 Composable Solvers

**Source**: [lib/composable.lisp](../../LambdaRLM/lib/composable.lisp)

Base interface for composable solvers that can be nested, combined, and orchestrated.

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `make-solver` | `(make-solver name solve-fn capabilities)` | `Solver` | Create a solver record. Basic constructor with name, function, and capability list. Uses default estimator. |
| `make-solver-with-estimator` | `(make-solver-with-estimator name solve-fn capabilities estimate-fn)` | `Solver` | Create solver with custom estimator. Full constructor for cost-aware solving. |
| `solver?` | `(solver? obj)` | `Bool` | Type predicate. Returns true if obj is a solver record. |
| `solver-name` | `(solver-name s)` | `Symbol` | Get solver name. Identifier accessor. |
| `solver-solve-fn` | `(solver-solve-fn s)` | `Procedure` | Get solve function. Raw function accessor. |
| `solver-capabilities` | `(solver-capabilities s)` | `List` | Get capabilities list. What the solver can do. |
| `solver-estimate-fn` | `(solver-estimate-fn s)` | `Procedure` | Get estimate function. Cost estimator accessor. |
| `solver-solve` | `(solver-solve solver problem context)` | `Result` | Invoke solver. Main entry point for running a solver on a problem. |
| `solver-estimate` | `(solver-estimate solver problem)` | `Estimate` | Estimate cost without solving. For budget allocation. |
| `estimate?` | `(estimate? obj)` | `Bool` | Type predicate for estimates. Check if obj is an estimate record. |
| `estimate-llm-calls` | `(estimate-llm-calls e)` | `Number` | Get estimated LLM calls. Resource extraction. |
| `estimate-tokens` | `(estimate-tokens e)` | `Number` | Get estimated tokens. Resource extraction. |
| `estimate-time-ms` | `(estimate-time-ms e)` | `Number` | Get estimated time in ms. Resource extraction. |
| `compose-sequential` | `(compose-sequential s1 s2)` | `Solver` | Chain two solvers sequentially. Output of s1 feeds to s2. Multi-stage processing. |
| `compose-parallel` | `(compose-parallel solvers merger)` | `Solver` | Run solvers in parallel and merge. Independent subproblems that need combining. |
| `compose-fallback` | `(compose-fallback primary fallbacks)` | `Solver` | Try primary, fall back on failure. Robust solving with backup strategies. |

## 4.2 Search Strategies

**Source**: [lib/strategies.lisp](../../LambdaRLM/lib/strategies.lisp)

Toolkit of search strategies for solvers to choose dynamically based on problem characteristics.

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `make-strategy` | `(make-strategy name execute-fn estimate-fn)` | `Strategy` | Create strategy record. Basic constructor for search strategies. |
| `strategy?` | `(strategy? obj)` | `Bool` | Type predicate. Check if obj is a strategy. |
| `strategy-name` | `(strategy-name s)` | `Symbol` | Get strategy name. Identifier accessor. |
| `strategy-execute` | `(strategy-execute strategy generator evaluator)` | `Result` | Execute strategy. Run search with given generator and evaluator. |
| `strategy-estimate-cost` | `(strategy-estimate-cost strategy problem)` | `Estimate` | Estimate strategy cost. For budget planning. |
| `strategy-beam-search` | `(strategy-beam-search k scorer)` | `Strategy` | Beam search strategy. Keeps top-k candidates at each level. Good for high branching factor. |
| `strategy-depth-first` | `(strategy-depth-first max-depth)` | `Strategy` | Depth-first search. Explores deeply with depth limit. Good for deep nesting. |
| `strategy-breadth-first` | `(strategy-breadth-first max-width)` | `Strategy` | Breadth-first search. Explores widely with width limit. Good for shallow exhaustive search. |
| `strategy-mcts` | `(strategy-mcts rollouts)` | `Strategy` | Monte Carlo tree search. Random rollouts for exploration/exploitation. Good for large branching. |
| `strategy-greedy` | `(strategy-greedy scorer)` | `Strategy` | Greedy search. Always picks best, no backtrack. Good for clear gradient. |
| `strategy-adaptive` | `(strategy-adaptive initial-strategy)` | `Strategy` | Adaptive search. Starts with initial, switches if failing. Good for unknown structure. |
| `meta-select-strategy` | `(meta-select-strategy problem)` | `Strategy` | LLM-guided strategy selection. Picks strategy based on problem features. |
| `analyze-problem-features` | `(analyze-problem-features problem)` | `Alist` | Extract problem features. For strategy selection heuristics. |
| `solve-with-strategy` | `(solve-with-strategy problem context strategy)` | `Result` | Helper for solvers. Standard integration of strategies. |

## 4.3 Streams Library

**Source**: [lib/streams.lisp](../../LambdaRLM/lib/streams.lisp)

SICP-style lazy streams for nondeterministic search.

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `empty-stream` | `empty-stream` | `Stream` | The empty stream constant. Base case for stream recursion. |
| `stream-null?` | `(stream-null? s)` | `Bool` | Check if stream is empty. Termination predicate. |
| `stream-cons` | `(stream-cons head tail-thunk)` | `Stream` | Create stream cell. Head eager, tail lazy. |
| `stream-car` | `(stream-car s)` | `Val` | Get stream head. First element. |
| `stream-cdr` | `(stream-cdr s)` | `Stream` | Force and get tail. Rest of stream. |
| `stream-map` | `(stream-map f s)` | `Stream` | Map over stream lazily. Transform elements. |
| `stream-filter` | `(stream-filter pred s)` | `Stream` | Filter stream lazily. Select matching. |
| `stream-interleave` | `(stream-interleave s1 s2)` | `Stream` | Fair merge of two streams. Prevents DFS starvation. Critical for fair enumeration. |
| `stream-interleave-lazy` | `(stream-interleave-lazy s1 s2-thunk)` | `Stream` | Fair merge with lazy second. Takes thunk for s2. |
| `stream-append` | `(stream-append s1 s2-thunk)` | `Stream` | Append streams. Second is thunk for laziness. |
| `stream-flatmap` | `(stream-flatmap f s)` | `Stream` | Stream monad bind (DFS order). Basic flatmap. |
| `stream-flatmap-fair` | `(stream-flatmap-fair f s)` | `Stream` | Fair flatmap with interleaving. Prevents starvation. |
| `stream-take` | `(stream-take n s)` | `List` | Take first n elements. Forces evaluation. |
| `stream-take-while` | `(stream-take-while pred s)` | `List` | Take while predicate. Conditional take. |
| `list->stream` | `(list->stream xs)` | `Stream` | Convert list to stream. Lift to lazy. |
| `stream->list` | `(stream->list s)` | `List` | Convert stream to list. Forces all - use with finite streams only. |
| `integers-from` | `(integers-from n)` | `Stream` | Infinite integer stream. Starting from n. |

## 4.4 Budget Management

**Source**: [lib/budget.lisp](../../LambdaRLM/lib/budget.lisp)

Hierarchical resource management for parent-child solver coordination.

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `make-budget` | `(make-budget llm-calls tokens time-ms)` | `Budget` | Create budget record. Specifies resource limits. |
| `budget?` | `(budget? obj)` | `Bool` | Type predicate. Check if obj is budget. |
| `budget-llm-calls` | `(budget-llm-calls b)` | `Number` | Get LLM call budget. Resource accessor. |
| `budget-tokens` | `(budget-tokens b)` | `Number` | Get token budget. Resource accessor. |
| `budget-time-ms` | `(budget-time-ms b)` | `Number` | Get time budget in ms. Resource accessor. |
| `budget-remaining` | `(budget-remaining b)` | `List` | Get all remaining resources. Full status. |
| `budget-consume` | `(budget-consume b resource-type amount)` | `Budget` | Deduct from budget. Returns new budget (immutable). |
| `budget-exhausted?` | `(budget-exhausted? b)` | `Bool` | Check if any resource depleted. Exhaustion test. |
| `with-budget` | `(with-budget b thunk)` | `Val` | Run thunk if budget available. Guard execution. |
| `budget-split` | `(budget-split b n)` | `List` | Split into n equal child budgets. Parallel distribution. |
| `budget-allocate` | `(budget-allocate b percentage)` | `Budget` | Allocate percentage of budget. Fractional allocation. |

## 4.5 Context Management

**Source**: [lib/context.lisp](../../LambdaRLM/lib/context.lisp)

Context records for parent-child solver coordination.

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `make-context` | `(make-context budget constraints parent-id)` | `Context` | Create root context. Initial context with budget and constraints. |
| `context?` | `(context? obj)` | `Bool` | Type predicate. Check if obj is context. |
| `context-budget` | `(context-budget ctx)` | `Budget` | Extract budget from context. Resource accessor. |
| `context-constraints` | `(context-constraints ctx)` | `Alist` | Get constraints alist. All constraints. |
| `context-parent-id` | `(context-parent-id ctx)` | `Symbol` | Get parent solver ID. Origin accessor. |
| `context-path` | `(context-path ctx)` | `List` | Get full ancestry path. Debug/trace support. |
| `context-constraint` | `(context-constraint ctx key)` | `Val \| #f` | Lookup specific constraint. Single constraint access. |
| `context-with-budget` | `(context-with-budget ctx new-budget)` | `Context` | Create context with new budget. Budget update. |
| `context-child` | `(context-child parent-ctx child-id budget-fraction)` | `Context` | Create child context. Allocates budget fraction, inherits constraints, extends path. |

## 4.6 Result Types

**Source**: [lib/results.lisp](../../LambdaRLM/lib/results.lisp)

Structured result types for solver outcomes.

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `success` | `(success value metadata)` | `Result` | Create success result. Full success with value and metadata. |
| `partial` | `(partial value progress metadata)` | `Result` | Create partial result. Incomplete with progress indicator (0.0-1.0). |
| `failure` | `(failure reason context)` | `Result` | Create failure result. Structured failure with machine-readable reason. |
| `success?` | `(success? r)` | `Bool` | Check if success. Type predicate. |
| `partial?` | `(partial? r)` | `Bool` | Check if partial. Type predicate. |
| `failure?` | `(failure? r)` | `Bool` | Check if failure. Type predicate. |
| `result?` | `(result? r)` | `Bool` | Check if any result variant. General type predicate. |
| `result-value` | `(result-value r)` | `Val` | Extract value from success/partial. Value accessor. |
| `result-metadata` | `(result-metadata r)` | `Alist` | Extract metadata from any result. Metadata accessor. |
| `result-progress` | `(result-progress r)` | `Number` | Extract progress from partial. Progress accessor. |
| `failure-reason` | `(failure-reason r)` | `Symbol` | Extract reason from failure. Error reason accessor. |
| `result-map` | `(result-map f r)` | `Result` | Transform value in result. Functor map - preserves result type. |
| `result-bind` | `(result-bind f r)` | `Result` | Chain results monadically. Bind - f receives value, returns new result. |

## 4.7 Meta-Search

**Source**: [lib/meta_search.lisp](../../LambdaRLM/lib/meta_search.lisp)

Meta-level search where LLM picks strategies.

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `meta-analyze` | `(meta-analyze problem)` | `Alist` | Extract problem features. Returns branching-factor, depth, constraints, size, domain-hints. |
| `meta-select-strategy` | `(meta-select-strategy features toolkit)` | `Strategy` | Select strategy based on features. Uses heuristics to match problem to strategy. |
| `meta-generate-plan` | `(meta-generate-plan problem strategy)` | `DomainTerm` | Generate Domain Algebra plan. Creates execution plan based on problem and strategy. |
| `meta-execute` | `(meta-execute solver problem context)` | `Result` | Execute with monitoring. Runs solver with progress tracking. |
| `meta-adapt` | `(meta-adapt result history)` | `Strategy \| #f` | Decide on adaptation. Returns new strategy or #f if should give up. |
| `meta-solve` | `(meta-solve problem context)` | `Result` | Full meta-search loop. Analyze, select, plan, execute, adapt until success or failure. |
| `get-strategy-toolkit` | `(get-strategy-toolkit)` | `List` | Get default strategy toolkit. Available strategies for selection. |
| `get-default-algebra` | `(get-default-algebra)` | `Algebra` | Get default domain algebra. For plan compilation. |
| `make-success` | `(make-success value metadata)` | `Result` | Create success result. Convenience constructor. |
| `make-partial` | `(make-partial value progress metadata)` | `Result` | Create partial result. Convenience constructor. |
| `make-failure` | `(make-failure reason context)` | `Result` | Create failure result. Convenience constructor. |
| `failure-recoverable?` | `(failure-recoverable? result)` | `Bool` | Check if failure allows retry. Determines if adaptation possible. |

## 4.8 Repair Loop

**Source**: [lib/repair_loop.lisp](../../LambdaRLM/lib/repair_loop.lisp)

Fixpoint repair loop for validation and auto-correction.

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `manifest-files` | `(manifest-files m)` | `List` | Get files from manifest. Extract file list. |
| `manifest-update` | `(manifest-update m path new-content)` | `Manifest` | Update file content in manifest. Immutable update. |
| `validate-file-entry` | `(validate-file-entry f types-code)` | `(path errors content) \| #f` | Validate single file. Returns failure info or #f if valid. |
| `validate-manifest` | `(validate-manifest m types-code)` | `List` | Validate all files. Returns list of failures. |
| `repair-prompt` | `(repair-prompt path errs bad-content)` | `String` | Generate repair prompt. Constructs LLM prompt for fixing file. |
| `repair-failures` | `(repair-failures m failures max-tokens max-workers)` | `Manifest` | Repair failed files in parallel. Uses LLM batch completion. |
| `repair-loop` | `(repair-loop initial-manifest types-code max-iters)` | `(ok manifest) \| (error reason iters)` | Main repair loop. Iterates validate-repair until convergence. |
| `stage-validated-manifest` | `(stage-validated-manifest m)` | `Bool` | Write validated files. Only stages if validation passed. |

## 4.9 Nondeterminism (amb)

**Source**: [lib/nondet.lisp](../../LambdaRLM/lib/nondet.lisp)

SICP 4.3-style nondeterministic search using stream-based backtracking.

### Monad Primitives

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `unit` | `(unit x)` | `Stream` | Lift value into nondeterminism monad. Returns singleton stream containing x. |
| `mzero` | `mzero` | `Stream` | Empty stream (failure). Represents no solutions. |
| `mplus` | `(mplus s1 s2)` | `Stream` | Merge two streams. Combines solution sets. Uses interleave for fairness. |
| `bind` | `(bind m f)` | `Stream` | Monadic bind. Applies f to each element of m, merges results. |

### Choice Operators

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `choose` | `(choose xs)` | `Val` | Nondeterministically choose from list. Returns any element of xs. |
| `guard` | `(guard pred)` | `Unit` | Guard clause. Fails if pred is false, succeeds otherwise. |
| `require` | `(require pred)` | `Unit` | Alias for guard. Assert constraint. |
| `amb-list` | `(amb-list xs)` | `Stream` | Convert list to stream of choices. List to stream lift. |
| `amb-1` | `(amb-1 a)` | `Val` | Choose from 1 option. Trivial choice. |
| `amb-2` | `(amb-2 a b)` | `Val` | Choose from 2 options. Binary choice. |
| `amb-3` | `(amb-3 a b c)` | `Val` | Choose from 3 options. Ternary choice. |
| `amb-4` | `(amb-4 a b c d)` | `Val` | Choose from 4 options. Quaternary choice. |
| `amb-5` | `(amb-5 a b c d e)` | `Val` | Choose from 5 options. Quinary choice. |

### Control Flow

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `amb-fail` | `(amb-fail)` | `Never` | Explicit failure. Triggers backtracking. |
| `amb-require` | `(amb-require pred)` | `Unit` | Require predicate. Fail if false. |
| `let-amb` | `(let-amb ((x choices) ...) body)` | `Stream` | Macro: bind multiple amb choices. Like let but nondeterministic. |

### Execution

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `run-nondet` | `(run-nondet thunk)` | `Stream` | Run nondeterministic computation. Returns stream of all solutions. |
| `run-all` | `(run-all thunk)` | `List` | Run and collect all solutions. Forces entire stream to list. |
| `run-first-matching` | `(run-first-matching thunk pred)` | `Val \| #f` | Find first solution matching pred. Short-circuits. |
| `amb-collect` | `(amb-collect n thunk)` | `List` | Collect up to n solutions. Bounded collection. |
| `amb-one` | `(amb-one thunk)` | `Val \| #f` | Get first solution or #f. Single result. |
| `amb-n` | `(amb-n n thunk)` | `List` | Alias for amb-collect. |

### Utilities

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `beam-select` | `(beam-select k scorer stream)` | `List` | Select top-k by score. Beam search support. |
| `take` | `(take n s)` | `List` | Take first n from stream. Bounded forcing. |

## 4.10 Domain Algebra

**Source**: [lib/domain_algebra.lisp](../../LambdaRLM/lib/domain_algebra.lisp)

Algebraic IR for symbolic reasoning and term rewriting.

### Sort Operations

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `define-sort` | `(define-sort name)` | `Sort` | Define a new sort (type). Creates named sort. |
| `sort?` | `(sort? x)` | `Bool` | Type predicate for sorts. |
| `sort-name` | `(sort-name s)` | `Symbol` | Get sort name. |

### Operation Definition

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `define-operation` | `(define-operation name signature)` | `Operation` | Define an operation with signature. Signature is list of input sorts + output sort. |
| `operation?` | `(operation? x)` | `Bool` | Type predicate for operations. |
| `operation-name` | `(operation-name op)` | `Symbol` | Get operation name. |
| `operation-signature` | `(operation-signature op)` | `List` | Get operation signature. |
| `operation-arity` | `(operation-arity op)` | `Number` | Get operation arity. Number of inputs. |

### Equation Definition

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `define-equation` | `(define-equation lhs rhs)` | `Equation` | Define rewrite rule. lhs ⟶ rhs. |
| `equation?` | `(equation? x)` | `Bool` | Type predicate for equations. |
| `equation-lhs` | `(equation-lhs eq)` | `Term` | Get left-hand side. |
| `equation-rhs` | `(equation-rhs eq)` | `Term` | Get right-hand side. |

### Algebra Construction

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `domain-algebra` | `(domain-algebra sorts ops equations)` | `Algebra` | Create algebra from components. Bundle sorts, operations, equations. |
| `domain-algebra?` | `(domain-algebra? x)` | `Bool` | Type predicate for algebras. |
| `domain-algebra-sorts` | `(domain-algebra-sorts a)` | `List` | Get all sorts in algebra. |
| `domain-algebra-operations` | `(domain-algebra-operations a)` | `List` | Get all operations in algebra. |
| `domain-algebra-equations` | `(domain-algebra-equations a)` | `List` | Get all equations in algebra. |

### Algebra Operations

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `algebra-find-operation` | `(algebra-find-operation a name)` | `Operation \| #f` | Find operation by name. Lookup in algebra. |
| `algebra-valid?` | `(algebra-valid? a)` | `Bool` | Check algebra well-formedness. Validates all signatures. |
| `equation-match` | `(equation-match pattern term)` | `Substitution \| #f` | Pattern match term against pattern. Returns bindings or #f. |
| `substitute` | `(substitute term subst)` | `Term` | Apply substitution to term. Variable replacement. |
| `algebra-simplify` | `(algebra-simplify a term)` | `Term` | Apply equations until fixpoint. Term rewriting. |
| `algebra-generators` | `(algebra-generators a)` | `List` | Get generator operations. Operations with no inputs. |

## 4.11 Provenance

**Source**: [lib/provenance.lisp](../../LambdaRLM/lib/provenance.lisp)

Evidence validation and staleness detection for audit trails.

### Predicates

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `tagged?` | `(tagged? x tag)` | `Bool` | Check if x is list with first element = tag. Structure predicate. |
| `nonempty-string?` | `(nonempty-string? s)` | `Bool` | Check if s is non-empty string. |
| `span?` | `(span? s)` | `Bool` | Check if s is valid span (start-line, end-line). |

### Evidence Structure

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `evidence?` | `(evidence? ev)` | `Bool` | Check if ev is evidence record. Type predicate. |
| `evidence-field` | `(evidence-field ev k)` | `Val \| #f` | Get field from evidence by key. Alist accessor. |
| `evidence-id` | `(evidence-id ev)` | `String \| #f` | Get evidence ID. Format: "ev:sha256:...". |

### Validation

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `validate-evidence` | `(validate-evidence ev)` | `List` | Validate evidence structure. Returns list of errors (empty if valid). |
| `evidence-stale?` | `(evidence-stale? ev)` | `Bool` | Check if evidence is stale. Compares fingerprint to current file. |

### Epistemic Modes

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `valid-epistemic-mode?` | `(valid-epistemic-mode? mode)` | `Bool` | Check if mode is valid. Valid: observed, measured, derived, inferred, hypothesized, assumed. |
| `valid-code-claim-mode?` | `(valid-code-claim-mode? mode)` | `Bool` | Check if mode valid for code claims. Code claims must be: observed, measured, derived. |

## 4.12 Failure Handling

**Source**: [lib/failure.lisp](../../LambdaRLM/lib/failure.lisp)

Structured failure handling for solvers.

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `make-failure` | `(make-failure type msg data)` | `Failure` | Create failure record. Type is keyword, msg is string, data is alist. |
| `failure?` | `(failure? x)` | `Bool` | Type predicate. Check if x is failure. |
| `failure-type` | `(failure-type f)` | `Keyword` | Get failure type. E.g., :budget-exceeded, :validation-failed. |
| `failure-message` | `(failure-message f)` | `String` | Get failure message. Human-readable description. |
| `failure-data` | `(failure-data f)` | `Alist` | Get failure data. Structured metadata. |
| `failure-recoverable?` | `(failure-recoverable? f)` | `Bool` | Check if failure allows retry. Based on failure type. |
| `escalate-failure` | `(escalate-failure f parent-ctx)` | `Failure` | Propagate failure up solver tree. Adds context. |
| `try-with-fallback` | `(try-with-fallback thunk fallback)` | `Val` | Try thunk, run fallback on failure. Error recovery. |

## 4.13 Scoring

**Source**: [lib/scoring.lisp](../../LambdaRLM/lib/scoring.lisp)

Scoring and ranking utilities for search.

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `make-score` | `(make-score value confidence source)` | `Score` | Create score record. Value is number, confidence 0-1, source is symbol. |
| `score?` | `(score? x)` | `Bool` | Type predicate for scores. |
| `score-value` | `(score-value s)` | `Number` | Get score value. |
| `score-confidence` | `(score-confidence s)` | `Number` | Get confidence (0-1). |
| `score-source` | `(score-source s)` | `Symbol` | Get score source. E.g., 'llm, 'heuristic, 'exact. |
| `combine-scores` | `(combine-scores scores combiner)` | `Score` | Combine multiple scores. Combiner: :sum, :product, :max, :min, :weighted-avg. |
| `rank-by-score` | `(rank-by-score items scorer)` | `List` | Sort items by score descending. |
| `top-k` | `(top-k k items scorer)` | `List` | Get top k items by score. |
| `normalize-scores` | `(normalize-scores scores)` | `List` | Normalize scores to sum to 1. |

## 4.14 Obligations

**Source**: [lib/obligations.lisp](../../LambdaRLM/lib/obligations.lisp)

Obligation tracking for solver contracts.

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `make-obligation` | `(make-obligation id type description deadline)` | `Obligation` | Create obligation record. Deadline is turn count or #f. |
| `obligation?` | `(obligation? x)` | `Bool` | Type predicate. |
| `obligation-id` | `(obligation-id o)` | `Symbol` | Get obligation ID. |
| `obligation-type` | `(obligation-type o)` | `Keyword` | Get type. E.g., :must, :should, :may. |
| `obligation-description` | `(obligation-description o)` | `String` | Get description. |
| `obligation-deadline` | `(obligation-deadline o)` | `Number \| #f` | Get deadline turn. |
| `obligation-status` | `(obligation-status o)` | `Keyword` | Get status. :pending, :fulfilled, :violated, :waived. |
| `fulfill-obligation` | `(fulfill-obligation o evidence)` | `Obligation` | Mark fulfilled with evidence. |
| `violate-obligation` | `(violate-obligation o reason)` | `Obligation` | Mark violated with reason. |
| `waive-obligation` | `(waive-obligation o reason)` | `Obligation` | Waive obligation with reason. |
| `check-obligations` | `(check-obligations obligations turn)` | `List` | Check all obligations at turn. Returns list of violations. |

## 4.15 Yield

**Source**: [lib/yield.lisp](../../LambdaRLM/lib/yield.lisp)

Cooperative yielding for long-running computations.

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `yield-point` | `(yield-point label)` | `Unit` | Mark yield point. Allows scheduler to interrupt. |
| `yield-if-budget-low` | `(yield-if-budget-low budget threshold)` | `Unit` | Yield if budget below threshold. Resource-aware yielding. |
| `with-yield-handler` | `(with-yield-handler handler thunk)` | `Val` | Run thunk with custom yield handler. |
| `make-checkpoint` | `(make-checkpoint state label)` | `Checkpoint` | Create computation checkpoint. For resumption. |
| `resume-from-checkpoint` | `(resume-from-checkpoint cp)` | `Val` | Resume from checkpoint. Continues computation. |

## 4.16 Core Utilities

**Source**: [lib/core_utils_v1.lisp](../../LambdaRLM/lib/core_utils_v1.lisp)

Core utility functions for LambdaRLM.

| Function | Signature | Returns | Description |
|----------|-----------|---------|-------------|
| `alist-get` | `(alist-get key alist default)` | `Val` | Get value from alist by key. Returns default if not found. |
| `alist-set` | `(alist-set key val alist)` | `Alist` | Set key in alist. Returns new alist. |
| `alist-update` | `(alist-update key f alist)` | `Alist` | Update key by applying f. |
| `alist-remove` | `(alist-remove key alist)` | `Alist` | Remove key from alist. |
| `alist-keys` | `(alist-keys alist)` | `List` | Get all keys. |
| `alist-vals` | `(alist-vals alist)` | `List` | Get all values. |
| `member?` | `(member? x xs)` | `Bool` | Check membership in list. |
| `assoc` | `(assoc key alist)` | `Pair \| #f` | Find pair by key. |
| `cadr` | `(cadr x)` | `Val` | (car (cdr x)). |
| `cddr` | `(cddr x)` | `List` | (cdr (cdr x)). |
| `caddr` | `(caddr x)` | `Val` | (car (cdr (cdr x))). |

---

# 5. Unimplemented Specifications

This section documents features that are **SPEC ONLY** (designed but not implemented) or **NOT STARTED** (planned but not begun).

### Status Legend

| Status | Meaning |
|--------|---------|
| **IMPLEMENTED** | Code exists and works in LambdaRLM lib/ or OmegaLLM src/ |
| **SPEC ONLY** | Documented in LambdaLLM ARCHITECTURE/ but not implemented |
| **NOT STARTED** | Planned in OmegaLLM CLAUDE-JOBS/ but work not begun |

## 5.1 OmegaLLM Monadic Primitives (NOT STARTED)

**Source**: [CLAUDE-JOBS/006-MONADIC-PRIMITIVES.md](../CLAUDE-JOBS/006-MONADIC-PRIMITIVES.md)

Core monadic primitives for nondeterminism in OmegaLLM. Requires new `KBind` frame in CEKS machine.

### Primitives

| Function | Signature | Returns | Status | Description |
|----------|-----------|---------|--------|-------------|
| `unit` | `(unit x)` | `Stream` | NOT STARTED | Lift value into monad. `(cons-stream x empty-stream)` |
| `mzero` | `mzero` | `Stream` | NOT STARTED | Empty stream (failure). |
| `mplus` | `(mplus m1 m2)` | `Stream` | NOT STARTED | Merge streams with interleave. Fair combination. |
| `bind` | `(bind m f)` | `Stream` | NOT STARTED | Monadic bind. Applies f to each element, merges. |

### Derived Operators

| Function | Signature | Returns | Status | Description |
|----------|-----------|---------|--------|-------------|
| `mdo` | `(mdo (x <- m) ... body)` | `Stream` | NOT STARTED | Macro: monadic do-notation. Desugars to nested bind. |
| `guard` | `(guard pred)` | `Stream` | NOT STARTED | `(if pred (unit #t) mzero)`. Filter solutions. |
| `msum` | `(msum ms)` | `Stream` | NOT STARTED | `(foldr mplus mzero ms)`. Merge list of streams. |
| `mfilter` | `(mfilter pred m)` | `Stream` | NOT STARTED | `(bind m (lambda (x) (guard (pred x)) (unit x)))`. |
| `mconcatMap` | `(mconcatMap f xs)` | `Stream` | NOT STARTED | `(msum (map f xs))`. Map and merge. |

### Implementation Note

Requires `KBind` frame in CEKS machine ([machineStep.ts](../src/core/eval/machineStep.ts)):
```typescript
interface KBind {
  tag: 'KBind';
  continuation: Val;  // The (lambda (x) ...) to apply
  remaining: Val;     // Rest of stream to process
}
```

## 5.2 OmegaLLM Solver Package (NOT STARTED)

**Source**: [CLAUDE-JOBS/008-SEARCH-PATTERNS-SOLVERS.md](../CLAUDE-JOBS/008-SEARCH-PATTERNS-SOLVERS.md)

Composable solver infrastructure for @omega/solver package.

### Budget Management

| Function | Signature | Returns | Status | Description |
|----------|-----------|---------|--------|-------------|
| `budget-split` | `(budget-split b n)` | `List` | NOT STARTED | Split budget into n equal parts. |
| `budget-allocate` | `(budget-allocate b percentage)` | `Budget` | NOT STARTED | Allocate percentage of budget. |

### Solver Primitives

| Function | Signature | Returns | Status | Description |
|----------|-----------|---------|--------|-------------|
| `make-solver` | `(make-solver name solve-fn estimate-fn)` | `Solver` | NOT STARTED | Create solver record. |
| `solver?` | `(solver? x)` | `Bool` | NOT STARTED | Type predicate. |
| `solver-solve` | `(solver-solve s problem ctx)` | `Result` | NOT STARTED | Invoke solver. |
| `solver-estimate` | `(solver-estimate s problem)` | `Estimate` | NOT STARTED | Estimate cost. |
| `solver-name` | `(solver-name s)` | `Symbol` | NOT STARTED | Get solver name. |

### Composition Combinators

| Function | Signature | Returns | Status | Description |
|----------|-----------|---------|--------|-------------|
| `compose-sequential` | `(compose-sequential s1 s2)` | `Solver` | NOT STARTED | Chain: output of s1 → input of s2. |
| `compose-parallel` | `(compose-parallel solvers merger)` | `Solver` | NOT STARTED | Run in parallel, merge results. |
| `compose-fallback` | `(compose-fallback primary fallbacks)` | `Solver` | NOT STARTED | Try primary, then fallbacks on failure. |
| `compose-retry` | `(compose-retry s max-retries)` | `Solver` | NOT STARTED | Retry solver up to max-retries. |

### Repair Patterns

| Function | Signature | Returns | Status | Description |
|----------|-----------|---------|--------|-------------|
| `repair-until-valid` | `(repair-until-valid gen validate repair max-iters)` | `Result` | NOT STARTED | Generate, validate, repair loop. |

### Fact Store

| Function | Signature | Returns | Status | Description |
|----------|-----------|---------|--------|-------------|
| `make-fact-store` | `(make-fact-store)` | `FactStore` | NOT STARTED | Create empty fact store. |
| `assert-fact` | `(assert-fact store fact)` | `FactStore` | NOT STARTED | Add fact (monotonic). |
| `query-fact` | `(query-fact store pattern)` | `Fact \| #f` | NOT STARTED | Query single fact. |
| `query-facts` | `(query-facts store pattern)` | `List` | NOT STARTED | Query all matching facts. |

### Fixpoint

| Function | Signature | Returns | Status | Description |
|----------|-----------|---------|--------|-------------|
| `fixpoint` | `(fixpoint thunk signature-fn max-iters)` | `Result` | NOT STARTED | Run until convergence. |
| `fixpoint-detect-cycle` | `(fixpoint-detect-cycle signatures)` | `Bool` | NOT STARTED | Check for cycles in signature history. |

## 5.3 LambdaLLM Diagnostics (SPEC ONLY)

**Source**: [ARCHITECTURE/15-DIAGNOSTICS.md](../../LambdaLLM/ARCHITECTURE/15-DIAGNOSTICS.md)

Structured error codes for the runtime.

### Error Codes

| Code | Category | Description |
|------|----------|-------------|
| `E0001` | Syntax | Malformed expression |
| `E0002` | Syntax | Unbalanced parentheses |
| `E0003` | Syntax | Invalid string literal |
| `E0100` | Type | Type mismatch |
| `E0101` | Type | Undefined variable |
| `E0102` | Type | Wrong number of arguments |
| `E0200` | Runtime | Division by zero |
| `E0201` | Runtime | Index out of bounds |
| `E0202` | Runtime | Null pointer dereference |
| `E0300` | LLM | Oracle timeout |
| `E0301` | LLM | Budget exhausted |
| `E0302` | LLM | Invalid response format |
| `E0303` | LLM | Tool call failed |

### Warning Codes

| Code | Category | Description |
|------|----------|-------------|
| `W0001` | Performance | Large context warning |
| `W0002` | Performance | Deep recursion warning |
| `W0003` | Style | Unused variable |
| `W0004` | Style | Unreachable code |
| `W0005` | LLM | Low confidence response |

## 5.4 LambdaLLM Facts System (SPEC ONLY)

**Source**: [ARCHITECTURE/23-FACTS.md](../../LambdaLLM/ARCHITECTURE/23-FACTS.md)

Monotone epistemic state for grounding claims.

| Function | Signature | Returns | Status | Description |
|----------|-----------|---------|--------|-------------|
| `assert` | `(assert pattern evidence)` | `Fact` | SPEC ONLY | Assert fact with evidence. Monotonic - facts never retract. |
| `fact?` | `(fact? pattern)` | `Bool` | SPEC ONLY | Check if fact exists. |
| `facts` | `(facts pattern)` | `List` | SPEC ONLY | Query all matching facts. |
| `fact/get` | `(fact/get pattern)` | `Fact \| #f` | SPEC ONLY | Get single fact or #f. |
| `with-facts` | `(with-facts facts body)` | `Val` | SPEC ONLY | Run body with facts in scope. |

## 5.5 LambdaLLM Fixpoint System (SPEC ONLY)

**Source**: [ARCHITECTURE/24-FIXPOINT.md](../../LambdaLLM/ARCHITECTURE/24-FIXPOINT.md)

Convergence detection via state signatures.

### Signature Modes

| Mode | Description |
|------|-------------|
| `hash` | SHA256 of serialized state. Fastest, may have collisions. |
| `structural` | Deep structural comparison. Slower, exact. |
| `semantic` | LLM-judged equivalence. Expensive, for fuzzy convergence. |

### Functions

| Function | Signature | Returns | Status | Description |
|----------|-----------|---------|--------|-------------|
| `fixpoint` | `(fixpoint thunk sig-mode max-iters)` | `FixpointResult` | SPEC ONLY | Run until convergence. Returns converged value or failure. |
| `fixpoint/outcome` | `(fixpoint/outcome thunk mode iters)` | `Outcome` | SPEC ONLY | Detailed outcome with iteration history. |
| `state-signature` | `(state-signature state mode)` | `String` | SPEC ONLY | Compute state signature. |
| `signatures-equal?` | `(signatures-equal? s1 s2)` | `Bool` | SPEC ONLY | Compare signatures. |
| `cycle-detected?` | `(cycle-detected? history)` | `Bool` | SPEC ONLY | Check for oscillation in history. |

## 5.6 LambdaLLM Experts System (SPEC ONLY)

**Source**: [ARCHITECTURE/29-EXPERTS.md](../../LambdaLLM/ARCHITECTURE/29-EXPERTS.md)

Three-layer architecture for expert personas.

### Layers

| Layer | Description |
|-------|-------------|
| **Tool Contract** | Raw tool capabilities. What the tool CAN do. |
| **Role Overlay** | Expert persona. Subset of tools + behavioral constraints. |
| **Task Envelope** | Specific task. Further narrows role for one job. |

### Functions

| Function | Signature | Returns | Status | Description |
|----------|-----------|---------|--------|-------------|
| `defexpert` | `(defexpert name role tools constraints)` | `Expert` | SPEC ONLY | Define expert persona. |
| `expert-call` | `(expert-call expert task context)` | `Result` | SPEC ONLY | Invoke expert on task. |
| `intent` | `(intent description)` | `Code` | SPEC ONLY | Special form: compile intent to code. |
| `with-role` | `(with-role role body)` | `Val` | SPEC ONLY | Run body with role active. |

### Output Modes

| Mode | Description |
|------|-------------|
| `pure-code` | Expert returns only code. |
| `code+rationale` | Code with explanation. |
| `multi-turn` | Interactive refinement. |

## 5.7 LambdaLLM Artifacts (SPEC ONLY)

**Source**: [ARCHITECTURE/26-ARTIFACTS.md](../../LambdaLLM/ARCHITECTURE/26-ARTIFACTS.md)

File/patch artifacts with validation.

| Function | Signature | Returns | Status | Description |
|----------|-----------|---------|--------|-------------|
| `artifact` | `(artifact type path content)` | `Artifact` | SPEC ONLY | Create artifact. Type: :file, :patch, :blob. |
| `artifact-validate` | `(artifact-validate a schema)` | `Bool` | SPEC ONLY | Validate artifact against schema. |
| `artifact-stage` | `(artifact-stage a)` | `Unit` | SPEC ONLY | Stage artifact for commit. |
| `artifact-commit` | `(artifact-commit artifacts)` | `Commit` | SPEC ONLY | Commit all staged artifacts. |
| `artifact-diff` | `(artifact-diff a1 a2)` | `Patch` | SPEC ONLY | Compute diff between artifacts. |

## 5.8 LambdaLLM Session (SPEC ONLY)

**Source**: [ARCHITECTURE/28-SESSION.md](../../LambdaLLM/ARCHITECTURE/28-SESSION.md)

Multi-turn session management.

| Function | Signature | Returns | Status | Description |
|----------|-----------|---------|--------|-------------|
| `session-start` | `(session-start config)` | `Session` | SPEC ONLY | Start new session. |
| `session-turn` | `(session-turn s input)` | `(Session, Output)` | SPEC ONLY | Process one turn. |
| `session-checkpoint` | `(session-checkpoint s)` | `Checkpoint` | SPEC ONLY | Save session state. |
| `session-restore` | `(session-restore cp)` | `Session` | SPEC ONLY | Restore from checkpoint. |
| `session-fork` | `(session-fork s)` | `Session` | SPEC ONLY | Fork session for speculation. |
| `session-merge` | `(session-merge s1 s2)` | `Session` | SPEC ONLY | Merge forked sessions. |

## 5.9 LambdaLLM Tooling (SPEC ONLY)

**Source**: [ARCHITECTURE/16-TOOLING.md](../../LambdaLLM/ARCHITECTURE/16-TOOLING.md)

Structured tool contracts.

| Function | Signature | Returns | Status | Description |
|----------|-----------|---------|--------|-------------|
| `deftool` | `(deftool name schema handler)` | `ToolDef` | SPEC ONLY | Define tool with JSON schema. |
| `tool-call` | `(tool-call name args)` | `Result` | SPEC ONLY | Call tool with validation. |
| `tool-available?` | `(tool-available? name)` | `Bool` | SPEC ONLY | Check if tool registered. |
| `tool-schema` | `(tool-schema name)` | `Schema` | SPEC ONLY | Get tool's JSON schema. |
| `with-tools` | `(with-tools tools body)` | `Val` | SPEC ONLY | Run body with tools available. |

---

# Appendix: Cross-Project Function Mapping

This section shows how functions in different layers relate to each other.

## FrameLisp → OmegaLLM Implementation

| FrameLisp | OmegaLLM Implementation |
|-----------|------------------------|
| `(infer prompt)` | `OracleReq({ tag: "ReqTool", call: { tool: "infer.op", prompt } })` |
| `(stream-map f s)` | `streamMap(ctx, s, f, evaluator)` |
| `(stream-filter p s)` | `streamFilter(ctx, s, p, evaluator)` |
| `(amb choices)` | `NondetPolicy + Job frontier` |
| `(with-budget budget flow)` | `ConcurrencyBudget + scheduler config` |

## LambdaRLM → FrameLisp Usage

| LambdaRLM Pattern | Uses FrameLisp |
|-------------------|----------------|
| `(solver-solve solver problem ctx)` | → `(bind (infer ...) validate)` |
| `(compose-sequential s1 s2)` | → `(bind (s1) s2)` |
| `(compose-parallel solvers merger)` | → `(all (map solve solvers))` |
| `(stream-flatmap-fair f s)` | → `(stream-interleave ...)` |
| `(meta-solve problem ctx)` | → `(retry-until success? 3 (execute-plan))` |

---

# 6. Architecture & Design Guides

For comprehensive architectural context, see these design documents:

## 6.1 Layer Architecture

**Source**: [ARCHITECTURE-LANGUAGES-1.md](./ARCHITECTURE-LANGUAGES-1.md)

The system uses Clean Architecture / Hexagonal principles with strict downward dependencies:

| Layer | Component | Language | Responsibility |
|-------|-----------|----------|----------------|
| L6 | Apps/Domain | LambdaLLM | Domain toolchains, agents, vertical solutions |
| L5 | LambdaRLM | Lisp | Solvers, strategies, meta-search, repair, provenance |
| L4 | LambdaLLM | Lisp | Macros, modules, stdlib, user ergonomics |
| L3 | FrameLisp | Spec+IR | Prompt algebra, Flow monad, kernel ops, serializable IR |
| L2 | OmegaLLM | TypeScript | CEKS evaluator, scheduler, streams, constraints, tools |
| L1 | Host | Node/TS | File system, network, DBs, vector stores |

**Key rule**: Dependencies point strictly downward. LambdaRLM never depends on OmegaLLM internals.

## 6.2 Type Universe & Monad Laws

**Source**: [ARCHITECTURE-LANGUAGES-2.md](./ARCHITECTURE-LANGUAGES-2.md)

### Canonical Outcome Type

```
Outcome[A] =
  | Done   { value: A, meta: Meta }
  | Fail   { failure: Failure, meta: Meta }
  | Pause  { suspended: Suspended, meta: Meta }
```

### The Outer Effect Rule

> **Flow is always the outermost effect.** Everything else is carried as data inside Flow.

Permitted:
- `Flow[A]`, `Flow[Stream[A]]`, `Flow[Result[A]]`, `Flow[List[A]]`

Forbidden (semantic traps):
- `Stream[Flow[A]]` - effects at unpredictable times
- `Result[Flow[A]]` - value-level branching hiding effects
- `Nondet[Flow[A]]` - search should schedule, not carry flows

### Nondeterminism Alignment

`Nondet[A] ≅ Stream[A]` — nondet primitives are stream primitives with fairness policies.

## 6.3 Effect Kinds & Signature Matrix

**Source**: [ARCHITECTURE-LANGUAGES-3.md](./ARCHITECTURE-LANGUAGES-3.md)

### Effect Kind Lattice

| Kind | Description | Replay | Optimize |
|------|-------------|--------|----------|
| **Pure** | Deterministic, referentially transparent | N/A | May reorder |
| **KernelEffect** | Handled by interpreter, deterministic given ports | Yes | No reorder |
| **PortEffect** | Crosses into adapters (LLM, tools), potentially nondet | Must log | Never |
| **ControlEffect** | Changes scheduling/nondeterminism | Log decisions | Never |
| **ObservationEffect** | Reads world (observe, time, RNG) | Through ports | Never |

### Runtime Ports (Hexagonal Architecture)

| Port | Operations | Purpose |
|------|------------|---------|
| `OraclePort` | `infer(prompt, options)` | LLM inference |
| `ToolPort` | `call(name, args, contract)` | External tool invocation |
| `StorePort` | `get(key)`, `put(key, val)` | Persistent state |
| `SinkPort` | `emit(channel, item)` | Streaming output |
| `SourcePort` | `observe(sourceId)` | External events |
| `ClockPort` | Deterministic time | Replay support |
| `RngPort` | Deterministic RNG | Sampling, MCTS |
| `RetrieverPort` | `rag(query, k, filters)` | Vector retrieval |

### Primitive Signature Requirements

Every kernel primitive must declare:
- `effectKind` - Pure, KernelEffect, PortEffect, etc.
- `budgetCostModel` - What resources it consumes
- `spanKind` - What provenance span it emits
- `possibleDiagnostics` - What error codes it can produce
- `determinismClass` - det, nondet, or conditional

## 6.4 Package Boundaries

### OmegaLLM (TypeScript)

| Package | Contents |
|---------|----------|
| `@omega/vm` | Val representation, CEKS machine, continuation frames |
| `@omega/effects` | Tool invocation, oracle requests, validation |
| `@omega/stream` | Stream cells, forcing, receipts, materialization |
| `@omega/concurrency` | Fiber scheduler, replay policy, event ledger |
| `@omega/constraints` | Constraint network engine, contradictions |
| `@omega/toolhost` | Tool registry, schema validation, adapters |

### LambdaLLM (Lisp)

| Module | Contents |
|--------|----------|
| `lambdallm.core` | Pure core + special forms |
| `lambdallm.flow` | FrameLisp execution algebra surface |
| `lambdallm.prompt` | Prompt algebra + transformers |
| `lambdallm.tooling` | deftool / tool-call wrappers |
| `lambdallm.stream` | Surface API over runtime streams |
| `lambdallm.diagnostics` | Error/warning objects, codes |

### LambdaRLM (Lisp)

| Module | Contents |
|--------|----------|
| `lambdaRLM.solver` | Solver record + composition combinators |
| `lambdaRLM.strategy` | Strategy record + implementations |
| `lambdaRLM.meta` | meta-analyze/select/plan/execute/adapt |
| `lambdaRLM.budget` | Budget record + algebra |
| `lambdaRLM.provenance` | Evidence model + staleness checks |
| `lambdaRLM.repair` | Validate/repair loop |
| `lambdaRLM.domain-algebra` | Sorts/ops/equations/rewriting |

---

# Document Info

- **Generated**: 2026-01-20
- **Sources**: FrameLisp spec, OmegaLLM src/, LambdaLLM ARCHITECTURE/, LambdaRLM lib/, OmegaLLM CLAUDE-JOBS/, ARCHITECTURE-LANGUAGES-*.md
- **Total Functions Documented**: ~350+
- **Projects Covered**: 4 (FrameLisp, OmegaLLM, LambdaLLM, LambdaRLM)
- **Implementation Status**:
  - **IMPLEMENTED**: Sections 1-4 (FrameLisp spec, OmegaLLM runtime, LambdaLLM stdlib, LambdaRLM lib)
  - **SPEC ONLY**: Section 5.3-5.9 (LambdaLLM ARCHITECTURE specs not yet implemented)
  - **NOT STARTED**: Section 5.1-5.2 (OmegaLLM CLAUDE-JOBS 006, 008)
