# Architecture Integration: OmegaLLM, LambdaLLM, and LambdaRLM

## Executive Summary

This document analyzes the relationship between three projects and recommends an integration strategy.

**Key Finding**: OmegaLLM is LambdaLLM evolved with a better kernel. LambdaLLM (v1) has a recursive eval interpreter; OmegaLLM (v2) rewrote this as a CEKS machine and added governance, provenance, streams, and amb. LambdaRLM is a Lisp library for reasoning/search that can leverage Omega's kernel primitives. FlowIR exists but is unnecessary (homoiconicity makes it redundant).

---

# Part I: The Three Projects In Depth

---

## 1. LambdaLLM: The Debuggable Agent Language

### The Core Idea

LambdaLLM is a **Lisp dialect designed for AI agent orchestration with first-class debugging support**. It answers the question: *"What if we designed a programming language specifically for building AI agents, where the ability to stop, inspect, and resume execution was a fundamental feature rather than an afterthought?"*

The key insight is that AI agents fail in complex ways—LLM calls timeout, tools return unexpected results, multi-step reasoning goes off track. Traditional languages treat debugging as tooling layered on top. LambdaLLM makes debugging a **language-level primitive** through its condition system.

### The Condition System: Non-Unwinding Error Handling

The most distinctive feature of LambdaLLM is its **non-unwinding condition system**, borrowed from Common Lisp but extended for agent use cases. When something goes wrong, instead of immediately unwinding the stack (like exceptions in Python/Java), the system:

1. **Signals a condition** without destroying the call stack
2. **Searches for a handler** in the dynamic scope
3. **The handler can inspect the full state** and choose how to proceed
4. **Restarts offer recovery options** defined at the error site

This means when an LLM call fails, you can:
- Retry with a different prompt
- Use a fallback value
- Ask the user for input
- Inspect the full context and make an informed decision

All without losing the execution state.

```lisp
(restart-case
  (llm.complete prompt)
  (use-fallback (val) val)
  (retry-with (new-prompt) (llm.complete new-prompt))
  (ask-user () (read-line "Enter value: ")))
```

### Serializable Closures: Time-Travel Debugging

LambdaLLM closures are **serializable**—you can save the entire state of a computation, including its captured environment, and resume it later. This enables:

- **Checkpointing**: Save agent state before risky operations
- **Time-travel**: Go back to any point and explore different paths
- **Session persistence**: Resume conversations across restarts
- **Debugging**: Capture failing states for later analysis

### The FFI Model: World and LLM as Ports

All side effects in LambdaLLM go through explicit interfaces:

- `world.*` - File operations, command execution, external tools
- `llm.*` - Language model calls with structured outputs

This isn't just for purity—it's for **replay and testing**. When all effects go through explicit ports, you can:
- Record all interactions for replay
- Mock external systems for testing
- Audit exactly what an agent did

### What LambdaLLM Actually Is

LambdaLLM is a **working TypeScript implementation** (v1). It has:
- **81 source files** in `src/`
- **66 test files**
- Recursive eval/apply interpreter
- Condition system, continuations, FFI
- Audit, replay, approval systems

The ARCHITECTURE folder contains shared specifications that both LambdaLLM and OmegaLLM conform to (sections 00-31). OmegaLLM extended this with section 32 ("LANGUAGE-OFFICIAL") which redesigned the evaluator.

**LambdaLLM = v1 implementation; OmegaLLM = v2 with better kernel.**

### Suggested Better Name

**FrameLisp** - Captures the "framework for agents" aspect while maintaining the Lisp heritage. The "Frame" also hints at stack frames, which are central to the debugging model.

---

## 2. OmegaLLM: The Dual-Plane Semantic Engine

### The Core Idea

OmegaLLM is a **runtime where inference is a first-class plane of computation, co-equal with evaluation**. It answers the question: *"What if 'understanding code' was as fundamental an operation as 'running code'?"*

The key insight is that LLMs don't just execute—they *interpret*, *predict*, *simulate*, and *transform*. These are different semantic operations than traditional evaluation. OmegaLLM makes this explicit by defining **four distinct semantic planes**:

1. **Extensional Evaluation (E)**: "Run it" - Traditional computation
2. **Intensional Evaluation (I)**: "Understand it" - Predict, simulate, analyze
3. **Search (S)**: "Explore possibilities" - Navigate spaces of meanings/programs
4. **Learning (L)**: "Update the understander" - Modify how intensional evaluation works

Classical languages model only (E). SICP gave us the methodology to make (E) explicit and malleable via `eval`/`apply`. **Omega extends the same methodology to (I), (S), and (L).**

### The Dual Kernel: eval and int

The Omega kernel has two fundamental operations:

```lisp
(eval '(f 10))                         ; extensional run → 21
(int  '(f 10) :engine engine:solver)   ; intensional meaning → Meaning
```

`eval` runs code and produces a value. `int` *interprets* code and produces a **Meaning**—a rich semantic object containing:

- **denotation**: What value(s) this might produce
- **residual**: Partially evaluated form
- **rewrite**: Equivalent but transformed version
- **invariants**: Inferred properties
- **effects**: Predicted side effects
- **cost**: Estimated resources needed
- **evidence**: Supporting citations/tests
- **confidence**: Calibrated belief (0-1)

This makes "LLM understands code" a **formal, inspectable artifact**, not a black-box prompt-response.

### The Meaning Object: Making Semantics Tangible

The `Meaning` type is Omega's key innovation. When an LLM "reasons about" code, it doesn't just produce text—it produces a structured semantic object:

```
Meaning = ⟨
  denotation    : Dist<Val> | Val      ; predicted results (possibly probabilistic)
  residual      : Expr                 ; partially evaluated program
  rewrite       : Expr                 ; meaning-preserving transform
  invariants    : Set<Inv>             ; what we know is true
  effects       : EffSummary           ; what side effects will happen
  cost          : CostModel            ; time/space/tokens/tool-calls
  evidence      : Seq<Evidence>        ; what supports this analysis
  obligation    : Obligation           ; what must be verified
  confidence    : Real [0,1]           ; how sure are we
⟩
```

This transforms vague "LLM analysis" into **formal objects with explicit uncertainty** that can be validated, composed, and reasoned about.

### Truth Governance: Epistemics as a First-Class Concern

Omega takes seriously that inference can be **wrong**. The language includes explicit machinery for:

- **Confidence levels**: Every semantic claim has calibrated uncertainty
- **Evidence tracking**: What observations support a conclusion
- **Obligations**: What must be proven before acting on a belief
- **Certificates**: Formal verification of claims

This is the "governance layer" that prevents agents from acting on hallucinations.

### Context Economics: Managing the Scarce Resource

LLM context is expensive and finite. Omega treats context as a **first-class resource** with explicit management:

- **Snapshots**: Save context state at checkpoints
- **Receipts**: Proof that context was consumed correctly
- **Compression/hydration**: Efficient context representation
- **VOI-driven retention**: Keep what's valuable, drop what isn't
- **Memoization**: Don't re-compute what's already known

This is "rocket staging" for agent context—explicit management of what to keep and what to jettison.

### What Actually Got Implemented

Omega started from LambdaLLM's spec but rewrote the philosophy in **Section 32: LANGUAGE-OFFICIAL**. The implementation began at section 32-13 (not from 00!). What exists now:

- **CEKS machine**: Working interpreter with Control, Environment, Kontinuation, Store
- **Continuations**: First-class `call/cc`
- **Conditions**: Non-unwinding handlers and restarts
- **Governance**: Profiles, budgets, security contexts
- **Provenance**: Graph of what-depends-on-what
- **Streams**: Lazy sequences for search
- **Amb**: Nondeterministic choice with backtracking
- **REPL**: Interactive evaluation with session management

### Suggested Better Name

**Omega** is already good. The Ω symbol suggests "completion" or "the final form"—appropriate for a language that aims to be the complete substrate for agent computation.

---

## 3. LambdaRLM: The Reasoning Machinery

### The Core Idea

LambdaRLM is a **Lisp library for structured search and reasoning**. It answers the question: *"How do we make LLMs actually good at hard problems instead of just generating plausible-looking answers?"*

The key insight is that LLMs fail at complex problems not because they're stupid, but because **they satisfice**—they produce the first answer that seems reasonable instead of exploring the solution space. LambdaRLM forces systematic exploration through:

- **CEGIS**: Synthesize → Verify → Repair → Iterate
- **Beam search**: Maintain multiple candidates, score, select
- **Meta-search**: Let the LLM choose its own strategy
- **Composable solvers**: Build complex search from simple pieces

### CEGIS: Counterexample-Guided Inductive Synthesis

The core pattern in LambdaRLM is **CEGIS**, borrowed from program synthesis:

1. **Synthesize**: LLM generates a candidate solution
2. **Verify**: Deterministic validators check for problems
3. **Repair**: LLM fixes only the failing parts (with specific error feedback)
4. **Iterate**: Until validators pass

This transforms "one-shot generation" into "iterative refinement with feedback." The LLM is no longer guessing—it's **converging** toward correctness.

But vanilla CEGIS has a problem: it converges to *"passes the tests"*, not *"is actually good."* A solution might be structurally valid but architecturally terrible.

### Beam CEGIS: Adding Selection Pressure

LambdaRLM extends CEGIS with **beam search**:

1. Generate **K candidates** (not just one)
2. Repair each to validity
3. **Score** each on quality metrics (not just pass/fail)
4. Keep top K
5. Iterate for depth D or until quality threshold

This adds **selection pressure**—the system doesn't just find *a* solution, it finds a *good* solution by maintaining population diversity and using scoring to guide search.

### Lazy Streams: Fair Search Enumeration

LambdaRLM uses **SICP-style lazy streams** for search:

```lisp
(define (stream-cons head tail-thunk)
  (cons head tail-thunk))  ; tail is delayed

(define (stream-interleave s1 s2)
  ;; Alternate between streams for fair enumeration
  (if (stream-null? s1) s2
      (stream-cons (stream-car s1)
                   (lambda () (stream-interleave s2 (stream-cdr s1))))))
```

This matters because naive depth-first search can get stuck in infinite branches. **Fair interleaving** ensures all possibilities get explored eventually.

### Meta-Search: The LLM Picks Its Own Strategy

The most advanced feature is **meta-search**: the LLM analyzes problem characteristics and selects an appropriate search strategy:

```lisp
(define (meta-select-strategy features toolkit)
  (cond
    ;; High branching + shallow depth → beam search
    ((and (= branching 'high) (= depth 'shallow))
     (find-strategy toolkit 'beam-search))
    ;; Deep problems → depth-first with limit
    ((= depth 'deep)
     (find-strategy toolkit 'depth-first))
    ;; Tight constraints → greedy (fast failure)
    ((= constraints 'tight)
     (find-strategy toolkit 'greedy))
    ...))
```

This is **learning to search**—the system adapts its exploration strategy based on what it learns about the problem.

### Domain Algebra: Forcing Representation Before Action

A key innovation is the **Domain Algebra**—a structured representation of the problem domain that must be produced *before* code generation:

```lisp
(domain-algebra
  (entities (Order) (ProcessingContext) (Receipt))
  (observables
    (observable (name "FraudService.check") (kind effect)))
  (constructors
    (ctor (name decision) (args (predicate) (then) (else))))
  (invariants
    (inv (name "public-signature") (check "...")))
  (objectives
    (obj (name "reduce-nesting") (metric "if-count delta"))))
```

This forces the LLM to **think before acting**—it can't just dump code, it has to articulate the problem structure first. This is Phase 0 gating that prevents premature convergence.

### Composable Solvers: Building from Pieces

Solvers in LambdaRLM are composable through three patterns:

```lisp
;; Sequential: output of s1 feeds s2
(compose-sequential analyze-solver implement-solver)

;; Parallel: run all, merge results
(compose-parallel (list solver-a solver-b solver-c) merge-fn)

;; Fallback: try primary, fall back on failure
(compose-fallback expensive-solver (list cheap-solver simple-solver))
```

This enables building complex search strategies from simple, testable pieces.

### Provenance: Evidence Tracking

LambdaRLM includes a full **provenance system**:

```lisp
(evidence
  (id "ev:sha256:...")
  (ref "src/OrderProcessor.java")
  (fingerprint "abc123...")
  (span (start-line 10) (end-line 50))
  (obtained-via "refactoring-analysis")
  (world-view "staged"))
```

Every claim about the code has **evidence with staleness detection**. If the underlying file changes, the evidence is marked stale.

### What LambdaRLM Is NOT

LambdaRLM is **not** a language—it's a **library**. It's meant to run on top of a Lisp interpreter (originally LambdaLLM, now Omega). It provides:

- Data structures (streams, solvers, results)
- Algorithms (CEGIS, beam search, meta-search)
- Protocols (composable solver interface, failure handling)

But no syntax, no semantics, no interpreter.

### Suggested Better Names

- **SolverKit**: Emphasizes the composable solver toolkit
- **ReasonLib**: Emphasizes the reasoning/search focus
- **SearchForge**: Emphasizes building search strategies
- Keep **LambdaRLM**: RLM = Reasoning Language Model, which is accurate

---

# Part II: The Language Tower - Truth vs Aspiration

---

## 4. What the Architecture Docs Claim

The ARCHITECTURE-LANGUAGES-1.md document describes a **6-layer language tower**:

```
L6  Product / Domain Apps
    - domain toolchains, agents, vertical solutions

L5  LambdaRLM (Reasoning Framework)
    - solvers, strategies, meta-search, repair loops
    - "depends only on L4 + stable L3 IR contracts"

L4  LambdaLLM (Language + Stdlib)
    - macros, module system
    - "compiles/lowers to FrameLisp IR"

L3  FrameLisp (IR + Algebraic Semantics)
    - Prompt algebra, Execution algebra
    - "MUST be serializable, hashable, replayable"

L2  OmegaLLM Runtime Substrate
    - evaluator (CEKS), streams, constraints
    - "interprets FrameLisp IR"

L1  Host Platform
    - Node/TS, external tools
```

**The key claim**: Everything flows through FrameLisp IR. LambdaLLM compiles to it. LambdaRLM depends on it. OmegaLLM interprets it.

---

## 5. What Actually Exists

| Layer | Docs Claim | Reality |
|-------|------------|---------|
| **L5 LambdaRLM** | "Depends on L4 + L3 IR contracts" | **Standalone Lisp files** that can run on any compatible Lisp interpreter |
| **L4 LambdaLLM** | "Compiles to FrameLisp IR" | **v1 implementation** (recursive eval). Does NOT compile to FlowIR. |
| **L3 FrameLisp IR** | "Serializable, hashable, replayable" | **TypeScript types + codec** (tested). **No interpreter.** Not integrated. |
| **L2 OmegaLLM** | "Interprets FrameLisp IR" | **v2 implementation** (CEKS machine). Does NOT interpret FlowIR. |

**The language tower does not exist.** It's a design document describing what SHOULD be, not what IS.

---

## 6. The Actual Current Architecture

What actually exists is much simpler:

```
┌─────────────────────────────────────────────────────────────┐
│ LambdaRLM (Standalone Lisp files)                          │
│   - Pure Lisp library for reasoning/search                 │
│   - Can run on any Lisp with right primitives              │
└─────────────────────────────────────────────────────────────┘
                        ↓ can run on
┌─────────────────────────────────────────────────────────────┐
│ OmegaLLM (v2 implementation)                               │
│   - CEKS machine with explicit state                       │
│   - Governance (profiles, budgets, security)               │
│   - Provenance (dependency graph)                          │
│   - Primitives: streams, amb, conditions                   │
└─────────────────────────────────────────────────────────────┘
                        ↑ evolved from
┌─────────────────────────────────────────────────────────────┐
│ LambdaLLM (v1 implementation)                              │
│   - Recursive eval with continuation passing               │
│   - 81 source files, 66 test files                         │
│   - Conditions, continuations, FFI, audit, replay          │
└─────────────────────────────────────────────────────────────┘
```

**Omega is LambdaLLM evolved.** LambdaRLM provides reasoning algorithms that leverage Omega's kernel primitives.

---

## 7. Why the Tower Wasn't Built (And Why That's Fine)

The architecture docs say:
> "Never let convenience APIs bypass FlowIR. Every new capability must either be a Flow node, a ToolPort call with a contract, or a pure library function that constructs Flow nodes."

But this was never implemented because:
1. **Homoiconicity**: Lisp forms ARE already an IR - why duplicate in TypeScript?
2. **Simpler path**: CEKS machine interprets Lisp directly with full expressiveness
3. **Port-level audit**: Provenance/replay happens at effect boundaries (receipts), not IR level

**The tower was aspirational.** The actual architecture is simpler: Omega interprets Lisp, RLM is a Lisp library, effects are receipted at port boundaries.

---

## 8. Is the Tower Worth Building?

### Arguments FOR the Tower (from the docs)

1. **Static analysis**: Lint FlowIR before execution (check budget domination, etc.)
2. **Canonical hashing**: FlowIR has deterministic serialization for caching
3. **Cross-system interchange**: JSON FlowIR is portable
4. **LLM reflection**: Show FlowIR to LLM for mutation/planning

### Arguments AGAINST the Tower

1. **Homoiconicity**: Lisp forms ARE already an IR. Why duplicate?
2. **Complexity**: Extra compilation step, extra representation, extra bugs
3. **Already working**: CEKS machine interprets Lisp directly and it works
4. **Port-level audit**: Provenance/replay can happen at the port boundary, not IR level

### The Simpler Alternative

Instead of building the FlowIR tower, we could:

1. **Load LambdaRLM as a Lisp library on Omega** (already compatible)
2. **Define a `framelisp/core.lisp` package** with the abstraction primitives
3. **Implement audit/caching at the port level** (where effects happen)
4. **Keep FlowIR as optional serialization format** (for tooling, not execution)

This gives us the benefits without the complexity.

---

# Part III: Practical Integration

---

## 9. Can LambdaRLM Run on Omega?

### 9.1 Lisp Dialect Compatibility

**Yes, with minor adaptations.** The LambdaRLM Lisp code uses standard constructs:

| Feature | LambdaRLM Uses | Omega Status |
|---------|----------------|--------------|
| `define`, `lambda`, `let` | ✓ | ✓ Supported |
| `if`, `cond`, `begin` | ✓ | ✓ Supported |
| `cons`, `car`, `cdr`, `list` | ✓ | ✓ Supported |
| `null?`, `list?`, `string?` | ✓ | ✓ Supported |
| Closures as thunks | ✓ | ✓ Supported |
| `set!` (mutation) | ✓ | ✓ Supported |
| `apply` | ✓ | ✓ Supported |

### 9.2 What May Need Adaptation

1. **`let*`**: Was recently identified as not supported in Omega's `compileText.ts` pipeline. Need to verify if the flowPipeline supports it.

2. **FFI calls**: LambdaRLM uses FFI patterns like `(world.read path)`, `(llm.complete prompt)`, `(world.fingerprint ref)`. These need to be mapped to Omega's primitives.

3. **Helper functions**: LambdaRLM defines some utilities that may already exist in Omega:
   - `member?` - check if element in list
   - `alist-get` - get value from association list
   - `regex-search`, `regex-findall` - regex operations

### 9.3 Test Strategy

To verify compatibility:
```lisp
;; Load streams.lisp and test basic operations
(require 'lambdarlm/streams)
(define s (integers-from 1))
(stream-take 5 s)  ;; Should return (1 2 3 4 5)
```

---

## 10. Confirmed: Omega Started at Section 32

### 10.1 Evidence

1. **OmegaLLM has the SAME ARCHITECTURE folder as LambdaLLM** (sections 00-31 are identical)

2. **Section 32 is titled "32-LANGUAGE-OFFICIAL"** and starts with:
   > "I'm going to restart from scratch and lay down a coherent, unified, 'balls-to-the-wall' language..."
   > "I'll call the language **Ω** (Omega). Rename later."

3. **Implementation files reference section 32**:
   ```typescript
   // SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-4.md
   // AUTO-EXTRACTED - Do not edit directly. Edit the source document.
   ```

4. **The 32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION series** has 19 parts (1-19) with detailed implementation specs.

### 10.2 What Section 32 Changed

The shift from LambdaLLM to Omega was philosophical:

**LambdaLLM (sections 00-31)**: "Lisp with LLM calls as FFI"

**Omega (section 32)**: "Dual-plane semantics where inference is co-equal with evaluation"

Key additions in section 32:
- Dual-plane computation (extensional + intensional)
- Truth governance (uncertainty, confidence, evidence, certificates)
- Learning as a first-class plane
- Quoted programs as primary substrate
- REPL re-entrancy under discipline

---

## 4. What is FlowIR? Should We Keep It?

### 4.1 What FlowIR Is

FlowIR is a TypeScript AST representation of Lisp programs:

```typescript
interface FInfer extends NodeBase {
  tag: "FInfer";
  prompt: PromptIR;
  options?: ValueIR;
}
```

It's designed for:
- Canonical serialization (JSON)
- Content-addressed hashing
- Static analysis (budget domination, timeout linting)
- Caching/replay

### 4.2 The Problem with FlowIR

**FlowIR breaks homoiconicity.** In Lisp, the AST IS the data. You don't need a separate IR because:

```lisp
;; This IS the AST:
(infer prompt options)

;; Why do we need this TypeScript representation?
{ tag: "FInfer", prompt: ..., options: ... }
```

### 4.3 Recommendation: Lisp-Level Abstraction Instead

Rather than FlowIR (TypeScript AST), use a **Lisp package** as the abstraction layer:

```lisp
;; framelisp/core.lisp - the abstraction layer
(defflow infer (prompt &optional options)
  "Oracle call with budget tracking"
  ...)

(defflow with-budget (budget &rest body)
  "Wrap execution in budget constraint"
  ...)
```

Then LambdaRLM and other libraries just `require` this package:

```lisp
(require 'framelisp/core)
(infer (build-prompt ...))
```

### 4.4 What FlowIR Provides That We Still Need

The FEATURES FlowIR was meant to provide are valid, but can be achieved in Lisp:

| Feature | FlowIR Approach | Lisp Approach |
|---------|-----------------|---------------|
| Canonical hashing | JSON codec | Canonical printer + SHA256 |
| Static analysis | TypeScript linter | Lisp form walker |
| Caching | Hash FlowIR nodes | Hash Lisp forms |
| Replay | Log FlowIR + responses | Log forms + responses |
| Budget verification | Lint pass | Walk form tree |

---

## 5. Recommended Architecture

### 5.1 The Stack

```
┌─────────────────────────────────────────────────────────────┐
│ Applications / Domain Agents                                │
├─────────────────────────────────────────────────────────────┤
│ LambdaRLM (ReasonLib)                                       │
│   - Solvers, strategies, meta-search, provenance            │
│   - Pure Lisp library, uses FrameLisp Core                  │
├─────────────────────────────────────────────────────────────┤
│ FrameLisp Core (Abstraction Layer)                          │
│   - defflow, infer, with-budget, tool-call                  │
│   - Lisp package, NOT TypeScript IR                         │
├─────────────────────────────────────────────────────────────┤
│ OmegaLLM Runtime                                            │
│   - CEKS machine, scheduler, streams, constraints           │
│   - Ports: Oracle, Tool, Store, Sink, Source                │
│   - Governance: profiles, budgets, security                 │
├─────────────────────────────────────────────────────────────┤
│ TypeScript Host                                             │
└─────────────────────────────────────────────────────────────┘
```

### 5.2 Integration Steps

1. **Create FrameLisp Core package** (Lisp, not TypeScript)
   - Define `defflow`, `infer`, `with-budget`, etc.
   - Map to Omega primitives

2. **Load LambdaRLM as a Lisp library**
   - Test `streams.lisp`, `composable.lisp`, etc. on Omega
   - Fix any compatibility issues (likely minor)

3. **Decide FlowIR's fate**
   - Option A: Keep for TypeScript tooling (linting, IDE integration)
   - Option B: Remove and do everything in Lisp (simpler, homoiconic)
   - Option C: Use FlowIR as serialization format only (save/load)

4. **Implement audit/provenance at port level** (not IR level)
   - Every port call emits spans
   - Caching at port boundary
   - Replay by replaying port interactions

---

## 6. Answers to Original Questions

### Q: Are LambdaLLM/LambdaRLM libraries ON TOP of Omega, or at the core?

**Answer**: They are libraries ON TOP. The core is the CEKS machine in `src/core/eval/`. LambdaLLM is the language spec that Omega implements. LambdaRLM is a library that runs on Omega.

### Q: Do they depend on each other?

**Answer**:
- LambdaRLM depends on LambdaLLM (uses the same Lisp dialect)
- Both run on Omega (the runtime)
- They are NOT orthogonal - LambdaRLM builds on the language features

### Q: Should traces/evidence be mandatory for every call?

**Answer**: No - make it **policy, not mandate**:
- Omega kernel: Every port call CAN emit spans (mechanism)
- FrameLisp Core / LambdaRLM: Decide WHEN to require evidence (policy)
- Budget accounting: Mandatory at kernel level
- Full provenance: Optional, library-level

### Q: Can we just load these as libraries/packages?

**Answer**: Yes! That's exactly what should happen:
```lisp
(require 'framelisp/core)   ;; Abstraction layer
(require 'lambdarlm/streams) ;; Lazy streams
(require 'lambdarlm/solvers) ;; Composable solvers
```

### Q: What is FlowIR for?

**Answer**: FlowIR was meant to be the "audit layer" for caching/replay/linting. But it breaks homoiconicity. The same goals can be achieved with:
- Canonical Lisp printing + hashing
- Lisp form walkers for linting
- Port-level logging for replay

---

## 7. Next Steps

1. **Immediate**: Test LambdaRLM's `streams.lisp` and `composable.lisp` on Omega REPL

2. **Short-term**: Create `framelisp/core.lisp` package with:
   - `defflow` macro
   - `infer` primitive wrapper
   - `with-budget` form
   - `tool-call` primitive wrapper

3. **Medium-term**: Decide FlowIR's role:
   - If keeping: Document as "serialization format for tooling"
   - If removing: Delete `src/frameir/` and simplify architecture

4. **Long-term**: Full integration test with a real LambdaRLM solver running on Omega

---

## Appendix A: File Locations

| Project | Path | Key Files |
|---------|------|-----------|
| LambdaLLM | `LambdaLLM/ARCHITECTURE/` | 00-SPECIFICATION.md through 32-LANGUAGE-OFFICIAL |
| LambdaRLM | `LambdaRLM/lib/` | streams.lisp, composable.lisp, meta_search.lisp |
| LambdaRLM | `LambdaRLM/EXPLORATION/` | EXPLORATION-1 through EXPLORATION-11 |
| OmegaLLM | `OmegaLLM/src/core/eval/` | machine.ts, machineStep.ts, values.ts |
| OmegaLLM | `OmegaLLM/src/frameir/` | FlowIR infrastructure (tested, not integrated) |
| OmegaLLM | `OmegaLLM/docs/` | ARCHITECTURE-LANGUAGES-1 through 6 |

## Appendix B: Lisp Dialect Quick Comparison

```lisp
;; LambdaLLM/LambdaRLM style:
(define (stream-cons head tail-thunk)
  (cons head tail-thunk))

(define (stream-car s) (car s))

(define (stream-cdr s)
  (apply (cadr s) '()))  ;; Force thunk

;; Should work on Omega as-is
;; Only potential issue: (apply fn '()) vs (fn)
```
