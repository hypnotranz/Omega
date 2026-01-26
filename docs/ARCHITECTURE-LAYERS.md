# ARCHITECTURE-LAYERS: The Verified Perspective

## Executive Summary

After careful analysis of code and documentation, here is the verified relationship between the three projects:

```
LambdaLLM (v1)  ──evolved into──>  OmegaLLM (v2)
                                       │
                                       │  provides kernel primitives
                                       ▼
                              LambdaRLM (reasoning library)
```

**The Key Insight:**
- **LambdaLLM** = v1 Lisp interpreter with condition system, continuations, and LLM FFI
- **OmegaLLM** = v2 interpreter with a **better kernel** (CEKS machine, governance, provenance, streams, amb)
- **LambdaRLM** = Pure Lisp library implementing reasoning algorithms that leverage kernel primitives

The 6-layer language tower in ARCHITECTURE-LANGUAGES-1.md is **aspirational, not actual**. What exists is simpler and more powerful.

---

## 1. LambdaLLM: The First Implementation (v1)

### What It Actually Is

LambdaLLM is a **working TypeScript implementation** of a Lisp interpreter designed for AI agents. It has:

- **81 TypeScript source files** in `src/`
- **66 test files** in `test/`
- Recursive eval/apply interpreter
- Non-unwinding condition system
- First-class continuations
- Serializable closures
- FFI ports for world/LLM operations

### Core Architecture (Recursive Eval)

```typescript
// LambdaLLM's eval.ts - recursive interpreter
export function evalExpr(
  expr: Value,
  env: Environment,
  cont: Continuation,
  ffi: FFI
): Value | Promise<Value> {
  if (isAtom(expr)) {
    return applyCont(cont, expr);
  }
  // ... recursive calls
}
```

The key characteristic: **eval recursively calls itself**. The continuation is explicit but the recursion is implicit in the call stack.

### Why v1 Needed Evolution

The recursive architecture has limitations:
1. **Stack depth** - Deep computations hit limits
2. **Stepping** - Hard to pause mid-evaluation for debugging
3. **Governance** - No built-in resource tracking
4. **Provenance** - No effect receipting at kernel level

---

## 2. OmegaLLM: The Better Kernel (v2)

### What Changed

OmegaLLM rewrote the eval/apply core as a **CEKS machine** - an explicit state machine that doesn't use recursive function calls.

```typescript
// OmegaLLM's machine.ts - explicit state machine
export type State = {
  control: Control;      // Current expression or value
  env: Env;              // Lexical bindings
  store: Store;          // Mutable cells
  kont: Frame[];         // Explicit continuation STACK (not recursive)
  handlers: HandlerFrame[];  // Condition handlers
  profile?: Profile;     // Governance
  budget?: RuntimeBudget; // Resource limits
  provenanceGraph?: ProvenanceGraph;  // What-depends-on-what
};
```

### The Two-Loops Architecture (RSR)

OmegaLLM implements what ARCHITECTURE-EXPLANATION.md calls the **Reentrant Semantic Runtime**:

**Loop 1: Language Loop (CEKS)**
```
state.step() → reduce expression → state'
```

**Loop 2: Effect Loop (RuntimeDriver)**
```
machine.step() → yield Req → driver.handle(Req) → send Resp → machine.step()
```

The breakthrough: **Effects are reified, not executed inline**. When the evaluator hits an LLM call, it doesn't call the LLM - it suspends and emits a request object. The driver handles it and resumes.

### What v2 Kernel Provides That v1 Doesn't

| Feature | LambdaLLM (v1) | OmegaLLM (v2) |
|---------|---------------|---------------|
| Eval architecture | Recursive functions | CEKS state machine |
| Stepping | Hard (call stack) | Easy (explicit state) |
| Effect handling | Inline execution | Reified requests |
| Budgets | None | Built-in RuntimeBudget |
| Profiles | None | Built-in Profile/SecurityContext |
| Provenance | Application-level | Kernel-level graph |
| Receipts | None | Per-effect receipts |
| Snapshots | Serializable closures | Full state snapshots |
| Streams | Library | Built-in lazy streams |
| Amb | Library | Built-in backtracking |

### The "Better Kernel" Principle

The kernel provides **primitives** that libraries can build on:

1. **Continuations** (`call/cc`) - captured explicitly in kont stack
2. **Conditions** - non-unwinding handlers in handlers stack
3. **Streams** - lazy sequences with fair interleaving
4. **Amb** - nondeterministic choice with backtracking
5. **Budgets** - resource tracking per computation
6. **Receipts** - effect logging for replay/audit

These aren't features "bolted on" - they're in the machine state itself.

---

## 3. LambdaRLM: Reasoning Algorithms (Library Layer)

### What It Is

LambdaRLM is a **pure Lisp library** implementing structured search and reasoning algorithms. It has no TypeScript - just Lisp files that run on a Lisp interpreter.

```
lib/
├── streams.lisp         # Lazy streams (SICP-style)
├── composable.lisp      # Solver interface + composition
├── meta_search.lisp     # Strategy selection
├── provenance.lisp      # Evidence validation
├── domain_algebra.lisp  # Problem representation
└── ...
```

### How It Uses Kernel Primitives

RLM algorithms are designed to leverage what the kernel provides:

| RLM Algorithm | Uses Kernel Primitive |
|---------------|----------------------|
| Lazy streams | `amb` for fair enumeration |
| Beam search | Streams + budget tracking |
| CEGIS loop | Conditions for recoverable failure |
| Meta-search | Backtracking via `amb` |
| Evidence tracking | Provenance graph |
| Composable solvers | First-class continuations |

### The Power of the Combination

```lisp
;; This RLM code leverages Omega's kernel:
(define (beam-cegis problem k depth)
  (with-budget (budget-for-beam k depth)          ; Omega's budgets
    (let ((candidates (amb-generate-k k problem))) ; Omega's amb
      (stream-filter valid?                        ; Omega's streams
        (stream-map repair candidates)))))         ; Lazy evaluation
```

The reasoning algorithms become powerful because they're built on primitives that provide:
- Backtracking (amb)
- Fair enumeration (streams)
- Resource control (budgets)
- Recovery (conditions)

---

## 4. The Actual Architecture (Not the Aspirational Tower)

### What ARCHITECTURE-LANGUAGES-1.md Claims

The docs describe a 6-layer tower:
```
L6  Apps
L5  LambdaRLM
L4  LambdaLLM (macros/modules)
L3  FrameLisp IR
L2  OmegaLLM Runtime
L1  Host
```

### What Actually Exists

```
┌─────────────────────────────────────────┐
│ Applications                             │
├─────────────────────────────────────────┤
│ LambdaRLM (Pure Lisp Library)           │
│   - Runs on any Lisp with right prims   │
│   - Uses streams, amb, conditions       │
├─────────────────────────────────────────┤
│ OmegaLLM Runtime (v2 Kernel)            │
│   - CEKS machine with explicit state    │
│   - Effect reification (RSR)            │
│   - Governance (profiles, budgets)      │
│   - Provenance (receipts, graph)        │
│   - Primitives: amb, streams, conditions│
├─────────────────────────────────────────┤
│ TypeScript Host                          │
└─────────────────────────────────────────┘
```

**What's missing from the tower:**
- L4 (LambdaLLM as separate layer) - Omega IS the implementation
- L3 (FlowIR) - Exists as code but isn't integrated

### Why FlowIR Isn't Needed

The tower claims FlowIR provides:
- Canonical hashing
- Static analysis
- Caching/replay
- LLM reflection

But OmegaLLM provides these at the **effect boundary** instead:
- **Canonical hashing**: Receipts hash requests
- **Static analysis**: Walk Lisp forms directly
- **Caching/replay**: Receipt ledger
- **LLM reflection**: Show Lisp forms (homoiconic!)

Lisp forms ARE already an IR. FlowIR duplicates this in TypeScript, breaking homoiconicity.

---

## 5. The Value of Each Layer

### TypeScript Host (L1)
**Value**: Performance, Node.js ecosystem, async I/O
**Provides**: The "metal" that runs the interpreter

### OmegaLLM Kernel (L2)
**Value**: Explicit state, effect reification, governance
**Provides**: The primitives that make AI agent programming tractable

**Key innovations over LambdaLLM:**
1. CEKS machine (steppable, inspectable)
2. Effect reification (receipts, replay)
3. Built-in governance (budgets, profiles)
4. Built-in provenance (dependency graph)

### LambdaRLM Library (L5)
**Value**: Structured reasoning, composable solvers
**Provides**: The algorithms for making LLMs actually solve hard problems

**Key contributions:**
1. CEGIS pattern (synthesize-verify-repair)
2. Beam search over solution spaces
3. Meta-search (LLM picks strategy)
4. Domain algebra (force problem representation)

---

## 6. How They Fit Together

### The Correct Mental Model

```
User Code
    │
    ▼
LambdaRLM (algorithms)
    │ uses
    ▼
Omega Kernel (primitives: amb, streams, conditions, budgets)
    │ interprets
    ▼
TypeScript (executes)
```

### Load Order

```lisp
;; In Omega REPL:
(require 'core/streams)       ; Omega's built-in
(require 'core/amb)           ; Omega's built-in
(require 'lambdarlm/solvers)  ; RLM library
(require 'lambdarlm/cegis)    ; RLM library

;; Now use RLM algorithms backed by Omega primitives
(beam-cegis problem 5 10)
```

### Integration Strategy

1. **Verify dialect compatibility** - Test RLM Lisp on Omega
2. **Map FFI calls** - RLM's `world.*` / `llm.*` to Omega's ports
3. **Leverage kernel** - RLM algorithms should use Omega's amb/streams
4. **Keep provenance at port level** - Use receipts, not FlowIR

---

## 7. What About LambdaLLM Now?

### Historical Role
LambdaLLM was v1 - the first working implementation. It proved the concept.

### Current Role
The ARCHITECTURE folder (sections 00-31) serves as **language specification** shared with Omega.

### Code Status
LambdaLLM's code could:
- Be archived as "v1 reference"
- Be maintained as a lighter alternative (no governance needed)
- Provide test cases that Omega should pass

The ARCHITECTURE specification is shared - both implementations should conform to the same Lisp semantics (sections 00-31).

---

## 8. Verification Against ARCHITECTURE-EXPLANATION.md

The RSR document describes:

| RSR Concept | Our Understanding | Verified? |
|-------------|-------------------|-----------|
| Two loops (eval/apply + effect) | Omega's CEKS + RuntimeDriver | ✓ |
| Effects reified as Req objects | Machine suspends, yields request | ✓ |
| Receipts for provenance | EffectLedger stores req/resp | ✓ |
| Snapshots for checkpoint | SnapshotRepo stores state | ✓ |
| Reentrancy (LLM calls eval) | Portal interface | ✓ |
| Replay via receipts | ReplayPolicy modes | ✓ |

**Conclusion**: ARCHITECTURE-EXPLANATION.md describes what OmegaLLM actually implements. Our understanding is correct.

---

## 9. Summary: The Verified Truth

1. **LambdaLLM** = v1 implementation (recursive eval, working code)
2. **OmegaLLM** = v2 implementation (CEKS machine, better kernel)
3. **LambdaRLM** = Pure Lisp library (reasoning algorithms)
4. **The tower** = Aspirational design, not actual architecture
5. **FlowIR** = Exists but unnecessary (homoiconicity makes it redundant)
6. **Integration** = Load RLM as library on Omega, leverage kernel primitives

The architecture is simpler and more powerful than the tower suggests. OmegaLLM's better kernel makes RLM algorithms practical. The combination provides:

- **Provenance** via receipts (not FlowIR)
- **Replay** via receipt ledger
- **Governance** via budgets/profiles
- **Search** via amb/streams
- **Recovery** via conditions

This is the verified perspective.
