# SICP vs OmegaLLM: Side-by-Side Comparison

**Purpose:** Map every SICP section to our current/needed OmegaLLM chapters

**Legend:**
- ✅ = We have this chapter
- 🔧 = Need to add this chapter
- ⚠️ = Optional/skip
- 🎯 = LLM-specific extension (not in SICP)

---

## PART I: Building Abstractions with Procedures

| SICP Section | SICP Title | OmegaLLM Chapter | Status | Notes |
|--------------|-----------|------------------|--------|-------|
| **1.1** | **The Elements of Programming** | | | |
| 1.1.1 | Expressions | Ch01: Getting Started | ✅ | Basic REPL |
| 1.1.2 | Naming and the Environment | Ch01: Getting Started | ✅ | `(define ...)` |
| 1.1.3 | Evaluating Combinations | Ch01: Getting Started | ✅ | Eval semantics |
| 1.1.4 | Compound Procedures | Ch01: Getting Started | ✅ | `(define (f x) ...)` |
| 1.1.5 | The Substitution Model | **Ch28: Semantic Substitution** | 🔧 | **NEED** |
| 1.1.6 | Conditional Expressions | Ch01: Getting Started | ✅ | `if`, `cond` |
| 1.1.7 | Example: Square Roots by Newton's Method | **Ch29: Iterative Refinement** | 🔧 | **NEED** |
| 1.1.8 | Procedures as Black-Box Abstractions | Ch11: Semantic Procedures as Black Boxes | ✅ | Encapsulation |
| **1.2** | **Procedures and Processes** | | | |
| 1.2.1 | Linear Recursion and Iteration | Ch12: Inference Processes | ✅ | Recursive vs iterative |
| 1.2.2 | Tree Recursion | **Ch30: Tree Recursion** | 🔧 | **NEED** |
| 1.2.3 | Orders of Growth | **Ch31: Cost Analysis** | 🔧 | **NEED** |
| 1.2.4 | Exponentiation | - | ⚠️ | Skip (specific algorithm) |
| 1.2.5 | Greatest Common Divisors | - | ⚠️ | Skip (specific algorithm) |
| 1.2.6 | Testing for Primality | - | ⚠️ | Skip (specific algorithm) |
| **1.3** | **Higher-Order Procedures** | | | |
| 1.3.1 | Procedures as Arguments | Ch04: Higher-Order LLM Functions | ✅ | HOFs |
| 1.3.1 | - | Ch13: Higher-Order Inference | ✅ | Fold with LLM |
| 1.3.2 | Lambda | Ch04: Higher-Order LLM Functions | ✅ | Lambdas |
| 1.3.3 | General Methods | **Ch32: Fixpoint/General Methods** | 🔧 | **NEED** |
| 1.3.4 | Returned Values | Ch04: Higher-Order LLM Functions | ✅ | Factories |
| | | | | |
| **(LLM-specific)** | **Nondeterministic Search** | Ch05: Nondeterministic Search | 🎯 | **LLM extension** |
| **(LLM-specific)** | **Multi-Shot Sampling** | Ch06: Multi-Shot Sampling | 🎯 | **LLM extension** |

**Part I Total: SICP 14 sections → OmegaLLM 10 current + 4 needed + 2 LLM-specific = 16 chapters**

---

## PART II: Building Abstractions with Data

| SICP Section | SICP Title | OmegaLLM Chapter | Status | Notes |
|--------------|-----------|------------------|--------|-------|
| **2.1** | **Data Abstraction** | | | |
| 2.1.1 | Rational Numbers | - | ⚠️ | Skip (specific example) |
| 2.1.2 | Abstraction Barriers | Ch14: Semantic Data Abstraction | ✅ | Data abstraction |
| 2.1.3 | What Is Data? | Ch14: Semantic Data Abstraction | ✅ | Data definition |
| 2.1.4 | Interval Arithmetic | - | ⚠️ | Skip (extended exercise) |
| **2.2** | **Hierarchical Data** | | | |
| 2.2.1 | Sequences | Ch15: Sequences as Semantic Interfaces | ✅ | Lists, map, filter |
| 2.2.2 | Hierarchical Structures | **Ch33: Hierarchical Semantics** | 🔧 | **NEED** |
| 2.2.3 | Conventional Interfaces | Ch15: Sequences as Semantic Interfaces | ✅ | Map, filter, fold |
| 2.2.4 | Picture Language | - | ⚠️ | Skip (graphics) |
| **2.3** | **Symbolic Data** | | | |
| 2.3.1 | Quotation | Ch16: Symbolic Semantic Data | ✅ | `quote` |
| 2.3.2 | Symbolic Differentiation | **Ch34: Semantic Differentiation** | 🔧 | **NEED** |
| 2.3.3 | Sets | - | ⚠️ | Skip (data structure) |
| 2.3.4 | Huffman Trees | - | ⚠️ | Skip (encoding) |
| **2.4** | **Multiple Representations** | | | |
| 2.4.1 | Complex Numbers | - | ⚠️ | Skip (specific example) |
| 2.4.2 | Tagged Data | **Ch35: Tagged Semantic Values** | 🔧 | **NEED** |
| 2.4.3 | Data-Directed Programming | Ch17: Multiple Representations | ✅ | Dispatch |
| 2.4.3 | - | Ch18: Generic Semantic Operations | ✅ | Generic ops |
| **2.5** | **Generic Operations** | | | |
| 2.5.1 | Generic Arithmetic | Ch18: Generic Semantic Operations | ✅ | Generic dispatch |
| 2.5.2 | Combining Types (Coercion) | **Ch36: Type Coercion** | 🔧 | **NEED** |
| 2.5.3 | Symbolic Algebra | - | ⚠️ | Skip (algebra) |

**Part II Total: SICP 18 sections → OmegaLLM 8 current + 4 needed = 12 chapters**

---

## PART III: Modularity, Objects, and State

| SICP Section | SICP Title | OmegaLLM Chapter | Status | Notes |
|--------------|-----------|------------------|--------|-------|
| **3.1** | **Assignment and State** | | | |
| 3.1.1 | Local State Variables | Ch19: Conversational State and Memory | ✅ | `set!`, state |
| 3.1.2 | Benefits of Assignment | Ch19: Conversational State and Memory | ✅ | State benefits |
| 3.1.3 | Costs of Assignment | - | ⚠️ | Skip (conceptual) |
| **3.2** | **Environment Model** | | | |
| 3.2.1 | Rules for Evaluation | Ch20: The Semantic Environment Model | ✅ | Environment |
| 3.2.2 | Applying Procedures | Ch20: The Semantic Environment Model | ✅ | Application |
| 3.2.3 | Frames as Repository | Ch20: The Semantic Environment Model | ✅ | Frames |
| 3.2.4 | Internal Definitions | Ch20: The Semantic Environment Model | ✅ | Internal `define` |
| **3.3** | **Mutable Data** | | | |
| 3.3.1 | Mutable Lists | Ch21: Mutable Semantic Structures | ✅ | `set-car!`, `set-cdr!` |
| 3.3.2 | Queues | **Ch37: Queues and Tables** | 🔧 | **NEED** |
| 3.3.3 | Tables | **Ch37: Queues and Tables** | 🔧 | **NEED** (same chapter) |
| 3.3.4 | Digital Circuits | **Ch38: State Machines/Constraints** | 🔧 | **NEED** |
| 3.3.5 | Constraint Propagation | **Ch38: State Machines/Constraints** | 🔧 | **NEED** (same chapter) |
| **3.4** | **Concurrency** | | | |
| 3.4.1 | Time in Concurrent Systems | Ch22: Concurrent Inference | ⚠️ | Sequential stand-in |
| 3.4.2 | Serializers/Mechanisms | **Ch39: Serializers/Sync** | 🔧 | **NEED** |
| **3.5** | **Streams** | | | |
| 3.5.1 | Streams Are Delayed Lists | Ch07: Lazy Streams | ✅ | Lazy streams |
| 3.5.2 | Infinite Streams | Ch23: Streams of Inference | ✅ | Infinite sequences |
| 3.5.3 | Stream Paradigm | Ch23: Streams of Inference | ✅ | Stream patterns |
| 3.5.4 | Delayed Evaluation | Ch07: Lazy Streams | ✅ | Delay |
| 3.5.5 | Modularity | Ch23: Streams of Inference | ✅ | Modularity |

**Part III Total: SICP 18 sections → OmegaLLM 13 current + 3 needed = 16 chapters**

---

## PART IV: Metalinguistic Abstraction

| SICP Section | SICP Title | OmegaLLM Chapter | Status | Notes |
|--------------|-----------|------------------|--------|-------|
| **4.1** | **Metacircular Evaluator** | | | |
| 4.1.1 | Core of Evaluator | Ch24: Oracle in Evaluator | ✅ | Eval/apply |
| 4.1.2 | Representing Expressions | Ch24: Oracle in Evaluator | ✅ | AST |
| 4.1.3 | Data Structures | Ch24: Oracle in Evaluator | ✅ | Environments |
| 4.1.4 | Running Evaluator | Ch24: Oracle in Evaluator | ✅ | Meta-circular |
| 4.1.5 | Data as Programs | Ch24: Oracle in Evaluator | ✅ | Conceptual |
| 4.1.6 | Internal Definitions | Ch24: Oracle in Evaluator | ✅ | Internal define |
| 4.1.7 | Analyzing Evaluator | **Ch40: Analyzing Evaluator** | 🔧 | **NEED** |
| **4.2** | **Lazy Evaluation** | | | |
| 4.2.1 | Normal vs Applicative Order | Ch25: Lazy Semantic Evaluation | ✅ | Order |
| 4.2.2 | Lazy Interpreter | Ch25: Lazy Semantic Evaluation | ✅ | Lazy eval |
| 4.2.3 | Streams as Lazy Lists | Ch07: Lazy Streams | ✅ | Already covered |
| 4.2.3 | - | Ch25: Lazy Semantic Evaluation | ✅ | Also here |
| **4.3** | **Nondeterministic Computing** | | | |
| 4.3.1 | Amb and Search | Ch05: Nondeterministic Search | ✅ | AMB (early) |
| 4.3.1 | - | Ch26: The AMB Inference Engine | ✅ | AMB (deep) |
| 4.3.2 | Examples | Ch26: The AMB Inference Engine | ✅ | Examples |
| 4.3.3 | Implementing AMB | Ch26: The AMB Inference Engine | ✅ | Implementation |
| **4.4** | **Logic Programming** | | | |
| 4.4.1 | Deductive Retrieval | Ch27: Logic Programming | ✅ | Queries |
| 4.4.2 | How Query System Works | **Ch41: Query System** | 🔧 | **NEED** |
| 4.4.3 | Is Logic Mathematical? | Ch27: Logic Programming | ✅ | Conceptual |
| 4.4.4 | Implementing Query System | **Ch41: Query System** | 🔧 | **NEED** (same) |
| 4.4.4.4 | Unification | **Ch42: Unification** | 🔧 | **NEED** |
| | | | | |
| **(LLM-specific)** | **The Agentic REPL** | Ch08: The Debugger | 🎯 | **LLM extension** |
| **(LLM-specific)** | **The Agentic REPL** | Ch09: The Agentic REPL | 🎯 | **LLM extension** |
| **(LLM-specific)** | **Full API Reference** | Ch10: Full API Reference | 🎯 | **LLM extension** |

**Part IV Total: SICP 23 sections → OmegaLLM 14 current + 3 needed + 3 LLM-specific = 20 chapters**

---

## PART V: Computing with Register Machines

| SICP Section | SICP Title | OmegaLLM Chapter | Status | Notes |
|--------------|-----------|------------------|--------|-------|
| **5.1** | **Designing Machines** | | | |
| 5.1.1 | Machine Language | **Ch43: State Machines** | 🔧 | **NEED** |
| 5.1.2 | Machine Abstraction | **Ch43: State Machines** | 🔧 | **NEED** (same) |
| 5.1.3 | Subroutines | - | ⚠️ | Skip (low-level) |
| 5.1.4 | Stack for Recursion | - | ⚠️ | Skip (low-level) |
| 5.1.5 | Instruction Summary | **Ch43: State Machines** | 🔧 | **NEED** (same) |
| **5.2** | **Simulator** | | | |
| 5.2.1-5.2.4 | Machine Simulator | - | ❌ | Implementation detail |
| **5.3** | **Storage/GC** | | | |
| 5.3.1-5.3.2 | Memory, GC | - | ❌ | JS runtime handles |
| **5.4** | **Explicit-Control Evaluator** | | | |
| 5.4.1 | Core of Evaluator | **Ch44: Explicit-Control Eval** | 🔧 | **NEED** |
| 5.4.2 | Tail Recursion | **Ch44: Explicit-Control Eval** | 🔧 | **NEED** (same) |
| 5.4.3 | Conditionals/Assignments | **Ch44: Explicit-Control Eval** | 🔧 | **NEED** (same) |
| 5.4.4 | Running Evaluator | **Ch44: Explicit-Control Eval** | 🔧 | **NEED** (same) |
| **5.5** | **Compilation** | | | |
| 5.5.1 | Compiler Structure | **Ch45: Semantic Compilation** | 🔧 | **NEED** |
| 5.5.2 | Compiling Expressions | **Ch45: Semantic Compilation** | 🔧 | **NEED** (same) |
| 5.5.3 | Compiling Combinations | **Ch45: Semantic Compilation** | 🔧 | **NEED** (same) |
| 5.5.4 | Instruction Sequences | **Ch45: Semantic Compilation** | 🔧 | **NEED** (same) |
| 5.5.5 | Example Code | **Ch45: Semantic Compilation** | 🔧 | **NEED** (same) |
| 5.5.6 | Lexical Addressing | - | ⚠️ | Skip (optimization) |
| 5.5.7 | Interface | - | ⚠️ | Skip (implementation) |

**Part V Total: SICP 22 sections → OmegaLLM 0 current + 3 needed = 3 chapters**

---

## PART VI: LLM-Specific Extensions (Beyond SICP)

| Section | Title | OmegaLLM Chapter | Status | Notes |
|---------|-------|------------------|--------|-------|
| **6.1** | **OPR Runtime** | **Ch46: OPR Multi-Kernel** | 🔧 | **NEED** |
| **6.2** | **Provenance** | **Ch47: Evidence Chains** | 🔧 | **NEED** |
| **6.3** | **Budget Management** | **Ch48: Token Budgets** | 🔧 | **NEED** |
| **6.4** | **Advanced Caching** | **Ch49: Semantic Caching** | 🔧 | **NEED** |
| **6.5** | **Multi-modal** | **Ch50: Multi-modal Semantics** | ⚠️ | Future |
| **6.6** | **Streaming** | **Ch51: Streaming Inference** | ⚠️ | Future |
| **6.7** | **Tool Use** | **Ch52: Tool Protocol** | ⚠️ | Future |
| **6.8** | **Multi-agent** | **Ch53: Agent Coordination** | ⚠️ | Future |
| **6.9** | **Expert Routing** | **Ch54: Expert Routing** | ⚠️ | Future |
| **6.10** | **Transactions** | **Ch55: Semantic Transactions** | ⚠️ | Future |

**Part VI Total: 10 LLM extensions → 4 needed + 6 future = 10 chapters**

---

## SUMMARY: Complete Chapter Plan

| Part | SICP Coverage | Current | Need | LLM-Specific | Total Chapters |
|------|---------------|---------|------|--------------|----------------|
| **I: Procedures** | SICP Ch1 | 10 | 4 | 2 | **16** |
| **II: Data** | SICP Ch2 | 8 | 4 | 0 | **12** |
| **III: State** | SICP Ch3 | 13 | 3 | 0 | **16** |
| **IV: Meta** | SICP Ch4 | 14 | 3 | 3 | **20** |
| **V: Machines** | SICP Ch5 | 0 | 3 | 0 | **3** |
| **VI: LLM Extensions** | Beyond SICP | 0 | 4 | 6 | **10** |
| **TOTAL** | | **45** | **21** | **11** | **77** |

### Breakdown:

**Current Manual:** 27 chapters (really 45 if we count Ch1-10 + Ch11-27)
- Ch1-10: Getting started + LLM basics
- Ch11-27: SICP-adapted patterns

**To Add for FULL Coverage:**
- **17 chapters** for missing SICP patterns (Ch28-45)
- **4 chapters** for core LLM features (Ch46-49)
- **6 chapters** for future LLM features (Ch50-55)

**Minimum Complete Manual:** 45 current + 21 needed = **66 chapters**

**Ultimate Complete Manual:** 45 + 21 + 11 future = **77 chapters**

---

## IMMEDIATE ACTION PLAN

### Phase 1: Complete SICP Coverage (17 chapters)
**Ch28-45** - All missing SICP patterns we've already implemented

### Phase 2: Core LLM Extensions (4 chapters)
**Ch46-49** - OPR, provenance, budgets, caching

### Phase 3: Future (6 chapters)
**Ch50-55** - Multi-modal, streaming, tools, agents, etc.

**Deliverable:** 66-chapter complete manual covering all SICP + core LLM features
