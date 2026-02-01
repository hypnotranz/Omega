# Structure and Interpretation of Linguistic Programs

**SICP for the Age of Language Models**

*Adapting Abelson and Sussman's classic for inference programming with LLMs*

---

## 🎯 What This Is

This manual adapts the principles and pedagogy of **Structure and Interpretation of Computer Programs** (SICP) to a new paradigm: **inference programming** with large language models.

### SICP (1985)
> "Programs are not just instructions for machines—they are expressions of ideas."
>
> *— Abelson and Sussman*

SICP taught computational thinking through:
- Abstraction and composition
- Higher-order procedures
- Streams and lazy evaluation
- Nondeterministic search
- Metalinguistic abstraction (building evaluators)

### This Manual (2026)
> "Language models give computers the ability to understand meaning. What happens when we apply SICP's principles to programs that invoke understanding?"

**Same principles. New primitive: semantic understanding.**

---

## 🔄 The SICP → Inference Mapping

| SICP Concept | Inference Equivalent | Chapter |
|--------------|---------------------|---------|
| **Numbers, strings** | **Semantic effects** | Ch 2, 11 |
| *Primitive data* | *`(effect infer.op "prompt")`* | |
| **Lambda procedures** | **Semantic procedures** | Ch 11-13 |
| *Functions that compute* | *Functions that ask and understand* | |
| **Higher-order functions** | **Higher-order inference** | Ch 4, 13 |
| *`map`, `filter`, `reduce`* | *Composing semantic operations* | |
| **Sequences** | **Semantic sequences** | Ch 15 |
| *Lists of data* | *Streams of meanings* | |
| **Nondeterminism (AMB)** | **AMB + LLM constraints** | Ch 5, 26 |
| *Backtracking search* | *Semantic constraint satisfaction* | |
| **Streams** | **Streams of inference** | Ch 7, 23 |
| *Lazy evaluation* | *Lazy semantic evaluation* | |
| **Meta-circular evaluator** | **Oracle-in-evaluator** | Ch 24 |
| *Eval/apply with data* | *Eval/apply with understanding* | |
| **State and assignment** | **Conversational state** | Ch 19-21 |
| *Mutable variables* | *Persistent semantic memory* | |
| **Concurrency** | **Concurrent inference** | Ch 22 |
| *Parallel computation* | *Parallel semantic operations* | |
| **Logic programming** | **Semantic logic** | Ch 27 |
| *Prolog-style queries* | *LLM-guided fact derivation* | |

**The core idea**: Replace computational primitives with **semantic primitives** that invoke understanding.

---

## 📁 What's In This Folder

```
MANUAL--STRUCTURE-AND-INTERPRETATION-OF-LINGUISTIC-PROGRAMS/
│
├── README.md                          ← YOU ARE HERE
│
├── chapters/                          ← 📖 THE TEXTBOOK
│   │                                     27 chapters + appendices
│   │                                     ~5,800 lines of SICP-adapted pedagogy
│   │
│   ├── USER-MANUAL.md                    Main entry point
│   ├── USER-MANUAL--00--Table-Of-Contents.md
│   ├── USER-MANUAL--00--Introduction.md
│   ├── USER-MANUAL--00--Quick-Reference.md
│   │
│   ├── USER-MANUAL--01--Getting-Started.md
│   ├── USER-MANUAL--02--Llm-Calls-As-Functions.md
│   ├── ... (Chapters 3-27)
│   │
│   ├── USER-MANUAL--91--Appendix-A-Configuration.md
│   ├── USER-MANUAL--92--Appendix-B-Design-Philosophy.md
│   └── USER-MANUAL--99--Epilogue-The-Structure-Of-Understanding.md
│
├── code-examples/                     ← 💻 RUNNABLE CODE FOR EACH CHAPTER
│   │
│   ├── lisp/                             For users to run and learn
│   │   ├── ch01-getting-started.lisp        Chapter 1 code
│   │   ├── ch02-llm-calls.lisp              Chapter 2 code
│   │   ├── ch03-composition.lisp
│   │   ├── ... (ch04-ch27.lisp)             27 total examples
│   │   │
│   │   │   Each file is a working demonstration of
│   │   │   that chapter's concepts in OmegaLLM Lisp
│   │   │
│   │   └── Run with:
│   │       npm run omega-repl -- --file code-examples/lisp/ch01-getting-started.lisp
│   │
│   └── typescript/                       TypeScript demo configurations
│       ├── ch01-getting-started.ts          For test harness validation
│       ├── ch02-llm-calls.ts
│       ├── ... (ch03-ch27.ts)
│       ├── config.ts
│       ├── shared.ts
│       └── specs.ts
│
├── demo-tests/                        ← ✅ AUTOMATED VALIDATION
│   │                                     Ensures all demos work
│   │
│   ├── by-chapter.spec.ts                Tests all 27 chapter demos pass
│   └── wow-pack.spec.ts                  Tests advanced showcase demos
│       │
│       │   Run with: npm test MANUAL*/demo-tests/
│       │
│
└── doc-tests/                         ← ✅ DOCUMENTATION VALIDATION
    └── ... (ensures docs are accurate)
```

---

## 📚 The Manual Structure (27 Chapters)

### Part I: OmegaLLM Basics (Chapters 1-10)

**Getting started with inference programming**

- **Chapter 1**: Getting Started — REPL, sessions, first LLM call
- **Chapter 2**: LLM Calls as Functions — `(effect infer.op "prompt")`
- **Chapter 3**: Functional Composition — Combining semantic operations
- **Chapter 4**: Higher-Order LLM Functions — Functions that return semantic operations
- **Chapter 5**: Nondeterministic Search — AMB for exploring possibilities
- **Chapter 6**: Multi-Shot Sampling — Multiple LLM calls, best answer
- **Chapter 7**: Lazy Streams — Infinite semantic sequences
- **Chapter 8**: The Debugger — Stepping through inference
- **Chapter 9**: The Agentic REPL — LLM as interactive partner
- **Chapter 10**: Full API Reference — Complete language reference

### Part II: SICP Principles for Inference (Chapters 11-27)

**Direct adaptation of SICP structure**

#### Building Abstractions with Semantic Procedures (SICP Chapter 1 → Our Ch 11-14)
- **Chapter 11**: Semantic Procedures as Black Boxes
  - *Corresponds to SICP 1.1: The Elements of Programming*
  - Primitive expressions, means of combination, means of abstraction
  - **But**: Primitives are semantic, not numeric
- **Chapter 12**: Inference Processes: Recursion and Iteration in Semantic Space
  - *Corresponds to SICP 1.2: Procedures and Processes*
  - Recursive vs iterative semantic processes
- **Chapter 13**: Higher-Order Inference
  - *Corresponds to SICP 1.3: Formulating Abstractions*
  - Procedures as arguments, procedures as return values
- **Chapter 14**: Semantic Data Abstraction
  - *Corresponds to SICP 2.1: Introduction to Data Abstraction*
  - Abstract semantic data types

#### Semantic Data Structures (SICP Chapter 2 → Our Ch 15-18)
- **Chapter 15**: Sequences as Semantic Interfaces
  - *Corresponds to SICP 2.2: Hierarchical Data*
  - Map, filter, reduce over semantic sequences
- **Chapter 16**: Symbolic Semantic Data
  - *Corresponds to SICP 2.3: Symbolic Data*
  - Symbols representing meanings
- **Chapter 17**: Multiple Representations of Meaning
  - *Corresponds to SICP 2.4: Multiple Representations*
  - Different models of the same concept
- **Chapter 18**: Generic Semantic Operations
  - *Corresponds to SICP 2.5: Systems with Generic Operations*
  - Dynamic dispatch, method synthesis via LLM

#### State and Concurrency (SICP Chapter 3 → Our Ch 19-23)
- **Chapter 19**: Conversational State and Memory
  - *Corresponds to SICP 3.1: Assignment and Local State*
  - Stateful semantic procedures
- **Chapter 20**: The Semantic Environment Model
  - *Corresponds to SICP 3.2: The Environment Model*
  - How bindings and state work
- **Chapter 21**: Mutable Semantic Structures
  - *Corresponds to SICP 3.3: Modeling with Mutable Data*
  - Changing meanings over time
- **Chapter 22**: Concurrent Inference
  - *Corresponds to SICP 3.4: Concurrency*
  - Parallel semantic operations, caching
- **Chapter 23**: Streams of Inference
  - *Corresponds to SICP 3.5: Streams*
  - Infinite semantic streams, delayed evaluation

#### Metalinguistic Abstraction (SICP Chapters 4-5 → Our Ch 24-27)
- **Chapter 24**: Metalinguistic Abstraction: The Oracle in the Evaluator
  - *Corresponds to SICP 4.1: The Metacircular Evaluator*
  - **Key innovation**: Eval/apply can suspend and ask LLM for guidance
- **Chapter 25**: Lazy Semantic Evaluation
  - *Corresponds to SICP 4.2: Lazy Evaluation*
  - Normal-order evaluation with semantic effects
- **Chapter 26**: The AMB Inference Engine
  - *Corresponds to SICP 4.3: Nondeterministic Computing*
  - AMB + LLM = semantic constraint satisfaction
- **Chapter 27**: Logic Programming with Semantic Facts
  - *Corresponds to SICP 4.4: Logic Programming*
  - Prolog-style queries with LLM-derived facts

---

## 🚀 How to Use This Manual

### For Users Learning OmegaLLM:

1. **Start with the Table of Contents**:
   ```bash
   cat chapters/USER-MANUAL--00--Table-Of-Contents.md
   ```

2. **Read Chapter 1**:
   ```bash
   cat chapters/USER-MANUAL--01--Getting-Started.md
   ```

3. **Run the Chapter 1 example**:
   ```bash
   npm run omega-repl -- --file code-examples/lisp/ch01-getting-started.lisp
   ```

4. **Continue through chapters 2-27**, reading each chapter and running its code example

5. **Reference the Quick Reference**:
   ```bash
   cat chapters/USER-MANUAL--00--Quick-Reference.md
   ```

### For Developers:

1. **Validate all chapter demos work**:
   ```bash
   npm test MANUAL*/demo-tests/by-chapter.spec.ts
   ```

2. **Check documentation validity**:
   ```bash
   npm test MANUAL*/doc-tests/
   ```

3. **Run all validation tests**:
   ```bash
   npm test MANUAL*/
   ```

---

## 🎓 Key Concepts: SICP → Inference

### 1. Primitives
**SICP**: `42`, `"hello"`, `+`, `*`
**Inference**: `(effect infer.op "What is the capital of France?")`

### 2. Combination
**SICP**: `(+ 1 2)`
**Inference**: `(effect infer.op (list "Translate: " (effect infer.op "Say hello")))`

### 3. Abstraction
**SICP**: `(define (square x) (* x x))`
**Inference**: `(define (sentiment text) (effect infer.op (list "Sentiment of: " text)))`

### 4. Higher-Order
**SICP**: `(map square (list 1 2 3))`
**Inference**: `(map (lambda (doc) (effect infer.op (list "Summarize: " doc))) documents)`

### 5. Nondeterminism
**SICP**: `(amb 1 2 3)` — try each value
**Inference**: `(amb strategies)` + `(require (good-enough? result))` — LLM evaluates constraint

### 6. Metacircular
**SICP**: Evaluator written in Scheme
**Inference**: Evaluator that can ask LLM "what should I do next?"

---

## 🔑 The Central Innovation

From Chapter 24 (The Oracle in the Evaluator):

> "The evaluator can **suspend** during eval/apply and ask the LLM for guidance.
> The LLM can evaluate subexpressions, observe the environment, and return results.
> This creates a **co-recursive loop**: Lisp ↔ LLM ↔ Lisp."

**SICP's eval/apply cycle**:
```scheme
(eval expr env)  → (apply proc args)  → (eval body env')  → ...
```

**OmegaLLM's eval/apply cycle with Oracle**:
```scheme
(eval expr env)
  → SUSPEND → ask LLM "what does this mean?"
  → LLM calls (eval subexpr env)
  → RESUME with LLM's answer
  → (apply proc args)
  → ...
```

This is **metalinguistic abstraction meets inference**: the language understands itself through understanding.

---

## 📖 From the Epilogue (Chapter 99)

> "We began with a simple observation: language models can understand meaning.
> We end with a new kind of programming—one where computation and comprehension intertwine.
>
> The lessons of SICP—abstraction, composition, laziness, nondeterminism, metalinguistic power—all apply, but transformed.
> Where Abelson and Sussman built evaluators for algorithms, we build evaluators for inference.
> Where they abstracted over procedures, we abstract over understanding.
>
> The programs in this book are not just instructions for computation.
> They are **invocations of meaning**."
>
> *— Adapted from Abelson and Sussman*

---

## 🌟 Why This Matters

**SICP (1985)** changed how we think about programming by showing that programs express ideas.

**This manual (2026)** shows that when programs can invoke understanding, those ideas become even more powerful.

You're not just learning a language. You're learning a new way to think about computation itself.

**Welcome to the structure and interpretation of inference programs.**

---

## 📂 Related Materials

**Outside this manual folder** (in OmegaLLM root):

- `src/` — Runtime implementation (evaluator, compiler, oracle, 31 subsystems)
- `test/` — Runtime unit tests (separate from manual validation tests)
- `demo/` — Additional showcase demos (omega-wow pack, 11 advanced demos)
- `ARCHITECTURE/` — Architecture documentation (69 files)
- `CLAUDE-JOBS/` — Planning documents

**This manual folder** is self-contained and includes everything a user needs to learn inference programming through the SICP paradigm.

---

*Start with `chapters/USER-MANUAL--00--Table-Of-Contents.md`*
