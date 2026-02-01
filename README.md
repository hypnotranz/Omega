# OmegaLLM

> **A governed, replayable semantic execution runtime for AI agents**

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**OmegaLLM is a governed, replayable semantic execution runtime.**
It *looks* like a small Lisp dialect, but the product value is the **kernel**: a controlled evaluator (step machine) where **LLM/tool calls are reified effects**, executions produce **receipts**, and runs are **debuggable, budgeted, policy‑enforced, and replayable**.

If most "LLM agents" feel like prompt glue and best‑effort scripts, OmegaLLM is the opposite: **structured computation over semantic primitives** (LLM-backed generators/predicates), with SICP‑style decomposition, validation, search, and recursion.

```lisp
;; LLM inference is not an ad-hoc API call; it's an effect boundary.
(effect infer.op "What is 2+2?")
=> "4"
```

**Repository**: [github.com/hypnotranz/OmegaLLM](https://github.com/hypnotranz/OmegaLLM)

**📖 [Read the Manual](MANUAL--STRUCTURE-AND-INTERPRETATION-OF-LINGUISTIC-PROGRAMS/)** — SICP for the Age of Language Models (27 chapters)
**🎨 [Demo Gallery](MANUAL--STRUCTURE-AND-INTERPRETATION-OF-LINGUISTIC-PROGRAMS/DEMO-GALLERY.md)** — All 49 demos with live outputs

---

## Table of Contents

- [Features](#features)
- [Why OmegaLLM](#why-omegallm)
- [Quick Start](#quick-start)
- [📖 The Manual](#-the-manual-structure-and-interpretation-of-linguistic-programs)
- [🎨 Demo Gallery](#-demo-gallery)
- [REPL Guide](#repl-guide)
- [Sessions: Persistent State for AI Agents](#sessions-persistent-state-for-ai-agents)
- [Core Primitives](#core-primitives-effects-search-streams)
- [Advanced Documentation](#advanced-documentation)
- [Contributing](#contributing)
- [License](#license)

---

## Features

### 🔒 **Governed Execution**
- **Policy enforcement**: Budget limits, security boundaries, capability restrictions
- **Receipts & provenance**: Every execution produces auditable evidence
- **Validation boundaries**: Type checking, schema validation, semantic predicates

### 🔄 **Replayable & Debuggable**
- **Step-through debugging**: Inspect machine state at every step
- **Time travel**: Rewind execution, jump to specific steps
- **Breakpoints**: Stop on step count, expression type, or effect operations
- **Session snapshots**: Save/restore entire evaluator state

### 🤖 **AI Agent-Friendly**
- **Sessioned evaluation**: Persistent state across discrete tool calls
- **Named workspaces**: Multiple isolated environments (like tmux for code)
- **Effect system**: LLM calls, file I/O, and tools are reified boundaries
- **Multi-shot search**: Generate and validate multiple candidates

### 🧩 **SICP-Style Primitives**
- **Higher-order functions**: map, filter, fold, streams
- **First-class continuations**: call/cc for non-local control
- **Lazy evaluation**: Infinite streams, delayed computation
- **Backtracking search**: amb operator for search problems

### 📊 **Semantic Computing**
- **LLM-backed predicates**: Use LLMs for validation/classification
- **Distributive inference**: Generate distributions over answers
- **Repair/retry loops**: Automatic validation and repair
- **Recursive decomposition**: Break problems into semantic subproblems

---

## Why OmegaLLM

### Chat-based LLM vs OmegaLLM (the core difference)

**Chat** is a single, linear context window:

* context grows until it blows up
* "subproblems" are informal and hard to isolate
* validation is human/manual
* "try again" adds more noise and makes drift worse
* debugging is essentially log archaeology

**OmegaLLM** is a *semantic computation plane*:

* LLM calls are **scoped** and **interruptible**
* each call can be **validated** at a boundary (types/schemas/semantic predicates)
* failures can trigger **repair/retry** or **backtracking search**
* recursion decomposes problems into bounded subcalls
* the runtime records **receipts** for audit and deterministic replay
* state persists in a named **workspace/session** across discrete invocations

In other words: **chat is conversational; OmegaLLM is executable semantics.**

---

## Quick Start

### 1. Install Dependencies

```bash
npm install
npm run build
```

### 2. Configure API Keys

Copy the example environment file and add your API keys:

```bash
cp .env.example .env
```

Then edit `.env` and add your API key(s):

```bash
# For Claude (Anthropic)
ANTHROPIC_API_KEY=sk-ant-your-actual-key-here

# For GPT-4 (OpenAI)
OPENAI_API_KEY=sk-your-actual-key-here
```

**At least one API key is required.** The `.env` file is gitignored and will never be committed.

### 3. Run the REPL

```bash
npm run omega-fast
```

REPL:

```text
Omega> (+ 1 2)
=> 3

Omega> (effect infer.op "Hello!")
=> "Hi there!"

Omega> :help
# Shows all REPL commands
```

---

## 📖 The Manual: Structure and Interpretation of Linguistic Programs

**SICP for the Age of Language Models**

The complete user manual adapts the principles of *Structure and Interpretation of Computer Programs* (SICP) for inference programming with LLMs.

### What's in the Manual

- **27 Core Chapters** — From basics to metalinguistic abstraction
- **49 Working Examples** — Every concept demonstrated with runnable code
- **SICP Principles Applied** — Higher-order functions, streams, nondeterminism, metacircular evaluation
- **Progressive Learning** — Start simple, build to advanced patterns

### Start Here

1. **[Table of Contents](MANUAL--STRUCTURE-AND-INTERPRETATION-OF-LINGUISTIC-PROGRAMS/chapters/USER-MANUAL--00--Table-Of-Contents.md)** — Navigate all 27 chapters
2. **[Introduction](MANUAL--STRUCTURE-AND-INTERPRETATION-OF-LINGUISTIC-PROGRAMS/chapters/USER-MANUAL--00--Introduction.md)** — Why this manual exists
3. **[Quick Reference](MANUAL--STRUCTURE-AND-INTERPRETATION-OF-LINGUISTIC-PROGRAMS/chapters/USER-MANUAL--00--Quick-Reference.md)** — Cheat sheet for common operations
4. **[Chapter 1: Getting Started](MANUAL--STRUCTURE-AND-INTERPRETATION-OF-LINGUISTIC-PROGRAMS/chapters/USER-MANUAL--01--Getting-Started.md)** — Your first steps

### Manual Structure

**Part I: OmegaLLM Basics (Chapters 1-10)**
- Getting started, LLM calls, composition, higher-order functions
- Nondeterministic search, multi-shot sampling, lazy streams
- The debugger, agentic REPL, full API reference

**Part II: SICP Principles for Inference (Chapters 11-27)**
- Building abstractions with semantic procedures (Ch 11-14)
- Semantic data structures (Ch 15-18)
- State and concurrency (Ch 19-23)
- Metalinguistic abstraction (Ch 24-27)

**[Read the Full Manual →](MANUAL--STRUCTURE-AND-INTERPRETATION-OF-LINGUISTIC-PROGRAMS/)**

---

## 🎨 Demo Gallery

**See all 49 demos with live LLM outputs**: **[DEMO-GALLERY.md](MANUAL--STRUCTURE-AND-INTERPRETATION-OF-LINGUISTIC-PROGRAMS/DEMO-GALLERY.md)**

Quick preview of key demos:

### Getting Started
- **[ch01-getting-started.lisp](demo/lisp/ch01-getting-started.lisp)** - Basic syntax and primitives
- **[ch02-llm-calls.lisp](demo/lisp/ch02-llm-calls.lisp)** - Your first LLM inference
- **[ch03-composition.lisp](demo/lisp/ch03-composition.lisp)** - Composing LLM calls
- **[ch04-higher-order.lisp](demo/lisp/ch04-higher-order.lisp)** - map, filter, fold over semantic operations

### Advanced Features
- **[ch05-nondeterministic.lisp](demo/lisp/ch05-nondeterministic.lisp)** - Search with amb operator
- **[ch06-multi-shot.lisp](demo/lisp/ch06-multi-shot.lisp)** - Generate multiple candidates
- **[ch07-lazy-streams.lisp](demo/lisp/ch07-lazy-streams.lisp)** - Infinite sequences (SICP Ch3)
- **[ch08-debugger.lisp](demo/lisp/ch08-debugger.lisp)** - Step-through debugging

### AI Agent Tools
- **[ch09-agentic-repl.lisp](demo/lisp/ch09-agentic-repl.lisp)** - Building agents with sessions
- **[ch13-higher-order-inference.lisp](demo/lisp/ch13-higher-order-inference.lisp)** - LLMs over LLMs
- **[ch19-conversational-state.lisp](demo/lisp/ch19-conversational-state.lisp)** - Stateful conversations

### Metalinguistic Abstractions
- **[ch21-metacircular.lisp](demo/lisp/ch21-metacircular.lisp)** - Evaluator in Omega
- **[ch24-logic-programming.lisp](demo/lisp/ch24-logic-programming.lisp)** - Prolog-style queries
- **[ch26-concurrent-inference.lisp](demo/lisp/ch26-concurrent-inference.lisp)** - Parallel LLM calls

### System Features
- **[ch33-budget-llm.lisp](demo/lisp/ch33-budget-llm.lisp)** - Budget enforcement
- **[ch34-sandbox.lisp](demo/lisp/ch34-sandbox.lisp)** - Sandboxed execution
- **[auto-traceability.lisp](demo/lisp/auto-traceability.lisp)** - Requirements tracing

**Run any demo**:
```bash
npm run manual 1    # Run chapter 1
npm run manual 8    # Run chapter 8 (debugger demo)
```

---

## REPL Guide

### Starting the REPL

```bash
npm run omega-fast           # Fast build, no type checking
npm run omega                # Full build with type checking
npm run omega-repl           # REPL only (after building)
```

### REPL Commands Reference

#### Basic Commands
```text
:help, :h              Show all REPL commands
:quit, :q              Exit the REPL
:env [name]            Show environment bindings
:defs                  Show all user definitions
```

#### Execution Control
```text
:step [n]              Execute n steps (default: 1)
:run, :continue, :c    Run to completion or next breakpoint
:stop                  Stop current execution
:state, :st            Show current machine state (CEKS)
:control               Show control expression
:stack                 Show continuation stack
```

#### Debugging
```text
:debug <expr>          Start debugging an expression
:break step N          Set breakpoint at step N
:break expr TAG        Break when evaluating expression with TAG
:break effect OP       Break when performing effect OP
:breaks, :breakpoints  List all breakpoints
:delbreak <id>         Delete breakpoint
:toggle <id>           Enable/disable breakpoint
```

#### Time Travel
```text
:trace                 Show execution trace
:goto <step>           Jump to specific step in trace
:back [n]              Rewind n steps (default: 1)
:history [n]           Show recent execution history
```

#### Snapshots & Persistence
```text
:save <name>           Save current state as snapshot
:restore <name>        Load snapshot
:snapshots, :snaps     List all snapshots
:export <name> <file>  Export snapshot to file
```

#### Recording
```text
:record on|off         Toggle trace recording
:dump <file>           Save trace to file
:load <file>           Load trace from file
```

### Example REPL Session

```lisp
Omega> (define (factorial n)
         (if (= n 0) 1 (* n (factorial (- n 1)))))
=> <closure>

Omega> (factorial 5)
=> 120

Omega> :defs
;; Shows all definitions including factorial

Omega> :debug (factorial 3)
;; Enters step-by-step debugger

Omega> :break step 5
;; Set breakpoint at step 5

Omega> :run
;; Runs until breakpoint

Omega> :state
;; Shows CEKS machine state

Omega> :save my-session
;; Saves entire state

Omega> :quit
```

---

## Sessions: Persistent State for AI Agents

**Why sessions matter**: Most LLM agent frameworks treat each tool call as isolated. OmegaLLM gives agents **persistent, named workspaces** that survive across discrete invocations.

### The Problem with Stateless Tool Calls

Traditional approach:
```python
# Agent calls tool multiple times, but state is lost each time
agent.call_tool("eval", "(define x 10)")     # x defined
agent.call_tool("eval", "(+ x 5)")           # ERROR: x is undefined!
```

### OmegaLLM's Solution: Named Sessions

With sessions, state persists:
```bash
# First tool call
omega --session agent1 -e "(define x 10)"
# => x defined in session "agent1"

# Second tool call (minutes or hours later)
omega --session agent1 -e "(+ x 5)"
# => 15  (x is still there!)

# Third tool call
omega --session agent1 -e "(define (foo n) (* n x))"
# => <closure>

# Fourth tool call
omega --session agent1 -e "(foo 3)"
# => 30
```

### What's in a Session?

A session is **not** just a variable dictionary. It's a complete, **persistent evaluator context** with full debugging capabilities:

#### 💾 Persistent State
- **Environment** - All bindings (variables, functions) survive across invocations
- **Store** - All allocated values remain in memory
- **Code** - Your definitions persist between calls

#### 🔍 Fully Traceable
- **Execution trace** - Every step recorded
- **Receipts** - Provenance records for every LLM call and effect
- **History** - Complete audit trail of what happened

#### 🐛 Debuggable & Steppable
- **Step through execution** - Execute one step at a time (`:step`)
- **Breakpoints** - Pause on specific steps, expressions, or effects
- **Inspect state** - See control, environment, store, continuation at any point (`:state`)
- **Time travel** - Rewind and replay execution (`:back`, `:goto`)

#### 🔄 Replayable
- **Deterministic replay** - Recreate exact execution from trace
- **Save/restore** - Snapshot entire session state (`:save`, `:restore`)
- **Export traces** - Save execution history to files for analysis

### Session Commands

```bash
# Create/use named session
omega --session <name> -e "<expression>"

# Multiple sessions (isolated)
omega --session agent1 -e "(define x 10)"
omega --session agent2 -e "(define x 99)"  # Different x!
omega --session agent1 -e "x"              # => 10
omega --session agent2 -e "x"              # => 99

# Sessions persist on disk
# Location: .omega-sessions/<session-name>/
```

### Sessions in the REPL

```bash
# Start REPL with named session
omega --session my-work

# Or in npm scripts
npm run omega-fast -- --session my-work
```

Inside REPL:
```text
Omega> (define data (list 1 2 3))
Omega> :save checkpoint1
Omega> :quit

# Later, in new REPL session
$ omega --session my-work
Omega> data              # Still there!
=> (1 2 3)
Omega> :restore checkpoint1  # Can restore snapshots
```

### Why This Matters for AI Agents

**Use case**: Code review agent

```bash
# Step 1: Agent scans codebase
omega --session review-123 -e "(define files (scan-directory \"src/\"))"

# Step 2: Agent analyzes patterns (5 minutes later)
omega --session review-123 -e "(define issues (analyze-files files))"

# Step 3: Agent generates report (user makes edits, agent resumes)
omega --session review-123 -e "(generate-report issues)"

# Step 4: Agent can resume even after hours
omega --session review-123 -e "(length issues)"  # Still works!
```

**The evaluator state is durable**, like a tmux session for code execution.

See **[ARCHITECTURE/28-SESSION.md](ARCHITECTURE/28-SESSION.md)** for implementation details.

---

## Core Primitives: Effects, Search, Streams

### Effects: Reified Boundaries (Commands, not ad-hoc side effects)

```lisp
;; LLM inference
(effect infer.op (list "Summarize in one sentence: " text))

;; Multi-shot sampling (distribution of candidates)
(effect search.op (list "Rewrite in three tones: " request))

;; Tooling (policy may gate these)
(effect file.read.op  "path/to/file.txt")
(effect file.write.op "path/to/file.txt" "content")
(effect shell.op "ls -la")
```

### Environment (LLM adapters)

#### Option 1: .env file (Recommended)

Create a `.env` file (copy from `.env.example`):

```bash
ANTHROPIC_API_KEY=sk-ant-your-key-here
OPENAI_API_KEY=sk-your-key-here

# Optional: select adapter/model
OMEGA_ADAPTER=openai        # or 'anthropic'
OMEGA_MODEL=gpt-4o          # override model
```

#### Option 2: Environment variables

```bash
export OPENAI_API_KEY=sk-...
export ANTHROPIC_API_KEY=sk-ant-...
export OMEGA_ADAPTER=openai        # or 'anthropic'
export OMEGA_MODEL=gpt-4o          # override model
```

**At least one API key is required.** The runtime checks `ANTHROPIC_API_KEY` first, then `OPENAI_API_KEY`.

### Higher-Order Semantic Functions

```lisp
;; Map LLM over data
(map (lambda (text) (effect infer.op (list "Sentiment: " text)))
     (list "I love this!" "This is terrible." "It's okay."))
=> ("positive" "negative" "neutral")

;; Filter with LLM predicate
(filter (lambda (code)
          (effect infer.op (list "Is this code secure? yes/no: " code)))
        code-samples)

;; Fold with validation
(fold-left
  (lambda (acc item)
    (if (valid? item) (cons item acc) acc))
  (list)
  items)
```

### Backtracking Search (amb)

```lisp
;; Generate and test
(define (solve-puzzle)
  (let ((a (amb 1 2 3))
        (b (amb 4 5 6)))
    (require (= (+ a b) 7))
    (list a b)))

(solve-puzzle)
=> (1 6)  ; or (2 5) or (3 4) - all valid solutions
```

### Lazy Streams (SICP)

```lisp
;; Infinite stream
(define (integers-from n)
  (stream-cons n (integers-from (+ n 1))))

(define nats (integers-from 1))

(stream-take 5 nats)
=> (1 2 3 4 5)

;; Filter infinite stream
(define evens (stream-filter even? nats))
(stream-take 3 evens)
=> (2 4 6)
```

---

## Advanced Documentation

### Architecture Specifications

For developers who want to understand the implementation, OmegaLLM has **50+ architecture documents** covering every aspect of the system:

### Core Specifications
- **[00-SPECIFICATION.md](ARCHITECTURE/00-SPECIFICATION.md)** - Language specification
- **[01-READER.md](ARCHITECTURE/01-READER.md)** - Parser and reader
- **[02-TYPES.md](ARCHITECTURE/02-TYPES.md)** - Type system
- **[04-EVALUATOR.md](ARCHITECTURE/04-EVALUATOR.md)** - CEKS machine evaluator

### Advanced Features
- **[05-CONTINUATIONS.md](ARCHITECTURE/05-CONTINUATIONS.md)** - First-class continuations (call/cc)
- **[06-CONDITIONS.md](ARCHITECTURE/06-CONDITIONS.md)** - Non-unwinding condition system
- **[11-MACROS.md](ARCHITECTURE/11-MACROS.md)** - Macro system
- **[12-CONCURRENCY.md](ARCHITECTURE/12-CONCURRENCY.md)** - Concurrent execution

### LLM Integration
- **[32-LANGUAGE-OFFICIAL-1.md](ARCHITECTURE/32-LANGUAGE-OFFICIAL-1.md)** - Official language spec
- **[33-MEANING.md](ARCHITECTURE/33-MEANING.md)** - Semantic meaning values
- **[34-DISTRIBUTIONS.md](ARCHITECTURE/34-DISTRIBUTIONS.md)** - Probability distributions

### Governance & Security
- **[21-SECURITY.md](ARCHITECTURE/21-SECURITY.md)** - Security model
- **[22-BUDGET.md](ARCHITECTURE/22-BUDGET.md)** - Budget enforcement
- **[23-POLICIES.md](ARCHITECTURE/23-POLICIES.md)** - Policy system

### Debugging & Tooling
- **[15-DIAGNOSTICS.md](ARCHITECTURE/15-DIAGNOSTICS.md)** - Debugging tools
- **[16-TOOLING.md](ARCHITECTURE/16-TOOLING.md)** - Developer tools
- **[28-SESSION.md](ARCHITECTURE/28-SESSION.md)** - Session management

**Browse all docs**: [ARCHITECTURE/](ARCHITECTURE/)

---

## Project Structure

```
OmegaLLM/
├── ARCHITECTURE/          # 50+ specification documents
├── src/                   # TypeScript implementation
│   ├── core/             # Evaluator, primitives, AST
│   ├── effects/          # Effect handlers
│   ├── oracle/           # LLM adapters
│   └── provenance/       # Receipt system
├── demo/                  # Examples and demos
│   └── lisp/             # 35 working Lisp demos
├── test/                  # Unit and integration tests
│   ├── unit/             # Unit tests
│   └── integration/      # Integration tests
├── bin/                   # CLI entry points
│   ├── omega.ts          # Main CLI
│   ├── omega-repl.ts     # REPL
│   └── manual-runner.ts  # Demo runner
├── .env.example          # API key template
└── README.md             # This file
```

---

## CLI Options

```bash
omega [options]                     # Start interactive REPL
omega [options] <file>              # Execute Omega file
omega --eval <code>                 # Evaluate code directly
omega --session <name> -e <expr>    # Evaluate in named session

Options:
  -h, --help                        Show help
  -v, --version                     Show version
  -e, --eval <code>                 Evaluate code and exit
  -d, --debug                       Enable debug mode
  --verbose                         Show detailed execution
  -s, --session <name>              Set session name for persistence
```

Examples:
```bash
# Interactive REPL
omega

# Execute file
omega program.lisp

# One-off evaluation
omega --eval "(+ 1 2 3)"

# Sessioned evaluation (for agents)
omega --session agent1 -e "(define x 10)"
omega --session agent1 -e "(+ x 5)"

# Debug mode
omega --debug --session debug1
```

---

## Contributing

We welcome contributions! Please:

1. Read the [ARCHITECTURE/](ARCHITECTURE/) docs to understand the system
2. Check existing issues and PRs
3. Write tests for new features
4. Follow the existing code style
5. Update documentation

---

## License

MIT License - see [LICENSE](LICENSE) file for details

---

## Resources

- **📖 User Manual**: [Structure and Interpretation of Linguistic Programs](MANUAL--STRUCTURE-AND-INTERPRETATION-OF-LINGUISTIC-PROGRAMS/)
- **🎨 Demo Gallery**: [All 49 demos with outputs](MANUAL--STRUCTURE-AND-INTERPRETATION-OF-LINGUISTIC-PROGRAMS/DEMO-GALLERY.md)
- **🏗️ Architecture Docs**: [ARCHITECTURE/](ARCHITECTURE/) (50+ specs for developers)
- **💬 Issues**: [GitHub Issues](https://github.com/hypnotranz/OmegaLLM/issues)
- **🗣️ Discussions**: [GitHub Discussions](https://github.com/hypnotranz/OmegaLLM/discussions)

---

**Built with inspiration from**:
- SICP (Structure and Interpretation of Computer Programs)
- Scheme R7RS specification
- Modern LLM agent frameworks
- Formal verification systems

**OmegaLLM**: Where semantic computing meets structured inference. 🚀
