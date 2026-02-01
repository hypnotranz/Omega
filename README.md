# OmegaLLM

> **A governed, replayable semantic execution runtime for AI agents**

## ⚡ TL;DR - Get Running in 60 Seconds

```bash
npm install && npm run build          # Install & build
echo "OPENAI_API_KEY=sk-..." > .env   # Add your API key
npm run omega-fast                     # Start REPL
```

```text
Omega> :help                           # SEE ALL COMMANDS FIRST
Omega> (+ 1 2)                         # Basic math
=> 3
Omega> (effect infer.op "Hello!")      # Call LLM
=> "Hi there!"
Omega> npm run manual 5                # Run demo (in another terminal)
```

**📖 [Demo Index](DEMO-INDEX.md)** — **All 39 demos organized by category** ← Start here!
**📖 [Demo Gallery](MANUAL--STRUCTURE-AND-INTERPRETATION-OF-LINGUISTIC-PROGRAMS/DEMO-GALLERY.md)** — See 49 working demos with live LLM outputs
**📖 [Full Manual](MANUAL--STRUCTURE-AND-INTERPRETATION-OF-LINGUISTIC-PROGRAMS/)** — 27 chapters, SICP for LLMs

---

## What Is This?

**OmegaLLM is a governed, replayable semantic execution runtime.**
It *looks* like a small Lisp dialect, but the product value is the **kernel**: a controlled evaluator (step machine) where **LLM/tool calls are reified effects**, executions produce **receipts**, and runs are **debuggable, budgeted, policy‑enforced, and replayable**.

If most "LLM agents" feel like prompt glue and best‑effort scripts, OmegaLLM is the opposite: **structured computation over semantic primitives** (LLM-backed generators/predicates), with SICP‑style decomposition, validation, search, and recursion.

```lisp
;; LLM inference is not an ad-hoc API call; it's an effect boundary.
(effect infer.op "What is 2+2?")
=> "4"
```

**Repository**: [github.com/hypnotranz/Omega](https://github.com/hypnotranz/Omega)

---

## Table of Contents

- [⚡ TL;DR](#-tldr---get-running-in-60-seconds)
- [🔥 Show Me The Cool Stuff](#-show-me-the-cool-stuff) ← **Start here!**
- [Quick Start](#quick-start)
- [⚠️ Common Gotchas](#️-common-gotchas)
- [Features at a Glance](#features-at-a-glance)
- [📖 The Manual](#-the-manual-structure-and-interpretation-of-linguistic-programs) (27 chapters)
- [🎨 Demo Gallery](#-demo-gallery) (49 demos)
- [REPL Guide](#repl-guide)
- [Sessions: Persistent State](#sessions-persistent-state-for-ai-agents)
- [Core Primitives](#core-primitives-effects-search-streams)
- [CLI Options](#cli-options)

---

## Features at a Glance

| Category | What You Get |
|----------|--------------|
| **LLM Calls** | `(effect infer.op "prompt")` — LLM inference as a first-class operation |
| **Agentic Mode** | `:ask "question"` — LLM with tool-use that can eval code iteratively |
| **Higher-Order** | `map`, `filter`, `fold` over LLM operations |
| **Backtracking** | `amb` operator — generate candidates, validate, auto-backtrack on failure |
| **Lazy Streams** | Infinite sequences, only force what you need |
| **Debugger** | `:debug`, `:step`, `:break`, `:state` — step through execution |
| **Time Travel** | `:goto N`, `:back`, `:trace` — jump to any point in execution |
| **Sessions** | `:session save/load/goto` — persistent state across restarts |
| **Snapshots** | `:save`, `:restore` — checkpoint and restore evaluator state |
| **Receipts** | Every LLM call produces auditable provenance |
| **OPR Kernels** | `:opr-run` — run structured inference programs |
| **Budget/Policy** | Enforce spending limits and capability restrictions |

**Run `:help` in the REPL to see all commands.**

---

## 🔥 Show Me The Cool Stuff

### 1. Structured Data Extraction (with confidence + source citations!)

```bash
npm run omega-fast -- --cmd ':opr-run opr.extract.v1 {"text":"John Smith (john@example.com) called about order #12345 on Jan 15.","schema":{"name":"string","email":"string","order_id":"string"}}'
```

**Output:**
```json
{
  "data": { "name": "John Smith", "email": "john@example.com", "order_id": "12345" },
  "confidence": { "name": 0.98, "email": 0.95, "order_id": 0.90 },
  "sources": {
    "name": "line 1: 'John Smith'",
    "email": "line 1: '(john@example.com)'",
    "order_id": "line 1: 'order #12345'"
  }
}
```

### 2. Agentic Mode - LLM Writes & Runs Code

```text
Omega> :ask "Define a fibonacci function and compute fib(10)"
; LLM writes: (define (fib n) ...)
; LLM evals: (fib 10)
; LLM sees result: 55
Answer: The result of fib(10) is 55.
```

### 3. Backtracking Search with LLM Validation

```lisp
;; Try tones until LLM confirms it matches "apologetic"
(let ((tone (amb "formal" "friendly" "apologetic")))
  (let ((reply (effect infer.op (list "Write a " tone " response..."))))
    (require (matches-tone? reply "apologetic"))
    reply))
;; Auto-backtracks through options until validation passes!
```

### 4. 10 Built-in OPR Kernels

```text
Omega> :opr-list
  opr.classify.v1   — Classify with confidence scores
  opr.extract.v1    — Extract structured data with sources
  opr.analyze.v1    — Analyze text/code
  opr.transform.v1  — Transform content
  opr.validate.v1   — Validate against criteria
  opr.plan.v1       — Generate plans
  ... and more
```

### 5. Full Debugger with Time Travel

```text
Omega> :debug (+ (* 2 3) (* 4 5))
Omega> :run
=== DONE at step 22 === Result: 26

Omega> :trace
  [0] Expr: Begin(1 exprs)
  [5] Expr: Var(*) | stack=3
  [10] Value: 3 | stack=3
  [19] Value: 20 | stack=2
  ...

Omega> :goto 10        ;; Jump back in time!
Control: Value: 3
```

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

### 🤖 **Perfect for AI Agents**

Why AI agents love OmegaLLM:

- **Persistent sessions** — Your definitions, functions, and state survive across tool calls. The agent sees exactly what programs you've already written in the session. No need to re-explain context every time.

- **Query the runtime** — The Oracle can evaluate subexpressions, inspect the environment, and get actual runtime values before responding. Not a one-shot completion—an **interactive coroutine** that reasons about live code.

- **Traceable execution** — Every step recorded with full provenance. The agent can inspect what happened, debug failures, and understand exactly where things went wrong. Complete audit trail of every LLM call and effect.

- **Deterministic replay** — Same inputs = same outputs. Agents can confidently retry failed operations knowing the behavior is reproducible. Save session snapshots and restore them later.

- **Session isolation** — Multiple named workspaces (`omega --session agent1`, `omega --session agent2`). Like tmux for code—work on different tasks without interference.

- **Effect boundaries** — LLM calls are reified as `(effect infer.op ...)`. Clean separation between computation and inference. Budget enforcement. Policy compliance.

- **Interactive debugging** — Step through execution (`:step`), set breakpoints, inspect state at any point (`:state`). Time travel (`:back`, `:goto`). Agents can diagnose issues systematically.

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

**First thing: Type `:help` to see what you can do:**

```text
Omega> :help
# Shows all REPL commands - debugging, sessions, execution control, etc.
```

Try some examples:

```text
Omega> (+ 1 2)
=> 3

Omega> (effect infer.op "Hello!")
=> "Hi there!"

Omega> :state
# Inspect the full evaluator state (control, environment, store, continuation)
```

### 4. Run the Demos

**Most impressive demo (start here!):**
```bash
npm run demo        # Showcase: Higher-order functions, backtracking search, agentic LLM!
```

This demo shows:
- ✓ Map/filter over LLM operations
- ✓ Backtracking search with `amb` + semantic validation
- ✓ Agentic LLM that queries your code to answer questions

**More examples:**
```bash
npm run manual 1    # Chapter 1: Getting Started
npm run manual 5    # Chapter 5: Backtracking search with amb
npm run manual 7    # Chapter 7: Lazy streams
npm run manual 8    # Chapter 8: The debugger
```

**See all 49 demos**: [DEMO-GALLERY.md](MANUAL--STRUCTURE-AND-INTERPRETATION-OF-LINGUISTIC-PROGRAMS/DEMO-GALLERY.md)

---

## ⚠️ Common Gotchas

Things that will trip you up:

### 1. Run `:help` first!
The REPL has tons of commands. Type `:help` immediately to see debugging, sessions, breakpoints, time travel, etc.

### 2. CLI flag is `--cmd`, not `--eval`
```bash
# WRONG
npm run omega-fast -- --eval "(+ 1 2)"

# RIGHT
npm run omega-fast -- --cmd "(+ 1 2)"
```

### 3. Sessions require explicit save/load/goto
The `--session` flag just names the session for recording. It does NOT auto-restore state.

**To persist state across process restarts:**
```text
# Session 1: Save before quitting
Omega> (define x 42)
Omega> :session save mywork
Omega> :quit

# Session 2: Load AND goto to restore
Omega> :session load mywork
Omega> :session goto 3        # <-- THIS restores the environment!
Omega> x
=> 42
```

`:session load` only loads the trace. `:session goto <seq>` actually restores the environment.

### 4. Session files location
Sessions are stored in `.omega-session/sessions/` (not `.omega-sessions/`):
```
.omega-session/sessions/<name>.jsonl       # Event log
.omega-session/sessions/<name>.index.json  # Index with checkpoints
```

### 5. Use the Demo Gallery!
Don't guess at syntax. The [Demo Gallery](MANUAL--STRUCTURE-AND-INTERPRETATION-OF-LINGUISTIC-PROGRAMS/DEMO-GALLERY.md) has 49 working examples with actual LLM outputs.

```bash
npm run manual 5   # See amb backtracking in action
npm run manual 7   # See lazy streams
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
- **[agent-security-audit.lisp](demo/lisp/agent-security-audit.lisp)** - 🔥 Map security analysis over entire codebase (vs 100 serial tool calls!)
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

**The REPL is the primary way to use OmegaLLM.** Start here.

### Starting the REPL

```bash
npm run omega-fast           # Recommended: Fast build, no type checking
npm run omega                # Full build with type checking
npm run omega-repl           # REPL only (after building)
```

**First command you should run: `:help`**

This shows ALL available REPL commands - debugging, execution control, sessions, breakpoints, time travel, and more.

### REPL Commands Reference

#### Basic Commands
```text
:help, :h              **RUN THIS FIRST** - Shows all REPL commands
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
:replay <file>         Load and replay trace from file
```

#### File Loading
```text
:loadfile <path>       Load and evaluate code from file
```

#### Agentic LLM Mode
```text
:ask <question>        Ask LLM with tool-use (it can eval code iteratively!)
:traces                List recent LLM interaction traces
:trace <id>            Show trace summary
:trace <id> -v         Show full trace (prompts, responses, tool calls)
```

#### OPR (Omega Protocol Runtime)
```text
:opr-list              List available OPR kernels
:opr-run <kernel> <json>  Run kernel with program JSON
:opr-receipts          Show OPR receipt chain for session
:opr-verify [file]     Verify OPR receipt chain integrity
```

#### Advanced Inspection
```text
:stack                 Show call stack
:frame <n>             Inspect stack frame N
:control               Show current control expression/value
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

**Within a single REPL session**, state persists naturally:
```lisp
Omega> (define x 42)
=> x
Omega> (+ x 10)
=> 52
```

**Across separate process invocations**, use `:session save` and `:session load` + `:session goto`:

```bash
# Session 1: Define things and save
$ npm run omega-fast
Omega> (define x 42)
=> x
Omega> (define (double n) (* n 2))
=> double
Omega> :session save mysession
Session saved as 'mysession'
Omega> :quit

# Session 2: Later (hours/days), restore and continue
$ npm run omega-fast
Omega> :session load mysession
Loaded session 'mysession' (6 events)
Omega> :session goto 5           # Jump to checkpoint to restore env
Jumped to seq 5
Omega> x                         # x is restored!
=> 42
Omega> (double x)
=> 84
```

**Session files are stored in:** `OmegaLLM/.omega-session/sessions/` (relative to project root)

When you're working from inside the `OmegaLLM/` directory, they're at `.omega-session/sessions/`.

**Example session**: After running `npm install`, an example session `getting-started.jsonl` is automatically created. Try loading it:
```text
Omega> :session load getting-started
Omega> :session goto 11
Omega> greeting
=> "Hello, OmegaLLM!"
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

```text
:session list              List all saved sessions
:session save <name>       Save current session to disk
:session load <name>       Load a session's trace (doesn't restore env yet)
:session goto <seq>        Jump to sequence number, RESTORES environment
:session trace             View the session's execution trace
:session fork <name>       Fork current session to new name
```

**Important**: `:session load` only loads the trace. You must `:session goto <seq>` to actually restore the environment state.

```bash
# Sessions persist on disk at (from project root):
OmegaLLM/.omega-session/sessions/<name>.jsonl      # Event log
OmegaLLM/.omega-session/sessions/<name>.index.json # Index with checkpoints

# Or if you're inside OmegaLLM/ directory:
.omega-session/sessions/<name>.jsonl
.omega-session/sessions/<name>.index.json
```

### Sessions in the REPL

```bash
# Start REPL (with optional session name for recording)
npm run omega-fast
npm run omega-fast -- --session my-work
```

**Complete example of session persistence**:
```text
# === First session ===
Omega> (define data (list 1 2 3))
=> data
Omega> (define (sum lst) (fold-left + 0 lst))
=> sum
Omega> (sum data)
=> 6
Omega> :session save mywork
Session saved as 'mywork'
Omega> :quit

# === Later: new process ===
$ npm run omega-fast
Omega> :session list
Saved sessions:
  mywork (8 events, 1 checkpoints)

Omega> :session load mywork
Loaded session 'mywork' (8 events)
Use :session goto <seq> to jump, :session trace to view

Omega> :session trace
[000] REPL > (define data (list 1 2 3))
[001] EVAL ~ (define data (list 1 2 3))
[002] OUT  => data
...
[007] SAVE * checkpoint (manual)

Omega> :session goto 7
Jumped to seq 7
  Replayed 0 steps

Omega> data                 # Environment restored!
=> (1 2 3)
Omega> (sum data)
=> 6
```

### Why This Matters for AI Agents

**Use case**: An AI agent that maintains state across tool calls.

The agent keeps a single REPL process running (or uses `:session save`/`:session load` + `:session goto` to persist across restarts):

```lisp
;; Agent's first tool call
Omega> (define files (list "auth.ts" "user.ts" "api.ts"))
=> files

;; Agent's second tool call (same REPL session)
Omega> (define issues (filter security-issue? files))
=> issues

;; Agent's third call
Omega> (generate-report issues)
=> "Security report: 2 issues found..."

;; Agent saves before shutdown
Omega> :session save agent-review-123
```

Later, the agent can restore:
```lisp
Omega> :session load agent-review-123
Omega> :session goto 8
Omega> issues              ;; Still there!
=> ("auth.ts" "api.ts")
```

**The evaluator state is durable** — like tmux for code execution.

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
omega [options] <file>              # Execute Omega file (via -f)
omega --cmd <code>                  # Evaluate single expression

Options:
  -c, --cmd <code>                  Evaluate code and exit
  -f, --file <path>                 Execute Omega file
  -s, --session <name>              Session name (for recording)
  -j, --json                        Output as JSON
  -v, --verbose                     Show detailed execution
```

Examples:
```bash
# Interactive REPL
npm run omega-fast

# One-off evaluation
npm run omega-fast -- --cmd "(+ 1 2 3)"

# Execute file
npm run omega-fast -- --file program.lisp

# Start REPL with verbose output
npm run omega-fast -- --verbose
```

**Note**: The `--session` flag names the session for recording, but does NOT auto-restore from a previous session. For cross-process persistence, use `:session save` / `:session load` + `:session goto` inside the REPL.

---

## Contributing

We welcome contributions! Please:

1. Read the [ARCHITECTURE/](ARCHITECTURE/) docs to understand the system
2. Check [DEFECTS/](DEFECTS/) for known issues before reporting
3. Check existing issues and PRs
4. Write tests for new features
5. Follow the existing code style
6. Update documentation

---

## License

Copyright © 2026 hypnotranz. All Rights Reserved.

See [LICENSE](LICENSE) file for details.

---

## Resources

- **📖 User Manual**: [Structure and Interpretation of Linguistic Programs](MANUAL--STRUCTURE-AND-INTERPRETATION-OF-LINGUISTIC-PROGRAMS/)
- **🎨 Demo Gallery**: [All 49 demos with outputs](MANUAL--STRUCTURE-AND-INTERPRETATION-OF-LINGUISTIC-PROGRAMS/DEMO-GALLERY.md)
- **🏗️ Architecture Docs**: [ARCHITECTURE/](ARCHITECTURE/) (50+ specs for developers)
- **💬 Issues**: [GitHub Issues](https://github.com/hypnotranz/Omega/issues)
- **🗣️ Discussions**: [GitHub Discussions](https://github.com/hypnotranz/Omega/discussions)

---

**Built with inspiration from**:
- SICP (Structure and Interpretation of Computer Programs)
- Scheme R7RS specification
- Modern LLM agent frameworks
- Formal verification systems

**OmegaLLM**: Where semantic computing meets structured inference. 🚀
