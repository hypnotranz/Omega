# LambdaLLM Runtime Architecture

## Overview

LambdaLLM is a Lisp runtime designed for AI agent orchestration. It follows the architectural patterns of real language runtimes (SBCL, Clojure, Racket, Lua) to enable:

- **Portability**: Runs in browser, Node, Deno, Bun, VS Code, CLI
- **Debuggability**: Time-travel, REPL-at-any-level, intervention
- **Extensibility**: New features as packages, not core changes

## Design Principles

### Runtime Architecture
1. **Protocol-first**: All interaction through a protocol (like nREPL)
2. **Continuations are data**: Call stack can be captured, serialized, restored
3. **Conditions don't unwind**: Errors can be handled without losing context
4. **Pure core**: Interpreter has no I/O knowledge, everything via FFI
5. **Lisp stdlib**: Most primitives defined in Lisp, not host language

### AI Agent Orchestration (What Makes This Special)
6. **Provenance is first-class**: Every claim must cite evidence; staleness detection prevents hallucination
7. **Facts are monotone**: Knowledge accumulates, never retracts; enables phase gating and proof obligations
8. **Fixpoint for convergence**: Iterative refinement with pluggable state projections and cycle detection
9. **Budget enforcement**: Tokens, cost, time, iterations tracked and enforced to prevent runaway agents
10. **Host controls the loop**: Session-based execution with policy enforcement; LLM analyzes, host executes
11. **Outcomes over exceptions**: Structured return types (ok, needs, error) with metadata for informed decisions
12. **Expert layers**: Three-tier intent compilation (contract → role → task) bridges natural language to code

## Architecture Documents

### Language Specification
| Document | What It Covers |
|----------|----------------|
| [00-SPECIFICATION.md](00-SPECIFICATION.md) | Formal language specification, EBNF grammar |

### Core Runtime (Layers 0-1)
| Document | What It Covers |
|----------|----------------|
| [01-READER.md](01-READER.md) | S-expression parsing, source locations |
| [02-TYPES.md](02-TYPES.md) | Core data types, serialization |
| [03-ENVIRONMENT.md](03-ENVIRONMENT.md) | Namespaces, bindings, modules |
| [04-EVALUATOR.md](04-EVALUATOR.md) | Pure interpreter, special forms |
| [05-CONTINUATIONS.md](05-CONTINUATIONS.md) | First-class continuations, call/cc |
| [06-CONDITIONS.md](06-CONDITIONS.md) | Condition/restart system (non-unwinding) |

### Runtime Services (Layers 2-3)
| Document | What It Covers |
|----------|----------------|
| [07-FFI.md](07-FFI.md) | Foreign function interface, World abstraction |
| [08-PROTOCOL.md](08-PROTOCOL.md) | nREPL-style protocol, middleware |
| [09-MODULES.md](09-MODULES.md) | Module system, require/import |
| [10-PERSISTENCE.md](10-PERSISTENCE.md) | Image saving, snapshots |
| [11-MACROS.md](11-MACROS.md) | Compile-time transforms, quasiquote |
| [12-CONCURRENCY.md](12-CONCURRENCY.md) | Async model, futures, actors |
| [13-MEMORY.md](13-MEMORY.md) | Memory model, GC considerations |

### Standard Library & Tooling
| Document | What It Covers |
|----------|----------------|
| [14-STDLIB.md](14-STDLIB.md) | Complete standard library reference |
| [15-DIAGNOSTICS.md](15-DIAGNOSTICS.md) | Error messages, source locations, help |
| [16-TOOLING.md](16-TOOLING.md) | REPL, CLI, LSP, DAP, profiler |
| [17-EMBEDDING.md](17-EMBEDDING.md) | API for embedding in TypeScript apps |

### Engineering & Operations
| Document | What It Covers |
|----------|----------------|
| [18-BOOTSTRAP.md](18-BOOTSTRAP.md) | Build process, self-hosting strategy |
| [19-VERSIONING.md](19-VERSIONING.md) | Compatibility, migrations, deprecation |
| [20-TESTING.md](20-TESTING.md) | Unit, property, integration, E2E tests |
| [21-SECURITY.md](21-SECURITY.md) | Sandboxing, capabilities, audit logging |

### AI Agent Orchestration (The Special Sauce)
These features make LambdaLLM unique for AI agent development:

| Document | What It Covers |
|----------|----------------|
| [22-PROVENANCE.md](22-PROVENANCE.md) | **Evidence-based reasoning** - content-addressed evidence, staleness detection, epistemic modes |
| [23-FACTS.md](23-FACTS.md) | **Monotone epistemic state** - facts as first-class knowledge, phase gating |
| [24-FIXPOINT.md](24-FIXPOINT.md) | **Convergence detection** - state projections, cycle detection, iterative refinement |
| [25-BUDGET.md](25-BUDGET.md) | **Resource tracking** - tokens, cost, time, iterations with enforcement |
| [26-ARTIFACTS.md](26-ARTIFACTS.md) | **Content-addressed memoization** - dependency tracking, auto-invalidation |
| [27-OUTCOMES.md](27-OUTCOMES.md) | **Structured return types** - ok, proposed, needs, error with metadata |
| [28-SESSION.md](28-SESSION.md) | **Host-controlled execution** - trust boundary, policy enforcement, audit |
| [29-EXPERTS.md](29-EXPERTS.md) | **Intent compilation** - three-layer expert system, natural language → Lisp |

## System Layers

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         LambdaLLM Runtime Stack                         │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  CLIENTS (all use same protocol)                                         │
│  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐        │
│  │ CLI     │  │ VS Code │  │ Browser │  │ Web UI  │  │ Matrix  │        │
│  │ (Bun)   │  │ Ext     │  │ (WASM)  │  │ (React) │  │ MQTT    │        │
│  └────┬────┘  └────┬────┘  └────┬────┘  └────┬────┘  └────┬────┘        │
│       │            │            │            │            │              │
│       └────────────┴────────────┴─────┬──────┴────────────┘              │
│                                       │                                  │
│  ═══════════════════════════════════════════════════════════════════    │
│  LAYER 5: PROTOCOL (the ONLY interface)                                  │
│  ═══════════════════════════════════════════════════════════════════    │
│  ┌───────────────────────────────────────────────────────────────┐      │
│  │  protocol.ts                                                   │      │
│  │  - Operations: eval, info, complete, interrupt                │      │
│  │  - Time-travel: snapshot, restore, fork, navigate             │      │
│  │  - Middleware stack for extensibility                         │      │
│  └───────────────────────────────────────────────────────────────┘      │
│                                       │                                  │
│  ═══════════════════════════════════════════════════════════════════    │
│  LAYER 4: DEBUGGER (built on continuations)                              │
│  ═══════════════════════════════════════════════════════════════════    │
│  ┌───────────────────────────────────────────────────────────────┐      │
│  │  snapshot.ts   navigator.ts   interactive.ts   branching.ts   │      │
│  │  - Capture: serialize continuation + env + world              │      │
│  │  - Navigate: query history, tree view                         │      │
│  │  - Intervene: REPL at level, inject, fork                     │      │
│  └───────────────────────────────────────────────────────────────┘      │
│                                       │                                  │
│  ═══════════════════════════════════════════════════════════════════    │
│  LAYER 3: EXTENSIONS (loaded at runtime, use FFI)                        │
│  ═══════════════════════════════════════════════════════════════════    │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────────┐      │
│  │  llm/           │  │  world/         │  │  stdlib/            │      │
│  │  openai.ts      │  │  fs-node.ts     │  │  core.lisp          │      │
│  │  anthropic.ts   │  │  fs-browser.ts  │  │  list.lisp          │      │
│  │  intent.lisp    │  │  fs-memory.ts   │  │  string.lisp        │      │
│  └─────────────────┘  └─────────────────┘  └─────────────────────┘      │
│                                       │                                  │
│  ═══════════════════════════════════════════════════════════════════    │
│  LAYER 2: RUNTIME SERVICES (implement FFI contracts)                     │
│  ═══════════════════════════════════════════════════════════════════    │
│  ┌───────────────────────────────────────────────────────────────┐      │
│  │  ffi.ts          artifacts.ts      trace.ts       metrics.ts  │      │
│  │  - FFI registry  - Memoization    - Cont marks   - Counters   │      │
│  │  - Call dispatch - Content hash   - Tree trace   - Budget     │      │
│  └───────────────────────────────────────────────────────────────┘      │
│                                       │                                  │
│  ═══════════════════════════════════════════════════════════════════    │
│  LAYER 1: CORE INTERPRETER (pure, no I/O)                                │
│  ═══════════════════════════════════════════════════════════════════    │
│  ┌───────────────────────────────────────────────────────────────┐      │
│  │  eval.ts         condition.ts      env.ts                      │      │
│  │  - Special forms - signal()       - Namespaces                │      │
│  │  - Apply         - handler-bind   - Bindings                  │      │
│  │  - ~200 lines    - invoke-restart - Module registry           │      │
│  └───────────────────────────────────────────────────────────────┘      │
│                                       │                                  │
│  ═══════════════════════════════════════════════════════════════════    │
│  LAYER 0: FOUNDATION (zero dependencies)                                 │
│  ═══════════════════════════════════════════════════════════════════    │
│  ┌───────────────────────────────────────────────────────────────┐      │
│  │  types.ts        reader.ts         continuation.ts             │      │
│  │  - Symbol        - Tokenizer       - Frame                    │      │
│  │  - Cons          - Parser          - Continuation             │      │
│  │  - Closure       - Source locs     - Marks                    │      │
│  │  - Procedure     - Pretty print    - call/cc                  │      │
│  └───────────────────────────────────────────────────────────────┘      │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

## File Structure

```
lambdallm/
├── src/
│   ├── core/                    # Layer 0-1: The runtime core
│   │   ├── types.ts             # Symbol, Cons, Closure, Procedure
│   │   ├── reader.ts            # S-expression parser
│   │   ├── continuation.ts      # First-class continuations
│   │   ├── env.ts               # Namespaces and bindings
│   │   ├── condition.ts         # Condition/restart system
│   │   └── eval.ts              # Pure interpreter (~200 lines)
│   │
│   ├── runtime/                 # Layer 2: Runtime services
│   │   ├── ffi.ts               # Foreign function interface
│   │   ├── artifacts.ts         # Memoization/caching
│   │   ├── trace.ts             # Tracing with cont marks
│   │   └── metrics.ts           # Counters and budget
│   │
│   ├── ext/                     # Layer 3: Extensions
│   │   ├── llm/                 # LLM adapters
│   │   │   ├── adapter.ts       # Common interface
│   │   │   ├── openai.ts        # OpenAI implementation
│   │   │   ├── anthropic.ts     # Anthropic implementation
│   │   │   └── intent.lisp      # Intent compilation in Lisp
│   │   │
│   │   ├── world/               # World/filesystem
│   │   │   ├── interface.ts     # Common interface
│   │   │   ├── fs-node.ts       # Node.js filesystem
│   │   │   ├── fs-browser.ts    # Browser virtual FS
│   │   │   └── fs-memory.ts     # In-memory (tests)
│   │   │
│   │   └── stdlib/              # Standard library (Lisp!)
│   │       ├── core.lisp        # Core primitives
│   │       ├── list.lisp        # List operations
│   │       ├── string.lisp      # String operations
│   │       └── io.lisp          # I/O via FFI
│   │
│   ├── debugger/                # Layer 4: Time-travel debugger
│   │   ├── snapshot.ts          # Capture/restore state
│   │   ├── navigator.ts         # Query and navigate history
│   │   ├── interactive.ts       # REPL at level, intervention
│   │   └── branching.ts         # Fork, diff, replay
│   │
│   └── protocol/                # Layer 5: Client protocol
│       ├── protocol.ts          # Operation definitions
│       ├── server.ts            # Protocol server
│       ├── middleware.ts        # Middleware stack
│       └── handlers/            # Operation handlers
│           ├── eval.ts
│           ├── info.ts
│           ├── complete.ts
│           └── debug.ts
│
├── cli/                         # CLI client (Bun)
│   └── index.ts
│
├── vscode/                      # VS Code extension
│   ├── extension.ts
│   └── package.json
│
├── browser/                     # Browser bundle
│   └── index.ts
│
└── stdlib/                      # Lisp standard library
    ├── core.lisp
    ├── list.lisp
    ├── string.lisp
    ├── math.lisp
    └── llm.lisp
```

## Technology Choices

### Why TypeScript?

| Requirement | TypeScript Solution |
|-------------|---------------------|
| Run in browser | Native - TypeScript IS JavaScript |
| Run in VS Code | Extensions are TypeScript |
| Run as CLI | Bun compiles to single binary |
| Run in `.folder` | Just copy the JS files |
| Type safety | Full static types |
| LLM calls | `fetch()` works everywhere |

### Build Targets

```
┌─────────────────────────────────────────────────────────────────┐
│  Source (TypeScript)                                            │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  src/**/*.ts + stdlib/**/*.lisp                         │    │
│  └─────────────────────────────────────────────────────────┘    │
│                              │                                   │
│              ┌───────────────┼───────────────┐                  │
│              ▼               ▼               ▼                  │
│  ┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐   │
│  │  CLI Binary     │ │  Browser Bundle │ │  VS Code Ext    │   │
│  │  (Bun compile)  │ │  (esbuild)      │ │  (vsce)         │   │
│  │                 │ │                 │ │                 │   │
│  │  ./lambdallm    │ │  lambdallm.js   │ │  .vsix package  │   │
│  │  Single binary  │ │  ~100KB         │ │                 │   │
│  └─────────────────┘ └─────────────────┘ └─────────────────┘   │
└─────────────────────────────────────────────────────────────────┘
```

## Comparison to Python Prototype

| Aspect | Python (LambdaRLM) | TypeScript (LambdaLLM) |
|--------|-------------------|------------------------|
| eval.py | 875 lines, knows everything | 200 lines, pure |
| Closures | Not serializable | Source + env snapshot |
| Continuations | None | First-class |
| Conditions | Exceptions unwind | signal/restart |
| stdlib | Python functions | Lisp code |
| Distribution | Needs Python | Single binary or JS |
| Browser | Pyodide (~10MB) | Native (~100KB) |

## Implementation Roadmap

### Phase 1: Core Runtime (Week 1)

```
Day 1-2: Foundation (Layer 0)
├── types.ts - Symbol, Cons, Closure, Procedure
├── reader.ts - S-expression parser with source locs
└── continuation.ts - Frame, Continuation, marks

Day 3-4: Interpreter (Layer 1)
├── env.ts - Namespace, bindings
├── condition.ts - signal, handler-bind, restart
└── eval.ts - Pure interpreter

Day 5-7: Runtime (Layer 2)
├── ffi.ts - FFI registry and dispatch
├── Basic REPL
└── Run simple programs
```

### Phase 2: Extensions (Week 2)

```
Day 1-2: World/FS
├── fs-node.ts
├── fs-browser.ts (virtual FS)
└── fs-memory.ts (tests)

Day 3-4: LLM Integration
├── adapter.ts (interface)
├── openai.ts
└── intent.lisp (in Lisp!)

Day 5-7: Standard Library
├── core.lisp (ported from Python)
├── list.lisp
└── string.lisp
```

### Phase 3: Debugger (Week 3)

```
Day 1-2: Snapshots
├── snapshot.ts (trivial - serialize continuation)
└── Storage backends

Day 3-4: Navigation
├── navigator.ts
└── Tree view

Day 5-7: Interactive
├── REPL at level
├── Fork/branch
└── Basic CLI
```

### Phase 4: Clients (Week 4)

```
Day 1-2: CLI
├── Bun single binary
└── All commands working

Day 3-5: VS Code Extension
├── Language support
├── Debugger integration
└── Tree view for execution

Day 6-7: Browser
├── WASM bundle
└── Demo page
```

## Migration from Python

The Python LambdaRLM code taught us what to build. Key migrations:

1. **sexp.py → reader.ts**: Keep the logic, add source locations
2. **env.py → env.ts**: Add namespace support
3. **eval.py → eval.ts**: Make pure, remove I/O knowledge
4. **stdlib.py → stdlib/*.lisp**: Rewrite in Lisp where possible
5. **llm.py → ext/llm/**: Same interface, TypeScript implementation

The test suite from LambdaRLM becomes the acceptance criteria:
- All 1011 tests must pass after migration
- Same Lisp semantics
- Same features (fixpoint, memo, subeval, intent)

## Success Criteria

### Runtime Goals
1. **Portability**: Same code runs in CLI, browser, VS Code
2. **Size**: Core runtime < 2000 lines TypeScript
3. **Performance**: Parse + eval simple program < 10ms
4. **Debuggability**: Can snapshot/restore any execution point
5. **Extensibility**: Add new FFI without touching core

### AI Agent Goals
6. **No Theater**: All agent claims grounded in verifiable evidence with staleness detection
7. **Cost Control**: Budget limits prevent runaway token usage; < $1 default per session
8. **Safe Execution**: Host-controlled loop with policy enforcement; no arbitrary shell commands
9. **Convergence**: Fixpoint detection for iterative refinement; cycle detection for oscillations
10. **Auditability**: Complete operation history for debugging and compliance
