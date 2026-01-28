# OmegaLLM Demo Library

A comprehensive collection of demos showing what OmegaLLM can do.

## Quick Start

```bash
# Run any demo through the REPL
npm run omega-fast

# Or run a specific chapter
npx tsx demo/by-chapter/index.ts --chapter ch02-llm-calls
```

## Demo Categories

### 📚 SICP-Style Tutorial (Chapters 1-27)

Progressive introduction to semantic programming, following SICP structure:

| Chapter | File | Concepts |
|---------|------|----------|
| 01 | `ch01-getting-started.lisp` | Basic definitions, REPL warmup |
| 02 | `ch02-llm-calls.lisp` | `infer.op` - LLM as first-class expression |
| 03 | `ch03-composition.lisp` | `map`/`filter` with semantic predicates |
| 04 | `ch04-higher-order.lisp` | Functions returning classifiers |
| 05 | `ch05-nondeterministic.lisp` | `amb` + backtracking |
| 06 | `ch06-multi-shot.lisp` | `search.op` for multiple candidates |
| 07 | `ch07-lazy-streams.lisp` | Lazy evaluation |
| 08 | `ch08-debugger.lisp` | Tracing and debugging |
| 09 | `ch09-agentic-repl.lisp` | `oracle-lambda` for agentic interaction |
| 10 | `ch10-api-reference.lisp` | API reference |
| 11-18 | ... | Data abstraction, sequences, generics |
| 19-23 | ... | State, mutation, concurrency |
| 24-27 | ... | Metacircular evaluator, logic programming |

### 🛠️ Real-World Use Cases

Practical applications showing OmegaLLM's power:

| File | Use Case | Key Features |
|------|----------|--------------|
| `usecase-coding-agent.lisp` | **Coding Agent** | Read/write files, run tests, make decisions |
| `usecase-code-review.lisp` | **Automated Code Review** | Git diff analysis, security/perf checks |
| `usecase-data-pipeline.lisp` | **Data Processing** | CSV→enriched data with LLM classification |
| `usecase-research-agent.lisp` | **Research Agent** | Search, synthesize, build knowledge |
| `usecase-workflow-orchestrator.lisp` | **Workflow/CI-CD** | Multi-step pipelines with LLM decisions |
| `usecase-interactive-tutor.lisp` | **Programming Tutor** | Adaptive learning, progress tracking |
| `auto-traceability.lisp` | **Doc Generation** | Pure mechanical extraction (no LLM) |

## Core Effects

```lisp
;; LLM Inference
(effect infer.op "What is 2+2?")              ; Single answer
(effect int.op "analyze this")                 ; Full Meaning object
(effect search.op "Generate 3 variations...")  ; Multiple samples

;; Nondeterminism
(amb "a" "b" "c")                              ; Choose one
(require (predicate))                          ; Constraint (backtrack if false)

;; File I/O
(effect file.read.op "path/to/file")
(effect file.write.op "path" "content")

;; Shell
(effect shell.op "grep -r 'pattern' .")
(effect shell.op "npm test")
```

## State Management

OmegaLLM supports mutable state for building stateful agents:

```lisp
;; Define mutable state
(define counter 0)

;; Modify with set!
(set! counter (+ counter 1))

;; Pattern: State object with closure
(define (make-conversation)
  (let ((history '()))
    (lambda (input)
      (let ((response (effect infer.op (list "History:" history "\nUser:" input))))
        (set! history (cons input history))
        response))))
```

## Running Demos

### In REPL

```bash
$ npm run omega-fast
omega> (load "demo/lisp/usecase-coding-agent.lisp")
omega> (agent-loop "Fix bugs in src/utils.ts" 5)
```

### With Capabilities

```bash
# Grant file and shell access
omega --file demo/lisp/auto-traceability.lisp --caps shell,file.read,file.write

# Inference only
omega --file demo/lisp/ch02-llm-calls.lisp --caps infer
```

### Through TypeScript

```bash
npx tsx demo/by-chapter/index.ts --chapter ch05-nondeterministic
```

## What Makes OmegaLLM Unique

1. **LLM as Expression**: `infer.op` is a first-class effect, not an API call
2. **Backtracking**: `amb` + `require` for constraint satisfaction with LLM predicates
3. **State + Loops**: Build agents that maintain context across interactions
4. **Shell + Files**: Embed coding agents that can actually read/modify code
5. **Composability**: Higher-order functions work naturally with inference
6. **Lazy Streams**: Process infinite/large datasets efficiently
7. **Debuggable**: Step through semantic evaluation in the web debugger
