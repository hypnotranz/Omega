# OmegaLLM

A Lisp dialect where LLM calls are first-class expressions.

```lisp
(effect infer.op "What is 2+2?")
=> "4"
```

## Quick Start

```bash
npm install
npm run build
npm run omega-fast
```

```
Omega> (+ 1 2)
=> 3

Omega> (effect infer.op "Hello!")
=> "Hi there!"
```

## Run Modes

| Command | Startup | Use Case |
|---------|---------|----------|
| `npm run omega-fast` | ~0.3s | Production, orchestration |
| `npm run omega-repl` | ~7s | Development (TypeScript) |

## Environment

```bash
# Required: set one of these
export OPENAI_API_KEY=sk-...
export ANTHROPIC_API_KEY=sk-ant-...

# Optional: select adapter
export OMEGA_ADAPTER=openai    # or 'anthropic'
export OMEGA_MODEL=gpt-4o      # override model
```

## Core Effects

```lisp
;; LLM inference
(effect infer.op "prompt")

;; Shell execution
(effect shell.op "echo hello")

;; File I/O
(effect file.read.op "path")
(effect file.write.op "path" "content")

;; Nondeterministic choice
(effect amb.choose (list (lambda () "a") (lambda () "b")))
```

## Documentation

- [USER-MANUAL.md](USER-MANUAL.md) - Full user guide with examples
- [OMEGA-SPECS.md](OMEGA-SPECS.md) - Formal language specification
- [ARCHITECTURE/](ARCHITECTURE/) - Implementation details

## Build

| Command | Time | Type Check | Use Case |
|---------|------|------------|----------|
| `npm run build` | ~5-13s | Yes | CI, releases, before commit |
| `npm run build:dev` | ~1s | No | Fast iteration |
| `npm run watch` | ~50ms | No | Auto-rebuild on save |

**Development workflow:**

```bash
# Terminal 1: auto-rebuild on save
npm run watch

# Terminal 2: run your code
npm run omega-fast
```

Your editor shows type errors in real-time. Run `npm run build` before committing to catch anything the editor missed.

```bash
npm run test       # Run tests
```

The build creates both `dist/` (TypeScript output) and `dist/omega-repl.mjs` (fast bundled REPL).
