# 16: Tooling

## Overview

LambdaLLM provides a complete development experience:
- REPL (interactive development)
- CLI (command-line interface)
- LSP (IDE integration)
- DAP (debugging)
- Profiler
- Formatter

---

## REPL

### Basic Usage

```
$ lambdallm repl
LambdaLLM v1.0.0
Type :help for help, :quit to exit

λ> (+ 1 2)
3

λ> (define (square x) (* x x))
#<closure square>

λ> (square 5)
25
```

### REPL Commands

| Command | Description |
|---------|-------------|
| `:help` | Show help |
| `:quit` | Exit REPL |
| `:clear` | Clear screen |
| `:reset` | Reset environment |
| `:load FILE` | Load and evaluate file |
| `:reload` | Reload last file |
| `:ns NAME` | Switch namespace |
| `:doc SYM` | Show documentation |
| `:source SYM` | Show source code |
| `:apropos PAT` | Search symbols |
| `:expand FORM` | Macro expand |
| `:trace ON/OFF` | Toggle tracing |
| `:time EXPR` | Time expression |
| `:save FILE` | Save session image |
| `:restore FILE` | Restore session image |

### Multi-line Input

```
λ> (define (factorial n)
..   (if (= n 0)
..       1
..       (* n (factorial (dec n)))))
#<closure factorial>
```

### History and Completion

- **Up/Down**: Navigate history
- **Tab**: Autocomplete
- **Ctrl+R**: Search history

---

## CLI Commands

### General

```bash
lambdallm --version        # Show version
lambdallm --help           # Show help
lambdallm repl             # Start REPL
```

### Evaluation

```bash
lambdallm eval "(+ 1 2)"                    # Evaluate expression
lambdallm run src/main.lisp                 # Run file
lambdallm run src/main.lisp --arg1 value    # With arguments
```

### Project Management

```bash
lambdallm init my-project        # Create new project
lambdallm build                  # Build project
lambdallm test                   # Run tests
lambdallm fmt                    # Format code
lambdallm lint                   # Check for issues
```

### Image Management

```bash
lambdallm image:save out.image   # Save runtime image
lambdallm image:load in.image    # Load runtime image
lambdallm image:info in.image    # Show image info
```

### Time-Travel

```bash
lambdallm history               # List execution traces
lambdallm inspect TRACE_ID      # Inspect trace
lambdallm checkout CONTEXT_ID   # Restore context
lambdallm diff CTX1 CTX2        # Compare contexts
```

---

## LSP (Language Server Protocol)

### Features

| Feature | LSP Method |
|---------|------------|
| Syntax highlighting | `textDocument/semanticTokens` |
| Diagnostics | `textDocument/publishDiagnostics` |
| Autocomplete | `textDocument/completion` |
| Hover info | `textDocument/hover` |
| Go to definition | `textDocument/definition` |
| Find references | `textDocument/references` |
| Rename | `textDocument/rename` |
| Format | `textDocument/formatting` |
| Code actions | `textDocument/codeAction` |

### Configuration

```json
// settings.json (VS Code)
{
  "lambdallm.lsp.path": "/usr/local/bin/lambdallm",
  "lambdallm.lsp.args": ["lsp"],
  "lambdallm.format.onSave": true,
  "lambdallm.lint.enabled": true
}
```

### Starting LSP Server

```bash
# Standalone
lambdallm lsp

# With logging
lambdallm lsp --log-level debug --log-file lsp.log
```

---

## DAP (Debug Adapter Protocol)

### Features

| Feature | DAP Method |
|---------|------------|
| Set breakpoint | `setBreakpoints` |
| Continue | `continue` |
| Step over | `next` |
| Step into | `stepIn` |
| Step out | `stepOut` |
| Evaluate | `evaluate` |
| Variables | `variables` |
| Stack trace | `stackTrace` |

### Breakpoints

```json
// launch.json (VS Code)
{
  "type": "lambdallm",
  "request": "launch",
  "name": "Debug LambdaLLM",
  "program": "${workspaceFolder}/src/main.lisp",
  "stopOnEntry": false
}
```

### Conditional Breakpoints

```lisp
;; In code
(break-when (> x 100))

;; Or via IDE on line 42, condition: (> x 100)
```

---

## Profiler

### CPU Profiling

```lisp
λ> (with-profiling
     (my-expensive-function))

Profile results:
  Total time: 1234ms

  Function           Calls    Self (ms)   Total (ms)   %
  ─────────────────────────────────────────────────────
  process-item       1000     800         900         65%
  parse-data         500      200         250         16%
  format-result      1000     100         100         8%
  ...
```

### Memory Profiling

```lisp
λ> (with-memory-profile
     (my-function))

Memory profile:
  Allocations: 50,000 objects
  Peak memory: 45 MB

  Type            Count      Size (KB)
  ────────────────────────────────────
  List            20,000     15,000
  String          15,000     20,000
  Closure         10,000     5,000
  ...
```

### CLI Profiling

```bash
lambdallm run --profile src/main.lisp
lambdallm run --profile=memory src/main.lisp
```

---

## Formatter

### Usage

```bash
# Format file in place
lambdallm fmt src/main.lisp

# Format and show diff
lambdallm fmt --check src/main.lisp

# Format all files
lambdallm fmt src/
```

### Rules

| Rule | Example |
|------|---------|
| Indent 2 spaces | Standard indent |
| Align parameters | Multi-line call alignment |
| Max line length 100 | Wrap long lines |
| One blank between top-level | Separate definitions |

### Before/After

```lisp
;; Before
(define (foo x y z)(if (> x 0)(+ y z)(- y z)))

;; After
(define (foo x y z)
  (if (> x 0)
      (+ y z)
      (- y z)))
```

### Configuration

```yaml
# .lambdallm-fmt.yaml
indent: 2
max-line-length: 100
align-parameters: true
blank-lines-between-definitions: 1
```

---

## Linter

### Usage

```bash
lambdallm lint src/
lambdallm lint --fix src/  # Auto-fix issues
```

### Rules

| Rule | Severity | Description |
|------|----------|-------------|
| unused-variable | warning | Variable defined but unused |
| undefined-variable | error | Using undefined variable |
| shadowed-variable | warning | Shadows outer binding |
| deprecated-function | warning | Using deprecated API |
| redundant-do | info | Unnecessary (do ...) |
| style/naming | info | Naming convention |

### Configuration

```yaml
# .lambdallm-lint.yaml
rules:
  unused-variable: warn
  shadowed-variable: off
  style/naming: error
exclude:
  - test/**
  - vendor/**
```

---

## Testing Framework

### Running Tests

```bash
lambdallm test                    # All tests
lambdallm test test/core_test.lisp  # Specific file
lambdallm test --watch            # Watch mode
lambdallm test --coverage         # With coverage
```

### Output

```
Running tests...

test/core_test.lisp
  ✓ addition-works
  ✓ subtraction-works
  ✗ division-by-zero
    Expected: throws division-by-zero
    Actual: returned 0

Results: 2 passed, 1 failed
Time: 0.5s
```

### Coverage Report

```
Coverage report:
  src/core.lisp          95%  ████████████████████░
  src/list.lisp          87%  █████████████████░░░░
  src/string.lisp        62%  ████████████░░░░░░░░░

  Total: 81%
```
