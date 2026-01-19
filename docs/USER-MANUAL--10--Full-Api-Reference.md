# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 10: Full API Reference
### Core Effects

| Effect | Description | Example |
|--------|-------------|---------|
| `infer.op` | Single LLM call, returns answer | `(effect infer.op "question")` |
| `int.op` | LLM call, returns full Meaning | `(effect int.op "question")` |
| `search.op` | Multi-shot sampling, returns Dist | `(effect search.op "question")` |
| `amb.choose` | Nondeterministic choice | `(effect amb.choose (list thunks...))` |
| `amb.fail` | Trigger backtracking | `(effect amb.fail "reason")` |

### Stream Primitives

| Function | Description |
|----------|-------------|
| `list->stream` | Convert list to lazy stream |
| `stream->list` | Force N elements to a list |
| `stream-car` | Get first element |
| `stream-cdr` | Get rest (forced) |
| `stream-map` | Lazy map |
| `stream-filter` | Lazy filter |
| `stream-take` | Take first N elements |
| `stream-null?` | Check if empty |
| `the-empty-stream` | Empty stream constant |

### REPL Commands

| Command | Description |
|---------|-------------|
| `:help` | Show all commands |
| `:defs` | List definitions |
| `:env` | Show environment |
| `:debug (expr)` | Start debugging |
| `:step [N]` | Step N times |
| `:run` | Run to completion/breakpoint |
| `:goto N` | Jump to step N |
| `:trace` | Show execution trace |
| `:break TYPE COND` | Set breakpoint |
| `:breaks` | List breakpoints |
| `:ask QUESTION` | Ask LLM agent |
| `:traces` | List agent traces |
| `:quit` | Exit |

---