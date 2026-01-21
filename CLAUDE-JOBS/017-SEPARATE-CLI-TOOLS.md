# JOB-017: Separate CLI Tools (REPL/Debugger)

**Priority**: P2 - Cleanup/Architecture
**Estimated Effort**: 1-2 days
**Skills Required**: TypeScript, CLI design, npm packaging
**Status**: NOT STARTED
**Depends On**: None (standalone cleanup task)

> **MUST READ**: [LOGISTICS.md](LOGISTICS.md) before starting - covers testing, file locations, and proof of completion requirements.

---

## Executive Summary

The OmegaLLM project has two substantial CLI tools embedded in `bin/`:

| Tool | File | Lines | Purpose |
|------|------|-------|---------|
| REPL | `bin/omega-repl.ts` | ~2,233 | Interactive Lisp REPL with LLM integration |
| Debugger | `bin/omega-debugger.ts` | ~1,198 | Step-through debugger with breakpoints, time-travel |

These tools are **tightly coupled** to `src/core/` internals through direct imports. This job:

1. **Defines a clean library API** in `src/index.ts` for external consumers
2. **Refactors CLI tools** to use only the public API
3. **Adds proper npm bin entries** in `package.json`
4. **Documents CLI usage** for end users

### Why Separate?

- **Testability**: Core library can be tested without CLI dependencies
- **Bundling**: CLI tools can be bundled separately or excluded
- **API surface**: Forces definition of what's "public" vs "internal"
- **Maintainability**: Changes to CLI don't affect library consumers

---

## What Already Exists

### bin/omega-repl.ts (2,233 lines)

Full-featured REPL with:

```typescript
// Current imports - directly into core internals
import { COWStore } from "../src/core/eval/store";
import { RuntimeImpl } from "../src/core/effects/runtimeImpl";
import { SnapshotRepo } from "../src/core/oracle/snapshots";
import { InMemoryReceiptStore } from "../src/core/oracle/receipts";
import { createOpenAIAdapter } from "../src/core/oracle/adapters";
import { runToCompletionWithState } from "../src/core/eval/run";
import { stepOnce } from "../src/core/eval/machineStep";
import { compileTextToExpr } from "../src/core/pipeline/compileText";
// ... many more
```

Features:
- CLI args: `--session <name>`, `--file <path>`, `--cmd <expr>`, `--json`, `--verbose`
- Session persistence to `~/.omega-sessions/`
- LLM adapters (OpenAI, Anthropic) with tool calling
- File loading and evaluation
- History and tab completion

### bin/omega-debugger.ts (1,198 lines)

Full-featured debugger with:

```typescript
// Similar internal imports
import { COWStore } from "../src/core/eval/store";
import { compileTextToExpr } from "../src/core/pipeline/compileText";
import { stepOnce } from "../src/core/eval/machineStep";
// ... etc
```

Features:
- Commands: `load`, `loadfile`, `step`, `continue`, `break`, `save`, `load-snapshot`, `dump`, `replay`, `goto`, `env`, `stack`
- Breakpoints with condition expressions
- Time-travel debugging (trace + goto)
- State snapshots for undo/redo
- Session persistence

### Test Usage

Tests **do NOT use** the CLI tools. They import core directly:

```typescript
// test/repl/debugger.spec.ts
import { COWStore } from "../../src/core/eval/store";
import { RuntimeImpl } from "../../src/core/effects/runtimeImpl";
import { compileTextToExpr } from "../../src/core/pipeline/compileText";
```

This is correct behavior - tests should use the library API.

---

## Gap Analysis

| Should Exist | Currently |
|--------------|-----------|
| `src/index.ts` with CLI exports | EXISTS (140 lines) but **missing** low-level CEKS machine exports |
| `package.json` bin entries | None (has module exports, missing `"bin"`) |
| CLI tools use public imports | Import 16+ internal paths directly |
| `--help` documentation | Minimal |
| Shebang + executable | Shebang exists, not executable |

### What `src/index.ts` Currently Exports (140 lines)

High-level API for library consumers:
- `OmegaRuntime`, `evalOmegaCode` - high-level runtime
- `Outcome`, `Failure`, `Diagnostic` - ADTs
- `FrameIR`, `ValueIR`, `PromptIR` - canonical IR
- `AnthropicAdapter`, `MCPClientAdapter` - adapters
- Governance: `Cap`, `Budget`, `Profile`
- Provenance: `ProvenanceGraph`, `evidenceId`

### What CLI Tools Need (NOT currently exported)

Low-level CEKS machine for step-through debugging:
- `COWStore`, `Store` - copy-on-write store
- `stepOnce`, `StepResult` - single-step execution
- `runToCompletionWithState` - batch execution
- `State`, `Frame`, `Env` - machine types
- `Val`, `VUnit`, `VNum`, etc. - value constructors
- `compileTextToExpr` - compilation
- `RuntimeImpl` - effects runtime
- `SnapshotRepo`, `InMemoryReceiptStore` - oracle state
- `createOpenAIAdapter` - OpenAI (only Anthropic exported currently)
- `installPrims` - primitive installation (in `test/helpers/`)

---

## Implementation Plan

### Phase 1: Extend Public API

**ADD** the following exports to the existing `src/index.ts` (which already has 140 lines):

```typescript
// src/index.ts - ADD TO EXISTING FILE

// ═══════════════════════════════════════════════════════════════════════════════
// CLI TOOLS - CEKS Machine (Low-Level)
// ═══════════════════════════════════════════════════════════════════════════════

// Store
export { COWStore, type Store } from "./core/eval/store";

// Single-step execution (for debugger)
export { stepOnce, type StepResult } from "./core/eval/machineStep";
export { runToCompletionWithState, runToCompletion } from "./core/eval/run";

// Machine types
export type { State, Frame } from "./core/eval/machine";
export type { Env } from "./core/eval/env";

// Value constructors (for pretty-printing)
export { VUnit, VNum, VStr, VBool, VSym, VVector, VClosure, VNative, VCont } from "./core/eval/values";

// Compilation
export { compileTextToExpr } from "./core/pipeline/compileText";

// Effects runtime
export { RuntimeImpl } from "./core/effects/runtimeImpl";

// Oracle state
export { SnapshotRepo } from "./core/oracle/snapshots";
export { InMemoryReceiptStore, type ReceiptStore } from "./core/oracle/receipts";

// OpenAI adapter (Anthropic already exported)
export { createOpenAIAdapter } from "./core/oracle/adapters";

// Primitive installation
export { installPrims } from "./prims/install";  // After moving from test/helpers/
```

### Phase 2: Relocate Test Helpers

Move `test/helpers/prims.ts` to `src/prims/install.ts` (it's needed for runtime, not just tests):

```bash
mkdir -p src/prims
mv test/helpers/prims.ts src/prims/install.ts
```

Update imports in tests and CLI tools.

### Phase 3: Refactor CLI Imports

Update both CLI tools to import from the public API:

```typescript
// bin/omega-repl.ts - BEFORE
import { COWStore } from "../src/core/eval/store";
import { RuntimeImpl } from "../src/core/effects/runtimeImpl";
// ...

// bin/omega-repl.ts - AFTER
import {
  COWStore,
  RuntimeImpl,
  compileTextToExpr,
  runToCompletionWithState,
  stepOnce,
  createOpenAIAdapter,
  SnapshotRepo,
  InMemoryReceiptStore,
  installPrims,
  // ...
} from "../src";
```

### Phase 4: Add package.json bin Entries

```json
{
  "bin": {
    "omega-repl": "./bin/omega-repl.ts",
    "omega-debugger": "./bin/omega-debugger.ts"
  },
  "scripts": {
    "repl": "tsx bin/omega-repl.ts",
    "debug": "tsx bin/omega-debugger.ts"
  }
}
```

### Phase 5: Add --help Documentation

Add comprehensive help to both tools:

```typescript
// bin/omega-repl.ts
function showHelp(): void {
  console.log(`
omega-repl - Interactive Omega Lisp REPL

USAGE
  npx omega-repl [options]
  npx omega-repl --file <path>
  npx omega-repl --cmd <expression>

OPTIONS
  --session <name>   Use named session (persists to ~/.omega-sessions/)
  --file <path>      Load and evaluate a .lisp file
  --cmd <expr>       Evaluate expression and exit
  --json             Output results as JSON
  --verbose          Show oracle/LLM transcripts
  --help             Show this help

EXAMPLES
  npx omega-repl                        # Start interactive REPL
  npx omega-repl --session myproject    # Resume session "myproject"
  npx omega-repl --file demo.lisp       # Load and run file
  npx omega-repl --cmd "(+ 1 2)"        # Evaluate and exit

ENVIRONMENT
  OPENAI_API_KEY      OpenAI API key for LLM features
  ANTHROPIC_API_KEY   Anthropic API key for LLM features

REPL COMMANDS
  :help               Show REPL commands
  :load <file>        Load a Lisp file
  :env                Show environment bindings
  :clear              Clear current session
  :quit               Exit REPL
`);
}

// bin/omega-debugger.ts
function showHelp(): void {
  console.log(`
omega-debugger - Step-through Debugger for Omega Lisp

USAGE
  npx omega-debugger [options]

OPTIONS
  --session <name>   Use named debug session
  --cmd <command>    Execute debugger command and exit
  --json             Output results as JSON
  --help             Show this help

DEBUGGER COMMANDS
  load <expr>         Load Lisp expression for debugging
  loadfile <path>     Load Lisp file for debugging
  step [n]            Step forward n steps (default: 1)
  continue            Run until breakpoint or completion
  break <line>        Set breakpoint at line
  break <fn>          Set breakpoint at function entry
  unbreak <id>        Remove breakpoint
  save <name>         Save current state as snapshot
  load-snapshot <n>   Restore snapshot (time-travel)
  dump                Dump execution trace to file
  replay              Replay execution from trace
  goto <step>         Jump to step number (time-travel)
  env                 Show current environment
  stack               Show current continuation stack
  quit                Exit debugger

EXAMPLES
  npx omega-debugger --cmd "load (define x 1)" --cmd "step"
  npx omega-debugger --session myproject
`);
}
```

### Phase 6: Optional - Create cli/ Directory

For better organization, consider moving tools to `cli/`:

```
cli/
  repl/
    index.ts          # Main REPL entry
    adapters.ts       # LLM adapters (OpenAI, Anthropic)
    session.ts        # Session persistence
    completion.ts     # Tab completion
  debugger/
    index.ts          # Main debugger entry
    commands.ts       # Command handlers
    breakpoints.ts    # Breakpoint logic
    trace.ts          # Time-travel trace
  shared/
    args.ts           # CLI argument parsing
    output.ts         # JSON/text output formatting
```

This is optional but recommended if the tools continue to grow.

---

## Output Files

```
src/
  index.ts              # EXTENDED: Add ~25 CLI-related exports to existing 140 lines
  prims/
    install.ts          # MOVED from test/helpers/prims.ts

bin/
  omega-repl.ts         # UPDATED: Import from src/index (was 16 internal imports)
  omega-debugger.ts     # UPDATED: Import from src/index (was 7 internal imports)

package.json            # UPDATED: Add bin entries and convenience scripts
```

### Folder Structure AFTER Job

```
OmegaLLM/
├── bin/                           # CLI Tools
│   ├── omega-repl.ts              # (UPDATED imports)
│   └── omega-debugger.ts          # (UPDATED imports)
│
├── src/
│   ├── index.ts                   # (EXTENDED with CLI exports)
│   │
│   ├── prims/                     # (NEW directory)
│   │   └── install.ts             # (MOVED from test/helpers/)
│   │
│   ├── core/                      # Internal (unchanged)
│   │   ├── eval/
│   │   │   ├── machine.ts
│   │   │   ├── machineStep.ts     # stepOnce() - now exported via index.ts
│   │   │   ├── run.ts             # runToCompletion() - now exported
│   │   │   ├── store.ts           # COWStore - now exported
│   │   │   ├── values.ts          # VUnit, VNum, etc - now exported
│   │   │   └── env.ts
│   │   ├── effects/
│   │   │   └── runtimeImpl.ts     # RuntimeImpl - now exported
│   │   ├── oracle/
│   │   │   ├── adapters/
│   │   │   │   └── index.ts       # createOpenAIAdapter - now exported
│   │   │   ├── snapshots.ts       # SnapshotRepo - now exported
│   │   │   └── receipts.ts        # InMemoryReceiptStore - now exported
│   │   └── pipeline/
│   │       └── compileText.ts     # compileTextToExpr - now exported
│   │
│   ├── frameir/                   # (unchanged)
│   ├── outcome/                   # (unchanged)
│   ├── registry/                  # (unchanged)
│   └── runtime.ts                 # (unchanged)
│
├── test/
│   ├── helpers/
│   │   ├── prims.ts               # (REMOVED - moved to src/prims/)
│   │   └── ...                    # Other helpers stay
│   └── ...
│
├── demo/                          # (unchanged)
│   ├── by-chapter/
│   └── lisp/
│
└── package.json                   # (UPDATED)
    {
      "bin": {
        "omega-repl": "./bin/omega-repl.ts",
        "omega-debugger": "./bin/omega-debugger.ts"
      },
      "scripts": {
        ...existing...,
        "repl": "tsx bin/omega-repl.ts",
        "debug": "tsx bin/omega-debugger.ts"
      }
    }
```

---

## Tasks

### Phase 1: Extend Public API
- [ ] Add ~25 CLI-related exports to existing `src/index.ts`
- [ ] Add section comment "CLI TOOLS - CEKS Machine (Low-Level)"

### Phase 2: Relocate Helpers
- [ ] Move `test/helpers/prims.ts` to `src/prims/install.ts`
- [ ] Update all test imports
- [ ] Update CLI imports

### Phase 3: Refactor CLI Imports
- [ ] Update `bin/omega-repl.ts` to use `import from "../src"`
- [ ] Update `bin/omega-debugger.ts` to use `import from "../src"`
- [ ] Verify no direct `src/core/` imports remain in bin/

### Phase 4: Package.json Updates
- [ ] Add `"bin"` entries for omega-repl and omega-debugger
- [ ] Add convenience scripts (`npm run repl`, `npm run debug`)

### Phase 5: Add Help Documentation
- [ ] Implement `--help` in omega-repl.ts
- [ ] Implement `--help` in omega-debugger.ts
- [ ] Add inline REPL `:help` command documentation

### Phase 6: Testing
- [ ] Verify `npx tsx bin/omega-repl.ts --help` works
- [ ] Verify `npx tsx bin/omega-debugger.ts --help` works
- [ ] Verify `npx tsx bin/omega-repl.ts --cmd "(+ 1 2)"` returns 3
- [ ] Verify session persistence still works
- [ ] Verify all tests still pass

---

## Verification Steps

```bash
# 1. Public API exports compile
npx tsc --noEmit src/index.ts

# 2. CLI tools only import from public API
grep -r "from.*src/core" bin/  # Should be empty after refactor

# 3. Help works
npx tsx bin/omega-repl.ts --help
npx tsx bin/omega-debugger.ts --help

# 4. Basic functionality
npx tsx bin/omega-repl.ts --cmd "(+ 1 2)"
# Should output: 3

# 5. Tests still pass
npm test
```

---

## Checklist

- [ ] `src/index.ts` extended with CLI exports (~25 new exports)
- [ ] `test/helpers/prims.ts` moved to `src/prims/install.ts`
- [ ] `bin/omega-repl.ts` uses only public imports
- [ ] `bin/omega-debugger.ts` uses only public imports
- [ ] `package.json` has bin entries
- [ ] `--help` implemented in both tools
- [ ] No `grep -r "from.*src/core" bin/` matches
- [ ] All tests pass
- [ ] REPL basic usage verified
- [ ] Debugger basic usage verified

---

## Dependencies

This job has **no dependencies** on other jobs. It's a cleanup/architecture task that can be done anytime.

However, completing this job makes future work cleaner:
- New CLI features can be added without touching core
- Library consumers have a stable API
- Documentation can reference the public API

---

## Notes

### Why Not a Separate Package?

We considered moving CLI tools to a separate npm package (`@omega/cli`), but:

1. **Monorepo overhead**: Adds build complexity
2. **Version sync**: CLI and core must stay in sync
3. **Single use case**: Most users want both

The current approach (single package, clear API boundary) is simpler.

### Test Strategy

Tests should **continue** to import from `src/core/` directly - this is intentional. Tests need access to internals for unit testing. The public API in `src/index.ts` is for **external consumers** (CLI tools, embedders).

```typescript
// Tests - OK to use internal paths
import { stepOnce } from "../../src/core/eval/machineStep";

// CLI tools - must use public API
import { stepOnce } from "../src";
```

---

## Proof of Completion

When this job is complete:

1. **Public API extended**
   ```bash
   grep -c "export" src/index.ts  # Should be ~60+ exports (was ~35)
   grep "CEKS Machine" src/index.ts  # Should show the new section header
   ```

2. **CLI tools use public API**
   ```bash
   grep -c 'from "../src"' bin/omega-repl.ts     # Should be 1
   grep -c 'from "../src/core' bin/omega-repl.ts  # Should be 0
   ```

3. **Help works**
   ```bash
   npx tsx bin/omega-repl.ts --help | head -10
   npx tsx bin/omega-debugger.ts --help | head -10
   ```

4. **Basic REPL works**
   ```bash
   echo "(+ 1 2)" | npx tsx bin/omega-repl.ts --cmd "(+ 1 2)"
   # Output: 3
   ```

5. **All tests pass**
   ```bash
   npm test
   ```

---

## Footer

| Field | Value |
|-------|-------|
| Created | 2025-01-21 |
| Last Updated | 2025-01-21 |
| Author | Claude |
| Related Docs | bin/omega-repl.ts, bin/omega-debugger.ts |
| Predecessors | None |
| Successors | None |
