# JOB-017d: CLI Unification - Single `omega` Command

**Priority**: P2 - Architecture Cleanup
**Estimated Effort**: 1 hour
**Skills Required**: TypeScript, npm packaging
**Status**: NOT STARTED
**Depends On**: 017a, 017b, 017c
**Blocks**: None (final job)

> **MUST READ**: [LOGISTICS.md](LOGISTICS.md) before starting.

---

## Executive Summary

This is the final job that brings together 017a, 017b, 017c:

1. **Rename** `bin/omega-repl.ts` → `bin/omega.ts`
2. **Delete** `bin/omega-debugger.ts`
3. **Update imports** in CLI to use public API
4. **Update package.json** bin entry
5. **Add comprehensive --help**

**Result**: Single `omega` command like `python`, `node`, `ghci`.

---

## Prerequisites

Before starting this job, verify:

- [ ] Job 017a complete: File loader handles multi-line S-expressions
- [ ] Job 017b complete: `src/index.ts` exports CLI tools
- [ ] Job 017c complete: REPL has all 7 merged debugger commands

---

## Implementation

### Step 1: Rename REPL File

```bash
cd OmegaLLM
git mv bin/omega-repl.ts bin/omega.ts
```

### Step 2: Update File Header

Open `bin/omega.ts` and update the header (lines 1-10):

**BEFORE:**
```typescript
#!/usr/bin/env npx tsx
// bin/omega-repl.ts - Omega Language REPL
// ...
```

**AFTER:**
```typescript
#!/usr/bin/env npx tsx
// bin/omega.ts
// Omega Language - Interactive REPL with debugging, LLM integration, and sessions
//
// Usage:
//   npx tsx bin/omega.ts              # Interactive REPL
//   npx tsx bin/omega.ts script.lisp  # Run file
//   npx tsx bin/omega.ts --cmd "(+ 1 2)"
//   npx tsx bin/omega.ts --help
//
// After npm link:
//   omega                             # Interactive REPL
//   omega script.lisp                 # Run file
```

### Step 3: Delete Debugger File

```bash
git rm bin/omega-debugger.ts
```

### Step 4: Update Imports to Use Public API

In `bin/omega.ts`, find all imports from `../src/core/` and replace with imports from `../src`:

**BEFORE** (scattered imports around lines 10-50):
```typescript
import { COWStore } from "../src/core/eval/store";
import { stepOnce } from "../src/core/eval/machineStep";
import { runToCompletionWithState, runToCompletion } from "../src/core/eval/run";
import { compileTextToExpr } from "../src/core/pipeline/compileText";
import type { State, Frame } from "../src/core/eval/machine";
import type { Val } from "../src/core/eval/values";
import type { Env } from "../src/core/eval/env";
import { RuntimeImpl } from "../src/core/effects/runtimeImpl";
import { InMemoryReceiptStore } from "../src/core/oracle/receipts";
import { SnapshotRepo } from "../src/core/oracle/snapshots";
import { createOpenAIAdapter } from "../src/core/oracle/adapters";
import { installPrims } from "../test/helpers/prims";
```

**AFTER** (single import block):
```typescript
import {
  // Core runtime
  OmegaRuntime,

  // CLI/Debugger support
  COWStore,
  stepOnce,
  runToCompletionWithState,
  runToCompletion,
  compileTextToExpr,
  RuntimeImpl,
  SnapshotRepo,
  InMemoryReceiptStore,
  createOpenAIAdapter,
  installPrims,

  // Types
  type Store,
  type StepResult,
  type State,
  type Frame,
  type Env,
  type Val,
  type ReceiptStore,
} from "../src";
```

**KEEP** these imports (they're Node.js/external):
```typescript
import * as readline from "readline";
import * as fs from "fs";
import * as path from "path";
```

### Step 5: Update package.json

Open `package.json` and update the `bin` section:

**BEFORE** (if exists):
```json
{
  "bin": {
    "omega-repl": "./bin/omega-repl.ts"
  }
}
```

**AFTER:**
```json
{
  "bin": {
    "omega": "./bin/omega.ts"
  },
  "scripts": {
    "omega": "tsx bin/omega.ts",
    "build": "tsc -p tsconfig.json",
    "test": "vitest run"
  }
}
```

### Step 6: Add Comprehensive --help

Find the `--help` handling in `bin/omega.ts` (or add if missing) and ensure it outputs:

```typescript
function showHelp(): void {
  console.log(`
omega - Omega Language REPL

USAGE
  omega [options]
  omega [file.lisp]
  omega --cmd <expression>

OPTIONS
  --session, -s <name>   Named session (persists to ~/.omega-sessions/)
  --file, -f <path>      Load and evaluate a .lisp file
  --cmd, -c <expr>       Evaluate expression/command and exit
  --json                 Output results as JSON
  --verbose              Show oracle/LLM transcripts
  --help, -h             Show this help

EXAMPLES
  omega                              # Interactive REPL
  omega demo.lisp                    # Run file
  omega --session work --cmd "(+ 1 2)"
  omega -s work -c ":debug (fact 5)" -c ":step 10"

REPL COMMANDS
  Expression Evaluation:
    (expression)                     Evaluate Lisp expression

  Session:
    :quit, :q                        Exit REPL
    :help, :h                        Show all commands
    :env [name]                      Show environment bindings
    :defs                            Show defined functions

  Debugging:
    :debug (expr)                    Load expression into debugger
    :step [N]                        Step N times (default 1)
    :run                             Run to completion/breakpoint
    :goto <N>                        Jump to step N (time-travel)
    :trace [start] [count]           Show execution trace
    :state                           Show current machine state
    :stack                           Show call stack
    :frame <N>                       Inspect stack frame N

  Breakpoints:
    :break step <N>                  Breakpoint at step N
    :break expr <type>               Breakpoint on expression type
    :break effect <op>               Breakpoint on effect operation
    :breaks                          List all breakpoints
    :delbreak <id>                   Delete breakpoint

  Trace Management:
    :dump <path>                     Save trace to JSON file
    :replay <path>                   Load and replay from dump
    :snapshot <name>                 Save named state snapshot
    :restore <name>                  Restore snapshot
    :snapshots                       List all snapshots
    :record on/off                   Toggle trace recording
    :history [N]                     Show last N steps

  LLM/Oracle:
    :ask <question>                  Ask LLM (requires API key)

ENVIRONMENT VARIABLES
  OPENAI_API_KEY                     OpenAI API key for oracle calls
  ANTHROPIC_API_KEY                  Anthropic API key for oracle calls
  OMEGA_VERBOSE                      Set to "1" for verbose mode

FILES
  ~/.omega-sessions/                 Session persistence directory
`);
}

// In argument parsing, add:
if (args.help) {
  showHelp();
  process.exit(0);
}
```

### Step 7: Verify No Internal Imports Remain

```bash
grep -n 'from "../src/core' bin/omega.ts
# Should return nothing

grep -n 'from "../test/helpers' bin/omega.ts
# Should return nothing
```

---

## Final State

### File Changes Summary

| Action | File | Notes |
|--------|------|-------|
| RENAME | `bin/omega-repl.ts` → `bin/omega.ts` | git mv |
| DELETE | `bin/omega-debugger.ts` | git rm |
| MODIFY | `bin/omega.ts` | Update header, imports, add --help |
| MODIFY | `package.json` | Update bin entry |

### Directory Structure After

```
bin/
└── omega.ts          # Single CLI tool (was omega-repl.ts)
                      # omega-debugger.ts DELETED
```

### Import Structure After

```typescript
// bin/omega.ts - CLEAN imports
import {
  OmegaRuntime,
  COWStore,
  stepOnce,
  // ... all from public API
} from "../src";

// NOT this (old style):
// import { COWStore } from "../src/core/eval/store";  ❌
```

---

## Verification

### Test 1: Single CLI File Exists

```bash
ls bin/*.ts
# Should show ONLY: bin/omega.ts
# Should NOT show: bin/omega-repl.ts, bin/omega-debugger.ts
```

### Test 2: Help Works

```bash
npx tsx bin/omega.ts --help
# Should show comprehensive help with all commands
```

### Test 3: Basic Evaluation

```bash
npx tsx bin/omega.ts --cmd "(+ 1 2 3)"
# Should output: 6
```

### Test 4: File Execution

```bash
npx tsx bin/omega.ts demo/lisp/ch02-llm-calls.lisp
# Should execute (may show oracle errors without API key, but no import errors)
```

### Test 5: Debugging Works

```bash
npx tsx bin/omega.ts --cmd ":debug (+ 1 2)" --cmd ":step 3" --cmd ":state"
# Should show debug state
```

### Test 6: New Merged Commands Work

```bash
npx tsx bin/omega.ts --cmd ":debug (+ 1 2)" --cmd ":run" --cmd ":dump /tmp/t.json"
# Should create dump file

npx tsx bin/omega.ts --cmd ":debug (+ 1 2)" --cmd ":step" --cmd ":snapshot test" --cmd ":snapshots"
# Should list snapshot "test"
```

### Test 7: No Internal Imports

```bash
grep -c 'from "../src/core' bin/omega.ts
# Should output: 0

grep -c 'from "../test/helpers' bin/omega.ts
# Should output: 0
```

### Test 8: Package.json Correct

```bash
grep '"omega"' package.json
# Should show bin entry for omega
```

### Test 9: All Tests Pass

```bash
npm test
# All tests should pass
```

---

## Checklist

- [ ] Prerequisites verified (017a, 017b, 017c complete)
- [ ] Renamed `bin/omega-repl.ts` → `bin/omega.ts`
- [ ] Deleted `bin/omega-debugger.ts`
- [ ] Updated file header in `bin/omega.ts`
- [ ] Replaced all `../src/core/` imports with `../src` imports
- [ ] Replaced `../test/helpers/` imports with `../src` imports
- [ ] Updated `package.json` bin entry to `"omega"`
- [ ] Added comprehensive `showHelp()` function
- [ ] Test 1 passes (single file)
- [ ] Test 2 passes (--help)
- [ ] Test 3 passes (basic eval)
- [ ] Test 4 passes (file execution)
- [ ] Test 5 passes (debugging)
- [ ] Test 6 passes (merged commands)
- [ ] Test 7 passes (no internal imports)
- [ ] Test 8 passes (package.json)
- [ ] Test 9 passes (all tests)

---

## Proof of Completion

```bash
# 1. Single CLI tool
ls bin/*.ts | wc -l
# Should output: 1

ls bin/*.ts
# Should output: bin/omega.ts

# 2. No internal imports
grep -c 'from "../src/core' bin/omega.ts
# Should output: 0

# 3. Help shows all commands
npx tsx bin/omega.ts --help | grep -c -E ":dump|:replay|:snapshot"
# Should output: 3+

# 4. Basic functionality
npx tsx bin/omega.ts --cmd "(* 6 7)"
# Should output: 42

# 5. All tests pass
npm test && echo "ALL TESTS PASS"
```

---

## Notes

### Why This Unification Matters

1. **Simpler**: Users learn one tool, not two
2. **Standard**: Matches Python, Node, SBCL, GHCi patterns
3. **Agent-Friendly**: Single command with `--session` for state persistence
4. **Less Code**: ~1,200 lines removed (debugger file)
5. **Maintainable**: One codebase to update

### Migration Path for Users

| Old Command | New Command |
|-------------|-------------|
| `npx tsx bin/omega-repl.ts` | `npx tsx bin/omega.ts` |
| `npx tsx bin/omega-debugger.ts` | `npx tsx bin/omega.ts` (use :debug) |
| After `npm link`: `omega-repl` | After `npm link`: `omega` |

---

## Footer

| Field | Value |
|-------|-------|
| Created | 2025-01-21 |
| Author | Claude |
| Part Of | Job 017 (CLI Unification) |
| Predecessors | 017a, 017b, 017c |
| Related Files | `bin/omega.ts`, `package.json` |
