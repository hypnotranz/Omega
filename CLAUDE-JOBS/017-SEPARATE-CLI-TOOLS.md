# JOB-017: Unify CLI into Single `omega` Command

**Priority**: P2 - Cleanup/Architecture
**Estimated Effort**: 1-2 days
**Skills Required**: TypeScript, CLI design, npm packaging
**Status**: NOT STARTED
**Depends On**: None (standalone cleanup task)

> **MUST READ**: [LOGISTICS.md](LOGISTICS.md) before starting

---

## Sub-Jobs (Parallelizable)

This job is split into 4 sub-jobs that can be run in parallel (except 017d):

| Sub-Job | Title | Can Parallel? | Depends On |
|---------|-------|---------------|------------|
| [017a](./017a-FILE-LOADER-FIX.md) | Fix File Loader | ✅ Yes | None |
| [017b](./017b-PUBLIC-API-EXTENSION.md) | Extend Public API | ✅ Yes | None |
| [017c](./017c-MERGE-DEBUGGER-COMMANDS.md) | Merge Debugger Commands | ✅ Yes | None |
| [017d](./017d-CLI-UNIFICATION.md) | Final Unification | ❌ No | 017a, 017b, 017c |

### Run with Beads (Parallel)

```bash
cd OmegaLLM

# Import features to beads
python ../codesmith/cli.py import CLAUDE-JOBS/017-features.yaml

# Run with 2 parallel workers
python ../codesmith/cli.py run --max-workers 2
```

### Dependency Graph

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│    017a     │     │    017b     │     │    017c     │
│ File Loader │     │ Public API  │     │ Merge Cmds  │
└──────┬──────┘     └──────┬──────┘     └──────┬──────┘
       │                   │                   │
       └───────────────────┼───────────────────┘
                           │
                           ▼
                    ┌─────────────┐
                    │    017d     │
                    │ Unification │
                    └─────────────┘
``` - covers testing, file locations, and proof of completion requirements.

---

## Executive Summary

The OmegaLLM project currently has **two CLI tools** that significantly overlap:

| Tool | File | Lines | Purpose |
|------|------|-------|---------|
| REPL | `bin/omega-repl.ts` | ~2,233 | Interactive REPL + LLM + **full debugging** |
| Debugger | `bin/omega-debugger.ts` | ~1,198 | Step-through debugger |

**Key finding**: The REPL already has full debugging built in (`:debug`, `:step`, `:run`, `:goto`, `:break`, `:trace`, `:state`, `:stack`, `:frame`). The debugger is redundant.

### What Other Languages Do

| Language | CLI Tool | Debugging |
|----------|----------|-----------|
| Python | `python` | `pdb.set_trace()` inside REPL |
| Node.js | `node` | `--inspect` flag, same process |
| Common Lisp | `sbcl` | Built into REPL |
| GHCi | `ghci` | `:step`, `:trace`, `:break` built in |
| Racket | `racket` | Debugger integrated |

**Pattern**: One tool, debugging as a mode within the REPL.

### This Job

1. **Merge** the 6 unique debugger features into the REPL
2. **Rename** `bin/omega-repl.ts` → `bin/omega.ts`
3. **Delete** `bin/omega-debugger.ts`
4. **Extend** `src/index.ts` with low-level CEKS exports for the CLI
5. **Add** proper `package.json` bin entry

**Result**: Single `omega` command like `python`, `node`, `ghci`.

---

## Feature Comparison

### What REPL Already Has (Debugging)

```
:debug (expr)    — load expression into debugger
:step [N]        — execute N steps
:run             — run to completion/breakpoint
:goto <N>        — time-travel to step N
:trace [s] [n]   — show execution trace
:state           — show current debug state
:stack           — show call stack
:frame <N>       — inspect frame
:break step <N>  — breakpoint at step
:break expr <t>  — breakpoint on expression type
:break effect <o>— breakpoint on effect
:breaks          — list breakpoints
:delbreak <id>   — delete breakpoint
```

### What Debugger Has (Unique - Must Merge)

| Command | Purpose |
|---------|---------|
| `dump <path>` | Save full execution trace to JSON file |
| `replay <path>` | Load and replay trace from file |
| `save <name>` | Save named state snapshot |
| `restore <name>` | Restore named snapshot |
| `record on/off` | Toggle trace recording |
| `history [n]` | Show recent step history |

These 6 commands will be added to the REPL as `:dump`, `:replay`, `:snapshot`, `:restore`, `:record`, `:history`.

---

## CLI Design (After Merge)

### Usage

```bash
# Interactive REPL
omega

# File execution
omega --file script.lisp
omega script.lisp              # shorthand

# Non-interactive (for agents)
omega --cmd "(define x 42)"
omega --session work --cmd "(+ x 1)"    # resume session

# Debugging via commands (same tool)
omega --cmd ":debug (broken-fn)"
omega --session work --cmd ":step 5"
omega --session work --cmd ":state"
omega --session work --cmd ":dump trace.json"

# JSON output for parsing
omega --cmd "(+ 1 2)" --json
# → {"result": 3, "ok": true}
```

### Flags

| Flag | Description |
|------|-------------|
| `--session <name>` | Named session (persists to `~/.omega-sessions/`) |
| `--file <path>` | Load and evaluate Lisp file |
| `--cmd <expr>` | Evaluate expression/command and exit |
| `--json` | Output results as JSON |
| `--verbose` | Show oracle/LLM transcripts |
| `--help` | Show help |

### Sessions (Agent-Friendly)

Sessions allow agents to maintain state across invocations:

```bash
# Agent turn 1: define something
omega --session mywork --cmd "(define factorial (lambda (n) (if (<= n 1) 1 (* n (factorial (- n 1))))))"

# Agent turn 2: use it (session remembers factorial)
omega --session mywork --cmd "(factorial 5)"
# → 120

# Agent turn 3: debug something
omega --session mywork --cmd ":debug (factorial 3)"
omega --session mywork --cmd ":step 10"
omega --session mywork --cmd ":state"

# Agent turn 4: dump trace for analysis
omega --session mywork --cmd ":dump /tmp/trace.json"
```

---

## Implementation Plan

### Phase 1: Merge Debugger Features into REPL

Add these commands to `bin/omega-repl.ts`:

```typescript
// Add to REPL command handling

// :dump - save trace to file
if (trimmed.startsWith(":dump ")) {
  const filepath = trimmed.slice(6).trim();
  if (!replState.debugTrace || replState.debugTrace.length === 0) {
    log("No debug trace to dump. Use :debug first.");
  } else {
    const data = JSON.stringify({
      code: replState.debugInitialCode,
      trace: replState.debugTrace,
      step: replState.debugStepCount,
    }, null, 2);
    fs.writeFileSync(filepath, data);
    log(`Trace dumped to ${filepath} (${replState.debugTrace.length} steps)`);
  }
  return { replState, output: output.join("\n"), shouldExit };
}

// :replay - load trace from file
if (trimmed.startsWith(":replay ")) {
  const filepath = trimmed.slice(8).trim();
  if (!fs.existsSync(filepath)) {
    log(`File not found: ${filepath}`);
  } else {
    const data = JSON.parse(fs.readFileSync(filepath, "utf8"));
    replState = debugLoadExpr(data.code, replState);
    replState.debugTrace = data.trace;
    log(`Loaded trace with ${data.trace.length} steps. Use :goto <n> to navigate.`);
  }
  return { replState, output: output.join("\n"), shouldExit };
}

// :snapshot - save named snapshot
if (trimmed.startsWith(":snapshot ")) {
  const name = trimmed.slice(10).trim();
  // Save current debug state to replState.snapshots map
  replState.snapshots = replState.snapshots || new Map();
  replState.snapshots.set(name, {
    state: cloneState(replState.debugState),
    step: replState.debugStepCount,
    trace: [...replState.debugTrace],
  });
  log(`Snapshot '${name}' saved at step ${replState.debugStepCount}`);
  return { replState, output: output.join("\n"), shouldExit };
}

// :restore - restore named snapshot
if (trimmed.startsWith(":restore ")) {
  const name = trimmed.slice(9).trim();
  const snap = replState.snapshots?.get(name);
  if (!snap) {
    log(`Snapshot '${name}' not found. Use :snapshots to list.`);
  } else {
    replState.debugState = cloneState(snap.state);
    replState.debugStepCount = snap.step;
    log(`Restored snapshot '${name}' at step ${snap.step}`);
    printDebugState(replState);
  }
  return { replState, output: output.join("\n"), shouldExit };
}

// :snapshots - list snapshots
if (trimmed === ":snapshots") {
  if (!replState.snapshots || replState.snapshots.size === 0) {
    log("No snapshots saved.");
  } else {
    log("Snapshots:");
    for (const [name, snap] of replState.snapshots) {
      log(`  ${name}: step ${snap.step}`);
    }
  }
  return { replState, output: output.join("\n"), shouldExit };
}

// :record - toggle trace recording
if (trimmed === ":record on" || trimmed === ":record off") {
  replState.recordingEnabled = trimmed === ":record on";
  log(`Trace recording: ${replState.recordingEnabled ? "ON" : "OFF"}`);
  return { replState, output: output.join("\n"), shouldExit };
}

// :history - show recent step history
if (trimmed === ":history" || trimmed.startsWith(":history ")) {
  const parts = trimmed.split(/\s+/);
  const count = parseInt(parts[1]) || 10;
  if (!replState.debugTrace || replState.debugTrace.length === 0) {
    log("No history. Use :debug first.");
  } else {
    const start = Math.max(0, replState.debugStepCount - count);
    log(`History (steps ${start}-${replState.debugStepCount}):`);
    for (let i = start; i <= replState.debugStepCount && i < replState.debugTrace.length; i++) {
      const t = replState.debugTrace[i];
      const marker = i === replState.debugStepCount ? " <-- current" : "";
      log(`  [${i}] ${t.controlSummary}${marker}`);
    }
  }
  return { replState, output: output.join("\n"), shouldExit };
}
```

### Phase 2: Rename REPL to omega

```bash
git mv bin/omega-repl.ts bin/omega.ts
```

Update the file header:

```typescript
#!/usr/bin/env npx tsx
// bin/omega.ts
// Omega Language - Interactive REPL with debugging, LLM integration, and sessions
//
// Run:  npx tsx bin/omega.ts
//       omega                       (after npm link)
```

### Phase 3: Delete Debugger

```bash
git rm bin/omega-debugger.ts
```

### Phase 4: Extend Public API

**ADD** the following exports to existing `src/index.ts`:

```typescript
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

### Phase 5: Relocate Test Helpers

Move `test/helpers/prims.ts` to `src/prims/install.ts`:

```bash
mkdir -p src/prims
git mv test/helpers/prims.ts src/prims/install.ts
```

Update imports in tests and CLI.

### Phase 6: Update package.json

```json
{
  "bin": {
    "omega": "./dist/bin/omega.js"
  },
  "scripts": {
    "build": "tsc -p tsconfig.json",
    "test": "vitest run",
    "omega": "tsx bin/omega.ts",
    "prepublishOnly": "npm run build && npm test"
  }
}
```

### Phase 7: Refactor CLI Imports

Update `bin/omega.ts` to import from public API:

```typescript
// BEFORE (16+ internal imports)
import { COWStore } from "../src/core/eval/store";
import { RuntimeImpl } from "../src/core/effects/runtimeImpl";
import { stepOnce } from "../src/core/eval/machineStep";
// ... many more

// AFTER (single import)
import {
  COWStore,
  RuntimeImpl,
  stepOnce,
  runToCompletionWithState,
  compileTextToExpr,
  createOpenAIAdapter,
  SnapshotRepo,
  InMemoryReceiptStore,
  installPrims,
  type State,
  type Frame,
  type Val,
} from "../src";
```

### Phase 8: Add --help

```typescript
function showHelp(): void {
  console.log(`
omega - Omega Language REPL

USAGE
  omega [options]
  omega [file.lisp]
  omega --cmd <expression>

OPTIONS
  --session <name>   Named session (persists to ~/.omega-sessions/)
  --file <path>      Load and evaluate a .lisp file
  --cmd <expr>       Evaluate expression/command and exit
  --json             Output results as JSON
  --verbose          Show oracle/LLM transcripts
  --help             Show this help

EXAMPLES
  omega                              # Interactive REPL
  omega demo.lisp                    # Run file
  omega --session work --cmd "(+ 1 2)"

REPL COMMANDS
  (expression)       Evaluate Lisp expression

  :help              Show all commands
  :quit              Exit

  :debug (expr)      Load expression into debugger
  :step [N]          Step N times (default 1)
  :run               Run to completion/breakpoint
  :goto <N>          Jump to step N (time-travel)
  :trace [s] [n]     Show execution trace
  :state             Show current state
  :stack             Show call stack

  :break step <N>    Breakpoint at step N
  :break expr <t>    Breakpoint on expression type
  :breaks            List breakpoints
  :delbreak <id>     Delete breakpoint

  :dump <path>       Save trace to JSON file
  :replay <path>     Load trace from file
  :snapshot <name>   Save named snapshot
  :restore <name>    Restore snapshot
  :history [N]       Show recent N steps

  :ask <question>    Ask LLM (agentic mode)
  :env [name]        Show environment bindings
  :defs              Show defined functions

ENVIRONMENT
  OPENAI_API_KEY      OpenAI API key for :ask
  ANTHROPIC_API_KEY   Anthropic API key for :ask
`);
}
```

---

## Output Files

```
BEFORE                              AFTER
------                              -----
bin/
  omega-repl.ts (2,233 lines)  →    omega.ts (~2,350 lines, merged)
  omega-debugger.ts (1,198)    →    (DELETED)

src/
  index.ts (140 lines)         →    index.ts (~165 lines, extended)
  prims/                       →    prims/
                                      install.ts (MOVED from test/helpers/)

test/helpers/
  prims.ts                     →    (MOVED to src/prims/)

package.json                   →    package.json (updated bin entry)
```

---

## Tasks

### Phase 1: Fix File Loader Bug
- [ ] Fix `--file` to handle multi-line S-expressions (currently splits by newline)
- [ ] Use `extractSexpressions()` instead of `.split("\n")`
- [ ] Strip comments before parsing
- [ ] Test with `demo/lisp/ch03-composition.lisp` (has multi-line defines)

**Bug location**: `bin/omega-repl.ts:2057`
```typescript
// BEFORE (broken)
const commands = fs.readFileSync(filePath, "utf8").split("\n");

// AFTER (fixed)
const content = fs.readFileSync(filePath, "utf8");
const cleanedContent = content
  .split("\n")
  .filter(line => !line.trim().startsWith(";"))
  .join("\n");
const sexprs = extractSexpressions(cleanedContent);
```

### Phase 2: Merge Debugger Features
- [ ] Add `:dump` command to REPL
- [ ] Add `:replay` command to REPL
- [ ] Add `:snapshot` command to REPL
- [ ] Add `:restore` command to REPL
- [ ] Add `:snapshots` command to REPL
- [ ] Add `:record` command to REPL
- [ ] Add `:history` command to REPL
- [ ] Update `:help` to show new commands

### Phase 3: Rename/Delete
- [ ] Rename `bin/omega-repl.ts` → `bin/omega.ts`
- [ ] Delete `bin/omega-debugger.ts`
- [ ] Update file headers and comments

### Phase 4: Extend Public API
- [ ] Add ~25 CLI-related exports to `src/index.ts`
- [ ] Add section comment "CLI TOOLS - CEKS Machine"

### Phase 5: Relocate Helpers
- [ ] Move `test/helpers/prims.ts` → `src/prims/install.ts`
- [ ] Update all test imports
- [ ] Update CLI imports

### Phase 6: Refactor CLI Imports
- [ ] Update `bin/omega.ts` to use `import from "../src"`
- [ ] Verify no direct `src/core/` imports remain

### Phase 7: Package.json
- [ ] Update `"bin"` entry to point to `omega`
- [ ] Update scripts

### Phase 8: Add Help
- [ ] Implement comprehensive `--help`
- [ ] Ensure `:help` shows all commands

### Phase 9: Testing
- [ ] Verify `omega --help` works
- [ ] Verify `omega --cmd "(+ 1 2)"` returns 3
- [ ] Verify `omega --session test --cmd ":debug (+ 1 2)"` works
- [ ] Verify `omega --session test --cmd ":step"` works
- [ ] Verify `omega --session test --cmd ":dump /tmp/t.json"` works
- [ ] Verify all tests pass

---

## Verification Steps

```bash
# 1. Single CLI tool exists
ls bin/*.ts
# Should show only: omega.ts

# 2. Help works
npx tsx bin/omega.ts --help

# 3. Basic evaluation
npx tsx bin/omega.ts --cmd "(+ 1 2)"
# Should output: 3

# 4. Debugging works
npx tsx bin/omega.ts --cmd ":debug (+ 1 2)" --json
npx tsx bin/omega.ts --cmd ":step" --json
npx tsx bin/omega.ts --cmd ":state" --json

# 5. Sessions work
npx tsx bin/omega.ts --session test --cmd "(define x 42)"
npx tsx bin/omega.ts --session test --cmd "x"
# Should output: 42

# 6. Dump/replay work
npx tsx bin/omega.ts --session test --cmd ":debug (factorial 3)"
npx tsx bin/omega.ts --session test --cmd ":run"
npx tsx bin/omega.ts --session test --cmd ":dump /tmp/trace.json"
cat /tmp/trace.json | head

# 7. No internal imports in CLI
grep -c 'from "../src/core' bin/omega.ts
# Should output: 0

# 8. All tests pass
npm test
```

---

## Checklist

- [ ] 6 new commands merged into REPL (`:dump`, `:replay`, `:snapshot`, `:restore`, `:record`, `:history`)
- [ ] `bin/omega-repl.ts` renamed to `bin/omega.ts`
- [ ] `bin/omega-debugger.ts` deleted
- [ ] `src/index.ts` extended with CLI exports
- [ ] `test/helpers/prims.ts` moved to `src/prims/install.ts`
- [ ] `bin/omega.ts` uses only public imports
- [ ] `package.json` has single bin entry for `omega`
- [ ] `--help` comprehensive
- [ ] Session persistence works
- [ ] Debug mode works via `--cmd`
- [ ] All tests pass

---

## Notes

### Why Merge?

1. **Simpler**: One tool to learn, document, maintain
2. **Standard**: Matches Python, Node, SBCL, GHCi
3. **Agent-friendly**: Single command with sessions handles all use cases
4. **Less code**: Remove ~1,198 lines of duplication

### What We Keep from Debugger

The 6 unique features (dump/replay/snapshots) are valuable for:
- Sharing execution traces
- Offline analysis
- Reproducing bugs

These are now available in the unified REPL.

### Session Format

Sessions store to `~/.omega-sessions/<name>.json`:

```json
{
  "env": { "x": 42, "factorial": "<closure>" },
  "history": ["(define x 42)", "(factorial 5)"],
  "debugState": null,
  "debugTrace": [],
  "snapshots": {}
}
```

---

## Proof of Completion

1. **Single tool**
   ```bash
   ls bin/*.ts | wc -l
   # Should output: 1
   ```

2. **New commands work**
   ```bash
   npx tsx bin/omega.ts --cmd ":help" | grep -c "dump\|replay\|snapshot"
   # Should output: 3+
   ```

3. **No internal imports**
   ```bash
   grep 'from "../src/core' bin/omega.ts
   # Should output nothing
   ```

4. **All tests pass**
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
