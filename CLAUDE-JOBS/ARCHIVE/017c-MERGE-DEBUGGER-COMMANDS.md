# JOB-017c: Merge Debugger Commands into REPL

**Priority**: P2 - Feature Merge
**Estimated Effort**: 2 hours
**Skills Required**: TypeScript
**Status**: NOT STARTED
**Depends On**: None
**Blocks**: 017d (CLI Unification)

> **MUST READ**: [LOGISTICS.md](LOGISTICS.md) before starting.

---

## Executive Summary

The REPL already has full debugging (`:debug`, `:step`, `:run`, `:goto`, `:break`, `:trace`, `:state`, `:stack`, `:frame`). The standalone debugger has 6 unique commands that must be merged:

| Debugger Command | REPL Command | Purpose |
|------------------|--------------|---------|
| `dump <path>` | `:dump <path>` | Save execution trace to JSON file |
| `replay <path>` | `:replay <path>` | Load and re-execute from dump file |
| `save <name>` | `:snapshot <name>` | Save named state snapshot |
| `restore <name>` | `:restore <name>` | Restore named snapshot |
| `record on/off` | `:record on/off` | Toggle trace recording |
| `history [n]` | `:history [n]` | Show recent step history |

---

## Implementation

### Step 1: Add State Fields to ReplState

Find the `ReplState` interface in `bin/omega-repl.ts` (around line 100-150).

**ADD** these fields:

```typescript
interface ReplState {
  // ... existing fields ...

  // NEW: Debugger merge - Snapshots
  snapshots: Map<string, {
    debugState: State | null;
    debugStepCount: number;
    debugTrace: TraceRecord[];
  }>;

  // NEW: Debugger merge - Recording
  recordingEnabled: boolean;
}
```

**ADD** the TraceRecord type (if not already present):

```typescript
interface TraceRecord {
  step: number;
  controlSummary: string;
  stackDepth: number;
  state: State;
}
```

### Step 2: Initialize New Fields

Find `initReplState()` function and add initialization:

```typescript
async function initReplState(): Promise<ReplState> {
  // ... existing initialization ...

  return {
    // ... existing fields ...

    // NEW: Debugger merge
    snapshots: new Map(),
    recordingEnabled: true, // Record by default
  };
}
```

### Step 3: Add :dump Command

Find the command handling section in `processReplCommand()` (look for `:trace` or `:state` commands, around line 1400-1800).

**ADD** after the `:trace` command handling:

```typescript
// :dump - Save trace to JSON file
if (trimmed.startsWith(":dump ")) {
  const filepath = trimmed.slice(6).trim();
  if (!filepath) {
    log("Usage: :dump <filepath>");
    return { replState, output: output.join("\n"), shouldExit };
  }

  if (!replState.debugMode || replState.trace.length === 0) {
    log("No debug trace to dump. Use :debug first, then :run or :step.");
    return { replState, output: output.join("\n"), shouldExit };
  }

  const dumpData = {
    version: 1,
    code: replState.debugInitialCode || "",
    timestamp: new Date().toISOString(),
    totalSteps: replState.trace.length,
    currentStep: replState.stepCount,
    traceSummary: replState.trace.map((t: any) => ({
      step: t.step,
      control: t.controlSummary || t.control || "?",
      stackDepth: t.stackDepth || 0,
    })),
  };

  try {
    fs.writeFileSync(filepath, JSON.stringify(dumpData, null, 2));
    log(`Dumped trace to ${filepath} (${replState.trace.length} steps)`);
  } catch (err: any) {
    log(`Error writing dump: ${err.message}`);
  }

  return { replState, output: output.join("\n"), shouldExit };
}
```

### Step 4: Add :replay Command

```typescript
// :replay - Load and replay from dump file
if (trimmed.startsWith(":replay ")) {
  const filepath = trimmed.slice(8).trim();
  if (!filepath) {
    log("Usage: :replay <filepath>");
    return { replState, output: output.join("\n"), shouldExit };
  }

  const resolved = path.resolve(filepath);
  if (!fs.existsSync(resolved)) {
    log(`File not found: ${resolved}`);
    return { replState, output: output.join("\n"), shouldExit };
  }

  try {
    const content = fs.readFileSync(resolved, "utf8");
    const data = JSON.parse(content);

    if (!data.code) {
      log("Dump file missing 'code' field - cannot replay.");
      return { replState, output: output.join("\n"), shouldExit };
    }

    log(`Loading dump from: ${resolved}`);
    log(`Original timestamp: ${data.timestamp}`);
    log(`Total steps: ${data.totalSteps}`);

    // Load the code into debugger
    const loadResult = await processReplCommand(`:debug ${data.code}`, replState);
    replState = loadResult.replState;

    // Run to completion to rebuild trace
    log("Replaying execution...");
    const runResult = await processReplCommand(`:run`, replState);
    replState = runResult.replState;

    log(`Replay complete. Use :goto <step> to navigate.`);
  } catch (err: any) {
    log(`Error loading dump: ${err.message}`);
  }

  return { replState, output: output.join("\n"), shouldExit };
}
```

### Step 5: Add :snapshot Command

```typescript
// :snapshot - Save named snapshot
if (trimmed.startsWith(":snapshot ")) {
  const name = trimmed.slice(10).trim();
  if (!name) {
    log("Usage: :snapshot <name>");
    return { replState, output: output.join("\n"), shouldExit };
  }

  if (!replState.debugMode || !replState.debugState) {
    log("No debug state to snapshot. Use :debug first.");
    return { replState, output: output.join("\n"), shouldExit };
  }

  // Clone the current debug state
  const snapshot = {
    debugState: JSON.parse(JSON.stringify(replState.debugState)),
    debugStepCount: replState.stepCount,
    debugTrace: [...replState.trace],
  };

  replState.snapshots.set(name, snapshot);
  log(`Snapshot '${name}' saved at step ${replState.stepCount}`);

  return { replState, output: output.join("\n"), shouldExit };
}

// :snapshots - List all snapshots
if (trimmed === ":snapshots") {
  if (replState.snapshots.size === 0) {
    log("No snapshots saved.");
  } else {
    log("Snapshots:");
    for (const [name, snap] of replState.snapshots) {
      log(`  ${name}: step ${snap.debugStepCount}`);
    }
  }
  return { replState, output: output.join("\n"), shouldExit };
}
```

### Step 6: Add :restore Command

```typescript
// :restore - Restore named snapshot
if (trimmed.startsWith(":restore ")) {
  const name = trimmed.slice(9).trim();
  if (!name) {
    log("Usage: :restore <name>");
    return { replState, output: output.join("\n"), shouldExit };
  }

  const snapshot = replState.snapshots.get(name);
  if (!snapshot) {
    log(`Snapshot '${name}' not found. Use :snapshots to list.`);
    return { replState, output: output.join("\n"), shouldExit };
  }

  replState.debugState = JSON.parse(JSON.stringify(snapshot.debugState));
  replState.stepCount = snapshot.debugStepCount;
  replState.trace = [...snapshot.debugTrace];

  log(`Restored snapshot '${name}' at step ${snapshot.debugStepCount}`);

  return { replState, output: output.join("\n"), shouldExit };
}
```

### Step 7: Add :record Command

```typescript
// :record - Toggle trace recording
if (trimmed === ":record on" || trimmed === ":record off") {
  replState.recordingEnabled = trimmed === ":record on";
  log(`Trace recording: ${replState.recordingEnabled ? "ON" : "OFF"}`);
  if (!replState.recordingEnabled) {
    log("Warning: Without recording, :goto and :dump won't capture new steps.");
  }
  return { replState, output: output.join("\n"), shouldExit };
}

if (trimmed === ":record") {
  log(`Trace recording is ${replState.recordingEnabled ? "ON" : "OFF"}`);
  return { replState, output: output.join("\n"), shouldExit };
}
```

### Step 8: Add :history Command

```typescript
// :history - Show recent step history
if (trimmed === ":history" || trimmed.startsWith(":history ")) {
  const parts = trimmed.split(/\s+/);
  const count = parseInt(parts[1]) || 10;

  if (!replState.debugMode || replState.trace.length === 0) {
    log("No history. Use :debug first, then :step or :run.");
    return { replState, output: output.join("\n"), shouldExit };
  }

  const start = Math.max(0, replState.stepCount - count);
  const end = Math.min(replState.trace.length, replState.stepCount + 1);

  log(`History (steps ${start}-${replState.stepCount}):`);
  for (let i = start; i < end && i < replState.trace.length; i++) {
    const t = replState.trace[i];
    const marker = i === replState.stepCount ? " <-- current" : "";
    const summary = (t as any).controlSummary || (t as any).control || "?";
    log(`  [${i}] ${summary}${marker}`);
  }

  return { replState, output: output.join("\n"), shouldExit };
}
```

### Step 9: Update :help Command

Find the `:help` command output and add the new commands:

```typescript
// In :help output, add these lines:
log("  :dump <path>     Save execution trace to JSON file");
log("  :replay <path>   Load and replay trace from file");
log("  :snapshot <name> Save named state snapshot");
log("  :restore <name>  Restore named snapshot");
log("  :snapshots       List all saved snapshots");
log("  :record on/off   Toggle trace recording");
log("  :history [n]     Show last n steps of history");
```

---

## Final State

### File Changes

**`bin/omega-repl.ts`** - Multiple additions:

| Location | Change |
|----------|--------|
| ReplState interface | Add `snapshots`, `recordingEnabled` fields |
| initReplState() | Initialize new fields |
| processReplCommand() | Add 7 new command handlers |
| :help output | Add 7 new command descriptions |

### New Commands Summary

| Command | Args | Description |
|---------|------|-------------|
| `:dump` | `<filepath>` | Export trace to JSON |
| `:replay` | `<filepath>` | Load dump and re-execute |
| `:snapshot` | `<name>` | Save current state with name |
| `:snapshots` | none | List all snapshots |
| `:restore` | `<name>` | Restore named snapshot |
| `:record` | `on/off` or none | Toggle/show recording status |
| `:history` | `[n]` | Show last n steps (default 10) |

---

## Verification

### Test 1: Dump and Replay

```bash
cd OmegaLLM
npx tsx bin/omega-repl.ts

# In REPL:
:debug (define (fact n) (if (<= n 1) 1 (* n (fact (- n 1)))))
:run
:debug (fact 5)
:run
:dump /tmp/fact-trace.json
:quit

# Verify dump file
cat /tmp/fact-trace.json | head -20

# Test replay
npx tsx bin/omega-repl.ts
:replay /tmp/fact-trace.json
:goto 10
:state
:quit
```

### Test 2: Snapshots

```bash
npx tsx bin/omega-repl.ts

# In REPL:
:debug (+ 1 2 3)
:step 3
:snapshot before-add
:step 5
:state
:restore before-add
:state
# Should show step 3 state, not step 8
:snapshots
# Should list "before-add"
:quit
```

### Test 3: Record Toggle

```bash
npx tsx bin/omega-repl.ts

# In REPL:
:debug (+ 1 2)
:record
# Should show "ON"
:record off
:record
# Should show "OFF"
:step 5
:history
# Should warn about limited history
:quit
```

### Test 4: History

```bash
npx tsx bin/omega-repl.ts

# In REPL:
:debug (define x 42)
:run
:debug (+ x 1)
:step 10
:history 5
# Should show last 5 steps with current marker
:quit
```

### Test 5: Help Updated

```bash
npx tsx bin/omega-repl.ts --cmd ":help" | grep -E "dump|replay|snapshot|restore|record|history"
# Should show all 7 new commands
```

---

## Checklist

- [ ] Added `snapshots` field to ReplState interface
- [ ] Added `recordingEnabled` field to ReplState interface
- [ ] Added TraceRecord interface (if needed)
- [ ] Initialized new fields in initReplState()
- [ ] Added `:dump` command handler
- [ ] Added `:replay` command handler
- [ ] Added `:snapshot` command handler
- [ ] Added `:snapshots` command handler
- [ ] Added `:restore` command handler
- [ ] Added `:record` command handler
- [ ] Added `:history` command handler
- [ ] Updated `:help` output with all 7 commands
- [ ] Test 1 passes (dump/replay)
- [ ] Test 2 passes (snapshots)
- [ ] Test 3 passes (record toggle)
- [ ] Test 4 passes (history)
- [ ] Test 5 passes (help shows new commands)

---

## Proof of Completion

```bash
# 1. New commands in help
npx tsx bin/omega-repl.ts --cmd ":help" 2>&1 | grep -c -E ":dump|:replay|:snapshot|:restore|:record|:history"
# Should output: 7 (or close to it)

# 2. Dump works
npx tsx bin/omega-repl.ts --cmd ":debug (+ 1 2)" --cmd ":run" --cmd ":dump /tmp/test.json"
test -f /tmp/test.json && echo "Dump created"
# Should output: Dump created

# 3. Snapshot works
npx tsx bin/omega-repl.ts --cmd ":debug (+ 1 2)" --cmd ":step" --cmd ":snapshot test" --cmd ":snapshots" 2>&1 | grep "test"
# Should show snapshot named "test"
```

---

## Notes

### Why These Commands Matter

- **:dump/:replay**: Share execution traces, debug remotely, reproduce bugs
- **:snapshot/:restore**: Compare states, undo steps, checkpoint experiments
- **:record**: Save memory when not debugging, enable for deep inspection
- **:history**: Quick overview without full trace listing

### State Serialization Limitation

Full state serialization (with closures, continuations) is complex. Current implementation stores trace summaries in dump files, not full state. For full state, use `:snapshot` (in-memory only).

---

## Footer

| Field | Value |
|-------|-------|
| Created | 2025-01-21 |
| Author | Claude |
| Part Of | Job 017 (CLI Unification) |
| Related Files | `bin/omega-repl.ts`, `bin/omega-debugger.ts` |
