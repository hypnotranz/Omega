# JOB-017b: Extend Public API for CLI Tools

**Priority**: P2 - Architecture Cleanup
**Estimated Effort**: 1 hour
**Skills Required**: TypeScript, Module Design
**Status**: NOT STARTED
**Depends On**: None
**Blocks**: 017d (CLI Unification)

> **MUST READ**: [LOGISTICS.md](LOGISTICS.md) before starting.

---

## Executive Summary

The CLI tools (`bin/omega-repl.ts`, `bin/omega-debugger.ts`) import 16+ internal paths from `src/core/`. This job adds the necessary exports to `src/index.ts` so CLI tools can use a single clean import.

**Goal**: CLI tools import from `"../src"` instead of `"../src/core/eval/store"`, `"../src/core/eval/machineStep"`, etc.

---

## Current State

### Internal Imports in omega-debugger.ts (lines 12-21)

```typescript
import { COWStore } from "../src/core/eval/store";
import { stepOnce, type StepResult } from "../src/core/eval/machineStep";
import { installPrims } from "../test/helpers/prims";
import { compileTextToExpr } from "../src/core/pipeline/compileText";
import type { State, Frame } from "../src/core/eval/machine";
import type { Val } from "../src/core/eval/values";
import type { Env } from "../src/core/eval/env";
```

### Internal Imports in omega-repl.ts (sampled)

```typescript
import { COWStore, type Store } from "../src/core/eval/store";
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
```

---

## Implementation

### Step 1: Add CLI Section to src/index.ts

Open `src/index.ts` and add a new section at the end (after line 140):

```typescript
// ═══════════════════════════════════════════════════════════════════════════════
// CLI TOOLS - CEKS Machine (Low-Level Debugging Support)
// ═══════════════════════════════════════════════════════════════════════════════

// Store (Copy-on-Write memory)
export { COWStore, type Store } from "./core/eval/store";

// Single-step execution (for debugger)
export { stepOnce, type StepResult } from "./core/eval/machineStep";

// Run to completion (for REPL)
export { runToCompletionWithState, runToCompletion } from "./core/eval/run";

// Machine state types
export type { State, Frame } from "./core/eval/machine";

// Environment type
export type { Env } from "./core/eval/env";

// Value constructors (for pretty-printing in debugger)
export {
  type NumVal,
  type StrVal,
  type BoolVal,
  type SymVal,
  type UnitVal,
  type ListVal,
  type VectorVal,
  type MapVal,
  type ClosureVal,
  type NativeVal,
  type ContVal,
  type MeaningVal as MeaningValFromValues,
  type DistVal as DistValFromValues,
  type PromptVal,
} from "./core/eval/values";

// Compilation (text -> AST)
export { compileTextToExpr } from "./core/pipeline/compileText";

// Effects runtime (for handling oracle calls)
export { RuntimeImpl } from "./core/effects/runtimeImpl";

// Oracle state management
export { SnapshotRepo } from "./core/oracle/snapshots";
export { InMemoryReceiptStore, type ReceiptStore } from "./core/oracle/receipts";

// OpenAI adapter (Anthropic already exported above)
export { createOpenAIAdapter } from "./core/oracle/adapters";

// Primitive installation (for initializing REPL environment)
export { installPrims } from "./core/prims";
```

### Step 2: Verify Each Export Exists

Before adding, verify each source file exists and exports the symbol:

| Symbol | Source File | Line |
|--------|-------------|------|
| `COWStore` | `src/core/eval/store.ts` | ~line 1-50 |
| `Store` (type) | `src/core/eval/store.ts` | interface |
| `stepOnce` | `src/core/eval/machineStep.ts` | export function |
| `StepResult` (type) | `src/core/eval/machineStep.ts` | export type |
| `runToCompletionWithState` | `src/core/eval/run.ts` | export function |
| `runToCompletion` | `src/core/eval/run.ts` | export function |
| `State` (type) | `src/core/eval/machine.ts` | export interface |
| `Frame` (type) | `src/core/eval/machine.ts` | export type |
| `Env` (type) | `src/core/eval/env.ts` | export type |
| `compileTextToExpr` | `src/core/pipeline/compileText.ts` | export function |
| `RuntimeImpl` | `src/core/effects/runtimeImpl.ts` | export class |
| `SnapshotRepo` | `src/core/oracle/snapshots.ts` | export class |
| `InMemoryReceiptStore` | `src/core/oracle/receipts.ts` | export class |
| `ReceiptStore` (type) | `src/core/oracle/receipts.ts` | export interface |
| `createOpenAIAdapter` | `src/core/oracle/adapters/index.ts` | export function |
| `installPrims` | `src/core/prims.ts` | export function |

### Step 3: Run TypeScript Check

```bash
cd OmegaLLM
npx tsc --noEmit
```

Fix any errors. Common issues:
- Symbol not exported from source file → add `export` keyword
- Conflicting type names → use `as` rename in export
- Circular dependency → reorder exports

---

## Final State

### File Changes

**`src/index.ts`** - Add ~35 lines at end:

| Section | Exports Added |
|---------|---------------|
| CLI TOOLS - CEKS Machine | `COWStore`, `Store`, `stepOnce`, `StepResult`, `runToCompletionWithState`, `runToCompletion`, `State`, `Frame`, `Env`, value types, `compileTextToExpr`, `RuntimeImpl`, `SnapshotRepo`, `InMemoryReceiptStore`, `ReceiptStore`, `createOpenAIAdapter`, `installPrims` |

### Expected src/index.ts Structure After Change

```typescript
// src/index.ts
// ... existing exports (lines 1-140) ...

// ═══════════════════════════════════════════════════════════════════════════════
// CLI TOOLS - CEKS Machine (Low-Level Debugging Support)
// ═══════════════════════════════════════════════════════════════════════════════

// Store
export { COWStore, type Store } from "./core/eval/store";

// ... rest of CLI exports ...
```

---

## Verification

### Test 1: TypeScript Compiles

```bash
cd OmegaLLM
npx tsc --noEmit
# Should have no errors
```

### Test 2: Test Consumer

Create a temporary test file:

```bash
cat > /tmp/test-imports.ts << 'EOF'
// Test that all CLI exports are accessible from public API
import {
  // Core runtime (already existed)
  OmegaRuntime,
  evalOmegaCode,

  // NEW: CLI/Debugger exports
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

  // NEW: Types
  type Store,
  type StepResult,
  type State,
  type Frame,
  type Env,
  type ReceiptStore,
} from "./src";

// Verify they're usable
const store = new COWStore();
console.log("COWStore created:", store);

const result = compileTextToExpr("(+ 1 2)");
console.log("Compiled:", result);

console.log("All imports successful!");
EOF

npx tsx /tmp/test-imports.ts
# Should print success messages, no import errors
```

### Test 3: Existing Tests Still Pass

```bash
npm test
# All tests should pass
```

### Test 4: No Duplicate Exports

```bash
# Check for duplicate export names
grep -E "^export " src/index.ts | sort | uniq -d
# Should be empty (no duplicates)
```

---

## Checklist

- [ ] Added CLI TOOLS section header comment to `src/index.ts`
- [ ] Added `COWStore` and `Store` type exports
- [ ] Added `stepOnce` and `StepResult` exports
- [ ] Added `runToCompletionWithState` and `runToCompletion` exports
- [ ] Added `State` and `Frame` type exports
- [ ] Added `Env` type export
- [ ] Added `compileTextToExpr` export
- [ ] Added `RuntimeImpl` export
- [ ] Added `SnapshotRepo` export
- [ ] Added `InMemoryReceiptStore` and `ReceiptStore` exports
- [ ] Added `createOpenAIAdapter` export
- [ ] Added `installPrims` export
- [ ] TypeScript compiles without errors
- [ ] Test consumer file imports successfully
- [ ] All existing tests pass

---

## Proof of Completion

```bash
# 1. New exports exist
grep -c "CLI TOOLS" src/index.ts
# Should output: 1

# 2. Key exports present
grep "export.*COWStore" src/index.ts
grep "export.*stepOnce" src/index.ts
grep "export.*installPrims" src/index.ts
# All should show matches

# 3. TypeScript compiles
npx tsc --noEmit && echo "OK"
# Should output: OK

# 4. Tests pass
npm test
```

---

## Notes

### Why This Matters

- **Encapsulation**: External tools shouldn't reach into `src/core/` internals
- **Stability**: Public API can remain stable while internals change
- **Documentation**: `src/index.ts` serves as API documentation
- **IDE Support**: Single import point for autocomplete

### Naming Conflicts

If there are naming conflicts (e.g., two types named `MeaningVal`), use aliased exports:

```typescript
export { MeaningVal as MeaningValFromValues } from "./core/eval/values";
export { MeaningVal as MeaningValFromOracle } from "./core/oracle/meaning";
```

---

## Footer

| Field | Value |
|-------|-------|
| Created | 2025-01-21 |
| Author | Claude |
| Part Of | Job 017 (CLI Unification) |
| Related Files | `src/index.ts`, `bin/omega-repl.ts`, `bin/omega-debugger.ts` |
