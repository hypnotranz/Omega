# LOGISTICS: Testing, Deployment, and Proof of Completion

**All CLAUDE-JOBS implementers MUST follow these procedures.**

---

## ⚠️ Authoritative Document Hierarchy

When implementing any job, the following document hierarchy applies:

| Priority | Document | Authority |
|----------|----------|-----------|
| 1 (Highest) | `docs/ARCHITECTURE-LANGUAGES-*.md` | **AUTHORITATIVE** - These define the canonical design |
| 2 | Job specifications in `CLAUDE-JOBS/*.md` | Derived from architecture docs |
| 3 | Existing source code | May be **legacy** - update to match architecture |
| 4 | Other docs (requirements, notes) | **Historical context only** - may be outdated |

### Key Principle

> **The ARCHITECTURE-LANGUAGES-*.md documents take precedence over all other documentation and existing code.**

If you encounter conflicts between:
- **Existing code vs. architecture docs** → Follow the architecture docs; the code needs to be updated
- **Old requirements vs. architecture docs** → Follow the architecture docs; requirements may be outdated
- **Job spec unclear?** → Consult the referenced architecture sections for the canonical design

### Architecture Document Map

| Document | Topics |
|----------|--------|
| `ARCHITECTURE-LANGUAGES-1.md` | §1-15: Core principles, Lisp semantics |
| `ARCHITECTURE-LANGUAGES-2.md` | §16-28: Value types, Outcome ADT, effects |
| `ARCHITECTURE-LANGUAGES-3.md` | §29-40: Effect kinds, ports, hexagonal architecture |
| `ARCHITECTURE-LANGUAGES-4.md` | §41-52: Compilation pipeline, replay, lint |
| `ARCHITECTURE-LANGUAGES-5.md` | §53-63: FrameIR specification |
| `ARCHITECTURE-LANGUAGES-6.md` | §64-75: Runtime, scheduling, ports API |

---

## Related Documentation

| Document | Purpose |
|----------|---------|
| **[000-STRATEGY-AND-ARCHITECTURE.md](000-STRATEGY-AND-ARCHITECTURE.md)** | Strategy (copy vs reimplement), abstraction boundaries |
| **[README.md](README.md)** | Job dependency graph, status tracking, package structure |
| **[../docs/LAMBDA-LLM--OMEGA-LLM-FEATURES.md](../docs/LAMBDA-LLM--OMEGA-LLM-FEATURES.md)** | Feature audit and inside-out worklist |

---

## 1. Where to Put Code

| Output Type | Location | Example |
|-------------|----------|---------|
| Core source files | `src/core/` | `src/core/eval/values.ts` |
| Package-specific | `src/core/{package}/` | `src/core/conditions/types.ts` |
| Test files | `test/{feature}/` | `test/continuations/call-cc.spec.ts` |
| Lisp libraries | `lib/` | `lib/monad.lisp` |

### Directory Structure

```
OmegaLLM/
├── src/core/
│   ├── eval/           # CEKS machine (values.ts, machine.ts, machineStep.ts)
│   ├── effects/        # Effect handlers, nondet, search
│   ├── oracle/         # Oracle adapters, provenance (meaning.ts, receipts.ts)
│   ├── pipeline/       # Compilation (compileText.ts, lower.ts)
│   ├── conditions/     # @omega/conditions (Job 005) - CREATE
│   ├── provenance/     # @omega/provenance (Job 007) - CREATE
│   └── solver/         # @omega/solver (Job 008) - CREATE
├── test/
│   ├── amb/            # Existing amb tests
│   ├── effects/        # Effect handler tests
│   ├── continuations/  # call/cc tests (Job 004) - CREATE
│   ├── conditions/     # Condition tests (Job 005) - CREATE
│   ├── monad/          # Monadic primitive tests (Job 006) - CREATE
│   ├── provenance/     # Provenance tests (Job 007) - CREATE
│   └── solver/         # Solver tests (Job 008) - CREATE
└── lib/
    └── monad.lisp      # mdo macro, guard, msum (Job 006) - CREATE
```

### File Naming Conventions

- TypeScript source: `camelCase.ts` (e.g., `machineStep.ts`)
- Test files: `{feature}.spec.ts` (e.g., `call-cc.spec.ts`)
- Lisp libraries: `snake_case.lisp` or `kebab-case.lisp`

---

## 2. How to Run Tests

### Test Framework

OmegaLLM uses **Vitest** for testing.

### All Tests

```bash
cd c:\Users\Richa\parmenides-dev\agent-harness\OmegaLLM
npm run test
```

### Watch Mode (During Development)

```bash
npm run test:watch
```

### Specific Test File

```bash
npx vitest run test/continuations/call-cc.spec.ts
```

### Pattern Matching

```bash
npx vitest run --grep "call/cc"
```

### Full Regression

Before marking any job as DONE:

```bash
npm run test
# Expected: 1124+ tests passing
```

---

## 3. Testing with Real Oracle (Claude)

### Environment Setup

Set API key (PowerShell):
```powershell
$env:ANTHROPIC_API_KEY = "your-key-here"
```

Or create `.env` file:
```
ANTHROPIC_API_KEY=your-key-here
```

### Live Tests

Live tests are in `test/live/` and require real API access:

```bash
# Full stack live test
npx vitest run test/live/full-stack-live.spec.ts

# OpenAI live test
npx vitest run test/live/openai-live.spec.ts
```

### Mock Mode (Default)

Most tests use `ScriptedOracleAdapter` which mocks oracle responses:

```typescript
import { ScriptedOracleAdapter } from "../../src/core/oracle/scriptedOracle";
const oracle = new ScriptedOracleAdapter();
oracle.script([
  { pattern: /test/, response: "mocked response" }
]);
```

---

## 4. Test File Template

```typescript
// test/{feature}/{feature}.spec.ts
import { describe, it, expect, beforeEach } from "vitest";
import { COWStore } from "../../src/core/eval/store";
import { RuntimeImpl } from "../../src/core/effects/runtimeImpl";
import { ScriptedOracleAdapter } from "../../src/core/oracle/scriptedOracle";
import { SnapshotRepo } from "../../src/core/oracle/snapshots";
import { InMemoryReceiptStore } from "../../src/core/oracle/receipts";
import { mockCommit } from "../helpers/runtime";
import { installPrims } from "../helpers/prims";
import type { State } from "../../src/core/eval/machine";
import { runToCompletion } from "../../src/core/eval/run";
import { compileTextToExpr } from "../../src/core/pipeline/compileText";
import type { Val } from "../../src/core/eval/values";
import type { Profile } from "../../src/core/governance/profile";

function createTestEnv(profile?: Profile) {
  const oracle = new ScriptedOracleAdapter();
  const snapshots = new SnapshotRepo();
  const receipts = new InMemoryReceiptStore("off");
  const runtime = new RuntimeImpl(oracle, snapshots, receipts, mockCommit, profile);
  return { runtime, oracle, snapshots, receipts };
}

function initialState(src: string): State {
  const expr = compileTextToExpr(src);
  const store0 = new COWStore();
  const prim = installPrims(store0);
  return { control: { tag: "Expr", e: expr }, env: prim.env, store: prim.store, kont: [], handlers: [] };
}

const fullProfile: Profile = {
  name: "test-profile",
  caps: ["*"],
  budgets: { maxOracleTurns: 1000, maxEvalSteps: 500_000, maxToolCalls: 1000 },
  truth: "speculative",
};

async function evalOmega(src: string, profile?: Profile): Promise<Val> {
  const { runtime } = createTestEnv(profile ?? fullProfile);
  const st0 = initialState(src);
  return runToCompletion(runtime, st0);
}

describe("Feature Name", () => {
  describe("Happy Path", () => {
    it("HP-1: Basic functionality", async () => {
      const result = await evalOmega(`(your-code-here)`);
      expect(result).toMatchObject({ tag: "Num", n: 42 });
    });
  });

  describe("Edge Cases", () => {
    it("EC-1: Edge case description", async () => {
      // Test edge case
    });
  });

  describe("Error Cases", () => {
    it("ERR-1: Error description", async () => {
      await expect(evalOmega(`(bad-code)`)).rejects.toThrow(/expected error/);
    });
  });
});
```

---

## 5. Proof of Completion

### Where to Put Proof

When a job is DONE, update the job document and optionally save proof:

```
traces/PROOF/JOB-{number}/
  ├── test-output.txt           # vitest output showing all tests pass
  ├── repl-example.txt          # REPL session demonstrating feature
  └── NOTES.md                  # Implementation notes, known limitations
```

### Required Steps

| Step | Command | Verification |
|------|---------|--------------|
| 1. Build passes | `npm run build` | No TypeScript errors |
| 2. All tests pass | `npm run test` | 1124+ tests passing |
| 3. New tests pass | `npx vitest run test/{new-feature}/` | All new tests green |
| 4. REPL works | Manual test | Feature works interactively |

### Marking Job as DONE

1. Run full regression: `npm run test`
2. Verify new tests exist and pass
3. Update job file: `Status: NOT STARTED` → `Status: DONE`
4. Update `README.md` job table
5. (Optional) Save proof artifacts

---

## 6. Building

### TypeScript Compilation

```bash
npm run build
```

Output goes to `dist/`.

### Type Checking Only

```bash
npx tsc --noEmit
```

---

## 7. Debugging Tips

### Verbose Test Output

```bash
npx vitest run --reporter=verbose
```

### Single Test Isolation

```bash
npx vitest run test/amb/amb.spec.ts -t "specific test name"
```

### Inspecting State

In tests, you can inspect CEKS machine state:

```typescript
import { machineStep } from "../../src/core/eval/machineStep";

// Step through evaluation manually
let state = initialState(`(+ 1 2)`);
while (state.control.tag !== "Val") {
  console.log("Control:", state.control);
  console.log("Kont:", state.kont.map(f => f.tag));
  state = machineStep(state);
}
```

### Adding to Primitives

When adding new primitives, register them in:
1. `src/core/prims.ts` - Define the primitive
2. `src/core/pipeline/compileText.ts` - Add to primitives list

---

## 8. Quick Reference

```
LOCATIONS:
  src/core/           → TypeScript source files
  test/               → Test files (*.spec.ts)
  lib/                → Lisp library files
  dist/               → Compiled output

COMMANDS:
  npm run build       → Compile TypeScript
  npm run test        → Run all tests
  npm run test:watch  → Watch mode
  npx vitest run X    → Run specific test

KEY FILES:
  src/core/eval/values.ts      → Value types (add ContinuationVal here)
  src/core/eval/machine.ts     → State type, Frame types
  src/core/eval/machineStep.ts → Evaluation step function
  src/core/prims.ts            → Primitive definitions
  src/core/pipeline/compileText.ts → Primitive registration

PROOF CHECKLIST:
  [ ] npm run build succeeds
  [ ] npm run test passes (1124+)
  [ ] New feature tests pass
  [ ] Job status updated to DONE
  [ ] README.md table updated
```

---

## 9. LambdaRLM Reference

Many OmegaLLM jobs have counterparts in LambdaRLM:

| OmegaLLM Job | LambdaRLM Equivalent | Notes |
|--------------|---------------------|-------|
| 004 (call/cc) | 06-YIELD | Different impl (CEKS vs interpreter) |
| 005 (Conditions) | 03-FAILURE | Richer CL-style conditions |
| 006 (Monadic) | 08-NONDET | OmegaLLM already has amb |
| 007 (Provenance) | 14-PROVENANCE | Same concept, TypeScript impl |
| 008 (Solvers) | 05-COMPOSABLE, 09-STRATEGIES | Combined into one job |

When stuck, check the LambdaRLM implementation for design patterns.

---

*Created: 2026-01-19*
*Reference: Based on [LambdaRLM/CLAUDE-JOBS/LOGISTICS.md](../../LambdaRLM/CLAUDE-JOBS/LOGISTICS.md)*
