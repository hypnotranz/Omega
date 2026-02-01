# SICP Manual User Experience Plan

**Date**: 2026-01-30
**Goal**: Make it trivially easy for users to read chapters and run demos

---

## ✅ CURRENT STATE: VERIFIED

### Coverage: COMPLETE ✅
- **27 chapters** (01-27): All have markdown docs + Lisp demos
- **3 appendices** (91, 92, 99): Reference material (no demos needed)
- **6 use-case demos**: Practical examples (code-review, coding-agent, data-pipeline, interactive-tutor, research-agent, workflow-orchestrator)
- **TOTAL**: 27 chapter demos + 6 use-case demos = 33 runnable Lisp examples

### Files Work ✅
```bash
# Tested successfully:
npm run omega-repl -- --file demo/lisp/ch01-getting-started.lisp
=> "Welcome to OmegaLLM. Describe what you want in everyday language."
```

### Duplicates: CONFIRMED ✅
All chapter demos exist in TWO locations (IDENTICAL):
- `demo/lisp/ch01-ch27.lisp` (27 files)
- `MANUAL/code-examples/lisp/ch01-ch27.lisp` (27 files - EXACT DUPLICATES)

All TypeScript test configs exist in TWO locations (IDENTICAL):
- `demo/by-chapter/ch01-ch27.ts` (27 files)
- `MANUAL/code-examples/typescript/ch01-ch27.ts` (27 files - EXACT DUPLICATES)

**Note**: `demo/lisp/` has 6 EXTRA use-case demos not in MANUAL

---

## 🎯 PROPOSED USER EXPERIENCE

### Learning Flow (SICP Style)

1. **Read chapter** → `MANUAL/chapters/USER-MANUAL--01--Getting-Started.md`
2. **See code inline** → Chapter shows examples with REPL interaction
3. **Try it yourself** → Type examples into REPL to learn by doing
4. **Run complete demo** → Execute full working example:
   ```bash
   npm run manual 1
   # or
   npm run manual ch01-getting-started
   ```
5. **Modify and experiment** → Edit the file, re-run, explore

### Why This Works

- **Read → Type → Run** matches SICP pedagogy
- **One command** to run any chapter demo
- **Files are in MANUAL/** (co-located with chapters)
- **Can modify** files and re-run immediately
- **Can load into REPL** for interactive exploration

---

## 📁 PROPOSED CLEAN STRUCTURE

### Final Structure

```
OmegaLLM/
├── MANUAL--STRUCTURE-AND-INTERPRETATION-OF-LINGUISTIC-PROGRAMS/
│   ├── README.md                          ← Entry point, explains SICP mapping
│   │
│   ├── chapters/                          ← The textbook (33 markdown files)
│   │   ├── USER-MANUAL--00--Table-Of-Contents.md
│   │   ├── USER-MANUAL--00--Introduction.md
│   │   ├── USER-MANUAL--00--Quick-Reference.md
│   │   ├── USER-MANUAL--01--Getting-Started.md
│   │   ├── ... (chapters 02-27)
│   │   ├── USER-MANUAL--91--Appendix-A-Configuration.md
│   │   ├── USER-MANUAL--92--Appendix-B-Design-Philosophy.md
│   │   └── USER-MANUAL--99--Epilogue-The-Structure-Of-Understanding.md
│   │
│   ├── examples/                          ← Runnable Lisp code (33 files)
│   │   ├── ch01-getting-started.lisp
│   │   ├── ... (ch02-ch27.lisp)
│   │   ├── usecase-code-review.lisp       ← 6 practical examples
│   │   ├── usecase-coding-agent.lisp
│   │   ├── usecase-data-pipeline.lisp
│   │   ├── usecase-interactive-tutor.lisp
│   │   ├── usecase-research-agent.lisp
│   │   └── usecase-workflow-orchestrator.lisp
│   │
│   ├── tests/                             ← Validation tests
│   │   ├── by-chapter/                    ← TypeScript test configs
│   │   │   ├── ch01-getting-started.ts
│   │   │   ├── ... (ch02-ch27.ts)
│   │   │   ├── index.ts
│   │   │   ├── specs.ts                   ← Actual test logic
│   │   │   └── shared.ts
│   │   │
│   │   ├── by-chapter.spec.ts             ← Test runner
│   │   └── wow-pack.spec.ts               ← Advanced demos test
│   │
│   └── package.json                       ← Manual-specific scripts
│       # "scripts": {
│       #   "ch01": "npx tsx ../bin/omega-repl.ts --file examples/ch01-getting-started.lisp",
│       #   ...
│       # }
│
├── demo/                                  ← Advanced showcase (NOT for learners)
│   ├── omega-wow/                         ← 11 advanced demos
│   │   ├── demo1-oracle-repl-stack.ts
│   │   ├── ... (demo2-demo11.ts)
│   │   └── wow-runner.spec.ts
│   │
│   ├── harness/                           ← Test infrastructure
│   │   ├── runner.ts
│   │   ├── adapter-factory.ts
│   │   ├── ledger.ts
│   │   └── types.ts
│   │
│   └── runWow.ts                          ← Omega-wow CLI
│
└── src/                                   ← Runtime implementation
    └── ... (31 subsystems)
```

### What Changes

**DELETE** (duplicates):
- ❌ `demo/lisp/` → All 27 chapter demos MOVE to `MANUAL/examples/`
- ❌ `demo/by-chapter/` → MOVE to `MANUAL/tests/by-chapter/`
- ❌ `MANUAL/code-examples/` → MERGE into examples/ and tests/

**KEEP** (unique content):
- ✅ `demo/omega-wow/` - Advanced showcase demos (separate from learning)
- ✅ `demo/harness/` - Test infrastructure
- ✅ `demo/runWow.ts` - Omega-wow runner
- ✅ 6 use-case demos from demo/lisp/ → MOVE to `MANUAL/examples/`

---

## 🚀 IMPLEMENTATION OPTIONS

### Option 1: NPM Scripts in MANUAL (RECOMMENDED)

**Add to `MANUAL/package.json`**:
```json
{
  "name": "@omegallm/manual-examples",
  "version": "1.0.0",
  "scripts": {
    "ch01": "npx tsx ../bin/omega-repl.ts --file examples/ch01-getting-started.lisp",
    "ch02": "npx tsx ../bin/omega-repl.ts --file examples/ch02-llm-calls.lisp",
    ...
    "ch27": "npx tsx ../bin/omega-repl.ts --file examples/ch27-logic-programming.lisp"
  }
}
```

**Usage**:
```bash
cd MANUAL--STRUCTURE-AND-INTERPRETATION-OF-LINGUISTIC-PROGRAMS
npm run ch01
npm run ch05
npm run ch13
```

**Pros**:
- Simple, no code changes to OmegaLLM runtime
- Scripts live WITH the manual
- Easy to maintain
- Can add aliases: `npm run getting-started`

**Cons**:
- Users must `cd` to MANUAL folder first

---

### Option 2: Global NPM Scripts (ALTERNATIVE)

**Add to root `package.json`**:
```json
{
  "scripts": {
    "manual": "npx tsx bin/manual-runner.ts"
  }
}
```

**Create `bin/manual-runner.ts`**:
```typescript
// Parse chapter number or name from args
// Run: npm run manual 1
// Run: npm run manual ch01-getting-started
// Run: npm run manual getting-started
```

**Usage**:
```bash
npm run manual 1
npm run manual ch05-nondeterministic
npm run manual getting-started
```

**Pros**:
- One universal command
- Works from any directory
- Can add interactive menu: `npm run manual` → shows chapter list

**Cons**:
- Requires new script file

---

### Option 3: REPL Command (ADVANCED)

**Add primitive to OmegaLLM**:
```lisp
Ω> (load-chapter 1)
=> "Loading Chapter 1: Getting Started..."
=> greeting
=> echo
=> "Welcome to OmegaLLM..."

Ω> (load-chapter "higher-order-inference")
=> "Loading Chapter 13: Higher-Order Inference..."
...
```

**Implementation**:
- Add `load-chapter` primitive to `src/core/prims.ts`
- Looks up chapter file in `MANUAL/examples/`
- Loads and evaluates it in current session

**Pros**:
- Most "Lispy" - feels natural in REPL
- Can load multiple chapters in one session
- Supports exploration

**Cons**:
- Requires runtime changes
- More complex to implement

---

## 📊 COMPARISON

| Approach | Ease | Flexibility | Code Changes | Best For |
|----------|------|-------------|--------------|----------|
| **NPM in MANUAL** | ⭐⭐⭐⭐ | ⭐⭐⭐ | None | **Beginners** |
| **Global NPM Script** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | Small | **All users** |
| **REPL Primitive** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | Medium | **Advanced** |

---

## 🎓 RECOMMENDED: HYBRID APPROACH

**Combine Options 1 + 2 for best UX:**

1. **Add NPM scripts to MANUAL/package.json** (no runtime changes)
2. **Add global `npm run manual <chapter>`** (one simple script)
3. **(Optional) Add `load-chapter` primitive later** (v2 feature)

### Implementation Steps

1. ✅ Verify all demos work (DONE)
2. ✅ Confirm duplicates (DONE)
3. Restructure folders:
   - Move `demo/lisp/ch*.lisp` → `MANUAL/examples/`
   - Move `demo/lisp/usecase-*.lisp` → `MANUAL/examples/`
   - Move `demo/by-chapter/` → `MANUAL/tests/by-chapter/`
   - Delete `MANUAL/code-examples/` (duplicates)
4. Create `MANUAL/package.json` with chapter scripts
5. Create `bin/manual-runner.ts` for global command
6. Update README to point to new locations
7. Update chapter markdown to show correct run commands
8. Run regression tests

---

## 📝 UPDATED RUN COMMANDS

### In Manual Chapters

Update all chapter files to show:

```markdown
## Running This Chapter's Examples

**Option 1**: Type examples into REPL yourself (recommended for learning)
```bash
npm run omega-repl
```

**Option 2**: Run the complete chapter demo
```bash
# From MANUAL folder:
cd MANUAL--STRUCTURE-AND-INTERPRETATION-OF-LINGUISTIC-PROGRAMS
npm run ch01

# Or from project root:
npm run manual 1
npm run manual getting-started
```

**Option 3**: Load directly with file path
```bash
npm run omega-repl -- --file MANUAL--STRUCTURE-AND-INTERPRETATION-OF-LINGUISTIC-PROGRAMS/examples/ch01-getting-started.lisp
```
```

---

## 🎯 FINAL DELIVERABLES

After implementation, users will be able to:

1. ✅ Read chapter markdown (clear, SICP-style pedagogy)
2. ✅ See inline code examples
3. ✅ Run `npm run manual 5` to execute Chapter 5 demo
4. ✅ Modify `MANUAL/examples/ch05-nondeterministic.lisp` and re-run
5. ✅ Load into REPL for interactive exploration
6. ✅ All 27 chapters + 6 use-cases available
7. ✅ No duplicates, clean structure
8. ✅ Tests still work (moved to MANUAL/tests/)

---

## 🔄 MIGRATION CHECKLIST

- [ ] Back up current state (git commit)
- [ ] Move demo/lisp/ → MANUAL/examples/
- [ ] Move demo/by-chapter/ → MANUAL/tests/by-chapter/
- [ ] Delete MANUAL/code-examples/ (duplicates)
- [ ] Create MANUAL/package.json with chapter scripts
- [ ] Create bin/manual-runner.ts
- [ ] Update root package.json: add "manual" script
- [ ] Update MANUAL/README.md with new run commands
- [ ] Update all 27 chapter markdown files with correct paths
- [ ] Update test imports (by-chapter.spec.ts, wow-pack.spec.ts)
- [ ] Run regression: `npm run build`
- [ ] Run regression: `npm test`
- [ ] Run regression: `npx tsx demo/runWow.ts --all`
- [ ] Test manual commands: `npm run manual 1`, etc.
- [ ] Update main README.md

---

## ✨ BONUS: Interactive Manual Menu (v2)

Future enhancement:

```bash
npm run manual

╔════════════════════════════════════════════════════════════╗
║  OmegaLLM Manual - Structure & Interpretation of Inference ║
╚════════════════════════════════════════════════════════════╝

Select a chapter to run:

  PART I: OmegaLLM Basics
    1.  Getting Started
    2.  LLM Calls as Functions
    3.  Functional Composition
    ...

  PART II: SICP Principles
    11. Semantic Procedures as Black Boxes
    12. Inference Processes
    ...

  Use Cases
    U1. Code Review
    U2. Coding Agent
    ...

Enter chapter number (1-27) or use-case (U1-U6):
```

---

**READY TO IMPLEMENT?** Let me know which approach to use, and I'll execute the migration.
