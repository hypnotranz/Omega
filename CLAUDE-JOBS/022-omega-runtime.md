# 022 - OmegaRuntime: Clean Public API with Hooks

## Overview

Extract a clean public runtime API from the entangled REPL/Server code. The runtime becomes THE package boundary - all consumers (REPL, HTTP server, tests, external tools) use the same interface.

---

## Strategy: Parallel Rewrite

This is a significant rewrite (~70 new files, 3 major consumers to rewire). Instead of refactoring in place, we use a **parallel folder approach**:

```
agent-harness/
├── OmegaLLM/              ← Working copy (rewrite happens here)
│   ├── src/runtime/       ← NEW: clean runtime API
│   ├── bin/omega-repl.ts  ← REWRITE: gut and rewire
│   └── ...
│
└── OmegaLLM-ref/          ← Reference copy (read-only)
    ├── bin/omega-repl.ts  ← Original 3163-line REPL (for reference)
    └── ...                ← Original code to compare against
```

### Steps

1. **Copy**: `cp -r OmegaLLM OmegaLLM-ref` (or `git worktree add`)
2. **Build**: Create clean `src/runtime/` in OmegaLLM, referencing OmegaLLM-ref for logic
3. **Rewire**: Gut `bin/omega-repl.ts`, wire to new runtime (keep ref open side-by-side)
4. **Test**: Run same inputs through both, compare outputs (OmegaLLM-ref is behavioral oracle)
5. **Delete**: When confident: `rm -rf OmegaLLM-ref`

### Why This Approach

| Concern | In-Place Refactor | Parallel Rewrite |
|---------|-------------------|------------------|
| Broken intermediate states | Yes, constantly | No, original works |
| Easy to compare old/new | Hard (git stash/diff) | Easy (two folders) |
| Risk of preserving cruft | High | Low (clean slate) |
| Git history | Messy WIP commits | Clean feature commits |
| Rollback | Complex | Delete and keep ref |

---

## Feature Traceability Checklist

Every feature mapped to its destination with STATUS tracking.
Update STATUS when a feature is planned in a refactoring step.

**STATUS VALUES:**
- `NOT_STARTED` - Not yet planned
- `PLANNED` - Assigned to a refactoring step
- `UNIT_TESTED` - Extracted and unit tests pass
- `REGR_TESTED` - Regression tests pass (REPL still works)
- `DONE` - Fully integrated and verified

**SOURCE DOCUMENTS:** Each feature references its authoritative source document for review.

```
┌──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
│                              FEATURE TRACEABILITY CHECKLIST                                                                   │
│   Step = Build phase step number | Wire step 9.x applies to ALL features                                                      │
│   SOURCE = authoritative document in ARCHITECTURE/ or docs/ folder                                                            │
├──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
│  ID  │ Feature                      │ Destination                  │ Step  │ Status      │ SOURCE                    │ Notes │
│══════╪══════════════════════════════╪══════════════════════════════╪═══════╪═════════════╪═══════════════════════════╪═══════│
│      │                              │                              │       │             │                           │       │
│      │ ━━━ CORE EVALUATION ━━━      │                              │       │             │                           │       │
│ E01  │ (expr) eval                  │ runtime.eval(expr)           │ 2.1   │ NOT_STARTED │ 01-EVAL.md                │ Main  │
│ E02  │ :loadfile <path>             │ runtime.loadFile(path)       │ 2.1   │ NOT_STARTED │ 01-EVAL.md                │ Main  │
│ E03  │ Error handling               │ runtime.on('error', ...)     │ 2.3   │ NOT_STARTED │ 06-CONDITIONS.md          │ Hook  │
│      │                              │                              │       │             │            │
│      │ ━━━ SESSION PERSISTENCE ━━━  │ runtime.session.*            │       │             │                           │       │
│ S01  │ :session list                │ session.list()               │ 5.1   │ NOT_STARTED │ 14-SESSION.md             │       │
│ S02  │ :session save <name>         │ session.save(name)           │ 5.1   │ NOT_STARTED │ 14-SESSION.md             │       │
│ S03  │ :session load <name>         │ session.load(name)           │ 5.1   │ NOT_STARTED │ 14-SESSION.md             │       │
│ S04  │ :session fork <name>         │ session.fork(name)           │ 5.1   │ NOT_STARTED │ 14-SESSION.md             │       │
│ S05  │ :session goto <seq>          │ session.goto(seq)            │ 5.1   │ NOT_STARTED │ 14-SESSION.md             │       │
│ S06  │ :session trace               │ session.getTrace()           │ 5.1   │ NOT_STARTED │ 14-SESSION.md             │       │
│ S07  │ :session checkpoints         │ session.getCheckpoints()     │ 5.1   │ NOT_STARTED │ 14-SESSION.md             │       │
│ S08  │ :session resume              │ session.resume()             │ 5.1   │ NOT_STARTED │ 14-SESSION.md             │       │
│ S09  │ SessionWriter internals      │ SessionProvider interface    │ 5.2   │ NOT_STARTED │ 14-SESSION.md             │ Plug  │
│ S10  │ SessionReader internals      │ SessionProvider interface    │ 5.2   │ NOT_STARTED │ 14-SESSION.md             │ Plug  │
│      │                              │                              │       │             │            │
│      │ ━━━ DEBUG MODE ━━━           │ runtime.debug.*              │       │             │                           │       │
│ D01  │ :debug (expr)                │ debug.load(expr)             │ 4.4   │ NOT_STARTED │ 07-DEBUG.md               │       │
│ D02  │ :step [N] / :s [N]           │ debug.step(n)                │ 4.4   │ NOT_STARTED │ 07-DEBUG.md               │       │
│ D03  │ :run / :continue / :c        │ debug.run()                  │ 4.4   │ NOT_STARTED │ 07-DEBUG.md               │       │
│ D04  │ :stop                        │ debug.stop()                 │ 4.4   │ NOT_STARTED │ 07-DEBUG.md               │       │
│ D05  │ :goto <N> / :g <N>           │ debug.goto(step)             │ 4.4   │ NOT_STARTED │ 07-DEBUG.md               │ Travel│
│ D06  │ :trace [s] [n]               │ debug.getTrace(s, n)         │ 4.4   │ NOT_STARTED │ 07-DEBUG.md               │       │
│ D07  │ :state / :st                 │ debug.getState()             │ 4.4   │ NOT_STARTED │ 07-DEBUG.md               │       │
│ D08  │ debugState internal          │ DebugSubsystem internal      │ 4.4   │ NOT_STARTED │ 07-DEBUG.md               │       │
│ D09  │ stepCount internal           │ DebugSubsystem internal      │ 4.4   │ NOT_STARTED │ 07-DEBUG.md               │       │
│ D10  │ trace[] internal             │ TraceProvider                │ 4.3   │ NOT_STARTED │ 07-DEBUG.md               │ Plug  │
│      │                              │                              │       │             │            │
│      │ ━━━ BREAKPOINTS ━━━          │ runtime.breakpoints.*        │       │             │                           │       │
│ B01  │ :break step <N>              │ breakpoints.add({type:'step'})│ 4.1  │ NOT_STARTED │ 07-DEBUG.md               │       │
│ B02  │ :break expr <tag>            │ breakpoints.add({type:'expr'})│ 4.1  │ NOT_STARTED │ 07-DEBUG.md               │       │
│ B03  │ :break effect <op>           │ breakpoints.add({type:'effect'})│4.1 │ NOT_STARTED │ 07-DEBUG.md               │       │
│ B04  │ :breaks / :breakpoints       │ breakpoints.list()           │ 4.1   │ NOT_STARTED │ 07-DEBUG.md               │       │
│ B05  │ :delbreak <id>               │ breakpoints.remove(id)       │ 4.1   │ NOT_STARTED │ 07-DEBUG.md               │       │
│ B06  │ :toggle <id>                 │ breakpoints.toggle(id)       │ 4.1   │ NOT_STARTED │ 07-DEBUG.md               │       │
│ B07  │ checkBreakpoints() internal  │ BreakpointManager.check()    │ 4.5   │ NOT_STARTED │ 07-DEBUG.md               │ GroupD│
│ B08  │ Breakpoint type              │ types/Breakpoint.ts          │ 0.2   │ NOT_STARTED │ 07-DEBUG.md               │ Type  │
│      │                              │                              │       │             │            │
│      │ ━━━ SNAPSHOTS ━━━            │ runtime.snapshots.*          │       │             │                           │       │
│ N01  │ :save <name>                 │ snapshots.save(name)         │ 4.2   │ NOT_STARTED │ 07-DEBUG.md               │       │
│ N02  │ :restore <name>              │ snapshots.restore(name)      │ 4.2   │ NOT_STARTED │ 07-DEBUG.md               │       │
│ N03  │ :snapshots / :snaps          │ snapshots.list()             │ 4.2   │ NOT_STARTED │ 07-DEBUG.md               │       │
│ N04  │ :export <name> <file>        │ snapshots.export(name, path) │ 4.2   │ NOT_STARTED │ 07-DEBUG.md               │       │
│ N05  │ snapshots Map internal       │ SnapshotProvider             │ 4.2   │ NOT_STARTED │ 07-DEBUG.md               │ Plug  │
│ N06  │ Snapshot type                │ types/SnapshotInfo.ts        │ 0.2   │ NOT_STARTED │ 07-DEBUG.md               │ Type  │
│      │                              │                              │       │             │            │
│      │ ━━━ HISTORY / TIME TRAVEL ━━━│ runtime.history.*            │       │             │                           │       │
│ H01  │ :back [N]                    │ history.back(n)              │ 4.3   │ NOT_STARTED │ 07-DEBUG.md               │       │
│ H02  │ :history [N] / :hist [N]     │ history.list(n)              │ 4.3   │ NOT_STARTED │ 07-DEBUG.md               │       │
│ H03  │ :record on|off               │ history.setRecording(bool)   │ 4.3   │ NOT_STARTED │ 07-DEBUG.md               │       │
│ H04  │ :dump <file>                 │ history.dump(path)           │ 4.3   │ NOT_STARTED │ 07-DEBUG.md               │       │
│ H05  │ :replay <file>               │ history.replay(path)         │ 4.3   │ NOT_STARTED │ 07-DEBUG.md               │       │
│ H06  │ history[] internal           │ TraceProvider                │ 4.3   │ NOT_STARTED │ 07-DEBUG.md               │ Plug  │
│ H07  │ maxHistory internal          │ RuntimeConfig.maxHistorySize │ 0.2   │ NOT_STARTED │ 07-DEBUG.md               │ Type  │
│ H08  │ recordingEnabled internal    │ HistoryManager internal      │ 4.3   │ NOT_STARTED │ 07-DEBUG.md               │       │
│      │                              │                              │       │             │            │
│      │ ━━━ STATE INSPECTION ━━━     │ runtime.inspect.*            │       │             │                           │       │
│ I01  │ :env                         │ inspect.env()                │ 3.1   │ NOT_STARTED │ 01-EVAL.md                │       │
│ I02  │ :env <name>                  │ inspect.env(name)            │ 3.1   │ NOT_STARTED │ 01-EVAL.md                │       │
│ I03  │ :defs                        │ inspect.defs()               │ 3.1   │ NOT_STARTED │ 01-EVAL.md                │       │
│ I04  │ :stack                       │ inspect.stack()              │ 3.1   │ NOT_STARTED │ 01-EVAL.md                │       │
│ I05  │ :frame <n>                   │ inspect.frame(n)             │ 3.1   │ NOT_STARTED │ 01-EVAL.md                │       │
│ I06  │ :control                     │ inspect.control()            │ 3.1   │ NOT_STARTED │ 01-EVAL.md                │       │
│ I07  │ envToBindings() internal     │ StateInspector internal      │ 3.1   │ NOT_STARTED │ 01-EVAL.md                │       │
│ I08  │ frameToString() internal     │ internal/FrameFormatter.ts   │ 1.2   │ NOT_STARTED │ 01-EVAL.md                │ Util  │
│ I09  │ controlToString() internal   │ internal/ControlFormatter.ts │ 1.2   │ NOT_STARTED │ 01-EVAL.md                │ Util  │
│ I10  │ valToSexp() internal         │ internal/ValueSerializer.ts  │ 1.2   │ NOT_STARTED │ 01-EVAL.md                │ Util  │
│      │                              │                              │       │             │            │
│      │ ━━━ LLM / AGENTIC ━━━        │ runtime.llm.*                │       │             │                           │       │
│ L01  │ :ask <question>              │ llm.ask(question)            │ 6.1   │ NOT_STARTED │ 10-ORACLE.md              │ Agent │
│ L02  │ :traces                      │ llm.getTraces()              │ 6.1   │ NOT_STARTED │ 10-ORACLE.md              │       │
│ L03  │ :trace <id>                  │ llm.getTrace(id)             │ 6.1   │ NOT_STARTED │ 10-ORACLE.md              │       │
│ L04  │ :trace <id> -v               │ llm.getTrace(id) + verbose   │ 6.1   │ NOT_STARTED │ 10-ORACLE.md              │       │
│ L05  │ OpenAIAdapter class          │ LLMAdapter interface impl    │ 0.2   │ NOT_STARTED │ 10-ORACLE.md              │ Type  │
│ L06  │ AnthropicAdapter class       │ LLMAdapter interface impl    │ 0.2   │ NOT_STARTED │ 10-ORACLE.md              │ Type  │
│ L07  │ TextBasedAdapter class       │ LLMAdapter interface impl    │ 0.2   │ NOT_STARTED │ 10-ORACLE.md              │ Type  │
│ L08  │ runAgenticQuery() internal   │ LLMIntegration.ask()         │ 6.1   │ NOT_STARTED │ 10-ORACLE.md              │       │
│ L09  │ executeToolCall() internal   │ LLMIntegration internal      │ 6.1   │ NOT_STARTED │ 10-ORACLE.md              │       │
│ L10  │ REPL_TOOLS constant          │ LLMIntegration internal      │ 6.1   │ NOT_STARTED │ 10-ORACLE.md              │       │
│ L11  │ llmTraces Map internal       │ LLMIntegration internal      │ 6.1   │ NOT_STARTED │ 10-ORACLE.md              │       │
│ L12  │ LLMTrace type                │ types/LLMTypes.ts            │ 0.2   │ NOT_STARTED │ 10-ORACLE.md              │ Type  │
│      │                              │                              │       │             │            │
│      │ ━━━ OPR KERNELS ━━━          │ runtime.opr.*                │       │             │                           │       │
│ O01  │ :opr-list                    │ opr.list()                   │ 6.3   │ NOT_STARTED │ 09-OPR.md                 │       │
│ O02  │ :opr-run <kernel> <json>     │ opr.run(kernel, program)     │ 6.3   │ NOT_STARTED │ 09-OPR.md                 │       │
│ O03  │ :opr-receipts                │ opr.getReceipts()            │ 6.3   │ NOT_STARTED │ 09-OPR.md                 │       │
│ O04  │ :opr-verify [file]           │ opr.verify()                 │ 6.3   │ NOT_STARTED │ 09-OPR.md                 │       │
│ O05  │ OprRuntime usage             │ OprIntegration wraps it      │ 6.3   │ NOT_STARTED │ 09-OPR.md                 │       │
│ O06  │ OprReceiptStore usage        │ ReceiptProvider              │ 6.3   │ NOT_STARTED │ 09-OPR.md                 │ Plug  │
│ O07  │ Receipt type                 │ types/OprTypes.ts            │ 0.2   │ NOT_STARTED │ 09-OPR.md                 │ Type  │
│      │                              │                              │       │             │            │
│      │ ━━━ EFFECT HANDLING ━━━      │ EffectHandlerRegistry        │       │             │                           │       │
│ F01  │ handleEffectExpression()     │ EffectHandlerRegistry        │ 2.2   │ NOT_STARTED │ 05-EFFECTS.md             │       │
│ F02  │ inferOp handlers             │ EffectHandler callbacks      │ 2.2   │ NOT_STARTED │ 05-EFFECTS.md             │ Plug  │
│ F03  │ RuntimeImpl usage            │ ExecutionEngine wraps it     │ 2.1   │ NOT_STARTED │ 05-EFFECTS.md             │       │
│      │                              │                              │       │             │                           │       │
│      │ ━━━ STATE MANAGEMENT ━━━     │ StateProvider                │       │             │                           │       │
│ M01  │ store / currentStore         │ StateProvider.createStore()  │ 1.1   │ NOT_STARTED │ 01-EVAL.md                │       │
│ M02  │ COWStore usage               │ MemoryStateProvider          │ 1.1   │ NOT_STARTED │ 01-EVAL.md                │       │
│ M03  │ cloneState() internal        │ internal/StateCloner.ts      │ 1.2   │ NOT_STARTED │ 01-EVAL.md                │ Util  │
│      │                              │                              │       │             │                           │       │
│      │ ━━━ GOVERNANCE ━━━           │ GovernanceProvider           │       │             │                           │       │
│ G01  │ Profile/Caps checks          │ GovernanceProvider.checkCap()│ 6.5   │ NOT_STARTED │ 21-SECURITY.md            │       │
│ G02  │ Budget tracking              │ GovernanceProvider.consume() │ 6.5   │ NOT_STARTED │ 25-BUDGET.md              │       │
│      │                              │                              │       │             │            │
│      │ ━━━ HOOKS/EVENTS ━━━         │ runtime.on(event, handler)   │       │             │                           │ NEW   │
│ V01  │ on('step', ...)              │ RuntimeEventEmitter          │ 2.3   │ NOT_STARTED │ 08-PROTOCOL.md            │ NEW   │
│ V02  │ on('before-llm', ...)        │ RuntimeEventEmitter          │ 6.2   │ NOT_STARTED │ 10-ORACLE.md              │ NEW   │
│ V03  │ on('after-llm', ...)         │ RuntimeEventEmitter          │ 6.2   │ NOT_STARTED │ 10-ORACLE.md              │ NEW   │
│ V04  │ on('effect', ...)            │ RuntimeEventEmitter          │ 2.3   │ NOT_STARTED │ 05-EFFECTS.md             │ NEW   │
│ V05  │ on('breakpoint-hit', ...)    │ RuntimeEventEmitter          │ 4.6   │ NOT_STARTED │ 07-DEBUG.md               │ GroupD│
│ V06  │ on('error', ...)             │ RuntimeEventEmitter          │ 2.3   │ NOT_STARTED │ 06-CONDITIONS.md          │ NEW   │
│ V07  │ on('done', ...)              │ RuntimeEventEmitter          │ 2.3   │ NOT_STARTED │ 08-PROTOCOL.md            │ NEW   │
│ V08  │ on('session-checkpoint',...)│ RuntimeEventEmitter          │ 5.3   │ NOT_STARTED │ 14-SESSION.md             │ NEW   │
│ V09  │ on('opr-receipt', ...)       │ RuntimeEventEmitter          │ 6.4   │ NOT_STARTED │ 09-OPR.md                 │ NEW   │
│      │                              │                              │       │             │                           │       │
│      │ ━━━ UI / CLI ONLY ━━━        │ STAYS IN bin/omega-repl.ts   │       │             │                           │ N/A   │
│ C01  │ :help / :h                   │ REPL only                    │ N/A   │ N/A         │ N/A                       │ CLI   │
│ C02  │ :quit / :q                   │ REPL only                    │ N/A   │ N/A         │ N/A                       │ CLI   │
│ C03  │ readline interface           │ REPL only                    │ N/A   │ N/A         │ N/A                       │ CLI   │
│ C04  │ prompt display               │ REPL only                    │ N/A   │ N/A         │ N/A                       │ CLI   │
│ C05  │ output formatting            │ REPL only                    │ N/A   │ N/A         │ N/A                       │ CLI   │
│ C06  │ VERBOSE flag                 │ REPL only                    │ N/A   │ N/A         │ N/A                       │ CLI   │
│ C07  │ parseArgs()                  │ REPL only                    │ N/A   │ N/A         │ N/A                       │ CLI   │
│ C08  │ extractSexpressions()        │ REPL only                    │ N/A   │ N/A         │ N/A                       │ CLI   │
│      │                              │                              │       │             │                           │       │
└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘

SUMMARY BY PHASE:
  Phase 0 (Types):     B08, N06, H07, L05-L07, L12, O07     (8 features)   → types/
  Phase 1 (State):     M01-M03, I08-I10                      (6 features)   → state layer
  Phase 2 (Exec):      E01-E03, F01-F03, V01, V04, V06, V07 (10 features)  → execution
  Phase 3 (Inspect):   I01-I07                               (7 features)   → inspection
  Phase 4 (Control):   B01-B07, N01-N05, H01-H06, H08,      (25 features)  → control
                       D01-D10, V05
  Phase 5 (Session):   S01-S10, V08                          (11 features)  → session
  Phase 6 (External):  L01-L04, L08-L11, V02-V03,           (16 features)  → integrations
                       O01-O06, V09, G01-G02
  Phase 7 (Facade):    ALL composed                          (-)            → OmegaRuntime
  Phase 9 (Wire):      ALL wired to consumers                (-)            → REPL/debugger
  ──────────────────────────────────────────────────────────────────
  TOTAL TO BUILD:      83 features across Phases 0-6
  TOTAL N/A (CLI):      8 features stay in REPL (C01-C08)
```

---

## Incremental Refactoring Plan

**TWO-STAGE APPROACH** to avoid half-broken intermediate states:

```
┌─────────────────────────────────────────────────────────────────────────────┐
│  STAGE 1: BUILD                        STAGE 2: WIRE                        │
│  ═══════════════                       ═══════════════                       │
│                                                                              │
│  • Create all new code in src/runtime/ • One atomic rewire of consumers     │
│  • Unit test each subsystem (mocks)    • Regression test vs old behavior    │
│  • Don't touch omega-repl.ts           • Gut REPL/debugger/server together  │
│  • Old REPL keeps working 100%         • Delete OmegaLLM-ref when done      │
│                                                                              │
│  Why: No half-broken states, easy      Why: Atomic switchover means no      │
│  to test new code in isolation,        partial wiring complexity            │
│  easy rollback (delete src/runtime/)                                        │
└─────────────────────────────────────────────────────────────────────────────┘
```

**DEPENDENCY ORDER** (why this sequence matters):

```
Types → Events → Providers                  (foundation - everything needs these)
    ↓
MemoryStateProvider + Utilities             (state layer - execution needs this)
    ↓
ExecutionEngine + EffectHandlerRegistry     (THE CORE - produces state)
    ↓
StateInspector                              (reads state from execution)
    ↓
BreakpointManager, SnapshotManager,         (control layer - hooks into execution)
HistoryManager, DebugSubsystem
    ↓
SessionManager                              (persists execution context)
    ↓
LLMIntegration, OprIntegration,             (external integrations)
GovernanceProvider
    ↓
OmegaRuntime facade                         (composes everything)
    ↓
WIRE (one atomic rewire)                    (consumers use new runtime)
```

---

## STAGE 1: BUILD (No REPL Changes)

### Phase 0: Foundation (Types, Events, Interfaces)

| Step | Description | Features | Test | Atomic? |
|------|-------------|----------|------|---------|
| 0.1  | Create `src/runtime/` folder structure | - | `npm run build` | - |
| 0.2  | Add all type definitions to `types/` | E01-E03, all type refs | Types compile | GROUP A |
| 0.3  | Add event system to `events/` | V01-V09 | Unit: emitter | GROUP A |
| 0.4  | Add provider interfaces (no impls) | M01-M03, G01-G02, F01-F03 | Types compile | GROUP A |

**GROUP A**: Steps 0.2-0.4 must be ONE commit (types reference each other)

---

### Phase 1: State Layer (Before Execution)

| Step | Description | Features | Test | Atomic? |
|------|-------------|----------|------|---------|
| 1.1  | `MemoryStateProvider` (wraps COWStore) | M01-M02 | Unit: create/clone store | GROUP B |
| 1.2  | Internal utilities: StateCloner, ValueSerializer | M03, I08-I10 | Unit: clone, serialize | GROUP B |

**GROUP B**: Steps 1.1-1.2 should be one commit (StateProvider needs utilities)

---

### Phase 2: Execution Layer (The Core)

| Step | Description | Features | Test | Atomic? |
|------|-------------|----------|------|---------|
| 2.1  | `ExecutionEngine` (step/run/eval) | E01-E03, F03 | Unit: eval returns correct Val | GROUP C |
| 2.2  | `EffectHandlerRegistry` | F01-F02 | Unit: register/invoke handlers | GROUP C |
| 2.3  | Emit step/effect/error/done events | V01, V04, V06, V07 | Unit: events fire | GROUP C |

**GROUP C**: Steps 2.1-2.3 must be one commit (engine needs effects and events)

**CHECKPOINT**: After Phase 2, you can eval() expressions programmatically!

---

### Phase 3: Inspection Layer (Reads Execution State)

| Step | Description | Features | Test | Atomic? |
|------|-------------|----------|------|---------|
| 3.1  | `StateInspector` + FrameFormatter, ControlFormatter | I01-I10 | Unit: inspect mock state | - |

**Depends on**: Phase 2 (needs State type from ExecutionEngine)

---

### Phase 4: Control Layer (Hooks Into Execution)

| Step | Description | Features | Test | Atomic? |
|------|-------------|----------|------|---------|
| 4.1  | `BreakpointManager` | B01-B08 | Unit: add/remove/check | - |
| 4.2  | `SnapshotManager` + `MemorySnapshotProvider` | N01-N06 | Unit: save/restore mock state | - |
| 4.3  | `HistoryManager` + `MemoryTraceProvider` | H01-H08 | Unit: record/back/list | - |
| 4.4  | `DebugSubsystem` (orchestrates above) | D01-D10 | Unit: load/step/run/goto | GROUP D |
| 4.5  | Integrate breakpoint checking into DebugSubsystem | B07 | Unit: breakpoint fires | GROUP D |
| 4.6  | Emit breakpoint-hit event | V05 | Unit: event fires on break | GROUP D |

**GROUP D**: Steps 4.4-4.6 should be one commit (debug needs integrated breakpoints)

**CHECKPOINT**: After Phase 4, debugging works programmatically!

---

### Phase 5: Session Layer

| Step | Description | Features | Test | Atomic? |
|------|-------------|----------|------|---------|
| 5.1  | `SessionManager` | S01-S08 | Unit: list/save/load/fork (mock provider) | - |
| 5.2  | `FilesystemSessionProvider` (wraps core/session/) | S09-S10 | Unit: read/write session files | - |
| 5.3  | Emit session-checkpoint event | V08 | Unit: event fires | - |

---

### Phase 6: External Integrations

| Step | Description | Features | Test | Atomic? |
|------|-------------|----------|------|---------|
| 6.1  | `LLMIntegration` | L01-L12 | Unit: ask() with mock adapter | - |
| 6.2  | Emit before-llm/after-llm events | V02, V03 | Unit: events fire | - |
| 6.3  | `OprIntegration` + `InMemoryReceiptProvider` | O01-O07 | Unit: run kernel, store receipt | - |
| 6.4  | Emit opr-receipt event | V09 | Unit: event fires | - |
| 6.5  | `LocalGovernanceProvider` | G01-G02 | Unit: check caps, consume budget | - |

---

### Phase 7: Facade (Composes Everything)

| Step | Description | Features | Test | Atomic? |
|------|-------------|----------|------|---------|
| 7.1  | `OmegaRuntime` class | ALL | Integration: full API works | GROUP E |
| 7.2  | `createRuntime()` factory | ALL | Integration: factory creates runtime | GROUP E |
| 7.3  | Public exports from `index.ts` | ALL | Integration: imports work | GROUP E |

**GROUP E**: Steps 7.1-7.3 should be one commit (facade is the public API)

**CHECKPOINT**: After Phase 7, external code can import and use OmegaRuntime!

---

## STAGE 2: WIRE (Atomic Switchover)

### Phase 8: Integration Tests (Before Wiring)

| Step | Description | Features | Test | Atomic? |
|------|-------------|----------|------|---------|
| 8.1  | Write integration tests using OmegaRuntime | ALL | Integration: compare with REPL | - |
| 8.2  | Create test harness: same inputs → compare outputs | ALL | Diff: old REPL vs new runtime | - |

**Purpose**: Confidence that new runtime behaves identically to old REPL

---

### Phase 9: Rewire Consumers (ONE ATOMIC CHANGE)

| Step | Description | Features | Test | Atomic? |
|------|-------------|----------|------|---------|
| 9.1  | Write new thin `omega-repl.ts` (~300 lines) | ALL | Regression: all REPL tests pass | GROUP F |
| 9.2  | Write new thin `omega-debugger.ts` (~200 lines) | ALL | Regression: debugger tests pass | GROUP F |
| 9.3  | Rewire `debugSession.ts` to use OmegaRuntime | ALL | Regression: server tests pass | GROUP F |

**GROUP F**: Steps 9.1-9.3 MUST be ONE commit (can't have half-wired system)

**CRITICAL**: Run FULL regression test suite before committing!

---

### Phase 10: Cleanup

| Step | Description | Features | Test | Atomic? |
|------|-------------|----------|------|---------|
| 10.1 | Delete `OmegaLLM-ref/` folder | - | Build passes | - |
| 10.2 | Remove deprecated code paths | - | All tests pass | - |
| 10.3 | Write documentation | - | Docs complete | - |

---

## Atomic Groups Summary

| Group | Steps | Reason | Commit Message |
|-------|-------|--------|----------------|
| A | 0.2-0.4 | Types reference each other, can't compile separately | `feat(runtime): add types, events, provider interfaces` |
| B | 1.1-1.2 | StateProvider needs utilities | `feat(runtime): add state layer` |
| C | 2.1-2.3 | Engine needs effects and events to work | `feat(runtime): add execution engine` |
| D | 4.4-4.6 | Debug needs integrated breakpoints | `feat(runtime): add debug subsystem` |
| E | 7.1-7.3 | Facade is the public API | `feat(runtime): add OmegaRuntime facade` |
| F | 9.1-9.3 | Can't have half-wired system | `refactor: rewire consumers to use OmegaRuntime` |

---

## Testing Strategy

| Phase | Test Type | What |
|-------|-----------|------|
| BUILD (0-7) | **Unit tests** | Each subsystem tested with mock dependencies |
| PRE-WIRE (8) | **Integration tests** | Full runtime tested, compare outputs with old REPL |
| POST-WIRE (9) | **Regression tests** | All existing REPL/debugger/server tests pass |
| CLEANUP (10) | **Final validation** | Full test suite, no regressions |

---

## Workflow

**For BUILD steps (0-7):**
1. Read step description
2. Update STATUS in checklist: `NOT_STARTED` → `PLANNED`
3. Implement the subsystem
4. Write unit tests (use mocks for dependencies)
5. Update STATUS: `PLANNED` → `UNIT_TESTED`
6. Commit with message referencing step number

**For WIRE step (9):**
1. Run integration tests from Phase 8 - must all pass
2. Update STATUS for ALL features: `UNIT_TESTED` → `PLANNED` (for wiring)
3. Create new thin consumers in ONE atomic change
4. Run FULL regression test suite
5. Update STATUS: `PLANNED` → `REGR_TESTED`
6. Commit with group message
7. Update STATUS: `REGR_TESTED` → `DONE`

---

## Foundation Layer (src/core/ - UNCHANGED)

The language processing, CESK machine, and core modules stay exactly as they are.
The new runtime WRAPS these - it doesn't replace them.

```
┌─────────────────────────────────────────────────────────────────────────────────────────┐
│                      FOUNDATION LAYER - STAYS UNCHANGED                                  │
│                      ══════════════════════════════════════                              │
├─────────────────────────────────────────────────────────────────────────────────────────┤
│                                                                                          │
│  src/core/eval/                     │ THE CESK MACHINE                                  │
│  ─────────────────────────────────────────────────────────────────────────────────────  │
│  machine.ts                         │ State, Control, Frame types                       │
│  machineStep.ts                     │ stepOnce() - single step execution                │
│  store.ts                           │ COWStore - copy-on-write store                    │
│  values.ts                          │ Val type - all value types                        │
│  env.ts                             │ Env, Ctx - environment/context                    │
│  run.ts                             │ runToCompletion(), runToCompletionWithState()    │
│                                     │                                                   │
│  src/core/pipeline/                 │ PARSER / COMPILER                                 │
│  ─────────────────────────────────────────────────────────────────────────────────────  │
│  compileText.ts                     │ compileTextToExpr() - text → AST                  │
│  (other pipeline files)             │ Tokenizer, parser internals                       │
│                                     │                                                   │
│  src/core/effects/                  │ EFFECT SYSTEM                                     │
│  ─────────────────────────────────────────────────────────────────────────────────────  │
│  runtimeImpl.ts                     │ RuntimeImpl - effect dispatch                     │
│  opcall.ts                          │ OpCall type, effect handling                      │
│                                     │                                                   │
│  src/core/oracle/                   │ LLM ADAPTERS                                      │
│  ─────────────────────────────────────────────────────────────────────────────────────  │
│  adapter.ts                         │ OracleAdapter interface                           │
│  adapters/openai.ts                 │ OpenAI implementation                             │
│  adapters/anthropic.ts              │ Anthropic implementation                          │
│  scriptedOracle.ts                  │ ScriptedOracleAdapter for testing                 │
│  snapshots.ts                       │ SnapshotRepo                                      │
│  receipts.ts                        │ InMemoryReceiptStore                              │
│                                     │                                                   │
│  src/core/opr/                      │ OPR RUNTIME                                       │
│  ─────────────────────────────────────────────────────────────────────────────────────  │
│  runtime.ts                         │ OprRuntime - kernel execution                     │
│  callbacks.ts                       │ OPR callback infrastructure                       │
│  receipts.ts                        │ OPR receipt store                                 │
│  kernels/                           │ All kernel definitions                            │
│  adapters/openai.ts                 │ OpenAIOprAdapter                                  │
│  adapters/anthropic.ts              │ AnthropicOprAdapter                               │
│                                     │                                                   │
│  src/core/governance/               │ GOVERNANCE / CAPS                                 │
│  ─────────────────────────────────────────────────────────────────────────────────────  │
│  profile.ts                         │ Profile loading                                   │
│  caps.ts                            │ CapSet - capability checking                      │
│  budgets.ts                         │ BudgetTracker                                     │
│                                     │                                                   │
│  src/core/session/                  │ SESSION PERSISTENCE INTERNALS                     │
│  ─────────────────────────────────────────────────────────────────────────────────────  │
│  writer.ts                          │ SessionWriter - writes session files              │
│  reader.ts                          │ SessionReader - reads session files               │
│  index.ts                           │ JumpController, renderTrace                       │
│  nativeRegistry.ts                  │ buildNativeRegistry()                             │
│  solverRegistry.ts                  │ buildSolverRegistry()                             │
│                                     │                                                   │
│  src/core/provenance/               │ PROVENANCE                                        │
│  ─────────────────────────────────────────────────────────────────────────────────────  │
│  graph.ts                           │ Provenance graph                                  │
│                                     │                                                   │
└─────────────────────────────────────────────────────────────────────────────────────────┘

WHY UNCHANGED:
  - These are the LANGUAGE IMPLEMENTATION - they define what Omega IS
  - The runtime layer orchestrates them, it doesn't replace them
  - ExecutionEngine calls machineStep.ts, it doesn't reimplement stepping
  - LLMIntegration uses oracle adapters, it doesn't create new ones
  - OprIntegration wraps OprRuntime, it doesn't reimplement kernels

ARCHITECTURE:
  ┌─────────────────────────────────────────────────────────────┐
  │                    CONSUMERS                                 │
  │  (REPL, HTTP Server, Tests, External Tools)                 │
  └──────────────────────────┬──────────────────────────────────┘
                             │
                             ▼
  ┌─────────────────────────────────────────────────────────────┐
  │                src/runtime/  (NEW LAYER)                     │
  │  OmegaRuntime, subsystems, providers, events                │
  │  ← This is what we're building                              │
  └──────────────────────────┬──────────────────────────────────┘
                             │ wraps/uses
                             ▼
  ┌─────────────────────────────────────────────────────────────┐
  │                 src/core/  (UNCHANGED)                       │
  │  eval/, effects/, oracle/, opr/, governance/, session/      │
  │  ← Language implementation, stays exactly as is             │
  └─────────────────────────────────────────────────────────────┘
```

---

## Source → Destination Mapping

Shows which code files/functions move where (implementation level).

```
┌─────────────────────────────────────────────────────────────────────────────────┐
│                     SOURCE → DESTINATION MAPPING                                 │
├─────────────────────────────────────────────────────────────────────────────────┤
│                                                                                  │
│  SOURCE (OmegaLLM-ref)                    DESTINATION (OmegaLLM)                │
│  ══════════════════════                   ══════════════════════                │
│                                                                                  │
│  ─── EXECUTION (step loop, eval) ───                                            │
│  bin/omega-repl.ts                                                              │
│    runStep(), evalExpr(), runToCompletion()                                     │
│    handleEffect(), doStep()               →  subsystems/ExecutionEngine.ts      │
│  src/core/eval/machine.ts                 →  (used internally, unchanged)       │
│  src/core/eval/machineStep.ts             →  (used internally, unchanged)       │
│                                                                                  │
│  ─── DEBUGGING ───                                                              │
│  bin/omega-repl.ts                                                              │
│    :debug, :step, :run, :stop, :goto      →  subsystems/DebugSubsystem.ts       │
│    debugState, stepCount, traceHistory    →  (internal state)                   │
│  bin/omega-debugger.ts (entire file)      →  subsystems/DebugSubsystem.ts       │
│                                                                                  │
│  ─── BREAKPOINTS ───                                                            │
│  bin/omega-repl.ts                                                              │
│    :break, :breaks, :delbreak, :toggle                                          │
│    breakpoints[], checkBreakpoint()       →  subsystems/BreakpointManager.ts    │
│                                                                                  │
│  ─── SNAPSHOTS ───                                                              │
│  bin/omega-repl.ts                                                              │
│    :save, :restore, :snapshots, :export                                         │
│    savedSnapshots Map                     →  subsystems/SnapshotManager.ts      │
│                                                                                  │
│  ─── SESSION PERSISTENCE ───                                                    │
│  bin/omega-repl.ts                                                              │
│    :session save/load/fork/list/goto                                            │
│  src/core/session/writer.ts               →  providers/FilesystemSessionProvider│
│  src/core/session/reader.ts               →  providers/FilesystemSessionProvider│
│                                                                                  │
│  ─── STATE INSPECTION ───                                                       │
│  bin/omega-repl.ts                                                              │
│    :env, :defs, :stack, :frame, :control                                        │
│    printEnv(), printStack(), etc.         →  subsystems/StateInspector.ts       │
│                                                                                  │
│  ─── HISTORY / TIME TRAVEL ───                                                  │
│  bin/omega-repl.ts                                                              │
│    :back, :history, :record, :dump, :replay                                     │
│    history[], recordTrace()               →  subsystems/HistoryManager.ts       │
│                                                                                  │
│  ─── LLM / AGENT ───                                                            │
│  bin/omega-repl.ts                                                              │
│    :ask, :traces, :trace                                                        │
│    agentLoop(), toolCalling               →  subsystems/LLMIntegration.ts       │
│  src/core/oracle/adapter.ts               →  (used internally, unchanged)       │
│  src/core/oracle/openai.ts                →  (used internally, unchanged)       │
│  src/core/oracle/anthropic.ts             →  (used internally, unchanged)       │
│                                                                                  │
│  ─── OPR KERNELS ───                                                            │
│  bin/omega-repl.ts                                                              │
│    :opr-list, :opr-run, :opr-receipts, :opr-verify                              │
│  src/core/opr/runtime.ts                  →  subsystems/OprIntegration.ts       │
│  src/core/opr/callbacks.ts                →  (used internally, unchanged)       │
│                                                                                  │
│  ─── EFFECT HANDLING ───                                                        │
│  bin/omega-repl.ts                                                              │
│    handleEffect(), inferOp handlers       →  providers/EffectHandlerRegistry.ts │
│  src/core/effects/runtimeImpl.ts          →  (used internally, unchanged)       │
│  src/core/effects/opcall.ts               →  (used internally, unchanged)       │
│                                                                                  │
│  ─── GOVERNANCE ───                                                             │
│  src/core/governance/profile.ts           →  providers/LocalGovernanceProvider  │
│  src/core/governance/caps.ts              →  (used internally, unchanged)       │
│  src/core/governance/budgets.ts           →  (used internally, unchanged)       │
│                                                                                  │
│  ─── STATE MANAGEMENT ───                                                       │
│  bin/omega-repl.ts                                                              │
│    currentStore, createStore()            →  providers/MemoryStateProvider.ts   │
│  src/core/eval/store.ts                   →  (used internally, unchanged)       │
│                                                                                  │
└─────────────────────────────────────────────────────────────────────────────────┘

WHAT STAYS IN src/core/ (UNCHANGED):
  - eval/machine.ts, machineStep.ts, store.ts, values.ts  (CESK machine)
  - effects/runtimeImpl.ts, opcall.ts                     (effect dispatch)
  - oracle/adapter.ts, openai.ts, anthropic.ts            (LLM adapters)
  - opr/runtime.ts, callbacks.ts, kernels/                (OPR implementation)
  - governance/profile.ts, caps.ts, budgets.ts            (governance core)
  - pipeline/compileText.ts                               (parser)
  - provenance/graph.ts                                   (provenance)

WHAT GETS CREATED (NEW):
  - src/runtime/                                          (entire new folder)
  - All types, events, providers, subsystems

WHAT GETS GUTTED (REWRITTEN):
  - bin/omega-repl.ts         3163 → ~300 lines (keep readline + dispatch only)
  - bin/omega-debugger.ts     1197 → ~200 lines (keep TUI + dispatch only)
  - src/server/debugSession.ts      → thin wrapper over runtime
```

---

## Problem Statement

Current state:
- `bin/omega-repl.ts` (3163 lines) - has its own step loop, effect handling, LLM wiring
- `bin/omega-debugger.ts` (1197 lines) - another step loop, breakpoints, time travel
- `src/server/debugSession.ts` - yet another implementation

Three places doing the same thing. No hooks. No events. External code can't observe "about to call LLM" - it's buried inside.

---

## Architecture Diagrams

### CURRENT Architecture (The Problem)

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         CURRENT ARCHITECTURE                                 │
│                         (Entangled, Duplicated)                              │
└─────────────────────────────────────────────────────────────────────────────┘

┌──────────────────────┐  ┌──────────────────────┐  ┌──────────────────────┐
│   bin/omega-repl.ts  │  │ bin/omega-debugger.ts│  │  src/server/         │
│      (3163 lines)    │  │    (1197 lines)      │  │  debugSession.ts     │
├──────────────────────┤  ├──────────────────────┤  ├──────────────────────┤
│ • Step loop          │  │ • Step loop          │  │ • Step loop          │
│ • Effect handling    │  │ • Effect handling    │  │ • Effect handling    │
│ • LLM adapters       │  │ • Breakpoints        │  │ • LLM adapters       │
│ • Session save/load  │  │ • State inspection   │  │ • Breakpoints        │
│ • Breakpoints        │  │ • Time travel        │  │ • History            │
│ • Time travel        │  └──────────┬───────────┘  │ • State serialization│
│ • OPR kernels        │             │              └──────────┬───────────┘
│ • :ask LLM agent     │             │                         │
│ • Trace recording    │             │                         │
│ • 40+ REPL commands  │             │                         │
└──────────┬───────────┘             │                         │
           │                         │                         │
           │    DUPLICATED LOGIC     │                         │
           │◄────────────────────────┼─────────────────────────┤
           │                         │                         │
           ▼                         ▼                         ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                              src/core/                                       │
├─────────────────────────────────────────────────────────────────────────────┤
│  eval/              effects/           oracle/            opr/              │
│  ├─machine.ts       ├─runtimeImpl.ts   ├─adapter.ts       ├─runtime.ts      │
│  ├─machineStep.ts   └─opcall.ts        ├─openai.ts        ├─callbacks.ts    │
│  ├─store.ts                            ├─anthropic.ts     └─kernels/        │
│  └─values.ts                           └─scripted.ts                        │
│                                                                              │
│  pipeline/          governance/        session/           provenance/       │
│  └─compileText.ts   ├─profile.ts       ├─writer.ts        └─graph.ts        │
│                     ├─caps.ts          └─reader.ts                          │
│                     └─budgets.ts                                            │
└─────────────────────────────────────────────────────────────────────────────┘

PROBLEMS:
  ✗ 3 implementations of step/run loop
  ✗ 3 implementations of effect handling
  ✗ No single public API
  ✗ No hooks/events - logic is buried
  ✗ Can't use runtime without bringing in REPL/HTTP dependencies
  ✗ 3163-line REPL mixes UI with runtime logic
```

### REPL Feature Inventory (What Must Be Preserved)

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                    REPL FEATURES (3163 lines)                                │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  EVALUATION                    SESSION                                       │
│  ├─ eval expression            ├─ :session save/load/fork                   │
│  └─ :loadfile                  ├─ :session goto <checkpoint>                │
│                                ├─ :session trace                            │
│                                └─ :session resume                            │
│                                                                              │
│  DEBUGGING                     BREAKPOINTS                                   │
│  ├─ :debug (expr)              ├─ :break step/expr/effect                   │
│  ├─ :step [N]                  ├─ :breaks (list)                            │
│  ├─ :run / :continue           ├─ :delbreak <id>                            │
│  ├─ :stop                      └─ :toggle <id>                              │
│  ├─ :goto <step>                                                             │
│  ├─ :trace                     SNAPSHOTS                                     │
│  └─ :state                     ├─ :save / :restore                          │
│                                ├─ :snapshots (list)                         │
│  INSPECTION                    └─ :export                                    │
│  ├─ :env / :env <name>                                                       │
│  ├─ :defs                      HISTORY & TIME TRAVEL                         │
│  ├─ :stack                     ├─ :back [N]                                 │
│  ├─ :frame <n>                 ├─ :history                                  │
│  └─ :control                   ├─ :record on/off                            │
│                                ├─ :dump / :replay                           │
│  LLM (AGENTIC)                                                               │
│  ├─ :ask <question>            OPR                                           │
│  ├─ :traces (list)             ├─ :opr-list                                 │
│  └─ :trace <id> [-v]           ├─ :opr-run <kernel> <json>                  │
│                                ├─ :opr-receipts                             │
│                                └─ :opr-verify                                │
└─────────────────────────────────────────────────────────────────────────────┘
```

### FUTURE Architecture (The Goal)

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         FUTURE ARCHITECTURE                                  │
│                         (Clean Boundaries, Hooks)                            │
└─────────────────────────────────────────────────────────────────────────────┘

┌──────────────────────┐  ┌──────────────────────┐  ┌──────────────────────┐
│   bin/omega-repl.ts  │  │  src/server/         │  │    External Code     │
│    (~300 lines)      │  │  debugServer.ts      │  │   (tests, tools)     │
├──────────────────────┤  ├──────────────────────┤  ├──────────────────────┤
│ • readline loop      │  │ • HTTP routes        │  │ • import runtime     │
│ • parse commands     │  │ • WebSocket          │  │ • call methods       │
│ • dispatch to runtime│  │ • dispatch to runtime│  │ • subscribe to hooks │
└──────────┬───────────┘  └──────────┬───────────┘  └──────────┬───────────┘
           │                         │                         │
           │         ALL USE THE SAME PUBLIC API               │
           │◄────────────────────────┼─────────────────────────┤
           │                         │                         │
           ▼                         ▼                         ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                                                                              │
│   src/runtime/index.ts  ◄──── THE PACKAGE BOUNDARY                          │
│   ════════════════════                                                       │
│                                                                              │
│   export class OmegaRuntime {                                                │
│                                                                              │
│     // ─── Core Execution ───                                                │
│     eval(code: string): Promise<EvalResult>                                  │
│     loadFile(path: string): Promise<void>                                    │
│                                                                              │
│     // ─── Debugging ───                                                     │
│     debug: {                                                                 │
│       load(code: string): void                                               │
│       step(n?: number): StepResult                                           │
│       run(): RunResult                                                       │
│       stop(): void                                                           │
│       goto(step: number): void                                               │
│       getState(): DebugState                                                 │
│       getTrace(): TraceEntry[]                                               │
│     }                                                                        │
│                                                                              │
│     // ─── Breakpoints ───                                                   │
│     breakpoints: {                                                           │
│       add(bp: BreakpointSpec): string                                        │
│       remove(id: string): void                                               │
│       list(): Breakpoint[]                                                   │
│       toggle(id: string): void                                               │
│     }                                                                        │
│                                                                              │
│     // ─── Snapshots ───                                                     │
│     snapshots: {                                                             │
│       save(name: string): void                                               │
│       restore(name: string): void                                            │
│       list(): SnapshotInfo[]                                                 │
│       export(name: string, path: string): void                               │
│     }                                                                        │
│                                                                              │
│     // ─── Session Persistence ───                                           │
│     session: {                                                               │
│       save(name: string): void                                               │
│       load(name: string): void                                               │
│       fork(name: string): void                                               │
│       list(): SessionInfo[]                                                  │
│       goto(checkpoint: string | number): void                                │
│       getTrace(): SessionTrace                                               │
│       getCheckpoints(): Checkpoint[]                                         │
│     }                                                                        │
│                                                                              │
│     // ─── Inspection ───                                                    │
│     inspect: {                                                               │
│       env(name?: string): EnvBindings | Value                                │
│       stack(): StackFrame[]                                                  │
│       frame(n: number): FrameDetail                                          │
│       control(): ControlInfo                                                 │
│       defs(): Definition[]                                                   │
│     }                                                                        │
│                                                                              │
│     // ─── LLM / Agent ───                                                   │
│     llm: {                                                                   │
│       configure(adapter: LLMAdapter): void                                   │
│       ask(question: string): Promise<AgentResult>                            │
│       getTraces(): LLMTrace[]                                                │
│       getTrace(id: string): LLMTraceDetail                                   │
│     }                                                                        │
│                                                                              │
│     // ─── OPR Kernels ───                                                   │
│     opr: {                                                                   │
│       list(): KernelInfo[]                                                   │
│       run(kernel: string, program: unknown): Promise<KernelResult>           │
│       getReceipts(): Receipt[]                                               │
│       verify(): VerifyResult                                                 │
│     }                                                                        │
│                                                                              │
│     // ─── History / Time Travel ───                                         │
│     history: {                                                               │
│       back(n?: number): void                                                 │
│       list(n?: number): HistoryEntry[]                                       │
│       setRecording(enabled: boolean): void                                   │
│       dump(path: string): void                                               │
│       replay(path: string): void                                             │
│     }                                                                        │
│                                                                              │
│     // ═══ HOOKS / EVENTS ═══  (THE KEY ADDITION)                            │
│     on(event: 'step', handler: (s: StepEvent) => void): Unsubscribe          │
│     on(event: 'before-llm', handler: (r: LLMRequest) => void): Unsubscribe   │
│     on(event: 'after-llm', handler: (r, res) => void): Unsubscribe           │
│     on(event: 'effect', handler: (e: Effect) => void): Unsubscribe           │
│     on(event: 'breakpoint-hit', handler: (bp) => void): Unsubscribe          │
│     on(event: 'error', handler: (err) => void): Unsubscribe                  │
│     on(event: 'done', handler: (result) => void): Unsubscribe                │
│     on(event: 'session-checkpoint', handler: (cp) => void): Unsubscribe      │
│     on(event: 'opr-receipt', handler: (r) => void): Unsubscribe              │
│   }                                                                          │
│                                                                              │
│   // Factory function                                                        │
│   export function createRuntime(config?: RuntimeConfig): OmegaRuntime        │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                          uses providers (pluggable)
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                         PLUGGABLE PROVIDERS                                  │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌─────────────────────┐  ┌─────────────────────┐  ┌─────────────────────┐  │
│  │   StateProvider     │  │  SessionProvider    │  │  SnapshotProvider   │  │
│  ├─────────────────────┤  ├─────────────────────┤  ├─────────────────────┤  │
│  │ • MemoryState       │  │ • FilesystemSession │  │ • MemorySnapshot    │  │
│  │ • FilesystemState   │  │ • DatabaseSession   │  │ • FilesystemSnap    │  │
│  │ • DatabaseState     │  │ • CloudSession      │  │ • CloudSnapshot     │  │
│  └─────────────────────┘  └─────────────────────┘  └─────────────────────┘  │
│                                                                              │
│  ┌─────────────────────┐  ┌─────────────────────┐  ┌─────────────────────┐  │
│  │  ReceiptProvider    │  │   TraceProvider     │  │ EffectHandlerReg    │  │
│  ├─────────────────────┤  ├─────────────────────┤  ├─────────────────────┤  │
│  │ • InMemoryReceipts  │  │ • MemoryTrace       │  │ • Default handlers  │  │
│  │ • FilesystemReceipts│  │ • FilesystemTrace   │  │ • Custom handlers   │  │
│  │ • DatabaseReceipts  │  │ • StreamingTrace    │  │ • Mock handlers     │  │
│  └─────────────────────┘  └─────────────────────┘  └─────────────────────┘  │
│                                                                              │
│  ┌─────────────────────┐                                                     │
│  │ GovernanceProvider  │                                                     │
│  ├─────────────────────┤                                                     │
│  │ • LocalGovernance   │                                                     │
│  │ • RemoteGovernance  │                                                     │
│  │ • DynamicCaps       │                                                     │
│  └─────────────────────┘                                                     │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                          uses internally
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                         src/core/ (unchanged internals)                      │
├─────────────────────────────────────────────────────────────────────────────┤
│  eval/    effects/    oracle/    opr/    governance/    pipeline/    etc.   │
└─────────────────────────────────────────────────────────────────────────────┘

BENEFITS:
  ✓ Single source of truth for runtime logic
  ✓ Clean public API - anyone can use programmatically
  ✓ Hooks let external code observe/intercept everything
  ✓ REPL becomes trivial (~300 lines of readline + dispatch)
  ✓ DebugServer just wraps runtime methods
  ✓ Tests use same API as production code
  ✓ No dependency pollution - runtime has no HTTP/readline deps
  ✓ All storage is pluggable via providers
```

---

### Subsystem Reference Table (24 Subsystems)

**Quick lookup for implementers: subsystem → source document → key methods**

```
┌──────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
│                           24 SUBSYSTEMS REFERENCE TABLE                                                           │
│                    Category │ Subsystem │ Source Document │ Key Methods                                          │
├──────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
│                                                                                                                   │
│  ═══ CORE (1) ════════════════════════════════════════════════════════════════════════════════════════════════  │
│  Execution   │ ExecutionEngine      │ 01-EVAL.md            │ eval(), step(), run(), loadFile()                 │
│                                                                                                                   │
│  ═══ DEBUGGING (6) ═══════════════════════════════════════════════════════════════════════════════════════════  │
│  Debug       │ DebugSubsystem       │ 07-DEBUG.md           │ load(), step(), run(), goto(), stop()             │
│  Debug       │ BreakpointManager    │ 07-DEBUG.md           │ add(), remove(), list(), toggle(), check()        │
│  Debug       │ SnapshotManager      │ 07-DEBUG.md           │ save(), restore(), list(), saveImage()            │
│  Debug       │ StateInspector       │ 01-EVAL.md            │ env(), stack(), frame(), defs(), control()        │
│  Debug       │ HistoryManager       │ 07-DEBUG.md           │ back(), list(), dump(), replay()                  │
│  Debug       │ SessionManager       │ 14-SESSION.md         │ save(), load(), fork(), goto(), getTrace()        │
│                                                                                                                   │
│  ═══ DATA (2) ════════════════════════════════════════════════════════════════════════════════════════════════  │
│  Data        │ ArtifactManager      │ 26-ARTIFACTS.md       │ get(), put(), stats(), clear()                    │
│  Data        │ FactsManager         │ 23-FACTS.md           │ all(), has(), count(), signature(), query()       │
│                                                                                                                   │
│  ═══ CONTROL (3) ═════════════════════════════════════════════════════════════════════════════════════════════  │
│  Control     │ ConditionsManager    │ 06-CONDITIONS.md      │ signal(), bind(), restart(), getRestarts()        │
│  Control     │ FixpointManager      │ 24-FIXPOINT.md        │ run(), getState(), signature(), isInFixpoint()    │
│  Control     │ TransactionManager   │ 27-OUTCOMES.md        │ begin(), propose(), commit(), rollback()          │
│                                                                                                                   │
│  ═══ GOVERNANCE (3) ══════════════════════════════════════════════════════════════════════════════════════════  │
│  Governance  │ ProvenanceManager    │ 22-PROVENANCE.md      │ capture(), verify(), isStale(), graph()           │
│  Governance  │ SecurityManager      │ 21-SECURITY.md        │ checkCap(), validateLLM(), getAuditLog()          │
│  Governance  │ BudgetManager        │ 25-BUDGET.md          │ remaining(), consume(), report(), hasRemaining()  │
│                                                                                                                   │
│  ═══ COMMUNICATION (2) ═══════════════════════════════════════════════════════════════════════════════════════  │
│  Comm        │ ProtocolServer       │ 08-PROTOCOL.md        │ handle(), use(), listen(), close()                │
│  Comm        │ ConcurrencyManager   │ 12-CONCURRENCY.md     │ spawn(), chan(), awaitAll(), fiberSpawn()         │
│                                                                                                                   │
│  ═══ LLM (3) ═════════════════════════════════════════════════════════════════════════════════════════════════  │
│  LLM         │ LLMIntegration       │ 10-ORACLE.md          │ ask(), configure(), getTraces(), getTrace()       │
│  LLM         │ OprIntegration       │ 09-OPR.md             │ list(), run(), getReceipts(), verify()            │
│  LLM         │ ExpertManager        │ 29-EXPERTS.md         │ registerRole(), compileIntent(), runParallel()    │
│                                                                                                                   │
│  ═══ SEMANTIC (4) ════════════════════════════════════════════════════════════════════════════════════════════  │
│  Semantic    │ AmbManager           │ USER-MANUAL--05,26    │ choose(), fail(), require(), allSolutions()       │
│  Semantic    │ StreamsManager       │ USER-MANUAL--07,23    │ consStream(), streamMap(), streamFilter()         │
│  Semantic    │ LogicManager         │ USER-MANUAL--27       │ assertFact(), query(), prove(), unify()           │
│  Semantic    │ MacroManager         │ 11-MACROS.md          │ define(), expand1(), expandAll(), gensym()        │
│                                                                                                                   │
└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
```

---

### File System Structure (Target)

**COMPLETE: All 24 subsystems with source document references.**

```
src/runtime/
├── index.ts                          # Public exports: createRuntime, OmegaRuntime, types
├── OmegaRuntime.ts                   # Main facade class (24 subsystems)
│
├── types/                            # ALL non-event types
│   ├── index.ts                      # Re-exports all types + aliases
│   │
│   │  ─── CORE TYPES ───
│   ├── RuntimeConfig.ts              # [01-EVAL.md] RuntimeConfig interface
│   ├── EvalResult.ts                 # [01-EVAL.md] EvalResult interface
│   ├── StepResult.ts                 # [01-EVAL.md] StepResult interface
│   ├── RunResult.ts                  # [01-EVAL.md] RunResult interface
│   ├── aliases.ts                    # Type aliases: Effect=OpCall, Receipt=OprReceipt, etc.
│   │
│   │  ─── DEBUG & SESSION ───
│   ├── DebugState.ts                 # [07-DEBUG.md] DebugState interface
│   ├── TraceEntry.ts                 # [07-DEBUG.md] TraceEntry interface
│   ├── Breakpoint.ts                 # [07-DEBUG.md] Breakpoint, BreakpointSpec
│   ├── SnapshotInfo.ts               # [07-DEBUG.md] SnapshotInfo, MachineSnapshot
│   ├── SessionInfo.ts                # [14-SESSION.md] SessionInfo, SessionData, Checkpoint
│   ├── HistoryEntry.ts               # [07-DEBUG.md] HistoryEntry interface
│   ├── ControlInfo.ts                # [01-EVAL.md] ControlInfo interface
│   ├── StackFrame.ts                 # [01-EVAL.md] StackFrame, FrameDetail
│   ├── EnvBindings.ts                # [01-EVAL.md] EnvBindings, Definition
│   │
│   │  ─── LLM & OPR ───
│   ├── LLMTypes.ts                   # [10-ORACLE.md] LLMAdapter, AgentResult, LLMTrace*
│   ├── OprTypes.ts                   # [09-OPR.md] KernelInfo, KernelResult, OprReceipt
│   │
│   │  ─── EFFECT & CONTEXT ───
│   ├── EffectContext.ts              # [05-EFFECTS.md] Effect handler context
│   ├── EffectResult.ts               # [05-EFFECTS.md] Effect handler result
│   ├── MachineSnapshot.ts            # [01-EVAL.md] Full machine state snapshot
│   ├── SessionData.ts                # [14-SESSION.md] Session serialization
│   ├── SessionTrace.ts               # [14-SESSION.md] Session trace entry
│   ├── VerifyResult.ts               # [09-OPR.md] OPR verification result
│   ├── Definition.ts                 # [01-EVAL.md] Definition entry
│   │
│   │  ─── OUTCOME TYPES (27-OUTCOMES.md) ───
│   ├── Outcome.ts                    # [27-OUTCOMES.md] ok|proposed|nonconverged|cycle|needs|error
│   ├── OutcomeMetadata.ts            # [27-OUTCOMES.md] iterations, tokens, cost, timeMs
│   ├── NeedType.ts                   # [27-OUTCOMES.md] needs-evidence, needs-reframing, etc.
│   ├── Proposal.ts                   # [27-OUTCOMES.md] write|delete|run proposals
│   │
│   │  ─── ARTIFACT TYPES (26-ARTIFACTS.md) ───
│   ├── Artifact.ts                   # [26-ARTIFACTS.md] ArtifactKey, Artifact, deps
│   ├── ArtifactStats.ts              # [26-ARTIFACTS.md] Cache statistics
│   │
│   │  ─── BUDGET TYPES (25-BUDGET.md) ───
│   ├── BudgetReport.ts               # [25-BUDGET.md] Full budget consumption report
│   │
│   │  ─── AMB TYPES (USER-MANUAL--05, 26) ───
│   ├── ChoicePoint.ts                # [USER-MANUAL--05] Backtracking state
│   ├── AmbResult.ts                  # [USER-MANUAL--26] AMB computation result
│   ├── BacktrackState.ts             # [USER-MANUAL--05] Backtrack stack state
│   │
│   │  ─── STREAM TYPES (USER-MANUAL--07, 23) ───
│   ├── LazyStream.ts                 # [USER-MANUAL--07] Stream type definition
│   ├── StreamElement.ts              # [USER-MANUAL--23] Element or delay
│   │
│   │  ─── LOGIC TYPES (USER-MANUAL--27) ───
│   ├── SemanticFact.ts               # [USER-MANUAL--27] Natural language fact
│   ├── SemanticRule.ts               # [USER-MANUAL--27] Condition → conclusion rule
│   ├── BindingFrame.ts               # [USER-MANUAL--27] Variable bindings
│   ├── QueryResult.ts                # [USER-MANUAL--27] Query result with confidence
│   │
│   │  ─── EXPERT TYPES (29-EXPERTS.md) ───
│   ├── ExpertRole.ts                 # [29-EXPERTS.md] Expert role definition
│   ├── OutputMode.ts                 # [29-EXPERTS.md] REPORT|PLAN|PROGRAM|ANALYSIS
│   ├── TaskEnvelope.ts               # [29-EXPERTS.md] Per-call context
│   ├── IntentResult.ts               # [29-EXPERTS.md] Compiled intent result
│   │
│   │  ─── MACRO TYPES (11-MACROS.md) ───
│   ├── Macro.ts                      # [11-MACROS.md] Macro definition
│   ├── MacroExpansion.ts             # [11-MACROS.md] Expansion result
│   │
│   │  ─── CONCURRENCY TYPES (12-CONCURRENCY.md) ───
│   ├── Fiber.ts                      # [12-CONCURRENCY.md] Fiber handle
│   ├── Mutex.ts                      # [12-CONCURRENCY.md] Mutex handle
│   ├── Channel.ts                    # [12-CONCURRENCY.md] Channel handle
│   └── Actor.ts                      # [12-CONCURRENCY.md] Actor handle
│
├── events/                           # ALL event types (26+ events)
│   ├── index.ts                      # Re-exports all events
│   ├── RuntimeEvent.ts               # RuntimeEvent union, Unsubscribe, handlers
│   ├── RuntimeEventEmitter.ts        # Typed event emitter class
│   │
│   │  ─── CORE EVENTS ───
│   ├── StepEvent.ts                  # [08-PROTOCOL.md] Step event
│   ├── BeforeLLMEvent.ts             # [10-ORACLE.md] Before LLM call
│   ├── AfterLLMEvent.ts              # [10-ORACLE.md] After LLM response
│   ├── EffectEvent.ts                # [05-EFFECTS.md] Effect occurred
│   ├── BreakpointHitEvent.ts         # [07-DEBUG.md] Breakpoint triggered
│   ├── ErrorEvent.ts                 # [06-CONDITIONS.md] Error event
│   ├── DoneEvent.ts                  # [08-PROTOCOL.md] Evaluation complete
│   ├── SessionCheckpointEvent.ts     # [14-SESSION.md] Session saved
│   ├── OprReceiptEvent.ts            # [09-OPR.md] OPR receipt generated
│   ├── LLMToolCallEvent.ts           # [10-ORACLE.md] Tool call during LLM
│   │
│   │  ─── CONDITIONS/FACTS/FIXPOINT (06, 23, 24) ───
│   ├── ConditionEvent.ts             # [06-CONDITIONS.md] Condition signaled
│   ├── FactAssertedEvent.ts          # [23-FACTS.md] Fact asserted
│   ├── FixpointIterEvent.ts          # [24-FIXPOINT.md] Fixpoint iteration
│   ├── FixpointDoneEvent.ts          # [24-FIXPOINT.md] Fixpoint complete
│   │
│   │  ─── BUDGET/SECURITY (25, 21) ───
│   ├── BudgetWarningEvent.ts         # [25-BUDGET.md] Budget at 80%
│   ├── BudgetExceededEvent.ts        # [25-BUDGET.md] Budget exceeded
│   ├── SecurityEvent.ts              # [21-SECURITY.md] Security event
│   ├── TaskCompleteEvent.ts          # [12-CONCURRENCY.md] Async task done
│   │
│   │  ─── AMB EVENTS (USER-MANUAL--05, 26) ───
│   ├── AmbChooseEvent.ts             # [USER-MANUAL--05] Choice made
│   ├── AmbFailEvent.ts               # [USER-MANUAL--05] Backtracking
│   ├── AmbSolutionEvent.ts           # [USER-MANUAL--26] Solution found
│   │
│   │  ─── STREAM EVENTS (USER-MANUAL--07, 23) ───
│   ├── StreamForceEvent.ts           # [USER-MANUAL--07] Element forced
│   │
│   │  ─── LOGIC EVENTS (USER-MANUAL--27) ───
│   ├── FactQueriedEvent.ts           # [USER-MANUAL--27] Query executed
│   ├── RuleAppliedEvent.ts           # [USER-MANUAL--27] Rule used
│   │
│   │  ─── EXPERT EVENTS (29-EXPERTS.md) ───
│   ├── IntentCompiledEvent.ts        # [29-EXPERTS.md] Intent compiled
│   ├── ExpertResultEvent.ts          # [29-EXPERTS.md] Expert returned
│   │
│   │  ─── MACRO EVENTS (11-MACROS.md) ───
│   ├── MacroExpandEvent.ts           # [11-MACROS.md] Macro expanded
│   │
│   │  ─── CONCURRENCY EVENTS (12-CONCURRENCY.md) ───
│   └── DeadlockDetectedEvent.ts      # [12-CONCURRENCY.md] Deadlock detected
│
├── providers/                        # Pluggable backend interfaces
│   ├── index.ts                      # Re-exports all providers
│   │
│   │  ─── CORE PROVIDERS ───
│   ├── StateProvider.ts              # [01-EVAL.md] StateProvider interface
│   ├── MemoryStateProvider.ts        # In-memory Store
│   ├── FilesystemStateProvider.ts    # Filesystem Store
│   ├── SessionProvider.ts            # [14-SESSION.md] SessionProvider interface
│   ├── FilesystemSessionProvider.ts  # Filesystem sessions
│   ├── SnapshotProvider.ts           # [07-DEBUG.md] SnapshotProvider interface
│   ├── MemorySnapshotProvider.ts     # In-memory snapshots
│   ├── FilesystemSnapshotProvider.ts # Filesystem snapshots
│   ├── ReceiptProvider.ts            # [09-OPR.md] ReceiptProvider interface
│   ├── InMemoryReceiptProvider.ts    # In-memory receipts
│   ├── FilesystemReceiptProvider.ts  # Filesystem receipts
│   ├── TraceProvider.ts              # [07-DEBUG.md] TraceProvider interface
│   ├── MemoryTraceProvider.ts        # In-memory traces
│   ├── StreamingTraceProvider.ts     # Streaming traces
│   ├── EffectHandlerRegistry.ts      # [05-EFFECTS.md] Effect handler registry
│   ├── EffectHandler.ts              # Effect handler type
│   ├── GovernanceProvider.ts         # [21-SECURITY.md] GovernanceProvider interface
│   ├── LocalGovernanceProvider.ts    # Local governance
│   ├── RemoteGovernanceProvider.ts   # Remote governance
│   │
│   │  ─── GAP ANALYSIS PROVIDERS ───
│   ├── ArtifactProvider.ts           # [26-ARTIFACTS.md] Content-addressed cache
│   ├── MemoryArtifactProvider.ts     # In-memory artifacts
│   ├── FilesystemArtifactProvider.ts # Filesystem artifacts
│   ├── ProvenanceProvider.ts         # [22-PROVENANCE.md] Evidence storage
│   ├── MemoryProvenanceProvider.ts   # In-memory provenance
│   ├── LogicProvider.ts              # [USER-MANUAL--27] Logic facts/rules
│   ├── MemoryLogicProvider.ts        # In-memory logic
│   ├── ExpertProvider.ts             # [29-EXPERTS.md] Expert role registry
│   ├── DefaultExpertProvider.ts      # Default expert roles
│   ├── StreamProvider.ts             # [USER-MANUAL--07] Stream storage
│   ├── MemoryStreamProvider.ts       # In-memory streams
│   ├── SecurityProvider.ts           # [21-SECURITY.md] Security policy
│   ├── LocalSecurityProvider.ts      # Local security
│   │
│   │  ─── MOCK PROVIDERS (for testing) ───
│   └── mocks/
│       ├── index.ts
│       ├── MockStateProvider.ts
│       ├── MockSnapshotProvider.ts
│       ├── MockReceiptProvider.ts
│       ├── MockTraceProvider.ts
│       ├── MockGovernanceProvider.ts
│       ├── MockEffectHandlerRegistry.ts
│       ├── MockEventEmitter.ts
│       ├── MockArtifactProvider.ts
│       ├── MockLogicProvider.ts
│       ├── MockExpertProvider.ts
│       └── MockStreamProvider.ts
│
├── subsystems/                       # ALL 24 SUBSYSTEMS
│   ├── index.ts                      # Re-exports all subsystems
│   │
│   │  ─── CORE (2) ───
│   ├── ExecutionEngine.ts            # [01-EVAL.md] Core eval/step/run
│   │
│   │  ─── DEBUGGING (6) ───
│   ├── DebugSubsystem.ts             # [07-DEBUG.md] debug.* methods
│   ├── BreakpointManager.ts          # [07-DEBUG.md] breakpoints.* methods
│   ├── SnapshotManager.ts            # [07-DEBUG.md] snapshots.* methods
│   ├── StateInspector.ts             # [01-EVAL.md] inspect.* methods
│   ├── HistoryManager.ts             # [07-DEBUG.md] history.* methods
│   ├── SessionManager.ts             # [14-SESSION.md] session.* methods
│   │
│   │  ─── DATA (2) ───
│   ├── ArtifactManager.ts            # [26-ARTIFACTS.md] artifacts.* methods
│   ├── FactsManager.ts               # [23-FACTS.md] facts.* methods
│   │
│   │  ─── CONTROL (3) ───
│   ├── ConditionsManager.ts          # [06-CONDITIONS.md] conditions.* methods
│   ├── FixpointManager.ts            # [24-FIXPOINT.md] fixpoint.* methods
│   ├── TransactionManager.ts         # [27-OUTCOMES.md] transaction.* methods
│   │
│   │  ─── GOVERNANCE (3) ───
│   ├── ProvenanceManager.ts          # [22-PROVENANCE.md] provenance.* methods
│   ├── SecurityManager.ts            # [21-SECURITY.md] security.* methods
│   ├── BudgetManager.ts              # [25-BUDGET.md] budget.* methods
│   │
│   │  ─── COMMUNICATION (2) ───
│   ├── ProtocolServer.ts             # [08-PROTOCOL.md] protocol.* methods
│   ├── ConcurrencyManager.ts         # [12-CONCURRENCY.md] concurrency.* methods
│   │
│   │  ─── LLM INTEGRATION (3) ───
│   ├── LLMIntegration.ts             # [10-ORACLE.md] llm.* methods
│   ├── OprIntegration.ts             # [09-OPR.md] opr.* methods
│   ├── ExpertManager.ts              # [29-EXPERTS.md] experts.* methods
│   │
│   │  ─── SEMANTIC (4) ───
│   ├── AmbManager.ts                 # [USER-MANUAL--05,26] amb.* methods
│   ├── StreamsManager.ts             # [USER-MANUAL--07,23] streams.* methods
│   ├── LogicManager.ts               # [USER-MANUAL--27] logic.* methods
│   └── MacroManager.ts               # [11-MACROS.md] macros.* methods
│
├── protocol/                         # nREPL-style protocol (08-PROTOCOL.md)
│   ├── index.ts
│   ├── Middleware.ts
│   ├── operations/
│   │   ├── eval.ts
│   │   ├── info.ts
│   │   ├── complete.ts
│   │   ├── interrupt.ts
│   │   ├── snapshot.ts
│   │   └── restore.ts
│   └── transports/
│       ├── StdioTransport.ts
│       ├── WebSocketTransport.ts
│       └── HttpTransport.ts
│
└── internal/                         # Internal utilities (NOT exported)
    ├── index.ts
    │
    │  ─── CORE UTILITIES ───
    ├── StateCloner.ts                # Deep clone machine state
    ├── ControlFormatter.ts           # Format control for display
    ├── FrameFormatter.ts             # Format stack frames
    ├── ValueSerializer.ts            # Serialize Val to JSON
    ├── IdGenerator.ts                # Generate unique IDs
    │
    │  ─── TRANSACTION SUPPORT (27-OUTCOMES.md) ───
    ├── StagedWorld.ts                # Staged world for transactions
    │
    │  ─── AMB SUPPORT (USER-MANUAL--05, 26) ───
    ├── BacktrackStack.ts             # Backtracking infrastructure
    ├── ChoicePoint.ts                # Choice point management
    │
    │  ─── STREAM SUPPORT (USER-MANUAL--07, 23) ───
    ├── LazyStream.ts                 # Lazy stream infrastructure
    ├── StreamForcer.ts               # Stream element forcer
    │
    │  ─── LOGIC SUPPORT (USER-MANUAL--27) ───
    ├── SemanticDatabase.ts           # Semantic fact storage
    ├── SemanticUnifier.ts            # Semantic pattern matching
    ├── BindingFrame.ts               # Variable binding frames
    │
    │  ─── EXPERT SUPPORT (29-EXPERTS.md) ───
    ├── ToolContract.ts               # Layer A: Tool contract
    ├── RoleOverlay.ts                # Layer B: Role overlay
    ├── TaskEnvelope.ts               # Layer C: Task envelope
    │
    │  ─── MACRO SUPPORT (11-MACROS.md) ───
    ├── MacroExpander.ts              # Macro expansion engine
    ├── Quasiquoter.ts                # Quasiquote processing
    │
    │  ─── CONCURRENCY SUPPORT (12-CONCURRENCY.md) ───
    ├── Actor.ts                      # Actor infrastructure
    ├── Channel.ts                    # Channel infrastructure
    ├── Fiber.ts                      # Fiber infrastructure
    ├── Mutex.ts                      # Mutex infrastructure
    ├── Serializer.ts                 # Serializer pattern
    └── Singleflight.ts               # Singleflight pattern
```

**File Naming Conventions:**
- One class/interface per file
- PascalCase filenames matching class/interface names
- `index.ts` in each folder for re-exports
- Interfaces and their default implementations in same folder
- Internal utilities not exposed in public API

---

### REPL Command → Runtime Method Mapping

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                    REPL COMMAND → RUNTIME METHOD MAPPING                     │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  REPL Command              →  Runtime Method                                 │
│  ════════════════════════════════════════════════════════════════════════   │
│                                                                              │
│  (expr)                    →  runtime.eval(expr)                             │
│  :loadfile <path>          →  runtime.loadFile(path)                         │
│                                                                              │
│  :debug (expr)             →  runtime.debug.load(expr)                       │
│  :step [N]                 →  runtime.debug.step(N)                          │
│  :run                      →  runtime.debug.run()                            │
│  :stop                     →  runtime.debug.stop()                           │
│  :goto <step>              →  runtime.debug.goto(step)                       │
│  :trace                    →  runtime.debug.getTrace()                       │
│  :state                    →  runtime.debug.getState()                       │
│                                                                              │
│  :break step <N>           →  runtime.breakpoints.add({type:'step', ...})    │
│  :break expr <tag>         →  runtime.breakpoints.add({type:'expr', ...})    │
│  :break effect <op>        →  runtime.breakpoints.add({type:'effect', ...})  │
│  :breaks                   →  runtime.breakpoints.list()                     │
│  :delbreak <id>            →  runtime.breakpoints.remove(id)                 │
│  :toggle <id>              →  runtime.breakpoints.toggle(id)                 │
│                                                                              │
│  :save <name>              →  runtime.snapshots.save(name)                   │
│  :restore <name>           →  runtime.snapshots.restore(name)                │
│  :snapshots                →  runtime.snapshots.list()                       │
│  :export <name> <file>     →  runtime.snapshots.export(name, file)           │
│                                                                              │
│  :session save <name>      →  runtime.session.save(name)                     │
│  :session load <name>      →  runtime.session.load(name)                     │
│  :session fork <name>      →  runtime.session.fork(name)                     │
│  :session list             →  runtime.session.list()                         │
│  :session goto <cp>        →  runtime.session.goto(cp)                       │
│  :session trace            →  runtime.session.getTrace()                     │
│  :session checkpoints      →  runtime.session.getCheckpoints()               │
│                                                                              │
│  :env                      →  runtime.inspect.env()                          │
│  :env <name>               →  runtime.inspect.env(name)                      │
│  :defs                     →  runtime.inspect.defs()                         │
│  :stack                    →  runtime.inspect.stack()                        │
│  :frame <n>                →  runtime.inspect.frame(n)                       │
│  :control                  →  runtime.inspect.control()                      │
│                                                                              │
│  :back [N]                 →  runtime.history.back(N)                        │
│  :history [N]              →  runtime.history.list(N)                        │
│  :record on/off            →  runtime.history.setRecording(bool)             │
│  :dump <file>              →  runtime.history.dump(file)                     │
│  :replay <file>            →  runtime.history.replay(file)                   │
│                                                                              │
│  :ask <question>           →  runtime.llm.ask(question)                      │
│  :traces                   →  runtime.llm.getTraces()                        │
│  :trace <id>               →  runtime.llm.getTrace(id)                       │
│                                                                              │
│  :opr-list                 →  runtime.opr.list()                             │
│  :opr-run <k> <json>       →  runtime.opr.run(k, json)                       │
│  :opr-receipts             →  runtime.opr.getReceipts()                      │
│  :opr-verify               →  runtime.opr.verify()                           │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Solution Architecture

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              CONSUMERS                                       │
│  (REPL, HTTP Server, Tests, External Tools)                                 │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                          import { createRuntime }
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                                                                              │
│   src/runtime/index.ts  ◄──── THE PACKAGE BOUNDARY                          │
│   ════════════════════                                                       │
│                                                                              │
│   OmegaRuntime {                                                             │
│     eval(), loadFile()                                                       │
│     debug.*, breakpoints.*, snapshots.*                                      │
│     session.*, inspect.*, history.*                                          │
│     llm.*, opr.*                                                             │
│     on(event, handler)  ◄── HOOKS                                            │
│   }                                                                          │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                          uses providers
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                         PLUGGABLE PROVIDERS                                  │
│  StateProvider, SessionProvider, SnapshotProvider, ReceiptProvider,         │
│  TraceProvider, EffectHandlerRegistry, GovernanceProvider                   │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                          uses internally
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                         src/core/ (unchanged)                                │
│  eval/, effects/, oracle/, opr/, governance/, pipeline/                     │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Complete Type Definitions

### RuntimeConfig

```typescript
interface RuntimeConfig {
  // ─── Pluggable Providers ───
  providers?: {
    state?: StateProvider;          // Default: MemoryStateProvider
    session?: SessionProvider;      // Default: FilesystemSessionProvider
    snapshots?: SnapshotProvider;   // Default: MemorySnapshotProvider
    receipts?: ReceiptProvider;     // Default: InMemoryReceiptProvider
    traces?: TraceProvider;         // Default: MemoryTraceProvider
    effects?: EffectHandlerRegistry;// Default: DefaultEffectRegistry
    governance?: GovernanceProvider;// Default: LocalGovernanceProvider
  };

  // ─── LLM Configuration ───
  llm?: {
    adapter?: LLMAdapter;           // OpenAI, Anthropic, Scripted
    model?: string;
    apiKey?: string;
  };

  // ─── OPR Configuration ───
  opr?: {
    adapter?: OprLLMAdapter;
    kernels?: KernelRegistry;
  };

  // ─── Options ───
  sessionDir?: string;              // For filesystem providers
  maxHistorySize?: number;          // Default: 1000
  recordTraces?: boolean;           // Default: true
}
```

### Provider Interfaces

```typescript
// ─── State Provider ───
interface StateProvider {
  createStore(): Store;
  loadStore(id: string): Promise<Store>;
  saveStore(id: string, store: Store): Promise<void>;
}

// ─── Session Provider ───
interface SessionProvider {
  list(): Promise<SessionInfo[]>;
  load(name: string): Promise<SessionData>;
  save(name: string, data: SessionData): Promise<void>;
  delete(name: string): Promise<void>;
  fork(source: string, target: string): Promise<void>;
}

// ─── Snapshot Provider ───
interface SnapshotProvider {
  list(): Promise<SnapshotInfo[]>;
  save(name: string, snapshot: MachineSnapshot): Promise<void>;
  load(name: string): Promise<MachineSnapshot>;
  delete(name: string): Promise<void>;
  export(name: string, path: string): Promise<void>;
}

// ─── Receipt Provider ───
interface ReceiptProvider {
  store(receipt: Receipt): Promise<void>;
  list(): Promise<Receipt[]>;
  get(id: string): Promise<Receipt | undefined>;
  verify(): Promise<VerifyResult>;
  clear(): Promise<void>;
}

// ─── Trace Provider ───
interface TraceProvider {
  record(entry: TraceEntry): void;
  list(start?: number, count?: number): TraceEntry[];
  get(step: number): TraceEntry | undefined;
  dump(path: string): Promise<void>;
  load(path: string): Promise<TraceEntry[]>;
  clear(): void;
}

// ─── Effect Handler Registry ───
interface EffectHandlerRegistry {
  register(op: string, handler: EffectHandler): void;
  get(op: string): EffectHandler | undefined;
  list(): string[];
  unregister(op: string): void;
}

type EffectHandler = (
  effect: Effect,
  context: EffectContext
) => Promise<EffectResult>;

// ─── Governance Provider ───
interface GovernanceProvider {
  getProfile(): Profile;
  getCaps(): CapSet;
  getBudget(): BudgetTracker;
  checkCap(cap: string): boolean;
  consumeBudget(type: string, amount: number): boolean;
  reset(): void;
}
```

### Main Runtime Interface

```typescript
interface OmegaRuntime {
  // ═══════════════════════════════════════════════════════════════════════
  // Core Execution
  // ═══════════════════════════════════════════════════════════════════════

  /**
   * Evaluate code and return result.
   * Emits: 'step', 'effect', 'before-llm', 'after-llm', 'done', 'error'
   */
  eval(code: string): Promise<EvalResult>;

  /**
   * Load and evaluate code from file.
   */
  loadFile(path: string): Promise<void>;

  /**
   * Get accumulated definitions.
   */
  getDefs(): string[];

  // ═══════════════════════════════════════════════════════════════════════
  // Debug Subsystem
  // ═══════════════════════════════════════════════════════════════════════

  debug: {
    /**
     * Load code into debugger (paused at start).
     */
    load(code: string): void;

    /**
     * Step N times (default 1).
     * Emits: 'step', 'breakpoint-hit', 'effect', 'done'
     */
    step(n?: number): StepResult;

    /**
     * Run until breakpoint, effect, or completion.
     * Emits: 'step', 'breakpoint-hit', 'effect', 'done'
     */
    run(maxSteps?: number): RunResult;

    /**
     * Stop execution.
     */
    stop(): void;

    /**
     * Jump to step N in trace (time travel).
     */
    goto(step: number): void;

    /**
     * Get current debug state.
     */
    getState(): DebugState | null;

    /**
     * Get execution trace.
     */
    getTrace(start?: number, count?: number): TraceEntry[];

    /**
     * Check if in debug mode.
     */
    isActive(): boolean;
  };

  // ═══════════════════════════════════════════════════════════════════════
  // Breakpoints
  // ═══════════════════════════════════════════════════════════════════════

  breakpoints: {
    /**
     * Add a breakpoint. Returns ID.
     */
    add(spec: BreakpointSpec): string;

    /**
     * Remove breakpoint by ID.
     */
    remove(id: string): void;

    /**
     * List all breakpoints.
     */
    list(): Breakpoint[];

    /**
     * Toggle breakpoint enabled/disabled.
     */
    toggle(id: string): void;

    /**
     * Clear all breakpoints.
     */
    clear(): void;
  };

  // ═══════════════════════════════════════════════════════════════════════
  // Snapshots
  // ═══════════════════════════════════════════════════════════════════════

  snapshots: {
    /**
     * Save current state as snapshot.
     */
    save(name: string): Promise<void>;

    /**
     * Restore snapshot (replaces current state).
     */
    restore(name: string): Promise<void>;

    /**
     * List all snapshots.
     */
    list(): Promise<SnapshotInfo[]>;

    /**
     * Delete snapshot.
     */
    delete(name: string): Promise<void>;

    /**
     * Export snapshot to file.
     */
    export(name: string, path: string): Promise<void>;
  };

  // ═══════════════════════════════════════════════════════════════════════
  // Session Persistence
  // ═══════════════════════════════════════════════════════════════════════

  session: {
    /**
     * Save current session.
     * Emits: 'session-checkpoint'
     */
    save(name: string): Promise<void>;

    /**
     * Load session (replaces current state).
     */
    load(name: string): Promise<void>;

    /**
     * Fork session from current point.
     */
    fork(name: string): Promise<void>;

    /**
     * List all sessions.
     */
    list(): Promise<SessionInfo[]>;

    /**
     * Jump to checkpoint or event.
     */
    goto(checkpoint: string | number): Promise<void>;

    /**
     * Get session trace.
     */
    getTrace(): SessionTrace;

    /**
     * Get all checkpoints.
     */
    getCheckpoints(): Checkpoint[];

    /**
     * Delete session.
     */
    delete(name: string): Promise<void>;
  };

  // ═══════════════════════════════════════════════════════════════════════
  // State Inspection
  // ═══════════════════════════════════════════════════════════════════════

  inspect: {
    /**
     * Get environment bindings.
     * If name provided, get specific binding.
     */
    env(name?: string): EnvBindings | Value | undefined;

    /**
     * Get call stack.
     */
    stack(): StackFrame[];

    /**
     * Get specific stack frame.
     */
    frame(n: number): FrameDetail | undefined;

    /**
     * Get current control (expr or value).
     */
    control(): ControlInfo;

    /**
     * Get all definitions.
     */
    defs(): Definition[];

    /**
     * Get full machine snapshot.
     */
    snapshot(): MachineSnapshot;
  };

  // ═══════════════════════════════════════════════════════════════════════
  // History & Time Travel
  // ═══════════════════════════════════════════════════════════════════════

  history: {
    /**
     * Go back N steps.
     */
    back(n?: number): void;

    /**
     * List recent history entries.
     */
    list(n?: number): HistoryEntry[];

    /**
     * Enable/disable trace recording.
     */
    setRecording(enabled: boolean): void;

    /**
     * Check if recording.
     */
    isRecording(): boolean;

    /**
     * Dump trace to file.
     */
    dump(path: string): Promise<void>;

    /**
     * Load and replay trace from file.
     */
    replay(path: string): Promise<void>;

    /**
     * Clear history.
     */
    clear(): void;
  };

  // ═══════════════════════════════════════════════════════════════════════
  // LLM / Agent
  // ═══════════════════════════════════════════════════════════════════════

  llm: {
    /**
     * Configure LLM adapter.
     */
    configure(adapter: LLMAdapter): void;

    /**
     * Check if LLM is configured.
     */
    isConfigured(): boolean;

    /**
     * Ask LLM with tool-calling agent loop.
     * Emits: 'before-llm', 'after-llm', 'llm-tool-call'
     */
    ask(question: string): Promise<AgentResult>;

    /**
     * Get all LLM interaction traces.
     */
    getTraces(): LLMTraceSummary[];

    /**
     * Get specific trace detail.
     */
    getTrace(id: string): LLMTraceDetail | undefined;

    /**
     * Clear traces.
     */
    clearTraces(): void;
  };

  // ═══════════════════════════════════════════════════════════════════════
  // OPR Kernels
  // ═══════════════════════════════════════════════════════════════════════

  opr: {
    /**
     * List available kernels.
     */
    list(): KernelInfo[];

    /**
     * Run kernel with program.
     * Emits: 'opr-receipt', 'before-llm', 'after-llm'
     */
    run(kernel: string, program: unknown): Promise<KernelResult>;

    /**
     * Get receipt chain for current session.
     */
    getReceipts(): Receipt[];

    /**
     * Verify receipt chain integrity.
     */
    verify(): VerifyResult;

    /**
     * Configure OPR adapter.
     */
    configure(adapter: OprLLMAdapter): void;
  };

  // ═══════════════════════════════════════════════════════════════════════
  // Event Hooks (THE KEY ADDITION)
  // ═══════════════════════════════════════════════════════════════════════

  /**
   * Subscribe to runtime events.
   * Returns unsubscribe function.
   */
  on<E extends RuntimeEvent>(
    event: E,
    handler: RuntimeEventHandler<E>
  ): Unsubscribe;

  /**
   * Subscribe to event (fires once, then auto-unsubscribes).
   */
  once<E extends RuntimeEvent>(
    event: E,
    handler: RuntimeEventHandler<E>
  ): Unsubscribe;

  /**
   * Unsubscribe from event.
   */
  off<E extends RuntimeEvent>(
    event: E,
    handler: RuntimeEventHandler<E>
  ): void;

  // ═══════════════════════════════════════════════════════════════════════
  // Lifecycle (HARMONY FIX H6: Complete lifecycle management)
  // ═══════════════════════════════════════════════════════════════════════

  /**
   * Reset runtime to initial state.
   */
  reset(): void;

  /**
   * Get runtime configuration.
   */
  getConfig(): RuntimeConfig;

  /**
   * Gracefully shutdown the runtime:
   * 1. Cancel pending operations
   * 2. Close protocol connections
   * 3. Flush pending traces
   * 4. Dispose all subsystems
   * 5. Release provider resources
   */
  dispose(): Promise<void>;

  /**
   * Check if runtime has been disposed.
   * After disposal, all other methods will throw.
   */
  isDisposed(): boolean;

  /**
   * Register cleanup handler for process exit.
   * Handler is called during dispose().
   * Returns unsubscribe function.
   */
  onShutdown(handler: () => Promise<void>): Unsubscribe;
}

// ─── Factory Function ───
export function createRuntime(config?: RuntimeConfig): OmegaRuntime;
```

### Event Types

```typescript
type RuntimeEvent =
  | 'step'
  | 'before-llm'
  | 'after-llm'
  | 'effect'
  | 'breakpoint-hit'
  | 'error'
  | 'done'
  | 'session-checkpoint'
  | 'opr-receipt'
  | 'llm-tool-call';

type Unsubscribe = () => void;

// Event payloads
interface StepEvent {
  step: number;
  control: ControlInfo;
  stackDepth: number;
  timestamp: number;
}

interface BeforeLLMEvent {
  requestId: string;
  type: 'oracle' | 'opr' | 'agent';
  kernel?: string;
  prompt: string;
  model?: string;
}

interface AfterLLMEvent {
  requestId: string;
  type: 'oracle' | 'opr' | 'agent';
  response: unknown;
  durationMs: number;
  tokens?: { input: number; output: number };
}

interface EffectEvent {
  op: string;
  args: unknown[];
  timestamp: number;
}

interface BreakpointHitEvent {
  breakpoint: Breakpoint;
  step: number;
  state: DebugState;
}

interface ErrorEvent {
  error: Error;
  context?: string;
  recoverable: boolean;
}

interface DoneEvent {
  result: Value;
  steps: number;
  durationMs: number;
}

interface SessionCheckpointEvent {
  name: string;
  step: number;
  timestamp: number;
}

interface OprReceiptEvent {
  receipt: Receipt;
  kernel: string;
}

interface LLMToolCallEvent {
  traceId: string;
  toolName: string;
  args: Record<string, unknown>;
  result?: string;
}

// Handler type
type RuntimeEventHandler<E extends RuntimeEvent> =
  E extends 'step' ? (event: StepEvent) => void :
  E extends 'before-llm' ? (event: BeforeLLMEvent) => void :
  E extends 'after-llm' ? (event: AfterLLMEvent) => void :
  E extends 'effect' ? (event: EffectEvent) => void :
  E extends 'breakpoint-hit' ? (event: BreakpointHitEvent) => void :
  E extends 'error' ? (event: ErrorEvent) => void :
  E extends 'done' ? (event: DoneEvent) => void :
  E extends 'session-checkpoint' ? (event: SessionCheckpointEvent) => void :
  E extends 'opr-receipt' ? (event: OprReceiptEvent) => void :
  E extends 'llm-tool-call' ? (event: LLMToolCallEvent) => void :
  never;
```

---

## Usage Examples

### Basic Usage

```typescript
import { createRuntime } from 'omegallm/runtime';

const runtime = createRuntime();

// Evaluate code
const result = await runtime.eval('(+ 1 2)');
console.log(result.value); // 3

// Load file
await runtime.loadFile('program.omega');
```

### With Hooks

```typescript
const runtime = createRuntime();

// Observe all steps
runtime.on('step', (event) => {
  console.log(`Step ${event.step}: ${event.control.summary}`);
});

// Intercept LLM calls
runtime.on('before-llm', (event) => {
  console.log(`LLM request: ${event.prompt.slice(0, 100)}...`);
});

runtime.on('after-llm', (event) => {
  console.log(`LLM response in ${event.durationMs}ms`);
});

// Handle errors
runtime.on('error', (event) => {
  console.error(`Error: ${event.error.message}`);
});

await runtime.eval('(infer.step.classify "hello world")');
```

### Debugging

```typescript
const runtime = createRuntime();

// Add breakpoint
runtime.breakpoints.add({ type: 'step', condition: 10 });
runtime.breakpoints.add({ type: 'effect', condition: 'infer.op' });

// Load into debugger
runtime.debug.load('(factorial 5)');

// Step through
runtime.on('breakpoint-hit', (event) => {
  console.log(`Hit breakpoint at step ${event.step}`);
});

while (runtime.debug.isActive()) {
  const result = runtime.debug.step();
  if (result.outcome === 'done') break;
  if (result.outcome === 'breakpoint') {
    console.log('Paused at breakpoint');
    // Inspect state
    console.log(runtime.inspect.stack());
    // Continue
    runtime.debug.run();
  }
}
```

### Custom Providers

```typescript
import { createRuntime } from 'omegallm/runtime';
import { S3SnapshotProvider } from './my-providers';

const runtime = createRuntime({
  providers: {
    snapshots: new S3SnapshotProvider({
      bucket: 'my-snapshots',
      region: 'us-west-2',
    }),
  },
});

// Snapshots now go to S3
await runtime.snapshots.save('checkpoint-1');
```

### HTTP Server Using Runtime

```typescript
import express from 'express';
import { createRuntime } from 'omegallm/runtime';

const app = express();
const runtime = createRuntime();

// Forward runtime events to WebSocket clients
runtime.on('step', (event) => {
  wss.clients.forEach(client => {
    client.send(JSON.stringify({ type: 'step', ...event }));
  });
});

app.post('/eval', async (req, res) => {
  const result = await runtime.eval(req.body.code);
  res.json(result);
});

app.post('/debug/step', (req, res) => {
  const result = runtime.debug.step(req.body.count);
  res.json(result);
});
```

---

## Class Diagram

**COMPLETE: All 24 subsystems with source document references.**

```
┌─────────────────────────────────────────────────────────────────────────────────────────────────┐
│                              CLASS DIAGRAM - COMPLETE 24 SUBSYSTEMS                              │
│                           OmegaRuntime and Component Relationships                               │
│           Each subsystem references its source document for implementation review                │
└─────────────────────────────────────────────────────────────────────────────────────────────────┘

                                   «factory»
                               ┌───────────────────┐
                               │  createRuntime()  │
                               │ ─────────────────  │
                               │ + config: Config  │
                               │ → OmegaRuntime    │
                               └─────────┬─────────┘
                                         │ creates
                                         ▼
┌────────────────────────────────────────────────────────────────────────────────────────────────┐
│                                     «facade»                                                    │
│                          OmegaRuntime (24 subsystems)                                           │
├────────────────────────────────────────────────────────────────────────────────────────────────┤
│  - config: RuntimeConfig                                                                        │
│  - events: RuntimeEventEmitter                                                                  │
│  ─── CORE (1) ───────────────────────────────────────────────────────────────────────────────  │
│  - execution: ExecutionEngine         [01-EVAL.md]                                              │
│  ─── DEBUGGING (6) ──────────────────────────────────────────────────────────────────────────  │
│  - debug: DebugSubsystem              [07-DEBUG.md]                                             │
│  - breakpoints: BreakpointManager     [07-DEBUG.md]                                             │
│  - snapshots: SnapshotManager         [07-DEBUG.md]                                             │
│  - inspect: StateInspector            [01-EVAL.md]                                              │
│  - history: HistoryManager            [07-DEBUG.md]                                             │
│  - session: SessionManager            [14-SESSION.md]                                           │
│  ─── DATA (2) ───────────────────────────────────────────────────────────────────────────────  │
│  - artifacts: ArtifactManager         [26-ARTIFACTS.md]                                         │
│  - facts: FactsManager                [23-FACTS.md]                                             │
│  ─── CONTROL (3) ────────────────────────────────────────────────────────────────────────────  │
│  - conditions: ConditionsManager      [06-CONDITIONS.md]                                        │
│  - fixpoint: FixpointManager          [24-FIXPOINT.md]                                          │
│  - transaction: TransactionManager    [27-OUTCOMES.md]                                          │
│  ─── GOVERNANCE (3) ─────────────────────────────────────────────────────────────────────────  │
│  - provenance: ProvenanceManager      [22-PROVENANCE.md]                                        │
│  - security: SecurityManager          [21-SECURITY.md]                                          │
│  - budget: BudgetManager              [25-BUDGET.md]                                            │
│  ─── COMMUNICATION (2) ──────────────────────────────────────────────────────────────────────  │
│  - protocol: ProtocolServer           [08-PROTOCOL.md]                                          │
│  - concurrency: ConcurrencyManager    [12-CONCURRENCY.md]                                       │
│  ─── LLM (3) ────────────────────────────────────────────────────────────────────────────────  │
│  - llm: LLMIntegration                [10-ORACLE.md]                                            │
│  - opr: OprIntegration                [09-OPR.md]                                               │
│  - experts: ExpertManager             [29-EXPERTS.md]                                           │
│  ─── SEMANTIC (4) ───────────────────────────────────────────────────────────────────────────  │
│  - amb: AmbManager                    [USER-MANUAL--05,26]                                      │
│  - streams: StreamsManager            [USER-MANUAL--07,23]                                      │
│  - logic: LogicManager                [USER-MANUAL--27]                                         │
│  - macros: MacroManager               [11-MACROS.md]                                            │
├────────────────────────────────────────────────────────────────────────────────────────────────┤
│  + eval(code): Promise<Outcome>   + on(event, handler): Unsubscribe                            │
│  + loadFile(path): Promise<void>  + once(event, handler): Unsubscribe                          │
└────────────────────────────────────────────────────────────────────────────────────────────────┘
         │
         │ composes
         ▼
┌─ SUBSYSTEMS (24 total) ───────────────────────────────────────────────────────────────────────┐
│                                                                                                │
│  ═══ CORE (1) [01-EVAL.md] ═══════════════════════════════════════════════════════════════   │
│  ┌──────────────────────┐                                                                     │
│  │   ExecutionEngine    │  CESK machine wrapper - eval/step/run                               │
│  ├──────────────────────┤                                                                     │
│  │ + eval(): Outcome    │                                                                     │
│  │ + step(): StepResult │                                                                     │
│  │ + run(): RunResult   │                                                                     │
│  └──────────────────────┘                                                                     │
│                                                                                                │
│  ═══ DEBUGGING (6) [07-DEBUG.md, 14-SESSION.md] ══════════════════════════════════════════   │
│  ┌──────────────────────┐  ┌──────────────────────┐  ┌──────────────────────┐                │
│  │   DebugSubsystem     │  │  BreakpointManager   │  │   SnapshotManager    │                │
│  ├──────────────────────┤  ├──────────────────────┤  ├──────────────────────┤                │
│  │ + load(), step()     │  │ + add(), remove()    │  │ + save(), restore()  │                │
│  │ + run(), goto()      │  │ + check(), toggle()  │  │ + saveImage()        │                │
│  └──────────────────────┘  └──────────────────────┘  └──────────────────────┘                │
│  ┌──────────────────────┐  ┌──────────────────────┐  ┌──────────────────────┐                │
│  │   StateInspector     │  │   HistoryManager     │  │   SessionManager     │                │
│  ├──────────────────────┤  ├──────────────────────┤  ├──────────────────────┤                │
│  │ + env(), stack()     │  │ + back(), list()     │  │ + save(), load()     │                │
│  │ + frame(), defs()    │  │ + dump(), replay()   │  │ + fork(), goto()     │                │
│  └──────────────────────┘  └──────────────────────┘  └──────────────────────┘                │
│                                                                                                │
│  ═══ DATA (2) [26-ARTIFACTS.md, 23-FACTS.md] ═════════════════════════════════════════════   │
│  ┌──────────────────────┐  ┌──────────────────────┐                                          │
│  │   ArtifactManager    │  │    FactsManager      │                                          │
│  ├──────────────────────┤  ├──────────────────────┤                                          │
│  │ + get(), put()       │  │ + all(), has()       │                                          │
│  │ + stats(), clear()   │  │ + count(), query()   │                                          │
│  └──────────────────────┘  └──────────────────────┘                                          │
│                                                                                                │
│  ═══ CONTROL (3) [06-CONDITIONS.md, 24-FIXPOINT.md, 27-OUTCOMES.md] ══════════════════════   │
│  ┌──────────────────────┐  ┌──────────────────────┐  ┌──────────────────────┐                │
│  │  ConditionsManager   │  │   FixpointManager    │  │ TransactionManager   │                │
│  ├──────────────────────┤  ├──────────────────────┤  ├──────────────────────┤                │
│  │ + signal(), bind()   │  │ + run(), getState()  │  │ + begin(), propose() │                │
│  │ + restart()          │  │ + signature()        │  │ + commit(), rollback()│               │
│  └──────────────────────┘  └──────────────────────┘  └──────────────────────┘                │
│                                                                                                │
│  ═══ GOVERNANCE (3) [22-PROVENANCE.md, 21-SECURITY.md, 25-BUDGET.md] ═════════════════════   │
│  ┌──────────────────────┐  ┌──────────────────────┐  ┌──────────────────────┐                │
│  │  ProvenanceManager   │  │   SecurityManager    │  │    BudgetManager     │                │
│  ├──────────────────────┤  ├──────────────────────┤  ├──────────────────────┤                │
│  │ + capture(), verify()│  │ + checkCap()         │  │ + remaining()        │                │
│  │ + isStale(), graph() │  │ + validateLLM()      │  │ + consume()          │                │
│  └──────────────────────┘  │ + getAuditLog()      │  │ + report()           │                │
│                            └──────────────────────┘  └──────────────────────┘                │
│                                                                                                │
│  ═══ COMMUNICATION (2) [08-PROTOCOL.md, 12-CONCURRENCY.md] ═══════════════════════════════   │
│  ┌──────────────────────┐  ┌──────────────────────┐                                          │
│  │   ProtocolServer     │  │ ConcurrencyManager   │                                          │
│  ├──────────────────────┤  ├──────────────────────┤                                          │
│  │ + handle(), listen() │  │ + spawn(), send()    │                                          │
│  │ + use(middleware)    │  │ + chan(), awaitAll() │                                          │
│  └──────────────────────┘  │ + fiberSpawn()       │                                          │
│                            └──────────────────────┘                                          │
│                                                                                                │
│  ═══ LLM (3) [10-ORACLE.md, 09-OPR.md, 29-EXPERTS.md] ════════════════════════════════════   │
│  ┌──────────────────────┐  ┌──────────────────────┐  ┌──────────────────────┐                │
│  │   LLMIntegration     │  │   OprIntegration     │  │   ExpertManager      │                │
│  ├──────────────────────┤  ├──────────────────────┤  ├──────────────────────┤                │
│  │ + ask(), configure() │  │ + list(), run()      │  │ + registerRole()     │                │
│  │ + getTraces()        │  │ + verify()           │  │ + compileIntent()    │                │
│  └──────────────────────┘  └──────────────────────┘  │ + runParallel()      │                │
│                                                      └──────────────────────┘                │
│                                                                                                │
│  ═══ SEMANTIC (4) [USER-MANUAL--05,07,23,26,27, 11-MACROS.md] ════════════════════════════   │
│  ┌──────────────────────┐  ┌──────────────────────┐  ┌──────────────────────┐                │
│  │     AmbManager       │  │   StreamsManager     │  │    LogicManager      │                │
│  ├──────────────────────┤  ├──────────────────────┤  ├──────────────────────┤                │
│  │ + choose(), fail()   │  │ + consStream()       │  │ + assertFact()       │                │
│  │ + require()          │  │ + streamMap()        │  │ + query(), prove()   │                │
│  │ + allSolutions()     │  │ + streamFilter()     │  │ + unify()            │                │
│  └──────────────────────┘  └──────────────────────┘  └──────────────────────┘                │
│  ┌──────────────────────┐                                                                     │
│  │    MacroManager      │                                                                     │
│  ├──────────────────────┤                                                                     │
│  │ + define(), expand1()│                                                                     │
│  │ + expandAll()        │                                                                     │
│  │ + gensym()           │                                                                     │
│  └──────────────────────┘                                                                     │
│                                                                                                │
└────────────────────────────────────────────────────────────────────────────────────────────────┘
                                           │
                                           │ uses
                                           ▼
┌─ PROVIDERS (Pluggable) ─────────────────────────────────────────────────────────────────────────┐
│                                                                                                  │
│  ═══ CORE PROVIDERS (7) ═════════════════════════════════════════════════════════════════════  │
│  «interface»           «interface»           «interface»           «interface»                  │
│  StateProvider         SessionProvider       SnapshotProvider      ReceiptProvider              │
│  [01-EVAL.md]          [14-SESSION.md]       [07-DEBUG.md]         [09-OPR.md]                  │
│  ─────────────         ───────────────       ────────────────      ───────────────              │
│  + createStore()       + list(), load()      + list(), save()      + store(), list()           │
│  + loadStore()         + save(), fork()      + load(), export()    + verify()                  │
│        │                     │                    │                      │                      │
│        ▼                     ▼                    ▼                      ▼                      │
│  MemoryState-         FilesystemSession-    MemorySnapshot-        InMemory-                    │
│  Provider             Provider              Provider               ReceiptProvider              │
│                                                                                                  │
│  «interface»           «interface»           «interface»                                        │
│  TraceProvider         EffectHandler-        GovernanceProvider                                 │
│  [07-DEBUG.md]         Registry              [21-SECURITY.md]                                   │
│  ─────────────         [05-EFFECTS.md]       ──────────────────                                 │
│  + record(), list()    ──────────────        + getProfile(), getCaps()                         │
│  + dump()              + register(), get()   + checkCap(), consumeBudget()                     │
│                                                                                                  │
│  ═══ GAP ANALYSIS PROVIDERS (6) ═════════════════════════════════════════════════════════════  │
│  «interface»           «interface»           «interface»                                        │
│  ArtifactProvider      ProvenanceProvider    LogicProvider                                      │
│  [26-ARTIFACTS.md]     [22-PROVENANCE.md]    [USER-MANUAL--27]                                  │
│  ─────────────         ────────────────      ─────────────                                      │
│  + get(), put()        + capture()           + assertFact()                                     │
│  + stats(), clear()    + verify(), graph()   + addRule(), query()                              │
│                                                                                                  │
│  «interface»           «interface»           «interface»                                        │
│  ExpertProvider        StreamProvider        SecurityProvider                                   │
│  [29-EXPERTS.md]       [USER-MANUAL--07]     [21-SECURITY.md]                                   │
│  ─────────────         ─────────────         ────────────────                                   │
│  + registerRole()      + store(), get()      + checkCap()                                       │
│  + getRole()           + invalidate()        + auditLog()                                       │
│                                                                                                  │
│  ═══ MOCK PROVIDERS (testing) ═══════════════════════════════════════════════════════════════  │
│  Mock*Provider for each interface - enables isolated unit testing                               │
│                                                                                                  │
└──────────────────────────────────────────────────────────────────────────────────────────────────┘
                                           │
                                           │ uses
                                           ▼
┌─ EVENTS (26+ event types) ──────────────────────────────────────────────────────────────────────┐
│                                                                                                  │
│  ┌────────────────────────────────────────────────────────────────────────────┐                 │
│  │                         RuntimeEventEmitter                                 │                 │
│  ├────────────────────────────────────────────────────────────────────────────┤                 │
│  │  + on(event, handler): Unsubscribe      + emit(event, data): void          │                 │
│  │  + once(event, handler): Unsubscribe    + off(event, handler): void        │                 │
│  └────────────────────────────────────────────────────────────────────────────┘                 │
│                                                                                                  │
│  ═══ CORE EVENTS (10) [08-PROTOCOL, 10-ORACLE, 05-EFFECTS, 07-DEBUG, etc.] ════════════════   │
│    StepEvent, EffectEvent, BeforeLLMEvent, AfterLLMEvent, BreakpointHitEvent,                  │
│    ErrorEvent, DoneEvent, SessionCheckpointEvent, OprReceiptEvent, LLMToolCallEvent            │
│                                                                                                  │
│  ═══ CONDITIONS/FACTS/FIXPOINT (4) [06, 23, 24] ═══════════════════════════════════════════   │
│    ConditionEvent, FactAssertedEvent, FixpointIterEvent, FixpointDoneEvent                     │
│                                                                                                  │
│  ═══ BUDGET/SECURITY/CONCURRENCY (4) [25, 21, 12] ═════════════════════════════════════════   │
│    BudgetWarningEvent, BudgetExceededEvent, SecurityEvent, TaskCompleteEvent                   │
│                                                                                                  │
│  ═══ AMB/STREAMS (4) [USER-MANUAL--05,07,26,23] ═══════════════════════════════════════════   │
│    AmbChooseEvent, AmbFailEvent, AmbSolutionEvent, StreamForceEvent                            │
│                                                                                                  │
│  ═══ LOGIC/EXPERTS (4) [USER-MANUAL--27, 29-EXPERTS] ══════════════════════════════════════   │
│    FactQueriedEvent, RuleAppliedEvent, IntentCompiledEvent, ExpertResultEvent                  │
│                                                                                                  │
│  ═══ MACRO/CONCURRENCY (2) [11-MACROS, 12-CONCURRENCY] ════════════════════════════════════   │
│    MacroExpandEvent, DeadlockDetectedEvent                                                      │
│                                                                                                  │
└──────────────────────────────────────────────────────────────────────────────────────────────────┘
                                           │
                                           │ wraps (does NOT modify)
                                           ▼
   ┌─ src/core/ (UNCHANGED) ───────────────────────────────────────────────────────────┐
   │                                                                                    │
   │  eval/             effects/         oracle/          opr/          governance/    │
   │  ├─ machine.ts     ├─ runtimeImpl   ├─ adapter.ts    ├─ runtime.ts  ├─ profile   │
   │  ├─ machineStep    └─ opcall.ts     ├─ openai.ts     ├─ callbacks   ├─ caps.ts   │
   │  ├─ store.ts                        ├─ anthropic     └─ kernels/    └─ budgets   │
   │  └─ values.ts                       └─ scripted.ts                               │
   │                                                                                    │
   │  pipeline/         session/         provenance/                                   │
   │  └─ compileText    ├─ writer.ts     └─ graph.ts                                  │
   │                    └─ reader.ts                                                   │
   │                                                                                    │
   └────────────────────────────────────────────────────────────────────────────────────┘

LEGEND:
  ┌─────────┐     «interface»     «facade»        «factory»
  │  Class  │     Interface       Main class      Factory fn
  └─────────┘
  ──────►  uses/depends on
  ─ ─ ─►  implements
```

---

## Sequence Diagrams

### Sequence 1: runtime.eval() Flow

```
┌─────────────────────────────────────────────────────────────────────────────────────────────────┐
│                              SEQUENCE: runtime.eval(code)                                        │
│              Shows complete flow from eval call to result with all hooks firing                  │
└─────────────────────────────────────────────────────────────────────────────────────────────────┘

  Consumer          OmegaRuntime       ExecutionEngine     EffectRegistry      EventEmitter
     │                   │                   │                   │                   │
     │  eval(code)       │                   │                   │                   │
     │──────────────────>│                   │                   │                   │
     │                   │                   │                   │                   │
     │                   │  compile(code)    │                   │                   │
     │                   │──────────────────>│                   │                   │
     │                   │      <expr>       │                   │                   │
     │                   │<──────────────────│                   │                   │
     │                   │                   │                   │                   │
     │                   │  run(expr)        │                   │                   │
     │                   │──────────────────>│                   │                   │
     │                   │                   │                   │                   │
     │                   │                   │─────┐             │                   │
     │                   │                   │     │ step loop   │                   │
     │                   │                   │<────┘             │                   │
     │                   │                   │                   │                   │
     │                   │                   │  emit('step', {step, state, tag})     │
     │                   │                   │──────────────────────────────────────>│
     │                   │                   │                   │                   │
     │  on('step') ←─────│───────────────────│───────────────────│───────────────────│
     │                   │                   │                   │                   │
     │                   │                   │                   │                   │
     │                   │       ┌───────────┴───────────┐       │                   │
     │                   │       │  if effect detected:  │       │                   │
     │                   │       └───────────┬───────────┘       │                   │
     │                   │                   │                   │                   │
     │                   │                   │  emit('effect', {op, args, step})     │
     │                   │                   │──────────────────────────────────────>│
     │                   │                   │                   │                   │
     │  on('effect') ←───│───────────────────│───────────────────│───────────────────│
     │                   │                   │                   │                   │
     │                   │                   │  get(op)          │                   │
     │                   │                   │──────────────────>│                   │
     │                   │                   │    <handler>      │                   │
     │                   │                   │<──────────────────│                   │
     │                   │                   │                   │                   │
     │                   │                   │  handler(effect, ctx)                 │
     │                   │                   │──────────────────>│                   │
     │                   │                   │    <result>       │                   │
     │                   │                   │<──────────────────│                   │
     │                   │                   │                   │                   │
     │                   │                   │─────┐             │                   │
     │                   │                   │     │ continue    │                   │
     │                   │                   │<────┘ stepping    │                   │
     │                   │                   │                   │                   │
     │                   │       ┌───────────┴───────────┐       │                   │
     │                   │       │   when done:          │       │                   │
     │                   │       └───────────┬───────────┘       │                   │
     │                   │                   │                   │                   │
     │                   │                   │  emit('done', {result, stepCount})    │
     │                   │                   │──────────────────────────────────────>│
     │                   │                   │                   │                   │
     │  on('done') ←─────│───────────────────│───────────────────│───────────────────│
     │                   │                   │                   │                   │
     │                   │   <RunResult>     │                   │                   │
     │                   │<──────────────────│                   │                   │
     │                   │                   │                   │                   │
     │   <EvalResult>    │                   │                   │                   │
     │<──────────────────│                   │                   │                   │
     │                   │                   │                   │                   │

  HOOKS FIRED:
    • 'step' - once per CESK machine step (many times)
    • 'effect' - when inferOp or other effect is detected
    • 'done' - when evaluation completes with result
    • 'error' - if exception occurs (not shown, replaces 'done')
```

---

### Sequence 2: runtime.debug.step() Flow

```
┌─────────────────────────────────────────────────────────────────────────────────────────────────┐
│                            SEQUENCE: runtime.debug.step(n)                                       │
│             Shows debug stepping with breakpoint checking and time travel                        │
└─────────────────────────────────────────────────────────────────────────────────────────────────┘

  Consumer          OmegaRuntime       DebugSubsystem     BreakpointMgr      HistoryManager
     │                   │                   │                   │                   │
     │  debug.load(code) │                   │                   │                   │
     │──────────────────>│                   │                   │                   │
     │                   │  load(code)       │                   │                   │
     │                   │──────────────────>│                   │                   │
     │                   │     <compiled>    │                   │                   │
     │                   │<──────────────────│                   │                   │
     │                   │                   │  record(entry)    │                   │
     │                   │                   │──────────────────────────────────────>│
     │   <void>          │                   │                   │                   │
     │<──────────────────│                   │                   │                   │
     │                   │                   │                   │                   │
     │  debug.step(5)    │                   │                   │                   │
     │──────────────────>│                   │                   │                   │
     │                   │  step(5)          │                   │                   │
     │                   │──────────────────>│                   │                   │
     │                   │                   │                   │                   │
     │                   │                   │─────┐ for i=1..5  │                   │
     │                   │                   │     │             │                   │
     │                   │                   │  stepOnce()       │                   │
     │                   │                   │─────┘             │                   │
     │                   │                   │                   │                   │
     │                   │                   │  check(state)     │                   │
     │                   │                   │──────────────────>│                   │
     │                   │                   │                   │                   │
     │                   │       ┌───────────┴───────────┐       │                   │
     │                   │       │ if breakpoint hit:    │       │                   │
     │                   │       └───────────┬───────────┘       │                   │
     │                   │                   │                   │                   │
     │                   │                   │     <Breakpoint>  │                   │
     │                   │                   │<──────────────────│                   │
     │                   │                   │                   │                   │
     │                   │                   │  emit('breakpoint-hit', {...})        │
     │  on('breakpoint-hit') ←───────────────│───────────────────│                   │
     │                   │                   │                   │                   │
     │                   │                   │  record(entry)    │                   │
     │                   │                   │──────────────────────────────────────>│
     │                   │                   │                   │                   │
     │                   │    <StepResult>   │                   │                   │
     │                   │   outcome:'break' │                   │                   │
     │                   │<──────────────────│                   │                   │
     │                   │                   │                   │                   │
     │   <StepResult>    │                   │                   │                   │
     │<──────────────────│                   │                   │                   │
     │                   │                   │                   │                   │
     │  debug.goto(3)    │  (TIME TRAVEL)    │                   │                   │
     │──────────────────>│                   │                   │                   │
     │                   │  goto(3)          │                   │                   │
     │                   │──────────────────>│                   │                   │
     │                   │                   │  get(3)           │                   │
     │                   │                   │──────────────────────────────────────>│
     │                   │                   │    <TraceEntry>   │                   │
     │                   │                   │<──────────────────────────────────────│
     │                   │                   │                   │                   │
     │                   │                   │  restore state from entry             │
     │                   │                   │─────┐             │                   │
     │                   │                   │<────┘             │                   │
     │                   │     <void>        │                   │                   │
     │                   │<──────────────────│                   │                   │
     │   <void>          │                   │                   │                   │
     │<──────────────────│                   │                   │                   │
     │                   │                   │                   │                   │

  HOOKS FIRED:
    • 'step' - once per machine step
    • 'breakpoint-hit' - when breakpoint condition matches
    • 'effect' - if effect occurs during step
```

---

### Sequence 3: runtime.llm.ask() Flow

```
┌─────────────────────────────────────────────────────────────────────────────────────────────────┐
│                              SEQUENCE: runtime.llm.ask(question)                                 │
│            Shows LLM agent loop with before/after hooks and tool calling                         │
└─────────────────────────────────────────────────────────────────────────────────────────────────┘

  Consumer          OmegaRuntime       LLMIntegration       LLMAdapter        EventEmitter
     │                   │                   │                   │                   │
     │  llm.ask(question)│                   │                   │                   │
     │──────────────────>│                   │                   │                   │
     │                   │  ask(question)    │                   │                   │
     │                   │──────────────────>│                   │                   │
     │                   │                   │                   │                   │
     │                   │                   │  buildRequest()   │                   │
     │                   │                   │─────┐             │                   │
     │                   │                   │<────┘             │                   │
     │                   │                   │                   │                   │
     │                   │                   │  emit('before-llm', {request, traceId})
     │                   │                   │──────────────────────────────────────>│
     │                   │                   │                   │                   │
     │  on('before-llm') ←───────────────────│───────────────────│───────────────────│
     │    (can modify)   │                   │                   │                   │
     │                   │                   │                   │                   │
     │                   │                   │  call(request)    │                   │
     │                   │                   │──────────────────>│                   │
     │                   │                   │                   │──────┐            │
     │                   │                   │                   │      │ API call   │
     │                   │                   │                   │<─────┘            │
     │                   │                   │   <response>      │                   │
     │                   │                   │<──────────────────│                   │
     │                   │                   │                   │                   │
     │                   │                   │  emit('after-llm', {request, response, durationMs})
     │                   │                   │──────────────────────────────────────>│
     │                   │                   │                   │                   │
     │  on('after-llm') ←────────────────────│───────────────────│───────────────────│
     │                   │                   │                   │                   │
     │                   │       ┌───────────┴───────────┐       │                   │
     │                   │       │ if tool_calls in resp │       │                   │
     │                   │       └───────────┬───────────┘       │                   │
     │                   │                   │                   │                   │
     │                   │                   │  executeToolCall()│                   │
     │                   │                   │─────┐             │                   │
     │                   │                   │<────┘             │                   │
     │                   │                   │                   │                   │
     │                   │                   │  (loop: more before-llm, call, after-llm)
     │                   │                   │──────────────────────────────────────>│
     │                   │                   │                   │                   │
     │                   │       ┌───────────┴───────────┐       │                   │
     │                   │       │ when final answer:    │       │                   │
     │                   │       └───────────┬───────────┘       │                   │
     │                   │                   │                   │                   │
     │                   │   <AgentResult>   │                   │                   │
     │                   │<──────────────────│                   │                   │
     │                   │                   │                   │                   │
     │   <AgentResult>   │                   │                   │                   │
     │<──────────────────│                   │                   │                   │
     │                   │                   │                   │                   │

  HOOKS FIRED (per LLM call, may loop multiple times):
    • 'before-llm' - BEFORE each API call (can inspect/modify request)
    • 'after-llm' - AFTER each API response (can inspect response, timing)

  AgentResult contains:
    • answer: string - final response
    • toolCalls: ToolCall[] - all tool invocations
    • traceId: string - for retrieving full trace later
    • tokenUsage: {prompt, completion, total}
```

---

### Sequence 4: Session Save/Load Flow

```
┌─────────────────────────────────────────────────────────────────────────────────────────────────┐
│                         SEQUENCE: runtime.session.save() / load()                                │
│                    Shows session persistence with checkpoint events                              │
└─────────────────────────────────────────────────────────────────────────────────────────────────┘

  Consumer          OmegaRuntime       SessionManager      SessionProvider    EventEmitter
     │                   │                   │                   │                   │
     │  session.save(nm) │                   │                   │                   │
     │──────────────────>│                   │                   │                   │
     │                   │  save(name)       │                   │                   │
     │                   │──────────────────>│                   │                   │
     │                   │                   │                   │                   │
     │                   │                   │  gatherState()    │                   │
     │                   │                   │─────┐             │                   │
     │                   │                   │<────┘ (from engine, history)          │
     │                   │                   │                   │                   │
     │                   │                   │  save(name, data) │                   │
     │                   │                   │──────────────────>│                   │
     │                   │                   │                   │──────┐            │
     │                   │                   │                   │      │ write file │
     │                   │                   │                   │<─────┘            │
     │                   │                   │     <void>        │                   │
     │                   │                   │<──────────────────│                   │
     │                   │                   │                   │                   │
     │                   │                   │  emit('session-checkpoint', {checkpoint, name})
     │                   │                   │──────────────────────────────────────>│
     │                   │                   │                   │                   │
     │  on('session-checkpoint') ←───────────│───────────────────│───────────────────│
     │                   │                   │                   │                   │
     │                   │     <void>        │                   │                   │
     │                   │<──────────────────│                   │                   │
     │   <void>          │                   │                   │                   │
     │<──────────────────│                   │                   │                   │
     │                   │                   │                   │                   │
     │   ─ ─ ─ ─ ─ ─ ─ ─ later ─ ─ ─ ─ ─ ─ ─│                   │                   │
     │                   │                   │                   │                   │
     │  session.load(nm) │                   │                   │                   │
     │──────────────────>│                   │                   │                   │
     │                   │  load(name)       │                   │                   │
     │                   │──────────────────>│                   │                   │
     │                   │                   │  load(name)       │                   │
     │                   │                   │──────────────────>│                   │
     │                   │                   │                   │──────┐            │
     │                   │                   │                   │      │ read file  │
     │                   │                   │                   │<─────┘            │
     │                   │                   │   <SessionData>   │                   │
     │                   │                   │<──────────────────│                   │
     │                   │                   │                   │                   │
     │                   │                   │  restoreState(data)                   │
     │                   │                   │─────┐ (to engine, history)            │
     │                   │                   │<────┘             │                   │
     │                   │                   │                   │                   │
     │                   │     <void>        │                   │                   │
     │                   │<──────────────────│                   │                   │
     │   <void>          │                   │                   │                   │
     │<──────────────────│                   │                   │                   │
     │                   │                   │                   │                   │

  HOOKS FIRED:
    • 'session-checkpoint' - when session is saved (includes checkpoint metadata)
```

---

## Implementation Plan

### Phase 1: Foundation (022-types, 022-providers, 022-events)
- Define all types and interfaces
- Implement provider interfaces with default implementations
- Implement typed event emitter

### Phase 2: Core Modules (022-execution through 022-history)
- Extract execution logic from REPL
- Create each subsystem as standalone module
- Wire up event emission

### Phase 3: LLM & OPR (022-llm, 022-opr)
- Extract LLM agent loop
- Wire up OPR with receipt provider

### Phase 4: Composition (022-runtime)
- Create main OmegaRuntime class
- Compose all subsystems
- Factory function with defaults

### Phase 5: Refactor Consumers (022-repl-refactor, 022-server-refactor)
- Refactor REPL to use runtime
- Refactor debug server to use runtime
- Verify behavior matches

### Phase 6: Tests & Docs (022-tests, 022-docs)
- Comprehensive tests
- API documentation
- Migration guide

---

## Success Criteria

1. **Single Source of Truth**: All runtime logic in `src/runtime/`
2. **Clean API**: External code uses typed methods, not REPL commands
3. **Hooks Work**: Can subscribe to any event from outside
4. **Pluggable**: All providers can be swapped
5. **REPL Simplified**: `bin/omega-repl.ts` under 300 lines
6. **Tests Pass**: Existing behavior preserved
7. **Zero Breaking Changes**: Old code still works during migration

---

## Integration Architecture: How Components Communicate

**CRITICAL: Every interface must be clearly defined for seamless integration.**

### Component Communication Diagram

```
┌─────────────────────────────────────────────────────────────────────────────────────────────────┐
│                      INTEGRATION ARCHITECTURE - WHO TALKS TO WHOM                                │
└─────────────────────────────────────────────────────────────────────────────────────────────────┘

                              ┌─────────────────────────┐
                              │       CONSUMERS         │
                              │  (REPL, Server, Tests)  │
                              └───────────┬─────────────┘
                                          │ uses
                                          ▼
┌─────────────────────────────────────────────────────────────────────────────────────────────────┐
│                                    OmegaRuntime                                                  │
│                                    (FACADE PATTERN)                                              │
│                                                                                                  │
│  SINGLE ENTRY POINT - All consumers use this interface only                                      │
│                                                                                                  │
│  PUBLIC API:                                                                                     │
│  ├── eval(code): Promise<Outcome>          ──→ ExecutionEngine                                  │
│  ├── loadFile(path): Promise<void>         ──→ ExecutionEngine                                  │
│  ├── on(event, handler): Unsubscribe       ──→ RuntimeEventEmitter                              │
│  ├── debug.*                               ──→ DebugSubsystem                                   │
│  ├── breakpoints.*                         ──→ BreakpointManager                                │
│  ├── snapshots.*                           ──→ SnapshotManager                                  │
│  ├── session.*                             ──→ SessionManager                                   │
│  ├── inspect.*                             ──→ StateInspector                                   │
│  ├── history.*                             ──→ HistoryManager                                   │
│  ├── llm.*                                 ──→ LLMIntegration                                   │
│  ├── opr.*                                 ──→ OprIntegration                                   │
│  ├── artifacts.*                           ──→ ArtifactManager                                  │
│  ├── facts.*                               ──→ FactsManager                                     │
│  ├── conditions.*                          ──→ ConditionsManager                                │
│  ├── fixpoint.*                            ──→ FixpointManager                                  │
│  ├── transaction.*                         ──→ TransactionManager                               │
│  ├── provenance.*                          ──→ ProvenanceManager                                │
│  ├── security.*                            ──→ SecurityManager                                  │
│  ├── budget.*                              ──→ BudgetManager                                    │
│  ├── protocol.*                            ──→ ProtocolServer                                   │
│  ├── concurrency.*                         ──→ ConcurrencyManager                               │
│  ├── amb.*                                 ──→ AmbManager                                       │
│  ├── streams.*                             ──→ StreamsManager                                   │
│  ├── logic.*                               ──→ LogicManager                                     │
│  ├── experts.*                             ──→ ExpertManager                                    │
│  └── macros.*                              ──→ MacroManager                                     │
│                                                                                                  │
└─────────────────────────────────────────────────────────────────────────────────────────────────┘
                    │
                    │ delegates to (MEDIATOR PATTERN)
                    ▼
┌─────────────────────────────────────────────────────────────────────────────────────────────────┐
│                                     SUBSYSTEMS                                                   │
│                                                                                                  │
│  Each subsystem:                                                                                 │
│  ├── Receives dependencies via constructor (DEPENDENCY INJECTION)                               │
│  ├── Emits events through shared RuntimeEventEmitter (OBSERVER PATTERN)                         │
│  ├── Uses providers for storage (STRATEGY PATTERN)                                              │
│  └── Does NOT know about other subsystems (LOOSE COUPLING)                                      │
│                                                                                                  │
│  ┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐       │
│  │ExecutionEngine  │───▶│RuntimeEvent-    │◀───│DebugSubsystem   │───▶│BreakpointMgr   │       │
│  │                 │    │Emitter          │    │                 │    │                 │       │
│  │ emits: step,    │    │                 │    │ emits:          │    │ emits:          │       │
│  │ effect, done,   │    │ ALL EVENTS      │    │ breakpoint-hit  │    │ breakpoint-hit  │       │
│  │ error           │    │ FLOW THROUGH    │    │                 │    │                 │       │
│  └────────┬────────┘    │ THIS HUB        │    └────────┬────────┘    └─────────────────┘       │
│           │             │                 │             │                                        │
│           │             └────────┬────────┘             │                                        │
│           │                      │                      │                                        │
│           ▼                      ▼                      ▼                                        │
│  ┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐                              │
│  │StateProvider    │    │Consumers        │    │HistoryManager   │                              │
│  │(STRATEGY)       │    │subscribe via    │    │                 │                              │
│  │                 │    │runtime.on()     │    │ uses            │                              │
│  │MemoryState      │    │                 │    │ TraceProvider   │                              │
│  │FilesystemState  │    │ CLI, Server,    │    │                 │                              │
│  │MockState        │    │ Tests, etc.     │    │                 │                              │
│  └─────────────────┘    └─────────────────┘    └─────────────────┘                              │
│                                                                                                  │
└─────────────────────────────────────────────────────────────────────────────────────────────────┘
```

### Design Patterns Used

```
┌────────────────────────────────────────────────────────────────────────────────────────────────┐
│                              DESIGN PATTERNS REFERENCE                                          │
├──────────────────┬─────────────────────────────────────┬────────────────────────────────────────┤
│ Pattern          │ Where Used                          │ Purpose                                │
├──────────────────┼─────────────────────────────────────┼────────────────────────────────────────┤
│ FACADE           │ OmegaRuntime class                  │ Single entry point for all consumers   │
│                  │                                     │ Hides complexity of 24 subsystems      │
├──────────────────┼─────────────────────────────────────┼────────────────────────────────────────┤
│ MEDIATOR         │ OmegaRuntime coordinates            │ Subsystems don't talk directly to      │
│                  │ subsystem interactions              │ each other - reduces coupling          │
├──────────────────┼─────────────────────────────────────┼────────────────────────────────────────┤
│ OBSERVER         │ RuntimeEventEmitter                 │ Decoupled event notification           │
│                  │ on(), emit(), off()                 │ Consumers subscribe to events          │
├──────────────────┼─────────────────────────────────────┼────────────────────────────────────────┤
│ STRATEGY         │ All Providers                       │ Swappable implementations              │
│                  │ StateProvider, SessionProvider,     │ (Memory, Filesystem, Mock)             │
│                  │ SnapshotProvider, etc.              │                                        │
├──────────────────┼─────────────────────────────────────┼────────────────────────────────────────┤
│ DEPENDENCY       │ All Subsystem constructors          │ Testability - inject mocks             │
│ INJECTION        │ receive providers as parameters     │                                        │
├──────────────────┼─────────────────────────────────────┼────────────────────────────────────────┤
│ FACTORY          │ createRuntime(config)               │ Creates OmegaRuntime with defaults     │
│                  │                                     │ or custom providers                    │
├──────────────────┼─────────────────────────────────────┼────────────────────────────────────────┤
│ ADAPTER          │ LLMAdapter interface                │ Wrap different LLM APIs (OpenAI,       │
│                  │ OpenAI, Anthropic, Mock             │ Anthropic) with common interface       │
├──────────────────┼─────────────────────────────────────┼────────────────────────────────────────┤
│ COMMAND          │ ProtocolServer operations           │ Encapsulate requests as objects        │
│                  │ eval, info, complete, etc.          │                                        │
├──────────────────┼─────────────────────────────────────┼────────────────────────────────────────┤
│ MEMENTO          │ SnapshotManager                     │ Capture and restore state              │
├──────────────────┼─────────────────────────────────────┼────────────────────────────────────────┤
│ STATE            │ DebugSubsystem                      │ Debug state machine                    │
│                  │ (idle, running, paused, stopped)    │ (controls step/run/stop behavior)      │
├──────────────────┼─────────────────────────────────────┼────────────────────────────────────────┤
│ CHAIN OF         │ ProtocolServer middleware           │ Request processing pipeline            │
│ RESPONSIBILITY   │ protocol.use(middleware)            │                                        │
├──────────────────┼─────────────────────────────────────┼────────────────────────────────────────┤
│ TEMPLATE METHOD  │ Provider base classes               │ Shared logic with extension points     │
├──────────────────┼─────────────────────────────────────┼────────────────────────────────────────┤
│ NULL OBJECT      │ MockProviders                       │ No-op implementations for testing      │
└──────────────────┴─────────────────────────────────────┴────────────────────────────────────────┘
```

---

## Testing & Demo Coverage Requirements

**CRITICAL: Every feature must be testable at unit, integration, E2E, and demo levels.**

### Testing Pyramid

```
                                    ╱╲
                                   ╱  ╲
                                  ╱ E2E╲         E2E Tests: CLI + Server (slowest)
                                 ╱──────╲
                                ╱        ╲
                               ╱  DEMOS   ╲       Demo Tests: Auto-discovered demos
                              ╱────────────╲
                             ╱              ╲
                            ╱  INTEGRATION   ╲     Integration: Subsystem combos
                           ╱──────────────────╲
                          ╱                    ╲
                         ╱      UNIT TESTS      ╲   Unit: Each subsystem isolated (fastest)
                        ╱────────────────────────╲
```

### Environment Variable Control

```
┌────────────────────────────────────────────────────────────────────────────────────────────────┐
│                          ENVIRONMENT VARIABLE CONFIGURATION                                     │
├──────────────────────────┬─────────────────────────┬────────────────────────────────────────────┤
│ Variable                 │ Values                  │ Purpose                                    │
├──────────────────────────┼─────────────────────────┼────────────────────────────────────────────┤
│ OMEGA_LLM_MODE           │ mock | live | scripted  │ Controls LLM behavior in tests/demos       │
│                          │                         │ mock: Use MockLLMAdapter                   │
│                          │                         │ live: Use real LLM (OpenAI/Anthropic)      │
│                          │                         │ scripted: Use ScriptedOracle               │
├──────────────────────────┼─────────────────────────┼────────────────────────────────────────────┤
│ OMEGA_LLM_PROVIDER       │ openai | anthropic      │ Which LLM provider for live mode           │
├──────────────────────────┼─────────────────────────┼────────────────────────────────────────────┤
│ OMEGA_TEST_TIMEOUT       │ number (ms)             │ Test timeout (default: 30000)              │
├──────────────────────────┼─────────────────────────┼────────────────────────────────────────────┤
│ OMEGA_DEMO_VERBOSE       │ true | false            │ Verbose output in demo tests               │
├──────────────────────────┼─────────────────────────┼────────────────────────────────────────────┤
│ OMEGA_SNAPSHOT_DIR       │ path                    │ Where to store test snapshots              │
├──────────────────────────┼─────────────────────────┼────────────────────────────────────────────┤
│ OMEGA_RECORD_MODE        │ true | false            │ Record LLM responses for replay            │
└──────────────────────────┴─────────────────────────┴────────────────────────────────────────────┘
```

### LLM Mock Infrastructure

```typescript
// src/runtime/providers/mocks/MockLLMAdapter.ts

/**
 * Mock LLM Adapter for testing without real LLM calls.
 *
 * USAGE IN TESTS:
 *
 *   // Option 1: Provide responses directly
 *   const mock = new MockLLMAdapter({
 *     responses: [
 *       { match: /classify/, response: '(list "category-a")' },
 *       { match: /extract/, response: '(list "entity1" "entity2")' },
 *     ]
 *   });
 *
 *   // Option 2: Use scripted responses from file
 *   const mock = MockLLMAdapter.fromScript('path/to/responses.json');
 *
 *   // Option 3: Record real responses for later replay
 *   const mock = MockLLMAdapter.recording();
 *   // ... run tests with real LLM ...
 *   mock.saveRecording('path/to/responses.json');
 */
export interface MockLLMAdapter extends LLMAdapter {
  // Configure mock responses
  addResponse(pattern: RegExp, response: string): void;

  // Verify calls were made
  getCalls(): LLMRequest[];
  getCallCount(): number;
  assertCalledWith(pattern: RegExp): void;

  // Record/replay mode
  startRecording(): void;
  stopRecording(): void;
  saveRecording(path: string): Promise<void>;

  // Load scripted responses
  static fromScript(path: string): Promise<MockLLMAdapter>;
}
```

### Demo Structure (Auto-Discovery)

```
demos/
├── __test_runner__.ts           # Auto-discovers and runs all demos
├── vitest.demos.config.ts       # Vitest config for demo tests
│
├── 01-basic-eval/               # Each demo has its own folder
│   ├── setup.ts                 # Demo-specific setup (providers, config)
│   ├── run.ts                   # Main demo script (can run standalone)
│   ├── run.test.ts              # Automated test wrapper for the demo
│   ├── expected-output.json     # Expected results for verification
│   ├── mock-responses.json      # LLM responses for mock mode (optional)
│   └── README.md                # Human-readable demo description
│
├── 02-debug-step-through/
│   ├── setup.ts
│   ├── run.ts
│   ├── run.test.ts
│   ├── expected-output.json
│   └── README.md
│
├── 03-breakpoints/
├── 04-snapshots/
├── 05-session-persistence/
├── 06-time-travel/
├── 07-llm-ask/
├── 08-opr-kernels/
├── 09-hooks-events/
├── 10-custom-providers/
├── 11-protocol-server/
├── 12-amb-backtracking/
├── 13-lazy-streams/
├── 14-logic-programming/
├── 15-expert-compilation/
├── 16-macro-expansion/
├── 17-conditions-restarts/
├── 18-fixpoint-convergence/
├── 19-transactions/
├── 20-provenance-tracking/
├── 21-budget-limits/
├── 22-security-caps/
├── 23-concurrent-fibers/
├── 24-artifacts-memoization/
└── ...
```

### Demo Auto-Discovery Infrastructure

```typescript
// demos/__test_runner__.ts

/**
 * Auto-discovers all demos and runs them as tests.
 *
 * DISCOVERY RULES:
 * - Each subfolder in demos/ is a demo
 * - Must have run.ts and run.test.ts
 * - Optionally has mock-responses.json for mock mode
 *
 * RUN ALL DEMOS:
 *   npm run test:demos              # Mock mode (fast)
 *   npm run test:demos:live         # Real LLM mode (slow)
 *
 * RUN SINGLE DEMO:
 *   npm run demo 01-basic-eval      # Interactive run
 *   npm run test:demo 01-basic-eval # Run as test
 */

import { glob } from 'glob';
import { describe, it, beforeAll } from 'vitest';

// Auto-discover demos
const demoFolders = glob.sync('demos/*/run.test.ts');

describe('Demo Suite', () => {
  for (const demoPath of demoFolders) {
    const demoName = demoPath.split('/')[1];

    describe(demoName, () => {
      let demo: DemoRunner;

      beforeAll(async () => {
        // Configure based on environment
        const llmMode = process.env.OMEGA_LLM_MODE || 'mock';
        demo = await DemoRunner.load(demoName, { llmMode });
      });

      it('should complete successfully', async () => {
        const result = await demo.run();
        expect(result.success).toBe(true);
      });

      it('should match expected output', async () => {
        const result = await demo.run();
        expect(result.output).toMatchSnapshot();
      });
    });
  }
});
```

---

## Complete Test Enumeration

**EVERY test file that MUST exist for full coverage:**

### Unit Tests (per subsystem)

```
┌────────────────────────────────────────────────────────────────────────────────────────────────┐
│                              UNIT TEST ENUMERATION                                              │
│                    Test each subsystem in isolation using mock providers                        │
├────────────────────────────────────────────────────────────────────────────────────────────────┤
│                                                                                                 │
│  tests/runtime/                                                                                 │
│  ├── OmegaRuntime.test.ts              # Facade creation, provider injection, event delegation │
│  │                                                                                              │
│  │  ═══ CORE ═══                                                                                │
│  ├── ExecutionEngine.test.ts           # eval(), step(), run(), effect handling               │
│  │   ├── test: evaluates simple expressions                                                    │
│  │   ├── test: step returns StepResult                                                         │
│  │   ├── test: run completes to done                                                           │
│  │   ├── test: emits step events                                                               │
│  │   ├── test: emits effect events                                                             │
│  │   ├── test: emits error events on failure                                                   │
│  │   ├── test: emits done event on completion                                                  │
│  │   ├── test: uses StateProvider correctly                                                    │
│  │   └── test: uses EffectHandlerRegistry correctly                                            │
│  │                                                                                              │
│  │  ═══ DEBUGGING ═══                                                                           │
│  ├── DebugSubsystem.test.ts            # load(), step(), run(), goto(), stop()                │
│  │   ├── test: load initializes debug session                                                  │
│  │   ├── test: step advances by N steps                                                        │
│  │   ├── test: run continues until breakpoint or done                                          │
│  │   ├── test: goto jumps to specific step (time travel)                                       │
│  │   ├── test: stop terminates debug session                                                   │
│  │   ├── test: getState returns current DebugState                                             │
│  │   ├── test: getTrace returns execution trace                                                │
│  │   └── test: integrates with BreakpointManager                                               │
│  │                                                                                              │
│  ├── BreakpointManager.test.ts         # add(), remove(), list(), toggle(), check()           │
│  │   ├── test: add step breakpoint                                                             │
│  │   ├── test: add expr breakpoint                                                             │
│  │   ├── test: add effect breakpoint                                                           │
│  │   ├── test: remove breakpoint by id                                                         │
│  │   ├── test: list all breakpoints                                                            │
│  │   ├── test: toggle breakpoint enabled/disabled                                              │
│  │   ├── test: check returns matching breakpoint                                               │
│  │   └── test: check returns null when no match                                                │
│  │                                                                                              │
│  ├── SnapshotManager.test.ts           # save(), restore(), list(), saveImage(), loadImage()  │
│  │   ├── test: save creates snapshot                                                           │
│  │   ├── test: restore loads snapshot                                                          │
│  │   ├── test: list returns all snapshots                                                      │
│  │   ├── test: export writes to file                                                           │
│  │   ├── test: saveImage persists full state                                                   │
│  │   ├── test: loadImage restores full state                                                   │
│  │   └── test: uses SnapshotProvider correctly                                                 │
│  │                                                                                              │
│  ├── StateInspector.test.ts            # env(), stack(), frame(), defs(), control()           │
│  │   ├── test: env returns all bindings                                                        │
│  │   ├── test: env(name) returns specific binding                                              │
│  │   ├── test: stack returns all frames                                                        │
│  │   ├── test: frame(n) returns specific frame                                                 │
│  │   ├── test: defs returns all definitions                                                    │
│  │   ├── test: control returns control info                                                    │
│  │   ├── test: facts returns fact inspection (23-FACTS.md)                                     │
│  │   └── test: factSignature returns signature                                                 │
│  │                                                                                              │
│  ├── HistoryManager.test.ts            # back(), list(), dump(), replay()                     │
│  │   ├── test: back(n) goes back n steps                                                       │
│  │   ├── test: list returns history entries                                                    │
│  │   ├── test: setRecording enables/disables                                                   │
│  │   ├── test: dump writes to file                                                             │
│  │   ├── test: replay loads from file                                                          │
│  │   └── test: uses TraceProvider correctly                                                    │
│  │                                                                                              │
│  ├── SessionManager.test.ts            # save(), load(), fork(), goto(), getTrace()           │
│  │   ├── test: save persists session                                                           │
│  │   ├── test: load restores session                                                           │
│  │   ├── test: fork creates copy                                                               │
│  │   ├── test: list returns all sessions                                                       │
│  │   ├── test: goto jumps to checkpoint                                                        │
│  │   ├── test: getTrace returns session trace                                                  │
│  │   ├── test: getCheckpoints returns checkpoints                                              │
│  │   ├── test: emits session-checkpoint event                                                  │
│  │   └── test: uses SessionProvider correctly                                                  │
│  │                                                                                              │
│  │  ═══ DATA ═══                                                                                │
│  ├── ArtifactManager.test.ts           # get(), put(), stats(), clear()                       │
│  │   ├── test: put stores artifact                                                             │
│  │   ├── test: get retrieves artifact                                                          │
│  │   ├── test: getByExpr validates fingerprints                                                │
│  │   ├── test: stats returns hit/miss rates                                                    │
│  │   ├── test: clear removes all                                                               │
│  │   ├── test: clearByExpr removes specific                                                    │
│  │   └── test: uses ArtifactProvider correctly                                                 │
│  │                                                                                              │
│  ├── FactsManager.test.ts              # all(), has(), count(), signature(), query()          │
│  │   ├── test: all returns all facts                                                           │
│  │   ├── test: has checks existence                                                            │
│  │   ├── test: get returns fact with metadata                                                  │
│  │   ├── test: count returns fact count                                                        │
│  │   ├── test: signature returns hash                                                          │
│  │   ├── test: query matches pattern                                                           │
│  │   └── test: emits fact-asserted event                                                       │
│  │                                                                                              │
│  │  ═══ CONTROL ═══                                                                             │
│  ├── ConditionsManager.test.ts         # signal(), bind(), restart()                          │
│  │   ├── test: signal raises condition                                                         │
│  │   ├── test: bind installs handler                                                           │
│  │   ├── test: restart invokes restart                                                         │
│  │   ├── test: getRestarts returns active restarts                                             │
│  │   └── test: emits condition event                                                           │
│  │                                                                                              │
│  ├── FixpointManager.test.ts           # run(), getState(), signature()                       │
│  │   ├── test: run executes fixpoint loop                                                      │
│  │   ├── test: getState returns current state                                                  │
│  │   ├── test: signature computes state hash                                                   │
│  │   ├── test: isInFixpoint returns status                                                     │
│  │   ├── test: returns ok on convergence                                                       │
│  │   ├── test: returns nonconverged on max iters                                               │
│  │   ├── test: returns cycle on repeated state                                                 │
│  │   ├── test: emits fixpoint-iter event                                                       │
│  │   └── test: emits fixpoint-done event                                                       │
│  │                                                                                              │
│  ├── TransactionManager.test.ts        # begin(), propose(), commit(), rollback()             │
│  │   ├── test: begin starts transaction                                                        │
│  │   ├── test: propose adds staged change                                                      │
│  │   ├── test: getProposals returns pending                                                    │
│  │   ├── test: commit applies all                                                              │
│  │   ├── test: rollback discards all                                                           │
│  │   └── test: isInTransaction returns status                                                  │
│  │                                                                                              │
│  │  ═══ GOVERNANCE ═══                                                                          │
│  ├── ProvenanceManager.test.ts         # capture(), verify(), isStale(), graph()              │
│  │   ├── test: capture stores evidence                                                         │
│  │   ├── test: verify validates evidence                                                       │
│  │   ├── test: isStale detects staleness                                                       │
│  │   ├── test: getEvidence retrieves by id                                                     │
│  │   ├── test: graph returns provenance graph                                                  │
│  │   ├── test: setMode changes epistemic mode                                                  │
│  │   └── test: uses ProvenanceProvider correctly                                               │
│  │                                                                                              │
│  ├── SecurityManager.test.ts           # checkCap(), validateLLM(), getAuditLog()             │
│  │   ├── test: checkCap returns boolean                                                        │
│  │   ├── test: requireCap throws on missing                                                    │
│  │   ├── test: getCaps returns all caps                                                        │
│  │   ├── test: validateLLMOutput validates                                                     │
│  │   ├── test: verifySignature verifies                                                        │
│  │   ├── test: getAuditLog returns log                                                         │
│  │   ├── test: getSandboxMode returns mode                                                     │
│  │   ├── test: setSandboxMode sets mode                                                        │
│  │   └── test: emits security-event                                                            │
│  │                                                                                              │
│  ├── BudgetManager.test.ts             # remaining(), consume(), report()                     │
│  │   ├── test: remaining returns budget                                                        │
│  │   ├── test: hasRemaining checks availability                                                │
│  │   ├── test: consume deducts budget                                                          │
│  │   ├── test: report returns full report                                                      │
│  │   ├── test: emits budget-warning at 80%                                                     │
│  │   └── test: emits budget-exceeded when empty                                                │
│  │                                                                                              │
│  │  ═══ COMMUNICATION ═══                                                                       │
│  ├── ProtocolServer.test.ts            # handle(), use(), listen()                            │
│  │   ├── test: handle processes request                                                        │
│  │   ├── test: use adds middleware                                                             │
│  │   ├── test: listen starts server                                                            │
│  │   ├── test: close stops server                                                              │
│  │   ├── test: eval operation works                                                            │
│  │   ├── test: info operation works                                                            │
│  │   ├── test: complete operation works                                                        │
│  │   └── test: interrupt operation works                                                       │
│  │                                                                                              │
│  ├── ConcurrencyManager.test.ts        # spawn(), chan(), awaitAll(), fiberSpawn()            │
│  │   ├── test: fiberSpawn creates fiber                                                        │
│  │   ├── test: fiberJoin waits for fiber                                                       │
│  │   ├── test: fiberSelect waits for first                                                     │
│  │   ├── test: parallelMap runs in parallel                                                    │
│  │   ├── test: makeMutex creates mutex                                                         │
│  │   ├── test: mutexAcquire/release works                                                      │
│  │   ├── test: makeSerializer creates serializer                                               │
│  │   ├── test: makeSingleflight deduplicates                                                   │
│  │   ├── test: spawn creates actor                                                             │
│  │   ├── test: send delivers message                                                           │
│  │   ├── test: ask gets response                                                               │
│  │   ├── test: chan creates channel                                                            │
│  │   ├── test: awaitAll waits for all                                                          │
│  │   ├── test: awaitAny waits for first                                                        │
│  │   ├── test: withTimeout times out                                                           │
│  │   ├── test: emits task-complete event                                                       │
│  │   └── test: emits deadlock-detected event                                                   │
│  │                                                                                              │
│  │  ═══ LLM ═══                                                                                 │
│  ├── LLMIntegration.test.ts            # ask(), configure(), getTraces()                      │
│  │   ├── test: configure sets adapter                                                          │
│  │   ├── test: ask executes query                                                              │
│  │   ├── test: getTraces returns all traces                                                    │
│  │   ├── test: getTrace returns specific trace                                                 │
│  │   ├── test: emits before-llm event                                                          │
│  │   ├── test: emits after-llm event                                                           │
│  │   ├── test: handles tool calls                                                              │
│  │   └── test: works with MockLLMAdapter                                                       │
│  │                                                                                              │
│  ├── OprIntegration.test.ts            # list(), run(), getReceipts(), verify()               │
│  │   ├── test: list returns kernels                                                            │
│  │   ├── test: run executes kernel                                                             │
│  │   ├── test: getReceipts returns receipts                                                    │
│  │   ├── test: verify validates receipts                                                       │
│  │   ├── test: emits opr-receipt event                                                         │
│  │   └── test: uses ReceiptProvider correctly                                                  │
│  │                                                                                              │
│  ├── ExpertManager.test.ts             # registerRole(), compileIntent(), runParallel()       │
│  │   ├── test: registerRole adds role                                                          │
│  │   ├── test: getRole retrieves role                                                          │
│  │   ├── test: listRoles returns all                                                           │
│  │   ├── test: compileIntent compiles                                                          │
│  │   ├── test: validateOutput validates                                                        │
│  │   ├── test: runParallel runs experts                                                        │
│  │   ├── test: batch processes multiple                                                        │
│  │   ├── test: emits intent-compiled event                                                     │
│  │   └── test: emits expert-result event                                                       │
│  │                                                                                              │
│  │  ═══ SEMANTIC ═══                                                                            │
│  ├── AmbManager.test.ts                # choose(), fail(), require(), allSolutions()          │
│  │   ├── test: choose returns choice                                                           │
│  │   ├── test: fail triggers backtrack                                                         │
│  │   ├── test: require fails if false                                                          │
│  │   ├── test: allSolutions collects all                                                       │
│  │   ├── test: firstSolution gets first                                                        │
│  │   ├── test: directedAmb uses LLM                                                            │
│  │   ├── test: getChoicePoint returns point                                                    │
│  │   ├── test: getBacktrackStack returns stack                                                 │
│  │   ├── test: resetSearch clears state                                                        │
│  │   ├── test: emits amb-choose event                                                          │
│  │   ├── test: emits amb-fail event                                                            │
│  │   └── test: emits amb-solution event                                                        │
│  │                                                                                              │
│  ├── StreamsManager.test.ts            # consStream(), streamMap(), streamFilter()            │
│  │   ├── test: listToStream converts list                                                      │
│  │   ├── test: consStream creates stream                                                       │
│  │   ├── test: emptyStream is empty                                                            │
│  │   ├── test: streamCar gets first                                                            │
│  │   ├── test: streamCdr gets rest                                                             │
│  │   ├── test: streamNull checks empty                                                         │
│  │   ├── test: streamToList forces n                                                           │
│  │   ├── test: streamMap applies f                                                             │
│  │   ├── test: streamFilter applies p                                                          │
│  │   ├── test: streamTake takes n                                                              │
│  │   ├── test: iterate creates infinite                                                        │
│  │   ├── test: repeat creates infinite                                                         │
│  │   ├── test: cycle cycles list                                                               │
│  │   └── test: emits stream-force event                                                        │
│  │                                                                                              │
│  ├── LogicManager.test.ts              # assertFact(), query(), prove(), unify()              │
│  │   ├── test: assertFact adds fact                                                            │
│  │   ├── test: addRule adds rule                                                               │
│  │   ├── test: getFacts returns facts                                                          │
│  │   ├── test: query executes query                                                            │
│  │   ├── test: prove does backward chain                                                       │
│  │   ├── test: closedWorldQuery uses CWA                                                       │
│  │   ├── test: unify matches pattern                                                           │
│  │   ├── test: and combines queries                                                            │
│  │   ├── test: or alternatives                                                                 │
│  │   ├── test: not negates                                                                     │
│  │   ├── test: retrieve finds matching                                                         │
│  │   ├── test: infer derives new                                                               │
│  │   ├── test: emits fact-queried event                                                        │
│  │   └── test: emits rule-applied event                                                        │
│  │                                                                                              │
│  ├── MacroManager.test.ts              # define(), expand1(), expandAll(), gensym()           │
│  │   ├── test: define creates macro                                                            │
│  │   ├── test: isMacro checks existence                                                        │
│  │   ├── test: getMacro retrieves macro                                                        │
│  │   ├── test: expand1 expands once                                                            │
│  │   ├── test: expandAll expands recursively                                                   │
│  │   ├── test: gensym generates unique                                                         │
│  │   ├── test: quasiquote processes template                                                   │
│  │   ├── test: unquote inserts value                                                           │
│  │   ├── test: splice inserts list                                                             │
│  │   └── test: emits macro-expand event                                                        │
│  │                                                                                              │
└────────────────────────────────────────────────────────────────────────────────────────────────┘
```

### Provider Tests

```
tests/runtime/providers/
├── MemoryStateProvider.test.ts
├── FilesystemStateProvider.test.ts
├── FilesystemSessionProvider.test.ts
├── MemorySnapshotProvider.test.ts
├── FilesystemSnapshotProvider.test.ts
├── InMemoryReceiptProvider.test.ts
├── FilesystemReceiptProvider.test.ts
├── MemoryTraceProvider.test.ts
├── StreamingTraceProvider.test.ts
├── MemoryArtifactProvider.test.ts
├── FilesystemArtifactProvider.test.ts
├── MemoryProvenanceProvider.test.ts
├── MemoryLogicProvider.test.ts
├── DefaultExpertProvider.test.ts
├── MemoryStreamProvider.test.ts
└── LocalSecurityProvider.test.ts
```

### Event System Tests

```
tests/runtime/events/
├── RuntimeEventEmitter.test.ts
│   ├── test: on subscribes to event
│   ├── test: emit fires all handlers
│   ├── test: off removes handler
│   ├── test: once fires only once
│   ├── test: unsubscribe works
│   └── test: type safety for all events
```

### Integration Tests

```
tests/runtime/integration/
├── hooks.test.ts                    # Event subscription across subsystems
├── regression.test.ts               # Compare with OmegaLLM-ref oracle
├── protocol.test.ts                 # nREPL protocol end-to-end
├── fixpoint.test.ts                 # Fixpoint convergence scenarios
├── transactions.test.ts             # Transaction commit/rollback
├── amb-backtracking.test.ts         # AMB search complete scenarios
├── lazy-streams.test.ts             # Stream forcing scenarios
├── logic-programming.test.ts        # Query/prove scenarios
├── expert-compilation.test.ts       # Intent compilation scenarios
├── macro-expansion.test.ts          # Macro expansion scenarios
├── concurrent-fibers.test.ts        # Concurrency scenarios
├── cross-subsystem.test.ts          # Multi-subsystem workflows
└── full-session.test.ts             # Complete session lifecycle
```

### E2E CLI Tests

```
tests/e2e/cli/
├── eval.test.ts                     # :eval equivalent via runtime.eval()
├── loadfile.test.ts                 # :loadfile equivalent
├── debug.test.ts                    # :debug/:step/:run/:stop
├── breakpoints.test.ts              # :break/:breaks/:delbreak/:toggle
├── snapshots.test.ts                # :save/:restore/:snapshots/:export
├── history.test.ts                  # :back/:history/:record/:dump/:replay
├── session.test.ts                  # :session *
├── inspect.test.ts                  # :env/:stack/:frame/:control/:defs
├── llm.test.ts                      # :ask/:traces/:trace
├── opr.test.ts                      # :opr-list/:opr-run/:opr-receipts/:opr-verify
├── image.test.ts                    # :image save/load
├── budget.test.ts                   # :budget
├── memo.test.ts                     # :memo-stats/:memo-clear
└── repl-parity.test.ts              # Full REPL command parity
```

### Demo Tests

```
tests/demos/
├── __runner__.test.ts               # Auto-discovery test runner
├── basic-eval.test.ts
├── debug-step-through.test.ts
├── breakpoints.test.ts
├── snapshots.test.ts
├── session-persistence.test.ts
├── time-travel.test.ts
├── llm-ask.test.ts
├── opr-kernels.test.ts
├── hooks-events.test.ts
├── custom-providers.test.ts
├── protocol-server.test.ts
├── amb-backtracking.test.ts
├── lazy-streams.test.ts
├── logic-programming.test.ts
├── expert-compilation.test.ts
├── macro-expansion.test.ts
├── conditions-restarts.test.ts
├── fixpoint-convergence.test.ts
├── transactions.test.ts
├── provenance-tracking.test.ts
├── budget-limits.test.ts
├── security-caps.test.ts
├── concurrent-fibers.test.ts
└── artifacts-memoization.test.ts
```

---

## Test Execution Commands

```bash
# Unit tests (fast, no LLM)
npm run test:unit

# Integration tests
npm run test:integration

# E2E CLI tests
npm run test:e2e

# All demo tests in mock mode (fast)
npm run test:demos

# All demo tests with real LLM (slow, requires API keys)
OMEGA_LLM_MODE=live npm run test:demos

# Single demo test
npm run test:demo 01-basic-eval

# Run specific demo interactively
npm run demo 01-basic-eval

# Record LLM responses for mock mode
OMEGA_RECORD_MODE=true OMEGA_LLM_MODE=live npm run demo 01-basic-eval

# Full test suite
npm run test

# Coverage report
npm run test:coverage
```

---

## AUDIT REPORT: Critical Issues & Fixes for First-Try Success

This section documents all issues found during rigorous audit and the fixes required.

### ISSUE 1: Missing Type Definitions

The following types are REFERENCED but not in the deliverables list:

```
┌──────────────────────────────────────────────────────────────────────────────────────────┐
│                          MISSING TYPE DEFINITIONS                                         │
├────────────────────────┬─────────────────────────────────┬────────────────────────────────┤
│ Type                   │ Where Referenced                │ Fix                            │
├────────────────────────┼─────────────────────────────────┼────────────────────────────────┤
│ Effect                 │ EffectHandler signature         │ Import from src/core/effects/  │
│ EffectContext          │ EffectHandler signature         │ ADD: types/EffectContext.ts    │
│ EffectResult           │ EffectHandler return            │ ADD: types/EffectResult.ts     │
│ MachineSnapshot        │ SnapshotProvider, inspect       │ ADD: types/MachineSnapshot.ts  │
│ SessionData            │ SessionProvider.load()          │ ADD: types/SessionData.ts      │
│ SessionTrace           │ session.getTrace()              │ ADD: types/SessionTrace.ts     │
│ Checkpoint             │ session.getCheckpoints()        │ ADD: types/Checkpoint.ts       │
│ VerifyResult           │ ReceiptProvider.verify()        │ ADD: types/VerifyResult.ts     │
│ AgentResult            │ llm.ask() return                │ ADD to types/LLMTypes.ts       │
│ LLMTraceSummary        │ llm.getTraces() return          │ ADD to types/LLMTypes.ts       │
│ LLMTraceDetail         │ llm.getTrace() return           │ ADD to types/LLMTypes.ts       │
│ OprLLMAdapter          │ RuntimeConfig.opr               │ ADD to types/OprTypes.ts       │
│ KernelRegistry         │ RuntimeConfig.opr               │ ADD to types/OprTypes.ts       │
│ Value                  │ inspect.env() return            │ Import from src/core/eval/     │
│ Definition             │ inspect.defs() return           │ ADD: types/Definition.ts       │
│ FrameDetail            │ inspect.frame() return          │ ADD to types/StackFrame.ts     │
│ Store                  │ StateProvider.createStore()     │ Import from src/core/eval/     │
│ Profile                │ GovernanceProvider              │ Import from src/core/governance│
│ CapSet                 │ GovernanceProvider              │ Import from src/core/governance│
│ BudgetTracker          │ GovernanceProvider              │ Import from src/core/governance│
│ Unsubscribe            │ on() return type                │ ADD to events/RuntimeEvent.ts  │
│ RuntimeEventHandler    │ on() parameter                  │ ADD to events/RuntimeEvent.ts  │
│ LLMToolCallEvent       │ events (line 1694)              │ ADD: events/LLMToolCallEvent.ts│
└────────────────────────┴─────────────────────────────────┴────────────────────────────────┘
```

**FIX**: Add these files to the deliverables in 022-types and add re-exports in types/index.ts for core types.

---

### ISSUE 2: Missing Mock Providers for Testing

**Problem**: To unit test subsystems, we need mock providers. Only MockSessionProvider listed.

**FIX**: Add to 022-providers deliverables:
```
src/runtime/providers/mocks/
├── index.ts
├── MockStateProvider.ts
├── MockSnapshotProvider.ts
├── MockReceiptProvider.ts
├── MockTraceProvider.ts
├── MockGovernanceProvider.ts
├── MockEffectHandlerRegistry.ts
└── MockEventEmitter.ts
```

---

### ISSUE 3: Dependency Injection Pattern

For testability, each subsystem MUST receive dependencies via constructor:

```
┌──────────────────────────────────────────────────────────────────────────────────────────┐
│                          DEPENDENCY INJECTION TABLE                                       │
├────────────────────────┬─────────────────────────────────────────────────────────────────┤
│ Subsystem              │ Constructor Dependencies                                        │
├────────────────────────┼─────────────────────────────────────────────────────────────────┤
│ ExecutionEngine        │ StateProvider, EffectHandlerRegistry, GovernanceProvider,      │
│                        │ RuntimeEventEmitter                                             │
├────────────────────────┼─────────────────────────────────────────────────────────────────┤
│ StateInspector         │ (none - receives State as method parameter)                    │
├────────────────────────┼─────────────────────────────────────────────────────────────────┤
│ BreakpointManager      │ (none - pure in-memory, receives State in check() method)      │
├────────────────────────┼─────────────────────────────────────────────────────────────────┤
│ SnapshotManager        │ SnapshotProvider, StateProvider                                │
├────────────────────────┼─────────────────────────────────────────────────────────────────┤
│ HistoryManager         │ TraceProvider                                                  │
├────────────────────────┼─────────────────────────────────────────────────────────────────┤
│ SessionManager         │ SessionProvider, RuntimeEventEmitter                           │
├────────────────────────┼─────────────────────────────────────────────────────────────────┤
│ DebugSubsystem         │ ExecutionEngine, BreakpointManager, HistoryManager,            │
│                        │ RuntimeEventEmitter                                             │
├────────────────────────┼─────────────────────────────────────────────────────────────────┤
│ LLMIntegration         │ RuntimeEventEmitter (LLMAdapter set via configure())           │
├────────────────────────┼─────────────────────────────────────────────────────────────────┤
│ OprIntegration         │ ReceiptProvider, RuntimeEventEmitter                           │
├────────────────────────┼─────────────────────────────────────────────────────────────────┤
│ OmegaRuntime           │ RuntimeConfig (creates all above internally)                   │
└────────────────────────┴─────────────────────────────────────────────────────────────────┘
```

---

### ISSUE 4: Integration Test Matrix

```
┌──────────────────────────────────────────────────────────────────────────────────────────┐
│                          INTEGRATION TEST MATRIX                                          │
├───────────────────────────────┬───────────────────────────────────────────────────────────┤
│ Test                          │ Verifies                                                  │
├───────────────────────────────┼───────────────────────────────────────────────────────────┤
│ step events fire              │ on('step') fires for every CESK step                      │
│ effect events fire            │ on('effect') fires when inferOp detected                  │
│ done event fires              │ on('done') fires with result when eval completes          │
│ error event fires             │ on('error') fires when exception thrown                   │
│ before-llm event fires        │ on('before-llm') fires before every LLM API call          │
│ after-llm event fires         │ on('after-llm') fires after every LLM response            │
│ breakpoint-hit event fires    │ on('breakpoint-hit') fires when breakpoint matches        │
│ unsubscribe works             │ after unsub(), handler no longer called                   │
│ once() fires only once        │ once() handler called exactly once then removed           │
├───────────────────────────────┼───────────────────────────────────────────────────────────┤
│ REGRESSION TESTS              │ Compare with OmegaLLM-ref behavioral oracle               │
├───────────────────────────────┼───────────────────────────────────────────────────────────┤
│ Basic arithmetic              │ (+ 1 2) → Result: 3                                       │
│ Define and use                │ (define x 5) x → Result: 5                                │
│ Lambda evaluation             │ ((lambda (x) x) 42) → Result: 42                          │
│ Recursive factorial           │ (factorial 5) → Result: 120                               │
│ InferOp fires                 │ (infer.step.classify) → Effect event                      │
│ Debug step works              │ debug.load + step(5) → Step count = 5                     │
│ Breakpoint stops              │ break at step 3 + run() → Stops at step 3                 │
│ Snapshot save/restore         │ save, mutate, restore → State restored                    │
│ Session save/load             │ save + reset + load → State restored from file            │
└───────────────────────────────┴───────────────────────────────────────────────────────────┘
```

---

### ISSUE 5: Edge Cases Coverage

```
┌──────────────────────────────────────────────────────────────────────────────────────────┐
│                          EDGE CASE COVERAGE                                               │
├──────────────────────────────────────────────────────────────────────────────────────────┤
│ EXECUTION                                                                                │
│   □ Empty string input → error                                                           │
│   □ Syntax error in code → error event, no crash                                         │
│   □ Infinite loop → maxSteps limit, timeout                                              │
│   □ Stack overflow → error event with context                                            │
│   □ Effect handler throws → error event, recoverable=false                               │
├──────────────────────────────────────────────────────────────────────────────────────────┤
│ DEBUGGING                                                                                │
│   □ debug.step(0) → no-op, returns current state                                         │
│   □ debug.step(-1) → error (invalid)                                                     │
│   □ debug.step() when not active → error                                                 │
│   □ debug.goto() to invalid step → error                                                 │
│   □ debug.goto() to future step → error (can only go back)                               │
│   □ Multiple debug.load() → replaces previous                                            │
├──────────────────────────────────────────────────────────────────────────────────────────┤
│ BREAKPOINTS                                                                              │
│   □ Duplicate breakpoint → allowed, both fire                                            │
│   □ Remove non-existent ID → no-op (idempotent)                                          │
│   □ Toggle non-existent ID → error                                                       │
│   □ Breakpoint with invalid condition → error on add()                                   │
├──────────────────────────────────────────────────────────────────────────────────────────┤
│ SNAPSHOTS                                                                                │
│   □ Restore non-existent → error                                                         │
│   □ Save with empty name → error                                                         │
│   □ Export to unwritable path → error                                                    │
├──────────────────────────────────────────────────────────────────────────────────────────┤
│ SESSIONS                                                                                 │
│   □ Load non-existent session → error                                                    │
│   □ Fork with existing name → error or overwrite (document choice)                       │
│   □ Goto invalid checkpoint → error                                                      │
├──────────────────────────────────────────────────────────────────────────────────────────┤
│ LLM                                                                                      │
│   □ ask() when not configured → error                                                    │
│   □ LLM API timeout → error event                                                        │
│   □ Tool call infinite loop → max iterations limit                                       │
├──────────────────────────────────────────────────────────────────────────────────────────┤
│ OPR                                                                                      │
│   □ Unknown kernel name → error                                                          │
│   □ Invalid program JSON → error                                                         │
│   □ Receipt verification fails → VerifyResult.valid=false                                │
└──────────────────────────────────────────────────────────────────────────────────────────┘
```

---

### ISSUE 6: Event System Type Safety

**Required type structure**:

```typescript
// Event name literal union
type RuntimeEventName =
  | 'step' | 'effect' | 'before-llm' | 'after-llm'
  | 'breakpoint-hit' | 'error' | 'done'
  | 'session-checkpoint' | 'opr-receipt' | 'llm-tool-call';

// Event payload map
interface RuntimeEventMap {
  'step': StepEvent;
  'effect': EffectEvent;
  'before-llm': BeforeLLMEvent;
  'after-llm': AfterLLMEvent;
  'breakpoint-hit': BreakpointHitEvent;
  'error': ErrorEvent;
  'done': DoneEvent;
  'session-checkpoint': SessionCheckpointEvent;
  'opr-receipt': OprReceiptEvent;
  'llm-tool-call': LLMToolCallEvent;
}

// Type-safe handler and emitter
type RuntimeEventHandler<E extends RuntimeEventName> = (event: RuntimeEventMap[E]) => void;
type Unsubscribe = () => void;
```

---

### ISSUE 7: State Ownership Model

```
┌──────────────────────────────────────────────────────────────────────────────────────────┐
│                          STATE OWNERSHIP MODEL                                            │
│                                                                                           │
│                    ExecutionEngine OWNS the state                                         │
│                    Other subsystems READ via getState()                                   │
│                    Only Snapshot/Session can REPLACE state                                │
└──────────────────────────────────────────────────────────────────────────────────────────┘

ExecutionEngine additional methods required:
  - getState(): State           // read-only view
  - getStore(): Store           // read-only view
  - getDefs(): Map<string, Val> // read-only view
  - setState(state: State): void // for restore/goto
  - serialize(): SerializedState // for session save
  - deserialize(data: SerializedState): void // for session load
```

---

### ISSUE 8: Concurrency Model

```
OmegaRuntime is SINGLE-THREADED. Only ONE mutating operation at a time.

Mutating operations:
  - eval(), debug.step(), debug.run(), debug.goto()
  - snapshots.restore(), session.load()
  - history.back(), history.replay()

If called while another mutating op is in progress:
  → Throw Error('Operation in progress')

Read operations are always safe:
  - inspect.*, breakpoints.list(), snapshots.list(), etc.
```

---

### CHECKLIST FOR FIRST-TRY SUCCESS

```
□ 1. All types from ISSUE 1 added to deliverables
□ 2. Mock providers from ISSUE 2 added to deliverables
□ 3. Dependency injection pattern applied to all subsystems (ISSUE 3)
□ 4. Integration test matrix implemented (ISSUE 4)
□ 5. Edge cases have test coverage (ISSUE 5)
□ 6. Event system is type-safe (ISSUE 6)
□ 7. State ownership model documented in code (ISSUE 7)
□ 8. Concurrency model implemented with mutex (ISSUE 8)
□ 9. OmegaLLM-ref created for behavioral oracle
□ 10. Each phase has passing unit tests before moving to next
```

---

### RECOMMENDED BUILD ORDER (Refined After Audit)

```
STAGE 1: FOUNDATION (parallel)
  [A] Types + Events + Provider Interfaces
  [A] Mock Providers
  → GATE: npm run typecheck passes

STAGE 2: CORE ENGINE (sequential)
  [B] MemoryStateProvider + unit tests
  [C] ExecutionEngine + EffectHandlerRegistry + unit tests
  → GATE: can eval() simple expr, see step/effect/done events

STAGE 3: DEBUGGING (sequential)
  [D] BreakpointManager + unit tests
  [D] HistoryManager + unit tests
  [D] SnapshotManager + unit tests
  [D] DebugSubsystem + unit tests
  → GATE: can debug.step() through code with breakpoints

STAGE 4: PERIPHERALS (parallel)
  [E] StateInspector + unit tests
  [E] SessionManager + unit tests
  [E] LLMIntegration + unit tests (ScriptedOracle)
  [E] OprIntegration + unit tests

STAGE 5: COMPOSITION
  [F] OmegaRuntime + createRuntime()
  [F] Integration tests from matrix
  → GATE: all integration tests pass

STAGE 6: WIRE (ATOMIC)
  [G] New omega-repl.ts (~300 lines)
  [G] New omega-debugger.ts (~200 lines)
  [G] New debugSession.ts
  [G] Full regression vs OmegaLLM-ref
  → GATE: same output for same input

STAGE 7: CLEANUP
  [H] Delete OmegaLLM-ref
  [H] Documentation
```

---

## FACT-CHECK ERRATA: Type Name Corrections

The following type names in the spec DO NOT match the actual codebase.
**IMPLEMENTERS MUST USE THE CORRECT NAMES.**

```
┌──────────────────────────────────────────────────────────────────────────────────────────┐
│                          FACT-CHECK ERRATA                                                │
│                    Verified against actual codebase 2026-01-29                            │
├──────────────────────────────────────────────────────────────────────────────────────────┤
│                                                                                           │
│  SPEC SAYS                     │ ACTUAL TYPE               │ LOCATION                    │
│  ═════════════════════════════════════════════════════════════════════════════════════   │
│                                                                                           │
│  Effect                        │ OpCall                    │ src/core/effects/opcall.ts  │
│  BudgetTracker                 │ Budget                    │ src/core/governance/budgets │
│  Receipt                       │ OprReceipt                │ src/core/opr/types.ts       │
│  Value (generic)               │ Val                       │ src/core/eval/values.ts     │
│                                                                                           │
│  oracle/openai.ts              │ oracle/adapters/openaiAdapter.ts                        │
│  oracle/anthropic.ts           │ oracle/adapters/anthropicAdapter.ts                     │
│                                                                                           │
├──────────────────────────────────────────────────────────────────────────────────────────┤
│                                                                                           │
│  VERIFIED CORE TYPES (USE THESE EXACT NAMES):                                            │
│  ═════════════════════════════════════════════                                           │
│                                                                                           │
│  Val                           │ Union type for all values │ src/core/eval/values.ts     │
│  Store                         │ Interface for CESK store  │ src/core/eval/store.ts      │
│  COWStore                      │ Copy-on-write Store impl  │ src/core/eval/store.ts      │
│  State                         │ CESK machine state        │ src/core/eval/machine.ts    │
│  Control                       │ Expr | Val                │ src/core/eval/machine.ts    │
│  Frame                         │ Continuation frame        │ src/core/eval/machine.ts    │
│  StepOutcome                   │ State | Done | Op         │ src/core/eval/machine.ts    │
│  Env                           │ Environment               │ src/core/eval/env.ts        │
│                                                                                           │
│  OpCall                        │ Effect operation call     │ src/core/effects/opcall.ts  │
│  Resumption                    │ Effect resumption         │ src/core/effects/opcall.ts  │
│                                                                                           │
│  Profile                       │ Governance profile        │ src/core/governance/profile │
│  CapSet                        │ Cap[] (string array)      │ src/core/governance/caps.ts │
│  Budget                        │ Runtime budget tracking   │ src/core/governance/budgets │
│  BudgetLimits                  │ Budget limit config       │ src/core/governance/budgets │
│                                                                                           │
│  OprReceipt                    │ OPR receipt chain entry   │ src/core/opr/types.ts       │
│  KernelOutput                  │ OPR kernel result         │ src/core/opr/types.ts       │
│  ValidationResult              │ OPR validation result     │ src/core/opr/types.ts       │
│                                                                                           │
│  OracleAdapter                 │ Oracle interface          │ src/core/oracle/adapter.ts  │
│  OracleSession                 │ Session interface         │ src/core/oracle/protocol.ts │
│                                                                                           │
├──────────────────────────────────────────────────────────────────────────────────────────┤
│                                                                                           │
│  VERIFIED REPL COMMANDS (ALL EXIST):                                                     │
│  ═══════════════════════════════════                                                     │
│                                                                                           │
│  ✓ :debug (expr)        ✓ :step [N]           ✓ :run                                    │
│  ✓ :goto <N>            ✓ :trace [s] [n]      ✓ :state                                  │
│  ✓ :break step/expr/effect  ✓ :breaks         ✓ :delbreak <id>  ✓ :toggle <id>          │
│  ✓ :save <name>         ✓ :restore <name>     ✓ :snapshots                              │
│  ✓ :session list/save/load/fork/goto/trace/checkpoints/resume                           │
│  ✓ :env [name]          ✓ :defs               ✓ :stack          ✓ :frame <n>            │
│  ✓ :back [N]            ✓ :history [N]                                                  │
│  ✓ :ask <question>      ✓ :traces             ✓ :trace <id> [-v]                        │
│  ✓ :opr-list            ✓ :opr-run            ✓ :opr-receipts   ✓ :opr-verify           │
│  ✓ :loadfile <path>     ✓ :help               ✓ :quit                                   │
│                                                                                           │
└──────────────────────────────────────────────────────────────────────────────────────────┘
```

### IMPORT CORRECTIONS for types/index.ts

```typescript
// CORRECT re-exports from src/core/:

// Eval types
export type { Val } from '../../core/eval/values';
export type { Store, StoreAddr } from '../../core/eval/store';
export { COWStore } from '../../core/eval/store';
export type { State, Control, Frame, StepOutcome, HandlerFrame } from '../../core/eval/machine';
export type { Env } from '../../core/eval/env';

// Effect types (NOT "Effect" - it's "OpCall")
export type { OpCall, Resumption } from '../../core/effects/opcall';

// Governance types (NOT "BudgetTracker" - it's "Budget")
export type { Profile, TruthRegime, EffectOp } from '../../core/governance/profile';
export type { CapSet, Cap } from '../../core/governance/caps';
export type { Budget, BudgetLimits } from '../../core/governance/budgets';

// OPR types (NOT "Receipt" - it's "OprReceipt")
export type { OprReceipt, KernelOutput, ValidationResult } from '../../core/opr/types';

// Oracle types
export type { OracleAdapter, OracleInit } from '../../core/oracle/adapter';
```

### Runtime Type Aliases (for cleaner API)

If you want to use cleaner names in the runtime API, create aliases:

```typescript
// In src/runtime/types/aliases.ts:
import type { OpCall } from '../../core/effects/opcall';
import type { Budget } from '../../core/governance/budgets';
import type { OprReceipt } from '../../core/opr/types';
import type { Val } from '../../core/eval/values';

// Cleaner names for runtime API
export type Effect = OpCall;           // "Effect" is an alias for OpCall
export type BudgetTracker = Budget;    // "BudgetTracker" is an alias for Budget
export type Receipt = OprReceipt;      // "Receipt" is an alias for OprReceipt
export type Value = Val;               // "Value" is an alias for Val
```

---

## FINAL VERIFICATION SUMMARY

```
CODEBASE STRUCTURE VERIFIED:
  ✓ src/core/eval/ - machine.ts, machineStep.ts, store.ts, values.ts, env.ts, run.ts
  ✓ src/core/effects/ - opcall.ts, runtimeImpl.ts, capture.ts
  ✓ src/core/oracle/ - adapter.ts, portal.ts, scriptedOracle.ts, protocol.ts
  ✓ src/core/oracle/adapters/ - openaiAdapter.ts, anthropicAdapter.ts, mcpAdapter.ts
  ✓ src/core/opr/ - runtime.ts, types.ts, callbacks.ts, receipts.ts
  ✓ src/core/governance/ - profile.ts, caps.ts, budgets.ts, enforcement.ts
  ✓ src/core/session/ - writer.ts, reader.ts, types.ts, index.ts

REPL COMMANDS VERIFIED:
  ✓ All 40+ commands confirmed to exist in bin/omega-repl.ts (3163 lines)

TYPE NAMES CORRECTED:
  • Effect → OpCall (create alias if desired)
  • BudgetTracker → Budget (create alias if desired)
  • Receipt → OprReceipt (create alias if desired)
  • Value → Val (create alias if desired)
```

---

## GAP ANALYSIS: Documented Features vs Runtime Spec

**COMPREHENSIVE AUDIT AGAINST docs/ AND ARCHITECTURE/ FOLDERS**
Audited: 2026-01-29

**ALL DOCUMENTED FEATURES ARE IN_SCOPE.**
The runtime is THE unified interface that must support every documented capability.

```
┌────────────────────────────────────────────────────────────────────────────────────────────────┐
│                           GAP ANALYSIS: ARCHITECTURE vs RUNTIME                                  │
│                           ALL FEATURES ARE IN_SCOPE                                              │
├────────────────────────────────────────────────────────────────────────────────────────────────┤
│                                                                                                  │
│  FEATURE                        │ SOURCE DOC          │ RUNTIME API             │ NOTES        │
│  ═══════════════════════════════════════════════════════════════════════════════════════════    │
│                                                                                                  │
│  ━━━ CONDITIONS SYSTEM (06-CONDITIONS.md) ━━━                                                   │
│  signal, handler-bind,          │ 06-CONDITIONS.md    │ runtime.conditions.*    │ CESK exposes │
│  restart-case, invoke-restart   │                     │ on('condition', ...)    │ hooks needed │
│  Non-unwinding error handling   │                     │ conditions.signal()     │              │
│  Restart selection              │                     │ conditions.restart()    │              │
│                                                                                                  │
│  ━━━ PROVENANCE SYSTEM (22-PROVENANCE.md) ━━━                                                   │
│  Evidence capture               │ 22-PROVENANCE.md    │ runtime.provenance.*    │ EvidenceReg  │
│  Evidence verification          │                     │ provenance.capture()    │ + Graph      │
│  Staleness detection            │                     │ provenance.verify()     │              │
│  EvidenceRegistry               │                     │ provenance.isStale()    │              │
│  ProvenanceGraph                │                     │ provenance.graph()      │              │
│  Epistemic modes                │                     │ provenance.getMode()    │              │
│                                                                                                  │
│  ━━━ FACTS SYSTEM (23-FACTS.md) ━━━                                                            │
│  assert, fact?, facts           │ 23-FACTS.md         │ runtime.facts.*         │ Read + event │
│  FactStore, monotone state      │                     │ facts.all()             │ hooks        │
│  Subeval isolation              │                     │ facts.has(expr)         │              │
│                                 │                     │ facts.count()           │              │
│                                 │                     │ facts.signature()       │              │
│                                 │                     │ on('fact-asserted',...)│              │
│                                                                                                  │
│  ━━━ FIXPOINT SYSTEM (24-FIXPOINT.md) ━━━                                                      │
│  fixpoint, fixpoint/outcome     │ 24-FIXPOINT.md      │ runtime.fixpoint.*      │ Control +    │
│  Convergence detection          │                     │ fixpoint.run(body,opts) │ monitoring   │
│  Cycle detection                │                     │ fixpoint.getState()     │              │
│  State signatures               │                     │ fixpoint.signature()    │              │
│                                 │                     │ on('fixpoint-iter',...)│              │
│                                 │                     │ on('fixpoint-done',...)│              │
│                                                                                                  │
│  ━━━ ARTIFACTS/MEMOIZATION (26-ARTIFACTS.md) ━━━                                               │
│  memo, memo/auto                │ 26-ARTIFACTS.md     │ runtime.artifacts.*     │ Full cache   │
│  Content-addressed caching      │                     │ artifacts.get(key)      │ API          │
│  Dependency tracking            │                     │ artifacts.put(key,val)  │              │
│  Cache invalidation             │                     │ artifacts.stats()       │              │
│                                 │                     │ artifacts.clear()       │              │
│                                                                                                  │
│  ━━━ OUTCOMES (27-OUTCOMES.md) ━━━                                                             │
│  Structured return types        │ 27-OUTCOMES.md      │ Outcome type for ALL    │ Unify ALL    │
│  ok, proposed, nonconverged,    │                     │ returns: eval(),run()   │ returns      │
│  cycle, needs, error            │                     │ debug.step(), etc.      │              │
│  Proposals/StagedWorld          │                     │ runtime.transaction.*   │ Full support │
│                                 │                     │ transaction.propose()   │              │
│                                 │                     │ transaction.commit()    │              │
│                                 │                     │ transaction.rollback()  │              │
│                                                                                                  │
│  ━━━ BUDGET ENFORCEMENT (25-BUDGET.md) ━━━                                                     │
│  Token/cost/time/iteration      │ 25-BUDGET.md        │ runtime.budget.*        │ Full budget  │
│  limits                         │                     │ budget.remaining(type)  │ API          │
│  with-budget scope              │                     │ budget.hasRemaining()   │              │
│  budget/remaining, has-rem?     │                     │ budget.consume(type,n)  │              │
│                                 │                     │ budget.report()         │              │
│                                 │                     │ on('budget-warning',...)│              │
│                                 │                     │ on('budget-exceeded',..)│              │
│                                                                                                  │
│  ━━━ SECURITY/CAPABILITIES (21-SECURITY.md) ━━━                                                │
│  Capability-based security      │ 21-SECURITY.md      │ runtime.security.*      │ Full caps    │
│  Sandbox modes                  │                     │ security.checkCap(cap)  │ + audit      │
│  LLM output validation          │                     │ security.validateLLM()  │              │
│  Module signing                 │                     │ security.verifySig()    │              │
│  Audit logging                  │                     │ security.getAuditLog()  │              │
│                                 │                     │ on('security-event',...)│              │
│                                                                                                  │
│  ━━━ PROTOCOL (08-PROTOCOL.md) ━━━                                                             │
│  nREPL-style protocol           │ 08-PROTOCOL.md      │ runtime.protocol.*      │ Built-in     │
│  eval, info, complete, interrupt│                     │ protocol.handle(req)    │ server       │
│  Transport-agnostic server      │                     │ protocol.use(middleware)│              │
│                                 │                     │ protocol.listen(opts)   │              │
│                                                                                                  │
│  ━━━ IMAGE PERSISTENCE (USER-MANUAL--09) ━━━                                                   │
│  :image save/load               │ USER-MANUAL--09     │ runtime.snapshots.*     │ Full image   │
│                                 │                     │ snapshots.saveImage()   │              │
│                                 │                     │ snapshots.loadImage()   │              │
│                                                                                                  │
│  ━━━ CONCURRENCY (12-CONCURRENCY.md) ━━━                                                       │
│  Async/await                    │ 12-CONCURRENCY.md   │ runtime.concurrency.*   │ Full async   │
│  await-all, await-any           │                     │ concurrency.awaitAll()  │ support      │
│  Actors                         │                     │ concurrency.spawn()     │              │
│  Channels                       │                     │ concurrency.chan()      │              │
│                                 │                     │ on('task-complete',...)│              │
│                                                                                                  │
└────────────────────────────────────────────────────────────────────────────────────────────────┘
```

### ALL Features Must Be Added to Runtime Spec

Every documented capability requires runtime API support:

#### 1. Add Outcome Types (from 27-OUTCOMES)

```typescript
// In src/runtime/types/Outcome.ts:

export type Outcome<T = Val> =
  | { tag: 'ok'; value: T; metadata: OutcomeMetadata }
  | { tag: 'proposed'; value: T; proposals: Proposal[]; metadata: OutcomeMetadata }
  | { tag: 'nonconverged'; value: T; iterations: number; reason: 'budget' | 'max-iterations'; metadata: OutcomeMetadata }
  | { tag: 'cycle'; value: T; cycleLength: number; metadata: OutcomeMetadata }
  | { tag: 'needs'; needType: NeedType; description: string; context?: Val; metadata: OutcomeMetadata }
  | { tag: 'error'; message: string; errorType: string; stacktrace?: string[]; metadata: OutcomeMetadata };

export interface OutcomeMetadata {
  iterations?: number;
  tokens?: number;
  cost?: number;
  timeMs?: number;
  signature?: string;
}

export type NeedType =
  | 'needs-evidence'
  | 'needs-reframing'
  | 'needs-primitive'
  | 'needs-clarification'
  | 'needs-approval';

export interface Proposal {
  type: 'write' | 'delete' | 'run';
  ref: string;
  content?: string;
  command?: string;
  timestamp: number;
}
```

#### 2. Add Artifact Provider (from 26-ARTIFACTS)

```typescript
// In src/runtime/providers/ArtifactProvider.ts:

export interface ArtifactKey {
  exprKey: string;
  depsFingerprint: string;
  paramsKey?: string;
}

export interface Artifact<T = Val> {
  key: ArtifactKey;
  value: T;
  metadata: {
    createdAt: number;
    accessedAt: number;
    accessCount: number;
    computeTimeMs: number;
    deps: Array<{ ref: string; fingerprint: string }>;
  };
}

export interface ArtifactProvider {
  get(key: ArtifactKey): Val | undefined;
  getByExpr(exprKey: string, currentFingerprints: Map<string, string>): Val | undefined;
  put(key: ArtifactKey, value: Val, metadata: Partial<Artifact['metadata']>): void;
  clear(): void;
  clearByExpr(exprKey: string): void;
  stats(): { count: number; capacity: number | null; hitCount: number; missCount: number };
  signature(): string;
}
```

#### 3. Expand Budget API (from 25-BUDGET)

```typescript
// Add to GovernanceProvider interface:

export interface GovernanceProvider {
  // Existing
  checkCap(cap: Cap): boolean;

  // Add Budget API
  consumeTokens(amount: number): void;
  consumeCost(amount: number): void;
  consumeIteration(): void;

  hasRemaining(type: 'tokens' | 'cost' | 'time' | 'iterations'): boolean;
  remaining(type: 'tokens' | 'cost' | 'time' | 'iterations'): number | null;
  remainingAll(): Record<'tokens' | 'cost' | 'time' | 'iterations', number | null>;

  report(): BudgetReport;
}

export interface BudgetReport {
  limits: BudgetLimits;
  consumed: Record<'tokens' | 'cost' | 'time' | 'iterations', number>;
  remaining: Record<'tokens' | 'cost' | 'time' | 'iterations', number | null>;
}
```

#### 4. Add Image Persistence (from USER-MANUAL--09)

```typescript
// Add to SnapshotManager API:

export interface SnapshotManager {
  // Existing
  save(name: string): SnapshotInfo;
  restore(name: string): State;
  list(): SnapshotInfo[];
  export(name: string, path: string): void;

  // Add Image persistence
  saveImage(path: string): void;  // Full state image to file
  loadImage(path: string): void;  // Restore from image file
}
```

#### 5. Add Facts Inspection API (from 23-FACTS)

```typescript
// Add to StateInspector API:

export interface StateInspector {
  // Existing
  env(name?: string): Record<string, Val>;
  defs(): string[];
  stack(): FrameInfo[];
  frame(n: number): FrameInfo | null;
  control(): string;

  // Add Facts inspection
  facts(): Val[];                    // Get all facts
  factCount(): number;               // Get fact count
  hasFact(expr: Val): boolean;       // Check if fact exists
  factSignature(): string;           // Get facts signature for fixpoint
}
```

### Updated Feature Checklist

Add these features to the traceability checklist:

```
│      │ ━━━ ARTIFACTS ━━━           │ runtime.artifacts.*        │       │             │ NEW        │
│ A01  │ memo cache lookup           │ artifacts.get(key)         │ 6.7   │ NOT_STARTED │            │
│ A02  │ memo cache store            │ artifacts.put(key, val)    │ 6.7   │ NOT_STARTED │            │
│ A03  │ :memo-stats                 │ artifacts.stats()          │ 6.7   │ NOT_STARTED │            │
│ A04  │ :memo-clear [expr]          │ artifacts.clear()          │ 6.7   │ NOT_STARTED │            │
│      │                              │                              │       │             │            │
│      │ ━━━ BUDGET API ━━━          │ runtime.governance.*       │       │             │ NEW        │
│ X01  │ :budget                     │ governance.report()        │ 6.6   │ NOT_STARTED │            │
│ X02  │ budget remaining check      │ governance.hasRemaining()  │ 6.6   │ NOT_STARTED │            │
│      │                              │                              │       │             │            │
│      │ ━━━ IMAGE PERSISTENCE ━━━   │ runtime.snapshots.*        │       │             │ NEW        │
│ P01  │ :image save <path>          │ snapshots.saveImage(path)  │ 4.7   │ NOT_STARTED │            │
│ P02  │ :image load <path>          │ snapshots.loadImage(path)  │ 4.7   │ NOT_STARTED │            │
│      │                              │                              │       │             │            │
│      │ ━━━ FACTS INSPECTION ━━━    │ runtime.inspect.*          │       │             │ NEW        │
│ T01  │ :facts                      │ inspect.facts()            │ 3.2   │ NOT_STARTED │            │
│ T02  │ :fact? <expr>               │ inspect.hasFact(expr)      │ 3.2   │ NOT_STARTED │            │
```

### Additional Subsystems Required

Based on the gap analysis, the following subsystems MUST be added:

#### 6. Conditions Subsystem (from 06-CONDITIONS)

```typescript
// In src/runtime/subsystems/ConditionsManager.ts:

export interface ConditionsManager {
  // Signal a condition
  signal(conditionType: string, payload: Val): void;

  // Install a handler
  bind(conditionType: string, handler: ConditionHandler): Unsubscribe;

  // Invoke a restart
  restart(restartName: string, value?: Val): void;

  // Get active restarts
  getRestarts(): RestartInfo[];

  // Get current condition (if in handler)
  getCurrentCondition(): ConditionInfo | null;
}

export interface ConditionHandler {
  (condition: ConditionInfo): HandlerAction;
}

export type HandlerAction =
  | { action: 'decline' }                     // Pass to outer handler
  | { action: 'invoke-restart'; name: string; value?: Val }
  | { action: 'return'; value: Val };

export interface RestartInfo {
  name: string;
  description?: string;
  interactive?: boolean;
}
```

#### 7. Provenance Subsystem (from 22-PROVENANCE)

```typescript
// In src/runtime/subsystems/ProvenanceManager.ts:

export interface ProvenanceManager {
  // Capture evidence
  capture(ref: string, options?: CaptureOptions): EvidenceId;

  // Verify evidence
  verify(evidenceId: EvidenceId): VerificationResult;

  // Check staleness
  isStale(evidenceId: EvidenceId): boolean;

  // Get evidence by ID
  getEvidence(id: EvidenceId): Evidence | undefined;

  // Get provenance graph
  graph(): ProvenanceGraph;

  // Set epistemic mode
  setMode(mode: EpistemicMode): void;
  getMode(): EpistemicMode;
}

export type EpistemicMode =
  | 'observed'     // Directly witnessed
  | 'measured'     // Computed/measured
  | 'derived'      // Logically derived
  | 'inferred'     // Probabilistically inferred
  | 'hypothesized' // Speculative
  | 'assumed';     // Taken as given

export interface Evidence {
  id: EvidenceId;
  ref: string;
  fingerprint: string;
  capturedAt: number;
  mode: EpistemicMode;
  metadata?: Record<string, Val>;
}
```

#### 8. Facts Subsystem (from 23-FACTS)

```typescript
// In src/runtime/subsystems/FactsManager.ts:

export interface FactsManager {
  // Get all facts
  all(): Val[];

  // Check if fact exists
  has(expr: Val): boolean;

  // Get fact with metadata
  get(expr: Val): FactInfo | undefined;

  // Get count
  count(): number;

  // Get signature for fixpoint detection
  signature(): string;

  // Query facts by pattern
  query(pattern: Val): Val[];
}

export interface FactInfo {
  expr: Val;
  evidence?: EvidenceId;
  assertedAt: number;
  assertedIn?: string;  // Namespace/subeval
}
```

#### 9. Fixpoint Subsystem (from 24-FIXPOINT)

```typescript
// In src/runtime/subsystems/FixpointManager.ts:

export interface FixpointManager {
  // Run fixpoint computation
  run(body: Val, options: FixpointOptions): Promise<Outcome>;

  // Get current fixpoint state (during execution)
  getState(): FixpointState | null;

  // Compute state signature
  signature(mode: SignatureMode): string;

  // Check if currently in fixpoint
  isInFixpoint(): boolean;
}

export interface FixpointOptions {
  maxIterations: number;
  mode: SignatureMode;
  detectCycles: boolean;
}

export type SignatureMode =
  | 'facts'
  | 'facts+bindings'
  | 'facts+world'
  | 'facts+bindings+world'
  | 'facts+bindings+world+artifacts';

export interface FixpointState {
  iteration: number;
  maxIterations: number;
  signatures: string[];
  lastValue: Val | null;
}
```

#### 10. Transaction Subsystem (from 27-OUTCOMES)

```typescript
// In src/runtime/subsystems/TransactionManager.ts:

export interface TransactionManager {
  // Start a transaction
  begin(): TransactionId;

  // Propose a world change (staged, not committed)
  propose(proposal: Proposal): void;

  // Get pending proposals
  getProposals(): Proposal[];

  // Commit all proposals
  commit(): Promise<void>;

  // Rollback all proposals
  rollback(): void;

  // Check if in transaction
  isInTransaction(): boolean;

  // Get current transaction ID
  getCurrentTransaction(): TransactionId | null;
}

export interface Proposal {
  type: 'write' | 'delete' | 'run';
  ref: string;
  content?: string;
  command?: string;
  timestamp: number;
}
```

#### 11. Security Subsystem (from 21-SECURITY)

```typescript
// In src/runtime/subsystems/SecurityManager.ts:

export interface SecurityManager {
  // Check capability
  checkCap(cap: Cap): boolean;
  requireCap(cap: Cap): void;  // Throws if missing

  // Get all capabilities
  getCaps(): CapSet;

  // Validate LLM output
  validateLLMOutput(output: string): ValidationResult;

  // Verify module signature
  verifySignature(module: string, signature: string): boolean;

  // Get audit log
  getAuditLog(options?: AuditLogOptions): AuditEntry[];

  // Sandbox mode
  getSandboxMode(): SandboxMode;
  setSandboxMode(mode: SandboxMode): void;
}

export type SandboxMode = 'none' | 'basic' | 'strict' | 'airgap';

export interface AuditEntry {
  timestamp: number;
  event: string;
  details: Record<string, Val>;
  outcome: 'allowed' | 'denied' | 'warning';
}
```

#### 12. Protocol Subsystem (from 08-PROTOCOL)

```typescript
// In src/runtime/subsystems/ProtocolServer.ts:

export interface ProtocolServer {
  // Handle a protocol request
  handle(request: ProtocolRequest): Promise<ProtocolResponse>;

  // Add middleware
  use(middleware: ProtocolMiddleware): void;

  // Start listening (transport-agnostic)
  listen(options: ListenOptions): Promise<void>;

  // Stop server
  close(): Promise<void>;
}

export interface ProtocolRequest {
  id: string;
  op: string;
  session?: string;
  [key: string]: any;
}

export interface ProtocolResponse {
  id: string;
  status: string[];
  [key: string]: any;
}

export type ListenOptions =
  | { transport: 'stdio' }
  | { transport: 'websocket'; port: number }
  | { transport: 'http'; port: number };
```

#### 13. Concurrency Subsystem (from 12-CONCURRENCY)

```typescript
// In src/runtime/subsystems/ConcurrencyManager.ts:

export interface ConcurrencyManager {
  // Wait for all promises
  awaitAll<T>(promises: Promise<T>[]): Promise<T[]>;

  // Wait for first promise
  awaitAny<T>(promises: Promise<T>[]): Promise<T>;

  // Spawn actor
  spawn(behavior: Val, initialState: Val): ActorId;

  // Send message to actor
  send(actor: ActorId, message: Val): void;

  // Ask actor (request-response)
  ask(actor: ActorId, message: Val): Promise<Val>;

  // Create channel
  chan<T>(): Channel<T>;

  // Timeout wrapper
  withTimeout<T>(ms: number, promise: Promise<T>): Promise<T>;

  // Cancel a task
  cancel(taskId: TaskId): void;
}

export interface Channel<T> {
  put(value: T): Promise<void>;
  take(): Promise<T>;
  close(): void;
}
```

#### 14. AMB/Backtracking Subsystem (from USER-MANUAL--05, USER-MANUAL--26)

```typescript
// In src/runtime/subsystems/AmbManager.ts:

export interface AmbManager {
  // Core AMB primitives
  choose<T>(alternatives: Array<() => T>): T;  // amb.choose effect
  fail(reason?: string): never;                 // amb.fail effect
  require(condition: boolean): void;            // Constraint - fails if false

  // Search control
  allSolutions<T>(thunk: () => T): T[];         // Collect ALL satisfying assignments
  firstSolution<T>(thunk: () => T): T | null;   // Get first solution
  directedAmb<T>(prompt: string, constraint: (x: T) => boolean): T; // LLM-guided

  // State management
  getChoicePoint(): ChoicePoint | null;
  getBacktrackStack(): ChoicePoint[];
  resetSearch(): void;
}

export interface ChoicePoint {
  id: string;
  alternatives: Array<() => Val>;
  currentIndex: number;
  state: MachineSnapshot;
}
```

#### 15. Lazy Streams Subsystem (from USER-MANUAL--07, USER-MANUAL--23)

```typescript
// In src/runtime/subsystems/StreamsManager.ts:

export interface StreamsManager {
  // Stream construction
  listToStream(list: Val[]): LazyStream;           // list->stream
  consStream(head: Val, tailThunk: () => LazyStream): LazyStream; // cons-stream
  emptyStream(): LazyStream;                       // the-empty-stream

  // Stream access
  streamCar(s: LazyStream): Val;                   // First element (forced)
  streamCdr(s: LazyStream): LazyStream;            // Rest (lazy)
  streamNull(s: LazyStream): boolean;              // Check if empty
  streamToList(s: LazyStream, n: number): Val[];   // Force n elements

  // Stream operations
  streamMap(f: (x: Val) => Val, s: LazyStream): LazyStream;      // Lazy map
  streamFilter(p: (x: Val) => boolean, s: LazyStream): LazyStream; // Lazy filter
  streamTake(s: LazyStream, n: number): LazyStream;              // Take first n
  streamFlatMap(f: (x: Val) => LazyStream, s: LazyStream): LazyStream; // Flat map

  // Infinite streams
  iterate(f: (x: Val) => Val, seed: Val): LazyStream;    // Infinite iteration
  repeat(value: Val): LazyStream;                        // Infinite repetition
  cycle(list: Val[]): LazyStream;                        // Cycle through list
}

export interface LazyStream {
  head: Val | (() => Val);  // Value or thunk
  tail: LazyStream | (() => LazyStream) | null;
  forced: boolean;
}
```

#### 16. Logic Programming Subsystem (from USER-MANUAL--27)

```typescript
// In src/runtime/subsystems/LogicManager.ts:

export interface LogicManager {
  // Semantic database
  assertFact(fact: string): void;                    // Assert natural language fact
  addRule(condition: string, conclusion: string): void; // Add semantic rule
  getFacts(): string[];                              // Get all facts

  // Querying
  query(question: string): QueryResult;              // Query the database
  prove(goal: string): ProofResult;                  // Backward chaining proof
  closedWorldQuery(q: string): Val | false;          // Closed world assumption

  // Semantic unification
  unify(pattern: string, text: string): BindingFrame | null; // Semantic pattern matching
  extendFrame(pattern: string, text: string, frame: BindingFrame): BindingFrame | null;

  // Compound queries
  and(q1: string, q2: string): QueryResult;          // Semantic AND
  or(q1: string, q2: string): QueryResult;           // Semantic OR
  not(q: string): QueryResult;                       // Negation as failure

  // Inference vs retrieval
  retrieve(term: string): string[];                  // Find matching facts
  infer(question: string): string;                   // Derive new information
}

export interface QueryResult {
  answer: string;
  confidence?: number;
  source: 'retrieval' | 'inference' | 'unknown';
}

export interface ProofResult {
  proved: boolean;
  explanation?: string;
  steps?: string[];
}

export interface BindingFrame {
  bindings: Record<string, Val>;
}
```

#### 17. Expert System Subsystem (from 29-EXPERTS.md)

```typescript
// In src/runtime/subsystems/ExpertManager.ts:

export interface ExpertManager {
  // Expert role management
  registerRole(role: ExpertRole): void;
  getRole(id: string): ExpertRole | undefined;
  listRoles(): ExpertRole[];

  // Intent compilation
  compileIntent(text: string, role: ExpertRole, mode: OutputMode): Promise<Val>;
  validateOutput(expr: Val, mode: OutputMode): ValidationResult;

  // Parallel execution
  runParallel(roles: string[], task: string, aggregation: AggregationMode): Promise<Val[]>;
  batch(intents: string[], role: string): Promise<Val[]>;
}

export interface ExpertRole {
  id: string;
  name: string;
  expertise: string[];
  metaExpertise: string;
  reasoningStyle: string;
}

export type OutputMode = 'REPORT' | 'PLAN' | 'PROGRAM' | 'ANALYSIS';
export type AggregationMode = 'first' | 'vote' | 'merge' | 'all';

export interface ValidationResult {
  valid: boolean;
  errors?: string[];
}
```

#### 18. Macro System Subsystem (from 11-MACROS.md)

```typescript
// In src/runtime/subsystems/MacroManager.ts:

export interface MacroManager {
  // Macro definition
  define(name: string, params: string[], body: Val): void;  // defmacro
  isMacro(name: string): boolean;
  getMacro(name: string): Macro | undefined;

  // Expansion
  expand1(expr: Val): Val;                 // macroexpand-1
  expandAll(expr: Val): Val;               // macroexpand (recursive)

  // Hygiene
  gensym(prefix?: string): Symbol;         // Generate unique symbol

  // Quasiquote support
  quasiquote(template: Val): Val;          // ` processing
  unquote(expr: Val): Val;                 // , processing
  splice(expr: Val): Val[];                // ,@ processing
}

export interface Macro {
  type: 'macro';
  name: string;
  params: string[];
  restParam?: string;
  body: Val;
  env: EnvSnapshot;
}
```

### Updated Feature Checklist (COMPLETE)

Add ALL these features to the traceability checklist:

```
│      │ ━━━ CONDITIONS ━━━          │ runtime.conditions.*       │       │             │ NEW        │
│ CD01 │ signal condition            │ conditions.signal()        │ 7.1   │ NOT_STARTED │            │
│ CD02 │ bind handler                │ conditions.bind()          │ 7.1   │ NOT_STARTED │            │
│ CD03 │ invoke restart              │ conditions.restart()       │ 7.1   │ NOT_STARTED │            │
│ CD04 │ on('condition', ...)        │ RuntimeEventEmitter        │ 7.1   │ NOT_STARTED │ Hook       │
│      │                              │                              │       │             │            │
│      │ ━━━ PROVENANCE ━━━          │ runtime.provenance.*       │       │             │ NEW        │
│ PV01 │ capture evidence            │ provenance.capture()       │ 7.2   │ NOT_STARTED │            │
│ PV02 │ verify evidence             │ provenance.verify()        │ 7.2   │ NOT_STARTED │            │
│ PV03 │ check staleness             │ provenance.isStale()       │ 7.2   │ NOT_STARTED │            │
│ PV04 │ get graph                   │ provenance.graph()         │ 7.2   │ NOT_STARTED │            │
│ PV05 │ epistemic mode              │ provenance.setMode()       │ 7.2   │ NOT_STARTED │            │
│      │                              │                              │       │             │            │
│      │ ━━━ FACTS ━━━               │ runtime.facts.*            │       │             │ NEW        │
│ FA01 │ :facts                      │ facts.all()                │ 7.3   │ NOT_STARTED │            │
│ FA02 │ :fact? <expr>               │ facts.has(expr)            │ 7.3   │ NOT_STARTED │            │
│ FA03 │ fact count                  │ facts.count()              │ 7.3   │ NOT_STARTED │            │
│ FA04 │ fact signature              │ facts.signature()          │ 7.3   │ NOT_STARTED │            │
│ FA05 │ on('fact-asserted', ...)    │ RuntimeEventEmitter        │ 7.3   │ NOT_STARTED │ Hook       │
│      │                              │                              │       │             │            │
│      │ ━━━ FIXPOINT ━━━            │ runtime.fixpoint.*         │       │             │ NEW        │
│ FX01 │ fixpoint run                │ fixpoint.run(body, opts)   │ 7.4   │ NOT_STARTED │            │
│ FX02 │ fixpoint state              │ fixpoint.getState()        │ 7.4   │ NOT_STARTED │            │
│ FX03 │ state signature             │ fixpoint.signature()       │ 7.4   │ NOT_STARTED │            │
│ FX04 │ on('fixpoint-iter', ...)    │ RuntimeEventEmitter        │ 7.4   │ NOT_STARTED │ Hook       │
│ FX05 │ on('fixpoint-done', ...)    │ RuntimeEventEmitter        │ 7.4   │ NOT_STARTED │ Hook       │
│      │                              │                              │       │             │            │
│      │ ━━━ TRANSACTIONS ━━━        │ runtime.transaction.*      │       │             │ NEW        │
│ TX01 │ begin transaction           │ transaction.begin()        │ 7.5   │ NOT_STARTED │            │
│ TX02 │ propose change              │ transaction.propose()      │ 7.5   │ NOT_STARTED │            │
│ TX03 │ commit                      │ transaction.commit()       │ 7.5   │ NOT_STARTED │            │
│ TX04 │ rollback                    │ transaction.rollback()     │ 7.5   │ NOT_STARTED │            │
│      │                              │                              │       │             │            │
│      │ ━━━ SECURITY ━━━            │ runtime.security.*         │       │             │ NEW        │
│ SC01 │ check capability            │ security.checkCap()        │ 7.6   │ NOT_STARTED │            │
│ SC02 │ validate LLM output         │ security.validateLLM()     │ 7.6   │ NOT_STARTED │            │
│ SC03 │ verify signature            │ security.verifySig()       │ 7.6   │ NOT_STARTED │            │
│ SC04 │ audit log                   │ security.getAuditLog()     │ 7.6   │ NOT_STARTED │            │
│ SC05 │ sandbox mode                │ security.getSandboxMode()  │ 7.6   │ NOT_STARTED │            │
│ SC06 │ on('security-event', ...)   │ RuntimeEventEmitter        │ 7.6   │ NOT_STARTED │ Hook       │
│      │                              │                              │       │             │            │
│      │ ━━━ PROTOCOL ━━━            │ runtime.protocol.*         │       │             │ NEW        │
│ PR01 │ handle request              │ protocol.handle(req)       │ 7.7   │ NOT_STARTED │            │
│ PR02 │ add middleware              │ protocol.use(mw)           │ 7.7   │ NOT_STARTED │            │
│ PR03 │ listen                      │ protocol.listen(opts)      │ 7.7   │ NOT_STARTED │            │
│      │                              │                              │       │             │            │
│      │ ━━━ CONCURRENCY ━━━         │ runtime.concurrency.*      │       │             │ NEW        │
│ CC01 │ await-all                   │ concurrency.awaitAll()     │ 7.8   │ NOT_STARTED │            │
│ CC02 │ await-any                   │ concurrency.awaitAny()     │ 7.8   │ NOT_STARTED │            │
│ CC03 │ spawn actor                 │ concurrency.spawn()        │ 7.8   │ NOT_STARTED │            │
│ CC04 │ channel operations          │ concurrency.chan()         │ 7.8   │ NOT_STARTED │            │
│ CC05 │ timeout                     │ concurrency.withTimeout()  │ 7.8   │ NOT_STARTED │            │
│      │                              │                              │       │             │            │
│      │ ━━━ ARTIFACTS ━━━           │ runtime.artifacts.*        │       │             │ NEW        │
│ AR01 │ memo cache lookup           │ artifacts.get(key)         │ 6.7   │ NOT_STARTED │            │
│ AR02 │ memo cache store            │ artifacts.put(key, val)    │ 6.7   │ NOT_STARTED │            │
│ AR03 │ :memo-stats                 │ artifacts.stats()          │ 6.7   │ NOT_STARTED │            │
│ AR04 │ :memo-clear [expr]          │ artifacts.clear()          │ 6.7   │ NOT_STARTED │            │
│      │                              │                              │       │             │            │
│      │ ━━━ BUDGET ━━━              │ runtime.budget.*           │       │             │ NEW        │
│ BG01 │ :budget                     │ budget.report()            │ 6.6   │ NOT_STARTED │            │
│ BG02 │ budget remaining            │ budget.remaining(type)     │ 6.6   │ NOT_STARTED │            │
│ BG03 │ budget consume              │ budget.consume(type, n)    │ 6.6   │ NOT_STARTED │            │
│ BG04 │ on('budget-warning', ...)   │ RuntimeEventEmitter        │ 6.6   │ NOT_STARTED │ Hook       │
│ BG05 │ on('budget-exceeded', ...)  │ RuntimeEventEmitter        │ 6.6   │ NOT_STARTED │ Hook       │
│      │                              │                              │       │             │            │
│      │ ━━━ IMAGE PERSISTENCE ━━━   │ runtime.snapshots.*        │       │             │ NEW        │
│ IM01 │ :image save <path>          │ snapshots.saveImage(path)  │ 4.7   │ NOT_STARTED │            │
│ IM02 │ :image load <path>          │ snapshots.loadImage(path)  │ 4.7   │ NOT_STARTED │            │
│      │                              │                              │       │             │            │
│      │ ━━━ AMB/BACKTRACKING ━━━    │ runtime.amb.*              │       │             │ NEW        │
│ AB01 │ amb.choose                   │ amb.choose(alternatives)   │ 7.9   │ NOT_STARTED │            │
│ AB02 │ amb.fail                     │ amb.fail(reason)           │ 7.9   │ NOT_STARTED │            │
│ AB03 │ require constraint           │ amb.require(condition)     │ 7.9   │ NOT_STARTED │            │
│ AB04 │ all-solutions                │ amb.allSolutions(thunk)    │ 7.9   │ NOT_STARTED │            │
│ AB05 │ directed-amb (LLM-guided)    │ amb.directedAmb(prompt,c)  │ 7.9   │ NOT_STARTED │            │
│ AB06 │ on('amb-choose', ...)        │ RuntimeEventEmitter        │ 7.9   │ NOT_STARTED │ Hook       │
│ AB07 │ on('amb-fail', ...)          │ RuntimeEventEmitter        │ 7.9   │ NOT_STARTED │ Hook       │
│      │                              │                              │       │             │            │
│      │ ━━━ LAZY STREAMS ━━━        │ runtime.streams.*          │       │             │ NEW        │
│ ST01 │ list->stream                 │ streams.listToStream(list) │ 7.10  │ NOT_STARTED │            │
│ ST02 │ cons-stream                  │ streams.consStream(h,t)    │ 7.10  │ NOT_STARTED │            │
│ ST03 │ stream-car                   │ streams.streamCar(s)       │ 7.10  │ NOT_STARTED │            │
│ ST04 │ stream-cdr                   │ streams.streamCdr(s)       │ 7.10  │ NOT_STARTED │            │
│ ST05 │ stream-map                   │ streams.streamMap(f,s)     │ 7.10  │ NOT_STARTED │            │
│ ST06 │ stream-filter                │ streams.streamFilter(p,s)  │ 7.10  │ NOT_STARTED │            │
│ ST07 │ stream->list                 │ streams.streamToList(s,n)  │ 7.10  │ NOT_STARTED │            │
│ ST08 │ on('stream-force', ...)      │ RuntimeEventEmitter        │ 7.10  │ NOT_STARTED │ Hook       │
│      │                              │                              │       │             │            │
│      │ ━━━ LOGIC PROGRAMMING ━━━   │ runtime.logic.*            │       │             │ NEW        │
│ LG01 │ assert-fact                  │ logic.assertFact(fact)     │ 7.11  │ NOT_STARTED │            │
│ LG02 │ add-rule                     │ logic.addRule(cond,concl)  │ 7.11  │ NOT_STARTED │            │
│ LG03 │ query                        │ logic.query(question)      │ 7.11  │ NOT_STARTED │            │
│ LG04 │ prove (backward chaining)    │ logic.prove(goal)          │ 7.11  │ NOT_STARTED │            │
│ LG05 │ semantic-unify               │ logic.unify(pattern,text)  │ 7.11  │ NOT_STARTED │            │
│ LG06 │ retrieve vs infer            │ logic.retrieve/infer()     │ 7.11  │ NOT_STARTED │            │
│ LG07 │ on('fact-queried', ...)      │ RuntimeEventEmitter        │ 7.11  │ NOT_STARTED │ Hook       │
│ LG08 │ on('rule-applied', ...)      │ RuntimeEventEmitter        │ 7.11  │ NOT_STARTED │ Hook       │
│      │                              │                              │       │             │            │
│      │ ━━━ EXPERT SYSTEM ━━━       │ runtime.experts.*          │       │             │ NEW        │
│ EX01 │ register expert role         │ experts.registerRole(role) │ 7.12  │ NOT_STARTED │            │
│ EX02 │ compile intent               │ experts.compileIntent()    │ 7.12  │ NOT_STARTED │            │
│ EX03 │ parallel experts             │ experts.runParallel()      │ 7.12  │ NOT_STARTED │            │
│ EX04 │ batch intents                │ experts.batch(intents)     │ 7.12  │ NOT_STARTED │            │
│ EX05 │ on('intent-compiled', ...)   │ RuntimeEventEmitter        │ 7.12  │ NOT_STARTED │ Hook       │
│ EX06 │ on('expert-result', ...)     │ RuntimeEventEmitter        │ 7.12  │ NOT_STARTED │ Hook       │
│      │                              │                              │       │             │            │
│      │ ━━━ MACRO SYSTEM ━━━        │ runtime.macros.*           │       │             │ NEW        │
│ MC01 │ defmacro                     │ macros.define(n,p,b)       │ 7.13  │ NOT_STARTED │            │
│ MC02 │ macroexpand-1                │ macros.expand1(expr)       │ 7.13  │ NOT_STARTED │            │
│ MC03 │ macroexpand (recursive)      │ macros.expandAll(expr)     │ 7.13  │ NOT_STARTED │            │
│ MC04 │ gensym                       │ macros.gensym(prefix)      │ 7.13  │ NOT_STARTED │            │
│ MC05 │ quasiquote/unquote/splice    │ macros.quasiquote()        │ 7.13  │ NOT_STARTED │            │
│ MC06 │ on('macro-expand', ...)      │ RuntimeEventEmitter        │ 7.13  │ NOT_STARTED │ Hook       │
│      │                              │                              │       │             │            │
│      │ ━━━ EXTENDED CONCURRENCY ━━━│ runtime.concurrency.*      │       │             │ NEW        │
│ CF01 │ fiber.spawn                  │ concurrency.fiberSpawn()   │ 7.8   │ NOT_STARTED │            │
│ CF02 │ fiber.join                   │ concurrency.fiberJoin()    │ 7.8   │ NOT_STARTED │            │
│ CF03 │ fiber.select                 │ concurrency.fiberSelect()  │ 7.8   │ NOT_STARTED │            │
│ CF04 │ parallel-map                 │ concurrency.parallelMap()  │ 7.8   │ NOT_STARTED │            │
│ CF05 │ mutex.acquire/release        │ concurrency.mutexAcquire() │ 7.8   │ NOT_STARTED │            │
│ CF06 │ make-serializer              │ concurrency.makeSerializer()│7.8   │ NOT_STARTED │            │
│ CF07 │ make-singleflight            │ concurrency.makeSingleflight()│7.8 │ NOT_STARTED │            │
│ CF08 │ voting/consensus             │ concurrency.voting()       │ 7.8   │ NOT_STARTED │            │
│ CF09 │ on('deadlock-detected', ...) │ RuntimeEventEmitter        │ 7.8   │ NOT_STARTED │ Hook       │
```

---

## ══════════════════════════════════════════════════════════════════════════════
## DESIGN HARMONY AUDIT
## ══════════════════════════════════════════════════════════════════════════════

This section documents design issues found during comprehensive review and their
resolutions to ensure HARMONY across all 24 subsystems.

### ════════════════════════════════════════════════════════════════════════════
### ISSUE H1: Missing Cross-Subsystem Dependencies (YAML)
### ════════════════════════════════════════════════════════════════════════════

**Problem**: Several subsystems have implicit runtime dependencies not declared
in their `depends_on` arrays.

**Analysis**:
| Subsystem       | Missing Dependency | Reason                                        |
|-----------------|-------------------|-----------------------------------------------|
| 022-amb         | 022-providers     | AmbManager needs StateProvider for choice points |
| 022-amb         | 022-execution     | amb.directedAmb() needs to evaluate candidates |
| 022-logic       | 022-llm           | logic.query() uses LLM for semantic matching  |
| 022-streams     | 022-execution     | Stream thunks execute in evaluation context   |
| 022-execution   | 022-macros        | Macro expansion happens before evaluation     |

**Resolution**: Update YAML `depends_on` arrays (see YAML FIXES section below).

### ════════════════════════════════════════════════════════════════════════════
### ISSUE H2: Dependency Injection Table Incomplete
### ════════════════════════════════════════════════════════════════════════════

**Problem**: The DI table in §5.3 is missing constructor dependencies for several
subsystems added in Gap Analysis.

**Resolution**: Add these rows to the Dependency Injection Table:

```
│ AmbManager             │ StateProvider, ExecutionEngine, RuntimeEventEmitter           │
│ StreamsManager         │ StateProvider, RuntimeEventEmitter                            │
│ LogicManager           │ LogicProvider, LLMIntegration, RuntimeEventEmitter            │
│ ExpertManager          │ ExpertProvider, LLMIntegration, RuntimeEventEmitter           │
│ MacroManager           │ RuntimeEventEmitter                                           │
│ ConditionsManager      │ RuntimeEventEmitter                                           │
│ FactsManager           │ StateProvider, RuntimeEventEmitter                            │
│ FixpointManager        │ FactsManager, RuntimeEventEmitter                             │
│ TransactionManager     │ StateProvider, RuntimeEventEmitter                            │
│ SecurityManager        │ SecurityProvider, RuntimeEventEmitter                         │
│ BudgetManager          │ GovernanceProvider, RuntimeEventEmitter                       │
│ ProvenanceManager      │ ProvenanceProvider, RuntimeEventEmitter                       │
│ ArtifactManager        │ ArtifactProvider, RuntimeEventEmitter                         │
│ ConcurrencyManager     │ RuntimeEventEmitter                                           │
│ ProtocolServer         │ ExecutionEngine, DebugSubsystem, SessionManager               │
```

### ════════════════════════════════════════════════════════════════════════════
### ISSUE H3: FactsManager vs LogicManager Naming Confusion
### ════════════════════════════════════════════════════════════════════════════

**Problem**: Both subsystems deal with "facts" but serve different purposes:
- FactsManager: Reads Lisp facts from CESK machine's monotone fact store
- LogicManager: Semantic logic programming with natural language facts

**Resolution**: Document the distinction clearly:

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                        FACTS vs LOGIC DISTINCTION                            │
├─────────────────────────────────────────────────────────────────────────────┤
│  FactsManager (DATA category)              LogicManager (SEMANTIC category) │
│  ─────────────────────────────────────     ───────────────────────────────  │
│  • Reads from CESK State fact store        • Separate semantic database     │
│  • Lisp values: (assert '(knows alice))    • NL strings: "Alice knows Bob"  │
│  • Source: 23-FACTS.md                     • Source: USER-MANUAL--27        │
│  • API: facts.all(), facts.has(expr)       • API: logic.assertFact(string)  │
│  • For fixpoint convergence detection      • For semantic reasoning/queries │
│  • NO LLM calls                            • USES LLM for inference         │
└─────────────────────────────────────────────────────────────────────────────┘
```

### ════════════════════════════════════════════════════════════════════════════
### ISSUE H4: API Naming Style Inconsistency (streams.*)
### ════════════════════════════════════════════════════════════════════════════

**Problem**: StreamsManager uses verbose prefixed names:
```typescript
streams.streamCar(s)    // Redundant 'stream' prefix
streams.streamCdr(s)
streams.streamMap(f, s)
```

**Resolution**: Standardize to short method names (namespace provides context):

```typescript
// BEFORE (verbose)                    // AFTER (harmonized)
streams.streamCar(s)          →        streams.car(s)
streams.streamCdr(s)          →        streams.cdr(s)
streams.streamNull(s)         →        streams.null(s)
streams.streamMap(f, s)       →        streams.map(f, s)
streams.streamFilter(p, s)    →        streams.filter(p, s)
streams.streamTake(s, n)      →        streams.take(s, n)
streams.streamToList(s, n)    →        streams.toList(s, n)
streams.streamFlatMap(f, s)   →        streams.flatMap(f, s)
streams.listToStream(list)    →        streams.fromList(list)
streams.consStream(h, t)      →        streams.cons(h, t)
streams.emptyStream()         →        streams.empty()
```

### ════════════════════════════════════════════════════════════════════════════
### ISSUE H5: Outcome Type Not Used Uniformly
### ════════════════════════════════════════════════════════════════════════════

**Problem**: Some APIs return Outcome, others don't, creating inconsistent error handling.

| API                      | Current Return     | Should Be              |
|--------------------------|-------------------|------------------------|
| runtime.eval()           | EvalResult        | Outcome<Val>           |
| fixpoint.run()           | Outcome           | ✓ Correct              |
| logic.query()            | QueryResult       | QueryResult (OK, domain-specific) |
| logic.prove()            | ProofResult       | ProofResult (OK, domain-specific) |
| amb.firstSolution()      | T \| null         | Outcome<T>             |
| amb.allSolutions()       | T[]               | Outcome<T[]>           |

**Resolution**:
1. Keep domain-specific result types for readability (QueryResult, ProofResult)
2. Ensure ALL potentially-failing operations return structured results
3. Add `toOutcome()` adapter method for interop:

```typescript
// In types/Outcome.ts
export function toOutcome<T>(result: T | null, errorMsg?: string): Outcome<T> {
  return result !== null
    ? { tag: 'ok', value: result, metadata: {} }
    : { tag: 'error', message: errorMsg ?? 'No result', errorType: 'empty', metadata: {} };
}
```

### ════════════════════════════════════════════════════════════════════════════
### ISSUE H6: Missing Lifecycle Management
### ════════════════════════════════════════════════════════════════════════════

**Problem**: No documented way to cleanly shutdown runtime or dispose resources.

**Resolution**: Add lifecycle methods to OmegaRuntime:

```typescript
// In OmegaRuntime.ts
export interface OmegaRuntime {
  // ... existing fields ...

  // ═══ LIFECYCLE ═══
  /**
   * Gracefully shutdown the runtime:
   * 1. Cancel pending operations
   * 2. Close protocol connections
   * 3. Flush pending traces
   * 4. Dispose all subsystems
   */
  dispose(): Promise<void>;

  /**
   * Check if runtime has been disposed
   */
  isDisposed(): boolean;

  /**
   * Register cleanup handler for process exit
   */
  onShutdown(handler: () => Promise<void>): Unsubscribe;
}

// All providers should optionally implement:
export interface Disposable {
  dispose?(): Promise<void>;
}
```

### ════════════════════════════════════════════════════════════════════════════
### ISSUE H7: Protocol Operations Incomplete
### ════════════════════════════════════════════════════════════════════════════

**Problem**: ProtocolServer only lists basic operations (eval, info, complete, interrupt,
snapshot, restore). Missing operations for full remote debugging.

**Resolution**: Document complete protocol operation set:

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                      COMPLETE PROTOCOL OPERATIONS                            │
├─────────────────────────────────────────────────────────────────────────────┤
│  CATEGORY        │ OPERATION      │ MAPS TO                                 │
├──────────────────┼───────────────┼─────────────────────────────────────────┤
│  Core            │ eval          │ runtime.eval()                          │
│                  │ load          │ runtime.loadFile()                      │
│                  │ interrupt     │ runtime.interrupt()                     │
│                  │ info          │ runtime.getInfo()                       │
├──────────────────┼───────────────┼─────────────────────────────────────────┤
│  Debug           │ debug/load    │ runtime.debug.load()                    │
│                  │ debug/step    │ runtime.debug.step()                    │
│                  │ debug/run     │ runtime.debug.run()                     │
│                  │ debug/stop    │ runtime.debug.stop()                    │
│                  │ debug/state   │ runtime.debug.getState()                │
├──────────────────┼───────────────┼─────────────────────────────────────────┤
│  Breakpoints     │ break/add     │ runtime.breakpoints.add()               │
│                  │ break/remove  │ runtime.breakpoints.remove()            │
│                  │ break/list    │ runtime.breakpoints.list()              │
│                  │ break/toggle  │ runtime.breakpoints.toggle()            │
├──────────────────┼───────────────┼─────────────────────────────────────────┤
│  Snapshots       │ snap/save     │ runtime.snapshots.save()                │
│                  │ snap/restore  │ runtime.snapshots.restore()             │
│                  │ snap/list     │ runtime.snapshots.list()                │
│                  │ snap/export   │ runtime.snapshots.export()              │
│                  │ image/save    │ runtime.snapshots.saveImage()           │
│                  │ image/load    │ runtime.snapshots.loadImage()           │
├──────────────────┼───────────────┼─────────────────────────────────────────┤
│  Session         │ session/save  │ runtime.session.save()                  │
│                  │ session/load  │ runtime.session.load()                  │
│                  │ session/fork  │ runtime.session.fork()                  │
│                  │ session/list  │ runtime.session.list()                  │
├──────────────────┼───────────────┼─────────────────────────────────────────┤
│  History         │ history/back  │ runtime.history.back()                  │
│                  │ history/list  │ runtime.history.list()                  │
│                  │ history/dump  │ runtime.history.dump()                  │
├──────────────────┼───────────────┼─────────────────────────────────────────┤
│  Inspect         │ inspect/env   │ runtime.inspect.env()                   │
│                  │ inspect/stack │ runtime.inspect.stack()                 │
│                  │ inspect/frame │ runtime.inspect.frame()                 │
│                  │ inspect/defs  │ runtime.inspect.defs()                  │
│                  │ inspect/facts │ runtime.inspect.facts()                 │
├──────────────────┼───────────────┼─────────────────────────────────────────┤
│  LLM             │ llm/ask       │ runtime.llm.ask()                       │
│                  │ llm/traces    │ runtime.llm.getTraces()                 │
│                  │ llm/configure │ runtime.llm.configure()                 │
├──────────────────┼───────────────┼─────────────────────────────────────────┤
│  Budget          │ budget/report │ runtime.budget.report()                 │
│                  │ budget/set    │ (set limits dynamically)                │
├──────────────────┼───────────────┼─────────────────────────────────────────┤
│  Artifacts       │ memo/stats    │ runtime.artifacts.stats()               │
│                  │ memo/clear    │ runtime.artifacts.clear()               │
├──────────────────┼───────────────┼─────────────────────────────────────────┤
│  Subscribe       │ subscribe     │ Subscribe to event stream via WebSocket │
│                  │ unsubscribe   │ Unsubscribe from events                 │
└─────────────────────────────────────────────────────────────────────────────┘
```

### ════════════════════════════════════════════════════════════════════════════
### ISSUE H8: Macro Expansion Pipeline Position
### ════════════════════════════════════════════════════════════════════════════

**Problem**: MacroManager exists but its integration with ExecutionEngine is not documented.
Where in the evaluation pipeline does macro expansion occur?

**Resolution**: Document the pipeline:

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         EVALUATION PIPELINE                                  │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│   Source Code (string)                                                      │
│         │                                                                   │
│         ▼                                                                   │
│   ┌─────────────┐                                                           │
│   │   READER    │  src/core/pipeline/compileText.ts                        │
│   │   (Parse)   │  String → AST (Val)                                      │
│   └──────┬──────┘                                                           │
│          │                                                                  │
│          ▼                                                                  │
│   ┌─────────────┐                                                           │
│   │   MACROS    │  runtime.macros.expandAll()                              │
│   │  (Expand)   │  AST → Expanded AST                                      │
│   └──────┬──────┘                                                           │
│          │                                                                  │
│          ▼                                                                  │
│   ┌─────────────┐                                                           │
│   │    CESK     │  src/core/eval/machine.ts                                │
│   │ (Evaluate)  │  Expanded AST → Value                                    │
│   └─────────────┘                                                           │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘

// ExecutionEngine.eval() pseudocode:
async eval(code: string): Promise<Outcome<Val>> {
  // 1. Parse
  const ast = this.reader.read(code);

  // 2. Macro expand (NEW: this step was implicit)
  const expanded = this.macros.expandAll(ast);

  // 3. Evaluate
  return this.machine.eval(expanded);
}
```

### ════════════════════════════════════════════════════════════════════════════
### ISSUE H9: Store vs State Terminology Clarity
### ════════════════════════════════════════════════════════════════════════════

**Problem**: The spec uses "State" and "Store" somewhat interchangeably.

**Resolution**: Document the CESK terminology:

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         CESK TERMINOLOGY                                     │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  State = The complete machine state (a 4-tuple):                            │
│  ┌────────────────────────────────────────────────────────────────────────┐ │
│  │  State = {                                                              │ │
│  │    control: Val,           // Current expression being evaluated        │ │
│  │    environment: Env,       // Variable bindings (Map<string, Address>)  │ │
│  │    kontinuation: Kont,     // Continuation stack (what to do next)      │ │
│  │    store: Store            // Heap (Map<Address, Val>)                  │ │
│  │  }                                                                      │ │
│  └────────────────────────────────────────────────────────────────────────┘ │
│                                                                             │
│  Store = Just the heap portion (Map<Address, Val>)                          │
│  • StateProvider creates/loads/saves full State                             │
│  • Store is accessed via State.store                                        │
│                                                                             │
│  Snapshot = Serialized State at a point in time                             │
│  Image = Full runtime including State + definitions + loaded libraries      │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

### ════════════════════════════════════════════════════════════════════════════
### ISSUE H10: Event Type Safety Pattern
### ════════════════════════════════════════════════════════════════════════════

**Problem**: The RuntimeEventEmitter interface loses type safety between event name
and payload type.

**Resolution**: Ensure the interface definition is type-safe:

```typescript
// In src/runtime/events/RuntimeEventEmitter.ts

// Event map - defines payload type for each event
export interface RuntimeEventMap {
  'step': StepEvent;
  'effect': EffectEvent;
  'before-llm': BeforeLLMEvent;
  'after-llm': AfterLLMEvent;
  'breakpoint-hit': BreakpointHitEvent;
  'error': ErrorEvent;
  'done': DoneEvent;
  'session-checkpoint': SessionCheckpointEvent;
  'opr-receipt': OprReceiptEvent;
  'llm-tool-call': LLMToolCallEvent;
  'condition': ConditionEvent;
  'fact-asserted': FactAssertedEvent;
  'fixpoint-iter': FixpointIterEvent;
  'fixpoint-done': FixpointDoneEvent;
  'budget-warning': BudgetWarningEvent;
  'budget-exceeded': BudgetExceededEvent;
  'security-event': SecurityEvent;
  'task-complete': TaskCompleteEvent;
  'amb-choose': AmbChooseEvent;
  'amb-fail': AmbFailEvent;
  'amb-solution': AmbSolutionEvent;
  'stream-force': StreamForceEvent;
  'fact-queried': FactQueriedEvent;
  'rule-applied': RuleAppliedEvent;
  'intent-compiled': IntentCompiledEvent;
  'expert-result': ExpertResultEvent;
  'macro-expand': MacroExpandEvent;
  'deadlock-detected': DeadlockDetectedEvent;
}

// Type-safe emitter interface
export interface RuntimeEventEmitter {
  /**
   * Subscribe to an event with type-safe payload
   */
  on<E extends keyof RuntimeEventMap>(
    event: E,
    handler: (payload: RuntimeEventMap[E]) => void
  ): Unsubscribe;

  /**
   * Emit an event with type-checked payload
   */
  emit<E extends keyof RuntimeEventMap>(
    event: E,
    payload: RuntimeEventMap[E]
  ): void;

  /**
   * One-time subscription
   */
  once<E extends keyof RuntimeEventMap>(
    event: E,
    handler: (payload: RuntimeEventMap[E]) => void
  ): Unsubscribe;

  /**
   * Remove specific handler
   */
  off<E extends keyof RuntimeEventMap>(
    event: E,
    handler: (payload: RuntimeEventMap[E]) => void
  ): void;

  /**
   * Remove all handlers for an event
   */
  removeAllListeners<E extends keyof RuntimeEventMap>(event?: E): void;
}
```

### ════════════════════════════════════════════════════════════════════════════
### ISSUE H11: Circular Dependency Prevention
### ════════════════════════════════════════════════════════════════════════════

**Problem**: Potential circular calls:
- LogicManager.query() → LLMIntegration.ask() → ExecutionEngine → Effects → LogicManager?

**Resolution**: Add safeguards:

```typescript
// In LogicManager.ts
export class LogicManager {
  private queryDepth = 0;
  private readonly MAX_QUERY_DEPTH = 10;

  async query(question: string): Promise<QueryResult> {
    if (this.queryDepth >= this.MAX_QUERY_DEPTH) {
      return {
        answer: 'Query depth limit exceeded',
        confidence: 0,
        source: 'error'
      };
    }

    this.queryDepth++;
    try {
      // ... perform query ...
    } finally {
      this.queryDepth--;
    }
  }
}
```

### ════════════════════════════════════════════════════════════════════════════
### ISSUE H12: Image vs Snapshot Distinction
### ════════════════════════════════════════════════════════════════════════════

**Problem**: saveImage/loadImage on SnapshotManager but Image ≠ Snapshot.

**Resolution**: Clarify the API placement:

```
Option A: Keep on SnapshotManager (chosen for simplicity)
─────────────────────────────────────────────────────────
  snapshots.save(name)      → Named in-memory snapshot
  snapshots.saveImage(path) → Full state to file

  Rationale: Both are "state preservation" - just different scope.

Option B: Move to SessionManager
───────────────────────────────
  session.saveImage(path)
  session.loadImage(path)

  Rationale: Image includes session metadata.

DECISION: Keep on SnapshotManager but document the distinction:
  - Snapshot: Machine state at a step (control, env, kont, store)
  - Image: Snapshot + definitions + facts + loaded libraries + config
```

---

## ══════════════════════════════════════════════════════════════════════════════
## HARMONY FIXES APPLIED
## ══════════════════════════════════════════════════════════════════════════════

### YAML Dependency Fixes Required

Update the following job definitions in `022-omega-runtime.yaml`:

```yaml
# FIX H1: Add missing dependencies

- id: "022-amb"
  depends_on: ["022-types", "022-events", "022-providers"]  # Added 022-providers

- id: "022-streams"
  depends_on: ["022-types", "022-events", "022-execution"]  # Added 022-execution

- id: "022-logic"
  depends_on: ["022-types", "022-events", "022-facts", "022-llm"]  # Added 022-llm

- id: "022-execution"
  depends_on: ["022-types", "022-events", "022-providers", "022-macros"]  # Added 022-macros
```

### Updated Subsystem Reference Table

| Category     | # | Subsystem           | Provider(s)              | Events Emitted              |
|--------------|---|---------------------|--------------------------|------------------------------|
| **CORE**     | 1 | ExecutionEngine     | StateProvider, EffectHandlerRegistry, GovernanceProvider | step, effect, error, done |
| **DEBUGGING**| 2 | DebugSubsystem      | (composes others)        | breakpoint-hit              |
|              | 3 | BreakpointManager   | -                        | -                           |
|              | 4 | SnapshotManager     | SnapshotProvider         | -                           |
|              | 5 | StateInspector      | -                        | -                           |
|              | 6 | HistoryManager      | TraceProvider            | -                           |
|              | 7 | SessionManager      | SessionProvider          | session-checkpoint          |
| **DATA**     | 8 | ArtifactManager     | ArtifactProvider         | -                           |
|              | 9 | FactsManager        | (reads CESK State)       | fact-asserted               |
| **CONTROL**  |10 | ConditionsManager   | -                        | condition                   |
|              |11 | FixpointManager     | (uses FactsManager)      | fixpoint-iter, fixpoint-done|
|              |12 | TransactionManager  | -                        | -                           |
| **GOVERNANCE**|13| ProvenanceManager   | ProvenanceProvider       | -                           |
|              |14 | SecurityManager     | SecurityProvider         | security-event              |
|              |15 | BudgetManager       | (wraps GovernanceProvider)| budget-warning, budget-exceeded |
| **COMMUNICATION**|16| ProtocolServer   | -                        | -                           |
|              |17 | ConcurrencyManager  | -                        | task-complete, deadlock-detected |
| **LLM**      |18 | LLMIntegration      | (uses Oracle adapters)   | before-llm, after-llm, llm-tool-call |
|              |19 | OprIntegration      | ReceiptProvider          | opr-receipt                 |
|              |20 | ExpertManager       | ExpertProvider           | intent-compiled, expert-result |
| **SEMANTIC** |21 | AmbManager          | StateProvider            | amb-choose, amb-fail, amb-solution |
|              |22 | StreamsManager      | StreamProvider           | stream-force                |
|              |23 | LogicManager        | LogicProvider            | fact-queried, rule-applied  |
|              |24 | MacroManager        | -                        | macro-expand                |

### Complete Dependency Injection Table (Harmonized)

```
┌────────────────────────┬────────────────────────────────────────────────────────────────┐
│ Subsystem              │ Constructor Dependencies                                       │
├────────────────────────┼────────────────────────────────────────────────────────────────┤
│  ═══ CORE ═══                                                                          │
│ ExecutionEngine        │ StateProvider, EffectHandlerRegistry, GovernanceProvider,      │
│                        │ MacroManager, RuntimeEventEmitter                              │
├────────────────────────┼────────────────────────────────────────────────────────────────┤
│  ═══ DEBUGGING ═══                                                                     │
│ DebugSubsystem         │ ExecutionEngine, BreakpointManager, HistoryManager,            │
│                        │ RuntimeEventEmitter                                            │
│ BreakpointManager      │ RuntimeEventEmitter                                            │
│ SnapshotManager        │ SnapshotProvider, StateProvider                                │
│ StateInspector         │ (reads current State)                                          │
│ HistoryManager         │ TraceProvider, RuntimeEventEmitter                             │
│ SessionManager         │ SessionProvider, SnapshotManager, RuntimeEventEmitter          │
├────────────────────────┼────────────────────────────────────────────────────────────────┤
│  ═══ DATA ═══                                                                          │
│ ArtifactManager        │ ArtifactProvider, RuntimeEventEmitter                          │
│ FactsManager           │ StateProvider (reads CESK facts), RuntimeEventEmitter          │
├────────────────────────┼────────────────────────────────────────────────────────────────┤
│  ═══ CONTROL ═══                                                                       │
│ ConditionsManager      │ RuntimeEventEmitter                                            │
│ FixpointManager        │ FactsManager, RuntimeEventEmitter                              │
│ TransactionManager     │ StateProvider, RuntimeEventEmitter                             │
├────────────────────────┼────────────────────────────────────────────────────────────────┤
│  ═══ GOVERNANCE ═══                                                                    │
│ ProvenanceManager      │ ProvenanceProvider, RuntimeEventEmitter                        │
│ SecurityManager        │ SecurityProvider, RuntimeEventEmitter                          │
│ BudgetManager          │ GovernanceProvider, RuntimeEventEmitter                        │
├────────────────────────┼────────────────────────────────────────────────────────────────┤
│  ═══ COMMUNICATION ═══                                                                 │
│ ProtocolServer         │ OmegaRuntime (accesses all subsystems for protocol ops)        │
│ ConcurrencyManager     │ RuntimeEventEmitter                                            │
├────────────────────────┼────────────────────────────────────────────────────────────────┤
│  ═══ LLM ═══                                                                           │
│ LLMIntegration         │ RuntimeEventEmitter                                            │
│ OprIntegration         │ ReceiptProvider, RuntimeEventEmitter                           │
│ ExpertManager          │ ExpertProvider, LLMIntegration, RuntimeEventEmitter            │
├────────────────────────┼────────────────────────────────────────────────────────────────┤
│  ═══ SEMANTIC ═══                                                                      │
│ AmbManager             │ StateProvider, ExecutionEngine, RuntimeEventEmitter            │
│ StreamsManager         │ StateProvider, RuntimeEventEmitter                             │
│ LogicManager           │ LogicProvider, LLMIntegration, RuntimeEventEmitter             │
│ MacroManager           │ RuntimeEventEmitter                                            │
└────────────────────────┴────────────────────────────────────────────────────────────────┘
```

### StreamsManager API (Harmonized Names)

```typescript
export interface StreamsManager {
  // Stream construction
  fromList(list: Val[]): LazyStream;         // was listToStream
  cons(head: Val, tailThunk: () => LazyStream): LazyStream;  // was consStream
  empty(): LazyStream;                        // was emptyStream

  // Stream access
  car(s: LazyStream): Val;                   // was streamCar
  cdr(s: LazyStream): LazyStream;            // was streamCdr
  null(s: LazyStream): boolean;              // was streamNull
  toList(s: LazyStream, n: number): Val[];   // was streamToList

  // Stream operations
  map(f: (x: Val) => Val, s: LazyStream): LazyStream;        // was streamMap
  filter(p: (x: Val) => boolean, s: LazyStream): LazyStream; // was streamFilter
  take(s: LazyStream, n: number): LazyStream;                // was streamTake
  flatMap(f: (x: Val) => LazyStream, s: LazyStream): LazyStream; // was streamFlatMap

  // Infinite streams
  iterate(f: (x: Val) => Val, seed: Val): LazyStream;
  repeat(value: Val): LazyStream;
  cycle(list: Val[]): LazyStream;
}
```

## ══════════════════════════════════════════════════════════════════════════════
## CRITICAL DESIGN FLAWS (Must Fix Before Implementation)
## ══════════════════════════════════════════════════════════════════════════════

### ═══════════════════════════════════════════════════════════════════════════
### FLAW F1: Budget Consumption Not Wired Through All LLM Paths
### ═══════════════════════════════════════════════════════════════════════════

**Problem**: Multiple subsystems make LLM calls but don't consume budget:

```
Path                                 Budget Check?
────────────────────────────────────────────────────
ExecutionEngine → effect infer.op   → ???
LLMIntegration.ask()                → ???
LogicManager.query()                → ???
ExpertManager.compileIntent()       → ???
AmbManager.directedAmb()            → ???
StreamsManager (lazy LLM thunks)    → ???
```

None of these subsystems have BudgetManager in their dependencies!

**Solution**: Budget check must happen at ONE central point - the LLM Adapter layer:

```typescript
// ALL LLM calls go through this adapter
interface LLMAdapter {
  call(request: LLMRequest): Promise<LLMResponse>;
}

// Budget-aware wrapper (Decorator pattern)
class BudgetAwareLLMAdapter implements LLMAdapter {
  constructor(
    private inner: LLMAdapter,
    private budget: BudgetManager,
    private emitter: RuntimeEventEmitter
  ) {}

  async call(request: LLMRequest): Promise<LLMResponse> {
    // Check budget BEFORE call
    if (!this.budget.hasRemaining('tokens')) {
      this.emitter.emit('budget-exceeded', { type: 'tokens' });
      throw new BudgetExceededError('tokens');
    }

    const response = await this.inner.call(request);

    // Consume AFTER call (actual tokens used)
    this.budget.consume('tokens', response.usage.totalTokens);
    this.budget.consume('cost', response.usage.cost);

    // Emit warning at 80%
    const limit = this.budget.getLimit('tokens');
    if (limit && this.budget.remaining('tokens')! < limit * 0.2) {
      this.emitter.emit('budget-warning', {
        type: 'tokens',
        remaining: this.budget.remaining('tokens'),
        limit
      });
    }

    return response;
  }
}
```

**Architecture Change Required**:
```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         LLM CALL FLOW (CORRECTED)                           │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  Any Subsystem (Logic, Expert, Amb, Streams, etc.)                         │
│       │                                                                     │
│       ▼                                                                     │
│  ┌──────────────────┐                                                       │
│  │ LLMIntegration   │  ← SINGLE entry point for all LLM calls               │
│  └────────┬─────────┘                                                       │
│           │                                                                 │
│           ▼                                                                 │
│  ┌──────────────────┐                                                       │
│  │ BudgetAware      │  ← Check/consume budget HERE (not in subsystems)      │
│  │ LLMAdapter       │  ← Emits budget-warning, budget-exceeded              │
│  └────────┬─────────┘                                                       │
│           │                                                                 │
│           ▼                                                                 │
│  ┌──────────────────┐                                                       │
│  │ Actual LLM       │  OpenAI / Anthropic / Mock                            │
│  │ Provider         │                                                       │
│  └──────────────────┘                                                       │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

### ═══════════════════════════════════════════════════════════════════════════
### FLAW F2: State Ownership Conflict (Amb vs Snapshot vs History)
### ═══════════════════════════════════════════════════════════════════════════

**Problem**: Three subsystems manipulate machine state independently:

| Subsystem       | Saves State When...           | Restores State When...        |
|-----------------|-------------------------------|-------------------------------|
| AmbManager      | amb.choose() creates choice   | amb.fail() backtracks         |
| SnapshotManager | snapshots.save(name)          | snapshots.restore(name)       |
| HistoryManager  | Every step (if recording)     | history.back() / debug.goto() |

**Conflict Scenario**:
```
1. User evaluates: (amb '(1 2 3))
   → AmbManager saves state S0, returns 1

2. User runs: (snapshots.save "checkpoint")
   → SnapshotManager saves state S1 (includes amb in progress)

3. User evaluates: (require (> x 1))
   → Fails! AmbManager backtracks to S0, tries 2

4. User runs: (snapshots.restore "checkpoint")
   → CONFLICT! Restores S1, but S1 has stale backtrack stack
   → AmbManager's internal state is now inconsistent with machine state
```

**Solution**: Introduce **StateCoordinator** that manages all state operations:

```typescript
// In src/runtime/internal/StateCoordinator.ts

export interface StateCoordinator {
  // State modification must go through coordinator
  checkpoint(source: 'amb' | 'snapshot' | 'history' | 'transaction'): CheckpointId;
  restore(id: CheckpointId, source: string): void;

  // Query current context
  isInAmbSearch(): boolean;
  isInTransaction(): boolean;
  isInFixpoint(): boolean;
  getContextDepth(): { amb: number; transaction: number; fixpoint: number };

  // Conflict prevention - returns error message or null if OK
  canRestore(source: string): string | null;
  canStartAmb(): string | null;
  canStartTransaction(): string | null;
}

// SnapshotManager must check coordinator:
class SnapshotManager {
  restore(name: string): void {
    const conflict = this.coordinator.canRestore('snapshot');
    if (conflict) {
      throw new StateConflictError(conflict);
      // e.g., "Cannot restore snapshot during amb search. Call amb.resetSearch() first."
    }
    // ... proceed with restore
  }
}

// Add StateCoordinator to OmegaRuntime:
interface OmegaRuntime {
  // ... existing subsystems ...

  readonly coordinator: StateCoordinator;  // For conflict checks
}
```

### ═══════════════════════════════════════════════════════════════════════════
### FLAW F3: Concurrency Model Undefined for Single-Threaded CESK
### ═══════════════════════════════════════════════════════════════════════════

**Problem**: ConcurrencyManager supports parallel fibers, but CESK machine is
inherently single-threaded. The spec doesn't define how this works.

**Decision**: Use **Cooperative Async Model** (matches JS/TypeScript nature):

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         CONCURRENCY MODEL                                    │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  "Parallel" fibers are NOT truly parallel.                                  │
│  They are cooperative coroutines that yield on async operations.            │
│                                                                             │
│  ┌─────────┐  ┌─────────┐  ┌─────────┐                                     │
│  │ Fiber 1 │  │ Fiber 2 │  │ Fiber 3 │    Each fiber = suspended CESK       │
│  │ RUNNING │  │ WAITING │  │ WAITING │                                      │
│  └────┬────┘  └────┬────┘  └────┬────┘                                     │
│       │            │            │                                           │
│       ▼            │            │                                           │
│  ┌─────────────────────────────────────┐                                   │
│  │         SINGLE CESK Machine          │   Only ONE fiber runs at a time   │
│  │         (time-multiplexed)           │                                   │
│  └─────────────────────────────────────┘                                   │
│                                                                             │
│  Fiber yields when:                                                         │
│    - Hits async effect (LLM call, IO)                                      │
│    - Calls fiber.yield()                                                    │
│    - Scheduler preempts (after N steps)                                     │
│                                                                             │
│  parallelMap(f, items) =                                                    │
│    1. Spawn fiber for each item                                            │
│    2. Run fibers in round-robin until all complete                         │
│    3. Collect results                                                       │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

**Add to ConcurrencyManager spec**:
```typescript
interface Fiber {
  id: FiberId;
  state: MachineSnapshot;
  status: 'ready' | 'running' | 'waiting' | 'completed' | 'failed';
  waitingOn?: Promise<Val>;
  result?: Val;
  error?: Error;
}

interface ConcurrencyManager {
  // ... existing API ...

  // Scheduling control
  setQuantum(steps: number): void;  // Steps before preemption (default: 100)
  yield(): void;                     // Voluntary yield

  // NOTE: "parallel" means concurrent async, NOT thread parallelism
}
```

### ═══════════════════════════════════════════════════════════════════════════
### FLAW F4: EvalResult vs Outcome Type Undefined Relationship
### ═══════════════════════════════════════════════════════════════════════════

**Problem**: Main interface returns `EvalResult`, Gap Analysis says `Outcome<Val>`.
Are these the same? Different? Related?

**Solution**: Define explicit type relationship:

```typescript
// Base structured result (from 27-OUTCOMES.md)
type Outcome<T = Val> =
  | { tag: 'ok'; value: T; metadata: OutcomeMetadata }
  | { tag: 'error'; message: string; errorType: string; metadata: OutcomeMetadata }
  | { tag: 'needs'; needType: NeedType; description: string; metadata: OutcomeMetadata }
  | { tag: 'proposed'; value: T; proposals: Proposal[]; metadata: OutcomeMetadata }
  | { tag: 'nonconverged'; value: T; iterations: number; metadata: OutcomeMetadata }
  | { tag: 'cycle'; value: T; cycleLength: number; metadata: OutcomeMetadata };

// EvalResult extends Outcome with eval-specific metadata
interface EvalResult extends Outcome<Val> {
  stepCount: number;           // How many CESK steps
  definitions: string[];       // Names defined during eval
  effects: EffectRecord[];     // Effects that occurred
  trace?: TraceEntry[];        // Optional step trace
}

// Conversion helper
function toEvalResult(outcome: Outcome<Val>, meta: EvalMeta): EvalResult {
  return { ...outcome, ...meta };
}

// Type usage:
// - runtime.eval() → EvalResult (richest info)
// - fixpoint.run() → Outcome<Val> (standard)
// - amb.firstSolution() → Outcome<Val>
// - logic.query() → QueryResult (domain-specific, NOT Outcome)
```

### ═══════════════════════════════════════════════════════════════════════════
### FLAW F5: Missing Event Listener Specification
### ═══════════════════════════════════════════════════════════════════════════

**Problem**: Events listed but WHO MUST LISTEN not specified. Some events
require mandatory internal listeners for correctness.

**Solution**: Add **Required Listeners** column:

```
┌────────────────────┬─────────────────────┬───────────────────────────────────┐
│ EVENT              │ EMITTED BY          │ REQUIRED LISTENERS (internal)     │
├────────────────────┼─────────────────────┼───────────────────────────────────┤
│ step               │ ExecutionEngine     │ HistoryManager (if recording)     │
│                    │                     │ DebugSubsystem (if debugging)     │
├────────────────────┼─────────────────────┼───────────────────────────────────┤
│ budget-exceeded    │ BudgetManager       │ ExecutionEngine (MUST STOP)       │
│                    │                     │ LLMIntegration (MUST REJECT)      │
├────────────────────┼─────────────────────┼───────────────────────────────────┤
│ fact-asserted      │ FactsManager        │ FixpointManager (update signature)│
│                    │                     │ ArtifactManager (invalidate cache)│
├────────────────────┼─────────────────────┼───────────────────────────────────┤
│ security-event     │ SecurityManager     │ (audit log - always recorded)     │
│ (with deny)        │                     │ ExecutionEngine (if 'block' type) │
├────────────────────┼─────────────────────┼───────────────────────────────────┤
│ amb-fail           │ AmbManager          │ TransactionManager (rollback      │
│                    │                     │   proposals in current choice)    │
└────────────────────┴─────────────────────┴───────────────────────────────────┘
```

### ═══════════════════════════════════════════════════════════════════════════
### FLAW F6: Missing YAML Dependencies (Additional)
### ═══════════════════════════════════════════════════════════════════════════

**Problem**: Several more missing dependencies found:

```yaml
# 022-amb needs execution (to evaluate thunks) and llm (for directedAmb)
- id: "022-amb"
  depends_on: ["022-types", "022-events", "022-providers", "022-execution", "022-llm"]

# 022-fixpoint needs artifacts (for signature mode 'facts+...+artifacts')
- id: "022-fixpoint"
  depends_on: ["022-types", "022-events", "022-facts", "022-artifacts"]

# 022-transaction needs to coordinate with amb (rollback on backtrack)
- id: "022-transaction"
  depends_on: ["022-types", "022-events", "022-amb"]  # Circular? See F7

# 022-protocol needs ALL subsystems to expose full API over wire
- id: "022-protocol"
  depends_on: ["022-types", "022-runtime"]  # Depends on full runtime
```

### ═══════════════════════════════════════════════════════════════════════════
### FLAW F7: Transaction ↔ Amb Circular Interaction
### ═══════════════════════════════════════════════════════════════════════════

**Problem**: Transaction and Amb have mutual dependencies:
- Amb needs Transaction to rollback proposals on backtrack
- Transaction needs Amb to check if in amb search before commit

This creates a potential circular dependency.

**Solution**: Use **StateCoordinator** as intermediary (breaks cycle):

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                  TRANSACTION ↔ AMB via COORDINATOR                          │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│            ┌─────────────────────┐                                          │
│            │  StateCoordinator   │  ← Both depend on coordinator            │
│            │  (mediator)         │                                          │
│            └──────────┬──────────┘                                          │
│                       │                                                     │
│         ┌─────────────┼─────────────┐                                       │
│         │             │             │                                       │
│         ▼             │             ▼                                       │
│  ┌─────────────┐      │      ┌─────────────┐                               │
│  │ AmbManager  │      │      │ Transaction │                               │
│  │             │──────┴──────│ Manager     │   No direct dependency        │
│  └─────────────┘             └─────────────┘                               │
│                                                                             │
│  AmbManager.fail():                                                         │
│    this.coordinator.notifyAmbBacktrack(checkpointId)                       │
│                                                                             │
│  TransactionManager listens to coordinator event:                           │
│    on('amb-backtrack', (id) => this.rollbackToCheckpoint(id))              │
│                                                                             │
│  TransactionManager.commit():                                               │
│    if (this.coordinator.isInAmbSearch()) throw "Cannot commit in amb"      │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

### ═══════════════════════════════════════════════════════════════════════════
### FLAW F8: Debugger Cannot Inspect Semantic Subsystem State
### ═══════════════════════════════════════════════════════════════════════════

**Problem**: DebugSubsystem only knows about CESK state. Cannot inspect:
- Current amb choice point stack
- Pending transaction proposals
- Fixpoint iteration count
- Current logic query depth

**Solution**: Add inspection APIs to OmegaRuntime:

```typescript
interface OmegaRuntime {
  // ... existing ...

  // Semantic state inspection (for debugger UI)
  getAmbState(): {
    inSearch: boolean;
    choicePoints: ChoicePointInfo[];
    currentIndex: number;
  } | null;

  getTransactionState(): {
    inTransaction: boolean;
    proposals: Proposal[];
    transactionId: string;
  } | null;

  getFixpointState(): {
    inFixpoint: boolean;
    iteration: number;
    maxIterations: number;
    lastSignature: string;
  } | null;

  getLogicState(): {
    queryDepth: number;
    currentQuery: string | null;
    factCount: number;
  };
}
```

---
