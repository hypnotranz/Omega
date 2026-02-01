# OmegaRuntime Job Index

## Progress Summary (Updated 2026-01-30)

| Layer | Jobs | Status |
|-------|------|--------|
| Layer 0: Foundation | 100, 110, 120 | ✅ 3/3 COMPLETE |
| Layer 1: Core Engine | 200, 210, 220, 230 | ✅ 4/4 COMPLETE |
| Layer 2: Debugging | 300, 310, 320, 330, 340, 350 | ✅ 6/6 COMPLETE |
| Layer 3: Data & Control | 400, 410, 420, 500, 510 | ✅ 5/5 COMPLETE |
| Layer 4: Governance | 600, 610, 620 | ✅ 3/3 COMPLETE |
| Layer 5: Communication | 700, 710 | ✅ 2/2 COMPLETE |
| Layer 6: LLM Integration | 800, 810, 820 | ✅ 3/3 COMPLETE |
| Layer 7: Semantic | 900, 910, 920 | ✅ 3/3 COMPLETE |
| Layer 8: Assembly | 1000 | ✅ 1/1 COMPLETE |
| Layer 9: Consumer Refactoring | 1100, 1110, 1120 | ✅ 3/3 COMPLETE |
| Layer 10: Testing | 1200, 1210, 1220, 1230, 1240 | ✅ 5/5 COMPLETE |
| Layer 11: Documentation | 1300 | ✅ 1/1 COMPLETE |

**TOTAL: 37/37 jobs complete (100%)** ✅

**Implementation Location**: `OmegaLLM-3/src/runtime/` (standalone repo)

---

## Purpose
Extract runtime logic from 3 duplicated implementations into single OmegaRuntime class with 24 subsystems.

## Source Files (to be refactored)
- `bin/omega-repl.ts` (3163 lines)
- `bin/omega-debugger.ts` (1197 lines)
- `src/server/debugSession.ts`

## Target Structure
```
src/runtime/
├── index.ts                 # Public API exports
├── OmegaRuntime.ts          # Main facade class
├── types/                   # Type definitions
├── events/                  # Event system
├── providers/               # Pluggable backends
├── subsystems/              # 24 subsystem classes
├── internal/                # Internal utilities
├── protocol/                # nREPL-style protocol
├── config/                  # Configuration
└── testing/                 # Mock/test infrastructure
```

---

## Job Dependency Graph

```
LAYER 0: FOUNDATION (100-199)
┌──────────────────────────────────────────────────────────────────┐
│  100-types ──┬──> 110-events                                     │
│              └──> 120-providers                                  │
└──────────────────────────────────────────────────────────────────┘
                    │
                    ▼
LAYER 1: CORE ENGINE (200-299)
┌──────────────────────────────────────────────────────────────────┐
│  200-macros ──> 210-execution ──> 220-state-coordinator          │
│                                   230-budget-llm-adapter         │
└──────────────────────────────────────────────────────────────────┘
                    │
                    ▼
LAYER 2: DEBUGGING (300-399)
┌──────────────────────────────────────────────────────────────────┐
│  300-breakpoints ──┐                                             │
│  310-inspector ────┼──> 350-debug-subsystem                      │
│  320-history ──────┤                                             │
│  330-snapshots ────┤                                             │
│  340-session ──────┘                                             │
└──────────────────────────────────────────────────────────────────┘
                    │
                    ▼
LAYER 3: DATA & CONTROL (400-599)
┌──────────────────────────────────────────────────────────────────┐
│  400-artifacts ──> 410-facts ──> 420-fixpoint                    │
│  500-conditions ──> 510-transaction                              │
└──────────────────────────────────────────────────────────────────┘
                    │
                    ▼
LAYER 4: GOVERNANCE (600-699)
┌──────────────────────────────────────────────────────────────────┐
│  600-provenance ──> 610-security ──> 620-budget                  │
└──────────────────────────────────────────────────────────────────┘
                    │
                    ▼
LAYER 5: COMMUNICATION (700-799)
┌──────────────────────────────────────────────────────────────────┐
│  700-concurrency ──> 710-protocol                                │
└──────────────────────────────────────────────────────────────────┘
                    │
                    ▼
LAYER 6: LLM INTEGRATION (800-899)
┌──────────────────────────────────────────────────────────────────┐
│  800-llm-integration ──> 810-opr-integration ──> 820-experts     │
└──────────────────────────────────────────────────────────────────┘
                    │
                    ▼
LAYER 7: SEMANTIC COMPUTATION (900-999)
┌──────────────────────────────────────────────────────────────────┐
│  900-amb ──> 910-streams ──> 920-logic                           │
└──────────────────────────────────────────────────────────────────┘
                    │
                    ▼
LAYER 8: ASSEMBLY (1000-1099)
┌──────────────────────────────────────────────────────────────────┐
│  1000-runtime-assembly (combines all subsystems)                 │
└──────────────────────────────────────────────────────────────────┘
                    │
                    ▼
LAYER 9: CONSUMER REFACTORING (1100-1199)
┌──────────────────────────────────────────────────────────────────┐
│  1100-repl-refactor                                              │
│  1110-server-refactor                                            │
│  1120-debugger-refactor                                          │
└──────────────────────────────────────────────────────────────────┘
                    │
                    ▼
LAYER 10: TESTING (1200-1299)
┌──────────────────────────────────────────────────────────────────┐
│  1200-mocks ──> 1210-unit-tests ──> 1220-integration-tests       │
│              ──> 1230-e2e-tests ──> 1240-demos                   │
└──────────────────────────────────────────────────────────────────┘
                    │
                    ▼
LAYER 11: DOCUMENTATION (1300-1399)
┌──────────────────────────────────────────────────────────────────┐
│  1300-documentation                                              │
└──────────────────────────────────────────────────────────────────┘
```

---

## Complete Job List

| Job ID | File | Title | Depends On | Status |
|--------|------|-------|------------|--------|
| **LAYER 0: FOUNDATION** |
| 100 | [100-types.md](100-types.md) | Core Type Definitions | - | ✅ COMPLETE |
| 110 | [110-events.md](110-events.md) | Event System | 100 | ✅ COMPLETE |
| 120 | [120-providers.md](120-providers.md) | Provider Interfaces | 100 | ✅ COMPLETE |
| **LAYER 1: CORE ENGINE** |
| 200 | [200-macros.md](200-macros.md) | Macro Manager | 100 | ✅ COMPLETE |
| 210 | [210-execution.md](210-execution.md) | Execution Engine | 100,110,120,200 | ✅ COMPLETE |
| 220 | [220-state-coordinator.md](220-state-coordinator.md) | State Coordinator | 100,110 | ✅ COMPLETE |
| 230 | [230-budget-llm-adapter.md](230-budget-llm-adapter.md) | Budget-Aware LLM Adapter | 100,110 | ✅ COMPLETE |
| **LAYER 2: DEBUGGING** |
| 300 | [300-breakpoints.md](300-breakpoints.md) | Breakpoint Manager | 100,110 | ✅ COMPLETE |
| 310 | [310-inspector.md](310-inspector.md) | State Inspector | 100 | ✅ COMPLETE |
| 320 | [320-history.md](320-history.md) | History Manager | 100,110,120 | ✅ COMPLETE |
| 330 | [330-snapshots.md](330-snapshots.md) | Snapshot Manager | 100,120,220 | ✅ COMPLETE |
| 340 | [340-session.md](340-session.md) | Session Manager | 100,110,120 | ✅ COMPLETE |
| 350 | [350-debug-subsystem.md](350-debug-subsystem.md) | Debug Subsystem | 210,300,320 | ✅ COMPLETE |
| **LAYER 3: DATA & CONTROL** |
| 400 | [400-artifacts.md](400-artifacts.md) | Artifact Manager | 100,120 | ✅ COMPLETE |
| 410 | [410-facts.md](410-facts.md) | Facts Manager | 100,110 | ✅ COMPLETE |
| 420 | [420-fixpoint.md](420-fixpoint.md) | Fixpoint Manager | 100,110,400,410 | ✅ COMPLETE |
| 500 | [500-conditions.md](500-conditions.md) | Conditions Manager | 100,110 | ✅ COMPLETE |
| 510 | [510-transaction.md](510-transaction.md) | Transaction Manager | 100,110,220 | ✅ COMPLETE |
| **LAYER 4: GOVERNANCE** |
| 600 | [600-provenance.md](600-provenance.md) | Provenance Manager | 100,120 | ✅ COMPLETE |
| 610 | [610-security.md](610-security.md) | Security Manager | 100,110,120 | ✅ COMPLETE |
| 620 | [620-budget.md](620-budget.md) | Budget Manager | 100,110,120 | ✅ COMPLETE |
| **LAYER 5: COMMUNICATION** |
| 700 | [700-concurrency.md](700-concurrency.md) | Concurrency Manager | 100,110 | ✅ COMPLETE |
| 710 | [710-protocol.md](710-protocol.md) | Protocol Server | 100,210 | ✅ COMPLETE |
| **LAYER 6: LLM INTEGRATION** |
| 800 | [800-llm-integration.md](800-llm-integration.md) | LLM Integration | 110,210,230 | ✅ COMPLETE |
| 810 | [810-opr-integration.md](810-opr-integration.md) | OPR Integration | 110,120,210 | ✅ COMPLETE |
| 820 | [820-experts.md](820-experts.md) | Expert Manager | 100,110,800 | ✅ COMPLETE |
| **LAYER 7: SEMANTIC** |
| 900 | [900-amb.md](900-amb.md) | AMB Manager | 100,110,120,210,220,800 | ✅ COMPLETE |
| 910 | [910-streams.md](910-streams.md) | Streams Manager | 100,110,210 | ✅ COMPLETE |
| 920 | [920-logic.md](920-logic.md) | Logic Manager | 100,110,410,800 | ✅ COMPLETE |
| **LAYER 8: ASSEMBLY** |
| 1000 | [1000-runtime-assembly.md](1000-runtime-assembly.md) | OmegaRuntime Assembly | ALL ABOVE | ✅ COMPLETE |
| **LAYER 9: CONSUMER REFACTORING** |
| 1100 | [1100-repl-refactor.md](1100-repl-refactor.md) | REPL Refactor | 1000 | ✅ COMPLETE |
| 1110 | [1110-server-refactor.md](1110-server-refactor.md) | Server Refactor | 1000 | ✅ COMPLETE |
| 1120 | [1120-debugger-refactor.md](1120-debugger-refactor.md) | Debugger Refactor | 1000 | ✅ COMPLETE |
| **LAYER 10: TESTING** |
| 1200 | [1200-mocks.md](1200-mocks.md) | Mock Infrastructure | 120 | ✅ COMPLETE |
| 1210 | [1210-unit-tests.md](1210-unit-tests.md) | Unit Tests | 1000,1200 | ✅ COMPLETE (1246 tests) |
| 1220 | [1220-integration-tests.md](1220-integration-tests.md) | Integration Tests | 1000,1200 | ✅ COMPLETE |
| 1230 | [1230-e2e-tests.md](1230-e2e-tests.md) | E2E CLI Tests | 1100 | ✅ COMPLETE |
| 1240 | [1240-demos.md](1240-demos.md) | Demo Coverage | 1000,1200 | ✅ COMPLETE |
| **LAYER 11: DOCUMENTATION** |
| 1300 | [1300-documentation.md](1300-documentation.md) | API Documentation | 1000 | ✅ COMPLETE |

---

## Design Flaws Resolved

Each job file includes resolution for these critical flaws:

| Flaw | Description | Resolved In |
|------|-------------|-------------|
| F1 | Budget not wired through all LLM paths | 230-budget-llm-adapter.md |
| F2 | State conflict Amb/Snapshot/History | 220-state-coordinator.md |
| F3 | Concurrency model undefined | 700-concurrency.md |
| F4 | EvalResult vs Outcome undefined | 100-types.md |
| F5 | Missing event listener spec | 110-events.md |
| F6 | Missing YAML dependencies | Each job's depends_on |
| F7 | Transaction ↔ Amb circular | 220-state-coordinator.md |
| F8 | Debugger can't inspect semantic state | 1000-runtime-assembly.md |

---

## Source Document References

| Document | Features Referenced |
|----------|---------------------|
| ARCHITECTURE/01-OVERVIEW.md | Core architecture |
| ARCHITECTURE/02-CESK.md | Machine model |
| ARCHITECTURE/06-CONDITIONS.md | Conditions system |
| ARCHITECTURE/08-PROTOCOL.md | nREPL protocol |
| ARCHITECTURE/11-MACROS.md | Macro system |
| ARCHITECTURE/12-CONCURRENCY.md | Concurrency model |
| ARCHITECTURE/21-SECURITY.md | Capability system |
| ARCHITECTURE/22-PROVENANCE.md | Evidence tracking |
| ARCHITECTURE/23-FACTS.md | Fact store |
| ARCHITECTURE/24-FIXPOINT.md | Convergence |
| ARCHITECTURE/25-BUDGET.md | Resource limits |
| ARCHITECTURE/26-ARTIFACTS.md | Memoization |
| ARCHITECTURE/27-OUTCOMES.md | Result types |
| ARCHITECTURE/29-EXPERTS.md | Expert system |
| docs/USER-MANUAL--05-*.md | AMB system |
| docs/USER-MANUAL--07-*.md | Streams |
| docs/USER-MANUAL--09-*.md | Image persistence |
| docs/USER-MANUAL--22-*.md | Concurrency patterns |
| docs/USER-MANUAL--23-*.md | Streams of inference |
| docs/USER-MANUAL--26-*.md | AMB inference engine |
| docs/USER-MANUAL--27-*.md | Logic programming |
