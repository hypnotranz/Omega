# 100: Core Type Definitions

## Status: COMPLETE ✅
- **Completed**: 2026-01-29
- **Location**: OmegaLLM-3/src/runtime/types/
- **Note**: Types consolidated into fewer files (SessionTypes.ts, LLMTypes.ts, etc.) instead of one-per-type

## Purpose
Define all public types and interfaces for the OmegaRuntime API.

## Dependencies
- None (foundation layer)
- **Status**: N/A (no dependencies)

## Source References
- ARCHITECTURE/02-CESK.md (Val, State, Store)
- ARCHITECTURE/27-OUTCOMES.md (Outcome types)
- ARCHITECTURE/25-BUDGET.md (Budget types)
- ARCHITECTURE/26-ARTIFACTS.md (Artifact types)

---

## Deliverables

### Primary Types
```
src/runtime/types/
├── index.ts                 # Re-exports all types
├── RuntimeConfig.ts         # Configuration options
├── Val.ts                   # Re-export from core (alias)
├── State.ts                 # CESK machine state
├── Store.ts                 # Heap (Map<Address, Val>)
└── Env.ts                   # Environment
```

### Result Types (FLAW F4 RESOLVED)
```
src/runtime/types/
├── Outcome.ts               # Base structured result type
├── OutcomeMetadata.ts       # Common metadata
├── NeedType.ts              # needs-evidence, needs-reframing, etc.
├── Proposal.ts              # For transaction mode
├── EvalResult.ts            # Outcome<Val> + eval-specific fields
├── StepResult.ts            # Single step result
├── RunResult.ts             # Run-to-completion result
└── KernelResult.ts          # OPR kernel result
```

### Debug Types
```
src/runtime/types/
├── DebugState.ts            # Current debug state
├── TraceEntry.ts            # Single trace entry
├── Breakpoint.ts            # Breakpoint definition
├── BreakpointSpec.ts        # Breakpoint creation spec
└── ControlInfo.ts           # Control expression info
```

### Session Types
```
src/runtime/types/
├── SnapshotInfo.ts          # Snapshot metadata
├── SessionInfo.ts           # Session metadata
├── SessionData.ts           # Full session data
├── SessionTrace.ts          # Session trace
├── Checkpoint.ts            # Named checkpoint
├── HistoryEntry.ts          # History entry
└── MachineSnapshot.ts       # Serialized machine state
```

### Stack/Frame Types
```
src/runtime/types/
├── StackFrame.ts            # Stack frame info
├── FrameDetail.ts           # Detailed frame view
└── EnvBindings.ts           # Environment bindings
```

### LLM Types
```
src/runtime/types/
├── LLMRequest.ts            # LLM call request
├── LLMResponse.ts           # LLM call response
├── LLMTrace.ts              # LLM trace entry
├── LLMTraceSummary.ts       # Summary view
├── LLMTraceDetail.ts        # Detailed view
├── AgentResult.ts           # Agent loop result
└── ToolCallRecord.ts        # Tool call record
```

### OPR Types
```
src/runtime/types/
├── KernelInfo.ts            # Kernel metadata
├── KernelRegistry.ts        # Kernel registry
├── Receipt.ts               # OPR receipt
└── VerifyResult.ts          # Verification result
```

### Effect Types
```
src/runtime/types/
├── EffectContext.ts         # Effect execution context
├── EffectResult.ts          # Effect result
├── EffectRecord.ts          # Recorded effect
└── Definition.ts            # Define result
```

### AMB Types (900-amb.md)
```
src/runtime/types/
├── ChoicePoint.ts           # Backtracking state
├── AmbResult.ts             # AMB computation result
└── BacktrackState.ts        # Full backtrack stack
```

### Stream Types (910-streams.md)
```
src/runtime/types/
├── LazyStream.ts            # Stream type
└── StreamElement.ts         # Element or thunk
```

### Logic Types (920-logic.md)
```
src/runtime/types/
├── SemanticFact.ts          # Natural language fact
├── SemanticRule.ts          # If-then rule
├── BindingFrame.ts          # Variable bindings
├── QueryResult.ts           # Query result
└── ProofResult.ts           # Proof result
```

### Expert Types (820-experts.md)
```
src/runtime/types/
├── ExpertRole.ts            # Role definition
├── OutputMode.ts            # REPORT, PLAN, PROGRAM, ANALYSIS
├── TaskEnvelope.ts          # Per-call context
├── IntentResult.ts          # Compiled intent
└── ValidationResult.ts      # Output validation
```

### Macro Types (200-macros.md)
```
src/runtime/types/
├── Macro.ts                 # Macro definition
└── MacroExpansion.ts        # Expansion result
```

### Concurrency Types (700-concurrency.md)
```
src/runtime/types/
├── Fiber.ts                 # Fiber handle
├── FiberStatus.ts           # ready/running/waiting/completed/failed
├── Mutex.ts                 # Mutex handle
├── Channel.ts               # Channel handle
├── Actor.ts                 # Actor handle
└── ActorMessage.ts          # Actor message
```

### Type Aliases (for cleaner API)
```
src/runtime/types/aliases.ts
─────────────────────────────
// Re-export core types with consistent names
export { OpCall as Effect } from '../../core/effects/opcall';
export { OprReceipt as Receipt } from '../../core/opr/types';
export { Budget } from '../../core/governance/budgets';
```

---

## Key Type Definitions

### Outcome<T> (FLAW F4)
```typescript
export type Outcome<T = Val> =
  | { tag: 'ok'; value: T; metadata: OutcomeMetadata }
  | { tag: 'error'; message: string; errorType: string; stacktrace?: string[]; metadata: OutcomeMetadata }
  | { tag: 'needs'; needType: NeedType; description: string; context?: Val; metadata: OutcomeMetadata }
  | { tag: 'proposed'; value: T; proposals: Proposal[]; metadata: OutcomeMetadata }
  | { tag: 'nonconverged'; value: T; iterations: number; reason: 'budget' | 'max-iterations'; metadata: OutcomeMetadata }
  | { tag: 'cycle'; value: T; cycleLength: number; metadata: OutcomeMetadata };

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
```

### EvalResult (extends Outcome)
```typescript
export interface EvalResult extends Outcome<Val> {
  stepCount: number;
  definitions: string[];
  effects: EffectRecord[];
  trace?: TraceEntry[];
}
```

### RuntimeConfig
```typescript
export interface RuntimeConfig {
  // Providers (all optional - defaults provided)
  stateProvider?: StateProvider;
  sessionProvider?: SessionProvider;
  snapshotProvider?: SnapshotProvider;
  receiptProvider?: ReceiptProvider;
  traceProvider?: TraceProvider;
  artifactProvider?: ArtifactProvider;
  governanceProvider?: GovernanceProvider;
  securityProvider?: SecurityProvider;
  provenanceProvider?: ProvenanceProvider;
  logicProvider?: LogicProvider;
  expertProvider?: ExpertProvider;
  streamProvider?: StreamProvider;
  effectRegistry?: EffectHandlerRegistry;

  // LLM configuration
  llmAdapter?: LLMAdapter;
  llmMode?: 'mock' | 'live' | 'scripted';

  // Budget limits
  budgetLimits?: BudgetLimits;

  // Debug options
  recordHistory?: boolean;
  maxHistorySize?: number;

  // Protocol options
  protocolTransport?: 'stdio' | 'websocket' | 'http';
  protocolPort?: number;
}
```

---

## Test Requirements

### Unit Tests: `tests/runtime/types/`
- [ ] Outcome type guards work correctly
- [ ] EvalResult extends Outcome properly
- [ ] Type aliases resolve correctly
- [ ] RuntimeConfig defaults are valid
- [ ] All types are JSON-serializable where needed

### Type Safety Tests
- [ ] TypeScript compiler catches invalid Outcome construction
- [ ] Event payloads match RuntimeEventMap
- [ ] Provider interfaces are correctly typed

---

## Acceptance Criteria
1. All types compile without errors
2. index.ts exports all public types
3. No circular imports
4. Types match source document specifications
5. JSDoc comments on all public types
