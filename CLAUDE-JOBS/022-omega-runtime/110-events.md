# 110: Event System

## Status: COMPLETE ✅
- **Completed**: 2026-01-29
- **Location**: OmegaLLM-3/src/runtime/events/
- **Note**: 40+ event types defined in RuntimeEventMap.ts

## Purpose
Type-safe event emitter for runtime hooks enabling consumer notification without coupling.

## Dependencies
- 100-types.md ✅

## Source References
- Design pattern: Observer (GoF)
- FLAW F5: Required listeners specification

---

## Deliverables

```
src/runtime/events/
├── index.ts                    # Re-exports
├── RuntimeEventMap.ts          # Event name → payload type mapping
├── RuntimeEventEmitter.ts      # Type-safe emitter class
├── Unsubscribe.ts              # Unsubscribe function type
│
├── # Core Events
├── StepEvent.ts
├── EffectEvent.ts
├── BeforeLLMEvent.ts
├── AfterLLMEvent.ts
├── BreakpointHitEvent.ts
├── ErrorEvent.ts
├── DoneEvent.ts
├── SessionCheckpointEvent.ts
├── OprReceiptEvent.ts
├── LLMToolCallEvent.ts
│
├── # Control Flow Events
├── ConditionEvent.ts
├── FactAssertedEvent.ts
├── FixpointIterEvent.ts
├── FixpointDoneEvent.ts
│
├── # Governance Events
├── BudgetWarningEvent.ts
├── BudgetExceededEvent.ts
├── SecurityEvent.ts
├── TaskCompleteEvent.ts
│
├── # Semantic Events
├── AmbChooseEvent.ts
├── AmbFailEvent.ts
├── AmbSolutionEvent.ts
├── StreamForceEvent.ts
├── FactQueriedEvent.ts
├── RuleAppliedEvent.ts
├── IntentCompiledEvent.ts
├── ExpertResultEvent.ts
├── MacroExpandEvent.ts
│
└── # Concurrency Events
    └── DeadlockDetectedEvent.ts
```

---

## Key Interfaces

### RuntimeEventMap (FLAW F5)
```typescript
export interface RuntimeEventMap {
  // Core execution
  'step': StepEvent;
  'effect': EffectEvent;
  'error': ErrorEvent;
  'done': DoneEvent;

  // LLM
  'before-llm': BeforeLLMEvent;
  'after-llm': AfterLLMEvent;
  'llm-tool-call': LLMToolCallEvent;

  // Debug
  'breakpoint-hit': BreakpointHitEvent;
  'session-checkpoint': SessionCheckpointEvent;

  // OPR
  'opr-receipt': OprReceiptEvent;

  // Conditions/Facts
  'condition': ConditionEvent;
  'fact-asserted': FactAssertedEvent;

  // Fixpoint
  'fixpoint-iter': FixpointIterEvent;
  'fixpoint-done': FixpointDoneEvent;

  // Budget/Security
  'budget-warning': BudgetWarningEvent;
  'budget-exceeded': BudgetExceededEvent;
  'security-event': SecurityEvent;

  // Concurrency
  'task-complete': TaskCompleteEvent;
  'deadlock-detected': DeadlockDetectedEvent;

  // AMB
  'amb-choose': AmbChooseEvent;
  'amb-fail': AmbFailEvent;
  'amb-solution': AmbSolutionEvent;

  // Streams
  'stream-force': StreamForceEvent;

  // Logic
  'fact-queried': FactQueriedEvent;
  'rule-applied': RuleAppliedEvent;

  // Expert
  'intent-compiled': IntentCompiledEvent;
  'expert-result': ExpertResultEvent;

  // Macro
  'macro-expand': MacroExpandEvent;
}
```

### RuntimeEventEmitter (Type-Safe)
```typescript
export type Unsubscribe = () => void;

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
   * Remove all handlers for an event (or all events)
   */
  removeAllListeners<E extends keyof RuntimeEventMap>(event?: E): void;

  /**
   * Get listener count for event
   */
  listenerCount<E extends keyof RuntimeEventMap>(event: E): number;
}
```

---

## Required Internal Listeners (FLAW F5)

These listeners MUST be wired during runtime assembly:

| Event | Listener | Action |
|-------|----------|--------|
| `step` | HistoryManager | Record if recording enabled |
| `step` | DebugSubsystem | Check breakpoints if debugging |
| `budget-exceeded` | ExecutionEngine | Stop evaluation |
| `budget-exceeded` | LLMIntegration | Reject LLM calls |
| `fact-asserted` | FixpointManager | Update signature |
| `fact-asserted` | ArtifactManager | Invalidate dependent cache |
| `security-event` (deny) | ExecutionEngine | Block if 'block' type |
| `amb-fail` | TransactionManager | Rollback proposals |

---

## Event Payload Examples

### StepEvent
```typescript
export interface StepEvent {
  stepNumber: number;
  control: string;        // Pretty-printed control expression
  envSize: number;        // Number of bindings
  stackDepth: number;     // Continuation depth
  timestamp: number;
}
```

### BudgetExceededEvent
```typescript
export interface BudgetExceededEvent {
  type: 'tokens' | 'cost' | 'time' | 'iterations';
  limit: number;
  consumed: number;
  operation: string;      // What tried to exceed
}
```

### AmbChooseEvent
```typescript
export interface AmbChooseEvent {
  choicePointId: string;
  alternativeCount: number;
  currentIndex: number;
  chosenValue: Val;
}
```

---

## Test Requirements

### Unit Tests: `tests/runtime/events/RuntimeEventEmitter.test.ts`
- [ ] on() returns working unsubscribe function
- [ ] emit() calls all subscribed handlers
- [ ] once() fires exactly once
- [ ] off() removes specific handler
- [ ] removeAllListeners() clears handlers
- [ ] Type safety: wrong payload type fails at compile time
- [ ] Handlers called in subscription order

---

## Acceptance Criteria
1. All events in RuntimeEventMap have corresponding payload type
2. Emitter is fully type-safe (compile-time errors for mismatched types)
3. No memory leaks from forgotten subscriptions
4. Event emission is synchronous (handlers complete before emit returns)
