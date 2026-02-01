# 230: Budget-Aware LLM Adapter (FLAW F1 Resolution)

## Status: COMPLETE ✅
- **Completed**: 2026-01-29
- **Location**: OmegaLLM-3/src/runtime/subsystems/BudgetAwareLLMAdapter.ts (7707 bytes)

## Purpose
Decorator that wraps all LLM calls to enforce budget limits. Ensures no LLM operation can bypass resource tracking.

## Dependencies
- 100-types.md ✅
- 110-events.md ✅

## Source References
- Design pattern: Decorator (GoF)
- FLAW F1: Budget consumption not wired through all LLM call paths
- ARCHITECTURE/25-BUDGET.md

---

## Problem Solved

**Without BudgetAwareLLMAdapter:**
```
              ┌─────────────┐
              │   Runtime   │
              └──────┬──────┘
                     │
     ┌───────────────┼───────────────┐
     │               │               │
     ▼               ▼               ▼
┌─────────┐   ┌─────────────┐   ┌─────────┐
│   AMB   │   │   Experts   │   │  Logic  │
└────┬────┘   └──────┬──────┘   └────┬────┘
     │               │               │
     ▼               ▼               ▼
┌─────────────────────────────────────────┐
│           LLMProvider (direct)          │  ← NO BUDGET CHECK!
└─────────────────────────────────────────┘
```

**With BudgetAwareLLMAdapter:**
```
              ┌─────────────┐
              │   Runtime   │
              └──────┬──────┘
                     │
     ┌───────────────┼───────────────┐
     │               │               │
     ▼               ▼               ▼
┌─────────┐   ┌─────────────┐   ┌─────────┐
│   AMB   │   │   Experts   │   │  Logic  │
└────┬────┘   └──────┬──────┘   └────┬────┘
     │               │               │
     └───────────────┼───────────────┘
                     │
                     ▼
┌─────────────────────────────────────────┐
│       BudgetAwareLLMAdapter             │  ← ALL CALLS GO HERE
│  ┌───────────────────────────────────┐  │
│  │ 1. Check budget                   │  │
│  │ 2. Emit 'llm-call-start'          │  │
│  │ 3. Call underlying provider       │  │
│  │ 4. Record consumption             │  │
│  │ 5. Emit 'llm-call-complete'       │  │
│  └───────────────────────────────────┘  │
└──────────────────┬──────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────┐
│           LLMProvider (actual)          │
└─────────────────────────────────────────┘
```

---

## Deliverables

```
src/runtime/internal/
├── BudgetAwareLLMAdapter.ts    # Main adapter class
└── BudgetLimits.ts             # Budget limit types
```

---

## Key Interface

```typescript
export interface BudgetLimits {
  maxTokens?: number;        // Total token limit
  maxCalls?: number;         // Total call count limit
  maxCost?: number;          // Cost in dollars
  maxDuration?: number;      // Milliseconds
}

export interface BudgetState {
  tokensUsed: number;
  callsUsed: number;
  costUsed: number;
  durationUsed: number;
}

export interface BudgetAwareLLMAdapter {
  /**
   * Check if operation can proceed within budget.
   * Throws BudgetExceededError if not.
   */
  checkBudget(estimatedTokens?: number): void;

  /**
   * Wrap an LLM call with budget tracking.
   * Records before/after metrics and emits events.
   */
  wrapCall<T>(
    operation: string,
    estimatedTokens: number,
    call: () => Promise<LLMResponse>
  ): Promise<LLMResponse>;

  /**
   * Get current budget consumption.
   */
  getState(): BudgetState;

  /**
   * Get remaining budget.
   */
  getRemaining(): BudgetLimits;

  /**
   * Reset budget (for new session).
   */
  reset(): void;

  /**
   * Check if any budget limit is exceeded.
   */
  isExceeded(): boolean;

  /**
   * Set/update budget limits.
   */
  setLimits(limits: Partial<BudgetLimits>): void;
}
```

---

## Implementation

```typescript
export class BudgetAwareLLMAdapterImpl implements BudgetAwareLLMAdapter {
  private state: BudgetState = {
    tokensUsed: 0,
    callsUsed: 0,
    costUsed: 0,
    durationUsed: 0
  };

  constructor(
    private readonly provider: LLMProvider,
    private limits: BudgetLimits,
    private readonly emitter: RuntimeEventEmitter
  ) {}

  checkBudget(estimatedTokens?: number): void {
    if (this.limits.maxTokens !== undefined) {
      const projected = this.state.tokensUsed + (estimatedTokens ?? 0);
      if (projected > this.limits.maxTokens) {
        throw new BudgetExceededError('tokens', this.limits.maxTokens, projected);
      }
    }

    if (this.limits.maxCalls !== undefined) {
      if (this.state.callsUsed >= this.limits.maxCalls) {
        throw new BudgetExceededError('calls', this.limits.maxCalls, this.state.callsUsed);
      }
    }

    // Similar checks for cost and duration...
  }

  async wrapCall<T>(
    operation: string,
    estimatedTokens: number,
    call: () => Promise<LLMResponse>
  ): Promise<LLMResponse> {
    // Pre-check
    this.checkBudget(estimatedTokens);

    const startTime = Date.now();

    // Emit start event
    this.emitter.emit('llm-call-start', {
      operation,
      estimatedTokens,
      budgetState: { ...this.state }
    });

    try {
      // Make the actual call
      const response = await call();

      // Record consumption
      const duration = Date.now() - startTime;
      this.state.tokensUsed += response.usage?.totalTokens ?? 0;
      this.state.callsUsed += 1;
      this.state.costUsed += this.estimateCost(response);
      this.state.durationUsed += duration;

      // Emit complete event
      this.emitter.emit('llm-call-complete', {
        operation,
        actualTokens: response.usage?.totalTokens ?? 0,
        duration,
        budgetState: { ...this.state }
      });

      return response;
    } catch (error) {
      // Emit error event
      this.emitter.emit('llm-call-error', {
        operation,
        error,
        budgetState: { ...this.state }
      });
      throw error;
    }
  }

  private estimateCost(response: LLMResponse): number {
    // Model-specific pricing
    const inputCost = (response.usage?.promptTokens ?? 0) * 0.00001;
    const outputCost = (response.usage?.completionTokens ?? 0) * 0.00003;
    return inputCost + outputCost;
  }

  // ... other methods
}
```

---

## BudgetExceededError

```typescript
export class BudgetExceededError extends Error {
  constructor(
    public readonly resource: 'tokens' | 'calls' | 'cost' | 'duration',
    public readonly limit: number,
    public readonly current: number
  ) {
    super(`Budget exceeded: ${resource} limit ${limit}, current ${current}`);
  }
}
```

---

## Integration Pattern

All subsystems that call LLMs MUST use the adapter:

```typescript
// In AmbManager
class AmbManager {
  constructor(private llmAdapter: BudgetAwareLLMAdapter) {}

  async evaluateAlternative(expr: Val): Promise<Val> {
    // WRONG: Direct provider call
    // const result = await this.provider.complete(prompt);

    // RIGHT: Use adapter
    const result = await this.llmAdapter.wrapCall(
      'amb-alternative',
      100, // estimated tokens
      () => this.provider.complete(prompt)
    );
    return result;
  }
}

// In ExpertsManager
class ExpertsManager {
  constructor(private llmAdapter: BudgetAwareLLMAdapter) {}

  async consultExpert(expert: Expert, question: string): Promise<string> {
    return this.llmAdapter.wrapCall(
      'expert-consult',
      500,
      () => expert.respond(question)
    );
  }
}
```

---

## Test Requirements

### Unit Tests: `tests/runtime/internal/BudgetAwareLLMAdapter.test.ts`
- [ ] checkBudget() passes when within limits
- [ ] checkBudget() throws BudgetExceededError when over token limit
- [ ] checkBudget() throws BudgetExceededError when over call limit
- [ ] wrapCall() records token usage correctly
- [ ] wrapCall() records call count correctly
- [ ] wrapCall() emits 'llm-call-start' event
- [ ] wrapCall() emits 'llm-call-complete' event
- [ ] wrapCall() emits 'llm-call-error' on failure
- [ ] getRemaining() calculates correct remaining budget
- [ ] reset() clears all usage counters
- [ ] setLimits() updates limits dynamically

### Integration Tests
- [ ] AMB operations are budget-tracked
- [ ] Expert consultations are budget-tracked
- [ ] Logic queries are budget-tracked
- [ ] Budget exceeded stops all LLM operations
- [ ] Events can be used to build usage dashboard

---

## Acceptance Criteria
1. ALL LLM calls go through the adapter (verified by code review)
2. Budget limits are enforced correctly
3. Events provide complete audit trail
4. Cost estimation is reasonably accurate
5. BudgetExceededError provides clear guidance
6. Performance overhead: <5ms per call
