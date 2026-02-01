# 620: Budget Manager

## Status: COMPLETE ✅
- **Completed**: 2026-01-29
- **Location**: OmegaLLM-3/src/runtime/subsystems/BudgetManager.ts (594 lines)

## Purpose
Manages resource budgets - tracking consumption and enforcing limits for tokens, costs, time, and operations.

## Dependencies
- 100-types.md ✅
- 110-events.md ✅
- 120-providers.md ✅

## Source References
- ARCHITECTURE/25-BUDGET.md
- 230-budget-llm-adapter.md (wraps LLM calls)
- Cloud cost management patterns

---

## Deliverables

```
src/runtime/subsystems/
├── BudgetManager.ts         # Main budget manager
└── budget/
    ├── BudgetConfig.ts      # Budget configuration
    ├── BudgetTracker.ts     # Consumption tracking
    └── BudgetAlerts.ts      # Threshold alerts
```

---

## Key Types

```typescript
export interface BudgetConfig {
  tokens?: {
    max: number;
    perCall?: number;        // Per-call limit
    perMinute?: number;      // Rate limit
  };
  cost?: {
    max: number;             // Dollars
    perSession?: number;
  };
  time?: {
    maxDuration: number;     // Total session time
    perEval: number;         // Per evaluation
  };
  operations?: {
    maxLLMCalls: number;
    maxEvals: number;
    maxSteps: number;
  };
  alerts?: BudgetAlert[];
}

export interface BudgetAlert {
  resource: 'tokens' | 'cost' | 'time' | 'operations';
  threshold: number;         // 0-1 percentage
  action: 'warn' | 'pause' | 'stop';
  message?: string;
}

export interface BudgetState {
  tokens: {
    used: number;
    remaining: number;
    rate: number;            // Tokens per minute
  };
  cost: {
    used: number;
    remaining: number;
  };
  time: {
    elapsed: number;
    remaining: number;
  };
  operations: {
    llmCalls: number;
    evals: number;
    steps: number;
  };
  alerts: TriggeredAlert[];
}

export interface TriggeredAlert {
  alert: BudgetAlert;
  triggeredAt: number;
  currentValue: number;
}
```

---

## Key Interface

```typescript
export interface BudgetManager {
  // ─── Configuration ───

  /**
   * Set budget configuration.
   */
  configure(config: BudgetConfig): void;

  /**
   * Get current configuration.
   */
  getConfig(): BudgetConfig;

  /**
   * Update specific limits.
   */
  setLimit(resource: string, limit: number): void;

  // ─── Tracking ───

  /**
   * Record token consumption.
   */
  recordTokens(count: number, source?: string): void;

  /**
   * Record cost.
   */
  recordCost(amount: number, source?: string): void;

  /**
   * Record LLM call.
   */
  recordLLMCall(): void;

  /**
   * Record evaluation.
   */
  recordEval(): void;

  /**
   * Record execution steps.
   */
  recordSteps(count: number): void;

  // ─── Queries ───

  /**
   * Get current budget state.
   */
  getState(): BudgetState;

  /**
   * Check if resource is available.
   */
  canConsume(resource: string, amount: number): boolean;

  /**
   * Check if any budget exceeded.
   */
  isExceeded(): boolean;

  /**
   * Get specific resource remaining.
   */
  getRemaining(resource: string): number;

  /**
   * Get usage percentage.
   */
  getUsagePercent(resource: string): number;

  // ─── Control ───

  /**
   * Reset all consumption tracking.
   */
  reset(): void;

  /**
   * Pause on budget alert.
   */
  pauseOnAlert(): void;

  /**
   * Resume after pause.
   */
  resume(): void;

  /**
   * Check if paused due to budget.
   */
  isPaused(): boolean;
}
```

---

## Alert Processing

```typescript
class BudgetManagerImpl implements BudgetManager {
  private checkAlerts(): void {
    const state = this.getState();

    for (const alert of this.config.alerts ?? []) {
      const usage = this.getUsagePercent(alert.resource);

      if (usage >= alert.threshold) {
        // Check if already triggered
        if (this.triggeredAlerts.has(alert)) continue;

        this.triggeredAlerts.add(alert);

        const triggered: TriggeredAlert = {
          alert,
          triggeredAt: Date.now(),
          currentValue: usage
        };

        this.emitter.emit('budget-alert', triggered);

        switch (alert.action) {
          case 'warn':
            console.warn(`Budget alert: ${alert.message ?? alert.resource + ' at ' + (usage * 100).toFixed(0) + '%'}`);
            break;
          case 'pause':
            this.pauseOnAlert();
            break;
          case 'stop':
            throw new BudgetExceededError(alert.resource, this.config[alert.resource]?.max, usage);
        }
      }
    }
  }

  recordTokens(count: number, source?: string): void {
    // Check pre-consumption
    if (!this.canConsume('tokens', count)) {
      throw new BudgetExceededError('tokens', this.config.tokens?.max, this.state.tokens.used + count);
    }

    // Record
    this.state.tokens.used += count;
    this.updateRate('tokens', count);

    // Emit event
    this.emitter.emit('budget-consumed', {
      resource: 'tokens',
      amount: count,
      source,
      remaining: this.state.tokens.remaining
    });

    // Check alerts
    this.checkAlerts();
  }
}
```

---

## Rate Limiting

```typescript
private rateWindows: Map<string, { timestamp: number; count: number }[]> = new Map();

private updateRate(resource: string, count: number): void {
  const now = Date.now();
  const window = this.rateWindows.get(resource) ?? [];

  // Add new entry
  window.push({ timestamp: now, count });

  // Remove entries older than 1 minute
  const oneMinuteAgo = now - 60000;
  const filtered = window.filter(e => e.timestamp > oneMinuteAgo);
  this.rateWindows.set(resource, filtered);

  // Calculate rate
  const rate = filtered.reduce((sum, e) => sum + e.count, 0);

  // Check rate limit
  const limit = this.config[resource]?.perMinute;
  if (limit && rate > limit) {
    throw new RateLimitError(resource, limit, rate);
  }
}
```

---

## Lisp Interface

```lisp
;; Configure budget
(budget.configure
  :tokens (:max 100000 :per-minute 10000)
  :cost (:max 5.00)
  :alerts ((:resource 'tokens :threshold 0.8 :action 'warn)))

;; Check remaining
(budget.remaining 'tokens)
; => 75000

;; Check usage
(budget.usage 'cost)
; => 0.35  ; 35% used

;; Get full state
(budget.state)
; => (:tokens (:used 25000 :remaining 75000 :rate 5000)
;     :cost (:used 1.75 :remaining 3.25)
;     ...)

;; Manual tracking
(budget.record 'tokens 500 :source "manual-call")

;; Reset
(budget.reset)
```

---

## Integration with BudgetAwareLLMAdapter

```typescript
// The BudgetManager provides the authoritative state
// The BudgetAwareLLMAdapter uses it for enforcement

class BudgetAwareLLMAdapter {
  constructor(
    private provider: LLMProvider,
    private budgetManager: BudgetManager,
    private emitter: RuntimeEventEmitter
  ) {}

  async wrapCall(...): Promise<LLMResponse> {
    // Check with budget manager
    if (!this.budgetManager.canConsume('operations', 1)) {
      throw new BudgetExceededError('operations', ...);
    }

    // Make call
    const response = await this.provider.complete(...);

    // Record consumption
    this.budgetManager.recordTokens(response.usage?.totalTokens ?? 0, 'llm');
    this.budgetManager.recordCost(this.estimateCost(response), 'llm');
    this.budgetManager.recordLLMCall();

    return response;
  }
}
```

---

## Test Requirements

### Unit Tests: `tests/runtime/subsystems/BudgetManager.test.ts`
- [ ] configure() sets config
- [ ] recordTokens() tracks correctly
- [ ] recordCost() tracks correctly
- [ ] canConsume() checks limits
- [ ] isExceeded() detects exceeded
- [ ] getRemaining() calculates correctly
- [ ] getUsagePercent() calculates correctly
- [ ] Alerts trigger at threshold
- [ ] Rate limiting works
- [ ] reset() clears state

### Integration Tests
- [ ] LLM calls consume budget
- [ ] Evaluations consume budget
- [ ] Alert actions fire correctly
- [ ] Pause/resume works
- [ ] Budget survives session reload

---

## Acceptance Criteria
1. All resource consumption is tracked
2. Limits prevent overspending
3. Alerts warn before limits hit
4. Rate limiting prevents burst usage
5. Integration with LLM adapter is seamless
6. Dashboard can display real-time usage
