# 25: Budget (Resource Tracking & Enforcement)

## The Problem

AI agents can consume unbounded resources:
- LLM tokens (expensive, ~$0.01-0.10/1K)
- API calls (rate limits)
- Time (user patience)
- Iterations (infinite loops)

**Budget enforces limits** to prevent runaway costs and ensure termination.

---

## Four Budget Dimensions

```typescript
enum BudgetType {
  TOKENS = 'tokens',       // LLM API tokens
  COST = 'cost',           // USD spent
  TIME = 'time',           // Wall-clock seconds
  ITERATIONS = 'iterations' // Fixpoint/loop iterations
}

interface BudgetLimits {
  tokens?: number;      // Max tokens (null = unlimited)
  cost?: number;        // Max USD (null = unlimited)
  time?: number;        // Max seconds (null = unlimited)
  iterations?: number;  // Max iterations (null = unlimited)
}
```

---

## Budget Class

```typescript
class Budget {
  private limits: BudgetLimits;
  private consumed: Record<BudgetType, number> = {
    tokens: 0,
    cost: 0,
    time: 0,
    iterations: 0,
  };
  private startTime: number;

  constructor(limits: BudgetLimits = {}) {
    this.limits = limits;
    this.startTime = Date.now();
  }

  // === Consumption ===

  consumeTokens(amount: number): void {
    if (amount < 0) throw new Error('Cannot consume negative tokens');
    this.consumed.tokens += amount;
    this.check('tokens');
  }

  consumeCost(amount: number): void {
    if (amount < 0) throw new Error('Cannot consume negative cost');
    this.consumed.cost += amount;
    this.check('cost');
  }

  consumeIteration(): void {
    this.consumed.iterations += 1;
    this.check('iterations');
  }

  // Time is computed, not consumed
  private get elapsedTime(): number {
    return (Date.now() - this.startTime) / 1000;
  }

  // === Queries ===

  hasRemaining(type: BudgetType): boolean {
    const limit = this.limits[type];
    if (limit === undefined || limit === null) return true;  // Unlimited

    if (type === 'time') {
      return this.elapsedTime < limit;
    }
    return this.consumed[type] < limit;
  }

  remaining(type: BudgetType): number | null {
    const limit = this.limits[type];
    if (limit === undefined || limit === null) return null;  // Unlimited

    if (type === 'time') {
      return Math.max(0, limit - this.elapsedTime);
    }
    return Math.max(0, limit - this.consumed[type]);
  }

  remainingAll(): Record<BudgetType, number | null> {
    return {
      tokens: this.remaining('tokens'),
      cost: this.remaining('cost'),
      time: this.remaining('time'),
      iterations: this.remaining('iterations'),
    };
  }

  // === Enforcement ===

  check(type: BudgetType): void {
    if (!this.hasRemaining(type)) {
      throw new BudgetExceededError(type, this.limits[type]!, this.consumed[type]);
    }
  }

  checkAll(): void {
    for (const type of Object.values(BudgetType)) {
      this.check(type);
    }
  }

  // === Lifecycle ===

  clone(): Budget {
    const clone = new Budget({ ...this.limits });
    clone.consumed = { ...this.consumed };
    clone.startTime = this.startTime;
    return clone;
  }

  reset(): void {
    this.consumed = { tokens: 0, cost: 0, time: 0, iterations: 0 };
    this.startTime = Date.now();
  }

  // === Reporting ===

  report(): BudgetReport {
    return {
      limits: { ...this.limits },
      consumed: { ...this.consumed, time: this.elapsedTime },
      remaining: this.remainingAll(),
    };
  }
}

class BudgetExceededError extends Error {
  constructor(
    public type: BudgetType,
    public limit: number,
    public consumed: number
  ) {
    super(`Budget exceeded: ${type} (${consumed}/${limit})`);
    this.name = 'BudgetExceededError';
  }
}
```

---

## Integration Points

### LLM Adapter Integration

```typescript
class OpenAIAdapter implements LLMAdapter {
  constructor(private budget: Budget) {}

  async complete(prompt: string, options?: LLMOptions): Promise<string> {
    // Check budget before call
    this.budget.check('tokens');
    this.budget.check('cost');
    this.budget.check('time');

    const response = await this.client.chat.completions.create({
      messages: [{ role: 'user', content: prompt }],
      ...options,
    });

    // Record consumption
    const usage = response.usage!;
    this.budget.consumeTokens(usage.total_tokens);

    // Estimate cost (model-dependent)
    const cost = this.estimateCost(usage, options?.model);
    this.budget.consumeCost(cost);

    return response.choices[0].message.content!;
  }

  private estimateCost(usage: Usage, model?: string): number {
    const rates = MODEL_RATES[model ?? 'gpt-4'];
    return (
      (usage.prompt_tokens / 1000) * rates.input +
      (usage.completion_tokens / 1000) * rates.output
    );
  }
}

const MODEL_RATES: Record<string, { input: number; output: number }> = {
  'gpt-4': { input: 0.03, output: 0.06 },
  'gpt-4-turbo': { input: 0.01, output: 0.03 },
  'gpt-3.5-turbo': { input: 0.0005, output: 0.0015 },
  'claude-3-opus': { input: 0.015, output: 0.075 },
  'claude-3-sonnet': { input: 0.003, output: 0.015 },
};
```

### Fixpoint Integration

```typescript
function evalFixpoint(body, env, options, budget): Value {
  for (let i = 0; i < options.maxIterations; i++) {
    // Check iteration budget
    budget.consumeIteration();  // Throws if exceeded

    // Evaluate (may consume tokens, time, cost)
    const value = evalExpr(body, env, ...);

    // Check time budget after each iteration
    budget.check('time');

    // ... convergence detection
  }
}
```

### Session Integration

```typescript
class Session {
  private budget: Budget;
  private turnCount: number = 0;

  constructor(config: SessionConfig) {
    this.budget = new Budget({
      tokens: config.maxTokens,
      cost: config.maxCost,
      time: config.maxTime,
      iterations: config.maxTurns,
    });
  }

  async turn(action: () => Promise<Value>): Promise<TurnResult> {
    // Check turn budget
    this.budget.consumeIteration();

    // Execute turn
    const result = await action();

    this.turnCount++;

    return {
      value: result,
      turnNumber: this.turnCount,
      budgetRemaining: this.budget.remainingAll(),
    };
  }

  canContinue(): boolean {
    return (
      this.budget.hasRemaining('tokens') &&
      this.budget.hasRemaining('cost') &&
      this.budget.hasRemaining('time') &&
      this.budget.hasRemaining('iterations')
    );
  }
}
```

---

## Lisp API

```lisp
;; Query remaining budget
(budget/remaining 'tokens)     ;; => 5000 or nil (unlimited)
(budget/remaining 'cost)       ;; => 0.50 or nil
(budget/remaining 'time)       ;; => 120.5 or nil
(budget/remaining 'iterations) ;; => 8 or nil

;; Check if budget allows operation
(budget/has-remaining? 'tokens)  ;; => #t or #f

;; Get full budget report
(budget/report)
;; => {:limits {:tokens 10000 :cost 1.0}
;;     :consumed {:tokens 5000 :cost 0.50 :time 30.2}
;;     :remaining {:tokens 5000 :cost 0.50 :time nil}}

;; Guard operations with budget check
(when (budget/has-remaining? 'tokens)
  (llm.complete prompt))

;; Explicit budget scoping
(with-budget {:tokens 1000 :cost 0.10}
  (expensive-operation))  ;; Throws if exceeds scoped budget
```

---

## Metrics Integration

Budget works with the metrics system:

```typescript
interface Metrics {
  // LLM metrics
  llm_complete_calls: number;
  llm_compile_intent_calls: number;
  llm_total_tokens: number;
  llm_total_cost: number;

  // World metrics
  world_read_calls: number;
  world_write_calls: number;
  world_fingerprint_calls: number;

  // Evaluation metrics
  fixpoint_iterations: number;
  subeval_calls: number;

  // Cache metrics
  memo_hits: number;
  memo_misses: number;

  // Fact metrics
  facts_added: number;
}

class MetricsCollector {
  private metrics: Metrics = {
    llm_complete_calls: 0,
    llm_compile_intent_calls: 0,
    llm_total_tokens: 0,
    llm_total_cost: 0,
    world_read_calls: 0,
    world_write_calls: 0,
    world_fingerprint_calls: 0,
    fixpoint_iterations: 0,
    subeval_calls: 0,
    memo_hits: 0,
    memo_misses: 0,
    facts_added: 0,
  };

  increment(key: keyof Metrics, amount: number = 1): void {
    this.metrics[key] += amount;
  }

  get(key: keyof Metrics): number {
    return this.metrics[key];
  }

  report(): Metrics {
    return { ...this.metrics };
  }
}
```

---

## Budget Strategies

### Strategy 1: Fixed Budget

```lisp
;; Simple fixed limits
(with-budget {:tokens 10000 :cost 1.0 :time 300}
  (solve-problem))
```

### Strategy 2: Per-Phase Budget

```lisp
;; Different budgets for different phases
(begin
  (with-budget {:tokens 2000}
    (phase-1-analysis))

  (with-budget {:tokens 5000}
    (phase-2-planning))

  (with-budget {:tokens 3000}
    (phase-3-execution)))
```

### Strategy 3: Adaptive Budget

```lisp
;; Check remaining and adjust strategy
(let ((remaining-tokens (budget/remaining 'tokens)))
  (cond
    ((> remaining-tokens 5000)
     (comprehensive-analysis))
    ((> remaining-tokens 2000)
     (quick-analysis))
    (else
     (minimal-analysis))))
```

### Strategy 4: Budget Reservation

```lisp
;; Reserve budget for critical operations
(let ((total-budget 10000)
      (reserve-for-fix 3000))
  (with-budget {:tokens (- total-budget reserve-for-fix)}
    (analysis-phase))

  ;; Reserved budget available for fixes
  (with-budget {:tokens reserve-for-fix}
    (apply-fixes)))
```

---

## Error Handling

```typescript
// Budget exceeded is a condition, not an exception
function handleBudgetExceeded(
  error: BudgetExceededError,
  cont: Continuation
): Value {
  // Signal condition with restarts
  return signalCondition({
    type: Symbol.for('budget-exceeded'),
    message: error.message,
    data: {
      budgetType: error.type,
      limit: error.limit,
      consumed: error.consumed,
    },
    restarts: [
      {
        name: Symbol.for('increase-budget'),
        description: 'Increase budget limit',
        handler: (newLimit: number) => {
          // Increase limit and retry
          budget.limits[error.type] = newLimit;
          return continueExecution(cont);
        },
      },
      {
        name: Symbol.for('use-cached'),
        description: 'Use cached result if available',
        handler: () => {
          return getCachedResult() ?? signalError('No cache available');
        },
      },
      {
        name: Symbol.for('abort'),
        description: 'Abort operation',
        handler: () => {
          return makeOutcome('nonconverged', null, { reason: 'budget' });
        },
      },
    ],
  }, cont);
}
```

---

## Summary

Budget provides:

1. **Four dimensions** - Tokens, cost, time, iterations
2. **Enforcement** - Automatic checking, exception on exceed
3. **Queries** - Check remaining before expensive operations
4. **Integration** - LLM adapters, fixpoint, sessions all respect budget
5. **Metrics** - Track actual consumption for analysis
6. **Strategies** - Fixed, per-phase, adaptive, reservation patterns

Budget is the cost control mechanism that makes AI agents production-safe.
