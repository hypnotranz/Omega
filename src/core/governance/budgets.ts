// src/core/governance/budgets.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Patch Set D1: Budget enforcement
// Patch Set D2: Ledger counters for effect emissions (Prompt 8)

export type BudgetLimits = {
  maxOracleTurns: number;
  maxEvalSteps: number;
  maxToolCalls: number;
  maxNestedDepth?: number;
};

/**
 * LedgerEntry: Record of an effect emission
 */
export type LedgerEntry = {
  timestamp: number;
  op: string;
  ctxDigest: string;
  stepCount: number;
  args?: unknown[];
};

/**
 * EffectCounter: Tracks counts per effect operation
 */
export type EffectCounters = Map<string, number>;

export type Budget = {
  limits: BudgetLimits;
  consumed: {
    oracleTurns: number;
    evalSteps: number;
    toolCalls: number;
  };
  /** Ledger: ordered list of effect emissions */
  ledger?: LedgerEntry[];
  /** Effect counters: count per operation */
  effectCounters?: EffectCounters;
};

export function budgetDefault(limits: Partial<BudgetLimits> = {}): Budget {
  return {
    limits: {
      maxOracleTurns: limits.maxOracleTurns ?? 10_000,
      maxEvalSteps: limits.maxEvalSteps ?? 500_000,
      maxToolCalls: limits.maxToolCalls ?? 9999,
      maxNestedDepth: limits.maxNestedDepth ?? 8,
    },
    consumed: {
      oracleTurns: 0,
      evalSteps: 0,
      toolCalls: 0,
    },
  };
}

export function budgetConsumeOracleTurn(b: Budget): Budget {
  const newConsumed = {
    ...b.consumed,
    oracleTurns: b.consumed.oracleTurns + 1,
  };
  if (newConsumed.oracleTurns > b.limits.maxOracleTurns) {
    throw new Error(`budget exhausted: oracleTurns (${newConsumed.oracleTurns} > ${b.limits.maxOracleTurns})`);
  }
  return { ...b, consumed: newConsumed };
}

export function budgetConsumeEvalStep(b: Budget, steps: number = 1): Budget {
  const newConsumed = {
    ...b.consumed,
    evalSteps: b.consumed.evalSteps + steps,
  };
  if (newConsumed.evalSteps > b.limits.maxEvalSteps) {
    throw new Error(`budget exhausted: evalSteps (${newConsumed.evalSteps} > ${b.limits.maxEvalSteps})`);
  }
  return { ...b, consumed: newConsumed };
}

export function budgetConsumeToolCall(b: Budget): Budget {
  const newConsumed = {
    ...b.consumed,
    toolCalls: b.consumed.toolCalls + 1,
  };
  if (newConsumed.toolCalls > b.limits.maxToolCalls) {
    throw new Error(`budget exhausted: toolCalls (${newConsumed.toolCalls} > ${b.limits.maxToolCalls})`);
  }
  return { ...b, consumed: newConsumed };
}

export function budgetRemaining(b: Budget): BudgetLimits {
  return {
    maxOracleTurns: b.limits.maxOracleTurns - b.consumed.oracleTurns,
    maxEvalSteps: b.limits.maxEvalSteps - b.consumed.evalSteps,
    maxToolCalls: b.limits.maxToolCalls - b.consumed.toolCalls,
    maxNestedDepth: b.limits.maxNestedDepth,
  };
}

/**
 * Mutable budget tracker for sharing between eval loop and runtime.
 * Wraps immutable budget functions in a stateful interface.
 * Extended with ledger counters for effect emissions (Prompt 8).
 */
export class BudgetTracker {
  private budget: Budget;
  private ledger: LedgerEntry[] = [];
  private effectCounters: Map<string, number> = new Map();
  private stepCount: number = 0;

  constructor(limits: Partial<BudgetLimits> = {}) {
    this.budget = budgetDefault(limits);
  }

  /** Consume one eval step. Throws if budget exhausted. */
  consumeEvalStep(steps: number = 1): void {
    this.budget = budgetConsumeEvalStep(this.budget, steps);
    this.stepCount += steps;
  }

  /** Consume one oracle turn. Throws if budget exhausted. */
  consumeOracleTurn(): void {
    this.budget = budgetConsumeOracleTurn(this.budget);
  }

  /** Consume one tool call. Throws if budget exhausted. */
  consumeToolCall(): void {
    this.budget = budgetConsumeToolCall(this.budget);
  }

  /** Get remaining budget limits. */
  remaining(): BudgetLimits {
    return budgetRemaining(this.budget);
  }

  /** Get current budget snapshot. */
  snapshot(): Budget {
    return {
      ...this.budget,
      consumed: { ...this.budget.consumed },
      limits: { ...this.budget.limits },
      ledger: this.ledger.slice(),
      effectCounters: new Map(this.effectCounters),
    };
  }

  /** Check if eval steps are exhausted (without throwing). */
  isEvalExhausted(): boolean {
    return this.budget.consumed.evalSteps >= this.budget.limits.maxEvalSteps;
  }

  /** Check if oracle turns are exhausted (without throwing). */
  isOracleExhausted(): boolean {
    return this.budget.consumed.oracleTurns >= this.budget.limits.maxOracleTurns;
  }

  /** Check if tool calls are exhausted (without throwing). */
  isToolExhausted(): boolean {
    return this.budget.consumed.toolCalls >= this.budget.limits.maxToolCalls;
  }

  /**
   * Consume an amb attempt/backtrack (compat shim for nondet runtime).
   * Currently just increments step counter to keep accounting monotonic.
   */
  consumeAmbAttempt(): void {
    this.consumeEvalStep(0);
  }

  // ─────────────────────────────────────────────────────────────────
  // Ledger operations (Prompt 8)
  // ─────────────────────────────────────────────────────────────────

  /**
   * Record an effect emission in the ledger.
   * Increments the counter for this operation type.
   */
  recordEffect(op: string, ctxDigest: string, args?: unknown[]): void {
    const entry: LedgerEntry = {
      timestamp: Date.now(),
      op,
      ctxDigest,
      stepCount: this.stepCount,
      args,
    };
    this.ledger.push(entry);

    // Increment effect counter
    const count = this.effectCounters.get(op) || 0;
    this.effectCounters.set(op, count + 1);
  }

  /**
   * Get the count of emissions for a specific effect operation.
   */
  getEffectCount(op: string): number {
    return this.effectCounters.get(op) || 0;
  }

  /**
   * Get all effect counts as an object.
   */
  getAllEffectCounts(): Record<string, number> {
    const result: Record<string, number> = {};
    for (const [op, count] of this.effectCounters) {
      result[op] = count;
    }
    return result;
  }

  /**
   * Get the total number of effect emissions.
   */
  getTotalEffectCount(): number {
    let total = 0;
    for (const count of this.effectCounters.values()) {
      total += count;
    }
    return total;
  }

  /**
   * Get the ledger entries.
   */
  getLedger(): LedgerEntry[] {
    return this.ledger.slice();
  }

  /**
   * Get ledger entries filtered by operation.
   */
  getLedgerByOp(op: string): LedgerEntry[] {
    return this.ledger.filter(e => e.op === op);
  }

  /**
   * Clear the ledger (for testing/reset).
   */
  clearLedger(): void {
    this.ledger = [];
    this.effectCounters.clear();
  }

  /**
   * Get current step count.
   */
  getStepCount(): number {
    return this.stepCount;
  }
}
