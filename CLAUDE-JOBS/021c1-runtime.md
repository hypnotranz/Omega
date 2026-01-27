# 021c1-runtime: OPR Runtime

> **Output**: `src/core/opr/runtime.ts`

> **Scope**: Implement OprRuntime class with execute loop, validation, retry, receipts
> **Architecture Reference**: [021-OPR-RUNTIME.md](021-OPR-RUNTIME.md#budget-integration)
> **Depends on**: job-021a1-validate, job-021a2-receipts, job-021a3-retry, job-021a4-hash, job-021a5-json-extract (Layer 5)

## Overview

The OprRuntime class is the main execution engine. It:
1. Builds prompts from kernel config
2. Calls the LLM adapter
3. Validates responses with structured violations
4. Retries with counterexample feedback on failure
5. Records receipts for all attempts
6. Respects budget constraints (local + session)

## File to Create

`src/core/opr/runtime.ts`

## Imports Contract

What this task needs from its dependencies:

### From 021-types (./types):
- OprStepResult
- OprStepResultOk
- KernelOutput
- OprBudgetConfig
- ProgressInvariants
- OprCapabilities
- ValidationViolation
- Hash

### From 021-types (./adapters/types):
- OprLLMAdapter

### From 021-receipts (./receipts):
- ReceiptStore

### From 021-prompts (./prompts):
- KernelPromptConfig

### From 021-validate (./validate):
- validateKernelOutput
- checkProgressInvariants

### From 021-retry (./retry):
- buildRepairPrompt

### From 021-jsonExtract (./jsonExtract):
- extractJsonObject

### From 021-hash (./hash):
- sha256Of

## Implementation

### Configuration Interface

```typescript
import type {
  OprStepResult, OprStepResultOk, KernelOutput,
  OprBudgetConfig, ProgressInvariants, OprCapabilities,
  ValidationViolation, Hash
} from './types';
import type { OprLLMAdapter } from './adapters/types';
import type { ReceiptStore } from './receipts';
import type { KernelPromptConfig } from './prompts';

export interface OprRuntimeConfig {
  /** Kernel configuration with prompt and capabilities */
  kernel: KernelPromptConfig;

  /** LLM adapter for making calls */
  adapter: OprLLMAdapter;

  /** Receipt storage */
  receipts: ReceiptStore;

  /** Budget configuration */
  budget: OprBudgetConfig;

  /** Progress invariants to enforce (optional) */
  invariants?: ProgressInvariants;
}

export interface OprExecuteParams {
  /** Program/data to pass to kernel */
  program: unknown;

  /** Current state (null for first step, memento from previous step) */
  state: unknown | null;
}
```

### OprRuntime Class

```typescript
import { validateKernelOutput, checkProgressInvariants } from './validate';
import { ReceiptBuilder } from './receipts';
import { buildRepairPrompt } from './retry';
import { extractJsonObject } from './jsonExtract';
import { sha256Of } from './hash';

export class OprRuntime {
  private config: OprRuntimeConfig;
  private receiptBuilder: ReceiptBuilder;

  constructor(config: OprRuntimeConfig) {
    this.config = config;
    this.receiptBuilder = new ReceiptBuilder(
      config.receipts,
      config.kernel.id,
      'step' // default op
    );
  }

  /**
   * Execute a single kernel step with validation and retry
   */
  async step(params: OprExecuteParams): Promise<OprStepResult> {
    const { kernel, adapter, budget, invariants } = this.config;
    const { maxAttempts, sessionBudget } = budget;

    let lastViolations: ValidationViolation[] = [];
    let repairContext: string | null = null;

    for (let attempt = 1; attempt <= maxAttempts; attempt++) {
      // Check session budget (if provided)
      if (sessionBudget) {
        if (!sessionBudget.hasRemaining('tokens')) {
          return this.makeBudgetExhaustedResult('session-tokens', attempt, lastViolations);
        }
        if (!sessionBudget.hasRemaining('cost')) {
          return this.makeBudgetExhaustedResult('session-cost', attempt, lastViolations);
        }
      }

      // Build request
      const request = this.buildRequest(params, repairContext);
      const requestHash = sha256Of(request);

      // Call LLM
      let response: string;
      try {
        response = await adapter.complete(request);
      } catch (e) {
        // Record timeout/error receipt
        this.receiptBuilder.timeout(attempt, requestHash);
        continue;
      }

      const responseHash = sha256Of(response);

      // Consume session budget (if provided)
      if (sessionBudget && adapter.getLastUsage) {
        const usage = adapter.getLastUsage();
        sessionBudget.consumeTokens(usage.totalTokens);
        sessionBudget.consumeCost(usage.estimatedCost);
      }

      // Extract JSON from response (handles markdown code blocks, etc.)
      const extracted = extractJsonObject(response);
      if (!extracted.ok) {
        lastViolations = [{
          path: '$',
          code: 'NOT_JSON',
          message: extracted.error,
        }];
        this.receiptBuilder.error(attempt, requestHash, responseHash, [extracted.error]);
        repairContext = buildRepairPrompt(lastViolations);
        continue;
      }

      // Validate response
      const validation = validateKernelOutput(
        JSON.stringify(extracted.json),
        { kernelId: kernel.id, op: 'step' }
      );

      if (!validation.ok) {
        lastViolations = validation.violations;
        this.receiptBuilder.error(
          attempt,
          requestHash,
          responseHash,
          validation.violations.map(v => v.message)
        );
        repairContext = buildRepairPrompt(validation.violations);
        continue;
      }

      const output = validation.parsed as KernelOutput;

      // Check progress invariants (if configured and we have previous state)
      if (invariants && params.state !== null && output.next_state !== null) {
        const invariantViolations = checkProgressInvariants(
          params.state as any,
          output.next_state,
          invariants
        );
        if (invariantViolations.length > 0) {
          lastViolations = invariantViolations;
          this.receiptBuilder.error(
            attempt,
            requestHash,
            responseHash,
            invariantViolations.map(v => v.message)
          );
          repairContext = buildRepairPrompt(invariantViolations);
          continue;
        }
      }

      // Success!
      this.receiptBuilder.success(
        attempt,
        requestHash,
        responseHash,
        output.diagnostics
      );

      return {
        tag: 'ok',
        ok: true,
        output,
        attempts: attempt,
        receipts: this.config.receipts.getAll(),
      };
    }

    // Exhausted local budget
    return this.makeBudgetExhaustedResult('attempts', maxAttempts, lastViolations);
  }

  /**
   * Run kernel to fixpoint (until next_state.done = true or null)
   */
  async runToFixpoint(params: OprExecuteParams): Promise<OprRunResult> {
    const results: OprStepResultOk[] = [];
    let currentState = params.state;
    let iterations = 0;
    const maxIterations = 100; // Safety limit

    while (iterations < maxIterations) {
      const result = await this.step({ program: params.program, state: currentState });

      if (result.tag !== 'ok') {
        return {
          tag: 'error',
          error: result,
          iterations,
          receipts: this.config.receipts.getAll(),
        };
      }

      results.push(result);
      iterations++;

      // Check termination
      const nextState = result.output.next_state;
      if (nextState === null || nextState.done === true) {
        return {
          tag: 'ok',
          results,
          finalState: nextState,
          iterations,
          receipts: this.config.receipts.getAll(),
        };
      }

      currentState = nextState;
    }

    // Max iterations reached
    return {
      tag: 'max-iterations',
      results,
      iterations,
      receipts: this.config.receipts.getAll(),
    };
  }

  private buildRequest(params: OprExecuteParams, repairContext: string | null): OprRequest {
    const { kernel } = this.config;

    return {
      kernelId: kernel.id,
      prompt: kernel.prompt,
      program: params.program,
      state: params.state,
      repairContext,
    };
  }

  private makeBudgetExhaustedResult(
    budgetType: 'attempts' | 'session-tokens' | 'session-cost',
    attempts: number,
    lastViolations: ValidationViolation[]
  ): OprStepResultBudgetExhausted {
    return {
      tag: 'budget-exhausted',
      ok: false,
      error: new OprBudgetExhaustedError(
        `Budget exhausted: ${budgetType}`,
        budgetType
      ),
      attempts,
      receipts: this.config.receipts.getAll(),
    };
  }
}
```

### Run Result Types

```typescript
export interface OprRunResultOk {
  tag: 'ok';
  results: OprStepResultOk[];
  finalState: KernelState | null;
  iterations: number;
  receipts: OprReceipt[];
}

export interface OprRunResultError {
  tag: 'error';
  error: OprStepResult;
  iterations: number;
  receipts: OprReceipt[];
}

export interface OprRunResultMaxIterations {
  tag: 'max-iterations';
  results: OprStepResultOk[];
  iterations: number;
  receipts: OprReceipt[];
}

export type OprRunResult = OprRunResultOk | OprRunResultError | OprRunResultMaxIterations;
```

### Request Type

```typescript
export interface OprRequest {
  kernelId: string;
  prompt: PromptDoc;
  program: unknown;
  state: unknown | null;
  repairContext: string | null;
}
```

## Exports Contract
```typescript
export {
  OprRuntime,
  type OprRuntimeConfig,
  type OprExecuteParams,
  type OprRequest,
  type OprRunResult,
  type OprRunResultOk,
  type OprRunResultError,
  type OprRunResultMaxIterations,
};
```

## Acceptance Criteria

1. [ ] OprRuntime constructor accepts config with kernel, adapter, receipts, budget
2. [ ] `step()` validates response and retries on failure with repair prompt
3. [ ] `step()` respects maxAttempts budget
4. [ ] `step()` respects session budget (tokens/cost)
5. [ ] `step()` creates receipts for all attempts
6. [ ] `step()` enforces progress invariants when configured
7. [ ] `runToFixpoint()` loops until done=true or null
8. [ ] `runToFixpoint()` has max iterations safety limit
9. [ ] All integration tests pass

## Test Cases (implement in 021g2-integration-tests)

- RT1: successful step on first attempt
- RT2: retry with repair prompt on validation failure
- RT3: budget exhausted after maxAttempts
- RT4: session budget enforcement
- RT5: progress invariant enforcement
- RT6: runToFixpoint terminates on done=true
- RT7: runToFixpoint respects max iterations
- RT8: receipt chain generated for multi-attempt step

## Verification

```bash
npx tsc --noEmit src/core/opr/runtime.ts
```
