/**
 * OPR Runtime
 *
 * The main execution engine for OPR kernel operations.
 * Handles validation, retry with counterexample feedback, and receipt generation.
 */

import type {
  OprStepResult,
  OprStepResultOk,
  OprStepResultBudgetExhausted,
  KernelOutput,
  KernelState,
  OprBudgetConfig,
  ProgressInvariants,
  ValidationViolation,
  Hash,
  OprReceipt,
} from './types';
import { OprBudgetExhaustedError } from './types';
import type { OprLLMAdapter, OprLLMRequest } from './adapters/types';
import type { ReceiptStore } from './receipts';
import { ReceiptBuilder } from './receipts';
import { validateKernelOutput, checkProgressInvariants } from './validate';
import { buildRepairPrompt } from './retry';
import { sha256Of } from './hash';
import type { PromptDoc } from '../../frameir/prompt';

/**
 * Kernel prompt configuration
 */
export interface KernelPromptConfig {
  /** Unique kernel identifier */
  id: string;

  /** Compiled prompt document */
  prompt: PromptDoc;

  /** Operation name (default: 'step') */
  op?: string;
}

/**
 * Session budget interface (optional)
 */
export interface SessionBudget {
  hasRemaining(type: 'tokens' | 'cost'): boolean;
  consumeTokens(tokens: number): void;
  consumeCost(cost: number): void;
}

/**
 * OPR Runtime configuration
 */
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

  /** Session budget (optional) */
  sessionBudget?: SessionBudget;
}

/**
 * Parameters for executing a step
 */
export interface OprExecuteParams {
  /** Program/data to pass to kernel */
  program: unknown;

  /** Current state (null for first step, memento from previous step) */
  state: unknown | null;
}

/**
 * Internal request type
 */
export interface OprRequest {
  kernelId: string;
  prompt: PromptDoc;
  program: unknown;
  state: unknown | null;
  repairContext: string | null;
}

/**
 * Run result types
 */
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

interface ExtractOk {
  ok: true;
  json: unknown;
}

interface ExtractError {
  ok: false;
  error: string;
}

type ExtractResult = ExtractOk | ExtractError;

/**
 * Extract JSON object from LLM response
 * Handles markdown code blocks and extra text
 */
function extractJsonObject(response: string): ExtractResult {
  // Try to find JSON in code blocks first
  const codeBlockMatch = response.match(/```(?:json)?\s*([\s\S]*?)```/);
  const content = codeBlockMatch ? codeBlockMatch[1].trim() : response.trim();

  // Find the first { and matching }
  const start = content.indexOf('{');
  if (start === -1) {
    return { ok: false, error: 'No JSON object found in response' } as ExtractError;
  }

  let depth = 0;
  let end = -1;
  for (let i = start; i < content.length; i++) {
    if (content[i] === '{') depth++;
    else if (content[i] === '}') {
      depth--;
      if (depth === 0) {
        end = i;
        break;
      }
    }
  }

  if (end === -1) {
    return { ok: false, error: 'Unbalanced braces in JSON' } as ExtractError;
  }

  const jsonStr = content.slice(start, end + 1);
  try {
    const json = JSON.parse(jsonStr);
    return { ok: true, json } as ExtractOk;
  } catch (e) {
    return { ok: false, error: `Invalid JSON: ${(e as Error).message}` } as ExtractError;
  }
}

/**
 * OPR Runtime - the main execution engine
 */
export class OprRuntime {
  private config: OprRuntimeConfig;
  private receiptBuilder: ReceiptBuilder;

  constructor(config: OprRuntimeConfig) {
    this.config = config;
    this.receiptBuilder = new ReceiptBuilder(
      config.receipts,
      config.kernel.id,
      config.kernel.op ?? 'step'
    );
  }

  /**
   * Execute a single kernel step with validation and retry
   */
  async step(params: OprExecuteParams): Promise<OprStepResult> {
    const { kernel, adapter, budget, invariants, sessionBudget } = this.config;
    const { maxAttempts } = budget;

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
      const userContent = this.formatUserContent(params, repairContext);
      const request: OprLLMRequest = {
        kernelId: kernel.id,
        prompt: kernel.prompt,
        userContent,
        repairContext: repairContext ?? undefined,
      };
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
      if (extracted.ok === false) {
        const errorMsg = (extracted as ExtractError).error;
        lastViolations = [
          {
            path: '$',
            code: 'NOT_JSON',
            message: errorMsg,
          },
        ];
        this.receiptBuilder.error(attempt, requestHash, responseHash, [errorMsg]);
        repairContext = buildRepairPrompt(lastViolations);
        continue;
      }

      // Validate response
      const validation = validateKernelOutput(JSON.stringify(extracted.json), {
        kernelId: kernel.id,
        op: kernel.op ?? 'step',
      });

      if (!validation.ok) {
        lastViolations = validation.violations;
        this.receiptBuilder.error(
          attempt,
          requestHash,
          responseHash,
          validation.violations.map((v) => v.message)
        );
        repairContext = buildRepairPrompt(validation.violations);
        continue;
      }

      const output = validation.parsed as KernelOutput;

      // Check progress invariants (if configured and we have previous state)
      if (invariants && params.state !== null && output.next_state !== null) {
        const invariantViolations = checkProgressInvariants(
          params.state as KernelState,
          output.next_state,
          invariants
        );
        if (invariantViolations.length > 0) {
          lastViolations = invariantViolations;
          this.receiptBuilder.error(
            attempt,
            requestHash,
            responseHash,
            invariantViolations.map((v) => v.message)
          );
          repairContext = buildRepairPrompt(invariantViolations);
          continue;
        }
      }

      // Success!
      this.receiptBuilder.success(attempt, requestHash, responseHash, output.diagnostics);

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

  /**
   * Format user content for the LLM request
   */
  private formatUserContent(params: OprExecuteParams, repairContext: string | null): string {
    const parts: string[] = [];

    if (repairContext) {
      parts.push(repairContext);
      parts.push('\n---\n');
    }

    parts.push('PROGRAM:');
    parts.push(JSON.stringify(params.program, null, 2));

    if (params.state !== null) {
      parts.push('\nCURRENT STATE:');
      parts.push(JSON.stringify(params.state, null, 2));
    }

    return parts.join('\n');
  }

  private makeBudgetExhaustedResult(
    budgetType: 'attempts' | 'session-tokens' | 'session-cost',
    attempts: number,
    lastViolations: ValidationViolation[]
  ): OprStepResultBudgetExhausted {
    return {
      tag: 'budget-exhausted',
      ok: false,
      error: new OprBudgetExhaustedError(`Budget exhausted: ${budgetType}`, budgetType),
      attempts,
      receipts: this.config.receipts.getAll(),
    };
  }
}
