/**
 * OPR Effect Handler
 *
 * Handles OPR effects from the CEKS machine, bridging between
 * Lisp's effect system and OPR's kernel execution.
 *
 * THE KEYSTONE: This handler now wires callbacks so that OPR kernels
 * can call back into the Lisp runtime, creating co-recursive
 * symbolic/neural computation.
 *
 * Effect names:
 * - "opr.step.<kernelId>" -> single kernel step
 * - "opr.fixpoint.<kernelId>" -> run kernel to completion WITH CALLBACKS
 * - "opr.list" -> list available kernels
 */

import type { OpCall } from '../effects/opcall';
import type { Val } from '../eval/values';
import { valToJson, jsonToVal } from './bridge';
import { OprRuntime, type OprRuntimeConfig, type OprRunResult } from './runtime';
import type { OprStepResult } from './types';
import { InMemoryReceiptStore } from './receipts';
import { getKernel, listKernels } from './kernels';
import type { OprLLMAdapter } from './adapters/types';
import type { CallbackContext } from './callbacks';

/**
 * Factory function to create a CallbackContext from current runtime state.
 * This is THE KEYSTONE connection between OPR and the Lisp runtime.
 */
export type CallbackContextFactory = () => CallbackContext;

/**
 * Configuration for the OPR effect handler
 */
export interface OprEffectHandlerConfig {
  /** LLM adapter for making calls */
  adapter: OprLLMAdapter;

  /** Default budget (max attempts per step) */
  defaultMaxAttempts?: number;

  /**
   * THE KEYSTONE: Factory to create callback context for kernel effects.
   *
   * When a kernel emits callback.eval_lisp, this factory creates the
   * context that can evaluate Lisp code and return results.
   *
   * Without this, OPR is "typed prompt RPC".
   * With this, OPR becomes a co-recursive symbolic/neural tower.
   */
  callbackFactory?: CallbackContextFactory;
}

/**
 * Check if an effect operation is an OPR effect
 */
export function isOprEffect(op: string): boolean {
  return (
    op.startsWith('opr.step.') ||
    op.startsWith('opr.fixpoint.') ||
    op === 'opr.list' ||
    op === 'opr.kernels'
  );
}

/**
 * Handle an OPR effect from the CEKS machine
 *
 * @param opcall - The effect operation from the machine
 * @param config - Handler configuration
 * @returns Val to resume the continuation with
 */
export async function handleOprEffect(
  opcall: OpCall,
  config: OprEffectHandlerConfig
): Promise<Val> {
  const { op, args } = opcall;

  // Handle "opr.list" / "opr.kernels" - return list of kernel IDs
  if (op === 'opr.list' || op === 'opr.kernels') {
    const kernelIds = listKernels();
    return jsonToVal(kernelIds);
  }

  // Parse effect name: "opr.step.opr.classify.v1" or "opr.fixpoint.opr.plan.v1"
  const match = op.match(/^opr\.(step|fixpoint)\.(.+)$/);
  if (!match) {
    return jsonToVal({
      ok: false,
      error: { code: 'INVALID_OPR_EFFECT', message: `Invalid OPR effect: ${op}` },
    });
  }

  const mode = match[1] as 'step' | 'fixpoint';
  const kernelId = match[2];

  // Find kernel
  const kernel = getKernel(kernelId);
  if (!kernel) {
    return jsonToVal({
      ok: false,
      error: { code: 'UNKNOWN_KERNEL', message: `Unknown kernel: ${kernelId}`, availableKernels: listKernels() },
    });
  }

  // Extract program and state from args
  // Args: [program, state?] (both as Lisp values)
  let program: unknown = {};
  let state: unknown = null;

  if (args.length > 0) {
    program = valToJson(args[0]);
  }
  if (args.length > 1) {
    state = valToJson(args[1]);
  }

  // Create receipt store for this invocation
  const receipts = new InMemoryReceiptStore();

  // THE KEYSTONE: Get callback context if factory is provided
  const callbacks = config.callbackFactory?.();

  // Create runtime configuration
  const runtimeConfig: OprRuntimeConfig = {
    kernel,
    adapter: config.adapter,
    receipts,
    budget: {
      maxAttempts: config.defaultMaxAttempts ?? 3,
    },
    callbacks, // THE KEYSTONE: Wire in callbacks
  };

  // Create runtime
  const runtime = new OprRuntime(runtimeConfig);

  try {
    if (mode === 'step') {
      // Single step execution
      const result = await runtime.step({ program, state });
      return stepResultToVal(result);
    } else {
      // Run to fixpoint WITH CALLBACKS (THE KEYSTONE)
      // If callbacks are provided, kernels can now call back into Lisp!
      const result = await runtime.runToFixpoint({ program, state });
      return runResultToVal(result);
    }
  } catch (e) {
    // Unexpected error
    return jsonToVal({
      ok: false,
      error: {
        code: 'OPR_EXECUTION_ERROR',
        message: e instanceof Error ? e.message : String(e),
      },
      receipts: receipts.getAll(),
    });
  }
}

/**
 * Convert OprStepResult to Val
 */
function stepResultToVal(result: OprStepResult): Val {
  if (result.tag === 'ok') {
    return jsonToVal({
      ok: true,
      tag: 'ok',
      output: result.output,
      attempts: result.attempts,
      receipts: result.receipts,
    });
  } else if (result.tag === 'budget-exhausted') {
    return jsonToVal({
      ok: false,
      tag: 'budget-exhausted',
      error: {
        code: result.error.code,
        message: result.error.message,
        budgetType: result.error.budgetType,
      },
      attempts: result.attempts,
      receipts: result.receipts,
    });
  } else {
    // Shouldn't happen, but handle gracefully
    return jsonToVal({
      ok: false,
      tag: 'unknown',
      error: { code: 'UNKNOWN_RESULT', message: 'Unknown step result type' },
    });
  }
}

/**
 * Convert OprRunResult to Val
 */
function runResultToVal(result: OprRunResult): Val {
  switch (result.tag) {
    case 'ok':
      return jsonToVal({
        ok: true,
        tag: 'ok',
        results: result.results.map(r => r.output),
        finalState: result.finalState,
        iterations: result.iterations,
        receipts: result.receipts,
      });

    case 'error':
      return jsonToVal({
        ok: false,
        tag: 'error',
        error: result.error,
        iterations: result.iterations,
        receipts: result.receipts,
      });

    case 'max-iterations':
      return jsonToVal({
        ok: false,
        tag: 'max-iterations',
        results: result.results.map(r => r.output),
        iterations: result.iterations,
        receipts: result.receipts,
      });
  }
}

/**
 * Create OPR effect handler with given configuration.
 *
 * This returns an object that can be used by the main run loop
 * to dispatch OPR effects.
 */
export function createOprEffectHandler(config: OprEffectHandlerConfig) {
  return {
    isOprEffect,
    handleOprEffect: (opcall: OpCall) => handleOprEffect(opcall, config),
  };
}

/**
 * Convenience: Extract kernel result data from OPR response
 *
 * Usage in Lisp-land after calling OPR:
 * ```lisp
 * (define result (effect "opr.step.opr.classify.v1" program))
 * (if (opr-ok? result)
 *     (opr-output result)
 *     (opr-error result))
 * ```
 */
export const OPR_PRIMITIVES = {
  'opr-ok?': (args: Val[]): Val => {
    if (args.length < 1) return { tag: 'Bool', b: false };
    const result = valToJson(args[0]) as any;
    return { tag: 'Bool', b: result?.ok === true };
  },

  'opr-output': (args: Val[]): Val => {
    if (args.length < 1) return { tag: 'Unit' };
    const result = valToJson(args[0]) as any;
    return jsonToVal(result?.output ?? result?.results ?? null);
  },

  'opr-error': (args: Val[]): Val => {
    if (args.length < 1) return { tag: 'Unit' };
    const result = valToJson(args[0]) as any;
    return jsonToVal(result?.error ?? null);
  },

  'opr-receipts': (args: Val[]): Val => {
    if (args.length < 1) return { tag: 'Unit' };
    const result = valToJson(args[0]) as any;
    return jsonToVal(result?.receipts ?? []);
  },

  'opr-attempts': (args: Val[]): Val => {
    if (args.length < 1) return { tag: 'Num', n: 0 };
    const result = valToJson(args[0]) as any;
    return { tag: 'Num', n: result?.attempts ?? result?.iterations ?? 0 };
  },
};
