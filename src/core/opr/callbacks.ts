/**
 * OPR Callback Executor
 *
 * THE KEYSTONE: This module wires OPR kernel effects back into the symbolic runtime,
 * creating the co-recursive symbolic/neural computation tower.
 *
 * When a kernel emits effects like `callback.eval_lisp`, this module:
 * 1. Extracts the effect payload
 * 2. Executes it against the Lisp runtime
 * 3. Returns the result to be fed back into the next kernel step
 *
 * This transforms OPR from "typed prompt RPC" into a genuine semantic abstract machine.
 */

import type { Effect, EffectType, CallbackResult } from './types';
import type { Val } from '../eval/values';
import { jsonToVal, valToJson } from './bridge';

/**
 * Callback context provided by the host runtime
 */
export interface CallbackContext {
  /**
   * Evaluate Lisp source code and return the result
   */
  evalLisp(source: string): Promise<Val>;

  /**
   * Retrieve an artifact by key
   */
  getArtifact?(key: string): Promise<unknown>;

  /**
   * Query the fact store
   */
  queryFacts?(query: string): Promise<unknown[]>;

  /**
   * Compute a hash of a value
   */
  computeHash?(value: unknown): Promise<string>;
}

/**
 * Result of processing all effects from a kernel step
 */
export interface CallbackBatchResult {
  /** All callback results keyed by correlation_id */
  results: Map<string, CallbackResult>;

  /** Any effects that couldn't be processed */
  unhandled: Effect[];

  /** Formatted context string to feed back to kernel */
  feedbackContext: string;
}

/**
 * Process a single effect and return its result
 */
async function processEffect(
  effect: Effect,
  ctx: CallbackContext
): Promise<CallbackResult> {
  const correlationId = effect.correlation_id ?? effect.idempotency_key;

  try {
    switch (effect.type) {
      case 'callback.eval_lisp': {
        // Payload should be { source: string } or just a string
        const payload = effect.payload as { source?: string } | string;
        const source = typeof payload === 'string' ? payload : payload.source;

        if (!source || typeof source !== 'string') {
          return {
            correlation_id: correlationId,
            ok: false,
            error: {
              code: 'INVALID_PAYLOAD',
              message: 'callback.eval_lisp requires source string',
            },
          };
        }

        const result = await ctx.evalLisp(source);
        return {
          correlation_id: correlationId,
          ok: true,
          value: valToJson(result),
        };
      }

      case 'callback.artifact.get': {
        if (!ctx.getArtifact) {
          return {
            correlation_id: correlationId,
            ok: false,
            error: {
              code: 'NOT_SUPPORTED',
              message: 'Artifact retrieval not available',
            },
          };
        }

        const payload = effect.payload as { key?: string } | string;
        const key = typeof payload === 'string' ? payload : payload.key;

        if (!key) {
          return {
            correlation_id: correlationId,
            ok: false,
            error: {
              code: 'INVALID_PAYLOAD',
              message: 'callback.artifact.get requires key',
            },
          };
        }

        const artifact = await ctx.getArtifact(key);
        return {
          correlation_id: correlationId,
          ok: true,
          value: artifact,
        };
      }

      case 'callback.facts.query': {
        if (!ctx.queryFacts) {
          return {
            correlation_id: correlationId,
            ok: false,
            error: {
              code: 'NOT_SUPPORTED',
              message: 'Fact query not available',
            },
          };
        }

        const payload = effect.payload as { query?: string } | string;
        const query = typeof payload === 'string' ? payload : payload.query;

        if (!query) {
          return {
            correlation_id: correlationId,
            ok: false,
            error: {
              code: 'INVALID_PAYLOAD',
              message: 'callback.facts.query requires query string',
            },
          };
        }

        const facts = await ctx.queryFacts(query);
        return {
          correlation_id: correlationId,
          ok: true,
          value: facts,
        };
      }

      case 'callback.hash': {
        if (!ctx.computeHash) {
          // Fallback: use JSON stringify + simple hash
          const payload = effect.payload;
          const str = JSON.stringify(payload);
          const hash = simpleHash(str);
          return {
            correlation_id: correlationId,
            ok: true,
            value: hash,
          };
        }

        const hash = await ctx.computeHash(effect.payload);
        return {
          correlation_id: correlationId,
          ok: true,
          value: hash,
        };
      }

      default: {
        return {
          correlation_id: correlationId,
          ok: false,
          error: {
            code: 'UNKNOWN_EFFECT',
            message: `Unknown effect type: ${effect.type}`,
          },
        };
      }
    }
  } catch (e) {
    return {
      correlation_id: correlationId,
      ok: false,
      error: {
        code: 'EXECUTION_ERROR',
        message: e instanceof Error ? e.message : String(e),
      },
    };
  }
}

/**
 * Simple hash function for fallback
 */
function simpleHash(str: string): string {
  let hash = 0;
  for (let i = 0; i < str.length; i++) {
    const char = str.charCodeAt(i);
    hash = ((hash << 5) - hash) + char;
    hash = hash & hash; // Convert to 32bit integer
  }
  return `simple:${Math.abs(hash).toString(16)}`;
}

/**
 * Process all effects from a kernel output
 */
export async function processEffects(
  effects: Effect[],
  ctx: CallbackContext
): Promise<CallbackBatchResult> {
  const results = new Map<string, CallbackResult>();
  const unhandled: Effect[] = [];

  // Process effects in parallel for efficiency
  const promises = effects.map(async (effect) => {
    const result = await processEffect(effect, ctx);
    if (!result.ok && result.error?.code === 'UNKNOWN_EFFECT') {
      unhandled.push(effect);
    }
    results.set(result.correlation_id, result);
    return result;
  });

  await Promise.all(promises);

  // Format feedback context for the next kernel step
  const feedbackContext = formatCallbackResults(results);

  return { results, unhandled, feedbackContext };
}

/**
 * Format callback results for feeding back to the kernel
 */
function formatCallbackResults(results: Map<string, CallbackResult>): string {
  if (results.size === 0) {
    return '';
  }

  const lines: string[] = ['CALLBACK RESULTS:'];

  for (const [id, result] of results) {
    if (result.ok) {
      lines.push(`  [${id}] OK: ${JSON.stringify(result.value)}`);
    } else {
      lines.push(`  [${id}] ERROR: ${result.error?.message ?? 'unknown'}`);
    }
  }

  return lines.join('\n');
}

/**
 * Check if any effects require callbacks
 */
export function hasCallbackEffects(effects: Effect[]): boolean {
  const callbackTypes: Set<string> = new Set([
    'callback.eval_lisp',
    'callback.artifact.get',
    'callback.facts.query',
    'callback.hash',
  ]);

  return effects.some((e) => callbackTypes.has(e.type));
}

/**
 * Extract callback effects from a list of effects
 */
export function extractCallbackEffects(effects: Effect[]): Effect[] {
  const callbackTypes: Set<string> = new Set([
    'callback.eval_lisp',
    'callback.artifact.get',
    'callback.facts.query',
    'callback.hash',
  ]);

  return effects.filter((e) => callbackTypes.has(e.type));
}
