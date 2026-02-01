/**
 * OPR Callback Wiring Tests - THE KEYSTONE
 *
 * These tests verify that OPR kernels can call back into the Lisp runtime,
 * creating co-recursive symbolic/neural computation.
 *
 * This is P1.4 from REQUIREMENTS.md:
 * "If you wire it: kernels become genuine small-step interpreters for
 * micro-languages, fixpoint execution becomes a real operational semantics
 * tower, you get co-recursive symbolic/neural computation."
 */

import { describe, it, expect, vi } from 'vitest';
import {
  processEffects,
  hasCallbackEffects,
  extractCallbackEffects,
  type CallbackContext,
} from '../src/core/opr/callbacks';
import { OprRuntime, type OprRuntimeConfig } from '../src/core/opr/runtime';
import { InMemoryReceiptStore } from '../src/core/opr/receipts';
import type { Effect, KernelOutput } from '../src/core/opr/types';
import type { OprLLMAdapter, OprLLMRequest } from '../src/core/opr/adapters/types';
import type { Val } from '../src/core/eval/values';

// ============================================================================
// Mock Callback Context
// ============================================================================

function createMockCallbackContext(evalResults: Map<string, Val>): CallbackContext {
  return {
    evalLisp: async (source: string): Promise<Val> => {
      const result = evalResults.get(source);
      if (result) return result;
      // Simple arithmetic evaluation for testing
      if (source === '(+ 1 2)') return { tag: 'Num', n: 3 };
      if (source === '(* 2 3)') return { tag: 'Num', n: 6 };
      if (source === '(list 1 2 3)') {
        return {
          tag: 'Vector',
          items: [
            { tag: 'Num', n: 1 },
            { tag: 'Vector', items: [
              { tag: 'Num', n: 2 },
              { tag: 'Vector', items: [
                { tag: 'Num', n: 3 },
                { tag: 'Unit' }
              ]}
            ]}
          ]
        };
      }
      throw new Error(`Unknown expression: ${source}`);
    },
    getArtifact: async (key: string) => {
      if (key === 'test-artifact') return { data: 'test-data' };
      throw new Error(`Unknown artifact: ${key}`);
    },
    queryFacts: async (query: string) => {
      if (query === 'human(X)') return ['human(socrates)', 'human(plato)'];
      return [];
    },
    computeHash: async (value: unknown) => {
      return `hash:${JSON.stringify(value).length}`;
    },
  };
}

// ============================================================================
// Tests for processEffects
// ============================================================================

describe('OPR Callback Processing', () => {
  describe('processEffects', () => {
    it('should process callback.eval_lisp effects', async () => {
      const effects: Effect[] = [
        {
          type: 'callback.eval_lisp',
          idempotency_key: 'eval_1',
          correlation_id: 'corr_1',
          payload: { source: '(+ 1 2)' },
        },
      ];

      const ctx = createMockCallbackContext(new Map());
      const result = await processEffects(effects, ctx);

      expect(result.results.size).toBe(1);
      const callbackResult = result.results.get('corr_1');
      expect(callbackResult?.ok).toBe(true);
      expect(callbackResult?.value).toBe(3);
    });

    it('should process callback.eval_lisp with string payload', async () => {
      const effects: Effect[] = [
        {
          type: 'callback.eval_lisp',
          idempotency_key: 'eval_2',
          payload: '(* 2 3)', // Direct string payload
        },
      ];

      const ctx = createMockCallbackContext(new Map());
      const result = await processEffects(effects, ctx);

      const callbackResult = result.results.get('eval_2');
      expect(callbackResult?.ok).toBe(true);
      expect(callbackResult?.value).toBe(6);
    });

    it('should process callback.artifact.get effects', async () => {
      const effects: Effect[] = [
        {
          type: 'callback.artifact.get',
          idempotency_key: 'art_1',
          correlation_id: 'art_corr_1',
          payload: { key: 'test-artifact' },
        },
      ];

      const ctx = createMockCallbackContext(new Map());
      const result = await processEffects(effects, ctx);

      const callbackResult = result.results.get('art_corr_1');
      expect(callbackResult?.ok).toBe(true);
      expect(callbackResult?.value).toEqual({ data: 'test-data' });
    });

    it('should process callback.facts.query effects', async () => {
      const effects: Effect[] = [
        {
          type: 'callback.facts.query',
          idempotency_key: 'facts_1',
          payload: { query: 'human(X)' },
        },
      ];

      const ctx = createMockCallbackContext(new Map());
      const result = await processEffects(effects, ctx);

      const callbackResult = result.results.get('facts_1');
      expect(callbackResult?.ok).toBe(true);
      expect(callbackResult?.value).toEqual(['human(socrates)', 'human(plato)']);
    });

    it('should process callback.hash effects', async () => {
      const effects: Effect[] = [
        {
          type: 'callback.hash',
          idempotency_key: 'hash_1',
          payload: { data: 'test' },
        },
      ];

      const ctx = createMockCallbackContext(new Map());
      const result = await processEffects(effects, ctx);

      const callbackResult = result.results.get('hash_1');
      expect(callbackResult?.ok).toBe(true);
      expect(callbackResult?.value).toMatch(/^hash:/);
    });

    it('should process multiple effects in parallel', async () => {
      const effects: Effect[] = [
        {
          type: 'callback.eval_lisp',
          idempotency_key: 'eval_a',
          payload: '(+ 1 2)',
        },
        {
          type: 'callback.eval_lisp',
          idempotency_key: 'eval_b',
          payload: '(* 2 3)',
        },
      ];

      const ctx = createMockCallbackContext(new Map());
      const result = await processEffects(effects, ctx);

      expect(result.results.size).toBe(2);
      expect(result.results.get('eval_a')?.value).toBe(3);
      expect(result.results.get('eval_b')?.value).toBe(6);
    });

    it('should handle errors gracefully', async () => {
      const effects: Effect[] = [
        {
          type: 'callback.eval_lisp',
          idempotency_key: 'eval_err',
          payload: '(undefined-function)',
        },
      ];

      const ctx = createMockCallbackContext(new Map());
      const result = await processEffects(effects, ctx);

      const callbackResult = result.results.get('eval_err');
      expect(callbackResult?.ok).toBe(false);
      expect(callbackResult?.error?.code).toBe('EXECUTION_ERROR');
    });

    it('should mark unknown effect types as unhandled', async () => {
      const effects: Effect[] = [
        {
          type: 'callback.unknown' as any,
          idempotency_key: 'unknown_1',
          payload: {},
        },
      ];

      const ctx = createMockCallbackContext(new Map());
      const result = await processEffects(effects, ctx);

      expect(result.unhandled).toHaveLength(1);
      expect(result.results.get('unknown_1')?.ok).toBe(false);
      expect(result.results.get('unknown_1')?.error?.code).toBe('UNKNOWN_EFFECT');
    });

    it('should format feedback context correctly', async () => {
      const effects: Effect[] = [
        {
          type: 'callback.eval_lisp',
          idempotency_key: 'eval_fb',
          payload: '(+ 1 2)',
        },
      ];

      const ctx = createMockCallbackContext(new Map());
      const result = await processEffects(effects, ctx);

      expect(result.feedbackContext).toContain('CALLBACK RESULTS:');
      expect(result.feedbackContext).toContain('[eval_fb] OK:');
    });
  });

  describe('hasCallbackEffects', () => {
    it('should return true for callback effects', () => {
      const effects: Effect[] = [
        { type: 'callback.eval_lisp', idempotency_key: 'k1', payload: {} },
      ];
      expect(hasCallbackEffects(effects)).toBe(true);
    });

    it('should return false for empty array', () => {
      expect(hasCallbackEffects([])).toBe(false);
    });

    it('should return false for non-callback effects', () => {
      // This shouldn't happen in practice, but test the logic
      const effects = [
        { type: 'some.other.effect' as any, idempotency_key: 'k1', payload: {} },
      ];
      expect(hasCallbackEffects(effects)).toBe(false);
    });
  });

  describe('extractCallbackEffects', () => {
    it('should extract only callback effects', () => {
      const effects: Effect[] = [
        { type: 'callback.eval_lisp', idempotency_key: 'k1', payload: {} },
        { type: 'callback.hash', idempotency_key: 'k2', payload: {} },
      ];
      const extracted = extractCallbackEffects(effects);
      expect(extracted).toHaveLength(2);
    });
  });
});

// ============================================================================
// Tests for OPR Runtime with Callbacks
// ============================================================================

describe('OPR Runtime Callback Integration', () => {
  // Create a mock LLM adapter that returns scripted responses
  function createScriptedAdapter(responses: string[]): OprLLMAdapter {
    let callIndex = 0;
    return {
      complete: async (_request: OprLLMRequest): Promise<string> => {
        if (callIndex >= responses.length) {
          throw new Error('No more scripted responses');
        }
        return responses[callIndex++];
      },
      getModel: () => 'scripted-test',
      supportsStreaming: () => false,
    };
  }

  it('should pass callback results to next kernel step', async () => {
    // Kernel step 1: Emits a callback to evaluate (+ 1 2)
    const step1Response = JSON.stringify({
      kernel: 'test.kernel',
      op: 'step',
      ok: true,
      result: { message: 'Need to evaluate expression' },
      next_state: { iteration: 1, done: false },
      effects: [
        {
          type: 'callback.eval_lisp',
          idempotency_key: 'eval_step1',
          correlation_id: 'eval_step1',
          payload: { source: '(+ 1 2)' },
        },
      ],
      diagnostics: {},
    });

    // Kernel step 2: Receives callback result, completes
    const step2Response = JSON.stringify({
      kernel: 'test.kernel',
      op: 'step',
      ok: true,
      result: { computed: 3, message: 'Got the result!' },
      next_state: { iteration: 2, done: true },
      effects: [],
      diagnostics: {},
    });

    const adapter = createScriptedAdapter([step1Response, step2Response]);
    const callbacks = createMockCallbackContext(new Map());

    const config: OprRuntimeConfig = {
      kernel: {
        id: 'test.kernel',
        op: 'step',
        prompt: { tag: 'PromptDoc', v: 'frameir@1', parts: [] },
      },
      adapter,
      receipts: new InMemoryReceiptStore(),
      budget: { maxAttempts: 3 },
      callbacks,
    };

    const runtime = new OprRuntime(config);
    const result = await runtime.runToFixpoint({
      program: { input: 'test' },
      state: null,
    });

    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.iterations).toBe(2);
      expect(result.results).toHaveLength(2);
      // The second step should have had access to callback results
      expect(result.finalState?.done).toBe(true);
    }
  });

  it('should support runWithCallbacks method', async () => {
    const step1Response = JSON.stringify({
      kernel: 'test.kernel',
      op: 'step',
      ok: true,
      result: { status: 'done' },
      next_state: null, // Terminate immediately
      effects: [],
      diagnostics: {},
    });

    const adapter = createScriptedAdapter([step1Response]);
    const callbacks = createMockCallbackContext(new Map());

    const config: OprRuntimeConfig = {
      kernel: {
        id: 'test.kernel',
        op: 'step',
        prompt: { tag: 'PromptDoc', v: 'frameir@1', parts: [] },
      },
      adapter,
      receipts: new InMemoryReceiptStore(),
      budget: { maxAttempts: 3 },
      // Note: callbacks not provided in config
    };

    const runtime = new OprRuntime(config);

    // Use runWithCallbacks to provide callbacks at runtime
    const result = await runtime.runWithCallbacks(
      { program: { input: 'test' }, state: null },
      callbacks
    );

    expect(result.tag).toBe('ok');
  });

  it('should continue fixpoint loop with callback effects', async () => {
    // Simulate a logic kernel that:
    // 1. Derives a fact
    // 2. Calls back to query for related facts
    // 3. Derives more facts based on callback results
    // 4. Reaches fixpoint

    const responses = [
      // Step 1: Derive initial fact, request callback
      JSON.stringify({
        kernel: 'opr.logic.v1',
        op: 'infer',
        ok: true,
        result: { delta: ['mortal(socrates)'] },
        next_state: { iteration: 1, derived: ['mortal(socrates)'], done: false },
        effects: [
          {
            type: 'callback.facts.query',
            idempotency_key: 'query_1',
            payload: { query: 'human(X)' },
          },
        ],
        diagnostics: {},
      }),
      // Step 2: Process callback results, continue
      JSON.stringify({
        kernel: 'opr.logic.v1',
        op: 'infer',
        ok: true,
        result: { delta: ['mortal(plato)'] },
        next_state: { iteration: 2, derived: ['mortal(socrates)', 'mortal(plato)'], done: false },
        effects: [],
        diagnostics: {},
      }),
      // Step 3: No more facts to derive, fixpoint reached
      JSON.stringify({
        kernel: 'opr.logic.v1',
        op: 'infer',
        ok: true,
        result: { delta: [] },
        next_state: { iteration: 3, derived: ['mortal(socrates)', 'mortal(plato)'], done: true },
        effects: [],
        diagnostics: {},
      }),
    ];

    const adapter = createScriptedAdapter(responses);
    const callbacks = createMockCallbackContext(new Map());

    const config: OprRuntimeConfig = {
      kernel: {
        id: 'opr.logic.v1',
        op: 'infer',
        prompt: { tag: 'PromptDoc', v: 'frameir@1', parts: [] },
      },
      adapter,
      receipts: new InMemoryReceiptStore(),
      budget: { maxAttempts: 3 },
      callbacks,
    };

    const runtime = new OprRuntime(config);
    const result = await runtime.runToFixpoint({
      program: {
        rules: ['mortal(X) :- human(X)'],
        facts: ['human(socrates)'],
      },
      state: null,
    });

    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.iterations).toBe(3);
      expect(result.finalState?.done).toBe(true);
      expect(result.finalState?.derived).toContain('mortal(socrates)');
      expect(result.finalState?.derived).toContain('mortal(plato)');
    }
  });
});

// ============================================================================
// Tests for the Co-Recursive Tower
// ============================================================================

describe('Co-Recursive Symbolic/Neural Tower', () => {
  it('should demonstrate Lisp -> LLM -> Lisp -> LLM loop', async () => {
    /**
     * This test demonstrates the full co-recursive tower:
     *
     * 1. Lisp calls OPR kernel
     * 2. Kernel emits callback.eval_lisp effect
     * 3. Runtime evaluates Lisp, returns result
     * 4. Kernel continues with result
     * 5. Kernel reaches fixpoint
     *
     * This is THE KEYSTONE: the LLM becomes a computational agent
     * that can request symbolic computation mid-execution.
     */

    const evalLog: string[] = [];

    const callbacks: CallbackContext = {
      evalLisp: async (source: string): Promise<Val> => {
        evalLog.push(`eval: ${source}`);
        if (source === '(compute-next-step 1)') {
          return { tag: 'Num', n: 2 };
        }
        if (source === '(compute-next-step 2)') {
          return { tag: 'Num', n: 3 };
        }
        if (source === '(compute-next-step 3)') {
          return { tag: 'Str', s: 'done' };
        }
        throw new Error(`Unknown: ${source}`);
      },
    };

    const responses = [
      // Step 1: Need to compute next step
      JSON.stringify({
        kernel: 'compute.kernel',
        op: 'step',
        ok: true,
        result: { currentStep: 1 },
        next_state: { iteration: 1, done: false },
        effects: [
          {
            type: 'callback.eval_lisp',
            idempotency_key: 'compute_1',
            payload: { source: '(compute-next-step 1)' },
          },
        ],
        diagnostics: {},
      }),
      // Step 2: Got result 2, compute next
      JSON.stringify({
        kernel: 'compute.kernel',
        op: 'step',
        ok: true,
        result: { currentStep: 2 },
        next_state: { iteration: 2, done: false },
        effects: [
          {
            type: 'callback.eval_lisp',
            idempotency_key: 'compute_2',
            payload: { source: '(compute-next-step 2)' },
          },
        ],
        diagnostics: {},
      }),
      // Step 3: Got result 3, compute next
      JSON.stringify({
        kernel: 'compute.kernel',
        op: 'step',
        ok: true,
        result: { currentStep: 3 },
        next_state: { iteration: 3, done: false },
        effects: [
          {
            type: 'callback.eval_lisp',
            idempotency_key: 'compute_3',
            payload: { source: '(compute-next-step 3)' },
          },
        ],
        diagnostics: {},
      }),
      // Step 4: Got "done", terminate
      JSON.stringify({
        kernel: 'compute.kernel',
        op: 'step',
        ok: true,
        result: { finalResult: 'completed' },
        next_state: { iteration: 4, done: true },
        effects: [],
        diagnostics: {},
      }),
    ];

    let responseIndex = 0;
    const adapter: OprLLMAdapter = {
      complete: async (request: OprLLMRequest): Promise<string> => {
        // Verify that callback results are being passed
        if (responseIndex > 0) {
          expect(request.userContent).toContain('CALLBACK RESULTS FROM PREVIOUS STEP');
        }
        return responses[responseIndex++];
      },
      getModel: () => 'test',
      supportsStreaming: () => false,
    };

    const config: OprRuntimeConfig = {
      kernel: {
        id: 'compute.kernel',
        op: 'step',
        prompt: { tag: 'PromptDoc', v: 'frameir@1', parts: [] },
      },
      adapter,
      receipts: new InMemoryReceiptStore(),
      budget: { maxAttempts: 3 },
      callbacks,
    };

    const runtime = new OprRuntime(config);
    const result = await runtime.runToFixpoint({
      program: { start: 1 },
      state: null,
    });

    // Verify the co-recursive execution
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.iterations).toBe(4);
      expect(result.finalState?.done).toBe(true);
    }

    // Verify that Lisp was called at each step
    expect(evalLog).toEqual([
      'eval: (compute-next-step 1)',
      'eval: (compute-next-step 2)',
      'eval: (compute-next-step 3)',
    ]);
  });
});
