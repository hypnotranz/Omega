/**
 * OPR Runtime Integration Tests
 *
 * End-to-end tests that exercise the full runtime with scripted adapter
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { OprRuntime, type KernelPromptConfig } from '../../../src/core/opr/runtime';
import { InMemoryReceiptStore } from '../../../src/core/opr/receipts';
import { ScriptedOprAdapter } from '../../../src/core/opr/adapters/scripted';
import { verifyReceiptChain } from '../../../src/core/opr/receipts';

// Mock prompt doc for testing
const mockPrompt: any = {
  tag: 'PromptDoc',
  parts: [{ tag: 'System', content: 'You are a test kernel' }],
};

describe('OprRuntime Integration', () => {
  let receipts: InMemoryReceiptStore;
  let kernel: KernelPromptConfig;

  beforeEach(() => {
    receipts = new InMemoryReceiptStore();
    kernel = {
      id: 'test.kernel.v1',
      prompt: mockPrompt,
      op: 'step',
    };
  });

  describe('step() with scripted adapter', () => {
    it('RT1: successful step on first attempt', async () => {
      const validResponse = JSON.stringify({
        kernel: 'test.kernel.v1',
        op: 'step',
        ok: true,
        result: { message: 'success' },
        next_state: { iteration: 1 },
        effects: [],
        diagnostics: {},
      });

      const adapter = new ScriptedOprAdapter({ responses: [validResponse] });
      const runtime = new OprRuntime({
        kernel,
        adapter,
        receipts,
        budget: { maxAttempts: 3 },
      });

      const result = await runtime.step({ program: { test: true }, state: null });

      expect(result.tag).toBe('ok');
      expect(result.attempts).toBe(1);
      if (result.tag === 'ok') {
        expect(result.output.result).toEqual({ message: 'success' });
        expect(result.output.next_state).toEqual({ iteration: 1 });
      }
      expect(receipts.count()).toBe(1);
      expect(receipts.getAll()[0].status).toBe('OK');
    });

    it('RT2: retry with repair prompt on validation failure', async () => {
      // First response is invalid (missing fields)
      const invalidResponse = JSON.stringify({
        kernel: 'test.kernel.v1',
        op: 'step',
        // Missing: ok, result, next_state, effects, diagnostics
      });

      // Second response is valid
      const validResponse = JSON.stringify({
        kernel: 'test.kernel.v1',
        op: 'step',
        ok: true,
        result: { fixed: true },
        next_state: null,
        effects: [],
        diagnostics: {},
      });

      const adapter = new ScriptedOprAdapter({
        responses: [invalidResponse, validResponse],
      });

      const runtime = new OprRuntime({
        kernel,
        adapter,
        receipts,
        budget: { maxAttempts: 3 },
      });

      const result = await runtime.step({ program: {}, state: null });

      expect(result.tag).toBe('ok');
      expect(result.attempts).toBe(2);
      expect(receipts.count()).toBe(2);
      expect(receipts.getAll()[0].status).toBe('ERROR');
      expect(receipts.getAll()[1].status).toBe('OK');
    });

    it('RT3: budget exhausted after maxAttempts', async () => {
      // All responses are invalid
      const invalidResponse = JSON.stringify({ invalid: true });

      const adapter = new ScriptedOprAdapter({
        responses: [invalidResponse, invalidResponse, invalidResponse],
      });

      const runtime = new OprRuntime({
        kernel,
        adapter,
        receipts,
        budget: { maxAttempts: 3 },
      });

      const result = await runtime.step({ program: {}, state: null });

      expect(result.tag).toBe('budget-exhausted');
      expect(result.attempts).toBe(3);
      expect(receipts.count()).toBe(3);
    });

    it('RT4: handles JSON in markdown code blocks', async () => {
      const responseWithCodeBlock = '```json\n' + JSON.stringify({
        kernel: 'test.kernel.v1',
        op: 'step',
        ok: true,
        result: { extracted: true },
        next_state: null,
        effects: [],
        diagnostics: {},
      }) + '\n```';

      const adapter = new ScriptedOprAdapter({
        responses: [responseWithCodeBlock],
      });

      const runtime = new OprRuntime({
        kernel,
        adapter,
        receipts,
        budget: { maxAttempts: 3 },
      });

      const result = await runtime.step({ program: {}, state: null });

      expect(result.tag).toBe('ok');
      if (result.tag === 'ok') {
        expect(result.output.result).toEqual({ extracted: true });
      }
    });

    it('RT5: receipt chain is valid after multiple attempts', async () => {
      const invalidResponse = '{ not valid json';
      const validResponse = JSON.stringify({
        kernel: 'test.kernel.v1',
        op: 'step',
        ok: true,
        result: {},
        next_state: null,
        effects: [],
        diagnostics: {},
      });

      const adapter = new ScriptedOprAdapter({
        responses: [invalidResponse, invalidResponse, validResponse],
      });

      const runtime = new OprRuntime({
        kernel,
        adapter,
        receipts,
        budget: { maxAttempts: 5 },
      });

      await runtime.step({ program: {}, state: null });

      const chain = receipts.getAll();
      expect(chain.length).toBe(3);

      // Verify chain integrity
      const verification = verifyReceiptChain(chain);
      expect(verification.valid).toBe(true);

      // First receipt should have null prev_receipt_hash
      expect(chain[0].prev_receipt_hash).toBeNull();

      // Each subsequent receipt should link to previous
      expect(chain[1].prev_receipt_hash).toBe(chain[0].receipt_hash);
      expect(chain[2].prev_receipt_hash).toBe(chain[1].receipt_hash);
    });
  });

  describe('runToFixpoint() with scripted adapter', () => {
    it('RT6: terminates on done=true', async () => {
      const responses = [
        JSON.stringify({
          kernel: 'test.kernel.v1',
          op: 'step',
          ok: true,
          result: { step: 1 },
          next_state: { iteration: 1, done: false },
          effects: [],
          diagnostics: {},
        }),
        JSON.stringify({
          kernel: 'test.kernel.v1',
          op: 'step',
          ok: true,
          result: { step: 2 },
          next_state: { iteration: 2, done: true },
          effects: [],
          diagnostics: {},
        }),
      ];

      const adapter = new ScriptedOprAdapter({ responses });

      const runtime = new OprRuntime({
        kernel,
        adapter,
        receipts,
        budget: { maxAttempts: 3 },
      });

      const result = await runtime.runToFixpoint({ program: {}, state: null });

      expect(result.tag).toBe('ok');
      if (result.tag === 'ok') {
        expect(result.iterations).toBe(2);
        expect(result.finalState?.done).toBe(true);
        expect(result.results.length).toBe(2);
      }
    });

    it('RT7: terminates on next_state=null', async () => {
      const response = JSON.stringify({
        kernel: 'test.kernel.v1',
        op: 'step',
        ok: true,
        result: { complete: true },
        next_state: null,
        effects: [],
        diagnostics: {},
      });

      const adapter = new ScriptedOprAdapter({ responses: [response] });

      const runtime = new OprRuntime({
        kernel,
        adapter,
        receipts,
        budget: { maxAttempts: 3 },
      });

      const result = await runtime.runToFixpoint({ program: {}, state: null });

      expect(result.tag).toBe('ok');
      if (result.tag === 'ok') {
        expect(result.iterations).toBe(1);
        expect(result.finalState).toBeNull();
      }
    });

    it('RT8: reports error on step failure', async () => {
      const invalidResponse = JSON.stringify({ invalid: true });

      const adapter = new ScriptedOprAdapter({
        responses: [invalidResponse, invalidResponse, invalidResponse],
      });

      const runtime = new OprRuntime({
        kernel,
        adapter,
        receipts,
        budget: { maxAttempts: 3 },
      });

      const result = await runtime.runToFixpoint({ program: {}, state: null });

      expect(result.tag).toBe('error');
      if (result.tag === 'error') {
        expect(result.error.tag).toBe('budget-exhausted');
        expect(result.iterations).toBe(0);
      }
    });
  });

  describe('progress invariants', () => {
    it('RT9: enforces iteration monotonicity', async () => {
      const responses = [
        JSON.stringify({
          kernel: 'test.kernel.v1',
          op: 'step',
          ok: true,
          result: {},
          next_state: { iteration: 1 },
          effects: [],
          diagnostics: {},
        }),
        // Second step has non-increasing iteration - should trigger repair
        JSON.stringify({
          kernel: 'test.kernel.v1',
          op: 'step',
          ok: true,
          result: {},
          next_state: { iteration: 1 }, // Same as before, not increasing!
          effects: [],
          diagnostics: {},
        }),
        // Correct response
        JSON.stringify({
          kernel: 'test.kernel.v1',
          op: 'step',
          ok: true,
          result: {},
          next_state: { iteration: 2, done: true },
          effects: [],
          diagnostics: {},
        }),
      ];

      const adapter = new ScriptedOprAdapter({ responses });

      const runtime = new OprRuntime({
        kernel,
        adapter,
        receipts,
        budget: { maxAttempts: 5 },
        invariants: {
          iterationMonotonic: true,
          derivedMonotonic: false,
          deltaTermination: false,
        },
      });

      // First step
      const result1 = await runtime.step({ program: {}, state: null });
      expect(result1.tag).toBe('ok');

      // Second step with state from first
      if (result1.tag === 'ok') {
        const result2 = await runtime.step({
          program: {},
          state: result1.output.next_state,
        });
        // Should succeed after retry with correct response
        expect(result2.tag).toBe('ok');
        if (result2.tag === 'ok') {
          expect(result2.attempts).toBe(2); // Took 2 attempts
        }
      }
    });
  });

  describe('effects handling', () => {
    it('RT10: captures effects in output', async () => {
      const response = JSON.stringify({
        kernel: 'test.kernel.v1',
        op: 'step',
        ok: true,
        result: {},
        next_state: null,
        effects: [
          {
            type: 'callback.eval_lisp',
            idempotency_key: 'effect-1',
            payload: { expr: '(+ 1 2)' },
          },
          {
            type: 'callback.artifact.get',
            idempotency_key: 'effect-2',
            payload: { path: '/some/artifact' },
          },
        ],
        diagnostics: {},
      });

      const adapter = new ScriptedOprAdapter({ responses: [response] });

      const runtime = new OprRuntime({
        kernel,
        adapter,
        receipts,
        budget: { maxAttempts: 3 },
      });

      const result = await runtime.step({ program: {}, state: null });

      expect(result.tag).toBe('ok');
      if (result.tag === 'ok') {
        expect(result.output.effects.length).toBe(2);
        expect(result.output.effects[0].type).toBe('callback.eval_lisp');
        expect(result.output.effects[1].type).toBe('callback.artifact.get');
      }
    });
  });
});
