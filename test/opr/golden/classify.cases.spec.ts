/**
 * Golden Test Cases: opr.classify.v1
 *
 * Tests classification kernel with various inputs and expected outputs.
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { OprRuntime } from '../../../src/core/opr/runtime';
import { InMemoryReceiptStore } from '../../../src/core/opr/receipts';
import { ScriptedOprAdapter } from '../../../src/core/opr/adapters/scripted';
import { CLASSIFY_KERNEL } from '../../../src/core/opr/kernels/classify';

describe('Golden: opr.classify.v1', () => {
  let receipts: InMemoryReceiptStore;

  beforeEach(() => {
    receipts = new InMemoryReceiptStore();
  });

  const runClassify = async (input: object, response: string) => {
    const adapter = new ScriptedOprAdapter({ responses: [response] });
    const runtime = new OprRuntime({
      kernel: CLASSIFY_KERNEL,
      adapter,
      receipts,
      budget: { maxAttempts: 3 },
    });
    return runtime.step({ program: input, state: null });
  };

  it('C1: single-label bug classification', async () => {
    const input = {
      item: 'App crashes with NullPointerException on startup',
      categories: ['bug', 'feature', 'question', 'docs'],
      multi_label: false,
    };

    const response = JSON.stringify({
      kernel: 'opr.classify.v1',
      op: 'classify',
      ok: true,
      result: {
        classification: 'bug',
        confidence: 0.95,
        scores: { bug: 0.95, feature: 0.02, question: 0.02, docs: 0.01 },
        reasoning: 'Contains crash and exception keywords',
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runClassify(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.classification).toBe('bug');
      expect(result.output.result.confidence).toBeGreaterThan(0.9);
    }
  });

  it('C2: single-label feature request', async () => {
    const input = {
      item: 'Add dark mode support to the settings page',
      categories: ['bug', 'feature', 'question', 'docs'],
      multi_label: false,
    };

    const response = JSON.stringify({
      kernel: 'opr.classify.v1',
      op: 'classify',
      ok: true,
      result: {
        classification: 'feature',
        confidence: 0.92,
        scores: { bug: 0.03, feature: 0.92, question: 0.03, docs: 0.02 },
        reasoning: 'Requests new functionality - dark mode',
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runClassify(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.classification).toBe('feature');
    }
  });

  it('C3: multi-label classification', async () => {
    const input = {
      item: 'How do I fix the login bug? Is there documentation?',
      categories: ['bug', 'feature', 'question', 'docs'],
      multi_label: true,
    };

    const response = JSON.stringify({
      kernel: 'opr.classify.v1',
      op: 'classify',
      ok: true,
      result: {
        classification: ['question', 'bug', 'docs'],
        confidence: 0.85,
        scores: { bug: 0.70, feature: 0.10, question: 0.88, docs: 0.75 },
        reasoning: 'Asks question about bug and requests docs',
      },
      next_state: null,
      effects: [],
      diagnostics: { close_call: true },
    });

    const result = await runClassify(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.classification).toContain('question');
      expect(result.output.result.classification).toContain('bug');
    }
  });

  it('C4: ambiguous input with close scores', async () => {
    const input = {
      item: 'The button does not work as expected',
      categories: ['bug', 'feature', 'question'],
      multi_label: false,
    };

    const response = JSON.stringify({
      kernel: 'opr.classify.v1',
      op: 'classify',
      ok: true,
      result: {
        classification: 'bug',
        confidence: 0.55,
        scores: { bug: 0.55, feature: 0.40, question: 0.05 },
        reasoning: 'Ambiguous - could be bug report or feature gap',
      },
      next_state: null,
      effects: [],
      diagnostics: { close_call: true, alternative: 'feature' },
    });

    const result = await runClassify(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.diagnostics.close_call).toBe(true);
    }
  });

  it('C5: sentiment classification', async () => {
    const input = {
      item: 'This product is absolutely amazing! Best purchase ever!',
      categories: ['positive', 'negative', 'neutral'],
      multi_label: false,
      context: 'product review sentiment',
    };

    const response = JSON.stringify({
      kernel: 'opr.classify.v1',
      op: 'classify',
      ok: true,
      result: {
        classification: 'positive',
        confidence: 0.98,
        scores: { positive: 0.98, negative: 0.01, neutral: 0.01 },
        reasoning: 'Strong positive language: amazing, best',
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runClassify(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.classification).toBe('positive');
      expect(result.output.result.confidence).toBeGreaterThan(0.95);
    }
  });

  it('C6: priority classification', async () => {
    const input = {
      item: 'Production is down! All users affected!',
      categories: ['critical', 'high', 'medium', 'low'],
      multi_label: false,
    };

    const response = JSON.stringify({
      kernel: 'opr.classify.v1',
      op: 'classify',
      ok: true,
      result: {
        classification: 'critical',
        confidence: 0.99,
        scores: { critical: 0.99, high: 0.01, medium: 0.0, low: 0.0 },
        reasoning: 'Production outage affecting all users',
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runClassify(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.classification).toBe('critical');
    }
  });

  it('C7: language detection', async () => {
    const input = {
      item: 'Bonjour, comment allez-vous?',
      categories: ['english', 'french', 'spanish', 'german'],
      multi_label: false,
    };

    const response = JSON.stringify({
      kernel: 'opr.classify.v1',
      op: 'classify',
      ok: true,
      result: {
        classification: 'french',
        confidence: 0.99,
        scores: { english: 0.0, french: 0.99, spanish: 0.01, german: 0.0 },
        reasoning: 'French greeting and question',
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runClassify(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.classification).toBe('french');
    }
  });

  it('C8: content moderation', async () => {
    const input = {
      item: 'Check out this legitimate business opportunity',
      categories: ['safe', 'spam', 'harmful', 'adult'],
      multi_label: false,
    };

    const response = JSON.stringify({
      kernel: 'opr.classify.v1',
      op: 'classify',
      ok: true,
      result: {
        classification: 'spam',
        confidence: 0.78,
        scores: { safe: 0.20, spam: 0.78, harmful: 0.01, adult: 0.01 },
        reasoning: 'Suspicious promotional language pattern',
      },
      next_state: null,
      effects: [],
      diagnostics: { close_call: true, alternative: 'safe' },
    });

    const result = await runClassify(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.classification).toBe('spam');
    }
  });

  it('C9: topic classification', async () => {
    const input = {
      item: 'The quantum entanglement experiment showed non-local correlations',
      categories: ['physics', 'chemistry', 'biology', 'mathematics'],
      multi_label: false,
    };

    const response = JSON.stringify({
      kernel: 'opr.classify.v1',
      op: 'classify',
      ok: true,
      result: {
        classification: 'physics',
        confidence: 0.96,
        scores: { physics: 0.96, chemistry: 0.02, biology: 0.01, mathematics: 0.01 },
        reasoning: 'Quantum physics terminology',
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runClassify(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.classification).toBe('physics');
    }
  });

  it('C10: empty item edge case', async () => {
    const input = {
      item: '',
      categories: ['a', 'b', 'c'],
      multi_label: false,
    };

    const response = JSON.stringify({
      kernel: 'opr.classify.v1',
      op: 'classify',
      ok: true,
      result: {
        classification: 'a',
        confidence: 0.33,
        scores: { a: 0.33, b: 0.33, c: 0.34 },
        reasoning: 'Empty input - no clear classification',
      },
      next_state: null,
      effects: [],
      diagnostics: { notes: ['Empty input provided'] },
    });

    const result = await runClassify(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.confidence).toBeLessThan(0.5);
    }
  });

  it('C11: retry on invalid response', async () => {
    const input = {
      item: 'Test item',
      categories: ['x', 'y'],
      multi_label: false,
    };

    const invalidResponse = JSON.stringify({ invalid: true });
    const validResponse = JSON.stringify({
      kernel: 'opr.classify.v1',
      op: 'classify',
      ok: true,
      result: { classification: 'x', confidence: 0.8, scores: { x: 0.8, y: 0.2 }, reasoning: 'test' },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const adapter = new ScriptedOprAdapter({ responses: [invalidResponse, validResponse] });
    const runtime = new OprRuntime({
      kernel: CLASSIFY_KERNEL,
      adapter,
      receipts,
      budget: { maxAttempts: 3 },
    });

    const result = await runtime.step({ program: input, state: null });
    expect(result.tag).toBe('ok');
    expect(result.attempts).toBe(2);
  });
});
