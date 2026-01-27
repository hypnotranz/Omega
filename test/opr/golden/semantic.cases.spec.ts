/**
 * Golden Test Cases: opr.semantic.v1
 *
 * Tests semantic understanding kernel with various texts and modes.
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { OprRuntime } from '../../../src/core/opr/runtime';
import { InMemoryReceiptStore } from '../../../src/core/opr/receipts';
import { ScriptedOprAdapter } from '../../../src/core/opr/adapters/scripted';
import { SEMANTIC_KERNEL } from '../../../src/core/opr/kernels/semantic';

describe('Golden: opr.semantic.v1', () => {
  let receipts: InMemoryReceiptStore;

  beforeEach(() => {
    receipts = new InMemoryReceiptStore();
  });

  const runSemantic = async (input: object, response: string) => {
    const adapter = new ScriptedOprAdapter({ responses: [response] });
    const runtime = new OprRuntime({
      kernel: SEMANTIC_KERNEL,
      adapter,
      receipts,
      budget: { maxAttempts: 3 },
    });
    return runtime.step({ program: input, state: null });
  };

  it('SM1: intent extraction - query', async () => {
    const input = {
      text: 'What is the weather like tomorrow in New York?',
      mode: 'intent',
    };

    const response = JSON.stringify({
      kernel: 'opr.semantic.v1',
      op: 'understand',
      ok: true,
      result: {
        intent: { action: 'query', target: 'weather', modifiers: ['temporal', 'location'] },
        entities: [],
        relations: [],
        presuppositions: [],
        confidence: 0.95,
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runSemantic(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.intent.action).toBe('query');
    }
  });

  it('SM2: entity extraction', async () => {
    const input = {
      text: 'John Smith works at Microsoft in Seattle.',
      mode: 'entities',
    };

    const response = JSON.stringify({
      kernel: 'opr.semantic.v1',
      op: 'understand',
      ok: true,
      result: {
        intent: null,
        entities: [
          { text: 'John Smith', type: 'person', normalized: 'John Smith' },
          { text: 'Microsoft', type: 'organization', normalized: 'Microsoft Corporation' },
          { text: 'Seattle', type: 'location', normalized: 'Seattle, WA, USA' },
        ],
        relations: [],
        presuppositions: [],
        confidence: 0.92,
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runSemantic(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.entities).toHaveLength(3);
    }
  });

  it('SM3: relation extraction', async () => {
    const input = {
      text: 'Alice is married to Bob.',
      mode: 'relations',
    };

    const response = JSON.stringify({
      kernel: 'opr.semantic.v1',
      op: 'understand',
      ok: true,
      result: {
        intent: null,
        entities: [
          { text: 'Alice', type: 'person' },
          { text: 'Bob', type: 'person' },
        ],
        relations: [{ subject: 'Alice', predicate: 'married_to', object: 'Bob' }],
        presuppositions: [],
        confidence: 0.98,
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runSemantic(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.relations[0].predicate).toBe('married_to');
    }
  });

  it('SM4: full semantic analysis', async () => {
    const input = {
      text: 'Can you book a flight from NYC to LA for next Friday?',
      mode: 'full',
    };

    const response = JSON.stringify({
      kernel: 'opr.semantic.v1',
      op: 'understand',
      ok: true,
      result: {
        intent: { action: 'request', target: 'booking', subtype: 'flight' },
        entities: [
          { text: 'NYC', type: 'location', normalized: 'New York City' },
          { text: 'LA', type: 'location', normalized: 'Los Angeles' },
          { text: 'next Friday', type: 'time', normalized: '2024-01-19' },
        ],
        relations: [
          { subject: 'flight', predicate: 'origin', object: 'NYC' },
          { subject: 'flight', predicate: 'destination', object: 'LA' },
          { subject: 'flight', predicate: 'date', object: 'next Friday' },
        ],
        presuppositions: ['speaker has authority to book', 'flights exist for this route'],
        confidence: 0.91,
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runSemantic(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.intent.subtype).toBe('flight');
      expect(result.output.result.relations).toHaveLength(3);
    }
  });

  it('SM5: command intent', async () => {
    const input = {
      text: 'Delete all files in the temp folder.',
      mode: 'intent',
    };

    const response = JSON.stringify({
      kernel: 'opr.semantic.v1',
      op: 'understand',
      ok: true,
      result: {
        intent: { action: 'delete', target: 'files', scope: 'all', location: 'temp folder' },
        entities: [],
        relations: [],
        presuppositions: ['user has permission'],
        confidence: 0.97,
      },
      next_state: null,
      effects: [],
      diagnostics: { warnings: ['Destructive action detected'] },
    });

    const result = await runSemantic(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.intent.action).toBe('delete');
    }
  });

  it('SM6: temporal expressions', async () => {
    const input = {
      text: 'The meeting was scheduled for yesterday at 3pm.',
      mode: 'entities',
    };

    const response = JSON.stringify({
      kernel: 'opr.semantic.v1',
      op: 'understand',
      ok: true,
      result: {
        intent: null,
        entities: [
          { text: 'yesterday at 3pm', type: 'time', normalized: '2024-01-26T15:00:00' },
          { text: 'meeting', type: 'event' },
        ],
        relations: [],
        presuppositions: [],
        confidence: 0.89,
      },
      next_state: null,
      effects: [],
      diagnostics: { notes: ['Relative time resolved to absolute'] },
    });

    const result = await runSemantic(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.entities[0].type).toBe('time');
    }
  });

  it('SM7: ambiguous text', async () => {
    const input = {
      text: 'I saw her duck.',
      mode: 'full',
    };

    const response = JSON.stringify({
      kernel: 'opr.semantic.v1',
      op: 'understand',
      ok: true,
      result: {
        intent: { action: 'inform', target: 'observation' },
        entities: [{ text: 'her', type: 'person' }],
        relations: [],
        presuppositions: [],
        confidence: 0.6,
      },
      next_state: null,
      effects: [],
      diagnostics: { ambiguities: ['duck could be noun (animal) or verb (to duck/dodge)'] },
    });

    const result = await runSemantic(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.confidence).toBeLessThan(0.7);
      expect(result.output.diagnostics.ambiguities).toBeDefined();
    }
  });

  it('SM8: negation handling', async () => {
    const input = {
      text: 'I do not want to cancel my subscription.',
      mode: 'intent',
    };

    const response = JSON.stringify({
      kernel: 'opr.semantic.v1',
      op: 'understand',
      ok: true,
      result: {
        intent: { action: 'retain', target: 'subscription', negated_intent: 'cancel' },
        entities: [],
        relations: [],
        presuppositions: ['user has a subscription'],
        confidence: 0.94,
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runSemantic(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.intent.negated_intent).toBe('cancel');
    }
  });

  it('SM9: context-dependent understanding', async () => {
    const input = {
      text: 'Give me more of the same.',
      context: { domain: 'restaurant', prior: ['User ordered coffee'] },
      mode: 'intent',
    };

    const response = JSON.stringify({
      kernel: 'opr.semantic.v1',
      op: 'understand',
      ok: true,
      result: {
        intent: { action: 'order', target: 'coffee', quantity: 'additional' },
        entities: [],
        relations: [],
        presuppositions: ['previous order exists'],
        confidence: 0.88,
      },
      next_state: null,
      effects: [],
      diagnostics: { notes: ['Resolved anaphoric reference using context'] },
    });

    const result = await runSemantic(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.intent.target).toBe('coffee');
    }
  });

  it('SM10: multi-sentence text', async () => {
    const input = {
      text: 'The project is due next week. We need to finish the report first.',
      mode: 'full',
    };

    const response = JSON.stringify({
      kernel: 'opr.semantic.v1',
      op: 'understand',
      ok: true,
      result: {
        intent: { action: 'inform', topics: ['deadline', 'task'] },
        entities: [
          { text: 'project', type: 'work_item' },
          { text: 'next week', type: 'time' },
          { text: 'report', type: 'work_item' },
        ],
        relations: [
          { subject: 'project', predicate: 'due_date', object: 'next week' },
          { subject: 'report', predicate: 'prerequisite_of', object: 'project' },
        ],
        presuppositions: [],
        confidence: 0.90,
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runSemantic(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.entities).toHaveLength(3);
    }
  });
});
