/**
 * Golden Test Cases: opr.extract.v1
 *
 * Tests data extraction kernel with various inputs and schemas.
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { OprRuntime } from '../../../src/core/opr/runtime';
import { InMemoryReceiptStore } from '../../../src/core/opr/receipts';
import { ScriptedOprAdapter } from '../../../src/core/opr/adapters/scripted';
import { EXTRACT_KERNEL } from '../../../src/core/opr/kernels/extract';

describe('Golden: opr.extract.v1', () => {
  let receipts: InMemoryReceiptStore;

  beforeEach(() => {
    receipts = new InMemoryReceiptStore();
  });

  const runExtract = async (input: object, response: string) => {
    const adapter = new ScriptedOprAdapter({ responses: [response] });
    const runtime = new OprRuntime({
      kernel: EXTRACT_KERNEL,
      adapter,
      receipts,
      budget: { maxAttempts: 3 },
    });
    return runtime.step({ program: input, state: null });
  };

  it('E1: extract person info from email', async () => {
    const input = {
      text: 'From: John Smith <john@example.com>\nDate: January 15, 2024\nSubject: Invoice #1234 for $1,500.00',
      schema: {
        type: 'object',
        properties: {
          name: { type: 'string' },
          email: { type: 'string' },
          date: { type: 'string' },
          amount: { type: 'number' },
        },
      },
    };

    const response = JSON.stringify({
      kernel: 'opr.extract.v1',
      op: 'extract',
      ok: true,
      result: {
        data: { name: 'John Smith', email: 'john@example.com', date: '2024-01-15', amount: 1500.0 },
        confidence: { name: 0.98, email: 0.99, date: 0.95, amount: 0.97 },
        sources: { name: 'From header', email: 'From header', date: 'Date line', amount: 'Subject line' },
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runExtract(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.data.name).toBe('John Smith');
      expect(result.output.result.data.amount).toBe(1500.0);
    }
  });

  it('E2: extract addresses from text', async () => {
    const input = {
      text: 'Ship to: 123 Main St, New York, NY 10001. Bill to: 456 Oak Ave, Los Angeles, CA 90001.',
      schema: {
        type: 'array',
        items: {
          type: 'object',
          properties: { street: { type: 'string' }, city: { type: 'string' }, state: { type: 'string' }, zip: { type: 'string' } },
        },
      },
    };

    const response = JSON.stringify({
      kernel: 'opr.extract.v1',
      op: 'extract',
      ok: true,
      result: {
        data: [
          { street: '123 Main St', city: 'New York', state: 'NY', zip: '10001' },
          { street: '456 Oak Ave', city: 'Los Angeles', state: 'CA', zip: '90001' },
        ],
        confidence: { overall: 0.94 },
        sources: { 0: 'Ship to section', 1: 'Bill to section' },
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runExtract(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.data).toHaveLength(2);
    }
  });

  it('E3: extract dates in various formats', async () => {
    const input = {
      text: 'Meeting scheduled for 01/15/2024. Deadline: March 20th, 2024. Completed 2024-02-01.',
      schema: { type: 'array', items: { type: 'string', format: 'date' } },
    };

    const response = JSON.stringify({
      kernel: 'opr.extract.v1',
      op: 'extract',
      ok: true,
      result: {
        data: ['2024-01-15', '2024-03-20', '2024-02-01'],
        confidence: { 0: 0.95, 1: 0.92, 2: 0.99 },
        sources: { 0: 'MM/DD/YYYY format', 1: 'Natural language', 2: 'ISO format' },
      },
      next_state: null,
      effects: [],
      diagnostics: { notes: ['Normalized all dates to ISO format'] },
    });

    const result = await runExtract(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.data).toHaveLength(3);
    }
  });

  it('E4: extract with missing required field', async () => {
    const input = {
      text: 'Customer: Jane Doe. Order placed.',
      schema: {
        type: 'object',
        properties: { name: { type: 'string' }, phone: { type: 'string' } },
        required: ['name', 'phone'],
      },
    };

    const response = JSON.stringify({
      kernel: 'opr.extract.v1',
      op: 'extract',
      ok: true,
      result: {
        data: { name: 'Jane Doe', phone: null },
        confidence: { name: 0.98, phone: 0.0 },
        sources: { name: 'Customer line' },
      },
      next_state: null,
      effects: [],
      diagnostics: { unmatched_fields: ['phone'] },
    });

    const result = await runExtract(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.data.phone).toBeNull();
      expect(result.output.diagnostics.unmatched_fields).toContain('phone');
    }
  });

  it('E5: extract numeric values', async () => {
    const input = {
      text: 'Revenue: $1,234,567.89. Units sold: 5,432. Growth: 15.7%',
      schema: {
        type: 'object',
        properties: { revenue: { type: 'number' }, units: { type: 'integer' }, growth: { type: 'number' } },
      },
    };

    const response = JSON.stringify({
      kernel: 'opr.extract.v1',
      op: 'extract',
      ok: true,
      result: {
        data: { revenue: 1234567.89, units: 5432, growth: 15.7 },
        confidence: { revenue: 0.99, units: 0.99, growth: 0.98 },
        sources: { revenue: 'Revenue line', units: 'Units line', growth: 'Growth line' },
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runExtract(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.data.revenue).toBe(1234567.89);
      expect(result.output.result.data.units).toBe(5432);
    }
  });

  it('E6: extract nested objects', async () => {
    const input = {
      text: 'Product: Widget Pro. Manufacturer: Acme Corp, founded 1990. Price: $99.99',
      schema: {
        type: 'object',
        properties: {
          product: { type: 'string' },
          manufacturer: { type: 'object', properties: { name: { type: 'string' }, founded: { type: 'integer' } } },
          price: { type: 'number' },
        },
      },
    };

    const response = JSON.stringify({
      kernel: 'opr.extract.v1',
      op: 'extract',
      ok: true,
      result: {
        data: { product: 'Widget Pro', manufacturer: { name: 'Acme Corp', founded: 1990 }, price: 99.99 },
        confidence: { product: 0.99, manufacturer: 0.95, price: 0.99 },
        sources: {},
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runExtract(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.data.manufacturer.name).toBe('Acme Corp');
    }
  });

  it('E7: extract URLs and emails', async () => {
    const input = {
      text: 'Visit https://example.com or email support@example.org',
      schema: {
        type: 'object',
        properties: { url: { type: 'string', format: 'uri' }, email: { type: 'string', format: 'email' } },
      },
    };

    const response = JSON.stringify({
      kernel: 'opr.extract.v1',
      op: 'extract',
      ok: true,
      result: {
        data: { url: 'https://example.com', email: 'support@example.org' },
        confidence: { url: 0.99, email: 0.99 },
        sources: {},
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runExtract(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.data.url).toBe('https://example.com');
    }
  });

  it('E8: extract from table-like text', async () => {
    const input = {
      text: 'Name    Age   City\nAlice   30    Boston\nBob     25    Denver',
      schema: {
        type: 'array',
        items: { type: 'object', properties: { name: { type: 'string' }, age: { type: 'integer' }, city: { type: 'string' } } },
      },
    };

    const response = JSON.stringify({
      kernel: 'opr.extract.v1',
      op: 'extract',
      ok: true,
      result: {
        data: [
          { name: 'Alice', age: 30, city: 'Boston' },
          { name: 'Bob', age: 25, city: 'Denver' },
        ],
        confidence: { overall: 0.96 },
        sources: { format: 'tabular' },
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runExtract(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.data[0].name).toBe('Alice');
    }
  });

  it('E9: extract with ambiguous data', async () => {
    const input = {
      text: 'Date: 01/02/03',
      schema: { type: 'object', properties: { date: { type: 'string', format: 'date' } } },
    };

    const response = JSON.stringify({
      kernel: 'opr.extract.v1',
      op: 'extract',
      ok: true,
      result: {
        data: { date: '2003-01-02' },
        confidence: { date: 0.6 },
        sources: {},
      },
      next_state: null,
      effects: [],
      diagnostics: { ambiguous: ['Could be MM/DD/YY, DD/MM/YY, or YY/MM/DD'] },
    });

    const result = await runExtract(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.diagnostics.ambiguous).toBeDefined();
    }
  });

  it('E10: extract empty result from irrelevant text', async () => {
    const input = {
      text: 'The quick brown fox jumps over the lazy dog.',
      schema: { type: 'object', properties: { phone: { type: 'string' }, ssn: { type: 'string' } } },
    };

    const response = JSON.stringify({
      kernel: 'opr.extract.v1',
      op: 'extract',
      ok: true,
      result: {
        data: { phone: null, ssn: null },
        confidence: { phone: 0.0, ssn: 0.0 },
        sources: {},
      },
      next_state: null,
      effects: [],
      diagnostics: { unmatched_fields: ['phone', 'ssn'] },
    });

    const result = await runExtract(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.data.phone).toBeNull();
    }
  });

  it('E11: retry on malformed response', async () => {
    const input = { text: 'Test', schema: { type: 'object', properties: { x: { type: 'string' } } } };

    const invalidResponse = '{ not valid json';
    const validResponse = JSON.stringify({
      kernel: 'opr.extract.v1',
      op: 'extract',
      ok: true,
      result: { data: { x: 'test' }, confidence: { x: 0.9 }, sources: {} },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const adapter = new ScriptedOprAdapter({ responses: [invalidResponse, validResponse] });
    const runtime = new OprRuntime({
      kernel: EXTRACT_KERNEL,
      adapter,
      receipts,
      budget: { maxAttempts: 3 },
    });

    const result = await runtime.step({ program: input, state: null });
    expect(result.tag).toBe('ok');
    expect(result.attempts).toBe(2);
  });
});
