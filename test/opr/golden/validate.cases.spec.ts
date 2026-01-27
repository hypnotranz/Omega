/**
 * Golden Test Cases: opr.validate.v1
 *
 * Tests validation kernel with various data and rules.
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { OprRuntime } from '../../../src/core/opr/runtime';
import { InMemoryReceiptStore } from '../../../src/core/opr/receipts';
import { ScriptedOprAdapter } from '../../../src/core/opr/adapters/scripted';
import { VALIDATE_KERNEL } from '../../../src/core/opr/kernels/validate';

describe('Golden: opr.validate.v1', () => {
  let receipts: InMemoryReceiptStore;

  beforeEach(() => {
    receipts = new InMemoryReceiptStore();
  });

  const runValidate = async (input: object, response: string) => {
    const adapter = new ScriptedOprAdapter({ responses: [response] });
    const runtime = new OprRuntime({
      kernel: VALIDATE_KERNEL,
      adapter,
      receipts,
      budget: { maxAttempts: 3 },
    });
    return runtime.step({ program: input, state: null });
  };

  it('V1: valid email format', async () => {
    const input = {
      data: { email: 'user@example.com' },
      rules: [{ field: 'email', rule: 'email_format' }],
    };

    const response = JSON.stringify({
      kernel: 'opr.validate.v1',
      op: 'validate',
      ok: true,
      result: { valid: true, errors: [], warnings: [], passed: ['email'] },
      next_state: null,
      effects: [],
      diagnostics: { rules_checked: 1 },
    });

    const result = await runValidate(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.valid).toBe(true);
    }
  });

  it('V2: invalid email format', async () => {
    const input = {
      data: { email: 'not-an-email' },
      rules: [{ field: 'email', rule: 'email_format' }],
    };

    const response = JSON.stringify({
      kernel: 'opr.validate.v1',
      op: 'validate',
      ok: true,
      result: {
        valid: false,
        errors: [{ field: 'email', rule: 'email_format', value: 'not-an-email', message: 'Invalid email format' }],
        warnings: [],
        passed: [],
      },
      next_state: null,
      effects: [],
      diagnostics: { rules_checked: 1 },
    });

    const result = await runValidate(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.valid).toBe(false);
      expect(result.output.result.errors).toHaveLength(1);
    }
  });

  it('V3: range validation', async () => {
    const input = {
      data: { age: 150 },
      rules: [{ field: 'age', rule: 'range', min: 0, max: 120 }],
    };

    const response = JSON.stringify({
      kernel: 'opr.validate.v1',
      op: 'validate',
      ok: true,
      result: {
        valid: false,
        errors: [{ field: 'age', rule: 'range', value: 150, message: 'Value must be between 0 and 120' }],
        warnings: [],
        passed: [],
      },
      next_state: null,
      effects: [],
      diagnostics: { rules_checked: 1 },
    });

    const result = await runValidate(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.errors[0].rule).toBe('range');
    }
  });

  it('V4: required field missing', async () => {
    const input = {
      data: { name: 'John' },
      rules: [{ field: 'email', rule: 'required' }],
    };

    const response = JSON.stringify({
      kernel: 'opr.validate.v1',
      op: 'validate',
      ok: true,
      result: {
        valid: false,
        errors: [{ field: 'email', rule: 'required', value: null, message: 'Field is required' }],
        warnings: [],
        passed: [],
      },
      next_state: null,
      effects: [],
      diagnostics: { rules_checked: 1 },
    });

    const result = await runValidate(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.errors[0].rule).toBe('required');
    }
  });

  it('V5: pattern validation', async () => {
    const input = {
      data: { zip: '1234' },
      rules: [{ field: 'zip', rule: 'pattern', pattern: '^\\d{5}$' }],
    };

    const response = JSON.stringify({
      kernel: 'opr.validate.v1',
      op: 'validate',
      ok: true,
      result: {
        valid: false,
        errors: [{ field: 'zip', rule: 'pattern', value: '1234', message: 'Must be 5 digits' }],
        warnings: [],
        passed: [],
      },
      next_state: null,
      effects: [],
      diagnostics: { rules_checked: 1 },
    });

    const result = await runValidate(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.valid).toBe(false);
    }
  });

  it('V6: cross-field validation', async () => {
    const input = {
      data: { start_date: '2024-01-15', end_date: '2024-01-10' },
      rules: [{ expr: 'start_date < end_date' }],
    };

    const response = JSON.stringify({
      kernel: 'opr.validate.v1',
      op: 'validate',
      ok: true,
      result: {
        valid: false,
        errors: [{ expr: 'start_date < end_date', message: 'start_date must be before end_date' }],
        warnings: [],
        passed: [],
      },
      next_state: null,
      effects: [],
      diagnostics: { rules_checked: 1 },
    });

    const result = await runValidate(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.valid).toBe(false);
    }
  });

  it('V7: multiple rules all pass', async () => {
    const input = {
      data: { email: 'test@test.com', age: 25, name: 'John' },
      rules: [
        { field: 'email', rule: 'email_format' },
        { field: 'age', rule: 'range', min: 0, max: 120 },
        { field: 'name', rule: 'required' },
      ],
    };

    const response = JSON.stringify({
      kernel: 'opr.validate.v1',
      op: 'validate',
      ok: true,
      result: { valid: true, errors: [], warnings: [], passed: ['email', 'age', 'name'] },
      next_state: null,
      effects: [],
      diagnostics: { rules_checked: 3 },
    });

    const result = await runValidate(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.passed).toHaveLength(3);
    }
  });

  it('V8: lenient mode with warnings', async () => {
    const input = {
      data: { phone: '1234567890' },
      rules: [{ field: 'phone', rule: 'phone_format' }],
      mode: 'lenient',
    };

    const response = JSON.stringify({
      kernel: 'opr.validate.v1',
      op: 'validate',
      ok: true,
      result: {
        valid: true,
        errors: [],
        warnings: [{ field: 'phone', message: 'Phone number missing country code' }],
        passed: ['phone'],
      },
      next_state: null,
      effects: [],
      diagnostics: { rules_checked: 1 },
    });

    const result = await runValidate(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.valid).toBe(true);
      expect(result.output.result.warnings).toHaveLength(1);
    }
  });

  it('V9: type validation', async () => {
    const input = {
      data: { count: 'five' },
      rules: [{ field: 'count', rule: 'type', type: 'number' }],
    };

    const response = JSON.stringify({
      kernel: 'opr.validate.v1',
      op: 'validate',
      ok: true,
      result: {
        valid: false,
        errors: [{ field: 'count', rule: 'type', value: 'five', message: 'Expected number, got string' }],
        warnings: [],
        passed: [],
      },
      next_state: null,
      effects: [],
      diagnostics: { rules_checked: 1 },
    });

    const result = await runValidate(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.errors[0].message).toContain('number');
    }
  });

  it('V10: URL validation', async () => {
    const input = {
      data: { website: 'https://example.com' },
      rules: [{ field: 'website', rule: 'url' }],
    };

    const response = JSON.stringify({
      kernel: 'opr.validate.v1',
      op: 'validate',
      ok: true,
      result: { valid: true, errors: [], warnings: [], passed: ['website'] },
      next_state: null,
      effects: [],
      diagnostics: { rules_checked: 1 },
    });

    const result = await runValidate(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.valid).toBe(true);
    }
  });

  it('V11: schema-based validation', async () => {
    const input = {
      data: { user: { name: 'John', age: 'not-a-number' } },
      schema: {
        type: 'object',
        properties: { user: { type: 'object', properties: { name: { type: 'string' }, age: { type: 'integer' } } } },
      },
    };

    const response = JSON.stringify({
      kernel: 'opr.validate.v1',
      op: 'validate',
      ok: true,
      result: {
        valid: false,
        errors: [{ field: 'user.age', rule: 'type', value: 'not-a-number', message: 'Expected integer' }],
        warnings: [],
        passed: ['user.name'],
      },
      next_state: null,
      effects: [],
      diagnostics: { rules_checked: 2 },
    });

    const result = await runValidate(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.errors[0].field).toBe('user.age');
    }
  });
});
