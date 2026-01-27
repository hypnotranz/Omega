import { describe, it, expect } from 'vitest';
import { buildRepairPrompt, shouldRetry } from '../../../src/core/opr/retry';

describe('buildRepairPrompt', () => {
  it('RY1: includes all violation fields', () => {
    const prompt = buildRepairPrompt([
      {
        path: '$.kernel',
        code: 'MISSING_FIELD',
        message: 'Missing required field: kernel',
      },
      {
        path: '$.ok',
        code: 'WRONG_TYPE',
        message: 'ok must be boolean',
        expected: 'boolean',
        actual: 'string',
      },
    ]);

    expect(prompt).toContain('VALIDATION ERRORS');
    expect(prompt).toContain('$.kernel');
    expect(prompt).toContain('MISSING_FIELD');
    expect(prompt).toContain('Expected: boolean');
    expect(prompt).toContain('Got: string');
  });

  it('RY2: instructs to return only JSON', () => {
    const prompt = buildRepairPrompt([]);
    expect(prompt).toContain('ONLY valid JSON');
    expect(prompt).toContain('Do NOT include markdown');
  });

  it('RY2b: includes path for each violation', () => {
    const prompt = buildRepairPrompt([
      { path: '$.effects[0].type', code: 'MISSING_FIELD', message: 'Missing type' },
      { path: '$.next_state', code: 'WRONG_TYPE', message: 'Wrong type' },
    ]);

    expect(prompt).toContain('$.effects[0].type');
    expect(prompt).toContain('$.next_state');
  });
});

describe('shouldRetry', () => {
  it('RY3: returns false for KERNEL_MISMATCH', () => {
    const result = shouldRetry([
      { path: '$.kernel', code: 'KERNEL_MISMATCH', message: 'Wrong kernel' },
    ]);
    expect(result).toBe(false);
  });

  it('RY3b: returns false for OP_MISMATCH', () => {
    const result = shouldRetry([{ path: '$.op', code: 'OP_MISMATCH', message: 'Wrong op' }]);
    expect(result).toBe(false);
  });

  it('RY4: returns true for fixable violations', () => {
    const result = shouldRetry([
      { path: '$.ok', code: 'MISSING_FIELD', message: 'Missing field' },
      { path: '$.result', code: 'WRONG_TYPE', message: 'Wrong type' },
    ]);
    expect(result).toBe(true);
  });

  it('RY4b: returns true for NOT_JSON', () => {
    const result = shouldRetry([{ path: '$', code: 'NOT_JSON', message: 'Invalid JSON' }]);
    expect(result).toBe(true);
  });

  it('RY4c: returns true for NOT_OBJECT', () => {
    const result = shouldRetry([{ path: '$', code: 'NOT_OBJECT', message: 'Not an object' }]);
    expect(result).toBe(true);
  });

  it('RY4d: returns true for INVALID_VALUE', () => {
    const result = shouldRetry([{ path: '$.iteration', code: 'INVALID_VALUE', message: 'Bad value' }]);
    expect(result).toBe(true);
  });
});
