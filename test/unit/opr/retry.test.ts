import { describe, it, expect } from 'vitest';
import type { ValidationViolation } from '../src/core/opr/types';
import {
  buildRepairPrompt,
  shouldRetry,
  formatViolationsForDisplay,
} from '../src/core/opr/retry';

describe('retry - buildRepairPrompt', () => {
  it('should create a repair prompt with violation details', () => {
    const violations: ValidationViolation[] = [
      {
        path: 'root.kernel',
        code: 'MISSING_FIELD',
        message: 'Required field is missing',
      },
    ];

    const prompt = buildRepairPrompt(violations);

    expect(prompt).toContain('YOUR PREVIOUS RESPONSE HAD VALIDATION ERRORS');
    expect(prompt).toContain('VIOLATIONS:');
    expect(prompt).toContain('root.kernel');
    expect(prompt).toContain('MISSING_FIELD');
    expect(prompt).toContain('Required field is missing');
    expect(prompt).toContain('INSTRUCTIONS:');
    expect(prompt).toContain('Fix ALL violations listed above');
    expect(prompt).toContain('Return ONLY valid JSON matching the OUTPUT CONTRACT');
    expect(prompt).toContain('Do NOT include markdown code blocks');
    expect(prompt).toContain('Return the corrected JSON response now:');
  });

  it('RY1: should build repair prompt with all violation fields', () => {
    const violations: ValidationViolation[] = [
      {
        path: 'root.kernel',
        code: 'WRONG_TYPE',
        message: 'Field should be a string',
        expected: 'string',
        actual: 'number',
      },
      {
        path: 'root.effects',
        code: 'NOT_OBJECT',
        message: 'Field should be an object',
      },
    ];

    const prompt = buildRepairPrompt(violations);

    // Check first violation with all fields
    expect(prompt).toContain('Path: root.kernel');
    expect(prompt).toContain('Code: WRONG_TYPE');
    expect(prompt).toContain('Field should be a string');
    expect(prompt).toContain('Expected: string');
    expect(prompt).toContain('Got: number');

    // Check second violation
    expect(prompt).toContain('Path: root.effects');
    expect(prompt).toContain('Code: NOT_OBJECT');
    expect(prompt).toContain('Field should be an object');
  });

  it('should format violations with optional expected and actual fields', () => {
    const violations: ValidationViolation[] = [
      {
        path: 'root.ok',
        code: 'WRONG_TYPE',
        message: 'Must be boolean',
        expected: 'boolean',
        actual: 'string',
      },
      {
        path: 'root.result',
        code: 'MISSING_FIELD',
        message: 'Field missing',
      },
    ];

    const prompt = buildRepairPrompt(violations);

    expect(prompt).toContain('Expected: boolean');
    expect(prompt).toContain('Got: string');
    // Second violation should not have Expected/Got lines
    const lines = prompt.split('\n');
    const resultIndex = lines.findIndex(l => l.includes('root.result'));
    const nextViolationOrEnd = lines.findIndex(
      (l, i) => i > resultIndex && (l.includes('Path:') || l.includes('INSTRUCTIONS:'))
    );
    const sectionLines = lines.slice(resultIndex, nextViolationOrEnd);
    expect(sectionLines.some(l => l.includes('Expected:'))).toBe(false);
  });

  it('should include instructions for JSON-only response', () => {
    const violations: ValidationViolation[] = [
      {
        path: 'root',
        code: 'NOT_JSON',
        message: 'Response was not valid JSON',
      },
    ];

    const prompt = buildRepairPrompt(violations);

    expect(prompt).toContain('1. Fix ALL violations listed above');
    expect(prompt).toContain('2. Return ONLY valid JSON matching the OUTPUT CONTRACT');
    expect(prompt).toContain('3. Do NOT include markdown code blocks');
    expect(prompt).toContain('4. Do NOT include any explanation or preamble');
  });
});

describe('retry - shouldRetry', () => {
  it('RY2: should return false for KERNEL_MISMATCH', () => {
    const violations: ValidationViolation[] = [
      {
        path: 'root.kernel',
        code: 'KERNEL_MISMATCH',
        message: 'Kernel name does not match request',
      },
    ];

    expect(shouldRetry(violations)).toBe(false);
  });

  it('should return false for OP_MISMATCH', () => {
    const violations: ValidationViolation[] = [
      {
        path: 'root.op',
        code: 'OP_MISMATCH',
        message: 'Operation name does not match request',
      },
    ];

    expect(shouldRetry(violations)).toBe(false);
  });

  it('should return false when any violation is KERNEL_MISMATCH (mixed violations)', () => {
    const violations: ValidationViolation[] = [
      {
        path: 'root.op',
        code: 'KERNEL_MISMATCH',
        message: 'Kernel mismatch',
      },
      {
        path: 'root.result',
        code: 'MISSING_FIELD',
        message: 'Missing field',
      },
    ];

    expect(shouldRetry(violations)).toBe(false);
  });

  it('should return false when any violation is OP_MISMATCH (mixed violations)', () => {
    const violations: ValidationViolation[] = [
      {
        path: 'root.kernel',
        code: 'MISSING_FIELD',
        message: 'Missing field',
      },
      {
        path: 'root.op',
        code: 'OP_MISMATCH',
        message: 'Op mismatch',
      },
    ];

    expect(shouldRetry(violations)).toBe(false);
  });

  it('RY3: should return true for MISSING_FIELD', () => {
    const violations: ValidationViolation[] = [
      {
        path: 'root.kernel',
        code: 'MISSING_FIELD',
        message: 'Required field kernel is missing',
      },
    ];

    expect(shouldRetry(violations)).toBe(true);
  });

  it('should return true for NOT_JSON', () => {
    const violations: ValidationViolation[] = [
      {
        path: 'root',
        code: 'NOT_JSON',
        message: 'Response is not valid JSON',
      },
    ];

    expect(shouldRetry(violations)).toBe(true);
  });

  it('should return true for NOT_OBJECT', () => {
    const violations: ValidationViolation[] = [
      {
        path: 'root',
        code: 'NOT_OBJECT',
        message: 'Root must be an object',
      },
    ];

    expect(shouldRetry(violations)).toBe(true);
  });

  it('should return true for WRONG_TYPE', () => {
    const violations: ValidationViolation[] = [
      {
        path: 'root.ok',
        code: 'WRONG_TYPE',
        message: 'Field ok should be boolean',
      },
    ];

    expect(shouldRetry(violations)).toBe(true);
  });

  it('should return true for INVALID_VALUE', () => {
    const violations: ValidationViolation[] = [
      {
        path: 'root.ok',
        code: 'INVALID_VALUE',
        message: 'Value is not valid',
      },
    ];

    expect(shouldRetry(violations)).toBe(true);
  });

  it('should return true when all violations are fixable', () => {
    const violations: ValidationViolation[] = [
      {
        path: 'root.kernel',
        code: 'MISSING_FIELD',
        message: 'Missing kernel',
      },
      {
        path: 'root.ok',
        code: 'WRONG_TYPE',
        message: 'Wrong type',
      },
      {
        path: 'root.effects',
        code: 'NOT_OBJECT',
        message: 'Not object',
      },
    ];

    expect(shouldRetry(violations)).toBe(true);
  });

  it('should return false with empty violations', () => {
    const violations: ValidationViolation[] = [];
    expect(shouldRetry(violations)).toBe(true);
  });
});

describe('retry - formatViolationsForDisplay', () => {
  it('RY4: should format violations for console display', () => {
    const violations: ValidationViolation[] = [
      {
        path: 'root.kernel',
        code: 'MISSING_FIELD',
        message: 'Required field is missing',
      },
    ];

    const formatted = formatViolationsForDisplay(violations);

    expect(formatted).toContain('[MISSING_FIELD]');
    expect(formatted).toContain('root.kernel');
    expect(formatted).toContain('Required field is missing');
  });

  it('should include expected and actual when present', () => {
    const violations: ValidationViolation[] = [
      {
        path: 'root.ok',
        code: 'WRONG_TYPE',
        message: 'Must be boolean',
        expected: 'boolean',
        actual: 'string',
      },
    ];

    const formatted = formatViolationsForDisplay(violations);

    expect(formatted).toContain('[WRONG_TYPE]');
    expect(formatted).toContain('root.ok');
    expect(formatted).toContain('Must be boolean');
    expect(formatted).toContain('expected boolean');
    expect(formatted).toContain('got string');
  });

  it('should not include expected/actual when not present', () => {
    const violations: ValidationViolation[] = [
      {
        path: 'root.result',
        code: 'MISSING_FIELD',
        message: 'Field missing',
      },
    ];

    const formatted = formatViolationsForDisplay(violations);

    expect(formatted).toContain('[MISSING_FIELD]');
    expect(formatted).not.toContain('expected');
    expect(formatted).not.toContain('got');
  });

  it('should format multiple violations separated by newlines', () => {
    const violations: ValidationViolation[] = [
      {
        path: 'root.kernel',
        code: 'MISSING_FIELD',
        message: 'Missing kernel',
      },
      {
        path: 'root.op',
        code: 'WRONG_TYPE',
        message: 'Op should be string',
        expected: 'string',
        actual: 'number',
      },
      {
        path: 'root.effects',
        code: 'NOT_OBJECT',
        message: 'Effects not array',
      },
    ];

    const formatted = formatViolationsForDisplay(violations);
    const lines = formatted.split('\n');

    expect(lines).toHaveLength(3);
    expect(lines[0]).toContain('[MISSING_FIELD]');
    expect(lines[1]).toContain('[WRONG_TYPE]');
    expect(lines[1]).toContain('expected string');
    expect(lines[2]).toContain('[NOT_OBJECT]');
  });

  it('should handle single violation without trailing newlines', () => {
    const violations: ValidationViolation[] = [
      {
        path: 'root.kernel',
        code: 'NOT_JSON',
        message: 'Not valid JSON',
      },
    ];

    const formatted = formatViolationsForDisplay(violations);

    expect(formatted.endsWith('\n')).toBe(false);
    expect(formatted.split('\n')).toHaveLength(1);
  });
});
