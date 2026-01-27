/**
 * Golden Test Cases: opr.review.v1
 *
 * Tests code review kernel with various code snippets and focus areas.
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { OprRuntime } from '../../../src/core/opr/runtime';
import { InMemoryReceiptStore } from '../../../src/core/opr/receipts';
import { ScriptedOprAdapter } from '../../../src/core/opr/adapters/scripted';
import { CODE_REVIEW_KERNEL } from '../../../src/core/opr/kernels/codeReview';

describe('Golden: opr.review.v1', () => {
  let receipts: InMemoryReceiptStore;

  beforeEach(() => {
    receipts = new InMemoryReceiptStore();
  });

  const runReview = async (input: object, response: string) => {
    const adapter = new ScriptedOprAdapter({ responses: [response] });
    const runtime = new OprRuntime({
      kernel: CODE_REVIEW_KERNEL,
      adapter,
      receipts,
      budget: { maxAttempts: 3 },
    });
    return runtime.step({ program: input, state: null });
  };

  it('R1: security vulnerability detection', async () => {
    const input = {
      code: 'const query = `SELECT * FROM users WHERE id = ${req.params.id}`;',
      language: 'javascript',
      focus: ['security'],
    };

    const response = JSON.stringify({
      kernel: 'opr.review.v1',
      op: 'review',
      ok: true,
      result: {
        findings: [
          { category: 'security', severity: 'critical', line: 1, code: 'template literal', message: 'SQL injection vulnerability', suggestion: 'Use parameterized queries' },
        ],
        summary: { critical: 1, high: 0, medium: 0, low: 0 },
        recommendation: 'NEEDS_CHANGES',
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runReview(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.findings[0].severity).toBe('critical');
      expect(result.output.result.recommendation).toBe('NEEDS_CHANGES');
    }
  });

  it('R2: performance issue detection', async () => {
    const input = {
      code: 'for (let i = 0; i < arr.length; i++) { await db.query(arr[i]); }',
      language: 'javascript',
      focus: ['performance'],
    };

    const response = JSON.stringify({
      kernel: 'opr.review.v1',
      op: 'review',
      ok: true,
      result: {
        findings: [
          { category: 'performance', severity: 'high', line: 1, message: 'N+1 query pattern', suggestion: 'Batch queries or use Promise.all' },
        ],
        summary: { critical: 0, high: 1, medium: 0, low: 0 },
        recommendation: 'NEEDS_CHANGES',
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runReview(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.findings[0].category).toBe('performance');
    }
  });

  it('R3: clean code approval', async () => {
    const input = {
      code: 'function add(a: number, b: number): number { return a + b; }',
      language: 'typescript',
    };

    const response = JSON.stringify({
      kernel: 'opr.review.v1',
      op: 'review',
      ok: true,
      result: {
        findings: [],
        summary: { critical: 0, high: 0, medium: 0, low: 0 },
        recommendation: 'APPROVED',
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runReview(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.recommendation).toBe('APPROVED');
    }
  });

  it('R4: bug detection - off by one', async () => {
    const input = {
      code: 'for (let i = 0; i <= arr.length; i++) { console.log(arr[i]); }',
      language: 'javascript',
      focus: ['bugs'],
    };

    const response = JSON.stringify({
      kernel: 'opr.review.v1',
      op: 'review',
      ok: true,
      result: {
        findings: [
          { category: 'bugs', severity: 'high', line: 1, message: 'Off-by-one error: will access arr[arr.length] which is undefined', suggestion: 'Use i < arr.length' },
        ],
        summary: { critical: 0, high: 1, medium: 0, low: 0 },
        recommendation: 'NEEDS_CHANGES',
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runReview(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.findings[0].message).toContain('Off-by-one');
    }
  });

  it('R5: style issues', async () => {
    const input = {
      code: 'function x(a,b,c,d,e,f,g,h,i,j) { return a+b+c+d+e+f+g+h+i+j; }',
      language: 'javascript',
      focus: ['style'],
    };

    const response = JSON.stringify({
      kernel: 'opr.review.v1',
      op: 'review',
      ok: true,
      result: {
        findings: [
          { category: 'style', severity: 'medium', line: 1, message: 'Too many parameters (10)', suggestion: 'Use an options object' },
          { category: 'style', severity: 'low', line: 1, message: 'Non-descriptive function name', suggestion: 'Use meaningful name' },
        ],
        summary: { critical: 0, high: 0, medium: 1, low: 1 },
        recommendation: 'DISCUSS',
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runReview(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.findings).toHaveLength(2);
    }
  });

  it('R6: XSS vulnerability', async () => {
    const input = {
      code: 'element.innerHTML = userInput;',
      language: 'javascript',
      focus: ['security'],
    };

    const response = JSON.stringify({
      kernel: 'opr.review.v1',
      op: 'review',
      ok: true,
      result: {
        findings: [
          { category: 'security', severity: 'critical', line: 1, message: 'XSS vulnerability: unsanitized user input', suggestion: 'Use textContent or sanitize input' },
        ],
        summary: { critical: 1, high: 0, medium: 0, low: 0 },
        recommendation: 'NEEDS_CHANGES',
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runReview(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.summary.critical).toBe(1);
    }
  });

  it('R7: null dereference bug', async () => {
    const input = {
      code: 'function getName(user) { return user.profile.name; }',
      language: 'javascript',
      focus: ['bugs'],
    };

    const response = JSON.stringify({
      kernel: 'opr.review.v1',
      op: 'review',
      ok: true,
      result: {
        findings: [
          { category: 'bugs', severity: 'high', line: 1, message: 'Potential null dereference: user or user.profile may be null', suggestion: 'Add null checks or use optional chaining' },
        ],
        summary: { critical: 0, high: 1, medium: 0, low: 0 },
        recommendation: 'NEEDS_CHANGES',
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runReview(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.findings[0].message).toContain('null');
    }
  });

  it('R8: code duplication', async () => {
    const input = {
      code: 'const a = x * 2 + 1;\nconst b = y * 2 + 1;\nconst c = z * 2 + 1;',
      language: 'javascript',
      focus: ['style'],
    };

    const response = JSON.stringify({
      kernel: 'opr.review.v1',
      op: 'review',
      ok: true,
      result: {
        findings: [
          { category: 'style', severity: 'low', message: 'Repeated pattern: consider extracting to function', suggestion: 'const transform = (v) => v * 2 + 1' },
        ],
        summary: { critical: 0, high: 0, medium: 0, low: 1 },
        recommendation: 'APPROVED',
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runReview(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.recommendation).toBe('APPROVED');
    }
  });

  it('R9: Python code review', async () => {
    const input = {
      code: 'import pickle\ndata = pickle.loads(user_input)',
      language: 'python',
      focus: ['security'],
    };

    const response = JSON.stringify({
      kernel: 'opr.review.v1',
      op: 'review',
      ok: true,
      result: {
        findings: [
          { category: 'security', severity: 'critical', line: 2, message: 'Insecure deserialization: pickle.loads on user input allows arbitrary code execution', suggestion: 'Use json.loads or validate input source' },
        ],
        summary: { critical: 1, high: 0, medium: 0, low: 0 },
        recommendation: 'NEEDS_CHANGES',
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runReview(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.findings[0].severity).toBe('critical');
    }
  });

  it('R10: mixed findings', async () => {
    const input = {
      code: 'eval(req.body.code); for(;;) {}',
      language: 'javascript',
    };

    const response = JSON.stringify({
      kernel: 'opr.review.v1',
      op: 'review',
      ok: true,
      result: {
        findings: [
          { category: 'security', severity: 'critical', line: 1, message: 'eval() on user input' },
          { category: 'bugs', severity: 'high', line: 1, message: 'Infinite loop' },
        ],
        summary: { critical: 1, high: 1, medium: 0, low: 0 },
        recommendation: 'NEEDS_CHANGES',
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runReview(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.findings).toHaveLength(2);
    }
  });
});
