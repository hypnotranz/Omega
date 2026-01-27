/**
 * Golden Test Cases: opr.synthesize.v1
 *
 * Tests code synthesis kernel with various specifications.
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { OprRuntime } from '../../../src/core/opr/runtime';
import { InMemoryReceiptStore } from '../../../src/core/opr/receipts';
import { ScriptedOprAdapter } from '../../../src/core/opr/adapters/scripted';
import { SYNTHESIZE_KERNEL } from '../../../src/core/opr/kernels/synthesize';

describe('Golden: opr.synthesize.v1', () => {
  let receipts: InMemoryReceiptStore;

  beforeEach(() => {
    receipts = new InMemoryReceiptStore();
  });

  const runSynthesize = async (input: object, response: string) => {
    const adapter = new ScriptedOprAdapter({ responses: [response] });
    const runtime = new OprRuntime({
      kernel: SYNTHESIZE_KERNEL,
      adapter,
      receipts,
      budget: { maxAttempts: 3 },
    });
    return runtime.step({ program: input, state: null });
  };

  it('S1: simple function synthesis', async () => {
    const input = {
      spec: 'Write a function that adds two numbers',
      language: 'typescript',
      signature: 'function add(a: number, b: number): number',
    };

    const response = JSON.stringify({
      kernel: 'opr.synthesize.v1',
      op: 'synthesize',
      ok: true,
      result: {
        code: 'function add(a: number, b: number): number {\n  return a + b;\n}',
        tests_passed: true,
        complexity: { time: 'O(1)', space: 'O(1)' },
        edge_cases_handled: [],
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runSynthesize(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.code).toContain('return a + b');
    }
  });

  it('S2: synthesis with examples', async () => {
    const input = {
      spec: 'Reverse a string',
      language: 'javascript',
      examples: [
        { input: '"hello"', output: '"olleh"' },
        { input: '""', output: '""' },
      ],
    };

    const response = JSON.stringify({
      kernel: 'opr.synthesize.v1',
      op: 'synthesize',
      ok: true,
      result: {
        code: 'function reverse(s) {\n  return s.split("").reverse().join("");\n}',
        tests_passed: true,
        complexity: { time: 'O(n)', space: 'O(n)' },
        edge_cases_handled: ['empty string'],
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runSynthesize(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.edge_cases_handled).toContain('empty string');
    }
  });

  it('S3: synthesis with constraints', async () => {
    const input = {
      spec: 'Sort an array of numbers',
      language: 'typescript',
      constraints: { pure: true, no_mutation: true, max_complexity: 'O(n log n)' },
    };

    const response = JSON.stringify({
      kernel: 'opr.synthesize.v1',
      op: 'synthesize',
      ok: true,
      result: {
        code: 'function sort(arr: number[]): number[] {\n  return [...arr].sort((a, b) => a - b);\n}',
        tests_passed: true,
        complexity: { time: 'O(n log n)', space: 'O(n)' },
        edge_cases_handled: ['empty array', 'single element'],
      },
      next_state: null,
      effects: [],
      diagnostics: { notes: ['Used spread operator to avoid mutation'] },
    });

    const result = await runSynthesize(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.code).toContain('[...arr]');
    }
  });

  it('S4: complex algorithm synthesis', async () => {
    const input = {
      spec: 'Implement binary search',
      language: 'typescript',
      signature: 'function binarySearch<T>(arr: T[], target: T): number',
    };

    const response = JSON.stringify({
      kernel: 'opr.synthesize.v1',
      op: 'synthesize',
      ok: true,
      result: {
        code: 'function binarySearch<T>(arr: T[], target: T): number {\n  let left = 0, right = arr.length - 1;\n  while (left <= right) {\n    const mid = Math.floor((left + right) / 2);\n    if (arr[mid] === target) return mid;\n    if (arr[mid] < target) left = mid + 1;\n    else right = mid - 1;\n  }\n  return -1;\n}',
        tests_passed: true,
        complexity: { time: 'O(log n)', space: 'O(1)' },
        edge_cases_handled: ['empty array', 'not found', 'single element'],
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runSynthesize(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.complexity.time).toBe('O(log n)');
    }
  });

  it('S5: Python code synthesis', async () => {
    const input = {
      spec: 'Calculate factorial',
      language: 'python',
      examples: [{ input: '5', output: '120' }],
    };

    const response = JSON.stringify({
      kernel: 'opr.synthesize.v1',
      op: 'synthesize',
      ok: true,
      result: {
        code: 'def factorial(n: int) -> int:\n    if n <= 1:\n        return 1\n    return n * factorial(n - 1)',
        tests_passed: true,
        complexity: { time: 'O(n)', space: 'O(n)' },
        edge_cases_handled: ['n=0', 'n=1'],
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runSynthesize(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.code).toContain('def factorial');
    }
  });

  it('S6: class synthesis', async () => {
    const input = {
      spec: 'Create a Stack class with push, pop, peek, isEmpty',
      language: 'typescript',
    };

    const response = JSON.stringify({
      kernel: 'opr.synthesize.v1',
      op: 'synthesize',
      ok: true,
      result: {
        code: 'class Stack<T> {\n  private items: T[] = [];\n  push(item: T): void { this.items.push(item); }\n  pop(): T | undefined { return this.items.pop(); }\n  peek(): T | undefined { return this.items[this.items.length - 1]; }\n  isEmpty(): boolean { return this.items.length === 0; }\n}',
        tests_passed: true,
        complexity: { time: 'O(1) per operation', space: 'O(n)' },
        edge_cases_handled: ['empty stack'],
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runSynthesize(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.code).toContain('class Stack');
    }
  });

  it('S7: async function synthesis', async () => {
    const input = {
      spec: 'Fetch JSON from URL with timeout',
      language: 'typescript',
    };

    const response = JSON.stringify({
      kernel: 'opr.synthesize.v1',
      op: 'synthesize',
      ok: true,
      result: {
        code: 'async function fetchWithTimeout(url: string, timeout: number): Promise<any> {\n  const controller = new AbortController();\n  const id = setTimeout(() => controller.abort(), timeout);\n  try {\n    const res = await fetch(url, { signal: controller.signal });\n    return await res.json();\n  } finally {\n    clearTimeout(id);\n  }\n}',
        tests_passed: true,
        complexity: { time: 'O(1)', space: 'O(1)' },
        edge_cases_handled: ['timeout', 'network error'],
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runSynthesize(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.code).toContain('async');
    }
  });

  it('S8: higher-order function synthesis', async () => {
    const input = {
      spec: 'Implement debounce function',
      language: 'typescript',
    };

    const response = JSON.stringify({
      kernel: 'opr.synthesize.v1',
      op: 'synthesize',
      ok: true,
      result: {
        code: 'function debounce<T extends (...args: any[]) => any>(fn: T, delay: number): (...args: Parameters<T>) => void {\n  let timeoutId: ReturnType<typeof setTimeout>;\n  return (...args) => {\n    clearTimeout(timeoutId);\n    timeoutId = setTimeout(() => fn(...args), delay);\n  };\n}',
        tests_passed: true,
        complexity: { time: 'O(1)', space: 'O(1)' },
        edge_cases_handled: ['rapid calls', 'cleanup'],
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runSynthesize(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.code).toContain('debounce');
    }
  });

  it('S9: recursive data structure', async () => {
    const input = {
      spec: 'Flatten nested array',
      language: 'typescript',
      examples: [{ input: '[[1, [2, 3]], [4, 5]]', output: '[1, 2, 3, 4, 5]' }],
    };

    const response = JSON.stringify({
      kernel: 'opr.synthesize.v1',
      op: 'synthesize',
      ok: true,
      result: {
        code: 'function flatten<T>(arr: (T | T[])[]): T[] {\n  return arr.reduce<T[]>((acc, val) => \n    acc.concat(Array.isArray(val) ? flatten(val) : val), []);\n}',
        tests_passed: true,
        complexity: { time: 'O(n)', space: 'O(n)' },
        edge_cases_handled: ['empty array', 'deeply nested'],
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runSynthesize(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.code).toContain('flatten');
    }
  });

  it('S10: tests failed response', async () => {
    const input = {
      spec: 'Impossible spec',
      language: 'typescript',
      examples: [{ input: '1', output: '2' }, { input: '1', output: '3' }],
    };

    const response = JSON.stringify({
      kernel: 'opr.synthesize.v1',
      op: 'synthesize',
      ok: true,
      result: {
        code: '// Unable to synthesize: contradictory examples',
        tests_passed: false,
        complexity: null,
        edge_cases_handled: [],
      },
      next_state: null,
      effects: [],
      diagnostics: { errors: ['Contradictory examples: same input with different outputs'] },
    });

    const result = await runSynthesize(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.tests_passed).toBe(false);
    }
  });
});
