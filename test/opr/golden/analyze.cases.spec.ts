/**
 * Golden Test Cases: opr.analyze.v1
 *
 * Tests code analysis kernel with various Lisp expressions.
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { OprRuntime } from '../../../src/core/opr/runtime';
import { InMemoryReceiptStore } from '../../../src/core/opr/receipts';
import { ScriptedOprAdapter } from '../../../src/core/opr/adapters/scripted';
import { ANALYZE_KERNEL } from '../../../src/core/opr/kernels/analyze';

describe('Golden: opr.analyze.v1', () => {
  let receipts: InMemoryReceiptStore;

  beforeEach(() => {
    receipts = new InMemoryReceiptStore();
  });

  const runAnalyze = async (input: object, response: string) => {
    const adapter = new ScriptedOprAdapter({ responses: [response] });
    const runtime = new OprRuntime({
      kernel: ANALYZE_KERNEL,
      adapter,
      receipts,
      budget: { maxAttempts: 3 },
    });
    return runtime.step({ program: input, state: null });
  };

  it('A1: factorial complexity analysis', async () => {
    const input = {
      expr: '(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))',
      focus: ['complexity', 'tail-recursion'],
    };

    const response = JSON.stringify({
      kernel: 'opr.analyze.v1',
      op: 'analyze',
      ok: true,
      result: {
        complexity: { time: 'O(n)', space: 'O(n)' },
        purity: { pure: true, effects: [] },
        tail_recursive: false,
        bindings: { bound: ['n'], free: ['=', '*', '-'] },
        calls: ['=', '*', '-', 'factorial'],
        patterns: ['recursion', 'conditional'],
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runAnalyze(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.tail_recursive).toBe(false);
      expect(result.output.result.complexity.time).toBe('O(n)');
    }
  });

  it('A2: tail-recursive function', async () => {
    const input = {
      expr: '(define (sum-iter lst acc) (if (null? lst) acc (sum-iter (cdr lst) (+ acc (car lst)))))',
    };

    const response = JSON.stringify({
      kernel: 'opr.analyze.v1',
      op: 'analyze',
      ok: true,
      result: {
        complexity: { time: 'O(n)', space: 'O(1)' },
        purity: { pure: true, effects: [] },
        tail_recursive: true,
        bindings: { bound: ['lst', 'acc'], free: ['null?', 'cdr', 'car', '+'] },
        calls: ['null?', 'cdr', 'car', '+', 'sum-iter'],
        patterns: ['tail-recursion', 'accumulator'],
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runAnalyze(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.tail_recursive).toBe(true);
      expect(result.output.result.complexity.space).toBe('O(1)');
    }
  });

  it('A3: impure function with effects', async () => {
    const input = {
      expr: '(define (log-and-add x y) (begin (display x) (+ x y)))',
      focus: ['purity'],
    };

    const response = JSON.stringify({
      kernel: 'opr.analyze.v1',
      op: 'analyze',
      ok: true,
      result: {
        complexity: { time: 'O(1)', space: 'O(1)' },
        purity: { pure: false, effects: ['io'] },
        tail_recursive: null,
        bindings: { bound: ['x', 'y'], free: ['display', '+'] },
        calls: ['display', '+'],
        patterns: ['sequencing'],
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runAnalyze(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.purity.pure).toBe(false);
      expect(result.output.result.purity.effects).toContain('io');
    }
  });

  it('A4: higher-order function', async () => {
    const input = {
      expr: '(define (compose f g) (lambda (x) (f (g x))))',
    };

    const response = JSON.stringify({
      kernel: 'opr.analyze.v1',
      op: 'analyze',
      ok: true,
      result: {
        complexity: { time: 'O(1)', space: 'O(1)' },
        purity: { pure: true, effects: [] },
        tail_recursive: null,
        bindings: { bound: ['f', 'g', 'x'], free: [] },
        calls: ['f', 'g'],
        patterns: ['higher-order', 'closure'],
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runAnalyze(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.patterns).toContain('higher-order');
    }
  });

  it('A5: map pattern recognition', async () => {
    const input = {
      expr: '(define (map f lst) (if (null? lst) () (cons (f (car lst)) (map f (cdr lst)))))',
    };

    const response = JSON.stringify({
      kernel: 'opr.analyze.v1',
      op: 'analyze',
      ok: true,
      result: {
        complexity: { time: 'O(n)', space: 'O(n)' },
        purity: { pure: true, effects: [] },
        tail_recursive: false,
        bindings: { bound: ['f', 'lst'], free: ['null?', 'cons', 'car', 'cdr'] },
        calls: ['null?', 'cons', 'car', 'cdr', 'f', 'map'],
        patterns: ['map', 'recursion', 'list-construction'],
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runAnalyze(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.patterns).toContain('map');
    }
  });

  it('A6: fold pattern recognition', async () => {
    const input = {
      expr: '(define (fold f init lst) (if (null? lst) init (fold f (f init (car lst)) (cdr lst))))',
    };

    const response = JSON.stringify({
      kernel: 'opr.analyze.v1',
      op: 'analyze',
      ok: true,
      result: {
        complexity: { time: 'O(n)', space: 'O(1)' },
        purity: { pure: true, effects: [] },
        tail_recursive: true,
        bindings: { bound: ['f', 'init', 'lst'], free: ['null?', 'car', 'cdr'] },
        calls: ['null?', 'f', 'car', 'cdr', 'fold'],
        patterns: ['fold', 'tail-recursion', 'accumulator'],
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runAnalyze(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.patterns).toContain('fold');
    }
  });

  it('A7: mutation detection', async () => {
    const input = {
      expr: '(define (mutate-list! lst val) (set-car! lst val))',
      focus: ['purity'],
    };

    const response = JSON.stringify({
      kernel: 'opr.analyze.v1',
      op: 'analyze',
      ok: true,
      result: {
        complexity: { time: 'O(1)', space: 'O(1)' },
        purity: { pure: false, effects: ['mutation'] },
        tail_recursive: null,
        bindings: { bound: ['lst', 'val'], free: ['set-car!'] },
        calls: ['set-car!'],
        patterns: ['mutation'],
      },
      next_state: null,
      effects: [],
      diagnostics: { notes: ['Function name ends with ! indicating mutation'] },
    });

    const result = await runAnalyze(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.purity.effects).toContain('mutation');
    }
  });

  it('A8: simple expression', async () => {
    const input = {
      expr: '(+ 1 2 3)',
    };

    const response = JSON.stringify({
      kernel: 'opr.analyze.v1',
      op: 'analyze',
      ok: true,
      result: {
        complexity: { time: 'O(1)', space: 'O(1)' },
        purity: { pure: true, effects: [] },
        tail_recursive: null,
        bindings: { bound: [], free: ['+'] },
        calls: ['+'],
        patterns: ['arithmetic'],
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runAnalyze(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.bindings.bound).toHaveLength(0);
    }
  });

  it('A9: let binding analysis', async () => {
    const input = {
      expr: '(let ((x 10) (y 20)) (+ x y))',
    };

    const response = JSON.stringify({
      kernel: 'opr.analyze.v1',
      op: 'analyze',
      ok: true,
      result: {
        complexity: { time: 'O(1)', space: 'O(1)' },
        purity: { pure: true, effects: [] },
        tail_recursive: null,
        bindings: { bound: ['x', 'y'], free: ['+'] },
        calls: ['+'],
        patterns: ['let-binding'],
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runAnalyze(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.bindings.bound).toContain('x');
      expect(result.output.result.bindings.bound).toContain('y');
    }
  });

  it('A10: continuation usage', async () => {
    const input = {
      expr: '(call/cc (lambda (k) (k 42)))',
      focus: ['purity'],
    };

    const response = JSON.stringify({
      kernel: 'opr.analyze.v1',
      op: 'analyze',
      ok: true,
      result: {
        complexity: { time: 'O(1)', space: 'O(1)' },
        purity: { pure: false, effects: ['control-flow'] },
        tail_recursive: null,
        bindings: { bound: ['k'], free: ['call/cc'] },
        calls: ['call/cc', 'k'],
        patterns: ['continuation', 'non-local-exit'],
      },
      next_state: null,
      effects: [],
      diagnostics: { notes: ['call/cc introduces control effects'] },
    });

    const result = await runAnalyze(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.patterns).toContain('continuation');
    }
  });
});
