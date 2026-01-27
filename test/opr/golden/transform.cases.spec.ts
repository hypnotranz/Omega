/**
 * Golden Test Cases: opr.transform.v1
 *
 * Tests code transformation kernel with various transformations.
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { OprRuntime } from '../../../src/core/opr/runtime';
import { InMemoryReceiptStore } from '../../../src/core/opr/receipts';
import { ScriptedOprAdapter } from '../../../src/core/opr/adapters/scripted';
import { TRANSFORM_KERNEL } from '../../../src/core/opr/kernels/transform';

describe('Golden: opr.transform.v1', () => {
  let receipts: InMemoryReceiptStore;

  beforeEach(() => {
    receipts = new InMemoryReceiptStore();
  });

  const runTransform = async (input: object, response: string) => {
    const adapter = new ScriptedOprAdapter({ responses: [response] });
    const runtime = new OprRuntime({
      kernel: TRANSFORM_KERNEL,
      adapter,
      receipts,
      budget: { maxAttempts: 3 },
    });
    return runtime.step({ program: input, state: null });
  };

  it('T1: JavaScript to TypeScript', async () => {
    const input = {
      code: 'function add(a, b) { return a + b; }',
      from: 'javascript',
      to: 'typescript',
    };

    const response = JSON.stringify({
      kernel: 'opr.transform.v1',
      op: 'transform',
      ok: true,
      result: {
        code: 'function add(a: number, b: number): number { return a + b; }',
        changes: [
          { type: 'add_type', location: 'parameter a', description: 'Added number type' },
          { type: 'add_type', location: 'parameter b', description: 'Added number type' },
          { type: 'add_type', location: 'return', description: 'Added return type' },
        ],
        warnings: [],
      },
      next_state: null,
      effects: [],
      diagnostics: { confidence: 0.95 },
    });

    const result = await runTransform(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.code).toContain(': number');
    }
  });

  it('T2: callback to async/await', async () => {
    const input = {
      code: 'fs.readFile(path, (err, data) => { if (err) reject(err); else resolve(data); });',
      from: 'javascript',
      to: 'async-await',
    };

    const response = JSON.stringify({
      kernel: 'opr.transform.v1',
      op: 'transform',
      ok: true,
      result: {
        code: 'const data = await fs.promises.readFile(path);',
        changes: [{ type: 'style', description: 'Converted callback to async/await' }],
        warnings: [],
      },
      next_state: null,
      effects: [],
      diagnostics: { confidence: 0.92 },
    });

    const result = await runTransform(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.code).toContain('await');
    }
  });

  it('T3: class to functional', async () => {
    const input = {
      code: 'class Counter { constructor() { this.count = 0; } increment() { this.count++; } }',
      from: 'javascript',
      to: 'functional',
    };

    const response = JSON.stringify({
      kernel: 'opr.transform.v1',
      op: 'transform',
      ok: true,
      result: {
        code: 'const createCounter = () => { let count = 0; return { increment: () => ++count, getCount: () => count }; };',
        changes: [{ type: 'paradigm', description: 'Converted class to closure-based module' }],
        warnings: ['State is now encapsulated differently'],
      },
      next_state: null,
      effects: [],
      diagnostics: { confidence: 0.85 },
    });

    const result = await runTransform(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.code).toContain('createCounter');
    }
  });

  it('T4: extract function refactoring', async () => {
    const input = {
      code: 'function process(items) { const filtered = items.filter(x => x > 0); const sorted = filtered.sort((a, b) => a - b); return sorted; }',
      from: 'javascript',
      to: 'refactor:extract-function',
      options: { extract: 'filter and sort logic' },
    };

    const response = JSON.stringify({
      kernel: 'opr.transform.v1',
      op: 'transform',
      ok: true,
      result: {
        code: 'const filterAndSort = (items) => items.filter(x => x > 0).sort((a, b) => a - b);\nfunction process(items) { return filterAndSort(items); }',
        changes: [{ type: 'extract', from: 'process', to: 'filterAndSort' }],
        warnings: [],
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runTransform(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.code).toContain('filterAndSort');
    }
  });

  it('T5: rename variable refactoring', async () => {
    const input = {
      code: 'const x = 10; const y = x * 2; console.log(y);',
      from: 'javascript',
      to: 'refactor:rename',
      options: { from: 'x', to: 'baseValue' },
    };

    const response = JSON.stringify({
      kernel: 'opr.transform.v1',
      op: 'transform',
      ok: true,
      result: {
        code: 'const baseValue = 10; const y = baseValue * 2; console.log(y);',
        changes: [{ type: 'rename', from: 'x', to: 'baseValue', occurrences: 2 }],
        warnings: [],
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runTransform(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.code).toContain('baseValue');
      expect(result.output.result.code).not.toContain('const x');
    }
  });

  it('T6: Python 2 to Python 3', async () => {
    const input = {
      code: 'print "hello"\nraw_input("Enter: ")',
      from: 'python2',
      to: 'python3',
    };

    const response = JSON.stringify({
      kernel: 'opr.transform.v1',
      op: 'transform',
      ok: true,
      result: {
        code: 'print("hello")\ninput("Enter: ")',
        changes: [
          { type: 'syntax', line: 1, description: 'print statement to function' },
          { type: 'rename', line: 2, from: 'raw_input', to: 'input' },
        ],
        warnings: [],
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runTransform(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.code).toContain('print("hello")');
    }
  });

  it('T7: inline variable', async () => {
    const input = {
      code: 'const temp = getValue(); return temp;',
      from: 'javascript',
      to: 'refactor:inline',
      options: { variable: 'temp' },
    };

    const response = JSON.stringify({
      kernel: 'opr.transform.v1',
      op: 'transform',
      ok: true,
      result: {
        code: 'return getValue();',
        changes: [{ type: 'inline', variable: 'temp' }],
        warnings: [],
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runTransform(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.code).not.toContain('temp');
    }
  });

  it('T8: add error handling', async () => {
    const input = {
      code: 'const data = JSON.parse(input);',
      from: 'javascript',
      to: 'add-error-handling',
    };

    const response = JSON.stringify({
      kernel: 'opr.transform.v1',
      op: 'transform',
      ok: true,
      result: {
        code: 'let data;\ntry {\n  data = JSON.parse(input);\n} catch (e) {\n  throw new Error(`Invalid JSON: ${e.message}`);\n}',
        changes: [{ type: 'add', description: 'Added try-catch for JSON.parse' }],
        warnings: [],
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runTransform(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.code).toContain('try');
      expect(result.output.result.code).toContain('catch');
    }
  });

  it('T9: preserve comments', async () => {
    const input = {
      code: '// Calculate sum\nfunction sum(a, b) { return a + b; }',
      from: 'javascript',
      to: 'typescript',
      options: { preserve_comments: true },
    };

    const response = JSON.stringify({
      kernel: 'opr.transform.v1',
      op: 'transform',
      ok: true,
      result: {
        code: '// Calculate sum\nfunction sum(a: number, b: number): number { return a + b; }',
        changes: [{ type: 'add_type', description: 'Added types' }],
        warnings: [],
      },
      next_state: null,
      effects: [],
      diagnostics: {},
    });

    const result = await runTransform(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.code).toContain('// Calculate sum');
    }
  });

  it('T10: semantic warning on complex transform', async () => {
    const input = {
      code: 'setTimeout(() => { this.update(); }, 1000);',
      from: 'javascript',
      to: 'arrow-to-function',
    };

    const response = JSON.stringify({
      kernel: 'opr.transform.v1',
      op: 'transform',
      ok: true,
      result: {
        code: 'setTimeout(function() { this.update(); }, 1000);',
        changes: [{ type: 'syntax', description: 'Arrow function to regular function' }],
        warnings: ['Semantic change: `this` binding will be different in regular function'],
      },
      next_state: null,
      effects: [],
      diagnostics: { manual_review_needed: ['line 1: this binding change'] },
    });

    const result = await runTransform(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.warnings.length).toBeGreaterThan(0);
      expect(result.output.diagnostics.manual_review_needed).toBeDefined();
    }
  });

  it('T11: minify code', async () => {
    const input = {
      code: 'function longFunctionName(parameter) {\n  const result = parameter * 2;\n  return result;\n}',
      from: 'javascript',
      to: 'minified',
    };

    const response = JSON.stringify({
      kernel: 'opr.transform.v1',
      op: 'transform',
      ok: true,
      result: {
        code: 'function a(b){return b*2}',
        changes: [
          { type: 'minify', description: 'Removed whitespace' },
          { type: 'rename', from: 'longFunctionName', to: 'a' },
          { type: 'inline', variable: 'result' },
        ],
        warnings: [],
      },
      next_state: null,
      effects: [],
      diagnostics: { size_reduction: '60%' },
    });

    const result = await runTransform(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.code.length).toBeLessThan(input.code.length);
    }
  });
});
