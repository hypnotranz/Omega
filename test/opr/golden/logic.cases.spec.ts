/**
 * Golden Test Cases: opr.logic.v1
 *
 * Tests logic inference kernel with horn clauses and facts.
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { OprRuntime } from '../../../src/core/opr/runtime';
import { InMemoryReceiptStore } from '../../../src/core/opr/receipts';
import { ScriptedOprAdapter } from '../../../src/core/opr/adapters/scripted';
import { LOGIC_KERNEL } from '../../../src/core/opr/kernels/logic';

describe('Golden: opr.logic.v1', () => {
  let receipts: InMemoryReceiptStore;

  beforeEach(() => {
    receipts = new InMemoryReceiptStore();
  });

  const runLogic = async (input: object, response: string) => {
    const adapter = new ScriptedOprAdapter({ responses: [response] });
    const runtime = new OprRuntime({
      kernel: LOGIC_KERNEL,
      adapter,
      receipts,
      budget: { maxAttempts: 3 },
    });
    return runtime.step({ program: input, state: null });
  };

  it('L1: classic Socrates syllogism', async () => {
    const input = {
      rules: ['mortal(X) :- human(X)'],
      facts: ['human(socrates)'],
    };

    const response = JSON.stringify({
      kernel: 'opr.logic.v1',
      op: 'infer',
      ok: true,
      result: { delta: ['mortal(socrates)'] },
      next_state: { iteration: 1, derived: ['mortal(socrates)'], done: false },
      effects: [],
      diagnostics: { rules_applied: ['mortal(X) :- human(X)'], bindings: [{ rule: 'mortal(X) :- human(X)', unifier: { X: 'socrates' } }] },
    });

    const result = await runLogic(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.delta).toContain('mortal(socrates)');
    }
  });

  it('L2: transitive closure', async () => {
    const input = {
      rules: ['ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z)', 'ancestor(X, Y) :- parent(X, Y)'],
      facts: ['parent(alice, bob)', 'parent(bob, charlie)'],
    };

    const response = JSON.stringify({
      kernel: 'opr.logic.v1',
      op: 'infer',
      ok: true,
      result: { delta: ['ancestor(alice, bob)', 'ancestor(bob, charlie)'] },
      next_state: { iteration: 1, derived: ['ancestor(alice, bob)', 'ancestor(bob, charlie)'], done: false },
      effects: [],
      diagnostics: { rules_applied: ['ancestor(X, Y) :- parent(X, Y)'] },
    });

    const result = await runLogic(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.delta).toContain('ancestor(alice, bob)');
    }
  });

  it('L3: fixpoint reached (empty delta)', async () => {
    const input = {
      rules: ['mortal(X) :- human(X)'],
      facts: ['human(socrates)', 'mortal(socrates)'],
    };

    const response = JSON.stringify({
      kernel: 'opr.logic.v1',
      op: 'infer',
      ok: true,
      result: { delta: [] },
      next_state: { iteration: 1, derived: [], done: true },
      effects: [],
      diagnostics: { notes: ['All derivable facts already known'] },
    });

    const result = await runLogic(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.next_state.done).toBe(true);
    }
  });

  it('L4: multiple rules fire', async () => {
    const input = {
      rules: ['can_fly(X) :- bird(X)', 'can_swim(X) :- fish(X)'],
      facts: ['bird(sparrow)', 'fish(salmon)'],
    };

    const response = JSON.stringify({
      kernel: 'opr.logic.v1',
      op: 'infer',
      ok: true,
      result: { delta: ['can_fly(sparrow)', 'can_swim(salmon)'] },
      next_state: { iteration: 1, derived: ['can_fly(sparrow)', 'can_swim(salmon)'], done: false },
      effects: [],
      diagnostics: { rules_applied: ['can_fly(X) :- bird(X)', 'can_swim(X) :- fish(X)'] },
    });

    const result = await runLogic(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.delta).toHaveLength(2);
    }
  });

  it('L5: no applicable rules', async () => {
    const input = {
      rules: ['mortal(X) :- human(X)'],
      facts: ['robot(r2d2)'],
    };

    const response = JSON.stringify({
      kernel: 'opr.logic.v1',
      op: 'infer',
      ok: true,
      result: { delta: [] },
      next_state: { iteration: 1, derived: [], done: true },
      effects: [],
      diagnostics: { notes: ['No rules matched any facts'] },
    });

    const result = await runLogic(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.delta).toHaveLength(0);
    }
  });

  it('L6: multi-premise rule', async () => {
    const input = {
      rules: ['grandparent(X, Z) :- parent(X, Y), parent(Y, Z)'],
      facts: ['parent(alice, bob)', 'parent(bob, charlie)'],
    };

    const response = JSON.stringify({
      kernel: 'opr.logic.v1',
      op: 'infer',
      ok: true,
      result: { delta: ['grandparent(alice, charlie)'] },
      next_state: { iteration: 1, derived: ['grandparent(alice, charlie)'], done: false },
      effects: [],
      diagnostics: { bindings: [{ rule: 'grandparent(X, Z) :- parent(X, Y), parent(Y, Z)', unifier: { X: 'alice', Y: 'bob', Z: 'charlie' } }] },
    });

    const result = await runLogic(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.delta).toContain('grandparent(alice, charlie)');
    }
  });

  it('L7: run to fixpoint multi-iteration', async () => {
    const input = {
      rules: ['reachable(X, Z) :- reachable(X, Y), edge(Y, Z)', 'reachable(X, Y) :- edge(X, Y)'],
      facts: ['edge(a, b)', 'edge(b, c)', 'edge(c, d)'],
    };

    const response1 = JSON.stringify({
      kernel: 'opr.logic.v1',
      op: 'infer',
      ok: true,
      result: { delta: ['reachable(a, b)', 'reachable(b, c)', 'reachable(c, d)'] },
      next_state: { iteration: 1, derived: ['reachable(a, b)', 'reachable(b, c)', 'reachable(c, d)'], done: false },
      effects: [],
      diagnostics: {},
    });

    const response2 = JSON.stringify({
      kernel: 'opr.logic.v1',
      op: 'infer',
      ok: true,
      result: { delta: ['reachable(a, c)', 'reachable(b, d)'] },
      next_state: { iteration: 2, derived: ['reachable(a, b)', 'reachable(b, c)', 'reachable(c, d)', 'reachable(a, c)', 'reachable(b, d)'], done: false },
      effects: [],
      diagnostics: {},
    });

    const response3 = JSON.stringify({
      kernel: 'opr.logic.v1',
      op: 'infer',
      ok: true,
      result: { delta: ['reachable(a, d)'] },
      next_state: { iteration: 3, derived: ['reachable(a, b)', 'reachable(b, c)', 'reachable(c, d)', 'reachable(a, c)', 'reachable(b, d)', 'reachable(a, d)'], done: false },
      effects: [],
      diagnostics: {},
    });

    const response4 = JSON.stringify({
      kernel: 'opr.logic.v1',
      op: 'infer',
      ok: true,
      result: { delta: [] },
      next_state: { iteration: 4, derived: ['reachable(a, b)', 'reachable(b, c)', 'reachable(c, d)', 'reachable(a, c)', 'reachable(b, d)', 'reachable(a, d)'], done: true },
      effects: [],
      diagnostics: {},
    });

    const adapter = new ScriptedOprAdapter({ responses: [response1, response2, response3, response4] });
    const runtime = new OprRuntime({
      kernel: LOGIC_KERNEL,
      adapter,
      receipts,
      budget: { maxAttempts: 3 },
    });

    const result = await runtime.runToFixpoint({ program: input, state: null });
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.iterations).toBe(4);
      expect(result.finalState.done).toBe(true);
    }
  });

  it('L8: negation as failure', async () => {
    const input = {
      rules: ['can_fly(X) :- bird(X), not(penguin(X))'],
      facts: ['bird(sparrow)', 'bird(penguin1)', 'penguin(penguin1)'],
    };

    const response = JSON.stringify({
      kernel: 'opr.logic.v1',
      op: 'infer',
      ok: true,
      result: { delta: ['can_fly(sparrow)'] },
      next_state: { iteration: 1, derived: ['can_fly(sparrow)'], done: false },
      effects: [],
      diagnostics: { notes: ['penguin1 excluded by negation'] },
    });

    const result = await runLogic(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.delta).toContain('can_fly(sparrow)');
      expect(result.output.result.delta).not.toContain('can_fly(penguin1)');
    }
  });

  it('L9: empty facts', async () => {
    const input = {
      rules: ['mortal(X) :- human(X)'],
      facts: [],
    };

    const response = JSON.stringify({
      kernel: 'opr.logic.v1',
      op: 'infer',
      ok: true,
      result: { delta: [] },
      next_state: { iteration: 1, derived: [], done: true },
      effects: [],
      diagnostics: {},
    });

    const result = await runLogic(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.next_state.done).toBe(true);
    }
  });

  it('L10: sibling relationship', async () => {
    const input = {
      rules: ['sibling(X, Y) :- parent(P, X), parent(P, Y), X \\= Y'],
      facts: ['parent(alice, bob)', 'parent(alice, carol)'],
    };

    const response = JSON.stringify({
      kernel: 'opr.logic.v1',
      op: 'infer',
      ok: true,
      result: { delta: ['sibling(bob, carol)', 'sibling(carol, bob)'] },
      next_state: { iteration: 1, derived: ['sibling(bob, carol)', 'sibling(carol, bob)'], done: false },
      effects: [],
      diagnostics: {},
    });

    const result = await runLogic(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.delta).toContain('sibling(bob, carol)');
    }
  });
});
