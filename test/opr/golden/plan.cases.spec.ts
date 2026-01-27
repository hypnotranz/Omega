/**
 * Golden Test Cases: opr.plan.v1
 *
 * Tests planning kernel with various goals and action sets.
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { OprRuntime } from '../../../src/core/opr/runtime';
import { InMemoryReceiptStore } from '../../../src/core/opr/receipts';
import { ScriptedOprAdapter } from '../../../src/core/opr/adapters/scripted';
import { PLAN_KERNEL } from '../../../src/core/opr/kernels/plan';

describe('Golden: opr.plan.v1', () => {
  let receipts: InMemoryReceiptStore;

  beforeEach(() => {
    receipts = new InMemoryReceiptStore();
  });

  const runPlan = async (input: object, response: string) => {
    const adapter = new ScriptedOprAdapter({ responses: [response] });
    const runtime = new OprRuntime({
      kernel: PLAN_KERNEL,
      adapter,
      receipts,
      budget: { maxAttempts: 3 },
    });
    return runtime.step({ program: input, state: null });
  };

  it('P1: simple deployment plan', async () => {
    const input = {
      goal: 'deploy to production',
      initial_state: { tested: false, reviewed: false, deployed: false },
      actions: [
        { name: 'run_tests', precond: [], effect: { tested: true } },
        { name: 'code_review', precond: ['tested=true'], effect: { reviewed: true } },
        { name: 'deploy', precond: ['reviewed=true'], effect: { deployed: true } },
      ],
    };

    const response = JSON.stringify({
      kernel: 'opr.plan.v1',
      op: 'plan',
      ok: true,
      result: {
        plan: [
          { step: 1, action: 'run_tests', state_after: { tested: true } },
          { step: 2, action: 'code_review', state_after: { tested: true, reviewed: true } },
          { step: 3, action: 'deploy', state_after: { tested: true, reviewed: true, deployed: true } },
        ],
        achieves_goal: true,
        total_steps: 3,
        critical_path: ['run_tests', 'code_review', 'deploy'],
      },
      next_state: { iteration: 1, done: true },
      effects: [],
      diagnostics: {},
    });

    const result = await runPlan(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.plan).toHaveLength(3);
      expect(result.output.result.achieves_goal).toBe(true);
    }
  });

  it('P2: plan with parallel actions', async () => {
    const input = {
      goal: 'prepare release',
      initial_state: {},
      actions: [
        { name: 'build_frontend', precond: [], effect: { frontend: true } },
        { name: 'build_backend', precond: [], effect: { backend: true } },
        { name: 'package', precond: ['frontend=true', 'backend=true'], effect: { packaged: true } },
      ],
    };

    const response = JSON.stringify({
      kernel: 'opr.plan.v1',
      op: 'plan',
      ok: true,
      result: {
        plan: [
          { step: 1, action: 'build_frontend', state_after: { frontend: true } },
          { step: 1, action: 'build_backend', state_after: { backend: true } },
          { step: 2, action: 'package', state_after: { frontend: true, backend: true, packaged: true } },
        ],
        achieves_goal: true,
        total_steps: 2,
        critical_path: ['build_frontend', 'package'],
      },
      next_state: { iteration: 1, done: true },
      effects: [],
      diagnostics: { parallel_steps: [1] },
    });

    const result = await runPlan(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.total_steps).toBe(2);
    }
  });

  it('P3: impossible goal', async () => {
    const input = {
      goal: 'fly to moon',
      initial_state: { has_rocket: false },
      actions: [{ name: 'drive_car', precond: [], effect: { moved: true } }],
    };

    const response = JSON.stringify({
      kernel: 'opr.plan.v1',
      op: 'plan',
      ok: true,
      result: {
        plan: [],
        achieves_goal: false,
        total_steps: 0,
        critical_path: [],
      },
      next_state: { iteration: 1, done: true },
      effects: [],
      diagnostics: { notes: ['No actions can achieve goal'] },
    });

    const result = await runPlan(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.achieves_goal).toBe(false);
    }
  });

  it('P4: plan with constraints', async () => {
    const input = {
      goal: 'complete task',
      initial_state: {},
      actions: [
        { name: 'fast_way', precond: [], effect: { done: true } },
        { name: 'slow_way', precond: [], effect: { done: true } },
      ],
      constraints: { forbidden_actions: ['fast_way'] },
    };

    const response = JSON.stringify({
      kernel: 'opr.plan.v1',
      op: 'plan',
      ok: true,
      result: {
        plan: [{ step: 1, action: 'slow_way', state_after: { done: true } }],
        achieves_goal: true,
        total_steps: 1,
        critical_path: ['slow_way'],
      },
      next_state: { iteration: 1, done: true },
      effects: [],
      diagnostics: { notes: ['fast_way excluded by constraint'] },
    });

    const result = await runPlan(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.plan[0].action).toBe('slow_way');
    }
  });

  it('P5: multi-iteration planning', async () => {
    const input = { goal: 'complex goal', initial_state: {}, actions: [] };

    const response1 = JSON.stringify({
      kernel: 'opr.plan.v1',
      op: 'plan',
      ok: true,
      result: { partial_plan: ['step1'], analysis: 'needs more work' },
      next_state: { iteration: 1, done: false },
      effects: [],
      diagnostics: {},
    });

    const response2 = JSON.stringify({
      kernel: 'opr.plan.v1',
      op: 'plan',
      ok: true,
      result: { plan: [{ step: 1, action: 'final', state_after: {} }], achieves_goal: true, total_steps: 1 },
      next_state: { iteration: 2, done: true },
      effects: [],
      diagnostics: {},
    });

    const adapter = new ScriptedOprAdapter({ responses: [response1, response2] });
    const runtime = new OprRuntime({
      kernel: PLAN_KERNEL,
      adapter,
      receipts,
      budget: { maxAttempts: 3 },
    });

    const result = await runtime.runToFixpoint({ program: input, state: null });
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.iterations).toBe(2);
    }
  });

  it('P6: required actions constraint', async () => {
    const input = {
      goal: 'secure deploy',
      initial_state: {},
      actions: [
        { name: 'security_scan', precond: [], effect: { scanned: true } },
        { name: 'deploy', precond: [], effect: { deployed: true } },
      ],
      constraints: { required_actions: ['security_scan'] },
    };

    const response = JSON.stringify({
      kernel: 'opr.plan.v1',
      op: 'plan',
      ok: true,
      result: {
        plan: [
          { step: 1, action: 'security_scan', state_after: { scanned: true } },
          { step: 2, action: 'deploy', state_after: { scanned: true, deployed: true } },
        ],
        achieves_goal: true,
        total_steps: 2,
      },
      next_state: { iteration: 1, done: true },
      effects: [],
      diagnostics: {},
    });

    const result = await runPlan(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      const actions = result.output.result.plan.map((s: any) => s.action);
      expect(actions).toContain('security_scan');
    }
  });

  it('P7: max steps constraint', async () => {
    const input = {
      goal: 'finish',
      initial_state: {},
      actions: [
        { name: 'a', precond: [], effect: { a: true } },
        { name: 'b', precond: ['a=true'], effect: { b: true } },
        { name: 'c', precond: ['b=true'], effect: { c: true } },
      ],
      constraints: { max_steps: 2 },
    };

    const response = JSON.stringify({
      kernel: 'opr.plan.v1',
      op: 'plan',
      ok: true,
      result: {
        plan: [
          { step: 1, action: 'a', state_after: { a: true } },
          { step: 2, action: 'b', state_after: { a: true, b: true } },
        ],
        achieves_goal: false,
        total_steps: 2,
      },
      next_state: { iteration: 1, done: true },
      effects: [],
      diagnostics: { notes: ['Stopped at max_steps=2'] },
    });

    const result = await runPlan(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.total_steps).toBe(2);
    }
  });

  it('P8: empty actions list', async () => {
    const input = { goal: 'do something', initial_state: {}, actions: [] };

    const response = JSON.stringify({
      kernel: 'opr.plan.v1',
      op: 'plan',
      ok: true,
      result: { plan: [], achieves_goal: false, total_steps: 0 },
      next_state: { iteration: 1, done: true },
      effects: [],
      diagnostics: { notes: ['No actions available'] },
    });

    const result = await runPlan(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.plan).toHaveLength(0);
    }
  });

  it('P9: goal already achieved', async () => {
    const input = {
      goal: 'be deployed',
      initial_state: { deployed: true },
      actions: [{ name: 'deploy', precond: [], effect: { deployed: true } }],
    };

    const response = JSON.stringify({
      kernel: 'opr.plan.v1',
      op: 'plan',
      ok: true,
      result: { plan: [], achieves_goal: true, total_steps: 0 },
      next_state: { iteration: 1, done: true },
      effects: [],
      diagnostics: { notes: ['Goal already satisfied'] },
    });

    const result = await runPlan(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.achieves_goal).toBe(true);
      expect(result.output.result.total_steps).toBe(0);
    }
  });

  it('P10: cyclic dependency detection', async () => {
    const input = {
      goal: 'finish',
      initial_state: {},
      actions: [
        { name: 'a', precond: ['b=true'], effect: { a: true } },
        { name: 'b', precond: ['a=true'], effect: { b: true } },
      ],
    };

    const response = JSON.stringify({
      kernel: 'opr.plan.v1',
      op: 'plan',
      ok: true,
      result: { plan: [], achieves_goal: false, total_steps: 0 },
      next_state: { iteration: 1, done: true },
      effects: [],
      diagnostics: { errors: ['Cyclic dependency detected'] },
    });

    const result = await runPlan(input, response);
    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.diagnostics.errors).toContain('Cyclic dependency detected');
    }
  });
});
