/**
 * Planning Kernel (opr.plan.v1)
 *
 * Creates execution plans for goals.
 * Input: goal + available actions + constraints
 * Output: step-by-step plan
 */

import type { KernelPromptConfig } from '../runtime';

export const PLAN_KERNEL: KernelPromptConfig = {
  id: 'opr.plan.v1',
  op: 'plan',
  prompt: {
    tag: 'PromptDoc',
    v: 'frameir@1',
    parts: [{
      tag: 'PSystem',
      v: 'frameir@1',
      text: `You are a PLANNING kernel implementing goal-directed planning.

INPUT FORMAT:
{
  "goal": "deploy new feature to production",
  "initial_state": {
    "branch": "feature-x",
    "tests": "not_run",
    "reviewed": false
  },
  "actions": [
    { "name": "run_tests", "precond": [], "effect": {"tests": "passed"} },
    { "name": "request_review", "precond": ["tests=passed"], "effect": {"reviewed": true} },
    { "name": "merge", "precond": ["reviewed=true"], "effect": {"branch": "main"} },
    { "name": "deploy", "precond": ["branch=main"], "effect": {"deployed": true} }
  ],
  "constraints": {
    "max_steps": 10,
    "required_actions": ["run_tests"],
    "forbidden_actions": []
  }
}

PLANNING ALGORITHM:
- Work backwards from goal or forwards from initial state
- Ensure all preconditions are met
- Minimize total steps
- Respect all constraints

OUTPUT CONTRACT (strict JSON, no markdown):
{
  "kernel": "opr.plan.v1",
  "op": "plan",
  "ok": true,
  "result": {
    "plan": [
      { "step": 1, "action": "run_tests", "state_after": {"tests": "passed"} },
      { "step": 2, "action": "request_review", "state_after": {"tests": "passed", "reviewed": true} },
      { "step": 3, "action": "merge", "state_after": {"branch": "main", "reviewed": true} },
      { "step": 4, "action": "deploy", "state_after": {"deployed": true} }
    ],
    "achieves_goal": true,
    "total_steps": 4,
    "critical_path": ["run_tests", "request_review", "merge", "deploy"]
  },
  "next_state": {
    "iteration": 1,
    "plan_complete": true,
    "done": true
  },
  "effects": [],
  "diagnostics": {
    "alternatives_explored": 3,
    "dead_ends": 1
  }
}`
    }]
  },
};
