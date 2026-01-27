/**
 * Logic Inference Kernel (opr.logic.v1)
 *
 * Performs forward-chaining inference over horn clauses.
 * Input: rules (horn clauses) + facts
 * Output: newly derived facts
 */

import type { KernelPromptConfig } from '../runtime';

export const LOGIC_KERNEL: KernelPromptConfig = {
  id: 'opr.logic.v1',
  op: 'infer',
  prompt: {
    tag: 'PromptDoc',
    v: 'frameir@1',
    parts: [{
      tag: 'PSystem',
      v: 'frameir@1',
      text: `You are a LOGIC INFERENCE kernel implementing forward-chaining deduction.

INPUT FORMAT:
{
  "rules": ["head :- body1, body2", ...],  // Horn clauses
  "facts": ["predicate(args)", ...]         // Ground facts
}

SEMANTICS:
- Rules are horn clauses: "conclusion :- premise1, premise2"
- Variables are UPPERCASE: X, Y, Person
- Constants are lowercase: socrates, 42, true
- Apply all rules whose premises are satisfied by current facts
- Derive new facts by unifying variables with constants

OUTPUT CONTRACT (strict JSON, no markdown):
{
  "kernel": "opr.logic.v1",
  "op": "infer",
  "ok": true,
  "result": {
    "delta": ["newly derived facts this iteration"]
  },
  "next_state": {
    "iteration": <int>,
    "derived": ["all derived facts so far"],
    "done": <true if delta is empty, false otherwise>
  },
  "effects": [],
  "diagnostics": {
    "rules_applied": ["which rules fired"],
    "bindings": [{"rule": "...", "unifier": {...}}]
  }
}

TERMINATION: Set done=true when no new facts can be derived (fixpoint).`
    }]
  },
};
