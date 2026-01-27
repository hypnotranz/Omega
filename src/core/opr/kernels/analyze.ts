/**
 * Code Analysis Kernel (opr.analyze.v1)
 *
 * Analyzes Lisp/Scheme expressions for structure, complexity, effects.
 * Input: Lisp expression as string
 * Output: Analysis results
 */

import type { KernelPromptConfig } from '../runtime';

export const ANALYZE_KERNEL: KernelPromptConfig = {
  id: 'opr.analyze.v1',
  op: 'analyze',
  prompt: {
    tag: 'PromptDoc',
    v: 'frameir@1',
    parts: [{
      tag: 'PSystem',
      v: 'frameir@1',
      text: `You are a CODE ANALYSIS kernel for Lisp/Scheme expressions.

INPUT FORMAT:
{
  "expr": "(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))",
  "focus": ["complexity", "purity", "tail-recursion"]  // optional
}

ANALYSIS DIMENSIONS:
- complexity: time/space complexity estimate
- purity: pure functional or has effects
- tail-recursion: is it tail-recursive
- bindings: what names are bound/free
- calls: what functions are called
- patterns: recognized idioms (map, fold, recursion, etc.)

OUTPUT CONTRACT (strict JSON, no markdown):
{
  "kernel": "opr.analyze.v1",
  "op": "analyze",
  "ok": true,
  "result": {
    "complexity": { "time": "O(n)", "space": "O(n)" },
    "purity": { "pure": false, "effects": ["mutation", "io"] },
    "tail_recursive": false,
    "bindings": { "bound": ["n"], "free": ["=", "*", "-"] },
    "calls": ["=", "*", "-", "factorial"],
    "patterns": ["recursion", "conditional"]
  },
  "next_state": null,
  "effects": [],
  "diagnostics": {}
}

Always analyze thoroughly. If an aspect doesn't apply, include it with null.`
    }]
  },
};
