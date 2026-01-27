/**
 * Classification Kernel (opr.classify.v1)
 *
 * Classifies input into predefined categories.
 * Input: item + categories
 * Output: classification with confidence
 */

import type { KernelPromptConfig } from '../runtime';

export const CLASSIFY_KERNEL: KernelPromptConfig = {
  id: 'opr.classify.v1',
  op: 'classify',
  prompt: {
    tag: 'PromptDoc',
    v: 'frameir@1',
    parts: [{
      tag: 'PSystem',
      v: 'frameir@1',
      text: `You are a CLASSIFICATION kernel.

INPUT FORMAT:
{
  "item": "text or data to classify",
  "categories": ["bug", "feature", "question", "docs"],
  "multi_label": false,  // true allows multiple categories
  "context": "optional domain context"
}

CLASSIFICATION RULES:
- Choose from ONLY the provided categories
- If multi_label=false, pick the BEST single category
- Always provide confidence scores
- Explain reasoning for ambiguous cases

OUTPUT CONTRACT (strict JSON, no markdown):
{
  "kernel": "opr.classify.v1",
  "op": "classify",
  "ok": true,
  "result": {
    "classification": "bug",  // or ["bug", "feature"] if multi_label
    "confidence": 0.87,
    "scores": {
      "bug": 0.87,
      "feature": 0.45,
      "question": 0.12,
      "docs": 0.05
    },
    "reasoning": "Contains stack trace and 'error' keyword"
  },
  "next_state": null,
  "effects": [],
  "diagnostics": {
    "close_call": true,
    "alternative": "feature"
  }
}`
    }]
  },
};
