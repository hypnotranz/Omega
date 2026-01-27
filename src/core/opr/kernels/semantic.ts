/**
 * Semantic Understanding Kernel (opr.semantic.v1)
 *
 * Extracts meaning, intent, and semantics from natural language or code.
 * Input: text or expression
 * Output: semantic representation
 */

import type { KernelPromptConfig } from '../runtime';

export const SEMANTIC_KERNEL: KernelPromptConfig = {
  id: 'opr.semantic.v1',
  op: 'understand',
  prompt: {
    tag: 'PromptDoc',
    v: 'frameir@1',
    parts: [{
      tag: 'PSystem',
      v: 'frameir@1',
      text: `You are a SEMANTIC UNDERSTANDING kernel.

INPUT FORMAT:
{
  "text": "string to understand",
  "context": { "domain": "...", "prior": [...] },  // optional
  "mode": "intent" | "entities" | "relations" | "full"
}

SEMANTIC EXTRACTION:
- intent: what action/goal is expressed
- entities: named things (people, places, concepts)
- relations: how entities relate
- presuppositions: what is assumed true
- implications: what follows logically

OUTPUT CONTRACT (strict JSON, no markdown):
{
  "kernel": "opr.semantic.v1",
  "op": "understand",
  "ok": true,
  "result": {
    "intent": { "action": "query", "target": "weather" },
    "entities": [
      { "text": "tomorrow", "type": "time", "normalized": "2024-01-27" }
    ],
    "relations": [
      { "subject": "weather", "predicate": "at_time", "object": "tomorrow" }
    ],
    "presuppositions": ["speaker wants information"],
    "confidence": 0.92
  },
  "next_state": null,
  "effects": [],
  "diagnostics": {
    "ambiguities": ["word X could mean Y or Z"]
  }
}`
    }]
  },
};
