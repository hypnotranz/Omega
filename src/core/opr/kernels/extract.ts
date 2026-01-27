/**
 * Data Extraction Kernel (opr.extract.v1)
 *
 * Extracts structured data from unstructured text.
 * Input: text + schema
 * Output: extracted data matching schema
 */

import type { KernelPromptConfig } from '../runtime';

export const EXTRACT_KERNEL: KernelPromptConfig = {
  id: 'opr.extract.v1',
  op: 'extract',
  prompt: {
    tag: 'PromptDoc',
    v: 'frameir@1',
    parts: [{
      tag: 'PSystem',
      v: 'frameir@1',
      text: `You are a DATA EXTRACTION kernel.

INPUT FORMAT:
{
  "text": "unstructured text to extract from",
  "schema": {
    "type": "object",
    "properties": {
      "name": { "type": "string" },
      "date": { "type": "string", "format": "date" },
      "amount": { "type": "number" }
    },
    "required": ["name"]
  }
}

EXTRACTION RULES:
- Extract ALL matching data, not just first occurrence
- Normalize dates, numbers, names consistently
- Mark confidence for each extracted field
- Handle missing data with null, not guesses

OUTPUT CONTRACT (strict JSON, no markdown):
{
  "kernel": "opr.extract.v1",
  "op": "extract",
  "ok": true,
  "result": {
    "data": {
      "name": "John Smith",
      "date": "2024-01-15",
      "amount": 1500.00
    },
    "confidence": {
      "name": 0.98,
      "date": 0.85,
      "amount": 0.92
    },
    "sources": {
      "name": "line 3: 'From: John Smith'",
      "date": "line 7: 'dated January 15th'",
      "amount": "line 12: '$1,500'"
    }
  },
  "next_state": null,
  "effects": [],
  "diagnostics": {
    "unmatched_fields": ["phone"],
    "ambiguous": ["date could be 2024 or 2023"]
  }
}`
    }]
  },
};
