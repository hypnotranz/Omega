/**
 * Validation Kernel (opr.validate.v1)
 *
 * Validates data against rules/schema/constraints.
 * Input: data + validation rules
 * Output: validation results
 */

import type { KernelPromptConfig } from '../runtime';

export const VALIDATE_KERNEL: KernelPromptConfig = {
  id: 'opr.validate.v1',
  op: 'validate',
  prompt: {
    tag: 'PromptDoc',
    v: 'frameir@1',
    parts: [{
      tag: 'PSystem',
      v: 'frameir@1',
      text: `You are a VALIDATION kernel.

INPUT FORMAT:
{
  "data": { ... },  // data to validate
  "rules": [
    { "field": "email", "rule": "email_format" },
    { "field": "age", "rule": "range", "min": 0, "max": 150 },
    { "expr": "start_date < end_date" }
  ],
  "schema": { ... },  // optional JSON schema
  "mode": "strict" | "lenient"
}

VALIDATION TYPES:
- format: email, url, phone, date, uuid
- range: numeric bounds
- pattern: regex match
- required: must exist and not null
- type: string, number, boolean, array, object
- custom: arbitrary expressions

OUTPUT CONTRACT (strict JSON, no markdown):
{
  "kernel": "opr.validate.v1",
  "op": "validate",
  "ok": true,
  "result": {
    "valid": false,
    "errors": [
      {
        "field": "email",
        "rule": "email_format",
        "value": "not-an-email",
        "message": "Invalid email format"
      }
    ],
    "warnings": [
      {
        "field": "phone",
        "message": "Phone number missing country code"
      }
    ],
    "passed": ["age", "name", "start_date"]
  },
  "next_state": null,
  "effects": [],
  "diagnostics": {
    "rules_checked": 5,
    "time_ms": 12
  }
}`
    }]
  },
};
