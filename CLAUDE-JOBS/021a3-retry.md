# 021a3-retry: Retry Logic

> **Output**: `src/core/opr/retry.ts`

> **Scope**: Implement retry decision logic and counterexample repair prompts
> **Architecture Reference**: [021-OPR-RUNTIME.md](021-OPR-RUNTIME.md)
> **Depends on**: job-021-types (Layer 1)

## Overview

Build repair prompts from validation violations to guide LLM toward correct output format.

## File to Create

`src/core/opr/retry.ts`

## Imports Contract

What this task needs from its dependencies:

### From 021-types (./types):
- ValidationViolation

## Implementation

```typescript
import type { ValidationViolation } from './types';

/**
 * Build a repair prompt from validation violations
 * This becomes the "counterexample" that guides the LLM to fix its output
 */
export function buildRepairPrompt(violations: ValidationViolation[]): string {
  const lines: string[] = [
    'YOUR PREVIOUS RESPONSE HAD VALIDATION ERRORS.',
    '',
    'VIOLATIONS:',
  ];

  for (const v of violations) {
    lines.push(`  - Path: ${v.path}`);
    lines.push(`    Code: ${v.code}`);
    lines.push(`    Error: ${v.message}`);
    if (v.expected) {
      lines.push(`    Expected: ${v.expected}`);
    }
    if (v.actual) {
      lines.push(`    Got: ${v.actual}`);
    }
    lines.push('');
  }

  lines.push('INSTRUCTIONS:');
  lines.push('1. Fix ALL violations listed above');
  lines.push('2. Return ONLY valid JSON matching the OUTPUT CONTRACT');
  lines.push('3. Do NOT include markdown code blocks');
  lines.push('4. Do NOT include any explanation or preamble');
  lines.push('');
  lines.push('Return the corrected JSON response now:');

  return lines.join('\n');
}

/**
 * Decide whether to retry based on violation type
 */
export function shouldRetry(violations: ValidationViolation[]): boolean {
  // Don't retry on kernel/op mismatch - indicates fundamental confusion
  const hasCriticalViolation = violations.some(v =>
    v.code === 'KERNEL_MISMATCH' || v.code === 'OP_MISMATCH'
  );

  if (hasCriticalViolation) {
    return false;
  }

  // Retry on fixable violations
  const fixableCodes: Set<string> = new Set([
    'NOT_JSON',
    'NOT_OBJECT',
    'MISSING_FIELD',
    'WRONG_TYPE',
    'INVALID_VALUE',
  ]);

  return violations.every(v => fixableCodes.has(v.code));
}

/**
 * Format violations for human-readable display
 */
export function formatViolationsForDisplay(violations: ValidationViolation[]): string {
  return violations.map(v => {
    let line = `[${v.code}] ${v.path}: ${v.message}`;
    if (v.expected && v.actual) {
      line += ` (expected ${v.expected}, got ${v.actual})`;
    }
    return line;
  }).join('\n');
}
```

## Exports Contract
```typescript
export {
  buildRepairPrompt,
  shouldRetry,
  formatViolationsForDisplay,
};
```

## Acceptance Criteria

1. [ ] `buildRepairPrompt` creates structured violation report
2. [ ] `shouldRetry` returns false for kernel/op mismatch
3. [ ] `shouldRetry` returns true for fixable violations
4. [ ] Repair prompt instructs LLM to return only JSON
5. [ ] Unit tests pass

## Test Cases

- RY1: builds repair prompt with all violation fields
- RY2: shouldRetry returns false for KERNEL_MISMATCH
- RY3: shouldRetry returns true for MISSING_FIELD
- RY4: formatViolationsForDisplay formats for console

## Verification

```bash
npx tsc --noEmit src/core/opr/retry.ts
```
