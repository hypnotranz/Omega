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
