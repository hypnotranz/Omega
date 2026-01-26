/**
 * ΩPR Validation Pipeline
 *
 * Single-parse validation that returns structured violations usable for
 * counterexample-guided retry.
 */

import type {
  ValidationResult,
  ValidationViolation,
  KernelState,
  ProgressInvariants,
} from './types';

/**
 * Validates kernel output JSON response.
 *
 * Performs single-parse validation, reusing the parsed result for repair prompts.
 *
 * @param raw - Raw JSON string from kernel
 * @param expected - Expected kernel ID and operation for security validation
 * @returns ValidationResult with structured violations
 */
export function validateKernelOutput(
  raw: string,
  expected: { kernelId: string; op: string }
): ValidationResult {
  const violations: ValidationViolation[] = [];

  // 1. Parse JSON (single parse, reuse result)
  let parsed: unknown;
  try {
    parsed = JSON.parse(raw);
  } catch (e) {
    return {
      ok: false,
      violations: [
        {
          path: '$',
          code: 'NOT_JSON',
          message: `Invalid JSON: ${(e as Error).message}`,
        },
      ],
    };
  }

  // 2. Must be object
  if (typeof parsed !== 'object' || parsed === null || Array.isArray(parsed)) {
    return {
      ok: false,
      parsed,
      violations: [
        {
          path: '$',
          code: 'NOT_OBJECT',
          message: 'Response must be a JSON object',
          expected: 'object',
          actual: Array.isArray(parsed) ? 'array' : typeof parsed,
        },
      ],
    };
  }

  const obj = parsed as Record<string, unknown>;

  // 3. Required fields
  const required = ['kernel', 'op', 'ok', 'result', 'next_state', 'effects', 'diagnostics'];
  for (const field of required) {
    if (!(field in obj)) {
      violations.push({
        path: `$.${field}`,
        code: 'MISSING_FIELD',
        message: `Missing required field: ${field}`,
      });
    }
  }

  // Continue validation only if we have the basic structure
  if (violations.length > 0) {
    return { ok: false, parsed, violations };
  }

  // 4. Type checks
  validateFieldType(obj, 'kernel', 'string', violations);
  validateFieldType(obj, 'op', 'string', violations);
  validateFieldType(obj, 'ok', 'boolean', violations);

  // 5. Kernel/op match (critical for security)
  if (obj.kernel !== expected.kernelId) {
    violations.push({
      path: '$.kernel',
      code: 'KERNEL_MISMATCH',
      message: `Expected kernel "${expected.kernelId}", got "${obj.kernel}"`,
      expected: expected.kernelId,
      actual: String(obj.kernel),
    });
  }

  if (obj.op !== expected.op) {
    violations.push({
      path: '$.op',
      code: 'OP_MISMATCH',
      message: `Expected op "${expected.op}", got "${obj.op}"`,
      expected: expected.op,
      actual: String(obj.op),
    });
  }

  // 6. Effects array validation
  if ('effects' in obj) {
    if (!Array.isArray(obj.effects)) {
      violations.push({
        path: '$.effects',
        code: 'WRONG_TYPE',
        message: 'effects must be an array',
        expected: 'array',
        actual: typeof obj.effects,
      });
    } else {
      validateEffectsArray(obj.effects, violations);
    }
  }

  // 7. next_state must be object or null
  if ('next_state' in obj && obj.next_state !== null && typeof obj.next_state !== 'object') {
    violations.push({
      path: '$.next_state',
      code: 'WRONG_TYPE',
      message: 'next_state must be object or null',
      expected: 'object | null',
      actual: typeof obj.next_state,
    });
  }

  return {
    ok: violations.length === 0,
    parsed,
    violations,
  };
}

/**
 * Validates a field type within an object.
 *
 * @param obj - Object to validate
 * @param field - Field name
 * @param expectedType - Expected typeof result
 * @param violations - Array to accumulate violations
 */
export function validateFieldType(
  obj: Record<string, unknown>,
  field: string,
  expectedType: string,
  violations: ValidationViolation[]
): void {
  if (field in obj && typeof obj[field] !== expectedType) {
    violations.push({
      path: `$.${field}`,
      code: 'WRONG_TYPE',
      message: `${field} must be a ${expectedType}`,
      expected: expectedType,
      actual: typeof obj[field],
    });
  }
}

/**
 * Validates an array of effects.
 *
 * Each effect must have:
 * - type: string (effect type)
 * - idempotency_key: string (for deduplication)
 *
 * @param effects - Array of effects to validate
 * @param violations - Array to accumulate violations
 */
export function validateEffectsArray(
  effects: unknown[],
  violations: ValidationViolation[]
): void {
  for (let i = 0; i < effects.length; i++) {
    const effect = effects[i];
    if (typeof effect !== 'object' || effect === null) {
      violations.push({
        path: `$.effects[${i}]`,
        code: 'WRONG_TYPE',
        message: `Effect at index ${i} must be an object`,
        expected: 'object',
        actual: typeof effect,
      });
      continue;
    }

    const e = effect as Record<string, unknown>;

    // Required effect fields
    if (!('type' in e) || typeof e.type !== 'string') {
      violations.push({
        path: `$.effects[${i}].type`,
        code: 'MISSING_FIELD',
        message: `Effect at index ${i} missing required field: type`,
      });
    }

    if (!('idempotency_key' in e) || typeof e.idempotency_key !== 'string') {
      violations.push({
        path: `$.effects[${i}].idempotency_key`,
        code: 'MISSING_FIELD',
        message: `Effect at index ${i} missing required field: idempotency_key`,
      });
    }
  }
}

/**
 * Checks progress invariants between state transitions.
 *
 * Enforces monotonicity properties when enabled:
 * - iterationMonotonic: iteration number must increase
 * - derivedMonotonic: derived facts can only grow
 *
 * @param prevState - Previous kernel state (null if initial)
 * @param nextState - Next kernel state
 * @param invariants - Progress invariant configuration
 * @returns Array of violations
 */
export function checkProgressInvariants(
  prevState: KernelState | null,
  nextState: KernelState,
  invariants: ProgressInvariants
): ValidationViolation[] {
  const violations: ValidationViolation[] = [];

  if (prevState === null) return violations;

  if (invariants.iterationMonotonic) {
    const prevIter = prevState.iteration ?? 0;
    const nextIter = nextState.iteration ?? 0;
    if (nextIter <= prevIter) {
      violations.push({
        path: '$.next_state.iteration',
        code: 'INVALID_VALUE',
        message: `Iteration must increase: ${prevIter} → ${nextIter}`,
      });
    }
  }

  if (invariants.derivedMonotonic) {
    const prevCount = prevState.derived?.length ?? 0;
    const nextCount = nextState.derived?.length ?? 0;
    if (nextCount < prevCount) {
      violations.push({
        path: '$.next_state.derived',
        code: 'INVALID_VALUE',
        message: `Derived facts must grow monotonically: ${prevCount} → ${nextCount}`,
      });
    }
  }

  return violations;
}
