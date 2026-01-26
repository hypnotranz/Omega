# 021a1-validate: Validation Pipeline

> **Scope**: Implement validateKernelOutput() with structured violations for repair prompts
> **Architecture Reference**: [021-OPR-RUNTIME.md](021-OPR-RUNTIME.md#validation-pipeline)
> **Depends on**: 021-types

## Overview

Single-parse validation that returns structured violations usable for counterexample-guided retry.

## File to Create

`src/core/opr/validate.ts`

## Implementation

### Core Function

```typescript
import type { ValidationResult, ValidationViolation, ViolationCode } from './types';

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
      violations: [{
        path: '$',
        code: 'NOT_JSON',
        message: `Invalid JSON: ${(e as Error).message}`,
      }],
    };
  }

  // 2. Must be object
  if (typeof parsed !== 'object' || parsed === null || Array.isArray(parsed)) {
    return {
      ok: false,
      parsed,
      violations: [{
        path: '$',
        code: 'NOT_OBJECT',
        message: 'Response must be a JSON object',
        expected: 'object',
        actual: Array.isArray(parsed) ? 'array' : typeof parsed,
      }],
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
```

### Helper Functions

```typescript
function validateFieldType(
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

function validateEffectsArray(
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
```

### Progress Invariant Validation

```typescript
import type { KernelState, ProgressInvariants } from './types';

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
```

## Exports

```typescript
export {
  validateKernelOutput,
  checkProgressInvariants,
  validateFieldType,
  validateEffectsArray,
};
```

## Acceptance Criteria

1. [ ] `validateKernelOutput` parses JSON only once and reuses result
2. [ ] Returns structured `ValidationViolation[]` with path, code, message
3. [ ] Validates kernel/op match against expected values
4. [ ] Validates effects array structure
5. [ ] `checkProgressInvariants` enforces monotonicity when enabled
6. [ ] All unit tests in `test/opr/unit/validate.spec.ts` pass

## Test Cases (implement in 021g1-unit-tests)

- V1: accepts valid kernel output
- V2: rejects non-JSON with NOT_JSON code
- V3: rejects missing required fields
- V4: rejects kernel mismatch
- V5: provides structured violations for repair prompt
- V6: validates effects array elements
- V7: enforces iteration monotonicity
- V8: enforces derived facts monotonicity
