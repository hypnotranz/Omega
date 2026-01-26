# 021g1-unit-tests: Unit Tests

> **Scope**: Implement unit tests for validate, receipts, retry, hash modules
> **Architecture Reference**: [021-OPR-RUNTIME.md](021-OPR-RUNTIME.md#test-suites)
> **Depends on**: 021a1-validate, 021a2-receipts, 021a3-retry, 021a4-hash

## Overview

Comprehensive unit tests for all core ΩPR modules.

## Files to Create

```
test/opr/unit/
├── validate.spec.ts
├── receipts.spec.ts
├── retry.spec.ts
└── hash.spec.ts
```

## validate.spec.ts

```typescript
import { describe, it, expect } from 'vitest';
import { validateKernelOutput, checkProgressInvariants } from '../../../src/core/opr/validate';

describe('validateKernelOutput', () => {
  const expected = { kernelId: 'opr.logic.v1', op: 'infer' };

  it('V1: accepts valid kernel output', () => {
    const raw = JSON.stringify({
      kernel: 'opr.logic.v1',
      op: 'infer',
      ok: true,
      result: { delta: [] },
      next_state: { iteration: 1 },
      effects: [],
      diagnostics: {}
    });

    const result = validateKernelOutput(raw, expected);
    expect(result.ok).toBe(true);
    expect(result.violations).toHaveLength(0);
    expect(result.parsed).toBeDefined();
  });

  it('V2: rejects non-JSON with NOT_JSON code', () => {
    const result = validateKernelOutput('This is not JSON at all', expected);
    expect(result.ok).toBe(false);
    expect(result.violations[0].code).toBe('NOT_JSON');
  });

  it('V3: rejects arrays with NOT_OBJECT code', () => {
    const result = validateKernelOutput('[]', expected);
    expect(result.ok).toBe(false);
    expect(result.violations[0].code).toBe('NOT_OBJECT');
  });

  it('V4: rejects missing required fields', () => {
    const raw = JSON.stringify({ kernel: 'opr.logic.v1' });
    const result = validateKernelOutput(raw, expected);
    expect(result.ok).toBe(false);
    const codes = result.violations.map(v => v.code);
    expect(codes).toContain('MISSING_FIELD');
  });

  it('V5: rejects kernel mismatch', () => {
    const raw = JSON.stringify({
      kernel: 'opr.WRONG.v1',
      op: 'infer',
      ok: true,
      result: {},
      next_state: null,
      effects: [],
      diagnostics: {}
    });

    const result = validateKernelOutput(raw, expected);
    expect(result.ok).toBe(false);
    expect(result.violations.some(v => v.code === 'KERNEL_MISMATCH')).toBe(true);
  });

  it('V6: provides structured violations for repair', () => {
    const raw = JSON.stringify({
      kernel: 'opr.logic.v1',
      op: 'infer',
      ok: 'not-a-boolean',
      result: null,
      next_state: 123,
      effects: 'not-array',
      diagnostics: {}
    });

    const result = validateKernelOutput(raw, expected);
    expect(result.ok).toBe(false);

    for (const v of result.violations) {
      expect(v.path).toBeDefined();
      expect(v.code).toBeDefined();
      expect(v.message).toBeDefined();
    }
  });

  it('V7: validates effects array elements', () => {
    const raw = JSON.stringify({
      kernel: 'opr.logic.v1',
      op: 'infer',
      ok: true,
      result: {},
      next_state: null,
      effects: [{ type: 'callback.eval_lisp' }], // missing idempotency_key
      diagnostics: {}
    });

    const result = validateKernelOutput(raw, expected);
    expect(result.ok).toBe(false);
    expect(result.violations.some(v => v.path.includes('effects'))).toBe(true);
  });
});

describe('checkProgressInvariants', () => {
  it('V8: enforces iteration monotonicity', () => {
    const violations = checkProgressInvariants(
      { iteration: 5 },
      { iteration: 3 },  // Decreased!
      { iterationMonotonic: true, derivedMonotonic: false, deltaTermination: false }
    );

    expect(violations.length).toBeGreaterThan(0);
    expect(violations[0].message).toContain('must increase');
  });

  it('V9: enforces derived facts monotonicity', () => {
    const violations = checkProgressInvariants(
      { derived: ['a', 'b', 'c'] },
      { derived: ['a'] },  // Decreased!
      { iterationMonotonic: false, derivedMonotonic: true, deltaTermination: false }
    );

    expect(violations.length).toBeGreaterThan(0);
  });

  it('V10: passes when invariants satisfied', () => {
    const violations = checkProgressInvariants(
      { iteration: 1, derived: ['a'] },
      { iteration: 2, derived: ['a', 'b'] },
      { iterationMonotonic: true, derivedMonotonic: true, deltaTermination: false }
    );

    expect(violations).toHaveLength(0);
  });
});
```

## receipts.spec.ts

```typescript
import { describe, it, expect, beforeEach } from 'vitest';
import {
  createReceipt,
  verifyReceiptChain,
  InMemoryReceiptStore,
  ReceiptBuilder
} from '../../../src/core/opr/receipts';
import { sha256Of } from '../../../src/core/opr/hash';

describe('createReceipt', () => {
  it('R1: creates receipt with self-hash', () => {
    const receipt = createReceipt({
      prevReceiptHash: null,
      requestHash: sha256Of('request'),
      responseHash: sha256Of('response'),
      kernelId: 'opr.logic.v1',
      op: 'infer',
      attempt: 1,
      status: 'OK',
      errors: [],
    });

    expect(receipt.receipt_id).toMatch(/^rct_/);
    expect(receipt.receipt_hash).toMatch(/^sha256:/);
    expect(receipt.receipt_version).toBe(1);
  });
});

describe('verifyReceiptChain', () => {
  it('R2: verifies valid chain', () => {
    const r1 = createReceipt({
      prevReceiptHash: null,
      requestHash: sha256Of('req1'),
      responseHash: sha256Of('resp1'),
      kernelId: 'test',
      op: 'step',
      attempt: 1,
      status: 'OK',
      errors: [],
    });

    const r2 = createReceipt({
      prevReceiptHash: r1.receipt_hash,
      requestHash: sha256Of('req2'),
      responseHash: sha256Of('resp2'),
      kernelId: 'test',
      op: 'step',
      attempt: 2,
      status: 'OK',
      errors: [],
    });

    const result = verifyReceiptChain([r1, r2]);
    expect(result.valid).toBe(true);
  });

  it('R3: detects self-hash tampering', () => {
    const r1 = createReceipt({
      prevReceiptHash: null,
      requestHash: sha256Of('req1'),
      responseHash: sha256Of('resp1'),
      kernelId: 'test',
      op: 'step',
      attempt: 1,
      status: 'OK',
      errors: [],
    });

    // Tamper with receipt
    const tampered = { ...r1, status: 'ERROR' as const };

    const result = verifyReceiptChain([tampered]);
    expect(result.valid).toBe(false);
    expect(result.error).toContain('self-hash mismatch');
  });

  it('R4: detects chain link tampering', () => {
    const r1 = createReceipt({
      prevReceiptHash: null,
      requestHash: sha256Of('req1'),
      responseHash: sha256Of('resp1'),
      kernelId: 'test',
      op: 'step',
      attempt: 1,
      status: 'OK',
      errors: [],
    });

    const r2 = createReceipt({
      prevReceiptHash: sha256Of('wrong-prev'),  // Wrong link!
      requestHash: sha256Of('req2'),
      responseHash: sha256Of('resp2'),
      kernelId: 'test',
      op: 'step',
      attempt: 2,
      status: 'OK',
      errors: [],
    });

    const result = verifyReceiptChain([r1, r2]);
    expect(result.valid).toBe(false);
    expect(result.brokenAt).toBe(1);
  });

  it('R5: handles empty chain', () => {
    const result = verifyReceiptChain([]);
    expect(result.valid).toBe(true);
  });

  it('R6: first receipt must have null prev_hash', () => {
    const r1 = createReceipt({
      prevReceiptHash: sha256Of('something'),  // Should be null!
      requestHash: sha256Of('req1'),
      responseHash: sha256Of('resp1'),
      kernelId: 'test',
      op: 'step',
      attempt: 1,
      status: 'OK',
      errors: [],
    });

    const result = verifyReceiptChain([r1]);
    expect(result.valid).toBe(false);
    expect(result.error).toContain('null prev_receipt_hash');
  });
});

describe('InMemoryReceiptStore', () => {
  let store: InMemoryReceiptStore;

  beforeEach(() => {
    store = new InMemoryReceiptStore();
  });

  it('R7: indexes by kernel', () => {
    const r1 = createReceipt({
      prevReceiptHash: null,
      requestHash: sha256Of('req1'),
      responseHash: sha256Of('resp1'),
      kernelId: 'kernel-a',
      op: 'step',
      attempt: 1,
      status: 'OK',
      errors: [],
    });

    const r2 = createReceipt({
      prevReceiptHash: r1.receipt_hash,
      requestHash: sha256Of('req2'),
      responseHash: sha256Of('resp2'),
      kernelId: 'kernel-b',
      op: 'step',
      attempt: 1,
      status: 'OK',
      errors: [],
    });

    store.add(r1);
    store.add(r2);

    expect(store.getByKernel('kernel-a')).toHaveLength(1);
    expect(store.getByKernel('kernel-b')).toHaveLength(1);
    expect(store.getAll()).toHaveLength(2);
  });
});

describe('ReceiptBuilder', () => {
  it('R8: chains receipts correctly', () => {
    const store = new InMemoryReceiptStore();
    const builder = new ReceiptBuilder(store, 'opr.logic.v1', 'infer');

    builder.error(1, sha256Of('req1'), sha256Of('resp1'), ['Parse error']);
    builder.error(2, sha256Of('req2'), sha256Of('resp2'), ['Validation error']);
    builder.success(3, sha256Of('req3'), sha256Of('resp3'));

    const receipts = store.getAll();
    expect(receipts).toHaveLength(3);
    expect(receipts[0].prev_receipt_hash).toBeNull();
    expect(receipts[1].prev_receipt_hash).toBe(receipts[0].receipt_hash);
    expect(receipts[2].prev_receipt_hash).toBe(receipts[1].receipt_hash);

    const verification = verifyReceiptChain(receipts);
    expect(verification.valid).toBe(true);
  });
});
```

## hash.spec.ts

```typescript
import { describe, it, expect } from 'vitest';
import { sha256Of, canonicalJson, verifyHash, newId } from '../../../src/core/opr/hash';

describe('sha256Of', () => {
  it('H1: returns sha256-prefixed string', () => {
    const hash = sha256Of({ test: 'data' });
    expect(hash).toMatch(/^sha256:[a-f0-9]{64}$/);
  });

  it('H2: same content produces same hash', () => {
    const hash1 = sha256Of({ a: 1, b: 2 });
    const hash2 = sha256Of({ a: 1, b: 2 });
    expect(hash1).toBe(hash2);
  });

  it('H3: different content produces different hash', () => {
    const hash1 = sha256Of({ a: 1 });
    const hash2 = sha256Of({ a: 2 });
    expect(hash1).not.toBe(hash2);
  });
});

describe('canonicalJson', () => {
  it('H4: sorts keys alphabetically', () => {
    const result = canonicalJson({ z: 1, a: 2, m: 3 });
    expect(result).toBe('{"a":2,"m":3,"z":1}');
  });

  it('H5: handles nested objects', () => {
    const result = canonicalJson({ b: { z: 1, a: 2 }, a: 1 });
    expect(result).toBe('{"a":1,"b":{"a":2,"z":1}}');
  });

  it('H6: omits undefined values', () => {
    const result = canonicalJson({ a: 1, b: undefined });
    expect(result).toBe('{"a":1}');
  });
});

describe('verifyHash', () => {
  it('H7: returns true for matching content', () => {
    const content = { test: 'data' };
    const hash = sha256Of(content);
    expect(verifyHash(content, hash)).toBe(true);
  });

  it('H8: returns false for tampered content', () => {
    const hash = sha256Of({ test: 'original' });
    expect(verifyHash({ test: 'tampered' }, hash)).toBe(false);
  });
});

describe('newId', () => {
  it('H9: generates unique IDs', () => {
    const ids = new Set<string>();
    for (let i = 0; i < 100; i++) {
      ids.add(newId());
    }
    expect(ids.size).toBe(100);
  });

  it('H10: generates hex strings', () => {
    const id = newId();
    expect(id).toMatch(/^[a-f0-9]+$/);
  });
});
```

## retry.spec.ts

```typescript
import { describe, it, expect } from 'vitest';
import { buildRepairPrompt, shouldRetry } from '../../../src/core/opr/retry';

describe('buildRepairPrompt', () => {
  it('RY1: includes all violation fields', () => {
    const prompt = buildRepairPrompt([
      {
        path: '$.kernel',
        code: 'MISSING_FIELD',
        message: 'Missing required field: kernel',
      },
      {
        path: '$.ok',
        code: 'WRONG_TYPE',
        message: 'ok must be boolean',
        expected: 'boolean',
        actual: 'string',
      }
    ]);

    expect(prompt).toContain('VALIDATION ERRORS');
    expect(prompt).toContain('$.kernel');
    expect(prompt).toContain('MISSING_FIELD');
    expect(prompt).toContain('Expected: boolean');
    expect(prompt).toContain('Got: string');
  });

  it('RY2: instructs to return only JSON', () => {
    const prompt = buildRepairPrompt([]);
    expect(prompt).toContain('ONLY valid JSON');
    expect(prompt).toContain('Do NOT include markdown');
  });
});

describe('shouldRetry', () => {
  it('RY3: returns false for KERNEL_MISMATCH', () => {
    const result = shouldRetry([
      { path: '$.kernel', code: 'KERNEL_MISMATCH', message: 'Wrong kernel' }
    ]);
    expect(result).toBe(false);
  });

  it('RY4: returns true for fixable violations', () => {
    const result = shouldRetry([
      { path: '$.ok', code: 'MISSING_FIELD', message: 'Missing field' },
      { path: '$.result', code: 'WRONG_TYPE', message: 'Wrong type' }
    ]);
    expect(result).toBe(true);
  });
});
```

## Acceptance Criteria

1. [ ] All validate tests pass (V1-V10)
2. [ ] All receipt tests pass (R1-R8)
3. [ ] All hash tests pass (H1-H10)
4. [ ] All retry tests pass (RY1-RY4)
5. [ ] `npm test test/opr/unit/` passes with 100% of tests
6. [ ] Coverage > 90% for validate, receipts, hash, retry modules
