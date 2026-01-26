import { describe, it, expect, expectTypeOf } from 'vitest';
import type {
  OprStepResult,
  OprStepResultOk,
  OprStepResultBudgetExhausted,
  OprStepResultValidationFailed,
  OprStepResultCapabilityViolation,
  KernelOutput,
  KernelState,
  Diagnostics,
  Effect,
  EffectType,
  Precondition,
  ValidationResult,
  ValidationViolation,
  ViolationCode,
  Hash,
  ReceiptId,
  EffectReceiptId,
  HashRef,
  OprReceipt,
  ReceiptStatus,
  CallbackResult,
  OprCapabilities,
  OprBudgetConfig,
  ProgressInvariants,
} from '../src/core/opr/types';
import {
  isOprStepResultOk,
  isOprStepResultBudgetExhausted,
  isOprStepResultValidationFailed,
  isOprStepResultCapabilityViolation,
  OprError,
  OprValidationError,
  OprBudgetExhaustedError,
  OprCapabilityError,
} from '../src/core/opr/types';

describe('OprStepResult - Discriminated Union', () => {
  describe('OprStepResultOk', () => {
    it('should be constructable with tag "ok"', () => {
      const result: OprStepResultOk = {
        tag: 'ok',
        ok: true,
        output: {
          kernel: 'test-kernel',
          op: 'test-op',
          ok: true,
          result: { data: 'test' },
          next_state: null,
          effects: [],
          diagnostics: {},
        },
        attempts: 1,
        receipts: [],
      };
      expect(result.tag).toBe('ok');
      expect(result.ok).toBe(true);
    });

    it('should require output field', () => {
      // Type test - ensures KernelOutput is required
      const result: OprStepResultOk = {
        tag: 'ok',
        ok: true,
        output: {
          kernel: 'k',
          op: 'o',
          ok: true,
          result: null,
          next_state: null,
          effects: [],
          diagnostics: {},
        },
        attempts: 1,
        receipts: [],
      };
      expectTypeOf(result.output).toMatchTypeOf<KernelOutput>();
    });

    it('should track attempts count', () => {
      const result: OprStepResultOk = {
        tag: 'ok',
        ok: true,
        output: {
          kernel: 'k',
          op: 'o',
          ok: true,
          result: null,
          next_state: null,
          effects: [],
          diagnostics: {},
        },
        attempts: 3,
        receipts: [],
      };
      expect(result.attempts).toBe(3);
    });

    it('should support receipts array', () => {
      const result: OprStepResultOk = {
        tag: 'ok',
        ok: true,
        output: {
          kernel: 'k',
          op: 'o',
          ok: true,
          result: null,
          next_state: null,
          effects: [],
          diagnostics: {},
        },
        attempts: 1,
        receipts: [
          {
            receipt_version: 1,
            receipt_id: 'rct_abc123' as ReceiptId,
            created_at: new Date().toISOString(),
            prev_receipt_hash: null,
            request_hash: 'sha256:abc' as Hash,
            response_hash: 'sha256:def' as Hash,
            kernel_id: 'k1',
            op: 'op1',
            attempt: 1,
            status: 'OK' as ReceiptStatus,
            errors: [],
            receipt_hash: 'sha256:abc' as Hash,
          },
        ],
      };
      expect(result.receipts).toHaveLength(1);
    });
  });

  describe('OprStepResultBudgetExhausted', () => {
    it('should be constructable with tag "budget-exhausted"', () => {
      const result: OprStepResultBudgetExhausted = {
        tag: 'budget-exhausted',
        ok: false,
        error: new OprBudgetExhaustedError('Budget exceeded', 'attempts'),
        attempts: 5,
        receipts: [],
      };
      expect(result.tag).toBe('budget-exhausted');
      expect(result.ok).toBe(false);
    });

    it('should optionally include lastOutput', () => {
      const result: OprStepResultBudgetExhausted = {
        tag: 'budget-exhausted',
        ok: false,
        error: new OprBudgetExhaustedError('Budget exceeded', 'session-tokens'),
        lastOutput: {
          kernel: 'k',
          op: 'o',
          ok: false,
          result: null,
          next_state: null,
          effects: [],
          diagnostics: {},
        },
        attempts: 5,
        receipts: [],
      };
      expect(result.lastOutput).toBeDefined();
    });
  });

  describe('OprStepResultValidationFailed', () => {
    it('should be constructable with tag "validation-failed"', () => {
      const violations: ValidationViolation[] = [
        {
          path: '$.field',
          code: 'MISSING_FIELD' as ViolationCode,
          message: 'Field is required',
        },
      ];
      const result: OprStepResultValidationFailed = {
        tag: 'validation-failed',
        ok: false,
        error: new OprValidationError('Validation failed', violations),
        violations,
        attempts: 3,
        receipts: [],
      };
      expect(result.tag).toBe('validation-failed');
      expect(result.violations).toHaveLength(1);
    });

    it('should include all violation details', () => {
      const violations: ValidationViolation[] = [
        {
          path: '$.type',
          code: 'WRONG_TYPE' as ViolationCode,
          message: 'Expected string',
          expected: 'string',
          actual: 'number',
        },
      ];
      const result: OprStepResultValidationFailed = {
        tag: 'validation-failed',
        ok: false,
        error: new OprValidationError('Validation failed', violations),
        violations,
        attempts: 1,
        receipts: [],
      };
      expect(result.violations[0].expected).toBe('string');
      expect(result.violations[0].actual).toBe('number');
    });
  });

  describe('OprStepResultCapabilityViolation', () => {
    it('should be constructable with tag "capability-violation"', () => {
      const result: OprStepResultCapabilityViolation = {
        tag: 'capability-violation',
        ok: false,
        error: new OprCapabilityError(
          'Capability not allowed',
          'callback.eval_lisp'
        ),
        requestedCapability: 'callback.eval_lisp',
        attempts: 1,
        receipts: [],
      };
      expect(result.tag).toBe('capability-violation');
      expect(result.requestedCapability).toBe('callback.eval_lisp');
    });
  });
});

describe('Type Guards', () => {
  describe('isOprStepResultOk', () => {
    it('should return true for ok results', () => {
      const result: OprStepResult = {
        tag: 'ok',
        ok: true,
        output: {
          kernel: 'k',
          op: 'o',
          ok: true,
          result: null,
          next_state: null,
          effects: [],
          diagnostics: {},
        },
        attempts: 1,
        receipts: [],
      };
      expect(isOprStepResultOk(result)).toBe(true);
      if (isOprStepResultOk(result)) {
        expectTypeOf(result.output).toMatchTypeOf<KernelOutput>();
      }
    });

    it('should return false for budget-exhausted results', () => {
      const result: OprStepResult = {
        tag: 'budget-exhausted',
        ok: false,
        error: new OprBudgetExhaustedError('test', 'attempts'),
        attempts: 1,
        receipts: [],
      };
      expect(isOprStepResultOk(result)).toBe(false);
    });

    it('should return false for validation-failed results', () => {
      const result: OprStepResult = {
        tag: 'validation-failed',
        ok: false,
        error: new OprValidationError('test', []),
        violations: [],
        attempts: 1,
        receipts: [],
      };
      expect(isOprStepResultOk(result)).toBe(false);
    });

    it('should return false for capability-violation results', () => {
      const result: OprStepResult = {
        tag: 'capability-violation',
        ok: false,
        error: new OprCapabilityError('test', 'callback.eval_lisp'),
        requestedCapability: 'callback.eval_lisp',
        attempts: 1,
        receipts: [],
      };
      expect(isOprStepResultOk(result)).toBe(false);
    });
  });

  describe('isOprStepResultBudgetExhausted', () => {
    it('should return true for budget-exhausted results', () => {
      const result: OprStepResult = {
        tag: 'budget-exhausted',
        ok: false,
        error: new OprBudgetExhaustedError('test', 'attempts'),
        attempts: 5,
        receipts: [],
      };
      expect(isOprStepResultBudgetExhausted(result)).toBe(true);
      if (isOprStepResultBudgetExhausted(result)) {
        expectTypeOf(result.error).toMatchTypeOf<OprBudgetExhaustedError>();
      }
    });

    it('should return false for other result types', () => {
      const okResult: OprStepResult = {
        tag: 'ok',
        ok: true,
        output: {
          kernel: 'k',
          op: 'o',
          ok: true,
          result: null,
          next_state: null,
          effects: [],
          diagnostics: {},
        },
        attempts: 1,
        receipts: [],
      };
      expect(isOprStepResultBudgetExhausted(okResult)).toBe(false);
    });
  });

  describe('isOprStepResultValidationFailed', () => {
    it('should return true for validation-failed results', () => {
      const result: OprStepResult = {
        tag: 'validation-failed',
        ok: false,
        error: new OprValidationError('test', []),
        violations: [],
        attempts: 1,
        receipts: [],
      };
      expect(isOprStepResultValidationFailed(result)).toBe(true);
      if (isOprStepResultValidationFailed(result)) {
        expectTypeOf(result.violations).toMatchTypeOf<ValidationViolation[]>();
      }
    });

    it('should return false for other result types', () => {
      const result: OprStepResult = {
        tag: 'ok',
        ok: true,
        output: {
          kernel: 'k',
          op: 'o',
          ok: true,
          result: null,
          next_state: null,
          effects: [],
          diagnostics: {},
        },
        attempts: 1,
        receipts: [],
      };
      expect(isOprStepResultValidationFailed(result)).toBe(false);
    });
  });

  describe('isOprStepResultCapabilityViolation', () => {
    it('should return true for capability-violation results', () => {
      const result: OprStepResult = {
        tag: 'capability-violation',
        ok: false,
        error: new OprCapabilityError('test', 'callback.eval_lisp'),
        requestedCapability: 'callback.eval_lisp',
        attempts: 1,
        receipts: [],
      };
      expect(isOprStepResultCapabilityViolation(result)).toBe(true);
      if (isOprStepResultCapabilityViolation(result)) {
        expectTypeOf(result.requestedCapability).toMatchTypeOf<string>();
      }
    });

    it('should return false for other result types', () => {
      const result: OprStepResult = {
        tag: 'ok',
        ok: true,
        output: {
          kernel: 'k',
          op: 'o',
          ok: true,
          result: null,
          next_state: null,
          effects: [],
          diagnostics: {},
        },
        attempts: 1,
        receipts: [],
      };
      expect(isOprStepResultCapabilityViolation(result)).toBe(false);
    });
  });
});

describe('KernelOutput Contract', () => {
  it('should have all required fields', () => {
    const output: KernelOutput = {
      kernel: 'my-kernel',
      op: 'my-operation',
      ok: true,
      result: { computed: 'value' },
      next_state: null,
      effects: [],
      diagnostics: {},
    };
    expect(output.kernel).toBe('my-kernel');
    expect(output.op).toBe('my-operation');
    expect(output.ok).toBe(true);
  });

  it('should support KernelState with optional fields', () => {
    const state: KernelState = {
      iteration: 5,
      facts: ['fact1', 'fact2'],
      derived: ['derived1'],
      done: false,
      custom_field: 'custom_value',
    };
    expect(state.iteration).toBe(5);
    expect(state.facts).toHaveLength(2);
  });

  it('should support Diagnostics with optional fields', () => {
    const diag1: Diagnostics = {};
    const diag2: Diagnostics = {
      invariants_checked: ['inv1'],
      notes: ['note1'],
      errors: ['error1'],
    };
    expect(diag1).toEqual({});
    expect(diag2.invariants_checked).toHaveLength(1);
  });
});

describe('Effect Types', () => {
  describe('Effect', () => {
    it('should support all effect types', () => {
      const effectTypes: EffectType[] = [
        'callback.eval_lisp',
        'callback.artifact.get',
        'callback.facts.query',
        'callback.hash',
      ];

      effectTypes.forEach((effectType) => {
        const effect: Effect = {
          type: effectType,
          idempotency_key: `key_${effectType}`,
          payload: {},
        };
        expect(effect.type).toBe(effectType);
      });
    });

    it('should support optional correlation_id', () => {
      const effect: Effect = {
        type: 'callback.eval_lisp',
        idempotency_key: 'key1',
        correlation_id: 'corr-123',
        payload: { code: '(+ 1 2)' },
      };
      expect(effect.correlation_id).toBe('corr-123');
    });

    it('should support optional preconditions', () => {
      const effect: Effect = {
        type: 'callback.artifact.get',
        idempotency_key: 'key2',
        payload: { artifact_id: 'art123' },
        preconditions: [
          {
            type: 'artifact_exists',
            value: 'art123',
          },
        ],
      };
      expect(effect.preconditions).toHaveLength(1);
    });

    it('should support all precondition types', () => {
      const preconditions: Precondition[] = [
        { type: 'fact_exists', value: 'fact1' },
        { type: 'artifact_exists', value: 'art1' },
        { type: 'capability_held', value: 'cap1' },
      ];
      preconditions.forEach((p) => {
        expectTypeOf(p).toMatchTypeOf<Precondition>();
      });
    });
  });
});

describe('Validation Types', () => {
  describe('ValidationResult', () => {
    it('should have ok flag', () => {
      const result: ValidationResult = {
        ok: true,
        parsed: { data: 'value' },
        violations: [],
      };
      expect(result.ok).toBe(true);
    });

    it('should support parsed value when ok', () => {
      const result: ValidationResult = {
        ok: true,
        parsed: { field: 'value' },
        violations: [],
      };
      expect(result.parsed).toEqual({ field: 'value' });
    });

    it('should track violations', () => {
      const violations: ValidationViolation[] = [
        {
          path: '$.missing',
          code: 'MISSING_FIELD' as ViolationCode,
          message: 'Field is missing',
        },
      ];
      const result: ValidationResult = {
        ok: false,
        violations,
      };
      expect(result.violations).toHaveLength(1);
    });
  });

  describe('ValidationViolation', () => {
    it('should support all violation codes', () => {
      const codes: ViolationCode[] = [
        'NOT_JSON',
        'NOT_OBJECT',
        'MISSING_FIELD',
        'WRONG_TYPE',
        'INVALID_VALUE',
        'KERNEL_MISMATCH',
        'OP_MISMATCH',
      ];

      codes.forEach((code) => {
        const violation: ValidationViolation = {
          path: '$.field',
          code,
          message: `Violation: ${code}`,
        };
        expect(violation.code).toBe(code);
      });
    });

    it('should support optional expected and actual fields', () => {
      const violation: ValidationViolation = {
        path: '$.type',
        code: 'WRONG_TYPE' as ViolationCode,
        message: 'Type mismatch',
        expected: 'string',
        actual: 'number',
      };
      expect(violation.expected).toBe('string');
      expect(violation.actual).toBe('number');
    });
  });
});

describe('Receipt Types', () => {
  describe('Hash (branded type)', () => {
    it('should accept sha256: prefix', () => {
      const hash: Hash = 'sha256:abcdef123456' as Hash;
      expect(hash).toMatch(/^sha256:/);
    });
  });

  describe('ReceiptId (branded type)', () => {
    it('should accept rct_ prefix', () => {
      const id: ReceiptId = 'rct_abc123' as ReceiptId;
      expect(id).toMatch(/^rct_/);
    });
  });

  describe('EffectReceiptId (branded type)', () => {
    it('should accept effr_ prefix', () => {
      const id: EffectReceiptId = 'effr_xyz789' as EffectReceiptId;
      expect(id).toMatch(/^effr_/);
    });
  });

  describe('OprReceipt', () => {
    it('should have all required fields', () => {
      const receipt: OprReceipt = {
        receipt_version: 1,
        receipt_id: 'rct_123' as ReceiptId,
        created_at: new Date().toISOString(),
        prev_receipt_hash: null,
        request_hash: 'sha256:req' as Hash,
        response_hash: 'sha256:resp' as Hash,
        kernel_id: 'k1',
        op: 'op1',
        attempt: 1,
        status: 'OK' as ReceiptStatus,
        errors: [],
        receipt_hash: 'sha256:rct' as Hash,
      };
      expect(receipt.receipt_version).toBe(1);
      expect(receipt.attempt).toBe(1);
    });

    it('should support optional diagnostics', () => {
      const receipt: OprReceipt = {
        receipt_version: 1,
        receipt_id: 'rct_123' as ReceiptId,
        created_at: new Date().toISOString(),
        prev_receipt_hash: 'sha256:prev' as Hash,
        request_hash: 'sha256:req' as Hash,
        response_hash: null,
        kernel_id: 'k1',
        op: 'op1',
        attempt: 1,
        status: 'ERROR' as ReceiptStatus,
        errors: ['Something failed'],
        diagnostics: {
          notes: ['Attempt failed'],
        },
        receipt_hash: 'sha256:rct' as Hash,
      };
      expect(receipt.diagnostics).toBeDefined();
      expect(receipt.status).toBe('ERROR');
    });

    it('should support all receipt statuses', () => {
      const statuses: ReceiptStatus[] = [
        'OK',
        'ERROR',
        'TIMEOUT',
        'CALLBACK_ERROR',
      ];

      statuses.forEach((status) => {
        const receipt: OprReceipt = {
          receipt_version: 1,
          receipt_id: 'rct_123' as ReceiptId,
          created_at: new Date().toISOString(),
          prev_receipt_hash: null,
          request_hash: 'sha256:req' as Hash,
          response_hash: null,
          kernel_id: 'k1',
          op: 'op1',
          attempt: 1,
          status,
          errors: [],
          receipt_hash: 'sha256:rct' as Hash,
        };
        expect(receipt.status).toBe(status);
      });
    });
  });
});

describe('Callback Types', () => {
  describe('CallbackResult', () => {
    it('should have correlation_id and ok flag', () => {
      const result: CallbackResult = {
        correlation_id: 'corr_123',
        ok: true,
        value: { result: 'success' },
      };
      expect(result.correlation_id).toBe('corr_123');
      expect(result.ok).toBe(true);
    });

    it('should support value when ok', () => {
      const result: CallbackResult = {
        correlation_id: 'corr_1',
        ok: true,
        value: { computed: 42 },
      };
      expect(result.value).toEqual({ computed: 42 });
    });

    it('should support error when not ok', () => {
      const result: CallbackResult = {
        correlation_id: 'corr_2',
        ok: false,
        error: {
          code: 'EVAL_ERROR',
          message: 'Evaluation failed',
        },
      };
      expect(result.error?.code).toBe('EVAL_ERROR');
    });
  });

  describe('OprCapabilities', () => {
    it('should define allowed callbacks', () => {
      const caps: OprCapabilities = {
        allowedCallbacks: new Set(['callback.eval_lisp', 'callback.hash']),
        maxCallbacksPerStep: 10,
        callbackTimeout: 30000,
      };
      expect(caps.allowedCallbacks.has('callback.eval_lisp')).toBe(true);
      expect(caps.maxCallbacksPerStep).toBe(10);
    });

    it('should support different callback configurations', () => {
      const restrictive: OprCapabilities = {
        allowedCallbacks: new Set(['callback.hash']),
        maxCallbacksPerStep: 1,
        callbackTimeout: 5000,
      };
      expect(restrictive.allowedCallbacks.size).toBe(1);
    });
  });
});

describe('Budget Types', () => {
  describe('OprBudgetConfig', () => {
    it('should define maxAttempts', () => {
      const config: OprBudgetConfig = {
        maxAttempts: 5,
      };
      expect(config.maxAttempts).toBe(5);
    });

    it('should support optional sessionBudget', () => {
      const config: OprBudgetConfig = {
        maxAttempts: 3,
        sessionBudget: undefined, // Allows undefined or actual Budget reference
      };
      expect(config.maxAttempts).toBe(3);
    });
  });
});

describe('Error Classes', () => {
  describe('OprError', () => {
    it('should extend Error', () => {
      const err = new OprError('Test error', 'TEST_CODE');
      expect(err).toBeInstanceOf(Error);
      expect(err.name).toBe('OprError');
    });

    it('should have code property', () => {
      const err = new OprError('Test error', 'MY_CODE');
      expect(err.code).toBe('MY_CODE');
    });

    it('should have message property', () => {
      const err = new OprError('Something went wrong', 'CODE');
      expect(err.message).toBe('Something went wrong');
    });
  });

  describe('OprValidationError', () => {
    it('should extend OprError', () => {
      const violations: ValidationViolation[] = [];
      const err = new OprValidationError('Validation failed', violations);
      expect(err).toBeInstanceOf(OprError);
      expect(err).toBeInstanceOf(Error);
    });

    it('should have VALIDATION_FAILED code', () => {
      const violations: ValidationViolation[] = [];
      const err = new OprValidationError('test', violations);
      expect(err.code).toBe('VALIDATION_FAILED');
    });

    it('should store violations', () => {
      const violations: ValidationViolation[] = [
        {
          path: '$.field',
          code: 'MISSING_FIELD' as ViolationCode,
          message: 'Field is missing',
        },
      ];
      const err = new OprValidationError('Validation failed', violations);
      expect(err.violations).toHaveLength(1);
      expect(err.violations[0].path).toBe('$.field');
    });

    it('should have correct name', () => {
      const err = new OprValidationError('test', []);
      expect(err.name).toBe('OprValidationError');
    });
  });

  describe('OprBudgetExhaustedError', () => {
    it('should extend OprError', () => {
      const err = new OprBudgetExhaustedError('Budget exceeded', 'attempts');
      expect(err).toBeInstanceOf(OprError);
      expect(err).toBeInstanceOf(Error);
    });

    it('should have BUDGET_EXHAUSTED code', () => {
      const err = new OprBudgetExhaustedError('Budget exceeded', 'attempts');
      expect(err.code).toBe('BUDGET_EXHAUSTED');
    });

    it('should store budgetType', () => {
      const err1 = new OprBudgetExhaustedError('msg', 'attempts');
      const err2 = new OprBudgetExhaustedError('msg', 'session-tokens');
      const err3 = new OprBudgetExhaustedError('msg', 'session-cost');

      expect(err1.budgetType).toBe('attempts');
      expect(err2.budgetType).toBe('session-tokens');
      expect(err3.budgetType).toBe('session-cost');
    });

    it('should have correct name', () => {
      const err = new OprBudgetExhaustedError('test', 'attempts');
      expect(err.name).toBe('OprBudgetExhaustedError');
    });
  });

  describe('OprCapabilityError', () => {
    it('should extend OprError', () => {
      const err = new OprCapabilityError('Not allowed', 'callback.eval_lisp');
      expect(err).toBeInstanceOf(OprError);
      expect(err).toBeInstanceOf(Error);
    });

    it('should have CAPABILITY_VIOLATION code', () => {
      const err = new OprCapabilityError('test', 'callback.eval_lisp');
      expect(err.code).toBe('CAPABILITY_VIOLATION');
    });

    it('should store requestedCapability', () => {
      const err = new OprCapabilityError('Not allowed', 'callback.artifact.get');
      expect(err.requestedCapability).toBe('callback.artifact.get');
    });

    it('should have correct name', () => {
      const err = new OprCapabilityError('test', 'callback.hash');
      expect(err.name).toBe('OprCapabilityError');
    });
  });
});

describe('Progress Invariants', () => {
  it('should track iteration monotonicity', () => {
    const invariants: ProgressInvariants = {
      iterationMonotonic: true,
      derivedMonotonic: true,
      deltaTermination: false,
    };
    expect(invariants.iterationMonotonic).toBe(true);
  });

  it('should track derived monotonicity', () => {
    const invariants: ProgressInvariants = {
      iterationMonotonic: false,
      derivedMonotonic: true,
      deltaTermination: true,
    };
    expect(invariants.derivedMonotonic).toBe(true);
  });

  it('should track termination', () => {
    const invariants: ProgressInvariants = {
      iterationMonotonic: true,
      derivedMonotonic: true,
      deltaTermination: true,
    };
    expect(invariants.deltaTermination).toBe(true);
  });
});

describe('Type Compatibility - Full Integration', () => {
  it('should support workflow: success case', () => {
    const output: KernelOutput = {
      kernel: 'my-kernel',
      op: 'process',
      ok: true,
      result: { value: 123 },
      next_state: { iteration: 1, done: false },
      effects: [
        {
          type: 'callback.hash',
          idempotency_key: 'hash_1',
          payload: { data: 'test' },
        },
      ],
      diagnostics: { notes: ['Processing successful'] },
    };

    const result: OprStepResultOk = {
      tag: 'ok',
      ok: true,
      output,
      attempts: 1,
      receipts: [],
    };

    if (isOprStepResultOk(result)) {
      expect(result.output.ok).toBe(true);
      expect(result.output.result).toEqual({ value: 123 });
    }
  });

  it('should support workflow: validation failure case', () => {
    const violations: ValidationViolation[] = [
      {
        path: '$.output.ok',
        code: 'WRONG_TYPE' as ViolationCode,
        message: 'Expected boolean',
        expected: 'boolean',
        actual: 'string',
      },
    ];

    const result: OprStepResultValidationFailed = {
      tag: 'validation-failed',
      ok: false,
      error: new OprValidationError('Output validation failed', violations),
      violations,
      attempts: 2,
      receipts: [],
    };

    if (isOprStepResultValidationFailed(result)) {
      expect(result.violations[0].code).toBe('WRONG_TYPE');
      expect(result.error.violations).toHaveLength(1);
    }
  });

  it('should support workflow: budget exhausted case', () => {
    const result: OprStepResultBudgetExhausted = {
      tag: 'budget-exhausted',
      ok: false,
      error: new OprBudgetExhaustedError(
        'Exceeded maximum attempts',
        'attempts'
      ),
      attempts: 5,
      receipts: [
        {
          receipt_version: 1,
          receipt_id: 'rct_1' as ReceiptId,
          created_at: new Date().toISOString(),
          prev_receipt_hash: null,
          request_hash: 'sha256:req' as Hash,
          response_hash: 'sha256:resp' as Hash,
          kernel_id: 'k1',
          op: 'op1',
          attempt: 1,
          status: 'ERROR' as ReceiptStatus,
          errors: ['Timeout'],
          receipt_hash: 'sha256:rct1' as Hash,
        },
      ],
    };

    if (isOprStepResultBudgetExhausted(result)) {
      expect(result.attempts).toBe(5);
      expect(result.receipts).toHaveLength(1);
    }
  });

  it('should support workflow: capability violation case', () => {
    const result: OprStepResultCapabilityViolation = {
      tag: 'capability-violation',
      ok: false,
      error: new OprCapabilityError(
        'Callback not allowed',
        'callback.eval_lisp'
      ),
      requestedCapability: 'callback.eval_lisp',
      attempts: 1,
      receipts: [],
    };

    if (isOprStepResultCapabilityViolation(result)) {
      expect(result.requestedCapability).toBe('callback.eval_lisp');
      expect(result.error.code).toBe('CAPABILITY_VIOLATION');
    }
  });
});
