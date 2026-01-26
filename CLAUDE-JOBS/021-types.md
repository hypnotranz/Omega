# 021-types: ΩPR Type System

> **Scope**: Define all TypeScript interfaces and types for the ΩPR Runtime
> **Architecture Reference**: [021-OPR-RUNTIME.md](021-OPR-RUNTIME.md)

## Overview

This job creates the foundational type system for ΩPR. All other jobs depend on these types.

## File to Create

`src/core/opr/types.ts`

## Type Definitions

### 1. Step Result Discriminated Union

```typescript
/** Base fields present in all OPR step results */
interface OprStepResultBase {
  attempts: number;
  receipts: OprReceipt[];
}

/** Successful step result */
interface OprStepResultOk extends OprStepResultBase {
  tag: 'ok';
  ok: true;
  output: KernelOutput;
}

/** Failed step result - budget exhausted */
interface OprStepResultBudgetExhausted extends OprStepResultBase {
  tag: 'budget-exhausted';
  ok: false;
  error: OprBudgetExhaustedError;
  lastOutput?: KernelOutput;
}

/** Failed step result - validation failure after all retries */
interface OprStepResultValidationFailed extends OprStepResultBase {
  tag: 'validation-failed';
  ok: false;
  error: OprValidationError;
  violations: ValidationViolation[];
}

/** Failed step result - capability violation (no retry) */
interface OprStepResultCapabilityViolation extends OprStepResultBase {
  tag: 'capability-violation';
  ok: false;
  error: OprCapabilityError;
  requestedCapability: string;
}

/** Discriminated union */
type OprStepResult =
  | OprStepResultOk
  | OprStepResultBudgetExhausted
  | OprStepResultValidationFailed
  | OprStepResultCapabilityViolation;
```

### 2. Type Guards

```typescript
function isOprStepResultOk(r: OprStepResult): r is OprStepResultOk;
function isOprStepResultBudgetExhausted(r: OprStepResult): r is OprStepResultBudgetExhausted;
function isOprStepResultValidationFailed(r: OprStepResult): r is OprStepResultValidationFailed;
function isOprStepResultCapabilityViolation(r: OprStepResult): r is OprStepResultCapabilityViolation;
```

### 3. Kernel Output Contract

```typescript
interface KernelOutput {
  kernel: string;
  op: string;
  ok: boolean;
  result: unknown;
  next_state: KernelState | null;
  effects: Effect[];
  diagnostics: Diagnostics;
}

interface KernelState {
  iteration?: number;
  facts?: string[];
  derived?: string[];
  done?: boolean;
  [key: string]: unknown;
}

interface Diagnostics {
  invariants_checked?: string[];
  notes?: string[];
  errors?: string[];
}
```

### 4. Effect Types

```typescript
interface Effect {
  type: EffectType;
  idempotency_key: string;
  correlation_id?: string;
  payload: unknown;
  preconditions?: Precondition[];
}

type EffectType =
  | 'callback.eval_lisp'
  | 'callback.artifact.get'
  | 'callback.facts.query'
  | 'callback.hash';

interface Precondition {
  type: 'fact_exists' | 'artifact_exists' | 'capability_held';
  value: unknown;
}
```

### 5. Validation Types

```typescript
interface ValidationResult {
  ok: boolean;
  parsed?: unknown;
  violations: ValidationViolation[];
}

interface ValidationViolation {
  path: string;
  code: ViolationCode;
  message: string;
  expected?: string;
  actual?: string;
}

type ViolationCode =
  | 'NOT_JSON'
  | 'NOT_OBJECT'
  | 'MISSING_FIELD'
  | 'WRONG_TYPE'
  | 'INVALID_VALUE'
  | 'KERNEL_MISMATCH'
  | 'OP_MISMATCH';
```

### 6. Receipt Types

```typescript
type Hash = `sha256:${string}`;
type ReceiptId = `rct_${string}`;
type EffectReceiptId = `effr_${string}`;
type HashRef = Hash | null;

interface OprReceipt {
  receipt_version: 1;
  receipt_id: ReceiptId;
  created_at: string;
  prev_receipt_hash: HashRef;
  request_hash: Hash;
  response_hash: HashRef;
  kernel_id: string;
  op: string;
  attempt: number;
  status: ReceiptStatus;
  errors: string[];
  diagnostics?: Diagnostics;
  receipt_hash: Hash;
}

type ReceiptStatus = 'OK' | 'ERROR' | 'TIMEOUT' | 'CALLBACK_ERROR';
```

### 7. Callback Types

```typescript
interface CallbackResult {
  correlation_id: string;
  ok: boolean;
  value?: unknown;
  error?: {
    code: string;
    message: string;
  };
}

interface OprCapabilities {
  allowedCallbacks: Set<EffectType>;
  maxCallbacksPerStep: number;
  callbackTimeout: number;
}
```

### 8. Budget Types

```typescript
interface OprBudgetConfig {
  maxAttempts: number;
  sessionBudget?: Budget;  // Reference to existing Budget type
}
```

### 9. Error Types

```typescript
class OprError extends Error {
  constructor(message: string, public readonly code: string) {
    super(message);
    this.name = 'OprError';
  }
}

class OprValidationError extends OprError {
  constructor(
    message: string,
    public readonly violations: ValidationViolation[]
  ) {
    super(message, 'VALIDATION_FAILED');
    this.name = 'OprValidationError';
  }
}

class OprBudgetExhaustedError extends OprError {
  constructor(
    message: string,
    public readonly budgetType: 'attempts' | 'session-tokens' | 'session-cost'
  ) {
    super(message, 'BUDGET_EXHAUSTED');
    this.name = 'OprBudgetExhaustedError';
  }
}

class OprCapabilityError extends OprError {
  constructor(
    message: string,
    public readonly requestedCapability: string
  ) {
    super(message, 'CAPABILITY_VIOLATION');
    this.name = 'OprCapabilityError';
  }
}
```

### 10. Progress Invariants

```typescript
interface ProgressInvariants {
  iterationMonotonic: boolean;
  derivedMonotonic: boolean;
  deltaTermination: boolean;
}
```

## Acceptance Criteria

1. [ ] File `src/core/opr/types.ts` exists with all types defined above
2. [ ] All types are exported from the module
3. [ ] Type guards are implemented and exported
4. [ ] Error classes extend base OprError
5. [ ] `npx tsc --noEmit` passes with no type errors
6. [ ] Types match the architecture spec in 021-OPR-RUNTIME.md

## Test Verification

```typescript
// Verify discriminated union works
const result: OprStepResult = { tag: 'ok', ok: true, output: {...}, attempts: 1, receipts: [] };
if (isOprStepResultOk(result)) {
  result.output; // TypeScript knows this is KernelOutput
}

// Verify branded types
const hash: Hash = 'sha256:abc123';  // OK
const badHash: Hash = 'md5:abc123';  // Should error

// Verify error inheritance
const err = new OprValidationError('test', []);
console.log(err instanceof OprError); // true
```
