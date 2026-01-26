/**
 * ΩPR Type System
 */

// Kernel Output Contract
export interface KernelState {
  iteration?: number;
  facts?: string[];
  derived?: string[];
  done?: boolean;
  [key: string]: unknown;
}

export interface Diagnostics {
  invariants_checked?: string[];
  notes?: string[];
  errors?: string[];
}

export interface KernelOutput {
  kernel: string;
  op: string;
  ok: boolean;
  result: unknown;
  next_state: KernelState | null;
  effects: Effect[];
  diagnostics: Diagnostics;
}

// Effect Types
export type EffectType =
  | 'callback.eval_lisp'
  | 'callback.artifact.get'
  | 'callback.facts.query'
  | 'callback.hash';

export interface Precondition {
  type: 'fact_exists' | 'artifact_exists' | 'capability_held';
  value: unknown;
}

export interface Effect {
  type: EffectType;
  idempotency_key: string;
  correlation_id?: string;
  payload: unknown;
  preconditions?: Precondition[];
}

// Validation Types
export type ViolationCode =
  | 'NOT_JSON'
  | 'NOT_OBJECT'
  | 'MISSING_FIELD'
  | 'WRONG_TYPE'
  | 'INVALID_VALUE'
  | 'KERNEL_MISMATCH'
  | 'OP_MISMATCH';

export interface ValidationViolation {
  path: string;
  code: ViolationCode;
  message: string;
  expected?: string;
  actual?: string;
}

export interface ValidationResult {
  ok: boolean;
  parsed?: unknown;
  violations: ValidationViolation[];
}

// Receipt Types
export type Hash = `sha256:${string}`;
export type ReceiptId = `rct_${string}`;
export type EffectReceiptId = `effr_${string}`;
export type HashRef = Hash | null;
export type ReceiptStatus = 'OK' | 'ERROR' | 'TIMEOUT' | 'CALLBACK_ERROR';

export interface OprReceipt {
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

// Callback and Capabilities Types
export interface CallbackResult {
  correlation_id: string;
  ok: boolean;
  value?: unknown;
  error?: {
    code: string;
    message: string;
  };
}

export interface OprCapabilities {
  allowedCallbacks: Set<EffectType>;
  maxCallbacksPerStep: number;
  callbackTimeout: number;
}

// Budget Types
export interface OprBudgetConfig {
  maxAttempts: number;
  sessionBudget?: unknown;
}

// Progress Invariants
export interface ProgressInvariants {
  iterationMonotonic: boolean;
  derivedMonotonic: boolean;
  deltaTermination: boolean;
}

// Error Classes
export class OprError extends Error {
  constructor(message: string, public readonly code: string) {
    super(message);
    this.name = 'OprError';
  }
}

export class OprValidationError extends OprError {
  constructor(
    message: string,
    public readonly violations: ValidationViolation[]
  ) {
    super(message, 'VALIDATION_FAILED');
    this.name = 'OprValidationError';
  }
}

export class OprBudgetExhaustedError extends OprError {
  constructor(
    message: string,
    public readonly budgetType: 'attempts' | 'session-tokens' | 'session-cost'
  ) {
    super(message, 'BUDGET_EXHAUSTED');
    this.name = 'OprBudgetExhaustedError';
  }
}

export class OprCapabilityError extends OprError {
  constructor(
    message: string,
    public readonly requestedCapability: string
  ) {
    super(message, 'CAPABILITY_VIOLATION');
    this.name = 'OprCapabilityError';
  }
}

// Step Result - Discriminated Union
export interface OprStepResultBase {
  attempts: number;
  receipts: OprReceipt[];
}

export interface OprStepResultOk extends OprStepResultBase {
  tag: 'ok';
  ok: true;
  output: KernelOutput;
}

export interface OprStepResultBudgetExhausted extends OprStepResultBase {
  tag: 'budget-exhausted';
  ok: false;
  error: OprBudgetExhaustedError;
  lastOutput?: KernelOutput;
}

export interface OprStepResultValidationFailed extends OprStepResultBase {
  tag: 'validation-failed';
  ok: false;
  error: OprValidationError;
  violations: ValidationViolation[];
}

export interface OprStepResultCapabilityViolation extends OprStepResultBase {
  tag: 'capability-violation';
  ok: false;
  error: OprCapabilityError;
  requestedCapability: string;
}

export type OprStepResult =
  | OprStepResultOk
  | OprStepResultBudgetExhausted
  | OprStepResultValidationFailed
  | OprStepResultCapabilityViolation;

// Type Guards
export function isOprStepResultOk(r: OprStepResult): r is OprStepResultOk {
  return r.tag === 'ok';
}

export function isOprStepResultBudgetExhausted(
  r: OprStepResult
): r is OprStepResultBudgetExhausted {
  return r.tag === 'budget-exhausted';
}

export function isOprStepResultValidationFailed(
  r: OprStepResult
): r is OprStepResultValidationFailed {
  return r.tag === 'validation-failed';
}

export function isOprStepResultCapabilityViolation(
  r: OprStepResult
): r is OprStepResultCapabilityViolation {
  return r.tag === 'capability-violation';
}
