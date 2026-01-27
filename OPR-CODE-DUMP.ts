/**
 * OPR (Omega Protocol Runtime) - Complete Code Dump
 * Generated: 2026-01-27
 * 
 * This file contains all OPR implementation files concatenated together.
 * Each section is prefixed with the file path.
 */


// ═══════════════════════════════════════════════════════════════════════════
// FILE: src/core/opr/types.ts
// ═══════════════════════════════════════════════════════════════════════════

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

// ═══════════════════════════════════════════════════════════════════════════
// FILE: src/core/opr/hash.ts
// ═══════════════════════════════════════════════════════════════════════════

/**
 * OPR Hash Utilities
 *
 * Deterministic hashing for content-addressed receipts and audit trail integrity.
 */

import { createHash, randomBytes } from 'crypto';
import type { Hash } from './types';

/**
 * Compute SHA-256 hash of content
 * Returns branded Hash type: `sha256:${hex}`
 */
export function sha256Of(content: unknown): Hash {
  const canonical = canonicalJson(content);
  const hash = createHash('sha256').update(canonical).digest('hex');
  return `sha256:${hash}` as Hash;
}

/**
 * Generate a new unique ID (for receipt IDs, etc.)
 */
export function newId(): string {
  return randomBytes(16).toString('hex');
}

/**
 * Canonical JSON serialization for deterministic hashing
 * - Sorts object keys alphabetically
 * - No extra whitespace
 * - Handles undefined by omitting keys
 */
export function canonicalJson(value: unknown): string {
  return JSON.stringify(value, (_, v) => {
    if (v && typeof v === 'object' && !Array.isArray(v)) {
      // Sort object keys
      const sorted: Record<string, unknown> = {};
      for (const key of Object.keys(v).sort()) {
        if (v[key] !== undefined) {
          sorted[key] = v[key];
        }
      }
      return sorted;
    }
    return v;
  });
}

/**
 * Verify a hash matches content
 */
export function verifyHash(content: unknown, expectedHash: Hash): boolean {
  const computed = sha256Of(content);
  return computed === expectedHash;
}

/**
 * Extract the hex portion from a Hash
 */
export function hashToHex(hash: Hash): string {
  return hash.slice(7); // Remove "sha256:" prefix
}

/**
 * Create a Hash from a hex string
 */
export function hexToHash(hex: string): Hash {
  return `sha256:${hex}` as Hash;
}

// ═══════════════════════════════════════════════════════════════════════════
// FILE: src/core/opr/receipts.ts
// ═══════════════════════════════════════════════════════════════════════════

/**
 * OPR Receipt Chain
 *
 * Hash-linked receipt chain for audit trail of all LLM attempts.
 * Each receipt links to the previous, forming a verifiable chain.
 */

import type { OprReceipt, Hash, HashRef, ReceiptId, ReceiptStatus, Diagnostics } from './types';
import { sha256Of, newId } from './hash';

/**
 * Parameters for creating a receipt
 */
export interface CreateReceiptParams {
  prevReceiptHash: HashRef;
  requestHash: Hash;
  responseHash: HashRef;
  kernelId: string;
  op: string;
  attempt: number;
  status: ReceiptStatus;
  errors: string[];
  diagnostics?: Diagnostics;
}

/**
 * Result of chain verification
 */
export interface ChainVerificationResult {
  valid: boolean;
  brokenAt?: number;
  error?: string;
}

/**
 * Interface for receipt storage
 */
export interface ReceiptStore {
  /** Add a receipt to the store */
  add(receipt: OprReceipt): void;

  /** Get all receipts for a kernel */
  getByKernel(kernelId: string): OprReceipt[];

  /** Get all receipts */
  getAll(): OprReceipt[];

  /** Get the last receipt (for chaining) */
  getLast(): OprReceipt | null;

  /** Clear all receipts */
  clear(): void;

  /** Get count */
  count(): number;
}

/**
 * Create a receipt with computed self-hash
 */
export function createReceipt(params: CreateReceiptParams): OprReceipt {
  const receiptWithoutHash: Omit<OprReceipt, 'receipt_hash'> = {
    receipt_version: 1,
    receipt_id: `rct_${newId()}` as ReceiptId,
    created_at: new Date().toISOString(),
    prev_receipt_hash: params.prevReceiptHash,
    request_hash: params.requestHash,
    response_hash: params.responseHash,
    kernel_id: params.kernelId,
    op: params.op,
    attempt: params.attempt,
    status: params.status,
    errors: params.errors,
    diagnostics: params.diagnostics,
  };

  // Compute self-hash
  const receipt_hash = computeReceiptHash(receiptWithoutHash);

  return { ...receiptWithoutHash, receipt_hash };
}

/**
 * Compute hash of a receipt (excluding the receipt_hash field)
 */
function computeReceiptHash(receipt: Omit<OprReceipt, 'receipt_hash'>): Hash {
  return sha256Of(receipt);
}

/**
 * Verify the integrity of a receipt chain
 */
export function verifyReceiptChain(receipts: OprReceipt[]): ChainVerificationResult {
  if (receipts.length === 0) {
    return { valid: true };
  }

  for (let i = 0; i < receipts.length; i++) {
    const receipt = receipts[i];

    // Verify self-hash
    const { receipt_hash: _, ...hashableFields } = receipt;
    const computedHash = sha256Of(hashableFields);
    if (computedHash !== receipt.receipt_hash) {
      return {
        valid: false,
        brokenAt: i,
        error: `Receipt ${i} self-hash mismatch: expected ${receipt.receipt_hash}, computed ${computedHash}`,
      };
    }

    // Verify chain link (except first)
    if (i > 0) {
      const prevReceipt = receipts[i - 1];
      if (receipt.prev_receipt_hash !== prevReceipt.receipt_hash) {
        return {
          valid: false,
          brokenAt: i,
          error: `Receipt ${i} chain link broken: prev_receipt_hash doesn't match previous receipt`,
        };
      }
    } else {
      // First receipt should have null prev_receipt_hash
      if (receipt.prev_receipt_hash !== null) {
        return {
          valid: false,
          brokenAt: 0,
          error: `First receipt should have null prev_receipt_hash`,
        };
      }
    }
  }

  return { valid: true };
}

/**
 * In-memory implementation of ReceiptStore
 */
export class InMemoryReceiptStore implements ReceiptStore {
  private receipts: OprReceipt[] = [];
  private byKernel = new Map<string, OprReceipt[]>();

  add(receipt: OprReceipt): void {
    this.receipts.push(receipt);

    const kernelReceipts = this.byKernel.get(receipt.kernel_id) ?? [];
    kernelReceipts.push(receipt);
    this.byKernel.set(receipt.kernel_id, kernelReceipts);
  }

  getByKernel(kernelId: string): OprReceipt[] {
    return this.byKernel.get(kernelId) ?? [];
  }

  getAll(): OprReceipt[] {
    return [...this.receipts];
  }

  getLast(): OprReceipt | null {
    return this.receipts.length > 0 ? this.receipts[this.receipts.length - 1] : null;
  }

  clear(): void {
    this.receipts = [];
    this.byKernel.clear();
  }

  count(): number {
    return this.receipts.length;
  }
}

/**
 * Fluent builder for creating receipts
 */
export class ReceiptBuilder {
  private store: ReceiptStore;
  private kernelId: string;
  private op: string;

  constructor(store: ReceiptStore, kernelId: string, op: string) {
    this.store = store;
    this.kernelId = kernelId;
    this.op = op;
  }

  /** Create and add a receipt for a successful attempt */
  success(
    attempt: number,
    requestHash: Hash,
    responseHash: Hash,
    diagnostics?: Diagnostics
  ): OprReceipt {
    const receipt = createReceipt({
      prevReceiptHash: this.store.getLast()?.receipt_hash ?? null,
      requestHash,
      responseHash,
      kernelId: this.kernelId,
      op: this.op,
      attempt,
      status: 'OK',
      errors: [],
      diagnostics,
    });
    this.store.add(receipt);
    return receipt;
  }

  /** Create and add a receipt for a failed attempt */
  error(
    attempt: number,
    requestHash: Hash,
    responseHash: HashRef,
    errors: string[]
  ): OprReceipt {
    const receipt = createReceipt({
      prevReceiptHash: this.store.getLast()?.receipt_hash ?? null,
      requestHash,
      responseHash,
      kernelId: this.kernelId,
      op: this.op,
      attempt,
      status: 'ERROR',
      errors,
    });
    this.store.add(receipt);
    return receipt;
  }

  /** Create and add a receipt for a timeout */
  timeout(attempt: number, requestHash: Hash): OprReceipt {
    const receipt = createReceipt({
      prevReceiptHash: this.store.getLast()?.receipt_hash ?? null,
      requestHash,
      responseHash: null,
      kernelId: this.kernelId,
      op: this.op,
      attempt,
      status: 'TIMEOUT',
      errors: ['Request timed out'],
    });
    this.store.add(receipt);
    return receipt;
  }

  /** Create and add a receipt for a callback error */
  callbackError(
    attempt: number,
    requestHash: Hash,
    responseHash: HashRef,
    errors: string[]
  ): OprReceipt {
    const receipt = createReceipt({
      prevReceiptHash: this.store.getLast()?.receipt_hash ?? null,
      requestHash,
      responseHash,
      kernelId: this.kernelId,
      op: this.op,
      attempt,
      status: 'CALLBACK_ERROR',
      errors,
    });
    this.store.add(receipt);
    return receipt;
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// FILE: src/core/opr/validate.ts
// ═══════════════════════════════════════════════════════════════════════════

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
        message: `Iteration must increase: ${prevIter} -> ${nextIter}`,
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
        message: `Derived facts must grow monotonically: ${prevCount} -> ${nextCount}`,
      });
    }
  }

  return violations;
}

// ═══════════════════════════════════════════════════════════════════════════
// FILE: src/core/opr/retry.ts
// ═══════════════════════════════════════════════════════════════════════════

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

// ═══════════════════════════════════════════════════════════════════════════
// FILE: src/core/opr/runtime.ts
// ═══════════════════════════════════════════════════════════════════════════

/**
 * OPR Runtime
 *
 * The main execution engine for OPR kernel operations.
 * Handles validation, retry with counterexample feedback, and receipt generation.
 */

import type {
  OprStepResult,
  OprStepResultOk,
  OprStepResultBudgetExhausted,
  KernelOutput,
  KernelState,
  OprBudgetConfig,
  ProgressInvariants,
  ValidationViolation,
  Hash,
  OprReceipt,
} from './types';
import { OprBudgetExhaustedError } from './types';
import type { OprLLMAdapter, OprLLMRequest } from './adapters/types';
import type { ReceiptStore } from './receipts';
import { ReceiptBuilder } from './receipts';
import { validateKernelOutput, checkProgressInvariants } from './validate';
import { buildRepairPrompt } from './retry';
import { sha256Of } from './hash';
import type { PromptDoc } from '../../frameir/prompt';

/**
 * Kernel prompt configuration
 */
export interface KernelPromptConfig {
  /** Unique kernel identifier */
  id: string;

  /** Compiled prompt document */
  prompt: PromptDoc;

  /** Operation name (default: 'step') */
  op?: string;
}

/**
 * Session budget interface (optional)
 */
export interface SessionBudget {
  hasRemaining(type: 'tokens' | 'cost'): boolean;
  consumeTokens(tokens: number): void;
  consumeCost(cost: number): void;
}

/**
 * OPR Runtime configuration
 */
export interface OprRuntimeConfig {
  /** Kernel configuration with prompt and capabilities */
  kernel: KernelPromptConfig;

  /** LLM adapter for making calls */
  adapter: OprLLMAdapter;

  /** Receipt storage */
  receipts: ReceiptStore;

  /** Budget configuration */
  budget: OprBudgetConfig;

  /** Progress invariants to enforce (optional) */
  invariants?: ProgressInvariants;

  /** Session budget (optional) */
  sessionBudget?: SessionBudget;
}

/**
 * Parameters for executing a step
 */
export interface OprExecuteParams {
  /** Program/data to pass to kernel */
  program: unknown;

  /** Current state (null for first step, memento from previous step) */
  state: unknown | null;
}

/**
 * Internal request type
 */
export interface OprRequest {
  kernelId: string;
  prompt: PromptDoc;
  program: unknown;
  state: unknown | null;
  repairContext: string | null;
}

/**
 * Run result types
 */
export interface OprRunResultOk {
  tag: 'ok';
  results: OprStepResultOk[];
  finalState: KernelState | null;
  iterations: number;
  receipts: OprReceipt[];
}

export interface OprRunResultError {
  tag: 'error';
  error: OprStepResult;
  iterations: number;
  receipts: OprReceipt[];
}

export interface OprRunResultMaxIterations {
  tag: 'max-iterations';
  results: OprStepResultOk[];
  iterations: number;
  receipts: OprReceipt[];
}

export type OprRunResult = OprRunResultOk | OprRunResultError | OprRunResultMaxIterations;

interface ExtractOk {
  ok: true;
  json: unknown;
}

interface ExtractError {
  ok: false;
  error: string;
}

type ExtractResult = ExtractOk | ExtractError;

/**
 * Extract JSON object from LLM response
 * Handles markdown code blocks and extra text
 */
function extractJsonObject(response: string): ExtractResult {
  // Try to find JSON in code blocks first
  const codeBlockMatch = response.match(/```(?:json)?\s*([\s\S]*?)```/);
  const content = codeBlockMatch ? codeBlockMatch[1].trim() : response.trim();

  // Find the first { and matching }
  const start = content.indexOf('{');
  if (start === -1) {
    return { ok: false, error: 'No JSON object found in response' } as ExtractError;
  }

  let depth = 0;
  let end = -1;
  for (let i = start; i < content.length; i++) {
    if (content[i] === '{') depth++;
    else if (content[i] === '}') {
      depth--;
      if (depth === 0) {
        end = i;
        break;
      }
    }
  }

  if (end === -1) {
    return { ok: false, error: 'Unbalanced braces in JSON' } as ExtractError;
  }

  const jsonStr = content.slice(start, end + 1);
  try {
    const json = JSON.parse(jsonStr);
    return { ok: true, json } as ExtractOk;
  } catch (e) {
    return { ok: false, error: `Invalid JSON: ${(e as Error).message}` } as ExtractError;
  }
}

/**
 * OPR Runtime - the main execution engine
 */
export class OprRuntime {
  private config: OprRuntimeConfig;
  private receiptBuilder: ReceiptBuilder;

  constructor(config: OprRuntimeConfig) {
    this.config = config;
    this.receiptBuilder = new ReceiptBuilder(
      config.receipts,
      config.kernel.id,
      config.kernel.op ?? 'step'
    );
  }

  /**
   * Execute a single kernel step with validation and retry
   */
  async step(params: OprExecuteParams): Promise<OprStepResult> {
    const { kernel, adapter, budget, invariants, sessionBudget } = this.config;
    const { maxAttempts } = budget;

    let lastViolations: ValidationViolation[] = [];
    let repairContext: string | null = null;

    for (let attempt = 1; attempt <= maxAttempts; attempt++) {
      // Check session budget (if provided)
      if (sessionBudget) {
        if (!sessionBudget.hasRemaining('tokens')) {
          return this.makeBudgetExhaustedResult('session-tokens', attempt, lastViolations);
        }
        if (!sessionBudget.hasRemaining('cost')) {
          return this.makeBudgetExhaustedResult('session-cost', attempt, lastViolations);
        }
      }

      // Build request
      const userContent = this.formatUserContent(params, repairContext);
      const request: OprLLMRequest = {
        kernelId: kernel.id,
        prompt: kernel.prompt,
        userContent,
        repairContext: repairContext ?? undefined,
      };
      const requestHash = sha256Of(request);

      // Call LLM
      let response: string;
      try {
        response = await adapter.complete(request);
      } catch (e) {
        // Record timeout/error receipt
        this.receiptBuilder.timeout(attempt, requestHash);
        continue;
      }

      const responseHash = sha256Of(response);

      // Consume session budget (if provided)
      if (sessionBudget && adapter.getLastUsage) {
        const usage = adapter.getLastUsage();
        sessionBudget.consumeTokens(usage.totalTokens);
        sessionBudget.consumeCost(usage.estimatedCost);
      }

      // Extract JSON from response (handles markdown code blocks, etc.)
      const extracted = extractJsonObject(response);
      if (extracted.ok === false) {
        const errorMsg = (extracted as ExtractError).error;
        lastViolations = [
          {
            path: '$',
            code: 'NOT_JSON',
            message: errorMsg,
          },
        ];
        this.receiptBuilder.error(attempt, requestHash, responseHash, [errorMsg]);
        repairContext = buildRepairPrompt(lastViolations);
        continue;
      }

      // Validate response
      const validation = validateKernelOutput(JSON.stringify(extracted.json), {
        kernelId: kernel.id,
        op: kernel.op ?? 'step',
      });

      if (!validation.ok) {
        lastViolations = validation.violations;
        this.receiptBuilder.error(
          attempt,
          requestHash,
          responseHash,
          validation.violations.map((v) => v.message)
        );
        repairContext = buildRepairPrompt(validation.violations);
        continue;
      }

      const output = validation.parsed as KernelOutput;

      // Check progress invariants (if configured and we have previous state)
      if (invariants && params.state !== null && output.next_state !== null) {
        const invariantViolations = checkProgressInvariants(
          params.state as KernelState,
          output.next_state,
          invariants
        );
        if (invariantViolations.length > 0) {
          lastViolations = invariantViolations;
          this.receiptBuilder.error(
            attempt,
            requestHash,
            responseHash,
            invariantViolations.map((v) => v.message)
          );
          repairContext = buildRepairPrompt(invariantViolations);
          continue;
        }
      }

      // Success!
      this.receiptBuilder.success(attempt, requestHash, responseHash, output.diagnostics);

      return {
        tag: 'ok',
        ok: true,
        output,
        attempts: attempt,
        receipts: this.config.receipts.getAll(),
      };
    }

    // Exhausted local budget
    return this.makeBudgetExhaustedResult('attempts', maxAttempts, lastViolations);
  }

  /**
   * Run kernel to fixpoint (until next_state.done = true or null)
   */
  async runToFixpoint(params: OprExecuteParams): Promise<OprRunResult> {
    const results: OprStepResultOk[] = [];
    let currentState = params.state;
    let iterations = 0;
    const maxIterations = 100; // Safety limit

    while (iterations < maxIterations) {
      const result = await this.step({ program: params.program, state: currentState });

      if (result.tag !== 'ok') {
        return {
          tag: 'error',
          error: result,
          iterations,
          receipts: this.config.receipts.getAll(),
        };
      }

      results.push(result);
      iterations++;

      // Check termination
      const nextState = result.output.next_state;
      if (nextState === null || nextState.done === true) {
        return {
          tag: 'ok',
          results,
          finalState: nextState,
          iterations,
          receipts: this.config.receipts.getAll(),
        };
      }

      currentState = nextState;
    }

    // Max iterations reached
    return {
      tag: 'max-iterations',
      results,
      iterations,
      receipts: this.config.receipts.getAll(),
    };
  }

  /**
   * Format user content for the LLM request
   */
  private formatUserContent(params: OprExecuteParams, repairContext: string | null): string {
    const parts: string[] = [];

    if (repairContext) {
      parts.push(repairContext);
      parts.push('\n---\n');
    }

    parts.push('PROGRAM:');
    parts.push(JSON.stringify(params.program, null, 2));

    if (params.state !== null) {
      parts.push('\nCURRENT STATE:');
      parts.push(JSON.stringify(params.state, null, 2));
    }

    return parts.join('\n');
  }

  private makeBudgetExhaustedResult(
    budgetType: 'attempts' | 'session-tokens' | 'session-cost',
    attempts: number,
    lastViolations: ValidationViolation[]
  ): OprStepResultBudgetExhausted {
    return {
      tag: 'budget-exhausted',
      ok: false,
      error: new OprBudgetExhaustedError(`Budget exhausted: ${budgetType}`, budgetType),
      attempts,
      receipts: this.config.receipts.getAll(),
    };
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// FILE: src/core/opr/index.ts
// ═══════════════════════════════════════════════════════════════════════════

export * from './types';
export * from './validate';
export * from './retry';
export * from './hash';
export * from './receipts';
export * from './runtime';
export * from './adapters';

// ═══════════════════════════════════════════════════════════════════════════
// FILE: src/core/opr/adapters/types.ts
// ═══════════════════════════════════════════════════════════════════════════

/**
 * OPR Adapter Interface
 *
 * Common interface for LLM adapters used by OprRuntime.
 * Adapters handle the actual LLM API calls.
 */

import type { PromptDoc } from '../../../frameir/prompt';

/**
 * Usage information from an LLM call
 */
export interface LLMUsage {
  promptTokens: number;
  completionTokens: number;
  totalTokens: number;
  estimatedCost: number; // in USD
}

/**
 * Request to send to LLM
 */
export interface OprLLMRequest {
  /** Kernel ID for context */
  kernelId: string;

  /** Compiled prompt (system + few-shot examples) */
  prompt: PromptDoc;

  /** User content (program, state, etc.) */
  userContent: string;

  /** Optional repair context from previous failed attempt */
  repairContext?: string;

  /** Max tokens for response */
  maxTokens?: number;

  /** Temperature (0-1) */
  temperature?: number;
}

/**
 * Interface for LLM adapters
 */
export interface OprLLMAdapter {
  /**
   * Send a completion request to the LLM
   * @returns The raw text response from the LLM
   */
  complete(request: OprLLMRequest): Promise<string>;

  /**
   * Get usage information from the last call (optional)
   */
  getLastUsage?(): LLMUsage;

  /**
   * Get the model identifier
   */
  getModel(): string;

  /**
   * Check if the adapter supports streaming
   */
  supportsStreaming(): boolean;
}

/**
 * Base configuration for all adapters
 */
export interface OprAdapterConfig {
  /** Model to use */
  model: string;

  /** Max tokens for response (default: 2000) */
  maxTokens?: number;

  /** Temperature (default: 0) */
  temperature?: number;

  /** Request timeout in ms (default: 60000) */
  timeout?: number;
}

/**
 * OpenAI-specific configuration
 */
export interface OpenAIAdapterConfig extends OprAdapterConfig {
  apiKey: string;
  baseURL?: string;
  organization?: string;
}

/**
 * Anthropic-specific configuration
 */
export interface AnthropicAdapterConfig extends OprAdapterConfig {
  apiKey: string;
  baseURL?: string;
}

/**
 * Scripted adapter configuration (for testing)
 */
export interface ScriptedAdapterConfig {
  /** Responses to return in sequence */
  responses: Array<string | { response: string; delay?: number }>;

  /** Whether to loop responses or throw on exhaustion */
  loop?: boolean;
}

/**
 * Abstract base class for LLM adapters
 * Provides common functionality like prompt compilation
 */
export abstract class BaseOprAdapter implements OprLLMAdapter {
  protected lastUsage: LLMUsage | null = null;

  abstract complete(request: OprLLMRequest): Promise<string>;
  abstract getModel(): string;

  getLastUsage(): LLMUsage {
    if (!this.lastUsage) {
      return {
        promptTokens: 0,
        completionTokens: 0,
        totalTokens: 0,
        estimatedCost: 0,
      };
    }
    return this.lastUsage;
  }

  supportsStreaming(): boolean {
    return false;
  }

  /**
   * Format the user message content
   */
  protected formatUserContent(request: OprLLMRequest): string {
    let content = request.userContent;

    if (request.repairContext) {
      content = `${request.repairContext}\n\n---\n\nORIGINAL REQUEST:\n${content}`;
    }

    return content;
  }

  /**
   * Estimate cost based on token usage and model
   */
  protected estimateCost(
    usage: { promptTokens: number; completionTokens: number },
    model: string
  ): number {
    // Rough estimates per 1M tokens
    const pricing: Record<string, { prompt: number; completion: number }> = {
      'gpt-4o': { prompt: 2.5, completion: 10.0 },
      'gpt-4o-mini': { prompt: 0.15, completion: 0.6 },
      'gpt-4-turbo': { prompt: 10.0, completion: 30.0 },
      'claude-3-opus': { prompt: 15.0, completion: 75.0 },
      'claude-3-sonnet': { prompt: 3.0, completion: 15.0 },
      'claude-3-haiku': { prompt: 0.25, completion: 1.25 },
    };

    const prices = pricing[model] ?? { prompt: 1.0, completion: 3.0 };

    return (
      (usage.promptTokens / 1_000_000) * prices.prompt +
      (usage.completionTokens / 1_000_000) * prices.completion
    );
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// FILE: src/core/opr/adapters/scripted.ts
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Scripted OPR Adapter
 *
 * Returns pre-defined responses in sequence. Used for testing.
 */

import type { OprLLMRequest, ScriptedAdapterConfig } from './types';
import { BaseOprAdapter } from './types';

/**
 * Adapter that returns scripted responses for testing
 */
export class ScriptedOprAdapter extends BaseOprAdapter {
  private responses: Array<string | { response: string; delay?: number }>;
  private loop: boolean;
  private index = 0;

  constructor(config: ScriptedAdapterConfig) {
    super();
    this.responses = config.responses;
    this.loop = config.loop ?? false;
  }

  async complete(request: OprLLMRequest): Promise<string> {
    if (this.index >= this.responses.length) {
      if (this.loop) {
        this.index = 0;
      } else {
        throw new Error('ScriptedOprAdapter: No more responses available');
      }
    }

    const item = this.responses[this.index++];
    const response = typeof item === 'string' ? item : item.response;
    const delay = typeof item === 'string' ? 0 : item.delay ?? 0;

    if (delay > 0) {
      await new Promise((resolve) => setTimeout(resolve, delay));
    }

    // Estimate token counts based on content length
    const promptTokens = Math.ceil(request.userContent.length / 4);
    const completionTokens = Math.ceil(response.length / 4);

    this.lastUsage = {
      promptTokens,
      completionTokens,
      totalTokens: promptTokens + completionTokens,
      estimatedCost: this.estimateCost({ promptTokens, completionTokens }, 'scripted'),
    };

    return response;
  }

  getModel(): string {
    return 'scripted';
  }

  /**
   * Reset the response index
   */
  reset(): void {
    this.index = 0;
  }

  /**
   * Get remaining response count
   */
  remainingResponses(): number {
    return Math.max(0, this.responses.length - this.index);
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// FILE: src/core/opr/adapters/openai.ts
// ═══════════════════════════════════════════════════════════════════════════

/**
 * OpenAI OPR Adapter
 *
 * Makes real calls to OpenAI API
 */

import type { OprLLMRequest, OpenAIAdapterConfig } from './types';
import { BaseOprAdapter } from './types';

export class OpenAIOprAdapter extends BaseOprAdapter {
  private config: OpenAIAdapterConfig;

  constructor(config: OpenAIAdapterConfig) {
    super();
    this.config = config;
  }

  async complete(request: OprLLMRequest): Promise<string> {
    const { apiKey, baseURL, organization, model, maxTokens, temperature, timeout } = this.config;

    // Build messages from prompt
    const messages: Array<{ role: string; content: string }> = [];

    // Extract system message from prompt (supports PSystem from FrameIR)
    if (request.prompt && request.prompt.parts) {
      for (const part of request.prompt.parts) {
        if ((part as any).tag === 'PSystem') {
          messages.push({ role: 'system', content: (part as any).text });
        } else if ((part as any).tag === 'System') {
          messages.push({ role: 'system', content: (part as any).content });
        }
      }
    }

    // Add user content
    messages.push({ role: 'user', content: request.userContent });

    const body = {
      model,
      messages,
      max_tokens: request.maxTokens ?? maxTokens ?? 2000,
      temperature: request.temperature ?? temperature ?? 0,
    };

    const headers: Record<string, string> = {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${apiKey}`,
    };

    if (organization) {
      headers['OpenAI-Organization'] = organization;
    }

    const url = `${baseURL ?? 'https://api.openai.com/v1'}/chat/completions`;

    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), timeout ?? 60000);

    try {
      const response = await fetch(url, {
        method: 'POST',
        headers,
        body: JSON.stringify(body),
        signal: controller.signal,
      });

      clearTimeout(timeoutId);

      if (!response.ok) {
        const error = await response.text();
        throw new Error(`OpenAI API error ${response.status}: ${error}`);
      }

      const data = await response.json();

      // Track usage
      if (data.usage) {
        this.lastUsage = {
          promptTokens: data.usage.prompt_tokens,
          completionTokens: data.usage.completion_tokens,
          totalTokens: data.usage.total_tokens,
          estimatedCost: this.estimateCost(
            {
              promptTokens: data.usage.prompt_tokens,
              completionTokens: data.usage.completion_tokens,
            },
            model
          ),
        };
      }

      return data.choices[0]?.message?.content ?? '';
    } catch (e) {
      clearTimeout(timeoutId);
      if ((e as Error).name === 'AbortError') {
        throw new Error('OpenAI request timed out');
      }
      throw e;
    }
  }

  getModel(): string {
    return this.config.model;
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// FILE: src/core/opr/adapters/anthropic.ts
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Anthropic OPR Adapter
 *
 * Makes real calls to Anthropic API
 */

import type { OprLLMRequest, AnthropicAdapterConfig } from './types';
import { BaseOprAdapter } from './types';

export class AnthropicOprAdapter extends BaseOprAdapter {
  private config: AnthropicAdapterConfig;

  constructor(config: AnthropicAdapterConfig) {
    super();
    this.config = config;
  }

  async complete(request: OprLLMRequest): Promise<string> {
    const { apiKey, baseURL, model, maxTokens, temperature, timeout } = this.config;

    // Extract system message from prompt (supports PSystem from FrameIR)
    let system = '';
    if (request.prompt && request.prompt.parts) {
      for (const part of request.prompt.parts) {
        if ((part as any).tag === 'PSystem') {
          system += (part as any).text + '\n';
        } else if ((part as any).tag === 'System') {
          system += (part as any).content + '\n';
        }
      }
    }

    const body: Record<string, unknown> = {
      model,
      max_tokens: request.maxTokens ?? maxTokens ?? 2000,
      messages: [{ role: 'user', content: request.userContent }],
    };

    if (system) {
      body.system = system.trim();
    }

    if ((request.temperature ?? temperature) !== undefined) {
      body.temperature = request.temperature ?? temperature;
    }

    const url = `${baseURL ?? 'https://api.anthropic.com'}/v1/messages`;

    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), timeout ?? 60000);

    try {
      const response = await fetch(url, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'x-api-key': apiKey,
          'anthropic-version': '2023-06-01',
        },
        body: JSON.stringify(body),
        signal: controller.signal,
      });

      clearTimeout(timeoutId);

      if (!response.ok) {
        const error = await response.text();
        throw new Error(`Anthropic API error ${response.status}: ${error}`);
      }

      const data = await response.json();

      // Track usage
      if (data.usage) {
        this.lastUsage = {
          promptTokens: data.usage.input_tokens,
          completionTokens: data.usage.output_tokens,
          totalTokens: data.usage.input_tokens + data.usage.output_tokens,
          estimatedCost: this.estimateCost(
            {
              promptTokens: data.usage.input_tokens,
              completionTokens: data.usage.output_tokens,
            },
            model
          ),
        };
      }

      // Extract text from content blocks
      const content = data.content ?? [];
      const text = content
        .filter((c: any) => c.type === 'text')
        .map((c: any) => c.text)
        .join('');

      return text;
    } catch (e) {
      clearTimeout(timeoutId);
      if ((e as Error).name === 'AbortError') {
        throw new Error('Anthropic request timed out');
      }
      throw e;
    }
  }

  getModel(): string {
    return this.config.model;
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// FILE: src/core/opr/adapters/index.ts
// ═══════════════════════════════════════════════════════════════════════════

/**
 * OPR Adapters
 */

export * from './types';
export { ScriptedOprAdapter } from './scripted';
export { OpenAIOprAdapter } from './openai';
export { AnthropicOprAdapter } from './anthropic';

// ═══════════════════════════════════════════════════════════════════════════
// FILE: src/core/opr/kernels/logic.ts
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Logic Inference Kernel (opr.logic.v1)
 *
 * Performs forward-chaining inference over horn clauses.
 * Input: rules (horn clauses) + facts
 * Output: newly derived facts
 */

import type { KernelPromptConfig } from '../runtime';

export const LOGIC_KERNEL: KernelPromptConfig = {
  id: 'opr.logic.v1',
  op: 'infer',
  prompt: {
    tag: 'PromptDoc',
    v: 'frameir@1',
    parts: [{
      tag: 'PSystem',
      v: 'frameir@1',
      text: `You are a LOGIC INFERENCE kernel implementing forward-chaining deduction.

INPUT FORMAT:
{
  "rules": ["head :- body1, body2", ...],  // Horn clauses
  "facts": ["predicate(args)", ...]         // Ground facts
}

SEMANTICS:
- Rules are horn clauses: "conclusion :- premise1, premise2"
- Variables are UPPERCASE: X, Y, Person
- Constants are lowercase: socrates, 42, true
- Apply all rules whose premises are satisfied by current facts
- Derive new facts by unifying variables with constants

OUTPUT CONTRACT (strict JSON, no markdown):
{
  "kernel": "opr.logic.v1",
  "op": "infer",
  "ok": true,
  "result": {
    "delta": ["newly derived facts this iteration"]
  },
  "next_state": {
    "iteration": <int>,
    "derived": ["all derived facts so far"],
    "done": <true if delta is empty, false otherwise>
  },
  "effects": [],
  "diagnostics": {
    "rules_applied": ["which rules fired"],
    "bindings": [{"rule": "...", "unifier": {...}}]
  }
}

TERMINATION: Set done=true when no new facts can be derived (fixpoint).`
    }]
  },
};

// ═══════════════════════════════════════════════════════════════════════════
// FILE: src/core/opr/kernels/analyze.ts
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Code Analysis Kernel (opr.analyze.v1)
 *
 * Analyzes Lisp/Scheme expressions for structure, complexity, effects.
 * Input: Lisp expression as string
 * Output: Analysis results
 */

import type { KernelPromptConfig } from '../runtime';

export const ANALYZE_KERNEL: KernelPromptConfig = {
  id: 'opr.analyze.v1',
  op: 'analyze',
  prompt: {
    tag: 'PromptDoc',
    v: 'frameir@1',
    parts: [{
      tag: 'PSystem',
      v: 'frameir@1',
      text: `You are a CODE ANALYSIS kernel for Lisp/Scheme expressions.

INPUT FORMAT:
{
  "expr": "(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))",
  "focus": ["complexity", "purity", "tail-recursion"]  // optional
}

ANALYSIS DIMENSIONS:
- complexity: time/space complexity estimate
- purity: pure functional or has effects
- tail-recursion: is it tail-recursive
- bindings: what names are bound/free
- calls: what functions are called
- patterns: recognized idioms (map, fold, recursion, etc.)

OUTPUT CONTRACT (strict JSON, no markdown):
{
  "kernel": "opr.analyze.v1",
  "op": "analyze",
  "ok": true,
  "result": {
    "complexity": { "time": "O(n)", "space": "O(n)" },
    "purity": { "pure": false, "effects": ["mutation", "io"] },
    "tail_recursive": false,
    "bindings": { "bound": ["n"], "free": ["=", "*", "-"] },
    "calls": ["=", "*", "-", "factorial"],
    "patterns": ["recursion", "conditional"]
  },
  "next_state": null,
  "effects": [],
  "diagnostics": {}
}

Always analyze thoroughly. If an aspect doesn't apply, include it with null.`
    }]
  },
};

// ═══════════════════════════════════════════════════════════════════════════
// FILE: src/core/opr/kernels/semantic.ts
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Semantic Understanding Kernel (opr.semantic.v1)
 *
 * Extracts meaning, intent, and semantics from natural language or code.
 * Input: text or expression
 * Output: semantic representation
 */

import type { KernelPromptConfig } from '../runtime';

export const SEMANTIC_KERNEL: KernelPromptConfig = {
  id: 'opr.semantic.v1',
  op: 'understand',
  prompt: {
    tag: 'PromptDoc',
    v: 'frameir@1',
    parts: [{
      tag: 'PSystem',
      v: 'frameir@1',
      text: `You are a SEMANTIC UNDERSTANDING kernel.

INPUT FORMAT:
{
  "text": "string to understand",
  "context": { "domain": "...", "prior": [...] },  // optional
  "mode": "intent" | "entities" | "relations" | "full"
}

SEMANTIC EXTRACTION:
- intent: what action/goal is expressed
- entities: named things (people, places, concepts)
- relations: how entities relate
- presuppositions: what is assumed true
- implications: what follows logically

OUTPUT CONTRACT (strict JSON, no markdown):
{
  "kernel": "opr.semantic.v1",
  "op": "understand",
  "ok": true,
  "result": {
    "intent": { "action": "query", "target": "weather" },
    "entities": [
      { "text": "tomorrow", "type": "time", "normalized": "2024-01-27" }
    ],
    "relations": [
      { "subject": "weather", "predicate": "at_time", "object": "tomorrow" }
    ],
    "presuppositions": ["speaker wants information"],
    "confidence": 0.92
  },
  "next_state": null,
  "effects": [],
  "diagnostics": {
    "ambiguities": ["word X could mean Y or Z"]
  }
}`
    }]
  },
};

// ═══════════════════════════════════════════════════════════════════════════
// FILE: src/core/opr/kernels/codeReview.ts
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Code Review Kernel (opr.review.v1)
 *
 * Reviews code for bugs, style, security, performance.
 * Input: code snippet with language
 * Output: review findings
 */

import type { KernelPromptConfig } from '../runtime';

export const CODE_REVIEW_KERNEL: KernelPromptConfig = {
  id: 'opr.review.v1',
  op: 'review',
  prompt: {
    tag: 'PromptDoc',
    v: 'frameir@1',
    parts: [{
      tag: 'PSystem',
      v: 'frameir@1',
      text: `You are a CODE REVIEW kernel.

INPUT FORMAT:
{
  "code": "function foo() { ... }",
  "language": "typescript",
  "focus": ["bugs", "security", "performance", "style"]  // optional
}

REVIEW CATEGORIES:
- bugs: logic errors, null derefs, off-by-one, race conditions
- security: injection, XSS, auth issues, secrets exposure
- performance: N+1, unnecessary allocations, blocking IO
- style: naming, complexity, duplication, readability
- correctness: does it do what it claims

SEVERITY: critical > high > medium > low > info

OUTPUT CONTRACT (strict JSON, no markdown):
{
  "kernel": "opr.review.v1",
  "op": "review",
  "ok": true,
  "result": {
    "findings": [
      {
        "category": "security",
        "severity": "high",
        "line": 42,
        "code": "eval(userInput)",
        "message": "Code injection vulnerability",
        "suggestion": "Use JSON.parse or a safe parser"
      }
    ],
    "summary": {
      "critical": 0,
      "high": 1,
      "medium": 2,
      "low": 3
    },
    "recommendation": "NEEDS_CHANGES" | "APPROVED" | "DISCUSS"
  },
  "next_state": null,
  "effects": [],
  "diagnostics": {}
}`
    }]
  },
};

// ═══════════════════════════════════════════════════════════════════════════
// FILE: src/core/opr/kernels/transform.ts
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Code Transform Kernel (opr.transform.v1)
 *
 * Transforms code from one form to another.
 * Input: code + transformation spec
 * Output: transformed code
 */

import type { KernelPromptConfig } from '../runtime';

export const TRANSFORM_KERNEL: KernelPromptConfig = {
  id: 'opr.transform.v1',
  op: 'transform',
  prompt: {
    tag: 'PromptDoc',
    v: 'frameir@1',
    parts: [{
      tag: 'PSystem',
      v: 'frameir@1',
      text: `You are a CODE TRANSFORM kernel.

INPUT FORMAT:
{
  "code": "source code",
  "from": "javascript",
  "to": "typescript",  // or transformation name
  "options": {
    "strict": true,
    "preserve_comments": true
  }
}

TRANSFORMATIONS:
- Language translation: JS->TS, Python->Rust, etc.
- Refactoring: extract function, inline, rename
- Style: callback->async/await, class->functional
- Optimization: loop unrolling, memoization
- Desugar: macros, syntax sugar

OUTPUT CONTRACT (strict JSON, no markdown):
{
  "kernel": "opr.transform.v1",
  "op": "transform",
  "ok": true,
  "result": {
    "code": "transformed source code",
    "changes": [
      { "type": "add_type", "line": 1, "description": "Added return type" },
      { "type": "rename", "from": "foo", "to": "processData" }
    ],
    "warnings": ["Possible semantic change at line 42"]
  },
  "next_state": null,
  "effects": [],
  "diagnostics": {
    "confidence": 0.95,
    "manual_review_needed": ["line 42: ambiguous conversion"]
  }
}`
    }]
  },
};

// ═══════════════════════════════════════════════════════════════════════════
// FILE: src/core/opr/kernels/extract.ts
// ═══════════════════════════════════════════════════════════════════════════

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

// ═══════════════════════════════════════════════════════════════════════════
// FILE: src/core/opr/kernels/classify.ts
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Classification Kernel (opr.classify.v1)
 *
 * Classifies input into predefined categories.
 * Input: item + categories
 * Output: classification with confidence
 */

import type { KernelPromptConfig } from '../runtime';

export const CLASSIFY_KERNEL: KernelPromptConfig = {
  id: 'opr.classify.v1',
  op: 'classify',
  prompt: {
    tag: 'PromptDoc',
    v: 'frameir@1',
    parts: [{
      tag: 'PSystem',
      v: 'frameir@1',
      text: `You are a CLASSIFICATION kernel.

INPUT FORMAT:
{
  "item": "text or data to classify",
  "categories": ["bug", "feature", "question", "docs"],
  "multi_label": false,  // true allows multiple categories
  "context": "optional domain context"
}

CLASSIFICATION RULES:
- Choose from ONLY the provided categories
- If multi_label=false, pick the BEST single category
- Always provide confidence scores
- Explain reasoning for ambiguous cases

OUTPUT CONTRACT (strict JSON, no markdown):
{
  "kernel": "opr.classify.v1",
  "op": "classify",
  "ok": true,
  "result": {
    "classification": "bug",  // or ["bug", "feature"] if multi_label
    "confidence": 0.87,
    "scores": {
      "bug": 0.87,
      "feature": 0.45,
      "question": 0.12,
      "docs": 0.05
    },
    "reasoning": "Contains stack trace and 'error' keyword"
  },
  "next_state": null,
  "effects": [],
  "diagnostics": {
    "close_call": true,
    "alternative": "feature"
  }
}`
    }]
  },
};

// ═══════════════════════════════════════════════════════════════════════════
// FILE: src/core/opr/kernels/synthesize.ts
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Code Synthesis Kernel (opr.synthesize.v1)
 *
 * Generates code from specification/description.
 * Input: specification + constraints
 * Output: generated code
 */

import type { KernelPromptConfig } from '../runtime';

export const SYNTHESIZE_KERNEL: KernelPromptConfig = {
  id: 'opr.synthesize.v1',
  op: 'synthesize',
  prompt: {
    tag: 'PromptDoc',
    v: 'frameir@1',
    parts: [{
      tag: 'PSystem',
      v: 'frameir@1',
      text: `You are a CODE SYNTHESIS kernel.

INPUT FORMAT:
{
  "spec": "natural language description of desired code",
  "language": "typescript",
  "signature": "function sortBy<T>(arr: T[], key: keyof T): T[]",  // optional
  "examples": [
    { "input": "[{a:2},{a:1}], 'a'", "output": "[{a:1},{a:2}]" }
  ],
  "constraints": {
    "pure": true,
    "no_mutation": true,
    "max_complexity": "O(n log n)"
  }
}

SYNTHESIS RULES:
- Generate WORKING code that satisfies all examples
- Respect all constraints
- Include type annotations
- Handle edge cases (empty input, null, etc.)

OUTPUT CONTRACT (strict JSON, no markdown):
{
  "kernel": "opr.synthesize.v1",
  "op": "synthesize",
  "ok": true,
  "result": {
    "code": "function sortBy<T>(arr: T[], key: keyof T): T[] {\\n  return [...arr].sort((a, b) => a[key] > b[key] ? 1 : -1);\\n}",
    "tests_passed": true,
    "complexity": { "time": "O(n log n)", "space": "O(n)" },
    "edge_cases_handled": ["empty array", "single element"]
  },
  "next_state": null,
  "effects": [],
  "diagnostics": {
    "alternatives_considered": ["using localeCompare for strings"],
    "assumptions": ["key values are comparable with >"]
  }
}`
    }]
  },
};

// ═══════════════════════════════════════════════════════════════════════════
// FILE: src/core/opr/kernels/validate.ts
// ═══════════════════════════════════════════════════════════════════════════

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

// ═══════════════════════════════════════════════════════════════════════════
// FILE: src/core/opr/kernels/plan.ts
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Planning Kernel (opr.plan.v1)
 *
 * Creates execution plans for goals.
 * Input: goal + available actions + constraints
 * Output: step-by-step plan
 */

import type { KernelPromptConfig } from '../runtime';

export const PLAN_KERNEL: KernelPromptConfig = {
  id: 'opr.plan.v1',
  op: 'plan',
  prompt: {
    tag: 'PromptDoc',
    v: 'frameir@1',
    parts: [{
      tag: 'PSystem',
      v: 'frameir@1',
      text: `You are a PLANNING kernel implementing goal-directed planning.

INPUT FORMAT:
{
  "goal": "deploy new feature to production",
  "initial_state": {
    "branch": "feature-x",
    "tests": "not_run",
    "reviewed": false
  },
  "actions": [
    { "name": "run_tests", "precond": [], "effect": {"tests": "passed"} },
    { "name": "request_review", "precond": ["tests=passed"], "effect": {"reviewed": true} },
    { "name": "merge", "precond": ["reviewed=true"], "effect": {"branch": "main"} },
    { "name": "deploy", "precond": ["branch=main"], "effect": {"deployed": true} }
  ],
  "constraints": {
    "max_steps": 10,
    "required_actions": ["run_tests"],
    "forbidden_actions": []
  }
}

PLANNING ALGORITHM:
- Work backwards from goal or forwards from initial state
- Ensure all preconditions are met
- Minimize total steps
- Respect all constraints

OUTPUT CONTRACT (strict JSON, no markdown):
{
  "kernel": "opr.plan.v1",
  "op": "plan",
  "ok": true,
  "result": {
    "plan": [
      { "step": 1, "action": "run_tests", "state_after": {"tests": "passed"} },
      { "step": 2, "action": "request_review", "state_after": {"tests": "passed", "reviewed": true} },
      { "step": 3, "action": "merge", "state_after": {"branch": "main", "reviewed": true} },
      { "step": 4, "action": "deploy", "state_after": {"deployed": true} }
    ],
    "achieves_goal": true,
    "total_steps": 4,
    "critical_path": ["run_tests", "request_review", "merge", "deploy"]
  },
  "next_state": {
    "iteration": 1,
    "plan_complete": true,
    "done": true
  },
  "effects": [],
  "diagnostics": {
    "alternatives_explored": 3,
    "dead_ends": 1
  }
}`
    }]
  },
};

// ═══════════════════════════════════════════════════════════════════════════
// FILE: src/core/opr/kernels/index.ts
// ═══════════════════════════════════════════════════════════════════════════

/**
 * OPR Kernel Registry
 *
 * Kernels are prompts that define how an LLM processes structured input.
 * Each kernel specifies:
 *   - Input format (what the "program" looks like)
 *   - Processing rules
 *   - Output schema (the JSON contract)
 */

import type { KernelPromptConfig } from '../runtime';

// Re-export all kernels
export { LOGIC_KERNEL } from './logic';
export { ANALYZE_KERNEL } from './analyze';
export { SEMANTIC_KERNEL } from './semantic';
export { CODE_REVIEW_KERNEL } from './codeReview';
export { TRANSFORM_KERNEL } from './transform';
export { EXTRACT_KERNEL } from './extract';
export { CLASSIFY_KERNEL } from './classify';
export { SYNTHESIZE_KERNEL } from './synthesize';
export { VALIDATE_KERNEL } from './validate';
export { PLAN_KERNEL } from './plan';

// Kernel registry
const kernels = new Map<string, KernelPromptConfig>();

export function registerKernel(config: KernelPromptConfig): void {
  kernels.set(config.id, config);
}

export function getKernel(id: string): KernelPromptConfig | undefined {
  return kernels.get(id);
}

export function listKernels(): string[] {
  return Array.from(kernels.keys());
}

// Auto-register all kernels on import
import { LOGIC_KERNEL } from './logic';
import { ANALYZE_KERNEL } from './analyze';
import { SEMANTIC_KERNEL } from './semantic';
import { CODE_REVIEW_KERNEL } from './codeReview';
import { TRANSFORM_KERNEL } from './transform';
import { EXTRACT_KERNEL } from './extract';
import { CLASSIFY_KERNEL } from './classify';
import { SYNTHESIZE_KERNEL } from './synthesize';
import { VALIDATE_KERNEL } from './validate';
import { PLAN_KERNEL } from './plan';

[
  LOGIC_KERNEL,
  ANALYZE_KERNEL,
  SEMANTIC_KERNEL,
  CODE_REVIEW_KERNEL,
  TRANSFORM_KERNEL,
  EXTRACT_KERNEL,
  CLASSIFY_KERNEL,
  SYNTHESIZE_KERNEL,
  VALIDATE_KERNEL,
  PLAN_KERNEL,
].forEach(registerKernel);

// ═══════════════════════════════════════════════════════════════════════════
// FILE: test/opr/unit/hash.spec.ts
// ═══════════════════════════════════════════════════════════════════════════

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

// ═══════════════════════════════════════════════════════════════════════════
// FILE: test/opr/unit/receipts.spec.ts
// ═══════════════════════════════════════════════════════════════════════════

import { describe, it, expect, beforeEach } from 'vitest';
import {
  createReceipt,
  verifyReceiptChain,
  InMemoryReceiptStore,
  ReceiptBuilder,
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
      prevReceiptHash: sha256Of('wrong-prev'), // Wrong link!
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
      prevReceiptHash: sha256Of('something'), // Should be null!
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

  it('R7b: getLast returns most recent receipt', () => {
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

    store.add(r1);
    expect(store.getLast()).toBe(r1);

    store.add(r2);
    expect(store.getLast()).toBe(r2);
  });

  it('R7c: clear removes all receipts', () => {
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

    store.add(r1);
    expect(store.count()).toBe(1);

    store.clear();
    expect(store.count()).toBe(0);
    expect(store.getAll()).toHaveLength(0);
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

  it('R8b: timeout creates receipt with null response', () => {
    const store = new InMemoryReceiptStore();
    const builder = new ReceiptBuilder(store, 'test', 'step');

    builder.timeout(1, sha256Of('req1'));

    const receipts = store.getAll();
    expect(receipts).toHaveLength(1);
    expect(receipts[0].status).toBe('TIMEOUT');
    expect(receipts[0].response_hash).toBeNull();
  });
});

// ═══════════════════════════════════════════════════════════════════════════
// FILE: test/opr/unit/retry.spec.ts
// ═══════════════════════════════════════════════════════════════════════════

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
      },
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

  it('RY2b: includes path for each violation', () => {
    const prompt = buildRepairPrompt([
      { path: '$.effects[0].type', code: 'MISSING_FIELD', message: 'Missing type' },
      { path: '$.next_state', code: 'WRONG_TYPE', message: 'Wrong type' },
    ]);

    expect(prompt).toContain('$.effects[0].type');
    expect(prompt).toContain('$.next_state');
  });
});

describe('shouldRetry', () => {
  it('RY3: returns false for KERNEL_MISMATCH', () => {
    const result = shouldRetry([
      { path: '$.kernel', code: 'KERNEL_MISMATCH', message: 'Wrong kernel' },
    ]);
    expect(result).toBe(false);
  });

  it('RY3b: returns false for OP_MISMATCH', () => {
    const result = shouldRetry([{ path: '$.op', code: 'OP_MISMATCH', message: 'Wrong op' }]);
    expect(result).toBe(false);
  });

  it('RY4: returns true for fixable violations', () => {
    const result = shouldRetry([
      { path: '$.ok', code: 'MISSING_FIELD', message: 'Missing field' },
      { path: '$.result', code: 'WRONG_TYPE', message: 'Wrong type' },
    ]);
    expect(result).toBe(true);
  });

  it('RY4b: returns true for NOT_JSON', () => {
    const result = shouldRetry([{ path: '$', code: 'NOT_JSON', message: 'Invalid JSON' }]);
    expect(result).toBe(true);
  });

  it('RY4c: returns true for NOT_OBJECT', () => {
    const result = shouldRetry([{ path: '$', code: 'NOT_OBJECT', message: 'Not an object' }]);
    expect(result).toBe(true);
  });

  it('RY4d: returns true for INVALID_VALUE', () => {
    const result = shouldRetry([{ path: '$.iteration', code: 'INVALID_VALUE', message: 'Bad value' }]);
    expect(result).toBe(true);
  });
});

// ═══════════════════════════════════════════════════════════════════════════
// FILE: test/opr/integration/runtime.spec.ts
// ═══════════════════════════════════════════════════════════════════════════

/**
 * OPR Runtime Integration Tests
 *
 * End-to-end tests that exercise the full runtime with scripted adapter
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { OprRuntime, type KernelPromptConfig } from '../../../src/core/opr/runtime';
import { InMemoryReceiptStore } from '../../../src/core/opr/receipts';
import { ScriptedOprAdapter } from '../../../src/core/opr/adapters/scripted';
import { verifyReceiptChain } from '../../../src/core/opr/receipts';

// Mock prompt doc for testing
const mockPrompt: any = {
  tag: 'PromptDoc',
  parts: [{ tag: 'System', content: 'You are a test kernel' }],
};

describe('OprRuntime Integration', () => {
  let receipts: InMemoryReceiptStore;
  let kernel: KernelPromptConfig;

  beforeEach(() => {
    receipts = new InMemoryReceiptStore();
    kernel = {
      id: 'test.kernel.v1',
      prompt: mockPrompt,
      op: 'step',
    };
  });

  describe('step() with scripted adapter', () => {
    it('RT1: successful step on first attempt', async () => {
      const validResponse = JSON.stringify({
        kernel: 'test.kernel.v1',
        op: 'step',
        ok: true,
        result: { message: 'success' },
        next_state: { iteration: 1 },
        effects: [],
        diagnostics: {},
      });

      const adapter = new ScriptedOprAdapter({ responses: [validResponse] });
      const runtime = new OprRuntime({
        kernel,
        adapter,
        receipts,
        budget: { maxAttempts: 3 },
      });

      const result = await runtime.step({ program: { test: true }, state: null });

      expect(result.tag).toBe('ok');
      expect(result.attempts).toBe(1);
      if (result.tag === 'ok') {
        expect(result.output.result).toEqual({ message: 'success' });
        expect(result.output.next_state).toEqual({ iteration: 1 });
      }
      expect(receipts.count()).toBe(1);
      expect(receipts.getAll()[0].status).toBe('OK');
    });

    it('RT2: retry with repair prompt on validation failure', async () => {
      // First response is invalid (missing fields)
      const invalidResponse = JSON.stringify({
        kernel: 'test.kernel.v1',
        op: 'step',
        // Missing: ok, result, next_state, effects, diagnostics
      });

      // Second response is valid
      const validResponse = JSON.stringify({
        kernel: 'test.kernel.v1',
        op: 'step',
        ok: true,
        result: { fixed: true },
        next_state: null,
        effects: [],
        diagnostics: {},
      });

      const adapter = new ScriptedOprAdapter({
        responses: [invalidResponse, validResponse],
      });

      const runtime = new OprRuntime({
        kernel,
        adapter,
        receipts,
        budget: { maxAttempts: 3 },
      });

      const result = await runtime.step({ program: {}, state: null });

      expect(result.tag).toBe('ok');
      expect(result.attempts).toBe(2);
      expect(receipts.count()).toBe(2);
      expect(receipts.getAll()[0].status).toBe('ERROR');
      expect(receipts.getAll()[1].status).toBe('OK');
    });

    it('RT3: budget exhausted after maxAttempts', async () => {
      // All responses are invalid
      const invalidResponse = JSON.stringify({ invalid: true });

      const adapter = new ScriptedOprAdapter({
        responses: [invalidResponse, invalidResponse, invalidResponse],
      });

      const runtime = new OprRuntime({
        kernel,
        adapter,
        receipts,
        budget: { maxAttempts: 3 },
      });

      const result = await runtime.step({ program: {}, state: null });

      expect(result.tag).toBe('budget-exhausted');
      expect(result.attempts).toBe(3);
      expect(receipts.count()).toBe(3);
    });

    it('RT4: handles JSON in markdown code blocks', async () => {
      const responseWithCodeBlock = '```json\n' + JSON.stringify({
        kernel: 'test.kernel.v1',
        op: 'step',
        ok: true,
        result: { extracted: true },
        next_state: null,
        effects: [],
        diagnostics: {},
      }) + '\n```';

      const adapter = new ScriptedOprAdapter({
        responses: [responseWithCodeBlock],
      });

      const runtime = new OprRuntime({
        kernel,
        adapter,
        receipts,
        budget: { maxAttempts: 3 },
      });

      const result = await runtime.step({ program: {}, state: null });

      expect(result.tag).toBe('ok');
      if (result.tag === 'ok') {
        expect(result.output.result).toEqual({ extracted: true });
      }
    });

    it('RT5: receipt chain is valid after multiple attempts', async () => {
      const invalidResponse = '{ not valid json';
      const validResponse = JSON.stringify({
        kernel: 'test.kernel.v1',
        op: 'step',
        ok: true,
        result: {},
        next_state: null,
        effects: [],
        diagnostics: {},
      });

      const adapter = new ScriptedOprAdapter({
        responses: [invalidResponse, invalidResponse, validResponse],
      });

      const runtime = new OprRuntime({
        kernel,
        adapter,
        receipts,
        budget: { maxAttempts: 5 },
      });

      await runtime.step({ program: {}, state: null });

      const chain = receipts.getAll();
      expect(chain.length).toBe(3);

      // Verify chain integrity
      const verification = verifyReceiptChain(chain);
      expect(verification.valid).toBe(true);

      // First receipt should have null prev_receipt_hash
      expect(chain[0].prev_receipt_hash).toBeNull();

      // Each subsequent receipt should link to previous
      expect(chain[1].prev_receipt_hash).toBe(chain[0].receipt_hash);
      expect(chain[2].prev_receipt_hash).toBe(chain[1].receipt_hash);
    });
  });

  describe('runToFixpoint() with scripted adapter', () => {
    it('RT6: terminates on done=true', async () => {
      const responses = [
        JSON.stringify({
          kernel: 'test.kernel.v1',
          op: 'step',
          ok: true,
          result: { step: 1 },
          next_state: { iteration: 1, done: false },
          effects: [],
          diagnostics: {},
        }),
        JSON.stringify({
          kernel: 'test.kernel.v1',
          op: 'step',
          ok: true,
          result: { step: 2 },
          next_state: { iteration: 2, done: true },
          effects: [],
          diagnostics: {},
        }),
      ];

      const adapter = new ScriptedOprAdapter({ responses });

      const runtime = new OprRuntime({
        kernel,
        adapter,
        receipts,
        budget: { maxAttempts: 3 },
      });

      const result = await runtime.runToFixpoint({ program: {}, state: null });

      expect(result.tag).toBe('ok');
      if (result.tag === 'ok') {
        expect(result.iterations).toBe(2);
        expect(result.finalState?.done).toBe(true);
        expect(result.results.length).toBe(2);
      }
    });

    it('RT7: terminates on next_state=null', async () => {
      const response = JSON.stringify({
        kernel: 'test.kernel.v1',
        op: 'step',
        ok: true,
        result: { complete: true },
        next_state: null,
        effects: [],
        diagnostics: {},
      });

      const adapter = new ScriptedOprAdapter({ responses: [response] });

      const runtime = new OprRuntime({
        kernel,
        adapter,
        receipts,
        budget: { maxAttempts: 3 },
      });

      const result = await runtime.runToFixpoint({ program: {}, state: null });

      expect(result.tag).toBe('ok');
      if (result.tag === 'ok') {
        expect(result.iterations).toBe(1);
        expect(result.finalState).toBeNull();
      }
    });

    it('RT8: reports error on step failure', async () => {
      const invalidResponse = JSON.stringify({ invalid: true });

      const adapter = new ScriptedOprAdapter({
        responses: [invalidResponse, invalidResponse, invalidResponse],
      });

      const runtime = new OprRuntime({
        kernel,
        adapter,
        receipts,
        budget: { maxAttempts: 3 },
      });

      const result = await runtime.runToFixpoint({ program: {}, state: null });

      expect(result.tag).toBe('error');
      if (result.tag === 'error') {
        expect(result.error.tag).toBe('budget-exhausted');
        expect(result.iterations).toBe(0);
      }
    });
  });

  describe('progress invariants', () => {
    it('RT9: enforces iteration monotonicity', async () => {
      const responses = [
        JSON.stringify({
          kernel: 'test.kernel.v1',
          op: 'step',
          ok: true,
          result: {},
          next_state: { iteration: 1 },
          effects: [],
          diagnostics: {},
        }),
        // Second step has non-increasing iteration - should trigger repair
        JSON.stringify({
          kernel: 'test.kernel.v1',
          op: 'step',
          ok: true,
          result: {},
          next_state: { iteration: 1 }, // Same as before, not increasing!
          effects: [],
          diagnostics: {},
        }),
        // Correct response
        JSON.stringify({
          kernel: 'test.kernel.v1',
          op: 'step',
          ok: true,
          result: {},
          next_state: { iteration: 2, done: true },
          effects: [],
          diagnostics: {},
        }),
      ];

      const adapter = new ScriptedOprAdapter({ responses });

      const runtime = new OprRuntime({
        kernel,
        adapter,
        receipts,
        budget: { maxAttempts: 5 },
        invariants: {
          iterationMonotonic: true,
          derivedMonotonic: false,
          deltaTermination: false,
        },
      });

      // First step
      const result1 = await runtime.step({ program: {}, state: null });
      expect(result1.tag).toBe('ok');

      // Second step with state from first
      if (result1.tag === 'ok') {
        const result2 = await runtime.step({
          program: {},
          state: result1.output.next_state,
        });
        // Should succeed after retry with correct response
        expect(result2.tag).toBe('ok');
        if (result2.tag === 'ok') {
          expect(result2.attempts).toBe(2); // Took 2 attempts
        }
      }
    });
  });

  describe('effects handling', () => {
    it('RT10: captures effects in output', async () => {
      const response = JSON.stringify({
        kernel: 'test.kernel.v1',
        op: 'step',
        ok: true,
        result: {},
        next_state: null,
        effects: [
          {
            type: 'callback.eval_lisp',
            idempotency_key: 'effect-1',
            payload: { expr: '(+ 1 2)' },
          },
          {
            type: 'callback.artifact.get',
            idempotency_key: 'effect-2',
            payload: { path: '/some/artifact' },
          },
        ],
        diagnostics: {},
      });

      const adapter = new ScriptedOprAdapter({ responses: [response] });

      const runtime = new OprRuntime({
        kernel,
        adapter,
        receipts,
        budget: { maxAttempts: 3 },
      });

      const result = await runtime.step({ program: {}, state: null });

      expect(result.tag).toBe('ok');
      if (result.tag === 'ok') {
        expect(result.output.effects.length).toBe(2);
        expect(result.output.effects[0].type).toBe('callback.eval_lisp');
        expect(result.output.effects[1].type).toBe('callback.artifact.get');
      }
    });
  });
});

// ═══════════════════════════════════════════════════════════════════════════
// FILE: bin/omega-repl.ts (OPR-related sections only)
// ═══════════════════════════════════════════════════════════════════════════

// --- OPR Imports (lines ~41-50) ---
// OPR imports
import { OprRuntime } from "../src/core/opr/runtime";
import { InMemoryReceiptStore as OprReceiptStore } from "../src/core/opr/receipts";
import { OpenAIOprAdapter } from "../src/core/opr/adapters/openai";
import { AnthropicOprAdapter } from "../src/core/opr/adapters/anthropic";
import { listKernels, getKernel } from "../src/core/opr/kernels";

// --- OPR Help Text (lines ~2089-2094) ---
    log("  OPR (Omega Protocol Runtime):");
    log("  :opr-list          — list available OPR kernels");
    log("  :opr-run <kernel> <json> — run kernel with program JSON");
    log("  :opr-receipts      — show OPR receipt chain for current session");
    log("  :opr-verify [file] — verify OPR receipt chain integrity");
    log("");

// --- OPR Commands Implementation (lines ~2678-2800) ---
  // OPR Commands
  if (trimmed === ":opr-list") {
    log("\nAvailable OPR Kernels:");
    log("======================");
    for (const id of listKernels()) {
      const kernel = getKernel(id);
      if (kernel) {
        log(`  ${id} (op: ${kernel.op})`);
      }
    }
    log("\nUse :opr-run <kernel-id> <program-json> to execute a kernel.");
    return { replState, output: output.join("\n"), shouldExit };
  }

  if (trimmed.startsWith(":opr-run ")) {
    const args = trimmed.slice(9).trim();
    const spaceIdx = args.indexOf(" ");
    if (spaceIdx === -1) {
      log("Usage: :opr-run <kernel-id> <program-json>");
      log("Example: :opr-run opr.classify.v1 {\"item\":\"error at line 42\",\"categories\":[\"bug\",\"feature\"]}");
      return { replState, output: output.join("\n"), shouldExit };
    }

    const kernelId = args.slice(0, spaceIdx);
    const programJson = args.slice(spaceIdx + 1).trim();

    const kernel = getKernel(kernelId);
    if (!kernel) {
      log(`Unknown kernel: ${kernelId}`);
      log("Use :opr-list to see available kernels.");
      return { replState, output: output.join("\n"), shouldExit };
    }

    let program: unknown;
    try {
      program = JSON.parse(programJson);
    } catch (e: any) {
      log(`Invalid JSON: ${e.message}`);
      return { replState, output: output.join("\n"), shouldExit };
    }

    // Get API key
    const openaiKey = process.env.OPENAI_API_KEY;
    const anthropicKey = process.env.ANTHROPIC_API_KEY;

    if (!openaiKey && !anthropicKey) {
      log("No API key found. Set OPENAI_API_KEY or ANTHROPIC_API_KEY.");
      return { replState, output: output.join("\n"), shouldExit };
    }

    // Create adapter
    const adapter = openaiKey
      ? new OpenAIOprAdapter({ apiKey: openaiKey, model: "gpt-4o-mini" })
      : new AnthropicOprAdapter({ apiKey: anthropicKey!, model: "claude-sonnet-4-20250514" });

    const receipts = new OprReceiptStore();
    const runtime = new OprRuntime({
      kernel,
      adapter,
      receipts,
      budget: { maxAttempts: 3 },
    });

    log(`\nRunning kernel: ${kernelId}`);
    log(`Program: ${JSON.stringify(program, null, 2)}`);
    log("\nCalling LLM...\n");

    try {
      const result = await runtime.step({ program, state: null });

      if (result.tag === "ok") {
        log("SUCCESS!");
        log(`\nResult: ${JSON.stringify(result.output.result, null, 2)}`);
        if (result.output.next_state) {
          log(`\nNext State: ${JSON.stringify(result.output.next_state, null, 2)}`);
        }
        if (result.output.effects && result.output.effects.length > 0) {
          log(`\nEffects: ${JSON.stringify(result.output.effects, null, 2)}`);
        }
        log(`\nAttempts: ${result.attempts}`);
      } else {
        log(`FAILED: ${result.tag}`);
        if ("error" in result) {
          log(`Error: ${(result.error as Error).message}`);
        }
      }

      // Store receipts in session
      (replState as any).oprReceipts = [
        ...((replState as any).oprReceipts || []),
        ...result.receipts,
      ];
      log(`\nReceipts: ${result.receipts.length} generated (use :opr-receipts to view)`);
    } catch (e: any) {
      log(`Error running kernel: ${e.message}`);
    }

    return { replState, output: output.join("\n"), shouldExit };
  }

  if (trimmed === ":opr-receipts") {
    const oprReceipts = (replState as any).oprReceipts;
    if (!oprReceipts || oprReceipts.length === 0) {
      log("No OPR receipts in current session.");
    } else {
      log("\nOPR Receipt Chain:");
      log("==================");
      for (let i = 0; i < oprReceipts.length; i++) {
        const r = oprReceipts[i];
        const statusIcon = r.status === 'OK' ? '[OK]' : '[X]';
        log(`${i + 1}. ${statusIcon} ${r.status} - ${r.kernel_id}:${r.op}`);
        log(`     Attempt: ${r.attempt}  Created: ${r.created_at}`);
        if (r.errors && r.errors.length > 0) {
          log(`     Errors: ${r.errors[0]}`);
        }
      }
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  if (trimmed === ":opr-verify" || trimmed.startsWith(":opr-verify ")) {
    const arg = trimmed.slice(12).trim();
    let receipts: any[];

    if (arg) {
      // Load from file
      try {
        const content = fs.readFileSync(arg, 'utf-8');
        receipts = JSON.parse(content);
      } catch (e: any) {
        log(`Error loading receipt file: ${e.message}`);
        return { replState, output: output.join("\n"), shouldExit };
      }
    } else {
      receipts = (replState as any).oprReceipts || [];
      if (receipts.length === 0) {
        log("No OPR receipts in session. Provide a file path to verify.");
        return { replState, output: output.join("\n"), shouldExit };
      }
    }

    // Simple chain verification
    let valid = true;
    let brokenAt = -1;
    let error = "";

    if (receipts.length > 0 && receipts[0].prev_receipt_hash !== null) {
      valid = false;
      brokenAt = 0;
      error = "First receipt should have null prev_receipt_hash";
    } else {
