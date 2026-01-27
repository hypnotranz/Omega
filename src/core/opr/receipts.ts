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
