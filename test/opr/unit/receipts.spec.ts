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
