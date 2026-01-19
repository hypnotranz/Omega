// src/core/oracle/receipts.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-14.md

import type { Hash } from "../artifacts/hash";
import { sha256JSON } from "../artifacts/hash";
import type { OracleReq, OracleResp } from "./protocol";

export type ReceiptMode = "off" | "record" | "replay";

export type OracleReceipt = {
  key: Hash;
  req: OracleReq;
  resp: OracleResp;
  timeMs?: number;
};

export interface ReceiptStore {
  mode: ReceiptMode;
  get(key: Hash): OracleReceipt | undefined;
  put(r: OracleReceipt): void;
}

/** In-memory implementation for now (good enough to validate protocol). */
export class InMemoryReceiptStore implements ReceiptStore {
  mode: ReceiptMode;
  private m = new Map<Hash, OracleReceipt>();

  constructor(mode: ReceiptMode = "off") {
    this.mode = mode;
  }

  get(key: Hash): OracleReceipt | undefined {
    return this.m.get(key);
  }

  put(r: OracleReceipt): void {
    this.m.set(r.key, r);
  }

  /**
   * Reset the store to initial state.
   * Useful for testing to ensure clean state between tests.
   */
  reset(mode?: ReceiptMode): void {
    this.m.clear();
    if (mode !== undefined) {
      this.mode = mode;
    }
  }

  /** Get all stored receipts (for debugging/testing). */
  all(): OracleReceipt[] {
    return Array.from(this.m.values());
  }

  /** Get the number of stored receipts. */
  size(): number {
    return this.m.size;
  }

  /** Check if a receipt exists for the given key. */
  has(key: Hash): boolean {
    return this.m.has(key);
  }
}

/** Deterministic key for request (you can extend with policyDigest, envelope, ctxDigest, etc.). */
export function receiptKey(req: OracleReq): Hash {
  return sha256JSON(req);
}
