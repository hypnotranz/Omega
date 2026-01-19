// src/core/oracle/ctxReceipts.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Patch Set C3: Context economics gateway (snapshot/compress/hydrate)

import type { Hash } from "../artifacts/hash";
import { sha256JSON } from "../artifacts/hash";
import type { EnvRef, StateRef } from "./protocol";

export type CtxReceipt = {
  tag: "CtxReceipt";
  rid: Hash;
  kind: "snapshot" | "compress";
  envRef: EnvRef;
  stateRef?: StateRef;
  meta?: unknown;
  timeMs: number;
};

export type CtxReceiptRepoOptions = {
  /**
   * Deterministic mode for testing.
   * If true, uses counter instead of timestamp in ID generation.
   */
  deterministic?: boolean;
  /** Optional prefix for IDs (useful for test isolation). */
  prefix?: string;
};

export class CtxReceiptRepo {
  private m = new Map<Hash, CtxReceipt>();
  private counter = 0;
  private deterministic: boolean;
  private prefix: string;

  constructor(opts: CtxReceiptRepoOptions = {}) {
    this.deterministic = opts.deterministic ?? false;
    this.prefix = opts.prefix ?? "";
  }

  private generateId(kind: string, data: unknown): { rid: Hash; t0: number } {
    const t0 = Date.now();
    if (this.deterministic) {
      const rid = sha256JSON({ kind, data, seq: ++this.counter });
      return { rid: this.prefix + rid, t0 };
    } else {
      const rid = sha256JSON({ kind, data, t0 });
      return { rid, t0 };
    }
  }

  snapshot(envRef: EnvRef, stateRef: StateRef | undefined, meta: unknown): CtxReceipt {
    const { rid, t0 } = this.generateId("snapshot", { envRef, stateRef, meta });
    const r: CtxReceipt = { tag: "CtxReceipt", rid, kind: "snapshot", envRef, stateRef, meta, timeMs: 0 };
    r.timeMs = this.deterministic ? 0 : Date.now() - t0;
    this.m.set(rid, r);
    return r;
  }

  compress(envRef: EnvRef, meta: unknown): CtxReceipt {
    const { rid, t0 } = this.generateId("compress", { envRef, meta });
    const r: CtxReceipt = { tag: "CtxReceipt", rid, kind: "compress", envRef, meta, timeMs: 0 };
    r.timeMs = this.deterministic ? 0 : Date.now() - t0;
    this.m.set(rid, r);
    return r;
  }

  get(rid: Hash): CtxReceipt | undefined {
    return this.m.get(rid);
  }

  has(rid: Hash): boolean {
    return this.m.has(rid);
  }

  clear(): void {
    this.m.clear();
    this.counter = 0;
  }

  /** Reset to initial state with new options. */
  reset(opts: CtxReceiptRepoOptions = {}): void {
    this.clear();
    this.deterministic = opts.deterministic ?? this.deterministic;
    this.prefix = opts.prefix ?? this.prefix;
  }

  /** Get all stored receipts (for debugging/testing). */
  all(): CtxReceipt[] {
    return Array.from(this.m.values());
  }

  /** Get the number of stored receipts. */
  size(): number {
    return this.m.size;
  }
}
