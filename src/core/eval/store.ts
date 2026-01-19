// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-3.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import type { Val } from "./values";

/**
 * StoreAddr is an explicit location (CESK-style). This enables mutation without
 * destroying lexical environment persistence.
 */
export type StoreAddr = number;

/**
 * Store is required to be *persistent* with respect to captured continuations.
 * "Persistent" here means: any snapshot referenced by a resumption must not
 * be affected by future writes in other branches.
 */
export interface Store {
  readonly next: number;

  /** Allocate a new cell initialized with v. Returns [newStore, addr]. */
  alloc(v: Val): [Store, StoreAddr];

  /** Read cell at addr; must throw on invalid addr in strict mode. */
  read(addr: StoreAddr): Val;

  /** Write cell; returns a new Store (or same Store if structural sharing). */
  write(addr: StoreAddr, v: Val): Store;

  /** Snapshot suitable for storing inside a multi-shot resumption. */
  snapshot(): Store;

  /** Debug: stable digest for receipts/differential tests (content-addressed). */
  digest(): string;
}

/**
 * Reference-grade Store implementation: copy-on-write Map snapshot.
 * Correct semantics, not fast. Replace with HAMT for production.
 */
export class COWStore implements Store {
  public readonly next: number;
  private readonly cells: Map<StoreAddr, Val>;

  constructor(next = 0, cells?: Map<StoreAddr, Val>) {
    this.next = next;
    this.cells = cells ?? new Map();
  }

  alloc(v: Val): [Store, StoreAddr] {
    const addr = this.next;
    const cells2 = new Map(this.cells);
    cells2.set(addr, v);
    return [new COWStore(addr + 1, cells2), addr];
  }

  read(addr: StoreAddr): Val {
    const v = this.cells.get(addr);
    if (v === undefined) {
      throw new Error(`Store.read: invalid addr ${addr}`);
    }
    return v;
  }

  write(addr: StoreAddr, v: Val): Store {
    if (!this.cells.has(addr)) {
      throw new Error(`Store.write: invalid addr ${addr}`);
    }
    const cells2 = new Map(this.cells);
    cells2.set(addr, v);
    return new COWStore(this.next, cells2);
  }

  snapshot(): Store {
    // Deep enough: Val is assumed immutable by convention; if not, you must deep-clone Val.
    return new COWStore(this.next, new Map(this.cells));
  }

  digest(): string {
    // Deterministic (slow) digest for testing.
    const entries = Array.from(this.cells.entries()).sort((a, b) => a[0] - b[0]);
    return JSON.stringify({ next: this.next, entries });
  }
}