// src/core/oracle/snapshots.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-14.md

import type { Hash } from "../artifacts/hash";
import type { Env } from "../eval/env";
import type { Store } from "../eval/store";
import type { State } from "../eval/machine";

export type EnvSnapshot = { env: Env; store: Store };
export type StateSnapshot = { state: State };

export type SnapshotRepoOptions = {
  /** Optional prefix for IDs (useful for test isolation). */
  prefix?: string;
  /** Starting counter value (useful for deterministic testing). */
  startCounter?: number;
};

export class SnapshotRepo {
  private envSnaps = new Map<Hash, EnvSnapshot>();
  private stateSnaps = new Map<Hash, StateSnapshot>();

  // Use incrementing IDs since Env/Store are Maps which don't serialize with JSON.stringify
  private envCounter: number;
  private stateCounter: number;
  private prefix: string;

  constructor(opts: SnapshotRepoOptions = {}) {
    this.prefix = opts.prefix ?? "";
    this.envCounter = opts.startCounter ?? 0;
    this.stateCounter = opts.startCounter ?? 0;
  }

  /**
   * Reset the repository to initial state.
   * Useful for testing to ensure deterministic IDs.
   */
  reset(opts: SnapshotRepoOptions = {}): void {
    this.envSnaps.clear();
    this.stateSnaps.clear();
    this.prefix = opts.prefix ?? this.prefix;
    this.envCounter = opts.startCounter ?? 0;
    this.stateCounter = opts.startCounter ?? 0;
  }

  /** Get the current prefix. */
  getPrefix(): string {
    return this.prefix;
  }

  /** Set the prefix (useful for test isolation). */
  setPrefix(prefix: string): void {
    this.prefix = prefix;
  }

  putEnv(s: EnvSnapshot): Hash {
    const h = `${this.prefix}env:${++this.envCounter}`;
    this.envSnaps.set(h, s);
    return h;
  }
  getEnv(h: Hash): EnvSnapshot {
    const s = this.envSnaps.get(h);
    if (!s) throw new Error(`SnapshotRepo: missing EnvRef ${h}`);
    return s;
  }

  putState(s: StateSnapshot): Hash {
    const h = `${this.prefix}state:${++this.stateCounter}`;
    this.stateSnaps.set(h, s);
    return h;
  }
  getState(h: Hash): StateSnapshot {
    const s = this.stateSnaps.get(h);
    if (!s) throw new Error(`SnapshotRepo: missing StateRef ${h}`);
    return s;
  }

  /** Get current ID counters (for debugging/testing). */
  getCounters(): { envCounter: number; stateCounter: number } {
    return { envCounter: this.envCounter, stateCounter: this.stateCounter };
  }
}
