// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-14.md
// Test helpers for Oracle Protocol

import type { CommitAdapter } from "../../src/core/effects/runtimeImpl";
import { RuntimeImpl } from "../../src/core/effects/runtimeImpl";
import type { Val } from "../../src/core/eval/values";
import { VUnit } from "../../src/core/eval/values";

// Re-export the real Oracle Protocol components
export { ScriptedOracleAdapter } from "../../src/core/oracle/scriptedOracle";
export { SnapshotRepo } from "../../src/core/oracle/snapshots";
export { InMemoryReceiptStore } from "../../src/core/oracle/receipts";

// Import for internal use
import { ScriptedOracleAdapter } from "../../src/core/oracle/scriptedOracle";
import { SnapshotRepo } from "../../src/core/oracle/snapshots";
import { InMemoryReceiptStore } from "../../src/core/oracle/receipts";

export const mockCommit: CommitAdapter = {
  async commit(_payload: Val): Promise<Val> {
    return VUnit;
  }
};

/**
 * Create a fully-configured RuntimeImpl with Oracle Protocol support.
 * Use this in tests instead of constructing RuntimeImpl directly.
 */
export function createTestRuntime(): RuntimeImpl {
  const oracle = new ScriptedOracleAdapter();
  const snapshots = new SnapshotRepo();
  const receipts = new InMemoryReceiptStore("off");
  return new RuntimeImpl(oracle, snapshots, receipts, mockCommit);
}
