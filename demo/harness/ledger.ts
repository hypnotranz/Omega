// demo/harness/ledger.ts
// Demo Ledger for event recording

import { createHash } from "crypto";
import type { DemoLedger, LedgerEvent, LedgerEventType } from "./types";

/**
 * Create a demo ledger for event recording.
 */
export function createDemoLedger(): DemoLedger {
  let events: LedgerEvent[] = [];
  let seq = 0;
  const snapshots = new Map<string, LedgerEvent[]>();

  function generateId(): string {
    return `evt-${seq}-${Date.now().toString(36)}`;
  }

  return {
    record(type: LedgerEventType, data: unknown, parentId?: string): string {
      seq++;
      const event: LedgerEvent = {
        id: generateId(),
        seq,
        type,
        timestamp: Date.now(),
        data,
        parentId,
      };
      events.push(event);
      return event.id;
    },

    getEvents(): LedgerEvent[] {
      return [...events];
    },

    getEventsByType(type: LedgerEventType): LedgerEvent[] {
      return events.filter(e => e.type === type);
    },

    getDigest(): string {
      const content = JSON.stringify(events.map(e => ({
        type: e.type,
        data: e.data,
      })));
      return createHash("sha256").update(content).digest("hex").slice(0, 16);
    },

    reset(): void {
      events = [];
      seq = 0;
      snapshots.clear();
    },

    snapshot(): string {
      const snapshotId = `snap-${Date.now().toString(36)}`;
      snapshots.set(snapshotId, [...events]);
      return snapshotId;
    },

    restore(snapshotId: string): void {
      const snapshot = snapshots.get(snapshotId);
      if (snapshot) {
        events = [...snapshot];
        seq = events.length;
      }
    },
  };
}

// ─────────────────────────────────────────────────────────────────
// Ledger Analysis Utilities
// ─────────────────────────────────────────────────────────────────

/**
 * Count events by type.
 */
export function countEventsByType(
  ledger: DemoLedger
): Record<LedgerEventType, number> {
  const counts: Partial<Record<LedgerEventType, number>> = {};
  for (const event of ledger.getEvents()) {
    counts[event.type] = (counts[event.type] ?? 0) + 1;
  }
  return counts as Record<LedgerEventType, number>;
}

/**
 * Find all commit events.
 */
export function findCommits(ledger: DemoLedger): LedgerEvent[] {
  return ledger.getEvents().filter(
    e => e.type === "commit.success" || e.type === "commit.denied"
  );
}

/**
 * Find all backtrack events.
 */
export function findBacktracks(ledger: DemoLedger): LedgerEvent[] {
  return ledger.getEvents().filter(e => e.type === "amb.backtrack");
}

/**
 * Find all generic.miss events.
 */
export function findGenericMisses(ledger: DemoLedger): LedgerEvent[] {
  return ledger.getEvents().filter(e => e.type === "generic.miss");
}

/**
 * Find all method installations.
 */
export function findMethodInstalls(ledger: DemoLedger): LedgerEvent[] {
  return ledger.getEvents().filter(e => e.type === "generic.install");
}

/**
 * Find all constraint violations.
 */
export function findConstraintViolations(ledger: DemoLedger): LedgerEvent[] {
  return ledger.getEvents().filter(e => e.type === "constraint.violation");
}

/**
 * Find all constraint repairs.
 */
export function findConstraintRepairs(ledger: DemoLedger): LedgerEvent[] {
  return ledger.getEvents().filter(e => e.type === "constraint.repair");
}

/**
 * Build event tree (events with their children).
 */
export function buildEventTree(ledger: DemoLedger): Map<string, LedgerEvent[]> {
  const tree = new Map<string, LedgerEvent[]>();
  tree.set("root", []);

  for (const event of ledger.getEvents()) {
    const parentId = event.parentId ?? "root";
    if (!tree.has(parentId)) {
      tree.set(parentId, []);
    }
    tree.get(parentId)!.push(event);
  }

  return tree;
}

/**
 * Format ledger as human-readable trace.
 */
export function formatLedgerTrace(ledger: DemoLedger): string {
  const lines: string[] = [];
  const tree = buildEventTree(ledger);

  function formatEvent(event: LedgerEvent, indent: number): void {
    const prefix = "  ".repeat(indent);
    const dataStr = typeof event.data === "string"
      ? event.data
      : JSON.stringify(event.data).slice(0, 60);
    lines.push(`${prefix}[${event.type}] ${dataStr}`);

    const children = tree.get(event.id) ?? [];
    for (const child of children) {
      formatEvent(child, indent + 1);
    }
  }

  const roots = tree.get("root") ?? [];
  for (const root of roots) {
    formatEvent(root, 0);
  }

  return lines.join("\n");
}
