// src/core/concurrency/critic.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Prompt 13: Concurrency critic for deadlock detection and race analysis

import type { Val, FiberId } from "../eval/values";
import type { Hash } from "../artifacts/hash";
import { sha256JSON } from "../artifacts/hash";
import type {
  FiberState,
  BlockReason,
  SchedulerState,
  ConcurrencyEvent,
  MutexState,
  IVarState,
  ChannelState,
} from "./types";
import { getMutexState, getIVarState, getChannelState } from "./sync";

// ─────────────────────────────────────────────────────────────────
// Deadlock detection
// ─────────────────────────────────────────────────────────────────

/**
 * DeadlockInfo: Information about a detected deadlock.
 */
export type DeadlockInfo = {
  /** Fibers involved in the deadlock */
  fibers: FiberId[];
  /** Wait-for edges (fiber -> what it's waiting for) */
  waitForGraph: Map<FiberId, { resource: string; holder?: FiberId }>;
  /** The cycle in the wait-for graph */
  cycle: FiberId[];
  /** Human-readable description */
  description: string;
  /** Timestamp of detection */
  detectedAt: number;
};

/**
 * Build a wait-for graph from the scheduler state.
 */
export function buildWaitForGraph(
  scheduler: SchedulerState
): Map<FiberId, { resource: string; holder?: FiberId }> {
  const graph = new Map<FiberId, { resource: string; holder?: FiberId }>();

  for (const [fiberId, fiber] of scheduler.fibers) {
    if (fiber.status !== "blocked" || !fiber.blockReason) continue;

    const reason = fiber.blockReason;
    switch (reason.tag) {
      case "join": {
        graph.set(fiberId, { resource: `fiber:${reason.fiberId}`, holder: reason.fiberId });
        break;
      }

      case "mutex": {
        const mutex = getMutexState(reason.mutexId);
        graph.set(fiberId, {
          resource: `mutex:${reason.mutexId}`,
          holder: mutex?.holder,
        });
        break;
      }

      case "ivar": {
        // IVars don't have holders in the same sense
        graph.set(fiberId, { resource: `ivar:${reason.ivarId}` });
        break;
      }

      case "channel": {
        graph.set(fiberId, {
          resource: `channel:${reason.op}:${reason.channelId}`,
        });
        break;
      }

      case "yield": {
        // Yielded fibers aren't really blocked
        break;
      }
    }
  }

  return graph;
}

/**
 * Detect cycles in the wait-for graph (classic deadlock detection).
 */
export function detectCycle(
  waitForGraph: Map<FiberId, { resource: string; holder?: FiberId }>
): FiberId[] | undefined {
  const visited = new Set<FiberId>();
  const stack = new Set<FiberId>();
  const path: FiberId[] = [];

  function dfs(fiberId: FiberId): FiberId[] | undefined {
    if (stack.has(fiberId)) {
      // Found a cycle
      const cycleStart = path.indexOf(fiberId);
      return path.slice(cycleStart);
    }

    if (visited.has(fiberId)) {
      return undefined;
    }

    visited.add(fiberId);
    stack.add(fiberId);
    path.push(fiberId);

    const edge = waitForGraph.get(fiberId);
    if (edge?.holder !== undefined) {
      const cycle = dfs(edge.holder);
      if (cycle) return cycle;
    }

    path.pop();
    stack.delete(fiberId);
    return undefined;
  }

  for (const fiberId of waitForGraph.keys()) {
    const cycle = dfs(fiberId);
    if (cycle) return cycle;
  }

  return undefined;
}

/**
 * Detect deadlocks in the scheduler.
 */
export function detectDeadlock(scheduler: SchedulerState): DeadlockInfo | undefined {
  // Build wait-for graph
  const waitForGraph = buildWaitForGraph(scheduler);

  // Detect cycle
  const cycle = detectCycle(waitForGraph);
  if (!cycle || cycle.length === 0) {
    return undefined;
  }

  // Build deadlock info
  const description = buildDeadlockDescription(cycle, waitForGraph, scheduler);

  return {
    fibers: cycle,
    waitForGraph,
    cycle,
    description,
    detectedAt: Date.now(),
  };
}

/**
 * Build a human-readable description of a deadlock.
 */
function buildDeadlockDescription(
  cycle: FiberId[],
  waitForGraph: Map<FiberId, { resource: string; holder?: FiberId }>,
  scheduler: SchedulerState
): string {
  const parts: string[] = ["Deadlock detected:"];

  for (let i = 0; i < cycle.length; i++) {
    const fiberId = cycle[i];
    const nextFiberId = cycle[(i + 1) % cycle.length];
    const fiber = scheduler.fibers.get(fiberId);
    const edge = waitForGraph.get(fiberId);

    const fiberName = fiber?.name ?? `fiber-${fiberId}`;
    parts.push(`  ${fiberName} waiting on ${edge?.resource ?? "unknown"}`);
  }

  return parts.join("\n");
}

// ─────────────────────────────────────────────────────────────────
// Race condition detection (static analysis)
// ─────────────────────────────────────────────────────────────────

/**
 * PotentialRace: A potential race condition.
 */
export type PotentialRace = {
  /** Type of race */
  kind: "read-write" | "write-write";
  /** Resource being accessed */
  resource: string;
  /** Fibers involved */
  fibers: FiberId[];
  /** Description */
  description: string;
};

/**
 * AccessRecord: Record of a resource access.
 */
export type AccessRecord = {
  fiberId: FiberId;
  resource: string;
  operation: "read" | "write";
  timestamp: number;
  protected: boolean;  // Was the access protected by a mutex?
};

/**
 * Analyze events for potential race conditions.
 */
export function analyzeRaces(
  events: ConcurrencyEvent[],
  accessRecords: AccessRecord[]
): PotentialRace[] {
  const races: PotentialRace[] = [];

  // Group accesses by resource
  const byResource = new Map<string, AccessRecord[]>();
  for (const record of accessRecords) {
    const existing = byResource.get(record.resource) ?? [];
    existing.push(record);
    byResource.set(record.resource, existing);
  }

  // Check each resource for unprotected concurrent accesses
  for (const [resource, accesses] of byResource) {
    // Filter to unprotected accesses
    const unprotected = accesses.filter(a => !a.protected);
    if (unprotected.length < 2) continue;

    // Check for conflicting accesses from different fibers
    const writes = unprotected.filter(a => a.operation === "write");
    const reads = unprotected.filter(a => a.operation === "read");

    // Write-write races
    const writeFibers = new Set(writes.map(w => w.fiberId));
    if (writeFibers.size > 1) {
      races.push({
        kind: "write-write",
        resource,
        fibers: Array.from(writeFibers),
        description: `Multiple fibers writing to ${resource} without synchronization`,
      });
    }

    // Read-write races
    if (writes.length > 0 && reads.length > 0) {
      const readFibers = new Set(reads.map(r => r.fiberId));
      for (const write of writes) {
        for (const readFiber of readFibers) {
          if (readFiber !== write.fiberId) {
            races.push({
              kind: "read-write",
              resource,
              fibers: [write.fiberId, readFiber],
              description: `Fiber ${write.fiberId} writes to ${resource} while fiber ${readFiber} reads without synchronization`,
            });
          }
        }
      }
    }
  }

  return races;
}

// ─────────────────────────────────────────────────────────────────
// Schedule exploration (for finding bugs)
// ─────────────────────────────────────────────────────────────────

/**
 * ScheduleExplorationConfig: Configuration for schedule exploration.
 */
export type ScheduleExplorationConfig = {
  /** Maximum number of schedules to explore */
  maxSchedules: number;
  /** Maximum steps per schedule */
  maxStepsPerSchedule: number;
  /** Random seed for deterministic exploration */
  seed: number;
  /** Stop on first failure? */
  stopOnFirstFailure: boolean;
};

export const DEFAULT_EXPLORATION_CONFIG: ScheduleExplorationConfig = {
  maxSchedules: 100,
  maxStepsPerSchedule: 10000,
  seed: 12345,
  stopOnFirstFailure: true,
};

/**
 * ScheduleExplorationResult: Result of schedule exploration.
 */
export type ScheduleExplorationResult = {
  /** Number of schedules explored */
  schedulesExplored: number;
  /** Schedules that led to deadlock */
  deadlockSchedules: Array<{ decisions: number[]; info: DeadlockInfo }>;
  /** Schedules that led to errors */
  errorSchedules: Array<{ decisions: number[]; fiberId: FiberId; error: string }>;
  /** All unique outcomes observed */
  outcomes: Map<Hash, { count: number; decisions: number[] }>;
  /** Exploration statistics */
  stats: {
    totalSteps: number;
    deadlocks: number;
    errors: number;
    successful: number;
  };
};

/**
 * Explore different schedules to find concurrency bugs.
 * This is a simplified DPOR/PCT-style exploration.
 */
export function exploreSchedules(
  createScheduler: () => { scheduler: SchedulerState; runFn: () => void },
  config: Partial<ScheduleExplorationConfig> = {}
): ScheduleExplorationResult {
  const fullConfig: ScheduleExplorationConfig = {
    ...DEFAULT_EXPLORATION_CONFIG,
    ...config,
  };

  const result: ScheduleExplorationResult = {
    schedulesExplored: 0,
    deadlockSchedules: [],
    errorSchedules: [],
    outcomes: new Map(),
    stats: {
      totalSteps: 0,
      deadlocks: 0,
      errors: 0,
      successful: 0,
    },
  };

  // Simple random exploration with different seeds
  for (let i = 0; i < fullConfig.maxSchedules; i++) {
    const seed = fullConfig.seed + i;
    result.schedulesExplored++;

    // Create scheduler with random policy
    const { scheduler, runFn } = createScheduler();
    scheduler.policy = { tag: "Random", seed };

    try {
      runFn();
    } catch {
      // Ignore exceptions from runFn
    }

    result.stats.totalSteps += scheduler.stepCount;

    // Check for deadlock
    const deadlock = detectDeadlock(scheduler);
    if (deadlock) {
      result.stats.deadlocks++;
      const decisions = scheduler.decisions.map(d => d.decisionIndex);
      result.deadlockSchedules.push({ decisions, info: deadlock });

      if (fullConfig.stopOnFirstFailure) {
        break;
      }
      continue;
    }

    // Check for errors
    let hasError = false;
    for (const [fiberId, fiber] of scheduler.fibers) {
      if (fiber.status === "error") {
        result.stats.errors++;
        const decisions = scheduler.decisions.map(d => d.decisionIndex);
        result.errorSchedules.push({
          decisions,
          fiberId,
          error: fiber.error ?? "Unknown error",
        });
        hasError = true;

        if (fullConfig.stopOnFirstFailure) {
          break;
        }
      }
    }

    if (hasError && fullConfig.stopOnFirstFailure) {
      break;
    }

    if (!hasError && !deadlock) {
      result.stats.successful++;

      // Record outcome
      const outcomeHash = hashSchedulerOutcome(scheduler);
      const existing = result.outcomes.get(outcomeHash);
      if (existing) {
        existing.count++;
      } else {
        const decisions = scheduler.decisions.map(d => d.decisionIndex);
        result.outcomes.set(outcomeHash, { count: 1, decisions });
      }
    }
  }

  return result;
}

/**
 * Hash the outcome of a scheduler run for comparison.
 */
function hashSchedulerOutcome(scheduler: SchedulerState): Hash {
  const results: Array<[FiberId, string]> = [];
  for (const [id, fiber] of scheduler.fibers) {
    if (fiber.status === "done" && fiber.result) {
      results.push([id, sha256JSON(fiber.result)]);
    }
  }
  results.sort((a, b) => a[0] - b[0]);
  return sha256JSON(results);
}

// ─────────────────────────────────────────────────────────────────
// Concurrency event analysis
// ─────────────────────────────────────────────────────────────────

/**
 * Summarize concurrency events for debugging.
 */
export function summarizeEvents(events: ConcurrencyEvent[]): {
  spawns: number;
  yields: number;
  joins: number;
  mutexOps: number;
  ivarOps: number;
  channelOps: number;
  deadlocks: number;
  errors: number;
} {
  const summary = {
    spawns: 0,
    yields: 0,
    joins: 0,
    mutexOps: 0,
    ivarOps: 0,
    channelOps: 0,
    deadlocks: 0,
    errors: 0,
  };

  for (const event of events) {
    switch (event.tag) {
      case "spawn":
        summary.spawns++;
        break;
      case "yield":
        summary.yields++;
        break;
      case "join":
        summary.joins++;
        break;
      case "mutexLock":
      case "mutexUnlock":
      case "mutexBlock":
        summary.mutexOps++;
        break;
      case "ivarPut":
      case "ivarTake":
      case "ivarBlock":
        summary.ivarOps++;
        break;
      case "chanSend":
      case "chanRecv":
        summary.channelOps++;
        break;
      case "deadlock":
        summary.deadlocks++;
        break;
      case "error":
        summary.errors++;
        break;
    }
  }

  return summary;
}

/**
 * Find events related to a specific fiber.
 */
export function filterEventsByFiber(
  events: ConcurrencyEvent[],
  fiberId: FiberId
): ConcurrencyEvent[] {
  return events.filter(event => {
    switch (event.tag) {
      case "spawn":
        return event.fiberId === fiberId || event.parentId === fiberId;
      case "yield":
      case "join":
      case "done":
      case "error":
      case "mutexLock":
      case "mutexUnlock":
      case "mutexBlock":
      case "ivarPut":
      case "ivarTake":
      case "ivarBlock":
      case "chanSend":
      case "chanRecv":
        return event.fiberId === fiberId;
      case "schedule":
        return event.decision.chosenFiberId === fiberId;
      case "deadlock":
        return event.blocked.includes(fiberId);
      default:
        return false;
    }
  });
}

// ─────────────────────────────────────────────────────────────────
// Diagnostic reporting
// ─────────────────────────────────────────────────────────────────

/**
 * ConcurrencyDiagnostic: A diagnostic report about concurrency issues.
 */
export type ConcurrencyDiagnostic = {
  severity: "info" | "warning" | "error";
  code: string;
  message: string;
  fiberId?: FiberId;
  resource?: string;
  suggestion?: string;
};

/**
 * Generate diagnostics from scheduler state and events.
 */
export function generateDiagnostics(
  scheduler: SchedulerState,
  events: ConcurrencyEvent[]
): ConcurrencyDiagnostic[] {
  const diagnostics: ConcurrencyDiagnostic[] = [];

  // Check for deadlock
  const deadlock = detectDeadlock(scheduler);
  if (deadlock) {
    diagnostics.push({
      severity: "error",
      code: "DEADLOCK",
      message: deadlock.description,
      suggestion: "Consider reordering lock acquisitions or using try-lock with timeout",
    });
  }

  // Check for starvation (fibers that haven't run in a while)
  const now = Date.now();
  for (const [fiberId, fiber] of scheduler.fibers) {
    if (fiber.status === "ready" && fiber.stepCount === 0) {
      const age = now - fiber.createdAt;
      if (age > 1000) {  // More than 1 second
        diagnostics.push({
          severity: "warning",
          code: "STARVATION",
          message: `Fiber ${fiber.name ?? fiberId} has been ready but not scheduled for ${age}ms`,
          fiberId,
          suggestion: "Consider using a fair scheduling policy",
        });
      }
    }
  }

  // Check for long-running fibers
  for (const [fiberId, fiber] of scheduler.fibers) {
    if (fiber.stepCount > 10000) {
      diagnostics.push({
        severity: "warning",
        code: "LONG_RUNNING",
        message: `Fiber ${fiber.name ?? fiberId} has run ${fiber.stepCount} steps`,
        fiberId,
        suggestion: "Consider adding yield points for better interleaving",
      });
    }
  }

  return diagnostics;
}
