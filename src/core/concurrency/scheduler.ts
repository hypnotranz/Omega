// src/core/concurrency/scheduler.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Prompt 13: Deterministic fiber scheduler with policy-based scheduling

import type { Val, FiberId, FiberVal } from "../eval/values";
import { VUnit } from "../eval/values";
import type { State } from "../eval/machine";
import { sha256JSON } from "../artifacts/hash";
import {
  type FiberState,
  type FiberStatus,
  type BlockReason,
  type SchedulePolicy,
  type ScheduleDecision,
  type SchedulerState,
  type SchedulerStatus,
  type ConcurrencyEvent,
  type ConcurrencyBudget,
  DEFAULT_CONCURRENCY_BUDGET,
  makeFiber,
} from "./types";

// ─────────────────────────────────────────────────────────────────
// Scheduler registry (global state for managing schedulers)
// ─────────────────────────────────────────────────────────────────

const schedulerRegistry = new Map<string, SchedulerState>();
let nextSchedulerId = 0;
let nextFiberId = 0;

/**
 * Generate a unique scheduler ID.
 */
function genSchedulerId(): string {
  return `sched-${nextSchedulerId++}`;
}

/**
 * Generate a unique fiber ID.
 */
export function genFiberId(): FiberId {
  return nextFiberId++;
}

/**
 * Reset the scheduler registry (for testing).
 */
export function resetSchedulerRegistry(): void {
  schedulerRegistry.clear();
  nextSchedulerId = 0;
  nextFiberId = 0;
}

// ─────────────────────────────────────────────────────────────────
// Scheduler creation and management
// ─────────────────────────────────────────────────────────────────

/**
 * Create a new scheduler with the given policy.
 */
export function createScheduler(
  policy: SchedulePolicy = { tag: "RoundRobin" },
  name?: string
): SchedulerState {
  const id = genSchedulerId();
  const state: SchedulerState = {
    fibers: new Map(),
    readyQueue: [],
    running: undefined,
    policy,
    stepCount: 0,
    decisions: [],
    replayIndex: 0,
    rngState: policy.tag === "Random" ? policy.seed : undefined,
  };

  schedulerRegistry.set(id, state);
  return state;
}

/**
 * Get a scheduler by ID.
 */
export function getScheduler(id: string): SchedulerState | undefined {
  return schedulerRegistry.get(id);
}

/**
 * Clone a scheduler state (for speculation/backtracking).
 */
export function cloneScheduler(state: SchedulerState): SchedulerState {
  const id = genSchedulerId();
  const cloned: SchedulerState = {
    fibers: new Map(
      Array.from(state.fibers.entries()).map(([k, v]) => [k, { ...v, children: [...v.children] }])
    ),
    readyQueue: [...state.readyQueue],
    running: state.running,
    policy: { ...state.policy } as SchedulePolicy,
    stepCount: state.stepCount,
    decisions: [...state.decisions],
    replayIndex: state.replayIndex,
    rngState: state.rngState,
  };

  schedulerRegistry.set(id, cloned);
  return cloned;
}

// ─────────────────────────────────────────────────────────────────
// Fiber management
// ─────────────────────────────────────────────────────────────────

/**
 * Spawn a new fiber in the scheduler.
 */
export function spawnFiber(
  scheduler: SchedulerState,
  machineState: State,
  options: { name?: string; parentId?: FiberId } = {}
): FiberState {
  const id = genFiberId();
  const fiber: FiberState = {
    id,
    name: options.name,
    machineState,
    status: "ready",
    stepCount: 0,
    parentId: options.parentId,
    children: [],
    createdAt: Date.now(),
  };

  // Register fiber
  scheduler.fibers.set(id, fiber);

  // Add to ready queue
  scheduler.readyQueue.push(id);

  // Update parent's children list
  if (options.parentId !== undefined) {
    const parent = scheduler.fibers.get(options.parentId);
    if (parent) {
      parent.children.push(id);
    }
  }

  return fiber;
}

/**
 * Get a fiber by ID.
 */
export function getFiber(scheduler: SchedulerState, id: FiberId): FiberState | undefined {
  return scheduler.fibers.get(id);
}

/**
 * Update a fiber's status.
 */
export function setFiberStatus(
  scheduler: SchedulerState,
  id: FiberId,
  status: FiberStatus,
  options: { blockReason?: BlockReason; result?: Val; error?: string } = {}
): void {
  const fiber = scheduler.fibers.get(id);
  if (!fiber) return;

  const oldStatus = fiber.status;
  fiber.status = status;
  fiber.blockReason = options.blockReason;

  if (status === "done") {
    fiber.result = options.result;
  } else if (status === "error") {
    fiber.error = options.error;
  }

  // Update ready queue based on status change
  if (oldStatus === "ready" && status !== "ready") {
    // Remove from ready queue
    const idx = scheduler.readyQueue.indexOf(id);
    if (idx >= 0) {
      scheduler.readyQueue.splice(idx, 1);
    }
  } else if (oldStatus !== "ready" && status === "ready") {
    // Add to ready queue
    if (!scheduler.readyQueue.includes(id)) {
      scheduler.readyQueue.push(id);
    }
  }

  // Clear running if this fiber was running
  if (scheduler.running === id && status !== "running") {
    scheduler.running = undefined;
  }
}

/**
 * Mark a fiber as blocked.
 */
export function blockFiber(
  scheduler: SchedulerState,
  id: FiberId,
  reason: BlockReason
): void {
  setFiberStatus(scheduler, id, "blocked", { blockReason: reason });
}

/**
 * Unblock a fiber (make it ready again).
 */
export function unblockFiber(scheduler: SchedulerState, id: FiberId): void {
  const fiber = scheduler.fibers.get(id);
  if (fiber && fiber.status === "blocked") {
    setFiberStatus(scheduler, id, "ready");
  }
}

/**
 * Complete a fiber with a result.
 */
export function completeFiber(
  scheduler: SchedulerState,
  id: FiberId,
  result: Val
): void {
  setFiberStatus(scheduler, id, "done", { result });

  // Unblock any fibers waiting to join this one
  for (const [, fiber] of scheduler.fibers) {
    if (
      fiber.status === "blocked" &&
      fiber.blockReason?.tag === "join" &&
      fiber.blockReason.fiberId === id
    ) {
      unblockFiber(scheduler, fiber.id);
    }
  }
}

/**
 * Fail a fiber with an error.
 */
export function failFiber(
  scheduler: SchedulerState,
  id: FiberId,
  error: string
): void {
  setFiberStatus(scheduler, id, "error", { error });
}

// ─────────────────────────────────────────────────────────────────
// Scheduling policies
// ─────────────────────────────────────────────────────────────────

/**
 * Simple LCG random number generator for deterministic scheduling.
 */
function lcgRandom(state: number): { value: number; nextState: number } {
  // Parameters from Numerical Recipes
  const a = 1664525;
  const c = 1013904223;
  const m = Math.pow(2, 32);
  const nextState = (a * state + c) % m;
  return { value: nextState / m, nextState };
}

/**
 * Select the next fiber to run based on the scheduling policy.
 */
export function selectNextFiber(scheduler: SchedulerState): FiberId | undefined {
  const readyFibers = scheduler.readyQueue;
  if (readyFibers.length === 0) return undefined;

  let chosenIndex: number;
  let chosenId: FiberId;

  switch (scheduler.policy.tag) {
    case "RoundRobin": {
      // Simple round-robin: take the first ready fiber
      chosenIndex = 0;
      chosenId = readyFibers[0];
      break;
    }

    case "FairRR": {
      // Fair round-robin with quantum
      // For now, same as round-robin but respects step limits
      chosenIndex = 0;
      chosenId = readyFibers[0];
      break;
    }

    case "Random": {
      // Deterministic random selection using LCG
      if (scheduler.rngState === undefined) {
        scheduler.rngState = 0;
      }
      const { value, nextState } = lcgRandom(scheduler.rngState);
      scheduler.rngState = nextState;
      chosenIndex = Math.floor(value * readyFibers.length);
      chosenId = readyFibers[chosenIndex];
      break;
    }

    case "Replay": {
      // Replay mode: use recorded decisions
      const decisions = scheduler.policy.decisions;
      if (scheduler.replayIndex < decisions.length) {
        chosenIndex = decisions[scheduler.replayIndex];
        scheduler.replayIndex++;
        // Validate the choice
        if (chosenIndex >= readyFibers.length) {
          // Invalid replay decision, fall back to round-robin
          chosenIndex = 0;
        }
        chosenId = readyFibers[chosenIndex];
      } else {
        // No more replay decisions, fall back to round-robin
        chosenIndex = 0;
        chosenId = readyFibers[0];
      }
      break;
    }

    default:
      chosenIndex = 0;
      chosenId = readyFibers[0];
  }

  // Record the decision
  const decision: ScheduleDecision = {
    readySetHash: hashReadySet(readyFibers),
    chosenFiberId: chosenId,
    decisionIndex: chosenIndex,
    stepCount: scheduler.stepCount,
    timestamp: Date.now(),
  };
  scheduler.decisions.push(decision);

  return chosenId;
}

/**
 * Hash the ready set for decision recording.
 */
function hashReadySet(readyFibers: FiberId[]): string {
  return sha256JSON(readyFibers);
}

// ─────────────────────────────────────────────────────────────────
// Scheduler execution
// ─────────────────────────────────────────────────────────────────

/**
 * Configuration for running the scheduler.
 */
export type SchedulerConfig = {
  /** Maximum total steps across all fibers */
  maxSteps: number;
  /** Maximum steps per fiber before forced yield */
  stepsPerQuantum: number;
  /** Event handler callback */
  onEvent?: (event: ConcurrencyEvent) => void;
  /** Step function for executing a single CEKS step */
  stepFn: (machineState: State) => { state: State; done: boolean; result?: Val; error?: string };
};

/**
 * Run the scheduler until completion or budget exhaustion.
 */
export function runScheduler(
  scheduler: SchedulerState,
  config: SchedulerConfig
): SchedulerStatus {
  let totalSteps = 0;

  while (totalSteps < config.maxSteps) {
    // Get current status
    const status = getSchedulerStatus(scheduler);

    // Check for terminal states
    if (status.tag === "done" || status.tag === "deadlock" || status.tag === "error") {
      return status;
    }

    // Select next fiber to run
    const nextFiberId = selectNextFiber(scheduler);
    if (nextFiberId === undefined) {
      // No ready fibers, check if done or deadlocked
      return getSchedulerStatus(scheduler);
    }

    // Run the selected fiber for its quantum
    const fiber = scheduler.fibers.get(nextFiberId)!;
    scheduler.running = nextFiberId;
    fiber.status = "running";

    // Remove from ready queue
    const idx = scheduler.readyQueue.indexOf(nextFiberId);
    if (idx >= 0) {
      scheduler.readyQueue.splice(idx, 1);
    }

    // Execute steps within quantum
    let quantumSteps = 0;
    while (quantumSteps < config.stepsPerQuantum) {
      const stepResult = config.stepFn(fiber.machineState);
      fiber.machineState = stepResult.state;
      fiber.stepCount++;
      scheduler.stepCount++;
      totalSteps++;
      quantumSteps++;

      if (stepResult.done) {
        if (stepResult.error) {
          failFiber(scheduler, nextFiberId, stepResult.error);
          config.onEvent?.({
            tag: "error",
            fiberId: nextFiberId,
            message: stepResult.error,
            timestamp: Date.now(),
          });
        } else {
          completeFiber(scheduler, nextFiberId, stepResult.result ?? VUnit);
          config.onEvent?.({
            tag: "done",
            fiberId: nextFiberId,
            valueHash: sha256JSON(stepResult.result ?? VUnit),
            timestamp: Date.now(),
          });
        }
        break;
      }

      // Check if fiber got blocked (would be set by effect handlers)
      if (fiber.status === "blocked") {
        break;
      }

      // Check budget
      if (totalSteps >= config.maxSteps) {
        break;
      }
    }

    // If still running, yield back to ready queue
    if (fiber.status === "running") {
      fiber.status = "ready";
      scheduler.readyQueue.push(nextFiberId);
      scheduler.running = undefined;

      config.onEvent?.({
        tag: "yield",
        fiberId: nextFiberId,
        timestamp: Date.now(),
      });
    }
  }

  // Budget exhausted
  return getSchedulerStatus(scheduler);
}

/**
 * Get the current status of the scheduler.
 */
export function getSchedulerStatus(scheduler: SchedulerState): SchedulerStatus {
  // Check if any fiber is running
  if (scheduler.running !== undefined) {
    return { tag: "running", fiberId: scheduler.running };
  }

  // Check if there are ready fibers
  if (scheduler.readyQueue.length > 0) {
    return { tag: "idle" };
  }

  // Check if all fibers are done
  const blockedFibers: FiberId[] = [];
  const results = new Map<FiberId, Val>();
  let hasError = false;
  let errorFiberId: FiberId | undefined;
  let errorMessage: string | undefined;

  for (const [id, fiber] of scheduler.fibers) {
    if (fiber.status === "blocked") {
      blockedFibers.push(id);
    } else if (fiber.status === "done" && fiber.result !== undefined) {
      results.set(id, fiber.result);
    } else if (fiber.status === "error") {
      hasError = true;
      errorFiberId = id;
      errorMessage = fiber.error;
    }
  }

  if (hasError && errorFiberId !== undefined) {
    return { tag: "error", fiberId: errorFiberId, message: errorMessage ?? "Unknown error" };
  }

  if (blockedFibers.length > 0) {
    // All remaining fibers are blocked - deadlock
    return { tag: "deadlock", blocked: blockedFibers };
  }

  // All fibers done
  return { tag: "done", results };
}

// ─────────────────────────────────────────────────────────────────
// Event ledger
// ─────────────────────────────────────────────────────────────────

/**
 * Create an event ledger for recording concurrency events.
 */
export function createEventLedger(): ConcurrencyEvent[] {
  return [];
}

/**
 * Record an event to the ledger.
 */
export function recordEvent(ledger: ConcurrencyEvent[], event: ConcurrencyEvent): void {
  ledger.push(event);
}

/**
 * Extract schedule decisions from an event ledger.
 */
export function extractDecisions(ledger: ConcurrencyEvent[]): number[] {
  return ledger
    .filter((e): e is Extract<ConcurrencyEvent, { tag: "schedule" }> => e.tag === "schedule")
    .map((e) => e.decision.decisionIndex);
}

/**
 * Create a replay policy from recorded decisions.
 */
export function createReplayPolicy(decisions: number[]): SchedulePolicy {
  return { tag: "Replay", decisions };
}

// ─────────────────────────────────────────────────────────────────
// Fiber operations (effects)
// ─────────────────────────────────────────────────────────────────

/**
 * Yield the current fiber, allowing other fibers to run.
 */
export function yieldFiber(scheduler: SchedulerState, fiberId: FiberId): void {
  const fiber = scheduler.fibers.get(fiberId);
  if (!fiber || fiber.status !== "running") return;

  fiber.status = "ready";
  scheduler.readyQueue.push(fiberId);
  scheduler.running = undefined;
}

/**
 * Join on another fiber (wait for it to complete).
 */
export function joinFiber(
  scheduler: SchedulerState,
  waiterId: FiberId,
  targetId: FiberId
): Val | undefined {
  const target = scheduler.fibers.get(targetId);
  if (!target) {
    // Target doesn't exist, return unit
    return VUnit;
  }

  if (target.status === "done") {
    // Already done, return result immediately
    return target.result ?? VUnit;
  }

  if (target.status === "error") {
    // Target errored
    return { tag: "Err", message: target.error ?? "Fiber error" };
  }

  // Block the waiter until target completes
  blockFiber(scheduler, waiterId, { tag: "join", fiberId: targetId });
  return undefined; // Indicates caller should suspend
}

/**
 * Get the result of a completed fiber.
 */
export function getFiberResult(scheduler: SchedulerState, fiberId: FiberId): Val | undefined {
  const fiber = scheduler.fibers.get(fiberId);
  if (!fiber) return undefined;
  if (fiber.status === "done") return fiber.result;
  if (fiber.status === "error") {
    return { tag: "Err", message: fiber.error ?? "Fiber error" };
  }
  return undefined;
}
