// src/core/concurrency/types.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Prompt 13: SICP-style concurrency types

import type { Val, FiberId, FiberVal, MutexVal, IVarVal, ChannelVal, ActorVal, SchedulerView } from "../eval/values";
import type { State } from "../eval/machine";
import type { Hash } from "../artifacts/hash";

// ─────────────────────────────────────────────────────────────────
// Fiber state
// ─────────────────────────────────────────────────────────────────

/**
 * FiberStatus: Current execution state of a fiber.
 */
export type FiberStatus = "ready" | "running" | "blocked" | "done" | "error";

/**
 * BlockReason: Why a fiber is blocked.
 */
export type BlockReason =
  | { tag: "join"; fiberId: FiberId }
  | { tag: "mutex"; mutexId: string }
  | { tag: "ivar"; ivarId: string }
  | { tag: "channel"; channelId: string; op: "send" | "recv" }
  | { tag: "yield" };

/**
 * FiberState: Internal state of a single fiber.
 */
export type FiberState = {
  /** Unique fiber identifier */
  id: FiberId;
  /** Human-readable name */
  name?: string;
  /** Current CEKS machine state */
  machineState: State;
  /** Current status */
  status: FiberStatus;
  /** Reason for blocking (if blocked) */
  blockReason?: BlockReason;
  /** Result value (if done) */
  result?: Val;
  /** Error message (if error) */
  error?: string;
  /** Step count within this fiber */
  stepCount: number;
  /** Parent fiber ID (if spawned by another fiber) */
  parentId?: FiberId;
  /** Child fiber IDs */
  children: FiberId[];
  /** Creation timestamp */
  createdAt: number;
};

// ─────────────────────────────────────────────────────────────────
// Scheduling policies
// ─────────────────────────────────────────────────────────────────

/**
 * SchedulePolicy: Strategy for selecting the next fiber to run.
 */
export type SchedulePolicy =
  | { tag: "RoundRobin"; decisions?: number[] }
  | { tag: "FairRR"; quantum: number; decisions?: number[] }
  | { tag: "Random"; seed: number; decisions?: number[] }
  | { tag: "Replay"; decisions: number[] };

/**
 * ScheduleDecision: Record of a scheduling choice.
 */
export type ScheduleDecision = {
  /** Hash of ready set at decision time */
  readySetHash: Hash;
  /** Chosen fiber ID */
  chosenFiberId: FiberId;
  /** Index in ready set */
  decisionIndex: number;
  /** Global step count */
  stepCount: number;
  /** Timestamp */
  timestamp: number;
};

// ─────────────────────────────────────────────────────────────────
// Scheduler state
// ─────────────────────────────────────────────────────────────────

/**
 * SchedulerState: Global state of the fiber scheduler.
 */
export type SchedulerState = {
  /** All fibers by ID */
  fibers: Map<FiberId, FiberState>;
  /** Ready queue (fiber IDs) */
  readyQueue: FiberId[];
  /** Currently running fiber */
  running?: FiberId;
  /** Scheduling policy */
  policy: SchedulePolicy;
  /** Global step count */
  stepCount: number;
  /** Schedule decisions for replay */
  decisions: ScheduleDecision[];
  /** Replay index (if replaying) */
  replayIndex: number;
  /** Random number generator state (for Random policy) */
  rngState?: number;
};

/**
 * SchedulerStatus: Current state of the scheduler.
 */
export type SchedulerStatus =
  | { tag: "running"; fiberId: FiberId }
  | { tag: "idle" }
  | { tag: "deadlock"; blocked: FiberId[] }
  | { tag: "done"; results: Map<FiberId, Val> }
  | { tag: "error"; fiberId: FiberId; message: string };

// ─────────────────────────────────────────────────────────────────
// Synchronization primitive states
// ─────────────────────────────────────────────────────────────────

/**
 * MutexState: Internal state of a mutex.
 */
export type MutexState = {
  /** Mutex ID */
  id: string;
  /** Name */
  name?: string;
  /** Fiber holding the lock (if any) */
  holder?: FiberId;
  /** Queue of fibers waiting for the lock */
  waitQueue: FiberId[];
  /** Acquisition count (for deadlock detection) */
  acquisitionCount: number;
};

/**
 * IVarState: Internal state of an IVar.
 */
export type IVarState = {
  /** IVar ID */
  id: string;
  /** Name */
  name?: string;
  /** Value (if written) */
  value?: Val;
  /** Whether value has been written */
  isFull: boolean;
  /** Fibers waiting for the value */
  waitQueue: FiberId[];
};

/**
 * ChannelState: Internal state of a channel.
 */
export type ChannelState = {
  /** Channel ID */
  id: string;
  /** Name */
  name?: string;
  /** Buffer size (0 = unbuffered) */
  bufferSize: number;
  /** Buffered values */
  buffer: Val[];
  /** Fibers waiting to send */
  sendQueue: Array<{ fiberId: FiberId; value: Val }>;
  /** Fibers waiting to receive */
  recvQueue: FiberId[];
  /** Whether channel is closed */
  closed: boolean;
};

/**
 * ActorState: Internal state of an actor.
 */
export type ActorState = {
  /** Actor ID */
  id: string;
  /** Name */
  name?: string;
  /** Underlying fiber ID */
  fiberId: FiberId;
  /** Mailbox (message queue) */
  mailbox: Val[];
  /** Whether actor is processing a message */
  processing: boolean;
};

// ─────────────────────────────────────────────────────────────────
// Concurrency event ledger
// ─────────────────────────────────────────────────────────────────

/**
 * ConcurrencyEvent: Record of a concurrency operation.
 */
export type ConcurrencyEvent =
  | { tag: "spawn"; fiberId: FiberId; parentId?: FiberId; timestamp: number }
  | { tag: "yield"; fiberId: FiberId; timestamp: number }
  | { tag: "join"; fiberId: FiberId; targetId: FiberId; timestamp: number }
  | { tag: "done"; fiberId: FiberId; valueHash: Hash; timestamp: number }
  | { tag: "error"; fiberId: FiberId; message: string; timestamp: number }
  | { tag: "schedule"; decision: ScheduleDecision }
  | { tag: "mutexLock"; fiberId: FiberId; mutexId: string; timestamp: number }
  | { tag: "mutexUnlock"; fiberId: FiberId; mutexId: string; timestamp: number }
  | { tag: "mutexBlock"; fiberId: FiberId; mutexId: string; timestamp: number }
  | { tag: "ivarPut"; fiberId: FiberId; ivarId: string; valueHash: Hash; timestamp: number }
  | { tag: "ivarTake"; fiberId: FiberId; ivarId: string; timestamp: number }
  | { tag: "ivarBlock"; fiberId: FiberId; ivarId: string; timestamp: number }
  | { tag: "chanSend"; fiberId: FiberId; channelId: string; valueHash: Hash; timestamp: number }
  | { tag: "chanRecv"; fiberId: FiberId; channelId: string; timestamp: number }
  | { tag: "deadlock"; blocked: FiberId[]; timestamp: number };

// ─────────────────────────────────────────────────────────────────
// Concurrency budget
// ─────────────────────────────────────────────────────────────────

/**
 * ConcurrencyBudget: Resource limits for concurrent execution.
 */
export type ConcurrencyBudget = {
  /** Maximum fibers that can be spawned */
  maxSpawns: number;
  /** Maximum yield operations */
  maxYields: number;
  /** Maximum total steps across all fibers */
  maxTotalSteps: number;
  /** Maximum steps per fiber before forced yield */
  maxStepsPerFiber: number;
  /** Maximum mutex operations */
  maxMutexOps: number;
  /** Maximum IVar operations */
  maxIVarOps: number;
};

export const DEFAULT_CONCURRENCY_BUDGET: ConcurrencyBudget = {
  maxSpawns: 100,
  maxYields: 10000,
  maxTotalSteps: 1000000,
  maxStepsPerFiber: 10000,
  maxMutexOps: 10000,
  maxIVarOps: 10000,
};

// ─────────────────────────────────────────────────────────────────
// Type guards and helpers
// ─────────────────────────────────────────────────────────────────

export function isFiber(v: Val): v is FiberVal {
  return v.tag === "Fiber";
}

export function isMutex(v: Val): v is MutexVal {
  return v.tag === "Mutex";
}

export function isIVar(v: Val): v is IVarVal {
  return v.tag === "IVar";
}

export function isChannel(v: Val): v is ChannelVal {
  return v.tag === "Channel";
}

export function isActor(v: Val): v is ActorVal {
  return v.tag === "Actor";
}

/**
 * Create a FiberVal.
 */
export function makeFiber(id: FiberId, name?: string): FiberVal {
  return { tag: "Fiber", id, name };
}

/**
 * Create a MutexVal.
 */
export function makeMutex(id: string, name?: string): MutexVal {
  return { tag: "Mutex", id, name };
}

/**
 * Create an IVarVal.
 */
export function makeIVar(id: string, name?: string): IVarVal {
  return { tag: "IVar", id, name };
}

/**
 * Create a ChannelVal.
 */
export function makeChannel(id: string, bufferSize: number, name?: string): ChannelVal {
  return { tag: "Channel", id, bufferSize, name };
}

/**
 * Create an ActorVal.
 */
export function makeActor(id: string, fiberId: FiberId, name?: string): ActorVal {
  return { tag: "Actor", id, fiberId, name };
}

/**
 * Create a SchedulerView from SchedulerState.
 */
export function createSchedulerView(state: SchedulerState): SchedulerView {
  const ready: FiberId[] = [];
  const blocked: Array<{ id: FiberId; on: string }> = [];
  const done: Array<{ id: FiberId; valueHash: Hash }> = [];

  for (const [id, fiber] of state.fibers) {
    if (fiber.status === "ready") {
      ready.push(id);
    } else if (fiber.status === "blocked" && fiber.blockReason) {
      blocked.push({ id, on: blockReasonToString(fiber.blockReason) });
    } else if (fiber.status === "done") {
      done.push({ id, valueHash: fiber.result ? hashVal(fiber.result) : "" });
    }
  }

  return {
    running: state.running,
    ready,
    blocked,
    done,
    stepCount: state.stepCount,
    policy: policyToString(state.policy),
  };
}

function blockReasonToString(reason: BlockReason): string {
  switch (reason.tag) {
    case "join": return `join:${reason.fiberId}`;
    case "mutex": return `mutex:${reason.mutexId}`;
    case "ivar": return `ivar:${reason.ivarId}`;
    case "channel": return `channel:${reason.op}:${reason.channelId}`;
    case "yield": return "yield";
  }
}

function policyToString(policy: SchedulePolicy): string {
  return policy.tag;
}

function hashVal(v: Val): Hash {
  try {
    return JSON.stringify(v).slice(0, 16);
  } catch {
    return v.tag;
  }
}
