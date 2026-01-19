// src/core/concurrency/sync.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Prompt 13: Synchronization primitives - Mutex, IVar, Channel

import type { Val, FiberId, MutexVal, IVarVal, ChannelVal } from "../eval/values";
import { VUnit } from "../eval/values";
import type {
  MutexState,
  IVarState,
  ChannelState,
  ConcurrencyEvent,
} from "./types";
import { makeMutex, makeIVar, makeChannel } from "./types";
import type { SchedulerState } from "./scheduler";
import { blockFiber, unblockFiber } from "./scheduler";

// ─────────────────────────────────────────────────────────────────
// Mutex registry
// ─────────────────────────────────────────────────────────────────

const mutexRegistry = new Map<string, MutexState>();
let nextMutexId = 0;

/**
 * Generate a unique mutex ID.
 */
function genMutexId(): string {
  return `mutex-${nextMutexId++}`;
}

/**
 * Reset the mutex registry (for testing).
 */
export function resetMutexRegistry(): void {
  mutexRegistry.clear();
  nextMutexId = 0;
}

// ─────────────────────────────────────────────────────────────────
// IVar registry
// ─────────────────────────────────────────────────────────────────

const ivarRegistry = new Map<string, IVarState>();
let nextIVarId = 0;

/**
 * Generate a unique IVar ID.
 */
function genIVarId(): string {
  return `ivar-${nextIVarId++}`;
}

/**
 * Reset the IVar registry (for testing).
 */
export function resetIVarRegistry(): void {
  ivarRegistry.clear();
  nextIVarId = 0;
}

// ─────────────────────────────────────────────────────────────────
// Channel registry
// ─────────────────────────────────────────────────────────────────

const channelRegistry = new Map<string, ChannelState>();
let nextChannelId = 0;

/**
 * Generate a unique channel ID.
 */
function genChannelId(): string {
  return `chan-${nextChannelId++}`;
}

/**
 * Reset the channel registry (for testing).
 */
export function resetChannelRegistry(): void {
  channelRegistry.clear();
  nextChannelId = 0;
}

// ─────────────────────────────────────────────────────────────────
// Mutex operations
// ─────────────────────────────────────────────────────────────────

/**
 * Create a new mutex.
 */
export function createMutex(name?: string): MutexVal {
  const id = genMutexId();
  const state: MutexState = {
    id,
    name,
    holder: undefined,
    waitQueue: [],
    acquisitionCount: 0,
  };
  mutexRegistry.set(id, state);
  return makeMutex(id, name);
}

/**
 * Get mutex state.
 */
export function getMutexState(id: string): MutexState | undefined {
  return mutexRegistry.get(id);
}

/**
 * Acquire a mutex (blocking).
 * Returns true if acquired immediately, false if blocked.
 */
export function acquireMutex(
  scheduler: SchedulerState,
  mutexId: string,
  fiberId: FiberId,
  onEvent?: (event: ConcurrencyEvent) => void
): boolean {
  const mutex = mutexRegistry.get(mutexId);
  if (!mutex) return false;

  if (mutex.holder === undefined) {
    // Mutex is free, acquire it
    mutex.holder = fiberId;
    mutex.acquisitionCount++;
    onEvent?.({
      tag: "mutexLock",
      fiberId,
      mutexId,
      timestamp: Date.now(),
    });
    return true;
  }

  if (mutex.holder === fiberId) {
    // Already held by this fiber - this is a deadlock attempt
    // In a real implementation, we might support reentrant mutexes
    return false;
  }

  // Mutex is held by another fiber, block and wait
  mutex.waitQueue.push(fiberId);
  blockFiber(scheduler, fiberId, { tag: "mutex", mutexId });
  onEvent?.({
    tag: "mutexBlock",
    fiberId,
    mutexId,
    timestamp: Date.now(),
  });
  return false;
}

/**
 * Release a mutex.
 * Returns the next fiber to wake up, if any.
 */
export function releaseMutex(
  scheduler: SchedulerState,
  mutexId: string,
  fiberId: FiberId,
  onEvent?: (event: ConcurrencyEvent) => void
): FiberId | undefined {
  const mutex = mutexRegistry.get(mutexId);
  if (!mutex) return undefined;

  if (mutex.holder !== fiberId) {
    // Not held by this fiber
    return undefined;
  }

  onEvent?.({
    tag: "mutexUnlock",
    fiberId,
    mutexId,
    timestamp: Date.now(),
  });

  // Check wait queue
  if (mutex.waitQueue.length > 0) {
    // Wake up next waiter
    const nextFiberId = mutex.waitQueue.shift()!;
    mutex.holder = nextFiberId;
    mutex.acquisitionCount++;
    unblockFiber(scheduler, nextFiberId);
    onEvent?.({
      tag: "mutexLock",
      fiberId: nextFiberId,
      mutexId,
      timestamp: Date.now(),
    });
    return nextFiberId;
  }

  // No waiters, release the mutex
  mutex.holder = undefined;
  return undefined;
}

/**
 * Try to acquire a mutex without blocking.
 * Returns true if acquired, false otherwise.
 */
export function tryAcquireMutex(
  mutexId: string,
  fiberId: FiberId,
  onEvent?: (event: ConcurrencyEvent) => void
): boolean {
  const mutex = mutexRegistry.get(mutexId);
  if (!mutex) return false;

  if (mutex.holder === undefined) {
    mutex.holder = fiberId;
    mutex.acquisitionCount++;
    onEvent?.({
      tag: "mutexLock",
      fiberId,
      mutexId,
      timestamp: Date.now(),
    });
    return true;
  }

  return false;
}

// ─────────────────────────────────────────────────────────────────
// IVar operations (single-assignment variables)
// ─────────────────────────────────────────────────────────────────

/**
 * Create a new IVar.
 */
export function createIVar(name?: string): IVarVal {
  const id = genIVarId();
  const state: IVarState = {
    id,
    name,
    value: undefined,
    isFull: false,
    waitQueue: [],
  };
  ivarRegistry.set(id, state);
  return makeIVar(id, name);
}

/**
 * Get IVar state.
 */
export function getIVarState(id: string): IVarState | undefined {
  return ivarRegistry.get(id);
}

/**
 * Put a value into an IVar.
 * Returns true if successful, false if already full.
 */
export function putIVar(
  scheduler: SchedulerState,
  ivarId: string,
  value: Val,
  fiberId: FiberId,
  onEvent?: (event: ConcurrencyEvent) => void
): boolean {
  const ivar = ivarRegistry.get(ivarId);
  if (!ivar) return false;

  if (ivar.isFull) {
    // Already has a value - IVars are single-assignment
    return false;
  }

  // Set the value
  ivar.value = value;
  ivar.isFull = true;

  onEvent?.({
    tag: "ivarPut",
    fiberId,
    ivarId,
    valueHash: JSON.stringify(value).slice(0, 16),
    timestamp: Date.now(),
  });

  // Wake up all waiting fibers
  for (const waiterId of ivar.waitQueue) {
    unblockFiber(scheduler, waiterId);
  }
  ivar.waitQueue = [];

  return true;
}

/**
 * Take a value from an IVar (blocking if empty).
 * Returns the value if available, undefined if blocked.
 */
export function takeIVar(
  scheduler: SchedulerState,
  ivarId: string,
  fiberId: FiberId,
  onEvent?: (event: ConcurrencyEvent) => void
): Val | undefined {
  const ivar = ivarRegistry.get(ivarId);
  if (!ivar) return undefined;

  if (ivar.isFull) {
    // Value is available
    onEvent?.({
      tag: "ivarTake",
      fiberId,
      ivarId,
      timestamp: Date.now(),
    });
    return ivar.value;
  }

  // Block until value is available
  ivar.waitQueue.push(fiberId);
  blockFiber(scheduler, fiberId, { tag: "ivar", ivarId });
  onEvent?.({
    tag: "ivarBlock",
    fiberId,
    ivarId,
    timestamp: Date.now(),
  });
  return undefined;
}

/**
 * Try to take a value from an IVar without blocking.
 */
export function tryTakeIVar(ivarId: string): Val | undefined {
  const ivar = ivarRegistry.get(ivarId);
  if (!ivar || !ivar.isFull) return undefined;
  return ivar.value;
}

/**
 * Check if an IVar is full.
 */
export function isIVarFull(ivarId: string): boolean {
  const ivar = ivarRegistry.get(ivarId);
  return ivar?.isFull ?? false;
}

// ─────────────────────────────────────────────────────────────────
// Channel operations
// ─────────────────────────────────────────────────────────────────

/**
 * Create a new channel.
 */
export function createChannel(bufferSize: number = 0, name?: string): ChannelVal {
  const id = genChannelId();
  const state: ChannelState = {
    id,
    name,
    bufferSize,
    buffer: [],
    sendQueue: [],
    recvQueue: [],
    closed: false,
  };
  channelRegistry.set(id, state);
  return makeChannel(id, bufferSize, name);
}

/**
 * Get channel state.
 */
export function getChannelState(id: string): ChannelState | undefined {
  return channelRegistry.get(id);
}

/**
 * Send a value to a channel (blocking if buffer is full).
 * Returns true if sent immediately, false if blocked.
 */
export function sendChannel(
  scheduler: SchedulerState,
  channelId: string,
  value: Val,
  fiberId: FiberId,
  onEvent?: (event: ConcurrencyEvent) => void
): boolean {
  const channel = channelRegistry.get(channelId);
  if (!channel || channel.closed) return false;

  // Check if there are waiting receivers
  if (channel.recvQueue.length > 0) {
    // Direct handoff to waiting receiver
    const receiverId = channel.recvQueue.shift()!;
    unblockFiber(scheduler, receiverId);
    // Store value for receiver to pick up
    channel.buffer.push(value);
    onEvent?.({
      tag: "chanSend",
      fiberId,
      channelId,
      valueHash: JSON.stringify(value).slice(0, 16),
      timestamp: Date.now(),
    });
    return true;
  }

  // Check if buffer has space
  if (channel.buffer.length < channel.bufferSize) {
    channel.buffer.push(value);
    onEvent?.({
      tag: "chanSend",
      fiberId,
      channelId,
      valueHash: JSON.stringify(value).slice(0, 16),
      timestamp: Date.now(),
    });
    return true;
  }

  // For unbuffered channels, or if buffer is full, block
  channel.sendQueue.push({ fiberId, value });
  blockFiber(scheduler, fiberId, { tag: "channel", channelId, op: "send" });
  return false;
}

/**
 * Receive a value from a channel (blocking if empty).
 * Returns the value if available, undefined if blocked.
 */
export function recvChannel(
  scheduler: SchedulerState,
  channelId: string,
  fiberId: FiberId,
  onEvent?: (event: ConcurrencyEvent) => void
): Val | undefined {
  const channel = channelRegistry.get(channelId);
  if (!channel) return undefined;

  // Check if there are buffered values
  if (channel.buffer.length > 0) {
    const value = channel.buffer.shift()!;

    // If there are waiting senders, move one to buffer
    if (channel.sendQueue.length > 0) {
      const sender = channel.sendQueue.shift()!;
      channel.buffer.push(sender.value);
      unblockFiber(scheduler, sender.fiberId);
    }

    onEvent?.({
      tag: "chanRecv",
      fiberId,
      channelId,
      timestamp: Date.now(),
    });
    return value;
  }

  // Check for waiting senders (unbuffered channel rendezvous)
  if (channel.sendQueue.length > 0) {
    const sender = channel.sendQueue.shift()!;
    unblockFiber(scheduler, sender.fiberId);
    onEvent?.({
      tag: "chanRecv",
      fiberId,
      channelId,
      timestamp: Date.now(),
    });
    return sender.value;
  }

  // Check if channel is closed
  if (channel.closed) {
    return { tag: "Sym", name: "closed" };
  }

  // Block until a value is available
  channel.recvQueue.push(fiberId);
  blockFiber(scheduler, fiberId, { tag: "channel", channelId, op: "recv" });
  return undefined;
}

/**
 * Close a channel.
 */
export function closeChannel(
  scheduler: SchedulerState,
  channelId: string
): void {
  const channel = channelRegistry.get(channelId);
  if (!channel || channel.closed) return;

  channel.closed = true;

  // Wake up all waiting receivers with closed signal
  for (const receiverId of channel.recvQueue) {
    unblockFiber(scheduler, receiverId);
  }
  channel.recvQueue = [];

  // Wake up all waiting senders (they will see closed channel)
  for (const { fiberId } of channel.sendQueue) {
    unblockFiber(scheduler, fiberId);
  }
  channel.sendQueue = [];
}

/**
 * Check if a channel is closed.
 */
export function isChannelClosed(channelId: string): boolean {
  const channel = channelRegistry.get(channelId);
  return channel?.closed ?? true;
}

// ─────────────────────────────────────────────────────────────────
// Serializer (SICP-style mutex wrapper)
// ─────────────────────────────────────────────────────────────────

/**
 * SerializerState: Wraps a mutex to provide SICP-style serialization.
 */
export type SerializerState = {
  mutexId: string;
  name?: string;
};

const serializerRegistry = new Map<string, SerializerState>();
let nextSerializerId = 0;

/**
 * Create a serializer (SICP-style).
 */
export function createSerializer(name?: string): { id: string; mutex: MutexVal } {
  const id = `serializer-${nextSerializerId++}`;
  const mutex = createMutex(name ? `${name}-mutex` : undefined);
  serializerRegistry.set(id, { mutexId: mutex.id, name });
  return { id, mutex };
}

/**
 * Get the mutex for a serializer.
 */
export function getSerializerMutex(id: string): string | undefined {
  return serializerRegistry.get(id)?.mutexId;
}

/**
 * Reset the serializer registry (for testing).
 */
export function resetSerializerRegistry(): void {
  serializerRegistry.clear();
  nextSerializerId = 0;
}

// ─────────────────────────────────────────────────────────────────
// Reset all sync registries
// ─────────────────────────────────────────────────────────────────

/**
 * Reset all synchronization registries (for testing).
 */
export function resetAllSyncRegistries(): void {
  resetMutexRegistry();
  resetIVarRegistry();
  resetChannelRegistry();
  resetSerializerRegistry();
}
