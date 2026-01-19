// src/core/concurrency/actor.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Prompt 13: Actor model with mailboxes

import type { Val, FiberId, ActorVal } from "../eval/values";
import { VUnit } from "../eval/values";
import type { State } from "../eval/machine";
import type { ActorState, ConcurrencyEvent } from "./types";
import { makeActor } from "./types";
import type { SchedulerState } from "./scheduler";
import { spawnFiber, getFiber, blockFiber, unblockFiber } from "./scheduler";

// ─────────────────────────────────────────────────────────────────
// Actor registry
// ─────────────────────────────────────────────────────────────────

const actorRegistry = new Map<string, ActorState>();
let nextActorId = 0;

/**
 * Generate a unique actor ID.
 */
function genActorId(): string {
  return `actor-${nextActorId++}`;
}

/**
 * Reset the actor registry (for testing).
 */
export function resetActorRegistry(): void {
  actorRegistry.clear();
  nextActorId = 0;
}

// ─────────────────────────────────────────────────────────────────
// Actor creation and management
// ─────────────────────────────────────────────────────────────────

/**
 * Create a new actor with a backing fiber.
 */
export function createActor(
  scheduler: SchedulerState,
  initialState: State,
  options: { name?: string; parentId?: FiberId } = {}
): ActorVal {
  const id = genActorId();

  // Spawn the backing fiber
  const fiber = spawnFiber(scheduler, initialState, {
    name: options.name ? `${options.name}-fiber` : undefined,
    parentId: options.parentId,
  });

  const state: ActorState = {
    id,
    name: options.name,
    fiberId: fiber.id,
    mailbox: [],
    processing: false,
  };

  actorRegistry.set(id, state);
  return makeActor(id, fiber.id, options.name);
}

/**
 * Get actor state.
 */
export function getActorState(id: string): ActorState | undefined {
  return actorRegistry.get(id);
}

/**
 * Get the fiber ID for an actor.
 */
export function getActorFiberId(id: string): FiberId | undefined {
  return actorRegistry.get(id)?.fiberId;
}

// ─────────────────────────────────────────────────────────────────
// Message passing
// ─────────────────────────────────────────────────────────────────

/**
 * Send a message to an actor's mailbox.
 * This is asynchronous - the caller does not wait.
 */
export function sendMessage(
  scheduler: SchedulerState,
  actorId: string,
  message: Val,
  onEvent?: (event: ConcurrencyEvent) => void
): boolean {
  const actor = actorRegistry.get(actorId);
  if (!actor) return false;

  // Add message to mailbox
  actor.mailbox.push(message);

  // Wake up the actor's fiber if it's blocked waiting for messages
  const fiber = getFiber(scheduler, actor.fiberId);
  if (fiber && fiber.status === "blocked" && fiber.blockReason?.tag === "channel") {
    // The actor was waiting for messages
    unblockFiber(scheduler, actor.fiberId);
  }

  return true;
}

/**
 * Receive a message from the actor's mailbox.
 * Returns the message if available, undefined if blocked.
 */
export function receiveMessage(
  scheduler: SchedulerState,
  actorId: string,
  fiberId: FiberId,
  onEvent?: (event: ConcurrencyEvent) => void
): Val | undefined {
  const actor = actorRegistry.get(actorId);
  if (!actor) return undefined;

  // Check that this is the actor's fiber
  if (actor.fiberId !== fiberId) return undefined;

  if (actor.mailbox.length > 0) {
    // Get next message
    const message = actor.mailbox.shift()!;
    actor.processing = true;
    return message;
  }

  // No messages, block until one arrives
  blockFiber(scheduler, fiberId, { tag: "channel", channelId: actorId, op: "recv" });
  return undefined;
}

/**
 * Indicate that message processing is complete.
 */
export function messageProcessed(actorId: string): void {
  const actor = actorRegistry.get(actorId);
  if (actor) {
    actor.processing = false;
  }
}

/**
 * Get the number of pending messages in the mailbox.
 */
export function mailboxSize(actorId: string): number {
  const actor = actorRegistry.get(actorId);
  return actor?.mailbox.length ?? 0;
}

/**
 * Check if the actor is currently processing a message.
 */
export function isProcessing(actorId: string): boolean {
  const actor = actorRegistry.get(actorId);
  return actor?.processing ?? false;
}

// ─────────────────────────────────────────────────────────────────
// Actor supervision
// ─────────────────────────────────────────────────────────────────

/**
 * SupervisorStrategy: How to handle actor failures.
 */
export type SupervisorStrategy =
  | { tag: "restart" }           // Restart the failed actor
  | { tag: "stop" }              // Stop the failed actor
  | { tag: "escalate" }          // Propagate failure to supervisor
  | { tag: "resume" };           // Ignore the failure

/**
 * SupervisorState: State of an actor supervisor.
 */
export type SupervisorState = {
  id: string;
  name?: string;
  children: string[];            // Actor IDs
  strategy: SupervisorStrategy;
  restartCount: Map<string, number>;
  maxRestarts: number;
};

const supervisorRegistry = new Map<string, SupervisorState>();
let nextSupervisorId = 0;

/**
 * Create a supervisor.
 */
export function createSupervisor(
  strategy: SupervisorStrategy = { tag: "restart" },
  options: { name?: string; maxRestarts?: number } = {}
): { id: string; supervisor: SupervisorState } {
  const id = `supervisor-${nextSupervisorId++}`;
  const supervisor: SupervisorState = {
    id,
    name: options.name,
    children: [],
    strategy,
    restartCount: new Map(),
    maxRestarts: options.maxRestarts ?? 3,
  };
  supervisorRegistry.set(id, supervisor);
  return { id, supervisor };
}

/**
 * Add an actor to a supervisor.
 */
export function supervisorAddChild(supervisorId: string, actorId: string): boolean {
  const supervisor = supervisorRegistry.get(supervisorId);
  if (!supervisor) return false;

  if (!supervisor.children.includes(actorId)) {
    supervisor.children.push(actorId);
  }
  return true;
}

/**
 * Handle a child actor failure.
 */
export function supervisorHandleFailure(
  scheduler: SchedulerState,
  supervisorId: string,
  actorId: string,
  error: string,
  restartFn: (actorId: string) => ActorVal | undefined
): { action: SupervisorStrategy["tag"]; newActor?: ActorVal } {
  const supervisor = supervisorRegistry.get(supervisorId);
  if (!supervisor) {
    return { action: "stop" };
  }

  switch (supervisor.strategy.tag) {
    case "restart": {
      // Check restart limit
      const restarts = (supervisor.restartCount.get(actorId) ?? 0) + 1;
      supervisor.restartCount.set(actorId, restarts);

      if (restarts > supervisor.maxRestarts) {
        // Too many restarts, stop instead
        return { action: "stop" };
      }

      // Restart the actor
      const newActor = restartFn(actorId);
      return { action: "restart", newActor };
    }

    case "stop":
      return { action: "stop" };

    case "escalate":
      return { action: "escalate" };

    case "resume":
      return { action: "resume" };

    default:
      return { action: "stop" };
  }
}

/**
 * Reset supervisor registry (for testing).
 */
export function resetSupervisorRegistry(): void {
  supervisorRegistry.clear();
  nextSupervisorId = 0;
}

// ─────────────────────────────────────────────────────────────────
// Ask pattern (synchronous request-response)
// ─────────────────────────────────────────────────────────────────

/**
 * AskRequest: A request with a reply-to address.
 */
export type AskRequest = {
  tag: "ask";
  payload: Val;
  replyTo: string;  // IVar ID for the response
};

/**
 * Create an ask request.
 */
export function createAskRequest(payload: Val, replyIVarId: string): Val {
  return {
    tag: "Map",
    entries: [
      [{ tag: "Sym", name: "tag" }, { tag: "Str", s: "ask" }],
      [{ tag: "Sym", name: "payload" }, payload],
      [{ tag: "Sym", name: "replyTo" }, { tag: "Str", s: replyIVarId }],
    ],
  };
}

/**
 * Check if a value is an ask request.
 */
export function isAskRequest(v: Val): boolean {
  if (v.tag !== "Map") return false;
  for (const [k, val] of v.entries) {
    if (k.tag === "Sym" && k.name === "tag" && val.tag === "Str" && val.s === "ask") {
      return true;
    }
  }
  return false;
}

/**
 * Extract the reply-to IVar ID from an ask request.
 */
export function extractReplyTo(v: Val): string | undefined {
  if (v.tag !== "Map") return undefined;
  for (const [k, val] of v.entries) {
    if (k.tag === "Sym" && k.name === "replyTo" && val.tag === "Str") {
      return val.s;
    }
  }
  return undefined;
}

/**
 * Extract the payload from an ask request.
 */
export function extractPayload(v: Val): Val | undefined {
  if (v.tag !== "Map") return undefined;
  for (const [k, val] of v.entries) {
    if (k.tag === "Sym" && k.name === "payload") {
      return val;
    }
  }
  return undefined;
}

// ─────────────────────────────────────────────────────────────────
// Reset all actor registries
// ─────────────────────────────────────────────────────────────────

/**
 * Reset all actor-related registries (for testing).
 */
export function resetAllActorRegistries(): void {
  resetActorRegistry();
  resetSupervisorRegistry();
}
