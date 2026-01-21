/**
 * ═══════════════════════════════════════════════════════════════════════════
 * Concurrent Inference Tests
 * ═══════════════════════════════════════════════════════════════════════════
 *
 * Quick Reference: docs/USER-MANUAL--00--Quick-Reference.md#22-concurrent-inference
 * Full Chapter:    docs/USER-MANUAL--22--Concurrent-Inference.md
 * Demo:            demo/by-chapter/ch22-concurrent-inference.ts
 * ═══════════════════════════════════════════════════════════════════════════
 */
// test/prompt13-concurrency/concurrency.spec.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Tests for Prompt 13: SICP-style concurrency

import { describe, it, expect, beforeEach } from "vitest";
import type { Val, FiberId } from "../../src/core/eval/values";
import { VUnit, VTrue, VFalse } from "../../src/core/eval/values";
import type { State } from "../../src/core/eval/machine";
import {
  // Types
  type FiberState,
  type FiberStatus,
  type SchedulePolicy,
  type SchedulerState,
  type SchedulerStatus,
  type ConcurrencyEvent,
  type MutexState,
  type IVarState,
  type ChannelState,
  DEFAULT_CONCURRENCY_BUDGET,
  isFiber,
  isMutex,
  isIVar,
  isChannel,
  makeFiber,
  makeMutex,
  makeIVar,
  makeChannel,
  createSchedulerView,

  // Scheduler
  createScheduler,
  resetSchedulerRegistry,
  genFiberId,
  spawnFiber,
  getFiber,
  setFiberStatus,
  blockFiber,
  unblockFiber,
  completeFiber,
  failFiber,
  selectNextFiber,
  runScheduler,
  getSchedulerStatus,
  createEventLedger,
  recordEvent,
  extractDecisions,
  createReplayPolicy,
  yieldFiber,
  joinFiber,
  getFiberResult,

  // Sync
  createMutex,
  getMutexState,
  acquireMutex,
  releaseMutex,
  tryAcquireMutex,
  createIVar,
  getIVarState,
  putIVar,
  takeIVar,
  tryTakeIVar,
  isIVarFull,
  createChannel,
  getChannelState,
  sendChannel,
  recvChannel,
  closeChannel,
  isChannelClosed,
  createSerializer,
  resetAllSyncRegistries,

  // Singleflight
  createSingleflightGroup,
  singleflightDo,
  singleflightComplete,
  singleflightWait,
  singleflightTryGet,
  singleflightInFlight,
  singleflightStats,
  singleflightKey,
  createMemoTable,
  memoGet,
  memoSet,
  memoClear,
  memoStats,
  resetSingleflightRegistry,
  resetMemoRegistry,

  // Actor
  createActor,
  getActorState,
  sendMessage,
  receiveMessage,
  mailboxSize,
  createSupervisor,
  supervisorAddChild,
  resetAllActorRegistries,

  // Critic
  buildWaitForGraph,
  detectCycle,
  detectDeadlock,
  summarizeEvents,
  filterEventsByFiber,
  generateDiagnostics,
} from "../../src/core/concurrency";

// ─────────────────────────────────────────────────────────────────
// Test helpers
// ─────────────────────────────────────────────────────────────────

/**
 * Create a mock machine state for testing.
 */
function mockMachineState(id: number): State {
  return {
    ctrl: { tag: "Val", v: { tag: "Num", n: id } },
    env: [],
    kont: { tag: "Done" },
    store: new Map(),
  } as unknown as State;
}

/**
 * Create a simple step function for testing.
 */
function createStepFn(stepsToComplete: Map<FiberId, number>): (state: State) => { state: State; done: boolean; result?: Val } {
  const stepCounts = new Map<FiberId, number>();

  return (state: State) => {
    // Extract fiber ID from state (simplified)
    const ctrl = state.ctrl as { tag: string; v?: Val };
    const fiberId = ctrl.v?.tag === "Num" ? (ctrl.v as { n: number }).n : 0;

    const current = (stepCounts.get(fiberId) ?? 0) + 1;
    stepCounts.set(fiberId, current);

    const needed = stepsToComplete.get(fiberId) ?? 5;
    if (current >= needed) {
      return {
        state,
        done: true,
        result: { tag: "Num", n: fiberId * 10 },
      };
    }

    return { state, done: false };
  };
}

// ─────────────────────────────────────────────────────────────────
// Setup
// ─────────────────────────────────────────────────────────────────

beforeEach(() => {
  resetSchedulerRegistry();
  resetAllSyncRegistries();
  resetSingleflightRegistry();
  resetMemoRegistry();
  resetAllActorRegistries();
});

// ─────────────────────────────────────────────────────────────────
// Test 13.1: Basic Fiber Spawn and Join
// ─────────────────────────────────────────────────────────────────

describe("Test 13.1: Basic Fiber Spawn and Join", () => {
  it("should spawn fibers correctly", () => {
    const scheduler = createScheduler();
    const fiber1 = spawnFiber(scheduler, mockMachineState(1), { name: "fiber1" });
    const fiber2 = spawnFiber(scheduler, mockMachineState(2), { name: "fiber2" });

    expect(fiber1.id).not.toBe(fiber2.id);
    expect(fiber1.status).toBe("ready");
    expect(fiber2.status).toBe("ready");
    expect(scheduler.fibers.size).toBe(2);
    expect(scheduler.readyQueue.length).toBe(2);
  });

  it("should track parent-child relationships", () => {
    const scheduler = createScheduler();
    const parent = spawnFiber(scheduler, mockMachineState(1), { name: "parent" });
    const child = spawnFiber(scheduler, mockMachineState(2), {
      name: "child",
      parentId: parent.id,
    });

    expect(child.parentId).toBe(parent.id);
    expect(parent.children).toContain(child.id);
  });

  it("should complete fibers with results", () => {
    const scheduler = createScheduler();
    const fiber = spawnFiber(scheduler, mockMachineState(1));
    const result: Val = { tag: "Num", n: 42 };

    completeFiber(scheduler, fiber.id, result);

    expect(fiber.status).toBe("done");
    expect(fiber.result).toEqual(result);
  });

  it("should join on completed fibers", () => {
    const scheduler = createScheduler();
    const target = spawnFiber(scheduler, mockMachineState(1));
    const waiter = spawnFiber(scheduler, mockMachineState(2));

    const result: Val = { tag: "Num", n: 42 };
    completeFiber(scheduler, target.id, result);

    const joinResult = joinFiber(scheduler, waiter.id, target.id);
    expect(joinResult).toEqual(result);
    expect(waiter.status).toBe("ready"); // Not blocked since target is done
  });

  it("should block on incomplete fiber join", () => {
    const scheduler = createScheduler();
    const target = spawnFiber(scheduler, mockMachineState(1));
    const waiter = spawnFiber(scheduler, mockMachineState(2));

    const joinResult = joinFiber(scheduler, waiter.id, target.id);
    expect(joinResult).toBeUndefined(); // Blocked
    expect(waiter.status).toBe("blocked");
    expect(waiter.blockReason?.tag).toBe("join");
  });

  it("should unblock waiters when target completes", () => {
    const scheduler = createScheduler();
    const target = spawnFiber(scheduler, mockMachineState(1));
    const waiter = spawnFiber(scheduler, mockMachineState(2));

    // Block waiter
    joinFiber(scheduler, waiter.id, target.id);
    expect(waiter.status).toBe("blocked");

    // Complete target
    completeFiber(scheduler, target.id, { tag: "Num", n: 42 });

    // Waiter should be unblocked
    expect(waiter.status).toBe("ready");
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 13.2: Deterministic Round-Robin Scheduling
// ─────────────────────────────────────────────────────────────────

describe("Test 13.2: Deterministic Round-Robin Scheduling", () => {
  it("should select fibers in round-robin order", () => {
    const scheduler = createScheduler({ tag: "RoundRobin" });
    const fiber1 = spawnFiber(scheduler, mockMachineState(1));
    const fiber2 = spawnFiber(scheduler, mockMachineState(2));
    const fiber3 = spawnFiber(scheduler, mockMachineState(3));

    // Note: selectNextFiber just records decisions, it doesn't remove from queue
    // The runScheduler function handles actual queue management
    expect(selectNextFiber(scheduler)).toBe(fiber1.id);
    expect(scheduler.decisions.length).toBe(1);
    expect(scheduler.decisions[0].chosenFiberId).toBe(fiber1.id);

    // Manually remove fiber1 to simulate what runScheduler does
    scheduler.readyQueue.shift();

    expect(selectNextFiber(scheduler)).toBe(fiber2.id);
    expect(scheduler.decisions[1].chosenFiberId).toBe(fiber2.id);
    scheduler.readyQueue.shift();

    expect(selectNextFiber(scheduler)).toBe(fiber3.id);
    expect(scheduler.decisions[2].chosenFiberId).toBe(fiber3.id);
  });

  it("should record scheduling decisions", () => {
    const scheduler = createScheduler({ tag: "RoundRobin" });
    spawnFiber(scheduler, mockMachineState(1));
    spawnFiber(scheduler, mockMachineState(2));

    selectNextFiber(scheduler);
    selectNextFiber(scheduler);

    expect(scheduler.decisions.length).toBe(2);
    expect(scheduler.decisions[0].decisionIndex).toBe(0);
    expect(scheduler.decisions[1].decisionIndex).toBe(0);
  });

  it("should run scheduler to completion", () => {
    const scheduler = createScheduler({ tag: "RoundRobin" });
    spawnFiber(scheduler, mockMachineState(1));
    spawnFiber(scheduler, mockMachineState(2));

    const stepsToComplete = new Map<FiberId, number>([
      [0, 3],
      [1, 3],
    ]);

    const status = runScheduler(scheduler, {
      maxSteps: 1000,
      stepsPerQuantum: 2,
      stepFn: createStepFn(stepsToComplete),
    });

    expect(status.tag).toBe("done");
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 13.3: Serializer Mutual Exclusion
// ─────────────────────────────────────────────────────────────────

describe("Test 13.3: Serializer Mutual Exclusion", () => {
  it("should create mutex correctly", () => {
    const mutex = createMutex("test-mutex");
    expect(mutex.tag).toBe("Mutex");
    expect(mutex.name).toBe("test-mutex");

    const state = getMutexState(mutex.id);
    expect(state).toBeDefined();
    expect(state?.holder).toBeUndefined();
    expect(state?.waitQueue).toEqual([]);
  });

  it("should acquire unlocked mutex", () => {
    const scheduler = createScheduler();
    const fiber = spawnFiber(scheduler, mockMachineState(1));
    const mutex = createMutex();

    const acquired = acquireMutex(scheduler, mutex.id, fiber.id);
    expect(acquired).toBe(true);

    const state = getMutexState(mutex.id);
    expect(state?.holder).toBe(fiber.id);
  });

  it("should block on locked mutex", () => {
    const scheduler = createScheduler();
    const fiber1 = spawnFiber(scheduler, mockMachineState(1));
    const fiber2 = spawnFiber(scheduler, mockMachineState(2));
    const mutex = createMutex();

    acquireMutex(scheduler, mutex.id, fiber1.id);
    const acquired = acquireMutex(scheduler, mutex.id, fiber2.id);

    expect(acquired).toBe(false);
    expect(fiber2.status).toBe("blocked");
    expect(fiber2.blockReason?.tag).toBe("mutex");
  });

  it("should release mutex and wake waiter", () => {
    const scheduler = createScheduler();
    const fiber1 = spawnFiber(scheduler, mockMachineState(1));
    const fiber2 = spawnFiber(scheduler, mockMachineState(2));
    const mutex = createMutex();

    acquireMutex(scheduler, mutex.id, fiber1.id);
    acquireMutex(scheduler, mutex.id, fiber2.id); // Blocks

    const nextFiber = releaseMutex(scheduler, mutex.id, fiber1.id);

    expect(nextFiber).toBe(fiber2.id);
    expect(fiber2.status).toBe("ready");

    const state = getMutexState(mutex.id);
    expect(state?.holder).toBe(fiber2.id);
  });

  it("should support try-acquire", () => {
    const fiber1Id = genFiberId();
    const fiber2Id = genFiberId();
    const mutex = createMutex();

    expect(tryAcquireMutex(mutex.id, fiber1Id)).toBe(true);
    expect(tryAcquireMutex(mutex.id, fiber2Id)).toBe(false);
  });

  it("should create serializers", () => {
    const { id, mutex } = createSerializer("bank-account");
    expect(id).toContain("serializer");
    expect(mutex.tag).toBe("Mutex");
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 13.4: Singleflight Oracle Deduplication
// ─────────────────────────────────────────────────────────────────

describe("Test 13.4: Singleflight Oracle Deduplication", () => {
  it("should create singleflight group", () => {
    const { id, group } = createSingleflightGroup("oracle-calls");
    expect(id).toContain("sfgroup");
    expect(group.name).toBe("oracle-calls");
    expect(group.stats.hits).toBe(0);
    expect(group.stats.misses).toBe(0);
  });

  it("should initiate new calls", () => {
    const { id } = createSingleflightGroup();
    const key = singleflightKey("oracle", [{ tag: "Str", s: "test" }]);
    const fiberId = genFiberId();

    const result = singleflightDo(id, key, fiberId);
    expect(result.tag).toBe("initiated");
    expect((result as { ivar: any }).ivar).toBeDefined();
  });

  it("should deduplicate concurrent calls", () => {
    const { id } = createSingleflightGroup();
    const key = singleflightKey("oracle", [{ tag: "Str", s: "test" }]);
    const fiber1 = genFiberId();
    const fiber2 = genFiberId();

    const result1 = singleflightDo(id, key, fiber1);
    const result2 = singleflightDo(id, key, fiber2);

    expect(result1.tag).toBe("initiated");
    expect(result2.tag).toBe("waiting");

    const stats = singleflightStats(id);
    expect(stats?.misses).toBe(1);
    expect(stats?.hits).toBe(1);
  });

  it("should complete and return results", () => {
    const scheduler = createScheduler();
    const fiber1 = spawnFiber(scheduler, mockMachineState(1));
    const fiber2 = spawnFiber(scheduler, mockMachineState(2));

    const { id } = createSingleflightGroup();
    const key = singleflightKey("oracle", [{ tag: "Str", s: "test" }]);

    singleflightDo(id, key, fiber1.id);
    singleflightDo(id, key, fiber2.id);

    const result: Val = { tag: "Str", s: "oracle-result" };
    const completed = singleflightComplete(scheduler, id, key, result, fiber1.id);
    expect(completed).toBe(true);

    const cachedResult = singleflightTryGet(id, key);
    expect(cachedResult).toEqual(result);
  });

  it("should track in-flight status", () => {
    const { id } = createSingleflightGroup();
    const key = singleflightKey("oracle", [{ tag: "Str", s: "test" }]);
    const fiberId = genFiberId();

    expect(singleflightInFlight(id, key)).toBe(false);

    singleflightDo(id, key, fiberId);
    expect(singleflightInFlight(id, key)).toBe(true);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 13.5: Deadlock Detection and Diagnosis
// ─────────────────────────────────────────────────────────────────

describe("Test 13.5: Deadlock Detection and Diagnosis", () => {
  it("should build wait-for graph", () => {
    const scheduler = createScheduler();
    const fiber1 = spawnFiber(scheduler, mockMachineState(1));
    const fiber2 = spawnFiber(scheduler, mockMachineState(2));
    const mutex1 = createMutex("mutex1");
    const mutex2 = createMutex("mutex2");

    // fiber1 holds mutex1, waiting for mutex2
    acquireMutex(scheduler, mutex1.id, fiber1.id);
    // fiber2 holds mutex2, waiting for mutex1
    acquireMutex(scheduler, mutex2.id, fiber2.id);

    // Now create the blocking scenario
    acquireMutex(scheduler, mutex2.id, fiber1.id); // fiber1 blocks
    acquireMutex(scheduler, mutex1.id, fiber2.id); // fiber2 blocks

    const graph = buildWaitForGraph(scheduler);
    expect(graph.size).toBe(2);
    expect(graph.has(fiber1.id)).toBe(true);
    expect(graph.has(fiber2.id)).toBe(true);
  });

  it("should detect simple deadlock cycle", () => {
    const scheduler = createScheduler();
    const fiber1 = spawnFiber(scheduler, mockMachineState(1));
    const fiber2 = spawnFiber(scheduler, mockMachineState(2));
    const mutex1 = createMutex("mutex1");
    const mutex2 = createMutex("mutex2");

    // Create deadlock: fiber1 holds mutex1, wants mutex2
    //                  fiber2 holds mutex2, wants mutex1
    acquireMutex(scheduler, mutex1.id, fiber1.id);
    acquireMutex(scheduler, mutex2.id, fiber2.id);
    acquireMutex(scheduler, mutex2.id, fiber1.id); // blocks
    acquireMutex(scheduler, mutex1.id, fiber2.id); // blocks

    const deadlock = detectDeadlock(scheduler);
    expect(deadlock).toBeDefined();
    expect(deadlock?.cycle.length).toBeGreaterThanOrEqual(2);
    expect(deadlock?.cycle).toContain(fiber1.id);
    expect(deadlock?.cycle).toContain(fiber2.id);
  });

  it("should not detect deadlock when none exists", () => {
    const scheduler = createScheduler();
    const fiber1 = spawnFiber(scheduler, mockMachineState(1));
    const fiber2 = spawnFiber(scheduler, mockMachineState(2));
    const mutex = createMutex();

    // fiber1 holds mutex, fiber2 waiting - no cycle
    acquireMutex(scheduler, mutex.id, fiber1.id);
    acquireMutex(scheduler, mutex.id, fiber2.id);

    const deadlock = detectDeadlock(scheduler);
    expect(deadlock).toBeUndefined();
  });

  it("should generate diagnostics", () => {
    const scheduler = createScheduler();
    const fiber = spawnFiber(scheduler, mockMachineState(1), { name: "test-fiber" });

    // Simulate many steps
    fiber.stepCount = 15000;

    const diagnostics = generateDiagnostics(scheduler, []);
    const longRunning = diagnostics.find(d => d.code === "LONG_RUNNING");
    expect(longRunning).toBeDefined();
    expect(longRunning?.fiberId).toBe(fiber.id);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 13.6: Random Scheduling with Seed Reproducibility
// ─────────────────────────────────────────────────────────────────

describe("Test 13.6: Random Scheduling with Seed Reproducibility", () => {
  it("should produce same decisions with same seed", () => {
    // First run
    const scheduler1 = createScheduler({ tag: "Random", seed: 12345 });
    spawnFiber(scheduler1, mockMachineState(1));
    spawnFiber(scheduler1, mockMachineState(2));
    spawnFiber(scheduler1, mockMachineState(3));

    const decisions1: number[] = [];
    for (let i = 0; i < 5; i++) {
      selectNextFiber(scheduler1);
      decisions1.push(scheduler1.decisions[i].decisionIndex);
      // Put fiber back for next selection
      const chosen = scheduler1.decisions[i].chosenFiberId;
      if (!scheduler1.readyQueue.includes(chosen)) {
        scheduler1.readyQueue.push(chosen);
      }
    }

    // Second run with same seed
    resetSchedulerRegistry();
    const scheduler2 = createScheduler({ tag: "Random", seed: 12345 });
    spawnFiber(scheduler2, mockMachineState(1));
    spawnFiber(scheduler2, mockMachineState(2));
    spawnFiber(scheduler2, mockMachineState(3));

    const decisions2: number[] = [];
    for (let i = 0; i < 5; i++) {
      selectNextFiber(scheduler2);
      decisions2.push(scheduler2.decisions[i].decisionIndex);
      const chosen = scheduler2.decisions[i].chosenFiberId;
      if (!scheduler2.readyQueue.includes(chosen)) {
        scheduler2.readyQueue.push(chosen);
      }
    }

    expect(decisions1).toEqual(decisions2);
  });

  it("should support replay policy", () => {
    const scheduler = createScheduler({ tag: "Random", seed: 42 });
    spawnFiber(scheduler, mockMachineState(1));
    spawnFiber(scheduler, mockMachineState(2));

    // Make some decisions
    selectNextFiber(scheduler);
    scheduler.readyQueue.push(scheduler.decisions[0].chosenFiberId);
    selectNextFiber(scheduler);

    const originalDecisions = scheduler.decisions.map(d => d.decisionIndex);

    // Create replay policy
    const replayPolicy = createReplayPolicy(originalDecisions);
    expect(replayPolicy.tag).toBe("Replay");
    expect(replayPolicy.decisions).toEqual(originalDecisions);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 13.7: IVar Single-Assignment Coordination
// ─────────────────────────────────────────────────────────────────

describe("Test 13.7: IVar Single-Assignment Coordination", () => {
  it("should create empty IVar", () => {
    const ivar = createIVar("test-ivar");
    expect(ivar.tag).toBe("IVar");
    expect(isIVarFull(ivar.id)).toBe(false);
  });

  it("should put value in IVar", () => {
    const scheduler = createScheduler();
    const fiber = spawnFiber(scheduler, mockMachineState(1));
    const ivar = createIVar();
    const value: Val = { tag: "Num", n: 42 };

    const success = putIVar(scheduler, ivar.id, value, fiber.id);
    expect(success).toBe(true);
    expect(isIVarFull(ivar.id)).toBe(true);

    const state = getIVarState(ivar.id);
    expect(state?.value).toEqual(value);
  });

  it("should fail on second put", () => {
    const scheduler = createScheduler();
    const fiber = spawnFiber(scheduler, mockMachineState(1));
    const ivar = createIVar();

    putIVar(scheduler, ivar.id, { tag: "Num", n: 1 }, fiber.id);
    const success = putIVar(scheduler, ivar.id, { tag: "Num", n: 2 }, fiber.id);

    expect(success).toBe(false);
  });

  it("should take value immediately if full", () => {
    const scheduler = createScheduler();
    const fiber1 = spawnFiber(scheduler, mockMachineState(1));
    const fiber2 = spawnFiber(scheduler, mockMachineState(2));
    const ivar = createIVar();
    const value: Val = { tag: "Num", n: 42 };

    putIVar(scheduler, ivar.id, value, fiber1.id);
    const taken = takeIVar(scheduler, ivar.id, fiber2.id);

    expect(taken).toEqual(value);
    expect(fiber2.status).toBe("ready"); // Not blocked
  });

  it("should block on empty IVar", () => {
    const scheduler = createScheduler();
    const fiber = spawnFiber(scheduler, mockMachineState(1));
    const ivar = createIVar();

    const taken = takeIVar(scheduler, ivar.id, fiber.id);

    expect(taken).toBeUndefined();
    expect(fiber.status).toBe("blocked");
    expect(fiber.blockReason?.tag).toBe("ivar");
  });

  it("should unblock waiters when filled", () => {
    const scheduler = createScheduler();
    const writer = spawnFiber(scheduler, mockMachineState(1));
    const reader = spawnFiber(scheduler, mockMachineState(2));
    const ivar = createIVar();

    // Reader blocks
    takeIVar(scheduler, ivar.id, reader.id);
    expect(reader.status).toBe("blocked");

    // Writer fills
    const value: Val = { tag: "Num", n: 42 };
    putIVar(scheduler, ivar.id, value, writer.id);

    // Reader unblocked
    expect(reader.status).toBe("ready");
  });

  it("should support try-take", () => {
    const scheduler = createScheduler();
    const fiber = spawnFiber(scheduler, mockMachineState(1));
    const ivar = createIVar();

    expect(tryTakeIVar(ivar.id)).toBeUndefined();

    putIVar(scheduler, ivar.id, { tag: "Num", n: 42 }, fiber.id);
    expect(tryTakeIVar(ivar.id)).toEqual({ tag: "Num", n: 42 });
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 13.8: Channel Communication
// ─────────────────────────────────────────────────────────────────

describe("Test 13.8: Channel Communication", () => {
  it("should create buffered channel", () => {
    const channel = createChannel(3, "test-channel");
    expect(channel.tag).toBe("Channel");
    expect(channel.bufferSize).toBe(3);

    const state = getChannelState(channel.id);
    expect(state?.bufferSize).toBe(3);
    expect(state?.buffer).toEqual([]);
    expect(state?.closed).toBe(false);
  });

  it("should send to buffered channel", () => {
    const scheduler = createScheduler();
    const fiber = spawnFiber(scheduler, mockMachineState(1));
    const channel = createChannel(2);

    const sent = sendChannel(scheduler, channel.id, { tag: "Num", n: 1 }, fiber.id);
    expect(sent).toBe(true);

    const state = getChannelState(channel.id);
    expect(state?.buffer.length).toBe(1);
  });

  it("should receive from buffered channel", () => {
    const scheduler = createScheduler();
    const sender = spawnFiber(scheduler, mockMachineState(1));
    const receiver = spawnFiber(scheduler, mockMachineState(2));
    const channel = createChannel(2);

    sendChannel(scheduler, channel.id, { tag: "Num", n: 42 }, sender.id);
    const value = recvChannel(scheduler, channel.id, receiver.id);

    expect(value).toEqual({ tag: "Num", n: 42 });
  });

  it("should block on empty channel receive", () => {
    const scheduler = createScheduler();
    const fiber = spawnFiber(scheduler, mockMachineState(1));
    const channel = createChannel(0); // Unbuffered

    const value = recvChannel(scheduler, channel.id, fiber.id);

    expect(value).toBeUndefined();
    expect(fiber.status).toBe("blocked");
    expect(fiber.blockReason?.tag).toBe("channel");
  });

  it("should support unbuffered rendezvous", () => {
    const scheduler = createScheduler();
    const sender = spawnFiber(scheduler, mockMachineState(1));
    const receiver = spawnFiber(scheduler, mockMachineState(2));
    const channel = createChannel(0);

    // Sender blocks on unbuffered channel
    const sent = sendChannel(scheduler, channel.id, { tag: "Num", n: 42 }, sender.id);
    expect(sent).toBe(false);
    expect(sender.status).toBe("blocked");

    // Receiver gets value directly from sender
    const value = recvChannel(scheduler, channel.id, receiver.id);
    expect(value).toEqual({ tag: "Num", n: 42 });
    expect(sender.status).toBe("ready"); // Sender unblocked
  });

  it("should close channel", () => {
    const scheduler = createScheduler();
    const channel = createChannel(1);

    expect(isChannelClosed(channel.id)).toBe(false);
    closeChannel(scheduler, channel.id);
    expect(isChannelClosed(channel.id)).toBe(true);
  });

  it("should return closed symbol on closed channel receive", () => {
    const scheduler = createScheduler();
    const fiber = spawnFiber(scheduler, mockMachineState(1));
    const channel = createChannel(0);

    closeChannel(scheduler, channel.id);
    const value = recvChannel(scheduler, channel.id, fiber.id);

    expect(value).toEqual({ tag: "Sym", name: "closed" });
  });
});

// ─────────────────────────────────────────────────────────────────
// Additional Tests: Type Guards and Helpers
// ─────────────────────────────────────────────────────────────────

describe("Type Guards", () => {
  it("should identify fiber values", () => {
    const fiber = makeFiber(1, "test");
    expect(isFiber(fiber)).toBe(true);
    expect(isFiber({ tag: "Num", n: 1 })).toBe(false);
  });

  it("should identify mutex values", () => {
    const mutex = makeMutex("mutex-1", "test");
    expect(isMutex(mutex)).toBe(true);
    expect(isMutex({ tag: "Num", n: 1 })).toBe(false);
  });

  it("should identify IVar values", () => {
    const ivar = makeIVar("ivar-1", "test");
    expect(isIVar(ivar)).toBe(true);
    expect(isIVar({ tag: "Num", n: 1 })).toBe(false);
  });

  it("should identify channel values", () => {
    const channel = makeChannel("chan-1", 5, "test");
    expect(isChannel(channel)).toBe(true);
    expect(isChannel({ tag: "Num", n: 1 })).toBe(false);
  });
});

describe("Event Ledger", () => {
  it("should record events", () => {
    const ledger = createEventLedger();
    const event: ConcurrencyEvent = {
      tag: "spawn",
      fiberId: 1,
      parentId: 0,
      timestamp: Date.now(),
    };

    recordEvent(ledger, event);
    expect(ledger.length).toBe(1);
    expect(ledger[0]).toEqual(event);
  });

  it("should summarize events", () => {
    const events: ConcurrencyEvent[] = [
      { tag: "spawn", fiberId: 1, timestamp: Date.now() },
      { tag: "spawn", fiberId: 2, parentId: 1, timestamp: Date.now() },
      { tag: "yield", fiberId: 1, timestamp: Date.now() },
      { tag: "mutexLock", fiberId: 1, mutexId: "m1", timestamp: Date.now() },
    ];

    const summary = summarizeEvents(events);
    expect(summary.spawns).toBe(2);
    expect(summary.yields).toBe(1);
    expect(summary.mutexOps).toBe(1);
  });

  it("should filter events by fiber", () => {
    const events: ConcurrencyEvent[] = [
      { tag: "spawn", fiberId: 1, timestamp: Date.now() },
      { tag: "spawn", fiberId: 2, parentId: 1, timestamp: Date.now() },
      { tag: "yield", fiberId: 1, timestamp: Date.now() },
      { tag: "yield", fiberId: 2, timestamp: Date.now() },
    ];

    const fiber1Events = filterEventsByFiber(events, 1);
    expect(fiber1Events.length).toBe(3); // spawn(1), spawn(2 with parent 1), yield(1)
  });
});

describe("Scheduler View", () => {
  it("should create scheduler view", () => {
    const scheduler = createScheduler();
    const fiber1 = spawnFiber(scheduler, mockMachineState(1), { name: "fiber1" });
    const fiber2 = spawnFiber(scheduler, mockMachineState(2), { name: "fiber2" });
    completeFiber(scheduler, fiber1.id, { tag: "Num", n: 10 });

    const view = createSchedulerView(scheduler);

    expect(view.ready).toContain(fiber2.id);
    expect(view.done.length).toBe(1);
    expect(view.done[0].id).toBe(fiber1.id);
    expect(view.policy).toBe("RoundRobin");
  });
});

describe("Memoization Table", () => {
  it("should cache and retrieve values", () => {
    const { id } = createMemoTable(100, "test-memo");
    const key = "test-key";
    const value: Val = { tag: "Str", s: "cached" };

    expect(memoGet(id, key)).toBeUndefined();

    memoSet(id, key, value);
    expect(memoGet(id, key)).toEqual(value);
  });

  it("should track statistics", () => {
    const { id } = createMemoTable(100);
    const key = "key1";

    memoGet(id, key); // miss
    memoSet(id, key, { tag: "Num", n: 1 });
    memoGet(id, key); // hit
    memoGet(id, key); // hit

    const stats = memoStats(id);
    expect(stats?.misses).toBe(1);
    expect(stats?.hits).toBe(2);
  });

  it("should evict when at capacity", () => {
    const { id } = createMemoTable(2);

    memoSet(id, "k1", { tag: "Num", n: 1 });
    memoSet(id, "k2", { tag: "Num", n: 2 });
    memoSet(id, "k3", { tag: "Num", n: 3 }); // Should evict k1

    const stats = memoStats(id);
    expect(stats?.size).toBe(2);
    expect(stats?.evictions).toBe(1);
  });

  it("should clear memo table", () => {
    const { id } = createMemoTable(100);
    memoSet(id, "k1", { tag: "Num", n: 1 });
    memoSet(id, "k2", { tag: "Num", n: 2 });

    memoClear(id);

    expect(memoGet(id, "k1")).toBeUndefined();
    expect(memoStats(id)?.size).toBe(0);
  });
});

describe("Actor Model", () => {
  it("should create actor with mailbox", () => {
    const scheduler = createScheduler();
    const actor = createActor(scheduler, mockMachineState(1), { name: "test-actor" });

    expect(actor.tag).toBe("Actor");
    expect(mailboxSize(actor.id)).toBe(0);
  });

  it("should send messages to actor", () => {
    const scheduler = createScheduler();
    const actor = createActor(scheduler, mockMachineState(1));

    sendMessage(scheduler, actor.id, { tag: "Str", s: "hello" });
    sendMessage(scheduler, actor.id, { tag: "Str", s: "world" });

    expect(mailboxSize(actor.id)).toBe(2);
  });

  it("should receive messages in order", () => {
    const scheduler = createScheduler();
    const actor = createActor(scheduler, mockMachineState(1));
    const state = getActorState(actor.id)!;

    sendMessage(scheduler, actor.id, { tag: "Num", n: 1 });
    sendMessage(scheduler, actor.id, { tag: "Num", n: 2 });

    const msg1 = receiveMessage(scheduler, actor.id, state.fiberId);
    const msg2 = receiveMessage(scheduler, actor.id, state.fiberId);

    expect(msg1).toEqual({ tag: "Num", n: 1 });
    expect(msg2).toEqual({ tag: "Num", n: 2 });
  });

  it("should create supervisor", () => {
    const { id, supervisor } = createSupervisor({ tag: "restart" }, { name: "test-supervisor" });

    expect(id).toContain("supervisor");
    expect(supervisor.strategy.tag).toBe("restart");
    expect(supervisor.children).toEqual([]);
  });
});
