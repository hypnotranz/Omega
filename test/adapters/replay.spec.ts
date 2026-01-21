import { describe, expect, it } from "vitest";
import {
  loggingClock,
  loggingOracle,
  loggingRng,
  loggingTool,
} from "../../src/adapters/logging";
import {
  ReplayLogEntry,
  replayClock,
  replayOracle,
  replayRng,
} from "../../src/adapters/replay";
import type { ExecContext, TraceEvent } from "../../src/ports";

interface ReplayLog {
  get(id: string): ReplayLogEntry | undefined;
  getByTag(tag: string): ReplayLogEntry[];
}

function createLog(entries: ReplayLogEntry[]): ReplayLog {
  return {
    get: (id: string) => entries.find((e) => e.id === id),
    getByTag: (tag: string) => entries.filter((e) => e.tag === tag),
  };
}

function createCtx(events: TraceEvent[] = []): ExecContext {
  return {
    runId: "run-log",
    caps: {},
    trace: {
      emit: (event) => events.push(event),
    },
  };
}

describe("logging adapters", () => {
  it("emit trace events for oracle, tool, clock, and rng", async () => {
    const events: TraceEvent[] = [];
    const ctx = createCtx(events);

    const oracle = loggingOracle({
      infer: async () => ({ text: "logged" }),
    });
    const tool = loggingTool({
      call: async () => ({ ok: true, value: "ok" }),
    });
    const clock = loggingClock({
      nowMs: () => 500,
      sleepMs: async () => {},
    });
    const rng = loggingRng({
      nextU32: () => 1234,
      nextFloat: () => 0.5,
      nextInt: () => 2,
    });

    await oracle.infer({ model: "gpt-4", prompt: "p" }, ctx);
    await tool.call({ name: "tool", args: {}, contractId: "c1" }, ctx);
    const now = clock.nowMs(ctx);
    const value = rng.nextU32(ctx);

    expect(now).toBe(500);
    expect(value).toBe(1234);

    const oracleEvent = events.find((e) => e.tag === "E_OracleCall");
    const toolEvent = events.find((e) => e.tag === "E_ToolCall");
    const clockEvent = events.find((e) => e.tag === "E_ClockRead");
    const rngEvent = events.find((e) => e.tag === "E_RngRead");

    expect(oracleEvent).toBeDefined();
    expect(toolEvent).toMatchObject({ tool: "tool" });
    expect(clockEvent).toMatchObject({ valueMs: 500 });
    expect(rngEvent).toMatchObject({ value: 1234 });
  });
});

describe("replay adapters", () => {
  it("replays oracle responses sequentially and throws when exhausted", async () => {
    const entries: ReplayLogEntry[] = [
      { id: "o1", tag: "E_OracleCall", response: { text: "one" } },
      { id: "o2", tag: "E_OracleCall", response: { text: "two" } },
    ];
    const log = createLog(entries);
    const oracle = replayOracle(log);
    const ctx = createCtx();

    await expect(oracle.infer({ model: "m", prompt: "" }, ctx)).resolves.toMatchObject({ text: "one" });
    await expect(oracle.infer({ model: "m", prompt: "" }, ctx)).resolves.toMatchObject({ text: "two" });
    await expect(oracle.infer({ model: "m", prompt: "" }, ctx)).rejects.toThrow(/exhausted/i);
  });

  it("replays clock reads in order and errors when empty", () => {
    const log = createLog([
      { id: "c1", tag: "E_ClockRead", value: 10 },
      { id: "c2", tag: "E_ClockRead", value: 20 },
    ]);
    const clock = replayClock(log);
    const ctx = createCtx();

    expect(clock.nowMs(ctx)).toBe(10);
    expect(clock.nowMs(ctx)).toBe(20);
    expect(() => clock.nowMs(ctx)).toThrow(/exhausted/i);
  });

  it("replays rng values for u32, float, and int", () => {
    const log = createLog([
      { id: "r1", tag: "E_RngRead", value: 0 },
      { id: "r2", tag: "E_RngRead", value: 0x7fffffff },
      { id: "r3", tag: "E_RngRead", value: 0x7fffffff },
    ]);
    const rng = replayRng(log);
    const ctx = createCtx();

    expect(rng.nextU32(ctx)).toBe(0);
    expect(rng.nextFloat(ctx)).toBeGreaterThanOrEqual(0);
    expect(rng.nextInt(0, 10, ctx)).toBeLessThan(10);
    expect(() => rng.nextU32(ctx)).toThrow(/exhausted/i);
  });

  it("round-trips logging events into replayed behavior", async () => {
    const events: TraceEvent[] = [];
    const ctx = createCtx(events);

    const realOracle = loggingOracle({
      infer: async () => ({ text: "real" }),
    });
    const realClock = loggingClock({
      nowMs: () => 111,
      sleepMs: async () => {},
    });
    const realRng = loggingRng({
      nextU32: () => 222,
      nextFloat: () => 0.5,
      nextInt: () => 7,
    });

    await realOracle.infer({ model: "gpt-4", prompt: "p" }, ctx);
    const time = realClock.nowMs(ctx);
    const rnd = realRng.nextU32(ctx);

    expect(time).toBe(111);
    expect(rnd).toBe(222);

    const oracleEvent = events.find((e) => e.tag === "E_OracleCall");
    const clockEvent = events.find((e) => e.tag === "E_ClockRead");
    const rngEvent = events.find((e) => e.tag === "E_RngRead");

    expect(oracleEvent?.id).toBeDefined();
    expect(clockEvent?.id).toBeDefined();
    expect(rngEvent?.id).toBeDefined();

    const replayLog = createLog([
      {
        id: oracleEvent!.id!,
        tag: oracleEvent!.tag,
        response: { text: "real" },
      },
      {
        id: clockEvent!.id!,
        tag: clockEvent!.tag,
        value: (clockEvent as any).valueMs,
      },
      {
        id: rngEvent!.id!,
        tag: rngEvent!.tag,
        value: (rngEvent as any).value,
      },
    ]);

    const replayedOracle = replayOracle(replayLog);
    const replayedClock = replayClock(replayLog);
    const replayedRng = replayRng(replayLog);
    const replayCtx = createCtx();

    await expect(replayedOracle.infer({ model: "gpt-4", prompt: "p" }, replayCtx)).resolves.toMatchObject({ text: "real" });
    expect(replayedClock.nowMs(replayCtx)).toBe(111);
    expect(replayedRng.nextU32(replayCtx)).toBe(222);
  });
});
