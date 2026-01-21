import { describe, expect, it } from "vitest";
import type { Span } from "../../src/frameir/meta";
import {
  childContext,
  createPortSet,
  restrictCaps,
  validateOracleCap,
  validateStoreCap,
  validateToolCap,
} from "../../src/ports";
import type {
  CapabilitySet,
  ExecContext,
  PortSet,
  TraceEvent,
} from "../../src/ports";

function createTraceSink(events: TraceEvent[] = []) {
  return {
    emit: (event: TraceEvent) => events.push(event),
  };
}

function baseCaps(): CapabilitySet {
  return {
    oracleCap: { allowedModels: ["gpt-4", "gpt-3.5"] },
    toolCap: { allowedContracts: new Set(["contract-1", "contract-2"]) },
    storeCap: { allowedStores: new Set(["store-1", "store-2"]) },
  };
}

function createCtx(events: TraceEvent[] = [], caps: CapabilitySet = baseCaps()): ExecContext {
  return {
    runId: "run-123",
    caps,
    trace: createTraceSink(events),
  };
}

describe("ExecContext helpers", () => {
  it("creates a child context with updated span", () => {
    const ctx = createCtx();
    const span: Span = { file: "test.ts", startLine: 1, startCol: 0 };

    const child = childContext(ctx, span);

    expect(child).not.toBe(ctx);
    expect(child.span).toEqual(span);
    expect(child.runId).toBe(ctx.runId);
    expect(child.caps).toBe(ctx.caps);
  });

  it("restricts capabilities without mutating parent", () => {
    const ctx = createCtx();
    const restricted = restrictCaps(ctx, {
      oracleCap: { allowedModels: ["gpt-3.5"] },
    });

    expect(restricted).not.toBe(ctx);
    expect(restricted.caps.oracleCap?.allowedModels).toEqual(["gpt-3.5"]);
    expect(restricted.caps.toolCap).toBe(ctx.caps.toolCap);
    expect(ctx.caps.oracleCap?.allowedModels).toEqual(["gpt-4", "gpt-3.5"]);
  });
});

describe("capability validation", () => {
  it("allows oracle calls for permitted models", () => {
    const ctx = createCtx();
    expect(() => validateOracleCap({ model: "gpt-4", prompt: "hi" }, ctx)).not.toThrow();
  });

  it("throws when oracle capability missing or model not allowed", () => {
    const ctxNoCap = createCtx([], { ...baseCaps(), oracleCap: undefined });
    expect(() => validateOracleCap({ model: "gpt-4", prompt: "hi" }, ctxNoCap)).toThrow(/No OracleCap/);

    const ctx = createCtx();
    expect(() => validateOracleCap({ model: "gpt-2", prompt: "hi" }, ctx)).toThrow(/not allowed/);
  });

  it("validates tool contracts", () => {
    const ctx = createCtx();
    expect(() => validateToolCap({ name: "t", args: {}, contractId: "contract-1" }, ctx)).not.toThrow();
    expect(() => validateToolCap({ name: "t", args: {}, contractId: "denied" }, ctx)).toThrow(/not allowed/);
  });

  it("validates store operations and read-only restrictions", () => {
    const ctx = createCtx();
    expect(() => validateStoreCap("store-1", "get", ctx)).not.toThrow();

    expect(() => validateStoreCap("store-3", "get", ctx)).toThrow(/not allowed/);

    const readOnlyCtx = createCtx([], {
      ...baseCaps(),
      storeCap: { allowedStores: new Set(["store-1"]), readOnly: true },
    });
    expect(() => validateStoreCap("store-1", "put", readOnlyCtx)).toThrow(/read-only/);
    expect(() => validateStoreCap("store-1", "get", readOnlyCtx)).not.toThrow();
  });
});

describe("PortSet", () => {
  it("creates a composite port set with all adapters wired", async () => {
    const events: TraceEvent[] = [];
    const ctx = createCtx(events);

    const ports: PortSet = createPortSet({
      oracle: {
        infer: async () => ({ text: "ok" }),
      },
      tool: {
        call: async () => ({ ok: true, value: 1 }),
      },
      store: {
        get: async () => "value",
        put: async () => {},
        has: async () => true,
        delete: async () => {},
      },
      sink: {
        emit: async () => {},
      },
      source: {
        observe: async () => "observed",
      },
      clock: {
        nowMs: () => 123,
        sleepMs: async () => {},
      },
      rng: {
        nextU32: () => 42,
        nextFloat: () => 0.42,
        nextInt: () => 7,
      },
    });

    expect(await ports.oracle.infer({ model: "gpt-4", prompt: "hi" }, ctx)).toMatchObject({ text: "ok" });
    expect(await ports.tool.call({ name: "t", args: {}, contractId: "contract-1" }, ctx)).toMatchObject({ ok: true });
    expect(await ports.store.get("store-1", "k", ctx)).toBe("value");
    expect(await ports.store.has("store-1", "k", ctx)).toBe(true);
    await ports.store.put("store-1", "k", "v", ctx);
    await ports.store.delete("store-1", "k", ctx);
    await ports.sink.emit("sink-1", { data: 1 }, ctx);
    expect(await ports.source.observe("source-1", undefined, ctx)).toBe("observed");
    expect(ports.clock.nowMs(ctx)).toBe(123);
    await ports.clock.sleepMs(1, ctx);
    expect(ports.rng.nextU32(ctx)).toBe(42);
    expect(ports.rng.nextFloat(ctx)).toBeCloseTo(0.42);
    expect(ports.rng.nextInt(0, 10, ctx)).toBe(7);

    expect(events).toHaveLength(0);
  });
});
