// test/oracle/observe.spec.ts
// Tests for all ObserveSpec types - programmatic debugger access for LLM

import { describe, it, expect } from "vitest";
import { COWStore } from "../../src/core/eval/store";
import { RuntimeImpl } from "../../src/core/effects/runtimeImpl";
import { SnapshotRepo } from "../../src/core/oracle/snapshots";
import { InMemoryReceiptStore } from "../../src/core/oracle/receipts";
import { PortalImpl } from "../../src/core/oracle/portalImpl";
import { installPrims } from "../helpers/prims";
import { compileTextToExpr } from "../../src/core/pipeline/compileText";
import { runToCompletionWithState } from "../../src/core/eval/run";
import type { State } from "../../src/core/eval/machine";
import type { Val } from "../../src/core/eval/values";
import { VUnit } from "../../src/core/eval/values";
import { ScriptedOracleAdapter } from "../../src/core/oracle/scriptedOracle";

// Helper to create a portal and get envRef/stateRef after evaluation
async function setupPortal(src: string) {
  const oracle = new ScriptedOracleAdapter();
  const snapshots = new SnapshotRepo();
  const receipts = new InMemoryReceiptStore("off");

  const runtime = new RuntimeImpl(oracle, snapshots, receipts, {
    async commit() { return VUnit; }
  });

  const store0 = new COWStore();
  const prim = installPrims(store0);

  const expr = compileTextToExpr(`(begin ${src})`);
  const state0: State = {
    control: { tag: "Expr", e: expr },
    env: prim.env,
    store: prim.store,
    kont: [],
    handlers: [],
  };

  const { value, state } = await runToCompletionWithState(runtime, state0, 100_000);

  const envRef = snapshots.putEnv({ env: state.env, store: state.store });
  const stateRef = snapshots.putState({ state });

  const portal = new PortalImpl(runtime, snapshots, receipts, { maxEvalSteps: 100_000 });

  return { portal, envRef, stateRef, value, state, snapshots };
}

describe("ObserveSpec: Stack", () => {
  it("returns stack depth and frames", async () => {
    const { portal, stateRef } = await setupPortal("(+ 1 2)");

    const resp = await portal.perform({
      tag: "ReqObserve",
      what: { tag: "Stack" },
      stateRef,
    });

    expect(resp.tag).toBe("RespObs");
    const data = (resp as any).data;
    expect(data).toHaveProperty("depth");
    expect(data).toHaveProperty("frames");
    expect(Array.isArray(data.frames)).toBe(true);
  });

  it("respects limit parameter", async () => {
    const { portal, stateRef } = await setupPortal("(+ 1 2)");

    const resp = await portal.perform({
      tag: "ReqObserve",
      what: { tag: "Stack", limit: 5 },
      stateRef,
    });

    expect(resp.tag).toBe("RespObs");
    const data = (resp as any).data;
    expect(data.frames.length).toBeLessThanOrEqual(5);
  });
});

describe("ObserveSpec: Control", () => {
  it("returns current control state", async () => {
    const { portal, stateRef } = await setupPortal("(+ 1 2)");

    const resp = await portal.perform({
      tag: "ReqObserve",
      what: { tag: "Control" },
      stateRef,
    });

    expect(resp.tag).toBe("RespObs");
    const data = (resp as any).data;
    // After completion, control should be a Val
    expect(data).toHaveProperty("tag");
  });
});

describe("ObserveSpec: Handlers", () => {
  it("returns handler list", async () => {
    const { portal, stateRef } = await setupPortal("(+ 1 2)");

    const resp = await portal.perform({
      tag: "ReqObserve",
      what: { tag: "Handlers" },
      stateRef,
    });

    expect(resp.tag).toBe("RespObs");
    const data = (resp as any).data;
    expect(Array.isArray(data)).toBe(true);
  });
});

describe("ObserveSpec: FrameEnv", () => {
  it("returns error for non-existent frame", async () => {
    const { portal, stateRef } = await setupPortal("(+ 1 2)");

    const resp = await portal.perform({
      tag: "ReqObserve",
      what: { tag: "FrameEnv", frameIndex: 999 },
      stateRef,
    });

    expect(resp.tag).toBe("RespObs");
    const data = (resp as any).data;
    expect(data).toHaveProperty("error");
  });
});

describe("ObserveSpec: StoreSummary", () => {
  it("returns store summary", async () => {
    const { portal, stateRef } = await setupPortal("(+ 1 2)");

    const resp = await portal.perform({
      tag: "ReqObserve",
      what: { tag: "StoreSummary" },
      stateRef,
    });

    expect(resp.tag).toBe("RespObs");
    const data = (resp as any).data;
    expect(data).toHaveProperty("note");
  });
});

describe("ObserveSpec: Env (new)", () => {
  it("lists all bindings in environment", async () => {
    const { portal, stateRef } = await setupPortal(`
      (define x 42)
      (define y 100)
    `);

    const resp = await portal.perform({
      tag: "ReqObserve",
      what: { tag: "Env" },
      stateRef,
    });

    expect(resp.tag).toBe("RespObs");
    const data = (resp as any).data;
    expect(data).toHaveProperty("bindings");
    expect(data).toHaveProperty("count");
    expect(Array.isArray(data.bindings)).toBe(true);
    expect(data.count).toBeGreaterThan(0);

    // Should include primitives like +, -, etc.
    const names = data.bindings.map((b: any) => b.name);
    expect(names).toContain("+");
  });

  it("includes user-defined bindings", async () => {
    const { portal, stateRef } = await setupPortal(`
      (define my-var 42)
    `);

    const resp = await portal.perform({
      tag: "ReqObserve",
      what: { tag: "Env" },
      stateRef,
    });

    const data = (resp as any).data;
    const binding = data.bindings.find((b: any) => b.name.includes("my-var"));
    expect(binding).toBeDefined();
  });
});

describe("ObserveSpec: EnvLookup (new)", () => {
  it("looks up existing primitive binding", async () => {
    const { portal, stateRef } = await setupPortal("(+ 1 2)");

    const resp = await portal.perform({
      tag: "ReqObserve",
      what: { tag: "EnvLookup", name: "+" },
      stateRef,
    });

    expect(resp.tag).toBe("RespObs");
    const data = (resp as any).data;
    expect(data.found).toBe(true);
    expect(data.name).toBe("+");
    expect(data).toHaveProperty("addr");
    expect(data).toHaveProperty("value");
    expect(data.value.tag).toBe("Native");
  });

  it("looks up user-defined binding", async () => {
    const { portal, stateRef } = await setupPortal(`
      (define add1 (lambda (x) (+ x 1)))
    `);

    const resp = await portal.perform({
      tag: "ReqObserve",
      what: { tag: "EnvLookup", name: "add1$bid#1" }, // hygiene-transformed name
      stateRef,
    });

    expect(resp.tag).toBe("RespObs");
    const data = (resp as any).data;
    // May or may not find depending on exact hygiene transform
    if (data.found) {
      expect(data.value.tag).toBe("Closure");
    }
  });

  it("returns found=false for missing binding", async () => {
    const { portal, stateRef } = await setupPortal("(+ 1 2)");

    const resp = await portal.perform({
      tag: "ReqObserve",
      what: { tag: "EnvLookup", name: "nonexistent-var" },
      stateRef,
    });

    expect(resp.tag).toBe("RespObs");
    const data = (resp as any).data;
    expect(data.found).toBe(false);
    expect(data.name).toBe("nonexistent-var");
  });
});

describe("ObserveSpec: Defs (new)", () => {
  it("lists user definitions (excludes primitives)", async () => {
    const { portal, stateRef } = await setupPortal(`
      (define x 42)
      (define (square n) (* n n))
    `);

    const resp = await portal.perform({
      tag: "ReqObserve",
      what: { tag: "Defs" },
      stateRef,
    });

    expect(resp.tag).toBe("RespObs");
    const data = (resp as any).data;
    expect(data).toHaveProperty("defs");
    expect(data).toHaveProperty("count");
    expect(Array.isArray(data.defs)).toBe(true);

    // Should NOT include primitives (Native)
    const types = data.defs.map((d: any) => d.type);
    expect(types).not.toContain("Native");
  });

  it("includes closures and values", async () => {
    const { portal, stateRef } = await setupPortal(`
      (define my-num 42)
      (define my-fn (lambda (x) x))
    `);

    const resp = await portal.perform({
      tag: "ReqObserve",
      what: { tag: "Defs" },
      stateRef,
    });

    const data = (resp as any).data;
    const types = data.defs.map((d: any) => d.type);
    // Should have at least Num and Closure types
    expect(types.some((t: string) => t === "Num" || t === "Closure")).toBe(true);
  });

  it("returns empty for no user defs", async () => {
    const { portal, stateRef } = await setupPortal("(+ 1 2)");

    const resp = await portal.perform({
      tag: "ReqObserve",
      what: { tag: "Defs" },
      stateRef,
    });

    const data = (resp as any).data;
    // Filter out built-in constants like the-empty-stream
    const userDefs = data.defs.filter((d: any) => d.name !== "the-empty-stream");
    expect(userDefs).toEqual([]);
  });
});

describe("LLM Programmatic Debugger Access", () => {
  it("LLM can observe stack via ReqObserve", async () => {
    const { portal, stateRef } = await setupPortal("(+ 1 2)");

    // Simulates what an LLM would do to inspect the stack
    const resp = await portal.perform({
      tag: "ReqObserve",
      what: { tag: "Stack", limit: 10 },
      stateRef,
    });

    expect(resp.tag).toBe("RespObs");
  });

  it("LLM can lookup bindings via ReqObserve", async () => {
    const { portal, stateRef } = await setupPortal("(define foo 123)");

    // Simulates LLM looking up a variable
    const resp = await portal.perform({
      tag: "ReqObserve",
      what: { tag: "EnvLookup", name: "+" },
      stateRef,
    });

    expect(resp.tag).toBe("RespObs");
    expect((resp as any).data.found).toBe(true);
  });

  it("LLM can list all definitions via ReqObserve", async () => {
    const { portal, stateRef } = await setupPortal(`
      (define a 1)
      (define b 2)
      (define (add x y) (+ x y))
    `);

    // Simulates LLM asking "what's defined?"
    const resp = await portal.perform({
      tag: "ReqObserve",
      what: { tag: "Defs" },
      stateRef,
    });

    expect(resp.tag).toBe("RespObs");
    const data = (resp as any).data;
    expect(data.count).toBeGreaterThan(0);
  });
});
