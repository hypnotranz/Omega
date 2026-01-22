import { describe, it, expect } from "vitest";
import { serializeState, deserializeState } from "../../src/core/session/serializer";
import { buildNativeRegistry } from "../../src/core/session/nativeRegistry";
import { buildSolverRegistry } from "../../src/core/session/solverRegistry";
import { COWStore } from "../../src/core/eval/store";
import { installPrims } from "../../src/core/prims";

describe("State Serializer", () => {
  function setup() {
    const store = new COWStore();
    const { env, store: primedStore } = installPrims(store);
    const nativeRegistry = buildNativeRegistry(primedStore);
    return { env, store: primedStore, nativeRegistry };
  }

  it("round-trips atomic values (Num, Bool, Str, Sym, Unit)", () => {
    const { env, store, nativeRegistry } = setup();

    for (const v of [
      { tag: "Num", n: 42 },
      { tag: "Bool", b: true },
      { tag: "Str", s: "hello" },
      { tag: "Sym", name: "foo" },
      { tag: "Unit" },
    ]) {
      const state = { control: { tag: "Val", v }, env, store, kont: [], handlers: [] };
      const json = JSON.stringify(serializeState(state));
      const restored = deserializeState(JSON.parse(json), nativeRegistry);
      expect(restored.control).toEqual(state.control);
    }
  });

  it("round-trips BigInt via string conversion", () => {
    const { env, store, nativeRegistry } = setup();
    const bigVal = { tag: "Int", value: BigInt("99999999999999999999") };
    const state = { control: { tag: "Val", v: bigVal }, env, store, kont: [], handlers: [] };

    const json = JSON.stringify(serializeState(state));
    expect(json).toContain("99999999999999999999");  // As string

    const restored = deserializeState(JSON.parse(json), nativeRegistry);
    expect(restored.control.v.value.toString()).toBe("99999999999999999999");
  });

  it("round-trips Pair and nested structures", () => {
    const { env, store, nativeRegistry } = setup();
    const pair = {
      tag: "Pair",
      car: { tag: "Num", n: 1 },
      cdr: { tag: "Pair", car: { tag: "Num", n: 2 }, cdr: { tag: "Unit" } }
    };
    const state = { control: { tag: "Val", v: pair }, env, store, kont: [], handlers: [] };

    const json = JSON.stringify(serializeState(state));
    const restored = deserializeState(JSON.parse(json), nativeRegistry);
    expect(restored.control.v.car.n).toBe(1);
    expect(restored.control.v.cdr.car.n).toBe(2);
  });

  it("round-trips Closure with captured environment", () => {
    const { env, store, nativeRegistry } = setup();
    const closure = {
      tag: "Closure",
      params: ["x", "y"],
      body: { tag: "App", fn: { tag: "Var", name: "+" }, args: [{ tag: "Var", name: "x" }, { tag: "Var", name: "y" }] },
      env: env,
    };
    const state = { control: { tag: "Val", v: closure }, env, store, kont: [], handlers: [] };

    const json = JSON.stringify(serializeState(state));
    const restored = deserializeState(JSON.parse(json), nativeRegistry);

    expect(restored.control.v.tag).toBe("Closure");
    expect(restored.control.v.params).toEqual(["x", "y"]);
    expect(restored.control.v.env).toBeDefined();
    expect(restored.control.v.env.tag).toBe("Ctx");
  });

  it("round-trips Native via registry lookup (fn not in JSON)", () => {
    const { env, store, nativeRegistry } = setup();
    const plusNative = nativeRegistry.get("+");
    const state = { control: { tag: "Val", v: plusNative }, env, store, kont: [], handlers: [] };

    const json = JSON.stringify(serializeState(state));
    expect(json).not.toContain('"fn"');  // Function not serialized
    expect(json).toContain('"name":"+"');

    const restored = deserializeState(JSON.parse(json), nativeRegistry);
    expect(restored.control.v.tag).toBe("Native");
    expect(restored.control.v.name).toBe("+");
    expect(typeof restored.control.v.fn).toBe("function");  // Restored from registry
  });

  it("round-trips Solver via registry lookup", () => {
    const store0 = new COWStore();
    const { env, store: primedStore } = installPrims(store0);

    const solver = {
      tag: "Solver",
      name: "test-solver",
      solve: () => ({ results: [], state: { control: { tag: "Val", v: { tag: "Unit" } } } }),
      estimate: () => ({ estimate: { tag: "CostEstimate", minCost: 0, maxCost: 0, expectedCost: 0, confidence: 1 }, state: { control: { tag: "Val", v: { tag: "Unit" } } } }),
    } as any;

    const [storeWithSolver] = primedStore.alloc(solver);
    const nativeRegistry = buildNativeRegistry(storeWithSolver);
    const solverRegistry = buildSolverRegistry(storeWithSolver);

    const state = { control: { tag: "Val", v: solver }, env, store: storeWithSolver, kont: [], handlers: [] };
    const json = JSON.stringify(serializeState(state));
    const restored = deserializeState(JSON.parse(json), nativeRegistry, solverRegistry);

    expect(restored.control.v.tag).toBe("Solver");
    expect(restored.control.v.name).toBe("test-solver");
    expect(typeof restored.control.v.solve).toBe("function");
    expect(typeof restored.control.v.estimate).toBe("function");
  });

  it("handles circular Ctx.parent without throwing", () => {
    const { env, store, nativeRegistry } = setup();
    const state = { control: { tag: "Val", v: { tag: "Unit" } }, env, store, kont: [], handlers: [] };

    // Should not throw "Converting circular structure to JSON"
    expect(() => JSON.stringify(serializeState(state))).not.toThrow();

    const json = JSON.stringify(serializeState(state));
    const restored = deserializeState(JSON.parse(json), nativeRegistry);
    expect(restored.env.tag).toBe("Ctx");
  });

  it("round-trips all 18 Frame types in kont", () => {
    const { env, store, nativeRegistry } = setup();

    const frames = [
      { tag: "KIf", conseq: { tag: "Num", n: 1 }, alt: { tag: "Num", n: 2 }, env },
      { tag: "KBegin", rest: [{ tag: "Num", n: 3 }], env },
      { tag: "KDefine", name: "x", env },
      { tag: "KSet", name: "y", env },
      { tag: "KAppFun", args: [{ tag: "Num", n: 4 }], env },
      { tag: "KAppArg", fnVal: { tag: "Unit" }, pending: [], acc: [], env },
      {
        tag: "KAppArgLazy",
        fnVal: { tag: "Unit" },
        pending: [{ expr: { tag: "Num", n: 5 }, idx: 0 }],
        acc: [{ idx: 0, val: { tag: "Unit" } }],
        env,
        totalArgs: 1,
        currentIdx: 0,
      },
      { tag: "KCall", savedEnv: env },
      { tag: "KEffect", op: "test.op", pending: [], acc: [], env },
      { tag: "KHandleBoundary", hid: "h1", savedHandlersDepth: 0, resumeTo: { kont: [{ tag: "KCall", savedEnv: env }], handlersDepth: 0 } },
      { tag: "KHandleReturn", mode: "exit", hid: "h1", targetKont: [{ tag: "KCall", savedEnv: env }], targetHandlersDepth: 0, savedHandlersDepth: 0 },
      { tag: "KPrompt", promptTag: { tag: "Sym", name: "prompt" }, handler: { tag: "Unit" }, env, savedKont: [], savedHandlersDepth: 0 },
      { tag: "KMatch", clauses: [], env },
      { tag: "KOracleLambda", params: ["a"], env },
      { tag: "KBind", fn: { tag: "Unit" }, env },
      { tag: "KHandlerBind", handlers: [{ type: Symbol.for("err"), handler: { tag: "Unit" } }] },
      { tag: "KRestartBind", restarts: [{ name: Symbol.for("restart"), fn: { tag: "Unit" }, description: "restart" }], savedKont: [], env, store, handlers: [] },
      { tag: "KSignaling", condition: { tag: "Condition", type: Symbol.for("cond"), message: "msg", data: { tag: "Unit" }, restarts: [] }, required: false },
    ];

    const state = { control: { tag: "Val", v: { tag: "Unit" } }, env, store, kont: frames, handlers: [] };

    const json = JSON.stringify(serializeState(state));
    const restored = deserializeState(JSON.parse(json), nativeRegistry);

    expect(restored.kont.length).toBe(frames.length);
    expect(restored.kont[0].tag).toBe("KIf");
    expect(restored.kont[6].tag).toBe("KAppArgLazy");
    expect(restored.kont[7].tag).toBe("KCall");
    expect(restored.kont[7].savedEnv.tag).toBe("Ctx");
    expect(restored.kont[9].tag).toBe("KHandleBoundary");
    expect(restored.kont[10].tag).toBe("KHandleReturn");
    expect(restored.kont[15].tag).toBe("KHandlerBind");
    expect(restored.kont[16].tag).toBe("KRestartBind");
    expect(restored.kont[17].tag).toBe("KSignaling");
  });

  it("round-trips Cont with Resumption (invoke reconstructed)", () => {
    const { env, store, nativeRegistry } = setup();

    const baseState = { control: { tag: "Val", v: { tag: "Unit" } }, env, store, kont: [], handlers: [] };
    const cont = {
      tag: "Cont",
      hid: "h-123",
      boundaryIndex: 0,
      resumption: {
        rid: "r-456",
        base: baseState,
        invoke: (v) => ({ ...baseState, control: { tag: "Val", v } }),
        digest: () => "test",
      },
    };

    const state = { control: { tag: "Val", v: cont }, env, store, kont: [], handlers: [] };

    const json = JSON.stringify(serializeState(state));
    const restored = deserializeState(JSON.parse(json), nativeRegistry);

    expect(restored.control.v.tag).toBe("Cont");
    expect(restored.control.v.resumption.rid).toBe("r-456");
    expect(typeof restored.control.v.resumption.invoke).toBe("function");

    // Test that invoke actually works
    const resumed = restored.control.v.resumption.invoke({ tag: "Num", n: 99 });
    expect(resumed.control.v.n).toBe(99);
  });

  it("round-trips Store with multiple cells", () => {
    const { env, nativeRegistry } = setup();
    let store = new COWStore();
    [store] = store.alloc({ tag: "Num", n: 10 });
    [store] = store.alloc({ tag: "Str", s: "hello" });
    [store] = store.alloc({ tag: "Bool", b: true });

    const state = { control: { tag: "Val", v: { tag: "Unit" } }, env, store, kont: [], handlers: [] };

    const json = JSON.stringify(serializeState(state));
    const restored = deserializeState(JSON.parse(json), nativeRegistry);

    expect(restored.store.read(0).n).toBe(10);
    expect(restored.store.read(1).s).toBe("hello");
    expect(restored.store.read(2).b).toBe(true);
  });

  it("round-trips Dist (distribution) values", () => {
    const { env, store, nativeRegistry } = setup();
    const dist = {
      tag: "Dist",
      support: [
        { v: { tag: "Str", s: "yes" }, w: 0.7 },
        { v: { tag: "Str", s: "no" }, w: 0.3 },
      ],
      normalized: true,
    };
    const state = { control: { tag: "Val", v: dist }, env, store, kont: [], handlers: [] };

    const json = JSON.stringify(serializeState(state));
    const restored = deserializeState(JSON.parse(json), nativeRegistry);

    expect(restored.control.v.support.length).toBe(2);
    expect(restored.control.v.support[0].w).toBe(0.7);
  });
});
