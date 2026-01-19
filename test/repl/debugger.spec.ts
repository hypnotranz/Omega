// test/repl/debugger.spec.ts
// Tests for REPL debugger functionality

import { describe, it, expect, beforeEach } from "vitest";
import { COWStore } from "../../src/core/eval/store";
import { RuntimeImpl } from "../../src/core/effects/runtimeImpl";
import { ScriptedOracleAdapter } from "../../src/core/oracle/scriptedOracle";
import { SnapshotRepo } from "../../src/core/oracle/snapshots";
import { InMemoryReceiptStore } from "../../src/core/oracle/receipts";
import { runToCompletionWithState } from "../../src/core/eval/run";
import { installPrims } from "../helpers/prims";
import { compileTextToExpr } from "../../src/core/pipeline/compileText";
import type { State, Frame } from "../../src/core/eval/machine";
import type { Val } from "../../src/core/eval/values";
import { VUnit } from "../../src/core/eval/values";
import { envGet } from "../../src/core/eval/env";

// Helper to evaluate and get final state (mimics REPL's evalInRepl)
async function evalWithState(src: string): Promise<{ value: Val; state: State }> {
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

  return runToCompletionWithState(runtime, state0, 100_000);
}

// Frame pretty-printing (from omega-repl.ts)
function frameToString(frame: Frame, index: number): string {
  switch (frame.tag) {
    case "KIf": return `  ${index}: KIf (if ... conseq alt)`;
    case "KBegin": return `  ${index}: KBegin (${frame.rest.length} more forms)`;
    case "KDefine": return `  ${index}: KDefine (define ${frame.name} ...)`;
    case "KSet": return `  ${index}: KSet (set! ${frame.name} ...)`;
    case "KAppFun": return `  ${index}: KAppFun (evaluating function, ${frame.args.length} args pending)`;
    case "KAppArg": return `  ${index}: KAppArg (evaluated fn, ${frame.acc.length}/${frame.pending.length + frame.acc.length} args)`;
    case "KCall": return `  ${index}: KCall (in function body)`;
    case "KEffect": return `  ${index}: KEffect (${frame.op}, ${frame.acc.length}/${frame.pending.length + frame.acc.length} args)`;
    case "KHandleBoundary": return `  ${index}: KHandleBoundary (handler ${frame.hid})`;
    case "KHandleReturn": return `  ${index}: KHandleReturn (${frame.mode} handler ${frame.hid})`;
    case "KMatch": return `  ${index}: KMatch (${frame.clauses.length} clauses)`;
    default: return `  ${index}: ${(frame as any).tag ?? "Unknown"}`;
  }
}

// Value pretty-printing (from omega-repl.ts)
function valToSexp(v: Val): string {
  switch (v.tag) {
    case "Unit": return "null";
    case "Num": return String((v as any).n);
    case "Bool": return (v as any).b ? "#t" : "#f";
    case "Str": return JSON.stringify((v as any).s);
    case "Sym": return (v as any).name;
    case "Vector": return `[${(v as any).items.map(valToSexp).join(" ")}]`;
    case "Closure": return `<closure>`;
    case "Native": return `<prim>`;
    case "Cont": return `<continuation>`;
    default: return JSON.stringify(v);
  }
}

describe("REPL Debugger: State Tracking", () => {
  it("captures final state after evaluation", async () => {
    const { value, state } = await evalWithState("(+ 1 2)");

    expect(value.tag).toBe("Num");
    expect((value as any).n).toBe(3);
    expect(state).toBeDefined();
    expect(state.control).toBeDefined();
  });

  it("final control is a value after completion", async () => {
    const { state } = await evalWithState("(* 3 4)");

    expect(state.control.tag).toBe("Val");
    if (state.control.tag === "Val") {
      expect(valToSexp(state.control.v)).toBe("12");
    }
  });

  it("final kont is empty after completion", async () => {
    const { state } = await evalWithState("(+ 1 2)");

    // After evaluation completes, continuation stack should be empty
    expect(state.kont.length).toBe(0);
  });

  it("final env contains bindings from defines (with internal names)", async () => {
    const { state } = await evalWithState(`
      (define x 42)
      (define y 100)
      x
    `);

    // User-defined bindings use internal names (hygiene: "x$bid#N")
    // Check that bindings with prefix "x$" and "y$" exist
    const frameKeys = Array.from(state.env.frame.keys());
    const xBinding = frameKeys.find(k => k.startsWith("x$"));
    const yBinding = frameKeys.find(k => k.startsWith("y$"));

    expect(xBinding).toBeDefined();
    expect(yBinding).toBeDefined();

    if (xBinding) {
      const xAddr = state.env.frame.get(xBinding);
      if (xAddr !== undefined) {
        const xVal = state.store.read(xAddr);
        expect(xVal.tag).toBe("Num");
        expect((xVal as any).n).toBe(42);
      }
    }

    if (yBinding) {
      const yAddr = state.env.frame.get(yBinding);
      if (yAddr !== undefined) {
        const yVal = state.store.read(yAddr);
        expect(yVal.tag).toBe("Num");
        expect((yVal as any).n).toBe(100);
      }
    }
  });

  it("env contains primitive bindings", async () => {
    const { state } = await evalWithState("1");

    // Primitives should be in the env (use envGet to check)
    expect(envGet(state.env, "+")).toBeDefined();
    expect(envGet(state.env, "-")).toBeDefined();
    expect(envGet(state.env, "*")).toBeDefined();
    expect(envGet(state.env, "/")).toBeDefined();
  });
});

describe("REPL Debugger: :env command functionality", () => {
  it("can lookup primitive binding", async () => {
    const { state } = await evalWithState("1");

    const plusAddr = envGet(state.env, "+");
    expect(plusAddr).toBeDefined();

    if (plusAddr !== undefined) {
      const plusVal = state.store.read(plusAddr);
      expect(plusVal.tag).toBe("Native");
    }
  });

  it("can lookup user-defined binding (with internal name)", async () => {
    const { state } = await evalWithState(`
      (define square (lambda (x) (* x x)))
      1
    `);

    // User bindings use internal names: "square$bid#N"
    const frameKeys = Array.from(state.env.frame.keys());
    const squareBinding = frameKeys.find(k => k.startsWith("square$"));
    expect(squareBinding).toBeDefined();

    if (squareBinding) {
      const squareAddr = state.env.frame.get(squareBinding);
      if (squareAddr !== undefined) {
        const squareVal = state.store.read(squareAddr);
        expect(squareVal.tag).toBe("Closure");
      }
    }
  });

  it("returns undefined for missing binding", async () => {
    const { state } = await evalWithState("1");

    const missing = envGet(state.env, "nonexistent");
    expect(missing).toBeUndefined();
  });
});

describe("REPL Debugger: :control command functionality", () => {
  it("shows value when evaluation complete", async () => {
    const { state } = await evalWithState("(+ 10 20)");

    expect(state.control.tag).toBe("Val");
    if (state.control.tag === "Val") {
      expect(state.control.v.tag).toBe("Num");
      expect((state.control.v as any).n).toBe(30);
    }
  });

  it("value can be formatted as sexp", async () => {
    const { state } = await evalWithState('"hello"');

    if (state.control.tag === "Val") {
      const sexp = valToSexp(state.control.v);
      expect(sexp).toBe('"hello"');
    }
  });
});

describe("REPL Debugger: Value formatting", () => {
  it("formats numbers", async () => {
    const { value } = await evalWithState("42");
    expect(valToSexp(value)).toBe("42");
  });

  it("formats booleans", async () => {
    const { value: t } = await evalWithState("#t");
    expect(valToSexp(t)).toBe("#t");

    const { value: f } = await evalWithState("#f");
    expect(valToSexp(f)).toBe("#f");
  });

  it("formats strings", async () => {
    const { value } = await evalWithState('"test"');
    expect(valToSexp(value)).toBe('"test"');
  });

  it("formats unit/null", async () => {
    const { value } = await evalWithState("(if #f 1 null)");
    expect(valToSexp(value)).toBe("null");
  });

  it("formats closures", async () => {
    const { value } = await evalWithState("(lambda (x) x)");
    expect(valToSexp(value)).toContain("closure");
  });

  it("formats primitives", async () => {
    const { state } = await evalWithState("1");
    const plusAddr = envGet(state.env, "+");
    if (plusAddr !== undefined) {
      const plusVal = state.store.read(plusAddr);
      expect(valToSexp(plusVal)).toBe("<prim>");
    }
  });
});

describe("REPL Debugger: Frame formatting", () => {
  // We can't easily capture intermediate stack states,
  // but we can test the frameToString function

  it("formats KIf frame", () => {
    const frame: Frame = {
      tag: "KIf",
      conseq: { tag: "Lit", v: { tag: "Num", n: 1 } } as any,
      alt: { tag: "Lit", v: { tag: "Num", n: 2 } } as any,
      env: new Map(),
    };
    const str = frameToString(frame, 0);
    expect(str).toContain("KIf");
    expect(str).toContain("conseq");
  });

  it("formats KBegin frame", () => {
    const frame: Frame = {
      tag: "KBegin",
      rest: [
        { tag: "Lit", v: { tag: "Num", n: 1 } },
        { tag: "Lit", v: { tag: "Num", n: 2 } },
      ] as any,
      env: new Map(),
    };
    const str = frameToString(frame, 1);
    expect(str).toContain("KBegin");
    expect(str).toContain("2 more forms");
  });

  it("formats KDefine frame", () => {
    const frame: Frame = {
      tag: "KDefine",
      name: "myvar",
      env: new Map(),
    };
    const str = frameToString(frame, 2);
    expect(str).toContain("KDefine");
    expect(str).toContain("myvar");
  });

  it("formats KAppFun frame", () => {
    const frame: Frame = {
      tag: "KAppFun",
      args: [{ tag: "Lit" }, { tag: "Lit" }] as any,
      env: new Map(),
    };
    const str = frameToString(frame, 0);
    expect(str).toContain("KAppFun");
    expect(str).toContain("2 args pending");
  });

  it("formats KAppArg frame", () => {
    const frame: Frame = {
      tag: "KAppArg",
      fnVal: { tag: "Native", fn: () => {} } as any,
      pending: [{ tag: "Lit" }] as any,
      acc: [{ tag: "Num", n: 1 }] as any,
      env: new Map(),
    };
    const str = frameToString(frame, 0);
    expect(str).toContain("KAppArg");
    expect(str).toContain("1/2 args");
  });

  it("formats KEffect frame", () => {
    const frame: Frame = {
      tag: "KEffect",
      op: "infer.op",
      pending: [],
      acc: [{ tag: "Str", s: "prompt" }] as any,
      env: new Map(),
    };
    const str = frameToString(frame, 0);
    expect(str).toContain("KEffect");
    expect(str).toContain("infer.op");
    expect(str).toContain("1/1 args");
  });
});

describe("REPL Integration: Complete evaluation flow", () => {
  it("defines and uses a function", async () => {
    const { value, state } = await evalWithState(`
      (define (fact n)
        (if (= n 0)
            1
            (* n (fact (- n 1)))))
      (fact 5)
    `);

    expect(value.tag).toBe("Num");
    expect((value as any).n).toBe(120);

    // fact should be in env (with internal name "fact$bid#N")
    const frameKeys = Array.from(state.env.frame.keys());
    const factBinding = frameKeys.find(k => k.startsWith("fact$"));
    expect(factBinding).toBeDefined();
  });

  it("let bindings are evaluated correctly", async () => {
    const { value } = await evalWithState(`
      (let ((x 10)
            (y 20))
        (+ x y))
    `);

    expect(value.tag).toBe("Num");
    expect((value as any).n).toBe(30);
  });

  it("nested expressions work", async () => {
    const { value } = await evalWithState(`
      (+ (* 2 3) (/ 10 2))
    `);

    expect(value.tag).toBe("Num");
    expect((value as any).n).toBe(11); // 6 + 5
  });

  it("conditionals work", async () => {
    const { value: t } = await evalWithState(`(if (> 5 3) "yes" "no")`);
    expect((t as any).s).toBe("yes");

    const { value: f } = await evalWithState(`(if (< 5 3) "yes" "no")`);
    expect((f as any).s).toBe("no");
  });

  it("multiple defines accumulate", async () => {
    const { value, state } = await evalWithState(`
      (define a 1)
      (define b 2)
      (define c 3)
      (+ a (+ b c))
    `);

    // Value should be 1 + 2 + 3 = 6
    expect(value.tag).toBe("Num");
    expect((value as any).n).toBe(6);

    // Check that bindings exist (with internal names "a$bid#N", etc.)
    const frameKeys = Array.from(state.env.frame.keys());
    expect(frameKeys.find(k => k.startsWith("a$"))).toBeDefined();
    expect(frameKeys.find(k => k.startsWith("b$"))).toBeDefined();
    expect(frameKeys.find(k => k.startsWith("c$"))).toBeDefined();
  });
});

// Helper to get definitions from state (mimics REPL :defs behavior)
function getDefsFromState(state: State): Array<{ name: string; type: string }> {
  const defs: Array<{ name: string; type: string }> = [];
  const seen = new Set<string>();

  // Walk env chain collecting non-Native values
  for (let cur: any = state.env; cur && cur.tag === "Ctx"; cur = cur.parent) {
    const frame = cur.frame as Map<string, number>;
    for (const [name, addr] of frame) {
      if (!seen.has(name)) {
        seen.add(name);
        try {
          const val = state.store.read(addr);
          if (val && val.tag !== "Native") {
            defs.push({ name, type: val.tag });
          }
        } catch {
          // skip stale refs
        }
      }
    }
  }
  return defs;
}

// Helper to get stack from state (mimics REPL :stack behavior)
function getStackFromState(state: State): Array<{ index: number; tag: string }> {
  return state.kont.map((fr, i) => ({
    index: i,
    tag: (fr as any).tag ?? "Unknown"
  }));
}

describe("REPL Debugger: :defs command functionality", () => {
  it("lists user-defined functions", async () => {
    const { state } = await evalWithState(`
      (define (add x y) (+ x y))
      (define (square n) (* n n))
      1
    `);

    const defs = getDefsFromState(state);
    expect(defs.length).toBeGreaterThan(0);

    // Should have closures for add and square
    const closures = defs.filter(d => d.type === "Closure");
    expect(closures.length).toBeGreaterThanOrEqual(2);
  });

  it("lists user-defined values", async () => {
    const { state } = await evalWithState(`
      (define mynum 42)
      (define mystr "hello")
      1
    `);

    const defs = getDefsFromState(state);
    const nums = defs.filter(d => d.type === "Num");
    const strs = defs.filter(d => d.type === "Str");

    expect(nums.length).toBeGreaterThanOrEqual(1);
    expect(strs.length).toBeGreaterThanOrEqual(1);
  });

  it("returns empty for no user definitions", async () => {
    const { state } = await evalWithState("(+ 1 2)");

    const defs = getDefsFromState(state);
    // Filter out built-in constants like the-empty-stream
    const userDefs = defs.filter(d => d.name !== "the-empty-stream");
    // No user definitions, only primitives (which are excluded)
    expect(userDefs.length).toBe(0);
  });

  it("excludes primitives (Native)", async () => {
    const { state } = await evalWithState("1");

    const defs = getDefsFromState(state);
    // All primitives are Native and should be excluded
    const natives = defs.filter(d => d.type === "Native");
    expect(natives.length).toBe(0);
  });
});

describe("REPL Debugger: :stack command functionality", () => {
  it("shows empty stack after completion", async () => {
    const { state } = await evalWithState("(+ 1 2)");

    const stack = getStackFromState(state);
    // After evaluation completes, stack should be empty
    expect(stack.length).toBe(0);
  });

  it("stack format includes frame tags", async () => {
    // We test the frameToString function with various frame types
    const frames: Frame[] = [
      { tag: "KIf", conseq: {} as any, alt: {} as any, env: new Map() },
      { tag: "KBegin", rest: [{} as any, {} as any], env: new Map() },
    ];

    for (const frame of frames) {
      const str = frameToString(frame, 0);
      expect(str).toContain(frame.tag);
    }
  });
});

describe("REPL Debugger: :frame command functionality", () => {
  it("frames have correct structure", async () => {
    // Test frame formatting function directly
    const frame: Frame = {
      tag: "KCall",
      saved: new Map(),
      env: new Map() as any,
    };

    const str = frameToString(frame, 5);
    expect(str).toContain("5:");
    expect(str).toContain("KCall");
  });

  it("all frame types are handled", () => {
    const frameTypes = [
      { tag: "KIf", conseq: {} as any, alt: {} as any, env: new Map() },
      { tag: "KBegin", rest: [], env: new Map() },
      { tag: "KDefine", name: "test", env: new Map() },
      { tag: "KSet", name: "test", env: new Map() },
      { tag: "KAppFun", args: [], env: new Map() },
      { tag: "KAppArg", fnVal: {} as any, pending: [], acc: [], env: new Map() },
      { tag: "KCall", saved: new Map(), env: new Map() },
      { tag: "KEffect", op: "test", pending: [], acc: [], env: new Map() },
      { tag: "KHandleBoundary", hid: "h1" },
      { tag: "KHandleReturn", hid: "h1", mode: "shallow" },
      { tag: "KMatch", clauses: [], env: new Map() },
    ] as Frame[];

    for (const frame of frameTypes) {
      const str = frameToString(frame, 0);
      expect(str).toBeDefined();
      expect(str.length).toBeGreaterThan(0);
    }
  });
});
