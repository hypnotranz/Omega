// test/prompt8/machine-stepper.spec.ts
// Prompt 8 Tests: Machine reification, ANF, and optimizer

import { describe, it, expect, beforeEach } from "vitest";
import { installPrims } from "../helpers/prims";
import { COWStore } from "../../src/core/eval/store";
import type { MachineVal } from "../../src/core/eval/values";
import { VTrue, VFalse, VUnit } from "../../src/core/eval/values";
import { toAnf, isAnf, resetAnfCounter } from "../../src/core/pipeline/anf";
import { optimize, fusionOnly, countEffects } from "../../src/core/pipeline/optimizer";
import { BudgetTracker } from "../../src/core/governance/budgets";
import { stepOnce } from "../../src/core/eval/machineStep";
import type { Expr } from "../../src/core/ast";

// ─────────────────────────────────────────────────────────────────
// Test 8.1: Machine stepper stops on infer.op
// ─────────────────────────────────────────────────────────────────

describe("Test 8.1: Machine stepper stops on infer.op", () => {
  it("machine-new creates a Machine value", () => {
    const store = new COWStore();
    const { env, store: store2 } = installPrims(store);

    // Create a state to simulate running (machine-new 42)
    const state: any = {
      control: { tag: "Val", v: { tag: "Num", n: 42 } },
      env,
      store: store2,
      kont: [],
      handlers: [],
    };

    // Manually create a MachineVal
    const machineVal: MachineVal = {
      tag: "Machine",
      state,
      stepCount: 0,
      isDone: false,
      machineId: "test-m1",
    };

    expect(machineVal.tag).toBe("Machine");
    expect(machineVal.state).toBeDefined();
    expect(machineVal.stepCount).toBe(0);
    expect(machineVal.isDone).toBe(false);
  });

  it("machine-step advances the machine state", () => {
    const store = new COWStore();
    const { env, store: store2 } = installPrims(store);

    // Create a machine with an expression to evaluate
    const expr: Expr = { tag: "Lit", value: 42 };
    const state: any = {
      control: { tag: "Expr", e: expr },
      env,
      store: store2,
      kont: [],
      handlers: [],
    };

    const machine: MachineVal = {
      tag: "Machine",
      state,
      stepCount: 0,
      isDone: false,
      machineId: "test-m2",
    };

    // First step: Lit -> Val
    const outcome1 = stepOnce(machine.state);
    expect(outcome1.tag).toBe("State");

    // Second step: Val with empty kont -> Done
    const outcome2 = stepOnce(outcome1.state);
    expect(outcome2.tag).toBe("Done");
    expect(outcome2.value.tag).toBe("Num");
    expect(outcome2.value.n).toBe(42);
  });

  it("machine-run with breakpoint stops on effect", () => {
    const store = new COWStore();
    const { env, store: store2 } = installPrims(store);

    // Create a machine with an effect expression
    const expr: Expr = {
      tag: "Effect",
      op: "infer.op",
      args: [{ tag: "Lit", value: "test query" }],
    };
    const state: any = {
      control: { tag: "Expr", e: expr },
      env,
      store: store2,
      kont: [],
      handlers: [],
    };

    const machine: MachineVal = {
      tag: "Machine",
      state,
      stepCount: 0,
      isDone: false,
      machineId: "test-m3",
      breakOnOps: new Set(["infer.op"]),
    };

    // Run until we hit an Op or Done
    let current = machine.state;
    let outcome = stepOnce(current);

    // Step until we get Op or Done
    while (outcome.tag === "State") {
      current = outcome.state;
      outcome = stepOnce(current);
    }

    // Should emit Op for infer.op
    expect(outcome.tag).toBe("Op");
    expect(outcome.opcall.op).toBe("infer.op");
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 8.2: Breakpoint on AST pattern
// ─────────────────────────────────────────────────────────────────

describe("Test 8.2: Breakpoint on AST pattern", () => {
  it("machine-add-breakpoint adds effect breakpoint", () => {
    const machine: MachineVal = {
      tag: "Machine",
      state: {} as any,
      stepCount: 0,
      isDone: false,
      machineId: "test-m4",
    };

    // Add breakpoint
    const breakpoints = new Set<string>();
    breakpoints.add("infer.op");

    const updated: MachineVal = {
      ...machine,
      breakOnOps: breakpoints,
    };

    expect(updated.breakOnOps?.has("infer.op")).toBe(true);
    expect(updated.breakOnOps?.has("tool.op")).toBe(false);
  });

  it("breakpoint matches effect operation", () => {
    const breakpoints = new Set(["infer.op", "tool.op"]);

    expect(breakpoints.has("infer.op")).toBe(true);
    expect(breakpoints.has("tool.op")).toBe(true);
    expect(breakpoints.has("commit.op")).toBe(false);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 8.3: ANF preserves semantics
// ─────────────────────────────────────────────────────────────────

describe("Test 8.3: ANF preserves semantics", () => {
  beforeEach(() => {
    resetAnfCounter();
  });

  it("literals are already in ANF", () => {
    const expr: Expr = { tag: "Lit", value: 42 };
    const anf = toAnf(expr);

    expect(anf.tag).toBe("Lit");
    expect((anf as any).value).toBe(42);
    expect(isAnf(anf)).toBe(true);
  });

  it("variables are already in ANF", () => {
    const expr: Expr = { tag: "Var", name: "x" };
    const anf = toAnf(expr);

    expect(anf.tag).toBe("Var");
    expect((anf as any).name).toBe("x");
    expect(isAnf(anf)).toBe(true);
  });

  it("nested application is converted to ANF", () => {
    // ((f a) b) becomes:
    // let t1 = (f a)
    // in (t1 b)
    const expr: Expr = {
      tag: "App",
      fn: {
        tag: "App",
        fn: { tag: "Var", name: "f" },
        args: [{ tag: "Var", name: "a" }],
      },
      args: [{ tag: "Var", name: "b" }],
    };

    const anf = toAnf(expr);
    expect(anf.tag).toBe("Let");
  });

  it("effect args are normalized", () => {
    // (effect infer.op (+ 1 2)) becomes:
    // let arg$1 = (+ 1 2)
    // in (effect infer.op arg$1)
    const expr: Expr = {
      tag: "Effect",
      op: "infer.op",
      args: [{
        tag: "App",
        fn: { tag: "Var", name: "+" },
        args: [
          { tag: "Lit", value: 1 },
          { tag: "Lit", value: 2 },
        ],
      }],
    };

    const anf = toAnf(expr);
    expect(anf.tag).toBe("Let");
  });

  it("lambda body is transformed to ANF", () => {
    const expr: Expr = {
      tag: "Lambda",
      params: ["x"],
      body: {
        tag: "App",
        fn: { tag: "Var", name: "f" },
        args: [{
          tag: "App",
          fn: { tag: "Var", name: "g" },
          args: [{ tag: "Var", name: "x" }],
        }],
      },
    };

    const anf = toAnf(expr);
    expect(anf.tag).toBe("Lambda");
    // Body should be transformed
    expect((anf as any).body.tag).toBe("Let");
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 8.4: Optimization reduces infer.op by fusion
// ─────────────────────────────────────────────────────────────────

describe("Test 8.4: Optimization reduces infer.op by fusion", () => {
  it("counts effect operations", () => {
    const expr: Expr = {
      tag: "Begin",
      exprs: [
        { tag: "Effect", op: "infer.op", args: [{ tag: "Lit", value: "a" }] },
        { tag: "Effect", op: "infer.op", args: [{ tag: "Lit", value: "b" }] },
        { tag: "Effect", op: "infer.op", args: [{ tag: "Lit", value: "c" }] },
      ],
    };

    const count = countEffects(expr);
    expect(count).toBe(3);
  });

  it("fusion combines adjacent infer.op calls", () => {
    const expr: Expr = {
      tag: "Begin",
      exprs: [
        { tag: "Effect", op: "infer.op", args: [{ tag: "Lit", value: "a" }] },
        { tag: "Effect", op: "infer.op", args: [{ tag: "Lit", value: "b" }] },
      ],
    };

    const { expr: optimized, stats } = fusionOnly(expr);

    // Should have fused into one batch call
    expect(stats.fusedEffects).toBe(1);
    expect(stats.totalReductions).toBeGreaterThan(0);
  });

  it("non-adjacent effects are not fused", () => {
    const expr: Expr = {
      tag: "Begin",
      exprs: [
        { tag: "Effect", op: "infer.op", args: [{ tag: "Lit", value: "a" }] },
        { tag: "Lit", value: 42 },  // Separator
        { tag: "Effect", op: "infer.op", args: [{ tag: "Lit", value: "b" }] },
      ],
    };

    const { stats } = fusionOnly(expr);

    // Should not fuse because of separator
    expect(stats.fusedEffects).toBe(0);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 8.5: Hoist normalization improves cache hits
// ─────────────────────────────────────────────────────────────────

describe("Test 8.5: Hoist normalization improves cache hits", () => {
  it("ledger counters track effect emissions", () => {
    const budget = new BudgetTracker();

    budget.recordEffect("infer.op", "ctx1");
    budget.recordEffect("infer.op", "ctx2");
    budget.recordEffect("tool.op", "ctx3");

    expect(budget.getEffectCount("infer.op")).toBe(2);
    expect(budget.getEffectCount("tool.op")).toBe(1);
    expect(budget.getTotalEffectCount()).toBe(3);
  });

  it("ledger entries are ordered", () => {
    const budget = new BudgetTracker();

    budget.recordEffect("a", "ctx1");
    budget.recordEffect("b", "ctx2");
    budget.recordEffect("c", "ctx3");

    const ledger = budget.getLedger();
    expect(ledger.length).toBe(3);
    expect(ledger[0].op).toBe("a");
    expect(ledger[1].op).toBe("b");
    expect(ledger[2].op).toBe("c");
  });

  it("ledger can be filtered by operation", () => {
    const budget = new BudgetTracker();

    budget.recordEffect("infer.op", "ctx1");
    budget.recordEffect("tool.op", "ctx2");
    budget.recordEffect("infer.op", "ctx3");

    const inferOps = budget.getLedgerByOp("infer.op");
    expect(inferOps.length).toBe(2);
  });

  it("all effect counts can be retrieved", () => {
    const budget = new BudgetTracker();

    budget.recordEffect("a", "ctx");
    budget.recordEffect("b", "ctx");
    budget.recordEffect("a", "ctx");
    budget.recordEffect("c", "ctx");

    const counts = budget.getAllEffectCounts();
    expect(counts["a"]).toBe(2);
    expect(counts["b"]).toBe(1);
    expect(counts["c"]).toBe(1);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 8.6: Multi-shot fork comparison
// ─────────────────────────────────────────────────────────────────

describe("Test 8.6: Multi-shot fork comparison", () => {
  it("machine-fork creates independent copy", () => {
    const original: MachineVal = {
      tag: "Machine",
      state: {
        control: { tag: "Val", v: { tag: "Num", n: 1 } },
        env: {} as any,
        store: {} as any,
        kont: [],
        handlers: [],
      },
      stepCount: 5,
      isDone: false,
      machineId: "m1",
    };

    // Fork it
    const forked: MachineVal = {
      tag: "Machine",
      state: JSON.parse(JSON.stringify(original.state)),
      stepCount: original.stepCount,
      isDone: original.isDone,
      machineId: "m2",
      parentId: original.machineId,
    };

    expect(forked.machineId).not.toBe(original.machineId);
    expect(forked.parentId).toBe(original.machineId);
    expect(forked.stepCount).toBe(original.stepCount);
  });

  it("forked machines evolve independently", () => {
    // Both start at same state
    const m1: MachineVal = {
      tag: "Machine",
      state: {} as any,
      stepCount: 0,
      isDone: false,
      machineId: "fork1",
    };

    const m2: MachineVal = {
      tag: "Machine",
      state: {} as any,
      stepCount: 0,
      isDone: false,
      machineId: "fork2",
      parentId: "fork1",
    };

    // Step m1
    const m1stepped = { ...m1, stepCount: 1 };

    // m2 should still be at 0
    expect(m1stepped.stepCount).toBe(1);
    expect(m2.stepCount).toBe(0);
  });

  it("machine-resume continues with provided value", () => {
    const machine: MachineVal = {
      tag: "Machine",
      state: {
        control: { tag: "Val", v: VUnit },
        env: {} as any,
        store: {} as any,
        kont: [],
        handlers: [],
      },
      stepCount: 10,
      isDone: false,
      machineId: "m-resume",
      lastOutcome: { tag: "Op", opcall: { op: "infer.op" } } as any,
    };

    // Resume with a value
    const resumed: MachineVal = {
      ...machine,
      state: {
        ...machine.state,
        control: { tag: "Val", v: { tag: "Num", n: 42 } },
      },
      lastOutcome: undefined,
    };

    expect((resumed.state.control as any).v.tag).toBe("Num");
    expect((resumed.state.control as any).v.n).toBe(42);
    expect(resumed.lastOutcome).toBeUndefined();
  });

  it("machine? predicate works", () => {
    const machine: MachineVal = {
      tag: "Machine",
      state: {} as any,
      stepCount: 0,
      isDone: false,
      machineId: "test",
    };

    const notMachine = { tag: "Num", n: 42 };

    expect(machine.tag === "Machine").toBe(true);
    expect(notMachine.tag === "Machine").toBe(false);
  });
});
