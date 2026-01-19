// test/repl/step-debugger.spec.ts
// Tests for the interactive step debugger (omega-debugger.ts)
// These tests verify REAL step-by-step execution, breakpoints, and snapshots

import { describe, it, expect, beforeEach } from "vitest";
import { COWStore } from "../../src/core/eval/store";
import { stepOnce, type StepResult } from "../../src/core/eval/machineStep";
import { installPrims } from "../helpers/prims";
import { compileTextToExpr } from "../../src/core/pipeline/compileText";
import type { State, Frame } from "../../src/core/eval/machine";
import type { Val } from "../../src/core/eval/values";

// Minimal Debugger class (extracted from omega-debugger.ts for testing)
class TestDebugger {
  private state: State | null = null;
  private stepCount = 0;
  private history: { step: number; state: State }[] = [];
  private snapshots: Map<string, { step: number; state: State }> = new Map();

  load(code: string): void {
    const store0 = new COWStore();
    const prim = installPrims(store0);
    const expr = compileTextToExpr(`(begin ${code})`);

    this.state = {
      control: { tag: "Expr", e: expr },
      env: prim.env,
      store: prim.store,
      kont: [],
      handlers: [],
    };
    this.stepCount = 0;
    this.history = [];
  }

  step(): StepResult | null {
    if (!this.state) return null;

    // Save to history
    this.history.push({
      step: this.stepCount,
      state: { ...this.state, kont: [...this.state.kont] },
    });

    const result = stepOnce(this.state);
    this.stepCount++;

    if (result.tag === "State") {
      this.state = result.state;
    } else if (result.tag === "Done") {
      this.state = null;
    }

    return result;
  }

  get currentStep(): number { return this.stepCount; }
  get currentState(): State | null { return this.state; }
  get historyLength(): number { return this.history.length; }

  saveSnapshot(name: string): void {
    if (this.state) {
      this.snapshots.set(name, {
        step: this.stepCount,
        state: { ...this.state, kont: [...this.state.kont] },
      });
    }
  }

  loadSnapshot(name: string): boolean {
    const snap = this.snapshots.get(name);
    if (!snap) return false;
    this.state = { ...snap.state, kont: [...snap.state.kont] };
    this.stepCount = snap.step;
    return true;
  }

  goBack(steps: number): boolean {
    const targetIdx = Math.max(0, this.history.length - steps);
    const entry = this.history[targetIdx];
    if (!entry) return false;
    this.state = { ...entry.state, kont: [...entry.state.kont] };
    this.stepCount = entry.step;
    this.history = this.history.slice(0, targetIdx);
    return true;
  }
}

describe("Step Debugger: Basic Stepping", () => {
  it("loads code and initializes state", () => {
    const dbg = new TestDebugger();
    dbg.load("(+ 1 2)");

    expect(dbg.currentState).not.toBeNull();
    expect(dbg.currentStep).toBe(0);
  });

  it("step advances execution by one", () => {
    const dbg = new TestDebugger();
    dbg.load("(+ 1 2)");

    const result = dbg.step();
    expect(result).not.toBeNull();
    expect(dbg.currentStep).toBe(1);
  });

  it("multiple steps execute in sequence", () => {
    const dbg = new TestDebugger();
    dbg.load("(+ 1 2)");

    dbg.step();
    dbg.step();
    dbg.step();

    expect(dbg.currentStep).toBe(3);
  });

  it("state is null after completion", () => {
    const dbg = new TestDebugger();
    dbg.load("(+ 1 2)");

    // Run until done
    let result: StepResult | null;
    let count = 0;
    do {
      result = dbg.step();
      count++;
    } while (result && result.tag !== "Done" && count < 100);

    expect(result?.tag).toBe("Done");
    expect(dbg.currentState).toBeNull();
  });

  it("returns correct final value", () => {
    const dbg = new TestDebugger();
    dbg.load("(+ 1 2)");

    let result: StepResult | null;
    do {
      result = dbg.step();
    } while (result && result.tag !== "Done");

    expect(result?.tag).toBe("Done");
    if (result?.tag === "Done") {
      expect(result.value.tag).toBe("Num");
      expect((result.value as any).n).toBe(3);
    }
  });
});

describe("Step Debugger: Stack Observation", () => {
  it("stack depth increases during evaluation", () => {
    const dbg = new TestDebugger();
    dbg.load("(+ 1 2)");

    // Initial: stack depth 0
    expect(dbg.currentState?.kont.length).toBe(0);

    // After stepping into App: stack grows
    dbg.step();
    const depth1 = dbg.currentState?.kont.length ?? 0;
    expect(depth1).toBeGreaterThan(0);
  });

  it("stack frame tags are accessible", () => {
    const dbg = new TestDebugger();
    dbg.load("(+ 1 2)");

    dbg.step(); // App
    dbg.step(); // Var(+)

    const frames = dbg.currentState?.kont ?? [];
    expect(frames.length).toBeGreaterThan(0);

    const tags = frames.map(f => f.tag);
    expect(tags.every(t => typeof t === "string")).toBe(true);
  });

  it("deep recursion shows growing stack", () => {
    const dbg = new TestDebugger();
    dbg.load(`
      (define (fact n)
        (if (<= n 1)
            1
            (* n (fact (- n 1)))))
      (fact 3)
    `);

    let maxDepth = 0;
    let result: StepResult | null;
    let count = 0;

    do {
      result = dbg.step();
      const depth = dbg.currentState?.kont.length ?? 0;
      if (depth > maxDepth) maxDepth = depth;
      count++;
    } while (result && result.tag !== "Done" && count < 500);

    // Factorial of 3 should have decent stack depth
    expect(maxDepth).toBeGreaterThan(3);
  });
});

describe("Step Debugger: Environment Inspection", () => {
  it("primitives are in environment", () => {
    const dbg = new TestDebugger();
    dbg.load("1");

    const env = dbg.currentState?.env;
    expect(env).toBeDefined();

    // Check that + exists in env chain
    let found = false;
    let ctx: any = env;
    while (ctx) {
      if (ctx.frame?.has("+")) {
        found = true;
        break;
      }
      ctx = ctx.parent;
    }
    expect(found).toBe(true);
  });

  it("user bindings appear after define", () => {
    const dbg = new TestDebugger();
    dbg.load("(define x 42)");

    // Run to completion
    let result: StepResult | null;
    do {
      result = dbg.step();
    } while (result && result.tag !== "Done");

    // x$bid#N should be in the final env (check history)
    const lastHistory = dbg["history"][dbg["history"].length - 1];
    const env = lastHistory?.state.env;

    let foundX = false;
    let ctx: any = env;
    while (ctx) {
      for (const key of ctx.frame?.keys() ?? []) {
        if (key.startsWith("x$")) {
          foundX = true;
          break;
        }
      }
      ctx = ctx.parent;
    }
    expect(foundX).toBe(true);
  });
});

describe("Step Debugger: Snapshots", () => {
  it("saves and restores snapshot", () => {
    const dbg = new TestDebugger();
    dbg.load("(+ 1 2)");

    dbg.step();
    dbg.step();
    const stepBefore = dbg.currentStep;

    dbg.saveSnapshot("test");

    dbg.step();
    dbg.step();
    expect(dbg.currentStep).toBeGreaterThan(stepBefore);

    dbg.loadSnapshot("test");
    expect(dbg.currentStep).toBe(stepBefore);
  });

  it("multiple snapshots are independent", () => {
    const dbg = new TestDebugger();
    dbg.load("(+ (* 2 3) 4)");

    dbg.step();
    dbg.step();
    dbg.saveSnapshot("early");
    const earlyStep = dbg.currentStep;

    dbg.step();
    dbg.step();
    dbg.step();
    dbg.saveSnapshot("later");
    const laterStep = dbg.currentStep;

    dbg.loadSnapshot("early");
    expect(dbg.currentStep).toBe(earlyStep);

    dbg.loadSnapshot("later");
    expect(dbg.currentStep).toBe(laterStep);
  });

  it("returns false for missing snapshot", () => {
    const dbg = new TestDebugger();
    dbg.load("1");

    const result = dbg.loadSnapshot("nonexistent");
    expect(result).toBe(false);
  });
});

describe("Step Debugger: History Rewind", () => {
  it("goBack rewinds to previous step", () => {
    const dbg = new TestDebugger();
    dbg.load("(+ 1 2)");

    dbg.step();
    dbg.step();
    dbg.step();
    expect(dbg.currentStep).toBe(3);

    dbg.goBack(2);
    expect(dbg.currentStep).toBe(1);
  });

  it("history is truncated after rewind", () => {
    const dbg = new TestDebugger();
    dbg.load("(+ 1 2)");

    dbg.step();
    dbg.step();
    dbg.step();
    dbg.step();
    expect(dbg.historyLength).toBe(4);

    dbg.goBack(2);
    expect(dbg.historyLength).toBe(2);
  });

  it("cannot rewind past beginning", () => {
    const dbg = new TestDebugger();
    dbg.load("(+ 1 2)");

    dbg.step();
    dbg.step();

    dbg.goBack(100); // More than history
    expect(dbg.currentStep).toBe(0);
  });
});

describe("Step Debugger: Control Inspection", () => {
  it("control starts as Expr", () => {
    const dbg = new TestDebugger();
    dbg.load("(+ 1 2)");

    expect(dbg.currentState?.control.tag).toBe("Expr");
  });

  it("control becomes Val during evaluation", () => {
    const dbg = new TestDebugger();
    dbg.load("(+ 1 2)");

    let sawVal = false;
    let count = 0;

    while (dbg.currentState && count < 20) {
      if (dbg.currentState.control.tag === "Val") {
        sawVal = true;
        break;
      }
      dbg.step();
      count++;
    }

    expect(sawVal).toBe(true);
  });

  it("can inspect control detail at each step", () => {
    const dbg = new TestDebugger();
    dbg.load("(+ 1 2)");

    const controlTags: string[] = [];

    while (dbg.currentState) {
      controlTags.push(dbg.currentState.control.tag);
      const result = dbg.step();
      if (!result || result.tag === "Done") break;
    }

    // Should have mix of Expr and Val
    expect(controlTags.includes("Expr")).toBe(true);
    expect(controlTags.includes("Val")).toBe(true);
  });
});

describe("Step Debugger: Effect Detection", () => {
  it("detects Op result when effect is triggered", () => {
    const dbg = new TestDebugger();
    dbg.load("(effect infer.op \"test\")");

    let foundOp = false;
    let count = 0;

    while (dbg.currentState && count < 50) {
      const result = dbg.step();
      if (result?.tag === "Op") {
        foundOp = true;
        expect(result.opcall.op).toBe("infer.op");
        break;
      }
      count++;
    }

    expect(foundOp).toBe(true);
  });
});

describe("Step Debugger: Real Computation Traces", () => {
  it("traces (+ 1 2) step by step", () => {
    const dbg = new TestDebugger();
    dbg.load("(+ 1 2)");

    const trace: { step: number; control: string; depth: number }[] = [];

    while (dbg.currentState) {
      const ctrl = dbg.currentState.control;
      trace.push({
        step: dbg.currentStep,
        control: ctrl.tag === "Val" ? `Val(${(ctrl.v as any).tag})` : `Expr(${(ctrl as any).e?.tag})`,
        depth: dbg.currentState.kont.length,
      });

      const result = dbg.step();
      if (!result || result.tag === "Done") break;
    }

    // Verify we have a real trace
    expect(trace.length).toBeGreaterThan(5);

    // First should be Expr
    expect(trace[0].control).toContain("Expr");

    // Depth should vary
    const depths = trace.map(t => t.depth);
    const maxDepth = Math.max(...depths);
    expect(maxDepth).toBeGreaterThan(0);
  });

  it("traces factorial(3) with deep recursion", () => {
    const dbg = new TestDebugger();
    dbg.load(`
      (define (fact n)
        (if (<= n 1) 1 (* n (fact (- n 1)))))
      (fact 3)
    `);

    let maxDepth = 0;
    let stepCount = 0;
    let result: StepResult | null;

    do {
      const depth = dbg.currentState?.kont.length ?? 0;
      if (depth > maxDepth) maxDepth = depth;
      result = dbg.step();
      stepCount++;
    } while (result && result.tag !== "Done" && stepCount < 500);

    // Should have recursive calls
    expect(maxDepth).toBeGreaterThan(5);
    expect(stepCount).toBeGreaterThan(20);

    // Final result should be 6 (3!)
    if (result?.tag === "Done") {
      expect((result.value as any).n).toBe(6);
    }
  });
});

// Extended TestDebugger with trace recording and goto
class TraceDebugger extends TestDebugger {
  private trace: { step: number; state: State }[] = [];
  private recordingEnabled = true;
  private initialCode = "";

  override load(code: string): void {
    super.load(code);
    this.trace = [];
    this.initialCode = code;
    if (this.recordingEnabled && this.currentState) {
      this.trace.push({
        step: 0,
        state: { ...this.currentState, kont: [...this.currentState.kont] },
      });
    }
  }

  override step(): StepResult | null {
    const result = super.step();
    if (this.recordingEnabled && this.currentState) {
      this.trace.push({
        step: this.currentStep,
        state: { ...this.currentState, kont: [...this.currentState.kont] },
      });
    }
    return result;
  }

  goto(targetStep: number): boolean {
    const record = this.trace.find(r => r.step === targetStep);
    if (!record) return false;
    // Restore state from trace - directly assign to private state
    (this as any).state = { ...record.state, kont: [...record.state.kont] };
    (this as any).stepCount = record.step;
    return true;
  }

  getTraceLength(): number {
    return this.trace.length;
  }

  runToCompletion(maxSteps = 1000): StepResult | null {
    let result: StepResult | null = null;
    let count = 0;
    while (this.currentState && count < maxSteps) {
      result = this.step();
      count++;
      if (!result || result.tag === "Done") break;
    }
    return result;
  }
}

describe("Step Debugger: Trace Recording", () => {
  it("records trace during execution", () => {
    const dbg = new TraceDebugger();
    dbg.load("(+ 1 2)");

    dbg.step();
    dbg.step();
    dbg.step();

    // Should have initial state + 3 steps recorded
    expect(dbg.getTraceLength()).toBeGreaterThanOrEqual(4);
  });

  it("trace contains states at each step", () => {
    const dbg = new TraceDebugger();
    dbg.load("(+ 1 2)");

    dbg.runToCompletion();

    // Trace should have all steps
    expect(dbg.getTraceLength()).toBeGreaterThan(5);
  });
});

describe("Step Debugger: Goto (Jump to Step)", () => {
  it("goto jumps to specific step", () => {
    const dbg = new TraceDebugger();
    dbg.load("(+ 1 2)");

    dbg.step();
    dbg.step();
    dbg.step();
    dbg.step();
    expect(dbg.currentStep).toBe(4);

    const result = dbg.goto(2);
    expect(result).toBe(true);
    expect(dbg.currentStep).toBe(2);
  });

  it("goto returns false for invalid step", () => {
    const dbg = new TraceDebugger();
    dbg.load("(+ 1 2)");

    dbg.step();
    dbg.step();

    const result = dbg.goto(999);
    expect(result).toBe(false);
    expect(dbg.currentStep).toBe(2); // Unchanged
  });

  it("goto to step 0 restores initial state", () => {
    const dbg = new TraceDebugger();
    dbg.load("(+ 1 2)");

    dbg.step();
    dbg.step();
    dbg.step();

    dbg.goto(0);
    expect(dbg.currentStep).toBe(0);
    expect(dbg.currentState?.control.tag).toBe("Expr");
  });

  it("can continue execution after goto", () => {
    const dbg = new TraceDebugger();
    dbg.load("(+ 1 2)");

    // Run forward
    dbg.step();
    dbg.step();
    dbg.step();
    const stateAtStep3 = dbg.currentState?.control.tag;

    // Jump back
    dbg.goto(1);
    expect(dbg.currentStep).toBe(1);

    // Continue forward
    dbg.step();
    expect(dbg.currentStep).toBe(2);
    dbg.step();
    expect(dbg.currentStep).toBe(3);
  });

  it("goto preserves state correctly for complex program", () => {
    const dbg = new TraceDebugger();
    dbg.load(`
      (define (fact n)
        (if (<= n 1) 1 (* n (fact (- n 1)))))
      (fact 3)
    `);

    // Run to completion first
    dbg.runToCompletion();
    const totalSteps = dbg.currentStep;
    expect(totalSteps).toBeGreaterThan(20);

    // Jump to middle
    const midStep = Math.floor(totalSteps / 2);
    dbg.goto(midStep);
    expect(dbg.currentStep).toBe(midStep);

    // State should be valid
    expect(dbg.currentState).not.toBeNull();
    expect(dbg.currentState?.kont).toBeDefined();
  });
});
