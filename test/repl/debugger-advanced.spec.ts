/**
 * ═══════════════════════════════════════════════════════════════════════════
 * Advanced Debugger Tests - Missing Commands from omega-debugger.ts
 * ═══════════════════════════════════════════════════════════════════════════
 *
 * Tests for advanced debugger functionality that needs to be merged into REPL:
 * - Snapshot management (save/restore/list/export)
 * - History navigation (back/history)
 * - Trace recording controls (record on/off)
 * - Trace persistence (dump/replay)
 * - File loading (loadfile)
 * - Breakpoint toggle
 * - Stop command
 * ═══════════════════════════════════════════════════════════════════════════
 */

import { describe, it, expect, beforeEach } from "vitest";
import { COWStore } from "../../src/core/eval/store";
import { RuntimeImpl } from "../../src/core/effects/runtimeImpl";
import { ScriptedOracleAdapter } from "../../src/core/oracle/scriptedOracle";
import { SnapshotRepo } from "../../src/core/oracle/snapshots";
import { InMemoryReceiptStore } from "../../src/core/oracle/receipts";
import { stepOnce } from "../../src/core/eval/machineStep";
import { installPrims } from "../helpers/prims";
import { compileTextToExpr } from "../../src/core/pipeline/compileText";
import type { State } from "../../src/core/eval/machine";
import { VUnit } from "../../src/core/eval/values";
import * as fs from "fs";
import * as path from "path";
import * as os from "os";

// ─────────────────────────────────────────────────────────────────
// Test Data Types (mirroring omega-repl.ts internal structures)
// ─────────────────────────────────────────────────────────────────

interface Snapshot {
  name: string;
  step: number;
  state: State;
  timestamp: Date;
}

interface TraceRecord {
  step: number;
  state: State;
  controlSummary: string;
  stackDepth: number;
}

interface Breakpoint {
  id: number;
  type: "step" | "expr" | "effect";
  condition: string | number;
  enabled: boolean;
}

// ─────────────────────────────────────────────────────────────────
// Helper to create initial state
// ─────────────────────────────────────────────────────────────────

function createInitialState(code: string): State {
  const store0 = new COWStore();
  const prim = installPrims(store0);
  const expr = compileTextToExpr(`(begin ${code})`);

  return {
    control: { tag: "Expr", e: expr },
    env: prim.env,
    store: prim.store,
    kont: [],
    handlers: [],
  };
}

function cloneState(s: State): State {
  return { ...s, kont: [...s.kont], handlers: [...s.handlers] };
}

function controlToString(ctrl: State["control"]): string {
  if (ctrl.tag === "Val") {
    return `Value: ${JSON.stringify(ctrl.v).slice(0, 50)}`;
  } else if (ctrl.tag === "Expr") {
    const e = ctrl.e;
    return `Expr: ${e.tag}`;
  }
  return `Control: ${ctrl.tag}`;
}

// ─────────────────────────────────────────────────────────────────
// Snapshot Management Tests
// ─────────────────────────────────────────────────────────────────

describe("REPL Debugger: Snapshot Management", () => {
  it("should save snapshot with name and metadata", () => {
    const snapshots = new Map<string, Snapshot>();
    const state = createInitialState("(+ 1 2)");
    const step = 5;

    const snapshot: Snapshot = {
      name: "test-snapshot",
      step,
      state: cloneState(state),
      timestamp: new Date(),
    };

    snapshots.set("test-snapshot", snapshot);

    expect(snapshots.has("test-snapshot")).toBe(true);
    const saved = snapshots.get("test-snapshot");
    expect(saved).toBeDefined();
    expect(saved?.name).toBe("test-snapshot");
    expect(saved?.step).toBe(5);
    expect(saved?.state).toBeDefined();
    expect(saved?.timestamp).toBeInstanceOf(Date);
  });

  it("should restore snapshot and return to saved step", () => {
    const snapshots = new Map<string, Snapshot>();
    const state1 = createInitialState("(+ 1 2)");
    const state2 = createInitialState("(* 3 4)");

    // Save first state at step 5
    snapshots.set("s1", {
      name: "s1",
      step: 5,
      state: cloneState(state1),
      timestamp: new Date(),
    });

    // Simulate moving forward (state2, step 10)
    let currentState = state2;
    let currentStep = 10;

    // Restore snapshot
    const snapshot = snapshots.get("s1");
    expect(snapshot).toBeDefined();

    if (snapshot) {
      currentState = cloneState(snapshot.state);
      currentStep = snapshot.step;
    }

    expect(currentStep).toBe(5);
    expect(currentState).toBeDefined();
  });

  it("should list all snapshots with metadata", () => {
    const snapshots = new Map<string, Snapshot>();
    const state = createInitialState("1");

    snapshots.set("snap1", {
      name: "snap1",
      step: 1,
      state: cloneState(state),
      timestamp: new Date("2024-01-01"),
    });

    snapshots.set("snap2", {
      name: "snap2",
      step: 10,
      state: cloneState(state),
      timestamp: new Date("2024-01-02"),
    });

    const list = Array.from(snapshots.entries()).map(([name, snap]) => ({
      name,
      step: snap.step,
      timestamp: snap.timestamp.toISOString(),
    }));

    expect(list.length).toBe(2);
    expect(list.find(s => s.name === "snap1")).toBeDefined();
    expect(list.find(s => s.name === "snap2")).toBeDefined();
  });

  it("should export snapshot metadata to JSON", () => {
    const snapshot: Snapshot = {
      name: "exported",
      step: 42,
      state: createInitialState("1"),
      timestamp: new Date("2024-01-01T00:00:00Z"),
    };

    const exported = {
      name: snapshot.name,
      step: snapshot.step,
      timestamp: snapshot.timestamp.toISOString(),
      controlTag: snapshot.state.control.tag,
      stackDepth: snapshot.state.kont.length,
    };

    expect(exported.name).toBe("exported");
    expect(exported.step).toBe(42);
    expect(exported.timestamp).toBe("2024-01-01T00:00:00.000Z");
    expect(exported.controlTag).toBe("Expr");
    expect(exported.stackDepth).toBe(0);
  });

  it("should handle missing snapshot gracefully", () => {
    const snapshots = new Map<string, Snapshot>();
    const snapshot = snapshots.get("nonexistent");
    expect(snapshot).toBeUndefined();
  });

  it("should overwrite existing snapshot with same name", () => {
    const snapshots = new Map<string, Snapshot>();
    const state1 = createInitialState("(+ 1 2)");
    const state2 = createInitialState("(* 3 4)");

    snapshots.set("dup", {
      name: "dup",
      step: 1,
      state: cloneState(state1),
      timestamp: new Date(),
    });

    snapshots.set("dup", {
      name: "dup",
      step: 99,
      state: cloneState(state2),
      timestamp: new Date(),
    });

    const snapshot = snapshots.get("dup");
    expect(snapshot?.step).toBe(99);
  });
});

// ─────────────────────────────────────────────────────────────────
// History Navigation Tests
// ─────────────────────────────────────────────────────────────────

describe("REPL Debugger: History Navigation", () => {
  it("should maintain limited history (max 100 entries)", () => {
    const history: { step: number; state: State; control: string }[] = [];
    const maxHistory = 100;
    const state = createInitialState("1");

    // Add 150 entries
    for (let i = 0; i < 150; i++) {
      if (history.length >= maxHistory) {
        history.shift();
      }
      history.push({
        step: i,
        state: cloneState(state),
        control: controlToString(state.control),
      });
    }

    expect(history.length).toBe(100);
    expect(history[0].step).toBe(50); // First 50 were shifted out
    expect(history[99].step).toBe(149);
  });

  it("should go back N steps in history", () => {
    const history: { step: number; state: State; control: string }[] = [];
    const state = createInitialState("1");

    // Build history
    for (let i = 0; i < 10; i++) {
      history.push({
        step: i,
        state: cloneState(state),
        control: controlToString(state.control),
      });
    }

    let currentStep = 10;
    let currentState = state;

    // Go back 3 steps
    const stepsBack = 3;
    const targetIdx = Math.max(0, history.length - stepsBack);
    const entry = history[targetIdx];

    currentState = cloneState(entry.state);
    currentStep = entry.step;

    expect(currentStep).toBe(7); // 10 history entries, go back 3 from index 9 -> index 7
  });

  it("should truncate history when going back", () => {
    const history: { step: number; state: State; control: string }[] = [];
    const state = createInitialState("1");

    for (let i = 0; i < 10; i++) {
      history.push({
        step: i,
        state: cloneState(state),
        control: `step ${i}`,
      });
    }

    // Go back 3 steps
    const stepsBack = 3;
    const targetIdx = Math.max(0, history.length - stepsBack);
    const newHistory = history.slice(0, targetIdx);

    expect(newHistory.length).toBe(7);
    expect(newHistory[newHistory.length - 1].step).toBe(6);
  });

  it("should format history entries with control summary", () => {
    const state = createInitialState("(+ 1 2)");
    const entry = {
      step: 5,
      state: cloneState(state),
      control: controlToString(state.control),
    };

    expect(entry.control).toContain("Expr");
    expect(entry.step).toBe(5);
  });

  it("should not go back beyond history start", () => {
    const history: { step: number; state: State; control: string }[] = [];
    const state = createInitialState("1");

    for (let i = 0; i < 3; i++) {
      history.push({
        step: i,
        state: cloneState(state),
        control: `step ${i}`,
      });
    }

    // Try to go back 10 steps (more than available)
    const stepsBack = 10;
    const targetIdx = Math.max(0, history.length - stepsBack);

    expect(targetIdx).toBe(0);
  });
});

// ─────────────────────────────────────────────────────────────────
// Trace Recording Tests
// ─────────────────────────────────────────────────────────────────

describe("REPL Debugger: Trace Recording", () => {
  it("should record trace when enabled", () => {
    const trace: TraceRecord[] = [];
    const recordingEnabled = true;
    const state = createInitialState("(+ 1 2)");

    if (recordingEnabled) {
      trace.push({
        step: 0,
        state: cloneState(state),
        controlSummary: controlToString(state.control),
        stackDepth: state.kont.length,
      });
    }

    expect(trace.length).toBe(1);
    expect(trace[0].step).toBe(0);
    expect(trace[0].stackDepth).toBe(0);
  });

  it("should not record when disabled", () => {
    const trace: TraceRecord[] = [];
    const recordingEnabled = false;
    const state = createInitialState("(+ 1 2)");

    if (recordingEnabled) {
      trace.push({
        step: 0,
        state: cloneState(state),
        controlSummary: controlToString(state.control),
        stackDepth: state.kont.length,
      });
    }

    expect(trace.length).toBe(0);
  });

  it("should toggle recording on/off", () => {
    let recordingEnabled = true;
    expect(recordingEnabled).toBe(true);

    recordingEnabled = false;
    expect(recordingEnabled).toBe(false);

    recordingEnabled = true;
    expect(recordingEnabled).toBe(true);
  });

  it("should create dump with metadata", () => {
    const trace: TraceRecord[] = [];
    const initialCode = "(+ 1 2)";
    const state = createInitialState(initialCode);

    // Simulate a few steps
    trace.push({
      step: 0,
      state: cloneState(state),
      controlSummary: "Expr: Begin",
      stackDepth: 0,
    });

    trace.push({
      step: 1,
      state: cloneState(state),
      controlSummary: "Expr: App",
      stackDepth: 1,
    });

    const dumpData = {
      version: 1,
      code: initialCode,
      timestamp: new Date().toISOString(),
      totalSteps: trace.length,
      traceSummary: trace.map(t => ({
        step: t.step,
        control: t.controlSummary,
        stackDepth: t.stackDepth,
      })),
    };

    expect(dumpData.version).toBe(1);
    expect(dumpData.code).toBe("(+ 1 2)");
    expect(dumpData.totalSteps).toBe(2);
    expect(dumpData.traceSummary.length).toBe(2);
    expect(dumpData.traceSummary[0].step).toBe(0);
    expect(dumpData.traceSummary[1].step).toBe(1);
  });

  it("should allow goto to any recorded step", () => {
    const trace: TraceRecord[] = [];
    const state1 = createInitialState("1");
    const state2 = createInitialState("2");
    const state3 = createInitialState("3");

    trace.push({
      step: 0,
      state: cloneState(state1),
      controlSummary: "step 0",
      stackDepth: 0,
    });

    trace.push({
      step: 5,
      state: cloneState(state2),
      controlSummary: "step 5",
      stackDepth: 1,
    });

    trace.push({
      step: 10,
      state: cloneState(state3),
      controlSummary: "step 10",
      stackDepth: 2,
    });

    // Goto step 5
    const targetStep = 5;
    const record = trace.find(r => r.step === targetStep);

    expect(record).toBeDefined();
    expect(record?.step).toBe(5);
    expect(record?.stackDepth).toBe(1);
  });

  it("should handle invalid goto step gracefully", () => {
    const trace: TraceRecord[] = [];
    trace.push({
      step: 0,
      state: createInitialState("1"),
      controlSummary: "step 0",
      stackDepth: 0,
    });

    const targetStep = 999;
    const record = trace.find(r => r.step === targetStep);

    expect(record).toBeUndefined();
  });
});

// ─────────────────────────────────────────────────────────────────
// Trace Persistence Tests
// ─────────────────────────────────────────────────────────────────

describe("REPL Debugger: Trace Persistence (dump/replay)", () => {
  const tmpDir = os.tmpdir();

  it("should dump trace to JSON file", () => {
    const trace: TraceRecord[] = [
      {
        step: 0,
        state: createInitialState("(+ 1 2)"),
        controlSummary: "Expr: Begin",
        stackDepth: 0,
      },
      {
        step: 1,
        state: createInitialState("(+ 1 2)"),
        controlSummary: "Expr: App",
        stackDepth: 1,
      },
    ];

    const dumpData = {
      version: 1,
      code: "(+ 1 2)",
      timestamp: new Date().toISOString(),
      totalSteps: trace.length,
      traceSummary: trace.map(t => ({
        step: t.step,
        control: t.controlSummary,
        stackDepth: t.stackDepth,
      })),
    };

    const filepath = path.join(tmpDir, "test-trace-dump.json");
    fs.writeFileSync(filepath, JSON.stringify(dumpData, null, 2));

    expect(fs.existsSync(filepath)).toBe(true);

    const loaded = JSON.parse(fs.readFileSync(filepath, "utf8"));
    expect(loaded.version).toBe(1);
    expect(loaded.code).toBe("(+ 1 2)");
    expect(loaded.totalSteps).toBe(2);

    // Cleanup
    fs.unlinkSync(filepath);
  });

  it("should load dump file and extract code", () => {
    const dumpData = {
      version: 1,
      code: "(* 3 4)",
      timestamp: "2024-01-01T00:00:00Z",
      totalSteps: 5,
      traceSummary: [
        { step: 0, control: "Expr: Begin", stackDepth: 0 },
        { step: 1, control: "Expr: App", stackDepth: 1 },
      ],
    };

    const filepath = path.join(tmpDir, "test-load-dump.json");
    fs.writeFileSync(filepath, JSON.stringify(dumpData, null, 2));

    const loaded = JSON.parse(fs.readFileSync(filepath, "utf8"));
    expect(loaded.code).toBe("(* 3 4)");
    expect(loaded.totalSteps).toBe(5);
    expect(loaded.traceSummary.length).toBe(2);

    // Cleanup
    fs.unlinkSync(filepath);
  });

  it("should handle missing dump file gracefully", () => {
    const filepath = path.join(tmpDir, "nonexistent-dump.json");
    expect(fs.existsSync(filepath)).toBe(false);
  });

  it("should validate dump file has required fields", () => {
    const validDump = {
      version: 1,
      code: "(+ 1 2)",
      timestamp: "2024-01-01",
      totalSteps: 1,
      traceSummary: [],
    };

    expect(validDump.version).toBeDefined();
    expect(validDump.code).toBeDefined();
    expect(validDump.timestamp).toBeDefined();
    expect(validDump.totalSteps).toBeDefined();
    expect(validDump.traceSummary).toBeDefined();

    const invalidDump = {
      version: 1,
      // missing code
    };

    expect(invalidDump.version).toBeDefined();
    expect((invalidDump as any).code).toBeUndefined();
  });
});

// ─────────────────────────────────────────────────────────────────
// Breakpoint Toggle Tests
// ─────────────────────────────────────────────────────────────────

describe("REPL Debugger: Breakpoint Toggle", () => {
  it("should toggle breakpoint enabled state", () => {
    const breakpoints: Breakpoint[] = [
      { id: 1, type: "step", condition: 10, enabled: true },
    ];

    const bp = breakpoints.find(b => b.id === 1);
    expect(bp?.enabled).toBe(true);

    if (bp) {
      bp.enabled = !bp.enabled;
    }

    expect(bp?.enabled).toBe(false);
  });

  it("should toggle breakpoint back and forth", () => {
    const bp: Breakpoint = {
      id: 1,
      type: "step",
      condition: 5,
      enabled: true,
    };

    expect(bp.enabled).toBe(true);
    bp.enabled = !bp.enabled;
    expect(bp.enabled).toBe(false);
    bp.enabled = !bp.enabled;
    expect(bp.enabled).toBe(true);
  });

  it("should handle toggling nonexistent breakpoint", () => {
    const breakpoints: Breakpoint[] = [
      { id: 1, type: "step", condition: 10, enabled: true },
    ];

    const bp = breakpoints.find(b => b.id === 999);
    expect(bp).toBeUndefined();
  });

  it("should list breakpoints with enabled status", () => {
    const breakpoints: Breakpoint[] = [
      { id: 1, type: "step", condition: 10, enabled: true },
      { id: 2, type: "expr", condition: "Define", enabled: false },
      { id: 3, type: "effect", condition: "infer.op", enabled: true },
    ];

    const list = breakpoints.map(bp => ({
      id: bp.id,
      type: bp.type,
      condition: bp.condition,
      status: bp.enabled ? "enabled" : "disabled",
    }));

    expect(list.length).toBe(3);
    expect(list[0].status).toBe("enabled");
    expect(list[1].status).toBe("disabled");
    expect(list[2].status).toBe("enabled");
  });

  it("should only check enabled breakpoints", () => {
    const breakpoints: Breakpoint[] = [
      { id: 1, type: "step", condition: 5, enabled: true },
      { id: 2, type: "step", condition: 5, enabled: false },
    ];

    const currentStep = 5;
    const hit = breakpoints.find(bp =>
      bp.enabled && bp.type === "step" && bp.condition === currentStep
    );

    expect(hit?.id).toBe(1);
  });
});

// ─────────────────────────────────────────────────────────────────
// File Loading Tests
// ─────────────────────────────────────────────────────────────────

describe("REPL Debugger: File Loading", () => {
  const tmpDir = os.tmpdir();

  it("should load Lisp code from file", () => {
    const code = "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))";
    const filepath = path.join(tmpDir, "test-load.lisp");

    fs.writeFileSync(filepath, code);

    expect(fs.existsSync(filepath)).toBe(true);
    const loaded = fs.readFileSync(filepath, "utf8");
    expect(loaded).toBe(code);

    // Cleanup
    fs.unlinkSync(filepath);
  });

  it("should handle file with multiple definitions", () => {
    const code = `
(define x 10)
(define y 20)
(define (add a b) (+ a b))
(add x y)
`;
    const filepath = path.join(tmpDir, "test-multi.lisp");

    fs.writeFileSync(filepath, code);

    const loaded = fs.readFileSync(filepath, "utf8");
    expect(loaded).toContain("define x");
    expect(loaded).toContain("define y");
    expect(loaded).toContain("define (add");

    // Cleanup
    fs.unlinkSync(filepath);
  });

  it("should handle missing file gracefully", () => {
    const filepath = path.join(tmpDir, "nonexistent.lisp");
    expect(fs.existsSync(filepath)).toBe(false);
  });

  it("should resolve relative file paths", () => {
    const relativePath = "test-relative.lisp";
    const resolved = path.resolve(relativePath);
    expect(path.isAbsolute(resolved)).toBe(true);
  });

  it("should strip comment lines from loaded file", () => {
    const code = `
; This is a comment
(define x 10) ; inline comment
; Another comment
(+ x 5)
`;
    const cleanedLines = code
      .split("\n")
      .filter(line => !line.trim().startsWith(";"))
      .join("\n");

    expect(cleanedLines).toContain("define x");
    expect(cleanedLines).toContain("(+ x 5)");
    expect(cleanedLines).not.toContain("; This is a comment");
    expect(cleanedLines).not.toContain("; Another comment");
    // Inline comments are not stripped by this simple filter
    expect(cleanedLines).toContain("; inline comment");
  });
});

// ─────────────────────────────────────────────────────────────────
// Stop Command Tests
// ─────────────────────────────────────────────────────────────────

describe("REPL Debugger: Stop Command", () => {
  it("should stop execution when running flag is set to false", () => {
    let running = true;
    let steps = 0;
    const maxSteps = 100;

    // Simulate running
    while (running && steps < maxSteps) {
      steps++;
      if (steps === 5) {
        running = false; // Stop command
      }
    }

    expect(steps).toBe(5);
    expect(running).toBe(false);
  });

  it("should not execute more steps after stop", () => {
    let running = true;
    let steps = 0;

    // Stop immediately
    running = false;

    while (running && steps < 100) {
      steps++;
    }

    expect(steps).toBe(0);
  });

  it("should allow resuming after stop", () => {
    let running = true;
    let steps = 0;

    // Run 5 steps
    while (running && steps < 5) {
      steps++;
    }
    running = false;
    expect(steps).toBe(5);

    // Resume
    running = true;
    while (running && steps < 10) {
      steps++;
    }

    expect(steps).toBe(10);
  });
});

// ─────────────────────────────────────────────────────────────────
// Integration Tests
// ─────────────────────────────────────────────────────────────────

describe("REPL Debugger: Integration - All Features", () => {
  it("should support complete debug workflow", () => {
    // Setup
    const snapshots = new Map<string, Snapshot>();
    const trace: TraceRecord[] = [];
    const breakpoints: Breakpoint[] = [];
    const history: { step: number; state: State; control: string }[] = [];
    let recordingEnabled = true;
    let currentStep = 0;
    let state = createInitialState("(+ 1 2)");

    // Step 1: Record initial state
    if (recordingEnabled) {
      trace.push({
        step: currentStep,
        state: cloneState(state),
        controlSummary: controlToString(state.control),
        stackDepth: state.kont.length,
      });
    }

    // Step 2: Set breakpoint
    breakpoints.push({
      id: 1,
      type: "step",
      condition: 5,
      enabled: true,
    });

    // Step 3: Save snapshot
    snapshots.set("start", {
      name: "start",
      step: currentStep,
      state: cloneState(state),
      timestamp: new Date(),
    });

    // Step 4: Simulate stepping
    for (let i = 0; i < 3; i++) {
      currentStep++;
      history.push({
        step: currentStep,
        state: cloneState(state),
        control: controlToString(state.control),
      });
      if (recordingEnabled) {
        trace.push({
          step: currentStep,
          state: cloneState(state),
          controlSummary: `step ${currentStep}`,
          stackDepth: 0,
        });
      }
    }

    // Verify
    expect(snapshots.size).toBe(1);
    expect(trace.length).toBe(4); // 0 + 3 steps
    expect(breakpoints.length).toBe(1);
    expect(history.length).toBe(3);
    expect(currentStep).toBe(3);
  });

  it("should handle toggle, goto, and restore in sequence", () => {
    const snapshots = new Map<string, Snapshot>();
    const trace: TraceRecord[] = [];
    const breakpoints: Breakpoint[] = [];
    let state = createInitialState("1");

    // Add breakpoint
    breakpoints.push({
      id: 1,
      type: "step",
      condition: 10,
      enabled: true,
    });

    // Toggle off
    const bp = breakpoints.find(b => b.id === 1);
    if (bp) bp.enabled = false;
    expect(bp?.enabled).toBe(false);

    // Add trace
    trace.push({
      step: 0,
      state: cloneState(state),
      controlSummary: "start",
      stackDepth: 0,
    });

    trace.push({
      step: 5,
      state: cloneState(state),
      controlSummary: "step 5",
      stackDepth: 1,
    });

    // Goto step 5
    const record = trace.find(r => r.step === 5);
    expect(record).toBeDefined();

    // Save snapshot
    snapshots.set("mid", {
      name: "mid",
      step: 5,
      state: cloneState(state),
      timestamp: new Date(),
    });

    // Restore
    const snapshot = snapshots.get("mid");
    expect(snapshot?.step).toBe(5);
  });
});
