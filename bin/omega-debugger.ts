#!/usr/bin/env npx tsx
// bin/omega-debugger.ts - REAL Interactive Step Debugger
// Run:  npx tsx bin/omega-debugger.ts
//
// A first-class Lisp debugger with:
//   - Step-through execution (step, next, continue)
//   - Breakpoints (break on step number, expression type, or effect)
//   - Full state inspection (env, store, stack, control)
//   - State save/load (dump snapshots, reload them)
//   - Interactive REPL at any point

import * as readline from "readline";
import * as fs from "fs";
import * as path from "path";
import { COWStore } from "../src/core/eval/store";
import { stepOnce, type StepResult } from "../src/core/eval/machineStep";
import { installPrims } from "../test/helpers/prims";
import { compileTextToExpr } from "../src/core/pipeline/compileText";
import type { State, Frame } from "../src/core/eval/machine";
import type { Val } from "../src/core/eval/values";
import type { Env } from "../src/core/eval/env";

// ─────────────────────────────────────────────────────────────────
// Pretty Printing
// ─────────────────────────────────────────────────────────────────
function valToString(v: Val, depth = 0): string {
  if (depth > 3) return "...";
  switch (v.tag) {
    case "Unit": return "()";
    case "Num": return String(v.n);
    case "Bool": return v.b ? "#t" : "#f";
    case "Str": return JSON.stringify(v.s);
    case "Sym": return `'${v.name}`;
    case "Vector": return `[${v.items.map(x => valToString(x, depth + 1)).join(" ")}]`;
    case "Closure": return `<closure (${(v as any).params?.join(" ") || "..."})>`;
    case "Native": return `<native>`;
    case "Cont": return `<continuation>`;
    default: return `<${v.tag}>`;
  }
}

function controlToString(ctrl: State["control"]): string {
  if (ctrl.tag === "Val") {
    return `Value: ${valToString(ctrl.v)}`;
  } else if (ctrl.tag === "Expr") {
    const e = ctrl.e;
    switch (e.tag) {
      case "Lit": return `Expr: Lit(${JSON.stringify(e.value)})`;
      case "Var": return `Expr: Var(${e.name})`;
      case "App": return `Expr: App(...)`;
      case "If": return `Expr: If(...)`;
      case "Lambda": return `Expr: Lambda(${(e as any).params?.length || 0} params)`;
      case "Begin": return `Expr: Begin(${(e as any).exprs?.length || 0} exprs)`;
      case "Define": return `Expr: Define(${(e as any).name || "?"})`;
      case "Effect": return `Expr: Effect(${(e as any).op || "?"})`;
      default: return `Expr: ${e.tag}`;
    }
  }
  return `Control: ${ctrl.tag}`;
}

function frameToString(frame: Frame, idx: number): string {
  switch (frame.tag) {
    case "KIf": return `[${idx}] KIf - waiting for test result`;
    case "KBegin": return `[${idx}] KBegin - ${frame.rest.length} exprs remaining`;
    case "KDefine": return `[${idx}] KDefine - defining ${frame.name}`;
    case "KSet": return `[${idx}] KSet - setting ${frame.name}`;
    case "KAppFun": return `[${idx}] KAppFun - evaluating function, ${frame.args.length} args to eval`;
    case "KAppArg": return `[${idx}] KAppArg - ${frame.acc.length}/${frame.pending.length + frame.acc.length} args evaluated`;
    case "KCall": return `[${idx}] KCall - executing function body`;
    case "KEffect": return `[${idx}] KEffect - effect ${frame.op}`;
    case "KHandleBoundary": return `[${idx}] KHandleBoundary - handler ${frame.hid}`;
    case "KHandleReturn": return `[${idx}] KHandleReturn - ${frame.mode}`;
    case "KMatch": return `[${idx}] KMatch - ${frame.clauses.length} clauses`;
    default: return `[${idx}] ${(frame as any).tag || "?"}`;
  }
}

// ─────────────────────────────────────────────────────────────────
// Debugger State
// ─────────────────────────────────────────────────────────────────
interface Breakpoint {
  id: number;
  type: "step" | "expr" | "effect" | "value";
  condition: string | number;
  enabled: boolean;
}

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

class Debugger {
  private state: State | null = null;
  private stepCount = 0;
  private breakpoints: Breakpoint[] = [];
  private nextBreakpointId = 1;
  private snapshots: Map<string, Snapshot> = new Map();
  private history: { step: number; state: State; control: string }[] = [];
  private maxHistory = 100;
  private running = false;
  private stepMode: "step" | "continue" | "next" = "step";

  // Full trace recording for jump-to-step and dump/load
  private trace: TraceRecord[] = [];
  private recordingEnabled = true;
  private initialCode: string = "";

  constructor() {}

  // Load and compile Lisp code
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
    this.trace = [];
    this.initialCode = code;

    // Record initial state
    if (this.recordingEnabled && this.state) {
      this.trace.push({
        step: 0,
        state: this.cloneState(this.state),
        controlSummary: controlToString(this.state.control),
        stackDepth: 0,
      });
    }

    console.log("Loaded. Type 'step' or 's' to begin stepping.");
    console.log(`Recording: ${this.recordingEnabled ? "ON" : "OFF"} (use 'record on/off' to toggle)`);
  }

  // Load from file
  loadFile(filepath: string): void {
    const resolved = path.resolve(filepath);
    if (!fs.existsSync(resolved)) {
      console.log(`File not found: ${resolved}`);
      return;
    }
    const code = fs.readFileSync(resolved, "utf8");
    this.load(code);
    console.log(`Loaded from: ${resolved}`);
  }

  // Execute one step
  step(): StepResult | null {
    if (!this.state) {
      console.log("No program loaded. Use 'load (code)' or 'loadfile path'");
      return null;
    }

    // Save to history (limited)
    if (this.history.length >= this.maxHistory) {
      this.history.shift();
    }
    this.history.push({
      step: this.stepCount,
      state: this.cloneState(this.state),
      control: controlToString(this.state.control),
    });

    const result = stepOnce(this.state);
    this.stepCount++;

    if (result.tag === "State") {
      this.state = result.state;

      // Record to full trace for jump-to-step
      if (this.recordingEnabled) {
        this.trace.push({
          step: this.stepCount,
          state: this.cloneState(this.state),
          controlSummary: controlToString(this.state.control),
          stackDepth: this.state.kont.length,
        });
      }
    } else if (result.tag === "Done") {
      console.log(`\n=== DONE at step ${this.stepCount} ===`);
      console.log(`Result: ${valToString(result.value)}`);
      console.log(`Total steps recorded: ${this.trace.length}`);
      this.state = null;
    } else if (result.tag === "Op") {
      console.log(`\n=== EFFECT at step ${this.stepCount} ===`);
      console.log(`Operation: ${result.opcall.op}`);
      console.log(`Args: ${result.opcall.args.map(a => valToString(a)).join(", ")}`);
      console.log("(Effects require runtime handling - use 'resume <value>' to continue)");
      this.state = result.state;
    }

    return result;
  }

  // Check if any breakpoint matches current state
  private checkBreakpoints(): Breakpoint | null {
    if (!this.state) return null;

    for (const bp of this.breakpoints) {
      if (!bp.enabled) continue;

      if (bp.type === "step" && this.stepCount === bp.condition) {
        return bp;
      }
      if (bp.type === "expr" && this.state.control.tag === "Expr") {
        const e = this.state.control.e;
        if (e.tag === bp.condition) return bp;
        if (e.tag === "Define" && (e as any).name === bp.condition) return bp;
        if (e.tag === "Var" && (e as any).name === bp.condition) return bp;
      }
      if (bp.type === "effect" && this.state.control.tag === "Expr") {
        const e = this.state.control.e;
        if (e.tag === "Effect" && (e as any).op === bp.condition) return bp;
      }
    }
    return null;
  }

  // Run until breakpoint or completion
  run(maxSteps = 100000): void {
    if (!this.state) {
      console.log("No program loaded.");
      return;
    }

    this.running = true;
    let steps = 0;

    while (this.running && this.state && steps < maxSteps) {
      // Check breakpoints BEFORE stepping
      const bp = this.checkBreakpoints();
      if (bp && steps > 0) {
        console.log(`\n*** Breakpoint ${bp.id} hit at step ${this.stepCount} ***`);
        console.log(`  Type: ${bp.type}, Condition: ${bp.condition}`);
        this.running = false;
        break;
      }

      const result = this.step();
      steps++;

      if (!result || result.tag === "Done" || result.tag === "Op") {
        this.running = false;
        break;
      }
    }

    if (this.running && steps >= maxSteps) {
      console.log(`\nStopped after ${maxSteps} steps (use 'continue' to keep going)`);
      this.running = false;
    }
  }

  // Stop running
  stop(): void {
    this.running = false;
    console.log("Stopped.");
  }

  // Clone state for history
  private cloneState(s: State): State {
    // Shallow clone - store is COW so this is safe
    return { ...s, kont: [...s.kont], handlers: [...s.handlers] };
  }

  // Print current state summary
  printState(): void {
    if (!this.state) {
      console.log("No program loaded or execution finished.");
      return;
    }

    console.log(`\n─── Step ${this.stepCount} ───`);
    console.log(`Control: ${controlToString(this.state.control)}`);
    console.log(`Stack depth: ${this.state.kont.length}`);
    console.log(`Handlers: ${this.state.handlers.length}`);
  }

  // Print the call stack
  printStack(): void {
    if (!this.state) {
      console.log("No state.");
      return;
    }

    const kont = this.state.kont;
    if (kont.length === 0) {
      console.log("Stack is empty.");
      return;
    }

    console.log(`\nStack (${kont.length} frames, top first):`);
    for (let i = kont.length - 1; i >= 0; i--) {
      console.log(frameToString(kont[i], i));
    }
  }

  // Inspect a specific stack frame
  inspectFrame(idx: number): void {
    if (!this.state) {
      console.log("No state.");
      return;
    }

    if (idx < 0 || idx >= this.state.kont.length) {
      console.log(`Frame ${idx} out of range (0-${this.state.kont.length - 1})`);
      return;
    }

    const frame = this.state.kont[idx];
    console.log(`\nFrame ${idx}: ${frame.tag}`);
    console.log(`  Raw: ${JSON.stringify(frame, null, 2).slice(0, 500)}`);

    if ("env" in frame && frame.env) {
      const bindings = this.envToBindings(frame.env as Env);
      console.log(`  Environment (${bindings.length} bindings):`);
      for (const b of bindings.slice(0, 10)) {
        const val = this.state!.store.read(b.addr);
        console.log(`    ${b.name} = ${valToString(val)}`);
      }
      if (bindings.length > 10) {
        console.log(`    ... and ${bindings.length - 10} more`);
      }
    }
  }

  // Print environment bindings
  printEnv(filter?: string): void {
    if (!this.state) {
      console.log("No state.");
      return;
    }

    const bindings = this.envToBindings(this.state.env);
    const filtered = filter
      ? bindings.filter(b => b.name.includes(filter))
      : bindings;

    console.log(`\nEnvironment (${filtered.length}${filter ? " matching" : ""} bindings):`);
    for (const b of filtered) {
      const val = this.state.store.read(b.addr);
      console.log(`  ${b.name} = ${valToString(val)}`);
    }
  }

  // Lookup a specific binding
  lookup(name: string): void {
    if (!this.state) {
      console.log("No state.");
      return;
    }

    // Search through the context chain for the binding
    let ctx: any = this.state.env;
    let addr: number | undefined;
    while (ctx) {
      if (ctx.frame && ctx.frame instanceof Map && ctx.frame.has(name)) {
        addr = ctx.frame.get(name);
        break;
      }
      ctx = ctx.parent;
    }

    if (addr === undefined) {
      console.log(`Binding '${name}' not found.`);
      return;
    }

    const val = this.state.store.read(addr);
    console.log(`${name} (@${addr}) = ${valToString(val)}`);
    console.log(`  Full: ${JSON.stringify(val, null, 2).slice(0, 1000)}`);
  }

  private envToBindings(env: Env): { name: string; addr: number }[] {
    const bindings: { name: string; addr: number }[] = [];
    // Traverse the context chain (Ctx has frame: Map<string, Addr> and parent: Ctx)
    let ctx: any = env;
    const seen = new Set<string>();
    while (ctx) {
      if (ctx.frame && ctx.frame instanceof Map) {
        for (const [name, addr] of ctx.frame.entries()) {
          if (!seen.has(name)) {
            seen.add(name);
            bindings.push({ name, addr: addr as number });
          }
        }
      }
      ctx = ctx.parent;
    }
    return bindings.sort((a, b) => a.name.localeCompare(b.name));
  }

  // Evaluate expression in current environment
  evalExpr(code: string): void {
    if (!this.state) {
      console.log("No state - load a program first.");
      return;
    }

    try {
      const expr = compileTextToExpr(code);
      const tempState: State = {
        control: { tag: "Expr", e: expr },
        env: this.state.env,
        store: this.state.store,
        kont: [],
        handlers: [],
      };

      // Run to completion (limited steps)
      let st = tempState;
      for (let i = 0; i < 10000; i++) {
        const result = stepOnce(st);
        if (result.tag === "Done") {
          console.log(`=> ${valToString(result.value)}`);
          return;
        } else if (result.tag === "Op") {
          console.log(`=> Effect: ${result.opcall.op}`);
          return;
        }
        st = result.state;
      }
      console.log("(eval exceeded 10000 steps)");
    } catch (e: any) {
      console.log(`Error: ${e.message}`);
    }
  }

  // Add a breakpoint
  addBreakpoint(type: Breakpoint["type"], condition: string | number): void {
    const bp: Breakpoint = {
      id: this.nextBreakpointId++,
      type,
      condition,
      enabled: true,
    };
    this.breakpoints.push(bp);
    console.log(`Breakpoint ${bp.id} added: ${type} = ${condition}`);
  }

  // List breakpoints
  listBreakpoints(): void {
    if (this.breakpoints.length === 0) {
      console.log("No breakpoints set.");
      return;
    }

    console.log("\nBreakpoints:");
    for (const bp of this.breakpoints) {
      console.log(`  ${bp.id}: ${bp.type} = ${bp.condition} [${bp.enabled ? "enabled" : "disabled"}]`);
    }
  }

  // Delete a breakpoint
  deleteBreakpoint(id: number): void {
    const idx = this.breakpoints.findIndex(bp => bp.id === id);
    if (idx < 0) {
      console.log(`Breakpoint ${id} not found.`);
      return;
    }
    this.breakpoints.splice(idx, 1);
    console.log(`Breakpoint ${id} deleted.`);
  }

  // Toggle breakpoint
  toggleBreakpoint(id: number): void {
    const bp = this.breakpoints.find(b => b.id === id);
    if (!bp) {
      console.log(`Breakpoint ${id} not found.`);
      return;
    }
    bp.enabled = !bp.enabled;
    console.log(`Breakpoint ${id} ${bp.enabled ? "enabled" : "disabled"}.`);
  }

  // Save current state as snapshot
  saveSnapshot(name: string): void {
    if (!this.state) {
      console.log("No state to save.");
      return;
    }

    const snapshot: Snapshot = {
      name,
      step: this.stepCount,
      state: this.cloneState(this.state),
      timestamp: new Date(),
    };
    this.snapshots.set(name, snapshot);
    console.log(`Saved snapshot '${name}' at step ${this.stepCount}`);
  }

  // Load a snapshot
  loadSnapshot(name: string): void {
    const snapshot = this.snapshots.get(name);
    if (!snapshot) {
      console.log(`Snapshot '${name}' not found.`);
      return;
    }

    this.state = this.cloneState(snapshot.state);
    this.stepCount = snapshot.step;
    console.log(`Loaded snapshot '${name}' (step ${this.stepCount})`);
  }

  // List snapshots
  listSnapshots(): void {
    if (this.snapshots.size === 0) {
      console.log("No snapshots saved.");
      return;
    }

    console.log("\nSnapshots:");
    for (const [name, snap] of this.snapshots) {
      console.log(`  ${name}: step ${snap.step} (${snap.timestamp.toISOString()})`);
    }
  }

  // Export snapshot to file
  exportSnapshot(name: string, filepath: string): void {
    const snapshot = this.snapshots.get(name);
    if (!snapshot) {
      console.log(`Snapshot '${name}' not found.`);
      return;
    }

    const data = JSON.stringify({
      name: snapshot.name,
      step: snapshot.step,
      timestamp: snapshot.timestamp.toISOString(),
      state: {
        controlTag: snapshot.state.control.tag,
        stackDepth: snapshot.state.kont.length,
        // Note: full state serialization would require more work
        note: "Partial export - full state requires runtime support"
      }
    }, null, 2);

    fs.writeFileSync(filepath, data);
    console.log(`Exported snapshot metadata to ${filepath}`);
  }

  // Go back in history
  goBack(steps = 1): void {
    if (this.history.length === 0) {
      console.log("No history available.");
      return;
    }

    const targetIdx = Math.max(0, this.history.length - steps);
    const entry = this.history[targetIdx];
    this.state = this.cloneState(entry.state);
    this.stepCount = entry.step;
    this.history = this.history.slice(0, targetIdx);
    console.log(`Rewound to step ${this.stepCount}`);
  }

  // Print history
  printHistory(count = 10): void {
    console.log(`\nHistory (last ${Math.min(count, this.history.length)} steps):`);
    const start = Math.max(0, this.history.length - count);
    for (let i = start; i < this.history.length; i++) {
      const h = this.history[i];
      console.log(`  [${h.step}] ${h.control}`);
    }
    console.log(`  [${this.stepCount}] <current>`);
  }

  get currentStep(): number {
    return this.stepCount;
  }

  get hasState(): boolean {
    return this.state !== null;
  }

  // Jump to a specific step in the recorded trace
  goto(targetStep: number): void {
    if (this.trace.length === 0) {
      console.log("No trace recorded. Run the program first with recording enabled.");
      return;
    }

    const record = this.trace.find(r => r.step === targetStep);
    if (!record) {
      console.log(`Step ${targetStep} not found in trace. Available: 0-${this.trace[this.trace.length - 1].step}`);
      return;
    }

    this.state = this.cloneState(record.state);
    this.stepCount = record.step;
    console.log(`Jumped to step ${this.stepCount}`);
  }

  // List recorded steps
  listTrace(start = 0, count = 20): void {
    if (this.trace.length === 0) {
      console.log("No trace recorded.");
      return;
    }

    console.log(`\nTrace (${this.trace.length} total steps recorded):`);
    const end = Math.min(start + count, this.trace.length);
    for (let i = start; i < end; i++) {
      const t = this.trace[i];
      const marker = t.step === this.stepCount ? " <-- current" : "";
      console.log(`  [${t.step}] ${t.controlSummary} | stack=${t.stackDepth}${marker}`);
    }
    if (end < this.trace.length) {
      console.log(`  ... (${this.trace.length - end} more, use 'trace ${end} ${count}' to see more)`);
    }
  }

  // Toggle recording on/off
  setRecording(enabled: boolean): void {
    this.recordingEnabled = enabled;
    console.log(`Recording: ${enabled ? "ON" : "OFF"}`);
    if (!enabled) {
      console.log("Warning: Without recording, 'goto' and 'dump' won't have new steps.");
    }
  }

  // Dump trace to file (replayable)
  dumpTrace(filepath: string): void {
    if (this.trace.length === 0) {
      console.log("No trace to dump.");
      return;
    }

    // Create a serializable representation
    const dumpData = {
      version: 1,
      code: this.initialCode,
      timestamp: new Date().toISOString(),
      totalSteps: this.trace.length,
      // Store trace summaries (not full state - that would be huge)
      traceSummary: this.trace.map(t => ({
        step: t.step,
        control: t.controlSummary,
        stackDepth: t.stackDepth,
      })),
    };

    fs.writeFileSync(filepath, JSON.stringify(dumpData, null, 2));
    console.log(`Dumped trace summary to ${filepath} (${this.trace.length} steps)`);
    console.log("To replay: load the code and step through, or use 'replay <file>'");
  }

  // Load and replay a dump file
  replayDump(filepath: string): void {
    const resolved = path.resolve(filepath);
    if (!fs.existsSync(resolved)) {
      console.log(`File not found: ${resolved}`);
      return;
    }

    try {
      const content = fs.readFileSync(resolved, "utf8");
      const data = JSON.parse(content);

      if (!data.code) {
        console.log("Dump file missing 'code' field - cannot replay.");
        return;
      }

      console.log(`Loading dump from: ${resolved}`);
      console.log(`Original timestamp: ${data.timestamp}`);
      console.log(`Total steps: ${data.totalSteps}`);

      // Re-run the code to rebuild the trace
      this.load(data.code);

      console.log("\nReplaying execution...");
      let count = 0;
      while (this.state && count < (data.totalSteps || 100000)) {
        const result = this.step();
        count++;
        if (!result || result.tag === "Done" || result.tag === "Op") break;
      }

      console.log(`\nReplay complete. ${this.trace.length} steps recorded.`);
      console.log("Use 'goto <step>' to jump to any step, or 'trace' to see the trace.");
    } catch (err: any) {
      console.log(`Error loading dump: ${err.message}`);
    }
  }

  // Run to completion and record everything
  runAndRecord(maxSteps = 100000): void {
    if (!this.state) {
      console.log("No program loaded.");
      return;
    }

    const wasRecording = this.recordingEnabled;
    this.recordingEnabled = true;

    console.log("Running to completion with full recording...");
    let count = 0;
    while (this.state && count < maxSteps) {
      const result = this.step();
      count++;
      if (!result || result.tag === "Done" || result.tag === "Op") break;
    }

    this.recordingEnabled = wasRecording;
    console.log(`\nRecorded ${this.trace.length} steps. Use 'goto <N>' to jump to any step.`);
  }
}

// ─────────────────────────────────────────────────────────────────
// Interactive REPL
// ─────────────────────────────────────────────────────────────────
function processCommand(debugger_: Debugger, trimmed: string): boolean {
  const parts = trimmed.split(/\s+/);
  const cmd = parts[0]?.toLowerCase();

  try {
    switch (cmd) {
      case "quit":
      case "q":
      case "exit":
        console.log("Goodbye!");
        return true; // Signal to exit

      case "help":
      case "h":
      case "?":
        console.log("Type a command. Main ones: step, continue, state, stack, env, eval");
        break;

      case "load":
        const codeMatch = trimmed.match(/^load\s+(.+)$/i);
        if (codeMatch) {
          debugger_.load(codeMatch[1]);
        } else {
          console.log("Usage: load (your lisp code here)");
        }
        break;

      case "loadfile":
        if (parts[1]) {
          debugger_.loadFile(parts[1]);
        } else {
          console.log("Usage: loadfile <path>");
        }
        break;

      case "s":
      case "step":
        debugger_.step();
        if (debugger_.hasState) debugger_.printState();
        break;

      case "n":
      case "next":
        const stepCount = parseInt(parts[1]) || 1;
        for (let i = 0; i < stepCount && debugger_.hasState; i++) {
          debugger_.step();
        }
        if (debugger_.hasState) debugger_.printState();
        break;

      case "c":
      case "continue":
      case "run":
        debugger_.run();
        if (debugger_.hasState) debugger_.printState();
        break;

      case "stop":
        debugger_.stop();
        break;

      case "state":
      case "st":
        debugger_.printState();
        break;

      case "stack":
      case "bt":
        debugger_.printStack();
        break;

      case "frame":
      case "f":
        const frameIdx = parseInt(parts[1]);
        if (isNaN(frameIdx)) {
          console.log("Usage: frame <number>");
        } else {
          debugger_.inspectFrame(frameIdx);
        }
        break;

      case "env":
      case "e":
        debugger_.printEnv(parts[1]);
        break;

      case "lookup":
      case "l":
        if (parts[1]) {
          debugger_.lookup(parts[1]);
        } else {
          console.log("Usage: lookup <name>");
        }
        break;

      case "eval":
        const evalMatch = trimmed.match(/^eval\s+(.+)$/i);
        if (evalMatch) {
          debugger_.evalExpr(evalMatch[1]);
        } else {
          console.log("Usage: eval (expression)");
        }
        break;

      case "break":
      case "b":
        if (parts[1] === "step" && parts[2]) {
          debugger_.addBreakpoint("step", parseInt(parts[2]));
        } else if (parts[1] === "expr" && parts[2]) {
          debugger_.addBreakpoint("expr", parts[2]);
        } else if (parts[1] === "effect" && parts[2]) {
          debugger_.addBreakpoint("effect", parts[2]);
        } else {
          console.log("Usage: break step <N> | break expr <tag> | break effect <op>");
        }
        break;

      case "breaks":
      case "breakpoints":
        debugger_.listBreakpoints();
        break;

      case "delete":
      case "del":
      case "d":
        if (parts[1]) {
          debugger_.deleteBreakpoint(parseInt(parts[1]));
        } else {
          console.log("Usage: delete <breakpoint-id>");
        }
        break;

      case "toggle":
        if (parts[1]) {
          debugger_.toggleBreakpoint(parseInt(parts[1]));
        } else {
          console.log("Usage: toggle <breakpoint-id>");
        }
        break;

      case "save":
        if (parts[1]) {
          debugger_.saveSnapshot(parts[1]);
        } else {
          console.log("Usage: save <name>");
        }
        break;

      case "restore":
      case "load-snapshot":
        if (parts[1]) {
          debugger_.loadSnapshot(parts[1]);
          debugger_.printState();
        } else {
          console.log("Usage: restore <name>");
        }
        break;

      case "snapshots":
      case "snaps":
        debugger_.listSnapshots();
        break;

      case "export":
        if (parts[1] && parts[2]) {
          debugger_.exportSnapshot(parts[1], parts[2]);
        } else {
          console.log("Usage: export <snapshot-name> <filepath>");
        }
        break;

      case "back":
        debugger_.goBack(parseInt(parts[1]) || 1);
        debugger_.printState();
        break;

      case "history":
      case "hist":
        debugger_.printHistory(parseInt(parts[1]) || 10);
        break;

      // Trace and dump/load commands
      case "goto":
      case "g":
        if (parts[1]) {
          debugger_.goto(parseInt(parts[1]));
          debugger_.printState();
        } else {
          console.log("Usage: goto <step-number>");
        }
        break;

      case "trace":
      case "tr":
        debugger_.listTrace(parseInt(parts[1]) || 0, parseInt(parts[2]) || 20);
        break;

      case "dump":
        if (parts[1]) {
          debugger_.dumpTrace(parts[1]);
        } else {
          console.log("Usage: dump <filepath>");
        }
        break;

      case "replay":
        if (parts[1]) {
          debugger_.replayDump(parts[1]);
        } else {
          console.log("Usage: replay <filepath>");
        }
        break;

      case "record":
        if (parts[1] === "on") {
          debugger_.setRecording(true);
        } else if (parts[1] === "off") {
          debugger_.setRecording(false);
        } else {
          console.log("Usage: record on|off");
        }
        break;

      case "runrecord":
      case "rr":
        debugger_.runAndRecord(parseInt(parts[1]) || 100000);
        break;

      case "":
        // Empty line - do nothing
        break;

      default:
        // Try to treat as Lisp expression
        if (trimmed.startsWith("(")) {
          if (!debugger_.hasState) {
            // No state - load it
            debugger_.load(trimmed);
          } else {
            // Has state - eval in current env
            debugger_.evalExpr(trimmed);
          }
        } else {
          console.log(`Unknown command: ${cmd}. Type 'help' for commands.`);
        }
    }
  } catch (err: any) {
    console.log(`Error: ${err.message}`);
  }

  return false; // Don't exit
}

// ─────────────────────────────────────────────────────────────────
// Session Persistence
// ─────────────────────────────────────────────────────────────────
const SESSION_DIR = path.join(process.env.HOME || process.env.USERPROFILE || ".", ".omega-sessions");

function ensureSessionDir() {
  if (!fs.existsSync(SESSION_DIR)) {
    fs.mkdirSync(SESSION_DIR, { recursive: true });
  }
}

function getSessionPath(name: string): string {
  return path.join(SESSION_DIR, `${name}.json`);
}

interface SessionData {
  code: string;
  stepCount: number;
  traceLength: number;
}

function saveSession(debugger_: Debugger, name: string): void {
  ensureSessionDir();
  const data: SessionData = {
    code: (debugger_ as any).initialCode,
    stepCount: debugger_.currentStep,
    traceLength: (debugger_ as any).trace?.length || 0,
  };
  fs.writeFileSync(getSessionPath(name), JSON.stringify(data, null, 2));
}

function loadSession(debugger_: Debugger, name: string): boolean {
  const sessionPath = getSessionPath(name);
  if (!fs.existsSync(sessionPath)) {
    return false;
  }

  try {
    const data: SessionData = JSON.parse(fs.readFileSync(sessionPath, "utf8"));
    if (data.code) {
      debugger_.load(data.code);
      // Replay to the saved step
      let count = 0;
      while (debugger_.hasState && count < data.stepCount) {
        debugger_.step();
        count++;
      }
      return true;
    }
  } catch (err) {
    console.error(`Error loading session: ${err}`);
  }
  return false;
}

// Parse command line args
function parseArgs(): { session?: string; cmd?: string; json?: boolean } {
  const args = process.argv.slice(2);
  const result: { session?: string; cmd?: string; json?: boolean } = {};

  for (let i = 0; i < args.length; i++) {
    if ((args[i] === "--session" || args[i] === "-s") && args[i + 1]) {
      result.session = args[++i];
    } else if ((args[i] === "--cmd" || args[i] === "-c") && args[i + 1]) {
      result.cmd = args[++i];
    } else if (args[i] === "--json" || args[i] === "-j") {
      result.json = true;
    }
  }

  return result;
}

async function main() {
  const args = parseArgs();
  const isTTY = process.stdin.isTTY && !args.session;

  // Session mode: load session, run command, save session, exit
  if (args.session && args.cmd) {
    const debugger_ = new Debugger();

    // Load existing session
    const loaded = loadSession(debugger_, args.session);

    // Run the command
    const output: string[] = [];
    const originalLog = console.log;
    console.log = (...msgs: any[]) => output.push(msgs.join(" "));

    processCommand(debugger_, args.cmd);

    console.log = originalLog;

    // Save session
    saveSession(debugger_, args.session);

    // Output result
    if (args.json) {
      const result = {
        session: args.session,
        step: debugger_.currentStep,
        hasState: debugger_.hasState,
        output: output.join("\n"),
      };
      console.log(JSON.stringify(result, null, 2));
    } else {
      output.forEach(line => console.log(line));
    }

    return;
  }

  // If just --session without --cmd, show session info
  if (args.session && !args.cmd) {
    const debugger_ = new Debugger();
    const loaded = loadSession(debugger_, args.session);
    if (loaded) {
      console.log(`Session '${args.session}' loaded at step ${debugger_.currentStep}`);
      debugger_.printState();
    } else {
      console.log(`Session '${args.session}' not found. Use --cmd to create it.`);
    }
    return;
  }

  console.log("╔═══════════════════════════════════════════════════════════════╗");
  console.log("║  Omega Debugger - Interactive Step Debugger                   ║");
  console.log("╚═══════════════════════════════════════════════════════════════╝");

  if (isTTY) {
    console.log("");
    console.log("Commands:");
    console.log("  load (code)      - Load Lisp code to debug");
    console.log("  loadfile <path>  - Load from file");
    console.log("  s, step          - Execute one step");
    console.log("  n, next [N]      - Execute N steps (default 1)");
    console.log("  c, continue      - Run until breakpoint or completion");
    console.log("  stop             - Stop execution");
    console.log("");
    console.log("  state            - Show current state summary");
    console.log("  stack            - Show call stack");
    console.log("  frame <N>        - Inspect stack frame N");
    console.log("  env [filter]     - Show environment bindings");
    console.log("  lookup <name>    - Lookup specific binding");
    console.log("  eval (expr)      - Evaluate expression in current env");
    console.log("");
    console.log("  break step <N>   - Break at step N");
    console.log("  break expr <tag> - Break on expression type (Define, Effect, etc)");
    console.log("  break effect <op> - Break on effect operation");
    console.log("  breaks           - List breakpoints");
    console.log("  delete <id>      - Delete breakpoint");
    console.log("  toggle <id>      - Enable/disable breakpoint");
    console.log("");
    console.log("  save <name>      - Save current state as snapshot");
    console.log("  restore <name>   - Restore snapshot");
    console.log("  snapshots        - List snapshots");
    console.log("  export <n> <f>   - Export snapshot to file");
    console.log("");
    console.log("  back [N]         - Go back N steps in history");
    console.log("  history [N]      - Show last N history entries");
    console.log("");
    console.log("  goto <N>, g <N>  - Jump to step N in recorded trace");
    console.log("  trace [s] [n]    - List trace (start at s, show n steps)");
    console.log("  runrecord, rr    - Run to completion with full recording");
    console.log("  record on|off    - Toggle trace recording");
    console.log("  dump <file>      - Save trace to file");
    console.log("  replay <file>    - Load and replay a dump file");
    console.log("");
    console.log("  help             - Show this help");
    console.log("  quit             - Exit debugger");
    console.log("");
  }

  const debugger_ = new Debugger();

  // Non-TTY: process piped input line by line
  if (!isTTY) {
    const rl = readline.createInterface({ input: process.stdin });
    for await (const line of rl) {
      const trimmed = line.trim();
      console.log(`dbg> ${trimmed}`);
      if (processCommand(debugger_, trimmed)) {
        break;
      }
    }
    return;
  }

  // TTY: interactive mode
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    prompt: "dbg> ",
  });

  rl.prompt();

  rl.on("line", (line) => {
    const trimmed = line.trim();
    if (processCommand(debugger_, trimmed)) {
      rl.close();
      process.exit(0);
    }
    rl.prompt();
  });

  rl.on("close", () => {
    console.log("\nGoodbye!");
    process.exit(0);
  });
}

main().catch(console.error);
