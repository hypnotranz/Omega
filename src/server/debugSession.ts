/**
 * Debug Session - Wrapper around CESK machine for web debugging
 *
 * Manages a single debug session with:
 * - Code loading and compilation
 * - Step execution with breakpoint checking
 * - State inspection
 * - History recording for time-travel
 */

import type {
  SessionConfig,
  MachineSnapshot,
  StepResult,
  Breakpoint,
  SerializedValue,
  SerializedFrame,
  SerializedStoreEntry,
} from './debugService';
import { serializeState, serializeValue, serializeFrame, extractBindings } from './stateSerializer';
import { COWStore } from '../core/eval/store';
import { stepOnce } from '../core/eval/machineStep';
import { installPrims } from '../core/prims';
import { compileTextToExpr } from '../core/pipeline/compileText';
import type { State, StepOutcome, Frame } from '../core/eval/machine';
import type { Val } from '../core/eval/values';
import { envGet } from '../core/eval/env';

// ============================================================
// HISTORY RECORD
// ============================================================

interface HistoryRecord {
  step: number;
  state: State;
  controlSummary: string;
  timestamp: string;
}

// ============================================================
// DEBUG SESSION
// ============================================================

let sessionCounter = 0;

export class DebugSession {
  readonly id: string;
  readonly config: SessionConfig;

  private state: State | null = null;
  private baseStore: COWStore;
  private baseEnv: any;

  private _stepCount = 0;
  private _status: 'idle' | 'running' | 'paused' | 'done' | 'error' | 'effect' = 'idle';
  private _lastOutcome: StepOutcome | null = null;
  private _error: Error | null = null;

  // Breakpoints
  private breakpoints: Breakpoint[] = [];
  private nextBreakpointId = 1;

  // History for time-travel
  private history: HistoryRecord[] = [];
  private maxHistory = 1000;

  // Execution control
  private paused = false;

  constructor(config: SessionConfig = {}) {
    this.id = `session_${++sessionCounter}_${Date.now().toString(36)}`;
    this.config = config;

    // Initialize base environment with primitives
    this.baseStore = new COWStore();
    const prim = installPrims(this.baseStore);
    this.baseEnv = prim.env;
    this.baseStore = prim.store as COWStore;
  }

  get stepCount(): number {
    return this._stepCount;
  }

  get status(): string {
    return this._status;
  }

  // ─────────────────────────────────────────────────────────────
  // CODE LOADING
  // ─────────────────────────────────────────────────────────────

  loadCode(code: string): { success: boolean; error?: string } {
    try {
      const expr = compileTextToExpr(`(begin ${code})`);

      this.state = {
        control: { tag: 'Expr', e: expr },
        env: this.baseEnv,
        store: this.baseStore.snapshot(),
        kont: [],
        handlers: [],
      };

      this._stepCount = 0;
      this._status = 'paused';
      this._lastOutcome = null;
      this._error = null;
      this.history = [];

      // Record initial state
      this.recordHistory();

      return { success: true };
    } catch (e) {
      this._status = 'error';
      this._error = e as Error;
      return { success: false, error: (e as Error).message };
    }
  }

  // ─────────────────────────────────────────────────────────────
  // STEPPING
  // ─────────────────────────────────────────────────────────────

  step(): StepResult {
    if (!this.state) {
      return {
        snapshot: this.getSnapshot(),
        outcome: 'error',
      };
    }

    if (this._status === 'done' || this._status === 'error') {
      return {
        snapshot: this.getSnapshot(),
        outcome: this._status === 'done' ? 'done' : 'error',
      };
    }

    try {
      const outcome = stepOnce(this.state);
      this._lastOutcome = outcome;
      this._stepCount++;

      switch (outcome.tag) {
        case 'State':
          this.state = outcome.state;
          this._status = 'paused';
          this.recordHistory();
          return {
            snapshot: this.getSnapshot(),
            outcome: this.checkBreakpoints() ?? 'stepped',
            breakpointId: this.getHitBreakpoint(),
          };

        case 'Done':
          this.state = outcome.state;
          this._status = 'done';
          this.recordHistory();
          return {
            snapshot: this.getSnapshot(),
            outcome: 'done',
          };

        case 'Op':
          this.state = outcome.state;
          this._status = 'effect';
          this.recordHistory();
          return {
            snapshot: this.getSnapshot(),
            outcome: 'effect',
          };
      }
    } catch (e) {
      this._status = 'error';
      this._error = e as Error;
      return {
        snapshot: this.getSnapshot(),
        outcome: 'error',
      };
    }
  }

  stepN(n: number): StepResult {
    let result: StepResult = { snapshot: this.getSnapshot(), outcome: 'stepped' };

    for (let i = 0; i < n; i++) {
      result = this.step();
      if (result.outcome !== 'stepped') break;
      if (this.paused) break;
    }

    return result;
  }

  continue(): StepResult {
    let result: StepResult = { snapshot: this.getSnapshot(), outcome: 'stepped' };
    const maxSteps = this.config.maxSteps ?? 10000;

    for (let i = 0; i < maxSteps; i++) {
      result = this.step();
      if (result.outcome !== 'stepped') break;
      if (this.paused) break;
    }

    return result;
  }

  run(maxSteps?: number): StepResult {
    return this.continue();
  }

  pause(): void {
    this.paused = true;
  }

  resumeWithValue(value: unknown): StepResult {
    if (this._status !== 'effect' || !this._lastOutcome || this._lastOutcome.tag !== 'Op') {
      return {
        snapshot: this.getSnapshot(),
        outcome: 'error',
      };
    }

    // Convert JSON value to Val and resume
    const { jsonToVal } = require('../core/opr/bridge');
    const val = jsonToVal(value);

    // Resume the continuation with the provided value
    const opcall = this._lastOutcome.opcall;
    if (opcall.resumption) {
      const newState = opcall.resumption.invoke(val);
      this.state = newState;
      this._status = 'paused';
      return this.step();
    }

    return {
      snapshot: this.getSnapshot(),
      outcome: 'error',
    };
  }

  // ─────────────────────────────────────────────────────────────
  // BREAKPOINTS
  // ─────────────────────────────────────────────────────────────

  addBreakpoint(bp: Omit<Breakpoint, 'id'>): string {
    const id = `bp_${this.nextBreakpointId++}`;
    this.breakpoints.push({ ...bp, id });
    return id;
  }

  removeBreakpoint(breakpointId: string): void {
    this.breakpoints = this.breakpoints.filter(bp => bp.id !== breakpointId);
  }

  listBreakpoints(): Breakpoint[] {
    return [...this.breakpoints];
  }

  toggleBreakpoint(breakpointId: string, enabled: boolean): void {
    const bp = this.breakpoints.find(b => b.id === breakpointId);
    if (bp) bp.enabled = enabled;
  }

  private checkBreakpoints(): 'breakpoint' | null {
    if (!this.state) return null;

    for (const bp of this.breakpoints) {
      if (!bp.enabled) continue;

      switch (bp.type) {
        case 'step':
          // Not applicable for step-based (would be checked externally)
          break;

        case 'expression':
          if (this.state.control.tag === 'Expr' && this.state.control.e.tag === bp.exprType) {
            return 'breakpoint';
          }
          break;

        case 'effect':
          if (this._lastOutcome?.tag === 'Op' && this._lastOutcome.opcall.op === bp.effectOp) {
            return 'breakpoint';
          }
          break;

        case 'binding':
          // Check if a specific variable was just defined
          if (this.state.control.tag === 'Val') {
            const frame = this.state.kont[this.state.kont.length - 1];
            if (frame?.tag === 'KDefine' && frame.name === bp.bindingName) {
              return 'breakpoint';
            }
          }
          break;
      }
    }

    return null;
  }

  private getHitBreakpoint(): string | undefined {
    if (!this.state) return undefined;

    for (const bp of this.breakpoints) {
      if (!bp.enabled) continue;

      switch (bp.type) {
        case 'expression':
          if (this.state.control.tag === 'Expr' && this.state.control.e.tag === bp.exprType) {
            return bp.id;
          }
          break;

        case 'effect':
          if (this._lastOutcome?.tag === 'Op' && this._lastOutcome.opcall.op === bp.effectOp) {
            return bp.id;
          }
          break;

        case 'binding':
          if (this.state.control.tag === 'Val') {
            const frame = this.state.kont[this.state.kont.length - 1];
            if (frame?.tag === 'KDefine' && frame.name === bp.bindingName) {
              return bp.id;
            }
          }
          break;
      }
    }

    return undefined;
  }

  // ─────────────────────────────────────────────────────────────
  // STATE INSPECTION
  // ─────────────────────────────────────────────────────────────

  getSnapshot(): MachineSnapshot {
    if (!this.state) {
      return {
        snapshotId: `empty_${Date.now()}`,
        step: 0,
        timestamp: new Date().toISOString(),
        control: { tag: 'Val', valType: 'Unit', valSummary: '()' },
        environment: [],
        callStack: [],
        handlers: [],
        status: this._status === 'idle' ? 'paused' : this._status,
        error: this._error ? { message: this._error.message, stack: this._error.stack } : undefined,
      };
    }

    const options: any = {};

    if (this._status === 'effect' && this._lastOutcome?.tag === 'Op') {
      options.pendingEffect = {
        op: this._lastOutcome.opcall.op,
        args: this._lastOutcome.opcall.args,
      };
    }

    if (this._status === 'done' && this._lastOutcome?.tag === 'Done') {
      options.result = this._lastOutcome.value;
    }

    if (this._status === 'error' && this._error) {
      options.error = this._error;
    }

    const status = this._status === 'idle' ? 'paused' : this._status;
    return serializeState(this.state, this._stepCount, status, options);
  }

  getBinding(name: string): SerializedValue | null {
    if (!this.state) return null;

    try {
      const addr = envGet(this.state.env, name);
      if (addr !== undefined) {
        const val = this.state.store.read(addr);
        return serializeValue(val, 4);
      }
    } catch {
      // Binding not found
    }

    return null;
  }

  getStoreEntry(address: number): SerializedStoreEntry | null {
    if (!this.state) return null;

    try {
      const val = this.state.store.read(address);
      if (val) {
        return {
          address,
          value: serializeValue(val, 4),
        };
      }
    } catch {
      // Address not found
    }

    return null;
  }

  evaluate(expr: string): { value: SerializedValue } | { error: string } {
    if (!this.state) {
      return { error: 'No active session' };
    }

    try {
      // Compile the expression
      const compiled = compileTextToExpr(expr);

      // Create a temporary state to evaluate in current environment
      const tempState: State = {
        control: { tag: 'Expr', e: compiled },
        env: this.state.env,
        store: this.state.store.snapshot(),
        kont: [],
        handlers: [],
      };

      // Run to completion (with step limit)
      let current = tempState;
      for (let i = 0; i < 1000; i++) {
        const outcome = stepOnce(current);
        if (outcome.tag === 'Done') {
          return { value: serializeValue(outcome.value, 4) };
        } else if (outcome.tag === 'Op') {
          return { error: `Effect encountered: ${outcome.opcall.op}` };
        }
        current = outcome.state;
      }

      return { error: 'Evaluation exceeded step limit' };
    } catch (e) {
      return { error: (e as Error).message };
    }
  }

  getCallStack(): SerializedFrame[] {
    if (!this.state) return [];
    return this.state.kont.map((frame, i) => serializeFrame(frame, i));
  }

  // ─────────────────────────────────────────────────────────────
  // TIME TRAVEL
  // ─────────────────────────────────────────────────────────────

  private recordHistory(): void {
    if (!this.state) return;

    const record: HistoryRecord = {
      step: this._stepCount,
      state: this.cloneState(this.state),
      controlSummary: this.controlSummary(),
      timestamp: new Date().toISOString(),
    };

    this.history.push(record);

    // Trim history if too long
    if (this.history.length > this.maxHistory) {
      this.history = this.history.slice(-this.maxHistory);
    }
  }

  private cloneState(state: State): State {
    // Deep clone the state for history
    return {
      control: state.control,
      env: state.env,
      store: (state.store as COWStore).snapshot(),
      kont: [...state.kont],
      handlers: [...state.handlers],
      profile: state.profile,
      budget: state.budget ? { ...state.budget } : undefined,
      sec: state.sec,
    };
  }

  private controlSummary(): string {
    if (!this.state) return '';

    const ctrl = this.state.control;
    if (ctrl.tag === 'Val') {
      return `Val: ${ctrl.v.tag}`;
    } else {
      return `Expr: ${ctrl.e.tag}`;
    }
  }

  jumpToStep(step: number): MachineSnapshot {
    const record = this.history.find(h => h.step === step);
    if (!record) {
      // If not in history, we can't jump
      return this.getSnapshot();
    }

    // Restore state from history
    this.state = this.cloneState(record.state);
    this._stepCount = record.step;
    this._status = 'paused';
    this._lastOutcome = null;
    this._error = null;

    return this.getSnapshot();
  }

  getHistory(): Array<{ step: number; control: string; timestamp: string }> {
    return this.history.map(h => ({
      step: h.step,
      control: h.controlSummary,
      timestamp: h.timestamp,
    }));
  }
}
