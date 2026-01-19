// test/helpers/debug.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-17.md
// Prompt 7: Programmatic debug harness for time-travel and multi-shot continuations
//
// Provides APIs for:
// - Running to suspension (effect boundary)
// - Stack inspection
// - Eval at arbitrary frame environments
// - Multi-shot continuation (forking)
// - Hermetic replay

import { COWStore } from "../../src/core/eval/store";
import { stepOnce, type StepResult } from "../../src/core/eval/machineStep";
import { installPrims } from "./prims";
import { compileTextToExpr } from "../../src/core/pipeline/compileText";
import type { State, Frame } from "../../src/core/eval/machine";
import type { Val } from "../../src/core/eval/values";
import type { Env } from "../../src/core/eval/env";
import type { OpCall } from "../../src/core/effects/opcall";

/**
 * Frame view for stack inspection
 */
export interface FrameView {
  index: number;
  tag: string;
  env?: Env;
  expr?: { tag: string };
}

/**
 * Suspension at an effect boundary
 */
export interface Suspension {
  stateRef: string;
  state: State;
  opcall: OpCall;
  stepCount: number;
}

/**
 * Debug harness for programmatic control of Omega execution
 */
export class DebugHarness {
  private states = new Map<string, State>();
  private stateCounter = 0;

  /**
   * Store a state and return a reference
   */
  putState(state: State): string {
    const ref = `dbg:state:${++this.stateCounter}`;
    // Clone state to preserve it
    this.states.set(ref, this.cloneState(state));
    return ref;
  }

  /**
   * Retrieve a stored state
   */
  getState(ref: string): State | undefined {
    return this.states.get(ref);
  }

  /**
   * Clone a state (shallow - COWStore handles copy-on-write)
   */
  private cloneState(s: State): State {
    return {
      ...s,
      kont: [...s.kont],
      handlers: [...(s.handlers || [])],
      // COWStore is copy-on-write, so we can share it
    };
  }

  /**
   * Run code until it suspends at an effect (like infer.op)
   */
  runToSuspension(
    code: string,
    stopOnOps: string[] = ["infer.op"],
    maxSteps = 100_000
  ): { suspension?: Suspension; result?: Val; error?: string } {
    try {
      const store0 = new COWStore();
      const prim = installPrims(store0);
      const expr = compileTextToExpr(`(begin ${code})`);

      let state: State = {
        control: { tag: "Expr", e: expr },
        env: prim.env,
        store: prim.store,
        kont: [],
        handlers: [],
      };

      let stepCount = 0;
      while (stepCount < maxSteps) {
        const result = stepOnce(state);
        stepCount++;

        if (result.tag === "Done") {
          return { result: result.value };
        }

        if (result.tag === "Op") {
          if (stopOnOps.includes(result.opcall.op)) {
            const stateRef = this.putState(result.state);
            return {
              suspension: {
                stateRef,
                state: result.state,
                opcall: result.opcall,
                stepCount,
              },
            };
          }
          // For non-stop ops, we'd need effect handling
          return { error: `Unhandled effect: ${result.opcall.op}` };
        }

        state = result.state;
      }

      return { error: `Exceeded ${maxSteps} steps` };
    } catch (e: any) {
      return { error: e.message };
    }
  }

  /**
   * Get stack frames from a state
   */
  stack(state: State): FrameView[] {
    return state.kont.map((frame: any, i) => ({
      index: i,
      tag: frame.tag || "Frame",
      env: frame.env,
      expr: frame.e ? { tag: frame.e.tag } : undefined,
    }));
  }

  /**
   * Evaluate an expression in a specific environment
   */
  evalAt(env: Env, store: any, code: string, maxSteps = 10_000): Val | { error: string } {
    try {
      const expr = compileTextToExpr(code);
      let state: State = {
        control: { tag: "Expr", e: expr },
        env,
        store,
        kont: [],
        handlers: [],
      };

      let stepCount = 0;
      while (stepCount < maxSteps) {
        const result = stepOnce(state);
        stepCount++;

        if (result.tag === "Done") {
          return result.value;
        }

        if (result.tag === "Op") {
          return { error: `evalAt hit effect: ${result.opcall.op}` };
        }

        state = result.state;
      }

      return { error: `evalAt exceeded ${maxSteps} steps` };
    } catch (e: any) {
      return { error: e.message };
    }
  }

  /**
   * Resume a suspension with a return value
   */
  resume(
    suspension: Suspension,
    returnValue: Val,
    maxSteps = 100_000
  ): { result?: Val; suspension?: Suspension; error?: string } {
    try {
      // Create resumed state with the return value
      let state: State = {
        ...suspension.state,
        control: { tag: "Val", v: returnValue },
      };

      let stepCount = suspension.stepCount;
      while (stepCount < maxSteps) {
        const result = stepOnce(state);
        stepCount++;

        if (result.tag === "Done") {
          return { result: result.value };
        }

        if (result.tag === "Op") {
          const stateRef = this.putState(result.state);
          return {
            suspension: {
              stateRef,
              state: result.state,
              opcall: result.opcall,
              stepCount,
            },
          };
        }

        state = result.state;
      }

      return { error: `resume exceeded ${maxSteps} steps` };
    } catch (e: any) {
      return { error: e.message };
    }
  }

  /**
   * Fork a suspension: run it with multiple different return values
   * This demonstrates multi-shot continuations
   */
  fork(
    suspension: Suspension,
    values: Val[]
  ): Array<{ result?: Val; suspension?: Suspension; error?: string }> {
    return values.map((value) => {
      // Clone the state for each branch
      const clonedState = this.cloneState(suspension.state);
      const clonedSuspension: Suspension = {
        ...suspension,
        state: clonedState,
      };
      return this.resume(clonedSuspension, value);
    });
  }

  /**
   * Run to completion with full trace recording
   */
  trace(
    code: string,
    maxSteps = 100_000
  ): {
    trace: Array<{ step: number; controlTag: string; stackDepth: number }>;
    result?: Val;
    error?: string;
  } {
    const traceLog: Array<{ step: number; controlTag: string; stackDepth: number }> = [];

    try {
      const store0 = new COWStore();
      const prim = installPrims(store0);
      const expr = compileTextToExpr(`(begin ${code})`);

      let state: State = {
        control: { tag: "Expr", e: expr },
        env: prim.env,
        store: prim.store,
        kont: [],
        handlers: [],
      };

      traceLog.push({
        step: 0,
        controlTag: state.control.tag === "Val" ? "Val" : (state.control as any).e?.tag || "Expr",
        stackDepth: 0,
      });

      let stepCount = 0;
      while (stepCount < maxSteps) {
        const result = stepOnce(state);
        stepCount++;

        if (result.tag === "Done") {
          traceLog.push({
            step: stepCount,
            controlTag: "Done",
            stackDepth: 0,
          });
          return { trace: traceLog, result: result.value };
        }

        if (result.tag === "Op") {
          traceLog.push({
            step: stepCount,
            controlTag: `Op:${result.opcall.op}`,
            stackDepth: result.state.kont.length,
          });
          return { trace: traceLog, error: `Unhandled effect: ${result.opcall.op}` };
        }

        state = result.state;
        traceLog.push({
          step: stepCount,
          controlTag: state.control.tag === "Val" ? "Val" : (state.control as any).e?.tag || "Expr",
          stackDepth: state.kont.length,
        });
      }

      return { trace: traceLog, error: `Exceeded ${maxSteps} steps` };
    } catch (e: any) {
      return { trace: traceLog, error: e.message };
    }
  }
}

/**
 * Create a fresh debug harness
 */
export function createDebugHarness(): DebugHarness {
  return new DebugHarness();
}

/**
 * Hermetic replay: given a transcript of oracle responses, replay execution
 */
export interface ReplayTranscript {
  code: string;
  responses: Array<{ op: string; response: Val }>;
}

export function hermeticReplay(
  transcript: ReplayTranscript,
  maxSteps = 100_000
): { result?: Val; matched: number; error?: string } {
  const harness = createDebugHarness();
  let responseIdx = 0;

  // Run until first suspension
  let current = harness.runToSuspension(transcript.code, ["infer.op"], maxSteps);

  while (current.suspension) {
    // Check if we have a recorded response
    if (responseIdx >= transcript.responses.length) {
      return { matched: responseIdx, error: "Ran out of recorded responses" };
    }

    const expected = transcript.responses[responseIdx];
    if (current.suspension.opcall.op !== expected.op) {
      return {
        matched: responseIdx,
        error: `Op mismatch: expected ${expected.op}, got ${current.suspension.opcall.op}`,
      };
    }

    // Resume with the recorded response
    current = harness.resume(current.suspension, expected.response, maxSteps);
    responseIdx++;
  }

  if (current.result) {
    return { result: current.result, matched: responseIdx };
  }

  return { matched: responseIdx, error: current.error };
}
