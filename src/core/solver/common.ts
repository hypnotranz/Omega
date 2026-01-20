import { stepOnce } from "../eval/machineStep";
import type { State, StepOutcome } from "../eval/machine";
import type { Val } from "../eval/values";
import type { ResultVal } from "./types";

export type ApplyFn = (proc: Val, args: Val[], state: State) => State | StepOutcome;

export function asNumber(v: Val, name = "number"): number {
  if (v.tag === "Num") return v.n;
  throw new Error(`expected ${name}`);
}

export function asString(v: Val, name = "string"): string {
  if (v.tag === "Str") return v.s;
  if (v.tag === "Sym") return v.name;
  throw new Error(`expected ${name}`);
}

export function isTruthy(v: Val): boolean {
  if (v.tag === "Bool") return v.b;
  if (v.tag === "Unit") return false;
  return true;
}

/**
 * Evaluate a procedure to a value, stopping on the first effect.
 * Effects are not handled here to keep solver helpers deterministic;
 * callers should avoid effectful solver functions.
 */
export function evalProcedureToValue(
  proc: Val,
  args: Val[],
  state: State,
  apply: ApplyFn
): { value: Val; state: State } {
  const first = apply(proc, args, state);
  let currentState: State;

  if (!first) {
    // Defensive: treat missing return as no state change.
    currentState = state;
  } else if ((first as StepOutcome).tag === "Op") {
    throw new Error("solver evaluation emitted an effect; unsupported in solver helpers");
  } else if ((first as StepOutcome).tag === "Done") {
    const done = first as Extract<StepOutcome, { tag: "Done" }>;
    return { value: done.value, state: done.state ?? state };
  } else if ((first as StepOutcome).tag === "State") {
    currentState = (first as Extract<StepOutcome, { tag: "State" }>).state;
  } else {
    currentState = first as State;
  }

  while (true) {
    const step = stepOnce(currentState);
    if (step.tag === "State") {
      currentState = step.state;
      continue;
    }
    if (step.tag === "Done") {
      return { value: step.value, state: step.state ?? currentState };
    }
    if (step.tag === "Op") {
      throw new Error("solver evaluation emitted an effect; unsupported in solver helpers");
    }
  }
}

export function makeResultVal(
  kind: "success" | "partial" | "failure" | string,
  solution?: Val,
  remaining?: Val,
  reason?: string,
  cost = 0
): ResultVal {
  return {
    tag: "Result",
    kind,
    solution,
    remaining,
    reason,
    cost,
  };
}
