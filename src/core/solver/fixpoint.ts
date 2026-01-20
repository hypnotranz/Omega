import type { State } from "../eval/machine";
import type { Val } from "../eval/values";
import type { ApplyFn } from "./common";
import { asString, evalProcedureToValue, isTruthy, makeResultVal } from "./common";

export function fixpoint(
  initial: Val,
  stepFn: Val,
  equalFn: Val,
  maxIterations: number,
  state: State,
  apply: ApplyFn
) {
  if (maxIterations < 0) {
    throw new Error("Max iterations must be non-negative");
  }

  let current = initial;
  let currentState = state;

  for (let i = 0; i < maxIterations; i++) {
    const { value: next, state: afterStep } = evalProcedureToValue(stepFn, [current], currentState, apply);
    currentState = afterStep;
    const { value: isEqual, state: afterEq } = evalProcedureToValue(equalFn, [current, next], currentState, apply);
    currentState = afterEq;

    if (isTruthy(isEqual)) {
      return makeResultVal("success", next, undefined, undefined, i + 1);
    }

    current = next;
  }

  return makeResultVal("partial", current, undefined, `Did not converge in ${maxIterations} iterations`, maxIterations);
}

export function fixpointWithCycleDetection(
  initial: Val,
  stepFn: Val,
  hashFn: Val,
  maxIterations: number,
  state: State,
  apply: ApplyFn
) {
  if (maxIterations < 0) {
    throw new Error("Max iterations must be non-negative");
  }

  let current = initial;
  let currentState = state;
  const seen = new Set<string>();

  for (let i = 0; i < maxIterations; i++) {
    const { value: hashVal, state: afterHash } = evalProcedureToValue(hashFn, [current], currentState, apply);
    currentState = afterHash;
    const hash = asString(hashVal, "hash");

    if (seen.has(hash)) {
      return makeResultVal("failure", undefined, undefined, `Cycle detected at iteration ${i}`, i);
    }
    seen.add(hash);

    const { value: next, state: afterStep } = evalProcedureToValue(stepFn, [current], currentState, apply);
    currentState = afterStep;

    const { value: nextHashVal, state: afterNextHash } = evalProcedureToValue(hashFn, [next], currentState, apply);
    currentState = afterNextHash;
    const nextHash = asString(nextHashVal, "hash");

    if (nextHash === hash) {
      return makeResultVal("success", next, undefined, undefined, i + 1);
    }

    current = next;
  }

  return makeResultVal("partial", current, undefined, `Max iterations (${maxIterations}) reached without convergence or cycle`, maxIterations);
}
