import type { State } from "../eval/machine";
import type { Val } from "../eval/values";
import type { ApplyFn } from "./common";
import { evalProcedureToValue, isTruthy, makeResultVal } from "./common";
import type { ResultVal } from "./types";

export function repairUntilValid(
  initial: Val,
  validator: Val,
  repairFn: Val,
  maxIterations: number,
  state: State,
  apply: ApplyFn
): ResultVal {
  if (maxIterations < 0) {
    throw new Error("Max iterations must be non-negative");
  }

  let current = initial;
  let currentState = state;

  for (let i = 0; i < maxIterations; i++) {
    const { value: isValidVal, state: afterCheck } = evalProcedureToValue(validator, [current], currentState, apply);
    currentState = afterCheck;

    if (isTruthy(isValidVal)) {
      return makeResultVal("success", current, undefined, undefined, i);
    }

    const { value: repaired, state: afterRepair } = evalProcedureToValue(repairFn, [current], currentState, apply);
    currentState = afterRepair;

    if ((repaired as any)?.tag === "Result") {
      const res = repaired as ResultVal;
      if (res.kind === "failure") {
        return res;
      }
      current = res.solution ?? current;
      continue;
    }

    current = repaired;
  }

  return makeResultVal("partial", current, undefined, `Max iterations (${maxIterations}) reached`, maxIterations);
}
