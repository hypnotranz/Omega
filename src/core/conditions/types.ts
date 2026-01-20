import type { Env } from "../eval/env";
import type { Store } from "../eval/store";
import type { Val } from "../eval/values";
import type { Frame, HandlerFrame } from "../eval/machine";

// Callable procedures used by handlers and restarts
export type CallableVal = Val;

export type ConditionVal = {
  tag: "Condition";
  type: symbol;
  message: string;
  data: Val;
  restarts: RestartPoint[];
};

export type RestartPoint = {
  name: symbol;
  description?: string;
  kont: Frame[];
  env: Env;
  store: Store;
  handlers: HandlerFrame[];
};

export type ConditionHandler = {
  type: symbol | "*";
  handler: CallableVal;
};

export type RestartBinding = {
  name: symbol;
  fn: CallableVal;
  description?: string;
};
