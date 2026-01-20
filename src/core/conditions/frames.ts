import type { Env } from "../eval/env";
import type { Store } from "../eval/store";
import type { Frame, HandlerFrame } from "../eval/machine";
import type { ConditionHandler, RestartBinding, ConditionVal } from "./types";

export type KHandlerBindFrame = { tag: "KHandlerBind"; handlers: ConditionHandler[] };

export type KRestartBindFrame = {
  tag: "KRestartBind";
  restarts: RestartBinding[];
  savedKont: Frame[];
  env: Env;
  store: Store;
  handlers: HandlerFrame[];
};

export type KSignalingFrame = { tag: "KSignaling"; condition: ConditionVal; required: boolean };

export type ConditionFrame = KHandlerBindFrame | KRestartBindFrame | KSignalingFrame;
