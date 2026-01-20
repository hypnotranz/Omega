import type { NodeBase } from "./meta";
import type { ValueIR, VRef } from "./value";
import type { PromptIR } from "./prompt";

// Pure value
export interface FPure extends NodeBase { tag: "FPure"; value: ValueIR }

// Monadic bind (binder is Fn reference, closure-converted)
export interface FBind extends NodeBase {
  tag: "FBind";
  flow: FlowIR;
  k: VRef;
}

// Error handling
export interface FCatch extends NodeBase {
  tag: "FCatch";
  flow: FlowIR;
  handler: VRef;
}

export interface FFail extends NodeBase {
  tag: "FFail";
  reason: ValueIR;
  ctx?: ValueIR;
}

// Resource control
export interface FWithBudget extends NodeBase {
  tag: "FWithBudget";
  budget: ValueIR;
  flow: FlowIR;
}

export interface FWithTimeout extends NodeBase {
  tag: "FWithTimeout";
  ms: ValueIR;
  flow: FlowIR;
}

// Concurrency
export interface FAll extends NodeBase { tag: "FAll"; flows: FlowIR[] }
export interface FRace extends NodeBase { tag: "FRace"; flows: FlowIR[] }
export interface FAny extends NodeBase { tag: "FAny"; flows: FlowIR[] }
export interface FSequence extends NodeBase { tag: "FSequence"; flows: FlowIR[] }

// Control flow
export interface FBranch extends NodeBase {
  tag: "FBranch";
  pred: ValueIR;
  then: FlowIR;
  else: FlowIR;
}

export interface FLoop extends NodeBase {
  tag: "FLoop";
  init: ValueIR;
  step: VRef;
  until: VRef;
}

// Effects (PortEffects - must emit spans, be replay-loggable)
export interface FInfer extends NodeBase {
  tag: "FInfer";
  prompt: PromptIR;
  options?: ValueIR;
}

export interface FToolCall extends NodeBase {
  tag: "FToolCall";
  tool: ValueIR;
  args: ValueIR;
  contract?: VRef;
}

export interface FValidate extends NodeBase {
  tag: "FValidate";
  schema: VRef;
  value: ValueIR;
}

export interface FCommit extends NodeBase {
  tag: "FCommit";
  store: VRef;
  key: ValueIR;
  value: ValueIR;
}

export interface FEmit extends NodeBase {
  tag: "FEmit";
  sink: VRef;
  item: ValueIR;
}

export interface FObserve extends NodeBase {
  tag: "FObserve";
  source: VRef;
  query?: ValueIR;
}

export interface FSuspend extends NodeBase {
  tag: "FSuspend";
  reason: ValueIR;
}

// Union of all flow types
export type FlowIR =
  | FPure
  | FBind
  | FCatch
  | FFail
  | FWithBudget
  | FWithTimeout
  | FAll | FRace | FAny | FSequence
  | FBranch
  | FLoop
  | FInfer
  | FToolCall
  | FValidate
  | FCommit
  | FEmit
  | FObserve
  | FSuspend;
