export type SessionEvent =
  | SessionHeaderEvent
  | InputEvent
  | StepEvent
  | CheckpointEvent
  | LLMRequestEvent
  | LLMResponseEvent
  | EffectEvent
  | ResumeEvent
  | ResultEvent
  | ErrorEvent;

export type SessionHeaderEvent = {
  type: "session";
  version: 1;
  id: string;
  created: string;
  profile?: string;
};

export type InputEvent = {
  seq: number;
  ts: number;
  type: "input";
  code: string;
};

export type StepEvent = {
  seq: number;
  ts: number;
  type: "step";
  d: number;
  ctrl: string;
};

export type CheckpointEvent = {
  seq: number;
  ts: number;
  type: "checkpoint";
  d: number;
  reason: "llm_boundary" | "periodic" | "manual" | "effect";
  stateId: string;
};

export type LLMRequestEvent = {
  seq: number;
  ts: number;
  type: "llm_req";
  d: number;
  model: string;
  promptPreview: string;
  receiptKey: string;
};

export type LLMResponseEvent = {
  seq: number;
  ts: number;
  type: "llm_resp";
  d: number;
  valuePreview: string;
  tokens?: number;
  durationMs: number;
  receiptKey: string;
};

export type EffectEvent = {
  seq: number;
  ts: number;
  type: "effect";
  d: number;
  op: string;
  argsPreview: string;
};

export type ResumeEvent = {
  seq: number;
  ts: number;
  type: "resume";
  d: number;
  valuePreview: string;
};

export type ResultEvent = {
  seq: number;
  ts: number;
  type: "result";
  value: string;
};

export type ErrorEvent = {
  seq: number;
  ts: number;
  type: "error";
  d: number;
  message: string;
  stack?: string;
};

export type SessionIndex = {
  sessionId: string;
  eventCount: number;
  checkpoints: CheckpointIndex[];
  states: Record<string, any>;
  receipts: Record<string, LLMReceipt>;
};

export type CheckpointIndex = {
  seq: number;
  byteOffset: number;
  stateId: string;
  reason: string;
};

export type LLMReceipt = {
  key: string;
  request: any;
  response: any;
  timestamp: number;
  durationMs: number;
  tokens?: number;
};
