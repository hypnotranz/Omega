import type { Failure } from "./failure";
import type { Span } from "../frameir/meta";

export interface OutcomeMeta {
  span?: Span;
  durationMs?: number;
  budgetUsed?: {
    llmCalls?: number;
    tokens?: number;
    timeMs?: number;
    toolCalls?: number;
  };
  evidenceIds?: string[];
}

export interface Done<A> {
  readonly tag: "Done";
  readonly value: A;
  readonly meta: OutcomeMeta;
}

export interface Fail {
  readonly tag: "Fail";
  readonly failure: Failure;
  readonly meta: OutcomeMeta;
}

export interface Suspended<A> {
  reason: SuspendReason;
  resume: (input: unknown) => Promise<Outcome<A>>;
  state?: unknown;
}

export type SuspendReason =
  | { tag: "AwaitingHumanInput"; prompt: string }
  | { tag: "AwaitingApproval"; action: string }
  | { tag: "ResourceExhausted"; resource: string }
  | { tag: "Custom"; reason: string; data?: unknown };

export interface Pause<A> {
  readonly tag: "Pause";
  readonly suspended: Suspended<A>;
  readonly meta: OutcomeMeta;
}

export type Outcome<A> = Done<A> | Fail | Pause<A>;
export type Ok<A> = Done<A>;
export type Err = Fail;

export function isDone<A>(o: Outcome<A>): o is Done<A> {
  return o.tag === "Done";
}

export function isFail<A>(o: Outcome<A>): o is Fail {
  return o.tag === "Fail";
}

export function isPause<A>(o: Outcome<A>): o is Pause<A> {
  return o.tag === "Pause";
}
