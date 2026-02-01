// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-4.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.
// Prompt 9: Add runtime governance (profile, budget, security)

import type { Expr, HandlerExpr, Pattern } from "../ast";
import type { Env } from "./env";
import type { Store } from "./store";
import type { Val } from "./values";
import type { OpCall } from "../effects/opcall";
import type { Profile, RuntimeBudget, RuntimeSecurity } from "../governance/profile";
import type { ConditionHandler, RestartBinding, ConditionVal } from "../conditions/types";
import type { ProvenanceGraph, SourceChecker } from "../provenance/graph";
import type { ProvenanceStore } from "../provenance/store/interface";

export type Control =
  | { tag: "Expr"; e: Expr }
  | { tag: "Val"; v: Val };

export type Frame =
  | { tag: "KIf"; conseq: Expr; alt: Expr; env: Env }
  | { tag: "KBegin"; rest: Expr[]; env: Env }
  | { tag: "KDefine"; name: string; env: Env }
  | { tag: "KSet"; name: string; env: Env }
  | { tag: "KAppFun"; args: Expr[]; env: Env }
  | { tag: "KAppArg"; fnVal: Val; pending: Expr[]; acc: Val[]; env: Env }
  | { tag: "KAppArgLazy"; fnVal: Val; pending: Array<{ expr: Expr; idx: number }>; acc: Array<{ idx: number; val: Val }>; env: Env; totalArgs: number; currentIdx: number }
  | { tag: "KCall"; savedEnv: Env }                                  // restore env after closure body
  | { tag: "KEffect"; op: string; pending: Expr[]; acc: Val[]; env: Env }
  | { tag: "KHandleBoundary"; hid: string; savedHandlersDepth: number; resumeTo?: { kont: Frame[]; handlersDepth: number } }
  | { tag: "KHandleReturn"; mode: "exit" | "resume"; hid: string; targetKont: Frame[]; targetHandlersDepth: number; savedHandlersDepth: number }
  | { tag: "KPrompt"; promptTag: Val; handler: Val; env: Env; savedKont: Frame[]; savedHandlersDepth: number }
  | { tag: "KMatch"; clauses: Array<{ pat: Pattern; body: Expr }>; env: Env }
  | { tag: "KOracleLambda"; params: string[]; env: Env }  // oracle-lambda: waiting for spec to evaluate
  | { tag: "KBind"; fn: Val; env: Env }
  | { tag: "KHandlerBind"; handlers: ConditionHandler[] }
  | { tag: "KRestartBind"; restarts: RestartBinding[]; savedKont: Frame[]; env: Env; store: Store; handlers: HandlerFrame[] }
  | { tag: "KSignaling"; condition: ConditionVal; required: boolean }
  // Higher-order function continuations (map, filter, fold with closures)
  | { tag: "KMapRest"; fn: Val; remaining: Val[]; acc: Val[]; env: Env }
  | { tag: "KFilterRest"; fn: Val; remaining: Val[]; currentItem: Val; acc: Val[]; env: Env }
  | { tag: "KFoldRest"; fn: Val; remaining: Val[]; env: Env }
  // Stream continuations for closure support
  | { tag: "KStreamMapHead"; fn: Val; streamTail: Val; env: Env }
  | { tag: "KStreamToListRest"; remaining: number; acc: Val[]; env: Env; pendingPromise?: Val };

export type HandlerFrame = {
  hid: string;
  env: Env;
  on: Map<string, { op: string; params: string[]; k: string; body: Expr }>;
  ret?: { v: string; body: Expr };
  fin?: { body: Expr };
};

export type State = {
  control: Control;
  /** Legacy alias used in a few tests */
  ctrl?: Control;
  env: Env;
  store: Store;
  kont: Frame[];           // bottom->top, push/pop at end
  handlers: HandlerFrame[]; // bottom->top, push/pop at end

  // ─────────────────────────────────────────────────────────────────
  // Prompt 9: Runtime Governance
  // ─────────────────────────────────────────────────────────────────

  /** Active profile - determines what effects/ops/requests are allowed */
  profile?: Profile;

  /** Mutable budget counters - decremented on each operation */
  budget?: RuntimeBudget;

  /** Current security context - intersection of profile caps and context caps */
  sec?: RuntimeSecurity;

  // Provenance graph + receipt store (optional)
  provenanceGraph?: ProvenanceGraph;
  provenanceStore?: ProvenanceStore;
  provenanceSourceChecker?: SourceChecker;
};

export type StepOutcome =
  | { tag: "State"; state: State; value?: undefined; opcall?: undefined }
  | { tag: "Done"; value: Val; state: State; opcall?: undefined }
  | { tag: "Op"; opcall: OpCall; state: State; value?: Val };
