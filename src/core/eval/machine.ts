// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-4.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.
// Prompt 9: Add runtime governance (profile, budget, security)

import type { Expr, HandlerExpr, Pattern } from "../ast";
import type { Env } from "./env";
import type { Store } from "./store";
import type { Val } from "./values";
import type { OpCall } from "../effects/opcall";
import type { Profile, RuntimeBudget, RuntimeSecurity } from "../governance/profile";

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
  | { tag: "KCall"; savedEnv: Env }                                  // restore env after closure body
  | { tag: "KEffect"; op: string; pending: Expr[]; acc: Val[]; env: Env }
  | { tag: "KHandleBoundary"; hid: string; savedHandlersDepth: number; resumeTo?: { kont: Frame[]; handlersDepth: number } }
  | { tag: "KHandleReturn"; mode: "exit" | "resume"; hid: string; targetKont: Frame[]; targetHandlersDepth: number; savedHandlersDepth: number }
  | { tag: "KMatch"; clauses: Array<{ pat: Pattern; body: Expr }>; env: Env }
  | { tag: "KOracleLambda"; params: string[]; env: Env };  // oracle-lambda: waiting for spec to evaluate

export type HandlerFrame = {
  hid: string;
  env: Env;
  on: Map<string, { op: string; params: string[]; k: string; body: Expr }>;
  ret?: { v: string; body: Expr };
  fin?: { body: Expr };
};

export type State = {
  control: Control;
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
};

export type StepOutcome =
  | { tag: "State"; state: State }
  | { tag: "Done"; value: Val; state: State }
  | { tag: "Op"; opcall: OpCall; state: State };