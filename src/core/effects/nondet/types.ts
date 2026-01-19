// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-3.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import type { State } from "../../eval/machine";
import type { Val } from "../../eval/values";

export type NondetMode = "first" | "all" | "best" | "sample";

export type ChoiceVec = Val[]; // amb.op passes choices as already-evaluated values (CBV). CBN can pass thunks.

export type ConstraintObs =
  | { tag: "Require"; predSyntaxHash: string; ok?: boolean }
  | { tag: "Note"; msg: string };

export type Job = {
  jid: string;
  state: State;
  depth: number;
  score: number;
  constraints: ConstraintObs[];
};

export type FrontierKind = "dfs" | "bfs" | "best" | "beam" | "sample";

export type NondetPolicy = {
  mode: NondetMode;
  frontier: FrontierKind;

  /** Fair scheduling quantum (steps per job). Required for BFS fairness under divergence. */
  quantumSteps: number;

  /** Beam width if frontier="beam". */
  beamWidth?: number;

  /** For best/sample: compute job score; can call inference (budgeted) */
  scoreChoice?: (ctx: { constraints: ConstraintObs[]; depth: number }, choice: Val) => Promise<number> | number;

  /** For pruning: return true to prune choice/job before exploring. */
  pruneChoice?: (ctx: { constraints: ConstraintObs[]; depth: number; score: number }, choice: Val) => Promise<boolean> | boolean;

  /** For best-solution aggregation: score complete results */
  scoreResult?: (v: Val) => Promise<number> | number;

  /** Optional RNG seed for sampling. */
  seed?: number;

  /** Step and wall budgets to bound explosion. */
  maxJobs?: number;
  maxTotalSteps?: number;
};

export type NondetResult =
  | { tag: "None" }
  | { tag: "One"; value: Val }
  | { tag: "Many"; values: Val[] };