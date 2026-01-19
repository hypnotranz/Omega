// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-4.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import type { State } from "./machine";
import type { Runtime } from "./runtime";
import type { Val } from "./values";
import { stepOnce } from "./machineStep";
import type { BudgetTracker } from "../governance/budgets";

export type RunOptions = {
  maxSteps?: number;
  budget?: BudgetTracker;
};

export async function runToCompletion(
  runtime: Runtime,
  initial: State,
  optionsOrMaxSteps: RunOptions | number = {}
): Promise<Val> {
  // Backward compatibility: accept number as maxSteps
  const options: RunOptions = typeof optionsOrMaxSteps === "number"
    ? { maxSteps: optionsOrMaxSteps }
    : optionsOrMaxSteps;

  const maxSteps = options.maxSteps ?? 1_000_000;
  const budget = options.budget;

  let st = initial;
  for (let i = 0; i < maxSteps; i++) {
    // Consume one eval step if budget tracking is enabled
    budget?.consumeEvalStep();

    const out = stepOnce(st);

    if (out.tag === "State") {
      st = out.state;
      continue;
    }
    if (out.tag === "Done") {
      return out.value;
    }
    if (out.tag === "Op") {
      const handled = await runtime.dispatch(out.state, out.opcall);
      if (handled === "Uncaught") {
        throw new Error(`Uncaught op: ${out.opcall.op}`);
      }
      st = handled;
      continue;
    }
  }
  throw new Error("runToCompletion: step budget exceeded");
}

/**
 * Like runToCompletion but returns both value AND final state.
 * Required by PortalImpl for oracle REPL re-entry.
 */
export async function runToCompletionWithState(
  runtime: Runtime,
  st0: State,
  optionsOrMaxSteps: RunOptions | number = {}
): Promise<{ value: Val; state: State }> {
  // Backward compatibility: accept number as maxSteps
  const options: RunOptions = typeof optionsOrMaxSteps === "number"
    ? { maxSteps: optionsOrMaxSteps }
    : optionsOrMaxSteps;

  const maxSteps = options.maxSteps ?? 1_000_000;
  const budget = options.budget;

  let st = st0;

  for (let i = 0; i < maxSteps; i++) {
    // Consume one eval step if budget tracking is enabled
    budget?.consumeEvalStep();

    const out = stepOnce(st);

    if (out.tag === "State") {
      st = out.state;
      continue;
    }

    if (out.tag === "Done") {
      return { value: out.value, state: (out.state ?? st) as any };
    }

    if (out.tag === "Op") {
      const handled = await runtime.dispatch(out.state, out.opcall);
      if (handled === "Uncaught") throw new Error(`Uncaught op: ${out.opcall.op}`);
      st = handled;
      continue;
    }

    throw new Error(`unknown stepOnce output: ${(out as any).tag}`);
  }

  throw new Error(`runToCompletionWithState exceeded maxSteps=${maxSteps}`);
}

// ============================================================================
// TRACING INFRASTRUCTURE
// ============================================================================

export type TraceEntry = {
  step: number;
  controlTag: string;
  controlDetail: string;
  stackDepth: number;
  stackTags: string[];
  outcome: "state" | "done" | "op";
  opName?: string;
};

export type TraceOptions = RunOptions & {
  trace?: boolean;
  onStep?: (entry: TraceEntry) => void;
};

function controlToString(control: State["control"]): { tag: string; detail: string } {
  if (control.tag === "Val") {
    const v = control.v;
    if (v.tag === "Num") return { tag: "Val", detail: `Num(${v.n})` };
    if (v.tag === "Str") return { tag: "Val", detail: `Str("${v.s.slice(0, 20)}${v.s.length > 20 ? "..." : ""}")` };
    if (v.tag === "Bool") return { tag: "Val", detail: `Bool(${v.b})` };
    if (v.tag === "Unit") return { tag: "Val", detail: "Unit" };
    if (v.tag === "Closure") return { tag: "Val", detail: `Closure(${(v as any).params?.join(",") || "?"})` };
    if (v.tag === "Native") return { tag: "Val", detail: "Native" };
    return { tag: "Val", detail: v.tag };
  }
  if (control.tag === "Expr") {
    const e = control.e;
    if (e.tag === "Lit") {
      if (typeof e.value === "number") return { tag: "Expr", detail: `Lit(${e.value})` };
      if (typeof e.value === "string") return { tag: "Expr", detail: `Lit("${e.value.slice(0, 15)}")` };
      return { tag: "Expr", detail: `Lit(${e.value})` };
    }
    if (e.tag === "Var") return { tag: "Expr", detail: `Var(${e.name})` };
    if (e.tag === "App") return { tag: "Expr", detail: `App(${(e as any).fn?.tag || "?"})` };
    if (e.tag === "If") return { tag: "Expr", detail: "If" };
    if (e.tag === "Lambda") return { tag: "Expr", detail: `Lambda(${(e as any).params?.length || 0} params)` };
    if (e.tag === "Begin") return { tag: "Expr", detail: `Begin(${(e as any).exprs?.length || 0} exprs)` };
    if (e.tag === "Define") return { tag: "Expr", detail: `Define(${(e as any).name || "?"})` };
    if (e.tag === "Effect") return { tag: "Expr", detail: `Effect(${(e as any).op || "?"})` };
    return { tag: "Expr", detail: e.tag };
  }
  return { tag: control.tag, detail: "?" };
}

/**
 * Run to completion with full execution tracing.
 * Returns value, final state, and trace log.
 */
export async function runWithTrace(
  runtime: Runtime,
  initial: State,
  optionsOrMaxSteps: TraceOptions | number = {}
): Promise<{ value: Val; state: State; trace: TraceEntry[] }> {
  const options: TraceOptions = typeof optionsOrMaxSteps === "number"
    ? { maxSteps: optionsOrMaxSteps, trace: true }
    : { trace: true, ...optionsOrMaxSteps };

  const maxSteps = options.maxSteps ?? 1_000_000;
  const budget = options.budget;
  const trace: TraceEntry[] = [];

  let st = initial;

  for (let i = 0; i < maxSteps; i++) {
    budget?.consumeEvalStep();

    const { tag, detail } = controlToString(st.control);
    const stackTags = st.kont.map(fr => (fr as any).tag || "?");

    const out = stepOnce(st);

    const entry: TraceEntry = {
      step: i,
      controlTag: tag,
      controlDetail: detail,
      stackDepth: st.kont.length,
      stackTags,
      outcome: out.tag === "State" ? "state" : out.tag === "Done" ? "done" : "op",
      opName: out.tag === "Op" ? out.opcall.op : undefined,
    };

    trace.push(entry);
    options.onStep?.(entry);

    if (out.tag === "State") {
      st = out.state;
      continue;
    }

    if (out.tag === "Done") {
      return { value: out.value, state: (out.state ?? st) as any, trace };
    }

    if (out.tag === "Op") {
      const handled = await runtime.dispatch(out.state, out.opcall);
      if (handled === "Uncaught") throw new Error(`Uncaught op: ${out.opcall.op}`);
      st = handled;
      continue;
    }

    throw new Error(`unknown stepOnce output: ${(out as any).tag}`);
  }

  throw new Error(`runWithTrace exceeded maxSteps=${maxSteps}`);
}

/**
 * Format trace for human-readable output
 */
export function formatTrace(trace: TraceEntry[], options?: { compact?: boolean }): string {
  if (options?.compact) {
    return trace.map(e =>
      `[${e.step}] ${e.controlTag}:${e.controlDetail} | stack=${e.stackDepth}${e.opName ? ` | OP=${e.opName}` : ""}`
    ).join("\n");
  }

  const lines: string[] = [];
  for (const e of trace) {
    lines.push(`Step ${e.step}:`);
    lines.push(`  Control: ${e.controlTag} - ${e.controlDetail}`);
    lines.push(`  Stack depth: ${e.stackDepth}`);
    if (e.stackTags.length > 0) {
      lines.push(`  Stack frames: [${e.stackTags.join(" <- ")}]`);
    }
    if (e.opName) {
      lines.push(`  >>> EFFECT: ${e.opName}`);
    }
    lines.push("");
  }
  return lines.join("\n");
}