import type { Diagnostic } from "../../outcome/diagnostic";
import type { FlowIR } from "../../frameir/flow";
import type { ValueIR } from "../../frameir/value";
import { normalizeFlow } from "../../frameir/normalize";
import { hashNode } from "../../frameir/hash";
import { CURRENT_IR_VERSION } from "../../frameir/version";
import type { NormalizeConfigFlow, NormalizeResultFlow, RewriteRecord } from "./types";

export function normalizeFlowIR(flow: FlowIR, config: NormalizeConfigFlow): NormalizeResultFlow {
  const diagnostics: Diagnostic[] = [];
  const rewrites: RewriteRecord[] = [];

  let current = config.flattenPrompts || config.flattenSequences ? normalizeFlow(flow) : flow;

  if (config.insertImplicitTimeouts) {
    const before = hashNode(current);
    current = {
      v: CURRENT_IR_VERSION,
      tag: "FWithTimeout",
      ms: timeoutValue(config),
      flow: current,
      meta: current.meta,
    } as any;
    rewrites.push({
      kind: "implicit-timeout",
      before,
      after: hashNode(current),
      location: current.meta?.span,
    });
  }

  if (config.insertImplicitBudgets) {
    const before = hashNode(current);
    current = {
      v: CURRENT_IR_VERSION,
      tag: "FWithBudget",
      budget: budgetValue(config),
      flow: current,
      meta: current.meta,
    } as any;
    rewrites.push({
      kind: "implicit-budget",
      before,
      after: hashNode(current),
      location: current.meta?.span,
    });
  }

  return {
    ok: diagnostics.length === 0,
    ir: current,
    rewrites,
    diagnostics,
  };
}

function budgetValue(config: NormalizeConfigFlow): ValueIR {
  const budget = config.defaultBudget ?? { llmCalls: 0, tokens: 0, timeMs: 0 };
  return {
    v: CURRENT_IR_VERSION,
    tag: "VRecord",
    entries: [
      { k: { v: CURRENT_IR_VERSION, tag: "VKeyword", name: "llmCalls" } as any, v: intVal(budget.llmCalls) },
      { k: { v: CURRENT_IR_VERSION, tag: "VKeyword", name: "tokens" } as any, v: intVal(budget.tokens) },
      { k: { v: CURRENT_IR_VERSION, tag: "VKeyword", name: "timeMs" } as any, v: intVal(budget.timeMs) },
    ],
  } as any;
}

function timeoutValue(config: NormalizeConfigFlow): ValueIR {
  const ms = config.defaultTimeoutMs ?? 0;
  return intVal(ms);
}

function intVal(n: number): ValueIR {
  return { v: CURRENT_IR_VERSION, tag: "VInt", value: String(n) } as any;
}
