import type { FlowIR } from "../../frameir/flow";
import type { IRBundle } from "../../frameir/bundle";
import { errorDiag, type Diagnostic } from "../../outcome/diagnostic";
import type { PrimitiveRegistry } from "../../registry";
import { buildFlowGraph, computeDominators, isDominatedBy } from "../analysis/dominators";
import type { Pass, PassResult } from "../types";

export const budgetDominatorPass: Pass = {
  id: "lint/budget-dominator",
  name: "Budget Dominator Check",
  phase: "lint",
  run(bundle: IRBundle, registry: PrimitiveRegistry): PassResult {
    const diagnostics: Diagnostic[] = [];

    const graph = buildFlowGraph(bundle.entry);
    const dominators = computeDominators(graph);

    for (const node of graph.nodes.values()) {
      const descriptor = registry.getByIrTag(node.flow.tag);
      if (!requiresBudgetGuard(descriptor)) continue;

      if (!isDominatedBy(node.id, "FWithBudget", graph, dominators)) {
        diagnostics.push(
          errorDiag(selectBudgetCode(descriptor?.effects ?? [], node.flow), budgetMessage(node.flow), {
            span: node.flow.meta?.span,
            data: { tag: node.flow.tag },
          })
        );
      }
    }

    return { diagnostics };
  },
};

function requiresBudgetGuard(descriptor: ReturnType<PrimitiveRegistry["getByIrTag"]>): boolean {
  return descriptor?.constraints?.mustBeDominatedByBudget === true;
}

function selectBudgetCode(effects: string[], flow: FlowIR): string {
  if (effects.includes("Tool")) return "E0601";
  if (effects.includes("Oracle")) return "E0600";
  return `E06-${flow.tag}`;
}

function budgetMessage(flow: FlowIR): string {
  if (flow.tag === "FToolCall") return "Tool call is not dominated by with-budget";
  if (flow.tag === "FInfer") return "Oracle call is not dominated by with-budget";
  return `${flow.tag} requires a budget guard`;
}
