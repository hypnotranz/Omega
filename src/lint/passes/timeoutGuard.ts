import type { FlowIR } from "../../frameir/flow";
import type { IRBundle } from "../../frameir/bundle";
import { warnDiag, type Diagnostic } from "../../outcome/diagnostic";
import type { PrimitiveRegistry } from "../../registry";
import { buildFlowGraph, computeDominators, isDominatedBy } from "../analysis/dominators";
import type { Pass, PassResult } from "../types";

export const timeoutGuardPass: Pass = {
  id: "lint/timeout-guard",
  name: "Timeout Guard Check",
  phase: "lint",
  run(bundle: IRBundle, registry: PrimitiveRegistry): PassResult {
    const diagnostics: Diagnostic[] = [];
    const graph = buildFlowGraph(bundle.entry);
    const dominators = computeDominators(graph);

    for (const node of graph.nodes.values()) {
      const descriptor = registry.getByIrTag(node.flow.tag);
      if (!requiresTimeoutGuard(descriptor)) continue;

      const dominated = isDominatedBy(node.id, "FWithTimeout", graph, dominators);
      if (!dominated) {
        diagnostics.push(
          warnDiag("W0010", timeoutMessage(node.flow), {
            span: node.flow.meta?.span,
            data: { tag: node.flow.tag },
          })
        );
      }
    }

    return { diagnostics };
  },
};

function requiresTimeoutGuard(descriptor: ReturnType<PrimitiveRegistry["getByIrTag"]>): boolean {
  return descriptor?.constraints?.mustBeDominatedByTimeout === true;
}

function timeoutMessage(flow: FlowIR): string {
  if (flow.tag === "FInfer") return "Oracle call is not guarded by with-timeout";
  return `${flow.tag} is not guarded by with-timeout`;
}
