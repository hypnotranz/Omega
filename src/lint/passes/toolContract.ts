import type { FlowIR, FToolCall } from "../../frameir/flow";
import type { IRBundle } from "../../frameir/bundle";
import { errorDiag, type Diagnostic } from "../../outcome/diagnostic";
import type { PrimitiveRegistry } from "../../registry";
import type { Pass, PassResult } from "../types";

export const toolContractPass: Pass = {
  id: "lint/tool-contract",
  name: "Tool Contract Check",
  phase: "lint",
  run(bundle: IRBundle, registry: PrimitiveRegistry): PassResult {
    const diagnostics: Diagnostic[] = [];

    traverse(bundle.entry, node => {
      const descriptor = registry.getByIrTag(node.tag);
      if (!requiresToolContract(descriptor) || node.tag !== "FToolCall") return;

      const toolNode = node as FToolCall;
      const contractRef = toolNode.contract;
      if (!contractRef) {
        diagnostics.push(
          errorDiag("E0610", "Tool call is missing a contract reference", {
            span: node.meta?.span,
            data: { tool: toolNode.tool },
          })
        );
        return;
      }

      const contractId = contractRef.ref?.id;
      if (!contractId || !bundle.toolContracts[contractId]) {
        diagnostics.push(
          errorDiag("E0611", `Tool contract not found: ${contractId ?? "unknown"}`, {
            span: node.meta?.span,
            data: { tool: toolNode.tool, contractId: contractId ?? null },
          })
        );
      }
    });

    return { diagnostics };
  },
};

function traverse(flow: FlowIR, visit: (node: FlowIR) => void): void {
  visit(flow);

  switch (flow.tag) {
    case "FBind":
    case "FCatch":
    case "FWithBudget":
    case "FWithTimeout":
      traverse(flow.flow, visit);
      break;
    case "FSequence":
    case "FAll":
    case "FAny":
    case "FRace":
      flow.flows.forEach(f => traverse(f, visit));
      break;
    case "FBranch":
      traverse(flow.then, visit);
      traverse(flow.else, visit);
      break;
    case "FPure":
    case "FFail":
    case "FLoop":
    case "FInfer":
    case "FToolCall":
    case "FValidate":
    case "FCommit":
    case "FEmit":
    case "FObserve":
    case "FSuspend":
      break;
  }
}

function requiresToolContract(descriptor: ReturnType<PrimitiveRegistry["getByIrTag"]>): boolean {
  return descriptor?.constraints?.requiresToolContract === true;
}
