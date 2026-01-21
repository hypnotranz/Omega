import type { FlowIR, FToolCall } from "../../frameir/flow";
import type { IRBundle } from "../../frameir/bundle";
import { errorDiag, type Diagnostic } from "../../outcome/diagnostic";
import type { PrimitiveRegistry } from "../../registry";
import type { Pass, PassResult } from "../types";
import type { ToolContractIR } from "../../frameir/contract";
import type { Span } from "../../frameir/meta";
import type { VRef } from "../../frameir/value";

export const toolContractPass: Pass = {
  id: "lint/tool-contract",
  name: "Tool Contract Check",
  phase: "lint",
  run(bundle: IRBundle, registry: PrimitiveRegistry): PassResult {
    const diagnostics: Diagnostic[] = [];
    const validatedContracts = new Set<string>();

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
        return;
      }

      if (!validatedContracts.has(contractId)) {
        validatedContracts.add(contractId);
        diagnostics.push(
          ...validateContractSchemas(bundle.toolContracts[contractId], bundle, node.meta?.span)
        );
      }
    });

    return { diagnostics };
  },
};

function validateContractSchemas(contract: ToolContractIR, bundle: IRBundle, span?: Span): Diagnostic[] {
  const diags: Diagnostic[] = [];
  const checks: Array<{ field: "inputSchema" | "outputSchema" | "errorSchema"; code: string; ref?: VRef }> = [
    { field: "inputSchema", code: "E0612", ref: contract.inputSchema },
    { field: "outputSchema", code: "E0613", ref: contract.outputSchema },
    { field: "errorSchema", code: "E0614", ref: contract.errorSchema },
  ];

  for (const check of checks) {
    if (!check.ref) {
      if (check.field === "errorSchema") continue;
      diags.push(
        errorDiag(check.code, `Tool contract ${contract.id} missing ${check.field} reference`, {
          span,
          data: { contractId: contract.id },
        })
      );
      continue;
    }

    const schemaId = check.ref.ref?.id;
    const kind = check.ref.ref?.kind;
    const exists = kind === "Schema" && schemaId && bundle.schemas[schemaId];
    if (!exists) {
      diags.push(
        errorDiag(check.code, `Tool contract ${contract.id} references missing schema: ${schemaId ?? "unknown"}`, {
          span,
          data: { contractId: contract.id, schemaId: schemaId ?? null, field: check.field },
        })
      );
    }
  }

  return diags;
}

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
