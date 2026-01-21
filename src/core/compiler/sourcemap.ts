import type { FlowIR } from "../../frameir/flow";
import type { FnDefIR } from "../../frameir/bundle";
import type { SourceMap } from "./types";
import { hashNode } from "../../frameir/hash";
import type { Span } from "../../frameir/meta";

export function buildSourceMap(entry: FlowIR, fns: FnDefIR[]): SourceMap {
  const irToSource = new Map<string, Span>();

  const record = (node: { meta?: { span?: Span } } | null | undefined): void => {
    if (!node || !node.meta?.span) return;
    irToSource.set(hashNode(node as any), node.meta.span);
  };

  const visitFlow = (flow: FlowIR): void => {
    record(flow);
    switch (flow.tag) {
      case "FPure":
        record(flow.value as any);
        break;
      case "FBind":
        visitFlow(flow.flow);
        break;
      case "FCatch":
        visitFlow(flow.flow);
        break;
      case "FWithBudget":
      case "FWithTimeout":
        visitFlow(flow.flow);
        break;
      case "FAll":
      case "FAny":
      case "FRace":
      case "FSequence":
        flow.flows.forEach(visitFlow);
        break;
      case "FBranch":
        visitFlow(flow.then);
        visitFlow(flow.else);
        break;
      case "FInfer":
      case "FToolCall":
      case "FValidate":
      case "FCommit":
      case "FEmit":
      case "FObserve":
      case "FSuspend":
        // No child flows to traverse
        break;
      case "FLoop":
        // Treat loop step/until as values
        break;
      default:
        break;
    }
  };

  visitFlow(entry);
  for (const fn of fns) {
    record(fn);
    if ((fn.body as any)?.tag?.startsWith("F")) {
      visitFlow(fn.body as FlowIR);
    }
  }

  return {
    version: 3,
    file: "",
    sourceRoot: "",
    sources: [],
    names: [],
    mappings: "",
    entries: [],
    irToSource,
  };
}
