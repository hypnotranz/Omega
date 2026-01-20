import type {
  FlowIR,
  FPure,
  FBind,
  FCatch,
  FFail,
  FWithBudget,
  FWithTimeout,
  FAll,
  FRace,
  FAny,
  FSequence,
  FBranch,
  FLoop,
  FInfer,
  FToolCall,
  FValidate,
  FCommit,
  FEmit,
  FObserve,
  FSuspend,
} from "./flow";

export interface FlowVisitor<R> {
  visitFPure?(node: FPure): R;
  visitFBind?(node: FBind): R;
  visitFCatch?(node: FCatch): R;
  visitFFail?(node: FFail): R;
  visitFWithBudget?(node: FWithBudget): R;
  visitFWithTimeout?(node: FWithTimeout): R;
  visitFAll?(node: FAll): R;
  visitFRace?(node: FRace): R;
  visitFAny?(node: FAny): R;
  visitFSequence?(node: FSequence): R;
  visitFBranch?(node: FBranch): R;
  visitFLoop?(node: FLoop): R;
  visitFInfer?(node: FInfer): R;
  visitFToolCall?(node: FToolCall): R;
  visitFValidate?(node: FValidate): R;
  visitFCommit?(node: FCommit): R;
  visitFEmit?(node: FEmit): R;
  visitFObserve?(node: FObserve): R;
  visitFSuspend?(node: FSuspend): R;
  default(node: FlowIR): R;
}

export function visitFlow<R>(flow: FlowIR, visitor: FlowVisitor<R>): R {
  const method = visitor[`visit${flow.tag}` as keyof FlowVisitor<R>];
  if (typeof method === "function") {
    return (method as (n: FlowIR) => R)(flow);
  }
  return visitor.default(flow);
}

/**
 * Recursively rewrite a flow graph by applying a transform to each node.
 * If transform returns a non-null node, it replaces the current node without descending.
 */
export function rewriteFlow(
  flow: FlowIR,
  transform: (node: FlowIR) => FlowIR | null
): FlowIR {
  const transformed = transform(flow);
  if (transformed !== null) {
    return transformed;
  }

  switch (flow.tag) {
    case "FBind":
    case "FCatch":
    case "FWithBudget":
    case "FWithTimeout": {
      const rewritten = rewriteFlow(flow.flow, transform);
      return rewritten === flow.flow ? flow : { ...flow, flow: rewritten };
    }

    case "FAll":
    case "FRace":
    case "FAny":
    case "FSequence": {
      const rewritten = flow.flows.map(f => rewriteFlow(f, transform));
      const changed = rewritten.some((f, i) => f !== flow.flows[i]);
      return changed ? { ...flow, flows: rewritten } : flow;
    }

    case "FBranch": {
      const thenFlow = rewriteFlow(flow.then, transform);
      const elseFlow = rewriteFlow(flow.else, transform);
      if (thenFlow === flow.then && elseFlow === flow.else) {
        return flow;
      }
      return { ...flow, then: thenFlow, else: elseFlow };
    }

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
      return flow;
  }
}
