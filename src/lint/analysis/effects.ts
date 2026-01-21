import type { FlowIR } from "../../frameir/flow";
import type { PrimitiveRegistry } from "../../registry";
import type { Effect } from "../../registry/types";

/**
 * Compute effects associated with a single node via registry lookup.
 */
export function nodeEffects(flow: FlowIR, registry: PrimitiveRegistry): Set<Effect> {
  const descriptor = registry.getByIrTag(flow.tag);
  const effects = new Set<Effect>();
  if (!descriptor) return effects;

  for (const effect of descriptor.effects) {
    effects.add(effect);
  }
  return effects;
}

export function requiresEffect(flow: FlowIR, effect: Effect, registry: PrimitiveRegistry): boolean {
  return nodeEffects(flow, registry).has(effect);
}

/**
 * Collect nodes that require a particular effect.
 */
export function collectByEffect(flow: FlowIR, effect: Effect, registry: PrimitiveRegistry): FlowIR[] {
  const nodes: FlowIR[] = [];

  const visit = (node: FlowIR): void => {
    if (requiresEffect(node, effect, registry)) {
      nodes.push(node);
    }

    switch (node.tag) {
      case "FBind":
      case "FCatch":
      case "FWithBudget":
      case "FWithTimeout":
        visit((node as any).flow);
        break;

      case "FSequence":
      case "FAll":
      case "FRace":
      case "FAny":
        for (const child of (node as any).flows) {
          visit(child);
        }
        break;

      case "FBranch":
        visit((node as any).then);
        visit((node as any).else);
        break;

      case "FLoop":
      case "FPure":
      case "FFail":
      case "FInfer":
      case "FToolCall":
      case "FValidate":
      case "FCommit":
      case "FEmit":
      case "FObserve":
      case "FSuspend":
        break;
    }
  };

  visit(flow);
  return nodes;
}
