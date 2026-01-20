import type { PromptIR, PromptDoc, PromptPart } from "./prompt";
import type { FlowIR, FSequence, FAll, FPure } from "./flow";
import type { VList } from "./value";
import { CURRENT_IR_VERSION } from "./version";

/**
 * Normalize prompt document:
 * - Flattens nested PromptDoc parts
 * - Removes empty text segments
 * - Recursively normalizes inner prompt containers
 */
export function normalizePrompt(prompt: PromptIR): PromptIR {
  const flattenPart = (part: PromptPart | PromptDoc): PromptPart[] => {
    if ((part as PromptDoc).tag === "PromptDoc") {
      return normalizePrompt(part as PromptDoc).parts;
    }

    switch (part.tag) {
      case "PXml":
      case "PTransform":
      case "PAttachSchema":
      case "PAttachFormat":
      case "PAttachTools": {
        const innerNormalized = normalizePrompt(part.inner);
        return [{ ...part, inner: innerNormalized } as PromptPart];
      }
      case "PNumbered": {
        return [{ ...part, items: part.items.map(normalizePrompt) } as PromptPart];
      }
      default:
        return [part as PromptPart];
    }
  };

  const flatParts: PromptPart[] = [];
  for (const part of prompt.parts) {
    flatParts.push(...flattenPart(part));
  }

  const nonEmpty = flatParts.filter(p => {
    if (p.tag === "PSystem" || p.tag === "PUser" || p.tag === "PAssistant") {
      return (p as { text: string }).text.trim().length > 0;
    }
    return true;
  });

  const merged: PromptPart[] = [];
  for (const part of nonEmpty) {
    const last = merged[merged.length - 1];
    if (
      last &&
      ("text" in last) &&
      ("text" in part) &&
      (last as any).tag === (part as any).tag
    ) {
      merged[merged.length - 1] = { ...(last as any), text: `${(last as any).text}${(last as any).text ? "\n" : ""}${(part as any).text}` };
    } else {
      merged.push(part);
    }
  }

  return {
    v: prompt.v ?? CURRENT_IR_VERSION,
    tag: "PromptDoc",
    parts: merged,
    meta: prompt.meta,
  };
}

/**
 * Normalize flow graph:
 * - Flattens nested FSequence nodes
 * - Collapses FAll([]) into FPure([]) for deterministic semantics
 */
export function normalizeFlow(flow: FlowIR): FlowIR {
  switch (flow.tag) {
    case "FSequence": {
      const normalizedChildren = flow.flows.map(normalizeFlow);
      const flat: FlowIR[] = [];
      for (const child of normalizedChildren) {
        if (child.tag === "FSequence") {
          flat.push(...(child as FSequence).flows);
        } else {
          flat.push(child);
        }
      }
      return { ...flow, flows: flat };
    }

    case "FAll": {
      if (flow.flows.length === 0) {
        return {
          v: flow.v ?? CURRENT_IR_VERSION,
          tag: "FPure",
          value: { v: flow.v ?? CURRENT_IR_VERSION, tag: "VList", items: [] } as VList,
        } as FPure;
      }
      return { ...flow, flows: flow.flows.map(normalizeFlow) };
    }

    case "FAny":
    case "FRace":
      return { ...flow, flows: flow.flows.map(normalizeFlow) };

    case "FBind":
    case "FCatch":
    case "FWithBudget":
    case "FWithTimeout":
      return { ...flow, flow: normalizeFlow(flow.flow) };

    case "FBranch":
      return { ...flow, then: normalizeFlow(flow.then), else: normalizeFlow(flow.else) };

    case "FCommit":
    case "FEmit":
    case "FFail":
    case "FInfer":
    case "FLoop":
    case "FObserve":
    case "FValidate":
    case "FToolCall":
    case "FPure":
    case "FSuspend":
      return flow;
  }
}
