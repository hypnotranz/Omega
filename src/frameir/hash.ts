import { createHash } from "crypto";
import type { NodeBase } from "./meta";
import { encodeCanonical } from "./codec";
import type { FlowIR } from "./flow";

export type NodeHash = `ir:sha256:${string}`;

/**
 * Compute a semantic hash for an IR node. Meta is excluded by default.
 */
export function hashNode(node: NodeBase, semanticSalt?: string): NodeHash {
  const canonical = encodeCanonical(node, { includeMeta: false });
  const input = semanticSalt ? `${canonical}:${semanticSalt}` : canonical;
  const digest = createHash("sha256").update(input).digest("hex").slice(0, 32);
  return `ir:sha256:${digest}`;
}

/**
 * Compute a merkle hash for a flow graph. Each node hash includes child hashes.
 */
export function merkleize(flow: FlowIR): Map<FlowIR, NodeHash> {
  const hashes = new Map<FlowIR, NodeHash>();

  const visit = (node: FlowIR): NodeHash => {
    if (hashes.has(node)) return hashes.get(node)!;
    const nodeWithChildHashes = replaceChildFlowsWithHashes(node, visit);
    const hash = hashNode(nodeWithChildHashes);
    hashes.set(node, hash);
    return hash;
  };

  visit(flow);
  return hashes;
}

function replaceChildFlowsWithHashes(node: FlowIR, getHash: (f: FlowIR) => NodeHash): NodeBase {
  switch (node.tag) {
    case "FBind":
      return { ...node, flow: getHash(node.flow) } as unknown as NodeBase;

    case "FCatch":
      return { ...node, flow: getHash(node.flow) } as unknown as NodeBase;

    case "FWithBudget":
      return { ...node, flow: getHash(node.flow) } as unknown as NodeBase;

    case "FWithTimeout":
      return { ...node, flow: getHash(node.flow) } as unknown as NodeBase;

    case "FAll":
    case "FRace":
    case "FAny":
    case "FSequence":
      return { ...node, flows: node.flows.map(getHash) } as unknown as NodeBase;

    case "FBranch":
      return { ...node, then: getHash(node.then), else: getHash(node.else) } as unknown as NodeBase;

    // Nodes without child flows
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
      return { ...node };
  }
}
