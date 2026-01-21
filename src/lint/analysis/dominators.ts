import type {
  FlowIR,
  FAll,
  FAny,
  FBind,
  FBranch,
  FCatch,
  FLoop,
  FRace,
  FSequence,
  FWithBudget,
  FWithTimeout,
} from "../../frameir/flow";

export interface FlowNode {
  id: string;
  flow: FlowIR;
  children: string[];
  parents: string[];
}

export interface FlowGraph {
  nodes: Map<string, FlowNode>;
  entryId: string;
}

/**
  * Build a flow graph with parent/child relationships for dominator analysis.
  */
export function buildFlowGraph(flow: FlowIR): FlowGraph {
  const nodes = new Map<string, FlowNode>();
  let nextId = 0;

  const addNode = (f: FlowIR, parentId?: string): string => {
    const id = `n${nextId++}`;
    const node: FlowNode = { id, flow: f, children: [], parents: parentId ? [parentId] : [] };
    nodes.set(id, node);

    if (parentId) {
      const parent = nodes.get(parentId);
      if (parent) {
        parent.children.push(id);
      }
    }

    switch (f.tag) {
      case "FBind": {
        const bind = f as FBind;
        addNode(bind.flow, id);
        break;
      }
      case "FCatch": {
        const caught = f as FCatch;
        addNode(caught.flow, id);
        break;
      }
      case "FWithBudget": {
        const wb = f as FWithBudget;
        addNode(wb.flow, id);
        break;
      }
      case "FWithTimeout": {
        const wt = f as FWithTimeout;
        addNode(wt.flow, id);
        break;
      }
      case "FSequence": {
        const seq = f as FSequence;
        let prev = id;
        for (const child of seq.flows) {
          prev = addNode(child, prev);
        }
        break;
      }
      case "FBranch": {
        const br = f as FBranch;
        addNode(br.then, id);
        addNode(br.else, id);
        break;
      }
      case "FAll":
      case "FRace":
      case "FAny": {
        const par = f as FAll | FRace | FAny;
        for (const child of par.flows) {
          addNode(child, id);
        }
        break;
      }
      case "FLoop": {
        // Loop references functions; no direct Flow child.
        break;
      }
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

    return id;
  };

  const entryId = addNode(flow);
  return { nodes, entryId };
}

/**
 * Compute dominators for each node in the graph.
 */
export function computeDominators(graph: FlowGraph): Map<string, Set<string>> {
  const { nodes, entryId } = graph;
  const dominators = new Map<string, Set<string>>();
  const allIds = new Set(nodes.keys());

  for (const id of nodes.keys()) {
    if (id === entryId) {
      dominators.set(id, new Set([id]));
    } else {
      dominators.set(id, new Set(allIds));
    }
  }

  let changed = true;
  while (changed) {
    changed = false;

    for (const [id, node] of nodes) {
      if (id === entryId) continue;

      const predDoms = node.parents.map(p => dominators.get(p) ?? new Set<string>());
      const intersection = buildIntersection(predDoms);
      intersection.add(id);

      const current = dominators.get(id)!;
      const currentStr = [...current].sort().join(",");
      const nextStr = [...intersection].sort().join(",");
      if (currentStr !== nextStr) {
        dominators.set(id, intersection);
        changed = true;
      }
    }
  }

  return dominators;
}

function buildIntersection(sets: Array<Set<string>>): Set<string> {
  if (sets.length === 0) {
    return new Set<string>();
  }

  const intersection = new Set<string>(sets[0]);
  for (let i = 1; i < sets.length; i++) {
    for (const value of Array.from(intersection)) {
      if (!sets[i].has(value)) {
        intersection.delete(value);
      }
    }
  }
  return intersection;
}

/**
 * Build a lookup map from FlowIR node references to graph node ids.
 */
export function indexNodesByRef(graph: FlowGraph): Map<FlowIR, string> {
  const index = new Map<FlowIR, string>();
  for (const [id, node] of graph.nodes) {
    index.set(node.flow, id);
  }
  return index;
}

/**
 * Check if nodeId is dominated by any node with the given tag.
 */
export function isDominatedBy(
  nodeId: string,
  tag: FlowIR["tag"],
  graph: FlowGraph,
  dominators: Map<string, Set<string>>
): boolean {
  const doms = dominators.get(nodeId);
  if (!doms) return false;

  for (const domId of doms) {
    const domNode = graph.nodes.get(domId);
    if (domNode?.flow.tag === tag) {
      return true;
    }
  }
  return false;
}
