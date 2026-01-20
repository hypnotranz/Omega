import { evidenceId, type Evidence, type OracleEvidence } from "./evidence";

export type ProvenanceNode = {
  id: string;
  evidence: Evidence;
  timestamp: number;
};

export type ProvenanceEdge = {
  from: string;
  to: string;
  relation: "derived-from" | "validated-by" | "depends-on";
};

export type ProvenanceGraphData = {
  nodes: ProvenanceNode[];
  edges: ProvenanceEdge[];
};

export type SourceChecker = {
  getSourceHash(receiptId: string): string | undefined;
};

export type StaleItem = {
  evidenceId: string;
  receiptId: string;
  originalHash: string;
  currentHash?: string;
};

export type StalenessReport = {
  isStale: boolean;
  staleItems: StaleItem[];
  totalSources: number;
};

export class ProvenanceGraph {
  private nodes: Map<string, ProvenanceNode> = new Map();
  private edges: ProvenanceEdge[] = [];
  private reverseIndex: Map<string, Set<string>> = new Map();

  addNode(evidence: Evidence): string {
    const id = evidenceId(evidence);
    if (!this.nodes.has(id)) {
      const ts = (evidence as any).timestamp ?? Date.now();
      this.nodes.set(id, { id, evidence, timestamp: ts });
    }
    return id;
  }

  addEdge(from: string, to: string, relation: ProvenanceEdge["relation"]): void {
    if (from === to) throw new Error("ProvenanceGraph: cannot create self-referential edge");
    if (!this.nodes.has(from) || !this.nodes.has(to)) {
      throw new Error("ProvenanceGraph: both nodes must exist before adding an edge");
    }
    this.edges.push({ from, to, relation });
    const rev = this.reverseIndex.get(to) ?? new Set<string>();
    rev.add(from);
    this.reverseIndex.set(to, rev);
  }

  derivedFrom(derived: Evidence, sources: Evidence[]): string {
    const derivedId = this.addNode(derived);
    for (const src of sources) {
      const srcId = this.addNode(src);
      this.addEdge(srcId, derivedId, "derived-from");
    }
    return derivedId;
  }

  getSources(targetId: string): ProvenanceNode[] {
    const seen = new Set<string>();
    const result: ProvenanceNode[] = [];
    const stack = [targetId];

    while (stack.length > 0) {
      const cur = stack.pop()!;
      const parents = this.reverseIndex.get(cur);
      if (!parents) continue;

      for (const p of parents) {
        if (seen.has(p)) continue;
        seen.add(p);
        const node = this.nodes.get(p);
        if (node) {
          result.push(node);
          stack.push(p);
        }
      }
    }

    return result;
  }

  checkStaleness(targetId: string, checker?: SourceChecker): StalenessReport {
    const oracleNodes: Map<string, ProvenanceNode & { evidence: OracleEvidence }> = new Map();

    const targetNode = this.nodes.get(targetId);
    if (targetNode && targetNode.evidence.tag === "OracleEvidence") {
      oracleNodes.set(targetNode.id, targetNode as any);
    }

    for (const n of this.getSources(targetId)) {
      if (n.evidence.tag === "OracleEvidence") {
        oracleNodes.set(n.id, n as any);
      }
    }

    const staleItems: StaleItem[] = [];
    if (checker) {
      for (const node of oracleNodes.values()) {
        const currentHash = checker.getSourceHash(node.evidence.receiptId);
        if (currentHash !== undefined && currentHash !== node.evidence.sourceHash) {
          staleItems.push({
            evidenceId: node.id,
            receiptId: node.evidence.receiptId,
            originalHash: node.evidence.sourceHash,
            currentHash,
          });
        }
      }
    }

    return {
      isStale: staleItems.length > 0,
      staleItems,
      totalSources: oracleNodes.size,
    };
  }

  toJSON(): ProvenanceGraphData {
    return {
      nodes: Array.from(this.nodes.values()),
      edges: this.edges.slice(),
    };
  }

  static fromJSON(data: ProvenanceGraphData): ProvenanceGraph {
    const g = new ProvenanceGraph();
    for (const n of data.nodes) {
      g.nodes.set(n.id, n);
    }
    g.edges = data.edges.slice();
    for (const e of g.edges) {
      const rev = g.reverseIndex.get(e.to) ?? new Set<string>();
      rev.add(e.from);
      g.reverseIndex.set(e.to, rev);
    }
    return g;
  }
}
