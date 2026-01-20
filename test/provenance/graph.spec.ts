import { describe, it, expect } from "vitest";
import { ProvenanceGraph } from "../../src/core/provenance/graph";
import { evidenceId, type DerivedEvidence, type OracleEvidence } from "../../src/core/provenance/evidence";

function oracleEvidence(receiptId: string, sourceHash: string, timestamp = 0): OracleEvidence {
  return { tag: "OracleEvidence", receiptId, sourceHash, timestamp };
}

function derivedEvidence(dependencies: string[], expr = "combine"): DerivedEvidence {
  return { tag: "DerivedEvidence", dependencies, derivationExpr: expr };
}

describe("ProvenanceGraph", () => {
  it("adds nodes and returns stable ids", () => {
    const g = new ProvenanceGraph();
    const ev = oracleEvidence("rx-1", "sh-1", 1000);
    const id = g.addNode(ev);
    expect(id).toBe(evidenceId(ev));
    expect(g.toJSON().nodes).toHaveLength(1);
  });

  it("records derivations and traces direct sources", () => {
    const g = new ProvenanceGraph();
    const src1 = oracleEvidence("rx-1", "sh-1", 1000);
    const src2 = oracleEvidence("rx-2", "sh-2", 2000);
    const derived = derivedEvidence([evidenceId(src1), evidenceId(src2)], "merge");

    const derivedId = g.derivedFrom(derived, [src1, src2]);
    expect(derivedId).toBe(evidenceId(derived));

    const sources = g.getSources(derivedId).map(n => n.id);
    expect(new Set(sources)).toEqual(new Set([evidenceId(src1), evidenceId(src2)]));
    expect(g.toJSON().edges).toHaveLength(2);
  });

  it("traces sources transitively", () => {
    const g = new ProvenanceGraph();
    const a = oracleEvidence("rx-a", "sh-a", 1);
    const b = derivedEvidence([evidenceId(a)], "step1");
    const c = derivedEvidence([evidenceId(b)], "step2");

    g.derivedFrom(b, [a]);
    g.derivedFrom(c, [b]);

    const ids = new Set(g.getSources(evidenceId(c)).map(n => n.id));
    expect(ids).toEqual(new Set([evidenceId(a), evidenceId(b)]));
  });

  it("deduplicates nodes when added multiple times", () => {
    const g = new ProvenanceGraph();
    const ev = oracleEvidence("rx-1", "sh-1", 5);
    const id1 = g.addNode(ev);
    const id2 = g.addNode(ev);
    expect(id1).toBe(id2);
    expect(g.toJSON().nodes).toHaveLength(1);
  });

  it("handles diamond dependencies without duplication", () => {
    const g = new ProvenanceGraph();
    const a = oracleEvidence("rx-a", "sh-a", 1);
    const b = derivedEvidence([evidenceId(a)], "B");
    const c = derivedEvidence([evidenceId(a)], "C");
    const d = derivedEvidence([evidenceId(b), evidenceId(c)], "D");

    g.derivedFrom(b, [a]);
    g.derivedFrom(c, [a]);
    g.derivedFrom(d, [b, c]);

    const sourcesOfD = g.getSources(evidenceId(d)).map(n => n.id);
    expect(new Set(sourcesOfD)).toEqual(new Set([evidenceId(a), evidenceId(b), evidenceId(c)]));
  });

  it("errors on self-referential edges", () => {
    const g = new ProvenanceGraph();
    const ev = oracleEvidence("rx-1", "sh-1", 0);
    const id = g.addNode(ev);
    expect(() => g.addEdge(id, id, "derived-from")).toThrow();
  });

  it("errors when adding edge for missing nodes", () => {
    const g = new ProvenanceGraph();
    expect(() => g.addEdge("missing", "also-missing", "derived-from")).toThrow();
  });

  it("returns empty sources for unknown evidence id", () => {
    const g = new ProvenanceGraph();
    expect(g.getSources("nonexistent")).toEqual([]);
  });
});
