import { describe, it, expect } from "vitest";
import { ProvenanceGraph, type SourceChecker } from "../../src/core/provenance/graph";
import { computeSourceHash, evidenceId, type DerivedEvidence, type OracleEvidence } from "../../src/core/provenance/evidence";
import type { MeaningVal } from "../../src/core/oracle/meaning";
import type { Val } from "../../src/core/eval/values";
import { evalWithProvenance, initialStateWithProvenance, listToArray, mapToObject } from "./helpers.ts";
import { runToCompletionWithState } from "../../src/core/eval/run";
import { RuntimeImpl } from "../../src/core/effects/runtimeImpl";
import { ScriptedOracleAdapter, SnapshotRepo, InMemoryReceiptStore, mockCommit } from "../helpers/runtime";

function oracleEvidence(receiptId: string, sourceHash: string, timestamp = 0): OracleEvidence {
  return { tag: "OracleEvidence", receiptId, sourceHash, timestamp };
}

function derivedEvidence(dependencies: string[], expr = "derive"): DerivedEvidence {
  return { tag: "DerivedEvidence", dependencies, derivationExpr: expr };
}

class StubSourceChecker implements SourceChecker {
  constructor(private hashes: Record<string, string>) {}
  getSourceHash(receiptId: string): string | undefined {
    return this.hashes[receiptId];
  }
  set(receiptId: string, hash: string): void {
    this.hashes[receiptId] = hash;
  }
}

describe("Provenance staleness", () => {
  it("detects stale oracle evidence via SourceChecker", async () => {
    const graph = new ProvenanceGraph();
    const ev = oracleEvidence("rx-1", "sh-original", 1);
    const derived = derivedEvidence([evidenceId(ev)], "identity");
    graph.derivedFrom(derived, [ev]);

    const checker = new StubSourceChecker({ "rx-1": "sh-original" });
    const fresh = await graph.checkStaleness(evidenceId(derived), checker);
    expect(fresh.isStale).toBe(false);

    checker.set("rx-1", "sh-updated");
    const stale = await graph.checkStaleness(evidenceId(derived), checker);
    expect(stale.isStale).toBe(true);
    expect(stale.staleItems[0]?.receiptId).toBe("rx-1");
  });

  it("records derivations with primitives and traces back to oracle sources", async () => {
    const srcEvidence = oracleEvidence("rx-a", computeSourceHash("payload-a"), 1);
    const srcMeaning: MeaningVal = { tag: "Meaning", denotation: { tag: "Num", n: 2 }, evidence: [srcEvidence] };
    const derivedMeaning: MeaningVal = { tag: "Meaning", denotation: { tag: "Num", n: 4 } };

    const { value } = await evalWithProvenance(
      `(let ((res (provenance-record d (list s) "double")))
         (provenance-trace res))`,
      { bindings: { s: srcMeaning as Val, d: derivedMeaning as Val } }
    );

    const trace = listToArray(value);
    expect(trace).toHaveLength(1);
    expect(trace[0]).toMatchObject({ tag: "Str", s: evidenceId(srcEvidence) });
  });

  it("checks staleness through primitive on derived values", async () => {
    const graph = new ProvenanceGraph();
    const checker = new StubSourceChecker({});
    const srcEvidence = oracleEvidence("rx-b", computeSourceHash("payload-b"), 10);
    const srcMeaning: MeaningVal = { tag: "Meaning", denotation: { tag: "Num", n: 1 }, evidence: [srcEvidence] };
    const derivedMeaning: MeaningVal = { tag: "Meaning", denotation: { tag: "Num", n: 2 } };

    // First run: record provenance (fresh)
    const firstRun = await runToCompletionWithState(
      new RuntimeImpl(new ScriptedOracleAdapter(), new SnapshotRepo(), new InMemoryReceiptStore("off"), mockCommit),
      initialStateWithProvenance(
        `(provenance-record d (list s) "combine")`,
        { graph, sourceChecker: checker, bindings: { s: srcMeaning as Val, d: derivedMeaning as Val } }
      )
    );
    const derivedVal = firstRun.value;
    const activeGraph = firstRun.state.provenanceGraph ?? graph;
    expect((derivedVal as MeaningVal).evidence?.length ?? 0).toBeGreaterThan(0);

    checker.set("rx-b", "sh-stale");
    expect(activeGraph.toJSON().nodes.length).toBeGreaterThan(0);
    const derivedEvidenceId = (derivedVal as MeaningVal).evidence ? evidenceId((derivedVal as MeaningVal).evidence![0]) : "";
    const directReport = activeGraph.checkStaleness(derivedEvidenceId, checker);
    expect(directReport.isStale).toBe(true);
    const secondRun = await runToCompletionWithState(
      new RuntimeImpl(new ScriptedOracleAdapter(), new SnapshotRepo(), new InMemoryReceiptStore("off"), mockCommit),
      initialStateWithProvenance(
        `(provenance-check-staleness v)`,
        { graph: activeGraph, sourceChecker: checker, bindings: { v: derivedVal as Val } }
      )
    );
    const report = secondRun.value;

    const fields = mapToObject(report);
    expect(fields["stale?"]).toMatchObject({ tag: "Bool", b: true });
    expect(fields["stale-count"]).toMatchObject({ tag: "Num", n: 1 });
    expect(fields["total-sources"]).toMatchObject({ tag: "Num", n: 1 });
  });
});
