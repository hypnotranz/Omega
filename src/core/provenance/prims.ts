import { getProvenanceGraph, getSourceChecker } from "./context";
import { evidenceId, evidenceToVal, type DerivedEvidence, type Evidence } from "./evidence";
import type { Val } from "../eval/values";
import { VFalse, VUnit } from "../eval/values";
import { isMeaning, meaning as mkMeaning, type MeaningVal } from "../oracle/meaning";
import type { State } from "../eval/machine";
import type { SourceChecker, ProvenanceGraph } from "./graph";

type DefFn = (name: string, v: Val) => void;

function strVal(s: string): Val {
  return { tag: "Str", s };
}

function numVal(n: number): Val {
  return { tag: "Num", n };
}

function toList(items: Val[]): Val {
  let result: Val = VUnit;
  for (let i = items.length - 1; i >= 0; i--) {
    result = { tag: "Vector", items: [items[i], result] };
  }
  return result;
}

function listToArray(list: Val): Val[] {
  const out: Val[] = [];
  let cur: Val = list;
  while (cur.tag === "Vector" && cur.items.length === 2) {
    out.push(cur.items[0]);
    cur = cur.items[1];
  }
  return out;
}

function asString(v: Val): string {
  if (v.tag === "Str") return v.s;
  if (v.tag === "Sym") return v.name;
  if (v.tag === "Num") return String(v.n);
  if (v.tag === "Bool") return v.b ? "true" : "false";
  if (v.tag === "Unit") return "";
  return JSON.stringify(v);
}

function attachEvidence(val: Val, evidence: Evidence[]): Val {
  if (isMeaning(val)) {
    const existing = val.evidence ?? [];
    return { ...val, evidence: [...existing, ...evidence] } as MeaningVal;
  }
  return mkMeaning({ denotation: val, evidence });
}

function collectEvidenceFromList(listVal: Val): Evidence[] {
  const out: Evidence[] = [];
  for (const item of listToArray(listVal)) {
    if (isMeaning(item) && item.evidence) {
      out.push(...item.evidence);
    }
  }
  return out;
}

function getGraph(state: State): ProvenanceGraph {
  const st = state as any;
  if (st.provenanceGraph) return st.provenanceGraph as ProvenanceGraph;
  const g = getProvenanceGraph();
  st.provenanceGraph = g;
  return g;
}

function getChecker(state: State): SourceChecker {
  const st = state as any;
  return (st.provenanceSourceChecker as SourceChecker | undefined) ?? getSourceChecker();
}

export function registerProvenancePrims(def: DefFn): void {
  // Return a summary of the active graph
  def("provenance-graph", {
    tag: "Native",
    name: "provenance-graph",
    arity: 0,
    fn: (_args, s) => {
      const graph = getGraph(s as State);
      const data = graph.toJSON();
      const result: Val = {
        tag: "Map",
        entries: [
          [strVal("nodes"), numVal(data.nodes.length)],
          [strVal("edges"), numVal(data.edges.length)],
        ],
      };
      return { ...s, control: { tag: "Val", v: result } };
    },
  });

  // Explicitly record a derivation between values
  def("provenance-record", {
    tag: "Native",
    name: "provenance-record",
    arity: 3,
    fn: (args, s) => {
      const [derivedVal, sourcesVal, opVal] = args;
      const graph = getGraph(s as State);
      const sources = collectEvidenceFromList(sourcesVal);
      const derivation: DerivedEvidence = {
        tag: "DerivedEvidence",
        dependencies: sources.map(evidenceId),
        derivationExpr: asString(opVal),
      };

      graph.derivedFrom(derivation, sources);
      const annotated = attachEvidence(derivedVal, [derivation]);
      return { ...s, control: { tag: "Val", v: annotated } };
    },
  });

  // Trace a value back to source evidence
  def("provenance-trace", {
    tag: "Native",
    name: "provenance-trace",
    arity: 1,
    fn: (args, s) => {
      const target = args[0];
      if (!isMeaning(target) || !target.evidence || target.evidence.length === 0) {
        return { ...s, control: { tag: "Val", v: VUnit } };
      }
      const graph = getGraph(s as State);
      const rootId = evidenceId(target.evidence[0]);
      const sources = graph.getSources(rootId);
      const ids = sources.map(src => strVal(src.id));
      return { ...s, control: { tag: "Val", v: toList(ids) } };
    },
  });

  // Check whether upstream oracle evidence has changed
  def("provenance-check-staleness", {
    tag: "Native",
    name: "provenance-check-staleness",
    arity: 1,
    fn: (args, s) => {
      const target = args[0];
      if (!isMeaning(target) || !target.evidence || target.evidence.length === 0) {
        const fallback: Val = {
          tag: "Map",
          entries: [
            [strVal("stale?"), { tag: "Bool", b: true }],
            [strVal("stale-count"), numVal(0)],
            [strVal("total-sources"), numVal(0)],
          ],
        };
        return { ...s, control: { tag: "Val", v: fallback } };
      }

      const graph = getGraph(s as State);
      const rootId = evidenceId(target.evidence[0]);
      const report = graph.checkStaleness(rootId, getChecker(s as State));
      const result: Val = {
        tag: "Map",
        entries: [
          [strVal("stale?"), { tag: "Bool", b: report.isStale }],
          [strVal("stale-count"), numVal(report.staleItems.length)],
          [strVal("total-sources"), numVal(report.totalSources)],
        ],
      };
      return { ...s, control: { tag: "Val", v: result } };
    },
  });

  // Convert evidence data to a Value (useful for inspection)
  def("evidence->val", {
    tag: "Native",
    name: "evidence->val",
    arity: 1,
    fn: (args, s) => {
      const ev = args[0] as any;
      if (!ev || typeof ev !== "object" || !("tag" in ev)) {
        return { ...s, control: { tag: "Val", v: VFalse } };
      }
      return { ...s, control: { tag: "Val", v: evidenceToVal(ev as Evidence) } };
    },
  });
}
