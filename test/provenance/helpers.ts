import { COWStore } from "../../src/core/eval/store";
import { envSet } from "../../src/core/eval/env";
import type { State } from "../../src/core/eval/machine";
import type { Val } from "../../src/core/eval/values";
import { runToCompletionWithState } from "../../src/core/eval/run";
import { compileTextToExpr } from "../../src/core/pipeline/compileText";
import { installPrims } from "../helpers/prims";
import { ProvenanceGraph, type SourceChecker } from "../../src/core/provenance/graph";
import type { ProvenanceStore } from "../../src/core/provenance/store/interface";
import { RuntimeImpl } from "../../src/core/effects/runtimeImpl";
import { ScriptedOracleAdapter, SnapshotRepo, InMemoryReceiptStore, mockCommit } from "../helpers/runtime";
import { setProvenanceGraph, setSourceChecker } from "../../src/core/provenance/context";

export type EvalOpts = {
  graph?: ProvenanceGraph;
  bindings?: Record<string, Val>;
  sourceChecker?: SourceChecker;
  provenanceStore?: ProvenanceStore;
};

export function listToArray(lst: Val): Val[] {
  const out: Val[] = [];
  let cur: Val = lst;
  while (cur.tag === "Vector" && cur.items.length === 2) {
    out.push(cur.items[0]);
    cur = cur.items[1];
  }
  if (cur.tag !== "Unit") throw new Error("expected proper list");
  return out;
}

export function mapToObject(v: Val): Record<string, Val> {
  if (v.tag !== "Map") throw new Error("expected Map value");
  const obj: Record<string, Val> = {};
  for (const [k, val] of v.entries) {
    const key = (k as any).tag === "Sym" ? (k as any).name : (k as any).s;
    obj[key] = val;
  }
  return obj;
}

export function initialStateWithProvenance(src: string, opts: EvalOpts = {}): State {
  const expr = compileTextToExpr(src);
  let store = new COWStore();
  const prim = installPrims(store);
  store = prim.store;
  let env = prim.env;

  for (const [name, val] of Object.entries(opts.bindings ?? {})) {
    const [s2, addr] = store.alloc(val);
    store = s2;
    env = envSet(env, name, addr);
  }

  const graph = opts.graph ?? new ProvenanceGraph();
  const checker = opts.sourceChecker;
  const provenanceStore = opts.provenanceStore;
  setProvenanceGraph(graph);
  if (checker) setSourceChecker(checker);

  return {
    control: { tag: "Expr", e: expr },
    env,
    store,
    kont: [],
    handlers: [],
    provenanceGraph: graph,
    provenanceSourceChecker: checker,
    provenanceStore,
  };
}

export async function evalWithProvenance(src: string, opts: EvalOpts = {}): Promise<{ value: Val; state: State }> {
  const runtime = new RuntimeImpl(new ScriptedOracleAdapter(), new SnapshotRepo(), new InMemoryReceiptStore("off"), mockCommit);
  const st = initialStateWithProvenance(src, opts);
  return runToCompletionWithState(runtime, st);
}
