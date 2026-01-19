// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-14.md
// Omega Harness with full Oracle Protocol support

import { COWStore } from "../../src/core/eval/store";
import { RuntimeImpl } from "../../src/core/effects/runtimeImpl";
import { ScriptedOracleAdapter, SnapshotRepo, InMemoryReceiptStore, mockCommit } from "./runtime";
import { installPrims } from "./prims";
import type { State } from "../../src/core/eval/machine";
import { runToCompletion } from "../../src/core/eval/run";
import type { Expr } from "../../src/core/ast";
import { compileTextToExpr } from "../../src/core/pipeline/compileText";

function initialState(expr: Expr): State {
  const store0 = new COWStore();
  const prim = installPrims(store0);
  return { control: { tag: "Expr", e: expr }, env: prim.env, store: prim.store, kont: [], handlers: [] };
}

export async function evalOmega(src: string): Promise<any> {
  const expr = compileTextToExpr(src);

  // Wire up the full Oracle Protocol
  const oracle = new ScriptedOracleAdapter();
  const snapshots = new SnapshotRepo();
  const receipts = new InMemoryReceiptStore("off");

  const runtime = new RuntimeImpl(oracle, snapshots, receipts, mockCommit);
  const v = await runToCompletion(runtime, initialState(expr), 500_000);

  if (v.tag === "Num") return v.n;
  if (v.tag === "Bool") return v.b;
  if (v.tag === "Str") return v.s;
  if (v.tag === "Unit") return null;
  return v;
}
