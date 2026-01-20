import { COWStore } from "../../src/core/eval/store";
import type { State } from "../../src/core/eval/machine";
import type { Val } from "../../src/core/eval/values";
import { runToCompletion } from "../../src/core/eval/run";
import { compileTextToExpr } from "../../src/core/pipeline/compileText";
import { installPrims } from "../helpers/prims";
import { RuntimeImpl } from "../../src/core/effects/runtimeImpl";
import { ScriptedOracleAdapter, SnapshotRepo, InMemoryReceiptStore, mockCommit } from "../helpers/runtime";

export function initialState(src: string): State {
  const expr = compileTextToExpr(src);
  const store0 = new COWStore();
  const prim = installPrims(store0);
  return { control: { tag: "Expr", e: expr }, env: prim.env, store: prim.store, kont: [], handlers: [] };
}

export async function evalOmega(src: string): Promise<Val> {
  const runtime = new RuntimeImpl(new ScriptedOracleAdapter(), new SnapshotRepo(), new InMemoryReceiptStore("off"), mockCommit);
  return runToCompletion(runtime, initialState(src));
}

export function expectSym(v: Val): string {
  if (v.tag !== "Sym") {
    throw new Error(`expected Sym, got ${v.tag}`);
  }
  return v.name;
}

export function expectBool(v: Val): boolean {
  if (v.tag !== "Bool") {
    throw new Error(`expected Bool, got ${v.tag}`);
  }
  return v.b;
}

export function listToArray(lst: Val): Val[] {
  const out: Val[] = [];
  let cur: Val = lst;
  while (cur.tag === "Vector" && cur.items.length === 2) {
    out.push(cur.items[0]);
    cur = cur.items[1];
  }
  if (cur.tag !== "Unit") {
    throw new Error("expected proper list");
  }
  return out;
}
