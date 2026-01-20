import fs from "fs";
import path from "path";
import { COWStore } from "../../src/core/eval/store";
import { compileTextToExpr } from "../../src/core/pipeline/compileText";
import { installPrims } from "../helpers/prims";
import type { State } from "../../src/core/eval/machine";
import type { Val } from "../../src/core/eval/values";
import { runToCompletion } from "../../src/core/eval/run";
import { RuntimeImpl } from "../../src/core/effects/runtimeImpl";
import { ScriptedOracleAdapter } from "../../src/core/oracle/scriptedOracle";
import { SnapshotRepo } from "../../src/core/oracle/snapshots";
import { InMemoryReceiptStore } from "../../src/core/oracle/receipts";
import { mockCommit } from "../helpers/runtime";
import type { Profile } from "../../src/core/governance/profile";
import { runSearch } from "../../src/core/effects/search/runner";
import type { SearchConfig } from "../../src/core/effects/search/types";
import { distValues } from "../../src/core/eval/dist";

function createRuntime(profile?: Profile): RuntimeImpl {
  const oracle = new ScriptedOracleAdapter();
  const snapshots = new SnapshotRepo();
  const receipts = new InMemoryReceiptStore("off");
  return new RuntimeImpl(oracle, snapshots, receipts, mockCommit, profile);
}

function initialState(src: string): State {
  const expr = compileTextToExpr(src);
  const store0 = new COWStore();
  const prim = installPrims(store0);
  return { control: { tag: "Expr", e: expr }, env: prim.env, store: prim.store, kont: [], handlers: [] };
}

let cachedMonadLib: string | null = null;
const monadLibPath = path.resolve(__dirname, "../../lib/monad.lisp");

export function withMonadLib(body: string): string {
  if (cachedMonadLib === null) {
    cachedMonadLib = fs.readFileSync(monadLibPath, "utf8");
  }
  return `(begin ${cachedMonadLib}\n${body}\n)`;
}

export async function evalAll(code: string, opts?: { useMonadLib?: boolean; profile?: Profile }): Promise<Val[]> {
  const program = opts?.useMonadLib ? withMonadLib(code) : code;
  const runtime = createRuntime(opts?.profile);
  const state = initialState(program);
  const config: SearchConfig = {
    strategy: "dfs",
    budget: { searchNodesLeft: 2000, searchDepthLeft: 500, solutionsLeft: 2000, maxFrontierSize: 2000 },
  };
  const result = await runSearch(runtime, state, config);
  return distValues(result.dist);
}

export async function evalOne(code: string, opts?: { useMonadLib?: boolean; profile?: Profile }): Promise<Val> {
  const program = opts?.useMonadLib ? withMonadLib(code) : code;
  const st0 = initialState(program);
  const runtime = createRuntime(opts?.profile);
  return runToCompletion(runtime, st0);
}

export function listToArray(v: Val): Val[] {
  const out: Val[] = [];
  let cur: Val = v;
  while (cur.tag === "Vector" && cur.items.length === 2) {
    out.push(cur.items[0]);
    cur = cur.items[1];
  }
  if (cur.tag !== "Unit") {
    out.push(cur);
  }
  return out;
}

export function valToJs(v: Val): unknown {
  switch (v.tag) {
    case "Num": return v.n;
    case "Bool": return v.b;
    case "Str": return v.s;
    case "Sym": return v.name;
    case "Unit": return null;
    case "Vector": {
      if (v.items.length === 2) {
        const arr: unknown[] = [];
        let cur: Val = v;
        while (cur.tag === "Vector" && cur.items.length === 2) {
          arr.push(valToJs(cur.items[0]));
          cur = cur.items[1];
        }
        if (cur.tag !== "Unit") {
          arr.push(valToJs(cur));
        }
        return arr;
      }
      return v.items.map(valToJs);
    }
    default:
      return v;
  }
}
