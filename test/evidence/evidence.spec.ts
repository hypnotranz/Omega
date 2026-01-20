import { describe, it, expect } from "vitest";
import { COWStore } from "../../src/core/eval/store";
import { envSet } from "../../src/core/eval/env";
import type { State } from "../../src/core/eval/machine";
import type { Val } from "../../src/core/eval/values";
import type { MeaningVal, Evidence } from "../../src/core/oracle/meaning";
import { evidenceId } from "../../src/core/provenance/evidence";
import { compileTextToExpr } from "../../src/core/pipeline/compileText";
import { installPrims } from "../helpers/prims";
import { runToCompletion } from "../../src/core/eval/run";
import { RuntimeImpl } from "../../src/core/effects/runtimeImpl";
import { ScriptedOracleAdapter } from "../../src/core/oracle/scriptedOracle";
import { SnapshotRepo } from "../../src/core/oracle/snapshots";
import { InMemoryReceiptStore } from "../../src/core/oracle/receipts";
import { mockCommit } from "../helpers/runtime";

function createRuntime() {
  const oracle = new ScriptedOracleAdapter();
  const snapshots = new SnapshotRepo();
  const receipts = new InMemoryReceiptStore("off");
  return new RuntimeImpl(oracle, snapshots, receipts, mockCommit);
}

async function evalWithBindings(src: string, bindings: Record<string, Val>): Promise<Val> {
  const expr = compileTextToExpr(src);
  let store = new COWStore();
  const prim = installPrims(store);
  store = prim.store;
  let env = prim.env;

  for (const [name, val] of Object.entries(bindings)) {
    const [s2, addr] = store.alloc(val);
    store = s2;
    env = envSet(env, name, addr);
  }

  const st: State = { control: { tag: "Expr", e: expr }, env, store, kont: [], handlers: [] };
  const runtime = createRuntime();
  return runToCompletion(runtime, st);
}

function expectStr(v: Val): string {
  if (v.tag !== "Str") throw new Error(`expected Str, got ${v.tag}`);
  return v.s;
}

function expectBool(v: Val): boolean {
  if (v.tag !== "Bool") throw new Error(`expected Bool, got ${v.tag}`);
  return v.b;
}

describe("Evidence primitives", () => {
  const ev: Evidence = { tag: "TestEvidence", passed: 1, total: 1 };
  const meaning: MeaningVal = { tag: "Meaning", denotation: { tag: "Num", n: 4 }, evidence: [ev] };
  const meaningId = evidenceId(ev);

  it("evidence-id returns a stable id for Meaning with evidence", async () => {
    const result = await evalWithBindings(`(evidence-id m)`, { m: meaning as Val });
    const id = expectStr(result);
    expect(id).toBe(meaningId);
  });

  it("verify-evidence is true when evidence exists", async () => {
    const result = await evalWithBindings(`(verify-evidence m)`, { m: meaning as Val });
    expect(expectBool(result)).toBe(true);
  });

  it("evidence-stale? is false for fresh evidence", async () => {
    const result = await evalWithBindings(`(evidence-stale? m)`, { m: meaning as Val });
    expect(expectBool(result)).toBe(false);
  });

  it("operations on non-Meaning values default to false/true", async () => {
    const id = await evalWithBindings(`(evidence-id 42)`, {});
    const verified = await evalWithBindings(`(verify-evidence 42)`, {});
    const stale = await evalWithBindings(`(evidence-stale? 42)`, {});

    expect(id).toMatchObject({ tag: "Bool", b: false });
    expect(expectBool(verified)).toBe(false);
    expect(expectBool(stale)).toBe(true);
  });
});
