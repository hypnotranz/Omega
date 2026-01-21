import { describe, expect, it } from "vitest";
import { COWStore } from "../../src/core/eval/store";
import { envEmpty } from "../../src/core/eval/env";
import { VUnit } from "../../src/core/eval/values";
import type { State } from "../../src/core/eval/machine";
import type { Val } from "../../src/core/eval/values";
import type { Resumption } from "../../src/core/effects/opcall";
import { RuntimeImpl, type CommitAdapter } from "../../src/core/effects/runtimeImpl";
import { DEFAULT_PROFILE } from "../../src/core/governance/profile";
import type { MeaningVal } from "../../src/core/oracle/meaning";
import type { Expr } from "../../src/core/ast";
import { InMemoryReceiptStore, ScriptedOracleAdapter, SnapshotRepo, mockCommit } from "../helpers/runtime";

function syntax(expr: Expr): Val {
  return { tag: "Syntax", stx: expr } as Val;
}

function makeRuntime(profile = DEFAULT_PROFILE, commit: CommitAdapter = mockCommit): RuntimeImpl {
  const oracle = new ScriptedOracleAdapter();
  const snapshots = new SnapshotRepo();
  const receipts = new InMemoryReceiptStore("off");
  return new RuntimeImpl(oracle, snapshots, receipts, commit, profile);
}

function baseState(profile = DEFAULT_PROFILE): State {
  return {
    control: { tag: "Val", v: VUnit },
    env: envEmpty(),
    store: new COWStore(),
    kont: [],
    handlers: [],
    profile,
  };
}

function makeResumption(base: State): { resumption: Resumption; getValue: () => Val | undefined } {
  let resumed: Val | undefined;
  const resumption: Resumption = {
    rid: "r-commit-rewrite",
    base,
    digest: () => "ctx",
    invoke(v: Val): State {
      resumed = v;
      return { ...base, control: { tag: "Val", v } };
    },
  };
  return { resumption, getValue: () => resumed };
}

describe("commit/rewrite obligations", () => {
  const okProfile = { ...DEFAULT_PROFILE, caps: ["*"] };

  it("fails OblNoMatch with scope all when pattern appears in residual", async () => {
    const runtime = makeRuntime(okProfile);
    const state = baseState(okProfile);
    const pattern: Expr = { tag: "Var", name: "forbidden" };
    const meaning: MeaningVal = {
      tag: "Meaning",
      residual: syntax(pattern),
      rewrite: syntax({ tag: "Var", name: "ok" }),
      obligations: [{ tag: "OblNoMatch", pattern, scope: "all" }],
    };
    const { resumption } = makeResumption(state);

    await expect(runtime.dispatch(state, {
      op: "commit/rewrite.op",
      args: [meaning as Val],
      resumption,
      ctxDigest: "ctx",
    })).rejects.toThrow(/OblNoMatch failed.+all/);
  });

  it("checks both residual and rewrite when scope is all and records evidence", async () => {
    const calls: Val[] = [];
    const commitStub: CommitAdapter = {
      async commit(payload: Val): Promise<Val> {
        calls.push(payload);
        return { tag: "Str", s: "committed" } as Val;
      },
    };
    const runtime = makeRuntime(okProfile, commitStub);
    const state = baseState(okProfile);
    const pattern: Expr = { tag: "Var", name: "bad" };
    const meaning: MeaningVal = {
      tag: "Meaning",
      residual: syntax({ tag: "Var", name: "left" }),
      rewrite: syntax({ tag: "Var", name: "right" }),
      obligations: [{ tag: "OblNoMatch", pattern, scope: "all" }],
    };
    const { resumption, getValue } = makeResumption(state);

    await runtime.dispatch(state, {
      op: "commit/rewrite.op",
      args: [meaning as Val],
      resumption,
      ctxDigest: "ctx",
    });

    const returned = getValue() as MeaningVal;
    expect(returned?.evidence?.[0]).toMatchObject({
      tag: "NoMatchEvidence",
      searched: 2,
      found: 0,
    });
    expect(calls.length).toBe(1);
  });

  it("accepts commit.rewrite capability without requiring commit.*", async () => {
    const profile = {
      ...DEFAULT_PROFILE,
      name: "commit-rewrite-only",
      caps: ["commit.rewrite", "eval", "apply", "observe", "test"],
      truth: "test-certified",
    };
    const runtime = makeRuntime(profile);
    const state = baseState(profile);
    const meaning: MeaningVal = {
      tag: "Meaning",
      rewrite: syntax({ tag: "Var", name: "ok" }),
      obligations: [],
    };
    const { resumption, getValue } = makeResumption(state);

    await runtime.dispatch(state, {
      op: "commit/rewrite.op",
      args: [meaning as Val],
      resumption,
      ctxDigest: "ctx",
    });

    const returned = getValue() as MeaningVal;
    expect(returned?.rewrite).toBeTruthy();
  });
});
