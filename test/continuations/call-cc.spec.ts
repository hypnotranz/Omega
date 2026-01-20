import { describe, it, expect } from "vitest";
import { COWStore } from "../../src/core/eval/store";
import type { State } from "../../src/core/eval/machine";
import type { Val } from "../../src/core/eval/values";
import { runToCompletion } from "../../src/core/eval/run";
import { compileTextToExpr } from "../../src/core/pipeline/compileText";
import { installPrims } from "../helpers/prims";
import { RuntimeImpl } from "../../src/core/effects/runtimeImpl";
import { ScriptedOracleAdapter } from "../../src/core/oracle/scriptedOracle";
import { SnapshotRepo } from "../../src/core/oracle/snapshots";
import { InMemoryReceiptStore } from "../../src/core/oracle/receipts";
import { mockCommit } from "../helpers/runtime";
import type { Profile } from "../../src/core/governance/profile";

function createRuntime(profile?: Profile) {
  const oracle = new ScriptedOracleAdapter();
  const snapshots = new SnapshotRepo();
  const receipts = new InMemoryReceiptStore("off");
  const runtime = new RuntimeImpl(oracle, snapshots, receipts, mockCommit, profile);
  return runtime;
}

function initialState(src: string, profile?: Profile): State {
  const expr = compileTextToExpr(src);
  const store0 = new COWStore();
  const prim = installPrims(store0);
  return {
    control: { tag: "Expr", e: expr },
    env: prim.env,
    store: prim.store,
    kont: [],
    handlers: [],
    profile,
  };
}

async function evalOmega(src: string, profile?: Profile): Promise<Val> {
  const runtime = createRuntime(profile);
  const st0 = initialState(src, profile);
  return runToCompletion(runtime, st0);
}

function expectNum(v: Val): number {
  if (v.tag !== "Num") throw new Error(`expected Num, got ${v.tag}`);
  return v.n;
}

function expectSym(v: Val): string {
  if (v.tag !== "Sym") throw new Error(`expected Sym, got ${v.tag}`);
  return v.name;
}

describe("call/cc", () => {
  it("HP-1: immediate escape returns provided value", async () => {
    const result = await evalOmega(`(call/cc (lambda (k) (k 42)))`);
    expectNum(result);
    expect(result).toMatchObject({ tag: "Num", n: 42 });
  });

  it("HP-2: escapes nested computation", async () => {
    const result = await evalOmega(`(call/cc (lambda (k) (+ 1 (+ 2 (k 5)))))`);
    expectNum(result);
    expect(result).toMatchObject({ tag: "Num", n: 5 });
  });

  it("HP-3: saved continuation can be invoked later", async () => {
    const result = await evalOmega(`
      (begin
        (define saved #f)
        (define res (+ 1 (call/cc (lambda (k) (set! saved k) 10))))
        (if (eq? saved #f)
            res
            (let ((k saved))
              (set! saved #f)
              (k 100))))
    `);
    expectNum(result);
    expect(result).toMatchObject({ tag: "Num", n: 101 });
  });

  it("EC-1: mutations before invoking continuation persist", async () => {
    const result = await evalOmega(`
      (begin
        (define x 0)
        (call/cc (lambda (k)
          (set! x 1)
          (k 'done)
          (set! x 2)))
        x)
    `);
    expectNum(result);
    expect(result).toMatchObject({ tag: "Num", n: 1 });
  });

  it("ERR-1: non-procedure argument raises", async () => {
    await expect(evalOmega(`(call/cc 42)`)).rejects.toThrow(/procedure/i);
  });
});
