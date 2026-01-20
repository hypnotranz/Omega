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

function createRuntime() {
  const oracle = new ScriptedOracleAdapter();
  const snapshots = new SnapshotRepo();
  const receipts = new InMemoryReceiptStore("off");
  return new RuntimeImpl(oracle, snapshots, receipts, mockCommit);
}

function initialState(src: string): State {
  const expr = compileTextToExpr(src);
  const store0 = new COWStore();
  const prim = installPrims(store0);
  return { control: { tag: "Expr", e: expr }, env: prim.env, store: prim.store, kont: [], handlers: [] };
}

async function evalOmega(src: string): Promise<Val> {
  const runtime = createRuntime();
  const st0 = initialState(src);
  return runToCompletion(runtime, st0);
}

function expectNum(v: Val): number {
  if (v.tag !== "Num") throw new Error(`expected Num, got ${v.tag}`);
  return v.n;
}

function listToArray(v: Val): Val[] {
  const out: Val[] = [];
  let cur: Val = v;
  while (cur.tag === "Vector" && cur.items.length === 2) {
    out.push(cur.items[0]);
    cur = cur.items[1];
  }
  return out;
}

describe("Delimited continuations (call-with-prompt / abort-to-prompt)", () => {
  it("HP-1: abort handles value with handler", async () => {
    const result = await evalOmega(`
      (call-with-prompt 'p
        (lambda () (+ 1 (abort-to-prompt 'p 5)))
        (lambda (k v) (* v 2)))
    `);
    expectNum(result);
    expect(result).toMatchObject({ tag: "Num", n: 10 });
  });

  it("HP-2: handler can resume continuation", async () => {
    const result = await evalOmega(`
      (call-with-prompt 'p
        (lambda () (+ 1 (abort-to-prompt 'p 5)))
        (lambda (k v) (k (* v 2))))
    `);
    expectNum(result);
    expect(result).toMatchObject({ tag: "Num", n: 11 });
  });

  it("HP-3: captured delimited continuation is reusable", async () => {
    const result = await evalOmega(`
      (begin
        (define saved #f)
        (define first
          (call-with-prompt 'p
            (lambda () (+ 1 (abort-to-prompt 'p 5)))
            (lambda (k v) (set! saved k) v)))
        (if (eq? saved #f)
            first
            (let ((k saved))
              (set! saved #f)
              (k 10))))
    `);
    expectNum(result);
    expect(result).toMatchObject({ tag: "Num", n: 11 });
  });

  it("EC-1: normal completion with no abort returns body value", async () => {
    const result = await evalOmega(`(call-with-prompt 'p (lambda () 42) (lambda (k v) 'unused))`);
    expectNum(result);
    expect(result).toMatchObject({ tag: "Num", n: 42 });
  });

  it("ERR-1: abort without matching prompt errors", async () => {
    await expect(evalOmega(`(abort-to-prompt 'missing 5)`)).rejects.toThrow(/prompt/i);
  });
});
