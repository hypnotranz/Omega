// test/oracle/impl16.spec.ts
// Tests for IMPLEMENTATION-16 Patch Sets A-D

import { describe, it, expect } from "vitest";
import { evalOmega } from "../helpers/omegaHarness";

// Import the new types
import { dist, distNormalize, distSample, distTopK, distFrom, isDist } from "../../src/core/eval/dist";
import { meaning, isMeaning, type MeaningVal } from "../../src/core/oracle/meaning";
import { matchAST } from "../../src/core/oracle/match";
import { CtxReceiptRepo } from "../../src/core/oracle/ctxReceipts";

describe("Patch Set A: Dist<Val> as first-class value", () => {
  it("A1: dist() creates a point distribution", () => {
    const d = dist({ tag: "Num", n: 42 });
    expect(d.tag).toBe("Dist");
    expect(d.support.length).toBe(1);
    expect(d.support[0].v).toEqual({ tag: "Num", n: 42 });
    expect(d.support[0].w).toBe(1);
    expect(d.normalized).toBe(true);
  });

  it("A2: distNormalize normalizes weights", () => {
    const d = distFrom([
      { v: { tag: "Num", n: 1 }, w: 2 },
      { v: { tag: "Num", n: 2 }, w: 2 },
    ]);
    const dn = distNormalize(d);
    expect(dn.normalized).toBe(true);
    expect(dn.support[0].w).toBe(0.5);
    expect(dn.support[1].w).toBe(0.5);
  });

  it("A3: distSample samples from distribution deterministically", () => {
    const d = distFrom([
      { v: { tag: "Num", n: 1 }, w: 1 },
      { v: { tag: "Num", n: 2 }, w: 1 },
    ]);
    // Same seed should give same result
    const v1 = distSample(d, 12345);
    const v2 = distSample(d, 12345);
    expect(v1).toEqual(v2);
  });

  it("A4: distTopK returns top k items by weight", () => {
    const d = distFrom([
      { v: { tag: "Num", n: 1 }, w: 0.1 },
      { v: { tag: "Num", n: 2 }, w: 0.5 },
      { v: { tag: "Num", n: 3 }, w: 0.4 },
    ]);
    const top = distTopK(d, 2);
    expect(top.support.length).toBe(2);
    // After normalization and sorting, top 2 should be n=2 (0.5) and n=3 (0.4)
    expect(top.support[0].v).toEqual({ tag: "Num", n: 2 });
    expect(top.support[1].v).toEqual({ tag: "Num", n: 3 });
  });

  it("A5: isDist correctly identifies Dist values", () => {
    const d = dist({ tag: "Num", n: 42 });
    expect(isDist(d)).toBe(true);
    expect(isDist({ tag: "Num", n: 42 })).toBe(false);
  });
});

describe("Patch Set A: Meaning as first-class value", () => {
  it("A6: meaning() creates a structured Meaning value", () => {
    const m = meaning({
      denotation: { tag: "Num", n: 42 },
      confidence: 0.9,
    });
    expect(m.tag).toBe("Meaning");
    expect(m.denotation).toEqual({ tag: "Num", n: 42 });
    expect(m.confidence).toBe(0.9);
  });

  it("A7: isMeaning correctly identifies Meaning values", () => {
    const m = meaning({ denotation: { tag: "Num", n: 42 } });
    expect(isMeaning(m)).toBe(true);
    expect(isMeaning({ tag: "Num", n: 42 })).toBe(false);
  });

  it("A8: Meaning supports all spec fields", () => {
    const m: MeaningVal = meaning({
      denotation: { tag: "Num", n: 42 },
      residual: { tag: "Str", s: "code" },
      rewrite: { tag: "Str", s: "optimized" },
      invariants: { tag: "Bool", b: true },
      effects: { tag: "Vector", items: [] },
      cost: { tag: "Num", n: 10 },
      paths: { tag: "Num", n: 3 },
      deps: { tag: "Vector", items: [] },
      memo: { tag: "Map", entries: [] },
      evidence: { tag: "Vector", items: [] },
      obligation: { tag: "Bool", b: true },
      confidence: 0.95,
      trace: { tag: "Str", s: "execution trace" },
      adoptEnvRef: "env:1",
      adoptStateRef: "state:1",
    });
    expect(m.tag).toBe("Meaning");
    expect(m.residual).toBeDefined();
    expect(m.rewrite).toBeDefined();
    expect(m.adoptEnvRef).toBe("env:1");
  });
});

describe("Patch Set B: int.op returns Meaning", () => {
  it("B1: int.op returns full Meaning value", async () => {
    const result = await evalOmega(`(effect int.op "what is 2+2?")`);
    expect(isMeaning(result)).toBe(true);
    expect((result as MeaningVal).denotation).toEqual({ tag: "Num", n: 42 });
  });

  it("B2: infer.op returns denotation directly (backward compatible)", async () => {
    const result = await evalOmega(`(effect infer.op "what is 2+2?")`);
    expect(result).toBe(42);
  });
});

describe("Patch Set C: AST Matching (matchAST)", () => {
  it("C1: exact match works", () => {
    const { ok, bindings } = matchAST(
      { tag: "App", fn: { tag: "Var", name: "+" } },
      { tag: "App", fn: { tag: "Var", name: "+" } }
    );
    expect(ok).toBe(true);
    expect(Object.keys(bindings).length).toBe(0);
  });

  it("C2: wildcard _ matches anything", () => {
    const { ok } = matchAST("_", { tag: "Num", n: 42 });
    expect(ok).toBe(true);
  });

  it("C3: variable binder ?x captures value", () => {
    const { ok, bindings } = matchAST(
      { tag: "App", fn: "?f" },
      { tag: "App", fn: { tag: "Var", name: "plus" } }
    );
    expect(ok).toBe(true);
    expect(bindings.f).toEqual({ tag: "Var", name: "plus" });
  });

  it("C4: same binder must match same value", () => {
    const { ok } = matchAST(
      { a: "?x", b: "?x" },
      { a: 1, b: 2 }
    );
    expect(ok).toBe(false);

    const { ok: ok2 } = matchAST(
      { a: "?x", b: "?x" },
      { a: 1, b: 1 }
    );
    expect(ok2).toBe(true);
  });

  it("C5: pattern with {tag:Var, name:'?x'} captures entire node", () => {
    // When the pattern has ?fn in name field, it captures the whole node
    const { ok, bindings } = matchAST(
      { tag: "App", fn: { tag: "Var", name: "?fn" } },
      { tag: "App", fn: { tag: "Var", name: "plus" } }
    );
    expect(ok).toBe(true);
    // The whole {tag: "Var", name: "plus"} is captured because ?fn in name makes it a binder
    expect(bindings.fn).toEqual({ tag: "Var", name: "plus" });
  });

  it("C6: arrays match element-wise", () => {
    const { ok, bindings } = matchAST(
      ["?a", "?b"],
      [1, 2]
    );
    expect(ok).toBe(true);
    expect(bindings.a).toBe(1);
    expect(bindings.b).toBe(2);
  });

  it("C7: tag mismatch fails", () => {
    const { ok } = matchAST(
      { tag: "App" },
      { tag: "Lambda" }
    );
    expect(ok).toBe(false);
  });
});

describe("Patch Set C: Context Receipts", () => {
  it("C8: snapshot creates receipt with envRef", () => {
    const repo = new CtxReceiptRepo();
    const r = repo.snapshot("env:1", "state:1", { note: "test" });
    expect(r.tag).toBe("CtxReceipt");
    expect(r.kind).toBe("snapshot");
    expect(r.envRef).toBe("env:1");
    expect(r.stateRef).toBe("state:1");
    expect(repo.has(r.rid)).toBe(true);
  });

  it("C9: compress creates receipt", () => {
    const repo = new CtxReceiptRepo();
    const r = repo.compress("env:2", { note: "compressed" });
    expect(r.kind).toBe("compress");
    expect(repo.get(r.rid)).toBeDefined();
  });

  it("C10: get returns stored receipt", () => {
    const repo = new CtxReceiptRepo();
    const r = repo.snapshot("env:3", undefined, {});
    const retrieved = repo.get(r.rid);
    expect(retrieved).toEqual(r);
  });

  it("C11: get returns undefined for unknown rid", () => {
    const repo = new CtxReceiptRepo();
    expect(repo.get("unknown-rid")).toBeUndefined();
  });
});

describe("Patch Set D: Governance (caps, budgets)", () => {
  // Note: These are unit tests for the governance modules
  // Integration tests would require setting up a full runtime

  it("D1: capRequire passes for wildcard", async () => {
    const { capRequire } = await import("../../src/core/governance/caps");
    // Should not throw
    expect(() => capRequire(["*"], "any.cap", "test")).not.toThrow();
  });

  it("D2: capRequire passes for exact match", async () => {
    const { capRequire } = await import("../../src/core/governance/caps");
    expect(() => capRequire(["eval", "apply"], "eval", "test")).not.toThrow();
  });

  it("D3: capRequire throws for missing cap", async () => {
    const { capRequire } = await import("../../src/core/governance/caps");
    expect(() => capRequire(["eval"], "commit", "test")).toThrow(/capability denied/);
  });

  it("D4: capRequire passes for domain wildcard", async () => {
    const { capRequire } = await import("../../src/core/governance/caps");
    expect(() => capRequire(["tool.*"], "tool.read", "test")).not.toThrow();
  });

  it("D5: budget tracks oracle turns", async () => {
    const { budgetDefault, budgetConsumeOracleTurn } = await import("../../src/core/governance/budgets");
    let b = budgetDefault({ maxOracleTurns: 3 });
    b = budgetConsumeOracleTurn(b);
    expect(b.consumed.oracleTurns).toBe(1);
    b = budgetConsumeOracleTurn(b);
    b = budgetConsumeOracleTurn(b);
    expect(() => budgetConsumeOracleTurn(b)).toThrow(/budget exhausted/);
  });

  it("D6: profiles have correct truth regimes", async () => {
    const { PROFILE_SPECULATIVE, PROFILE_TEST_CERTIFIED, PROFILE_PROOF_CERTIFIED } = await import("../../src/core/governance/profile");
    expect(PROFILE_SPECULATIVE.truth).toBe("speculative");
    expect(PROFILE_TEST_CERTIFIED.truth).toBe("test-certified");
    expect(PROFILE_PROOF_CERTIFIED.truth).toBe("proof-certified");
  });
});
