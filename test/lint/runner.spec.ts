import { describe, it, expect } from "vitest";
import { LintRunner } from "../../src/lint/runner";
import type { Pass } from "../../src/lint/types";
import type { IRBundle } from "../../src/frameir/bundle";
import type { FlowIR, FPure, FWithBudget } from "../../src/frameir/flow";
import { CURRENT_IR_VERSION } from "../../src/frameir/version";
import { errorDiag } from "../../src/outcome/diagnostic";
import { PrimitiveRegistry } from "../../src/registry/registry";

const V = CURRENT_IR_VERSION;

describe("LintRunner", () => {
  it("runs passes in phase order then id order", () => {
    const calls: string[] = [];
    const runner = new LintRunner({}, new PrimitiveRegistry());
    runner.register(makePass("lint/b", "lint", () => { calls.push("b"); return { diagnostics: [] }; }));
    runner.register(makePass("normalize/a", "normalize", () => { calls.push("a"); return { diagnostics: [] }; }));
    runner.register(makePass("lint/a", "lint", () => { calls.push("c"); return { diagnostics: [] }; }));

    runner.run(makeBundle(pureFlow()));
    expect(calls).toEqual(["a", "c", "b"]);
  });

  it("applies severity overrides", () => {
    const runner = new LintRunner({ passes: { "lint/error": { enabled: true, severityOverride: "warning" } } }, new PrimitiveRegistry());
    runner.register(makePass("lint/error", "lint", () => ({ diagnostics: [errorDiag("X", "oops")] })));

    const { diagnostics } = runner.run(makeBundle(pureFlow()));
    expect(diagnostics).toHaveLength(1);
    expect(diagnostics[0].severity).toBe("warning");
  });

  it("skips disabled passes", () => {
    let ran = false;
    const runner = new LintRunner({ passes: { "lint/skip": { enabled: false } } }, new PrimitiveRegistry());
    runner.register(makePass("lint/skip", "lint", () => { ran = true; return { diagnostics: [] }; }));

    const { diagnostics } = runner.run(makeBundle(pureFlow()));
    expect(ran).toBe(false);
    expect(diagnostics).toHaveLength(0);
  });

  it("feeds transformed bundle into subsequent passes", () => {
    const seen: string[] = [];
    const runner = new LintRunner({}, new PrimitiveRegistry());

    runner.register(makePass("normalize/transform", "normalize", bundle => {
      seen.push(bundle.entry.tag);
      const wrapped: FWithBudget = { v: V, tag: "FWithBudget", budget: { v: V, tag: "VInt", value: "1" }, flow: bundle.entry };
      return { diagnostics: [], transformed: { ...bundle, entry: wrapped } };
    }));

    runner.register(makePass("lint/check", "lint", bundle => {
      seen.push(bundle.entry.tag);
      return { diagnostics: [] };
    }));

    runner.run(makeBundle(pureFlow()));
    expect(seen).toEqual(["FPure", "FWithBudget"]);
  });
});

function makePass(id: string, phase: Pass["phase"], run: Pass["run"]): Pass {
  return { id, name: id, phase, run };
}

function pureFlow(): FPure {
  return { v: V, tag: "FPure", value: { v: V, tag: "VNil" } };
}

function makeBundle(entry: FlowIR): IRBundle {
  return { v: V, entry, fns: {}, schemas: {}, toolContracts: {} };
}
