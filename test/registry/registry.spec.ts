import { describe, it, expect, beforeEach } from "vitest";
import {
  PrimitiveRegistry,
  defaultRegistry,
  apropos,
  filterByEffect,
  filterByLayer,
  findPrimitive,
} from "../../src/registry";
import { frameLispDescriptors } from "../../src/registry/descriptors/framelisp";
import type { PrimitiveDescriptor } from "../../src/registry/types";
import { validateDescriptor, validateRegistry } from "../../src/registry/validate";

const baseDescriptor: PrimitiveDescriptor = {
  id: "framelisp/test",
  layer: "FrameLisp",
  kind: "Function",
  signature: {
    params: [{ name: "x", type: "Int" }],
    returns: "Int",
  },
  effects: ["Pure"],
  doc: { summary: "Test primitive" },
  lowering: { kind: "Intrinsic", irTag: "FTest" },
  version: "1.0.0",
};

function makeDescriptor(overrides: Partial<PrimitiveDescriptor> = {}): PrimitiveDescriptor {
  const hasSignature = Object.prototype.hasOwnProperty.call(overrides, "signature");
  const hasEffects = Object.prototype.hasOwnProperty.call(overrides, "effects");
  const hasDoc = Object.prototype.hasOwnProperty.call(overrides, "doc");
  const hasLowering = Object.prototype.hasOwnProperty.call(overrides, "lowering");
  const hasConstraints = Object.prototype.hasOwnProperty.call(overrides, "constraints");

  return {
    ...baseDescriptor,
    ...overrides,
    signature: hasSignature
      ? overrides.signature as PrimitiveDescriptor["signature"]
      : {
          params: [...baseDescriptor.signature.params],
          returns: baseDescriptor.signature.returns,
        },
    effects: hasEffects ? overrides.effects as PrimitiveDescriptor["effects"] : [...baseDescriptor.effects],
    doc: hasDoc ? overrides.doc as PrimitiveDescriptor["doc"] : { ...baseDescriptor.doc },
    lowering: hasLowering ? overrides.lowering : baseDescriptor.lowering,
    constraints: hasConstraints ? overrides.constraints : baseDescriptor.constraints,
  };
}

describe("PrimitiveRegistry", () => {
  let registry: PrimitiveRegistry;

  beforeEach(() => {
    registry = new PrimitiveRegistry();
  });

  describe("Happy Path", () => {
    it("registers and retrieves descriptors", () => {
      const desc = makeDescriptor();
      registry.register(desc);

      const retrieved = registry.get(desc.id);
      expect(retrieved).toEqual(desc);
      expect(registry.getAll()).toHaveLength(1);
    });

    it("queries descriptors by layer and effect", () => {
      const pure = makeDescriptor({ id: "framelisp/pure", effects: ["Pure"] });
      const oracle = makeDescriptor({
        id: "framelisp/infer",
        effects: ["Oracle"],
        constraints: { mustBeDominatedByBudget: true },
        doc: { summary: "LLM inference" },
      });

      registry.register(pure);
      registry.register(oracle);

      const frameLisp = registry.getByLayer("FrameLisp");
      expect(frameLisp.map(d => d.id)).toEqual(expect.arrayContaining([pure.id, oracle.id]));

      const oracleEffects = registry.getByEffect("Oracle");
      expect(oracleEffects.map(d => d.id)).toContain(oracle.id);
    });

    it("supports full-text search and IR tag lookup", () => {
      const desc = makeDescriptor({
        id: "framelisp/bind",
        doc: { summary: "Monadic bind operation" },
        lowering: { kind: "Intrinsic", irTag: "FBind" },
        effects: ["Control"],
      });
      registry.register(desc);

      const results = registry.search("monadic");
      expect(results.map(d => d.id)).toContain(desc.id);

      const byIrTag = registry.getByIrTag("FBind");
      expect(byIrTag?.id).toBe(desc.id);
    });

    it("hydrates from JSON and preserves indexes", () => {
      const desc = makeDescriptor({ id: "framelisp/another", effects: ["Oracle"], constraints: { mustBeDominatedByBudget: true } });
      registry.register(desc);

      const cloned = PrimitiveRegistry.fromJSON(registry.toJSON());
      expect(cloned.get(desc.id)?.id).toBe(desc.id);
      expect(cloned.getByEffect("Oracle").map(d => d.id)).toContain(desc.id);
      expect(cloned.getByLayer("FrameLisp").map(d => d.id)).toContain(desc.id);
    });

    it("exposes helper queries", () => {
      const desc = makeDescriptor({
        id: "framelisp/catch",
        doc: { summary: "Catch failure" },
        effects: ["Control"],
        lowering: { kind: "Intrinsic", irTag: "FCatch" },
      });
      registry.register(desc);

      expect(findPrimitive(registry, desc.id)?.id).toBe(desc.id);
      expect(filterByEffect(registry, "Control").map(d => d.id)).toContain(desc.id);
      expect(filterByLayer(registry, "FrameLisp").map(d => d.id)).toContain(desc.id);
      expect(apropos(registry, "catch").map(d => d.id)).toContain(desc.id);
    });
  });

  describe("Edge Cases", () => {
    it("throws on duplicate registration", () => {
      const desc = makeDescriptor();
      registry.register(desc);
      expect(() => registry.register(desc)).toThrow(/already registered/i);
    });

    it("returns undefined for unknown ids and empty lists for missing indexes", () => {
      expect(registry.get("framelisp/missing")).toBeUndefined();
      expect(registry.getByLayer("LambdaLLM")).toEqual([]);
      expect(registry.getByEffect("Oracle")).toEqual([]);
      expect(registry.search("nope")).toEqual([]);
    });

    it("matches search in a case-insensitive manner", () => {
      const desc = makeDescriptor({ id: "framelisp/log", doc: { summary: "Emit LOG output" } });
      registry.register(desc);

      expect(registry.search("log")).toHaveLength(1);
      expect(registry.search("LOG")).toHaveLength(1);
    });
  });

  describe("Validation", () => {
    it("detects missing required fields", () => {
      const invalid = makeDescriptor({
        id: "framelisp/invalid",
        // @ts-expect-error intentional invalid descriptor for validation
        signature: undefined,
        // @ts-expect-error intentional invalid descriptor for validation
        doc: {},
        version: "",
      });
      registry.register(invalid as PrimitiveDescriptor);

      const { valid, errors } = registry.validate();
      expect(valid).toBe(false);
      expect(errors.join(" ")).toMatch(/signature/);
      expect(errors.join(" ")).toMatch(/doc\.summary/);
      expect(errors.join(" ")).toMatch(/version/);
    });

    it("enforces effect-specific constraints", () => {
      const oracle = makeDescriptor({
        id: "framelisp/infer",
        effects: ["Oracle"],
        constraints: {},
      });
      const tool = makeDescriptor({
        id: "framelisp/call-tool",
        effects: ["Tool"],
        constraints: {},
      });
      registry.register(oracle);
      registry.register(tool);

      const results = validateRegistry(registry);
      expect(results.valid).toBe(false);
      expect(results.errors.some(e => e.includes("Oracle"))).toBe(true);
      expect(results.errors.some(e => e.includes("Tool"))).toBe(true);
    });

    it("validates individual descriptors", () => {
      const valid = makeDescriptor({
        id: "framelisp/infer",
        effects: ["Oracle"],
        constraints: { mustBeDominatedByBudget: true },
      });
      const oracleResult = validateDescriptor(valid);
      expect(oracleResult.valid).toBe(true);
      expect(oracleResult.errors).toHaveLength(0);
    });
  });

  describe("Default registry seed", () => {
    it("registers core FrameLisp primitives", () => {
      const ids = frameLispDescriptors.map(d => d.id);
      expect(ids).toContain("framelisp/infer");
      expect(ids).toContain("framelisp/bind");

      const seeded = defaultRegistry.getByLayer("FrameLisp").map(d => d.id);
      expect(seeded).toEqual(expect.arrayContaining(ids));
    });
  });
});
