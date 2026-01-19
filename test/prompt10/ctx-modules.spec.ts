// test/prompt10/ctx-modules.spec.ts
// Tests for Prompt 10: Modules + Sealed Contexts + Capability Attenuation + Receipts + Compression/Hydration

import { describe, it, expect, beforeEach } from "vitest";
import {
  Ctx,
  ctxRootFromProfile,
  ctxDefine,
  ctxExtend,
  ctxSeal,
  ctxLookup,
  ctxAttenuateCaps,
  ctxExtendAttenuated,
  ctxHasCap,
  ctxRequireCap,
  ctxSealAsModule,
  ctxImportFromModule,
  ctxProject,
  ctxEstimateSize,
  ctxCompress,
  ctxHydrate,
  ctxHydrateById,
  CtxStore,
  globalCtxStore,
  DEFAULT_PROJECTION_SCHEMA,
  RESTRICTED_PROJECTION_SCHEMA,
  ProjectionSchema,
  CompressedCtx,
} from "../../src/core/ctx/ctx";
import {
  PROFILE_EXPLORE,
  PROFILE_PRAGMATIC,
  PROFILE_STRICT,
  PROFILE_AIRGAP,
  makeProfile,
} from "../../src/core/governance/profile";
import type { Val, CtxVal, ModuleVal, ReceiptRefVal } from "../../src/core/eval/values";

describe("Prompt 10: Modules + Sealed Contexts + Capability Attenuation", () => {
  // ─────────────────────────────────────────────────────────────────
  // Test 10.1: Ctx structure with sealed flag, caps, constraints
  // ─────────────────────────────────────────────────────────────────
  describe("10.1: Ctx structure", () => {
    it("creates Ctx from profile with correct structure", () => {
      const ctx = ctxRootFromProfile(PROFILE_PRAGMATIC);

      expect(ctx.tag).toBe("Ctx");
      expect(ctx.cid).toBeDefined();
      expect(ctx.sealed).toBe(false);
      expect(ctx.profile).toBe("pragmatic");
      expect(Array.isArray(ctx.caps)).toBe(true);
      expect(ctx.constraints).toBeDefined();
      expect(ctx.evidence).toEqual([]);
    });

    it("ctx has budgets from profile", () => {
      const ctx = ctxRootFromProfile(PROFILE_PRAGMATIC);

      expect(ctx.budgets).toBeDefined();
      expect(ctx.budgets.maxOracleTurns).toBe(PROFILE_PRAGMATIC.budgets.maxOracleTurns);
      expect(ctx.budgets.maxEvalSteps).toBe(PROFILE_PRAGMATIC.budgets.maxEvalSteps);
    });

    it("ctx has caps from profile", () => {
      const ctx = ctxRootFromProfile(PROFILE_EXPLORE);

      // PROFILE_EXPLORE has caps: ["cap.eval", "cap.apply", "cap.observe", "cap.infer"]
      // makeProfile generates both cap.X and X versions for legacy compatibility
      expect(ctx.caps).toContain("eval");
      expect(ctx.caps).toContain("apply");
      expect(ctx.caps).toContain("observe");
    });

    it("ctx is content-addressed (cid changes on modification)", () => {
      const ctx1 = ctxRootFromProfile(PROFILE_PRAGMATIC);
      const ctx2 = ctxDefine(ctx1, "x", 1);

      expect(ctx1.cid).not.toBe(ctx2.cid);
    });
  });

  // ─────────────────────────────────────────────────────────────────
  // Test 10.2: Sealing contexts
  // ─────────────────────────────────────────────────────────────────
  describe("10.2: Context sealing", () => {
    it("ctxSeal sets sealed flag and adds Sealed constraint", () => {
      const ctx = ctxRootFromProfile(PROFILE_PRAGMATIC);
      expect(ctx.sealed).toBe(false);

      const sealed = ctxSeal(ctx);
      expect(sealed.sealed).toBe(true);
      expect(sealed.constraints.some(c => c.tag === "Sealed")).toBe(true);
    });

    it("sealed context throws on ctxDefine", () => {
      const ctx = ctxRootFromProfile(PROFILE_PRAGMATIC);
      const sealed = ctxSeal(ctx);

      expect(() => ctxDefine(sealed, "x", 1)).toThrow(/sealed/i);
    });

    it("sealing is idempotent", () => {
      const ctx = ctxRootFromProfile(PROFILE_PRAGMATIC);
      const sealed1 = ctxSeal(ctx);
      const sealed2 = ctxSeal(sealed1);

      expect(sealed1.cid).toBe(sealed2.cid);
      expect(sealed2.sealed).toBe(true);
    });

    it("child of sealed context inherits sealed flag", () => {
      const ctx = ctxRootFromProfile(PROFILE_PRAGMATIC);
      const sealed = ctxSeal(ctx);
      const child = ctxExtend(sealed, [["y", 2]]);

      expect(child.sealed).toBe(true);
    });
  });

  // ─────────────────────────────────────────────────────────────────
  // Test 10.3: Capability attenuation
  // ─────────────────────────────────────────────────────────────────
  describe("10.3: Capability attenuation", () => {
    it("ctxHasCap checks capability presence", () => {
      const ctx = ctxRootFromProfile(PROFILE_EXPLORE);

      expect(ctxHasCap(ctx, "eval")).toBe(true);
      expect(ctxHasCap(ctx, "nonexistent")).toBe(false);
    });

    it("wildcard cap grants all capabilities", () => {
      const profile = makeProfile({
        name: "wildcard-test",
        caps: ["*"],
        ops: [],
        oracleReqs: [],
        budgets: { maxOracleTurns: 100, maxEvalSteps: 1000, maxToolCalls: 0 },
        truth: "speculative",
      });
      const ctx = ctxRootFromProfile(profile);

      expect(ctxHasCap(ctx, "anything")).toBe(true);
      expect(ctxHasCap(ctx, "cap.any.thing")).toBe(true);
    });

    it("ctxAttenuateCaps restricts capabilities (intersection)", () => {
      const ctx = ctxRootFromProfile(PROFILE_EXPLORE);
      // PROFILE_EXPLORE has: eval, apply, observe, infer

      const attenuated = ctxAttenuateCaps(ctx, ["eval", "observe"]);

      expect(ctxHasCap(attenuated, "eval")).toBe(true);
      expect(ctxHasCap(attenuated, "observe")).toBe(true);
      expect(ctxHasCap(attenuated, "apply")).toBe(false);
      expect(ctxHasCap(attenuated, "infer")).toBe(false);
    });

    it("attenuation cannot expand capabilities", () => {
      const ctx = ctxRootFromProfile(PROFILE_AIRGAP);
      // PROFILE_AIRGAP has limited caps

      const attenuated = ctxAttenuateCaps(ctx, ["eval", "apply", "tool", "commit"]);

      // Can only have caps that were already present
      expect(ctxHasCap(attenuated, "tool")).toBe(false);
      expect(ctxHasCap(attenuated, "commit")).toBe(false);
    });

    it("ctxExtendAttenuated creates child with restricted caps", () => {
      const ctx = ctxRootFromProfile(PROFILE_EXPLORE);
      const child = ctxExtendAttenuated(ctx, [["x", 1]], ["eval"]);

      expect(ctxLookup(child, "x")).toBe(1);
      expect(ctxHasCap(child, "eval")).toBe(true);
      expect(ctxHasCap(child, "apply")).toBe(false);
    });

    it("ctxRequireCap throws on missing capability", () => {
      const ctx = ctxRootFromProfile(PROFILE_AIRGAP);

      expect(() => ctxRequireCap(ctx, "tool", "test")).toThrow(/capability denied.*tool/i);
    });
  });

  // ─────────────────────────────────────────────────────────────────
  // Test 10.4: Module sealing
  // ─────────────────────────────────────────────────────────────────
  describe("10.4: Module sealing", () => {
    it("ctxSealAsModule creates module with exports", () => {
      const ctx = ctxRootFromProfile(PROFILE_PRAGMATIC);
      const withBindings = ctxDefine(ctxDefine(ctx, "pub", 1), "priv", 2);

      const mod = ctxSealAsModule(withBindings, ["pub"]);

      expect(mod.moduleId).toBeDefined();
      expect(mod.sealedCtx.sealed).toBe(true);
      expect(mod.exports.has("pub")).toBe(true);
      expect(mod.exports.has("priv")).toBe(false);
    });

    it("module has content-addressed ID", () => {
      const ctx = ctxRootFromProfile(PROFILE_PRAGMATIC);
      const mod1 = ctxSealAsModule(ctxDefine(ctx, "x", 1), ["x"]);
      const mod2 = ctxSealAsModule(ctxDefine(ctx, "x", 1), ["x"]);

      // Same context + exports = same module ID
      expect(mod1.moduleId).toBe(mod2.moduleId);
    });

    it("module metadata is optional", () => {
      const ctx = ctxRootFromProfile(PROFILE_PRAGMATIC);
      const mod = ctxSealAsModule(ctx, [], { name: "test-module", version: "1.0.0" });

      expect(mod.meta?.name).toBe("test-module");
      expect(mod.meta?.version).toBe("1.0.0");
    });
  });

  // ─────────────────────────────────────────────────────────────────
  // Test 10.5: Module imports
  // ─────────────────────────────────────────────────────────────────
  describe("10.5: Module imports", () => {
    it("ctxImportFromModule allows importing exported names", () => {
      const ctx = ctxRootFromProfile(PROFILE_PRAGMATIC);
      const withBindings = ctxDefine(ctxDefine(ctx, "pub", 42), "priv", 99);
      const mod = ctxSealAsModule(withBindings, ["pub"]);

      const addr = ctxImportFromModule(ctx, mod.sealedCtx, mod.exports, "pub");
      expect(addr).toBe(42);
    });

    it("ctxImportFromModule denies non-exported names", () => {
      const ctx = ctxRootFromProfile(PROFILE_PRAGMATIC);
      const withBindings = ctxDefine(ctxDefine(ctx, "pub", 42), "priv", 99);
      const mod = ctxSealAsModule(withBindings, ["pub"]);

      expect(() => ctxImportFromModule(ctx, mod.sealedCtx, mod.exports, "priv"))
        .toThrow(/not exported/i);
    });

    it("importing non-existent name returns undefined", () => {
      const ctx = ctxRootFromProfile(PROFILE_PRAGMATIC);
      const mod = ctxSealAsModule(ctx, ["nonexistent"]);

      // Name is in exports but not actually bound
      const addr = ctxImportFromModule(ctx, mod.sealedCtx, mod.exports, "nonexistent");
      expect(addr).toBeUndefined();
    });
  });

  // ─────────────────────────────────────────────────────────────────
  // Test 10.6: ProjectionSchema for ReqObserve
  // ─────────────────────────────────────────────────────────────────
  describe("10.6: ProjectionSchema", () => {
    it("DEFAULT_PROJECTION_SCHEMA includes all fields", () => {
      expect(DEFAULT_PROJECTION_SCHEMA.includeCid).toBe(true);
      expect(DEFAULT_PROJECTION_SCHEMA.includeCaps).toBe(true);
      expect(DEFAULT_PROJECTION_SCHEMA.includeBudgets).toBe(true);
      expect(DEFAULT_PROJECTION_SCHEMA.includeConstraints).toBe(true);
    });

    it("RESTRICTED_PROJECTION_SCHEMA redacts sensitive data", () => {
      expect(RESTRICTED_PROJECTION_SCHEMA.includeCaps).toBe(false);
      expect(RESTRICTED_PROJECTION_SCHEMA.includeBudgets).toBe(false);
      expect(RESTRICTED_PROJECTION_SCHEMA.redactKeys).toContain("secret");
      expect(RESTRICTED_PROJECTION_SCHEMA.redactKeys).toContain("password");
    });

    it("ctxProject uses schema to control output", () => {
      const ctx = ctxRootFromProfile(PROFILE_PRAGMATIC);

      const fullProj = ctxProject(ctx, DEFAULT_PROJECTION_SCHEMA) as Record<string, unknown>;
      expect(fullProj.caps).toBeDefined();
      expect(fullProj.budgets).toBeDefined();

      const restrictedProj = ctxProject(ctx, RESTRICTED_PROJECTION_SCHEMA) as Record<string, unknown>;
      expect(restrictedProj.caps).toBeUndefined();
      expect(restrictedProj.budgets).toBeUndefined();
    });

    it("ctxProject redacts keys matching patterns", () => {
      const ctx = ctxRootFromProfile(PROFILE_PRAGMATIC);
      const withSecrets = ctxDefine(ctxDefine(ctx, "secret_key", 1), "public_data", 2);

      const proj = ctxProject(withSecrets, RESTRICTED_PROJECTION_SCHEMA) as Record<string, unknown>;
      const keys = proj.frameKeys as string[];

      expect(keys).toContain("public_data");
      expect(keys).not.toContain("secret_key");
    });

    it("ctxProject clips keys to maxKeys", () => {
      let ctx = ctxRootFromProfile(PROFILE_PRAGMATIC);
      for (let i = 0; i < 100; i++) {
        ctx = ctxDefine(ctx, `key_${i}`, i);
      }

      const schema: ProjectionSchema = { maxKeys: 10 };
      const proj = ctxProject(ctx, schema) as Record<string, unknown>;
      const keys = proj.frameKeys as string[];

      expect(keys.length).toBeLessThanOrEqual(11); // 10 + "…"
      expect(keys[keys.length - 1]).toBe("…");
    });
  });

  // ─────────────────────────────────────────────────────────────────
  // Test 10.7: Compression/Hydration
  // ─────────────────────────────────────────────────────────────────
  describe("10.7: Compression and Hydration", () => {
    let store: CtxStore;

    beforeEach(() => {
      store = new CtxStore();
    });

    it("ctxCompress creates lightweight representation", () => {
      const ctx = ctxRootFromProfile(PROFILE_PRAGMATIC);
      const compressed = ctxCompress(ctx);

      expect(compressed.tag).toBe("CompressedCtx");
      expect(compressed.cid).toBe(ctx.cid);
      expect(compressed.profile).toBe("pragmatic");
      expect(compressed.sealed).toBe(false);
      expect(typeof compressed.estimatedBytes).toBe("number");
    });

    it("ctxEstimateSize returns reasonable estimate", () => {
      const ctx = ctxRootFromProfile(PROFILE_PRAGMATIC);
      const size1 = ctxEstimateSize(ctx);

      // Add some bindings
      let ctx2 = ctx;
      for (let i = 0; i < 10; i++) {
        ctx2 = ctxDefine(ctx2, `key_${i}`, i);
      }
      const size2 = ctxEstimateSize(ctx2);

      expect(size2).toBeGreaterThan(size1);
    });

    it("CtxStore put/get roundtrips", () => {
      const ctx = ctxRootFromProfile(PROFILE_PRAGMATIC);
      const cid = store.put(ctx);

      expect(store.has(cid)).toBe(true);
      expect(store.get(cid)).toBe(ctx);
    });

    it("ctxHydrate restores context from store", () => {
      const ctx = ctxRootFromProfile(PROFILE_PRAGMATIC);
      store.put(ctx);

      const compressed = ctxCompress(ctx);
      const hydrated = ctxHydrate(compressed, store);

      expect(hydrated).toBe(ctx);
      expect(hydrated.cid).toBe(ctx.cid);
    });

    it("ctxHydrateById restores by cid directly", () => {
      const ctx = ctxRootFromProfile(PROFILE_PRAGMATIC);
      const cid = store.put(ctx);

      const hydrated = ctxHydrateById(cid, store);
      expect(hydrated).toBe(ctx);
    });

    it("ctxHydrate throws if context not in store", () => {
      const ctx = ctxRootFromProfile(PROFILE_PRAGMATIC);
      const compressed = ctxCompress(ctx);
      // Don't store it

      expect(() => ctxHydrate(compressed, store)).toThrow(/not found in store/i);
    });

    it("compressed.capSummary reflects capability level", () => {
      const fullCaps = makeProfile({
        name: "full",
        caps: ["*"],
        ops: [],
        oracleReqs: [],
        budgets: { maxOracleTurns: 100, maxEvalSteps: 1000, maxToolCalls: 0 },
        truth: "speculative",
      });
      const noCaps = makeProfile({
        name: "none",
        caps: [],
        ops: [],
        oracleReqs: [],
        budgets: { maxOracleTurns: 100, maxEvalSteps: 1000, maxToolCalls: 0 },
        truth: "speculative",
      });

      const ctxFull = ctxRootFromProfile(fullCaps);
      const ctxNone = ctxRootFromProfile(noCaps);
      const ctxRestricted = ctxRootFromProfile(PROFILE_AIRGAP);

      expect(ctxCompress(ctxFull).capSummary).toBe("full");
      expect(ctxCompress(ctxNone).capSummary).toBe("none");
      expect(ctxCompress(ctxRestricted).capSummary).toBe("restricted");
    });
  });

  // ─────────────────────────────────────────────────────────────────
  // Test 10.8: Val types for Ctx, Module, ReceiptRef
  // ─────────────────────────────────────────────────────────────────
  describe("10.8: Val types integration", () => {
    it("CtxVal can wrap a Ctx", () => {
      const ctx = ctxRootFromProfile(PROFILE_PRAGMATIC);
      const ctxVal: CtxVal = { tag: "Ctx", ctx };

      expect(ctxVal.tag).toBe("Ctx");
      expect(ctxVal.ctx.cid).toBe(ctx.cid);
    });

    it("ModuleVal has correct structure", () => {
      const ctx = ctxRootFromProfile(PROFILE_PRAGMATIC);
      const mod = ctxSealAsModule(ctxDefine(ctx, "x", 1), ["x"]);

      const modVal: ModuleVal = {
        tag: "Module",
        moduleId: mod.moduleId,
        sealedCtx: mod.sealedCtx,
        exports: mod.exports,
        meta: { name: "test" },
      };

      expect(modVal.tag).toBe("Module");
      expect(modVal.moduleId).toBe(mod.moduleId);
      expect(modVal.exports.has("x")).toBe(true);
    });

    it("ReceiptRefVal tracks receipt kind", () => {
      const snapshotRef: ReceiptRefVal = {
        tag: "ReceiptRef",
        rid: "sha256:abc123",
        kind: "snapshot",
      };

      const compressRef: ReceiptRefVal = {
        tag: "ReceiptRef",
        rid: "sha256:def456",
        kind: "compress",
      };

      expect(snapshotRef.kind).toBe("snapshot");
      expect(compressRef.kind).toBe("compress");
    });

    it("Val union accepts Prompt 10 types", () => {
      const ctx = ctxRootFromProfile(PROFILE_PRAGMATIC);

      // These should all be valid Val assignments
      const vals: Val[] = [
        { tag: "Ctx", ctx },
        {
          tag: "Module",
          moduleId: "sha256:test",
          sealedCtx: ctxSeal(ctx),
          exports: new Set(["x"]),
        },
        { tag: "ReceiptRef", rid: "sha256:test", kind: "snapshot" },
      ];

      expect(vals.length).toBe(3);
      expect(vals[0].tag).toBe("Ctx");
      expect(vals[1].tag).toBe("Module");
      expect(vals[2].tag).toBe("ReceiptRef");
    });
  });

  // ─────────────────────────────────────────────────────────────────
  // Additional integration tests
  // ─────────────────────────────────────────────────────────────────
  describe("Integration: capability-attenuated module import", () => {
    it("importing from module respects caller's caps", () => {
      // Create a module with full caps
      const fullCtx = ctxRootFromProfile(PROFILE_PRAGMATIC);
      const modCtx = ctxDefine(fullCtx, "sensitive_fn", 999);
      const mod = ctxSealAsModule(modCtx, ["sensitive_fn"]);

      // Caller with restricted caps
      const restrictedCtx = ctxRootFromProfile(PROFILE_AIRGAP);

      // Import succeeds (module boundary allows import)
      const addr = ctxImportFromModule(restrictedCtx, mod.sealedCtx, mod.exports, "sensitive_fn");
      expect(addr).toBe(999);

      // But caller's context still has restricted caps
      expect(ctxHasCap(restrictedCtx, "tool")).toBe(false);
    });
  });
});
