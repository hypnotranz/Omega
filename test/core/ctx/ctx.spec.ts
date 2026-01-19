// test/core/ctx/ctx.spec.ts
// Unit tests for Ctx operations - to diagnose debugger test failures

import { describe, it, expect } from "vitest";
import {
  ctxRootFromProfile,
  ctxDefine,
  ctxLookup,
  ctxExtend
} from "../../../src/core/ctx/ctx";
import { DEFAULT_PROFILE } from "../../../src/core/governance/profile";

describe("Ctx operations", () => {
  describe("ctxDefine", () => {
    it("adds binding to frame", () => {
      const ctx = ctxRootFromProfile(DEFAULT_PROFILE);
      const ctx2 = ctxDefine(ctx, "x", "addr:1" as any);

      const result = ctxLookup(ctx2, "x");
      expect(result).toBe("addr:1");
    });

    it("preserves existing bindings", () => {
      const ctx = ctxRootFromProfile(DEFAULT_PROFILE);
      const ctx2 = ctxDefine(ctx, "x", "addr:1" as any);
      const ctx3 = ctxDefine(ctx2, "y", "addr:2" as any);

      expect(ctxLookup(ctx3, "x")).toBe("addr:1");
      expect(ctxLookup(ctx3, "y")).toBe("addr:2");
    });

    it("doesn't modify original ctx", () => {
      const ctx = ctxRootFromProfile(DEFAULT_PROFILE);
      const ctx2 = ctxDefine(ctx, "x", "addr:1" as any);

      expect(ctxLookup(ctx, "x")).toBeUndefined();
      expect(ctxLookup(ctx2, "x")).toBe("addr:1");
    });

    it("new Ctx has correct tag", () => {
      const ctx = ctxRootFromProfile(DEFAULT_PROFILE);
      const ctx2 = ctxDefine(ctx, "x", "addr:1" as any);

      expect(ctx2.tag).toBe("Ctx");
    });

    it("new Ctx has frame as Map", () => {
      const ctx = ctxRootFromProfile(DEFAULT_PROFILE);
      const ctx2 = ctxDefine(ctx, "x", "addr:1" as any);

      expect(ctx2.frame).toBeInstanceOf(Map);
      expect(ctx2.frame.get("x")).toBe("addr:1");
    });
  });

  describe("ctxLookup", () => {
    it("returns undefined for missing binding", () => {
      const ctx = ctxRootFromProfile(DEFAULT_PROFILE);
      expect(ctxLookup(ctx, "nonexistent")).toBeUndefined();
    });

    it("finds binding in current frame", () => {
      const ctx = ctxRootFromProfile(DEFAULT_PROFILE);
      const ctx2 = ctxDefine(ctx, "x", "addr:1" as any);

      expect(ctxLookup(ctx2, "x")).toBe("addr:1");
    });

    it("finds binding in parent frame", () => {
      const ctx = ctxRootFromProfile(DEFAULT_PROFILE);
      const ctx2 = ctxDefine(ctx, "x", "addr:1" as any);
      const ctx3 = ctxExtend(ctx2, [["y", "addr:2" as any]]);

      // x is in parent (ctx2), y is in current (ctx3)
      expect(ctxLookup(ctx3, "x")).toBe("addr:1");
      expect(ctxLookup(ctx3, "y")).toBe("addr:2");
    });
  });

  describe("integration: multiple defines", () => {
    it("handles sequential defines", () => {
      let ctx = ctxRootFromProfile(DEFAULT_PROFILE);
      ctx = ctxDefine(ctx, "a", "addr:1" as any);
      ctx = ctxDefine(ctx, "b", "addr:2" as any);
      ctx = ctxDefine(ctx, "c", "addr:3" as any);

      expect(ctxLookup(ctx, "a")).toBe("addr:1");
      expect(ctxLookup(ctx, "b")).toBe("addr:2");
      expect(ctxLookup(ctx, "c")).toBe("addr:3");
    });
  });
});
