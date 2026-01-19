// test/core/governance/caps.spec.ts
// Tests for capability enforcement

import { describe, it, expect } from "vitest";
import {
  capRequire,
  capHas,
  DEFAULT_CAPS,
  FULL_CAPS,
} from "../../../src/core/governance/caps";

describe("capability system", () => {
  describe("capHas", () => {
    it("returns true for exact match", () => {
      expect(capHas(["eval", "apply"], "eval")).toBe(true);
      expect(capHas(["eval", "apply"], "apply")).toBe(true);
    });

    it("returns false for missing capability", () => {
      expect(capHas(["eval"], "apply")).toBe(false);
      expect(capHas([], "eval")).toBe(false);
    });

    it("wildcard * grants all capabilities", () => {
      expect(capHas(["*"], "eval")).toBe(true);
      expect(capHas(["*"], "apply")).toBe(true);
      expect(capHas(["*"], "tool.bash")).toBe(true);
      expect(capHas(["*"], "anything.at.all")).toBe(true);
    });

    it("domain wildcard grants all in domain", () => {
      expect(capHas(["tool.*"], "tool.bash")).toBe(true);
      expect(capHas(["tool.*"], "tool.fs.read")).toBe(true);
      expect(capHas(["tool.*"], "eval")).toBe(false);
    });
  });

  describe("capRequire", () => {
    it("does not throw when capability is present", () => {
      expect(() => capRequire(["eval"], "eval", "test")).not.toThrow();
    });

    it("throws when capability is missing", () => {
      expect(() => capRequire([], "eval", "test")).toThrow("capability denied");
      expect(() => capRequire(["apply"], "eval", "test")).toThrow("capability denied");
    });

    it("includes context in error message", () => {
      expect(() => capRequire([], "eval", "myContext")).toThrow("myContext");
    });

    it("wildcard * satisfies any capability", () => {
      expect(() => capRequire(["*"], "eval", "test")).not.toThrow();
      expect(() => capRequire(["*"], "tool.bash", "test")).not.toThrow();
    });

    it("domain wildcard satisfies domain capabilities", () => {
      expect(() => capRequire(["tool.*"], "tool.bash", "test")).not.toThrow();
      expect(() => capRequire(["tool.*"], "eval", "test")).toThrow();
    });
  });

  describe("predefined capability sets", () => {
    it("DEFAULT_CAPS includes basic capabilities", () => {
      expect(capHas(DEFAULT_CAPS, "eval")).toBe(true);
      expect(capHas(DEFAULT_CAPS, "apply")).toBe(true);
      expect(capHas(DEFAULT_CAPS, "observe")).toBe(true);
    });

    it("FULL_CAPS grants everything", () => {
      expect(capHas(FULL_CAPS, "eval")).toBe(true);
      expect(capHas(FULL_CAPS, "tool.bash")).toBe(true);
      expect(capHas(FULL_CAPS, "commit.rewrite")).toBe(true);
    });
  });
});
