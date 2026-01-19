// test/core/receipts/snapshots.spec.ts
// Tests for snapshot repository

import { describe, it, expect } from "vitest";
import { SnapshotRepo, type EnvSnapshot } from "../../../src/core/oracle/snapshots";

describe("SnapshotRepo", () => {
  describe("environment snapshots", () => {
    it("stores and retrieves env snapshots", () => {
      const repo = new SnapshotRepo();
      const snap: EnvSnapshot = {
        env: new Map([["x", { tag: "Num", n: 42 } as any]]),
        store: new Map(),
      };

      const ref = repo.putEnv(snap);
      const retrieved = repo.getEnv(ref);

      expect(retrieved).toBeDefined();
      expect(retrieved.env.get("x")).toEqual({ tag: "Num", n: 42 });
    });

    it("generates unique refs for each snapshot", () => {
      const repo = new SnapshotRepo();
      const snap1: EnvSnapshot = { env: new Map(), store: new Map() };
      const snap2: EnvSnapshot = { env: new Map(), store: new Map() };

      const ref1 = repo.putEnv(snap1);
      const ref2 = repo.putEnv(snap2);

      expect(ref1).not.toBe(ref2);
    });

    it("throws for missing env ref", () => {
      const repo = new SnapshotRepo();
      expect(() => repo.getEnv("nonexistent" as any)).toThrow("missing EnvRef");
    });
  });

  describe("state snapshots", () => {
    it("stores and retrieves state snapshots", () => {
      const repo = new SnapshotRepo();
      const mockState = {
        tag: "Eval" as const,
        ctrl: { tag: "Num", n: 1 } as any,
        env: new Map(),
        kont: [] as any[],
        store: new Map()
      };

      const ref = repo.putState({ state: mockState });
      const retrieved = repo.getState(ref);

      expect(retrieved).toBeDefined();
      expect(retrieved.state.tag).toBe("Eval");
    });

    it("generates unique refs for state snapshots", () => {
      const repo = new SnapshotRepo();
      const mockState = { tag: "Halt" as const, value: { tag: "Unit" } as any };

      const ref1 = repo.putState({ state: mockState as any });
      const ref2 = repo.putState({ state: mockState as any });

      expect(ref1).not.toBe(ref2);
    });

    it("throws for missing state ref", () => {
      const repo = new SnapshotRepo();
      expect(() => repo.getState("nonexistent" as any)).toThrow("missing StateRef");
    });
  });

  describe("ref formats", () => {
    it("env refs start with 'env:'", () => {
      const repo = new SnapshotRepo();
      const ref = repo.putEnv({ env: new Map(), store: new Map() });
      expect(ref.startsWith("env:")).toBe(true);
    });

    it("state refs start with 'state:'", () => {
      const repo = new SnapshotRepo();
      const mockState = { tag: "Halt" as const, value: { tag: "Unit" } as any };
      const ref = repo.putState({ state: mockState as any });
      expect(ref.startsWith("state:")).toBe(true);
    });
  });

  describe("deterministic IDs (new features)", () => {
    it("generates sequential env refs", () => {
      const repo = new SnapshotRepo();
      const ref1 = repo.putEnv({ env: new Map(), store: new Map() });
      const ref2 = repo.putEnv({ env: new Map(), store: new Map() });
      const ref3 = repo.putEnv({ env: new Map(), store: new Map() });

      expect(ref1).toBe("env:1");
      expect(ref2).toBe("env:2");
      expect(ref3).toBe("env:3");
    });

    it("generates sequential state refs", () => {
      const repo = new SnapshotRepo();
      const mockState = { tag: "Halt" as const, value: { tag: "Unit" } as any };
      const ref1 = repo.putState({ state: mockState as any });
      const ref2 = repo.putState({ state: mockState as any });

      expect(ref1).toBe("state:1");
      expect(ref2).toBe("state:2");
    });

    it("accepts prefix option", () => {
      const repo = new SnapshotRepo({ prefix: "test-" });
      const ref = repo.putEnv({ env: new Map(), store: new Map() });

      expect(ref).toBe("test-env:1");
    });

    it("accepts startCounter option", () => {
      const repo = new SnapshotRepo({ startCounter: 100 });
      const ref1 = repo.putEnv({ env: new Map(), store: new Map() });
      const ref2 = repo.putEnv({ env: new Map(), store: new Map() });

      expect(ref1).toBe("env:101");
      expect(ref2).toBe("env:102");
    });

    it("reset() clears snapshots and counters", () => {
      const repo = new SnapshotRepo();
      repo.putEnv({ env: new Map(), store: new Map() });
      repo.putEnv({ env: new Map(), store: new Map() });
      repo.putState({ state: { tag: "Halt" } as any });

      repo.reset();

      // Counters reset
      const ref1 = repo.putEnv({ env: new Map(), store: new Map() });
      expect(ref1).toBe("env:1");

      // Old refs are gone
      expect(() => repo.getEnv("env:1")).not.toThrow(); // new env:1 exists
      expect(() => repo.getEnv("env:2")).toThrow("missing EnvRef");
    });

    it("reset() with options", () => {
      const repo = new SnapshotRepo();
      repo.putEnv({ env: new Map(), store: new Map() });

      repo.reset({ prefix: "new-", startCounter: 50 });

      const ref = repo.putEnv({ env: new Map(), store: new Map() });
      expect(ref).toBe("new-env:51");
    });

    it("getPrefix() returns current prefix", () => {
      const repo = new SnapshotRepo({ prefix: "myprefix-" });
      expect(repo.getPrefix()).toBe("myprefix-");
    });

    it("setPrefix() changes prefix", () => {
      const repo = new SnapshotRepo();
      repo.setPrefix("changed-");

      const ref = repo.putEnv({ env: new Map(), store: new Map() });
      expect(ref.startsWith("changed-")).toBe(true);
    });

    it("getCounters() returns current counters", () => {
      const repo = new SnapshotRepo();
      repo.putEnv({ env: new Map(), store: new Map() });
      repo.putEnv({ env: new Map(), store: new Map() });
      repo.putState({ state: { tag: "Halt" } as any });

      const counters = repo.getCounters();
      expect(counters.envCounter).toBe(2);
      expect(counters.stateCounter).toBe(1);
    });

    it("IDs are deterministic across instances with same options", () => {
      const repo1 = new SnapshotRepo({ prefix: "det-" });
      const repo2 = new SnapshotRepo({ prefix: "det-" });

      const ref1a = repo1.putEnv({ env: new Map(), store: new Map() });
      const ref2a = repo2.putEnv({ env: new Map(), store: new Map() });

      // Both start at same counter, so same IDs
      expect(ref1a).toBe(ref2a);
      expect(ref1a).toBe("det-env:1");
    });
  });
});
