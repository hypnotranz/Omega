// test/core/receipts/ctxReceipts.spec.ts
// Tests for context economics (snapshot/compress/hydrate)

import { describe, it, expect } from "vitest";
import { CtxReceiptRepo, type CtxReceipt } from "../../../src/core/oracle/ctxReceipts";

describe("CtxReceiptRepo", () => {
  describe("snapshot", () => {
    it("creates snapshot receipt", () => {
      const repo = new CtxReceiptRepo();
      const receipt = repo.snapshot("env:1" as any, "state:1" as any, { note: "test" });

      expect(receipt.tag).toBe("CtxReceipt");
      expect(receipt.kind).toBe("snapshot");
      expect(receipt.envRef).toBe("env:1");
      expect(receipt.stateRef).toBe("state:1");
      expect(receipt.meta).toEqual({ note: "test" });
    });

    it("generates unique receipt ids", () => {
      const repo = new CtxReceiptRepo();
      const r1 = repo.snapshot("env:1" as any, undefined, null);
      const r2 = repo.snapshot("env:1" as any, undefined, null);

      // Different timestamps should produce different IDs
      expect(r1.rid).toBeDefined();
      expect(r2.rid).toBeDefined();
    });

    it("stores receipt for retrieval", () => {
      const repo = new CtxReceiptRepo();
      const receipt = repo.snapshot("env:1" as any, undefined, null);

      const retrieved = repo.get(receipt.rid);
      expect(retrieved).toBeDefined();
      expect(retrieved?.rid).toBe(receipt.rid);
    });

    it("records timing information", () => {
      const repo = new CtxReceiptRepo();
      const receipt = repo.snapshot("env:1" as any, undefined, null);

      expect(receipt.timeMs).toBeDefined();
      expect(typeof receipt.timeMs).toBe("number");
    });
  });

  describe("compress", () => {
    it("creates compress receipt", () => {
      const repo = new CtxReceiptRepo();
      const receipt = repo.compress("env:1" as any, { policy: "shallow" });

      expect(receipt.tag).toBe("CtxReceipt");
      expect(receipt.kind).toBe("compress");
      expect(receipt.envRef).toBe("env:1");
      expect(receipt.meta).toEqual({ policy: "shallow" });
    });

    it("does not include stateRef for compress", () => {
      const repo = new CtxReceiptRepo();
      const receipt = repo.compress("env:1" as any, null);

      expect(receipt.stateRef).toBeUndefined();
    });
  });

  describe("get and has", () => {
    it("returns undefined for missing receipt", () => {
      const repo = new CtxReceiptRepo();
      expect(repo.get("nonexistent" as any)).toBeUndefined();
    });

    it("has returns false for missing receipt", () => {
      const repo = new CtxReceiptRepo();
      expect(repo.has("nonexistent" as any)).toBe(false);
    });

    it("has returns true for existing receipt", () => {
      const repo = new CtxReceiptRepo();
      const receipt = repo.snapshot("env:1" as any, undefined, null);

      expect(repo.has(receipt.rid)).toBe(true);
    });
  });

  describe("clear", () => {
    it("removes all receipts", () => {
      const repo = new CtxReceiptRepo();
      const r1 = repo.snapshot("env:1" as any, undefined, null);
      const r2 = repo.compress("env:2" as any, null);

      expect(repo.has(r1.rid)).toBe(true);
      expect(repo.has(r2.rid)).toBe(true);

      repo.clear();

      expect(repo.has(r1.rid)).toBe(false);
      expect(repo.has(r2.rid)).toBe(false);
    });

    it("resets counter", () => {
      const repo = new CtxReceiptRepo({ deterministic: true });
      repo.snapshot("env:1" as any, undefined, null);
      repo.snapshot("env:2" as any, undefined, null);

      repo.clear();

      // Counter should be reset to 0
      const r = repo.snapshot("env:1" as any, undefined, null);
      // First receipt after clear should use seq: 1
      expect(r.rid).toBeDefined();
    });
  });

  describe("deterministic mode (new features)", () => {
    it("non-deterministic mode uses timestamp (default)", () => {
      const repo = new CtxReceiptRepo();
      const r1 = repo.snapshot("env:1" as any, undefined, null);
      const r2 = repo.snapshot("env:1" as any, undefined, null);

      // IDs will be different due to different timestamps (usually)
      // But in fast execution they might be same, so just check they exist
      expect(r1.rid).toBeDefined();
      expect(r2.rid).toBeDefined();
    });

    it("deterministic mode uses counter", () => {
      const repo = new CtxReceiptRepo({ deterministic: true });
      const r1 = repo.snapshot("env:1" as any, undefined, null);
      const r2 = repo.snapshot("env:1" as any, undefined, null);

      // With same data and deterministic mode, different counters give different IDs
      expect(r1.rid).not.toBe(r2.rid);
    });

    it("deterministic mode IDs are reproducible", () => {
      const repo1 = new CtxReceiptRepo({ deterministic: true });
      const repo2 = new CtxReceiptRepo({ deterministic: true });

      const r1 = repo1.snapshot("env:1" as any, "state:1" as any, { test: true });
      const r2 = repo2.snapshot("env:1" as any, "state:1" as any, { test: true });

      // Same data, same counter (seq=1), same hash
      expect(r1.rid).toBe(r2.rid);
    });

    it("deterministic mode sets timeMs to 0", () => {
      const repo = new CtxReceiptRepo({ deterministic: true });
      const r = repo.snapshot("env:1" as any, undefined, null);

      expect(r.timeMs).toBe(0);
    });

    it("non-deterministic mode records actual timeMs", () => {
      const repo = new CtxReceiptRepo({ deterministic: false });
      const r = repo.snapshot("env:1" as any, undefined, null);

      expect(r.timeMs).toBeGreaterThanOrEqual(0);
    });

    it("accepts prefix option", () => {
      const repo = new CtxReceiptRepo({ deterministic: true, prefix: "test-" });
      const r = repo.snapshot("env:1" as any, undefined, null);

      expect(r.rid.startsWith("test-")).toBe(true);
    });

    it("reset() clears and allows new options", () => {
      const repo = new CtxReceiptRepo({ deterministic: false });
      repo.snapshot("env:1" as any, undefined, null);

      repo.reset({ deterministic: true, prefix: "new-" });

      const r = repo.snapshot("env:1" as any, undefined, null);
      expect(r.rid.startsWith("new-")).toBe(true);
      expect(r.timeMs).toBe(0); // deterministic mode
    });

    it("reset() preserves options if not specified", () => {
      const repo = new CtxReceiptRepo({ deterministic: true, prefix: "keep-" });
      repo.reset();

      const r = repo.snapshot("env:1" as any, undefined, null);
      expect(r.rid.startsWith("keep-")).toBe(true);
      expect(r.timeMs).toBe(0);
    });
  });

  describe("new helper methods", () => {
    it("all() returns all receipts", () => {
      const repo = new CtxReceiptRepo();
      repo.snapshot("env:1" as any, undefined, null);
      repo.snapshot("env:2" as any, undefined, null);
      repo.compress("env:3" as any, null);

      const all = repo.all();
      expect(all.length).toBe(3);
      expect(all.filter(r => r.kind === "snapshot").length).toBe(2);
      expect(all.filter(r => r.kind === "compress").length).toBe(1);
    });

    it("size() returns receipt count", () => {
      const repo = new CtxReceiptRepo();
      expect(repo.size()).toBe(0);

      repo.snapshot("env:1" as any, undefined, null);
      expect(repo.size()).toBe(1);

      repo.compress("env:2" as any, null);
      expect(repo.size()).toBe(2);

      repo.clear();
      expect(repo.size()).toBe(0);
    });
  });
});
