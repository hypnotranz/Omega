// test/core/receipts/receipts.spec.ts
// Tests for receipt store and oracle receipts

import { describe, it, expect } from "vitest";
import {
  InMemoryReceiptStore,
  receiptKey,
  type OracleReceipt,
} from "../../../src/core/oracle/receipts";

describe("receipt system", () => {
  describe("receiptKey", () => {
    it("generates deterministic key for same request", () => {
      const req = { tag: "ReqEval" as const, qexpr: "(+ 1 2)", envRef: "env:1" as any };
      const k1 = receiptKey(req);
      const k2 = receiptKey(req);
      expect(k1).toBe(k2);
    });

    it("generates different keys for different requests", () => {
      const req1 = { tag: "ReqEval" as const, qexpr: "(+ 1 2)", envRef: "env:1" as any };
      const req2 = { tag: "ReqEval" as const, qexpr: "(+ 1 3)", envRef: "env:1" as any };
      const k1 = receiptKey(req1);
      const k2 = receiptKey(req2);
      expect(k1).not.toBe(k2);
    });
  });

  describe("InMemoryReceiptStore", () => {
    it("starts with off mode by default", () => {
      const store = new InMemoryReceiptStore();
      expect(store.mode).toBe("off");
    });

    it("accepts mode parameter", () => {
      const store = new InMemoryReceiptStore("record");
      expect(store.mode).toBe("record");
    });

    it("stores and retrieves receipts", () => {
      const store = new InMemoryReceiptStore("record");
      const receipt: OracleReceipt = {
        key: "test-key" as any,
        req: { tag: "ReqEval", qexpr: "(+ 1 2)", envRef: "env:1" as any },
        resp: { tag: "RespVal", value: { tag: "Num", n: 3 } },
        timeMs: 10,
      };

      store.put(receipt);
      const retrieved = store.get("test-key" as any);

      expect(retrieved).toBeDefined();
      expect(retrieved?.key).toBe("test-key");
      expect(retrieved?.timeMs).toBe(10);
    });

    it("returns undefined for missing key", () => {
      const store = new InMemoryReceiptStore();
      expect(store.get("nonexistent" as any)).toBeUndefined();
    });

    it("overwrites existing receipts with same key", () => {
      const store = new InMemoryReceiptStore("record");
      const receipt1: OracleReceipt = {
        key: "test-key" as any,
        req: { tag: "ReqEval", qexpr: "(+ 1 2)", envRef: "env:1" as any },
        resp: { tag: "RespVal", value: { tag: "Num", n: 3 } },
        timeMs: 10,
      };
      const receipt2: OracleReceipt = {
        key: "test-key" as any,
        req: { tag: "ReqEval", qexpr: "(+ 1 2)", envRef: "env:1" as any },
        resp: { tag: "RespVal", value: { tag: "Num", n: 3 } },
        timeMs: 20,
      };

      store.put(receipt1);
      store.put(receipt2);
      const retrieved = store.get("test-key" as any);

      expect(retrieved?.timeMs).toBe(20);
    });

    describe("new helper methods", () => {
      it("reset() clears all receipts", () => {
        const store = new InMemoryReceiptStore("record");
        store.put({
          key: "key1" as any,
          req: { tag: "ReqEval", qexpr: "(+ 1 2)", envRef: "env:1" as any },
          resp: { tag: "RespAck" },
        });
        store.put({
          key: "key2" as any,
          req: { tag: "ReqEval", qexpr: "(+ 3 4)", envRef: "env:1" as any },
          resp: { tag: "RespAck" },
        });

        store.reset();

        expect(store.get("key1" as any)).toBeUndefined();
        expect(store.get("key2" as any)).toBeUndefined();
        expect(store.size()).toBe(0);
      });

      it("reset() with mode changes mode", () => {
        const store = new InMemoryReceiptStore("record");
        expect(store.mode).toBe("record");

        store.reset("replay");

        expect(store.mode).toBe("replay");
      });

      it("reset() without mode preserves mode", () => {
        const store = new InMemoryReceiptStore("record");
        store.reset();
        expect(store.mode).toBe("record");
      });

      it("all() returns all stored receipts", () => {
        const store = new InMemoryReceiptStore("record");
        const r1: OracleReceipt = {
          key: "key1" as any,
          req: { tag: "ReqEval", qexpr: "(+ 1 2)", envRef: "env:1" as any },
          resp: { tag: "RespAck" },
        };
        const r2: OracleReceipt = {
          key: "key2" as any,
          req: { tag: "ReqEval", qexpr: "(+ 3 4)", envRef: "env:1" as any },
          resp: { tag: "RespAck" },
        };

        store.put(r1);
        store.put(r2);

        const all = store.all();
        expect(all.length).toBe(2);
        expect(all.some(r => r.key === "key1")).toBe(true);
        expect(all.some(r => r.key === "key2")).toBe(true);
      });

      it("size() returns number of stored receipts", () => {
        const store = new InMemoryReceiptStore("record");
        expect(store.size()).toBe(0);

        store.put({
          key: "key1" as any,
          req: { tag: "ReqEval", qexpr: "(+ 1 2)", envRef: "env:1" as any },
          resp: { tag: "RespAck" },
        });
        expect(store.size()).toBe(1);

        store.put({
          key: "key2" as any,
          req: { tag: "ReqEval", qexpr: "(+ 3 4)", envRef: "env:1" as any },
          resp: { tag: "RespAck" },
        });
        expect(store.size()).toBe(2);
      });

      it("has() returns true for existing key", () => {
        const store = new InMemoryReceiptStore("record");
        store.put({
          key: "exists" as any,
          req: { tag: "ReqEval", qexpr: "(+ 1 2)", envRef: "env:1" as any },
          resp: { tag: "RespAck" },
        });

        expect(store.has("exists" as any)).toBe(true);
      });

      it("has() returns false for missing key", () => {
        const store = new InMemoryReceiptStore();
        expect(store.has("nonexistent" as any)).toBe(false);
      });
    });
  });
});
