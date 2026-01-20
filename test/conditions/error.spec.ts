import { describe, it, expect } from "vitest";
import { evalOmega, expectSym } from "./helpers";

describe("conditions: error", () => {
  describe("Happy Path", () => {
    it("HP-1: error handled via restart", async () => {
      const result = await evalOmega(`
        (handler-bind ((fatal (lambda (c) (invoke-restart 'recover 'saved))))
          (restart-bind ((recover (lambda (v) v)))
            (error 'fatal "Something broke" '())))
      `);
      expectSym(result);
      expect(result).toMatchObject({ tag: "Sym", name: "saved" });
    });
  });

  describe("Error Cases", () => {
    it("ERR-1: error with no handler is fatal", async () => {
      await expect(evalOmega(`(error 'unhandled "no handler" '())`)).rejects.toThrow(/Unhandled/i);
    });

    it("ERR-2: handler that returns without restart is fatal", async () => {
      await expect(evalOmega(`
        (handler-bind ((e (lambda (c) 'did-nothing)))
          (error 'e "test" '()))
      `)).rejects.toThrow(/Unhandled/i);
    });
  });
});
