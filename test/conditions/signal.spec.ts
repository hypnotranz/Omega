import { describe, it, expect } from "vitest";
import type { Val } from "../../src/core/eval/values";
import { evalOmega, expectSym, expectBool, listToArray } from "./helpers";

function expectNum(v: Val): number {
  if (v.tag !== "Num") {
    throw new Error(`expected Num, got ${v.tag}`);
  }
  return v.n;
}

describe("conditions: signal", () => {
  describe("Happy Path", () => {
    it("HP-1: handler catches and invokes restart", async () => {
      const result = await evalOmega(`
        (handler-bind ((parse-error (lambda (c) (invoke-restart 'use-default 0))))
          (restart-bind ((use-default (lambda (v) v)))
            (signal 'parse-error "bad input" '())))
      `);
      expectNum(result);
      expect(result).toMatchObject({ tag: "Num", n: 0 });
    });

    it("HP-2: handler runs and body continues without unwinding", async () => {
      const result = await evalOmega(`
        (begin
          (define log '())
          (handler-bind ((warning (lambda (c) (set! log (cons 'handled log)))))
            (signal 'warning "minor issue" '())
            (set! log (cons 'continued log)))
          log)
      `);
      const items = listToArray(result);
      const names = items.map(v => expectSym(v));
      expect(names).toEqual(["continued", "handled"]);
    });

    it("HP-3: multiple handlers, first match wins", async () => {
      const result = await evalOmega(`
        (handler-bind ((type-a (lambda (c) 'a-handler))
                       (type-b (lambda (c) 'b-handler)))
          (restart-bind ((return-val (lambda (v) v)))
            (invoke-restart 'return-val (signal 'type-b "test" '()))))
      `);
      expectSym(result);
      expect(result).toMatchObject({ tag: "Sym", name: "b-handler" });
    });

    it("HP-4: nested handler-bind, inner shadows outer", async () => {
      const result = await evalOmega(`
        (handler-bind ((error (lambda (c) 'outer)))
          (handler-bind ((error (lambda (c) 'inner)))
            (restart-bind ((r (lambda (v) v)))
              (invoke-restart 'r (signal 'error "test" '())))))
      `);
      expectSym(result);
      expect(result).toMatchObject({ tag: "Sym", name: "inner" });
    });
  });

  describe("Edge Cases", () => {
    it("EC-1: signal with no matching handler returns unit", async () => {
      const result = await evalOmega(`(signal 'unknown-type "no handler" '())`);
      expect(result).toMatchObject({ tag: "Unit" });
    });

    it("EC-2: handler declines and body result is returned", async () => {
      const result = await evalOmega(`
        (handler-bind ((warning (lambda (c) 'declined)))
          (signal 'warning "test" '())
          'body-result)
      `);
      expectSym(result);
      expect(result).toMatchObject({ tag: "Sym", name: "body-result" });
    });

    it("EC-3: invoking missing restart raises", async () => {
      await expect(evalOmega(`
        (handler-bind ((e (lambda (c) (invoke-restart 'nonexistent 0))))
          (restart-bind ((other (lambda (v) v)))
            (signal 'e "test" '())))
      `)).rejects.toThrow(/nonexistent/i);
    });

    it("EC-4: handler can signal another condition", async () => {
      const result = await evalOmega(`
        (handler-bind ((inner (lambda (c) 'inner-handled)))
          (handler-bind ((outer (lambda (c) (signal 'inner "from handler" '()))))
            (restart-bind ((r (lambda (v) v)))
              (invoke-restart 'r (signal 'outer "test" '())))))
      `);
      expectSym(result);
      expect(result).toMatchObject({ tag: "Sym", name: "inner-handled" });
    });

    it("EC-5: restart can accept multiple arguments", async () => {
      const result = await evalOmega(`
        (handler-bind ((e (lambda (c) (invoke-restart 'multi 1 2 3))))
          (restart-bind ((multi (lambda (a b c) (+ a b c))))
            (signal 'e "test" '())))
      `);
      expectNum(result);
      expect(result).toMatchObject({ tag: "Num", n: 6 });
    });
  });
});
