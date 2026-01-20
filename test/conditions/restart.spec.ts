import { describe, it, expect } from "vitest";
import type { Val } from "../../src/core/eval/values";
import { evalOmega, expectBool, expectSym, listToArray } from "./helpers";

function expectNum(v: Val): number {
  if (v.tag !== "Num") throw new Error(`expected Num, got ${v.tag}`);
  return v.n;
}

describe("conditions: restarts", () => {
  describe("Happy Path", () => {
    it("HP-1: find-restart reports availability", async () => {
      const result = await evalOmega(`
        (restart-bind ((my-restart (lambda (v) v)))
          (find-restart 'my-restart))
      `);
      expectBool(result);
      expect(result).toMatchObject({ tag: "Bool", b: true });
    });

    it("HP-2: compute-restarts returns current restarts", async () => {
      const result = await evalOmega(`
        (restart-bind ((r1 (lambda () 1))
                       (r2 (lambda () 2)))
          (compute-restarts))
      `);
      const items = listToArray(result);
      expect(items.length).toBe(2);
    });

    it("HP-3: nested restart-bind accumulates restarts", async () => {
      const result = await evalOmega(`
        (restart-bind ((outer (lambda () 'outer)))
          (restart-bind ((inner (lambda () 'inner)))
            (compute-restarts)))
      `);
      const items = listToArray(result);
      expect(items.length).toBe(2);
    });
  });

  describe("Edge Cases", () => {
    it("EC-1: find-restart false when not found", async () => {
      const result = await evalOmega(`(find-restart 'nonexistent)`);
      expectBool(result);
      expect(result).toMatchObject({ tag: "Bool", b: false });
    });

    it("EC-2: innermost restart with same name is chosen", async () => {
      const result = await evalOmega(`
        (restart-bind ((name (lambda () 'outer)))
          (restart-bind ((name (lambda () 'inner)))
            (invoke-restart 'name)))
      `);
      expectSym(result);
      expect(result).toMatchObject({ tag: "Sym", name: "inner" });
    });

    it("EC-3: restart invoked outside its dynamic extent errors", async () => {
      await expect(evalOmega(`
        (begin
          (define saved-restart #f)
          (restart-bind ((r (lambda () (set! saved-restart #t) 'ok)))
            (set! saved-restart (lambda () (invoke-restart 'r))))
          (saved-restart))
      `)).rejects.toThrow(/restart/i);
    });
  });
});
