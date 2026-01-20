import { describe, it, expect } from "vitest";
import { COWStore } from "../../src/core/eval/store";
import { installPrims } from "../helpers/prims";
import { envGet } from "../../src/core/eval/env";
import type { Val } from "../../src/core/eval/values";
import { evalAll, evalOne, listToArray, withMonadLib } from "./utils";

function expectNum(v: Val): number {
  if (v.tag !== "Num") throw new Error(`expected Num, got ${v.tag}`);
  return v.n;
}

function expectBool(v: Val): boolean {
  if (v.tag !== "Bool") throw new Error(`expected Bool, got ${v.tag}`);
  return v.b;
}

function expectSym(v: Val): string {
  if (v.tag !== "Sym") throw new Error(`expected Sym, got ${v.tag}`);
  return v.name;
}

function toNums(vals: Val[]): number[] {
  return vals.map(expectNum);
}

function listNums(v: Val): number[] {
  return listToArray(v).map(expectNum);
}

describe("monadic primitives", () => {
  describe("registration", () => {
    it("installs monad primitives into the initial environment", () => {
      const prim = installPrims(new COWStore());
      expect(envGet(prim.env, "unit")).toBeDefined();
      expect(envGet(prim.env, "mzero")).toBeDefined();
      expect(envGet(prim.env, "mplus")).toBeDefined();
      expect(envGet(prim.env, "bind")).toBeDefined();
    });
  });

  describe("unit", () => {
    it("HP-1: returns numeric value unchanged", async () => {
      const values = await evalAll("(unit 42)");
      expect(values).toHaveLength(1);
      expect(expectNum(values[0])).toBe(42);
    });

    it("HP-2: wraps compound list values", async () => {
      const values = await evalAll("(unit (list 1 2 3))");
      expect(listNums(values[0])).toEqual([1, 2, 3]);
    });

    it("HP-3: preserves closures", async () => {
      const values = await evalAll("(unit (lambda (x) x))");
      expect(values[0].tag).toBe("Closure");
    });

    it("EC-1: treats #f as a value, not failure", async () => {
      const values = await evalAll("(unit #f)");
      expect(expectBool(values[0])).toBe(false);
    });

    it("EC-2: handles empty list", async () => {
      const values = await evalAll("(unit '())");
      expect(values[0].tag).toBe("Unit");
    });
  });

  describe("mzero", () => {
    it("HP-1: yields no results", async () => {
      const values = await evalAll("(mzero)");
      expect(values).toHaveLength(0);
    });

    it("HP-2: short-circuits within a bind chain", async () => {
      const values = await evalAll("(bind (mzero) (lambda (x) (unit x)))");
      expect(values).toHaveLength(0);
    });
  });

  describe("mplus", () => {
    it("HP-1: combines two successful computations", async () => {
      const values = await evalAll("(mplus (unit 1) (unit 2))");
      expect(toNums(values)).toEqual([1, 2]);
    });

    it("HP-2: nests to support multiple alternatives", async () => {
      const values = await evalAll("(mplus (unit 'a) (mplus (unit 'b) (unit 'c)))");
      expect(values.map(expectSym)).toEqual(["a", "b", "c"]);
    });

    it("EC-1: mzero is left identity", async () => {
      const values = await evalAll("(mplus (mzero) (unit 42))");
      expect(toNums(values)).toEqual([42]);
    });

    it("EC-2: mzero is right identity", async () => {
      const values = await evalAll("(mplus (unit 42) (mzero))");
      expect(toNums(values)).toEqual([42]);
    });

    it("EC-3: two failing branches stay empty", async () => {
      const values = await evalAll("(mplus (mzero) (mzero))");
      expect(values).toHaveLength(0);
    });
  });

  describe("bind", () => {
    it("HP-1: binds through identity function", async () => {
      const values = await evalAll("(bind (unit 42) (lambda (x) (unit x)))");
      expect(toNums(values)).toEqual([42]);
    });

    it("HP-2: chains computations", async () => {
      const values = await evalAll("(bind (unit 5) (lambda (x) (unit (* x 2))))");
      expect(toNums(values)).toEqual([10]);
    });

    it("HP-3: propagates multiple results", async () => {
      const values = await evalAll("(bind (mplus (unit 1) (unit 2)) (lambda (x) (unit (* x 10))))");
      expect(toNums(values)).toEqual([10, 20]);
    });

    it("HP-4: flattens nested nondeterminism", async () => {
      const values = await evalAll(`
        (bind (mplus (unit 1) (unit 2))
              (lambda (x) (mplus (unit x) (unit (- x)))))
      `);
      expect(toNums(values)).toEqual([1, -1, 2, -2]);
    });

    it("EC-1: mzero producer yields no results", async () => {
      const values = await evalAll("(bind (mzero) (lambda (x) (unit x)))");
      expect(values).toHaveLength(0);
    });

    it("EC-2: function that always fails", async () => {
      const values = await evalAll("(bind (mplus (unit 1) (unit 2)) (lambda (x) (mzero)))");
      expect(values).toHaveLength(0);
    });

    it("EC-3: filters out odd numbers", async () => {
      const values = await evalAll(`
        (bind (mplus (unit 1) (unit 2) (unit 3))
              (lambda (x) (if (even? x) (unit x) (mzero))))
      `);
      expect(toNums(values)).toEqual([2]);
    });

    it("ERR-1: rejects non-procedure binding function", async () => {
      await expect(evalOne("(bind (unit 1) 42)")).rejects.toThrow();
    });
  });

  describe("library helpers (monad.lisp)", () => {
    it("guard succeeds when predicate is true", async () => {
      const values = await evalAll(withMonadLib(`(mdo (_ <- (guard #t)) (unit 'ok))`));
      expect(values.map(expectSym)).toEqual(["ok"]);
    });

    it("guard fails when predicate is false", async () => {
      const values = await evalAll(withMonadLib(`(mdo (_ <- (guard #f)) (unit 'ok))`));
      expect(values).toHaveLength(0);
    });

    it("msum concatenates computations", async () => {
      const values = await evalAll(withMonadLib(`(msum (list (unit 1) (unit 2) (unit 3)))`));
      expect(toNums(values)).toEqual([1, 2, 3]);
    });

    it("msum skips failures", async () => {
      const values = await evalAll(withMonadLib(`(msum (list (mzero) (unit 1) (mzero) (unit 2)))`));
      expect(toNums(values)).toEqual([1, 2]);
    });

    it("mfilter keeps values matching predicate", async () => {
      const values = await evalAll(withMonadLib(`(mfilter even? (msum (list (unit 1) (unit 2) (unit 3))))`));
      expect(toNums(values)).toEqual([2]);
    });

    it("mconcatMap maps and flattens", async () => {
      const values = await evalAll(withMonadLib(`
        (mconcatMap (lambda (x) (mplus (unit x) (unit (- x))))
                    (list 1 2))
      `));
      expect(toNums(values)).toEqual([1, -1, 2, -2]);
    });

    it("mdo threads bindings", async () => {
      const values = await evalAll(withMonadLib(`
        (mdo (x <- (unit 2))
             (y <- (unit 3))
             (unit (* x y)))
      `));
      expect(toNums(values)).toEqual([6]);
    });

    it("mdo handles nondeterministic bindings", async () => {
      const values = await evalAll(withMonadLib(`
        (mdo (x <- (mplus (unit 1) (unit 2)))
             (y <- (mplus (unit 10) (unit 20)))
             (unit (+ x y)))
      `));
      expect(toNums(values)).toEqual([11, 21, 12, 22]);
    });

    it("mdo respects guard-driven failure", async () => {
      const values = await evalAll(withMonadLib(`
        (mdo (x <- (msum (list (unit 1) (unit 2) (unit 3) (unit 4))))
             (_ <- (guard (even? x)))
             (unit x))
      `));
      expect(toNums(values)).toEqual([2, 4]);
    });
  });
});
