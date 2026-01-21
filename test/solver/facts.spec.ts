/**
 * ═══════════════════════════════════════════════════════════════════════════
 * Semantic Facts & Logic Tests
 * ═══════════════════════════════════════════════════════════════════════════
 *
 * Quick Reference: docs/USER-MANUAL--00--Quick-Reference.md#27-logic-programming-with-semantic-facts
 * Full Chapter:    docs/USER-MANUAL--27--Logic-Programming-With-Semantic-Facts.md
 * Demo:            demo/by-chapter/ch27-logic-programming.ts
 * ═══════════════════════════════════════════════════════════════════════════
 */
import { describe, it, expect } from "vitest";
import { evalOne, listToArray, expectBool, expectNum, expectStr, asFactStore } from "./utils";

describe("fact store", () => {
  it("creates an empty fact store", async () => {
    const store = await evalOne("(make-fact-store)");
    const fs = asFactStore(store);
    expect(fs.tag).toBe("FactStore");
  });

  it("asserts and queries a fact", async () => {
    const value = await evalOne(`
      (begin
        (define fs (make-fact-store))
        (define fs2 (assert-fact fs "x.type" "integer"))
        (query-fact fs2 "x.type"))
    `);
    expect(expectStr(value)).toBe("integer");
  });

  it("supports pattern queries", async () => {
    const value = await evalOne(`
      (begin
        (define fs (make-fact-store))
        (define fs1 (assert-fact fs "user.name" "Alice"))
        (define fs2 (assert-fact fs1 "user.age" 30))
        (define fs3 (assert-fact fs2 "config.debug" #t))
        (query-facts fs3 "user\\\\..*"))
    `);
    const pairs = listToArray(value).map(listToArray);
    expect(pairs).toHaveLength(2);
    const names = pairs.map(([k]) => expectStr(k)).sort();
    expect(names).toEqual(["user.age", "user.name"]);
  });

  it("is immutable on update", async () => {
    const value = await evalOne(`
      (begin
        (define fs1 (make-fact-store))
        (define fs2 (assert-fact fs1 "x" 1))
        (list (query-fact fs1 "x") (query-fact fs2 "x")))
    `);
    const [first, second] = listToArray(value);
    expect(expectBool(first)).toBe(false);
    expect(expectNum(second)).toBe(1);
  });

  it("rejects conflicting facts", async () => {
    await expect(evalOne(`
      (begin
        (define fs (assert-fact (make-fact-store) "x" 1))
        (assert-fact fs "x" 2))
    `)).rejects.toThrow(/already exists/);
  });
});
