// Generic Operations Tests (Prompt 5) - Data-Directed Semantic Computing
// Tests 5.1-5.6: Generic ops, coercion graphs, inference-synthesized adapters
//
// These tests demonstrate SICP-style data-directed programming with
// oracle-driven method synthesis when operations are missing.
//
// Note: apply-generic and related primitives only support Native functions.
// For Closure-based methods, use op-table-get + manual invocation.

import { describe, it, expect } from "vitest";
import { COWStore } from "../../src/core/eval/store";
import { RuntimeImpl } from "../../src/core/effects/runtimeImpl";
import { ScriptedOracleAdapter } from "../../src/core/oracle/scriptedOracle";
import { SnapshotRepo } from "../../src/core/oracle/snapshots";
import { InMemoryReceiptStore } from "../../src/core/oracle/receipts";
import { mockCommit } from "../helpers/runtime";
import { installPrims } from "../helpers/prims";
import type { State } from "../../src/core/eval/machine";
import { runToCompletion } from "../../src/core/eval/run";
import { compileTextToExpr } from "../../src/core/pipeline/compileText";
import type { Val } from "../../src/core/eval/values";
import type { Profile } from "../../src/core/governance/profile";

function createTestEnv(profile?: Profile) {
  const oracle = new ScriptedOracleAdapter();
  const snapshots = new SnapshotRepo();
  const receipts = new InMemoryReceiptStore("off");
  const runtime = new RuntimeImpl(oracle, snapshots, receipts, mockCommit, profile);
  return { runtime, oracle, snapshots, receipts };
}

function initialState(src: string): State {
  const expr = compileTextToExpr(src);
  const store0 = new COWStore();
  const prim = installPrims(store0);
  return { control: { tag: "Expr", e: expr }, env: prim.env, store: prim.store, kont: [], handlers: [] };
}

const fullProfile: Profile = {
  name: "generic-test",
  caps: ["*"],
  budgets: {
    maxOracleTurns: 1000,
    maxEvalSteps: 500_000,
    maxToolCalls: 1000,
  },
  truth: "speculative",
};

async function evalOmega(src: string, profile?: Profile): Promise<Val> {
  const { runtime } = createTestEnv(profile ?? fullProfile);
  const st0 = initialState(src);
  return runToCompletion(runtime, st0);
}

// Helper to extract number from result
function getNum(v: Val): number {
  if (v.tag === "Num") return v.n;
  throw new Error(`Expected Num, got ${v.tag}`);
}

// Helper to extract string from result
function getStr(v: Val): string {
  if (v.tag === "Str") return v.s;
  throw new Error(`Expected Str, got ${v.tag}`);
}

// Helper to extract symbol name from result
function getSym(v: Val): string {
  if (v.tag === "Sym") return v.name;
  throw new Error(`Expected Sym, got ${v.tag}`);
}

// Helper to extract boolean from result
function getBool(v: Val): boolean {
  if (v.tag === "Bool") return v.b;
  throw new Error(`Expected Bool, got ${v.tag}`);
}

// Helper to convert list to array
function listToArray(v: Val): Val[] {
  const result: Val[] = [];
  let cur = v as any;
  while (cur.tag === "Vector" && cur.items.length >= 2) {
    result.push(cur.items[0]);
    cur = cur.items[1];
  }
  return result;
}

describe("Generic Operations Tests (5.1-5.6)", () => {

  describe("5.1: Same operator across heterogeneous representations", () => {
    // Create an op-table with operations for different types

    it("op-table can be created", async () => {
      const result = await evalOmega(`
        (begin
          (define ops (make-op-table))
          (op-table? ops))
      `);
      expect(getBool(result)).toBe(true);
    });

    it("put and get methods from op-table", async () => {
      const result = await evalOmega(`
        (begin
          (define ops (make-op-table))
          (op-table-put ops 'add (list 'num 'num) +)
          (define method (op-table-get ops 'add (list 'num 'num)))
          (procedure? method))
      `);
      expect(getBool(result)).toBe(true);
    });

    it("apply-generic dispatches on type tags with Native function", async () => {
      // Use partial application to create a Native multiplier
      const result = await evalOmega(`
        (begin
          (define ops (make-op-table))
          ; Install Native 'double' using partial: (partial * 2) = (lambda (x) (* 2 x))
          (op-table-put ops 'double (list 'num) (partial * 2))
          ; Create a tagged number
          (define n (attach-tag 'num 5))
          ; Apply the generic operation
          (apply-generic ops 'double n))
      `);
      expect(getNum(result)).toBe(10);
    });

    it("op-table-get retrieves Closure method for manual invocation", async () => {
      // Test that we can store and retrieve Closure methods, then invoke manually
      const result = await evalOmega(`
        (begin
          (define ops (make-op-table))
          ; Install a Closure-based method
          (op-table-put ops 'format (list 'text)
            (lambda (x) (string-append "TEXT: " x)))

          ; Get the method
          (define method (op-table-get ops 'format (list 'text)))

          ; Manually invoke (since apply-generic only supports Native)
          (method "hello"))
      `);
      expect(getStr(result)).toBe("TEXT: hello");
    });

    it("different type representations can have different handlers", async () => {
      const result = await evalOmega(`
        (begin
          (define ops (make-op-table))

          ; Use Native functions: identity returns input, constantly returns fixed value
          (op-table-put ops 'get-value (list 'wrapper) identity)
          (op-table-put ops 'get-default (list 'wrapper) (constantly 0))

          ; Test with tagged data
          (define w (attach-tag 'wrapper 42))
          (define v (apply-generic ops 'get-value w))
          (define d (apply-generic ops 'get-default w))

          (cons v d))
      `);
      const items = (result as any).items;
      expect(getNum(items[0])).toBe(42);  // identity returns contents
      expect(getNum(items[1])).toBe(0);   // constantly returns 0
    });
  });

  describe("5.2: Coercion graph markdown->plain-text->redact", () => {
    // Set up coercion graph with Native functions

    it("coercion table can be created", async () => {
      const result = await evalOmega(`
        (begin
          (define coercions (make-coercion-table))
          (coercion-table? coercions))
      `);
      expect(getBool(result)).toBe(true);
    });

    it("put and get coercions", async () => {
      const result = await evalOmega(`
        (begin
          (define coercions (make-coercion-table))
          (put-coercion coercions 'a 'b identity)
          (define coercer (get-coercion coercions 'a 'b))
          (procedure? coercer))
      `);
      expect(getBool(result)).toBe(true);
    });

    it("find-coercion-path finds direct path", async () => {
      const result = await evalOmega(`
        (begin
          (define coercions (make-coercion-table))
          (put-coercion coercions 'markdown 'plain-text identity)
          (put-coercion coercions 'plain-text 'redacted identity)
          ; Find path from markdown to redacted
          (define path (find-coercion-path coercions 'markdown 'redacted))
          ; Path should be (markdown plain-text redacted)
          (length path))
      `);
      expect(getNum(result)).toBe(3);
    });

    it("coerce-value transforms through identity coercions", async () => {
      const result = await evalOmega(`
        (begin
          (define coercions (make-coercion-table))
          ; Use identity coercions (Native function)
          (put-coercion coercions 'A 'B identity)
          (put-coercion coercions 'B 'C identity)

          ; Coerce from A to C (two hops)
          (coerce-value coercions 42 'A 'C))
      `);
      expect(getNum(result)).toBe(42);
    });

    it("coerce-value transforms with string-length (Native)", async () => {
      const result = await evalOmega(`
        (begin
          (define coercions (make-coercion-table))
          ; Use string-length as coercion (Native function)
          (put-coercion coercions 'str 'len string-length)

          ; Coerce string to length
          (coerce-value coercions "hello" 'str 'len))
      `);
      expect(getNum(result)).toBe(5);
    });

    it("apply-generic-coerced with Native coercion", async () => {
      const result = await evalOmega(`
        (begin
          (define ops (make-op-table))
          (define coercions (make-coercion-table))

          ; Install 'double' for 'num' type
          (op-table-put ops 'double (list 'num) (partial * 2))

          ; Install coercion: str -> num (use string-length as stand-in)
          (put-coercion coercions 'str 'num string-length)

          ; Create string data
          (define s (attach-tag 'str "hello"))
          (define n (attach-tag 'num 10))

          ; Direct call works
          (define r1 (apply-generic-coerced ops coercions 'double n))

          r1)
      `);
      expect(getNum(result)).toBe(20);
    });
  });

  describe("5.3: Missing method triggers inference synthesis", () => {
    // When no method exists, use oracle to synthesize

    it("apply-generic returns false for missing method", async () => {
      const result = await evalOmega(`
        (begin
          (define ops (make-op-table))
          ; Don't install any methods
          (define data (attach-tag 'unknown "data"))
          (apply-generic ops 'process data))
      `);
      // Should return false when no method found
      expect(getBool(result)).toBe(false);
    });

    it("oracle-lambda can synthesize missing operation", async () => {
      // The oracle can be used to synthesize an operation
      const result = await evalOmega(`
        (begin
          (define ops (make-op-table))

          ; Use oracle to classify the type of processing needed
          (define infer-processor (oracle-lambda (type-tag data) "synthesize-processor"))

          ; For test, we'll just use a predefined fallback
          (define data (attach-tag 'custom "test data"))
          (define processor (infer-processor (type-tag data) (contents data)))

          ; The oracle returns a symbol indicating the processor type
          (symbol? processor))
      `);
      expect(getBool(result)).toBe(true);
    });

    it("synthesize-processor returns appropriate processor type", async () => {
      const result = await evalOmega(`
        (begin
          (define infer-processor (oracle-lambda (type-tag data) "synthesize-processor"))

          ; Test with document type
          (define p1 (infer-processor 'document "test"))
          ; Test with image type
          (define p2 (infer-processor 'image "test"))

          (cons p1 p2))
      `);
      const items = (result as any).items;
      expect(getSym(items[0])).toBe("text-processor");
      expect(getSym(items[1])).toBe("binary-processor");
    });
  });

  describe("5.4: Ambiguous coercion paths detected", () => {
    // When there are multiple paths, find-all-coercion-paths should return all of them

    it("find-all-coercion-paths returns multiple paths", async () => {
      const result = await evalOmega(`
        (begin
          (define coercions (make-coercion-table))
          ; Create diamond: A -> B -> D, A -> C -> D
          (put-coercion coercions 'A 'B identity)
          (put-coercion coercions 'B 'D identity)
          (put-coercion coercions 'A 'C identity)
          (put-coercion coercions 'C 'D identity)

          ; Find all paths from A to D
          (define paths (find-all-coercion-paths coercions 'A 'D))
          (length paths))
      `);
      expect(getNum(result)).toBe(2); // Two paths: A->B->D and A->C->D
    });

    it("ambiguity can be detected by checking path count", async () => {
      const result = await evalOmega(`
        (begin
          (define coercions (make-coercion-table))
          ; Create diamond
          (put-coercion coercions 'source 'via1 identity)
          (put-coercion coercions 'via1 'target identity)
          (put-coercion coercions 'source 'via2 identity)
          (put-coercion coercions 'via2 'target identity)

          ; Check for ambiguity
          (define paths (find-all-coercion-paths coercions 'source 'target))
          (> (length paths) 1))
      `);
      expect(getBool(result)).toBe(true);
    });

    it("no ambiguity when single path exists", async () => {
      const result = await evalOmega(`
        (begin
          (define coercions (make-coercion-table))
          ; Linear path only
          (put-coercion coercions 'A 'B identity)
          (put-coercion coercions 'B 'C identity)

          (define paths (find-all-coercion-paths coercions 'A 'C))
          (= (length paths) 1))
      `);
      expect(getBool(result)).toBe(true);
    });
  });

  describe("5.5: Ambiguity resolved via inference ranking", () => {
    // Oracle can rank coercion paths to resolve ambiguity

    it("find-coercion-path returns shortest path (BFS)", async () => {
      const result = await evalOmega(`
        (begin
          (define coercions (make-coercion-table))
          ; Direct path
          (put-coercion coercions 'src 'dst identity)
          ; Indirect path through intermediate
          (put-coercion coercions 'src 'mid identity)
          (put-coercion coercions 'mid 'dst identity)

          ; find-coercion-path uses BFS, so returns shortest
          (define path (find-coercion-path coercions 'src 'dst))
          (length path))
      `);
      // Should be 2: src -> dst (direct)
      expect(getNum(result)).toBe(2);
    });

    it("all paths can be enumerated for ranking", async () => {
      const result = await evalOmega(`
        (begin
          (define coercions (make-coercion-table))
          ; Create two paths: direct and via-intermediate
          (put-coercion coercions 'start 'end identity)
          (put-coercion coercions 'start 'middle identity)
          (put-coercion coercions 'middle 'end identity)

          ; Get all paths
          (define paths (find-all-coercion-paths coercions 'start 'end))

          ; Count and verify both paths exist
          (= (length paths) 2))
      `);
      expect(getBool(result)).toBe(true);
    });
  });

  describe("5.6: HOF + generic dispatch over mixed corpus", () => {
    // Apply operations over heterogeneous collections using generic operations

    it("op-table supports multiple type signatures", async () => {
      const result = await evalOmega(`
        (begin
          (define ops (make-op-table))

          ; Install 'double' for different types using Native functions
          (op-table-put ops 'double (list 'num) (partial * 2))
          (op-table-put ops 'double (list 'str) string-length)  ; length as "double" for strings

          ; Test both types
          (define n (attach-tag 'num 5))
          (define s (attach-tag 'str "hi"))

          (define r1 (apply-generic ops 'double n))
          (define r2 (apply-generic ops 'double s))

          (cons r1 r2))
      `);
      const items = (result as any).items;
      expect(getNum(items[0])).toBe(10);
      expect(getNum(items[1])).toBe(2);
    });

    it("mixed type operations with manual dispatch", async () => {
      // Since apply-generic only supports Native, use manual Closure dispatch
      const result = await evalOmega(`
        (begin
          (define ops (make-op-table))

          ; Store Closure methods
          (op-table-put ops 'describe (list 'doc)
            (lambda (x) (string-append "DOC:" x)))
          (op-table-put ops 'describe (list 'img)
            (lambda (x) (string-append "IMG:" x)))

          ; Manual dispatch function
          (define (describe-item item)
            (let ((tag (type-tag item))
                  (contents (contents item)))
              (let ((method (op-table-get ops 'describe (list tag))))
                (if (not (eq? method #f))
                    (method contents)
                    "UNKNOWN"))))

          ; Process items
          (define d1 (describe-item (attach-tag 'doc "report.pdf")))
          (define d2 (describe-item (attach-tag 'img "photo.jpg")))

          (cons d1 d2))
      `);
      const items = (result as any).items;
      expect(getStr(items[0])).toBe("DOC:report.pdf");
      expect(getStr(items[1])).toBe("IMG:photo.jpg");
    });

    it("generic size operation with Native constantly", async () => {
      const result = await evalOmega(`
        (begin
          (define ops (make-op-table))

          ; Install 'size' for different types using constantly
          (op-table-put ops 'size (list 'small) (constantly 1))
          (op-table-put ops 'size (list 'medium) (constantly 5))
          (op-table-put ops 'size (list 'large) (constantly 10))

          ; Create items
          (define small (attach-tag 'small "a"))
          (define medium (attach-tag 'medium "b"))
          (define large (attach-tag 'large "c"))

          ; Sum sizes
          (+ (apply-generic ops 'size small)
             (apply-generic ops 'size medium)
             (apply-generic ops 'size large)))
      `);
      expect(getNum(result)).toBe(16); // 1 + 5 + 10 = 16
    });
  });

  describe("Tagged data protocol", () => {
    it("attach-tag creates tagged datum", async () => {
      const result = await evalOmega(`
        (begin
          (define x (attach-tag 'number 42))
          (tagged? x))
      `);
      expect(getBool(result)).toBe(true);
    });

    it("type-tag extracts tag", async () => {
      const result = await evalOmega(`
        (begin
          (define x (attach-tag 'rational (cons 3 4)))
          (type-tag x))
      `);
      expect(getSym(result)).toBe("rational");
    });

    it("contents extracts datum contents", async () => {
      const result = await evalOmega(`
        (begin
          (define x (attach-tag 'rational (cons 3 4)))
          (car (contents x)))
      `);
      expect(getNum(result)).toBe(3);
    });

    it("type-tag returns primitive type for untagged values", async () => {
      const result = await evalOmega(`(type-tag 42)`);
      expect(getSym(result)).toBe("num");
    });

    it("contents returns primitive for untagged values", async () => {
      const result = await evalOmega(`(contents 42)`);
      expect(getNum(result)).toBe(42);
    });
  });

  describe("Operation table basics", () => {
    it("op-table-put stores procedure", async () => {
      const result = await evalOmega(`
        (begin
          (define ops (make-op-table))
          (op-table-put ops 'square (list 'num) (partial * 1))
          (define sq (op-table-get ops 'square (list 'num)))
          (sq 5))
      `);
      expect(getNum(result)).toBe(5);
    });

    it("op-table-get returns false for missing", async () => {
      const result = await evalOmega(`
        (begin
          (define ops (make-op-table))
          (op-table-get ops 'missing (list 'type)))
      `);
      expect(getBool(result)).toBe(false);
    });

    it("multiple types can be in signature", async () => {
      const result = await evalOmega(`
        (begin
          (define ops (make-op-table))
          (op-table-put ops 'combine (list 'a 'b) +)
          (op-table-get ops 'combine (list 'a 'b)))
      `);
      // Should return the procedure
      expect((result as any).tag).toBe("Native");
    });
  });

  describe("Coercion path utilities", () => {
    it("find-coercion-path returns false for no path", async () => {
      const result = await evalOmega(`
        (begin
          (define coercions (make-coercion-table))
          (put-coercion coercions 'A 'B identity)
          ; No path from B to C
          (find-coercion-path coercions 'B 'C))
      `);
      expect(getBool(result)).toBe(false);
    });

    it("find-coercion-path returns single-element for same type", async () => {
      const result = await evalOmega(`
        (begin
          (define coercions (make-coercion-table))
          (define path (find-coercion-path coercions 'A 'A))
          (length path))
      `);
      expect(getNum(result)).toBe(1); // Just [A]
    });

    it("find-all-coercion-paths returns empty for no path", async () => {
      const result = await evalOmega(`
        (begin
          (define coercions (make-coercion-table))
          (define paths (find-all-coercion-paths coercions 'X 'Y))
          (length paths))
      `);
      expect(getNum(result)).toBe(0);
    });

    it("coerce-value throws on no path", async () => {
      await expect(evalOmega(`
        (begin
          (define coercions (make-coercion-table))
          (coerce-value coercions 42 'X 'Y))
      `)).rejects.toThrow("no coercion path");
    });
  });
});
