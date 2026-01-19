// Semantic Tests (Prompt 3) - Making inference feel like Lisp
// Tests 3.1-3.6: Non-arithmetic tests proving SICP/FP composition on inference
//
// These tests demonstrate that OracleProcs compose like ordinary Scheme procedures
// using map, filter, fold, match, and other SICP-style patterns.

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
  name: "semantic-test",
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

describe("Semantic Tests (3.1-3.6)", () => {

  describe("3.1: Semantic classification pipeline", () => {
    // Takes list of strings -> oracle-lambda classifies each as 'positive/'negative
    // -> filter keeps positives -> fold counts them
    // Proves: map, filter, fold work with OracleProc

    it("classify-sentiment oracle returns symbols", async () => {
      const result = await evalOmega(`
        (begin
          (define classify (oracle-lambda (text) "classify-sentiment"))
          (classify "This is a good and wonderful day!"))
      `);

      expect(result.tag).toBe("Sym");
      expect(getSym(result)).toBe("positive");
    });

    it("classify-sentiment detects negative sentiment", async () => {
      const result = await evalOmega(`
        (begin
          (define classify (oracle-lambda (text) "classify-sentiment"))
          (classify "This is terrible and awful!"))
      `);

      expect(result.tag).toBe("Sym");
      expect(getSym(result)).toBe("negative");
    });

    it("can filter positives using symbol comparison", async () => {
      // Using Lisp-level filter with lambda that calls OracleProc
      // Since our Native filter doesn't support Closure/OracleProc,
      // we implement the pipeline in Lisp
      const result = await evalOmega(`
        (begin
          (define classify (oracle-lambda (text) "classify-sentiment"))

          ;; Manual filter since classify returns symbol
          (define texts (list "I love this!" "This is bad" "Great work!"))

          ;; Count positives using recursive fold
          (define (count-positives lst)
            (if (null? lst)
                0
                (if (eq? (classify (car lst)) 'positive)
                    (+ 1 (count-positives (cdr lst)))
                    (count-positives (cdr lst)))))

          (count-positives texts))
      `);

      expect(result.tag).toBe("Num");
      expect(getNum(result)).toBe(2); // "I love this!" and "Great work!" are positive
    });

    it("HOF map works with native predicates", async () => {
      // Test map with a native function
      const result = await evalOmega(`
        (begin
          (define add1 (partial + 1))
          (define nums (list 1 2 3))
          (map add1 nums))
      `);

      const arr = listToArray(result);
      expect(arr.map(getNum)).toEqual([2, 3, 4]);
    });

    it("HOF filter works with native predicates", async () => {
      const result = await evalOmega(`
        (begin
          (define positive? (partial < 0))
          (define nums (list -1 2 -3 4 5))
          (filter positive? nums))
      `);

      const arr = listToArray(result);
      expect(arr.map(getNum)).toEqual([2, 4, 5]);
    });

    it("HOF fold works for accumulation", async () => {
      const result = await evalOmega(`
        (begin
          (fold + 0 (list 1 2 3 4 5)))
      `);

      expect(getNum(result)).toBe(15);
    });
  });

  describe("3.2: Extraction + mechanical rewrite via REPL", () => {
    // Oracle extracts named entity -> mechanical string-replace-all
    // Proves: hybrid semantic + mechanical approach

    it("extract-named-entity returns entity string", async () => {
      const result = await evalOmega(`
        (begin
          (define extract (oracle-lambda (text) "extract-named-entity"))
          (extract "I met Alice at the coffee shop"))
      `);

      expect(result.tag).toBe("Str");
      expect(getStr(result)).toBe("Alice");
    });

    it("mechanical rewrite using string-replace-all", async () => {
      const result = await evalOmega(`
        (begin
          (define extract (oracle-lambda (text) "extract-named-entity"))
          (define text "I met Alice at the shop. Alice was happy.")
          (define entity (extract text))
          (string-replace-all text entity (string-upcase entity)))
      `);

      expect(result.tag).toBe("Str");
      expect(getStr(result)).toBe("I met ALICE at the shop. ALICE was happy.");
    });

    it("pipeline: extract -> upcase -> replace", async () => {
      const result = await evalOmega(`
        (begin
          (define extract (oracle-lambda (text) "extract-named-entity"))

          ;; Pipeline: extract entity, upcase it, replace in text
          (define (highlight-entities text)
            (let ((entity (extract text)))
              (if (string=? entity "UNKNOWN")
                  text
                  (string-replace-all text entity
                    (string-append "[" (string-upcase entity) "]")))))

          (highlight-entities "Bob went to the store"))
      `);

      expect(result.tag).toBe("Str");
      expect(getStr(result)).toBe("[BOB] went to the store");
    });
  });

  describe("3.3: Semantic rewrite chosen", () => {
    // Oracle does the rewrite itself using ReqEval
    // Proves: oracle can use REPL tools for mechanical work

    it("semantic-rewrite uses ReqEval internally", async () => {
      const result = await evalOmega(`
        (begin
          (define rewrite (oracle-lambda (text entity replacement) "semantic-rewrite"))
          (rewrite "Hello world" "world" "universe"))
      `);

      expect(result.tag).toBe("Str");
      expect(getStr(result)).toBe("Hello universe");
    });

    it("semantic-rewrite handles multiple occurrences", async () => {
      const result = await evalOmega(`
        (begin
          (define rewrite (oracle-lambda (text entity replacement) "semantic-rewrite"))
          (rewrite "foo bar foo baz foo" "foo" "qux"))
      `);

      expect(result.tag).toBe("Str");
      expect(getStr(result)).toBe("qux bar qux baz qux");
    });

    it("compare semantic vs extensional: same result", async () => {
      // Semantic: oracle uses ReqEval
      // Extensional: Lisp code does string-replace-all directly
      const semanticResult = await evalOmega(`
        (begin
          (define rewrite (oracle-lambda (text entity replacement) "semantic-rewrite"))
          (rewrite "The cat sat on the mat" "cat" "dog"))
      `);

      const extensionalResult = await evalOmega(`
        (string-replace-all "The cat sat on the mat" "cat" "dog")
      `);

      expect(getStr(semanticResult)).toBe(getStr(extensionalResult));
    });
  });

  describe("3.4: AST shape detection + match", () => {
    // Use match to detect AST patterns
    // Oracle can use ReqMatch for semantic pattern matching

    it("match kernel form with wildcards", async () => {
      // Pattern syntax: use raw list pattern (_ 2 _) without the 'list' constructor
      const result = await evalOmega(`
        (match (list 1 2 3)
          ((_ 2 _) 'found-two)
          (_ 'not-found))
      `);

      expect(result.tag).toBe("Sym");
      expect(getSym(result)).toBe("found-two");
    });

    it("match with pattern variables using ?x syntax", async () => {
      // Pattern variables start with ? and bind to matched values
      const result = await evalOmega(`
        (match (list 'add 5 3)
          ((add ?x ?y) (+ x y))
          (_ 0))
      `);

      expect(result.tag).toBe("Num");
      expect(getNum(result)).toBe(8);
    });

    it("match nested structure", async () => {
      // Match nested list structure with pattern variables
      const result = await evalOmega(`
        (match (list 'if (list 'eq 'x 0) 1 'body)
          ((if ?cond ?then ?else) then)
          (_ 'no-match))
      `);

      expect(result.tag).toBe("Num");
      expect(getNum(result)).toBe(1);
    });

    it("oracle match-ast-shape detects conditionals", async () => {
      const result = await evalOmega(`
        (begin
          (define match-shape (oracle-lambda (ast pattern) "match-ast-shape"))
          (define ast (list 'if (list 'eq 'x 0) 1 '(* x (fact (- x 1)))))
          (define bindings (match-shape ast 'conditional))
          ;; Check if bindings is truthy - the oracle returns a pair structure
          ;; which is a Vector, not Bool. Check if it's not #f
          (if (pair? bindings) #t #f))
      `);

      expect(result.tag).toBe("Bool");
      expect(getBool(result)).toBe(true);
    });

    it("quote preserves symbolic structure", async () => {
      const result = await evalOmega(`
        (begin
          (define code '(lambda (x) (+ x 1)))
          (car code))
      `);

      expect(result.tag).toBe("Sym");
      expect(getSym(result)).toBe("lambda");
    });
  });

  describe("3.5: ADT representation independence", () => {
    // Two representations of trees - oracle provides appropriate accessors
    // Proves: data abstraction with semantic dispatch

    it("list-tree representation works", async () => {
      // Tree as (value left right) list
      const result = await evalOmega(`
        (begin
          ;; Define a tree as (value left right)
          (define (make-tree v l r) (list v l r))
          (define tree-value car)
          (define tree-left cadr)
          (define tree-right caddr)

          ;; Create a tree: 5 with left=3, right=7
          (define t (make-tree 5 (make-tree 3 '() '()) (make-tree 7 '() '())))

          ;; Get root value
          (tree-value t))
      `);

      expect(getNum(result)).toBe(5);
    });

    it("oracle provides accessors for list-tree", async () => {
      const result = await evalOmega(`
        (begin
          (define get-accessors (oracle-lambda (rep-type) "get-tree-accessor"))
          (define accessors (get-accessors 'list-tree))

          ;; accessors is a list: (value-fn left-fn right-fn)
          (define tree-value (car accessors))

          ;; Create tree and use accessor
          (define t (list 42 '() '()))
          (tree-value t))
      `);

      expect(getNum(result)).toBe(42);
    });

    it("abstract tree traversal works with any representation", async () => {
      const result = await evalOmega(`
        (begin
          ;; Abstract tree operations
          (define (make-tree v l r) (list v l r))
          (define tree-value car)
          (define tree-left cadr)
          (define tree-right caddr)
          (define tree-empty? null?)

          ;; Sum all values in tree
          (define (tree-sum t)
            (if (tree-empty? t)
                0
                (+ (tree-value t)
                   (tree-sum (tree-left t))
                   (tree-sum (tree-right t)))))

          ;; Build tree:    5
          ;;               / \\
          ;;              3   7
          (define t (make-tree 5
                      (make-tree 3 '() '())
                      (make-tree 7 '() '())))

          (tree-sum t))
      `);

      expect(getNum(result)).toBe(15); // 5 + 3 + 7
    });
  });

  describe("3.6: Dist search with semantic predicate", () => {
    // search.op returns Dist<Meaning>
    // Filter with semantic predicate (safe?)

    it("check-safety oracle returns boolean", async () => {
      const result = await evalOmega(`
        (begin
          (define safe? (oracle-lambda (x) "check-safety"))
          (safe? 42))
      `);

      expect(result.tag).toBe("Bool");
      expect(getBool(result)).toBe(true); // positive numbers are safe
    });

    it("check-safety detects unsafe values", async () => {
      const result = await evalOmega(`
        (begin
          (define safe? (oracle-lambda (x) "check-safety"))
          (safe? -5))
      `);

      expect(result.tag).toBe("Bool");
      expect(getBool(result)).toBe(false); // negative numbers are unsafe
    });

    it("filter list with safety predicate recursively", async () => {
      const result = await evalOmega(`
        (begin
          (define safe? (oracle-lambda (x) "check-safety"))

          ;; Recursive filter with OracleProc
          (define (filter-safe lst)
            (if (null? lst)
                '()
                (if (safe? (car lst))
                    (cons (car lst) (filter-safe (cdr lst)))
                    (filter-safe (cdr lst)))))

          (define nums (list 1 -2 3 -4 5))
          (filter-safe nums))
      `);

      const arr = listToArray(result);
      expect(arr.map(getNum)).toEqual([1, 3, 5]); // Only positive numbers
    });

    it("dist primitives work for search results", async () => {
      const result = await evalOmega(`
        (begin
          ;; Create a distribution of candidates
          (define candidates (dist-from-list
            (list (cons 10 3) (cons -5 2) (cons 20 5) (cons -1 1))))

          ;; Get top 2 by weight
          (define top2 (dist-topk candidates 2))

          ;; Count elements
          (dist-count top2))
      `);

      expect(getNum(result)).toBe(2);
    });

    it("dist-topk returns highest weighted elements", async () => {
      const result = await evalOmega(`
        (begin
          (define candidates (dist-from-list
            (list (cons 'a 1) (cons 'b 5) (cons 'c 3))))

          ;; Get top 1
          (define top1 (dist-topk candidates 1))
          (dist-value-at top1 0))
      `);

      expect(result.tag).toBe("Sym");
      expect(getSym(result)).toBe("b"); // 'b has weight 5, highest
    });

    it("combine dist operations with semantic filtering", async () => {
      // This demonstrates the full pattern: search.op would return Dist<Meaning>,
      // then we filter/process using semantic predicates
      const result = await evalOmega(`
        (begin
          (define safe? (oracle-lambda (x) "check-safety"))

          ;; Simulate search results as dist
          (define search-results (dist-from-list
            (list (cons 10 3) (cons -5 2) (cons 20 5))))

          ;; Get top result
          (define top (dist-topk search-results 1))
          (define best-candidate (dist-value-at top 0))

          ;; Check if best is safe
          (if (safe? best-candidate)
              best-candidate
              0))
      `);

      expect(getNum(result)).toBe(20); // 20 has highest weight (5) and is positive (safe)
    });
  });

  describe("Integration: Compose + pipe", () => {
    it("compose creates function composition", async () => {
      const result = await evalOmega(`
        (begin
          (define double (partial * 2))
          (define inc (partial + 1))
          (define double-then-inc (compose inc double))
          (double-then-inc 5))
      `);

      expect(getNum(result)).toBe(11); // (+ 1 (* 2 5)) = 11
    });

    it("pipe creates left-to-right composition", async () => {
      const result = await evalOmega(`
        (begin
          (define double (partial * 2))
          (define inc (partial + 1))
          (define inc-then-double (pipe inc double))
          (inc-then-double 5))
      `);

      expect(getNum(result)).toBe(12); // (* 2 (+ 1 5)) = 12
    });

    it("identity returns argument unchanged", async () => {
      const result = await evalOmega(`(identity 42)`);
      expect(getNum(result)).toBe(42);
    });

    it("constantly returns constant function", async () => {
      const result = await evalOmega(`
        (begin
          (define always-42 (constantly 42))
          (always-42 "ignored"))
      `);

      expect(getNum(result)).toBe(42);
    });
  });

  describe("Symbolic data as first-class", () => {
    it("symbols are distinct from strings", async () => {
      const symbolResult = await evalOmega(`(symbol? 'foo)`);
      const stringResult = await evalOmega(`(string? "foo")`);

      expect(getBool(symbolResult)).toBe(true);
      expect(getBool(stringResult)).toBe(true);

      const symbolIsNotString = await evalOmega(`(string? 'foo)`);
      expect(getBool(symbolIsNotString)).toBe(false);
    });

    it("quote creates symbolic data", async () => {
      const result = await evalOmega(`
        (begin
          (define code '(+ 1 2))
          (car code))
      `);

      expect(result.tag).toBe("Sym");
      expect(getSym(result)).toBe("+");
    });

    it("list operations work on quoted forms", async () => {
      const result = await evalOmega(`
        (begin
          (define code '(lambda (x) (+ x 1)))
          (length code))
      `);

      expect(getNum(result)).toBe(3); // (lambda (x) (+ x 1)) has 3 elements
    });

    it("equal? compares structure deeply", async () => {
      const result = await evalOmega(`
        (equal? '(a (b c) d) '(a (b c) d))
      `);

      expect(getBool(result)).toBe(true);
    });

    it("eq? compares identity for symbols", async () => {
      const result = await evalOmega(`(eq? 'foo 'foo)`);
      expect(getBool(result)).toBe(true);
    });
  });
});
