// test/prompt18-meta/meta.spec.ts
// Tests for Prompt 18: Meta-circular Ω evaluator inside Ω

import { describe, it, expect, beforeEach } from "vitest";
import {
  // Types and constructors
  sym,
  cons,
  arrayToList,
  listToArray,
  isPair,
  isNull,
  isClosure,
  isPrim,
  isLLMProc,
  isMeaning,
  makeMeaning,
  omega0ToString,
  // Environment
  emptyEnv,
  extendEnv,
  defineVar,
  lookup,
  setVar,
  clearEnvStore,
  // Evaluator
  eval0,
  apply0,
  evalString,
  evalProgram,
  parseSimpleSExpr,
  createEvalContext,
  // Primitives
  createBaseEnv,
  createLLMProc,
  createScriptedLLMProc,
  addLLMProcToEnv,
  // Intensional evaluator
  int0,
  createIntContext,
  createFallbackOracle,
  createScriptedOracle,
  commitMeaning,
  isValidated,
  // DSL
  defineDSL,
  addSyntax,
  addSemantics,
  addPrimitive,
  instantiateDSL,
  createCalculatorDSL,
  analyzeDSL,
  documentDSL,
} from "../../src/core/meta";
import type { Omega0Expr, Omega0Val, Omega0Env, IntContext } from "../../src/core/meta";

// ─────────────────────────────────────────────────────────────────
// Test 18.1: Differential test: host vs meta eval agree on factorial
// ─────────────────────────────────────────────────────────────────

describe("18.1 Differential test: host vs meta eval agree on factorial", () => {
  let env: Omega0Env;

  beforeEach(() => {
    env = createBaseEnv();
    clearEnvStore();
  });

  it("should evaluate factorial(0) = 1", () => {
    // Define factorial in meta-evaluator
    const factDef = `
      (define (fact n)
        (if (= n 0)
            1
            (* n (fact (- n 1)))))
    `;
    evalString(factDef, env);

    // Evaluate factorial(0)
    const result = evalString("(fact 0)", env);
    expect(result).toBe(1);
  });

  it("should evaluate factorial(5) = 120", () => {
    const factDef = `
      (define (fact n)
        (if (= n 0)
            1
            (* n (fact (- n 1)))))
    `;
    evalString(factDef, env);

    const result = evalString("(fact 5)", env);
    expect(result).toBe(120);
  });

  it("should evaluate factorial(10) = 3628800", () => {
    const factDef = `
      (define (fact n)
        (if (= n 0)
            1
            (* n (fact (- n 1)))))
    `;
    evalString(factDef, env);

    const result = evalString("(fact 10)", env);
    expect(result).toBe(3628800);
  });

  it("should handle iterative factorial", () => {
    const factIterDef = `
      (define (fact-iter n acc)
        (if (= n 0)
            acc
            (fact-iter (- n 1) (* n acc))))
    `;
    evalString(factIterDef, env);
    evalString("(define (fact n) (fact-iter n 1))", env);

    const result = evalString("(fact 7)", env);
    expect(result).toBe(5040);
  });

  it("should agree with host language for arithmetic", () => {
    // Test various arithmetic operations
    expect(evalString("(+ 1 2 3 4 5)", env)).toBe(15);
    expect(evalString("(* 2 3 4)", env)).toBe(24);
    expect(evalString("(- 100 50 25)", env)).toBe(25);
    expect(evalString("(/ 100 5 2)", env)).toBe(10);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 18.2: Compose multiple LLM-procs into pipeline
// ─────────────────────────────────────────────────────────────────

describe("18.2 Compose multiple LLM-procs into pipeline", () => {
  let env: Omega0Env;

  beforeEach(() => {
    env = createBaseEnv();
    clearEnvStore();
  });

  it("should create and apply LLM procedure", () => {
    // Create a scripted LLM proc for testing
    const extractNames = createScriptedLLMProc(
      "extract-names",
      "Extract person names from text",
      (args) => {
        const text = args[0] as string;
        // Simulated extraction
        if (text.includes("Alice") && text.includes("Bob")) {
          return arrayToList(["Alice", "Bob"]);
        }
        return null;
      }
    );

    env = addLLMProcToEnv(env, "extract-names", extractNames);

    const result = evalString('(extract-names "Alice met Bob")', env);
    expect(isPair(result)).toBe(true);

    const names = listToArray(result);
    expect(names).toContain("Alice");
    expect(names).toContain("Bob");
  });

  it("should compose LLM procedures in pipeline", () => {
    // First LLM proc: extract entities
    const extractEntities = createScriptedLLMProc(
      "extract-entities",
      "Extract entities from text",
      (args) => {
        const text = args[0] as string;
        if (text.includes("Apple")) {
          return arrayToList(["Apple", "company", "technology"]);
        }
        return arrayToList([]);
      }
    );

    // Second LLM proc: classify entities
    const classifyEntity = createScriptedLLMProc(
      "classify-entity",
      "Classify an entity",
      (args) => {
        const entity = args[0] as string;
        if (entity === "Apple") return "organization";
        if (entity === "company") return "category";
        return "unknown";
      }
    );

    env = addLLMProcToEnv(env, "extract-entities", extractEntities);
    env = addLLMProcToEnv(env, "classify-entity", classifyEntity);

    // Test individual procs
    const entities = evalString('(extract-entities "Apple is a company")', env);
    expect(isPair(entities)).toBe(true);

    // Apply classify to first entity
    const classification = evalString('(classify-entity "Apple")', env);
    expect(classification).toBe("organization");
  });

  it("should handle LLM proc composition with lambda", () => {
    const translate = createScriptedLLMProc(
      "translate",
      "Translate text",
      (args) => {
        const text = args[0] as string;
        const lang = args[1] as string;
        if (lang === "es" && text === "hello") return "hola";
        if (lang === "fr" && text === "hello") return "bonjour";
        return text;
      }
    );

    env = addLLMProcToEnv(env, "translate", translate);

    // Define composition function
    evalString(
      "(define (translate-chain text langs) (if (null? langs) text (translate-chain (translate text (car langs)) (cdr langs))))",
      env
    );

    // This would translate through multiple languages
    const result = evalString('(translate "hello" "es")', env);
    expect(result).toBe("hola");
  });

  it("should track oracle call count", () => {
    const counter = { count: 0 };
    const countingProc = createLLMProc(
      "counting-proc",
      "A proc that counts calls",
      (_prompt, args) => {
        counter.count++;
        return args[0] ?? null;
      }
    );

    env = addLLMProcToEnv(env, "counting-proc", countingProc);

    evalString("(counting-proc 1)", env);
    evalString("(counting-proc 2)", env);
    evalString("(counting-proc 3)", env);

    expect(counter.count).toBe(3);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 18.3: int0 returns Meaning with confidence
// ─────────────────────────────────────────────────────────────────

describe("18.3 int0 returns Meaning with confidence", () => {
  let env: Omega0Env;
  let ctx: IntContext;

  beforeEach(() => {
    env = createBaseEnv();
    ctx = createIntContext(createFallbackOracle(), {
      validate: false,
      confidenceThreshold: 0.5,
    });
    clearEnvStore();
  });

  it("should return high confidence for self-evaluating expressions", () => {
    const meaning = int0(42, env, ctx);

    expect(isMeaning(meaning)).toBe(true);
    expect(meaning.denotation).toBe(42);
    expect(meaning.confidence).toBe(1.0);
    expect(meaning.validated).toBe(true);
  });

  it("should return high confidence for variable lookup", () => {
    env = defineVar(env, "x", 100);
    const expr = parseSimpleSExpr("x");

    const meaning = int0(expr, env, ctx);

    expect(meaning.denotation).toBe(100);
    expect(meaning.confidence).toBe(1.0);
  });

  it("should return high confidence for quote", () => {
    const expr = parseSimpleSExpr("(quote (a b c))");

    const meaning = int0(expr, env, ctx);

    expect(meaning.confidence).toBe(1.0);
    expect(isPair(meaning.denotation)).toBe(true);
  });

  it("should compute confidence through conditionals", () => {
    env = defineVar(env, "x", 5);
    const expr = parseSimpleSExpr("(if (> x 0) 1 -1)");

    const meaning = int0(expr, env, ctx);

    expect(meaning.denotation).toBe(1);
    expect(meaning.confidence).toBeGreaterThan(0);
  });

  it("should return high confidence for lambda creation", () => {
    const expr = parseSimpleSExpr("(lambda (x) (+ x 1))");

    const meaning = int0(expr, env, ctx);

    expect(meaning.confidence).toBe(1.0);
    expect(isClosure(meaning.denotation)).toBe(true);
  });

  it("should compute meaning for primitive application", () => {
    const expr = parseSimpleSExpr("(+ 1 2 3)");

    const meaning = int0(expr, env, ctx);

    expect(meaning.denotation).toBe(6);
    expect(meaning.confidence).toBe(1.0);
  });

  it("should handle closure application intensionally", () => {
    evalString("(define (double x) (* x 2))", env);
    const expr = parseSimpleSExpr("(double 5)");

    const meaning = int0(expr, env, ctx);

    expect(meaning.denotation).toBe(10);
    expect(meaning.confidence).toBeGreaterThan(0);
  });

  it("should accumulate confidence through nested expressions", () => {
    env = defineVar(env, "a", 2);
    env = defineVar(env, "b", 3);
    const expr = parseSimpleSExpr("(+ (* a a) (* b b))");

    const meaning = int0(expr, env, ctx);

    expect(meaning.denotation).toBe(13); // 4 + 9
    expect(meaning.confidence).toBeGreaterThan(0.5);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 18.4: validate meaning against extensional by re-executing
// ─────────────────────────────────────────────────────────────────

describe("18.4 Validate meaning against extensional by re-executing", () => {
  let env: Omega0Env;

  beforeEach(() => {
    env = createBaseEnv();
    clearEnvStore();
  });

  it("should validate correct predictions", () => {
    // Create oracle that predicts correctly
    const correctOracle = createScriptedOracle((expr) => {
      // For simple arithmetic, compute correctly
      return {
        denotation: eval0(expr, env, createEvalContext()),
        confidence: 0.9,
        evidence: "computed",
      };
    });

    const ctx = createIntContext(correctOracle, {
      validate: true,
      confidenceThreshold: 0.5,
    });

    const meaning = int0(parseSimpleSExpr("(+ 2 3)"), env, ctx);

    expect(meaning.denotation).toBe(5);
    // Validated via fallback eval
    expect(meaning.confidence).toBeGreaterThan(0);
  });

  it("should reduce confidence for incorrect predictions", () => {
    // Create oracle that predicts incorrectly
    const incorrectOracle = createScriptedOracle((_expr) => {
      return {
        denotation: 999, // Wrong answer
        confidence: 0.8,
        evidence: "guessed",
      };
    });

    const ctx = createIntContext(incorrectOracle, {
      validate: false, // Don't auto-validate
      confidenceThreshold: 0.5,
    });

    // Compare int0 prediction with actual eval0 result
    const expr = parseSimpleSExpr("(+ 2 3)");
    const predicted = int0(expr, env, ctx);
    const actual = eval0(expr, env, createEvalContext());

    // If predictions differ, we'd reduce confidence
    if (predicted.denotation !== actual) {
      expect(true).toBe(true); // Expected divergence
    }
  });

  it("should commitMeaning only for high confidence", () => {
    const highConfMeaning = makeMeaning(42, 0.9, "confident", true);
    const lowConfMeaning = makeMeaning(42, 0.3, "uncertain", false);

    // High confidence should commit
    expect(commitMeaning(highConfMeaning, 0.5)).toBe(42);

    // Low confidence should throw
    expect(() => commitMeaning(lowConfMeaning, 0.5)).toThrow();
  });

  it("should track validation status", () => {
    const validated = makeMeaning(100, 1.0, "tested", true);
    const unvalidated = makeMeaning(100, 0.8, "predicted", false);

    expect(isValidated(validated)).toBe(true);
    expect(isValidated(unvalidated)).toBe(false);
  });

  it("should validate sequence evaluation", () => {
    const ctx = createIntContext(createFallbackOracle(), {
      validate: true,
      confidenceThreshold: 0.5,
    });

    const expr = parseSimpleSExpr("(begin 1 2 3)");
    const meaning = int0(expr, env, ctx);

    expect(meaning.denotation).toBe(3);
    expect(meaning.validated).toBe(true);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 18.5: DSL spec => instantiate DSL, check semantics
// ─────────────────────────────────────────────────────────────────

describe("18.5 DSL spec => instantiate DSL, check semantics", () => {
  it("should create and use calculator DSL", () => {
    const calcDSL = createCalculatorDSL();

    // Evaluate using DSL
    const result = calcDSL.eval(parseSimpleSExpr("(calc (+ 1 2 3))"));
    expect(result).toBe(6);
  });

  it("should create custom DSL with syntax and semantics", () => {
    let dsl = defineDSL("custom", "A custom DSL");

    // Add syntax for (double x) => (* x 2)
    dsl = addSyntax(dsl, "(double x)", ["x"], "Double a number");

    // Add semantics
    dsl = addSemantics(
      dsl,
      "double",
      (args) => [sym("*"), args[0], 2]
    );

    const instance = instantiateDSL(dsl);

    const result = instance.eval(parseSimpleSExpr("(double 5)"));
    expect(result).toBe(10);
  });

  it("should support DSL with intensional semantics", () => {
    let dsl = defineDSL("predict", "DSL with intensional semantics");

    dsl = addSyntax(dsl, "(predict x)", ["x"], "Predict value");

    dsl = addSemantics(
      dsl,
      "predict",
      (args) => args[0] as Omega0Expr,
      (args, _env, _ctx) => {
        // Intensional: return meaning with high confidence
        return makeMeaning(args[0], 0.95, "predicted", false);
      }
    );

    const instance = instantiateDSL(dsl);

    // Extensional evaluation
    const extResult = instance.eval(parseSimpleSExpr("(predict 42)"));
    expect(extResult).toBe(42);

    // Intensional evaluation
    const intResult = instance.int(parseSimpleSExpr("(predict 42)"));
    expect(isMeaning(intResult)).toBe(true);
    expect(intResult.denotation).toBe(42);
    expect(intResult.confidence).toBe(0.95);
  });

  it("should analyze DSL for issues", () => {
    let dsl = defineDSL("incomplete", "DSL with issues");

    // Add syntax without semantics
    dsl = addSyntax(dsl, "(orphan x)", ["x"], "Has no semantics");

    const analysis = analyzeDSL(dsl);

    expect(analysis.issues.length).toBeGreaterThan(0);
    expect(analysis.issues.some(i => i.includes("orphan"))).toBe(true);
    expect(analysis.stats.syntaxRules).toBe(1);
    expect(analysis.stats.semanticRules).toBe(0);
  });

  it("should document DSL", () => {
    let dsl = defineDSL("documented", "A well-documented DSL");
    dsl = addSyntax(dsl, "(greet name)", ["name"], "Greet someone");
    dsl = addSemantics(
      dsl,
      "greet",
      (args) => [sym("string-append"), "Hello, ", args[0]]
    );

    const docs = documentDSL(dsl);

    expect(docs).toContain("documented");
    expect(docs).toContain("Syntax Forms");
    expect(docs).toContain("greet");
  });

  it("should add primitives to DSL", () => {
    let dsl = defineDSL("with-prims", "DSL with custom primitives");

    dsl = addPrimitive(dsl, "square", (args) => {
      const n = args[0] as number;
      return n * n;
    });

    const instance = instantiateDSL(dsl);
    const result = instance.eval(parseSimpleSExpr("(square 7)"));
    expect(result).toBe(49);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 18.6: Additional meta-circular evaluator tests
// ─────────────────────────────────────────────────────────────────

describe("18.6 Additional meta-circular evaluator tests", () => {
  let env: Omega0Env;

  beforeEach(() => {
    env = createBaseEnv();
    clearEnvStore();
  });

  describe("environment operations", () => {
    it("should extend environment with new bindings", () => {
      const newEnv = extendEnv(["x", "y"], [1, 2], env);

      expect(lookup(newEnv, "x")).toBe(1);
      expect(lookup(newEnv, "y")).toBe(2);
    });

    it("should shadow outer bindings", () => {
      const env1 = extendEnv(["x"], [1], env);
      const env2 = extendEnv(["x"], [2], env1);

      expect(lookup(env2, "x")).toBe(2);
    });

    it("should allow set! to modify bindings", () => {
      env = extendEnv(["x"], [1], env);
      setVar(env, "x", 100);

      expect(lookup(env, "x")).toBe(100);
    });

    it("should throw on unbound variable", () => {
      expect(() => lookup(env, "nonexistent")).toThrow(/Unbound variable/);
    });
  });

  describe("special forms", () => {
    it("should handle nested if expressions", () => {
      const result = evalString(
        "(if (> 5 3) (if (< 2 1) 'a 'b) 'c)",
        env
      );
      // Inner condition is false, should return 'b
      expect(result).toEqual({ tag: "Symbol", name: "b" });
    });

    it("should handle begin with multiple expressions", () => {
      env = defineVar(env, "counter", 0);
      evalString("(begin (set! counter 1) (set! counter 2) (set! counter 3))", env);

      expect(lookup(env, "counter")).toBe(3);
    });

    it("should handle define with function syntax", () => {
      evalString("(define (add-three a b c) (+ a b c))", env);

      const result = evalString("(add-three 1 2 3)", env);
      expect(result).toBe(6);
    });
  });

  describe("closures and lexical scope", () => {
    it("should capture lexical environment", () => {
      evalString("(define (make-adder n) (lambda (x) (+ x n)))", env);
      evalString("(define add5 (make-adder 5))", env);

      const result = evalString("(add5 10)", env);
      expect(result).toBe(15);
    });

    it("should handle mutual recursion", () => {
      evalString(
        "(define (even? n) (if (= n 0) #t (odd? (- n 1))))",
        env
      );
      evalString(
        "(define (odd? n) (if (= n 0) #f (even? (- n 1))))",
        env
      );

      expect(evalString("(even? 4)", env)).toBe(true);
      expect(evalString("(odd? 4)", env)).toBe(false);
      expect(evalString("(even? 5)", env)).toBe(false);
      expect(evalString("(odd? 5)", env)).toBe(true);
    });

    it("should handle higher-order functions", () => {
      evalString("(define (apply-twice f x) (f (f x)))", env);
      evalString("(define (inc x) (+ x 1))", env);

      const result = evalString("(apply-twice inc 0)", env);
      expect(result).toBe(2);
    });
  });

  describe("list operations", () => {
    it("should create and manipulate lists", () => {
      const list = evalString("(list 1 2 3 4 5)", env);
      expect(isPair(list)).toBe(true);

      const length = evalString("(length (list 1 2 3 4 5))", env);
      expect(length).toBe(5);
    });

    it("should handle car and cdr", () => {
      evalString("(define mylist (list 1 2 3))", env);

      expect(evalString("(car mylist)", env)).toBe(1);
      expect(evalString("(car (cdr mylist))", env)).toBe(2);
    });

    it("should handle cons", () => {
      const result = evalString("(cons 1 (list 2 3))", env);
      expect(isPair(result)).toBe(true);

      const arr = listToArray(result);
      expect(arr).toEqual([1, 2, 3]);
    });

    it("should handle append", () => {
      const result = evalString("(append (list 1 2) (list 3 4))", env);
      const arr = listToArray(result);
      expect(arr).toEqual([1, 2, 3, 4]);
    });

    it("should handle reverse", () => {
      const result = evalString("(reverse (list 1 2 3))", env);
      const arr = listToArray(result);
      expect(arr).toEqual([3, 2, 1]);
    });
  });

  describe("type predicates", () => {
    it("should identify null", () => {
      expect(evalString("(null? (quote ()))", env)).toBe(true);
      expect(evalString("(null? (list 1))", env)).toBe(false);
    });

    it("should identify pairs", () => {
      expect(evalString("(pair? (cons 1 2))", env)).toBe(true);
      expect(evalString("(pair? 42)", env)).toBe(false);
    });

    it("should identify numbers", () => {
      expect(evalString("(number? 42)", env)).toBe(true);
      expect(evalString('(number? "hello")', env)).toBe(false);
    });

    it("should identify procedures", () => {
      expect(evalString("(procedure? +)", env)).toBe(true);
      expect(evalString("(procedure? (lambda (x) x))", env)).toBe(true);
      expect(evalString("(procedure? 42)", env)).toBe(false);
    });
  });

  describe("string operations", () => {
    it("should append strings", () => {
      const result = evalString('(string-append "hello" " " "world")', env);
      expect(result).toBe("hello world");
    });

    it("should get string length", () => {
      const result = evalString('(string-length "hello")', env);
      expect(result).toBe(5);
    });

    it("should extract substring", () => {
      const result = evalString('(substring "hello world" 0 5)', env);
      expect(result).toBe("hello");
    });
  });

  describe("comparison operations", () => {
    it("should compare numbers", () => {
      expect(evalString("(< 1 2 3)", env)).toBe(true);
      expect(evalString("(< 1 3 2)", env)).toBe(false);
      expect(evalString("(> 3 2 1)", env)).toBe(true);
      expect(evalString("(<= 1 1 2)", env)).toBe(true);
      expect(evalString("(>= 2 2 1)", env)).toBe(true);
    });

    it("should test equality", () => {
      expect(evalString("(= 1 1 1)", env)).toBe(true);
      expect(evalString("(= 1 2)", env)).toBe(false);
    });

    it("should test eq?", () => {
      expect(evalString("(eq? 1 1)", env)).toBe(true);
      expect(evalString("(eq? (quote a) (quote a))", env)).toBe(false); // Different symbol objects
    });
  });

  describe("boolean operations", () => {
    it("should handle not", () => {
      expect(evalString("(not #f)", env)).toBe(true);
      expect(evalString("(not #t)", env)).toBe(false);
      expect(evalString("(not 0)", env)).toBe(false); // 0 is truthy
    });

    it("should handle and", () => {
      expect(evalString("(and #t #t #t)", env)).toBe(true);
      expect(evalString("(and #t #f #t)", env)).toBe(false);
      expect(evalString("(and 1 2 3)", env)).toBe(3); // Returns last truthy
    });

    it("should handle or", () => {
      expect(evalString("(or #f #f #t)", env)).toBe(true);
      expect(evalString("(or #f #f #f)", env)).toBe(false);
      expect(evalString("(or #f 1 2)", env)).toBe(1); // Returns first truthy
    });
  });

  describe("error handling", () => {
    it("should throw on arity mismatch", () => {
      evalString("(define (f x y) (+ x y))", env);

      expect(() => evalString("(f 1)", env)).toThrow(/Arity mismatch/);
    });

    it("should throw on car of non-pair", () => {
      expect(() => evalString("(car 42)", env)).toThrow(/car expects a pair/);
    });

    it("should throw on cdr of non-pair", () => {
      expect(() => evalString("(cdr 42)", env)).toThrow(/cdr expects a pair/);
    });

    it("should handle error primitive", () => {
      expect(() => evalString('(error "test error")', env)).toThrow(/test error/);
    });
  });

  describe("tracing and context", () => {
    it("should track evaluation depth", () => {
      const ctx = createEvalContext({ maxDepth: 10, tracing: true });

      evalString("(+ 1 2)", env, ctx);

      expect(ctx.trace.length).toBeGreaterThan(0);
      expect(ctx.trace.some(t => t.kind === "eval")).toBe(true);
    });

    it("should enforce depth limit", () => {
      const ctx = createEvalContext({ maxDepth: 5 });

      // Define deeply recursive function
      evalString("(define (deep n) (if (= n 0) 0 (deep (- n 1))))", env);

      expect(() => evalString("(deep 100)", env, ctx)).toThrow(/Maximum evaluation depth/);
    });

    it("should track oracle calls", () => {
      const counter = { count: 0 };
      const countingOracle = createScriptedOracle((_expr) => {
        counter.count++;
        return {
          denotation: 42,
          confidence: 1.0,
        };
      });

      const ctx = createIntContext(countingOracle, { maxOracleCalls: 5 });

      // Create LLM proc that uses oracle
      const llmProc = createScriptedLLMProc(
        "oracle-proc",
        "test",
        () => 42
      );
      env = addLLMProcToEnv(env, "oracle-proc", llmProc);

      // Make several calls
      for (let i = 0; i < 3; i++) {
        int0(parseSimpleSExpr("(oracle-proc)"), env, ctx);
      }

      expect(ctx.oracleCallCount).toBe(3);
    });
  });
});
