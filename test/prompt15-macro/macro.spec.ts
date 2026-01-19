// test/prompt15-macro/macro.spec.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Prompt 15: Hygienic macros + phase separation + semantic macros tests

import { describe, it, expect, beforeEach } from "vitest";

// Core syntax types
import {
  type Syntax,
  type Scope,
  type SIdent,
  type SList,
  isIdent,
  isList,
  addScope,
  removeScope,
  flipScope,
  freshScope,
  datum,
} from "../../src/core/syntax/syntax";

// Binding and resolution
import {
  type Env,
  type Binding,
  resolveIdent,
  freeIdentifierEq,
} from "../../src/core/syntax/binding";

// Macro types
import {
  type MacroTransformer,
  type MacroEnv,
  type MacroBinding,
  type ExpansionResult,
  type ExpansionContext,
  type ObligationRef,
  type ObligationKind,
  type MacroProfile,
  getMacroProfile,
  MACRO_PROFILES,
} from "../../src/core/macro/types";

// Hygiene utilities
import {
  freshScopeId,
  resetScopeCounter,
  makeDefScope,
  makeUseScope,
  makeIntroducerScope,
  makeBindScope,
  scopeSubset,
  scopeIntersection,
  scopeUnion,
  scopeDifference,
  scopeEqual,
  addScopeToSyntax,
  removeScopeFromSyntax,
  flipScopeOnSyntax,
  syntaxIdentName,
  syntaxScopes,
  makeIdent,
  makeAtom,
  makeList,
  resolveMacroIdent,
  freeIdentifierEqual,
  boundIdentifierEqual,
  syntaxToDatum,
  datumToSyntax,
  collectPatternVariables,
  alphaNormalize,
  alphaEquivalent,
} from "../../src/core/macro/hygiene";

// Expander
import {
  createMacroEnv,
  addMacroBinding,
  makeExpansionContext,
  expand,
  macroexpand1,
  macroexpand,
  makeSyntaxRulesTransformer,
  makeSemanticTransformer,
  clearExpansionState,
  getExpansionEvents,
  resetBindingIds,
  isExpansionCacheable,
  generateExpansionReceipt,
} from "../../src/core/macro/expander";

// Semantic macros
import {
  createSemanticState,
  applySemanticMacro,
  createObligation,
  createObligationsFromSpec,
  satisfyObligation,
  allObligationsSatisfied,
  getUnsatisfiedObligations,
  createEvidence,
  commitSemanticExpansion,
  handleReqExpand,
  parseDefSemantic,
  createSemanticFunction,
  parsePipelineMacro,
  createExpansionReceipt,
  verifyExpansionReceipt,
  resetSemanticState,
  resetObligationIds,
  resetEvidenceIds,
} from "../../src/core/macro/semantic";

// Syntax-rules
import {
  compileSyntaxRules,
  applySyntaxRules,
  type SRTransformer,
} from "../../src/core/expand/syntaxRules";

// ─────────────────────────────────────────────────────────────────
// Test Helpers
// ─────────────────────────────────────────────────────────────────

function makeTestIdent(name: string, scopes: Scope[] = []): SIdent {
  return { tag: "Ident", name, scopes };
}

function makeTestList(items: Syntax[], scopes: Scope[] = []): SList {
  return { tag: "List", items, scopes };
}

function makeTestAtom(value: any, scopes: Scope[] = []): Syntax {
  return { tag: "Atom", value, scopes };
}

// Create a simple syntax-rules transformer for testing
function createSimpleSyntaxRules(
  literals: SIdent[],
  rules: Array<{ pat: Syntax; tmpl: Syntax }>
): SRTransformer {
  return compileSyntaxRules(
    1, // phaseOut
    [], // envDefOut
    literals,
    rules
  );
}

// ─────────────────────────────────────────────────────────────────
// Test Suite
// ─────────────────────────────────────────────────────────────────

describe("Prompt 15: Hygienic Macros + Phase Separation + Semantic Macros", () => {
  beforeEach(() => {
    resetScopeCounter();
    resetBindingIds();
    resetSemanticState();
    clearExpansionState();
  });

  // ─────────────────────────────────────────────────────────────────
  // Test 15.1: Hygiene - Macro-Introduced Identifier Does NOT Capture User
  // ─────────────────────────────────────────────────────────────────

  describe("Test 15.1: Hygiene - macro-introduced identifier does NOT capture user identifiers", () => {
    it("should keep user tmp distinct from macro tmp", () => {
      // Define macro with-temp that introduces tmp internally
      // (define-syntax with-temp (syntax-rules () ((_ x) (let ((tmp 10)) x))))
      // User code: (let ((tmp 99)) (with-temp tmp))
      // Expected: user tmp (99) is returned, not captured by macro's tmp

      const defScope = makeDefScope("with-temp");
      const userScope = freshScopeId();

      // Pattern: (_ x)
      const pattern = makeTestList([
        makeTestIdent("_", [defScope]),
        makeTestIdent("x", [defScope]),
      ], [defScope]);

      // Template: (let ((tmp 10)) x)
      const template = makeTestList([
        makeTestIdent("let", [defScope]),
        makeTestList([
          makeTestList([
            makeTestIdent("tmp", [defScope]), // macro's tmp
            makeTestAtom(10, [defScope]),
          ], [defScope]),
        ], [defScope]),
        makeTestIdent("x", [defScope]), // pattern variable, will be substituted
      ], [defScope]);

      const tr = createSimpleSyntaxRules([], [{ pat: pattern, tmpl: template }]);

      // User input: (with-temp tmp) where user's tmp has userScope
      const userTmp = makeTestIdent("tmp", [userScope]);
      const macroCall = makeTestList([
        makeTestIdent("with-temp", [userScope]),
        userTmp,
      ], [userScope]);

      // Apply macro
      const scopeCounter = { n: 10 };
      const result = applySyntaxRules(tr, macroCall, [], scopeCounter);

      // The result should have:
      // - macro's tmp with introducer scope (different from user's tmp)
      // - user's tmp substituted from pattern variable x

      expect(isList(result)).toBe(true);
      if (isList(result)) {
        // (let ((tmp 10)) <user-tmp>)
        expect(result.items.length).toBe(3);

        // Check that the body (third item) is the user's tmp
        const body = result.items[2];
        expect(isIdent(body)).toBe(true);
        if (isIdent(body)) {
          expect(body.name).toBe("tmp");
          // User's tmp should have userScope
          expect(body.scopes).toContain(userScope);
        }

        // Check that macro's tmp in bindings has introducer scope
        const bindings = result.items[1];
        expect(isList(bindings)).toBe(true);
        if (isList(bindings) && isList(bindings.items[0])) {
          const macroTmpBinding = bindings.items[0].items[0];
          expect(isIdent(macroTmpBinding)).toBe(true);
          if (isIdent(macroTmpBinding)) {
            expect(macroTmpBinding.name).toBe("tmp");
            // Macro's tmp should NOT have userScope
            expect(macroTmpBinding.scopes).not.toContain(userScope);
          }
        }
      }
    });

    it("should distinguish macro-introduced and user identifiers via scopes", () => {
      const macroScope = makeDefScope("test-macro");
      const userScope = freshScopeId();
      const introScope = makeIntroducerScope("test-macro");

      const macroIdent = makeIdent("x", [macroScope, introScope]);
      const userIdent = makeIdent("x", [userScope]);

      // Same name but different scope sets
      expect(macroIdent.name).toBe(userIdent.name);
      expect(scopeEqual(macroIdent.scopes, userIdent.scopes)).toBe(false);

      // bound-identifier=? should be false
      expect(boundIdentifierEqual(macroIdent, userIdent)).toBe(false);
    });
  });

  // ─────────────────────────────────────────────────────────────────
  // Test 15.2: Hygiene - User Binding Does NOT Capture Macro-Introduced
  // ─────────────────────────────────────────────────────────────────

  describe("Test 15.2: Hygiene - user binding does NOT capture macro-introduced binding", () => {
    it("should generate fresh binder for swap macro", () => {
      // Macro swap! introduces t internally
      // (define-syntax swap! (syntax-rules () ((_ a b) (let ((t a)) (set! a b) (set! b t)))))
      // User: (let ((t 100)) (swap! x t))
      // Macro's t should not conflict with user's t

      const defScope = makeDefScope("swap!");

      // Pattern: (_ a b)
      const pattern = makeTestList([
        makeTestIdent("_", [defScope]),
        makeTestIdent("a", [defScope]),
        makeTestIdent("b", [defScope]),
      ], [defScope]);

      // Template: (let ((t a)) (set! a b) (set! b t))
      const template = makeTestList([
        makeTestIdent("let", [defScope]),
        makeTestList([
          makeTestList([
            makeTestIdent("t", [defScope]),  // macro-introduced t
            makeTestIdent("a", [defScope]),  // pattern var
          ], [defScope]),
        ], [defScope]),
        makeTestList([
          makeTestIdent("set!", [defScope]),
          makeTestIdent("a", [defScope]),
          makeTestIdent("b", [defScope]),
        ], [defScope]),
        makeTestList([
          makeTestIdent("set!", [defScope]),
          makeTestIdent("b", [defScope]),
          makeTestIdent("t", [defScope]),  // macro-introduced t
        ], [defScope]),
      ], [defScope]);

      const tr = createSimpleSyntaxRules([], [{ pat: pattern, tmpl: template }]);

      // User input
      const userScope = freshScopeId();
      const macroCall = makeTestList([
        makeTestIdent("swap!", [userScope]),
        makeTestIdent("x", [userScope]),
        makeTestIdent("t", [userScope]),  // user's t
      ], [userScope]);

      const scopeCounter = { n: 100 };
      const result = applySyntaxRules(tr, macroCall, [], scopeCounter);

      expect(isList(result)).toBe(true);
      if (isList(result)) {
        // Verify macro-introduced t has different scope than user's t
        const bindings = result.items[1];
        if (isList(bindings) && isList(bindings.items[0])) {
          const macroT = bindings.items[0].items[0];
          expect(isIdent(macroT)).toBe(true);
          if (isIdent(macroT)) {
            // Macro's t should have introducer scope (s#N format from syntaxRules)
            // The applySyntaxRules function generates fresh scopes like s#101, s#102
            expect(macroT.scopes.some(s => s.startsWith("s#"))).toBe(true);
            // And it should NOT have the user's scope as primary identifier
            // (it may have use scope from the macro call, but not user's original scope)
          }
        }

        // User's t (from second argument) should retain user scope
        const lastSetStx = result.items[3];
        if (isList(lastSetStx)) {
          const userT = lastSetStx.items[1]; // (set! b t) -> b is user's t
          expect(isIdent(userT)).toBe(true);
          if (isIdent(userT)) {
            expect(userT.scopes).toContain(userScope);
          }
        }
      }
    });

    it("should preserve alpha-equivalence after hygienic expansion", () => {
      // Two expansions of the same macro should be alpha-equivalent
      const defScope = makeDefScope("test");
      const pattern = makeTestList([
        makeTestIdent("_", [defScope]),
        makeTestIdent("x", [defScope]),
      ], [defScope]);
      const template = makeTestList([
        makeTestIdent("let", [defScope]),
        makeTestList([
          makeTestList([
            makeTestIdent("tmp", [defScope]),
            makeTestIdent("x", [defScope]),
          ], [defScope]),
        ], [defScope]),
        makeTestIdent("tmp", [defScope]),
      ], [defScope]);

      const tr = createSimpleSyntaxRules([], [{ pat: pattern, tmpl: template }]);

      // First expansion
      const call1 = makeTestList([
        makeTestIdent("test", []),
        makeTestAtom(42, []),
      ], []);
      const scopeCounter1 = { n: 0 };
      const result1 = applySyntaxRules(tr, call1, [], scopeCounter1);

      // Second expansion (fresh scope counter)
      const call2 = makeTestList([
        makeTestIdent("test", []),
        makeTestAtom(42, []),
      ], []);
      const scopeCounter2 = { n: 0 };
      const result2 = applySyntaxRules(tr, call2, [], scopeCounter2);

      // Results should be alpha-equivalent
      expect(alphaEquivalent(result1, result2)).toBe(true);
    });
  });

  // ─────────────────────────────────────────────────────────────────
  // Test 15.3: Phase Separation
  // ─────────────────────────────────────────────────────────────────

  describe("Test 15.3: Phase separation - runtime define cannot create macro", () => {
    it("should maintain separate macro and runtime environments", () => {
      const macroEnv = createMacroEnv();
      const runtimeEnv: Env = [];

      const defScope = makeDefScope("foo");

      // Add macro binding
      const macroTransformer: MacroTransformer = {
        tag: "SyntaxRules",
        transformer: createSimpleSyntaxRules([], []),
        defScope,
        name: "foo",
      };

      const newMacroEnv = addMacroBinding(macroEnv, "foo", macroTransformer, 1, [defScope]);

      // Add runtime binding with same name
      const runtimeBinding: Binding = {
        bid: "runtime-foo",
        name: "foo",
        scopes: [defScope],
        phase: 0,
        kind: "value",
        value: 42,
      };
      const newRuntimeEnv: Env = [...runtimeEnv, runtimeBinding];

      // Resolve at phase 1 (macro) - should find macro
      const macroIdent = makeIdent("foo", [defScope]);
      const macroResolved = resolveMacroIdent(macroIdent, newMacroEnv, 1);
      expect(macroResolved).not.toBeNull();
      expect(macroResolved?.name).toBe("foo");

      // Resolve at phase 0 (runtime) - should find runtime binding
      const runtimeResolved = resolveIdent(macroIdent, newRuntimeEnv, 0);
      expect(runtimeResolved).not.toBeNull();
      expect(runtimeResolved?.kind).toBe("value");
      expect(runtimeResolved?.value).toBe(42);

      // They are different bindings
      expect(macroResolved?.bindingId).not.toBe(runtimeResolved?.bid);
    });

    it("should prevent macro env access from runtime phase", () => {
      const macroEnv = createMacroEnv();
      const defScope = makeDefScope("macro-only");

      const transformer: MacroTransformer = {
        tag: "SyntaxRules",
        transformer: createSimpleSyntaxRules([], []),
        defScope,
        name: "macro-only",
      };

      const newMacroEnv = addMacroBinding(macroEnv, "macro-only", transformer, 1, [defScope]);

      // Try to resolve at phase 0 - should not find it
      const ident = makeIdent("macro-only", [defScope]);
      const result = resolveMacroIdent(ident, newMacroEnv, 0);
      expect(result).toBeNull();

      // Resolve at phase 1 - should find it
      const result1 = resolveMacroIdent(ident, newMacroEnv, 1);
      expect(result1).not.toBeNull();
    });
  });

  // ─────────────────────────────────────────────────────────────────
  // Test 15.4: Macroexpansion is Observable and Replayable
  // ─────────────────────────────────────────────────────────────────

  describe("Test 15.4: Macroexpansion is observable and replayable", () => {
    it("should produce deterministic expansion for pure macros", () => {
      const macroEnv = createMacroEnv();
      const runtimeEnv: Env = [];
      const defScope = makeDefScope("double");

      // Define macro: (double x) -> (+ x x)
      const pattern = makeTestList([
        makeTestIdent("_", [defScope]),
        makeTestIdent("x", [defScope]),
      ], [defScope]);
      const template = makeTestList([
        makeTestIdent("+", [defScope]),
        makeTestIdent("x", [defScope]),
        makeTestIdent("x", [defScope]),
      ], [defScope]);

      const tr = createSimpleSyntaxRules([], [{ pat: pattern, tmpl: template }]);
      const transformer: MacroTransformer = {
        tag: "SyntaxRules",
        transformer: tr,
        defScope,
        name: "double",
      };

      const envWithMacro = addMacroBinding(macroEnv, "double", transformer, 1, [defScope]);

      // Expand twice with fresh contexts
      const input = makeTestList([
        makeTestIdent("double", [defScope]),
        makeTestAtom(5, []),
      ], []);

      const ctx1 = makeExpansionContext(runtimeEnv, envWithMacro, "pragmatic");
      ctx1.scopeCounter = { n: 0 };
      const result1 = macroexpand1(input, ctx1);

      const ctx2 = makeExpansionContext(runtimeEnv, envWithMacro, "pragmatic");
      ctx2.scopeCounter = { n: 0 };
      const result2 = macroexpand1(input, ctx2);

      // Both should be cacheable (non-semantic)
      expect(isExpansionCacheable(result1)).toBe(true);
      expect(isExpansionCacheable(result2)).toBe(true);

      // Results should be alpha-equivalent
      expect(alphaEquivalent(result1.expanded, result2.expanded)).toBe(true);
    });

    it("should generate expansion receipts", () => {
      const input = makeTestList([
        makeTestIdent("test", []),
        makeTestAtom(42, []),
      ], []);

      const result: ExpansionResult = {
        expanded: makeTestList([
          makeTestIdent("+", []),
          makeTestAtom(42, []),
          makeTestAtom(42, []),
        ], []),
        obligations: [],
        evidence: [],
        semantic: false,
      };

      const receipt = createExpansionReceipt(input, result, "pragmatic");

      expect(receipt.id).toMatch(/^receipt-/);
      expect(receipt.inputHash).toBeDefined();
      expect(receipt.outputHash).toBeDefined();
      expect(receipt.profileName).toBe("pragmatic");
      expect(receipt.semantic).toBe(false);

      // Verify receipt
      expect(verifyExpansionReceipt(input, result, receipt)).toBe(true);

      // Different input should not verify
      const differentInput = makeTestList([
        makeTestIdent("other", []),
        makeTestAtom(99, []),
      ], []);
      expect(verifyExpansionReceipt(differentInput, result, receipt)).toBe(false);
    });

    it("should track expansion events", () => {
      clearExpansionState();

      const macroEnv = createMacroEnv();
      const runtimeEnv: Env = [];
      const defScope = makeDefScope("traced");

      const pattern = makeTestList([
        makeTestIdent("_", [defScope]),
      ], [defScope]);
      const template = makeTestAtom(42, [defScope]);

      const tr = createSimpleSyntaxRules([], [{ pat: pattern, tmpl: template }]);
      const transformer: MacroTransformer = {
        tag: "SyntaxRules",
        transformer: tr,
        defScope,
        name: "traced",
      };

      const envWithMacro = addMacroBinding(macroEnv, "traced", transformer, 1, [defScope]);

      const input = makeTestList([
        makeTestIdent("traced", [defScope]),
      ], [defScope]);

      const ctx = makeExpansionContext(runtimeEnv, envWithMacro, "pragmatic");
      ctx.tracing = true;
      macroexpand1(input, ctx);

      const events = getExpansionEvents();
      expect(events.length).toBeGreaterThan(0);
      expect(events[0].macro).toBe("traced");
      expect(events[0].semantic).toBe(false);
    });
  });

  // ─────────────────────────────────────────────────────────────────
  // Test 15.5: Semantic Macro - Define Semantic Function
  // ─────────────────────────────────────────────────────────────────

  describe("Test 15.5: Semantic macro - define semantic function via macro", () => {
    it("should parse defsemantic syntax", () => {
      // (defsemantic (sanitize-text txt) :goal { kind "sanitize" } :contract safety)
      const stx = makeTestList([
        makeTestIdent("defsemantic", []),
        makeTestList([
          makeTestIdent("sanitize-text", []),
          makeTestIdent("txt", []),
        ], []),
        makeTestIdent(":goal", []),
        makeTestList([
          makeTestIdent("kind", []),
          makeTestAtom("sanitize", []),
        ], []),
        makeTestIdent(":contract", []),
        makeTestIdent("safety", []),
      ], []);

      const spec = parseDefSemantic(stx);
      expect(spec).not.toBeNull();
      expect(spec?.name).toBe("sanitize-text");
      expect(spec?.params).toEqual(["txt"]);
      expect(spec?.goal.kind).toBe("sanitize");
      expect(spec?.contract).toBe("safety");
    });

    it("should create semantic function transformer", () => {
      const spec = {
        name: "process-doc",
        params: ["doc"],
        goal: { kind: "process" },
        contract: "safety",
      };

      const scopeCounter = { n: 0 };
      const transformer = createSemanticFunction(spec, scopeCounter);

      expect(transformer.tag).toBe("SemanticMacro");
      expect(transformer.name).toBe("process-doc");

      if (transformer.tag === "SemanticMacro") {
        expect(transformer.obligations).toBeDefined();
        expect(transformer.obligations?.required).toContain("test");
      }
    });

    it("should create obligations from specification", () => {
      const spec = {
        required: ["test", "metamorphic"] as ObligationKind[],
        optional: ["invariant"] as ObligationKind[],
        custom: ["Must preserve meaning"],
      };

      const obligations = createObligationsFromSpec(spec);

      expect(obligations.length).toBe(4);
      expect(obligations.some(o => o.kind === "test")).toBe(true);
      expect(obligations.some(o => o.kind === "metamorphic")).toBe(true);
      expect(obligations.some(o => o.kind === "invariant")).toBe(true);
      expect(obligations.some(o => o.description?.includes("Must preserve"))).toBe(true);
    });
  });

  // ─────────────────────────────────────────────────────────────────
  // Test 15.6: Semantic Macro Governance
  // ─────────────────────────────────────────────────────────────────

  describe("Test 15.6: Semantic macro governance - explore vs pragmatic", () => {
    it("should allow expansion under explore profile", () => {
      const profile = getMacroProfile("explore");
      expect(profile.canExpandPure).toBe(true);
      expect(profile.canExpandSemantic).toBe(true);
      expect(profile.canCommit).toBe(false);
      expect(profile.canInfer).toBe(true);
    });

    it("should allow commit under pragmatic profile", () => {
      const profile = getMacroProfile("pragmatic");
      expect(profile.canExpandPure).toBe(true);
      expect(profile.canExpandSemantic).toBe(true);
      expect(profile.canCommit).toBe(true);
      expect(profile.canInfer).toBe(true);
    });

    it("should deny semantic expansion under airgap profile", () => {
      const profile = getMacroProfile("airgap");
      expect(profile.canExpandPure).toBe(true);
      expect(profile.canExpandSemantic).toBe(false);
      expect(profile.canCommit).toBe(false);
      expect(profile.canInfer).toBe(false);
    });

    it("should deny commit under explore profile", () => {
      const expansion: ExpansionResult = {
        expanded: makeTestAtom(42, []),
        obligations: [createObligation("test", "Test obligation")],
        evidence: [],
        semantic: true,
      };

      // Satisfy the obligation
      const satisfiedObligations = expansion.obligations.map(o =>
        satisfyObligation(o, createEvidence("test-pass", "Tests passed"))
      );

      const commitRequest = {
        expansion: { ...expansion, obligations: satisfiedObligations },
        profileName: "explore",
        obligations: satisfiedObligations,
      };

      const caps = new Set(["cap.macro.commit"]);
      const result = commitSemanticExpansion(commitRequest, caps);

      expect(result.tag).toBe("denied");
      if (result.tag === "denied") {
        expect(result.reason).toContain("explore");
      }
    });

    it("should allow commit under pragmatic with satisfied obligations", () => {
      const expansion: ExpansionResult = {
        expanded: makeTestAtom(42, []),
        obligations: [createObligation("test", "Test obligation")],
        evidence: [],
        semantic: true,
      };

      // Satisfy the obligation
      const satisfiedObligations = expansion.obligations.map(o =>
        satisfyObligation(o, createEvidence("test-pass", "Tests passed"))
      );

      const commitRequest = {
        expansion: { ...expansion, obligations: satisfiedObligations },
        profileName: "pragmatic",
        obligations: satisfiedObligations,
      };

      const caps = new Set(["cap.macro.commit"]);
      const result = commitSemanticExpansion(commitRequest, caps);

      expect(result.tag).toBe("committed");
    });

    it("should require obligations before commit", () => {
      const expansion: ExpansionResult = {
        expanded: makeTestAtom(42, []),
        obligations: [createObligation("test", "Test obligation")],
        evidence: [],
        semantic: true,
      };

      const commitRequest = {
        expansion,
        profileName: "pragmatic",
        obligations: expansion.obligations, // Not satisfied
      };

      const caps = new Set(["cap.macro.commit"]);
      const result = commitSemanticExpansion(commitRequest, caps);

      expect(result.tag).toBe("pending");
      if (result.tag === "pending") {
        expect(result.unsatisfied.length).toBe(1);
      }
    });
  });

  // ─────────────────────────────────────────────────────────────────
  // Test 15.7: Oracle-Driven Macro Debugging
  // ─────────────────────────────────────────────────────────────────

  describe("Test 15.7: Oracle-driven macro debugging", () => {
    it("should handle ReqExpand for single step", () => {
      const macroEnv = createMacroEnv();
      const runtimeEnv: Env = [];
      const defScope = makeDefScope("inc");

      // Simple macro: (inc x) -> (+ x 1)
      const pattern = makeTestList([
        makeTestIdent("_", [defScope]),
        makeTestIdent("x", [defScope]),
      ], [defScope]);
      const template = makeTestList([
        makeTestIdent("+", [defScope]),
        makeTestIdent("x", [defScope]),
        makeTestAtom(1, [defScope]),
      ], [defScope]);

      const tr = createSimpleSyntaxRules([], [{ pat: pattern, tmpl: template }]);
      const transformer: MacroTransformer = {
        tag: "SyntaxRules",
        transformer: tr,
        defScope,
        name: "inc",
      };

      const envWithMacro = addMacroBinding(macroEnv, "inc", transformer, 1, [defScope]);

      const input = makeTestList([
        makeTestIdent("inc", [defScope]),
        makeTestAtom(5, []),
      ], []);

      const req = {
        tag: "ReqExpand" as const,
        qstx: input,
        envRef: "test-env" as any,
        mode: "1" as const,
        profileName: "pragmatic",
      };

      // Pass expand functions to avoid circular dependency
      const expandFns = { expand, macroexpand1 };
      const resp = handleReqExpand(req, runtimeEnv, envWithMacro, expandFns);

      expect(resp.tag).toBe("RespSyntax");
      expect(isList(resp.stx)).toBe(true);
    });

    it("should handle ReqExpand for full expansion", () => {
      const macroEnv = createMacroEnv();
      const runtimeEnv: Env = [];

      const input = makeTestList([
        makeTestIdent("begin", []),
        makeTestAtom(1, []),
        makeTestAtom(2, []),
      ], []);

      const req = {
        tag: "ReqExpand" as const,
        qstx: input,
        envRef: "test-env" as any,
        mode: "*" as const,
        profileName: "pragmatic",
      };

      // Pass expand functions to avoid circular dependency
      const expandFns = { expand, macroexpand1 };
      const resp = handleReqExpand(req, runtimeEnv, macroEnv, expandFns);

      expect(resp.tag).toBe("RespSyntax");
      // Result should be the same since no macros defined
      expect(JSON.stringify(resp.stx)).toBe(JSON.stringify(input));
    });

    it("should respect profile in ReqExpand", () => {
      const macroEnv = createMacroEnv();
      const runtimeEnv: Env = [];

      const input = makeTestList([makeTestIdent("test", [])], []);

      const req = {
        tag: "ReqExpand" as const,
        qstx: input,
        envRef: "test-env" as any,
        mode: "*" as const,
        profileName: "airgap", // Restricted profile
      };

      // Pass expand functions to avoid circular dependency
      const expandFns = { expand, macroexpand1 };
      const resp = handleReqExpand(req, runtimeEnv, macroEnv, expandFns);

      expect(resp.tag).toBe("RespSyntax");
      // Should still expand pure macros under airgap
    });
  });

  // ─────────────────────────────────────────────────────────────────
  // Test 15.8: Hygiene + Semantic Rewrite - Pipeline Macro
  // ─────────────────────────────────────────────────────────────────

  describe("Test 15.8: Hygiene + semantic rewrite - pipeline macro", () => {
    it("should parse pipeline macro definition", () => {
      // (pipeline (plan :kind plan) (fetch :kind fetch) (act :kind act) (verify :kind verify))
      const stx = makeTestList([
        makeTestIdent("pipeline", []),
        makeTestList([
          makeTestIdent("plan", []),
          makeTestIdent(":kind", []),
          makeTestIdent("plan", []),
        ], []),
        makeTestList([
          makeTestIdent("fetch", []),
          makeTestIdent(":kind", []),
          makeTestIdent("fetch", []),
        ], []),
        makeTestList([
          makeTestIdent("act", []),
          makeTestIdent(":kind", []),
          makeTestIdent("act", []),
          makeTestIdent(":snapshot", []),
          makeTestAtom(true, []),
        ], []),
        makeTestList([
          makeTestIdent("verify", []),
          makeTestIdent(":kind", []),
          makeTestIdent("verify", []),
        ], []),
      ], []);

      const stages = parsePipelineMacro(stx);
      expect(stages).not.toBeNull();
      expect(stages?.length).toBe(4);
      expect(stages?.[0].name).toBe("plan");
      expect(stages?.[0].kind).toBe("plan");
      expect(stages?.[2].name).toBe("act");
      expect(stages?.[2].snapshot).toBe(true);
    });

    it("should preserve hygiene in semantic expansion", () => {
      // Semantic expansion should add appropriate scopes
      const defScope = makeDefScope("semantic-pipeline");
      const userScope = freshScopeId();

      // Simulated semantic expansion output with introduced binding
      const expandedOutput = makeTestList([
        makeTestIdent("let", [defScope]),
        makeTestList([
          makeTestList([
            makeTestIdent("stage-result", [defScope]), // introduced
            makeTestAtom(null, []),
          ], [defScope]),
        ], [defScope]),
        makeTestIdent("user-var", [userScope]), // from user
      ], [defScope]);

      // The introduced binding should be distinguishable from user bindings
      const bindings = expandedOutput.items[1];
      if (isList(bindings) && isList(bindings.items[0])) {
        const introducedVar = bindings.items[0].items[0];
        const userVar = expandedOutput.items[2];

        expect(isIdent(introducedVar)).toBe(true);
        expect(isIdent(userVar)).toBe(true);

        if (isIdent(introducedVar) && isIdent(userVar)) {
          // Different scope sets
          expect(boundIdentifierEqual(introducedVar, userVar)).toBe(false);
        }
      }
    });

    it("should attach rewrite obligations", () => {
      const state = createSemanticState("pragmatic");

      // Simulate creating obligations for a rewrite proposal
      const obligations: ObligationRef[] = [
        createObligation("test", "Regression tests for rewrite"),
        createObligation("metamorphic", "Semantic equivalence check"),
      ];

      state.obligations.push(...obligations);

      expect(state.obligations.length).toBe(2);
      expect(allObligationsSatisfied(state.obligations)).toBe(false);

      // Satisfy obligations
      state.obligations = state.obligations.map(o =>
        satisfyObligation(o, createEvidence("test-pass", "Passed"))
      );

      expect(allObligationsSatisfied(state.obligations)).toBe(true);
    });
  });

  // ─────────────────────────────────────────────────────────────────
  // Additional Tests: Scope Operations
  // ─────────────────────────────────────────────────────────────────

  describe("Scope operations", () => {
    it("should compute scope subset correctly", () => {
      expect(scopeSubset(["a", "b"], ["a", "b", "c"])).toBe(true);
      expect(scopeSubset(["a", "b", "c"], ["a", "b"])).toBe(false);
      expect(scopeSubset([], ["a", "b"])).toBe(true);
    });

    it("should compute scope intersection correctly", () => {
      const result = scopeIntersection(["a", "b", "c"], ["b", "c", "d"]);
      expect(result).toContain("b");
      expect(result).toContain("c");
      expect(result).not.toContain("a");
      expect(result).not.toContain("d");
    });

    it("should compute scope union correctly", () => {
      const result = scopeUnion(["a", "b"], ["b", "c"]);
      expect(result).toContain("a");
      expect(result).toContain("b");
      expect(result).toContain("c");
    });

    it("should compute scope difference correctly", () => {
      const result = scopeDifference(["a", "b", "c"], ["b"]);
      expect(result).toContain("a");
      expect(result).toContain("c");
      expect(result).not.toContain("b");
    });

    it("should check scope equality correctly", () => {
      expect(scopeEqual(["a", "b"], ["a", "b"])).toBe(true);
      expect(scopeEqual(["a", "b"], ["b", "a"])).toBe(true);
      expect(scopeEqual(["a", "b"], ["a", "b", "c"])).toBe(false);
    });
  });

  // ─────────────────────────────────────────────────────────────────
  // Additional Tests: Syntax Manipulation
  // ─────────────────────────────────────────────────────────────────

  describe("Syntax manipulation", () => {
    it("should add scope to syntax tree", () => {
      const stx = makeTestList([
        makeTestIdent("foo", ["s1"]),
        makeTestIdent("bar", ["s1"]),
      ], ["s1"]);

      const result = addScopeToSyntax(stx, "s2");

      expect(result.scopes).toContain("s1");
      expect(result.scopes).toContain("s2");

      if (isList(result)) {
        expect(result.items[0].scopes).toContain("s2");
        expect(result.items[1].scopes).toContain("s2");
      }
    });

    it("should remove scope from syntax tree", () => {
      const stx = makeTestList([
        makeTestIdent("foo", ["s1", "s2"]),
        makeTestIdent("bar", ["s1", "s2"]),
      ], ["s1", "s2"]);

      const result = removeScopeFromSyntax(stx, "s2");

      expect(result.scopes).toContain("s1");
      expect(result.scopes).not.toContain("s2");

      if (isList(result)) {
        expect(result.items[0].scopes).not.toContain("s2");
        expect(result.items[1].scopes).not.toContain("s2");
      }
    });

    it("should flip scope on syntax tree", () => {
      const stx = makeTestIdent("foo", ["s1"]);

      // Add scope
      const withScope = flipScopeOnSyntax(stx, "s2");
      expect(withScope.scopes).toContain("s2");

      // Remove scope by flipping again
      const withoutScope = flipScopeOnSyntax(withScope, "s2");
      expect(withoutScope.scopes).not.toContain("s2");
    });

    it("should convert syntax to datum and back", () => {
      const stx = makeTestList([
        makeTestIdent("foo", ["s1"]),
        makeTestAtom(42, ["s1"]),
      ], ["s1"]);

      const datum = syntaxToDatum(stx);
      expect(Array.isArray(datum)).toBe(true);
      expect(typeof datum[0]).toBe("symbol");
      expect(datum[1]).toBe(42);

      // Convert back
      const backStx = datumToSyntax(stx, datum);
      expect(isList(backStx)).toBe(true);
      // Scopes should be copied from context
      expect(backStx.scopes).toContain("s1");
    });

    it("should collect pattern variables", () => {
      const pattern = makeTestList([
        makeTestIdent("_", []),
        makeTestIdent("x", []),
        makeTestIdent("y", []),
        makeTestIdent("...", []),
      ], []);

      const vars = collectPatternVariables(pattern);
      expect(vars.has("x")).toBe(true);
      expect(vars.has("y")).toBe(true);
      expect(vars.has("_")).toBe(false); // wildcard
      expect(vars.has("...")).toBe(false); // ellipsis
    });
  });

  // ─────────────────────────────────────────────────────────────────
  // Additional Tests: Obligation Management
  // ─────────────────────────────────────────────────────────────────

  describe("Obligation management", () => {
    it("should create and satisfy obligations", () => {
      const obligation = createObligation("test", "Run tests");
      expect(obligation.satisfied).toBe(false);

      const evidence = createEvidence("test-pass", "All tests passed");
      const satisfied = satisfyObligation(obligation, evidence);

      expect(satisfied.satisfied).toBe(true);
      expect(satisfied.evidence).toBe(evidence);
    });

    it("should get unsatisfied obligations", () => {
      const obligations: ObligationRef[] = [
        createObligation("test", "Test 1"),
        satisfyObligation(createObligation("test", "Test 2"), createEvidence("test-pass")),
        createObligation("metamorphic", "Metamorphic test"),
      ];

      const unsatisfied = getUnsatisfiedObligations(obligations);
      expect(unsatisfied.length).toBe(2);
      expect(unsatisfied.every(o => !o.satisfied)).toBe(true);
    });

    it("should check all obligations satisfied", () => {
      const unsatisfied: ObligationRef[] = [
        createObligation("test", "Test 1"),
        createObligation("test", "Test 2"),
      ];

      expect(allObligationsSatisfied(unsatisfied)).toBe(false);

      const satisfied = unsatisfied.map(o =>
        satisfyObligation(o, createEvidence("test-pass"))
      );

      expect(allObligationsSatisfied(satisfied)).toBe(true);
    });
  });

  // ─────────────────────────────────────────────────────────────────
  // Additional Tests: Profile Configuration
  // ─────────────────────────────────────────────────────────────────

  describe("Profile configuration", () => {
    it("should provide correct profiles", () => {
      expect(MACRO_PROFILES["explore"]).toBeDefined();
      expect(MACRO_PROFILES["pragmatic"]).toBeDefined();
      expect(MACRO_PROFILES["strict"]).toBeDefined();
      expect(MACRO_PROFILES["airgap"]).toBeDefined();
      expect(MACRO_PROFILES["macro/pure"]).toBeDefined();
      expect(MACRO_PROFILES["macro/semantic"]).toBeDefined();
    });

    it("should fall back to pragmatic for unknown profile", () => {
      const profile = getMacroProfile("unknown-profile");
      expect(profile.name).toBe("pragmatic");
    });

    it("should configure macro/pure correctly", () => {
      const profile = getMacroProfile("macro/pure");
      expect(profile.canExpandPure).toBe(true);
      expect(profile.canExpandSemantic).toBe(false);
      expect(profile.canInfer).toBe(false);
    });
  });
});
