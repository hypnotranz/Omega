/**
 * ═══════════════════════════════════════════════════════════════════════════
 * Generic Semantic Operations Tests
 * ═══════════════════════════════════════════════════════════════════════════
 *
 * Quick Reference: docs/USER-MANUAL--00--Quick-Reference.md#18-generic-semantic-operations
 * Full Chapter:    docs/USER-MANUAL--18--Generic-Semantic-Operations.md
 * Demo:            demo/by-chapter/ch18-generic-semantic.ts
 * ═══════════════════════════════════════════════════════════════════════════
 */
// test/prompt14-generic/generic.spec.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Tests for Prompt 14: Generic operations with data-directed programming

import { describe, it, expect, beforeEach } from "vitest";
import type { Val } from "../../src/core/eval/values";
import { VUnit, VTrue, VFalse } from "../../src/core/eval/values";
import {
  // Types
  type TypeTag,
  type TypeSignature,
  type MethodEntry,
  type CoercionEntry,
  type CoercionPath,
  type GenericMissPayload,
  type SynthesisCandidate,
  type SynthesisResult,
  type CommitRequest,
  type ObligationRef,
  isTagged,
  isGenericRegistry,
  isGenericMiss,
  attachTag,
  getTypeTag,
  getContents,
  extractSignature,
  makeGenericRegistry,
  makeGenericMiss,
  signatureToKey,
  keyToSignature,

  // Registry
  createRegistry,
  getRegistry,
  resetRegistryStore,
  defGeneric,
  getGenericOp,
  hasGenericOp,
  defMethod,
  lookupMethod,
  removeMethod,
  getMethodsForOp,
  listOperations,
  defCoercion,
  lookupCoercion,
  removeCoercion,
  getCoercionsFrom,
  getCoercionsTo,
  getAllTypeTags,
  getRegistryStats,
  commitMethod,
  commitCoercion,
  logGenericEvent,
  getRecentEvents,
  clearEventLog,
  countEvents,
  getRegistrySummary,

  // Coercion
  buildCoercionGraph,
  findCoercionPath,
  findAllCoercionPaths,
  findSignatureCoercion,
  findReachableSignatures,
  detectAmbiguity,
  isSubtype,
  findCommonSupertype,
  getReachableTypes,

  // Dispatch
  type DispatchConfig,
  resolveDispatch,
  applyGeneric,
  getDispatchInfo,
  listApplicableMethods,

  // Synthesis
  generateSynthesisKey,
  isSynthesisInFlight,
  registerInFlightSynthesis,
  clearInFlightSynthesis,
  registerStrategy,
  clearStrategies,
  synthesizeMethod,
  commitSynthesizedMethod,
  createObligation,
  satisfyObligation,
  createTestObligations,
  scoreCandidate,
  selectBestCandidate,
  canSynthesize,
  canCommit,
  getRequiredObligations,
  resetSynthesisState,
} from "../../src/core/generic";

// ─────────────────────────────────────────────────────────────────
// Test Helpers
// ─────────────────────────────────────────────────────────────────

/**
 * Create a mock procedure value.
 */
function mockProc(name: string): Val {
  return {
    tag: "Native",
    name,
    arity: 1,
    fn: (args: Val[]) => args[0],
  } as Val;
}

/**
 * Create a simple apply function for testing.
 */
function simpleApply(proc: Val, args: Val[]): Val {
  if (proc.tag === "Native") {
    return (proc as any).fn(args);
  }
  return VUnit;
}

/**
 * Create a tagged document value.
 */
function makeDoc(typeTag: TypeTag, content: string): Val {
  return attachTag(typeTag, { tag: "Str", s: content });
}

// ─────────────────────────────────────────────────────────────────
// Setup
// ─────────────────────────────────────────────────────────────────

beforeEach(() => {
  resetRegistryStore();
  resetSynthesisState();
  clearEventLog();
});

// ─────────────────────────────────────────────────────────────────
// Test 14.1: Basic Data-Directed Dispatch
// ─────────────────────────────────────────────────────────────────

describe("Test 14.1: Basic Data-Directed Dispatch", () => {
  it("should create tagged values correctly", () => {
    const doc = attachTag("Doc/Email", { tag: "Str", s: "Hello" });
    expect(isTagged(doc)).toBe(true);
    expect(getTypeTag(doc)).toBe("Doc/Email");
    expect(getContents(doc)).toEqual({ tag: "Str", s: "Hello" });
  });

  it("should infer primitive type tags", () => {
    expect(getTypeTag({ tag: "Num", n: 42 })).toBe("Primitive/Num");
    expect(getTypeTag({ tag: "Str", s: "test" })).toBe("Primitive/Str");
    expect(getTypeTag({ tag: "Bool", b: true })).toBe("Primitive/Bool");
  });

  it("should extract signature from arguments", () => {
    const emailDoc = makeDoc("Doc/Email", "email content");
    const ticketDoc = makeDoc("Doc/Ticket", "ticket content");

    const sig1 = extractSignature([emailDoc]);
    const sig2 = extractSignature([emailDoc, ticketDoc]);

    expect(sig1).toEqual(["Doc/Email"]);
    expect(sig2).toEqual(["Doc/Email", "Doc/Ticket"]);
  });

  it("should create registry and define generic operation", () => {
    const reg = createRegistry("test-registry");
    expect(isGenericRegistry(reg)).toBe(true);

    defGeneric(reg.id, "sanitize", 1);
    expect(hasGenericOp(reg.id, "sanitize")).toBe(true);

    const opDef = getGenericOp(reg.id, "sanitize");
    expect(opDef?.arity).toBe(1);
  });

  it("should register and lookup methods", () => {
    const reg = createRegistry();
    defGeneric(reg.id, "sanitize", 1);

    const sanitizeEmail = mockProc("sanitize-email");
    const sanitizeTicket = mockProc("sanitize-ticket");

    defMethod(reg.id, "sanitize", ["Doc/Email"], sanitizeEmail);
    defMethod(reg.id, "sanitize", ["Doc/Ticket"], sanitizeTicket);

    const emailMethod = lookupMethod(reg.id, "sanitize", ["Doc/Email"]);
    const ticketMethod = lookupMethod(reg.id, "sanitize", ["Doc/Ticket"]);

    expect(emailMethod?.proc).toEqual(sanitizeEmail);
    expect(ticketMethod?.proc).toEqual(sanitizeTicket);
  });

  it("should dispatch to correct method based on type tag", () => {
    const reg = createRegistry();
    defGeneric(reg.id, "sanitize", 1);

    // Sanitize returns the content with "[SANITIZED]" prefix
    const sanitizeEmail: Val = {
      tag: "Native",
      name: "sanitize-email",
      arity: 1,
      fn: (args) => ({ tag: "Str", s: `[EMAIL] ${(args[0] as any).s}` }),
    } as Val;

    const sanitizeTicket: Val = {
      tag: "Native",
      name: "sanitize-ticket",
      arity: 1,
      fn: (args) => ({ tag: "Str", s: `[TICKET] ${(args[0] as any).s}` }),
    } as Val;

    defMethod(reg.id, "sanitize", ["Doc/Email"], sanitizeEmail);
    defMethod(reg.id, "sanitize", ["Doc/Ticket"], sanitizeTicket);

    const emailDoc = makeDoc("Doc/Email", "email body");
    const ticketDoc = makeDoc("Doc/Ticket", "ticket desc");

    const result1 = applyGeneric(reg.id, "sanitize", [emailDoc], simpleApply);
    const result2 = applyGeneric(reg.id, "sanitize", [ticketDoc], simpleApply);

    expect(result1.tag).toBe("success");
    expect((result1 as any).value).toEqual({ tag: "Str", s: "[EMAIL] email body" });

    expect(result2.tag).toBe("success");
    expect((result2 as any).value).toEqual({ tag: "Str", s: "[TICKET] ticket desc" });
  });

  it("should record method hits in stats", () => {
    const reg = createRegistry();
    defMethod(reg.id, "sanitize", ["Doc/Email"], mockProc("sanitize-email"));

    lookupMethod(reg.id, "sanitize", ["Doc/Email"]);
    lookupMethod(reg.id, "sanitize", ["Doc/Email"]);
    lookupMethod(reg.id, "sanitize", ["Doc/Ticket"]); // Miss

    const stats = getRegistryStats(reg.id);
    expect(stats?.methodHits).toBe(2);
    expect(stats?.methodMisses).toBe(1);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 14.2: Coercion Path Resolution
// ─────────────────────────────────────────────────────────────────

describe("Test 14.2: Coercion Path Resolution", () => {
  it("should register and lookup coercions", () => {
    const reg = createRegistry();

    const extractBody: Val = {
      tag: "Native",
      name: "extract-body",
      arity: 1,
      fn: (args) => ({ tag: "Str", s: `body: ${(args[0] as any).s}` }),
    } as Val;

    defCoercion(reg.id, "Doc/Email", "Text/Plain", extractBody);

    const coercion = lookupCoercion(reg.id, "Doc/Email", "Text/Plain");
    expect(coercion).toBeDefined();
    expect(coercion?.fromTag).toBe("Doc/Email");
    expect(coercion?.toTag).toBe("Text/Plain");
  });

  it("should find direct coercion path", () => {
    const reg = createRegistry();
    defCoercion(reg.id, "Doc/Email", "Text/Plain", mockProc("extract"));

    const path = findCoercionPath(reg.id, "Doc/Email", "Text/Plain");
    expect(path).toBeDefined();
    expect(path?.steps.length).toBe(1);
    expect(path?.steps[0].fromTag).toBe("Doc/Email");
    expect(path?.steps[0].toTag).toBe("Text/Plain");
  });

  it("should find multi-step coercion path", () => {
    const reg = createRegistry();
    defCoercion(reg.id, "Doc/Email", "Text/Raw", mockProc("to-raw"));
    defCoercion(reg.id, "Text/Raw", "Text/Normalized", mockProc("normalize"));
    defCoercion(reg.id, "Text/Normalized", "Text/Plain", mockProc("to-plain"));

    const path = findCoercionPath(reg.id, "Doc/Email", "Text/Plain");
    expect(path).toBeDefined();
    expect(path?.steps.length).toBe(3);
    expect(path?.totalCost).toBe(3);
  });

  it("should dispatch via coercion when direct method missing", () => {
    const reg = createRegistry();

    // Register coercion
    const extractBody: Val = {
      tag: "Native",
      name: "extract-body",
      arity: 1,
      fn: (args) => ({ tag: "Str", s: (args[0] as any).s }),
    } as Val;
    defCoercion(reg.id, "Doc/Email", "Text/Plain", extractBody);

    // Register method for Text/Plain
    const sanitizePlain: Val = {
      tag: "Native",
      name: "sanitize-plain",
      arity: 1,
      fn: (args) => ({ tag: "Str", s: `[CLEAN] ${(args[0] as any).s}` }),
    } as Val;
    defMethod(reg.id, "sanitize", ["Text/Plain"], sanitizePlain);

    // Dispatch with Doc/Email (should coerce to Text/Plain first)
    const emailDoc = makeDoc("Doc/Email", "email content");
    const result = resolveDispatch(reg.id, "sanitize", [emailDoc]);

    expect(result.tag).toBe("coerced");
    if (result.tag === "coerced") {
      expect(result.path.steps.length).toBeGreaterThan(0);
      expect(result.method.signature).toEqual(["Text/Plain"]);
    }
  });

  it("should prefer lower-cost coercion paths", () => {
    const reg = createRegistry();

    // Two paths: one with cost 1, one with cost 3
    defCoercion(reg.id, "A", "B", mockProc("a-to-b"), { cost: 1 });
    defCoercion(reg.id, "A", "C", mockProc("a-to-c"), { cost: 1 });
    defCoercion(reg.id, "C", "D", mockProc("c-to-d"), { cost: 1 });
    defCoercion(reg.id, "D", "B", mockProc("d-to-b"), { cost: 1 });

    const path = findCoercionPath(reg.id, "A", "B");
    expect(path?.totalCost).toBe(1); // Direct path preferred
  });

  it("should return undefined for missing coercion path", () => {
    const reg = createRegistry();
    defCoercion(reg.id, "A", "B", mockProc("a-to-b"));

    const path = findCoercionPath(reg.id, "A", "C");
    expect(path).toBeUndefined();
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 14.3: Missing Method Triggers Synthesis
// ─────────────────────────────────────────────────────────────────

describe("Test 14.3: Missing Method Triggers Synthesis", () => {
  it("should return miss for unresolved dispatch", () => {
    const reg = createRegistry();
    defGeneric(reg.id, "sanitize", 1);

    const chatDoc = makeDoc("Doc/ChatTranscript", "chat content");
    const result = applyGeneric(reg.id, "sanitize", [chatDoc], simpleApply);

    expect(result.tag).toBe("miss");
    if (result.tag === "miss") {
      expect(result.miss.op).toBe("sanitize");
      expect(result.miss.signature).toEqual(["Doc/ChatTranscript"]);
    }
  });

  it("should log miss events", () => {
    const reg = createRegistry();
    const chatDoc = makeDoc("Doc/ChatTranscript", "chat");

    applyGeneric(reg.id, "sanitize", [chatDoc], simpleApply);

    expect(countEvents("miss")).toBe(1);
    const events = getRecentEvents();
    const missEvent = events.find(e => e.tag === "miss");
    expect(missEvent).toBeDefined();
  });

  it("should generate synthesis key for singleflight", () => {
    const miss: GenericMissPayload = {
      op: "sanitize",
      signature: ["Doc/ChatTranscript"],
      argsPreview: [],
      registryId: "reg-1",
    };

    const key1 = generateSynthesisKey(miss);
    const key2 = generateSynthesisKey(miss);

    expect(key1).toBe(key2);
    expect(key1.length).toBeGreaterThan(0);
  });

  it("should track in-flight synthesis", async () => {
    const key = "test-key";
    expect(isSynthesisInFlight(key)).toBe(false);

    const promise = Promise.resolve({
      tag: "success" as const,
      candidate: {
        id: "c1",
        proc: mockProc("test"),
        confidence: 0.9,
        kind: "method" as const,
      },
      testsPassed: true,
    });

    registerInFlightSynthesis(key, promise);
    expect(isSynthesisInFlight(key)).toBe(true);

    await promise;
    // After resolution, should be cleared
    await new Promise(resolve => setTimeout(resolve, 10));
    expect(isSynthesisInFlight(key)).toBe(false);
  });

  it("should synthesize method with custom strategy", async () => {
    const reg = createRegistry();

    // Register a synthesis strategy
    registerStrategy({
      name: "test-strategy",
      generateCandidates: async (miss) => [
        {
          id: "candidate-1",
          proc: mockProc("synthesized-sanitize"),
          confidence: 0.9,
          kind: "method",
          description: "Auto-synthesized sanitizer",
        },
      ],
    });

    const miss: GenericMissPayload = {
      op: "sanitize",
      signature: ["Doc/ChatTranscript"],
      argsPreview: [],
      registryId: reg.id,
    };

    const result = await synthesizeMethod(miss);
    expect(result.tag).toBe("success");
    if (result.tag === "success") {
      expect(result.candidate.id).toBe("candidate-1");
    }
  });

  it("should commit synthesized method with obligations", async () => {
    const reg = createRegistry();

    const obligation = createObligation("test", "Sanitize must work");
    const satisfiedObligation = satisfyObligation(obligation);

    const request: CommitRequest = {
      kind: "method",
      op: "sanitize",
      signature: ["Doc/ChatTranscript"],
      proc: mockProc("synthesized-sanitize"),
      obligations: [satisfiedObligation],
      registryId: reg.id,
    };

    const caps = new Set(["cap.generic.method.define"]);
    const result = await commitSynthesizedMethod(request, caps);

    expect(result.tag).toBe("committed");
    if (result.tag === "committed") {
      expect(result.hash).toBeDefined();
    }

    // Method should now be in registry
    const method = lookupMethod(reg.id, "sanitize", ["Doc/ChatTranscript"]);
    expect(method).toBeDefined();
    expect(method?.synthesized).toBe(true);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 14.4: Inferred Coercion Synthesis
// ─────────────────────────────────────────────────────────────────

describe("Test 14.4: Inferred Coercion Synthesis", () => {
  it("should identify when coercion is needed", () => {
    const reg = createRegistry();
    defMethod(reg.id, "policy-check", ["Text/Plain", "Policy/Strict"], mockProc("check"));

    const markdownDoc = makeDoc("Text/Markdown", "# Header");
    const policy = attachTag("Policy/Strict", { tag: "Sym", name: "strict" });

    const result = resolveDispatch(reg.id, "policy-check", [markdownDoc, policy]);

    // Should be a miss because no coercion from Markdown to Plain
    expect(result.tag).toBe("miss");
  });

  it("should commit coercion with obligations", async () => {
    const reg = createRegistry();

    const stripMarkdown: Val = {
      tag: "Native",
      name: "strip-markdown",
      arity: 1,
      fn: (args) => {
        const s = (args[0] as any).s;
        return { tag: "Str", s: s.replace(/[#*_]/g, "") };
      },
    } as Val;

    const request: CommitRequest = {
      kind: "coercion",
      fromTag: "Text/Markdown",
      toTag: "Text/Plain",
      proc: stripMarkdown,
      obligations: [satisfyObligation(createObligation("test"))],
      registryId: reg.id,
    };

    const caps = new Set(["cap.generic.method.define"]);
    const result = await commitSynthesizedMethod(request, caps);

    expect(result.tag).toBe("committed");

    // Coercion should now be in registry
    const coercion = lookupCoercion(reg.id, "Text/Markdown", "Text/Plain");
    expect(coercion).toBeDefined();
  });

  it("should dispatch after coercion is installed", () => {
    const reg = createRegistry();

    // Install method for Text/Plain
    defMethod(reg.id, "sanitize", ["Text/Plain"], {
      tag: "Native",
      name: "sanitize-plain",
      arity: 1,
      fn: (args) => ({ tag: "Str", s: `[CLEAN] ${(args[0] as any).s}` }),
    } as Val);

    // Install coercion
    defCoercion(reg.id, "Text/Markdown", "Text/Plain", {
      tag: "Native",
      name: "strip-md",
      arity: 1,
      fn: (args) => ({ tag: "Str", s: (args[0] as any).s.replace(/[#*]/g, "") }),
    } as Val);

    const markdownDoc = makeDoc("Text/Markdown", "# Hello");
    const result = resolveDispatch(reg.id, "sanitize", [markdownDoc]);

    expect(result.tag).toBe("coerced");
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 14.5: Coercion Ambiguity Detection
// ─────────────────────────────────────────────────────────────────

describe("Test 14.5: Coercion Ambiguity Detection", () => {
  it("should detect ambiguous coercion paths", () => {
    const reg = createRegistry();

    // Two paths with same cost
    defCoercion(reg.id, "Doc/Email", "Text/Plain", mockProc("direct"), { cost: 2 });
    defCoercion(reg.id, "Doc/Email", "Text/Markdown", mockProc("to-md"), { cost: 1 });
    defCoercion(reg.id, "Text/Markdown", "Text/Plain", mockProc("md-to-plain"), { cost: 1 });

    const { ambiguous, paths } = detectAmbiguity(reg.id, "Doc/Email", "Text/Plain");

    expect(ambiguous).toBe(true);
    expect(paths.length).toBe(2);
  });

  it("should not report ambiguity when costs differ", () => {
    const reg = createRegistry();

    defCoercion(reg.id, "Doc/Email", "Text/Plain", mockProc("direct"), { cost: 1 });
    defCoercion(reg.id, "Doc/Email", "Text/Markdown", mockProc("to-md"), { cost: 2 });
    defCoercion(reg.id, "Text/Markdown", "Text/Plain", mockProc("md-to-plain"), { cost: 2 });

    const { ambiguous } = detectAmbiguity(reg.id, "Doc/Email", "Text/Plain");

    expect(ambiguous).toBe(false);
  });

  it("should reject dispatch on ambiguity when configured", () => {
    const reg = createRegistry();

    // Create two methods that can both be reached from type A
    defMethod(reg.id, "op", ["B"], mockProc("op-b"));
    defMethod(reg.id, "op", ["C"], mockProc("op-c"));

    // Create two coercion paths with same cost
    defCoercion(reg.id, "A", "B", mockProc("a-to-b"), { cost: 1 });
    defCoercion(reg.id, "A", "C", mockProc("a-to-c"), { cost: 1 });

    const doc = attachTag("A", { tag: "Str", s: "data" });
    const result = resolveDispatch(reg.id, "op", [doc], { rejectAmbiguous: true });

    // Both methods are reachable with same cost - ambiguous
    expect(result.tag).toBe("ambiguous");
  });

  it("should log ambiguity events", () => {
    const reg = createRegistry();

    defCoercion(reg.id, "A", "B", mockProc("1"), { cost: 1 });
    defCoercion(reg.id, "A", "C", mockProc("2"), { cost: 0.5 });
    defCoercion(reg.id, "C", "B", mockProc("3"), { cost: 0.5 });

    detectAmbiguity(reg.id, "A", "B");

    expect(countEvents("ambiguity")).toBe(1);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 14.6: Profile Enforcement
// ─────────────────────────────────────────────────────────────────

describe("Test 14.6: Profile Enforcement", () => {
  it("should allow synthesis in explore profile", () => {
    expect(canSynthesize("explore", "method")).toBe(true);
    expect(canSynthesize("explore", "coercion")).toBe(true);
  });

  it("should deny commit in explore profile", () => {
    expect(canCommit("explore", "method")).toBe(false);
    expect(canCommit("explore", "coercion")).toBe(false);
  });

  it("should allow commit in pragmatic profile", () => {
    expect(canCommit("pragmatic", "method")).toBe(true);
    expect(canSynthesize("pragmatic", "method")).toBe(true);
  });

  it("should deny synthesis in airgap profile", () => {
    expect(canSynthesize("airgap", "method")).toBe(false);
    expect(canCommit("airgap", "method")).toBe(false);
  });

  it("should return required obligations per profile", () => {
    expect(getRequiredObligations("explore", "method")).toEqual([]);
    expect(getRequiredObligations("pragmatic", "method")).toEqual(["test"]);
    expect(getRequiredObligations("strict", "method")).toEqual(["test", "metamorphic", "invariant"]);
  });

  it("should deny commit when missing capabilities", async () => {
    const reg = createRegistry();

    const request: CommitRequest = {
      kind: "method",
      op: "sanitize",
      signature: ["Doc/Test"],
      proc: mockProc("test"),
      obligations: [],
      registryId: reg.id,
    };

    const emptyCaps = new Set<string>();
    const result = await commitSynthesizedMethod(request, emptyCaps);

    expect(result.tag).toBe("denied");
    if (result.tag === "denied") {
      expect(result.missingCaps).toContain("cap.generic.method.define");
    }
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 14.7: Singleflight for Concurrent Misses
// ─────────────────────────────────────────────────────────────────

describe("Test 14.7: Singleflight for Concurrent Misses", () => {
  it("should deduplicate concurrent synthesis requests", async () => {
    let callCount = 0;

    // Strategy that counts calls
    registerStrategy({
      name: "counting-strategy",
      generateCandidates: async () => {
        callCount++;
        await new Promise(resolve => setTimeout(resolve, 10));
        return [
          {
            id: "c1",
            proc: mockProc("synth"),
            confidence: 0.9,
            kind: "method" as const,
          },
        ];
      },
    });

    const miss: GenericMissPayload = {
      op: "sanitize",
      signature: ["Doc/Test"],
      argsPreview: [],
      registryId: "reg-1",
    };

    // Start two concurrent synthesis requests
    const promise1 = synthesizeMethod(miss, { requireTests: false });
    const promise2 = synthesizeMethod(miss, { requireTests: false });

    const [result1, result2] = await Promise.all([promise1, promise2]);

    // Both should succeed with same candidate
    expect(result1.tag).toBe("success");
    expect(result2.tag).toBe("success");

    // But strategy should only be called once
    expect(callCount).toBe(1);
  });

  it("should allow separate synthesis for different signatures", async () => {
    let callCount = 0;

    registerStrategy({
      name: "counting-strategy",
      generateCandidates: async () => {
        callCount++;
        return [{ id: `c${callCount}`, proc: mockProc("synth"), confidence: 0.9, kind: "method" as const }];
      },
    });

    const miss1: GenericMissPayload = {
      op: "sanitize",
      signature: ["Doc/Type1"],
      argsPreview: [],
      registryId: "reg-1",
    };

    const miss2: GenericMissPayload = {
      op: "sanitize",
      signature: ["Doc/Type2"],
      argsPreview: [],
      registryId: "reg-1",
    };

    await Promise.all([
      synthesizeMethod(miss1, { requireTests: false }),
      synthesizeMethod(miss2, { requireTests: false }),
    ]);

    // Should be called twice for different signatures
    expect(callCount).toBe(2);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 14.8: Cost-Aware Candidate Selection
// ─────────────────────────────────────────────────────────────────

describe("Test 14.8: Cost-Aware Candidate Selection", () => {
  it("should score candidates based on confidence", () => {
    const highConfidence: SynthesisCandidate = {
      id: "c1",
      proc: mockProc("high"),
      confidence: 0.9,
      kind: "method",
    };

    const lowConfidence: SynthesisCandidate = {
      id: "c2",
      proc: mockProc("low"),
      confidence: 0.5,
      kind: "method",
    };

    const score1 = scoreCandidate(highConfidence);
    const score2 = scoreCandidate(lowConfidence);

    expect(score1).toBeGreaterThan(score2);
  });

  it("should select best candidate from list", () => {
    const candidates: SynthesisCandidate[] = [
      { id: "c1", proc: mockProc("low"), confidence: 0.5, kind: "method" },
      { id: "c2", proc: mockProc("high"), confidence: 0.9, kind: "method" },
      { id: "c3", proc: mockProc("mid"), confidence: 0.7, kind: "method" },
    ];

    const best = selectBestCandidate(candidates);
    expect(best?.id).toBe("c2");
  });

  it("should respect hard constraints in selection", () => {
    const candidates: SynthesisCandidate[] = [
      { id: "c1", proc: mockProc("high"), confidence: 0.9, kind: "method" },
      { id: "c2", proc: mockProc("low"), confidence: 0.5, kind: "coercion" },
    ];

    // Constraint: only methods
    const best = selectBestCandidate(candidates, {
      hardConstraints: [(c) => c.kind === "method"],
    });

    expect(best?.id).toBe("c1");
  });

  it("should return undefined when all candidates fail constraints", () => {
    const candidates: SynthesisCandidate[] = [
      { id: "c1", proc: mockProc("a"), confidence: 0.9, kind: "coercion" },
      { id: "c2", proc: mockProc("b"), confidence: 0.8, kind: "coercion" },
    ];

    const best = selectBestCandidate(candidates, {
      hardConstraints: [(c) => c.kind === "method"],
    });

    expect(best).toBeUndefined();
  });

  it("should create test obligations", () => {
    const testCases = [
      { input: { tag: "Str", s: "test" } as Val, expected: { tag: "Str", s: "TEST" } as Val },
      { input: { tag: "Str", s: "hello" } as Val, expected: { tag: "Str", s: "HELLO" } as Val },
    ];

    const obligations = createTestObligations("sanitize", ["Text/Plain"], testCases);

    expect(obligations.length).toBe(2);
    expect(obligations[0].kind).toBe("test");
    expect(obligations[1].kind).toBe("test");
  });
});

// ─────────────────────────────────────────────────────────────────
// Additional Tests: Registry Operations
// ─────────────────────────────────────────────────────────────────

describe("Registry Operations", () => {
  it("should list all operations", () => {
    const reg = createRegistry();
    defMethod(reg.id, "op1", ["A"], mockProc("op1"));
    defMethod(reg.id, "op1", ["B"], mockProc("op1b"));
    defMethod(reg.id, "op2", ["A"], mockProc("op2"));

    const ops = listOperations(reg.id);

    expect(ops.length).toBe(2);
    expect(ops.find(o => o.op === "op1")?.methodCount).toBe(2);
    expect(ops.find(o => o.op === "op2")?.methodCount).toBe(1);
  });

  it("should remove methods", () => {
    const reg = createRegistry();
    defMethod(reg.id, "op", ["A"], mockProc("op"));

    expect(lookupMethod(reg.id, "op", ["A"])).toBeDefined();

    removeMethod(reg.id, "op", ["A"]);

    expect(lookupMethod(reg.id, "op", ["A"])).toBeUndefined();
  });

  it("should get registry summary", () => {
    const reg = createRegistry("test-reg");
    defMethod(reg.id, "op1", ["A"], mockProc("op1"));
    defCoercion(reg.id, "A", "B", mockProc("a-to-b"));

    const summary = getRegistrySummary(reg.id);

    expect(summary?.name).toBe("test-reg");
    expect(summary?.methodCount).toBe(1);
    expect(summary?.coercionCount).toBe(1);
  });

  it("should convert signature keys", () => {
    const sig: TypeSignature = ["A", "B", "C"];
    const key = signatureToKey(sig);
    const restored = keyToSignature(key);

    expect(restored).toEqual(sig);
  });
});

// ─────────────────────────────────────────────────────────────────
// Additional Tests: Coercion Graph Utilities
// ─────────────────────────────────────────────────────────────────

describe("Coercion Graph Utilities", () => {
  it("should check subtype relationship", () => {
    const reg = createRegistry();
    defCoercion(reg.id, "A", "B", mockProc("a-to-b"));
    defCoercion(reg.id, "B", "C", mockProc("b-to-c"));

    expect(isSubtype(reg.id, "A", "C")).toBe(true);
    expect(isSubtype(reg.id, "C", "A")).toBe(false);
    expect(isSubtype(reg.id, "A", "A")).toBe(true);
  });

  it("should find common supertype", () => {
    const reg = createRegistry();
    defCoercion(reg.id, "A", "C", mockProc("a-to-c"));
    defCoercion(reg.id, "B", "C", mockProc("b-to-c"));

    const common = findCommonSupertype(reg.id, "A", "B");
    expect(common).toBe("C");
  });

  it("should get reachable types", () => {
    const reg = createRegistry();
    defCoercion(reg.id, "A", "B", mockProc("1"));
    defCoercion(reg.id, "B", "C", mockProc("2"));
    defCoercion(reg.id, "C", "D", mockProc("3"));

    const reachable = getReachableTypes(reg.id, "A");

    expect(reachable.has("A")).toBe(true);
    expect(reachable.has("B")).toBe(true);
    expect(reachable.has("C")).toBe(true);
    expect(reachable.has("D")).toBe(true);
  });

  it("should get all coercions to a type", () => {
    const reg = createRegistry();
    defCoercion(reg.id, "A", "C", mockProc("a-to-c"));
    defCoercion(reg.id, "B", "C", mockProc("b-to-c"));
    defCoercion(reg.id, "D", "E", mockProc("d-to-e"));

    const toC = getCoercionsTo(reg.id, "C");

    expect(toC.length).toBe(2);
    expect(toC.map(c => c.fromTag).sort()).toEqual(["A", "B"]);
  });
});

// ─────────────────────────────────────────────────────────────────
// Additional Tests: Dispatch Introspection
// ─────────────────────────────────────────────────────────────────

describe("Dispatch Introspection", () => {
  it("should get dispatch info for exact match", () => {
    const reg = createRegistry();
    defMethod(reg.id, "op", ["A"], mockProc("op-a"));

    const info = getDispatchInfo(reg.id, "op", ["A"]);

    expect(info.exactMatch).toBe(true);
    expect(info.candidateCount).toBe(1);
    expect(info.ambiguous).toBe(false);
  });

  it("should get dispatch info for coercion match", () => {
    const reg = createRegistry();
    defMethod(reg.id, "op", ["B"], mockProc("op-b"));
    defCoercion(reg.id, "A", "B", mockProc("a-to-b"));

    const info = getDispatchInfo(reg.id, "op", ["A"]);

    expect(info.exactMatch).toBe(false);
    expect(info.coercionAvailable).toBe(true);
    expect(info.candidateCount).toBe(1);
  });

  it("should list applicable methods", () => {
    const reg = createRegistry();
    defMethod(reg.id, "op", ["A"], mockProc("op-a"));
    defMethod(reg.id, "op", ["B"], mockProc("op-b"));
    defCoercion(reg.id, "A", "B", mockProc("a-to-b"));

    const methods = listApplicableMethods(reg.id, "op", ["A"]);

    // Should include exact match and coercion match
    expect(methods.length).toBe(2);
    expect(methods.some(m => m.method.signature[0] === "A")).toBe(true);
    expect(methods.some(m => m.method.signature[0] === "B" && m.path)).toBe(true);
  });
});
