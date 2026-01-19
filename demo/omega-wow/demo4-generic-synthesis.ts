// demo/omega-wow/demo4-generic-synthesis.ts
// Demo 4: Generic operations synthesize missing semantic methods
//
// PURPOSE: Prove "semantic functions compose like FP functions" AND the
// system can evolve safely when it encounters new data types.

import type {
  DemoDefinition,
  DemoContext,
  DemoResult,
  InvariantSpec,
  DemoMetrics,
} from "../harness/types";

// ─────────────────────────────────────────────────────────────────
// Types
// ─────────────────────────────────────────────────────────────────

interface Tagged<T extends string, V> {
  tag: T;
  value: V;
}

type TextPlain = Tagged<"Doc/TextPlain", string>;
type Markdown = Tagged<"Doc/Markdown", string>;
type ChatTranscript = Tagged<"Doc/ChatTranscript", ChatMessage[]>;
type DocType = TextPlain | Markdown | ChatTranscript;

interface ChatMessage {
  role: "user" | "assistant" | "system";
  content: string;
  timestamp: string;
}

interface MethodRegistry {
  methods: Map<string, Map<string, SemanticMethod>>;
  coercions: Map<string, Map<string, CoercionMethod>>;
}

interface SemanticMethod {
  name: string;
  targetType: string;
  implementation: (value: unknown) => unknown;
  evidence: string;
}

interface CoercionMethod {
  from: string;
  to: string;
  coerce: (value: unknown) => unknown;
  evidence: string;
}

interface GenericMissEvent {
  operation: string;
  type: string;
  resolution: "method" | "coercion" | "fail";
  evidence: string;
}

// ─────────────────────────────────────────────────────────────────
// Method Registry
// ─────────────────────────────────────────────────────────────────

function createMethodRegistry(): MethodRegistry {
  return {
    methods: new Map(),
    coercions: new Map(),
  };
}

function registerMethod(
  registry: MethodRegistry,
  operation: string,
  targetType: string,
  method: SemanticMethod
): void {
  if (!registry.methods.has(operation)) {
    registry.methods.set(operation, new Map());
  }
  registry.methods.get(operation)!.set(targetType, method);
}

function registerCoercion(
  registry: MethodRegistry,
  from: string,
  to: string,
  coercion: CoercionMethod
): void {
  if (!registry.coercions.has(from)) {
    registry.coercions.set(from, new Map());
  }
  registry.coercions.get(from)!.set(to, coercion);
}

function lookupMethod(
  registry: MethodRegistry,
  operation: string,
  type: string
): SemanticMethod | undefined {
  return registry.methods.get(operation)?.get(type);
}

function lookupCoercion(
  registry: MethodRegistry,
  from: string,
  to: string
): CoercionMethod | undefined {
  return registry.coercions.get(from)?.get(to);
}

// ─────────────────────────────────────────────────────────────────
// Built-in Methods
// ─────────────────────────────────────────────────────────────────

function setupBuiltinMethods(registry: MethodRegistry): void {
  // sanitize for Doc/TextPlain
  registerMethod(registry, "sanitize", "Doc/TextPlain", {
    name: "sanitize-text-plain",
    targetType: "Doc/TextPlain",
    implementation: (value: unknown) => {
      const doc = value as TextPlain;
      let text = doc.value;

      // Remove sensitive patterns
      text = text.replace(/\b\d{3}-\d{2}-\d{4}\b/g, "[SSN]");
      text = text.replace(/\b\d{4}-\d{4}-\d{4}-\d{4}\b/g, "[CARD]");
      text = text.replace(/password\s*[:=]\s*\S+/gi, "[PASSWORD]");

      return { tag: "Doc/TextPlain", value: text } as TextPlain;
    },
    evidence: "builtin-text-sanitize",
  });

  // sanitize for Doc/Markdown
  registerMethod(registry, "sanitize", "Doc/Markdown", {
    name: "sanitize-markdown",
    targetType: "Doc/Markdown",
    implementation: (value: unknown) => {
      const doc = value as Markdown;
      let text = doc.value;

      // Remove sensitive patterns (preserve markdown)
      text = text.replace(/\b\d{3}-\d{2}-\d{4}\b/g, "`[SSN]`");
      text = text.replace(/\b\d{4}-\d{4}-\d{4}-\d{4}\b/g, "`[CARD]`");
      text = text.replace(/password\s*[:=]\s*\S+/gi, "`[PASSWORD]`");

      return { tag: "Doc/Markdown", value: text } as Markdown;
    },
    evidence: "builtin-markdown-sanitize",
  });

  // Coercion: Markdown -> TextPlain
  registerCoercion(registry, "Doc/Markdown", "Doc/TextPlain", {
    from: "Doc/Markdown",
    to: "Doc/TextPlain",
    coerce: (value: unknown) => {
      const doc = value as Markdown;
      // Strip markdown formatting
      const plain = doc.value
        .replace(/[*_`#]/g, "")
        .replace(/\[([^\]]+)\]\([^)]+\)/g, "$1");
      return { tag: "Doc/TextPlain", value: plain } as TextPlain;
    },
    evidence: "builtin-md-to-text",
  });
}

// ─────────────────────────────────────────────────────────────────
// Generic Application
// ─────────────────────────────────────────────────────────────────

interface ApplyGenericResult {
  success: boolean;
  value: unknown;
  missEvent?: GenericMissEvent;
  synthesized?: boolean;
}

async function applyGeneric(
  registry: MethodRegistry,
  operation: string,
  doc: DocType,
  ctx: DemoContext
): Promise<ApplyGenericResult> {
  const type = doc.tag;

  // 1. Try direct method lookup
  const method = lookupMethod(registry, operation, type);
  if (method) {
    const result = method.implementation(doc);
    return { success: true, value: result };
  }

  // 2. No method - trigger generic.miss
  ctx.ledger.record("generic.miss", { operation, type });

  // 3. Ask oracle for resolution
  const response = ctx.oracle.handle("InferOp", {
    op: "propose-method",
    args: [operation, type, doc],
  }) as {
    value: {
      resolution: "direct-method" | "coercion" | "fail";
      method?: {
        implementation: string;
        evidence: string;
      };
      coercion?: {
        targetType: string;
        evidence: string;
      };
    };
  };

  const proposal = response.value;

  if (proposal.resolution === "fail") {
    return {
      success: false,
      value: null,
      missEvent: {
        operation,
        type,
        resolution: "fail",
        evidence: "no-method-found",
      },
    };
  }

  // 4. Validate proposal via ReqTest
  const testResult = ctx.oracle.handle("ReqTest", {
    testSpec: {
      type: "metamorphic",
      property: "sanitize-idempotent",
    },
    value: proposal,
  }) as { passed: boolean; detail?: string };

  if (!testResult.passed && ctx.profile.requireTests) {
    ctx.ledger.record("commit.denied", {
      operation,
      type,
      reason: "test-failed",
      detail: testResult.detail,
    });
    return {
      success: false,
      value: null,
      missEvent: {
        operation,
        type,
        resolution: "fail",
        evidence: "validation-failed",
      },
    };
  }

  // 5. Install method/coercion based on proposal
  if (proposal.resolution === "direct-method" && proposal.method) {
    // Synthesize and install method for ChatTranscript
    const synthesizedMethod: SemanticMethod = {
      name: `synthesized-${operation}-${type}`,
      targetType: type,
      implementation: createSynthesizedMethod(operation, type),
      evidence: proposal.method.evidence,
    };

    if (ctx.profile.allowCommit) {
      registerMethod(registry, operation, type, synthesizedMethod);
      ctx.ledger.record("generic.install", {
        kind: "method",
        operation,
        type,
        evidence: synthesizedMethod.evidence,
      });
      ctx.ledger.record("commit.success", {
        kind: "method-install",
        operation,
        type,
      });

      // Apply the newly installed method
      const result = synthesizedMethod.implementation(doc);
      return {
        success: true,
        value: result,
        missEvent: {
          operation,
          type,
          resolution: "method",
          evidence: synthesizedMethod.evidence,
        },
        synthesized: true,
      };
    } else {
      ctx.ledger.record("commit.denied", {
        kind: "method-install",
        operation,
        type,
        reason: "profile-disallows-commit",
      });
    }
  }

  if (proposal.resolution === "coercion" && proposal.coercion) {
    const targetType = proposal.coercion.targetType;

    // Check if target type has the method
    const targetMethod = lookupMethod(registry, operation, targetType);
    if (!targetMethod) {
      return {
        success: false,
        value: null,
        missEvent: {
          operation,
          type,
          resolution: "fail",
          evidence: "coercion-target-missing-method",
        },
      };
    }

    // Create and install coercion
    const synthesizedCoercion: CoercionMethod = {
      from: type,
      to: targetType,
      coerce: createSynthesizedCoercion(type, targetType),
      evidence: proposal.coercion.evidence,
    };

    if (ctx.profile.allowCommit) {
      registerCoercion(registry, type, targetType, synthesizedCoercion);
      ctx.ledger.record("generic.install", {
        kind: "coercion",
        from: type,
        to: targetType,
        evidence: synthesizedCoercion.evidence,
      });
      ctx.ledger.record("commit.success", {
        kind: "coercion-install",
        from: type,
        to: targetType,
      });

      // Apply coercion then method
      const coerced = synthesizedCoercion.coerce(doc);
      const result = targetMethod.implementation(coerced);
      return {
        success: true,
        value: result,
        missEvent: {
          operation,
          type,
          resolution: "coercion",
          evidence: synthesizedCoercion.evidence,
        },
        synthesized: true,
      };
    } else {
      ctx.ledger.record("commit.denied", {
        kind: "coercion-install",
        from: type,
        to: targetType,
        reason: "profile-disallows-commit",
      });
    }
  }

  return {
    success: false,
    value: null,
    missEvent: {
      operation,
      type,
      resolution: "fail",
      evidence: "commit-denied",
    },
  };
}

// ─────────────────────────────────────────────────────────────────
// Synthesized Methods
// ─────────────────────────────────────────────────────────────────

function createSynthesizedMethod(operation: string, type: string): (value: unknown) => unknown {
  if (operation === "sanitize" && type === "Doc/ChatTranscript") {
    return (value: unknown) => {
      const doc = value as ChatTranscript;
      const sanitizedMessages = doc.value.map(msg => ({
        ...msg,
        content: msg.content
          .replace(/\b\d{3}-\d{2}-\d{4}\b/g, "[SSN]")
          .replace(/\b\d{4}-\d{4}-\d{4}-\d{4}\b/g, "[CARD]")
          .replace(/password\s*[:=]\s*\S+/gi, "[PASSWORD]"),
      }));
      return { tag: "Doc/ChatTranscript", value: sanitizedMessages } as ChatTranscript;
    };
  }

  // Default: identity
  return (value: unknown) => value;
}

function createSynthesizedCoercion(from: string, to: string): (value: unknown) => unknown {
  if (from === "Doc/ChatTranscript" && to === "Doc/TextPlain") {
    return (value: unknown) => {
      const doc = value as ChatTranscript;
      const text = doc.value
        .map(msg => `[${msg.role}]: ${msg.content}`)
        .join("\n\n");
      return { tag: "Doc/TextPlain", value: text } as TextPlain;
    };
  }

  // Default: extract value
  return (value: unknown) => {
    const tagged = value as Tagged<string, unknown>;
    return { tag: to, value: tagged.value };
  };
}

// ─────────────────────────────────────────────────────────────────
// Demo Implementation
// ─────────────────────────────────────────────────────────────────

async function runGenericSynthesisDemo(ctx: DemoContext): Promise<DemoResult> {
  const startTime = Date.now();
  let steps = 0;
  let missCount = 0;
  let installCount = 0;

  // ─────────────────────────────────────────────────────────────
  // Setup
  // ─────────────────────────────────────────────────────────────

  const registry = createMethodRegistry();
  setupBuiltinMethods(registry);

  // Test documents
  const textDoc: TextPlain = {
    tag: "Doc/TextPlain",
    value: "User SSN: 123-45-6789. Password: secret123",
  };

  const chatDoc: ChatTranscript = {
    tag: "Doc/ChatTranscript",
    value: [
      {
        role: "user",
        content: "My SSN is 987-65-4321, can you help?",
        timestamp: "2024-01-15T10:00:00Z",
      },
      {
        role: "assistant",
        content: "I'll help but please don't share sensitive info like SSNs.",
        timestamp: "2024-01-15T10:00:05Z",
      },
      {
        role: "user",
        content: "My credit card is 4111-1111-1111-1111",
        timestamp: "2024-01-15T10:01:00Z",
      },
    ],
  };

  // ─────────────────────────────────────────────────────────────
  // Configure oracle scripts
  // ─────────────────────────────────────────────────────────────

  ctx.oracle.addScript({
    match: (req, type) =>
      type === "InferOp" && (req as any)?.op === "propose-method",
    respond: (req) => {
      const { args } = req as { args: [string, string, unknown] };
      const [operation, docType] = args;

      if (operation === "sanitize" && docType === "Doc/ChatTranscript") {
        // Propose direct method
        return {
          value: {
            resolution: "direct-method",
            method: {
              implementation: "chat-sanitizer",
              evidence: "synthesized-chat-sanitize",
            },
          },
        };
      }

      return {
        value: {
          resolution: "fail",
        },
      };
    },
  });

  ctx.oracle.addScript({
    match: (req, type) => type === "ReqTest",
    respond: () => ({
      passed: true,
      detail: "metamorphic-test-passed",
      evidence: "test-executed",
    }),
  });

  // ─────────────────────────────────────────────────────────────
  // Execute
  // ─────────────────────────────────────────────────────────────

  ctx.ledger.record("demo.start", { phase: "generic-synthesis" });

  // First run: sanitize TextPlain (should hit builtin method)
  const result1 = await applyGeneric(registry, "sanitize", textDoc, ctx);
  steps++;

  // First run: sanitize ChatTranscript (should trigger miss → synthesis)
  const result2 = await applyGeneric(registry, "sanitize", chatDoc, ctx);
  steps++;

  if (result2.missEvent) {
    missCount++;
    if (result2.synthesized) {
      installCount++;
    }
  }

  // Second run: sanitize ChatTranscript (should hit installed method, no miss)
  const result3 = await applyGeneric(registry, "sanitize", chatDoc, ctx);
  steps++;

  // Check if second run triggered miss
  const secondRunMiss = result3.missEvent !== undefined;

  ctx.ledger.record("demo.end", {
    missCount,
    installCount,
    secondRunMiss,
  });

  // ─────────────────────────────────────────────────────────────
  // Return result
  // ─────────────────────────────────────────────────────────────

  const metrics: DemoMetrics = {
    inferCalls: ctx.oracle.getCount("InferOp"),
    oracleReqEval: ctx.oracle.getCount("ReqEval"),
    oracleReqApply: ctx.oracle.getCount("ReqApply"),
    oracleReqObserve: ctx.oracle.getCount("ReqObserve"),
    oracleReqTest: ctx.oracle.getCount("ReqTest"),
    oracleReqReturn: ctx.oracle.getCount("ReqReturn"),
    steps,
    wallMs: Date.now() - startTime,
    genericMiss: missCount,
    methodsInstalled: installCount,
  };

  return {
    outputs: [
      {
        textResult: result1,
        firstChatResult: result2,
        secondChatResult: result3,
        missCount,
        installCount,
        secondRunMiss,
      },
    ],
    success: result1.success && result2.success,
    metrics,
    transcript: ctx.oracle.getTranscript(),
  };
}

// ─────────────────────────────────────────────────────────────────
// Invariants
// ─────────────────────────────────────────────────────────────────

const invariants: InvariantSpec[] = [
  {
    name: "first-run-triggers-miss",
    check: (result) => {
      const output = result.outputs[0] as { missCount: number };
      const ok = output.missCount > 0;
      return {
        name: "first-run-triggers-miss",
        ok,
        detail: `Miss count: ${output.missCount}`,
      };
    },
  },
  {
    name: "method-installed-after-miss",
    check: (result, ctx) => {
      const installs = ctx.ledger.getEventsByType("generic.install");
      const ok = installs.length > 0;
      return {
        name: "method-installed-after-miss",
        ok,
        detail: `${installs.length} method(s) installed`,
      };
    },
  },
  {
    name: "second-run-no-miss",
    check: (result) => {
      const output = result.outputs[0] as { secondRunMiss: boolean };
      const ok = !output.secondRunMiss;
      return {
        name: "second-run-no-miss",
        ok,
        detail: output.secondRunMiss
          ? "Second run triggered unexpected miss"
          : "Second run used installed method",
      };
    },
  },
  {
    name: "strict-profile-requires-tests",
    check: (result, ctx) => {
      if (ctx.profile.name === "strict" || ctx.profile.requireTests) {
        const tests = ctx.oracle.getCount("ReqTest");
        const ok = tests > 0;
        return {
          name: "strict-profile-requires-tests",
          ok,
          detail: `${tests} test(s) executed`,
        };
      }
      return {
        name: "strict-profile-requires-tests",
        ok: true,
        detail: `Profile ${ctx.profile.name}: test requirement skipped`,
      };
    },
  },
  {
    name: "chat-transcript-sanitized",
    check: (result) => {
      const output = result.outputs[0] as {
        firstChatResult: ApplyGenericResult;
      };

      if (!output.firstChatResult.success) {
        return {
          name: "chat-transcript-sanitized",
          ok: false,
          detail: "Chat sanitization failed",
        };
      }

      const sanitized = output.firstChatResult.value as ChatTranscript;
      const hasUnsanitized = sanitized.value.some(
        msg =>
          /\b\d{3}-\d{2}-\d{4}\b/.test(msg.content) ||
          /\b\d{4}-\d{4}-\d{4}-\d{4}\b/.test(msg.content)
      );

      const ok = !hasUnsanitized;
      return {
        name: "chat-transcript-sanitized",
        ok,
        detail: ok
          ? "All sensitive data removed from chat"
          : "Sensitive data remains in chat",
      };
    },
  },
];

// ─────────────────────────────────────────────────────────────────
// Demo Definition
// ─────────────────────────────────────────────────────────────────

export const demo4GenericSynthesis: DemoDefinition = {
  id: "generic-miss-synthesis",
  name: "Generic Operations Synthesize Missing Methods",
  description: "Proves SICP generic-arithmetic architecture applied to semantic capabilities with governed auto-extension",
  tags: ["generic", "synthesis", "method-dispatch", "coercion", "governance"],
  run: runGenericSynthesisDemo,
  invariants,
};
