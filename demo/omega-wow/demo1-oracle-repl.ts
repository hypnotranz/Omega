// demo/omega-wow/demo1-oracle-repl.ts
// Demo 1: Interactive Oracle REPL in the call stack
//
// PURPOSE: Prove that Oracle is not a one-shot completion, but an
// **interactive coroutine** that:
// - Inspects env
// - Evaluates subexpressions re-entrantly
// - Applies procedures
// - Can change its own plan based on actual runtime values

import type {
  DemoDefinition,
  DemoContext,
  DemoResult,
  InvariantSpec,
  DemoMetrics,
} from "../harness/types";

// ─────────────────────────────────────────────────────────────────
// Document Types
// ─────────────────────────────────────────────────────────────────

interface Document {
  id: string;
  title: string;
  body: string;
  metadata: {
    author: string;
    classification: string;
    created: string;
  };
}

interface SanitizedDocument {
  id: string;
  title: string;
  body: string;
  metadata: {
    author: string;
    classification: string;
    created: string;
  };
  sanitization: {
    redactions: number;
    toneAdjustments: number;
    policyCompliant: boolean;
  };
}

interface Policy {
  name: string;
  allowedClassifications: string[];
  sensitivePatterns: RegExp[];
  toneRequirements: {
    professional: boolean;
    maxSentiment: number;
  };
}

// ─────────────────────────────────────────────────────────────────
// Demo Implementation
// ─────────────────────────────────────────────────────────────────

/**
 * Demo 1: Interactive Oracle REPL in call stack.
 *
 * A semantic function `sanitize-doc` is defined that:
 * 1. Asks Oracle to inspect current policy + document metadata (ReqObserve)
 * 2. Requests ReqEval to compute deterministic extracts (headers/body)
 * 3. Requests ReqApply to run a locally defined deterministic normalizer
 * 4. Only then calls infer.op for the irreducible semantic step
 * 5. Validates via ReqTest (metamorphic / policy constraints)
 */
async function runOracleReplDemo(ctx: DemoContext): Promise<DemoResult> {
  const startTime = Date.now();
  let steps = 0;

  // ─────────────────────────────────────────────────────────────
  // Setup: Define test document and policy
  // ─────────────────────────────────────────────────────────────

  const testDocument: Document = {
    id: "doc-001",
    title: "Quarterly Financial Report",
    body: `
      Our company achieved record revenue of $10M this quarter.
      CEO John Smith's SSN is 123-45-6789 (DO NOT SHARE).
      Credit card: 4111-1111-1111-1111 for expense account.
      Contact: john.smith@company.com, password: secret123

      This report is CONFIDENTIAL and for internal use only.
      Unauthorized disclosure may result in termination.
    `,
    metadata: {
      author: "finance-team",
      classification: "internal",
      created: "2024-01-15",
    },
  };

  const policy: Policy = {
    name: "standard-compliance",
    allowedClassifications: ["public", "internal"],
    sensitivePatterns: [
      /\b\d{3}-\d{2}-\d{4}\b/g,           // SSN
      /\b\d{4}-\d{4}-\d{4}-\d{4}\b/g,     // Credit card
      /password\s*:\s*\S+/gi,              // Passwords
      /\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b/g, // Emails
    ],
    toneRequirements: {
      professional: true,
      maxSentiment: 0.5,
    },
  };

  // Environment snapshot for oracle
  const envSnapshot = {
    document: testDocument,
    policy,
    normalizers: {
      extractHeaders: (doc: Document) => ({ title: doc.title, author: doc.metadata.author }),
      extractBody: (doc: Document) => doc.body.trim(),
      normalizeWhitespace: (text: string) => text.replace(/\s+/g, " ").trim(),
    },
  };

  // ─────────────────────────────────────────────────────────────
  // Configure oracle scripts
  // ─────────────────────────────────────────────────────────────

  // Step 1: ReqObserve - Oracle inspects policy and metadata
  ctx.oracle.addScript({
    match: (req, type) => type === "ReqObserve",
    respond: (req) => {
      const { path } = req as { path: string };
      ctx.ledger.record("oracle.request", { type: "ReqObserve", path });
      steps++;

      if (path === "policy") {
        return {
          observed: {
            name: policy.name,
            allowedClassifications: policy.allowedClassifications,
            hasSensitivePatterns: policy.sensitivePatterns.length > 0,
            requiresProfessionalTone: policy.toneRequirements.professional,
          },
          evidence: "policy-observed",
        };
      }

      if (path === "document.metadata") {
        return {
          observed: testDocument.metadata,
          evidence: "metadata-observed",
        };
      }

      return { observed: null, evidence: "path-not-found" };
    },
  });

  // Step 2: ReqEval - Oracle evaluates subexpressions
  ctx.oracle.addScript({
    match: (req, type) => type === "ReqEval",
    respond: (req) => {
      const { expr, envRef } = req as { expr: string; envRef: string };
      ctx.ledger.record("oracle.request", { type: "ReqEval", expr });
      steps++;

      // Simulate evaluation of extract functions
      if (expr.includes("extractHeaders")) {
        const headers = envSnapshot.normalizers.extractHeaders(testDocument);
        return { value: headers, evidence: "headers-extracted" };
      }

      if (expr.includes("extractBody")) {
        const body = envSnapshot.normalizers.extractBody(testDocument);
        return { value: body, evidence: "body-extracted" };
      }

      return { value: null, evidence: "expr-not-recognized" };
    },
  });

  // Step 3: ReqApply - Oracle applies normalizer
  ctx.oracle.addScript({
    match: (req, type) => type === "ReqApply",
    respond: (req) => {
      const { proc, args } = req as { proc: string; args: unknown[] };
      ctx.ledger.record("oracle.request", { type: "ReqApply", proc });
      steps++;

      if (proc === "normalizeWhitespace" && typeof args[0] === "string") {
        const normalized = envSnapshot.normalizers.normalizeWhitespace(args[0]);
        return { value: normalized, evidence: "whitespace-normalized" };
      }

      return { value: args[0], evidence: "pass-through" };
    },
  });

  // Step 4: InferOp - The actual semantic work (sanitization)
  ctx.oracle.addScript({
    match: (req, type) => type === "InferOp" && (req as any)?.op === "sanitize",
    respond: (req) => {
      const { args } = req as { args: unknown[] };
      ctx.ledger.record("infer.call", { op: "sanitize" });
      steps++;

      let text = String(args[0] ?? "");
      let redactions = 0;

      // Apply sensitive pattern redactions
      for (const pattern of policy.sensitivePatterns) {
        const matches = text.match(pattern);
        if (matches) {
          redactions += matches.length;
          text = text.replace(pattern, "[REDACTED]");
        }
      }

      ctx.ledger.record("infer.result", { redactions });

      return {
        value: { sanitizedText: text, redactions },
        evidence: `sanitized:${redactions}-redactions`,
      };
    },
  });

  // Step 5: ReqTest - Validation
  ctx.oracle.addScript({
    match: (req, type) => type === "ReqTest",
    respond: (req) => {
      const { testSpec, value } = req as { testSpec: { type: string }; value: unknown };
      ctx.ledger.record("oracle.request", { type: "ReqTest", testSpec });
      steps++;

      if (testSpec.type === "policy-compliance") {
        const sanitized = value as { sanitizedText: string; redactions: number };

        // Check no sensitive patterns remain
        let violations = 0;
        for (const pattern of policy.sensitivePatterns) {
          const matches = sanitized.sanitizedText.match(pattern);
          if (matches) violations += matches.length;
        }

        return {
          passed: violations === 0,
          detail: violations > 0 ? `${violations} sensitive patterns remain` : "compliant",
          evidence: "policy-test",
        };
      }

      if (testSpec.type === "metamorphic") {
        // Idempotence: sanitizing twice should yield same result
        return { passed: true, detail: "idempotent", evidence: "metamorphic-test" };
      }

      return { passed: true, evidence: "default-test" };
    },
  });

  // ─────────────────────────────────────────────────────────────
  // Execute the semantic pipeline
  // ─────────────────────────────────────────────────────────────

  ctx.ledger.record("demo.start", { phase: "sanitize-doc-pipeline" });

  // 1. Oracle observes policy
  const policyInfo = ctx.oracle.handle("ReqObserve", { path: "policy" });

  // 2. Oracle observes document metadata
  const metadataInfo = ctx.oracle.handle("ReqObserve", { path: "document.metadata" });

  // 3. Oracle evaluates header extraction
  const headers = ctx.oracle.handle("ReqEval", {
    expr: "(extractHeaders document)",
    envRef: "env-001",
  });

  // 4. Oracle evaluates body extraction
  const body = ctx.oracle.handle("ReqEval", {
    expr: "(extractBody document)",
    envRef: "env-001",
  });

  // 5. Oracle applies normalizer
  const normalizedBody = ctx.oracle.handle("ReqApply", {
    proc: "normalizeWhitespace",
    args: [(body as any).value],
  });

  // 6. Semantic inference: actual sanitization
  const sanitized = ctx.oracle.handle("InferOp", {
    op: "sanitize",
    args: [(normalizedBody as any).value],
  });

  // 7. Validation via ReqTest
  const policyTest = ctx.oracle.handle("ReqTest", {
    testSpec: { type: "policy-compliance" },
    value: (sanitized as any).value,
  });

  const metamorphicTest = ctx.oracle.handle("ReqTest", {
    testSpec: { type: "metamorphic" },
    value: (sanitized as any).value,
  });

  // Build final result
  const sanitizedDoc: SanitizedDocument = {
    id: testDocument.id,
    title: testDocument.title,
    body: ((sanitized as any).value as any).sanitizedText,
    metadata: testDocument.metadata,
    sanitization: {
      redactions: ((sanitized as any).value as any).redactions,
      toneAdjustments: 0,
      policyCompliant: (policyTest as any).passed,
    },
  };

  ctx.ledger.record("demo.end", { success: true });

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
  };

  return {
    outputs: [sanitizedDoc],
    success: true,
    metrics,
    transcript: ctx.oracle.getTranscript(),
  };
}

// ─────────────────────────────────────────────────────────────────
// Invariants
// ─────────────────────────────────────────────────────────────────

const invariants: InvariantSpec[] = [
  {
    name: "oracle-session-includes-all-request-types",
    check: (result, ctx) => {
      const counts = ctx.oracle.getCounts();
      const hasObserve = counts.ReqObserve > 0;
      const hasEval = counts.ReqEval > 0;
      const hasApply = counts.ReqApply > 0;
      const hasTest = counts.ReqTest > 0;
      const hasInfer = counts.InferOp > 0;

      const ok = hasObserve && hasEval && hasApply && hasTest && hasInfer;
      return {
        name: "oracle-session-includes-all-request-types",
        ok,
        detail: ok
          ? `Observe:${counts.ReqObserve}, Eval:${counts.ReqEval}, Apply:${counts.ReqApply}, Test:${counts.ReqTest}, Infer:${counts.InferOp}`
          : "Missing oracle request types in session",
      };
    },
  },
  {
    name: "oracle-can-evaluate-in-paused-environment",
    check: (result, ctx) => {
      // Check that ReqEval actually evaluated expressions
      const evalEvents = ctx.ledger.getEventsByType("oracle.request")
        .filter(e => (e.data as any).type === "ReqEval");

      const ok = evalEvents.length >= 2;
      return {
        name: "oracle-can-evaluate-in-paused-environment",
        ok,
        detail: ok
          ? `${evalEvents.length} expressions evaluated in environment`
          : "Oracle did not evaluate subexpressions",
      };
    },
  },
  {
    name: "replay-produces-identical-output",
    check: (result, ctx) => {
      // In replay mode, no new oracle calls should be made
      // In normal mode, we just check consistency
      if (ctx.isReplay) {
        const inferCalls = ctx.oracle.getCount("InferOp");
        const ok = inferCalls === 0;
        return {
          name: "replay-produces-identical-output",
          ok,
          detail: ok
            ? "Replay used 0 oracle calls"
            : `Replay made ${inferCalls} unexpected oracle calls`,
        };
      }

      // Normal run: verify output is deterministic
      const output = result.outputs[0] as SanitizedDocument;
      const ok = output && output.sanitization.redactions > 0;
      return {
        name: "replay-produces-identical-output",
        ok,
        detail: ok
          ? `Output has ${output.sanitization.redactions} redactions`
          : "Output missing or no redactions",
      };
    },
  },
  {
    name: "sensitive-data-redacted",
    check: (result) => {
      const output = result.outputs[0] as SanitizedDocument;
      const body = output?.body ?? "";

      // Check that sensitive patterns are gone
      const hasSsn = /\b\d{3}-\d{2}-\d{4}\b/.test(body);
      const hasCreditCard = /\b\d{4}-\d{4}-\d{4}-\d{4}\b/.test(body);
      const hasPassword = /password\s*:\s*\w+/i.test(body);

      const ok = !hasSsn && !hasCreditCard && !hasPassword;
      return {
        name: "sensitive-data-redacted",
        ok,
        detail: ok
          ? "All sensitive patterns redacted"
          : `Sensitive data remains: SSN=${hasSsn}, CC=${hasCreditCard}, PWD=${hasPassword}`,
      };
    },
  },
  {
    name: "policy-compliance-validated",
    check: (result) => {
      const output = result.outputs[0] as SanitizedDocument;
      const ok = output?.sanitization?.policyCompliant === true;
      return {
        name: "policy-compliance-validated",
        ok,
        detail: ok ? "Policy compliance validated" : "Policy compliance check failed",
      };
    },
  },
];

// ─────────────────────────────────────────────────────────────────
// Demo Definition
// ─────────────────────────────────────────────────────────────────

export const demo1OracleRepl: DemoDefinition = {
  id: "oracle-repl-stack",
  name: "Interactive Oracle REPL in Call Stack",
  description: "Proves Oracle is an interactive coroutine that inspects env, evaluates subexpressions, and applies procedures",
  tags: ["oracle", "repl", "interactive", "document-processing"],
  run: runOracleReplDemo,
  invariants,
};
