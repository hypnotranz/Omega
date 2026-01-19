// demo/harness/oracle-adapter.ts
// Scripted Oracle Adapter for deterministic demos

import { createHash } from "crypto";
import type {
  ScriptedOracleAdapter,
  OracleScriptEntry,
  OracleRequestType,
  OracleInteraction,
  OracleTranscript,
  DemoContext,
} from "./types";

/**
 * Create a scripted oracle adapter for deterministic demo runs.
 */
export function createScriptedOracleAdapter(
  demoId: string,
  seed: number,
  profile: string
): ScriptedOracleAdapter {
  const scripts: OracleScriptEntry[] = [];
  const interactions: OracleInteraction[] = [];
  const counts: Record<OracleRequestType, number> = {
    ReqEval: 0,
    ReqApply: 0,
    ReqObserve: 0,
    ReqTest: 0,
    ReqReturn: 0,
    InferOp: 0,
  };
  let seq = 0;
  let replayTranscript: OracleTranscript | null = null;
  let replayIndex = 0;
  let ctx: DemoContext | null = null;

  function generateId(): string {
    return `oi-${demoId}-${seed}-${seq}`;
  }

  function computeDigest(interactions: OracleInteraction[]): string {
    const content = JSON.stringify(interactions.map(i => ({
      type: i.type,
      request: i.request,
      response: i.response,
    })));
    return createHash("sha256").update(content).digest("hex").slice(0, 16);
  }

  const adapter: ScriptedOracleAdapter = {
    addScript(entry: OracleScriptEntry): void {
      scripts.push(entry);
    },

    handle(type: OracleRequestType, request: unknown): unknown {
      counts[type]++;
      seq++;

      // In replay mode, use recorded response
      if (replayTranscript && replayIndex < replayTranscript.interactions.length) {
        const recorded = replayTranscript.interactions[replayIndex];
        if (recorded.type === type) {
          replayIndex++;
          interactions.push({
            id: generateId(),
            seq,
            type,
            request,
            response: recorded.response,
            timestamp: Date.now(),
          });
          return recorded.response;
        }
        // Mismatch - fall through to scripted handling
        console.warn(`Replay mismatch at ${replayIndex}: expected ${recorded.type}, got ${type}`);
      }

      // Find matching script
      for (const script of scripts) {
        if (script.match(request, type)) {
          const response = script.respond(request, ctx!);

          if (script.sideEffect) {
            script.sideEffect(request, ctx!);
          }

          interactions.push({
            id: generateId(),
            seq,
            type,
            request,
            response,
            timestamp: Date.now(),
          });

          return response;
        }
      }

      // Default response
      const defaultResponse = createDefaultResponse(type, request);
      interactions.push({
        id: generateId(),
        seq,
        type,
        request,
        response: defaultResponse,
        timestamp: Date.now(),
      });

      return defaultResponse;
    },

    getCount(type: OracleRequestType): number {
      return counts[type];
    },

    getCounts(): Record<OracleRequestType, number> {
      return { ...counts };
    },

    getTranscript(): OracleTranscript {
      return {
        id: `transcript-${demoId}-${seed}`,
        demoId,
        seed,
        profile,
        interactions: [...interactions],
        digest: computeDigest(interactions),
      };
    },

    reset(): void {
      interactions.length = 0;
      seq = 0;
      replayIndex = 0;
      for (const key of Object.keys(counts) as OracleRequestType[]) {
        counts[key] = 0;
      }
    },

    loadTranscript(transcript: OracleTranscript): void {
      replayTranscript = transcript;
      replayIndex = 0;
    },

    isReplaying(): boolean {
      return replayTranscript !== null;
    },
  };

  // Allow setting context after creation
  (adapter as any).setContext = (c: DemoContext) => { ctx = c; };

  return adapter;
}

/**
 * Create default response for oracle request type.
 */
function createDefaultResponse(type: OracleRequestType, request: unknown): unknown {
  switch (type) {
    case "ReqEval":
      // Return a simple evaluation result
      return { value: null, evidence: "default-eval" };

    case "ReqApply":
      // Return application result
      return { value: null, evidence: "default-apply" };

    case "ReqObserve":
      // Return observation
      return { observed: {}, evidence: "default-observe" };

    case "ReqTest":
      // Return test result
      return { passed: true, evidence: "default-test" };

    case "ReqReturn":
      // Acknowledge return
      return { acknowledged: true };

    case "InferOp":
      // Return inference result based on operation
      const op = (request as any)?.op ?? "unknown";
      return createDefaultInferResponse(op, request);

    default:
      return null;
  }
}

/**
 * Create default inference response based on operation.
 */
function createDefaultInferResponse(op: string, request: unknown): unknown {
  const args = (request as any)?.args ?? [];

  switch (op) {
    case "classify":
      return {
        value: { category: "general", confidence: 0.85 },
        evidence: "default-classify",
      };

    case "sanitize":
      // Return sanitized version of input
      const text = typeof args[0] === "string" ? args[0] : String(args[0] ?? "");
      return {
        value: text.replace(/\b(password|secret|ssn|credit.?card)\b/gi, "[REDACTED]"),
        evidence: "default-sanitize",
      };

    case "rewrite":
      return {
        value: args[0] ?? "",
        evidence: "default-rewrite",
      };

    case "extract":
      return {
        value: [],
        evidence: "default-extract",
      };

    case "summarize":
      const input = typeof args[0] === "string" ? args[0] : "";
      return {
        value: input.slice(0, 100) + (input.length > 100 ? "..." : ""),
        evidence: "default-summarize",
      };

    case "validate":
      return {
        value: { valid: true, issues: [] },
        evidence: "default-validate",
      };

    case "generate":
      return {
        value: "Generated content placeholder",
        evidence: "default-generate",
      };

    case "propose-method":
      return {
        value: {
          methodName: "proposed-method",
          implementation: "(lambda (x) x)",
          confidence: 0.7,
        },
        evidence: "default-propose",
      };

    case "propose-repair":
      return {
        value: {
          repair: "identity",
          confidence: 0.6,
        },
        evidence: "default-repair",
      };

    default:
      return {
        value: null,
        evidence: `default-${op}`,
      };
  }
}

// ─────────────────────────────────────────────────────────────────
// Common Script Patterns
// ─────────────────────────────────────────────────────────────────

/**
 * Create a script entry for classification.
 */
export function classifyScript(
  patterns: Map<string, string>,
  defaultCategory: string = "general"
): OracleScriptEntry {
  return {
    match: (req, type) =>
      type === "InferOp" && (req as any)?.op === "classify",
    respond: (req) => {
      const text = String((req as any)?.args?.[0] ?? "");
      for (const [pattern, category] of patterns) {
        if (text.toLowerCase().includes(pattern.toLowerCase())) {
          return {
            value: { category, confidence: 0.9 },
            evidence: `matched-pattern:${pattern}`,
          };
        }
      }
      return {
        value: { category: defaultCategory, confidence: 0.7 },
        evidence: "no-pattern-match",
      };
    },
  };
}

/**
 * Create a script entry for sanitization.
 */
export function sanitizeScript(
  sensitivePatterns: RegExp[],
  replacement: string = "[REDACTED]"
): OracleScriptEntry {
  return {
    match: (req, type) =>
      type === "InferOp" && (req as any)?.op === "sanitize",
    respond: (req) => {
      let text = String((req as any)?.args?.[0] ?? "");
      let redactionCount = 0;

      for (const pattern of sensitivePatterns) {
        const matches = text.match(pattern);
        if (matches) {
          redactionCount += matches.length;
          text = text.replace(pattern, replacement);
        }
      }

      return {
        value: text,
        evidence: `redactions:${redactionCount}`,
      };
    },
  };
}

/**
 * Create a script entry for validation.
 */
export function validateScript(
  validators: Array<{
    name: string;
    check: (text: string) => { valid: boolean; issue?: string };
  }>
): OracleScriptEntry {
  return {
    match: (req, type) =>
      type === "InferOp" && (req as any)?.op === "validate",
    respond: (req) => {
      const text = String((req as any)?.args?.[0] ?? "");
      const issues: string[] = [];

      for (const validator of validators) {
        const result = validator.check(text);
        if (!result.valid && result.issue) {
          issues.push(`${validator.name}: ${result.issue}`);
        }
      }

      return {
        value: { valid: issues.length === 0, issues },
        evidence: `validators:${validators.length}`,
      };
    },
  };
}

/**
 * Create a script entry for ReqEval.
 */
export function reqEvalScript(
  evaluator: (expr: unknown, envSnapshot: unknown) => unknown
): OracleScriptEntry {
  return {
    match: (req, type) => type === "ReqEval",
    respond: (req) => {
      const { expr, envSnapshot } = req as any;
      try {
        const value = evaluator(expr, envSnapshot);
        return { value, evidence: "eval-success" };
      } catch (err) {
        return { value: null, error: String(err), evidence: "eval-error" };
      }
    },
  };
}

/**
 * Create a script entry for ReqApply.
 */
export function reqApplyScript(
  applicator: (proc: unknown, args: unknown[]) => unknown
): OracleScriptEntry {
  return {
    match: (req, type) => type === "ReqApply",
    respond: (req) => {
      const { proc, args } = req as any;
      try {
        const value = applicator(proc, args ?? []);
        return { value, evidence: "apply-success" };
      } catch (err) {
        return { value: null, error: String(err), evidence: "apply-error" };
      }
    },
  };
}

/**
 * Create a script entry for ReqObserve.
 */
export function reqObserveScript(
  observer: (path: string, envSnapshot: unknown) => unknown
): OracleScriptEntry {
  return {
    match: (req, type) => type === "ReqObserve",
    respond: (req) => {
      const { path, envSnapshot } = req as any;
      try {
        const observed = observer(path, envSnapshot);
        return { observed, evidence: "observe-success" };
      } catch (err) {
        return { observed: null, error: String(err), evidence: "observe-error" };
      }
    },
  };
}

/**
 * Create a script entry for ReqTest.
 */
export function reqTestScript(
  tester: (testSpec: unknown, value: unknown) => { passed: boolean; detail?: string }
): OracleScriptEntry {
  return {
    match: (req, type) => type === "ReqTest",
    respond: (req) => {
      const { testSpec, value } = req as any;
      const result = tester(testSpec, value);
      return { ...result, evidence: "test-executed" };
    },
  };
}
