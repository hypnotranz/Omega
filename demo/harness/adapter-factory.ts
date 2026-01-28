// demo/harness/adapter-factory.ts
// Adapter factory with dependency injection for mock vs live LLM mode

import type {
  ScriptedOracleAdapter,
  OracleRequestType,
  OracleScriptEntry,
  OracleTranscript,
  OracleInteraction,
  DemoContext,
} from "./types";
import { createScriptedOracleAdapter } from "./oracle-adapter";
import { createHash } from "crypto";

// ─────────────────────────────────────────────────────────────────
// Configuration
// ─────────────────────────────────────────────────────────────────

/**
 * Adapter mode configuration.
 *
 * Set via environment variable OMEGA_ADAPTER_MODE or pass directly:
 *   - "mock"  : Use scripted responses (deterministic, no LLM calls)
 *   - "live"  : Use real LLM calls (OpenAI/Anthropic)
 *   - "hybrid": Use scripts when available, fallback to LLM
 */
export type AdapterMode = "mock" | "live" | "hybrid";

/**
 * Live LLM configuration.
 */
export type LiveLLMConfig = {
  provider: "openai" | "anthropic";
  model: string;
  apiKey?: string;
  baseUrl?: string;
  maxTokens?: number;
  temperature?: number;
};

/**
 * Factory configuration.
 */
export type AdapterFactoryConfig = {
  mode: AdapterMode;
  live?: LiveLLMConfig;
};

// ─────────────────────────────────────────────────────────────────
// Environment-based Configuration
// ─────────────────────────────────────────────────────────────────

/**
 * Get adapter mode from environment.
 *
 * Environment variables:
 *   OMEGA_ADAPTER_MODE  : "mock" | "live" | "hybrid" (default: "mock")
 *   OMEGA_LLM_PROVIDER  : "openai" | "anthropic" (default: "openai")
 *   OMEGA_LLM_MODEL     : model name (default: "gpt-4.1-nano")
 *   OPENAI_API_KEY      : API key for OpenAI
 *   ANTHROPIC_API_KEY   : API key for Anthropic
 */
export function getConfigFromEnv(): AdapterFactoryConfig {
  const mode = (process.env.OMEGA_ADAPTER_MODE ?? "mock") as AdapterMode;

  if (mode === "mock") {
    return { mode: "mock" };
  }

  const provider = (process.env.OMEGA_LLM_PROVIDER ?? "openai") as "openai" | "anthropic";
  const model = process.env.OMEGA_LLM_MODEL ?? "gpt-4.1-nano"; // cheap model
  const apiKey = provider === "openai"
    ? process.env.OPENAI_API_KEY
    : process.env.ANTHROPIC_API_KEY;

  return {
    mode,
    live: {
      provider,
      model,
      apiKey,
      maxTokens: 2000,
      temperature: 0.3,
    },
  };
}

// ─────────────────────────────────────────────────────────────────
// Live LLM Adapter Wrapper
// ─────────────────────────────────────────────────────────────────

/**
 * Wraps a real LLM adapter to implement ScriptedOracleAdapter interface.
 *
 * This bridge allows demos to use real LLM calls while maintaining
 * compatibility with the existing demo harness infrastructure.
 */
export function createLiveLLMAdapter(
  demoId: string,
  seed: number,
  profile: string,
  config: LiveLLMConfig
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

  /**
   * Make a real LLM call for an InferOp request.
   */
  async function callLLM(request: unknown): Promise<unknown> {
    const { op, args } = request as { op: string; args: unknown[] };

    // Build prompt from request
    const prompt = buildPromptForOp(op, args);

    // Call real LLM
    const result = await makeLLMCall(config, prompt);

    // Parse and return
    return parseOprResult(op, result);
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
      }

      // For InferOp, we need async - throw sync marker
      if (type === "InferOp") {
        throw new LiveCallRequired(type, request);
      }

      // Non-InferOp: check scripts, then default
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

  // Async handle for InferOp
  (adapter as any).handleAsync = async (type: OracleRequestType, request: unknown): Promise<unknown> => {
    counts[type]++;
    seq++;

    // In replay mode
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
    }

    // For InferOp, call real LLM
    if (type === "InferOp") {
      const response = await callLLM(request);
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

    // Other types: use sync handle
    return adapter.handle(type, request);
  };

  (adapter as any).setContext = (c: DemoContext) => { ctx = c; };
  (adapter as any).isLive = true;
  (adapter as any).config = config;

  return adapter;
}

// ─────────────────────────────────────────────────────────────────
// Live Call Marker
// ─────────────────────────────────────────────────────────────────

/**
 * Thrown when a sync handle() is called but requires async LLM call.
 * The caller should catch this and use handleAsync() instead.
 */
export class LiveCallRequired extends Error {
  constructor(
    public readonly type: OracleRequestType,
    public readonly request: unknown
  ) {
    super("Live LLM call required - use handleAsync()");
    this.name = "LiveCallRequired";
  }
}

// ─────────────────────────────────────────────────────────────────
// Factory
// ─────────────────────────────────────────────────────────────────

/**
 * Create an oracle adapter based on configuration.
 *
 * @example
 * ```ts
 * // Mock mode (default)
 * const adapter = createOracleAdapter("demo1", 42, "pragmatic");
 *
 * // Live mode from env
 * process.env.OMEGA_ADAPTER_MODE = "live";
 * const liveAdapter = createOracleAdapter("demo1", 42, "pragmatic");
 *
 * // Explicit live mode
 * const explicitAdapter = createOracleAdapter("demo1", 42, "pragmatic", {
 *   mode: "live",
 *   live: { provider: "openai", model: "gpt-4.1-nano" }
 * });
 * ```
 */
export function createOracleAdapter(
  demoId: string,
  seed: number,
  profile: string,
  config?: AdapterFactoryConfig
): ScriptedOracleAdapter {
  const resolvedConfig = config ?? getConfigFromEnv();

  if (resolvedConfig.mode === "mock") {
    return createScriptedOracleAdapter(demoId, seed, profile);
  }

  if (!resolvedConfig.live) {
    console.warn("[AdapterFactory] Live mode requested but no config - falling back to mock");
    return createScriptedOracleAdapter(demoId, seed, profile);
  }

  if (!resolvedConfig.live.apiKey) {
    console.warn("[AdapterFactory] No API key - falling back to mock");
    return createScriptedOracleAdapter(demoId, seed, profile);
  }

  return createLiveLLMAdapter(demoId, seed, profile, resolvedConfig.live);
}

// ─────────────────────────────────────────────────────────────────
// LLM Call Helpers
// ─────────────────────────────────────────────────────────────────

/**
 * Build prompt for an OPR operation.
 */
function buildPromptForOp(op: string, args: unknown[]): string {
  switch (op) {
    case "classify":
      return `Classify the following text into a category. Return JSON: {"category": "...", "confidence": 0.X}

Text: ${JSON.stringify(args[0])}
Categories (if specified): ${args[1] ? JSON.stringify(args[1]) : "determine appropriate category"}

Return ONLY the JSON, no other text.`;

    case "sanitize":
      return `Sanitize the following text by redacting sensitive information (PII, credentials, etc). Return only the sanitized text.

Text: ${args[0]}`;

    case "extract":
      return `Extract structured data from the text. Return JSON array of extracted items.

Text: ${args[0]}
Schema: ${args[1] ? JSON.stringify(args[1]) : "extract key entities"}

Return ONLY the JSON array, no other text.`;

    case "summarize":
      return `Summarize the following text concisely.

Text: ${args[0]}

Return only the summary, no preamble.`;

    case "validate":
      return `Validate the following against the schema. Return JSON: {"valid": true/false, "issues": ["..."]}

Data: ${JSON.stringify(args[0])}
Schema: ${args[1] ? JSON.stringify(args[1]) : "validate structure"}

Return ONLY the JSON, no other text.`;

    case "generate":
      return `Generate text based on the following prompt.

Prompt: ${args[0]}

Return only the generated text.`;

    case "propose-method":
      return `Given the method miss, propose an implementation. Return JSON:
{"methodName": "...", "implementation": "(lambda (x) ...)", "confidence": 0.X}

Method: ${args[0]}
Context: ${JSON.stringify(args[1])}

Return ONLY the JSON, no other text.`;

    case "propose-repair":
      return `Given the constraint violation, propose a repair. Return JSON:
{"repair": "(lambda (x) ...)", "confidence": 0.X}

Violation: ${JSON.stringify(args[0])}
Constraints: ${args[1] ? JSON.stringify(args[1]) : "infer constraints"}

Return ONLY the JSON, no other text.`;

    default:
      return `Execute the operation "${op}" with arguments: ${JSON.stringify(args)}

Return a JSON result appropriate for the operation.`;
  }
}

/**
 * Make actual LLM API call.
 */
async function makeLLMCall(config: LiveLLMConfig, prompt: string): Promise<string> {
  const { provider, model, apiKey, baseUrl, maxTokens, temperature } = config;

  if (provider === "openai") {
    const url = `${baseUrl ?? "https://api.openai.com/v1"}/chat/completions`;
    const response = await fetch(url, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        "Authorization": `Bearer ${apiKey}`,
      },
      body: JSON.stringify({
        model,
        messages: [
          { role: "system", content: "You are a precise data processor. Return only the requested format, no explanations." },
          { role: "user", content: prompt },
        ],
        max_tokens: maxTokens ?? 2000,
        temperature: temperature ?? 0.3,
      }),
    });

    if (!response.ok) {
      const err = await response.text();
      throw new Error(`OpenAI API error: ${err}`);
    }

    const data = await response.json();
    return data.choices?.[0]?.message?.content ?? "";
  }

  if (provider === "anthropic") {
    const url = `${baseUrl ?? "https://api.anthropic.com"}/v1/messages`;
    const response = await fetch(url, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        "x-api-key": apiKey!,
        "anthropic-version": "2023-06-01",
      },
      body: JSON.stringify({
        model,
        max_tokens: maxTokens ?? 2000,
        messages: [
          { role: "user", content: prompt },
        ],
      }),
    });

    if (!response.ok) {
      const err = await response.text();
      throw new Error(`Anthropic API error: ${err}`);
    }

    const data = await response.json();
    return data.content?.[0]?.text ?? "";
  }

  throw new Error(`Unknown provider: ${provider}`);
}

/**
 * Parse LLM result into OPR response format.
 */
function parseOprResult(op: string, raw: string): unknown {
  try {
    // Try to extract JSON from the response
    const jsonMatch = raw.match(/\{[\s\S]*\}|\[[\s\S]*\]/);
    if (jsonMatch) {
      const parsed = JSON.parse(jsonMatch[0]);
      return { value: parsed, evidence: `live-${op}` };
    }

    // For text operations, return as string
    return { value: raw.trim(), evidence: `live-${op}` };
  } catch {
    return { value: raw.trim(), evidence: `live-${op}-parse-fallback` };
  }
}

/**
 * Create default response for non-InferOp types.
 */
function createDefaultResponse(type: OracleRequestType, request: unknown): unknown {
  switch (type) {
    case "ReqEval":
      return { value: null, evidence: "default-eval" };
    case "ReqApply":
      return { value: null, evidence: "default-apply" };
    case "ReqObserve":
      return { observed: {}, evidence: "default-observe" };
    case "ReqTest":
      return { passed: true, evidence: "default-test" };
    case "ReqReturn":
      return { acknowledged: true };
    default:
      return null;
  }
}

// ─────────────────────────────────────────────────────────────────
// Async Oracle Helper
// ─────────────────────────────────────────────────────────────────

/**
 * Helper to handle oracle requests that may need async LLM calls.
 *
 * In mock mode, this is just a sync wrapper.
 * In live mode, this handles the async LLM call transparently.
 *
 * @example
 * ```ts
 * // Instead of: const result = ctx.oracle.handle("InferOp", { op: "classify", args: [text] });
 * // Use:
 * const result = await oracleHandle(ctx.oracle, "InferOp", { op: "classify", args: [text] });
 * ```
 */
export async function oracleHandle(
  oracle: ScriptedOracleAdapter,
  type: OracleRequestType,
  request: unknown
): Promise<unknown> {
  // Check if this is a live adapter with async support
  const handleAsync = (oracle as any).handleAsync;
  if (handleAsync && typeof handleAsync === "function") {
    return handleAsync(type, request);
  }

  // Mock mode: sync is fine
  return oracle.handle(type, request);
}

/**
 * Check if an adapter is in live mode.
 */
export function isLiveAdapter(oracle: ScriptedOracleAdapter): boolean {
  return (oracle as any).isLive === true;
}
