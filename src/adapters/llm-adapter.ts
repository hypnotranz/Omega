/**
 * LLM Adapters for OmegaLLM Oracle
 *
 * Provides concrete implementations of OracleAdapter that connect to real LLMs.
 * Reads API keys from environment variables - NEVER hardcode keys.
 *
 * Usage:
 *   import { createAnthropicOracle, createOpenAIOracle } from "./adapters/llm-adapter";
 *
 *   // Set ANTHROPIC_API_KEY or OPENAI_API_KEY in environment
 *   const oracle = createAnthropicOracle();
 *   const runtime = new RuntimeImpl(oracle, snapshots, receipts, commitAdapter);
 */

import type { CommitAdapter } from "../core/effects/runtimeImpl";
import type { LegacyOracleAdapter } from "../core/oracle/legacyAdapter";
import type { Val } from "../core/eval/values";
import type { OracleAdapter } from "../core/oracle/adapter";
import { LegacyOracleWrapper } from "../core/oracle/legacyAdapter";

// ============================================================
// Value Conversion Helpers
// ============================================================

/** Convert Omega Val to a prompt string for the LLM */
function valToPrompt(v: Val): string {
  switch (v.tag) {
    case "Str": return v.s;
    case "Num": return String(v.n);
    case "Bool": return v.b ? "true" : "false";
    case "Unit": return "";
    case "Sym": return v.name;
    case "Vector": return JSON.stringify(v.items.map(valToPrompt));
    case "Map": {
      const obj: Record<string, any> = {};
      for (const [k, val] of v.entries) {
        const key = k.tag === "Str" ? k.s : k.tag === "Sym" ? k.name : JSON.stringify(k);
        obj[key] = valToPrompt(val);
      }
      return JSON.stringify(obj);
    }
    default:
      return JSON.stringify(v);
  }
}

/** Parse LLM response text back to Val */
function responseToVal(text: string): Val {
  // Try to parse as JSON first
  try {
    const parsed = JSON.parse(text);
    return jsToVal(parsed);
  } catch {
    // Return as string
    return { tag: "Str", s: text };
  }
}

/** Convert JavaScript value to Val */
function jsToVal(x: unknown): Val {
  if (x === null || x === undefined) return { tag: "Unit" };
  if (typeof x === "number") return { tag: "Num", n: x };
  if (typeof x === "boolean") return { tag: "Bool", b: x };
  if (typeof x === "string") return { tag: "Str", s: x };
  if (Array.isArray(x)) return { tag: "Vector", items: x.map(jsToVal) };
  if (typeof x === "object") {
    const entries: Array<[Val, Val]> = Object.entries(x).map(([k, v]) => [
      { tag: "Str", s: k } as Val,
      jsToVal(v)
    ]);
    return { tag: "Map", entries };
  }
  return { tag: "Str", s: String(x) };
}

// ============================================================
// Anthropic (Claude) Adapter
// ============================================================

export interface AnthropicConfig {
  apiKey?: string;     // defaults to ANTHROPIC_API_KEY env var
  model?: string;      // defaults to claude-sonnet-4-20250514
  maxTokens?: number;  // defaults to 1024
}

function createAnthropicLegacy(config: AnthropicConfig = {}): LegacyOracleAdapter {
  const apiKey = config.apiKey || process.env.ANTHROPIC_API_KEY;
  if (!apiKey) {
    throw new Error("ANTHROPIC_API_KEY environment variable not set");
  }

  const model = config.model || "claude-sonnet-4-20250514";
  const maxTokens = config.maxTokens || 1024;

  return {
    async infer(payload: Val, _ctxDigest: string): Promise<Val> {
      const prompt = valToPrompt(payload);

      const response = await fetch("https://api.anthropic.com/v1/messages", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
          "x-api-key": apiKey,
          "anthropic-version": "2023-06-01"
        },
        body: JSON.stringify({
          model,
          max_tokens: maxTokens,
          messages: [{ role: "user", content: prompt }]
        })
      });

      if (!response.ok) {
        const error = await response.text();
        throw new Error(`Anthropic API error: ${response.status} ${error}`);
      }

      const data = await response.json() as any;

      // Extract text from response
      const text = data.content?.[0]?.text || "";
      return responseToVal(text);
    }
  };
}

export function createAnthropicOracle(config: AnthropicConfig = {}): OracleAdapter {
  return new LegacyOracleWrapper(createAnthropicLegacy(config));
}

// ============================================================
// OpenAI Adapter
// ============================================================

export interface OpenAIConfig {
  apiKey?: string;     // defaults to OPENAI_API_KEY env var
  model?: string;      // defaults to gpt-4-turbo-preview
  maxTokens?: number;  // defaults to 1024
  baseUrl?: string;    // for OpenAI-compatible APIs
}

function createOpenAILegacy(config: OpenAIConfig = {}): LegacyOracleAdapter {
  const apiKey = config.apiKey || process.env.OPENAI_API_KEY;
  if (!apiKey) {
    throw new Error("OPENAI_API_KEY environment variable not set");
  }

  const model = config.model || "gpt-4-turbo-preview";
  const maxTokens = config.maxTokens || 1024;
  const baseUrl = config.baseUrl || "https://api.openai.com/v1";

  return {
    async infer(payload: Val, _ctxDigest: string): Promise<Val> {
      const prompt = valToPrompt(payload);

      const response = await fetch(`${baseUrl}/chat/completions`, {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
          "Authorization": `Bearer ${apiKey}`
        },
        body: JSON.stringify({
          model,
          max_tokens: maxTokens,
          messages: [{ role: "user", content: prompt }]
        })
      });

      if (!response.ok) {
        const error = await response.text();
        throw new Error(`OpenAI API error: ${response.status} ${error}`);
      }

      const data = await response.json() as any;

      // Extract text from response
      const text = data.choices?.[0]?.message?.content || "";
      return responseToVal(text);
    }
  };
}

export function createOpenAIOracle(config: OpenAIConfig = {}): OracleAdapter {
  return new LegacyOracleWrapper(createOpenAILegacy(config));
}

// ============================================================
// Commit Adapter (for side effects)
// ============================================================

export interface ConsoleCommitConfig {
  log?: boolean;  // whether to log commits
}

/** Simple commit adapter that logs to console */
export function createConsoleCommit(config: ConsoleCommitConfig = {}): CommitAdapter {
  const shouldLog = config.log ?? true;

  return {
    async commit(payload: Val, _ctxDigest: string): Promise<Val> {
      if (shouldLog) {
        console.log("[COMMIT]", valToPrompt(payload));
      }
      return { tag: "Unit" };
    }
  };
}

// ============================================================
// Auto-detect Oracle (tries env vars)
// ============================================================

/**
 * Create an Oracle adapter by auto-detecting available API keys.
 * Checks ANTHROPIC_API_KEY first, then OPENAI_API_KEY.
 */
export function createAutoOracle(): OracleAdapter {
  if (process.env.ANTHROPIC_API_KEY) {
    console.log("[Oracle] Using Anthropic Claude");
    return createAnthropicOracle();
  }
  if (process.env.OPENAI_API_KEY) {
    console.log("[Oracle] Using OpenAI");
    return createOpenAIOracle();
  }
  throw new Error(
    "No LLM API key found. Set ANTHROPIC_API_KEY or OPENAI_API_KEY environment variable."
  );
}
