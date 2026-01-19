// test/core/config/config.spec.ts
// Tests for configuration system

import { describe, it, expect, beforeEach, afterEach } from "vitest";
import {
  configFromEnv,
  configFromObject,
  mergeConfigs,
  validateConfig,
  getApiKeyFromEnv,
  DEFAULT_CONFIG,
  DEFAULT_MODELS,
} from "../../../src/core/config/config";

describe("configFromEnv", () => {
  const originalEnv = process.env;

  beforeEach(() => {
    // Create a fresh copy
    process.env = { ...originalEnv };
  });

  afterEach(() => {
    process.env = originalEnv;
  });

  it("returns defaults when no env vars set", () => {
    // Clear relevant env vars
    delete process.env.OMEGA_PROVIDER;
    delete process.env.OMEGA_MODEL;
    delete process.env.OMEGA_API_KEY;
    delete process.env.ANTHROPIC_API_KEY;

    const config = configFromEnv();
    expect(config.llm.provider).toBe(DEFAULT_CONFIG.llm.provider);
    expect(config.llm.model).toBe(DEFAULT_CONFIG.llm.model);
    expect(config.runtime.maxOracleTurns).toBe(DEFAULT_CONFIG.runtime.maxOracleTurns);
  });

  it("reads provider from OMEGA_PROVIDER", () => {
    process.env.OMEGA_PROVIDER = "openai";
    const config = configFromEnv();
    expect(config.llm.provider).toBe("openai");
    // Should also use provider's default model
    expect(config.llm.model).toBe(DEFAULT_MODELS.openai);
  });

  it("reads model from OMEGA_MODEL", () => {
    process.env.OMEGA_MODEL = "gpt-4-turbo";
    const config = configFromEnv();
    expect(config.llm.model).toBe("gpt-4-turbo");
  });

  it("reads API key from OMEGA_API_KEY", () => {
    process.env.OMEGA_API_KEY = "test-key";
    const config = configFromEnv();
    expect(config.llm.apiKey).toBe("test-key");
  });

  it("reads runtime limits", () => {
    process.env.OMEGA_MAX_ORACLE_TURNS = "5000";
    process.env.OMEGA_MAX_EVAL_STEPS = "250000";
    const config = configFromEnv();
    expect(config.runtime.maxOracleTurns).toBe(5000);
    expect(config.runtime.maxEvalSteps).toBe(250000);
  });
});

describe("getApiKeyFromEnv", () => {
  const originalEnv = process.env;

  beforeEach(() => {
    process.env = { ...originalEnv };
  });

  afterEach(() => {
    process.env = originalEnv;
  });

  it("returns OMEGA_API_KEY if set", () => {
    process.env.OMEGA_API_KEY = "generic-key";
    process.env.ANTHROPIC_API_KEY = "anthropic-key";

    expect(getApiKeyFromEnv("anthropic")).toBe("generic-key");
  });

  it("returns ANTHROPIC_API_KEY for anthropic provider", () => {
    delete process.env.OMEGA_API_KEY;
    process.env.ANTHROPIC_API_KEY = "anthropic-key";

    expect(getApiKeyFromEnv("anthropic")).toBe("anthropic-key");
  });

  it("returns OPENAI_API_KEY for openai provider", () => {
    delete process.env.OMEGA_API_KEY;
    process.env.OPENAI_API_KEY = "openai-key";

    expect(getApiKeyFromEnv("openai")).toBe("openai-key");
  });

  it("returns undefined for ollama", () => {
    delete process.env.OMEGA_API_KEY;
    expect(getApiKeyFromEnv("ollama")).toBeUndefined();
  });
});

describe("configFromObject", () => {
  it("parses basic config object", () => {
    const config = configFromObject({
      llm: {
        provider: "openai",
        model: "gpt-4o",
        maxTokens: 8192,
      },
      runtime: {
        maxOracleTurns: 5000,
      },
    });

    expect(config.llm.provider).toBe("openai");
    expect(config.llm.model).toBe("gpt-4o");
    expect(config.llm.maxTokens).toBe(8192);
    expect(config.runtime.maxOracleTurns).toBe(5000);
  });

  it("handles snake_case keys", () => {
    const config = configFromObject({
      llm: {
        api_key: "test-key",
        base_url: "http://localhost:8080",
        timeout_ms: 60000,
        max_tokens: 2048,
      },
      runtime: {
        max_oracle_turns: 3000,
        max_eval_steps: 100000,
      },
    });

    expect(config.llm.apiKey).toBe("test-key");
    expect(config.llm.baseUrl).toBe("http://localhost:8080");
    expect(config.llm.timeoutMs).toBe(60000);
    expect(config.llm.maxTokens).toBe(2048);
    expect(config.runtime.maxOracleTurns).toBe(3000);
    expect(config.runtime.maxEvalSteps).toBe(100000);
  });

  it("uses defaults for missing fields", () => {
    const config = configFromObject({});
    expect(config.llm.provider).toBe(DEFAULT_CONFIG.llm.provider);
    expect(config.runtime.maxToolCalls).toBe(DEFAULT_CONFIG.runtime.maxToolCalls);
  });
});

describe("mergeConfigs", () => {
  it("merges LLM config", () => {
    const merged = mergeConfigs(
      { llm: { provider: "anthropic", model: "claude-3-opus", timeoutMs: 60000, maxTokens: 4096 } },
      { llm: { model: "claude-sonnet-4-20250514" } as any }
    );

    expect(merged.llm.provider).toBe("anthropic");
    expect(merged.llm.model).toBe("claude-sonnet-4-20250514");
    expect(merged.llm.timeoutMs).toBe(60000);
  });

  it("merges runtime config", () => {
    const merged = mergeConfigs(
      { runtime: { maxOracleTurns: 5000, maxEvalSteps: 100000, maxToolCalls: 500, maxNestedDepth: 8 } },
      { runtime: { maxToolCalls: 1000 } as any }
    );

    expect(merged.runtime.maxOracleTurns).toBe(5000);
    expect(merged.runtime.maxToolCalls).toBe(1000);
  });

  it("later configs override earlier ones", () => {
    const merged = mergeConfigs(
      { llm: { model: "a" } as any },
      { llm: { model: "b" } as any },
      { llm: { model: "c" } as any }
    );

    expect(merged.llm.model).toBe("c");
  });
});

describe("validateConfig", () => {
  it("validates complete config with API key", () => {
    const config = {
      ...DEFAULT_CONFIG,
      llm: { ...DEFAULT_CONFIG.llm, apiKey: "test-key" },
    };
    const result = validateConfig(config);
    expect(result.valid).toBe(true);
    expect(result.errors).toHaveLength(0);
  });

  it("errors on missing API key for anthropic", () => {
    const config = {
      ...DEFAULT_CONFIG,
      llm: { ...DEFAULT_CONFIG.llm, provider: "anthropic" as const, apiKey: undefined },
    };
    const result = validateConfig(config);
    expect(result.valid).toBe(false);
    expect(result.errors.some(e => e.includes("Missing API key"))).toBe(true);
  });

  it("allows missing API key for ollama", () => {
    const config = {
      ...DEFAULT_CONFIG,
      llm: { ...DEFAULT_CONFIG.llm, provider: "ollama" as const, apiKey: undefined },
    };
    const result = validateConfig(config);
    expect(result.valid).toBe(true);
  });

  it("warns on very low eval steps", () => {
    const config = {
      ...DEFAULT_CONFIG,
      llm: { ...DEFAULT_CONFIG.llm, apiKey: "key" },
      runtime: { ...DEFAULT_CONFIG.runtime, maxEvalSteps: 50 },
    };
    const result = validateConfig(config);
    expect(result.warnings.some(w => w.includes("very low"))).toBe(true);
  });

  it("errors on invalid maxOracleTurns", () => {
    const config = {
      ...DEFAULT_CONFIG,
      llm: { ...DEFAULT_CONFIG.llm, apiKey: "key" },
      runtime: { ...DEFAULT_CONFIG.runtime, maxOracleTurns: 0 },
    };
    const result = validateConfig(config);
    expect(result.valid).toBe(false);
    expect(result.errors.some(e => e.includes("maxOracleTurns"))).toBe(true);
  });
});
