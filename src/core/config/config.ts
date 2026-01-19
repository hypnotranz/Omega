// src/core/config/config.ts
// Configuration system for OmegaLLM
// Based on LambdaRLM's config system

import * as fs from "fs";
import * as path from "path";

// =========================================================================
// Configuration Types
// =========================================================================

export type OracleProvider = "anthropic" | "openai" | "ollama";

export type LLMConfig = {
  /** LLM provider: anthropic, openai, or ollama */
  provider: OracleProvider;
  /** Model name (provider-specific) */
  model: string;
  /** API key (optional, can use env vars) */
  apiKey?: string;
  /** API base URL (for ollama or custom endpoints) */
  baseUrl?: string;
  /** Request timeout in milliseconds */
  timeoutMs: number;
  /** Maximum tokens for responses */
  maxTokens: number;
  /** System prompt override */
  systemPrompt?: string;
};

export type RuntimeConfig = {
  /** Maximum oracle turns before termination */
  maxOracleTurns: number;
  /** Maximum eval steps per operation */
  maxEvalSteps: number;
  /** Maximum tool calls */
  maxToolCalls: number;
  /** Maximum nested depth */
  maxNestedDepth: number;
};

export type OmegaConfig = {
  llm: LLMConfig;
  runtime: RuntimeConfig;
};

// =========================================================================
// Default Configuration
// =========================================================================

export const DEFAULT_MODELS: Record<OracleProvider, string> = {
  anthropic: "claude-sonnet-4-20250514",
  openai: "gpt-4o",
  ollama: "llama3.1",
};

export const DEFAULT_BASE_URLS: Partial<Record<OracleProvider, string>> = {
  ollama: "http://localhost:11434",
};

export const DEFAULT_LLM_CONFIG: LLMConfig = {
  provider: "anthropic",
  model: DEFAULT_MODELS.anthropic,
  timeoutMs: 120_000,
  maxTokens: 4096,
};

export const DEFAULT_RUNTIME_CONFIG: RuntimeConfig = {
  maxOracleTurns: 10_000,
  maxEvalSteps: 500_000,
  maxToolCalls: 1000,
  maxNestedDepth: 16,
};

export const DEFAULT_CONFIG: OmegaConfig = {
  llm: DEFAULT_LLM_CONFIG,
  runtime: DEFAULT_RUNTIME_CONFIG,
};

// =========================================================================
// Configuration Loading
// =========================================================================

/**
 * Get API key for provider from environment variables.
 * Checks both generic and provider-specific env vars.
 */
export function getApiKeyFromEnv(provider: OracleProvider): string | undefined {
  // Check generic env var first
  const generic = process.env.OMEGA_API_KEY;
  if (generic) return generic;

  // Check provider-specific env vars
  switch (provider) {
    case "anthropic":
      return process.env.ANTHROPIC_API_KEY;
    case "openai":
      return process.env.OPENAI_API_KEY;
    case "ollama":
      // Ollama doesn't need an API key for local instance
      return undefined;
  }
}

/**
 * Load configuration from environment variables.
 */
export function configFromEnv(prefix = "OMEGA"): OmegaConfig {
  const provider = (process.env[`${prefix}_PROVIDER`] as OracleProvider) || DEFAULT_LLM_CONFIG.provider;
  const model = process.env[`${prefix}_MODEL`] || DEFAULT_MODELS[provider] || DEFAULT_LLM_CONFIG.model;
  const apiKey = process.env[`${prefix}_API_KEY`] || getApiKeyFromEnv(provider);
  const baseUrl = process.env[`${prefix}_BASE_URL`] || DEFAULT_BASE_URLS[provider];
  const timeoutMs = parseInt(process.env[`${prefix}_TIMEOUT_MS`] || "", 10) || DEFAULT_LLM_CONFIG.timeoutMs;
  const maxTokens = parseInt(process.env[`${prefix}_MAX_TOKENS`] || "", 10) || DEFAULT_LLM_CONFIG.maxTokens;

  const maxOracleTurns = parseInt(process.env[`${prefix}_MAX_ORACLE_TURNS`] || "", 10) || DEFAULT_RUNTIME_CONFIG.maxOracleTurns;
  const maxEvalSteps = parseInt(process.env[`${prefix}_MAX_EVAL_STEPS`] || "", 10) || DEFAULT_RUNTIME_CONFIG.maxEvalSteps;
  const maxToolCalls = parseInt(process.env[`${prefix}_MAX_TOOL_CALLS`] || "", 10) || DEFAULT_RUNTIME_CONFIG.maxToolCalls;
  const maxNestedDepth = parseInt(process.env[`${prefix}_MAX_NESTED_DEPTH`] || "", 10) || DEFAULT_RUNTIME_CONFIG.maxNestedDepth;

  return {
    llm: {
      provider,
      model,
      apiKey,
      baseUrl,
      timeoutMs,
      maxTokens,
    },
    runtime: {
      maxOracleTurns,
      maxEvalSteps,
      maxToolCalls,
      maxNestedDepth,
    },
  };
}

/**
 * Load configuration from a JSON or YAML file.
 */
export function configFromFile(filePath: string): OmegaConfig {
  if (!fs.existsSync(filePath)) {
    throw new Error(`Config file not found: ${filePath}`);
  }

  const content = fs.readFileSync(filePath, "utf8");
  const ext = path.extname(filePath).toLowerCase();

  let data: Record<string, unknown>;

  if (ext === ".json") {
    data = JSON.parse(content);
  } else if (ext === ".yaml" || ext === ".yml") {
    // Simple YAML parser for basic configs
    data = parseSimpleYaml(content);
  } else {
    throw new Error(`Unsupported config file format: ${ext}`);
  }

  return configFromObject(data);
}

/**
 * Create configuration from a plain object (e.g., from parsed JSON/YAML).
 */
export function configFromObject(data: Record<string, unknown>): OmegaConfig {
  const llmData = (data.llm as Record<string, unknown>) || {};
  const runtimeData = (data.runtime as Record<string, unknown>) || {};

  const provider = (llmData.provider as OracleProvider) || DEFAULT_LLM_CONFIG.provider;

  return {
    llm: {
      provider,
      model: (llmData.model as string) || DEFAULT_MODELS[provider] || DEFAULT_LLM_CONFIG.model,
      apiKey: (llmData.apiKey as string) || (llmData.api_key as string),
      baseUrl: (llmData.baseUrl as string) || (llmData.base_url as string) || DEFAULT_BASE_URLS[provider],
      timeoutMs: (llmData.timeoutMs as number) || (llmData.timeout_ms as number) || DEFAULT_LLM_CONFIG.timeoutMs,
      maxTokens: (llmData.maxTokens as number) || (llmData.max_tokens as number) || DEFAULT_LLM_CONFIG.maxTokens,
      systemPrompt: (llmData.systemPrompt as string) || (llmData.system_prompt as string),
    },
    runtime: {
      maxOracleTurns: (runtimeData.maxOracleTurns as number) || (runtimeData.max_oracle_turns as number) || DEFAULT_RUNTIME_CONFIG.maxOracleTurns,
      maxEvalSteps: (runtimeData.maxEvalSteps as number) || (runtimeData.max_eval_steps as number) || DEFAULT_RUNTIME_CONFIG.maxEvalSteps,
      maxToolCalls: (runtimeData.maxToolCalls as number) || (runtimeData.max_tool_calls as number) || DEFAULT_RUNTIME_CONFIG.maxToolCalls,
      maxNestedDepth: (runtimeData.maxNestedDepth as number) || (runtimeData.max_nested_depth as number) || DEFAULT_RUNTIME_CONFIG.maxNestedDepth,
    },
  };
}

/**
 * Merge configs with later ones overriding earlier ones.
 */
export function mergeConfigs(...configs: Partial<OmegaConfig>[]): OmegaConfig {
  let result = { ...DEFAULT_CONFIG };

  for (const cfg of configs) {
    if (cfg.llm) {
      result.llm = { ...result.llm, ...cfg.llm };
    }
    if (cfg.runtime) {
      result.runtime = { ...result.runtime, ...cfg.runtime };
    }
  }

  return result;
}

/**
 * Auto-detect and load configuration.
 * Priority: CLI args > config file > environment > defaults
 */
export function loadConfig(options?: {
  configFile?: string;
  overrides?: Partial<OmegaConfig>;
}): OmegaConfig {
  // Start with env config (includes defaults)
  let config = configFromEnv();

  // Load file config if specified
  if (options?.configFile) {
    const fileConfig = configFromFile(options.configFile);
    config = mergeConfigs(config, fileConfig);
  } else {
    // Try to find default config files
    const defaultPaths = ["omega.config.json", "omega.config.yaml", "omega.config.yml", "config.json", "config.yaml"];
    for (const p of defaultPaths) {
      if (fs.existsSync(p)) {
        const fileConfig = configFromFile(p);
        config = mergeConfigs(config, fileConfig);
        break;
      }
    }
  }

  // Apply overrides
  if (options?.overrides) {
    config = mergeConfigs(config, options.overrides);
  }

  // Resolve API key from environment if not set
  if (!config.llm.apiKey) {
    config.llm.apiKey = getApiKeyFromEnv(config.llm.provider);
  }

  return config;
}

// =========================================================================
// Simple YAML Parser (for basic configs only)
// =========================================================================

function parseSimpleYaml(content: string): Record<string, unknown> {
  const result: Record<string, unknown> = {};
  const stack: Array<{ obj: Record<string, unknown>; indent: number }> = [{ obj: result, indent: -1 }];

  const lines = content.split("\n");

  for (const rawLine of lines) {
    // Skip empty lines and comments
    const trimmed = rawLine.trim();
    if (!trimmed || trimmed.startsWith("#")) continue;

    // Calculate indentation
    const indent = rawLine.search(/\S/);
    if (indent < 0) continue;

    // Pop stack to find parent at correct indent level
    while (stack.length > 1 && stack[stack.length - 1]!.indent >= indent) {
      stack.pop();
    }

    const parent = stack[stack.length - 1]!.obj;

    // Parse key: value
    const colonIdx = trimmed.indexOf(":");
    if (colonIdx < 0) continue;

    const key = trimmed.slice(0, colonIdx).trim();
    let value: string | number | boolean | Record<string, unknown> = trimmed.slice(colonIdx + 1).trim();

    if (value === "") {
      // Nested object
      const nested: Record<string, unknown> = {};
      parent[key] = nested;
      stack.push({ obj: nested, indent });
    } else {
      // Parse value
      if (value === "true") {
        parent[key] = true;
      } else if (value === "false") {
        parent[key] = false;
      } else if (value === "null") {
        parent[key] = null;
      } else if (/^-?\d+$/.test(value)) {
        parent[key] = parseInt(value, 10);
      } else if (/^-?\d+\.\d+$/.test(value)) {
        parent[key] = parseFloat(value);
      } else if ((value.startsWith('"') && value.endsWith('"')) || (value.startsWith("'") && value.endsWith("'"))) {
        parent[key] = value.slice(1, -1);
      } else {
        parent[key] = value;
      }
    }
  }

  return result;
}

// =========================================================================
// Config Validation
// =========================================================================

export type ConfigValidation = {
  valid: boolean;
  errors: string[];
  warnings: string[];
};

export function validateConfig(config: OmegaConfig): ConfigValidation {
  const errors: string[] = [];
  const warnings: string[] = [];

  // Check API key for non-ollama providers
  if (config.llm.provider !== "ollama" && !config.llm.apiKey) {
    errors.push(`Missing API key for provider: ${config.llm.provider}. Set ${config.llm.provider.toUpperCase()}_API_KEY or OMEGA_API_KEY`);
  }

  // Check reasonable limits
  if (config.runtime.maxOracleTurns < 1) {
    errors.push("maxOracleTurns must be at least 1");
  }
  if (config.runtime.maxEvalSteps < 100) {
    warnings.push("maxEvalSteps is very low, may cause premature termination");
  }

  return {
    valid: errors.length === 0,
    errors,
    warnings,
  };
}
