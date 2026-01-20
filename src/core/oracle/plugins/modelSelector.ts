// src/core/oracle/plugins/modelSelector.ts
// Model Selection - Lisp-level model specification with default + per-call override

import type { OracleAdapter, OracleInit } from "../adapter";
import type { OracleSession, OracleReq, OracleResp } from "../protocol";
import type { MeaningVal } from "../meaning";
import type { Val } from "../../eval/values";
import { registry as globalRegistry, type PluginRegistry, type BasePluginConfig } from "./registry";

// ═══════════════════════════════════════════════════════════════════════════
// MODEL CONFIGURATION
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Model configuration for the selector
 */
export type ModelConfig = {
  /** Default model to use when not specified */
  defaultModel: string;
  /** Default plugin to use when model doesn't specify */
  defaultPlugin?: string;
  /** Shared config for all adapters */
  sharedConfig?: Partial<BasePluginConfig>;
  /** Per-model config overrides */
  modelConfigs?: Record<string, Partial<BasePluginConfig>>;
  /** Custom registry (for testing), defaults to global registry */
  registry?: PluginRegistry;
};

/**
 * Parse model specification from Val payload
 *
 * Supports multiple formats:
 * - `(infer "question")` → use default model
 * - `(infer {:model "gpt-4o" :payload "question"})` → use specified model
 * - `(infer {:plugin "anthropic" :model "claude-3-opus" :payload "question"})` → explicit plugin
 * - `(infer {:model "anthropic:claude-3-opus" :payload "question"})` → plugin:model format
 */
function parseModelSpec(payload: Val): { model?: string; plugin?: string; actualPayload: Val } {
  if (payload.tag !== "Map") {
    // Simple payload - use default
    return { actualPayload: payload };
  }

  // Look for model/plugin keys in the map
  let model: string | undefined;
  let plugin: string | undefined;
  let actualPayload: Val = payload;

  for (const [k, v] of payload.entries) {
    const key = k.tag === "Str" ? k.s : k.tag === "Sym" ? k.name : null;

    if (key === "model" && v.tag === "Str") {
      model = v.s;
    } else if (key === "plugin" && v.tag === "Str") {
      plugin = v.s;
    } else if (key === "payload") {
      actualPayload = v;
    }
  }

  // If model contains ":", parse as plugin:model
  if (model && model.includes(":")) {
    const [p, m] = model.split(":", 2);
    plugin = plugin || p;
    model = m;
  }

  return { model, plugin, actualPayload };
}

// ═══════════════════════════════════════════════════════════════════════════
// MODEL SELECTOR ADAPTER
// ═══════════════════════════════════════════════════════════════════════════

/**
 * ModelSelectorAdapter - Routes to appropriate adapter based on model specification
 *
 * Usage:
 * ```typescript
 * const selector = new ModelSelectorAdapter({
 *   defaultModel: "claude-sonnet-4-20250514",
 *   defaultPlugin: "anthropic",
 *   sharedConfig: { maxTokens: 4096 },
 * });
 *
 * const runtime = new RuntimeImpl(selector, snapshots, receipts, commit);
 *
 * // In Lisp:
 * (infer "question")                                    ; uses default
 * (infer {:model "gpt-4o" :payload "question"})         ; uses gpt-4o
 * (infer {:model "openai:gpt-4o" :payload "question"})  ; explicit plugin
 * ```
 */
export class ModelSelectorAdapter implements OracleAdapter {
  private config: ModelConfig;
  private adapterCache = new Map<string, OracleAdapter>();
  private registry: PluginRegistry;

  constructor(config: ModelConfig) {
    this.config = config;
    this.registry = config.registry || globalRegistry;
  }

  startSession(init: OracleInit): OracleSession {
    const selector = this;

    return (async function* (): OracleSession {
      // Extract model specification from payload
      const payload = init.tag === "Infer" ? init.payload : { tag: "Unit" as const };
      const { model, plugin, actualPayload } = parseModelSpec(payload);

      // Resolve which adapter to use
      const adapter = selector.getOrCreateAdapter(model, plugin);

      // Create modified init with actual payload
      const modifiedInit: OracleInit = init.tag === "Infer"
        ? { ...init, payload: actualPayload }
        : init;

      // Delegate to the resolved adapter
      const session = adapter.startSession(modifiedInit);

      // Passthrough the coroutine
      let resp: OracleResp = { tag: "RespAck" };
      while (true) {
        const step = await session.next(resp);
        if (step.done) {
          return step.value;
        }
        resp = yield step.value as OracleReq;
      }
    })();
  }

  /**
   * Get or create an adapter for the given model/plugin
   */
  private getOrCreateAdapter(model?: string, plugin?: string): OracleAdapter {
    // Use defaults if not specified
    const resolvedModel = model || this.config.defaultModel;
    const resolvedPlugin = plugin || this.config.defaultPlugin;

    // Build cache key
    const cacheKey = resolvedPlugin
      ? `${resolvedPlugin}:${resolvedModel}`
      : resolvedModel;

    // Check cache
    let adapter = this.adapterCache.get(cacheKey);
    if (adapter) return adapter;

    // Build config for this adapter
    const adapterConfig: BasePluginConfig = {
      ...this.config.sharedConfig,
      ...this.config.modelConfigs?.[resolvedModel],
      model: resolvedModel,
    };

    // Create adapter
    if (resolvedPlugin) {
      const pluginObj = this.registry.get(resolvedPlugin);
      if (!pluginObj) {
        throw new Error(`Unknown plugin: ${resolvedPlugin}`);
      }
      adapter = pluginObj.createAdapter(adapterConfig);
    } else {
      // Auto-detect plugin from model name
      const candidates = this.registry.findByModel(resolvedModel);
      if (candidates.length === 0) {
        throw new Error(`No plugin found for model: ${resolvedModel}`);
      }
      adapter = candidates[0].createAdapter(adapterConfig);
    }

    // Cache and return
    this.adapterCache.set(cacheKey, adapter);
    return adapter;
  }

  /**
   * Clear the adapter cache (useful for testing)
   */
  clearCache(): void {
    this.adapterCache.clear();
  }

  /**
   * Get the current config
   */
  getConfig(): ModelConfig {
    return { ...this.config };
  }

  /**
   * Update the default model
   */
  setDefaultModel(model: string): void {
    this.config.defaultModel = model;
  }

  /**
   * Update the default plugin
   */
  setDefaultPlugin(plugin: string): void {
    this.config.defaultPlugin = plugin;
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// LISP PRIMITIVES FOR MODEL SELECTION
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Helper to build a model-specified inference payload
 *
 * In Lisp, you'd use:
 *   (infer (with-model "gpt-4o" "my question"))
 *
 * This helper builds the map structure for that.
 */
export function withModel(model: string, payload: Val): Val {
  return {
    tag: "Map",
    entries: [
      [{ tag: "Str", s: "model" }, { tag: "Str", s: model }],
      [{ tag: "Str", s: "payload" }, payload],
    ],
  };
}

/**
 * Helper to build a plugin-specified inference payload
 *
 * In Lisp:
 *   (infer (with-plugin "anthropic" "claude-3-opus" "my question"))
 */
export function withPlugin(plugin: string, model: string, payload: Val): Val {
  return {
    tag: "Map",
    entries: [
      [{ tag: "Str", s: "plugin" }, { tag: "Str", s: plugin }],
      [{ tag: "Str", s: "model" }, { tag: "Str", s: model }],
      [{ tag: "Str", s: "payload" }, payload],
    ],
  };
}

// ═══════════════════════════════════════════════════════════════════════════
// FACTORY FUNCTIONS
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Create a model selector with sensible defaults
 *
 * Tries to detect which plugins are available and picks a good default.
 */
export function createModelSelector(overrides?: Partial<ModelConfig>): ModelSelectorAdapter {
  // Try to find a working plugin
  const registry = overrides?.registry ?? globalRegistry;
  let defaultPlugin: string | undefined;
  let defaultModel: string;

  if (registry.has("anthropic") && process.env.ANTHROPIC_API_KEY) {
    defaultPlugin = "anthropic";
    defaultModel = "claude-sonnet-4-20250514";
  } else if (registry.has("openai") && process.env.OPENAI_API_KEY) {
    defaultPlugin = "openai";
    defaultModel = "gpt-4o";
  } else if (registry.has("ollama")) {
    defaultPlugin = "ollama";
    defaultModel = "llama3.1";
  } else {
    // Fallback - will error at runtime if no plugins available
    defaultModel = "claude-sonnet-4-20250514";
  }

  return new ModelSelectorAdapter({
    defaultModel: overrides?.defaultModel || defaultModel,
    defaultPlugin: overrides?.defaultPlugin || defaultPlugin,
    sharedConfig: overrides?.sharedConfig,
    modelConfigs: overrides?.modelConfigs,
    registry,
  });
}

/**
 * Create a model selector for a specific plugin
 */
export function createPluginSelector(
  pluginId: string,
  model?: string,
  config?: Partial<BasePluginConfig> & { registry?: PluginRegistry }
): ModelSelectorAdapter {
  const registry = config?.registry ?? globalRegistry;
  const plugin = registry.get(pluginId);
  if (!plugin) {
    throw new Error(`Unknown plugin: ${pluginId}`);
  }

  return new ModelSelectorAdapter({
    defaultModel: model || plugin.defaultModel,
    defaultPlugin: pluginId,
    sharedConfig: config,
  });
}

export { parseModelSpec };
