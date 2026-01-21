// src/core/oracle/plugins/registry.ts
// Plugin Registry - pluggable LLM adapter system with capability declarations

import type { OracleAdapter } from "../adapter";

// ═══════════════════════════════════════════════════════════════════════════
// CAPABILITY TYPES - What each plugin can do
// ═══════════════════════════════════════════════════════════════════════════

/** Oracle Protocol support */
export type ProtocolCaps = {
  reqEval: boolean;      // Can execute ReqEval
  reqApply: boolean;     // Can execute ReqApply
  reqObserve: boolean;   // Can execute ReqObserve
  reqMatch: boolean;     // Can execute ReqMatch
  reqAssert: boolean;    // Can execute ReqAssert
};

/** Tool calling capabilities */
export type ToolingCaps = {
  native: boolean;                                    // Has native tool calling
  format?: "anthropic" | "openai" | "ollama" | "mcp"; // Which format
  maxTools?: number;                                  // Tool limit per call
};

/** MCP protocol support */
export type MCPCaps = {
  client: boolean;   // Can call MCP servers
  server: boolean;   // Can expose as MCP server
};

/** Session behavior */
export type SessionCaps = {
  multiTurn: boolean;     // Can maintain conversation state
  streaming: boolean;     // Can stream partial responses
  maxContext: number;     // Token limit
};

/** I/O capabilities */
export type IOCaps = {
  vision: boolean;           // Can process images
  audio: boolean;            // Can process audio
  structuredOutput: boolean; // Can return JSON schema-constrained output
};

/** Full capability declaration */
export type PluginCapabilities = {
  oracleProtocol: ProtocolCaps;
  tooling: ToolingCaps;
  mcp: MCPCaps;
  session: SessionCaps;
  io: IOCaps;
};

/** Deep partial type for filtering */
type DeepPartial<T> = {
  [P in keyof T]?: T[P] extends object ? DeepPartial<T[P]> : T[P];
};

/** Capability filter (allows partial matching) */
export type CapabilityFilter = DeepPartial<PluginCapabilities>;

// ═══════════════════════════════════════════════════════════════════════════
// PLUGIN CONFIG TYPES
// ═══════════════════════════════════════════════════════════════════════════

/** Base config all plugins accept */
export type BasePluginConfig = {
  apiKey?: string;
  model?: string;
  maxTokens?: number;
  temperature?: number;
  systemPrompt?: string;
  baseUrl?: string;
  /** Enable streaming mode - accumulates responses from stream chunks */
  streaming?: boolean;
};

/** Config validation result */
export type ConfigValidation = {
  valid: boolean;
  errors?: string[];
};

/** Health check result */
export type HealthCheckResult = {
  ok: boolean;
  message?: string;
  latencyMs?: number;
};

// ═══════════════════════════════════════════════════════════════════════════
// PLUGIN INTERFACE
// ═══════════════════════════════════════════════════════════════════════════

/**
 * OraclePlugin - Contract for all LLM adapter plugins
 *
 * Each plugin:
 * 1. Declares its capabilities honestly
 * 2. Can validate configuration before creation
 * 3. Creates OracleAdapter instances
 * 4. Optionally provides health checking
 */
export interface OraclePlugin {
  /** Unique identifier (e.g., "anthropic", "openai", "ollama") */
  readonly id: string;

  /** Human-readable name */
  readonly name: string;

  /** What this plugin supports */
  readonly capabilities: PluginCapabilities;

  /** Supported model identifiers for this plugin */
  readonly supportedModels: string[];

  /** Default model if none specified */
  readonly defaultModel: string;

  /** Validate config before creation */
  validateConfig(config: BasePluginConfig): ConfigValidation;

  /** Create an adapter instance */
  createAdapter(config: BasePluginConfig): OracleAdapter;

  /** Optional: Check if this plugin can work (API key set, service reachable) */
  healthCheck?(): Promise<HealthCheckResult>;
}

// ═══════════════════════════════════════════════════════════════════════════
// CAPABILITY QUERY HELPERS
// ═══════════════════════════════════════════════════════════════════════════

/** Check if capabilities match a filter */
function matchesCapabilities(
  caps: PluginCapabilities,
  filter: CapabilityFilter
): boolean {
  // Check oracleProtocol
  if (filter.oracleProtocol) {
    const p = filter.oracleProtocol;
    const c = caps.oracleProtocol;
    if (p.reqEval && !c.reqEval) return false;
    if (p.reqApply && !c.reqApply) return false;
    if (p.reqObserve && !c.reqObserve) return false;
    if (p.reqMatch && !c.reqMatch) return false;
    if (p.reqAssert && !c.reqAssert) return false;
  }

  // Check tooling
  if (filter.tooling) {
    const t = filter.tooling;
    const c = caps.tooling;
    if (t.native && !c.native) return false;
    if (t.format && c.format !== t.format) return false;
  }

  // Check mcp
  if (filter.mcp) {
    const m = filter.mcp;
    const c = caps.mcp;
    if (m.client && !c.client) return false;
    if (m.server && !c.server) return false;
  }

  // Check session
  if (filter.session) {
    const s = filter.session;
    const c = caps.session;
    if (s.multiTurn && !c.multiTurn) return false;
    if (s.streaming && !c.streaming) return false;
    if (s.maxContext && c.maxContext < s.maxContext) return false;
  }

  // Check io
  if (filter.io) {
    const i = filter.io;
    const c = caps.io;
    if (i.vision && !c.vision) return false;
    if (i.audio && !c.audio) return false;
    if (i.structuredOutput && !c.structuredOutput) return false;
  }

  return true;
}

/** Score a plugin's capabilities (higher = better for Oracle) */
function scoreCapabilities(caps: PluginCapabilities): number {
  let score = 0;

  // Protocol support (most important)
  if (caps.oracleProtocol.reqEval) score += 10;
  if (caps.oracleProtocol.reqApply) score += 10;
  if (caps.oracleProtocol.reqObserve) score += 5;
  if (caps.oracleProtocol.reqMatch) score += 3;
  if (caps.oracleProtocol.reqAssert) score += 2;

  // Native tools (very valuable)
  if (caps.tooling.native) score += 15;

  // MCP support
  if (caps.mcp.client) score += 5;
  if (caps.mcp.server) score += 3;

  // Session features
  if (caps.session.multiTurn) score += 5;
  if (caps.session.streaming) score += 2;

  // I/O
  if (caps.io.structuredOutput) score += 5;
  if (caps.io.vision) score += 2;

  return score;
}

// ═══════════════════════════════════════════════════════════════════════════
// PLUGIN REGISTRY
// ═══════════════════════════════════════════════════════════════════════════

/**
 * PluginRegistry - Central registry for Oracle plugins
 *
 * Usage:
 *   import { registry } from "./plugins/registry";
 *
 *   // List what's available
 *   console.log(registry.list().map(p => p.id));
 *
 *   // Query by capability
 *   const mcpPlugins = registry.query({ mcp: { client: true } });
 *
 *   // Find best for Oracle protocol
 *   const best = registry.findBestForOracle();
 *
 *   // Create adapter
 *   const adapter = best.createAdapter({ model: "claude-sonnet-4-20250514" });
 */
export class PluginRegistry {
  private plugins = new Map<string, OraclePlugin>();

  /** Register a new plugin */
  register(plugin: OraclePlugin): void {
    if (this.plugins.has(plugin.id)) {
      throw new Error(`Plugin '${plugin.id}' already registered`);
    }
    this.plugins.set(plugin.id, plugin);
  }

  /** Unregister a plugin */
  unregister(id: string): boolean {
    return this.plugins.delete(id);
  }

  /** Get a plugin by ID */
  get(id: string): OraclePlugin | undefined {
    return this.plugins.get(id);
  }

  /** Check if a plugin is registered */
  has(id: string): boolean {
    return this.plugins.has(id);
  }

  /** List all registered plugins */
  list(): OraclePlugin[] {
    return [...this.plugins.values()];
  }

  /** Query plugins by capability filter */
  query(filter: CapabilityFilter): OraclePlugin[] {
    return this.list().filter(p => matchesCapabilities(p.capabilities, filter));
  }

  /** Find plugins that support a specific model */
  findByModel(model: string): OraclePlugin[] {
    return this.list().filter(p =>
      p.supportedModels.some(m => {
        if (m === model) return true;
        if (m.includes("*") && model.startsWith(m.replace("*", ""))) return true;
        if (model.startsWith(m)) return true;
        return false;
      })
    );
  }

  /** Find the best plugin for Oracle protocol (scored by capabilities) */
  findBestForOracle(): OraclePlugin | undefined {
    const candidates = this.query({
      oracleProtocol: { reqEval: true, reqApply: true, reqObserve: true }
    });

    if (candidates.length === 0) {
      // Fallback: any plugin with reqEval
      const fallback = this.query({ oracleProtocol: { reqEval: true } });
      if (fallback.length === 0) return undefined;
      return fallback.sort((a, b) =>
        scoreCapabilities(b.capabilities) - scoreCapabilities(a.capabilities)
      )[0];
    }

    return candidates.sort((a, b) =>
      scoreCapabilities(b.capabilities) - scoreCapabilities(a.capabilities)
    )[0];
  }

  /** Check health of all plugins */
  async healthCheckAll(): Promise<Map<string, HealthCheckResult>> {
    const results = new Map<string, HealthCheckResult>();

    for (const plugin of this.plugins.values()) {
      if (plugin.healthCheck) {
        try {
          const result = await plugin.healthCheck();
          results.set(plugin.id, result);
        } catch (err) {
          results.set(plugin.id, {
            ok: false,
            message: err instanceof Error ? err.message : String(err)
          });
        }
      } else {
        results.set(plugin.id, { ok: true, message: "No health check defined" });
      }
    }

    return results;
  }

  /** Get a summary of all plugins and their capabilities */
  summary(): PluginSummary[] {
    return this.list().map(p => ({
      id: p.id,
      name: p.name,
      defaultModel: p.defaultModel,
      hasNativeTools: p.capabilities.tooling.native,
      hasMCP: p.capabilities.mcp.client || p.capabilities.mcp.server,
      hasVision: p.capabilities.io.vision,
      maxContext: p.capabilities.session.maxContext,
      score: scoreCapabilities(p.capabilities),
    }));
  }
}

export type PluginSummary = {
  id: string;
  name: string;
  defaultModel: string;
  hasNativeTools: boolean;
  hasMCP: boolean;
  hasVision: boolean;
  maxContext: number;
  score: number;
};

// ═══════════════════════════════════════════════════════════════════════════
// GLOBAL REGISTRY INSTANCE
// ═══════════════════════════════════════════════════════════════════════════

/** Global plugin registry - import and use directly */
export const registry = new PluginRegistry();

// ═══════════════════════════════════════════════════════════════════════════
// HELPER: Create adapter from model string
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Create an adapter from a model specification
 *
 * @param model Model name or "plugin:model" format
 * @param config Optional additional config
 * @returns OracleAdapter
 *
 * Examples:
 *   createAdapterFromModel("claude-sonnet-4-20250514")       // auto-detect plugin
 *   createAdapterFromModel("anthropic:claude-3-opus")  // explicit plugin
 *   createAdapterFromModel("openai:gpt-4o")            // explicit plugin
 *   createAdapterFromModel("ollama:llama3.1:70b")      // explicit plugin
 */
export function createAdapterFromModel(
  model: string,
  config: Partial<BasePluginConfig> = {}
): OracleAdapter {
  // Check for "plugin:model" format
  if (model.includes(":")) {
    const [pluginId, modelName] = model.split(":", 2);
    const plugin = registry.get(pluginId);
    if (!plugin) {
      throw new Error(`Unknown plugin: ${pluginId}`);
    }
    return plugin.createAdapter({ ...config, model: modelName });
  }

  // Auto-detect plugin from model name
  const candidates = registry.findByModel(model);
  if (candidates.length === 0) {
    throw new Error(`No plugin found for model: ${model}`);
  }

  // Use first match (could be smarter about this)
  return candidates[0].createAdapter({ ...config, model });
}

/**
 * Get or create the default Oracle adapter
 * Prefers: 1) anthropic if available, 2) openai, 3) any with reqEval
 */
export function getDefaultAdapter(config: Partial<BasePluginConfig> = {}): OracleAdapter {
  // Try anthropic first
  const anthropic = registry.get("anthropic");
  if (anthropic) {
    const validation = anthropic.validateConfig(config);
    if (validation.valid) {
      return anthropic.createAdapter(config);
    }
  }

  // Try openai
  const openai = registry.get("openai");
  if (openai) {
    const validation = openai.validateConfig(config);
    if (validation.valid) {
      return openai.createAdapter(config);
    }
  }

  // Fall back to best available
  const best = registry.findBestForOracle();
  if (!best) {
    throw new Error("No Oracle plugins registered. Install an adapter plugin first.");
  }

  return best.createAdapter(config);
}
