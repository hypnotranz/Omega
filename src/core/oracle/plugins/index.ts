// src/core/oracle/plugins/index.ts
// Plugin System - exports and auto-registration

// Core registry (must be imported first)
export {
  registry,
  PluginRegistry,
  createAdapterFromModel,
  getDefaultAdapter,
  type OraclePlugin,
  type PluginCapabilities,
  type CapabilityFilter,
  type ProtocolCaps,
  type ToolingCaps,
  type MCPCaps,
  type SessionCaps,
  type IOCaps,
  type BasePluginConfig,
  type ConfigValidation,
  type HealthCheckResult,
  type PluginSummary,
} from "./registry";

// Import plugins to trigger auto-registration
import "./anthropic";
import "./openai";
import "./ollama";

// Re-export plugin classes for direct use
export { anthropicPlugin, AnthropicOracleAdapter } from "./anthropic";
export { openaiPlugin, OpenAIOracleAdapter } from "./openai";
export { ollamaPlugin, OllamaOracleAdapter, modelSupportsTools } from "./ollama";

// Model selection
export {
  ModelSelectorAdapter,
  createModelSelector,
  createPluginSelector,
  withModel,
  withPlugin,
  parseModelSpec,
  type ModelConfig,
} from "./modelSelector";
