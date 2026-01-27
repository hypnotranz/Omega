/**
 * Shared LLM Transport Interface
 *
 * This is the hexagonal port that both Oracle and OPR use to talk to LLMs.
 * Having a single transport means:
 * - No duplicate adapter code
 * - Shared rate limiting, caching, etc.
 * - Unified cost tracking
 */

/**
 * A message in the conversation
 */
export interface LLMMessage {
  role: 'system' | 'user' | 'assistant';
  content: string;
}

/**
 * Request to the LLM
 */
export interface LLMRequest {
  messages: LLMMessage[];
  model?: string;
  temperature?: number;
  maxTokens?: number;
  /** Optional metadata for tracking/logging */
  metadata?: {
    kernelId?: string;
    op?: string;
    attempt?: number;
    requestId?: string;
  };
}

/**
 * Usage information from LLM response
 */
export interface LLMUsage {
  promptTokens: number;
  completionTokens: number;
  totalTokens: number;
  estimatedCost: number;
}

/**
 * Response from the LLM
 */
export interface LLMResponse {
  text: string;
  usage?: LLMUsage;
  model?: string;
  finishReason?: 'stop' | 'length' | 'content_filter' | string;
}

/**
 * The transport interface - both Oracle and OPR adapters implement this
 */
export interface LLMTransport {
  /** Send a completion request */
  complete(request: LLMRequest): Promise<LLMResponse>;

  /** Get the default model name */
  getModel(): string;

  /** Optional: get last usage stats */
  getLastUsage?(): LLMUsage | undefined;
}

/**
 * Configuration for creating transports
 */
export interface OpenAITransportConfig {
  apiKey: string;
  model: string;
  baseURL?: string;
  timeout?: number;
}

export interface AnthropicTransportConfig {
  apiKey: string;
  model: string;
  baseURL?: string;
  timeout?: number;
}

/**
 * Cost estimation helpers (rough per-token pricing)
 */
const MODEL_COSTS: Record<string, { input: number; output: number }> = {
  // OpenAI
  'gpt-4o': { input: 0.005, output: 0.015 },
  'gpt-4o-mini': { input: 0.00015, output: 0.0006 },
  'gpt-4-turbo': { input: 0.01, output: 0.03 },
  'gpt-4': { input: 0.03, output: 0.06 },
  'gpt-3.5-turbo': { input: 0.0005, output: 0.0015 },
  // Anthropic
  'claude-3-opus-20240229': { input: 0.015, output: 0.075 },
  'claude-3-sonnet-20240229': { input: 0.003, output: 0.015 },
  'claude-3-haiku-20240307': { input: 0.00025, output: 0.00125 },
  'claude-3-5-sonnet-20241022': { input: 0.003, output: 0.015 },
};

export function estimateCost(
  usage: { promptTokens: number; completionTokens: number },
  model: string
): number {
  const costs = MODEL_COSTS[model] ?? { input: 0.001, output: 0.002 };
  return (usage.promptTokens * costs.input + usage.completionTokens * costs.output) / 1000;
}
