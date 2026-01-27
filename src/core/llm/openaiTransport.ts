/**
 * OpenAI LLM Transport Implementation
 *
 * Single implementation used by both Oracle and OPR systems.
 */

import type {
  LLMTransport,
  LLMRequest,
  LLMResponse,
  LLMUsage,
  OpenAITransportConfig,
} from './transport';
import { estimateCost } from './transport';

export class OpenAITransport implements LLMTransport {
  private config: OpenAITransportConfig;
  private lastUsage?: LLMUsage;

  constructor(config: OpenAITransportConfig) {
    this.config = config;
  }

  async complete(request: LLMRequest): Promise<LLMResponse> {
    const url = `${this.config.baseURL ?? 'https://api.openai.com/v1'}/chat/completions`;

    const body: Record<string, unknown> = {
      model: request.model ?? this.config.model,
      messages: request.messages,
    };

    if (request.temperature !== undefined) {
      body.temperature = request.temperature;
    }

    if (request.maxTokens !== undefined) {
      body.max_tokens = request.maxTokens;
    }

    const controller = new AbortController();
    const timeoutId = setTimeout(
      () => controller.abort(),
      this.config.timeout ?? 60000
    );

    try {
      const response = await fetch(url, {
        method: 'POST',
        headers: {
          'Authorization': `Bearer ${this.config.apiKey}`,
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(body),
        signal: controller.signal,
      });

      clearTimeout(timeoutId);

      if (!response.ok) {
        const error = await response.text();
        throw new Error(`OpenAI API error ${response.status}: ${error}`);
      }

      const data = await response.json();

      // Track usage
      if (data.usage) {
        this.lastUsage = {
          promptTokens: data.usage.prompt_tokens,
          completionTokens: data.usage.completion_tokens,
          totalTokens: data.usage.total_tokens,
          estimatedCost: estimateCost(
            {
              promptTokens: data.usage.prompt_tokens,
              completionTokens: data.usage.completion_tokens,
            },
            request.model ?? this.config.model
          ),
        };
      }

      const text = data.choices?.[0]?.message?.content ?? '';
      const finishReason = data.choices?.[0]?.finish_reason;

      return {
        text,
        usage: this.lastUsage,
        model: data.model,
        finishReason,
      };
    } catch (e) {
      clearTimeout(timeoutId);
      if ((e as Error).name === 'AbortError') {
        throw new Error('OpenAI request timed out');
      }
      throw e;
    }
  }

  getModel(): string {
    return this.config.model;
  }

  getLastUsage(): LLMUsage | undefined {
    return this.lastUsage;
  }
}
