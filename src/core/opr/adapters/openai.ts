/**
 * OpenAI OPR Adapter
 *
 * Makes real calls to OpenAI API
 */

import type { OprLLMRequest, OpenAIAdapterConfig } from './types';
import { BaseOprAdapter } from './types';

export class OpenAIOprAdapter extends BaseOprAdapter {
  private config: OpenAIAdapterConfig;

  constructor(config: OpenAIAdapterConfig) {
    super();
    this.config = config;
  }

  async complete(request: OprLLMRequest): Promise<string> {
    const { apiKey, baseURL, organization, model, maxTokens, temperature, timeout } = this.config;

    // Build messages from prompt
    const messages: Array<{ role: string; content: string }> = [];

    // Extract system message from prompt (supports PSystem from FrameIR)
    if (request.prompt && request.prompt.parts) {
      for (const part of request.prompt.parts) {
        if ((part as any).tag === 'PSystem') {
          messages.push({ role: 'system', content: (part as any).text });
        } else if ((part as any).tag === 'System') {
          messages.push({ role: 'system', content: (part as any).content });
        }
      }
    }

    // Add user content
    messages.push({ role: 'user', content: request.userContent });

    const body = {
      model,
      messages,
      max_tokens: request.maxTokens ?? maxTokens ?? 2000,
      temperature: request.temperature ?? temperature ?? 0,
    };

    const headers: Record<string, string> = {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${apiKey}`,
    };

    if (organization) {
      headers['OpenAI-Organization'] = organization;
    }

    const url = `${baseURL ?? 'https://api.openai.com/v1'}/chat/completions`;

    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), timeout ?? 60000);

    try {
      const response = await fetch(url, {
        method: 'POST',
        headers,
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
          estimatedCost: this.estimateCost(
            {
              promptTokens: data.usage.prompt_tokens,
              completionTokens: data.usage.completion_tokens,
            },
            model
          ),
        };
      }

      return data.choices[0]?.message?.content ?? '';
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
}
