/**
 * Anthropic OPR Adapter
 *
 * Makes real calls to Anthropic API
 */

import type { OprLLMRequest, AnthropicAdapterConfig } from './types';
import { BaseOprAdapter } from './types';

export class AnthropicOprAdapter extends BaseOprAdapter {
  private config: AnthropicAdapterConfig;

  constructor(config: AnthropicAdapterConfig) {
    super();
    this.config = config;
  }

  async complete(request: OprLLMRequest): Promise<string> {
    const { apiKey, baseURL, model, maxTokens, temperature, timeout } = this.config;

    // Extract system message from prompt (supports PSystem from FrameIR)
    let system = '';
    if (request.prompt && request.prompt.parts) {
      for (const part of request.prompt.parts) {
        if ((part as any).tag === 'PSystem') {
          system += (part as any).text + '\n';
        } else if ((part as any).tag === 'System') {
          system += (part as any).content + '\n';
        }
      }
    }

    const body: Record<string, unknown> = {
      model,
      max_tokens: request.maxTokens ?? maxTokens ?? 2000,
      messages: [{ role: 'user', content: request.userContent }],
    };

    if (system) {
      body.system = system.trim();
    }

    if ((request.temperature ?? temperature) !== undefined) {
      body.temperature = request.temperature ?? temperature;
    }

    const url = `${baseURL ?? 'https://api.anthropic.com'}/v1/messages`;

    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), timeout ?? 60000);

    try {
      const response = await fetch(url, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'x-api-key': apiKey,
          'anthropic-version': '2023-06-01',
        },
        body: JSON.stringify(body),
        signal: controller.signal,
      });

      clearTimeout(timeoutId);

      if (!response.ok) {
        const error = await response.text();
        throw new Error(`Anthropic API error ${response.status}: ${error}`);
      }

      const data = await response.json();

      // Track usage
      if (data.usage) {
        this.lastUsage = {
          promptTokens: data.usage.input_tokens,
          completionTokens: data.usage.output_tokens,
          totalTokens: data.usage.input_tokens + data.usage.output_tokens,
          estimatedCost: this.estimateCost(
            {
              promptTokens: data.usage.input_tokens,
              completionTokens: data.usage.output_tokens,
            },
            model
          ),
        };
      }

      // Extract text from content blocks
      const content = data.content ?? [];
      const text = content
        .filter((c: any) => c.type === 'text')
        .map((c: any) => c.text)
        .join('');

      return text;
    } catch (e) {
      clearTimeout(timeoutId);
      if ((e as Error).name === 'AbortError') {
        throw new Error('Anthropic request timed out');
      }
      throw e;
    }
  }

  getModel(): string {
    return this.config.model;
  }
}
