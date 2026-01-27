/**
 * Transport-based OPR Adapter
 *
 * Wraps the shared LLMTransport to implement OPR's adapter interface.
 * This eliminates duplicate adapter code - both Oracle and OPR use
 * the same underlying transport.
 */

import type { OprLLMAdapter, OprLLMRequest, OprUsageInfo } from './types';
import type { LLMTransport, LLMMessage } from '../../llm/transport';

export class TransportOprAdapter implements OprLLMAdapter {
  private transport: LLMTransport;
  private lastUsage?: OprUsageInfo;

  constructor(transport: LLMTransport) {
    this.transport = transport;
  }

  async complete(request: OprLLMRequest): Promise<string> {
    const messages: LLMMessage[] = [];

    // Extract system message from FrameIR prompt
    if (request.prompt?.parts) {
      for (const part of request.prompt.parts as any[]) {
        if (part.tag === 'PSystem') {
          messages.push({ role: 'system', content: part.text });
        } else if (part.tag === 'System') {
          messages.push({ role: 'system', content: part.content });
        }
      }
    }

    // Add user content
    messages.push({ role: 'user', content: request.userContent });

    // Make request through transport
    const response = await this.transport.complete({
      messages,
      temperature: request.temperature,
      maxTokens: request.maxTokens,
      metadata: {
        kernelId: request.kernelId,
      },
    });

    // Track usage
    if (response.usage) {
      this.lastUsage = {
        promptTokens: response.usage.promptTokens,
        completionTokens: response.usage.completionTokens,
        totalTokens: response.usage.totalTokens,
        estimatedCost: response.usage.estimatedCost,
      };
    }

    return response.text;
  }

  getModel(): string {
    return this.transport.getModel();
  }

  getLastUsage(): OprUsageInfo | undefined {
    return this.lastUsage;
  }
}
