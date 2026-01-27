/**
 * Scripted OPR Adapter
 *
 * Returns pre-defined responses in sequence. Used for testing.
 */

import type { OprLLMRequest, ScriptedAdapterConfig } from './types';
import { BaseOprAdapter } from './types';

/**
 * Adapter that returns scripted responses for testing
 */
export class ScriptedOprAdapter extends BaseOprAdapter {
  private responses: Array<string | { response: string; delay?: number }>;
  private loop: boolean;
  private index = 0;

  constructor(config: ScriptedAdapterConfig) {
    super();
    this.responses = config.responses;
    this.loop = config.loop ?? false;
  }

  async complete(request: OprLLMRequest): Promise<string> {
    if (this.index >= this.responses.length) {
      if (this.loop) {
        this.index = 0;
      } else {
        throw new Error('ScriptedOprAdapter: No more responses available');
      }
    }

    const item = this.responses[this.index++];
    const response = typeof item === 'string' ? item : item.response;
    const delay = typeof item === 'string' ? 0 : item.delay ?? 0;

    if (delay > 0) {
      await new Promise((resolve) => setTimeout(resolve, delay));
    }

    // Estimate token counts based on content length
    const promptTokens = Math.ceil(request.userContent.length / 4);
    const completionTokens = Math.ceil(response.length / 4);

    this.lastUsage = {
      promptTokens,
      completionTokens,
      totalTokens: promptTokens + completionTokens,
      estimatedCost: this.estimateCost({ promptTokens, completionTokens }, 'scripted'),
    };

    return response;
  }

  getModel(): string {
    return 'scripted';
  }

  /**
   * Reset the response index
   */
  reset(): void {
    this.index = 0;
  }

  /**
   * Get remaining response count
   */
  remainingResponses(): number {
    return Math.max(0, this.responses.length - this.index);
  }
}
