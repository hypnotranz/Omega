# 021d1-adapter-interface: OPR Adapter Interface

> **Output**: `src/core/opr/adapters/types.ts`

> **Scope**: Define OprLLMAdapter interface and base types for LLM adapters
> **Architecture Reference**: [021-OPR-RUNTIME.md](021-OPR-RUNTIME.md)
> **Depends on**: job-021-types (Layer 1)

## Overview

Common interface for LLM adapters used by OprRuntime. Adapters handle the actual LLM API calls.

## File to Create

`src/core/opr/adapters/types.ts`

## Imports Contract

What this task needs from its dependencies:

### From 021-prompt (../../../frameir/prompt):
- PromptDoc

## Implementation

```typescript
import type { PromptDoc } from '../../../frameir/prompt';

/**
 * Usage information from an LLM call
 */
export interface LLMUsage {
  promptTokens: number;
  completionTokens: number;
  totalTokens: number;
  estimatedCost: number;  // in USD
}

/**
 * Request to send to LLM
 */
export interface OprLLMRequest {
  /** Kernel ID for context */
  kernelId: string;

  /** Compiled prompt (system + few-shot examples) */
  prompt: PromptDoc;

  /** User content (program, state, etc.) */
  userContent: string;

  /** Optional repair context from previous failed attempt */
  repairContext?: string;

  /** Max tokens for response */
  maxTokens?: number;

  /** Temperature (0-1) */
  temperature?: number;
}

/**
 * Interface for LLM adapters
 */
export interface OprLLMAdapter {
  /**
   * Send a completion request to the LLM
   * @returns The raw text response from the LLM
   */
  complete(request: OprLLMRequest): Promise<string>;

  /**
   * Get usage information from the last call (optional)
   */
  getLastUsage?(): LLMUsage;

  /**
   * Get the model identifier
   */
  getModel(): string;

  /**
   * Check if the adapter supports streaming
   */
  supportsStreaming(): boolean;
}

/**
 * Base configuration for all adapters
 */
export interface OprAdapterConfig {
  /** Model to use */
  model: string;

  /** Max tokens for response (default: 2000) */
  maxTokens?: number;

  /** Temperature (default: 0) */
  temperature?: number;

  /** Request timeout in ms (default: 60000) */
  timeout?: number;
}

/**
 * OpenAI-specific configuration
 */
export interface OpenAIAdapterConfig extends OprAdapterConfig {
  apiKey: string;
  baseURL?: string;
  organization?: string;
}

/**
 * Anthropic-specific configuration
 */
export interface AnthropicAdapterConfig extends OprAdapterConfig {
  apiKey: string;
  baseURL?: string;
}

/**
 * Scripted adapter configuration (for testing)
 */
export interface ScriptedAdapterConfig {
  /** Responses to return in sequence */
  responses: Array<string | { response: string; delay?: number }>;

  /** Whether to loop responses or throw on exhaustion */
  loop?: boolean;
}
```

## Abstract Base Adapter

```typescript
/**
 * Abstract base class for LLM adapters
 * Provides common functionality like prompt compilation
 */
export abstract class BaseOprAdapter implements OprLLMAdapter {
  protected lastUsage: LLMUsage | null = null;

  abstract complete(request: OprLLMRequest): Promise<string>;
  abstract getModel(): string;

  getLastUsage(): LLMUsage {
    if (!this.lastUsage) {
      return {
        promptTokens: 0,
        completionTokens: 0,
        totalTokens: 0,
        estimatedCost: 0,
      };
    }
    return this.lastUsage;
  }

  supportsStreaming(): boolean {
    return false;
  }

  /**
   * Format the user message content
   */
  protected formatUserContent(request: OprLLMRequest): string {
    let content = request.userContent;

    if (request.repairContext) {
      content = `${request.repairContext}\n\n---\n\nORIGINAL REQUEST:\n${content}`;
    }

    return content;
  }

  /**
   * Estimate cost based on token usage and model
   */
  protected estimateCost(usage: { promptTokens: number; completionTokens: number }, model: string): number {
    // Rough estimates per 1M tokens
    const pricing: Record<string, { prompt: number; completion: number }> = {
      'gpt-4o': { prompt: 2.50, completion: 10.00 },
      'gpt-4o-mini': { prompt: 0.15, completion: 0.60 },
      'gpt-4-turbo': { prompt: 10.00, completion: 30.00 },
      'claude-3-opus': { prompt: 15.00, completion: 75.00 },
      'claude-3-sonnet': { prompt: 3.00, completion: 15.00 },
      'claude-3-haiku': { prompt: 0.25, completion: 1.25 },
    };

    const prices = pricing[model] ?? { prompt: 1.00, completion: 3.00 };

    return (
      (usage.promptTokens / 1_000_000) * prices.prompt +
      (usage.completionTokens / 1_000_000) * prices.completion
    );
  }
}
```

## Exports Contract
```typescript
export {
  type OprLLMAdapter,
  type OprLLMRequest,
  type LLMUsage,
  type OprAdapterConfig,
  type OpenAIAdapterConfig,
  type AnthropicAdapterConfig,
  type ScriptedAdapterConfig,
  BaseOprAdapter,
};
```

## Index File

Create `src/core/opr/adapters/index.ts`:

```typescript
export * from './types';
export { ScriptedOprAdapter } from './scripted';
export { OpenAIOprAdapter } from './openai';
export { AnthropicOprAdapter } from './anthropic';
```

## Acceptance Criteria

1. [ ] `OprLLMAdapter` interface defined with `complete()` method
2. [ ] `OprLLMRequest` includes prompt, userContent, repairContext
3. [ ] `LLMUsage` tracks tokens and estimated cost
4. [ ] `BaseOprAdapter` provides common functionality
5. [ ] Config types defined for OpenAI, Anthropic, Scripted
6. [ ] Cost estimation implemented for common models

## Test Cases

- AI1: BaseOprAdapter.formatUserContent includes repair context
- AI2: estimateCost calculates reasonable values
- AI3: getLastUsage returns zeros when no calls made

## Verification

```bash
npx tsc --noEmit src/core/opr/adapters/types.ts
```
