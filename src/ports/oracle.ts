import type { ExecContext } from "./types";

/**
 * Request to oracle (LLM).
 */
export interface OracleRequest {
  /** Model identifier */
  model: string;

  /** Rendered prompt text */
  prompt: string;

  /** Tool specifications (for tool-augmented chat) */
  tools?: unknown[];

  /** Output schema for structured mode */
  outputSchema?: unknown;

  /** Sampling temperature */
  temperature?: number;

  /** Max tokens to generate */
  maxTokens?: number;

  /** Seed for reproducibility */
  seed?: number;

  /** Provider-specific metadata */
  metadata?: Record<string, unknown>;
}

/**
 * Response from oracle.
 */
export interface OracleResponse {
  /** Generated text */
  text: string;

  /** Token usage statistics */
  usage?: {
    promptTokens?: number;
    completionTokens?: number;
    totalTokens?: number;
  };

  /** Tool calls requested by model */
  toolCalls?: Array<{
    id: string;
    name: string;
    arguments: string;
  }>;

  /** Provider-specific raw response */
  raw?: unknown;
}

/**
 * Oracle port interface.
 */
export interface OraclePort {
  /**
   * Send inference request to oracle.
   * @throws if capability check fails or oracle error
   */
  infer(req: OracleRequest, ctx: ExecContext): Promise<OracleResponse>;
}

/**
 * Validate oracle request against capabilities.
 */
export function validateOracleCap(req: OracleRequest, ctx: ExecContext): void {
  const cap = ctx.caps.oracleCap;
  if (!cap) {
    throw new Error("No OracleCap in context");
  }
  if (!cap.allowedModels.includes(req.model)) {
    throw new Error(`Model ${req.model} not allowed by capability`);
  }
}
