import type { ExecContext } from "./types";

/**
 * Tool call request.
 */
export interface ToolCall {
  /** Tool name */
  name: string;

  /** Arguments (validated against contract.inputSchema before call) */
  args: unknown;

  /** Contract ID for audited execution */
  contractId: string;
}

/**
 * Tool call result.
 */
export interface ToolResult {
  /** Whether call succeeded */
  ok: boolean;

  /** Result value (validated against contract.outputSchema) */
  value?: unknown;

  /** Error info if failed */
  error?: {
    type: string;
    message: string;
    data?: unknown;
  };

  /** Provider-specific raw result */
  raw?: unknown;
}

/**
 * Tool port interface.
 * All external tool invocation must go through this port.
 */
export interface ToolPort {
  /**
   * Call an external tool.
   * @param call - The tool call request
   * @param ctx - Execution context
   * @returns Tool result
   */
  call(call: ToolCall, ctx: ExecContext): Promise<ToolResult>;
}

/**
 * Validate tool call against capabilities.
 */
export function validateToolCap(call: ToolCall, ctx: ExecContext): void {
  const cap = ctx.caps.toolCap;
  if (!cap) {
    throw new Error("No ToolCap in context");
  }
  if (!cap.allowedContracts.has(call.contractId)) {
    throw new Error(`Contract ${call.contractId} not allowed by capability`);
  }
}
