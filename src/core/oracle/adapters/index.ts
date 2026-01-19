// src/core/oracle/adapters/index.ts
// Oracle adapter plugins

export * from "./types";
export { AnthropicAdapter, createAnthropicAdapter } from "./anthropicAdapter";
export { OpenAIAdapter, createOpenAIAdapter } from "./openaiAdapter";
export { MCPClientAdapter, OmegaMCPServer, BidirectionalMCPAdapter } from "./mcpAdapter";

/**
 * Adapter Selection Guide:
 *
 * ┌─────────────────────────────────────────────────────────────────────┐
 * │ Adapter                │ Use Case                                   │
 * ├─────────────────────────────────────────────────────────────────────┤
 * │ ScriptedOracleAdapter  │ Tests, deterministic replay                │
 * │ OpenAIAdapter          │ GPT-4o with native tool calling            │
 * │ AnthropicAdapter       │ Claude with native tool calling            │
 * │ MCPClientAdapter       │ Connect to any MCP-compatible LLM server   │
 * │ OmegaMCPServer         │ Expose Omega runtime to external LLMs      │
 * │ BidirectionalMCPAdapter│ Full orchestration with tool composition   │
 * │ DepthTrackingAdapter   │ Wrapper to prevent infinite recursion      │
 * │ TracingAdapter         │ Wrapper for debugging/logging              │
 * └─────────────────────────────────────────────────────────────────────┘
 *
 * Composition Pattern:
 *
 *   const adapter = new DepthTrackingAdapter(
 *     new TracingAdapter(
 *       createAnthropicAdapter({ model: "claude-sonnet-4-20250514" })
 *     ),
 *     maxDepth: 4
 *   );
 *
 * MCP Pattern (Omega as server):
 *
 *   // In your MCP server setup
 *   const mcpServer = new OmegaMCPServer(portal, envRef, stateRef);
 *   // Expose mcpServer.listTools(), mcpServer.callTool() to MCP protocol
 *
 * MCP Pattern (Omega as client):
 *
 *   const adapter = new MCPClientAdapter("http://localhost:3000/mcp");
 *   // Adapter calls out to MCP server for inference
 */
