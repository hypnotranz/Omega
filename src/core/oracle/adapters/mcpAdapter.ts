// src/core/oracle/adapters/mcpAdapter.ts
// MCP (Model Context Protocol) adapter
//
// MCP allows the LLM to access external tools/resources through a standardized protocol.
// This adapter exposes the Omega runtime as an MCP server that LLMs can call into.

import type { OracleAdapter, OracleInit } from "../adapter";
import type { OracleSession, OracleReq, OracleResp, Meaning, QExpr } from "../protocol";
import type { AdapterCaps, OracleAdapterWithCaps } from "./types";
import { meaning } from "../meaning";

/**
 * MCP Tool definition (subset of MCP spec)
 */
export type MCPTool = {
  name: string;
  description: string;
  inputSchema: {
    type: "object";
    properties: Record<string, { type: string; description?: string }>;
    required?: string[];
  };
};

/**
 * MCP Resource definition
 */
export type MCPResource = {
  uri: string;
  name: string;
  description?: string;
  mimeType?: string;
};

/**
 * MCP Server interface - what we expose TO the LLM
 */
export interface MCPServer {
  /** List available tools */
  listTools(): MCPTool[];

  /** Execute a tool */
  callTool(name: string, args: Record<string, unknown>): Promise<unknown>;

  /** List available resources */
  listResources(): MCPResource[];

  /** Read a resource */
  readResource(uri: string): Promise<{ contents: unknown; mimeType: string }>;
}

/**
 * Omega runtime as MCP server
 * Exposes eval/apply/observe as MCP tools
 */
export class OmegaMCPServer implements MCPServer {
  constructor(
    private portal: { perform(req: OracleReq): Promise<OracleResp> },
    private envRef: string,
    private stateRef: string
  ) {}

  listTools(): MCPTool[] {
    return [
      {
        name: "omega_eval",
        description: "Evaluate a Lisp expression in the Omega runtime",
        inputSchema: {
          type: "object",
          properties: {
            expr: { type: "string", description: "Lisp expression" },
          },
          required: ["expr"],
        },
      },
      {
        name: "omega_observe",
        description: "Observe runtime state",
        inputSchema: {
          type: "object",
          properties: {
            what: { type: "string", description: "stack|control|handlers|store" },
          },
          required: ["what"],
        },
      },
      {
        name: "omega_match",
        description: "Pattern match on AST",
        inputSchema: {
          type: "object",
          properties: {
            expr: { type: "string", description: "Expression to match" },
            pattern: { type: "string", description: "Pattern with ?binders" },
          },
          required: ["expr", "pattern"],
        },
      },
    ];
  }

  async callTool(name: string, args: Record<string, unknown>): Promise<unknown> {
    switch (name) {
      case "omega_eval": {
        const resp = await this.portal.perform({
          tag: "ReqEval",
          qexpr: args.expr as string,
          envRef: this.envRef,
        });
        if (resp.tag === "RespVal") return resp.value;
        if (resp.tag === "RespError") throw new Error(resp.message);
        return resp;
      }

      case "omega_observe": {
        const whatMap: Record<string, any> = {
          stack: { tag: "Stack", limit: 20 },
          control: { tag: "Control" },
          handlers: { tag: "Handlers" },
          store: { tag: "StoreSummary" },
        };
        const resp = await this.portal.perform({
          tag: "ReqObserve",
          what: whatMap[args.what as string] ?? { tag: "Control" },
          stateRef: this.stateRef,
        });
        if (resp.tag === "RespObs") return resp.data;
        return resp;
      }

      case "omega_match": {
        const resp = await this.portal.perform({
          tag: "ReqMatch",
          qexpr: args.expr as string,
          pattern: args.pattern as string,
          envRef: this.envRef,
        });
        if (resp.tag === "RespObs") return resp.data;
        return resp;
      }

      default:
        throw new Error(`Unknown tool: ${name}`);
    }
  }

  listResources(): MCPResource[] {
    return [
      {
        uri: "omega://env",
        name: "Current Environment",
        description: "The current variable bindings",
        mimeType: "application/json",
      },
      {
        uri: "omega://state",
        name: "Machine State",
        description: "The current CEKS machine state",
        mimeType: "application/json",
      },
    ];
  }

  async readResource(uri: string): Promise<{ contents: unknown; mimeType: string }> {
    if (uri === "omega://env") {
      // Would need to serialize env properly
      return { contents: { envRef: this.envRef }, mimeType: "application/json" };
    }
    if (uri === "omega://state") {
      return { contents: { stateRef: this.stateRef }, mimeType: "application/json" };
    }
    throw new Error(`Unknown resource: ${uri}`);
  }
}

/**
 * MCP Client adapter - calls an external MCP server
 * This is for when the Oracle IS an MCP client talking to some service
 */
export class MCPClientAdapter implements OracleAdapterWithCaps {
  constructor(
    private serverUrl: string,
    private authToken?: string
  ) {}

  capabilities(): AdapterCaps {
    return {
      multiTurn: true,
      toolCalling: true,
      mcp: true,
      streaming: false,    // MCP doesn't stream by default
      vision: false,
      maxContext: Infinity, // MCP doesn't have context limits
    };
  }

  startSession(init: OracleInit): OracleSession {
    const serverUrl = this.serverUrl;

    return (async function* (): OracleSession {
      // STUB: In production, this would:
      // 1. Connect to MCP server at serverUrl
      // 2. List available tools
      // 3. Decide which tools to call based on init.payload
      // 4. Call tools and process results
      // 5. Return final Meaning

      // For now, just return a placeholder
      return meaning({
        denotation: { tag: "Str", s: "MCP adapter stub" },
        confidence: 0,
        trace: { tag: "Str", s: `adapter=mcp serverUrl=${serverUrl}` },
      });
    })();
  }
}

/**
 * Bidirectional MCP adapter - Omega acts as BOTH client and server
 *
 * This is the powerful pattern:
 * - LLM calls Omega tools via MCP
 * - Omega can call LLM tools via MCP
 * - Enables tool composition and orchestration
 */
export class BidirectionalMCPAdapter implements OracleAdapterWithCaps {
  constructor(
    private mcpServerUrl: string,  // Where to call OUT
    private listenPort: number,     // Where we listen for calls IN
  ) {}

  capabilities(): AdapterCaps {
    return {
      multiTurn: true,
      toolCalling: true,
      mcp: true,
      streaming: true,
      vision: false,
      maxContext: Infinity,
    };
  }

  startSession(init: OracleInit): OracleSession {
    // STUB: Would set up bidirectional MCP connection
    return (async function* (): OracleSession {
      return meaning({
        denotation: { tag: "Str", s: "bidirectional MCP stub" },
        confidence: 0,
      });
    })();
  }
}
