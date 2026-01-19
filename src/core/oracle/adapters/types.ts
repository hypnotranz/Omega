// src/core/oracle/adapters/types.ts
// Plugin interface for Oracle backends

import type { OracleAdapter, OracleInit } from "../adapter";
import type { OracleSession, OracleReq, OracleResp, Meaning } from "../protocol";
import { meaning } from "../meaning";

/**
 * Configuration for LLM-based Oracle adapters
 */
export type LLMConfig = {
  model: string;
  apiKey?: string;
  baseUrl?: string;
  maxTokens?: number;
  temperature?: number;
  systemPrompt?: string;
};

/**
 * Tool definition for tool-calling LLMs (OpenAI/Anthropic style)
 */
export type ToolDef = {
  name: string;
  description: string;
  parameters: Record<string, unknown>; // JSON Schema
};

/**
 * Extended adapter interface with capabilities discovery
 */
export interface OracleAdapterWithCaps extends OracleAdapter {
  /** What this adapter supports */
  capabilities(): AdapterCaps;
}

export type AdapterCaps = {
  /** Can do multi-turn conversations */
  multiTurn: boolean;
  /** Supports native tool calling (not prompt-based) */
  toolCalling: boolean;
  /** Supports MCP (Model Context Protocol) */
  mcp: boolean;
  /** Can stream responses */
  streaming: boolean;
  /** Supports vision/images */
  vision: boolean;
  /** Max context window */
  maxContext: number;
};

/**
 * Depth-tracking wrapper - prevents infinite recursion
 */
export class DepthTrackingAdapter implements OracleAdapter {
  private currentDepth = 0;

  constructor(
    private inner: OracleAdapter,
    private maxDepth: number = 8
  ) {}

  startSession(init: OracleInit): OracleSession {
    const maxDepth = this.maxDepth;
    if (this.currentDepth >= maxDepth) {
      // Return a session that immediately fails with proper Meaning
      return (async function* (): OracleSession {
        return meaning({
          denotation: { tag: "Unit" },
          confidence: 0,
          trace: { tag: "Str", s: `max nested depth ${maxDepth} exceeded` },
        });
      })();
    }

    this.currentDepth++;
    const innerSession = this.inner.startSession(init);

    // Wrap to decrement depth on completion
    const self = this;
    return (async function* (): OracleSession {
      try {
        let resp: OracleResp = { tag: "RespAck" };
        while (true) {
          const step = await innerSession.next(resp);
          if (step.done) {
            return step.value;
          }
          resp = yield step.value as OracleReq;
        }
      } finally {
        self.currentDepth--;
      }
    })();
  }
}

/**
 * Logging/tracing wrapper for debugging
 */
export class TracingAdapter implements OracleAdapter {
  constructor(
    private inner: OracleAdapter,
    private log: (msg: string, data?: unknown) => void = console.log
  ) {}

  startSession(init: OracleInit): OracleSession {
    this.log("Oracle session started", { tag: init.tag, envRef: init.envRef });
    const innerSession = this.inner.startSession(init);
    const log = this.log;

    return (async function* (): OracleSession {
      let resp: OracleResp = { tag: "RespAck" };
      let turn = 0;
      while (true) {
        const step = await innerSession.next(resp);
        if (step.done) {
          log("Oracle session completed", { result: step.value });
          return step.value;
        }
        turn++;
        log(`Oracle turn ${turn}`, { req: step.value });
        resp = yield step.value as OracleReq;
        log(`Oracle turn ${turn} response`, { resp });
      }
    })();
  }
}
