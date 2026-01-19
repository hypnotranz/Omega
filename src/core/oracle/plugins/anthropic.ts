// src/core/oracle/plugins/anthropic.ts
// Anthropic Claude Plugin - Real implementation with native tool calling

import type { OracleAdapter, OracleInit } from "../adapter";
import type { OracleSession, OracleReq, OracleResp, QExpr } from "../protocol";
import type { Val } from "../../eval/values";
import { meaning } from "../meaning";
import {
  registry,
  type OraclePlugin,
  type PluginCapabilities,
  type BasePluginConfig,
  type ConfigValidation,
  type HealthCheckResult,
} from "./registry";

// ═══════════════════════════════════════════════════════════════════════════
// ANTHROPIC API TYPES
// ═══════════════════════════════════════════════════════════════════════════

type AnthropicMessage = {
  role: "user" | "assistant";
  content: AnthropicContent[];
};

type AnthropicContent =
  | { type: "text"; text: string }
  | { type: "tool_use"; id: string; name: string; input: Record<string, unknown> }
  | { type: "tool_result"; tool_use_id: string; content: string };

type AnthropicTool = {
  name: string;
  description: string;
  input_schema: {
    type: "object";
    properties: Record<string, { type: string; description?: string; enum?: string[] }>;
    required?: string[];
  };
};

type AnthropicRequest = {
  model: string;
  max_tokens: number;
  system?: string;
  tools?: AnthropicTool[];
  messages: AnthropicMessage[];
};

type AnthropicResponse = {
  id: string;
  type: "message";
  role: "assistant";
  content: AnthropicContent[];
  model: string;
  stop_reason: "end_turn" | "tool_use" | "max_tokens" | "stop_sequence";
  usage: { input_tokens: number; output_tokens: number };
};

// ═══════════════════════════════════════════════════════════════════════════
// ORACLE TOOLS
// ═══════════════════════════════════════════════════════════════════════════

const ORACLE_TOOLS: AnthropicTool[] = [
  {
    name: "omega_eval",
    description: "Evaluate a Lisp expression in the Omega runtime. Returns the result value.",
    input_schema: {
      type: "object",
      properties: {
        expr: { type: "string", description: "Lisp expression to evaluate, e.g. '(+ 1 2)' or '(define x 42)'" },
      },
      required: ["expr"],
    },
  },
  {
    name: "omega_apply",
    description: "Apply a function to arguments in the runtime.",
    input_schema: {
      type: "object",
      properties: {
        fn: { type: "string", description: "Function name or lambda expression" },
        args: { type: "string", description: "Arguments as a Lisp list, e.g. '(1 2 3)'" },
      },
      required: ["fn", "args"],
    },
  },
  {
    name: "omega_observe",
    description: "Observe runtime state for debugging or analysis.",
    input_schema: {
      type: "object",
      properties: {
        what: {
          type: "string",
          enum: ["stack", "control", "handlers", "store", "env", "env_lookup", "defs"],
          description: "What to observe: stack (call stack), control (current expression), handlers (effect handlers), store (memory), env (list all bindings), env_lookup (lookup specific binding by name), defs (list user definitions)",
        },
        name: {
          type: "string",
          description: "For env_lookup: the name of the binding to look up",
        },
      },
      required: ["what"],
    },
  },
  {
    name: "omega_match",
    description: "Pattern match an expression against a pattern with ?x binders.",
    input_schema: {
      type: "object",
      properties: {
        expr: { type: "string", description: "Expression to match" },
        pattern: { type: "string", description: "Pattern with ?x binders, e.g. '(cons ?head ?tail)'" },
      },
      required: ["expr", "pattern"],
    },
  },
  {
    name: "omega_return",
    description: "Return a final result and end the Oracle session. Call this when you have computed your answer.",
    input_schema: {
      type: "object",
      properties: {
        value: { type: "string", description: "The result value as a Lisp literal, e.g. '42' or '(list \"a\" \"b\")'" },
        confidence: { type: "string", description: "Confidence 0-1 as a decimal string, e.g. '0.95'" },
        explanation: { type: "string", description: "Brief explanation of the result (for trace)" },
      },
      required: ["value"],
    },
  },
];

// ═══════════════════════════════════════════════════════════════════════════
// VAL CONVERSION HELPERS
// ═══════════════════════════════════════════════════════════════════════════

function valToString(v: Val): string {
  switch (v.tag) {
    case "Str": return JSON.stringify(v.s);
    case "Num": return String(v.n);
    case "Bool": return v.b ? "#t" : "#f";
    case "Unit": return "()";
    case "Sym": return v.name;
    case "Vector": return `[${v.items.map(valToString).join(" ")}]`;
    case "Map": {
      const pairs = v.entries.map(([k, val]) => `${valToString(k)} ${valToString(val)}`);
      return `{${pairs.join(" ")}}`;
    }
    case "Pair": return `(${valToString(v.car)} . ${valToString(v.cdr)})`;
    case "Meaning": return `(meaning ${valToString(v.denotation ?? { tag: "Unit" })} :confidence ${v.confidence ?? 0})`;
    default:
      return JSON.stringify(v);
  }
}

function valToJSON(v: Val): unknown {
  switch (v.tag) {
    case "Str": return v.s;
    case "Num": return v.n;
    case "Bool": return v.b;
    case "Unit": return null;
    case "Sym": return v.name;
    case "Vector": return v.items.map(valToJSON);
    case "Map": {
      const obj: Record<string, unknown> = {};
      for (const [k, val] of v.entries) {
        const key = k.tag === "Str" ? k.s : k.tag === "Sym" ? k.name : JSON.stringify(k);
        obj[key] = valToJSON(val);
      }
      return obj;
    }
    default:
      return valToString(v);
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// ANTHROPIC ADAPTER
// ═══════════════════════════════════════════════════════════════════════════

class AnthropicOracleAdapter implements OracleAdapter {
  private apiKey: string;
  private model: string;
  private maxTokens: number;
  private systemPrompt: string;

  constructor(config: BasePluginConfig) {
    this.apiKey = config.apiKey || process.env.ANTHROPIC_API_KEY || "";
    this.model = config.model || "claude-sonnet-4-20250514";
    this.maxTokens = config.maxTokens || 4096;
    this.systemPrompt = config.systemPrompt || DEFAULT_SYSTEM_PROMPT;
  }

  startSession(init: OracleInit): OracleSession {
    const adapter = this;
    const envRef = init.envRef;
    const stateRef = init.stateRef;

    return (async function* (): OracleSession {
      // Build initial user message
      const userContent = init.tag === "Infer"
        ? `Please compute a result for the following request:\n\n${valToString(init.payload)}\n\nUse the omega_* tools to interact with the runtime, then call omega_return when done.`
        : `Apply the procedure ${valToString(init.proc)} to arguments ${init.args.map(valToString).join(" ")}.\n\nUse the omega_* tools, then call omega_return when done.`;

      const messages: AnthropicMessage[] = [
        { role: "user", content: [{ type: "text", text: userContent }] },
      ];

      // Conversation loop
      let turnCount = 0;
      const maxTurns = 20; // Safety limit

      while (turnCount < maxTurns) {
        turnCount++;

        // Call Anthropic API
        const response = await adapter.callAPI(messages);

        // Process response content
        const assistantContent = response.content;
        messages.push({ role: "assistant", content: assistantContent });

        // Check for tool use
        const toolUses = assistantContent.filter(c => c.type === "tool_use");

        if (toolUses.length === 0) {
          // No tool use - check if there's text to interpret as final answer
          const textContent = assistantContent.find(c => c.type === "text");
          if (textContent && textContent.type === "text") {
            return meaning({
              denotation: { tag: "Str", s: textContent.text },
              confidence: 0.5,
              trace: { tag: "Str", s: `anthropic:${adapter.model}:no-tool-return` },
            });
          }
          // Empty response
          return meaning({
            denotation: { tag: "Unit" },
            confidence: 0,
            trace: { tag: "Str", s: "empty response from LLM" },
          });
        }

        // Process each tool use
        const toolResults: AnthropicContent[] = [];

        for (const toolUse of toolUses) {
          if (toolUse.type !== "tool_use") continue;

          const { id, name, input } = toolUse;

          // Handle omega_return specially - it ends the session
          if (name === "omega_return") {
            const valueStr = String(input.value || "()");
            const confidenceStr = String(input.confidence || "0.8");
            const explanation = String(input.explanation || "");

            return meaning({
              denotation: { tag: "Str", s: valueStr }, // Will be parsed by caller
              confidence: parseFloat(confidenceStr) || 0.8,
              trace: { tag: "Str", s: explanation || `anthropic:${adapter.model}` },
            });
          }

          // Handle other tools by yielding OracleReq
          let req: OracleReq;
          switch (name) {
            case "omega_eval":
              req = {
                tag: "ReqEval",
                qexpr: String(input.expr) as unknown as QExpr,
                envRef,
              };
              break;

            case "omega_apply":
              // Parse fn and args - simplified for now
              req = {
                tag: "ReqEval",
                qexpr: `(${input.fn} ${input.args || ""})` as unknown as QExpr,
                envRef,
              };
              break;

            case "omega_observe":
              const whatStr = String(input.what);
              let observeSpec: import("../protocol").ObserveSpec;
              switch (whatStr) {
                case "stack": observeSpec = { tag: "Stack", limit: 20 }; break;
                case "handlers": observeSpec = { tag: "Handlers" }; break;
                case "store": observeSpec = { tag: "StoreSummary" }; break;
                case "env": observeSpec = { tag: "Env" }; break;
                case "env_lookup": observeSpec = { tag: "EnvLookup", name: String(input.name || "") }; break;
                case "defs": observeSpec = { tag: "Defs" }; break;
                default: observeSpec = { tag: "Control" };
              }
              req = {
                tag: "ReqObserve",
                what: observeSpec,
                stateRef,
              };
              break;

            case "omega_match":
              req = {
                tag: "ReqMatch",
                qexpr: String(input.expr) as unknown as QExpr,
                pattern: String(input.pattern),
                envRef,
              };
              break;

            default:
              // Unknown tool - return error
              toolResults.push({
                type: "tool_result",
                tool_use_id: id,
                content: `Error: Unknown tool '${name}'`,
              });
              continue;
          }

          // Yield the request and get response
          const resp: OracleResp = yield req;

          // Convert response to tool result
          let resultContent: string;
          switch (resp.tag) {
            case "RespVal":
              resultContent = JSON.stringify(valToJSON(resp.value));
              break;
            case "RespObs":
              resultContent = JSON.stringify(resp.data);
              break;
            case "RespError":
              resultContent = `Error: ${resp.message}`;
              break;
            case "RespAck":
              resultContent = "OK";
              break;
            default:
              resultContent = JSON.stringify(resp);
          }

          toolResults.push({
            type: "tool_result",
            tool_use_id: id,
            content: resultContent,
          });
        }

        // Add tool results to conversation
        messages.push({ role: "user", content: toolResults });
      }

      // Max turns exceeded
      return meaning({
        denotation: { tag: "Unit" },
        confidence: 0,
        trace: { tag: "Str", s: `max turns (${maxTurns}) exceeded` },
      });
    })();
  }

  private async callAPI(messages: AnthropicMessage[]): Promise<AnthropicResponse> {
    const request: AnthropicRequest = {
      model: this.model,
      max_tokens: this.maxTokens,
      system: this.systemPrompt,
      tools: ORACLE_TOOLS,
      messages,
    };

    const response = await fetch("https://api.anthropic.com/v1/messages", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        "x-api-key": this.apiKey,
        "anthropic-version": "2023-06-01",
      },
      body: JSON.stringify(request),
    });

    if (!response.ok) {
      const errorText = await response.text();
      throw new Error(`Anthropic API error: ${response.status} ${errorText}`);
    }

    return await response.json() as AnthropicResponse;
  }
}

const DEFAULT_SYSTEM_PROMPT = `You are an Oracle for the Omega Lisp runtime. Your job is to compute results for inference requests.

You have access to the Omega runtime through tools:
- omega_eval: Evaluate Lisp expressions
- omega_apply: Apply functions to arguments
- omega_observe: Observe runtime state (stack, control, handlers, store)
- omega_match: Pattern match expressions
- omega_return: Return your final answer (MUST call this when done)

When given a task:
1. Think about what computations or lookups you need
2. Use omega_eval to run Lisp code as needed
3. Use omega_observe if you need to understand the current state
4. When you have your answer, call omega_return with the result

You can use your world knowledge for factual questions, but express your confidence level.
For computations, always verify with omega_eval when possible.

IMPORTANT: You MUST call omega_return when you have your final answer.`;

// ═══════════════════════════════════════════════════════════════════════════
// PLUGIN DEFINITION
// ═══════════════════════════════════════════════════════════════════════════

const anthropicPlugin: OraclePlugin = {
  id: "anthropic",
  name: "Anthropic Claude",

  capabilities: {
    oracleProtocol: {
      reqEval: true,
      reqApply: true,
      reqObserve: true,
      reqMatch: true,
      reqAssert: true,
    },
    tooling: {
      native: true,
      format: "anthropic",
      maxTools: 64,
    },
    mcp: {
      client: true,  // Claude can use MCP servers
      server: false,
    },
    session: {
      multiTurn: true,
      streaming: true,
      maxContext: 200_000,
    },
    io: {
      vision: true,
      audio: false,
      structuredOutput: true,
    },
  },

  supportedModels: [
    "claude-sonnet-4-20250514",
    "claude-opus-4-20250514",
    "claude-3-5-sonnet",
    "claude-3-5-haiku",
    "claude-3-opus",
    "claude-3-sonnet",
    "claude-3-haiku",
    "claude-*", // Wildcard for future models
  ],

  defaultModel: "claude-sonnet-4-20250514",

  validateConfig(config: BasePluginConfig): ConfigValidation {
    const apiKey = config.apiKey || process.env.ANTHROPIC_API_KEY;
    if (!apiKey) {
      return {
        valid: false,
        errors: ["Missing API key: set ANTHROPIC_API_KEY environment variable or pass apiKey in config"],
      };
    }
    return { valid: true };
  },

  createAdapter(config: BasePluginConfig): OracleAdapter {
    return new AnthropicOracleAdapter({
      ...config,
      model: config.model || this.defaultModel,
    });
  },

  async healthCheck(): Promise<HealthCheckResult> {
    const apiKey = process.env.ANTHROPIC_API_KEY;
    if (!apiKey) {
      return { ok: false, message: "ANTHROPIC_API_KEY not set" };
    }

    const start = Date.now();
    try {
      // Simple API call to check connectivity
      const response = await fetch("https://api.anthropic.com/v1/messages", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
          "x-api-key": apiKey,
          "anthropic-version": "2023-06-01",
        },
        body: JSON.stringify({
          model: "claude-3-haiku-20240307",
          max_tokens: 10,
          messages: [{ role: "user", content: "ping" }],
        }),
      });

      const latencyMs = Date.now() - start;

      if (response.ok) {
        return { ok: true, message: "API reachable", latencyMs };
      } else if (response.status === 401) {
        return { ok: false, message: "Invalid API key" };
      } else {
        return { ok: false, message: `API error: ${response.status}` };
      }
    } catch (err) {
      return {
        ok: false,
        message: `Connection failed: ${err instanceof Error ? err.message : String(err)}`,
      };
    }
  },
};

// ═══════════════════════════════════════════════════════════════════════════
// AUTO-REGISTER
// ═══════════════════════════════════════════════════════════════════════════

registry.register(anthropicPlugin);

export { anthropicPlugin, AnthropicOracleAdapter };
