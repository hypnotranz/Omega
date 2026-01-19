// src/core/oracle/plugins/openai.ts
// OpenAI Plugin - Real implementation with function calling

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
// OPENAI API TYPES
// ═══════════════════════════════════════════════════════════════════════════

type OpenAIMessage = {
  role: "system" | "user" | "assistant" | "tool";
  content: string | null;
  tool_calls?: OpenAIToolCall[];
  tool_call_id?: string;
};

type OpenAIToolCall = {
  id: string;
  type: "function";
  function: {
    name: string;
    arguments: string; // JSON string
  };
};

type OpenAITool = {
  type: "function";
  function: {
    name: string;
    description: string;
    parameters: {
      type: "object";
      properties: Record<string, { type: string; description?: string; enum?: string[] }>;
      required?: string[];
    };
  };
};

type OpenAIRequest = {
  model: string;
  max_tokens?: number;
  messages: OpenAIMessage[];
  tools?: OpenAITool[];
  tool_choice?: "auto" | "none" | { type: "function"; function: { name: string } };
};

type OpenAIResponse = {
  id: string;
  object: "chat.completion";
  choices: Array<{
    index: number;
    message: OpenAIMessage;
    finish_reason: "stop" | "tool_calls" | "length" | "content_filter";
  }>;
  usage: {
    prompt_tokens: number;
    completion_tokens: number;
    total_tokens: number;
  };
};

// ═══════════════════════════════════════════════════════════════════════════
// ORACLE TOOLS (OpenAI format)
// ═══════════════════════════════════════════════════════════════════════════

const ORACLE_TOOLS: OpenAITool[] = [
  {
    type: "function",
    function: {
      name: "omega_eval",
      description: "Evaluate a Lisp expression in the Omega runtime. Returns the result value.",
      parameters: {
        type: "object",
        properties: {
          expr: { type: "string", description: "Lisp expression to evaluate, e.g. '(+ 1 2)' or '(define x 42)'" },
        },
        required: ["expr"],
      },
    },
  },
  {
    type: "function",
    function: {
      name: "omega_apply",
      description: "Apply a function to arguments in the runtime.",
      parameters: {
        type: "object",
        properties: {
          fn: { type: "string", description: "Function name or lambda expression" },
          args: { type: "string", description: "Arguments as a Lisp list, e.g. '(1 2 3)'" },
        },
        required: ["fn", "args"],
      },
    },
  },
  {
    type: "function",
    function: {
      name: "omega_observe",
      description: "Observe runtime state for debugging or analysis.",
      parameters: {
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
  },
  {
    type: "function",
    function: {
      name: "omega_match",
      description: "Pattern match an expression against a pattern with ?x binders.",
      parameters: {
        type: "object",
        properties: {
          expr: { type: "string", description: "Expression to match" },
          pattern: { type: "string", description: "Pattern with ?x binders, e.g. '(cons ?head ?tail)'" },
        },
        required: ["expr", "pattern"],
      },
    },
  },
  {
    type: "function",
    function: {
      name: "omega_return",
      description: "Return a final result and end the Oracle session. Call this when you have computed your answer.",
      parameters: {
        type: "object",
        properties: {
          value: { type: "string", description: "The result value as a Lisp literal, e.g. '42' or '(list \"a\" \"b\")'" },
          confidence: { type: "string", description: "Confidence 0-1 as a decimal string, e.g. '0.95'" },
          explanation: { type: "string", description: "Brief explanation of the result (for trace)" },
        },
        required: ["value"],
      },
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
// OPENAI ADAPTER
// ═══════════════════════════════════════════════════════════════════════════

class OpenAIOracleAdapter implements OracleAdapter {
  private apiKey: string;
  private model: string;
  private maxTokens: number;
  private systemPrompt: string;
  private baseUrl: string;

  constructor(config: BasePluginConfig) {
    this.apiKey = config.apiKey || process.env.OPENAI_API_KEY || "";
    this.model = config.model || "gpt-4o";
    this.maxTokens = config.maxTokens || 4096;
    this.systemPrompt = config.systemPrompt || DEFAULT_SYSTEM_PROMPT;
    this.baseUrl = config.baseUrl || "https://api.openai.com/v1";
  }

  startSession(init: OracleInit): OracleSession {
    const adapter = this;
    const envRef = init.envRef;
    const stateRef = init.stateRef;

    return (async function* (): OracleSession {
      // Build initial messages
      const userContent = init.tag === "Infer"
        ? `Please compute a result for the following request:\n\n${valToString(init.payload)}\n\nUse the omega_* tools to interact with the runtime, then call omega_return when done.`
        : `Apply the procedure ${valToString(init.proc)} to arguments ${init.args.map(valToString).join(" ")}.\n\nUse the omega_* tools, then call omega_return when done.`;

      const messages: OpenAIMessage[] = [
        { role: "system", content: adapter.systemPrompt },
        { role: "user", content: userContent },
      ];

      // Conversation loop
      let turnCount = 0;
      const maxTurns = 20;

      while (turnCount < maxTurns) {
        turnCount++;

        // Call OpenAI API
        const response = await adapter.callAPI(messages);
        const choice = response.choices[0];
        const assistantMessage = choice.message;

        // Add assistant response to history
        messages.push(assistantMessage);

        // Check for tool calls
        const toolCalls = assistantMessage.tool_calls || [];

        if (toolCalls.length === 0) {
          // No tool calls - check if there's content
          if (assistantMessage.content) {
            return meaning({
              denotation: { tag: "Str", s: assistantMessage.content },
              confidence: 0.5,
              trace: { tag: "Str", s: `openai:${adapter.model}:no-tool-return` },
            });
          }
          return meaning({
            denotation: { tag: "Unit" },
            confidence: 0,
            trace: { tag: "Str", s: "empty response from LLM" },
          });
        }

        // Process each tool call
        for (const toolCall of toolCalls) {
          const { id, function: fn } = toolCall;
          const name = fn.name;
          let input: Record<string, unknown>;

          try {
            input = JSON.parse(fn.arguments);
          } catch {
            messages.push({
              role: "tool",
              tool_call_id: id,
              content: "Error: Invalid JSON in tool arguments",
            });
            continue;
          }

          // Handle omega_return - ends session
          if (name === "omega_return") {
            const valueStr = String(input.value || "()");
            const confidenceStr = String(input.confidence || "0.8");
            const explanation = String(input.explanation || "");

            return meaning({
              denotation: { tag: "Str", s: valueStr },
              confidence: parseFloat(confidenceStr) || 0.8,
              trace: { tag: "Str", s: explanation || `openai:${adapter.model}` },
            });
          }

          // Build OracleReq
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
              messages.push({
                role: "tool",
                tool_call_id: id,
                content: `Error: Unknown tool '${name}'`,
              });
              continue;
          }

          // Yield request and get response
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

          messages.push({
            role: "tool",
            tool_call_id: id,
            content: resultContent,
          });
        }
      }

      // Max turns exceeded
      return meaning({
        denotation: { tag: "Unit" },
        confidence: 0,
        trace: { tag: "Str", s: `max turns (${maxTurns}) exceeded` },
      });
    })();
  }

  private async callAPI(messages: OpenAIMessage[]): Promise<OpenAIResponse> {
    const request: OpenAIRequest = {
      model: this.model,
      max_tokens: this.maxTokens,
      messages,
      tools: ORACLE_TOOLS,
      tool_choice: "auto",
    };

    const response = await fetch(`${this.baseUrl}/chat/completions`, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        "Authorization": `Bearer ${this.apiKey}`,
      },
      body: JSON.stringify(request),
    });

    if (!response.ok) {
      const errorText = await response.text();
      throw new Error(`OpenAI API error: ${response.status} ${errorText}`);
    }

    return await response.json() as OpenAIResponse;
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

const openaiPlugin: OraclePlugin = {
  id: "openai",
  name: "OpenAI GPT",

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
      format: "openai",
      maxTools: 128,
    },
    mcp: {
      client: false,
      server: false,
    },
    session: {
      multiTurn: true,
      streaming: true,
      maxContext: 128_000, // GPT-4o context
    },
    io: {
      vision: true,
      audio: true, // GPT-4o supports audio
      structuredOutput: true,
    },
  },

  supportedModels: [
    "gpt-4o",
    "gpt-4o-mini",
    "gpt-4-turbo",
    "gpt-4-turbo-preview",
    "gpt-4",
    "gpt-3.5-turbo",
    "o1-preview",
    "o1-mini",
    "gpt-*", // Wildcard
  ],

  defaultModel: "gpt-4o",

  validateConfig(config: BasePluginConfig): ConfigValidation {
    const apiKey = config.apiKey || process.env.OPENAI_API_KEY;
    if (!apiKey) {
      return {
        valid: false,
        errors: ["Missing API key: set OPENAI_API_KEY environment variable or pass apiKey in config"],
      };
    }
    return { valid: true };
  },

  createAdapter(config: BasePluginConfig): OracleAdapter {
    return new OpenAIOracleAdapter({
      ...config,
      model: config.model || this.defaultModel,
    });
  },

  async healthCheck(): Promise<HealthCheckResult> {
    const apiKey = process.env.OPENAI_API_KEY;
    if (!apiKey) {
      return { ok: false, message: "OPENAI_API_KEY not set" };
    }

    const start = Date.now();
    try {
      const response = await fetch("https://api.openai.com/v1/models", {
        headers: {
          "Authorization": `Bearer ${apiKey}`,
        },
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

registry.register(openaiPlugin);

export { openaiPlugin, OpenAIOracleAdapter };
