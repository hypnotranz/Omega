// src/core/oracle/plugins/ollama.ts
// Ollama Plugin - Local models with optional tool calling

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
// OLLAMA API TYPES
// ═══════════════════════════════════════════════════════════════════════════

type OllamaMessage = {
  role: "system" | "user" | "assistant" | "tool";
  content: string;
  tool_calls?: OllamaToolCall[];
};

type OllamaToolCall = {
  function: {
    name: string;
    arguments: Record<string, unknown>;
  };
};

type OllamaTool = {
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

type OllamaRequest = {
  model: string;
  messages: OllamaMessage[];
  stream: false;
  tools?: OllamaTool[];
  options?: {
    temperature?: number;
    num_predict?: number;
  };
};

type OllamaResponse = {
  model: string;
  message: OllamaMessage;
  done: boolean;
  total_duration?: number;
  load_duration?: number;
  prompt_eval_count?: number;
  eval_count?: number;
};

// Models known to support tool calling
const TOOL_CAPABLE_MODELS = [
  "llama3.1",
  "llama3.2",
  "llama3.3",
  "mistral",
  "mixtral",
  "qwen2",
  "qwen2.5",
  "command-r",
  "firefunction",
];

function modelSupportsTools(model: string): boolean {
  return TOOL_CAPABLE_MODELS.some(m => model.toLowerCase().includes(m));
}

// ═══════════════════════════════════════════════════════════════════════════
// ORACLE TOOLS (Ollama format - same as OpenAI)
// ═══════════════════════════════════════════════════════════════════════════

const ORACLE_TOOLS: OllamaTool[] = [
  {
    type: "function",
    function: {
      name: "omega_eval",
      description: "Evaluate a Lisp expression in the Omega runtime. Returns the result value.",
      parameters: {
        type: "object",
        properties: {
          expr: { type: "string", description: "Lisp expression to evaluate" },
        },
        required: ["expr"],
      },
    },
  },
  {
    type: "function",
    function: {
      name: "omega_return",
      description: "Return a final result and end the session.",
      parameters: {
        type: "object",
        properties: {
          value: { type: "string", description: "The result value as a Lisp literal" },
          confidence: { type: "string", description: "Confidence 0-1" },
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
// TEXT-BASED PROTOCOL (fallback for models without tool support)
// ═══════════════════════════════════════════════════════════════════════════

const TEXT_PROTOCOL_SYSTEM = `You are an Oracle for the Omega Lisp runtime.

When you need to evaluate Lisp code, use this format:
EVAL: (expression here)

When you have your final answer, use this format:
RETURN: value | confidence

Example:
User: What is 2 + 2?
Assistant: Let me evaluate that.
EVAL: (+ 2 2)
[System returns: 4]
RETURN: 4 | 0.99

Important:
- Use EVAL: to run Lisp code
- Use RETURN: to give your final answer with confidence (0-1)
- You can use multiple EVAL: calls before RETURN:`;

function parseTextProtocol(text: string): { type: "eval"; expr: string } | { type: "return"; value: string; confidence: number } | null {
  // Check for RETURN: pattern
  const returnMatch = text.match(/RETURN:\s*(.+?)\s*\|\s*([\d.]+)/);
  if (returnMatch) {
    return {
      type: "return",
      value: returnMatch[1].trim(),
      confidence: parseFloat(returnMatch[2]) || 0.5,
    };
  }

  // Check for EVAL: pattern
  const evalMatch = text.match(/EVAL:\s*(.+?)(?:\n|$)/);
  if (evalMatch) {
    return {
      type: "eval",
      expr: evalMatch[1].trim(),
    };
  }

  return null;
}

// ═══════════════════════════════════════════════════════════════════════════
// OLLAMA ADAPTER
// ═══════════════════════════════════════════════════════════════════════════

class OllamaOracleAdapter implements OracleAdapter {
  private model: string;
  private baseUrl: string;
  private useTools: boolean;
  private maxTokens: number;
  private temperature: number;

  constructor(config: BasePluginConfig) {
    this.model = config.model || "llama3.1";
    this.baseUrl = config.baseUrl || "http://localhost:11434";
    this.useTools = modelSupportsTools(this.model);
    this.maxTokens = config.maxTokens || 2048;
    this.temperature = config.temperature || 0.7;
  }

  startSession(init: OracleInit): OracleSession {
    if (this.useTools) {
      return this.startToolSession(init);
    } else {
      return this.startTextSession(init);
    }
  }

  // Tool-calling session (for capable models)
  private startToolSession(init: OracleInit): OracleSession {
    const adapter = this;
    const envRef = init.envRef;

    return (async function* (): OracleSession {
      const userContent = init.tag === "Infer"
        ? `Compute a result for: ${valToString(init.payload)}\n\nUse omega_eval to run Lisp code, then call omega_return with your answer.`
        : `Apply ${valToString(init.proc)} to ${init.args.map(valToString).join(" ")}`;

      const messages: OllamaMessage[] = [
        { role: "system", content: "You are an Oracle for a Lisp runtime. Use the tools to evaluate expressions and return results." },
        { role: "user", content: userContent },
      ];

      let turnCount = 0;
      const maxTurns = 15;

      while (turnCount < maxTurns) {
        turnCount++;

        const response = await adapter.callAPI(messages, true);
        const msg = response.message;
        messages.push(msg);

        const toolCalls = msg.tool_calls || [];

        if (toolCalls.length === 0) {
          // No tool calls - parse content as answer
          return meaning({
            denotation: { tag: "Str", s: msg.content || "" },
            confidence: 0.5,
            trace: { tag: "Str", s: `ollama:${adapter.model}:text-only` },
          });
        }

        for (const toolCall of toolCalls) {
          const { name, arguments: args } = toolCall.function;

          if (name === "omega_return") {
            return meaning({
              denotation: { tag: "Str", s: String(args.value || "") },
              confidence: parseFloat(String(args.confidence || "0.7")) || 0.7,
              trace: { tag: "Str", s: `ollama:${adapter.model}` },
            });
          }

          if (name === "omega_eval") {
            const req: OracleReq = {
              tag: "ReqEval",
              qexpr: String(args.expr) as unknown as QExpr,
              envRef,
            };

            const resp: OracleResp = yield req;

            let result: string;
            if (resp.tag === "RespVal") {
              result = JSON.stringify(valToJSON(resp.value));
            } else if (resp.tag === "RespError") {
              result = `Error: ${resp.message}`;
            } else {
              result = JSON.stringify(resp);
            }

            messages.push({
              role: "tool",
              content: result,
            });
          }
        }
      }

      return meaning({
        denotation: { tag: "Unit" },
        confidence: 0,
        trace: { tag: "Str", s: "max turns exceeded" },
      });
    })();
  }

  // Text-based session (for models without tool support)
  private startTextSession(init: OracleInit): OracleSession {
    const adapter = this;
    const envRef = init.envRef;

    return (async function* (): OracleSession {
      const userContent = init.tag === "Infer"
        ? `Compute a result for: ${valToString(init.payload)}`
        : `Apply ${valToString(init.proc)} to ${init.args.map(valToString).join(" ")}`;

      const messages: OllamaMessage[] = [
        { role: "system", content: TEXT_PROTOCOL_SYSTEM },
        { role: "user", content: userContent },
      ];

      let turnCount = 0;
      const maxTurns = 15;

      while (turnCount < maxTurns) {
        turnCount++;

        const response = await adapter.callAPI(messages, false);
        const content = response.message.content;
        messages.push({ role: "assistant", content });

        const parsed = parseTextProtocol(content);

        if (!parsed) {
          // No protocol command - treat as final answer
          return meaning({
            denotation: { tag: "Str", s: content },
            confidence: 0.3,
            trace: { tag: "Str", s: `ollama:${adapter.model}:raw-text` },
          });
        }

        if (parsed.type === "return") {
          return meaning({
            denotation: { tag: "Str", s: parsed.value },
            confidence: parsed.confidence,
            trace: { tag: "Str", s: `ollama:${adapter.model}:text-protocol` },
          });
        }

        if (parsed.type === "eval") {
          const req: OracleReq = {
            tag: "ReqEval",
            qexpr: parsed.expr as unknown as QExpr,
            envRef,
          };

          const resp: OracleResp = yield req;

          let result: string;
          if (resp.tag === "RespVal") {
            result = JSON.stringify(valToJSON(resp.value));
          } else if (resp.tag === "RespError") {
            result = `Error: ${resp.message}`;
          } else {
            result = JSON.stringify(resp);
          }

          messages.push({
            role: "user",
            content: `[Eval result: ${result}]`,
          });
        }
      }

      return meaning({
        denotation: { tag: "Unit" },
        confidence: 0,
        trace: { tag: "Str", s: "max turns exceeded" },
      });
    })();
  }

  private async callAPI(messages: OllamaMessage[], useTools: boolean): Promise<OllamaResponse> {
    const request: OllamaRequest = {
      model: this.model,
      messages,
      stream: false,
      options: {
        temperature: this.temperature,
        num_predict: this.maxTokens,
      },
    };

    if (useTools) {
      request.tools = ORACLE_TOOLS;
    }

    const response = await fetch(`${this.baseUrl}/api/chat`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(request),
    });

    if (!response.ok) {
      const errorText = await response.text();
      throw new Error(`Ollama API error: ${response.status} ${errorText}`);
    }

    return await response.json() as OllamaResponse;
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// PLUGIN DEFINITION
// ═══════════════════════════════════════════════════════════════════════════

const ollamaPlugin: OraclePlugin = {
  id: "ollama",
  name: "Ollama Local",

  capabilities: {
    oracleProtocol: {
      reqEval: true,
      reqApply: true,
      reqObserve: false, // Not reliable via text protocol
      reqMatch: false,
      reqAssert: false,
    },
    tooling: {
      native: false, // Model-dependent, conservative default
      format: "ollama",
    },
    mcp: {
      client: false,
      server: false,
    },
    session: {
      multiTurn: true,
      streaming: true,
      maxContext: 8192, // Conservative default, model-dependent
    },
    io: {
      vision: false, // Model-dependent
      audio: false,
      structuredOutput: false,
    },
  },

  supportedModels: [
    "llama3.1",
    "llama3.2",
    "llama3.3",
    "llama2",
    "mistral",
    "mixtral",
    "codellama",
    "phi",
    "phi3",
    "qwen",
    "qwen2",
    "gemma",
    "gemma2",
    "*", // Any model name
  ],

  defaultModel: "llama3.1",

  validateConfig(config: BasePluginConfig): ConfigValidation {
    // No API key needed for local Ollama
    return { valid: true };
  },

  createAdapter(config: BasePluginConfig): OracleAdapter {
    return new OllamaOracleAdapter({
      ...config,
      model: config.model || this.defaultModel,
    });
  },

  async healthCheck(): Promise<HealthCheckResult> {
    const baseUrl = process.env.OLLAMA_HOST || "http://localhost:11434";
    const start = Date.now();

    try {
      const response = await fetch(`${baseUrl}/api/tags`);
      const latencyMs = Date.now() - start;

      if (response.ok) {
        const data = await response.json() as { models?: unknown[] };
        const modelCount = data.models?.length || 0;
        return {
          ok: true,
          message: `Ollama running with ${modelCount} models`,
          latencyMs,
        };
      } else {
        return { ok: false, message: `Ollama error: ${response.status}` };
      }
    } catch (err) {
      return {
        ok: false,
        message: `Ollama not reachable at ${baseUrl}: ${err instanceof Error ? err.message : String(err)}`,
      };
    }
  },
};

// ═══════════════════════════════════════════════════════════════════════════
// AUTO-REGISTER
// ═══════════════════════════════════════════════════════════════════════════

registry.register(ollamaPlugin);

export { ollamaPlugin, OllamaOracleAdapter, modelSupportsTools };
