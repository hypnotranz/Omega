// src/core/oracle/adapters/anthropicAdapter.ts
// Anthropic Claude adapter with native tool calling
// Supports both streaming and blocking modes

import type { OracleAdapter, OracleInit } from "../adapter";
import type { OracleSession, OracleReq, OracleResp, Meaning, QExpr, ObserveSpec } from "../protocol";
import type { Val } from "../../eval/values";
import type { LLMConfig, AdapterCaps, OracleAdapterWithCaps, ToolDef } from "./types";
import { meaning } from "../meaning";

// ═══════════════════════════════════════════════════════════════════════════
// TOOL DEFINITIONS
// ═══════════════════════════════════════════════════════════════════════════

const ORACLE_TOOLS: ToolDef[] = [
  {
    name: "eval",
    description: "Evaluate a Lisp expression in the current environment. Returns the result value.",
    parameters: {
      type: "object",
      properties: {
        expr: { type: "string", description: "Lisp expression to evaluate (e.g., '(+ 1 2)')" },
      },
      required: ["expr"],
    },
  },
  {
    name: "apply",
    description: "Apply a function to arguments",
    parameters: {
      type: "object",
      properties: {
        fn: { type: "string", description: "Function name or expression" },
        args: { type: "array", items: { type: "string" }, description: "Arguments" },
      },
      required: ["fn", "args"],
    },
  },
  {
    name: "observe",
    description: "Observe runtime state (stack, control, handlers, store)",
    parameters: {
      type: "object",
      properties: {
        what: {
          type: "string",
          enum: ["stack", "control", "handlers", "store", "env", "defs"],
          description: "What to observe",
        },
      },
      required: ["what"],
    },
  },
  {
    name: "match",
    description: "Match an expression against a pattern with ?x binders",
    parameters: {
      type: "object",
      properties: {
        expr: { type: "string", description: "Expression to match" },
        pattern: { type: "string", description: "Pattern with ?x binders" },
      },
      required: ["expr", "pattern"],
    },
  },
  {
    name: "assert",
    description: "Assert a condition - useful for validation",
    parameters: {
      type: "object",
      properties: {
        predicate: { type: "string", description: "Predicate expression" },
        msg: { type: "string", description: "Error message if false" },
      },
      required: ["predicate", "msg"],
    },
  },
  {
    name: "return",
    description: "Return a final result and end the session. Call this when you have computed the answer.",
    parameters: {
      type: "object",
      properties: {
        value: { type: "string", description: "Value to return (as Lisp literal)" },
        confidence: { type: "number", description: "Confidence 0-1" },
      },
      required: ["value"],
    },
  },
];

// Convert to Anthropic tool format
function toAnthropicTools(): any[] {
  return ORACLE_TOOLS.map(t => ({
    name: t.name,
    description: t.description,
    input_schema: t.parameters,
  }));
}

// ═══════════════════════════════════════════════════════════════════════════
// HELPERS
// ═══════════════════════════════════════════════════════════════════════════

function parseValLiteral(s: string): Val {
  s = s.trim();
  if (s === "#t" || s === "true") return { tag: "Bool", b: true };
  if (s === "#f" || s === "false") return { tag: "Bool", b: false };
  if (s === "()" || s === "unit" || s === "null") return { tag: "Unit" };
  if (/^-?\d+(\.\d+)?$/.test(s)) return { tag: "Num", n: parseFloat(s) };
  if (s.startsWith('"') && s.endsWith('"')) return { tag: "Str", s: s.slice(1, -1) };
  if (s.startsWith("'")) return { tag: "Sym", name: s.slice(1) };
  return { tag: "Str", s };
}

function formatVal(v: Val): string {
  switch (v.tag) {
    case "Num": return String(v.n);
    case "Bool": return v.b ? "#t" : "#f";
    case "Str": return v.s;
    case "Sym": return v.name;
    case "Unit": return "()";
    case "Vector": return `[${v.items.map(formatVal).join(" ")}]`;
    case "Closure": return `<closure>`;
    case "Native": return `<native ${v.name}>`;
    default: return JSON.stringify(v);
  }
}

function parseObserveSpec(what: string): ObserveSpec {
  switch (what.toLowerCase()) {
    case "stack": return { tag: "Stack", limit: 10 };
    case "control": return { tag: "Control" };
    case "handlers": return { tag: "Handlers" };
    case "store": return { tag: "StoreSummary", maxCells: 50 };
    case "env": return { tag: "Env" };
    case "defs": return { tag: "Defs" };
    default: return { tag: "Control" };
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// STREAMING HELPERS
// ═══════════════════════════════════════════════════════════════════════════

type StreamingToolUse = {
  id: string;
  name: string;
  input: string;  // JSON string accumulated from deltas
};

type StreamingState = {
  textContent: string;
  toolUses: Map<number, StreamingToolUse>;
  currentToolIndex: number;
};

/**
 * Parse SSE stream from Anthropic API
 */
async function* parseAnthropicSSE(response: Response): AsyncGenerator<any> {
  const reader = response.body!.getReader();
  const decoder = new TextDecoder();
  let buffer = "";

  while (true) {
    const { done, value } = await reader.read();
    if (done) break;

    buffer += decoder.decode(value, { stream: true });
    const lines = buffer.split("\n");
    buffer = lines.pop() || "";

    for (const line of lines) {
      const trimmed = line.trim();
      if (!trimmed.startsWith("data: ")) continue;
      const data = trimmed.slice(6);
      if (data === "[DONE]") return;

      try {
        yield JSON.parse(data);
      } catch {
        // Skip malformed JSON
      }
    }
  }
}

/**
 * Process Anthropic streaming event
 */
function processStreamEvent(state: StreamingState, event: any): void {
  switch (event.type) {
    case "content_block_start":
      if (event.content_block?.type === "tool_use") {
        state.currentToolIndex = event.index;
        state.toolUses.set(event.index, {
          id: event.content_block.id,
          name: event.content_block.name,
          input: "",
        });
      }
      break;

    case "content_block_delta":
      if (event.delta?.type === "text_delta") {
        state.textContent += event.delta.text || "";
      } else if (event.delta?.type === "input_json_delta") {
        const tool = state.toolUses.get(event.index);
        if (tool) {
          tool.input += event.delta.partial_json || "";
        }
      }
      break;

    case "message_stop":
      // Message complete
      break;
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// TOOL CALL PROCESSING
// ═══════════════════════════════════════════════════════════════════════════

type ToolCallResult =
  | { done: false; toolResults: any[] }
  | { done: true; meaning: Meaning };

async function* processToolCalls(
  toolUses: { id: string; name: string; input: any }[],
  init: OracleInit,
  model: string,
  turn: number
): AsyncGenerator<OracleReq, ToolCallResult, OracleResp> {
  const toolResults: any[] = [];

  for (const tc of toolUses) {
    const toolArgs = tc.input;
    let req: OracleReq;
    let resp: OracleResp;
    let resultContent: string;

    switch (tc.name) {
      case "eval":
        req = {
          tag: "ReqEval",
          qexpr: toolArgs.expr as string as unknown as QExpr,
          envRef: init.envRef,
        };
        resp = yield req;
        if (resp.tag === "RespVal") {
          resultContent = formatVal(resp.value);
        } else if (resp.tag === "RespError") {
          resultContent = `Error: ${resp.message}`;
        } else {
          resultContent = JSON.stringify(resp);
        }
        break;

      case "apply":
        const fnVal = parseValLiteral(toolArgs.fn);
        const argsVals = (toolArgs.args as string[]).map(parseValLiteral);
        req = {
          tag: "ReqApply",
          fn: fnVal,
          args: argsVals,
          envRef: init.envRef,
        };
        resp = yield req;
        if (resp.tag === "RespVal") {
          resultContent = formatVal(resp.value);
        } else if (resp.tag === "RespError") {
          resultContent = `Error: ${resp.message}`;
        } else {
          resultContent = JSON.stringify(resp);
        }
        break;

      case "observe":
        req = {
          tag: "ReqObserve",
          what: parseObserveSpec(toolArgs.what),
          stateRef: init.stateRef,
        };
        resp = yield req;
        if (resp.tag === "RespObs") {
          resultContent = JSON.stringify(resp.data, null, 2);
        } else if (resp.tag === "RespError") {
          resultContent = `Error: ${resp.message}`;
        } else {
          resultContent = JSON.stringify(resp);
        }
        break;

      case "match":
        req = {
          tag: "ReqMatch",
          qexpr: toolArgs.expr as string as unknown as QExpr,
          pattern: toolArgs.pattern as string as unknown as QExpr,
          envRef: init.envRef,
        };
        resp = yield req;
        resultContent = JSON.stringify(resp);
        break;

      case "assert":
        req = {
          tag: "ReqAssert",
          predicate: toolArgs.predicate as string as unknown as QExpr,
          msg: toolArgs.msg,
          envRef: init.envRef,
        };
        resp = yield req;
        if (resp.tag === "RespAck") {
          resultContent = "Assertion passed";
        } else if (resp.tag === "RespError") {
          resultContent = `Assertion failed: ${resp.message}`;
        } else {
          resultContent = JSON.stringify(resp);
        }
        break;

      case "return":
        const retVal = parseValLiteral(toolArgs.value);
        const confidence = toolArgs.confidence ?? 0.9;
        return {
          done: true,
          meaning: meaning({
            denotation: retVal,
            confidence,
            trace: { tag: "Str", s: `adapter=anthropic model=${model} turns=${turn + 1}` },
          }),
        };

      default:
        resultContent = `Unknown tool: ${tc.name}`;
    }

    toolResults.push({
      type: "tool_result",
      tool_use_id: tc.id,
      content: resultContent!,
    });
  }

  return { done: false, toolResults };
}

// ═══════════════════════════════════════════════════════════════════════════
// ANTHROPIC ADAPTER
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Anthropic Claude adapter with native tool calling
 * Supports both streaming and blocking modes via config.streaming
 */
export class AnthropicAdapter implements OracleAdapterWithCaps {
  constructor(private config: LLMConfig) {}

  capabilities(): AdapterCaps {
    return {
      multiTurn: true,
      toolCalling: true,
      mcp: false,
      streaming: true,
      vision: true,
      maxContext: 200_000,  // Claude 3.5 Sonnet / Claude 4
    };
  }

  startSession(init: OracleInit): OracleSession {
    if (this.config.streaming) {
      return this.startStreamingSession(init);
    } else {
      return this.startBlockingSession(init);
    }
  }

  // ─────────────────────────────────────────────────────────────────────────
  // STREAMING MODE
  // ─────────────────────────────────────────────────────────────────────────

  private startStreamingSession(init: OracleInit): OracleSession {
    const config = this.config;
    const tools = toAnthropicTools();

    return (async function* (): OracleSession {
      const systemPrompt = config.systemPrompt ?? `You are an Oracle for a Lisp runtime.

CRITICAL: You MUST use the 'return' tool to provide your final answer. Do NOT write text answers directly.

For simple questions (yes/no, factual answers):
- Call the 'return' tool immediately with your answer

For computation requiring Lisp:
- Use 'eval' tool first, then 'return' tool with the result

You have these tools:
- return(value): Provide your final answer (REQUIRED)
- eval(expr): Run Lisp code only if needed`;

      const userPrompt = init.tag === "Infer"
        ? formatVal(init.payload)
        : `Apply procedure ${formatVal(init.proc)} to args: ${init.args.map(formatVal).join(", ")}`;

      const messages: any[] = [
        { role: "user", content: userPrompt },
      ];

      const apiKey = config.apiKey ?? process.env.ANTHROPIC_API_KEY;
      if (!apiKey) {
        return meaning({
          denotation: { tag: "Unit" },
          confidence: 0,
          trace: { tag: "Str", s: "error: ANTHROPIC_API_KEY not set" },
        });
      }

      const baseUrl = config.baseUrl ?? "https://api.anthropic.com";
      const model = config.model ?? "claude-sonnet-4-20250514";
      const maxTurns = 20;

      for (let turn = 0; turn < maxTurns; turn++) {
        const response = await fetch(`${baseUrl}/v1/messages`, {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
            "x-api-key": apiKey,
            "anthropic-version": "2023-06-01",
          },
          body: JSON.stringify({
            model,
            max_tokens: config.maxTokens ?? 4096,
            system: systemPrompt,
            tools,
            messages,
            stream: true,  // ← STREAMING ENABLED
          }),
        });

        if (!response.ok) {
          const err = await response.text();
          return meaning({
            denotation: { tag: "Unit" },
            confidence: 0,
            trace: { tag: "Str", s: `Anthropic API error: ${err}` },
          });
        }

        // Accumulate streaming events
        const state: StreamingState = {
          textContent: "",
          toolUses: new Map(),
          currentToolIndex: -1,
        };

        for await (const event of parseAnthropicSSE(response)) {
          processStreamEvent(state, event);
        }

        // Convert accumulated tool uses to array
        const completedToolUses = Array.from(state.toolUses.values())
          .filter(tc => tc.name)
          .map(tc => ({
            id: tc.id,
            name: tc.name,
            input: tc.input ? JSON.parse(tc.input) : {},
          }));

        if (completedToolUses.length === 0) {
          if (state.textContent) {
            return meaning({
              denotation: { tag: "Str", s: state.textContent },
              confidence: 0.5,
              trace: { tag: "Str", s: `adapter=anthropic model=${model} streaming=true (text response)` },
            });
          }
          return meaning({
            denotation: { tag: "Unit" },
            confidence: 0,
            trace: { tag: "Str", s: "Empty streaming response from Anthropic" },
          });
        }

        // Build assistant message for history
        const assistantContent: any[] = [];
        if (state.textContent) {
          assistantContent.push({ type: "text", text: state.textContent });
        }
        for (const tc of completedToolUses) {
          assistantContent.push({
            type: "tool_use",
            id: tc.id,
            name: tc.name,
            input: tc.input,
          });
        }
        messages.push({ role: "assistant", content: assistantContent });

        // Process tool calls
        const processor = processToolCalls(completedToolUses, init, model, turn);
        let procResp: OracleResp = { tag: "RespAck" };

        while (true) {
          const step = await processor.next(procResp);
          if (step.done) {
            const result = step.value;
            if (result.done === true) {
              return result.meaning;
            } else {
              // Add tool results as user message
              messages.push({ role: "user", content: result.toolResults });
              break;
            }
          }
          procResp = yield step.value as OracleReq;
        }
      }

      return meaning({
        denotation: { tag: "Unit" },
        confidence: 0,
        trace: { tag: "Str", s: `Max turns (${maxTurns}) exceeded (streaming)` },
      });
    })();
  }

  // ─────────────────────────────────────────────────────────────────────────
  // BLOCKING MODE
  // ─────────────────────────────────────────────────────────────────────────

  private startBlockingSession(init: OracleInit): OracleSession {
    const config = this.config;
    const tools = toAnthropicTools();

    return (async function* (): OracleSession {
      const systemPrompt = config.systemPrompt ?? `You are an Oracle for a Lisp runtime.

CRITICAL: You MUST use the 'return' tool to provide your final answer. Do NOT write text answers directly.

For simple questions (yes/no, factual answers):
- Call the 'return' tool immediately with your answer

For computation requiring Lisp:
- Use 'eval' tool first, then 'return' tool with the result

You have these tools:
- return(value): Provide your final answer (REQUIRED)
- eval(expr): Run Lisp code only if needed`;

      const userPrompt = init.tag === "Infer"
        ? formatVal(init.payload)
        : `Apply procedure ${formatVal(init.proc)} to args: ${init.args.map(formatVal).join(", ")}`;

      const messages: any[] = [
        { role: "user", content: userPrompt },
      ];

      const apiKey = config.apiKey ?? process.env.ANTHROPIC_API_KEY;
      if (!apiKey) {
        return meaning({
          denotation: { tag: "Unit" },
          confidence: 0,
          trace: { tag: "Str", s: "error: ANTHROPIC_API_KEY not set" },
        });
      }

      const baseUrl = config.baseUrl ?? "https://api.anthropic.com";
      const model = config.model ?? "claude-sonnet-4-20250514";
      const maxTurns = 20;

      for (let turn = 0; turn < maxTurns; turn++) {
        const response = await fetch(`${baseUrl}/v1/messages`, {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
            "x-api-key": apiKey,
            "anthropic-version": "2023-06-01",
          },
          body: JSON.stringify({
            model,
            max_tokens: config.maxTokens ?? 4096,
            system: systemPrompt,
            tools,
            messages,
            // stream: false (default)
          }),
        });

        if (!response.ok) {
          const err = await response.text();
          return meaning({
            denotation: { tag: "Unit" },
            confidence: 0,
            trace: { tag: "Str", s: `Anthropic API error: ${err}` },
          });
        }

        const data = await response.json() as any;

        // Extract text and tool_use blocks from content
        const content = data.content || [];
        let textContent = "";
        const toolUses: { id: string; name: string; input: any }[] = [];

        for (const block of content) {
          if (block.type === "text") {
            textContent += block.text;
          } else if (block.type === "tool_use") {
            toolUses.push({
              id: block.id,
              name: block.name,
              input: block.input,
            });
          }
        }

        if (toolUses.length === 0) {
          if (textContent) {
            return meaning({
              denotation: { tag: "Str", s: textContent },
              confidence: 0.5,
              trace: { tag: "Str", s: `adapter=anthropic model=${model} (text response)` },
            });
          }
          return meaning({
            denotation: { tag: "Unit" },
            confidence: 0,
            trace: { tag: "Str", s: "Empty response from Anthropic" },
          });
        }

        // Add assistant message to history
        messages.push({ role: "assistant", content });

        // Process tool calls
        const processor = processToolCalls(toolUses, init, model, turn);
        let procResp: OracleResp = { tag: "RespAck" };

        while (true) {
          const step = await processor.next(procResp);
          if (step.done) {
            const result = step.value;
            if (result.done === true) {
              return result.meaning;
            } else {
              // Add tool results as user message
              messages.push({ role: "user", content: result.toolResults });
              break;
            }
          }
          procResp = yield step.value as OracleReq;
        }
      }

      return meaning({
        denotation: { tag: "Unit" },
        confidence: 0,
        trace: { tag: "Str", s: `Max turns (${maxTurns}) exceeded` },
      });
    })();
  }
}

/**
 * Factory function for creating Anthropic adapter with env vars
 */
export function createAnthropicAdapter(overrides?: Partial<LLMConfig>): AnthropicAdapter {
  return new AnthropicAdapter({
    model: overrides?.model ?? "claude-sonnet-4-20250514",
    apiKey: overrides?.apiKey ?? process.env.ANTHROPIC_API_KEY,
    maxTokens: overrides?.maxTokens ?? 4096,
    temperature: overrides?.temperature ?? 0.7,
    ...overrides,
  });
}
