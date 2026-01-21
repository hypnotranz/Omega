// src/core/oracle/adapters/openaiAdapter.ts
// Real OpenAI Oracle adapter with native tool calling

import type { OracleAdapter, OracleInit } from "../adapter";
import type { OracleSession, OracleReq, OracleResp, Meaning, QExpr, ObserveSpec } from "../protocol";
import type { Val } from "../../eval/values";
import type { LLMConfig, AdapterCaps, OracleAdapterWithCaps, ToolDef } from "./types";
import { meaning } from "../meaning";

// Tool definitions that map to Oracle protocol requests
const ORACLE_TOOLS: ToolDef[] = [
  {
    name: "eval",
    description: "Evaluate a Lisp expression in the current environment. Returns the result value.",
    parameters: {
      type: "object",
      properties: {
        expr: { type: "string", description: "Lisp expression to evaluate (e.g., '(+ 1 2)' or '(define (f x) (* x x))')" },
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
        args: { type: "array", items: { type: "string" }, description: "Arguments as Lisp expressions" },
      },
      required: ["fn", "args"],
    },
  },
  {
    name: "observe",
    description: "Observe runtime state - useful for introspection",
    parameters: {
      type: "object",
      properties: {
        what: {
          type: "string",
          enum: ["stack", "control", "handlers", "store", "env", "defs"],
          description: "What to observe: stack (call stack), control (current expression), handlers (effect handlers), store (memory), env (environment bindings), defs (top-level definitions)",
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
        pattern: { type: "string", description: "Pattern with ?x binders (e.g., '(?op ?a ?b)')" },
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
        predicate: { type: "string", description: "Predicate expression that should evaluate to true" },
        msg: { type: "string", description: "Error message if assertion fails" },
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
        value: { type: "string", description: "Value to return as a Lisp literal (e.g., '42', '#t', '(list 1 2 3)')" },
        confidence: { type: "number", description: "Confidence in result from 0 to 1 (default: 0.9)" },
      },
      required: ["value"],
    },
  },
];

// Convert to OpenAI function format
function toOpenAITools(): any[] {
  return ORACLE_TOOLS.map(t => ({
    type: "function",
    function: {
      name: t.name,
      description: t.description,
      parameters: t.parameters,
    },
  }));
}

// Parse a Lisp literal string into a Val (simplified)
function parseValLiteral(s: string): Val {
  s = s.trim();
  if (s === "#t" || s === "true") return { tag: "Bool", b: true };
  if (s === "#f" || s === "false") return { tag: "Bool", b: false };
  if (s === "()" || s === "unit" || s === "null") return { tag: "Unit" };
  if (/^-?\d+(\.\d+)?$/.test(s)) return { tag: "Num", n: parseFloat(s) };
  if (s.startsWith('"') && s.endsWith('"')) return { tag: "Str", s: s.slice(1, -1) };
  if (s.startsWith("'")) return { tag: "Sym", name: s.slice(1) };
  // For complex expressions, just wrap as string - let eval handle it
  return { tag: "Str", s };
}

// Check if a Val is a proper list (cons cells ending in Unit/null)
function isProperList(v: Val): boolean {
  let current = v;
  while (current.tag === "Vector" && current.items.length === 2) {
    current = current.items[1];
  }
  return current.tag === "Unit";
}

// Extract elements from a proper list
function listToArray(v: Val): Val[] {
  const result: Val[] = [];
  let current = v;
  while (current.tag === "Vector" && current.items.length === 2) {
    result.push(current.items[0]);
    current = current.items[1];
  }
  return result;
}

// Check if a list is all strings
function isStringList(v: Val): boolean {
  if (!isProperList(v)) return false;
  return listToArray(v).every(item => item.tag === "Str");
}

// Concatenate a list of strings into a single string
function concatStringList(v: Val): string {
  return listToArray(v).map(item => (item as { tag: "Str"; s: string }).s).join("");
}

// Format Val for display (used in prompts to LLM)
function formatVal(v: Val): string {
  switch (v.tag) {
    case "Num": return String(v.n);
    case "Bool": return v.b ? "#t" : "#f";
    case "Str": return v.s;  // Return raw string without quotes for prompt
    case "Sym": return v.name;
    case "Unit": return "()";
    case "Vector": {
      // Check if it's a list of strings - concatenate them for cleaner prompt
      if (isStringList(v)) {
        return concatStringList(v);
      }
      // Otherwise format as list
      if (isProperList(v)) {
        return `(${listToArray(v).map(formatVal).join(" ")})`;
      }
      return `[${v.items.map(formatVal).join(" ")}]`;
    }
    case "Closure": return `<closure ${v.params.join(" ")}>`;
    case "Native": return `<native ${v.name}>`;
    case "Meaning": return `<meaning conf=${v.confidence}>`;
    default: return JSON.stringify(v);
  }
}

// Format ObserveSpec
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

type StreamingToolCall = {
  id: string;
  name: string;
  arguments: string;
};

/**
 * Parse SSE stream chunks from OpenAI streaming response
 */
async function* parseSSEStream(response: Response): AsyncGenerator<any> {
  const reader = response.body!.getReader();
  const decoder = new TextDecoder();
  let buffer = "";

  while (true) {
    const { done, value } = await reader.read();
    if (done) break;

    buffer += decoder.decode(value, { stream: true });
    const lines = buffer.split("\n");
    buffer = lines.pop() || "";  // Keep incomplete line in buffer

    for (const line of lines) {
      const trimmed = line.trim();
      if (!trimmed || !trimmed.startsWith("data: ")) continue;
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
 * Accumulate streaming chunks into complete tool calls
 */
function accumulateStreamChunk(
  toolCalls: Map<number, StreamingToolCall>,
  contentBuffer: { content: string },
  chunk: any
): void {
  const delta = chunk.choices?.[0]?.delta;
  if (!delta) return;

  // Accumulate content
  if (delta.content) {
    contentBuffer.content += delta.content;
  }

  // Accumulate tool calls
  if (delta.tool_calls) {
    for (const tc of delta.tool_calls) {
      const idx = tc.index;
      const existing = toolCalls.get(idx) ?? { id: "", name: "", arguments: "" };

      if (tc.id) existing.id = tc.id;
      if (tc.function?.name) existing.name = tc.function.name;
      if (tc.function?.arguments) existing.arguments += tc.function.arguments;

      toolCalls.set(idx, existing);
    }
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// TOOL CALL PROCESSING (shared between streaming and blocking)
// ═══════════════════════════════════════════════════════════════════════════

type ToolCallResult =
  | { done: false; toolResults: any[] }
  | { done: true; meaning: Meaning };

/**
 * Process tool calls and yield to runtime - used by both modes
 */
async function* processToolCalls(
  toolCalls: { id: string; name: string; arguments: string }[],
  init: OracleInit,
  model: string,
  turn: number
): AsyncGenerator<OracleReq, ToolCallResult, OracleResp> {
  const toolResults: any[] = [];

  for (const tc of toolCalls) {
    const toolName = tc.name;
    let toolArgs: any;
    try {
      toolArgs = JSON.parse(tc.arguments || "{}");
    } catch {
      toolArgs = {};
    }

    let req: OracleReq;
    let resp: OracleResp;
    let resultContent: string;

    switch (toolName) {
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
            trace: { tag: "Str", s: `adapter=openai model=${model} turns=${turn + 1}` },
          }),
        };

      default:
        resultContent = `Unknown tool: ${toolName}`;
    }

    toolResults.push({
      role: "tool",
      tool_call_id: tc.id,
      content: resultContent!,
    });
  }

  return { done: false, toolResults };
}

// ═══════════════════════════════════════════════════════════════════════════
// OPENAI ADAPTER
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Real OpenAI Oracle adapter with tool calling
 * Supports both streaming and blocking modes via config.streaming
 */
export class OpenAIAdapter implements OracleAdapterWithCaps {
  constructor(private config: LLMConfig) {}

  capabilities(): AdapterCaps {
    return {
      multiTurn: true,
      toolCalling: true,
      mcp: false,
      streaming: true,
      vision: true,
      maxContext: 128_000,  // GPT-4
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
  // STREAMING MODE - accumulates tool calls from SSE chunks
  // ─────────────────────────────────────────────────────────────────────────

  private startStreamingSession(init: OracleInit): OracleSession {
    const config = this.config;
    const tools = toOpenAITools();

    return (async function* (): OracleSession {
      const systemPrompt = config.systemPrompt ?? `You are an Oracle that answers questions using tool calls.

CRITICAL: You MUST use the 'return' tool to provide your answer. Do NOT write text answers directly.

For simple questions (yes/no, factual answers):
- Call the 'return' tool immediately with your answer

For computation requiring Lisp:
- Use 'eval' tool first, then 'return' tool with the result

You have these tools:
- return(value): Provide your final answer (REQUIRED for all responses)
- eval(expr): Run Lisp code only if needed`;

      const userPrompt = init.tag === "Infer"
        ? formatVal(init.payload)
        : `Apply procedure ${formatVal(init.proc)} to args: ${init.args.map(formatVal).join(", ")}`;

      const messages: any[] = [
        { role: "system", content: systemPrompt },
        { role: "user", content: userPrompt },
      ];

      const apiKey = config.apiKey ?? process.env.OPENAI_API_KEY;
      if (!apiKey) {
        return meaning({
          denotation: { tag: "Unit" },
          confidence: 0,
          trace: { tag: "Str", s: "error: OPENAI_API_KEY not set" },
        });
      }

      const baseUrl = config.baseUrl ?? "https://api.openai.com/v1";
      const model = config.model ?? "gpt-4o";
      const maxTurns = 20;

      for (let turn = 0; turn < maxTurns; turn++) {
        // Call OpenAI with streaming enabled
        const response = await fetch(`${baseUrl}/chat/completions`, {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
            "Authorization": `Bearer ${apiKey}`,
          },
          body: JSON.stringify({
            model,
            messages,
            tools,
            tool_choice: "auto",
            max_tokens: config.maxTokens ?? 4096,
            temperature: config.temperature ?? 0.7,
            stream: true,  // ← STREAMING ENABLED
          }),
        });

        if (!response.ok) {
          const err = await response.text();
          return meaning({
            denotation: { tag: "Unit" },
            confidence: 0,
            trace: { tag: "Str", s: `OpenAI API error: ${err}` },
          });
        }

        // Accumulate streaming chunks
        const toolCalls = new Map<number, StreamingToolCall>();
        const contentBuffer = { content: "" };

        for await (const chunk of parseSSEStream(response)) {
          accumulateStreamChunk(toolCalls, contentBuffer, chunk);
        }

        // Convert accumulated tool calls to array
        const completedToolCalls = Array.from(toolCalls.values()).filter(tc => tc.name);

        if (completedToolCalls.length === 0) {
          // No tool calls - check if we have content
          if (contentBuffer.content) {
            return meaning({
              denotation: { tag: "Str", s: contentBuffer.content },
              confidence: 0.5,
              trace: { tag: "Str", s: `adapter=openai model=${model} streaming=true (text response)` },
            });
          }
          return meaning({
            denotation: { tag: "Unit" },
            confidence: 0,
            trace: { tag: "Str", s: "Empty streaming response from OpenAI" },
          });
        }

        // Build assistant message for history
        const assistantMsg: any = {
          role: "assistant",
          content: contentBuffer.content || null,
          tool_calls: completedToolCalls.map((tc, idx) => ({
            id: tc.id,
            type: "function",
            function: { name: tc.name, arguments: tc.arguments },
          })),
        };
        messages.push(assistantMsg);

        // Process tool calls using shared logic
        const processor = processToolCalls(completedToolCalls, init, model, turn);
        let procResp: OracleResp = { tag: "RespAck" };

        while (true) {
          const step = await processor.next(procResp);
          if (step.done) {
            const result = step.value;
            if (result.done === true) {
              return result.meaning;
            } else {
              // Add tool results to messages and continue
              messages.push(...result.toolResults);
              break;
            }
          }
          // Yield request to runtime
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
  // BLOCKING MODE - original implementation (waits for full response)
  // ─────────────────────────────────────────────────────────────────────────

  private startBlockingSession(init: OracleInit): OracleSession {
    const config = this.config;
    const tools = toOpenAITools();

    return (async function* (): OracleSession {
      const systemPrompt = config.systemPrompt ?? `You are an Oracle that answers questions using tool calls.

CRITICAL: You MUST use the 'return' tool to provide your answer. Do NOT write text answers directly.

For simple questions (yes/no, factual answers):
- Call the 'return' tool immediately with your answer

For computation requiring Lisp:
- Use 'eval' tool first, then 'return' tool with the result

You have these tools:
- return(value): Provide your final answer (REQUIRED for all responses)
- eval(expr): Run Lisp code only if needed`;

      const userPrompt = init.tag === "Infer"
        ? formatVal(init.payload)
        : `Apply procedure ${formatVal(init.proc)} to args: ${init.args.map(formatVal).join(", ")}`;

      const messages: any[] = [
        { role: "system", content: systemPrompt },
        { role: "user", content: userPrompt },
      ];

      const apiKey = config.apiKey ?? process.env.OPENAI_API_KEY;
      if (!apiKey) {
        return meaning({
          denotation: { tag: "Unit" },
          confidence: 0,
          trace: { tag: "Str", s: "error: OPENAI_API_KEY not set" },
        });
      }

      const baseUrl = config.baseUrl ?? "https://api.openai.com/v1";
      const model = config.model ?? "gpt-4o";
      const maxTurns = 20;

      for (let turn = 0; turn < maxTurns; turn++) {
        // Call OpenAI (blocking)
        const response = await fetch(`${baseUrl}/chat/completions`, {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
            "Authorization": `Bearer ${apiKey}`,
          },
          body: JSON.stringify({
            model,
            messages,
            tools,
            tool_choice: "auto",
            max_tokens: config.maxTokens ?? 4096,
            temperature: config.temperature ?? 0.7,
            // stream: false (default)
          }),
        });

        if (!response.ok) {
          const err = await response.text();
          return meaning({
            denotation: { tag: "Unit" },
            confidence: 0,
            trace: { tag: "Str", s: `OpenAI API error: ${err}` },
          });
        }

        const data = await response.json() as any;
        const choice = data.choices?.[0];
        const assistantMsg = choice?.message;

        if (!assistantMsg) {
          return meaning({
            denotation: { tag: "Unit" },
            confidence: 0,
            trace: { tag: "Str", s: "No response from OpenAI" },
          });
        }

        messages.push(assistantMsg);

        const toolCalls = assistantMsg.tool_calls;
        if (!toolCalls || toolCalls.length === 0) {
          if (assistantMsg.content) {
            return meaning({
              denotation: { tag: "Str", s: assistantMsg.content },
              confidence: 0.5,
              trace: { tag: "Str", s: `adapter=openai model=${model} (text response, no return tool)` },
            });
          }
          return meaning({
            denotation: { tag: "Unit" },
            confidence: 0,
            trace: { tag: "Str", s: "Empty response from OpenAI" },
          });
        }

        // Convert to common format and process
        const normalizedCalls = toolCalls.map((tc: any) => ({
          id: tc.id,
          name: tc.function?.name || "",
          arguments: tc.function?.arguments || "{}",
        }));

        const processor = processToolCalls(normalizedCalls, init, model, turn);
        let procResp: OracleResp = { tag: "RespAck" };

        while (true) {
          const step = await processor.next(procResp);
          if (step.done) {
            const result = step.value;
            if (result.done === true) {
              return result.meaning;
            } else {
              messages.push(...result.toolResults);
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
 * Factory function for creating OpenAI adapter with env vars
 */
export function createOpenAIAdapter(overrides?: Partial<LLMConfig>): OpenAIAdapter {
  return new OpenAIAdapter({
    model: overrides?.model ?? "gpt-4o",
    apiKey: overrides?.apiKey ?? process.env.OPENAI_API_KEY,
    maxTokens: overrides?.maxTokens ?? 4096,
    temperature: overrides?.temperature ?? 0.7,
    ...overrides,
  });
}
