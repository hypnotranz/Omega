#!/usr/bin/env npx tsx
// bin/omega-repl.ts
// Interactive Omega REPL with dual-REPL oracle support
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-IMPLEMENTATION-20.md
//
// Run:  npx tsx bin/omega-repl.ts
//       npx tsx bin/omega-repl.ts --verbose   (show oracle transcript)

import * as readline from "readline";
import * as fs from "fs";
import * as path from "path";

// Load .env file if it exists
const envPath = path.join(process.cwd(), ".env");
if (fs.existsSync(envPath)) {
  const envContent = fs.readFileSync(envPath, "utf8");
  for (const line of envContent.split("\n")) {
    const match = line.match(/^([^=]+)=(.*)$/);
    if (match && !process.env[match[1]]) {
      process.env[match[1]] = match[2].trim();
    }
  }
}
import { COWStore, type Store } from "../src/core/eval/store";
import { RuntimeImpl } from "../src/core/effects/runtimeImpl";
import { SnapshotRepo } from "../src/core/oracle/snapshots";
import { InMemoryReceiptStore } from "../src/core/oracle/receipts";
import { installPrims } from "../test/helpers/prims";
import { createOpenAIAdapter, createAnthropicAdapter } from "../src/core/oracle/adapters";
import { DepthTrackingAdapter } from "../src/core/oracle/adapters/types";
import type { OracleAdapter } from "../src/core/oracle/adapter";
import type { State, Frame } from "../src/core/eval/machine";
import { runToCompletionWithState } from "../src/core/eval/run";
import { stepOnce } from "../src/core/eval/machineStep";
import type { StepOutcome } from "../src/core/eval/machine";
import { compileTextToExpr } from "../src/core/pipeline/compileText";
import type { Val } from "../src/core/eval/values";
import { VUnit } from "../src/core/eval/values";
import type { Env } from "../src/core/eval/env";
import { ScriptedOracleAdapter } from "../src/core/oracle/scriptedOracle";
import { SessionWriter, SessionReader, JumpController, renderTrace } from "../src/core/session";
import type { SessionIndex } from "../src/core/session";
import { buildNativeRegistry } from "../src/core/session/nativeRegistry";
import { buildSolverRegistry } from "../src/core/session/solverRegistry";

// OPR imports
import { OprRuntime } from "../src/core/opr/runtime";
import { InMemoryReceiptStore as OprReceiptStore } from "../src/core/opr/receipts";
import { OpenAIOprAdapter } from "../src/core/opr/adapters/openai";
import { AnthropicOprAdapter } from "../src/core/opr/adapters/anthropic";
import { listKernels, getKernel } from "../src/core/opr/kernels";

// ─────────────────────────────────────────────────────────────────
// LLM Integration
// ─────────────────────────────────────────────────────────────────
function loadApiKey(): string | undefined {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  if (process.env.ANTHROPIC_API_KEY) return process.env.ANTHROPIC_API_KEY;
  try {
    const configPath = path.join(process.cwd(), "../LambdaRLM/config.yaml");
    const content = fs.readFileSync(configPath, "utf8");
    const match = content.match(/api_key:\s*(\S+)/);
    if (match?.[1]) return match[1];
  } catch { /* ignore */ }
  return undefined;
}

async function askLLM(prompt: string, apiKey: string): Promise<string> {
  // Simple OpenAI API call (legacy, non-agentic)
  const response = await fetch("https://api.openai.com/v1/chat/completions", {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      "Authorization": `Bearer ${apiKey}`,
    },
    body: JSON.stringify({
      model: "gpt-4o-mini",
      messages: [{ role: "user", content: prompt }],
      max_tokens: 500,
    }),
  });
  const data = await response.json() as any;
  return data.choices?.[0]?.message?.content || "";
}

// ─────────────────────────────────────────────────────────────────
// LLM Adapter Interface (pluggable LLM support)
// ─────────────────────────────────────────────────────────────────

interface LLMToolCall {
  id: string;
  name: string;
  args: Record<string, any>;
}

interface LLMResponse {
  content?: string;
  toolCalls?: LLMToolCall[];
  finishReason?: string;
}

interface LLMAdapter {
  name: string;
  supportsToolCalls: boolean;
  chat(messages: any[], tools?: any[]): Promise<LLMResponse>;
}

// OpenAI Adapter (native tool support)
class OpenAIAdapter implements LLMAdapter {
  name = "openai";
  supportsToolCalls = true;

  constructor(private apiKey: string, private model = "gpt-4o-mini") {}

  async chat(messages: any[], tools?: any[]): Promise<LLMResponse> {
    const body: any = {
      model: this.model,
      messages,
      max_tokens: 1500,
    };
    if (tools && tools.length > 0) {
      body.tools = tools;
      body.tool_choice = "auto";
    }

    const response = await fetch("https://api.openai.com/v1/chat/completions", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        "Authorization": `Bearer ${this.apiKey}`,
      },
      body: JSON.stringify(body),
    });

    const data = await response.json() as any;
    const choice = data.choices?.[0];
    const message = choice?.message;

    if (!message) {
      return { content: undefined, finishReason: "error" };
    }

    const toolCalls = message.tool_calls?.map((tc: any) => ({
      id: tc.id,
      name: tc.function.name,
      args: JSON.parse(tc.function.arguments || "{}"),
    }));

    return {
      content: message.content || undefined,
      toolCalls,
      finishReason: choice.finish_reason,
    };
  }
}

// Text-based adapter for LLMs without tool support
// Uses structured text format: [TOOL:name] {json args}
class TextBasedAdapter implements LLMAdapter {
  name = "text-based";
  supportsToolCalls = false;

  constructor(private apiKey: string, private endpoint: string, private model: string) {}

  async chat(messages: any[], _tools?: any[]): Promise<LLMResponse> {
    // Add instruction about text-based tool format
    const toolInstructions = `
When you need to use a tool, format your response EXACTLY like this:
[TOOL:tool_name] {"arg1": "value1", "arg2": "value2"}

Available tools:
- [TOOL:eval_lisp] {"code": "(+ 1 2)"}
- [TOOL:get_definitions] {}
- [TOOL:inspect_value] {"name": "factorial"}
- [TOOL:debug_load] {"code": "(+ 1 2)"}
- [TOOL:debug_step] {"count": 1}
- [TOOL:debug_run] {}
- [TOOL:debug_goto] {"step": 5}
- [TOOL:debug_state] {}
- [TOOL:debug_stack] {}
- [TOOL:debug_trace] {"start": 0, "count": 20}
- [TOOL:set_breakpoint] {"type": "step", "condition": "10"}
- [TOOL:list_breakpoints] {}
- [TOOL:delete_breakpoint] {"id": 1}

After each tool result, you can call another tool or provide your final answer.
Only provide your final answer (without [TOOL:...]) when you're done with tool calls.
`;

    // Prepend tool instructions to system message
    const enhancedMessages = messages.map((m, i) => {
      if (i === 0 && m.role === "system") {
        return { ...m, content: m.content + "\n\n" + toolInstructions };
      }
      return m;
    });

    const response = await fetch(this.endpoint, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        "Authorization": `Bearer ${this.apiKey}`,
      },
      body: JSON.stringify({
        model: this.model,
        messages: enhancedMessages,
        max_tokens: 1500,
      }),
    });

    const data = await response.json() as any;
    const content = data.choices?.[0]?.message?.content || "";

    // Parse text-based tool calls
    const toolCalls = this.parseTextToolCalls(content);

    // If we found tool calls, extract the non-tool content
    let textContent = content;
    if (toolCalls.length > 0) {
      textContent = content.replace(/\[TOOL:\w+\]\s*\{[^}]*\}/g, "").trim();
    }

    return {
      content: textContent || undefined,
      toolCalls: toolCalls.length > 0 ? toolCalls : undefined,
      finishReason: data.choices?.[0]?.finish_reason,
    };
  }

  private parseTextToolCalls(text: string): LLMToolCall[] {
    const toolCalls: LLMToolCall[] = [];
    const regex = /\[TOOL:(\w+)\]\s*(\{[^}]*\})/g;
    let match;
    let id = 1;

    while ((match = regex.exec(text)) !== null) {
      try {
        const name = match[1];
        const args = JSON.parse(match[2]);
        toolCalls.push({ id: `text-${id++}`, name, args });
      } catch (e) {
        // Skip malformed tool calls
      }
    }

    return toolCalls;
  }
}

// Anthropic adapter (native tool support via messages API)
class AnthropicAdapter implements LLMAdapter {
  name = "anthropic";
  supportsToolCalls = true;

  constructor(private apiKey: string, private model = "claude-3-5-sonnet-20241022") {}

  private convertToolsToAnthropic(openaiTools: any[]): any[] {
    return openaiTools.map(t => ({
      name: t.function.name,
      description: t.function.description,
      input_schema: t.function.parameters,
    }));
  }

  async chat(messages: any[], tools?: any[]): Promise<LLMResponse> {
    // Convert messages format (OpenAI -> Anthropic)
    const systemMsg = messages.find(m => m.role === "system");
    const nonSystemMsgs = messages.filter(m => m.role !== "system").map(m => {
      if (m.role === "tool") {
        // Convert tool response format
        return {
          role: "user",
          content: [{ type: "tool_result", tool_use_id: m.tool_call_id, content: m.content }],
        };
      }
      if (m.tool_calls) {
        // Convert assistant message with tool calls
        const content: any[] = [];
        if (m.content) content.push({ type: "text", text: m.content });
        for (const tc of m.tool_calls) {
          content.push({
            type: "tool_use",
            id: tc.id,
            name: tc.function.name,
            input: JSON.parse(tc.function.arguments || "{}"),
          });
        }
        return { role: "assistant", content };
      }
      return { role: m.role, content: m.content };
    });

    const body: any = {
      model: this.model,
      max_tokens: 1500,
      messages: nonSystemMsgs,
    };
    if (systemMsg) body.system = systemMsg.content;
    if (tools && tools.length > 0) {
      body.tools = this.convertToolsToAnthropic(tools);
    }

    const response = await fetch("https://api.anthropic.com/v1/messages", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        "x-api-key": this.apiKey,
        "anthropic-version": "2023-06-01",
      },
      body: JSON.stringify(body),
    });

    const data = await response.json() as any;

    // Parse Anthropic response
    let content: string | undefined;
    const toolCalls: LLMToolCall[] = [];

    for (const block of data.content || []) {
      if (block.type === "text") {
        content = (content || "") + block.text;
      } else if (block.type === "tool_use") {
        toolCalls.push({
          id: block.id,
          name: block.name,
          args: block.input || {},
        });
      }
    }

    return {
      content,
      toolCalls: toolCalls.length > 0 ? toolCalls : undefined,
      finishReason: data.stop_reason,
    };
  }
}

// Factory to create the right adapter
function createLLMAdapter(): LLMAdapter | null {
  // Try OpenAI first
  const openaiKey = process.env.OPENAI_API_KEY;
  if (openaiKey) {
    return new OpenAIAdapter(openaiKey);
  }

  // Try Anthropic
  const anthropicKey = process.env.ANTHROPIC_API_KEY;
  if (anthropicKey) {
    return new AnthropicAdapter(anthropicKey);
  }

  // Try loading from config
  try {
    const configPath = path.join(process.cwd(), "../LambdaRLM/config.yaml");
    const content = fs.readFileSync(configPath, "utf8");
    const keyMatch = content.match(/api_key:\s*(\S+)/);
    const providerMatch = content.match(/provider:\s*(\S+)/);

    if (keyMatch?.[1]) {
      const provider = providerMatch?.[1]?.toLowerCase() || "openai";
      if (provider === "anthropic") {
        return new AnthropicAdapter(keyMatch[1]);
      }
      return new OpenAIAdapter(keyMatch[1]);
    }
  } catch { /* ignore */ }

  return null;
}

// ─────────────────────────────────────────────────────────────────
// Agentic LLM with Tool Calls
// ─────────────────────────────────────────────────────────────────

// Tool definitions for OpenAI function calling (also used by Anthropic)
const REPL_TOOLS = [
  // === EVALUATION ===
  {
    type: "function" as const,
    function: {
      name: "eval_lisp",
      description: "Evaluate a Lisp s-expression and return the result. Use this to run code, test functions, or compute values.",
      parameters: {
        type: "object",
        properties: {
          code: { type: "string", description: "The Lisp s-expression to evaluate, e.g. '(+ 1 2)' or '(define (fact n) (if (<= n 1) 1 (* n (fact (- n 1)))))'" }
        },
        required: ["code"]
      }
    }
  },
  {
    type: "function" as const,
    function: {
      name: "get_definitions",
      description: "Get a list of all user-defined functions and variables in the current session.",
      parameters: { type: "object", properties: {} }
    }
  },
  {
    type: "function" as const,
    function: {
      name: "inspect_value",
      description: "Look up the current value of a defined variable or function.",
      parameters: {
        type: "object",
        properties: {
          name: { type: "string", description: "The name of the variable or function to inspect" }
        },
        required: ["name"]
      }
    }
  },

  // === DEBUGGING ===
  {
    type: "function" as const,
    function: {
      name: "debug_load",
      description: "Load an expression into the debugger for step-by-step execution. This starts a debug session.",
      parameters: {
        type: "object",
        properties: {
          code: { type: "string", description: "The Lisp expression to debug" }
        },
        required: ["code"]
      }
    }
  },
  {
    type: "function" as const,
    function: {
      name: "debug_step",
      description: "Execute N steps in the debugger. Returns the current state after stepping.",
      parameters: {
        type: "object",
        properties: {
          count: { type: "number", description: "Number of steps to execute (default 1)" }
        }
      }
    }
  },
  {
    type: "function" as const,
    function: {
      name: "debug_run",
      description: "Run the debugger to completion or until a breakpoint is hit.",
      parameters: { type: "object", properties: {} }
    }
  },
  {
    type: "function" as const,
    function: {
      name: "debug_goto",
      description: "Jump to a specific step in the execution trace (time travel debugging).",
      parameters: {
        type: "object",
        properties: {
          step: { type: "number", description: "The step number to jump to" }
        },
        required: ["step"]
      }
    }
  },
  {
    type: "function" as const,
    function: {
      name: "debug_state",
      description: "Get the current debugger state including control, stack depth, and step number.",
      parameters: { type: "object", properties: {} }
    }
  },
  {
    type: "function" as const,
    function: {
      name: "debug_stack",
      description: "Get the current call stack showing all continuation frames.",
      parameters: { type: "object", properties: {} }
    }
  },
  {
    type: "function" as const,
    function: {
      name: "debug_trace",
      description: "Get the execution trace showing all recorded steps.",
      parameters: {
        type: "object",
        properties: {
          start: { type: "number", description: "Start index (default 0)" },
          count: { type: "number", description: "Number of steps to show (default 20)" }
        }
      }
    }
  },

  // === BREAKPOINTS ===
  {
    type: "function" as const,
    function: {
      name: "set_breakpoint",
      description: "Set a breakpoint. Types: 'step' (break at step N), 'expr' (break on expression type), 'effect' (break on effect operation).",
      parameters: {
        type: "object",
        properties: {
          type: { type: "string", enum: ["step", "expr", "effect"], description: "Breakpoint type" },
          condition: { type: "string", description: "Condition: step number, expression tag (If, App, Define, etc.), or effect name" }
        },
        required: ["type", "condition"]
      }
    }
  },
  {
    type: "function" as const,
    function: {
      name: "list_breakpoints",
      description: "List all set breakpoints.",
      parameters: { type: "object", properties: {} }
    }
  },
  {
    type: "function" as const,
    function: {
      name: "delete_breakpoint",
      description: "Delete a breakpoint by ID.",
      parameters: {
        type: "object",
        properties: {
          id: { type: "number", description: "Breakpoint ID to delete" }
        },
        required: ["id"]
      }
    }
  }
];

// Trace types for recording agentic interactions
interface LLMTraceEntry {
  type: "request" | "response" | "tool_call" | "tool_result" | "env_snapshot";
  timestamp: number;
  data: any;
}

interface LLMTrace {
  id: string;
  question: string;
  answer?: string;
  startTime: number;
  endTime?: number;
  entries: LLMTraceEntry[];
  toolCallCount: number;
}

// In-memory trace store (could be persisted later)
const llmTraces: Map<string, LLMTrace> = new Map();

function generateTraceId(): string {
  return `t-${Date.now().toString(36)}-${Math.random().toString(36).slice(2, 8)}`;
}

// Execute a tool call against the REPL
async function executeToolCall(
  toolName: string,
  args: any,
  replState: ReplState,
  trace: LLMTrace
): Promise<{ result: string; replState: ReplState }> {
  const startTime = Date.now();
  let result: string;

  // Helper to get active state (debug or last)
  const getActiveState = () => replState.debugState || replState.lastState;

  switch (toolName) {
    // === EVALUATION TOOLS ===
    case "eval_lisp": {
      const code = args.code as string;
      try {
        const { value, replState: newState } = await evalInRepl(code, replState);
        replState = newState;
        result = valToSexp(value);
      } catch (err: any) {
        result = `error: ${err.message}`;
      }
      break;
    }

    case "get_definitions": {
      if (replState.defs.length === 0) {
        result = "(no definitions yet)";
      } else {
        result = replState.defs.join("\n");
      }
      break;
    }

    case "inspect_value": {
      const name = args.name as string;
      const activeState = getActiveState();
      if (!activeState) {
        result = "(no state available)";
      } else {
        let ctx: any = activeState.env;
        let addr: number | undefined;
        while (ctx) {
          if (ctx.frame?.has?.(name)) {
            addr = ctx.frame.get(name);
            break;
          }
          ctx = ctx.parent;
        }
        if (addr !== undefined) {
          const val = activeState.store.read(addr);
          result = valToSexp(val);
        } else {
          result = `(binding '${name}' not found)`;
        }
      }
      break;
    }

    // === DEBUG TOOLS ===
    case "debug_load": {
      const code = args.code as string;
      try {
        replState = debugLoadExpr(code, replState);
        result = `Debug session started for: ${code.slice(0, 50)}${code.length > 50 ? "..." : ""}\nStep 0: ${controlToString(replState.debugState!.control)}\nUse debug_step to begin stepping.`;
      } catch (err: any) {
        result = `error: ${err.message}`;
      }
      break;
    }

    case "debug_step": {
      const count = (args.count as number) || 1;
      if (!replState.debugState) {
        result = "(no debug session - use debug_load first)";
      } else {
        const lines: string[] = [];
        for (let i = 0; i < count; i++) {
          const { result: stepResult, replState: newState } = debugStep(replState);
          replState = newState;
          if (!stepResult) {
            lines.push("(step failed)");
            break;
          }
          if (stepResult.tag === "Done") {
            lines.push(`DONE at step ${replState.stepCount}: ${valToSexp(stepResult.value)}`);
            break;
          }
          if (stepResult.tag === "Op") {
            lines.push(`EFFECT at step ${replState.stepCount}: ${stepResult.opcall.op}`);
            break;
          }
        }
        if (replState.debugState) {
          lines.push(`Step ${replState.stepCount}: ${controlToString(replState.debugState.control)}`);
          lines.push(`Stack depth: ${replState.debugState.kont.length}`);
        }
        result = lines.join("\n");
      }
      break;
    }

    case "debug_run": {
      if (!replState.debugState) {
        result = "(no debug session - use debug_load first)";
      } else {
        const startStep = replState.stepCount;
        replState = debugRun(replState);
        const endStep = replState.stepCount;

        if (replState.debugState) {
          result = `Ran ${endStep - startStep} steps. Stopped at step ${endStep}.\n${controlToString(replState.debugState.control)}\nStack depth: ${replState.debugState.kont.length}`;
        } else {
          // Completed - get the last trace entry to find the result
          const lastTrace = replState.trace[replState.trace.length - 1];
          if (lastTrace?.state.control.tag === "Val") {
            result = `Completed after ${endStep} steps. Result: ${valToSexp(lastTrace.state.control.v)}`;
          } else {
            result = `Completed after ${endStep} steps.`;
          }
        }
      }
      break;
    }

    case "debug_goto": {
      const step = args.step as number;
      if (replState.trace.length === 0) {
        result = "(no trace recorded - run the program first)";
      } else {
        replState = debugGoto(replState, step);
        if (replState.debugState) {
          result = `Jumped to step ${replState.stepCount}.\n${controlToString(replState.debugState.control)}\nStack depth: ${replState.debugState.kont.length}`;
        } else {
          result = `Failed to jump to step ${step}. Available: 0-${replState.trace[replState.trace.length - 1].step}`;
        }
      }
      break;
    }

    case "debug_state": {
      if (!replState.debugState) {
        result = "(no debug session active)";
      } else {
        result = [
          `Step: ${replState.stepCount}`,
          `Control: ${controlToString(replState.debugState.control)}`,
          `Stack depth: ${replState.debugState.kont.length}`,
          `Handlers: ${replState.debugState.handlers.length}`,
          `Trace length: ${replState.trace.length}`,
        ].join("\n");
      }
      break;
    }

    case "debug_stack": {
      const activeState = getActiveState();
      if (!activeState) {
        result = "(no state available)";
      } else {
        const kont = activeState.kont;
        if (kont.length === 0) {
          result = "(empty stack)";
        } else {
          const lines = [`Stack depth: ${kont.length}`];
          for (let i = kont.length - 1; i >= 0; i--) {
            lines.push(frameToString(kont[i], i));
          }
          result = lines.join("\n");
        }
      }
      break;
    }

    case "debug_trace": {
      const start = (args.start as number) || 0;
      const count = (args.count as number) || 20;
      if (replState.trace.length === 0) {
        result = "(no trace recorded)";
      } else {
        const lines = [`Trace (${replState.trace.length} steps recorded):`];
        const end = Math.min(start + count, replState.trace.length);
        for (let i = start; i < end; i++) {
          const t = replState.trace[i];
          const marker = t.step === replState.stepCount ? " <-- current" : "";
          lines.push(`  [${t.step}] ${t.controlSummary} | stack=${t.stackDepth}${marker}`);
        }
        if (end < replState.trace.length) {
          lines.push(`  ... (${replState.trace.length - end} more)`);
        }
        result = lines.join("\n");
      }
      break;
    }

    // === BREAKPOINT TOOLS ===
    case "set_breakpoint": {
      const type = args.type as Breakpoint["type"];
      const condition = args.condition as string;
      if (!["step", "expr", "effect"].includes(type)) {
        result = "Invalid breakpoint type. Use: step, expr, or effect";
      } else {
        const bp: Breakpoint = {
          id: replState.nextBreakpointId++,
          type,
          condition: type === "step" ? parseInt(condition) : condition,
          enabled: true,
        };
        replState.breakpoints.push(bp);
        result = `Breakpoint ${bp.id} added: ${type} = ${bp.condition}`;
      }
      break;
    }

    case "list_breakpoints": {
      if (replState.breakpoints.length === 0) {
        result = "No breakpoints set.";
      } else {
        const lines = ["Breakpoints:"];
        for (const bp of replState.breakpoints) {
          lines.push(`  ${bp.id}: ${bp.type} = ${bp.condition} [${bp.enabled ? "enabled" : "disabled"}]`);
        }
        result = lines.join("\n");
      }
      break;
    }

    case "delete_breakpoint": {
      const id = args.id as number;
      const idx = replState.breakpoints.findIndex(bp => bp.id === id);
      if (idx < 0) {
        result = `Breakpoint ${id} not found.`;
      } else {
        replState.breakpoints.splice(idx, 1);
        result = `Breakpoint ${id} deleted.`;
      }
      break;
    }

    default:
      result = `unknown tool: ${toolName}`;
  }

  // Record tool result in trace
  trace.entries.push({
    type: "tool_result",
    timestamp: Date.now(),
    data: { toolName, args, result, durationMs: Date.now() - startTime }
  });

  return { result, replState };
}

// Main agentic loop
async function runAgenticQuery(
  question: string,
  replState: ReplState,
  adapter: LLMAdapter,
  maxIterations = 15
): Promise<{ answer: string; traceId: string; replState: ReplState }> {
  const traceId = generateTraceId();
  const trace: LLMTrace = {
    id: traceId,
    question,
    startTime: Date.now(),
    entries: [],
    toolCallCount: 0
  };

  // System prompt explaining the environment with debugging capabilities
  const systemPrompt = `You are an assistant with access to a Lisp REPL (Omega dialect) with FULL DEBUGGING capabilities.
You can evaluate Lisp code, inspect definitions, debug step-by-step, set breakpoints, and time-travel through execution.

Current definitions in the session:
${replState.defs.length > 0 ? replState.defs.join("\n") : "(none)"}

AVAILABLE PRIMITIVES:
- Arithmetic: +, -, *, /, modulo
- Comparison: =, <, >, <=, >=, eq?
- Logic: not, and, or
- List ops: cons, car, cdr, list, append, null?, pair?
- Forms: define, lambda, let, if, begin, quote

AVAILABLE TOOLS:
1. EVALUATION:
   - eval_lisp: Run Lisp code and get the result
   - get_definitions: See all defined functions
   - inspect_value: Look up a specific binding

2. DEBUGGING (step through execution):
   - debug_load: Load expression into debugger
   - debug_step: Execute N steps
   - debug_run: Run to completion/breakpoint
   - debug_goto: Jump to any step (time travel!)
   - debug_state: Current state info
   - debug_stack: View call stack
   - debug_trace: View execution trace

3. BREAKPOINTS:
   - set_breakpoint: Break on step/expr/effect
   - list_breakpoints: Show breakpoints
   - delete_breakpoint: Remove breakpoint

This is a metacircular Lisp - if you need a function that doesn't exist (like map, filter, reverse, length),
DEFINE IT YOURSELF using the primitives above. For example:
  (define (length lst) (if (null? lst) 0 (+ 1 (length (cdr lst)))))
  (define (map f lst) (if (null? lst) (list) (cons (f (car lst)) (map f (cdr lst)))))

When the user asks a question:
1. If it requires computation, use eval_lisp to run code
2. If you need helper functions that don't exist, define them first
3. If debugging is needed, use debug_load to step through execution
4. Test your code before giving the final answer
5. Provide a clear, concise final answer

Always verify your results with actual evaluation rather than guessing.`;

  const messages: any[] = [
    { role: "system", content: systemPrompt },
    { role: "user", content: question }
  ];

  // Record initial request
  trace.entries.push({
    type: "request",
    timestamp: Date.now(),
    data: { systemPrompt, question, adapter: adapter.name }
  });

  for (let iter = 0; iter < maxIterations; iter++) {
    // Call LLM through adapter
    const response = await adapter.chat(messages, REPL_TOOLS);

    if (!response.content && !response.toolCalls) {
      trace.entries.push({ type: "response", timestamp: Date.now(), data: { error: "No response from LLM", raw: response } });
      break;
    }

    // Record LLM response
    trace.entries.push({
      type: "response",
      timestamp: Date.now(),
      data: { content: response.content, toolCalls: response.toolCalls, finishReason: response.finishReason }
    });

    // Check if LLM wants to call tools
    if (response.toolCalls && response.toolCalls.length > 0) {
      // Add assistant message with tool calls (in OpenAI format for message history)
      const assistantMsg: any = { role: "assistant", content: response.content || null };
      assistantMsg.tool_calls = response.toolCalls.map(tc => ({
        id: tc.id,
        type: "function",
        function: { name: tc.name, arguments: JSON.stringify(tc.args) }
      }));
      messages.push(assistantMsg);

      for (const toolCall of response.toolCalls) {
        // Record tool call
        trace.entries.push({
          type: "tool_call",
          timestamp: Date.now(),
          data: { id: toolCall.id, name: toolCall.name, args: toolCall.args }
        });
        trace.toolCallCount++;

        // Execute the tool
        const { result, replState: newState } = await executeToolCall(toolCall.name, toolCall.args, replState, trace);
        replState = newState;

        // Add tool result to messages
        messages.push({
          role: "tool",
          tool_call_id: toolCall.id,
          content: result
        });
      }
      // Continue the loop to get next LLM response
      continue;
    }

    // No tool calls - this is the final answer
    if (response.content) {
      trace.answer = response.content;
      trace.endTime = Date.now();
      llmTraces.set(traceId, trace);
      return { answer: response.content, traceId, replState };
    }

    // Unexpected state
    break;
  }

  // Fallback if we hit max iterations or error
  trace.answer = "(no answer - max iterations or error)";
  trace.endTime = Date.now();
  llmTraces.set(traceId, trace);
  return { answer: trace.answer, traceId, replState };
}

// Format trace for display
function formatTrace(trace: LLMTrace, verbose = false): string {
  const lines: string[] = [];
  lines.push(`Trace ID: ${trace.id}`);
  lines.push(`Question: ${trace.question}`);
  lines.push(`Answer: ${trace.answer || "(none)"}`);
  lines.push(`Duration: ${trace.endTime ? trace.endTime - trace.startTime : "?"}ms`);
  lines.push(`Tool calls: ${trace.toolCallCount}`);

  if (verbose) {
    lines.push("");
    lines.push("─── Full Trace ───");
    for (const entry of trace.entries) {
      const ts = new Date(entry.timestamp).toISOString().slice(11, 23);
      switch (entry.type) {
        case "request":
          lines.push(`[${ts}] REQUEST`);
          lines.push(`  Question: ${entry.data.question}`);
          break;
        case "response":
          lines.push(`[${ts}] LLM RESPONSE`);
          if (entry.data.content) lines.push(`  Content: ${entry.data.content.slice(0, 200)}${entry.data.content.length > 200 ? "..." : ""}`);
          if (entry.data.tool_calls) lines.push(`  Tool calls: ${entry.data.tool_calls.length}`);
          break;
        case "tool_call":
          lines.push(`[${ts}] TOOL CALL: ${entry.data.name}`);
          lines.push(`  Args: ${JSON.stringify(entry.data.args)}`);
          break;
        case "tool_result":
          lines.push(`[${ts}] TOOL RESULT (${entry.data.durationMs}ms)`);
          lines.push(`  Result: ${entry.data.result.slice(0, 200)}${entry.data.result.length > 200 ? "..." : ""}`);
          break;
        case "env_snapshot":
          lines.push(`[${ts}] ENV SNAPSHOT`);
          lines.push(`  Defs: ${entry.data.defs?.length || 0}`);
          break;
      }
    }
  }

  return lines.join("\n");
}

// ─────────────────────────────────────────────────────────────────
// Configuration
// ─────────────────────────────────────────────────────────────────
const VERBOSE = process.argv.includes("--verbose") || process.argv.includes("-v");

// ─────────────────────────────────────────────────────────────────
// Session Persistence
// ─────────────────────────────────────────────────────────────────
const SESSION_DIR = path.join(process.env.HOME || process.env.USERPROFILE || ".", ".omega-sessions");
const SESSION_LOG_DIR = process.env.OMEGA_SESSION_DIR
  ? path.resolve(process.env.OMEGA_SESSION_DIR)
  : path.join(process.cwd(), ".omega-session");

function ensureSessionDir() {
  if (!fs.existsSync(SESSION_DIR)) {
    fs.mkdirSync(SESSION_DIR, { recursive: true });
  }
}

function getSessionPath(name: string): string {
  return path.join(SESSION_DIR, `repl-${name}.json`);
}

interface SessionData {
  defs: string[];
  debugMode: boolean;
  debugCode?: string;
  stepCount: number;
  traceLength: number;
  breakpoints: Breakpoint[];
  nextBreakpointId: number;
  recordingEnabled: boolean;
}

function saveSession(replState: ReplState, name: string): void {
  ensureSessionDir();
  const data: SessionData = {
    defs: replState.defs,
    debugMode: replState.debugMode,
    debugCode: replState.debugCode,
    stepCount: replState.stepCount,
    traceLength: replState.trace.length,
    breakpoints: replState.breakpoints,
    nextBreakpointId: replState.nextBreakpointId,
    recordingEnabled: replState.recordingEnabled,
  };
  fs.writeFileSync(getSessionPath(name), JSON.stringify(data, null, 2));
}

async function loadSession(name: string): Promise<ReplState | null> {
  const sessionPath = getSessionPath(name);
  if (!fs.existsSync(sessionPath)) {
    return null;
  }

  try {
    const data: SessionData = JSON.parse(fs.readFileSync(sessionPath, "utf8"));
    let replState = await initReplState();

    // Restore defines
    replState.defs = data.defs || [];
    replState.breakpoints = data.breakpoints || [];
    replState.nextBreakpointId = data.nextBreakpointId || 1;
    replState.recordingEnabled = data.recordingEnabled !== undefined ? data.recordingEnabled : true;

    // If there was a debug session, restore it
    if (data.debugMode && data.debugCode) {
      replState = debugLoadExpr(data.debugCode, replState);

      // Replay to the saved step
      let count = 0;
      while (replState.debugState && count < data.stepCount) {
        const { replState: newState } = debugStep(replState);
        replState = newState;
        count++;
        if (!replState.debugState) break;
      }
    }

    return replState;
  } catch (err) {
    console.error(`Error loading session: ${err}`);
    return null;
  }
}

// Parse command line args
function parseArgs(): { session?: string; cmd?: string; json?: boolean; file?: string } {
  const args = process.argv.slice(2);
  const result: { session?: string; cmd?: string; json?: boolean; file?: string } = {};

  for (let i = 0; i < args.length; i++) {
    if ((args[i] === "--session" || args[i] === "-s") && args[i + 1]) {
      result.session = args[++i];
    } else if ((args[i] === "--cmd" || args[i] === "-c") && args[i + 1]) {
      result.cmd = args[++i];
    } else if ((args[i] === "--json" || args[i] === "-j")) {
      result.json = true;
    } else if ((args[i] === "--file" || args[i] === "-f") && args[i + 1]) {
      result.file = args[++i];
    }
  }

  return result;
}

// ─────────────────────────────────────────────────────────────────
// S-expression extraction (handles multi-line)
// ─────────────────────────────────────────────────────────────────
function extractSexpressions(text: string): string[] {
  const results: string[] = [];
  let current = "";
  let depth = 0;
  let inString = false;
  let escape = false;

  for (const char of text) {
    if (escape) {
      current += char;
      escape = false;
      continue;
    }

    if (char === "\\") {
      current += char;
      escape = true;
      continue;
    }

    if (char === '"') {
      inString = !inString;
      current += char;
      continue;
    }

    if (inString) {
      current += char;
      continue;
    }

    if (char === "(") {
      if (depth === 0 && current.trim()) {
        // Push any non-paren content before this
        results.push(current.trim());
        current = "";
      }
      depth++;
      current += char;
    } else if (char === ")") {
      depth--;
      current += char;
      if (depth === 0) {
        results.push(current.trim());
        current = "";
      }
    } else {
      current += char;
    }
  }

  // Handle any remaining content (atoms, numbers, etc.)
  if (current.trim()) {
    results.push(current.trim());
  }

  return results.filter(s => s.length > 0);
}

// ─────────────────────────────────────────────────────────────────
// Pretty printing values as s-expressions
// ─────────────────────────────────────────────────────────────────
function valToSexp(v: Val): string {
  switch (v.tag) {
    case "Unit": return "null";
    case "Num": return String(v.n);
    case "Bool": return v.b ? "#t" : "#f";
    case "Str": {
      // Check if it's a serialized symbol from quote (workaround)
      if (v.s.startsWith('{"sym":"')) {
        try {
          const parsed = JSON.parse(v.s);
          if (parsed.sym) return parsed.sym;
        } catch { /* ignore */ }
      }
      return JSON.stringify(v.s);
    }
    case "Sym": return v.name;
    case "Vector": {
      // Check if it's a proper list (cons cells ending in null)
      const items: Val[] = [];
      let current: Val = v;
      while (current.tag === "Vector" && current.items.length === 2) {
        items.push(current.items[0]);
        current = current.items[1];
      }
      if (current.tag === "Unit") {
        // Proper list
        return `(${items.map(valToSexp).join(" ")})`;
      }
      // Improper list or vector
      if (items.length > 0) {
        return `(${items.map(valToSexp).join(" ")} . ${valToSexp(current)})`;
      }
      // Regular vector
      return `[${v.items.map(valToSexp).join(" ")}]`;
    }
    case "Closure": return `<closure (${(v as any).params?.join(" ") || "..."})>`;
    case "Native": return `<prim>`;
    case "Cont": return `<continuation>`;
    case "OracleProc": return `<oracle-proc>`;
    case "Meaning": {
      const m = v as any;
      const parts: string[] = ["meaning"];
      if (m.denotation) parts.push(`(denotation ${valToSexp(m.denotation)})`);
      if (m.confidence !== undefined) parts.push(`(confidence ${m.confidence})`);
      if (m.trace) parts.push(`(trace ${valToSexp(m.trace)})`);
      return `(${parts.join(" ")})`;
    }
    default:
      return JSON.stringify(v);
  }
}

// ─────────────────────────────────────────────────────────────────
// Debugging types
// ─────────────────────────────────────────────────────────────────
interface TraceRecord {
  step: number;
  state: State;
  controlSummary: string;
  stackDepth: number;
}

interface Breakpoint {
  id: number;
  type: "step" | "expr" | "effect";
  condition: string | number;
  enabled: boolean;
}

interface Snapshot {
  name: string;
  step: number;
  state: State;
  timestamp: Date;
}

// ─────────────────────────────────────────────────────────────────
// Persistent REPL state (accumulates defines)
// ─────────────────────────────────────────────────────────────────
interface ReplState {
  store: Store;
  env: State["env"];
  baseState: State;
  defs: string[];  // history of (define ...) forms
  lastState?: State;  // Last evaluated state for debugging
  lastError?: Error;  // Last error for debugging
  nativeRegistry: Map<string, Val>;
  solverRegistry: Map<string, Val>;
  sessionDir: string;
  sessionWriter?: SessionWriter;
  loadedSession?: SessionReader;
  jumpController?: JumpController;
  sessionState?: State;
  sessionSeq?: number;
  sessionName?: string;

  // Debug mode state
  debugMode: boolean;
  debugState?: State;  // Current debug state (stepping mode)
  stepCount: number;
  trace: TraceRecord[];
  breakpoints: Breakpoint[];
  nextBreakpointId: number;
  debugCode?: string;  // Code being debugged

  // Advanced debugger features
  snapshots: Map<string, Snapshot>;  // Named snapshots
  history: { step: number; state: State; control: string }[];  // Step history (limited)
  maxHistory: number;  // Max history entries (default 100)
  recordingEnabled: boolean;  // Toggle trace recording
  running: boolean;  // Execution control flag
}

async function initReplState(): Promise<ReplState> {
  const store0 = new COWStore();
  const prim = installPrims(store0);
  const baseState: State = {
    control: { tag: "Val", v: VUnit },
    env: prim.env,
    store: prim.store,
    kont: [],
    handlers: [],
  };
  const nativeRegistry = buildNativeRegistry(prim.store);
  const solverRegistry = buildSolverRegistry(prim.store);
  return {
    store: prim.store,
    env: prim.env,
    baseState,
    defs: [],
    debugMode: false,
    stepCount: 0,
    trace: [],
    breakpoints: [],
    nextBreakpointId: 1,
    snapshots: new Map(),
    history: [],
    maxHistory: 100,
    recordingEnabled: true,
    running: false,
    nativeRegistry,
    solverRegistry,
    sessionDir: SESSION_LOG_DIR,
    sessionWriter: undefined,
    loadedSession: undefined,
    jumpController: undefined,
    sessionState: baseState,
    sessionSeq: undefined,
    sessionName: "current",
  };
}

// ─────────────────────────────────────────────────────────────────
// Evaluate one expression in the REPL state
// ─────────────────────────────────────────────────────────────────
function createReplRuntime(): RuntimeImpl {
  const snapshots = new SnapshotRepo();
  const receipts = new InMemoryReceiptStore("off");
  const scripted = process.env.OMEGA_SCRIPTED_ORACLE === "1";

  // Select adapter: OMEGA_ADAPTER=anthropic|openai (default: openai)
  const adapterType = process.env.OMEGA_ADAPTER ?? "openai";
  const model = process.env.OMEGA_MODEL; // Optional model override

  let baseOracle;
  if (scripted) {
    baseOracle = undefined;
  } else if (adapterType === "anthropic") {
    baseOracle = createAnthropicAdapter(model ? { model } : undefined);
  } else {
    baseOracle = createOpenAIAdapter(model ? { model } : undefined);
  }

  const oracle = baseOracle
    ? new DepthTrackingAdapter(baseOracle, 8)
    : new ScriptedOracleAdapter();

  return new RuntimeImpl(oracle as unknown as OracleAdapter, snapshots, receipts, {
    async commit(_payload: Val) { return VUnit; }
  });
}

async function evalInRepl(
  src: string,
  replState: ReplState,
  opts?: { baseState?: State }
): Promise<{ value: Val; replState: ReplState; state: State }> {
  const runtime = createReplRuntime();

  // Check if this is a define - extract the name
  const defineMatch = src.trim().match(/^\(define\s+(?:\(([\w\-\?\!]+)|([\w\-\?\!]+))/);
  const isDefine = !!defineMatch;
  const defineName = defineMatch?.[1] || defineMatch?.[2]; // function name or variable name

  let defsToUse = replState.defs;
  if (isDefine && defineName) {
    defsToUse = replState.defs.filter(d => {
      const prevMatch = d.trim().match(/^\(define\s+(?:\(([\w\-\?\!]+)|([\w\-\?\!]+))/);
      const prevName = prevMatch?.[1] || prevMatch?.[2];
      return prevName !== defineName;
    });
  }

  const allForms = [...defsToUse, src];
  const baseState = opts?.baseState ?? replState.sessionState ?? replState.baseState;
  const programSrc = `(begin\n${allForms.join("\n")}\n)`;
  const expr = compileTextToExpr(programSrc);
  const activeExpr = expr.tag === "Begin" && expr.exprs.length > 0
    ? expr.exprs[expr.exprs.length - 1]
    : expr;

  const state0: State = {
    control: { tag: "Expr", e: activeExpr },
    env: baseState?.env ?? replState.env,
    store: baseState?.store ?? replState.store,
    kont: [],
    handlers: baseState?.handlers ?? [],
  };

  const { value, state: finalState } = await runToCompletionWithState(runtime, state0, 100_000);
  normalizeCtxBindings(finalState.env as any);

  if (isDefine && defineName) {
    // Remove previous definition with same name to allow redefinition
    replState.defs = replState.defs.filter(d => {
      const prevMatch = d.trim().match(/^\(define\s+(?:\(([\w\-\?\!]+)|([\w\-\?\!]+))/);
      const prevName = prevMatch?.[1] || prevMatch?.[2];
      return prevName !== defineName;
    });
    replState.defs.push(src);
  }

  // Store final state for debugging and future evaluations
  replState.lastState = finalState;
  replState.baseState = finalState;
  replState.sessionState = finalState;
  replState.env = finalState.env;
  replState.store = finalState.store;
  replState.lastError = undefined;

  // For defines, return the symbol name (Lisp convention) instead of void
  // BUT only if it's JUST a define (not define + call)
  const hasMultipleExprs = extractSexpressions(src.trim()).length > 1;
  if (isDefine && defineName && !hasMultipleExprs) {
    return { value: { tag: "Sym", name: defineName } as Val, replState, state: finalState };
  }

  return { value, replState, state: finalState };
}

function normalizeCtxBindings(env?: any): void {
  let ctx = env;
  while (ctx) {
    if (ctx.frame && ctx.frame instanceof Map) {
      for (const [name, addr] of Array.from(ctx.frame.entries())) {
        const plain = typeof name === "string" ? name.replace(/\$bid#\d+$/, "") : name;
        if (typeof plain === "string" && plain !== name && !ctx.frame.has(plain)) {
          ctx.frame.set(plain, addr);
        }
      }
    }
    ctx = ctx.parent;
  }
}

function handleEffectExpression(src: string, replState: ReplState): { value: Val; state: State } {
  const baseState = replState.sessionState ?? replState.baseState;
  const opMatch = src.match(/^\(effect\s+([^\s\)]+)(.*)$/s);
  const op = opMatch?.[1] ?? "effect";
  const argsPreview = (opMatch?.[2] ?? "").trim();

  if (baseState) {
    replState.sessionWriter?.checkpoint(baseState, "llm_boundary");
  }
  replState.sessionWriter?.pushDepth();
  replState.sessionWriter?.step(`effect:${op}`);
  replState.sessionWriter?.effect(op, [argsPreview]);

  const receiptKey = replState.sessionWriter?.llmRequest("mock-llm", argsPreview, { op, args: argsPreview }) ?? "";
  const response = `mock:${op}`;
  if (receiptKey) {
    replState.sessionWriter?.llmResponse(
      receiptKey,
      response,
      { request: { op, args: argsPreview }, response: { content: response } },
      0
    );
  }

  const finalState: State = {
    control: { tag: "Val", v: { tag: "Str", s: response } as Val },
    env: baseState?.env ?? replState.env,
    store: baseState?.store ?? replState.store,
    kont: [],
    handlers: [],
  };

  normalizeCtxBindings(finalState.env as any);
  replState.sessionWriter?.checkpoint(finalState, "llm_boundary");
  replState.sessionWriter?.resume(response);
  replState.sessionWriter?.popDepth();
  replState.baseState = finalState;
  replState.sessionState = finalState;
  replState.lastState = finalState;

  return { value: finalState.control.v, state: finalState };
}

// ─────────────────────────────────────────────────────────────────
// Debugger utilities
// ─────────────────────────────────────────────────────────────────
function frameToString(frame: Frame, index: number): string {
  switch (frame.tag) {
    case "KIf": return `  ${index}: KIf (if ... conseq alt)`;
    case "KBegin": return `  ${index}: KBegin (${frame.rest.length} more forms)`;
    case "KDefine": return `  ${index}: KDefine (define ${frame.name} ...)`;
    case "KSet": return `  ${index}: KSet (set! ${frame.name} ...)`;
    case "KAppFun": return `  ${index}: KAppFun (evaluating function, ${frame.args.length} args pending)`;
    case "KAppArg": return `  ${index}: KAppArg (evaluated fn, ${frame.acc.length}/${frame.pending.length + frame.acc.length} args)`;
    case "KCall": return `  ${index}: KCall (in function body)`;
    case "KEffect": return `  ${index}: KEffect (${frame.op}, ${frame.acc.length}/${frame.pending.length + frame.acc.length} args)`;
    case "KHandleBoundary": return `  ${index}: KHandleBoundary (handler ${frame.hid})`;
    case "KHandleReturn": return `  ${index}: KHandleReturn (${frame.mode} handler ${frame.hid})`;
    case "KMatch": return `  ${index}: KMatch (${frame.clauses.length} clauses)`;
    default: return `  ${index}: ${(frame as any).tag ?? "Unknown"}`;
  }
}

function envToBindings(env: Env): Array<{ name: string; addr: number }> {
  const bindings: Array<{ name: string; addr: number }> = [];
  // Traverse the context chain (Ctx has frame: Map<string, Addr> and parent: Ctx)
  let ctx: any = env;
  const seen = new Set<string>();
  while (ctx) {
    if (ctx.frame && ctx.frame instanceof Map) {
      for (const [name, addr] of ctx.frame.entries()) {
        if (!seen.has(name)) {
          seen.add(name);
          bindings.push({ name, addr: addr as number });
        }
      }
    }
    ctx = ctx.parent;
  }
  return bindings.sort((a, b) => a.name.localeCompare(b.name));
}

// ─────────────────────────────────────────────────────────────────
// Debug mode helpers
// ─────────────────────────────────────────────────────────────────
function controlToString(ctrl: State["control"]): string {
  if (ctrl.tag === "Val") {
    return `Value: ${valToSexp(ctrl.v)}`;
  }
  if (ctrl.tag === "Expr") {
    const e = ctrl.e;
    switch (e.tag) {
      case "Lit": return `Expr: Lit(${JSON.stringify(e.value)})`;
      case "Var": return `Expr: Var(${e.name})`;
      case "App": return `Expr: App(...)`;
      case "If": return `Expr: If(...)`;
      case "Lambda": return `Expr: Lambda(${(e as any).params?.length || 0} params)`;
      case "Begin": return `Expr: Begin(${(e as any).exprs?.length || 0} exprs)`;
      case "Define": return `Expr: Define(${(e as any).name || "?"})`;
      case "Effect": return `Expr: Effect(${(e as any).op || "?"})`;
      default: return `Expr: ${e.tag}`;
    }
  }
  // Fallback for any future control types
  return `Control: ${(ctrl as any).tag}`;
}

function cloneState(s: State): State {
  return { ...s, kont: [...s.kont], handlers: [...s.handlers] };
}

function checkBreakpoints(state: State, stepCount: number, breakpoints: Breakpoint[]): Breakpoint | null {
  for (const bp of breakpoints) {
    if (!bp.enabled) continue;

    if (bp.type === "step" && stepCount === bp.condition) {
      return bp;
    }
    if (bp.type === "expr" && state.control.tag === "Expr") {
      const e = state.control.e;
      if (e.tag === bp.condition) return bp;
      if (e.tag === "Define" && (e as any).name === bp.condition) return bp;
      if (e.tag === "Var" && (e as any).name === bp.condition) return bp;
    }
    if (bp.type === "effect" && state.control.tag === "Expr") {
      const e = state.control.e;
      if (e.tag === "Effect" && (e as any).op === bp.condition) return bp;
    }
  }
  return null;
}

function debugLoadExpr(src: string, replState: ReplState): ReplState {
  // Build full program with accumulated defines
  const allForms = [...replState.defs, src];
  const fullProgram = `(begin\n${allForms.join("\n")}\n)`;
  const expr = compileTextToExpr(fullProgram);

  // Fresh state with primitives
  const store0 = new COWStore();
  const prim = installPrims(store0);

  const state0: State = {
    control: { tag: "Expr", e: expr },
    env: prim.env,
    store: prim.store,
    kont: [],
    handlers: [],
  };

  // Record initial state
  const trace: TraceRecord[] = [{
    step: 0,
    state: cloneState(state0),
    controlSummary: controlToString(state0.control),
    stackDepth: 0,
  }];

  return {
    ...replState,
    debugMode: true,
    debugState: state0,
    stepCount: 0,
    trace,
    debugCode: src,
  };
}

function debugStep(replState: ReplState): { result: StepOutcome | null; replState: ReplState } {
  if (!replState.debugState) {
    console.log("(no debug session - use :debug (expr) to start)");
    return { result: null, replState };
  }

  // Save to history (limited)
  if (replState.history.length >= replState.maxHistory) {
    replState.history.shift();
  }
  replState.history.push({
    step: replState.stepCount,
    state: cloneState(replState.debugState),
    control: controlToString(replState.debugState.control),
  });

  const result = stepOnce(replState.debugState);
  replState.stepCount++;

  if (result.tag === "State") {
    replState.debugState = result.state;

    // Record to trace (if recording enabled)
    if (replState.recordingEnabled) {
      replState.trace.push({
        step: replState.stepCount,
        state: cloneState(result.state),
        controlSummary: controlToString(result.state.control),
        stackDepth: result.state.kont.length,
      });
    }
  } else if (result.tag === "Done") {
    console.log(`\n=== DONE at step ${replState.stepCount} ===`);
    console.log(`Result: ${valToSexp(result.value)}`);
    console.log(`Trace: ${replState.trace.length} steps recorded. Use :trace to view, :goto N to jump.`);

    // Check if it was a define - add to defs
    if (replState.debugCode?.trim().startsWith("(define")) {
      replState.defs.push(replState.debugCode);
    }

    // Exit debug mode but KEEP the trace for post-mortem inspection
    replState.debugMode = false;
    replState.debugState = undefined;
    // Note: trace is preserved so :trace and :goto still work
  } else if (result.tag === "Op") {
    console.log(`\n=== EFFECT at step ${replState.stepCount} ===`);
    console.log(`Operation: ${result.opcall.op}`);
    console.log(`Args: ${result.opcall.args.map(a => valToSexp(a)).join(", ")}`);
    replState.debugState = result.state;
  }

  return { result, replState };
}

function debugRun(replState: ReplState, maxSteps = 100000): ReplState {
  if (!replState.debugState) {
    console.log("(no debug session - use :debug (expr) to start)");
    return replState;
  }

  let steps = 0;
  while (replState.debugState && steps < maxSteps) {
    // Check breakpoints BEFORE stepping (skip first step)
    if (steps > 0) {
      const bp = checkBreakpoints(replState.debugState, replState.stepCount, replState.breakpoints);
      if (bp) {
        console.log(`\n*** Breakpoint ${bp.id} hit at step ${replState.stepCount} ***`);
        console.log(`  Type: ${bp.type}, Condition: ${bp.condition}`);
        break;
      }
    }

    const { result, replState: newState } = debugStep(replState);
    replState = newState;
    steps++;

    if (!result || result.tag === "Done" || result.tag === "Op") {
      break;
    }
  }

  if (steps >= maxSteps && replState.debugState) {
    console.log(`\nStopped after ${maxSteps} steps (use :run to continue)`);
  }

  return replState;
}

function debugGoto(replState: ReplState, targetStep: number): ReplState {
  if (replState.trace.length === 0) {
    console.log("(no trace recorded - run the program first)");
    return replState;
  }

  const record = replState.trace.find(r => r.step === targetStep);
  if (!record) {
    console.log(`Step ${targetStep} not found. Available: 0-${replState.trace[replState.trace.length - 1].step}`);
    return replState;
  }

  replState.debugState = cloneState(record.state);
  replState.stepCount = record.step;
  replState.debugMode = true;
  console.log(`Jumped to step ${replState.stepCount}`);
  return replState;
}

function printDebugState(replState: ReplState): void {
  if (!replState.debugState) {
    console.log("(no debug session active)");
    return;
  }

  console.log(`\n─── Step ${replState.stepCount} ───`);
  console.log(`Control: ${controlToString(replState.debugState.control)}`);
  console.log(`Stack depth: ${replState.debugState.kont.length}`);
  console.log(`Handlers: ${replState.debugState.handlers.length}`);
}

function printTrace(replState: ReplState, start = 0, count = 20): void {
  if (replState.trace.length === 0) {
    console.log("(no trace recorded)");
    return;
  }

  console.log(`\nTrace (${replState.trace.length} steps recorded):`);
  const end = Math.min(start + count, replState.trace.length);
  for (let i = start; i < end; i++) {
    const t = replState.trace[i];
    const marker = t.step === replState.stepCount ? " <-- current" : "";
    console.log(`  [${t.step}] ${t.controlSummary} | stack=${t.stackDepth}${marker}`);
  }
  if (end < replState.trace.length) {
    console.log(`  ... (${replState.trace.length - end} more, use ':trace ${end} ${count}' to see more)`);
  }
}

// ─────────────────────────────────────────────────────────────────
// Unified command processor (for both interactive and session mode)
// ─────────────────────────────────────────────────────────────────
async function processReplCommand(
  trimmed: string,
  replState: ReplState
): Promise<{ replState: ReplState; output: string; shouldExit: boolean }> {
  const output: string[] = [];
  const log = (...msgs: any[]) => output.push(msgs.join(" "));
  let shouldExit = false;

  // Helper to get the active state (debug or last)
  const getActiveState = () => replState.debugState || replState.sessionState || replState.lastState;

  // Session management commands
  const sessionsPath = path.join(replState.sessionDir, "sessions");

  if (trimmed === ":session list") {
    if (!fs.existsSync(sessionsPath)) {
      log("No saved sessions.");
      return { replState, output: output.join("\n"), shouldExit };
    }
    const files = fs.readdirSync(sessionsPath).filter(f => f.endsWith(".jsonl"));
    if (files.length === 0) {
      log("No saved sessions.");
    } else {
      log("Saved sessions:");
      for (const f of files) {
        const name = f.replace(".jsonl", "");
        const indexPath = path.join(sessionsPath, `${name}.index.json`);
        if (fs.existsSync(indexPath)) {
          const idx = JSON.parse(fs.readFileSync(indexPath, "utf8"));
          log(`  ${name} (${idx.eventCount ?? 0} events, ${(idx.checkpoints ?? []).length} checkpoints)`);
        } else {
          log(`  ${name}`);
        }
      }
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  if (trimmed.startsWith(":session save ")) {
    const name = trimmed.slice(14).trim();
    if (!name) {
      log("Usage: :session save <name>");
      return { replState, output: output.join("\n"), shouldExit };
    }
    fs.mkdirSync(sessionsPath, { recursive: true });
    try {
      replState.sessionWriter?.close();
      const baseName = replState.sessionWriter?.getSessionId() || replState.sessionName || "current";
      const srcJsonl = path.join(sessionsPath, `${baseName}.jsonl`);
      const srcIndex = path.join(sessionsPath, `${baseName}.index.json`);
      const dstJsonl = path.join(sessionsPath, `${name}.jsonl`);
      const dstIndex = path.join(sessionsPath, `${name}.index.json`);
      if (fs.existsSync(srcJsonl)) fs.copyFileSync(srcJsonl, dstJsonl);
      if (fs.existsSync(srcIndex)) fs.copyFileSync(srcIndex, dstIndex);
      log(`Session saved as '${name}'`);
    } catch (err: any) {
      log(`Error saving session: ${err.message}`);
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  if (trimmed.startsWith(":session fork ")) {
    const name = trimmed.slice(14).trim();
    if (!name) {
      log("Usage: :session fork <name>");
      return { replState, output: output.join("\n"), shouldExit };
    }
    fs.mkdirSync(sessionsPath, { recursive: true });
    const sourceName = replState.loadedSession
      ? (replState.sessionName || "current")
      : (replState.sessionWriter?.getSessionId() || replState.sessionName || "current");
    const sourceJsonl = path.join(sessionsPath, `${sourceName}.jsonl`);
    const sourceIndex = path.join(sessionsPath, `${sourceName}.index.json`);

    if (!fs.existsSync(sourceJsonl) || !fs.existsSync(sourceIndex)) {
      log(`Session '${sourceName}' not found`);
      return { replState, output: output.join("\n"), shouldExit };
    }

    try {
      const rawLines = fs.readFileSync(sourceJsonl, "utf8").split(/\r?\n/).filter(Boolean);
      const events = rawLines.map(line => JSON.parse(line));
      const maxSeq = events.reduce((max, e) => (typeof (e as any).seq === "number" ? Math.max(max, (e as any).seq) : max), -1);
      const cutoffSeq = replState.sessionSeq !== undefined ? Math.min(replState.sessionSeq, maxSeq) : maxSeq;
      const trimmedEvents = events.filter(e => !("seq" in e) || (e as any).seq <= cutoffSeq);

      if (trimmedEvents.length > 0 && (trimmedEvents[0] as any).type === "session") {
        trimmedEvents[0] = { ...(trimmedEvents[0] as any), id: name, created: new Date().toISOString() };
      } else {
        trimmedEvents.unshift({ type: "session", version: 1, id: name, created: new Date().toISOString() });
      }

      const forkJsonl = path.join(sessionsPath, `${name}.jsonl`);
      const checkpointOffsets = new Map<number, number>();
      let offset = 0;
      const lines: string[] = [];
      for (const event of trimmedEvents) {
        const line = `${JSON.stringify(event)}\n`;
        if ((event as any).type === "checkpoint" && typeof (event as any).seq === "number") {
          checkpointOffsets.set((event as any).seq, offset);
        }
        lines.push(line);
        offset += Buffer.byteLength(line, "utf8");
      }
      fs.writeFileSync(forkJsonl, lines.join(""));

      const indexData = JSON.parse(fs.readFileSync(sourceIndex, "utf8"));
      const checkpoints = (indexData.checkpoints ?? [])
        .filter((cp: any) => cp.seq <= cutoffSeq)
        .map((cp: any) => ({
          ...cp,
          byteOffset: checkpointOffsets.get(cp.seq) ?? cp.byteOffset,
        }));
      const stateIds = new Set(checkpoints.map((cp: any) => cp.stateId));
      const states: Record<string, any> = {};
      for (const id of stateIds) {
        if (indexData.states?.[id] !== undefined) {
          states[id] = indexData.states[id];
        }
      }
      const receiptKeys = new Set(trimmedEvents.filter(e => (e as any).type === "llm_resp").map(e => (e as any).receiptKey));
      const receipts: Record<string, any> = {};
      for (const key of receiptKeys) {
        if (indexData.receipts?.[key] !== undefined) {
          receipts[key] = indexData.receipts[key];
        }
      }

      const forkIndex: SessionIndex = {
        sessionId: name,
        eventCount: cutoffSeq >= 0 ? cutoffSeq + 1 : 0,
        checkpoints,
        states,
        receipts,
      };

      const forkIndexPath = path.join(sessionsPath, `${name}.index.json`);
      fs.writeFileSync(forkIndexPath, JSON.stringify(forkIndex, null, 2));

      const forkReader = new SessionReader(forkJsonl, forkIndexPath, replState.nativeRegistry, replState.solverRegistry);
      await forkReader.loadAll();

      replState.loadedSession = forkReader;
      replState.jumpController = new JumpController(forkReader);

      replState.sessionWriter?.close();
      replState.sessionWriter = new SessionWriter(replState.sessionDir, name, { append: true, index: forkIndex });
      replState.sessionName = name;
      replState.sessionSeq = cutoffSeq >= 0 ? cutoffSeq : undefined;

      log(`Session forked as '${name}'`);
    } catch (err: any) {
      log(`Error forking session: ${err.message}`);
    }

    return { replState, output: output.join("\n"), shouldExit };
  }

  if (trimmed.startsWith(":session load ")) {
    const name = trimmed.slice(14).trim();
    if (!name) {
      log("Usage: :session load <name>");
      return { replState, output: output.join("\n"), shouldExit };
    }
    const eventFile = path.join(sessionsPath, `${name}.jsonl`);
    const indexFile = path.join(sessionsPath, `${name}.index.json`);
    if (!fs.existsSync(eventFile) || !fs.existsSync(indexFile)) {
      log(`Session '${name}' not found`);
      return { replState, output: output.join("\n"), shouldExit };
    }
    try {
      const reader = new SessionReader(eventFile, indexFile, replState.nativeRegistry, replState.solverRegistry);
      await reader.loadAll();
      replState.loadedSession = reader;
      replState.jumpController = new JumpController(reader);
      replState.sessionName = name;
      replState.defs = reader.getAllEvents()
        .filter(e => (e as any).type === "input" && (e as any).code?.trim?.().startsWith("(define"))
        .map(e => (e as any).code as string);
      log(`Loaded session '${name}' (${reader.getEventCount()} events)`);
      log("Use :session goto <seq> to jump, :session trace to view");
    } catch (err: any) {
      log(`Error loading session: ${err.message}`);
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  if (trimmed.startsWith(":session goto ")) {
    const seq = parseInt(trimmed.slice(14).trim(), 10);
    if (!replState.loadedSession || !replState.jumpController) {
      log("No session loaded. Use :session load <name> first.");
      return { replState, output: output.join("\n"), shouldExit };
    }
    if (isNaN(seq)) {
      log("Usage: :session goto <seq>");
      return { replState, output: output.join("\n"), shouldExit };
    }
    try {
      const result = await replState.jumpController.jumpTo(seq);
      replState.sessionState = result.state;
      replState.baseState = result.state;
      replState.lastState = result.state;
      replState.sessionSeq = result.seq;
      replState.debugMode = true;
      if (replState.loadedSession) {
        replState.defs = replState.loadedSession.getAllEvents()
          .filter(e => (e as any).type === "input" && typeof (e as any).seq === "number" && (e as any).seq <= seq && (e as any).code?.trim?.().startsWith("(define"))
          .map(e => (e as any).code as string);
      }
      log(`Jumped to seq ${result.seq}`);
      log(`  Replayed ${result.replayedSteps} steps`);
      log(`  Used ${result.usedReceipts.length} cached LLM receipts`);
      log(`  Control: ${controlToString(result.state.control)}`);
    } catch (err: any) {
      log(err.message);
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  if (trimmed === ":session checkpoints") {
    if (!replState.loadedSession) {
      log("No session loaded.");
    } else {
      const cps = replState.loadedSession.getCheckpoints();
      log(`Checkpoints (${cps.length}):`);
      for (const cp of cps) {
        log(`  [${String(cp.seq).padStart(3, "0")}] ${cp.reason} (state: ${cp.stateId})`);
      }
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  if (trimmed === ":session trace" || trimmed.startsWith(":session trace ")) {
    if (!replState.loadedSession) {
      log("No session loaded. Use :session load <name> first.");
    } else {
      const events = replState.loadedSession.getAllEvents();
      const verbose = trimmed.includes("--verbose") || trimmed.includes("-v");
      const traceOutput = renderTrace(events, verbose ? { showTime: true } : undefined);
      log(traceOutput);
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  if (trimmed === ":session resume") {
    if (!replState.loadedSession || replState.sessionState === undefined) {
      log("No state to resume from. Use :session goto <seq> first.");
      return { replState, output: output.join("\n"), shouldExit };
    }
    const currentSeq = replState.sessionSeq ?? 0;
    const cps = replState.loadedSession.getCheckpoints();
    const next = cps.find(cp => cp.seq > currentSeq);
    if (!next) {
      log("No later checkpoint to resume to.");
      return { replState, output: output.join("\n"), shouldExit };
    }
    const state = replState.loadedSession.getCheckpointState(next.stateId);
    replState.sessionState = state;
    replState.baseState = state;
    replState.lastState = state;
    replState.sessionSeq = next.seq;
    const events = replState.loadedSession.getEventsInRange(currentSeq + 1, next.seq);
    const usedReceipts = events.filter(e => (e as any).type === "llm_resp").map(e => (e as any).receiptKey);
    if (usedReceipts.length > 0) {
      log(`Used ${usedReceipts.length} cached receipts`);
    }
    log(`Resumed to seq ${next.seq}`);
    return { replState, output: output.join("\n"), shouldExit };
  }

  // Commands
  if (trimmed === ":quit" || trimmed === ":q") {
    log("Goodbye!");
    shouldExit = true;
    return { replState, output: output.join("\n"), shouldExit };
  }

  if (trimmed === ":help" || trimmed === ":h") {
    log("Commands:");
    log("  :help, :h      — show this help");
    log("");
    log("  EVALUATION:");
    log("  (expr)         — evaluate expression immediately");
    log("  :loadfile <path> — load and evaluate code from file");
    log("");
    log("  SESSION:");
    log("  :session list            - list saved sessions");
    log("  :session save <name>     - save current session");
    log("  :session fork <name>     - fork session from current point");
    log("  :session load <name>     - load a saved session");
    log("  :session goto <seq>      - jump to a checkpoint or event");
    log("  :session trace           - render session trace");
    log("  :session checkpoints     - list checkpoints");
    log("  :session resume          - resume to next checkpoint");
    log("");
    log("  DEBUGGING:");
    log("  :debug (expr)  — load expression into debugger (step mode)");
    log("  :step [N]      — execute N steps (default 1)");
    log("  :run           — run until breakpoint or completion");
    log("  :stop          — stop execution");
    log("  :goto <N>      — jump to step N in recorded trace");
    log("  :trace [s] [n] — show trace (start at s, show n steps)");
    log("  :state         — show current debug state");
    log("");
    log("  BREAKPOINTS:");
    log("  :break step <N>    — break at step N");
    log("  :break expr <tag>  — break on expression type");
    log("  :break effect <op> — break on effect operation");
    log("  :breaks            — list all breakpoints");
    log("  :delbreak <id>     — delete breakpoint");
    log("  :toggle <id>       — toggle breakpoint on/off");
    log("");
    log("  SNAPSHOTS:");
    log("  :save <name>       — save current state as snapshot");
    log("  :restore <name>    — restore snapshot");
    log("  :snapshots         — list all snapshots");
    log("  :export <name> <file> — export snapshot to file");
    log("");
    log("  HISTORY & TIME TRAVEL:");
    log("  :back [N]          — go back N steps in history");
    log("  :history [N]       — show last N history entries");
    log("  :record on|off     — toggle trace recording");
    log("  :dump <file>       — save trace to file");
    log("  :replay <file>     — load and replay dump");
    log("");
    log("  INSPECTION:");
    log("  :env           — show current environment bindings");
    log("  :env <name>    — lookup specific binding");
    log("  :defs          — show defined functions");
    log("  :stack         — show call stack");
    log("  :frame <n>     — inspect frame N");
    log("  :control       — show current control (expr or value)");
    log("");
    log("  LLM (Agentic):");
    log("  :ask <question>    — ask LLM (uses tool calls to eval code iteratively)");
    log("  :traces            — list recent LLM interaction traces");
    log("  :trace <id>        — show trace summary");
    log("  :trace <id> -v     — show full trace (prompts, responses, tool calls)");
    log("");
    log("  OPR (Omega Protocol Runtime):");
    log("  :opr-list          — list available OPR kernels");
    log("  :opr-run <kernel> <json> — run kernel with program JSON");
    log("  :opr-receipts      — show OPR receipt chain for current session");
    log("  :opr-verify [file] — verify OPR receipt chain integrity");
    log("");
    log("  :quit, :q      — exit REPL");
    return { replState, output: output.join("\n"), shouldExit };
  }

  // :debug - load expression into debugger
  if (trimmed.startsWith(":debug ")) {
    const expr = trimmed.slice(7).trim();
    if (!expr) {
      log("Usage: :debug (expression)");
    } else {
      replState = debugLoadExpr(expr, replState);
      log(`Debug session started. Use :step to begin stepping.`);
      printDebugState(replState);
      output.push(...getConsoleOutput(() => printDebugState(replState)));
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  // :step - step in debugger
  if (trimmed === ":step" || trimmed.startsWith(":step ")) {
    const parts = trimmed.split(/\s+/);
    const count = parseInt(parts[1]) || 1;

    for (let i = 0; i < count; i++) {
      const { result, replState: newState } = debugStep(replState);
      replState = newState;
      if (!result || result.tag === "Done" || result.tag === "Op") break;
    }

    if (replState.debugState) {
      output.push(...getConsoleOutput(() => printDebugState(replState)));
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  // :run - run to completion or breakpoint
  if (trimmed === ":run" || trimmed === ":continue" || trimmed === ":c") {
    replState = debugRun(replState);
    if (replState.debugState) {
      output.push(...getConsoleOutput(() => printDebugState(replState)));
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  // :goto - jump to step in trace
  if (trimmed.startsWith(":goto ") || trimmed.startsWith(":g ")) {
    const parts = trimmed.split(/\s+/);
    const step = parseInt(parts[1]);
    if (isNaN(step)) {
      log("Usage: :goto <step-number>");
    } else {
      replState = debugGoto(replState, step);
      if (replState.debugState) {
        output.push(...getConsoleOutput(() => printDebugState(replState)));
      }
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  // :trace - show debug trace (no args or numeric args only)
  // Note: :trace <id> and :trace --last are handled later for LLM traces
  if (trimmed === ":trace") {
    output.push(...getConsoleOutput(() => printTrace(replState, 0, 20)));
    return { replState, output: output.join("\n"), shouldExit };
  }
  if (trimmed.startsWith(":trace ")) {
    const parts = trimmed.split(/\s+/);
    // Only handle if first arg is a number (debug trace pagination)
    const firstArg = parts[1];
    if (firstArg && /^\d+$/.test(firstArg)) {
      const start = parseInt(parts[1]) || 0;
      const count = parseInt(parts[2]) || 20;
      output.push(...getConsoleOutput(() => printTrace(replState, start, count)));
      return { replState, output: output.join("\n"), shouldExit };
    }
    // Otherwise fall through to LLM trace handler below
  }

  // :state - show debug state
  if (trimmed === ":state" || trimmed === ":st") {
    output.push(...getConsoleOutput(() => printDebugState(replState)));
    return { replState, output: output.join("\n"), shouldExit };
  }

  // :break - add breakpoint
  if (trimmed.startsWith(":break ") || trimmed.startsWith(":b ")) {
    const parts = trimmed.split(/\s+/);
    const type = parts[1] as Breakpoint["type"];
    const condition = parts[2];

    if (!type || !condition) {
      log("Usage: :break step <N> | :break expr <tag> | :break effect <op>");
    } else if (!["step", "expr", "effect"].includes(type)) {
      log("Invalid breakpoint type. Use: step, expr, or effect");
    } else {
      const bp: Breakpoint = {
        id: replState.nextBreakpointId++,
        type,
        condition: type === "step" ? parseInt(condition) : condition,
        enabled: true,
      };
      replState.breakpoints.push(bp);
      log(`Breakpoint ${bp.id} added: ${type} = ${bp.condition}`);
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  // :breaks - list breakpoints
  if (trimmed === ":breaks" || trimmed === ":breakpoints") {
    if (replState.breakpoints.length === 0) {
      log("No breakpoints set.");
    } else {
      log("Breakpoints:");
      for (const bp of replState.breakpoints) {
        log(`  ${bp.id}: ${bp.type} = ${bp.condition} [${bp.enabled ? "enabled" : "disabled"}]`);
      }
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  // :delbreak - delete breakpoint
  if (trimmed.startsWith(":delbreak ") || trimmed.startsWith(":del ")) {
    const parts = trimmed.split(/\s+/);
    const id = parseInt(parts[1]);
    if (isNaN(id)) {
      log("Usage: :delbreak <id>");
    } else {
      const idx = replState.breakpoints.findIndex(bp => bp.id === id);
      if (idx < 0) {
        log(`Breakpoint ${id} not found.`);
      } else {
        replState.breakpoints.splice(idx, 1);
        log(`Breakpoint ${id} deleted.`);
      }
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  // :toggle - toggle breakpoint
  if (trimmed.startsWith(":toggle ")) {
    const parts = trimmed.split(/\s+/);
    const id = parseInt(parts[1]);
    if (isNaN(id)) {
      log("Usage: :toggle <id>");
    } else {
      const bp = replState.breakpoints.find(b => b.id === id);
      if (!bp) {
        log(`Breakpoint ${id} not found.`);
      } else {
        bp.enabled = !bp.enabled;
        log(`Breakpoint ${id} ${bp.enabled ? "enabled" : "disabled"}.`);
      }
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  // :save - save snapshot
  if (trimmed.startsWith(":save ")) {
    const name = trimmed.slice(6).trim();
    if (!name) {
      log("Usage: :save <name>");
    } else if (!replState.debugState) {
      log("(no debug session - use :debug (expr) first)");
    } else {
      const snapshot: Snapshot = {
        name,
        step: replState.stepCount,
        state: cloneState(replState.debugState),
        timestamp: new Date(),
      };
      replState.snapshots.set(name, snapshot);
      log(`Saved snapshot '${name}' at step ${replState.stepCount}`);
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  // :restore - restore snapshot
  if (trimmed.startsWith(":restore ")) {
    const name = trimmed.slice(9).trim();
    if (!name) {
      log("Usage: :restore <name>");
    } else {
      const snapshot = replState.snapshots.get(name);
      if (!snapshot) {
        log(`Snapshot '${name}' not found.`);
      } else {
        replState.debugState = cloneState(snapshot.state);
        replState.stepCount = snapshot.step;
        replState.debugMode = true;
        log(`Loaded snapshot '${name}' (step ${replState.stepCount})`);
        output.push(...getConsoleOutput(() => printDebugState(replState)));
      }
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  // :snapshots - list snapshots
  if (trimmed === ":snapshots" || trimmed === ":snaps") {
    if (replState.snapshots.size === 0) {
      log("No snapshots saved.");
    } else {
      log("\nSnapshots:");
      for (const [name, snap] of replState.snapshots) {
        log(`  ${name}: step ${snap.step} (${snap.timestamp.toISOString()})`);
      }
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  // :export - export snapshot to file
  if (trimmed.startsWith(":export ")) {
    const parts = trimmed.slice(8).trim().split(/\s+/);
    const name = parts[0];
    const filepath = parts.slice(1).join(" ");
    if (!name || !filepath) {
      log("Usage: :export <snapshot-name> <filepath>");
    } else {
      const snapshot = replState.snapshots.get(name);
      if (!snapshot) {
        log(`Snapshot '${name}' not found.`);
      } else {
        const data = JSON.stringify({
          name: snapshot.name,
          step: snapshot.step,
          timestamp: snapshot.timestamp.toISOString(),
          controlTag: snapshot.state.control.tag,
          stackDepth: snapshot.state.kont.length,
        }, null, 2);
        try {
          fs.writeFileSync(filepath, data);
          log(`Exported snapshot metadata to ${filepath}`);
        } catch (err: any) {
          log(`Error exporting: ${err.message}`);
        }
      }
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  // :back - go back in history
  if (trimmed === ":back" || trimmed.startsWith(":back ")) {
    const parts = trimmed.split(/\s+/);
    const steps = parseInt(parts[1]) || 1;
    if (replState.history.length === 0) {
      log("No history available.");
    } else {
      const targetIdx = Math.max(0, replState.history.length - steps);
      const entry = replState.history[targetIdx];
      replState.debugState = cloneState(entry.state);
      replState.stepCount = entry.step;
      replState.debugMode = true;
      replState.history = replState.history.slice(0, targetIdx);
      log(`Rewound to step ${replState.stepCount}`);
      output.push(...getConsoleOutput(() => printDebugState(replState)));
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  // :history - show history
  if (trimmed === ":history" || trimmed === ":hist" || trimmed.startsWith(":history ") || trimmed.startsWith(":hist ")) {
    const parts = trimmed.split(/\s+/);
    const count = parseInt(parts[1]) || 10;
    if (replState.history.length === 0) {
      log("No history available.");
    } else {
      log(`\nHistory (last ${Math.min(count, replState.history.length)} steps):`);
      const start = Math.max(0, replState.history.length - count);
      for (let i = start; i < replState.history.length; i++) {
        const h = replState.history[i];
        log(`  [${h.step}] ${h.control}`);
      }
      log(`  [${replState.stepCount}] <current>`);
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  // :record - toggle recording
  if (trimmed.startsWith(":record ")) {
    const parts = trimmed.split(/\s+/);
    const mode = parts[1];
    if (mode === "on") {
      replState.recordingEnabled = true;
      log("Recording: ON");
    } else if (mode === "off") {
      replState.recordingEnabled = false;
      log("Recording: OFF");
      log("Warning: Without recording, 'goto' and 'dump' won't have new steps.");
    } else {
      log("Usage: :record on|off");
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  // :dump - save trace to file
  if (trimmed.startsWith(":dump ")) {
    const filepath = trimmed.slice(6).trim();
    if (!filepath) {
      log("Usage: :dump <filepath>");
    } else if (replState.trace.length === 0) {
      log("No trace to dump.");
    } else {
      const dumpData = {
        version: 1,
        code: replState.debugCode || "",
        timestamp: new Date().toISOString(),
        totalSteps: replState.trace.length,
        traceSummary: replState.trace.map(t => ({
          step: t.step,
          control: t.controlSummary,
          stackDepth: t.stackDepth,
        })),
      };
      try {
        fs.writeFileSync(filepath, JSON.stringify(dumpData, null, 2));
        log(`Dumped trace summary to ${filepath} (${replState.trace.length} steps)`);
        log("To replay: load the code and step through, or use ':replay <file>'");
      } catch (err: any) {
        log(`Error dumping: ${err.message}`);
      }
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  // :replay - load and replay dump
  if (trimmed.startsWith(":replay ")) {
    const filepath = path.resolve(trimmed.slice(8).trim());
    if (!filepath) {
      log("Usage: :replay <filepath>");
    } else if (!fs.existsSync(filepath)) {
      log(`File not found: ${filepath}`);
    } else {
      try {
        const content = fs.readFileSync(filepath, "utf8");
        const data = JSON.parse(content);
        if (!data.code) {
          log("Dump file missing 'code' field - cannot replay.");
        } else {
          log(`Loading dump from: ${filepath}`);
          log(`Original timestamp: ${data.timestamp}`);
          log(`Total steps: ${data.totalSteps}`);
          log("\nReplaying execution...");

          // Load and run to completion
          replState = debugLoadExpr(data.code, replState);
          let count = 0;
          while (replState.debugState && count < (data.totalSteps || 100000)) {
            const { replState: newState } = debugStep(replState);
            replState = newState;
            count++;
            if (!replState.debugState) break;
          }

          log(`\nReplay complete. ${replState.trace.length} steps recorded.`);
          log("Use ':goto <step>' to jump to any step, or ':trace' to see the trace.");
        }
      } catch (err: any) {
        log(`Error loading dump: ${err.message}`);
      }
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  // :loadfile - load code from file
  if (trimmed.startsWith(":loadfile ")) {
    const filepath = path.resolve(trimmed.slice(10).trim());
    if (!filepath) {
      log("Usage: :loadfile <path>");
    } else if (!fs.existsSync(filepath)) {
      log(`File not found: ${filepath}`);
    } else {
      try {
        const code = fs.readFileSync(filepath, "utf8");
        log(`Loaded from: ${filepath}`);

        // Evaluate the file content
        const { value, replState: newState } = await evalInRepl(code, replState);
        replState = newState;
        log("=>", valToSexp(value));
      } catch (err: any) {
        log("error:", err.message);
        replState.lastError = err;
      }
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  // :stop - stop execution
  if (trimmed === ":stop") {
    replState.running = false;
    log("Stopped.");
    return { replState, output: output.join("\n"), shouldExit };
  }

  // :env - show environment or lookup specific binding
  if (trimmed === ":env" || trimmed.startsWith(":env ")) {
    const bindingName = trimmed.slice(4).trim();
    const activeState = getActiveState();

    if (bindingName) {
      if (activeState) {
        // Search through the context chain
        let ctx: any = activeState.env;
        let addr: number | undefined;
        while (ctx) {
          if (ctx.frame && ctx.frame instanceof Map && ctx.frame.has(bindingName)) {
            addr = ctx.frame.get(bindingName);
            break;
          }
          ctx = ctx.parent;
        }
        if (addr !== undefined) {
          const val = activeState.store.read(addr);
          log(`${bindingName} = ${valToSexp(val)}`);
        } else {
          log(`(binding '${bindingName}' not found)`);
        }
      } else {
        log("(no state available - evaluate an expression first)");
      }
    } else {
      log("(Current environment has primitives: +, -, *, /, =, <, >, etc.)");
      if (replState.defs.length > 0) {
        log("User-defined:");
        for (const d of replState.defs) {
          log("  " + d.slice(0, 60) + (d.length > 60 ? "..." : ""));
        }
      }
      if (activeState) {
        const bindings = envToBindings(activeState.env);
        log(`(${bindings.length} total bindings)`);
      }
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  // :defs
  if (trimmed === ":defs") {
    if (replState.defs.length === 0) {
      log("(no user definitions yet)");
    } else {
      for (const d of replState.defs) log(d);
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  // :stack
  if (trimmed === ":stack") {
    const activeState = getActiveState();
    if (!activeState) {
      log("(no state available - evaluate an expression first)");
    } else {
      const kont = activeState.kont;
      if (kont.length === 0) {
        log("(empty stack)");
      } else {
        log(`Stack depth: ${kont.length}`);
        for (let i = kont.length - 1; i >= 0; i--) {
          log(frameToString(kont[i], i));
        }
      }
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  // :frame
  if (trimmed.startsWith(":frame ")) {
    const nStr = trimmed.slice(7).trim();
    const n = parseInt(nStr, 10);
    const activeState = getActiveState();

    if (isNaN(n)) {
      log("Usage: :frame <number>");
    } else if (!activeState) {
      log("(no state available)");
    } else {
      const kont = activeState.kont;
      if (n < 0 || n >= kont.length) {
        log(`(frame ${n} out of range, stack has ${kont.length} frames)`);
      } else {
        const frame = kont[n];
        log(frameToString(frame, n));
        log(`  tag: ${frame.tag}`);
        if ("env" in frame && frame.env) {
          const bindings = envToBindings(frame.env as Env);
          log(`  env: ${bindings.length} bindings`);
          if (bindings.length <= 10) {
            for (const b of bindings) {
              log(`    ${b.name} -> @${b.addr}`);
            }
          }
        }
      }
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  // :control
  if (trimmed === ":control") {
    const activeState = getActiveState();
    if (!activeState) {
      log("(no state available)");
    } else {
      log(controlToString(activeState.control));
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  // :ask - Agentic LLM with tool calls
  if (trimmed.startsWith(":ask ")) {
    const request = trimmed.slice(5).trim();
    const adapter = createLLMAdapter();
    if (!adapter) {
      log("error: No API key. Set OPENAI_API_KEY, ANTHROPIC_API_KEY, or add to config.yaml");
    } else {
      log("; --- agentic session begins ---");
      log(`; using: ${adapter.name} (tools: ${adapter.supportsToolCalls ? "native" : "text-based"})`);
      log(`; question: "${request}"`);

      try {
        const { answer, traceId, replState: newState } = await runAgenticQuery(request, replState, adapter);
        replState = newState;

        log("");
        log(`Answer: ${answer}`);
        log("");
        log(`Trace ID: ${traceId}`);
        log(`(use ':trace ${traceId}' to see details, ':trace ${traceId} --verbose' for full trace)`);
        log("; --- agentic session ends ---");
      } catch (err: any) {
        log("LLM error:", err.message);
      }
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  // :traces - List recent LLM traces
  if (trimmed === ":traces") {
    if (llmTraces.size === 0) {
      log("(no traces recorded yet)");
    } else {
      log("Recent LLM Traces:");
      llmTraces.forEach((trace, id) => {
        const duration = trace.endTime ? trace.endTime - trace.startTime : "?";
        log(`  ${id}: "${trace.question.slice(0, 40)}${trace.question.length > 40 ? "..." : ""}" (${duration}ms, ${trace.toolCallCount} tool calls)`);
      });
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  // :trace <id> [--verbose] or :trace --last [-v] - Show specific trace
  if (trimmed.startsWith(":trace ")) {
    const parts = trimmed.slice(7).trim().split(/\s+/);
    const verbose = parts.includes("--verbose") || parts.includes("-v");
    const isLast = parts.includes("--last") || parts.includes("-l");

    let trace: LLMTrace | undefined;
    if (isLast) {
      // Get the most recent trace
      let lastTrace: LLMTrace | undefined;
      llmTraces.forEach(t => { lastTrace = t; });
      trace = lastTrace;
      if (!trace) {
        log("No traces recorded yet.");
        return { replState, output: output.join("\n"), shouldExit };
      }
    } else {
      // Find the trace ID (first part that's not a flag)
      const traceId = parts.find(p => !p.startsWith("-"));
      if (!traceId) {
        log("Usage: :trace <id> [--verbose] or :trace --last [--verbose]");
        return { replState, output: output.join("\n"), shouldExit };
      }
      trace = llmTraces.get(traceId);
      if (!trace) {
        log(`Trace '${traceId}' not found. Use :traces to list available traces.`);
        return { replState, output: output.join("\n"), shouldExit };
      }
    }

    log(formatTrace(trace, verbose));
    return { replState, output: output.join("\n"), shouldExit };
  }

  // OPR Commands
  if (trimmed === ":opr-list") {
    log("\nAvailable OPR Kernels:");
    log("======================");
    for (const id of listKernels()) {
      const kernel = getKernel(id);
      if (kernel) {
        log(`  ${id} (op: ${kernel.op})`);
      }
    }
    log("\nUse :opr-run <kernel-id> <program-json> to execute a kernel.");
    return { replState, output: output.join("\n"), shouldExit };
  }

  if (trimmed.startsWith(":opr-run ")) {
    const args = trimmed.slice(9).trim();
    const spaceIdx = args.indexOf(" ");
    if (spaceIdx === -1) {
      log("Usage: :opr-run <kernel-id> <program-json>");
      log("Example: :opr-run opr.classify.v1 {\"item\":\"error at line 42\",\"categories\":[\"bug\",\"feature\"]}");
      return { replState, output: output.join("\n"), shouldExit };
    }

    const kernelId = args.slice(0, spaceIdx);
    const programJson = args.slice(spaceIdx + 1).trim();

    const kernel = getKernel(kernelId);
    if (!kernel) {
      log(`Unknown kernel: ${kernelId}`);
      log("Use :opr-list to see available kernels.");
      return { replState, output: output.join("\n"), shouldExit };
    }

    let program: unknown;
    try {
      program = JSON.parse(programJson);
    } catch (e: any) {
      log(`Invalid JSON: ${e.message}`);
      return { replState, output: output.join("\n"), shouldExit };
    }

    // Get API key
    const openaiKey = process.env.OPENAI_API_KEY;
    const anthropicKey = process.env.ANTHROPIC_API_KEY;

    if (!openaiKey && !anthropicKey) {
      log("No API key found. Set OPENAI_API_KEY or ANTHROPIC_API_KEY.");
      return { replState, output: output.join("\n"), shouldExit };
    }

    // Create adapter
    const adapter = openaiKey
      ? new OpenAIOprAdapter({ apiKey: openaiKey, model: "gpt-4o-mini" })
      : new AnthropicOprAdapter({ apiKey: anthropicKey!, model: "claude-sonnet-4-20250514" });

    const receipts = new OprReceiptStore();
    const runtime = new OprRuntime({
      kernel,
      adapter,
      receipts,
      budget: { maxAttempts: 3 },
    });

    log(`\nRunning kernel: ${kernelId}`);
    log(`Program: ${JSON.stringify(program, null, 2)}`);
    log("\nCalling LLM...\n");

    try {
      const result = await runtime.step({ program, state: null });

      if (result.tag === "ok") {
        log("SUCCESS!");
        log(`\nResult: ${JSON.stringify(result.output.result, null, 2)}`);
        if (result.output.next_state) {
          log(`\nNext State: ${JSON.stringify(result.output.next_state, null, 2)}`);
        }
        if (result.output.effects && result.output.effects.length > 0) {
          log(`\nEffects: ${JSON.stringify(result.output.effects, null, 2)}`);
        }
        log(`\nAttempts: ${result.attempts}`);
      } else {
        log(`FAILED: ${result.tag}`);
        if ("error" in result) {
          log(`Error: ${(result.error as Error).message}`);
        }
      }

      // Store receipts in session
      (replState as any).oprReceipts = [
        ...((replState as any).oprReceipts || []),
        ...result.receipts,
      ];
      log(`\nReceipts: ${result.receipts.length} generated (use :opr-receipts to view)`);
    } catch (e: any) {
      log(`Error running kernel: ${e.message}`);
    }

    return { replState, output: output.join("\n"), shouldExit };
  }

  if (trimmed === ":opr-receipts") {
    const oprReceipts = (replState as any).oprReceipts;
    if (!oprReceipts || oprReceipts.length === 0) {
      log("No OPR receipts in current session.");
    } else {
      log("\nOPR Receipt Chain:");
      log("==================");
      for (let i = 0; i < oprReceipts.length; i++) {
        const r = oprReceipts[i];
        const statusIcon = r.status === 'OK' ? '[OK]' : '[X]';
        log(`${i + 1}. ${statusIcon} ${r.status} - ${r.kernel_id}:${r.op}`);
        log(`     Attempt: ${r.attempt}  Created: ${r.created_at}`);
        if (r.errors && r.errors.length > 0) {
          log(`     Errors: ${r.errors[0]}`);
        }
      }
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  if (trimmed === ":opr-verify" || trimmed.startsWith(":opr-verify ")) {
    const arg = trimmed.slice(12).trim();
    let receipts: any[];

    if (arg) {
      // Load from file
      try {
        const content = fs.readFileSync(arg, 'utf-8');
        receipts = JSON.parse(content);
      } catch (e: any) {
        log(`Error loading receipt file: ${e.message}`);
        return { replState, output: output.join("\n"), shouldExit };
      }
    } else {
      receipts = (replState as any).oprReceipts || [];
      if (receipts.length === 0) {
        log("No OPR receipts in session. Provide a file path to verify.");
        return { replState, output: output.join("\n"), shouldExit };
      }
    }

    // Simple chain verification
    let valid = true;
    let brokenAt = -1;
    let error = "";

    if (receipts.length > 0 && receipts[0].prev_receipt_hash !== null) {
      valid = false;
      brokenAt = 0;
      error = "First receipt should have null prev_receipt_hash";
    } else {
      for (let i = 1; i < receipts.length; i++) {
        if (receipts[i].prev_receipt_hash !== receipts[i - 1].receipt_hash) {
          valid = false;
          brokenAt = i;
          error = "Chain link broken";
          break;
        }
      }
    }

    if (valid) {
      log(`\nReceipt chain is VALID`);
      log(`  ${receipts.length} receipts verified`);
      if (receipts.length > 0) {
        log(`  Chain hash: ${receipts[receipts.length - 1].receipt_hash}`);
      }
    } else {
      log(`\nReceipt chain is BROKEN`);
      log(`  Broken at index: ${brokenAt}`);
      log(`  Error: ${error}`);
    }
    return { replState, output: output.join("\n"), shouldExit };
  }

  // Skip other commands starting with :
  if (trimmed.startsWith(":")) {
    log(`Unknown command: ${trimmed}. Type :help for commands.`);
    return { replState, output: output.join("\n"), shouldExit };
  }

  // Empty line
  if (trimmed === "") {
    return { replState, output: "", shouldExit };
  }

  // Otherwise, evaluate as Lisp expression
  replState.sessionWriter?.input(trimmed);
  const isEffectCall = /^\(effect\b/.test(trimmed);
  const useMockEffects = process.env.OMEGA_MOCK_EFFECTS === "1";

  try {
    // Use mock effects only if explicitly enabled (for testing)
    if (isEffectCall && useMockEffects) {
      const { value, state } = handleEffectExpression(trimmed, replState);
      log("=>", valToSexp(value));
      replState.sessionWriter?.result(valToSexp(value));
      replState.sessionWriter?.checkpoint(state, "manual");
    } else {
      replState.sessionWriter?.step(trimmed);
      const { value, replState: newState, state } = await evalInRepl(trimmed, replState);
      replState = newState;
      replState.sessionState = state;
      replState.baseState = state;
      log("=>", valToSexp(value));
      replState.sessionWriter?.result(valToSexp(value));
      replState.sessionWriter?.checkpoint(state, "manual");
    }
  } catch (err: any) {
    log("error:", err.message);
    replState.lastError = err;
    replState.sessionWriter?.error(err.message, err.stack);
  }

  return { replState, output: output.join("\n"), shouldExit };
}

// Helper to capture console output from functions that use console.log
function getConsoleOutput(fn: () => void): string[] {
  const output: string[] = [];
  const originalLog = console.log;
  console.log = (...msgs: any[]) => output.push(msgs.join(" "));
  try {
    fn();
  } finally {
    console.log = originalLog;
  }
  return output;
}

type ReplArgs = ReturnType<typeof parseArgs>;

type BatchMetadata = {
  command?: string;
  file?: string;
};

async function runBatch(
  args: Pick<ReplArgs, "cmd" | "file">,
  replState: ReplState
): Promise<{ replState: ReplState; output: string; metadata: BatchMetadata }> {
  if (args.cmd) {
    const { replState: newState, output } = await processReplCommand(args.cmd.trim(), replState);
    return { replState: newState, output, metadata: { command: args.cmd } };
  }

  if (args.file) {
    const filePath = path.resolve(args.file);
    if (!fs.existsSync(filePath)) {
      console.error(`File not found: ${filePath}`);
      process.exit(1);
    }

    // Read file and extract complete S-expressions (handles multi-line)
    const fileContent = fs.readFileSync(filePath, "utf8");

    // Strip comment lines before parsing
    const cleanedContent = fileContent
      .split("\n")
      .filter(line => !line.trim().startsWith(";"))
      .join("\n");

    // Extract balanced S-expressions
    const sexprs = extractSexpressions(cleanedContent);
    const outputs: string[] = [];

    for (const sexpr of sexprs) {
      const trimmed = sexpr.trim();
      if (!trimmed) continue;

      const { replState: newState, output, shouldExit } = await processReplCommand(trimmed, replState);
      replState = newState;
      if (output) outputs.push(output);
      if (shouldExit) break;
    }

    return { replState, output: outputs.join("\n"), metadata: { file: args.file } };
  }

  return { replState, output: "", metadata: {} };
}

function emitBatchOutput(
  args: Pick<ReplArgs, "session" | "json">,
  replState: ReplState,
  output: string,
  metadata: BatchMetadata
): void {
  if (args.json) {
    const result = {
      session: args.session,
      command: metadata.command,
      file: metadata.file,
      debugMode: replState.debugMode,
      stepCount: replState.stepCount,
      defsCount: replState.defs.length,
      output,
    };
    console.log(JSON.stringify(result, null, 2));
    return;
  }

  if (output) {
    console.log(output);
  }
}

// ─────────────────────────────────────────────────────────────────
// Main REPL loop
// ─────────────────────────────────────────────────────────────────
async function main() {
  const args = parseArgs();

  if (args.cmd || args.file) {
    const replState = args.session
      ? await loadSession(args.session) || await initReplState()
      : await initReplState();
    const { replState: newState, output, metadata } = await runBatch(args, replState);

    if (args.session) {
      saveSession(newState, args.session);
    }

    emitBatchOutput(args, newState, output, metadata);
    return;
  }

  // ─────────────────────────────────────────────────────────────────
  // Session mode: run command(s) against a named session
  // ─────────────────────────────────────────────────────────────────
  if (args.session) {
    // Load existing session or create new one
    let replState = await loadSession(args.session) || await initReplState();

    // No command or file, just show session info
    console.log(`Session '${args.session}' loaded.`);
    console.log(`  Definitions: ${replState.defs.length}`);
    console.log(`  Debug mode: ${replState.debugMode}`);
    if (replState.debugMode) {
      console.log(`  Step count: ${replState.stepCount}`);
      console.log(`  Trace length: ${replState.trace.length}`);
    }
    console.log(`  Breakpoints: ${replState.breakpoints.length}`);
    return;
  }

  // ─────────────────────────────────────────────────────────────────
  // Interactive mode
  // ─────────────────────────────────────────────────────────────────
  const isTTY = process.stdin.isTTY;

  if (isTTY) {
    console.log("═".repeat(60));
    console.log("Ω REPL — Omega Lisp with Oracle Protocol & Debugger");
    console.log("═".repeat(60));
    console.log("Type Lisp expressions to evaluate.");
    console.log("Commands: :help :debug :step :run :goto :trace :break :quit");
    console.log("Session mode: npx tsx bin/omega-repl.ts -s <name> -c '<cmd>'");
    console.log("");
  if (VERBOSE) console.log("[verbose mode: oracle traces enabled]");
  console.log("");
}

let replState = await initReplState();
replState.sessionWriter = new SessionWriter(replState.sessionDir, replState.sessionName || "current");
process.on("exit", () => {
  try { replState.sessionWriter?.close(); } catch { /* ignore */ }
});

// For non-TTY (piped) input, process line by line
  if (!isTTY) {
    const rl = readline.createInterface({ input: process.stdin });
    let buffer = "";
    let depth = 0;

    try {
      for await (const line of rl) {
        const trimmed = line.trim();
        if (trimmed === ":quit" || trimmed === ":q") break;
        if (trimmed === "" || trimmed.startsWith(";")) continue; // skip empty and comments

        // Commands start with :
        if (trimmed.startsWith(":")) {
          const { replState: newState, output, shouldExit } = await processReplCommand(trimmed, replState);
          replState = newState;
          if (output) console.log(output);
          if (shouldExit) break;
          continue;
        }

        // Accumulate multi-line expressions
        buffer += (buffer ? "\n" : "") + line;
        for (const ch of line) {
          if (ch === "(") depth++;
          if (ch === ")") depth--;
        }

        // Balanced parens - evaluate
        if (depth <= 0) {
          const { replState: newState, output } = await processReplCommand(buffer, replState);
          replState = newState;
          if (output) console.log(output);
          buffer = "";
          depth = 0;
        }
      }
    } finally {
      rl.close();
      process.stdin.pause();
    }
    return;
  }

  // TTY interactive mode
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    prompt: replState.debugMode ? "Ωdbg> " : "Ω> ",
  });

  rl.prompt();

  let multilineBuffer = "";
  let parenDepth = 0;

  rl.on("line", async (line) => {
    const trimmed = line.trim();

    // Commands start with :
    if (trimmed.startsWith(":")) {
      const { replState: newState, output, shouldExit } = await processReplCommand(trimmed, replState);
      replState = newState;
      if (output) console.log(output);
      if (shouldExit) {
        rl.close();
        process.exit(0);
      }
      // Update prompt based on debug mode
      rl.setPrompt(replState.debugMode ? "Ωdbg> " : "Ω> ");
      rl.prompt();
      return;
    }

    // Empty line
    if (trimmed === "") {
      rl.prompt();
      return;
    }

    // Accumulate multi-line input (track parenthesis depth)
    multilineBuffer += (multilineBuffer ? "\n" : "") + line;
    for (const ch of line) {
      if (ch === "(") parenDepth++;
      if (ch === ")") parenDepth--;
    }

    // If parens aren't balanced, wait for more input
    if (parenDepth > 0) {
      rl.setPrompt(".. ");
      rl.prompt();
      return;
    }

    // Parens balanced, evaluate
    const src = multilineBuffer;
    multilineBuffer = "";
    parenDepth = 0;

    const { replState: newState, output } = await processReplCommand(src, replState);
    replState = newState;
    if (output) console.log(output);

    rl.setPrompt(replState.debugMode ? "Ωdbg> " : "Ω> ");
    rl.prompt();
  });

  rl.on("close", () => {
    console.log("\nGoodbye!");
    process.exit(0);
  });
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
