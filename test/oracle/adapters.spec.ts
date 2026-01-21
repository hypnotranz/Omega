// test/oracle/adapters.spec.ts
// Unit tests for Oracle adapters - streaming and blocking modes

import { describe, it, expect, beforeEach, vi, afterEach } from "vitest";
import { OpenAIAdapter, createOpenAIAdapter } from "../../src/core/oracle/adapters/openaiAdapter";
import { AnthropicAdapter, createAnthropicAdapter } from "../../src/core/oracle/adapters/anthropicAdapter";
import type { OracleInit, OracleResp } from "../../src/core/oracle/protocol";

// ═══════════════════════════════════════════════════════════════════════════
// MOCK HELPERS
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Create a mock ReadableStream from SSE data lines
 */
function createSSEStream(lines: string[]): ReadableStream<Uint8Array> {
  const encoder = new TextEncoder();
  let index = 0;

  return new ReadableStream({
    pull(controller) {
      if (index < lines.length) {
        controller.enqueue(encoder.encode(lines[index] + "\n"));
        index++;
      } else {
        controller.close();
      }
    },
  });
}

/**
 * Create a mock fetch Response for streaming
 */
function createStreamingResponse(lines: string[], ok = true): Response {
  return {
    ok,
    body: createSSEStream(lines),
    text: async () => lines.join("\n"),
    json: async () => ({}),
  } as Response;
}

/**
 * Create a mock fetch Response for blocking (JSON)
 */
function createBlockingResponse(data: any, ok = true): Response {
  return {
    ok,
    text: async () => JSON.stringify(data),
    json: async () => data,
  } as Response;
}

// ═══════════════════════════════════════════════════════════════════════════
// OPENAI SSE FORMAT TESTS
// ═══════════════════════════════════════════════════════════════════════════

describe("OpenAI Adapter - SSE Parsing", () => {
  const originalFetch = global.fetch;

  beforeEach(() => {
    vi.stubEnv("OPENAI_API_KEY", "test-key");
  });

  afterEach(() => {
    global.fetch = originalFetch;
    vi.unstubAllEnvs();
  });

  it("parses streaming tool call chunks correctly", async () => {
    // Simulate OpenAI SSE format with chunked tool call
    const sseLines = [
      'data: {"choices":[{"delta":{"role":"assistant","content":null,"tool_calls":[{"index":0,"id":"call_123","function":{"name":"return","arguments":""}}]}}]}',
      'data: {"choices":[{"delta":{"tool_calls":[{"index":0,"function":{"arguments":"{\\"value"}}]}}]}',
      'data: {"choices":[{"delta":{"tool_calls":[{"index":0,"function":{"arguments":"\\": \\"42"}}]}}]}',
      'data: {"choices":[{"delta":{"tool_calls":[{"index":0,"function":{"arguments":"\\", \\"confidence"}}]}}]}',
      'data: {"choices":[{"delta":{"tool_calls":[{"index":0,"function":{"arguments":"\\": 0.95}"}}]}}]}',
      'data: {"choices":[{"finish_reason":"tool_calls"}]}',
      "data: [DONE]",
    ];

    global.fetch = vi.fn().mockResolvedValue(createStreamingResponse(sseLines));

    const adapter = new OpenAIAdapter({ model: "gpt-4o", streaming: true });

    const init: OracleInit = {
      tag: "Infer",
      payload: { tag: "Str", s: "What is 6 * 7?" },
      envRef: "env-1",
      stateRef: "state-1",
    };

    const session = adapter.startSession(init);
    const result = await session.next({ tag: "RespAck" });

    expect(result.done).toBe(true);
    expect(result.value?.tag).toBe("Meaning");
    if (result.value?.tag === "Meaning") {
      // Should have parsed the return tool and returned the value
      expect(result.value.denotation).toEqual({ tag: "Num", n: 42 });
      expect(result.value.confidence).toBe(0.95);
      expect(result.value.trace?.tag).toBe("Str");
    }
  });

  it("handles text-only streaming response", async () => {
    const sseLines = [
      'data: {"choices":[{"delta":{"role":"assistant","content":"Hello"}}]}',
      'data: {"choices":[{"delta":{"content":" there!"}}]}',
      'data: {"choices":[{"finish_reason":"stop"}]}',
      "data: [DONE]",
    ];

    global.fetch = vi.fn().mockResolvedValue(createStreamingResponse(sseLines));

    const adapter = new OpenAIAdapter({ model: "gpt-4o", streaming: true });

    const init: OracleInit = {
      tag: "Infer",
      payload: { tag: "Str", s: "Say hi" },
      envRef: "env-1",
      stateRef: "state-1",
    };

    const session = adapter.startSession(init);
    const result = await session.next({ tag: "RespAck" });

    expect(result.done).toBe(true);
    if (result.value?.tag === "Meaning") {
      // Text-only response wraps in Str with lower confidence
      expect(result.value.denotation).toEqual({ tag: "Str", s: "Hello there!" });
      expect(result.value.confidence).toBe(0.5);
    }
  });

  it("accumulates multiple tool calls from stream", async () => {
    // First tool call: eval
    // Second tool call: return
    const sseLines = [
      'data: {"choices":[{"delta":{"tool_calls":[{"index":0,"id":"call_1","function":{"name":"eval","arguments":""}}]}}]}',
      'data: {"choices":[{"delta":{"tool_calls":[{"index":0,"function":{"arguments":"{\\"expr\\": \\"(+ 1 2)\\"}"}}]}}]}',
      'data: {"choices":[{"delta":{"tool_calls":[{"index":1,"id":"call_2","function":{"name":"return","arguments":""}}]}}]}',
      'data: {"choices":[{"delta":{"tool_calls":[{"index":1,"function":{"arguments":"{\\"value\\": \\"3\\"}"}}]}}]}',
      "data: [DONE]",
    ];

    global.fetch = vi.fn().mockResolvedValue(createStreamingResponse(sseLines));

    const adapter = new OpenAIAdapter({ model: "gpt-4o", streaming: true });

    const init: OracleInit = {
      tag: "Infer",
      payload: { tag: "Str", s: "Add 1+2" },
      envRef: "env-1",
      stateRef: "state-1",
    };

    const session = adapter.startSession(init);

    // First next - should yield eval request
    let step = await session.next({ tag: "RespAck" });
    expect(step.done).toBe(false);
    expect(step.value?.tag).toBe("ReqEval");

    // Respond to eval, then should get return result
    step = await session.next({ tag: "RespVal", value: { tag: "Num", n: 3 } });
    expect(step.done).toBe(true);
    if (step.value?.tag === "Meaning") {
      expect(step.value.denotation).toEqual({ tag: "Num", n: 3 });
    }
  });
});

describe("OpenAI Adapter - Blocking Mode", () => {
  const originalFetch = global.fetch;

  beforeEach(() => {
    vi.stubEnv("OPENAI_API_KEY", "test-key");
  });

  afterEach(() => {
    global.fetch = originalFetch;
    vi.unstubAllEnvs();
  });

  it("parses blocking tool call response", async () => {
    const responseData = {
      choices: [{
        message: {
          role: "assistant",
          content: null,
          tool_calls: [{
            id: "call_abc",
            type: "function",
            function: {
              name: "return",
              arguments: '{"value": "42", "confidence": 0.9}',
            },
          }],
        },
      }],
    };

    global.fetch = vi.fn().mockResolvedValue(createBlockingResponse(responseData));

    const adapter = new OpenAIAdapter({ model: "gpt-4o", streaming: false });

    const init: OracleInit = {
      tag: "Infer",
      payload: { tag: "Str", s: "What is the answer?" },
      envRef: "env-1",
      stateRef: "state-1",
    };

    const session = adapter.startSession(init);
    const result = await session.next({ tag: "RespAck" });

    expect(result.done).toBe(true);
    if (result.value?.tag === "Meaning") {
      expect(result.value.denotation).toEqual({ tag: "Num", n: 42 });
    }
  });

  it("handles blocking text response (no tool calls)", async () => {
    const responseData = {
      choices: [{
        message: {
          role: "assistant",
          content: "I cannot use tools right now.",
        },
      }],
    };

    global.fetch = vi.fn().mockResolvedValue(createBlockingResponse(responseData));

    const adapter = new OpenAIAdapter({ model: "gpt-4o", streaming: false });

    const init: OracleInit = {
      tag: "Infer",
      payload: { tag: "Str", s: "Question" },
      envRef: "env-1",
      stateRef: "state-1",
    };

    const session = adapter.startSession(init);
    const result = await session.next({ tag: "RespAck" });

    expect(result.done).toBe(true);
    if (result.value?.tag === "Meaning") {
      expect(result.value.denotation).toEqual({ tag: "Str", s: "I cannot use tools right now." });
      expect(result.value.confidence).toBe(0.5);
    }
  });

  it("handles API error", async () => {
    global.fetch = vi.fn().mockResolvedValue({
      ok: false,
      text: async () => "Rate limit exceeded",
    });

    const adapter = new OpenAIAdapter({ model: "gpt-4o", streaming: false });

    const init: OracleInit = {
      tag: "Infer",
      payload: { tag: "Str", s: "Question" },
      envRef: "env-1",
      stateRef: "state-1",
    };

    const session = adapter.startSession(init);
    const result = await session.next({ tag: "RespAck" });

    expect(result.done).toBe(true);
    if (result.value?.tag === "Meaning") {
      expect(result.value.confidence).toBe(0);
      expect((result.value.trace as any)?.s).toContain("API error");
    }
  });
});

// ═══════════════════════════════════════════════════════════════════════════
// ANTHROPIC SSE FORMAT TESTS
// ═══════════════════════════════════════════════════════════════════════════

describe("Anthropic Adapter - SSE Parsing", () => {
  const originalFetch = global.fetch;

  beforeEach(() => {
    vi.stubEnv("ANTHROPIC_API_KEY", "test-key");
  });

  afterEach(() => {
    global.fetch = originalFetch;
    vi.unstubAllEnvs();
  });

  it("parses streaming tool_use blocks correctly", async () => {
    // Anthropic uses content_block_start, content_block_delta, message_stop
    const sseLines = [
      'data: {"type":"message_start","message":{"id":"msg_1","role":"assistant","content":[]}}',
      'data: {"type":"content_block_start","index":0,"content_block":{"type":"tool_use","id":"toolu_1","name":"return"}}',
      'data: {"type":"content_block_delta","index":0,"delta":{"type":"input_json_delta","partial_json":"{\\"value"}}',
      'data: {"type":"content_block_delta","index":0,"delta":{"type":"input_json_delta","partial_json":"\\": \\"hello\\"}"}}',
      'data: {"type":"content_block_stop","index":0}',
      'data: {"type":"message_stop"}',
    ];

    global.fetch = vi.fn().mockResolvedValue(createStreamingResponse(sseLines));

    const adapter = new AnthropicAdapter({ model: "claude-sonnet-4-20250514", streaming: true });

    const init: OracleInit = {
      tag: "Infer",
      payload: { tag: "Str", s: "Say hello" },
      envRef: "env-1",
      stateRef: "state-1",
    };

    const session = adapter.startSession(init);
    const result = await session.next({ tag: "RespAck" });

    expect(result.done).toBe(true);
    if (result.value?.tag === "Meaning") {
      expect(result.value.denotation).toEqual({ tag: "Str", s: "hello" });
    }
  });

  it("handles text_delta in streaming", async () => {
    const sseLines = [
      'data: {"type":"message_start","message":{"id":"msg_1","role":"assistant","content":[]}}',
      'data: {"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}',
      'data: {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"Hello "}}',
      'data: {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"world!"}}',
      'data: {"type":"content_block_stop","index":0}',
      'data: {"type":"message_stop"}',
    ];

    global.fetch = vi.fn().mockResolvedValue(createStreamingResponse(sseLines));

    const adapter = new AnthropicAdapter({ model: "claude-sonnet-4-20250514", streaming: true });

    const init: OracleInit = {
      tag: "Infer",
      payload: { tag: "Str", s: "Greet me" },
      envRef: "env-1",
      stateRef: "state-1",
    };

    const session = adapter.startSession(init);
    const result = await session.next({ tag: "RespAck" });

    expect(result.done).toBe(true);
    if (result.value?.tag === "Meaning") {
      expect(result.value.denotation).toEqual({ tag: "Str", s: "Hello world!" });
      expect(result.value.confidence).toBe(0.5);
    }
  });

  it("accumulates multiple tool blocks from stream", async () => {
    const sseLines = [
      'data: {"type":"message_start","message":{"id":"msg_1","role":"assistant","content":[]}}',
      // First tool: eval
      'data: {"type":"content_block_start","index":0,"content_block":{"type":"tool_use","id":"toolu_1","name":"eval"}}',
      'data: {"type":"content_block_delta","index":0,"delta":{"type":"input_json_delta","partial_json":"{\\"expr\\": \\"(* 6 7)\\"}"}}',
      'data: {"type":"content_block_stop","index":0}',
      // Second tool: return
      'data: {"type":"content_block_start","index":1,"content_block":{"type":"tool_use","id":"toolu_2","name":"return"}}',
      'data: {"type":"content_block_delta","index":1,"delta":{"type":"input_json_delta","partial_json":"{\\"value\\": \\"42\\"}"}}',
      'data: {"type":"content_block_stop","index":1}',
      'data: {"type":"message_stop"}',
    ];

    global.fetch = vi.fn().mockResolvedValue(createStreamingResponse(sseLines));

    const adapter = new AnthropicAdapter({ model: "claude-sonnet-4-20250514", streaming: true });

    const init: OracleInit = {
      tag: "Infer",
      payload: { tag: "Str", s: "What is 6*7?" },
      envRef: "env-1",
      stateRef: "state-1",
    };

    const session = adapter.startSession(init);

    // First next - should yield eval request
    let step = await session.next({ tag: "RespAck" });
    expect(step.done).toBe(false);
    expect(step.value?.tag).toBe("ReqEval");

    // Respond to eval
    step = await session.next({ tag: "RespVal", value: { tag: "Num", n: 42 } });
    expect(step.done).toBe(true);
    if (step.value?.tag === "Meaning") {
      expect(step.value.denotation).toEqual({ tag: "Num", n: 42 });
    }
  });
});

describe("Anthropic Adapter - Blocking Mode", () => {
  const originalFetch = global.fetch;

  beforeEach(() => {
    vi.stubEnv("ANTHROPIC_API_KEY", "test-key");
  });

  afterEach(() => {
    global.fetch = originalFetch;
    vi.unstubAllEnvs();
  });

  it("parses blocking tool_use response", async () => {
    const responseData = {
      content: [{
        type: "tool_use",
        id: "toolu_abc",
        name: "return",
        input: { value: "42", confidence: 0.9 },
      }],
    };

    global.fetch = vi.fn().mockResolvedValue(createBlockingResponse(responseData));

    const adapter = new AnthropicAdapter({ model: "claude-sonnet-4-20250514", streaming: false });

    const init: OracleInit = {
      tag: "Infer",
      payload: { tag: "Str", s: "What is 6*7?" },
      envRef: "env-1",
      stateRef: "state-1",
    };

    const session = adapter.startSession(init);
    const result = await session.next({ tag: "RespAck" });

    expect(result.done).toBe(true);
    if (result.value?.tag === "Meaning") {
      expect(result.value.denotation).toEqual({ tag: "Num", n: 42 });
    }
  });

  it("handles blocking text response", async () => {
    const responseData = {
      content: [{
        type: "text",
        text: "I'm not sure how to help with that.",
      }],
    };

    global.fetch = vi.fn().mockResolvedValue(createBlockingResponse(responseData));

    const adapter = new AnthropicAdapter({ model: "claude-sonnet-4-20250514", streaming: false });

    const init: OracleInit = {
      tag: "Infer",
      payload: { tag: "Str", s: "Do something" },
      envRef: "env-1",
      stateRef: "state-1",
    };

    const session = adapter.startSession(init);
    const result = await session.next({ tag: "RespAck" });

    expect(result.done).toBe(true);
    if (result.value?.tag === "Meaning") {
      expect(result.value.denotation).toEqual({ tag: "Str", s: "I'm not sure how to help with that." });
      expect(result.value.confidence).toBe(0.5);
    }
  });

  it("handles mixed text and tool_use blocks", async () => {
    const responseData = {
      content: [
        { type: "text", text: "Let me calculate that..." },
        { type: "tool_use", id: "toolu_1", name: "return", input: { value: "42" } },
      ],
    };

    global.fetch = vi.fn().mockResolvedValue(createBlockingResponse(responseData));

    const adapter = new AnthropicAdapter({ model: "claude-sonnet-4-20250514", streaming: false });

    const init: OracleInit = {
      tag: "Infer",
      payload: { tag: "Str", s: "Calculate" },
      envRef: "env-1",
      stateRef: "state-1",
    };

    const session = adapter.startSession(init);
    const result = await session.next({ tag: "RespAck" });

    expect(result.done).toBe(true);
    if (result.value?.tag === "Meaning") {
      expect(result.value.denotation).toEqual({ tag: "Num", n: 42 });
    }
  });

  it("handles API error", async () => {
    global.fetch = vi.fn().mockResolvedValue({
      ok: false,
      text: async () => "Invalid API key",
    });

    const adapter = new AnthropicAdapter({ model: "claude-sonnet-4-20250514", streaming: false });

    const init: OracleInit = {
      tag: "Infer",
      payload: { tag: "Str", s: "Question" },
      envRef: "env-1",
      stateRef: "state-1",
    };

    const session = adapter.startSession(init);
    const result = await session.next({ tag: "RespAck" });

    expect(result.done).toBe(true);
    if (result.value?.tag === "Meaning") {
      expect(result.value.confidence).toBe(0);
      expect((result.value.trace as any)?.s).toContain("API error");
    }
  });
});

// ═══════════════════════════════════════════════════════════════════════════
// MODE DISPATCH TESTS
// ═══════════════════════════════════════════════════════════════════════════

describe("Adapter Mode Dispatch", () => {
  const originalFetch = global.fetch;

  beforeEach(() => {
    vi.stubEnv("OPENAI_API_KEY", "test-key");
    vi.stubEnv("ANTHROPIC_API_KEY", "test-key");
  });

  afterEach(() => {
    global.fetch = originalFetch;
    vi.unstubAllEnvs();
  });

  it("OpenAI uses streaming when config.streaming=true", async () => {
    let capturedBody: any;
    global.fetch = vi.fn().mockImplementation(async (_url, opts) => {
      capturedBody = JSON.parse(opts?.body as string);
      // Return streaming response
      return createStreamingResponse([
        'data: {"choices":[{"delta":{"tool_calls":[{"index":0,"id":"c1","function":{"name":"return","arguments":"{\\"value\\": \\"1\\"}"}}]}}]}',
        "data: [DONE]",
      ]);
    });

    const adapter = new OpenAIAdapter({ model: "gpt-4o", streaming: true });
    const session = adapter.startSession({
      tag: "Infer",
      payload: { tag: "Str", s: "test" },
      envRef: "env",
      stateRef: "state",
    });
    await session.next({ tag: "RespAck" });

    expect(capturedBody.stream).toBe(true);
  });

  it("OpenAI does not use streaming when config.streaming=false", async () => {
    let capturedBody: any;
    global.fetch = vi.fn().mockImplementation(async (_url, opts) => {
      capturedBody = JSON.parse(opts?.body as string);
      return createBlockingResponse({
        choices: [{
          message: {
            role: "assistant",
            tool_calls: [{
              id: "c1",
              type: "function",
              function: { name: "return", arguments: '{"value": "1"}' },
            }],
          },
        }],
      });
    });

    const adapter = new OpenAIAdapter({ model: "gpt-4o", streaming: false });
    const session = adapter.startSession({
      tag: "Infer",
      payload: { tag: "Str", s: "test" },
      envRef: "env",
      stateRef: "state",
    });
    await session.next({ tag: "RespAck" });

    expect(capturedBody.stream).toBeUndefined();
  });

  it("Anthropic uses streaming when config.streaming=true", async () => {
    let capturedBody: any;
    global.fetch = vi.fn().mockImplementation(async (_url, opts) => {
      capturedBody = JSON.parse(opts?.body as string);
      return createStreamingResponse([
        'data: {"type":"content_block_start","index":0,"content_block":{"type":"tool_use","id":"t1","name":"return"}}',
        'data: {"type":"content_block_delta","index":0,"delta":{"type":"input_json_delta","partial_json":"{\\"value\\": \\"1\\"}"}}',
        'data: {"type":"message_stop"}',
      ]);
    });

    const adapter = new AnthropicAdapter({ model: "claude-sonnet-4-20250514", streaming: true });
    const session = adapter.startSession({
      tag: "Infer",
      payload: { tag: "Str", s: "test" },
      envRef: "env",
      stateRef: "state",
    });
    await session.next({ tag: "RespAck" });

    expect(capturedBody.stream).toBe(true);
  });

  it("Anthropic does not use streaming when config.streaming=false", async () => {
    let capturedBody: any;
    global.fetch = vi.fn().mockImplementation(async (_url, opts) => {
      capturedBody = JSON.parse(opts?.body as string);
      return createBlockingResponse({
        content: [{
          type: "tool_use",
          id: "t1",
          name: "return",
          input: { value: "1" },
        }],
      });
    });

    const adapter = new AnthropicAdapter({ model: "claude-sonnet-4-20250514", streaming: false });
    const session = adapter.startSession({
      tag: "Infer",
      payload: { tag: "Str", s: "test" },
      envRef: "env",
      stateRef: "state",
    });
    await session.next({ tag: "RespAck" });

    expect(capturedBody.stream).toBeUndefined();
  });
});

// ═══════════════════════════════════════════════════════════════════════════
// CAPABILITIES TESTS
// ═══════════════════════════════════════════════════════════════════════════

describe("Adapter Capabilities", () => {
  it("OpenAI adapter reports correct capabilities", () => {
    const adapter = new OpenAIAdapter({ model: "gpt-4o" });
    const caps = adapter.capabilities();

    expect(caps.multiTurn).toBe(true);
    expect(caps.toolCalling).toBe(true);
    expect(caps.streaming).toBe(true);
    expect(caps.vision).toBe(true);
    expect(caps.maxContext).toBe(128_000);
  });

  it("Anthropic adapter reports correct capabilities", () => {
    const adapter = new AnthropicAdapter({ model: "claude-sonnet-4-20250514" });
    const caps = adapter.capabilities();

    expect(caps.multiTurn).toBe(true);
    expect(caps.toolCalling).toBe(true);
    expect(caps.streaming).toBe(true);
    expect(caps.vision).toBe(true);
    expect(caps.maxContext).toBe(200_000);
  });
});

// ═══════════════════════════════════════════════════════════════════════════
// FACTORY FUNCTION TESTS
// ═══════════════════════════════════════════════════════════════════════════

describe("Adapter Factory Functions", () => {
  it("createOpenAIAdapter uses defaults", () => {
    const adapter = createOpenAIAdapter();
    // Just verify it creates without error
    expect(adapter).toBeInstanceOf(OpenAIAdapter);
  });

  it("createOpenAIAdapter accepts overrides", () => {
    const adapter = createOpenAIAdapter({
      model: "gpt-4-turbo",
      maxTokens: 8192,
      streaming: true,
    });
    expect(adapter).toBeInstanceOf(OpenAIAdapter);
  });

  it("createAnthropicAdapter uses defaults", () => {
    const adapter = createAnthropicAdapter();
    expect(adapter).toBeInstanceOf(AnthropicAdapter);
  });

  it("createAnthropicAdapter accepts overrides", () => {
    const adapter = createAnthropicAdapter({
      model: "claude-opus-4-20250514",
      maxTokens: 8192,
      streaming: true,
    });
    expect(adapter).toBeInstanceOf(AnthropicAdapter);
  });
});

// ═══════════════════════════════════════════════════════════════════════════
// TOOL CALL PROCESSING TESTS
// ═══════════════════════════════════════════════════════════════════════════

describe("Tool Call Processing", () => {
  const originalFetch = global.fetch;

  beforeEach(() => {
    vi.stubEnv("OPENAI_API_KEY", "test-key");
  });

  afterEach(() => {
    global.fetch = originalFetch;
    vi.unstubAllEnvs();
  });

  it("processes eval tool and yields request", async () => {
    const responseData = {
      choices: [{
        message: {
          role: "assistant",
          tool_calls: [{
            id: "call_1",
            type: "function",
            function: { name: "eval", arguments: '{"expr": "(+ 1 2)"}' },
          }],
        },
      }],
    };

    // Second call after tool result - returns final answer
    const secondResponseData = {
      choices: [{
        message: {
          role: "assistant",
          tool_calls: [{
            id: "call_2",
            type: "function",
            function: { name: "return", arguments: '{"value": "3"}' },
          }],
        },
      }],
    };

    let callCount = 0;
    global.fetch = vi.fn().mockImplementation(async () => {
      callCount++;
      return createBlockingResponse(callCount === 1 ? responseData : secondResponseData);
    });

    const adapter = new OpenAIAdapter({ model: "gpt-4o", streaming: false });

    const init: OracleInit = {
      tag: "Infer",
      payload: { tag: "Str", s: "Add 1+2" },
      envRef: "env-1",
      stateRef: "state-1",
    };

    const session = adapter.startSession(init);

    // First next - should yield eval request
    let step = await session.next({ tag: "RespAck" });
    expect(step.done).toBe(false);
    expect(step.value).toEqual({
      tag: "ReqEval",
      qexpr: "(+ 1 2)" as any,
      envRef: "env-1",
    });

    // Respond with value
    step = await session.next({ tag: "RespVal", value: { tag: "Num", n: 3 } });
    expect(step.done).toBe(true);
  });

  it("processes observe tool correctly", async () => {
    const responseData = {
      choices: [{
        message: {
          role: "assistant",
          tool_calls: [{
            id: "call_1",
            type: "function",
            function: { name: "observe", arguments: '{"what": "stack"}' },
          }],
        },
      }],
    };

    const secondResponseData = {
      choices: [{
        message: {
          role: "assistant",
          tool_calls: [{
            id: "call_2",
            type: "function",
            function: { name: "return", arguments: '{"value": "done"}' },
          }],
        },
      }],
    };

    let callCount = 0;
    global.fetch = vi.fn().mockImplementation(async () => {
      callCount++;
      return createBlockingResponse(callCount === 1 ? responseData : secondResponseData);
    });

    const adapter = new OpenAIAdapter({ model: "gpt-4o", streaming: false });

    const init: OracleInit = {
      tag: "Infer",
      payload: { tag: "Str", s: "Show stack" },
      envRef: "env-1",
      stateRef: "state-1",
    };

    const session = adapter.startSession(init);

    let step = await session.next({ tag: "RespAck" });
    expect(step.done).toBe(false);
    expect(step.value?.tag).toBe("ReqObserve");
    if (step.value?.tag === "ReqObserve") {
      expect(step.value.what).toEqual({ tag: "Stack", limit: 10 });
    }

    // Respond with observation
    step = await session.next({ tag: "RespObs", data: { frames: [] } });
    expect(step.done).toBe(true);
  });
});

// ═══════════════════════════════════════════════════════════════════════════
// MISSING API KEY TESTS
// ═══════════════════════════════════════════════════════════════════════════

describe("Missing API Key Handling", () => {
  afterEach(() => {
    vi.unstubAllEnvs();
  });

  it("OpenAI returns error meaning when no API key", async () => {
    // Explicitly unset the key
    vi.stubEnv("OPENAI_API_KEY", "");

    const adapter = new OpenAIAdapter({ model: "gpt-4o" });
    const session = adapter.startSession({
      tag: "Infer",
      payload: { tag: "Str", s: "test" },
      envRef: "env",
      stateRef: "state",
    });

    const result = await session.next({ tag: "RespAck" });
    expect(result.done).toBe(true);
    if (result.value?.tag === "Meaning") {
      expect(result.value.confidence).toBe(0);
      expect((result.value.trace as any)?.s).toContain("OPENAI_API_KEY");
    }
  });

  it("Anthropic returns error meaning when no API key", async () => {
    vi.stubEnv("ANTHROPIC_API_KEY", "");

    const adapter = new AnthropicAdapter({ model: "claude-sonnet-4-20250514" });
    const session = adapter.startSession({
      tag: "Infer",
      payload: { tag: "Str", s: "test" },
      envRef: "env",
      stateRef: "state",
    });

    const result = await session.next({ tag: "RespAck" });
    expect(result.done).toBe(true);
    if (result.value?.tag === "Meaning") {
      expect(result.value.confidence).toBe(0);
      expect((result.value.trace as any)?.s).toContain("ANTHROPIC_API_KEY");
    }
  });
});
