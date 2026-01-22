import { describe, it, expect } from "vitest";
import { renderEvent, renderTrace } from "../../src/core/session/render";
import type { SessionEvent } from "../../src/core/session/types";

describe("Pretty Renderer", () => {

  it("renders session header", () => {
    const event: SessionEvent = {
      type: "session",
      version: 1,
      id: "test-session",
      created: "2024-01-21T12:00:00Z",
    };

    const output = renderEvent(event);
    expect(output).toContain("test-session");
    expect(output).toContain("2024-01-21");
  });

  it("renders input event with REPL source", () => {
    const event: SessionEvent = {
      seq: 0,
      ts: Date.now(),
      type: "input",
      code: "(define x 42)",
    };

    const output = renderEvent(event);
    expect(output).toMatch(/\[000\]/);
    expect(output).toContain("REPL");
    expect(output).toContain(">");
    expect(output).toContain("(define x 42)");
  });

  it("renders step event with EVAL source", () => {
    const event: SessionEvent = {
      seq: 1,
      ts: Date.now(),
      type: "step",
      d: 0,
      ctrl: "define:x",
    };

    const output = renderEvent(event);
    expect(output).toMatch(/\[001\]/);
    expect(output).toContain("EVAL");
    expect(output).toContain("~");
    expect(output).toContain("define:x");
  });

  it("renders step with depth indentation", () => {
    const event: SessionEvent = {
      seq: 5,
      ts: Date.now(),
      type: "step",
      d: 2,
      ctrl: "nested:call",
    };

    const output = renderEvent(event);
    // Should have 4 spaces of indentation (2 per depth level)
    expect(output).toMatch(/\[005\]\s{4,}EVAL/);
  });

  it("renders checkpoint with SAVE source", () => {
    const event: SessionEvent = {
      seq: 3,
      ts: Date.now(),
      type: "checkpoint",
      d: 1,
      reason: "llm_boundary",
      stateId: "state-3",
    };

    const output = renderEvent(event);
    expect(output).toContain("SAVE");
    expect(output).toContain("*");
    expect(output).toContain("checkpoint");
    expect(output).toContain("llm_boundary");
  });

  it("renders effect with EFCT source", () => {
    const event: SessionEvent = {
      seq: 4,
      ts: Date.now(),
      type: "effect",
      d: 1,
      op: "infer.op",
      argsPreview: '["test"]',
    };

    const output = renderEvent(event);
    expect(output).toContain("EFCT");
    expect(output).toContain("!");
    expect(output).toContain("infer.op");
  });

  it("renders llm_req with LLM source and -> arrow", () => {
    const event: SessionEvent = {
      seq: 5,
      ts: Date.now(),
      type: "llm_req",
      d: 1,
      model: "gpt-4",
      promptPreview: "Test prompt here",
      receiptKey: "abc123",
    };

    const output = renderEvent(event);
    expect(output).toContain("LLM");
    expect(output).toContain("->");
    expect(output).toContain("gpt-4");
    expect(output).toContain("Test prompt");
  });

  it("renders llm_resp with LLM source and <- arrow", () => {
    const event: SessionEvent = {
      seq: 6,
      ts: Date.now(),
      type: "llm_resp",
      d: 1,
      valuePreview: "Response text",
      durationMs: 1234,
      tokens: 50,
      receiptKey: "abc123",
    };

    const output = renderEvent(event);
    expect(output).toContain("LLM");
    expect(output).toContain("<-");
    expect(output).toContain("Response text");
    expect(output).toContain("1234ms");
  });

  it("renders resume with RSME source", () => {
    const event: SessionEvent = {
      seq: 7,
      ts: Date.now(),
      type: "resume",
      d: 1,
      valuePreview: "resumed value",
    };

    const output = renderEvent(event);
    expect(output).toContain("RSME");
    expect(output).toContain("<~");
    expect(output).toContain("resumed value");
  });

  it("renders result with OUT source", () => {
    const event: SessionEvent = {
      seq: 10,
      ts: Date.now(),
      type: "result",
      value: "42",
    };

    const output = renderEvent(event);
    expect(output).toContain("OUT");
    expect(output).toContain("=>");
    expect(output).toContain("42");
  });

  it("renders error with ERR source", () => {
    const event: SessionEvent = {
      seq: 11,
      ts: Date.now(),
      type: "error",
      d: 0,
      message: "Something went wrong",
      stack: "Error at...",
    };

    const output = renderEvent(event);
    expect(output).toContain("ERR");
    expect(output).toContain("!!");
    expect(output).toContain("Something went wrong");
  });

  it("truncates long values", () => {
    const event: SessionEvent = {
      seq: 0,
      ts: Date.now(),
      type: "input",
      code: "x".repeat(200),
    };

    const output = renderEvent(event, { maxValueWidth: 50 });
    expect(output.length).toBeLessThan(200);
    expect(output).toContain("...");
  });

  it("renderTrace combines multiple events", () => {
    const events: SessionEvent[] = [
      { type: "session", version: 1, id: "test", created: "2024-01-21" },
      { seq: 0, ts: Date.now(), type: "input", code: "(+ 1 2)" },
      { seq: 1, ts: Date.now(), type: "step", d: 0, ctrl: "eval:+" },
      { seq: 2, ts: Date.now(), type: "result", value: "3" },
    ];

    const output = renderTrace(events);
    const lines = output.split("\n");

    expect(lines.length).toBe(4);
    expect(lines[0]).toContain("test");
    expect(lines[1]).toContain("REPL");
    expect(lines[2]).toContain("EVAL");
    expect(lines[3]).toContain("OUT");
  });

  it("optionally shows sequence numbers", () => {
    const event: SessionEvent = {
      seq: 5,
      ts: Date.now(),
      type: "result",
      value: "ok",
    };

    const withSeq = renderEvent(event, { showSeq: true });
    const withoutSeq = renderEvent(event, { showSeq: false });

    expect(withSeq).toContain("[005]");
    expect(withoutSeq).not.toContain("[005]");
  });
});
