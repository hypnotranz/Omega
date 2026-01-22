# JOB-018d: Pretty Renderer

**Priority**: P2 - Nice to Have
**Estimated Effort**: 1-2 hours
**Status**: DONE
**Depends On**: 018b (types.ts)
**Blocks**: 018e (optional - REPL can work without pretty output)

> **MUST READ**: [LOGISTICS.md](LOGISTICS.md) before starting.

---

## Goal

Implement `renderEvent()` and `renderTrace()` - convert session events to readable, structured output.

**Done State**: Events render with `[seq] SOURCE > content` format with proper depth indentation.

---

## Output Format

```
[000] REPL > (classify "I am frustrated")
[001] EVAL ~ call:classify
[002]   EVAL ~ effect:infer.op
[003]   SAVE * checkpoint (llm_boundary)
[004]   EFCT ! infer.op
[005]   LLM  -> gpt-4o-mini: "Sentiment: I am frustrated"
[006]   LLM  <- "negative" (1234ms)
[007]   SAVE * checkpoint (llm_boundary)
[008]   RSME <~ "negative"
[009] EVAL ~ return
[010] OUT  => "negative"
```

**Key elements:**
- `[seq]` - 3-digit zero-padded sequence number
- Indentation - 2 spaces per depth level
- Source - 4-char tag: REPL, EVAL, SAVE, EFCT, LLM, RSME, OUT, ERR
- Symbol - Visual indicator: `>`, `~`, `*`, `!`, `->`, `<-`, `<~`, `=>`, `!!`
- Content - Truncated to max width

---

## TDD: Write Tests First

Create `test/session/render.spec.ts`:

```typescript
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
```

---

## Implementation

```typescript
// src/core/session/render.ts

import type { SessionEvent } from "./types";

export type RenderOptions = {
  showTime?: boolean;
  showSeq?: boolean;
  maxValueWidth?: number;
  colors?: boolean;
};

const DEFAULT_OPTIONS: RenderOptions = {
  showTime: false,
  showSeq: true,
  maxValueWidth: 60,
  colors: false,
};

export function renderEvent(event: SessionEvent, opts: RenderOptions = {}): string {
  const o = { ...DEFAULT_OPTIONS, ...opts };

  if (event.type === "session") {
    return `=== Session ${event.id} (${event.created}) ===`;
  }

  if (!('seq' in event)) return "";

  const seq = o.showSeq ? `[${String(event.seq).padStart(3, "0")}]` : "";
  const depth = 'd' in event ? (event as any).d : 0;
  const indent = "  ".repeat(depth);

  let source: string;
  let symbol: string;
  let content: string;

  switch (event.type) {
    case "input":
      source = "REPL";
      symbol = ">";
      content = truncate(event.code, o.maxValueWidth!);
      break;

    case "step":
      source = "EVAL";
      symbol = "~";
      content = event.ctrl;
      break;

    case "checkpoint":
      source = "SAVE";
      symbol = "*";
      content = `checkpoint (${event.reason})`;
      break;

    case "effect":
      source = "EFCT";
      symbol = "!";
      content = event.op;
      break;

    case "llm_req":
      source = "LLM ";
      symbol = "->";
      content = `${event.model}: ${truncate(event.promptPreview, o.maxValueWidth! - 20)}`;
      break;

    case "llm_resp":
      source = "LLM ";
      symbol = "<-";
      content = `${truncate(event.valuePreview, o.maxValueWidth! - 20)} (${event.durationMs}ms)`;
      break;

    case "resume":
      source = "RSME";
      symbol = "<~";
      content = truncate(event.valuePreview, o.maxValueWidth!);
      break;

    case "result":
      source = "OUT ";
      symbol = "=>";
      content = truncate(event.value, o.maxValueWidth!);
      break;

    case "error":
      source = "ERR ";
      symbol = "!!";
      content = event.message;
      break;

    default:
      return "";
  }

  return `${seq} ${indent}${source} ${symbol} ${content}`;
}

function truncate(s: string, max: number): string {
  if (s.length <= max) return s;
  return s.slice(0, max - 3) + "...";
}

export function renderTrace(events: SessionEvent[], opts: RenderOptions = {}): string {
  return events.map(e => renderEvent(e, opts)).filter(Boolean).join("\n");
}
```

---

## Verification

```bash
cd OmegaLLM
npm test -- --run test/session/render.spec.ts

# Expected: All tests pass
```

---

## Checklist

- [ ] Implement `src/core/session/render.ts`
- [ ] All render.spec.ts tests pass
- [ ] Output format matches spec
- [ ] Depth indentation works correctly
- [ ] Values truncate properly

---

## Files Created

```
src/core/session/
└── render.ts            # renderEvent(), renderTrace()
```
