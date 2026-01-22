# JOB-018b: Session Writer and File Format

**Priority**: P1 - Core Infrastructure
**Estimated Effort**: 2-3 hours
**Status**: NOT STARTED
**Depends On**: 018a (State Serialization)
**Blocks**: 018c, 018e

> **MUST READ**: [LOGISTICS.md](LOGISTICS.md) before starting.

---

## Goal

Implement `SessionWriter` - writes session events to JSONL file with checkpoint index.

**Done State**: Can write events to JSONL, checkpoints are indexed, files are valid JSON.

---

## TDD: Write Tests First

Create `test/session/writer.spec.ts`:

```typescript
import { describe, it, expect, beforeEach, afterEach } from "vitest";
import * as fs from "fs";
import * as path from "path";
import { SessionWriter } from "../../src/core/session/writer";
import { COWStore } from "../../src/core/eval/store";
import { installPrims } from "../../src/core/prims";

const TEST_DIR = ".omega-session-test-writer";

describe("SessionWriter", () => {
  beforeEach(() => {
    if (fs.existsSync(TEST_DIR)) {
      fs.rmSync(TEST_DIR, { recursive: true });
    }
  });

  afterEach(() => {
    if (fs.existsSync(TEST_DIR)) {
      fs.rmSync(TEST_DIR, { recursive: true });
    }
  });

  it("creates session directory structure", () => {
    const writer = new SessionWriter(TEST_DIR);

    expect(fs.existsSync(TEST_DIR)).toBe(true);
    expect(fs.existsSync(path.join(TEST_DIR, "sessions"))).toBe(true);
  });

  it("writes header event as first line", () => {
    const writer = new SessionWriter(TEST_DIR, "test-session");
    writer.close();

    const content = fs.readFileSync(
      path.join(TEST_DIR, "sessions", "test-session.jsonl"),
      "utf8"
    );
    const firstLine = JSON.parse(content.split("\n")[0]);

    expect(firstLine.type).toBe("session");
    expect(firstLine.version).toBe(1);
    expect(firstLine.id).toBe("test-session");
  });

  it("writes input events with sequence numbers", () => {
    const writer = new SessionWriter(TEST_DIR, "input-test");

    writer.input("(define x 1)");
    writer.input("(+ x 2)");
    writer.close();

    const content = fs.readFileSync(
      path.join(TEST_DIR, "sessions", "input-test.jsonl"),
      "utf8"
    );
    const lines = content.trim().split("\n").map(l => JSON.parse(l));

    const inputs = lines.filter(l => l.type === "input");
    expect(inputs.length).toBe(2);
    expect(inputs[0].seq).toBe(0);
    expect(inputs[0].code).toBe("(define x 1)");
    expect(inputs[1].seq).toBe(1);
    expect(inputs[1].code).toBe("(+ x 2)");
  });

  it("writes step events with depth", () => {
    const writer = new SessionWriter(TEST_DIR, "step-test");

    writer.step("eval:define");
    writer.pushDepth();
    writer.step("eval:lambda");
    writer.popDepth();
    writer.step("return");
    writer.close();

    const content = fs.readFileSync(
      path.join(TEST_DIR, "sessions", "step-test.jsonl"),
      "utf8"
    );
    const lines = content.trim().split("\n").map(l => JSON.parse(l));
    const steps = lines.filter(l => l.type === "step");

    expect(steps[0].d).toBe(0);
    expect(steps[1].d).toBe(1);
    expect(steps[2].d).toBe(0);
  });

  it("writes checkpoint with state and updates index", () => {
    const store = new COWStore();
    const { env, store: primedStore } = installPrims(store);
    const state = {
      control: { tag: "Val", v: { tag: "Num", n: 42 } },
      env,
      store: primedStore,
      kont: [],
      handlers: [],
    };

    const writer = new SessionWriter(TEST_DIR, "checkpoint-test");
    writer.checkpoint(state, "llm_boundary");
    writer.close();

    // Check JSONL has checkpoint event
    const content = fs.readFileSync(
      path.join(TEST_DIR, "sessions", "checkpoint-test.jsonl"),
      "utf8"
    );
    const lines = content.trim().split("\n").map(l => JSON.parse(l));
    const checkpoint = lines.find(l => l.type === "checkpoint");

    expect(checkpoint).toBeDefined();
    expect(checkpoint.reason).toBe("llm_boundary");
    expect(checkpoint.stateId).toBeDefined();

    // Check index file has checkpoint and state
    const index = JSON.parse(
      fs.readFileSync(
        path.join(TEST_DIR, "sessions", "checkpoint-test.index.json"),
        "utf8"
      )
    );

    expect(index.checkpoints.length).toBe(1);
    expect(index.checkpoints[0].stateId).toBe(checkpoint.stateId);
    expect(index.states[checkpoint.stateId]).toBeDefined();
  });

  it("writes LLM request/response events with receipt", () => {
    const writer = new SessionWriter(TEST_DIR, "llm-test");

    const receiptKey = writer.llmRequest("gpt-4", "Hello world", { model: "gpt-4", prompt: "Hello world" });
    writer.llmResponse(receiptKey, "Hi there!", { request: {}, response: { content: "Hi there!" } }, 500, 10);
    writer.close();

    const content = fs.readFileSync(
      path.join(TEST_DIR, "sessions", "llm-test.jsonl"),
      "utf8"
    );
    const lines = content.trim().split("\n").map(l => JSON.parse(l));

    const req = lines.find(l => l.type === "llm_req");
    const resp = lines.find(l => l.type === "llm_resp");

    expect(req.model).toBe("gpt-4");
    expect(req.receiptKey).toBeDefined();
    expect(resp.receiptKey).toBe(req.receiptKey);
    expect(resp.durationMs).toBe(500);

    // Check receipt is in index
    const index = JSON.parse(
      fs.readFileSync(
        path.join(TEST_DIR, "sessions", "llm-test.index.json"),
        "utf8"
      )
    );
    expect(index.receipts[receiptKey]).toBeDefined();
    expect(index.receipts[receiptKey].response.content).toBe("Hi there!");
  });

  it("writes effect and resume events", () => {
    const writer = new SessionWriter(TEST_DIR, "effect-test");

    writer.effect("infer.op", ["test prompt"]);
    writer.resume("result value");
    writer.close();

    const content = fs.readFileSync(
      path.join(TEST_DIR, "sessions", "effect-test.jsonl"),
      "utf8"
    );
    const lines = content.trim().split("\n").map(l => JSON.parse(l));

    expect(lines.find(l => l.type === "effect")).toBeDefined();
    expect(lines.find(l => l.type === "resume")).toBeDefined();
  });

  it("writes result and error events", () => {
    const writer = new SessionWriter(TEST_DIR, "result-test");

    writer.result("42");
    writer.error("Something went wrong", "stack trace here");
    writer.close();

    const content = fs.readFileSync(
      path.join(TEST_DIR, "sessions", "result-test.jsonl"),
      "utf8"
    );
    const lines = content.trim().split("\n").map(l => JSON.parse(l));

    const result = lines.find(l => l.type === "result");
    const error = lines.find(l => l.type === "error");

    expect(result.value).toBe("42");
    expect(error.message).toBe("Something went wrong");
  });

  it("JSONL is valid (each line parses independently)", () => {
    const writer = new SessionWriter(TEST_DIR, "valid-jsonl");

    writer.input("(+ 1 2)");
    writer.step("eval:app");
    writer.result("3");
    writer.close();

    const content = fs.readFileSync(
      path.join(TEST_DIR, "sessions", "valid-jsonl.jsonl"),
      "utf8"
    );

    // Every non-empty line should parse
    for (const line of content.split("\n")) {
      if (line.trim()) {
        expect(() => JSON.parse(line)).not.toThrow();
      }
    }
  });

  it("index file is valid JSON", () => {
    const store = new COWStore();
    const { env, store: primedStore } = installPrims(store);

    const writer = new SessionWriter(TEST_DIR, "valid-index");
    writer.checkpoint({ control: { tag: "Val", v: { tag: "Unit" } }, env, store: primedStore, kont: [], handlers: [] }, "manual");
    writer.close();

    expect(() =>
      JSON.parse(
        fs.readFileSync(
          path.join(TEST_DIR, "sessions", "valid-index.index.json"),
          "utf8"
        )
      )
    ).not.toThrow();
  });
});
```

---

## Event Types

Create `src/core/session/types.ts`:

```typescript
export type SessionEvent =
  | SessionHeaderEvent
  | InputEvent
  | StepEvent
  | CheckpointEvent
  | LLMRequestEvent
  | LLMResponseEvent
  | EffectEvent
  | ResumeEvent
  | ResultEvent
  | ErrorEvent;

export type SessionHeaderEvent = {
  type: "session";
  version: 1;
  id: string;
  created: string;
  profile?: string;
};

export type InputEvent = {
  seq: number;
  ts: number;
  type: "input";
  code: string;
};

export type StepEvent = {
  seq: number;
  ts: number;
  type: "step";
  d: number;
  ctrl: string;
};

export type CheckpointEvent = {
  seq: number;
  ts: number;
  type: "checkpoint";
  d: number;
  reason: "llm_boundary" | "periodic" | "manual" | "effect";
  stateId: string;
};

export type LLMRequestEvent = {
  seq: number;
  ts: number;
  type: "llm_req";
  d: number;
  model: string;
  promptPreview: string;
  receiptKey: string;
};

export type LLMResponseEvent = {
  seq: number;
  ts: number;
  type: "llm_resp";
  d: number;
  valuePreview: string;
  tokens?: number;
  durationMs: number;
  receiptKey: string;
};

export type EffectEvent = {
  seq: number;
  ts: number;
  type: "effect";
  d: number;
  op: string;
  argsPreview: string;
};

export type ResumeEvent = {
  seq: number;
  ts: number;
  type: "resume";
  d: number;
  valuePreview: string;
};

export type ResultEvent = {
  seq: number;
  ts: number;
  type: "result";
  value: string;
};

export type ErrorEvent = {
  seq: number;
  ts: number;
  type: "error";
  d: number;
  message: string;
  stack?: string;
};

export type SessionIndex = {
  sessionId: string;
  eventCount: number;
  checkpoints: CheckpointIndex[];
  states: Record<string, any>;
  receipts: Record<string, LLMReceipt>;
};

export type CheckpointIndex = {
  seq: number;
  byteOffset: number;
  stateId: string;
  reason: string;
};

export type LLMReceipt = {
  key: string;
  request: any;
  response: any;
  timestamp: number;
  durationMs: number;
  tokens?: number;
};
```

---

## SessionWriter Implementation

See full implementation in parent job 018. Key methods:

- `constructor(sessionDir, sessionId?)` - Creates dirs, writes header
- `input(code)` - Logs user input
- `step(controlSummary)` - Logs CEKS step
- `pushDepth() / popDepth()` - Tracks nesting
- `checkpoint(state, reason)` - Serializes state, updates index
- `llmRequest(model, prompt, fullReq)` - Returns receiptKey
- `llmResponse(receiptKey, value, fullResp, durationMs)` - Stores receipt
- `effect(op, args)` - Logs effect trigger
- `resume(value)` - Logs effect resumption
- `result(value)` - Logs final result
- `error(message, stack?)` - Logs error
- `close()` - Finalizes index

---

## Verification

```bash
cd OmegaLLM
npm test -- --run test/session/writer.spec.ts

# Expected: All tests pass
```

---

## Checklist

- [ ] Create `src/core/session/types.ts`
- [ ] Implement `src/core/session/writer.ts`
- [ ] All writer.spec.ts tests pass
- [ ] JSONL files are valid (each line parses)
- [ ] Index files are valid JSON

---

## Files Created

```
src/core/session/
├── types.ts             # Event types, SessionIndex
└── writer.ts            # SessionWriter class
```
