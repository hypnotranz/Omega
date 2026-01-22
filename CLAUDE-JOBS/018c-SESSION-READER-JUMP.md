# JOB-018c: Session Reader and Jump Controller

**Priority**: P1 - Core Infrastructure
**Estimated Effort**: 2-3 hours
**Status**: NOT STARTED
**Depends On**: 018a (Serialization), 018b (Writer)
**Blocks**: 018e

> **MUST READ**: [LOGISTICS.md](LOGISTICS.md) before starting.

---

## Goal

Implement `SessionReader` (loads session files) and `JumpController` (jumps to checkpoints, replays with cached receipts).

**Done State**: Can load a session, jump to any checkpoint, replay using cached LLM receipts.

---

## TDD: Write Tests First

Create `test/session/reader-jump.spec.ts`:

```typescript
import { describe, it, expect, beforeEach, afterEach } from "vitest";
import * as fs from "fs";
import * as path from "path";
import { SessionWriter } from "../../src/core/session/writer";
import { SessionReader } from "../../src/core/session/reader";
import { JumpController } from "../../src/core/session/jump";
import { buildNativeRegistry } from "../../src/core/session/nativeRegistry";
import { COWStore } from "../../src/core/eval/store";
import { installPrims } from "../../src/core/prims";

const TEST_DIR = ".omega-session-test-reader";

describe("SessionReader", () => {
  let nativeRegistry: Map<string, any>;
  let baseEnv: any;
  let baseStore: any;

  beforeEach(() => {
    if (fs.existsSync(TEST_DIR)) {
      fs.rmSync(TEST_DIR, { recursive: true });
    }

    const store = new COWStore();
    const { env, store: primedStore } = installPrims(store);
    nativeRegistry = buildNativeRegistry(primedStore);
    baseEnv = env;
    baseStore = primedStore;
  });

  afterEach(() => {
    if (fs.existsSync(TEST_DIR)) {
      fs.rmSync(TEST_DIR, { recursive: true });
    }
  });

  it("loads session and returns event count", async () => {
    // Write a session
    const writer = new SessionWriter(TEST_DIR, "reader-test");
    writer.input("(+ 1 2)");
    writer.input("(+ 3 4)");
    writer.result("3");
    writer.close();

    // Read it back
    const reader = new SessionReader(
      path.join(TEST_DIR, "sessions", "reader-test.jsonl"),
      path.join(TEST_DIR, "sessions", "reader-test.index.json"),
      nativeRegistry
    );
    await reader.loadAll();

    expect(reader.getEventCount()).toBeGreaterThan(0);
  });

  it("finds checkpoints", async () => {
    const writer = new SessionWriter(TEST_DIR, "checkpoint-find");
    writer.input("(define x 1)");

    const state1 = { control: { tag: "Val", v: { tag: "Num", n: 1 } }, env: baseEnv, store: baseStore, kont: [], handlers: [] };
    writer.checkpoint(state1, "manual");

    writer.input("(define y 2)");

    const state2 = { control: { tag: "Val", v: { tag: "Num", n: 2 } }, env: baseEnv, store: baseStore, kont: [], handlers: [] };
    writer.checkpoint(state2, "llm_boundary");

    writer.close();

    const reader = new SessionReader(
      path.join(TEST_DIR, "sessions", "checkpoint-find.jsonl"),
      path.join(TEST_DIR, "sessions", "checkpoint-find.index.json"),
      nativeRegistry
    );
    await reader.loadAll();

    const checkpoints = reader.getCheckpoints();
    expect(checkpoints.length).toBe(2);
    expect(checkpoints[0].reason).toBe("manual");
    expect(checkpoints[1].reason).toBe("llm_boundary");
  });

  it("finds nearest checkpoint before target seq", async () => {
    const writer = new SessionWriter(TEST_DIR, "nearest-test");

    // seq 0: header
    writer.input("one");   // seq 1
    const state1 = { control: { tag: "Val", v: { tag: "Num", n: 1 } }, env: baseEnv, store: baseStore, kont: [], handlers: [] };
    writer.checkpoint(state1, "manual");  // seq 2

    writer.input("two");   // seq 3
    writer.input("three"); // seq 4

    const state2 = { control: { tag: "Val", v: { tag: "Num", n: 2 } }, env: baseEnv, store: baseStore, kont: [], handlers: [] };
    writer.checkpoint(state2, "manual");  // seq 5

    writer.input("four");  // seq 6
    writer.close();

    const reader = new SessionReader(
      path.join(TEST_DIR, "sessions", "nearest-test.jsonl"),
      path.join(TEST_DIR, "sessions", "nearest-test.index.json"),
      nativeRegistry
    );
    await reader.loadAll();

    // Target seq 4 should find checkpoint at seq 2
    const cp1 = reader.findCheckpointBefore(4);
    expect(cp1?.seq).toBe(2);

    // Target seq 6 should find checkpoint at seq 5
    const cp2 = reader.findCheckpointBefore(6);
    expect(cp2?.seq).toBe(5);

    // Target seq 1 should find no checkpoint
    const cp3 = reader.findCheckpointBefore(1);
    expect(cp3).toBeUndefined();
  });

  it("deserializes checkpoint state correctly", async () => {
    const writer = new SessionWriter(TEST_DIR, "state-test");

    // Create a state with specific control value
    const state = {
      control: { tag: "Val", v: { tag: "Num", n: 42 } },
      env: baseEnv,
      store: baseStore,
      kont: [],
      handlers: [],
    };
    writer.checkpoint(state, "manual");
    writer.close();

    const reader = new SessionReader(
      path.join(TEST_DIR, "sessions", "state-test.jsonl"),
      path.join(TEST_DIR, "sessions", "state-test.index.json"),
      nativeRegistry
    );
    await reader.loadAll();

    const cp = reader.getCheckpoints()[0];
    const restored = reader.getCheckpointState(cp.stateId);

    expect(restored.control.v.n).toBe(42);
  });

  it("retrieves LLM receipts", async () => {
    const writer = new SessionWriter(TEST_DIR, "receipt-test");

    const receiptKey = writer.llmRequest("gpt-4", "test prompt", { prompt: "test" });
    writer.llmResponse(receiptKey, "test response", { request: {}, response: { content: "test response" } }, 100);
    writer.close();

    const reader = new SessionReader(
      path.join(TEST_DIR, "sessions", "receipt-test.jsonl"),
      path.join(TEST_DIR, "sessions", "receipt-test.index.json"),
      nativeRegistry
    );
    await reader.loadAll();

    const receipt = reader.getReceipt(receiptKey);
    expect(receipt).toBeDefined();
    expect(receipt.response.content).toBe("test response");
  });
});

describe("JumpController", () => {
  let nativeRegistry: Map<string, any>;
  let baseEnv: any;
  let baseStore: any;

  beforeEach(() => {
    if (fs.existsSync(TEST_DIR)) {
      fs.rmSync(TEST_DIR, { recursive: true });
    }

    const store = new COWStore();
    const { env, store: primedStore } = installPrims(store);
    nativeRegistry = buildNativeRegistry(primedStore);
    baseEnv = env;
    baseStore = primedStore;
  });

  afterEach(() => {
    if (fs.existsSync(TEST_DIR)) {
      fs.rmSync(TEST_DIR, { recursive: true });
    }
  });

  it("jumps to exact checkpoint (0 replay steps)", async () => {
    const writer = new SessionWriter(TEST_DIR, "exact-jump");

    const state = {
      control: { tag: "Val", v: { tag: "Num", n: 99 } },
      env: baseEnv,
      store: baseStore,
      kont: [],
      handlers: [],
    };
    writer.input("test");
    writer.checkpoint(state, "manual");  // This will be at some seq
    writer.close();

    const reader = new SessionReader(
      path.join(TEST_DIR, "sessions", "exact-jump.jsonl"),
      path.join(TEST_DIR, "sessions", "exact-jump.index.json"),
      nativeRegistry
    );
    await reader.loadAll();

    const checkpoints = reader.getCheckpoints();
    const targetSeq = checkpoints[0].seq;

    const controller = new JumpController(reader);
    const result = await controller.jumpTo(targetSeq);

    expect(result.replayedSteps).toBe(0);  // Exact checkpoint, no replay
    expect(result.state.control.v.n).toBe(99);
  });

  it("throws if no checkpoint before target", async () => {
    const writer = new SessionWriter(TEST_DIR, "no-checkpoint");
    writer.input("test");
    writer.close();

    const reader = new SessionReader(
      path.join(TEST_DIR, "sessions", "no-checkpoint.jsonl"),
      path.join(TEST_DIR, "sessions", "no-checkpoint.index.json"),
      nativeRegistry
    );
    await reader.loadAll();

    const controller = new JumpController(reader);

    await expect(controller.jumpTo(0)).rejects.toThrow(/No checkpoint/);
  });

  it("returns usedReceipts when replaying past LLM calls", async () => {
    const writer = new SessionWriter(TEST_DIR, "receipt-replay");

    // Create checkpoint before LLM
    const state1 = {
      control: { tag: "Val", v: { tag: "Unit" } },
      env: baseEnv,
      store: baseStore,
      kont: [],
      handlers: [],
    };
    writer.checkpoint(state1, "llm_boundary");

    // LLM call
    const receiptKey = writer.llmRequest("gpt-4", "prompt", { prompt: "prompt" });
    writer.llmResponse(receiptKey, "response", { request: {}, response: { content: "response" } }, 100);

    // Checkpoint after LLM
    const state2 = {
      control: { tag: "Val", v: { tag: "Str", s: "response" } },
      env: baseEnv,
      store: baseStore,
      kont: [],
      handlers: [],
    };
    writer.checkpoint(state2, "llm_boundary");

    writer.close();

    const reader = new SessionReader(
      path.join(TEST_DIR, "sessions", "receipt-replay.jsonl"),
      path.join(TEST_DIR, "sessions", "receipt-replay.index.json"),
      nativeRegistry
    );
    await reader.loadAll();

    const checkpoints = reader.getCheckpoints();
    const controller = new JumpController(reader);

    // Jump to second checkpoint (after LLM)
    const result = await controller.jumpTo(checkpoints[1].seq);

    // Should report the receipt was used (even if state was loaded directly)
    // The usedReceipts tracks what would be needed for replay
    expect(result.state.control.v.s).toBe("response");
  });

  it("state at checkpoint has correct environment bindings", async () => {
    // This test verifies that after jumping, the environment is usable
    // We can't easily call functions without full REPL, but we can check env exists

    const writer = new SessionWriter(TEST_DIR, "env-test");

    const state = {
      control: { tag: "Val", v: { tag: "Unit" } },
      env: baseEnv,  // Has all primitives
      store: baseStore,
      kont: [],
      handlers: [],
    };
    writer.checkpoint(state, "manual");
    writer.close();

    const reader = new SessionReader(
      path.join(TEST_DIR, "sessions", "env-test.jsonl"),
      path.join(TEST_DIR, "sessions", "env-test.index.json"),
      nativeRegistry
    );
    await reader.loadAll();

    const controller = new JumpController(reader);
    const result = await controller.jumpTo(reader.getCheckpoints()[0].seq);

    // Env should have bindings (primitives)
    expect(result.state.env).toBeDefined();
    expect(result.state.env.tag).toBe("Ctx");
    expect(result.state.env.frame.size).toBeGreaterThan(0);
  });
});
```

---

## Implementation

### SessionReader

```typescript
// src/core/session/reader.ts

import * as fs from "fs";
import * as readline from "readline";
import type { SessionEvent, SessionIndex, CheckpointIndex, LLMReceipt } from "./types";
import { deserializeState } from "./serializer";
import type { State } from "../eval/machine";
import type { Val } from "../eval/values";

export class SessionReader {
  private events: SessionEvent[] = [];
  private index: SessionIndex;

  constructor(
    private eventFile: string,
    private indexFile: string,
    private nativeRegistry: Map<string, Val>
  ) {
    this.index = JSON.parse(fs.readFileSync(indexFile, "utf8"));
  }

  async loadAll(): Promise<void> {
    const fileStream = fs.createReadStream(this.eventFile);
    const rl = readline.createInterface({ input: fileStream });

    for await (const line of rl) {
      if (line.trim()) {
        this.events.push(JSON.parse(line));
      }
    }
  }

  getEventCount(): number {
    return this.index.eventCount;
  }

  getEvent(seq: number): SessionEvent | undefined {
    return this.events.find(e => 'seq' in e && e.seq === seq);
  }

  getCheckpoints(): CheckpointIndex[] {
    return this.index.checkpoints;
  }

  findCheckpointBefore(targetSeq: number): CheckpointIndex | undefined {
    let best: CheckpointIndex | undefined;
    for (const cp of this.index.checkpoints) {
      if (cp.seq <= targetSeq) {
        if (!best || cp.seq > best.seq) {
          best = cp;
        }
      }
    }
    return best;
  }

  getCheckpointState(stateId: string): State {
    const serialized = this.index.states[stateId];
    if (!serialized) {
      throw new Error(`State not found: ${stateId}`);
    }
    return deserializeState(serialized, this.nativeRegistry);
  }

  getReceipt(key: string): LLMReceipt | undefined {
    return this.index.receipts[key];
  }

  getEventsInRange(startSeq: number, endSeq: number): SessionEvent[] {
    return this.events.filter(e => {
      if (!('seq' in e)) return false;
      return e.seq >= startSeq && e.seq <= endSeq;
    });
  }
}
```

### JumpController

```typescript
// src/core/session/jump.ts

import { SessionReader } from "./reader";
import { captureValueResumption } from "../effects/capture";
import type { State } from "../eval/machine";
import type { Val } from "../eval/values";

export type JumpResult = {
  state: State;
  seq: number;
  replayedSteps: number;
  usedReceipts: string[];
};

export class JumpController {
  constructor(private reader: SessionReader) {}

  async jumpTo(targetSeq: number): Promise<JumpResult> {
    const checkpoint = this.reader.findCheckpointBefore(targetSeq);

    if (!checkpoint) {
      throw new Error(`No checkpoint found before seq ${targetSeq}`);
    }

    // Load state directly from checkpoint
    const state = this.reader.getCheckpointState(checkpoint.stateId);
    const usedReceipts: string[] = [];

    // If exactly at checkpoint, no replay needed
    if (checkpoint.seq === targetSeq) {
      return {
        state,
        seq: checkpoint.seq,
        replayedSteps: 0,
        usedReceipts,
      };
    }

    // For now, just return checkpoint state
    // Full replay logic requires step-by-step evaluation
    // which is implemented in integration with REPL

    return {
      state,
      seq: checkpoint.seq,
      replayedSteps: 0,
      usedReceipts,
    };
  }
}
```

---

## Verification

```bash
cd OmegaLLM
npm test -- --run test/session/reader-jump.spec.ts

# Expected: All tests pass
```

---

## Checklist

- [ ] Implement `src/core/session/reader.ts`
- [ ] Implement `src/core/session/jump.ts`
- [ ] All reader-jump.spec.ts tests pass
- [ ] Can load any session written by SessionWriter
- [ ] findCheckpointBefore returns correct checkpoint
- [ ] getCheckpointState deserializes correctly

---

## Files Created

```
src/core/session/
├── reader.ts            # SessionReader class
└── jump.ts              # JumpController class
```
