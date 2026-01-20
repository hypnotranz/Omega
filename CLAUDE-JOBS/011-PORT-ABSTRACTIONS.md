# JOB-011: Implement Port Abstractions (Hexagonal Architecture)

**Priority**: P1 - Critical Infrastructure
**Estimated Effort**: 3-4 days
**Skills Required**: TypeScript, interface design, adapter patterns
**Status**: NOT STARTED
**Depends On**: [009-FRAMEIR-PACKAGE](./009-FRAMEIR-PACKAGE.md)

> **MUST READ**: [LOGISTICS.md](LOGISTICS.md) before starting - covers testing, file locations, and proof of completion requirements.

---

## Quick Start

```bash
# 1. Read the architecture spec
cat docs/ARCHITECTURE-LANGUAGES-3.md | grep -A 50 "§32"
cat docs/ARCHITECTURE-LANGUAGES-6.md | grep -A 100 "§71"

# 2. Create port files
mkdir -p src/ports src/adapters
touch src/ports/{types,oracle,tool,store,sink,source,clock,rng,composite,index}.ts
touch src/adapters/{logging,replay,index}.ts

# 3. Run tests as you implement
npx vitest run test/ports/
npx vitest run test/adapters/

# 4. Verify type safety
npx tsc --noEmit
```

---

## Dependencies

| Type | Job | Reason |
|------|-----|--------|
| **REQUIRES** | [009-FRAMEIR-PACKAGE](./009-FRAMEIR-PACKAGE.md) | Needs Span type from meta.ts |
| **BLOCKS** | [012-OUTCOME-FAILURE-DIAGNOSTIC](./012-OUTCOME-FAILURE-DIAGNOSTIC.md) | Outcomes need ExecContext for error context |
| **BLOCKS** | [015-REPLAY-SYSTEM](./015-REPLAY-SYSTEM.md) | Replay requires port interfaces |

---

## What Already Exists

### Current Oracle Adapter (partial port concept)
```typescript
// src/core/oracle/adapter.ts - existing adapter pattern
export interface OracleAdapter {
  infer(meaning: Meaning, opts?: Partial<OracleOptions>): Promise<InferResult>;
}
```

### Current Effect Types (need migration to ports)
```typescript
// src/core/effects/runtimeImpl.ts - effects not using port abstraction
export interface RuntimeEffects {
  infer: ...
  callTool: ...
  // These should use OraclePort, ToolPort
}
```

### Gap Analysis
| Existing | Missing |
|----------|---------|
| OracleAdapter | Doesn't carry ExecContext, no capability checks |
| RuntimeEffects | Not structured as ports, no replay support |
| No ClockPort | Time access is raw Date.now() |
| No RngPort | Random access is raw Math.random() |
| No tracing | Port operations don't emit trace events |

---

## Reference Documents

This job implements specifications from:

| Document | Sections | Topics |
|----------|----------|--------|
| [ARCHITECTURE-LANGUAGES-3.md](../docs/ARCHITECTURE-LANGUAGES-3.md) | §32 | Ports & Adapters pattern |
| [ARCHITECTURE-LANGUAGES-6.md](../docs/ARCHITECTURE-LANGUAGES-6.md) | §71 | Port interfaces, ExecContext |

**Key Quote from §32**:
> "Your current FrameLisp kernel ops (`infer`, `call-tool`, `commit`, `emit`, `observe`) are already screaming for a Hexagonal split. Define these as *abstract ports* (interfaces) in OmegaLLM kernel."

---

## Executive Summary

Implement **abstract port interfaces** for all external effects:

1. **OraclePort** - LLM inference
2. **ToolPort** - External tool invocation
3. **StorePort** - Persistent storage
4. **SinkPort** - Output streaming
5. **SourcePort** - Input observation
6. **ClockPort** - Deterministic time
7. **RngPort** - Deterministic randomness

This enables:
- **Testability** - Swap real ports with fakes
- **Replay** - Log port interactions for deterministic replay
- **Policy injection** - Experts/roles can restrict port access
- **Capability enforcement** - Object-capability security

---

## Output Files

| File | Action | Description |
|------|--------|-------------|
| `src/ports/index.ts` | **CREATE** | Package exports |
| `src/ports/types.ts` | **CREATE** | ExecContext, CapabilitySet types |
| `src/ports/oracle.ts` | **CREATE** | OraclePort interface + types |
| `src/ports/tool.ts` | **CREATE** | ToolPort interface + types |
| `src/ports/store.ts` | **CREATE** | StorePort interface |
| `src/ports/sink.ts` | **CREATE** | SinkPort interface |
| `src/ports/source.ts` | **CREATE** | SourcePort interface |
| `src/ports/clock.ts` | **CREATE** | ClockPort interface |
| `src/ports/rng.ts` | **CREATE** | RngPort interface |
| `src/ports/composite.ts` | **CREATE** | PortSet combining all ports |
| `src/adapters/index.ts` | **CREATE** | Adapter exports |
| `src/adapters/logging.ts` | **CREATE** | LoggingPort decorators |
| `src/adapters/replay.ts` | **CREATE** | ReplayPort adapters |
| `test/ports/ports.spec.ts` | **CREATE** | Port interface tests |
| `test/adapters/replay.spec.ts` | **CREATE** | Replay adapter tests |

---

## Task 1: Define ExecContext and Capabilities

**Reference**: [ARCHITECTURE-LANGUAGES-6.md §71.2](../docs/ARCHITECTURE-LANGUAGES-6.md)

### 1.1 Create `src/ports/types.ts`

```typescript
import { Span } from "../frameir/meta";

/**
 * Budget state for resource tracking.
 */
export interface BudgetState {
  llmCalls?: { used: number; limit: number };
  tokens?: { used: number; limit: number };
  timeMs?: { used: number; limit: number };
  toolCalls?: { used: number; limit: number };
}

/**
 * Capability tokens for object-capability security.
 */
export interface CapabilitySet {
  /** Allowed oracle models */
  oracleCap?: {
    allowedModels: string[];
    constraints?: Record<string, unknown>;
  };

  /** Allowed tool contracts */
  toolCap?: {
    allowedContracts: Set<string>;  // contract IDs
  };

  /** Allowed stores */
  storeCap?: {
    allowedStores: Set<string>;     // store IDs
    readOnly?: boolean;
  };

  /** Allowed sinks */
  sinkCap?: {
    allowedSinks: Set<string>;      // sink IDs
  };

  /** Allowed sources */
  sourceCap?: {
    allowedSources: Set<string>;    // source IDs
  };
}

/**
 * Trace event types for replay logging.
 */
export type TraceEvent =
  | { tag: "E_OracleCall"; id: string; durationMs: number }
  | { tag: "E_ToolCall"; id: string; tool: string; durationMs: number }
  | { tag: "E_StoreOp"; id: string; op: "get" | "put"; key: string }
  | { tag: "E_SinkEmit"; id: string; sink: string }
  | { tag: "E_SourceObserve"; id: string; source: string }
  | { tag: "E_ClockRead"; id: string; valueMs: number }
  | { tag: "E_RngRead"; id: string; value: number }
  | { tag: "E_SchedulerDecision"; fiberId: string; choice: number };

/**
 * Trace sink for logging events.
 */
export interface TraceSink {
  emit(event: TraceEvent): void;
}

/**
 * Execution context passed to all port operations.
 * Carries correlation ID, span, budget, capabilities, and trace sink.
 */
export interface ExecContext {
  /** Correlation ID for the entire execution */
  runId: string;

  /** Current span for provenance */
  span?: Span;

  /** Remaining budget */
  budget?: BudgetState;

  /** Capability tokens */
  caps: CapabilitySet;

  /** Trace event sink */
  trace: TraceSink;
}

/**
 * Create a child context with updated span.
 */
export function childContext(parent: ExecContext, span: Span): ExecContext {
  return { ...parent, span };
}

/**
 * Create a context with restricted capabilities.
 */
export function restrictCaps(ctx: ExecContext, caps: Partial<CapabilitySet>): ExecContext {
  return {
    ...ctx,
    caps: {
      ...ctx.caps,
      ...caps
    }
  };
}
```

---

## Task 2: Define OraclePort

**Reference**: [ARCHITECTURE-LANGUAGES-6.md §71.1](../docs/ARCHITECTURE-LANGUAGES-6.md)

### 2.1 Create `src/ports/oracle.ts`

```typescript
import { ExecContext } from "./types";

/**
 * Request to oracle (LLM).
 */
export interface OracleRequest {
  /** Model identifier */
  model: string;

  /** Rendered prompt text */
  prompt: string;

  /** Tool specifications (for tool-augmented chat) */
  tools?: unknown[];

  /** Output schema for structured mode */
  outputSchema?: unknown;

  /** Sampling temperature */
  temperature?: number;

  /** Max tokens to generate */
  maxTokens?: number;

  /** Seed for reproducibility */
  seed?: number;

  /** Provider-specific metadata */
  metadata?: Record<string, unknown>;
}

/**
 * Response from oracle.
 */
export interface OracleResponse {
  /** Generated text */
  text: string;

  /** Token usage statistics */
  usage?: {
    promptTokens?: number;
    completionTokens?: number;
    totalTokens?: number;
  };

  /** Tool calls requested by model */
  toolCalls?: Array<{
    id: string;
    name: string;
    arguments: string;
  }>;

  /** Provider-specific raw response */
  raw?: unknown;
}

/**
 * Oracle port interface.
 * All LLM access must go through this port.
 */
export interface OraclePort {
  /**
   * Send inference request to oracle.
   * @param req - The inference request
   * @param ctx - Execution context (for capabilities, tracing)
   * @returns Oracle response
   * @throws if capability check fails or oracle error
   */
  infer(req: OracleRequest, ctx: ExecContext): Promise<OracleResponse>;
}

/**
 * Validate oracle request against capabilities.
 */
export function validateOracleCap(req: OracleRequest, ctx: ExecContext): void {
  const cap = ctx.caps.oracleCap;
  if (!cap) {
    throw new Error("No OracleCap in context");
  }
  if (!cap.allowedModels.includes(req.model)) {
    throw new Error(`Model ${req.model} not allowed by capability`);
  }
}
```

---

## Task 3: Define ToolPort

### 3.1 Create `src/ports/tool.ts`

```typescript
import { ExecContext } from "./types";

/**
 * Tool call request.
 */
export interface ToolCall {
  /** Tool name */
  name: string;

  /** Arguments (validated against contract.inputSchema before call) */
  args: unknown;

  /** Contract ID for audited execution */
  contractId: string;
}

/**
 * Tool call result.
 */
export interface ToolResult {
  /** Whether call succeeded */
  ok: boolean;

  /** Result value (validated against contract.outputSchema) */
  value?: unknown;

  /** Error info if failed */
  error?: {
    type: string;
    message: string;
    data?: unknown;
  };

  /** Provider-specific raw result */
  raw?: unknown;
}

/**
 * Tool port interface.
 * All external tool invocation must go through this port.
 */
export interface ToolPort {
  /**
   * Call an external tool.
   * @param call - The tool call request
   * @param ctx - Execution context
   * @returns Tool result
   */
  call(call: ToolCall, ctx: ExecContext): Promise<ToolResult>;
}

/**
 * Validate tool call against capabilities.
 */
export function validateToolCap(call: ToolCall, ctx: ExecContext): void {
  const cap = ctx.caps.toolCap;
  if (!cap) {
    throw new Error("No ToolCap in context");
  }
  if (!cap.allowedContracts.has(call.contractId)) {
    throw new Error(`Contract ${call.contractId} not allowed by capability`);
  }
}
```

---

## Task 4: Define StorePort

### 4.1 Create `src/ports/store.ts`

```typescript
import { ExecContext } from "./types";

/**
 * Store port interface.
 * Persistent key-value storage.
 */
export interface StorePort {
  /**
   * Get value by key.
   * @returns value or null if not found
   */
  get(storeId: string, key: string, ctx: ExecContext): Promise<unknown | null>;

  /**
   * Put value by key.
   */
  put(storeId: string, key: string, value: unknown, ctx: ExecContext): Promise<void>;

  /**
   * Check if key exists.
   */
  has(storeId: string, key: string, ctx: ExecContext): Promise<boolean>;

  /**
   * Delete key.
   */
  delete(storeId: string, key: string, ctx: ExecContext): Promise<void>;
}

/**
 * Validate store operation against capabilities.
 */
export function validateStoreCap(storeId: string, op: "get" | "put" | "delete", ctx: ExecContext): void {
  const cap = ctx.caps.storeCap;
  if (!cap) {
    throw new Error("No StoreCap in context");
  }
  if (!cap.allowedStores.has(storeId)) {
    throw new Error(`Store ${storeId} not allowed by capability`);
  }
  if (cap.readOnly && (op === "put" || op === "delete")) {
    throw new Error(`Store ${storeId} is read-only`);
  }
}
```

---

## Task 5: Define SinkPort and SourcePort

### 5.1 Create `src/ports/sink.ts`

```typescript
import { ExecContext } from "./types";

/**
 * Sink port interface.
 * For emitting items to output streams.
 */
export interface SinkPort {
  /**
   * Emit item to sink.
   */
  emit(sinkId: string, item: unknown, ctx: ExecContext): Promise<void>;
}
```

### 5.2 Create `src/ports/source.ts`

```typescript
import { ExecContext } from "./types";

/**
 * Source port interface.
 * For observing from input streams.
 */
export interface SourcePort {
  /**
   * Observe from source.
   * @param query - Optional query/filter
   * @returns Observed value
   */
  observe(sourceId: string, query: unknown | undefined, ctx: ExecContext): Promise<unknown>;
}
```

---

## Task 6: Define ClockPort and RngPort

**Critical for deterministic replay.**

### 6.1 Create `src/ports/clock.ts`

```typescript
import { ExecContext } from "./types";

/**
 * Clock port interface.
 * MUST be used for all time access to enable deterministic replay.
 */
export interface ClockPort {
  /**
   * Get current time in milliseconds.
   * In replay mode, returns logged time.
   */
  nowMs(ctx: ExecContext): number;

  /**
   * Sleep for specified duration.
   * In replay mode, may be no-op or use logged time.
   */
  sleepMs(ms: number, ctx: ExecContext): Promise<void>;
}
```

### 6.2 Create `src/ports/rng.ts`

```typescript
import { ExecContext } from "./types";

/**
 * RNG port interface.
 * MUST be used for all randomness to enable deterministic replay.
 */
export interface RngPort {
  /**
   * Get next random 32-bit unsigned integer.
   * In replay mode, returns logged value.
   */
  nextU32(ctx: ExecContext): number;

  /**
   * Get random float in [0, 1).
   */
  nextFloat(ctx: ExecContext): number;

  /**
   * Get random integer in [min, max).
   */
  nextInt(min: number, max: number, ctx: ExecContext): number;
}
```

---

## Task 7: Create Composite PortSet

### 7.1 Create `src/ports/composite.ts`

```typescript
import { OraclePort } from "./oracle";
import { ToolPort } from "./tool";
import { StorePort } from "./store";
import { SinkPort } from "./sink";
import { SourcePort } from "./source";
import { ClockPort } from "./clock";
import { RngPort } from "./rng";

/**
 * Complete set of ports for execution.
 */
export interface PortSet {
  oracle: OraclePort;
  tool: ToolPort;
  store: StorePort;
  sink: SinkPort;
  source: SourcePort;
  clock: ClockPort;
  rng: RngPort;
}

/**
 * Create a port set with all required ports.
 */
export function createPortSet(ports: PortSet): PortSet {
  return ports;
}
```

---

## Task 8: Implement Logging Decorators

**Reference**: [ARCHITECTURE-LANGUAGES-6.md §72](../docs/ARCHITECTURE-LANGUAGES-6.md)

### 8.1 Create `src/adapters/logging.ts`

```typescript
import { OraclePort, OracleRequest, OracleResponse } from "../ports/oracle";
import { ToolPort, ToolCall, ToolResult } from "../ports/tool";
import { ClockPort } from "../ports/clock";
import { RngPort } from "../ports/rng";
import { ExecContext, TraceEvent, TraceSink } from "../ports/types";

/**
 * Wrap oracle port with logging.
 */
export function loggingOracle(inner: OraclePort): OraclePort {
  return {
    async infer(req: OracleRequest, ctx: ExecContext): Promise<OracleResponse> {
      const id = `oracle:${Date.now()}:${Math.random().toString(36).slice(2)}`;
      const start = Date.now();

      try {
        const res = await inner.infer(req, ctx);
        ctx.trace.emit({
          tag: "E_OracleCall",
          id,
          durationMs: Date.now() - start
        });
        return res;
      } catch (e) {
        ctx.trace.emit({
          tag: "E_OracleCall",
          id,
          durationMs: Date.now() - start
        });
        throw e;
      }
    }
  };
}

/**
 * Wrap tool port with logging.
 */
export function loggingTool(inner: ToolPort): ToolPort {
  return {
    async call(call: ToolCall, ctx: ExecContext): Promise<ToolResult> {
      const id = `tool:${Date.now()}:${Math.random().toString(36).slice(2)}`;
      const start = Date.now();

      const res = await inner.call(call, ctx);
      ctx.trace.emit({
        tag: "E_ToolCall",
        id,
        tool: call.name,
        durationMs: Date.now() - start
      });
      return res;
    }
  };
}

/**
 * Wrap clock port with logging.
 */
export function loggingClock(inner: ClockPort): ClockPort {
  return {
    nowMs(ctx: ExecContext): number {
      const value = inner.nowMs(ctx);
      const id = `clock:${Date.now()}`;
      ctx.trace.emit({ tag: "E_ClockRead", id, valueMs: value });
      return value;
    },
    async sleepMs(ms: number, ctx: ExecContext): Promise<void> {
      return inner.sleepMs(ms, ctx);
    }
  };
}

/**
 * Wrap RNG port with logging.
 */
export function loggingRng(inner: RngPort): RngPort {
  return {
    nextU32(ctx: ExecContext): number {
      const value = inner.nextU32(ctx);
      const id = `rng:${Date.now()}`;
      ctx.trace.emit({ tag: "E_RngRead", id, value });
      return value;
    },
    nextFloat(ctx: ExecContext): number {
      return inner.nextFloat(ctx);
    },
    nextInt(min: number, max: number, ctx: ExecContext): number {
      return inner.nextInt(min, max, ctx);
    }
  };
}
```

---

## Task 9: Implement Replay Adapters

### 9.1 Create `src/adapters/replay.ts`

```typescript
import { OraclePort, OracleRequest, OracleResponse } from "../ports/oracle";
import { ClockPort } from "../ports/clock";
import { RngPort } from "../ports/rng";
import { ExecContext } from "../ports/types";

/**
 * Replay event log entry.
 */
export interface ReplayLogEntry {
  id: string;
  tag: string;
  request?: unknown;
  response?: unknown;
  value?: unknown;
}

/**
 * Replay log source.
 */
export interface ReplayLog {
  get(id: string): ReplayLogEntry | undefined;
  getByTag(tag: string): ReplayLogEntry[];
}

/**
 * Create replay oracle that returns logged responses.
 */
export function replayOracle(log: ReplayLog): OraclePort {
  const entries = log.getByTag("E_OracleCall");
  let index = 0;

  return {
    async infer(req: OracleRequest, ctx: ExecContext): Promise<OracleResponse> {
      const entry = entries[index++];
      if (!entry) {
        throw new Error("Replay log exhausted for oracle calls");
      }
      // Could validate req matches logged request
      return entry.response as OracleResponse;
    }
  };
}

/**
 * Create replay clock that returns logged times.
 */
export function replayClock(log: ReplayLog): ClockPort {
  const entries = log.getByTag("E_ClockRead");
  let index = 0;

  return {
    nowMs(ctx: ExecContext): number {
      const entry = entries[index++];
      if (!entry) {
        throw new Error("Replay log exhausted for clock reads");
      }
      return entry.value as number;
    },
    async sleepMs(ms: number, ctx: ExecContext): Promise<void> {
      // No-op in replay mode
    }
  };
}

/**
 * Create replay RNG that returns logged values.
 */
export function replayRng(log: ReplayLog): RngPort {
  const entries = log.getByTag("E_RngRead");
  let index = 0;

  return {
    nextU32(ctx: ExecContext): number {
      const entry = entries[index++];
      if (!entry) {
        throw new Error("Replay log exhausted for RNG reads");
      }
      return entry.value as number;
    },
    nextFloat(ctx: ExecContext): number {
      return this.nextU32(ctx) / 0xFFFFFFFF;
    },
    nextInt(min: number, max: number, ctx: ExecContext): number {
      return min + (this.nextU32(ctx) % (max - min));
    }
  };
}
```

---

## Verification Steps

### 1. Type Checking
```bash
npx tsc --noEmit src/ports/**/*.ts src/adapters/**/*.ts
```

### 2. Interface Tests
```bash
npx vitest run test/ports/
```

Tests must verify:
- Capability validation works
- Context propagation works
- Logging decorators emit events
- Replay adapters return logged values

### 3. Determinism Test
```bash
npx vitest run test/adapters/replay.spec.ts
```

Run a flow twice:
1. First with real ports + logging
2. Then with replay ports from log
3. Assert identical results

---

## Checklist

- [ ] Create `src/ports/types.ts` with ExecContext
- [ ] Create `src/ports/oracle.ts` with OraclePort
- [ ] Create `src/ports/tool.ts` with ToolPort
- [ ] Create `src/ports/store.ts` with StorePort
- [ ] Create `src/ports/sink.ts` with SinkPort
- [ ] Create `src/ports/source.ts` with SourcePort
- [ ] Create `src/ports/clock.ts` with ClockPort
- [ ] Create `src/ports/rng.ts` with RngPort
- [ ] Create `src/ports/composite.ts` with PortSet
- [ ] Create `src/adapters/logging.ts` with decorators
- [ ] Create `src/adapters/replay.ts` with replay adapters
- [ ] Create test suites
- [ ] All tests pass

---

## Success Criteria

1. **All ports defined** - Complete interface coverage
2. **Capability validation** - Access denied without proper caps
3. **Logging works** - All port operations emit trace events
4. **Replay works** - Deterministic replay from log
5. **Context propagation** - Span, budget, caps flow through

---

## Test Plan

### Happy Path Tests

| ID | Test Case | Input | Expected Output |
|----|-----------|-------|-----------------|
| HP-1 | OraclePort.infer returns response | Valid request with allowed model | OracleResponse with text |
| HP-2 | ToolPort.call executes tool | Valid tool call with allowed contract | ToolResult with ok=true |
| HP-3 | StorePort.get/put round-trips | put("key", "value") then get("key") | Returns "value" |
| HP-4 | ClockPort.nowMs returns time | Call nowMs() | Returns number > 0 |
| HP-5 | RngPort.nextU32 returns integer | Call nextU32() | Returns 0 ≤ n < 2^32 |
| HP-6 | SinkPort.emit sends to sink | emit("out", { data: 1 }) | No error, event logged |
| HP-7 | SourcePort.observe returns value | observe("input", query) | Returns observed value |
| HP-8 | PortSet composes all ports | createPortSet({...}) | PortSet with all 7 ports |
| HP-9 | childContext propagates span | childContext(ctx, newSpan) | New ctx with updated span |
| HP-10 | restrictCaps limits access | restrictCaps(ctx, { oracleCap: limited }) | Reduced capability set |

### Edge Case Tests

| ID | Test Case | Input | Expected Output |
|----|-----------|-------|-----------------|
| EC-1 | Empty capability set | CapabilitySet with no caps | All port calls fail capability check |
| EC-2 | Read-only store | storeCap.readOnly = true | get() works, put() throws |
| EC-3 | Budget exhausted | budget.llmCalls.used >= limit | OraclePort.infer should check budget |
| EC-4 | Zero timeout oracle | Oracle request with instant timeout | Proper timeout handling |
| EC-5 | RNG boundaries | nextInt(0, 1) | Always returns 0 |
| EC-6 | Empty replay log | Replay with no entries | Throws "log exhausted" |
| EC-7 | Null trace sink | TraceSink that silently drops | Port operations succeed |
| EC-8 | Missing span in context | ExecContext with span = undefined | Operations work without span |

### Error Cases

| ID | Test Case | Input | Expected Error |
|----|-----------|-------|----------------|
| ERR-1 | Disallowed model | oracleCap.allowedModels = ["gpt-4"], req.model = "gpt-3.5" | "Model gpt-3.5 not allowed by capability" |
| ERR-2 | Disallowed contract | toolCap.allowedContracts missing contractId | "Contract X not allowed by capability" |
| ERR-3 | Disallowed store | storeCap.allowedStores missing storeId | "Store X not allowed by capability" |
| ERR-4 | No OracleCap | ctx.caps.oracleCap = undefined | "No OracleCap in context" |
| ERR-5 | No ToolCap | ctx.caps.toolCap = undefined | "No ToolCap in context" |
| ERR-6 | No StoreCap | ctx.caps.storeCap = undefined | "No StoreCap in context" |
| ERR-7 | Replay mismatch | Replay oracle gets different request | Could warn/error on mismatch |

### Integration Tests

| ID | Test Case | Description |
|----|-----------|-------------|
| INT-1 | Logging decorator chain | loggingOracle(realOracle) emits E_OracleCall |
| INT-2 | Full replay round-trip | Record with logging, replay with replay adapters, assert identical |
| INT-3 | Mixed real + replay | Some ports real, some replay, execution completes |
| INT-4 | Context through call chain | ctx flows through oracle → tool → store sequence |
| INT-5 | Budget tracking | Budget decremented after each oracle call |

---

## Notes

### Why Hexagonal Architecture

The port abstraction enables:
1. **Testability**: Swap `OraclePort` with a mock that returns scripted responses
2. **Replay**: Log all port I/O, replay with deterministic adapters
3. **Policy Injection**: Wrap ports with capability-checking decorators
4. **Observability**: Logging decorators emit structured trace events

### Port vs. Adapter Distinction

- **Port**: Pure interface definition (e.g., `OraclePort`)
- **Adapter**: Concrete implementation (e.g., `OpenAIOracleAdapter`)
- **Decorator**: Wraps an adapter to add cross-cutting concerns (e.g., `loggingOracle`)

### ExecContext Design

ExecContext carries:
- `runId`: Correlation ID for the entire execution (for distributed tracing)
- `span`: Current provenance span (for error attribution)
- `budget`: Remaining resource budget (for governance)
- `caps`: Capability tokens (for security)
- `trace`: Sink for trace events (for observability)

This is a **dependency injection** pattern - ports receive everything they need through context.

### Replay Determinism Requirements

For deterministic replay to work:
- **ALL** non-deterministic operations must go through ports
- Time: Use `ClockPort.nowMs()` not `Date.now()`
- Random: Use `RngPort.nextU32()` not `Math.random()`
- External I/O: Oracle, Tool, Store, Sink, Source all logged

---

## Proof of Completion

When this job is complete:

1. **File Structure Verified**
   ```bash
   ls src/ports/*.ts   # 9 files
   ls src/adapters/*.ts  # 3 files
   ```

2. **All Interfaces Exported**
   ```bash
   grep "export interface" src/ports/*.ts | wc -l  # Should be ≥ 10
   ```

3. **Tests Pass**
   ```bash
   npx vitest run test/ports/ --reporter=verbose
   npx vitest run test/adapters/ --reporter=verbose
   ```

4. **Type Check Clean**
   ```bash
   npx tsc --noEmit 2>&1 | grep -c "error" # Should be 0
   ```

5. **Replay Integration Works**
   ```bash
   npx vitest run test/adapters/replay.spec.ts
   # Must include test that records, replays, and asserts identical results
   ```

---

## Footer

| Field | Value |
|-------|-------|
| Created | 2025-01-20 |
| Last Updated | 2025-01-20 |
| Author | Claude |
| Related Docs | [ARCHITECTURE-LANGUAGES-3.md §32](../docs/ARCHITECTURE-LANGUAGES-3.md), [ARCHITECTURE-LANGUAGES-6.md §71](../docs/ARCHITECTURE-LANGUAGES-6.md) |
| Predecessor | [009-FRAMEIR-PACKAGE](./009-FRAMEIR-PACKAGE.md) |
| Successors | [012-OUTCOME-FAILURE-DIAGNOSTIC](./012-OUTCOME-FAILURE-DIAGNOSTIC.md), [015-REPLAY-SYSTEM](./015-REPLAY-SYSTEM.md) |
