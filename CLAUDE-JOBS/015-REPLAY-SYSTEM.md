# JOB-015: Replay System

**Priority**: P1 - Critical Infrastructure
**Estimated Effort**: 3-4 days
**Skills Required**: TypeScript, deterministic execution, content-addressed storage
**Status**: NOT STARTED
**Depends On**: [011-PORT-ABSTRACTIONS](./011-PORT-ABSTRACTIONS.md), [012-OUTCOME-FAILURE-DIAGNOSTIC](./012-OUTCOME-FAILURE-DIAGNOSTIC.md)

> **MUST READ**: [LOGISTICS.md](LOGISTICS.md) before starting - covers testing, file locations, and proof of completion requirements.

---

## Quick Start

```bash
# 1. Read the architecture spec
cat docs/ARCHITECTURE-LANGUAGES-4.md | grep -A 100 "§48"
cat docs/ARCHITECTURE-LANGUAGES-6.md | grep -A 50 "§72"

# 2. Create replay files
mkdir -p src/core/replay test/replay/golden
touch src/core/replay/{events,log,adapters,runner,guards,index}.ts

# 3. Run tests as you implement
npx vitest run test/replay/

# 4. Verify type safety
npx tsc --noEmit
```

---

## Dependencies

| Type | Job | Reason |
|------|-----|--------|
| **REQUIRES** | [011-PORT-ABSTRACTIONS](./011-PORT-ABSTRACTIONS.md) | Logging/replay adapters wrap ports |
| **REQUIRES** | [012-OUTCOME-FAILURE-DIAGNOSTIC](./012-OUTCOME-FAILURE-DIAGNOSTIC.md) | Returns Outcome, emits Diagnostics |
| **REQUIRES** | [014-COMPILATION-PIPELINE](./014-COMPILATION-PIPELINE.md) | Needs compiled bundles to replay |

---

## What Already Exists

### No Existing Replay Infrastructure
Currently OmegaLLM has no replay capability. All effects are executed live.

### Scheduler Decisions (partial)
```typescript
// src/core/concurrency/scheduler.ts - has decision logging
// extractDecisions(ledger) exists but not integrated with ports
```

### Gap Analysis
| Should Exist (per §48, §72) | Currently |
|------------------------------|-----------|
| LoggingPort wrappers | No port logging |
| ReplayPort adapters | No replay support |
| Content-addressed blob store | Inline payloads |
| Scheduler decision logging | Partial |
| Flow hash validation | None |
| Determinism guards | None |

---

## Architecture Reference

From ARCHITECTURE-LANGUAGES-4.md §48:

> To replay a run exactly you must record:
> 1. **Scheduler decisions** - you already have `extractDecisions(ledger)`
> 2. **All PortEffect interactions** - oracle requests/responses, tool calls/results, observe results, store reads/writes
> 3. **Randomness** - RNG seeds and draws, or provide RNG via port
>
> **ReplayPort architecture**: Implement `ReplayOraclePort`, `ReplayToolPort`, etc., as **Proxy** adapters that:
> - on first run: record interactions keyed by `spanId` + request hash
> - on replay: return recorded responses and validate request hashes match

From ARCHITECTURE-LANGUAGES-6.md §72:

> **Core replay principles:**
> 1. Every effect is logged as an event with enough data to reproduce the result
> 2. In replay mode, ports become "replay adapters" that read from the log
> 3. Scheduler decisions are logged, so concurrency is replayable

## Type Definitions

### Replay Events

```typescript
// src/core/replay/events.ts

import { OracleRequest, OracleResponse } from '../ports/oracle';
import { ToolCall, ToolResult } from '../ports/tool';
import { Span } from '../frameir/meta';

/**
 * Replay event taxonomy
 *
 * Reference: ARCHITECTURE-LANGUAGES-6.md §72.2
 * Use hashes (claim checks) instead of embedding large payloads
 */

export type ReplayEvent =
  | SchedulerDecisionEvent
  | OracleCallEvent
  | ToolCallEvent
  | StorePutEvent
  | StoreGetEvent
  | SinkEmitEvent
  | SourceObserveEvent
  | ClockNowEvent
  | RngNextEvent
  | YieldPointEvent
  | FailureEvent;

export interface EventBase {
  timestamp: number;       // Logical timestamp (monotonic counter)
  spanId: string;          // Current span ID
  parentSpanId?: string;   // Parent span for hierarchy
}

export interface SchedulerDecisionEvent extends EventBase {
  tag: 'E_SchedulerDecision';
  fiberId: string;
  choice: number;          // Which fiber/branch was chosen
  alternatives: number;    // Total alternatives available
}

export interface OracleCallEvent extends EventBase {
  tag: 'E_OracleCall';
  id: string;              // Unique call ID
  reqHash: string;         // Hash of request for matching
  req: OracleRequest;      // Full request (for debugging)
  resHash: string;         // Hash of response
  res: OracleResponse;     // Full response
  durationMs: number;
}

export interface ToolCallEvent extends EventBase {
  tag: 'E_ToolCall';
  id: string;
  callHash: string;        // Hash of tool call
  call: ToolCall;
  resHash: string;
  res: ToolResult;
  durationMs: number;
}

export interface StorePutEvent extends EventBase {
  tag: 'E_StorePut';
  storeId: string;
  key: string;
  valueHash: string;       // Content-addressed value
}

export interface StoreGetEvent extends EventBase {
  tag: 'E_StoreGet';
  storeId: string;
  key: string;
  hit: boolean;
  valueHash?: string;      // Only if hit
}

export interface SinkEmitEvent extends EventBase {
  tag: 'E_SinkEmit';
  sinkId: string;
  itemHash: string;
}

export interface SourceObserveEvent extends EventBase {
  tag: 'E_SourceObserve';
  sourceId: string;
  queryHash: string;
  resHash: string;
}

export interface ClockNowEvent extends EventBase {
  tag: 'E_ClockNow';
  valueMs: number;
}

export interface RngNextEvent extends EventBase {
  tag: 'E_RngNextU32';
  value: number;
}

export interface YieldPointEvent extends EventBase {
  tag: 'E_YieldPoint';
  label: string;
  flowHash?: string;       // Current flow node hash
}

export interface FailureEvent extends EventBase {
  tag: 'E_Failure';
  code: string;
  message: string;
  context?: Record<string, unknown>;
}
```

### Replay Log

```typescript
// src/core/replay/log.ts

import { ReplayEvent } from './events';

/**
 * Replay log format
 *
 * Reference: ARCHITECTURE-LANGUAGES-6.md §72
 * "Store payload blobs separately keyed by hash. This avoids log bloat."
 */

export interface ReplayLog {
  version: '1';
  runId: string;
  startTime: string;       // ISO 8601
  endTime?: string;
  flowHash: string;        // Hash of executed flow
  semanticSalt: string;    // Config hash (model, tool versions, etc.)
  events: ReplayEvent[];
  blobs: Map<string, unknown>;  // hash -> payload
  metadata: {
    oracleConfig?: Record<string, unknown>;
    toolVersions?: Record<string, string>;
    rngSeed?: number;
  };
}

/**
 * Replay log writer
 */
export interface ReplayLogWriter {
  readonly runId: string;

  /** Record an event */
  record(event: Omit<ReplayEvent, 'timestamp'>): void;

  /** Store a blob, returns content hash */
  storeBlob(value: unknown): string;

  /** Finalize and return complete log */
  finalize(): ReplayLog;
}

/**
 * Replay log reader
 */
export interface ReplayLogReader {
  readonly runId: string;
  readonly events: ReplayEvent[];

  /** Get next event of given type, optionally matching predicate */
  nextEvent<T extends ReplayEvent['tag']>(
    tag: T,
    match?: (e: Extract<ReplayEvent, { tag: T }>) => boolean
  ): Extract<ReplayEvent, { tag: T }> | undefined;

  /** Peek at next event without consuming */
  peekEvent(): ReplayEvent | undefined;

  /** Get blob by hash */
  getBlob(hash: string): unknown | undefined;

  /** Check if replay is complete */
  isComplete(): boolean;

  /** Get replay progress */
  progress(): { current: number; total: number };
}

/**
 * Create a replay log writer
 */
export function createReplayLogWriter(
  runId: string,
  flowHash: string,
  semanticSalt: string,
  metadata?: ReplayLog['metadata']
): ReplayLogWriter {
  const events: ReplayEvent[] = [];
  const blobs = new Map<string, unknown>();
  let timestamp = 0;

  return {
    runId,

    record(event) {
      events.push({ ...event, timestamp: timestamp++ } as ReplayEvent);
    },

    storeBlob(value) {
      const hash = computeContentHash(value);
      blobs.set(hash, value);
      return hash;
    },

    finalize() {
      return {
        version: '1',
        runId,
        startTime: events[0] ? new Date().toISOString() : new Date().toISOString(),
        endTime: new Date().toISOString(),
        flowHash,
        semanticSalt,
        events,
        blobs,
        metadata: metadata || {}
      };
    }
  };
}

/**
 * Create a replay log reader from a log
 */
export function createReplayLogReader(log: ReplayLog): ReplayLogReader {
  let cursor = 0;

  return {
    runId: log.runId,
    events: log.events,

    nextEvent(tag, match) {
      while (cursor < log.events.length) {
        const event = log.events[cursor];
        if (event.tag === tag) {
          if (!match || match(event as any)) {
            cursor++;
            return event as any;
          }
        }
        cursor++;
      }
      return undefined;
    },

    peekEvent() {
      return log.events[cursor];
    },

    getBlob(hash) {
      return log.blobs.get(hash);
    },

    isComplete() {
      return cursor >= log.events.length;
    },

    progress() {
      return { current: cursor, total: log.events.length };
    }
  };
}

function computeContentHash(value: unknown): string {
  // Use canonical JSON + SHA-256
  const canonical = JSON.stringify(value, Object.keys(value as object).sort());
  // In real impl, use crypto.subtle.digest or similar
  return `sha256:${simpleHash(canonical)}`;
}

function simpleHash(str: string): string {
  let hash = 0;
  for (let i = 0; i < str.length; i++) {
    const char = str.charCodeAt(i);
    hash = ((hash << 5) - hash) + char;
    hash = hash & hash;
  }
  return Math.abs(hash).toString(16).padStart(8, '0');
}
```

### Replay Adapters

```typescript
// src/core/replay/adapters.ts

import { OraclePort, OracleRequest, OracleResponse } from '../ports/oracle';
import { ToolPort, ToolCall, ToolResult } from '../ports/tool';
import { StorePort } from '../ports/store';
import { ClockPort } from '../ports/clock';
import { RngPort } from '../ports/rng';
import { ExecContext } from '../ports/context';
import { ReplayLogWriter, ReplayLogReader } from './log';

/**
 * Logging decorator for ports - records all interactions
 *
 * Reference: ARCHITECTURE-LANGUAGES-6.md §72.3
 * "Real ports: LoggingPort(realPort, traceSink)"
 */

export function loggingOraclePort(
  inner: OraclePort,
  writer: ReplayLogWriter
): OraclePort {
  return {
    async infer(req: OracleRequest, ctx: ExecContext): Promise<OracleResponse> {
      const callId = generateCallId();
      const reqHash = writer.storeBlob(req);
      const start = Date.now();

      const res = await inner.infer(req, ctx);

      const resHash = writer.storeBlob(res);
      writer.record({
        tag: 'E_OracleCall',
        spanId: ctx.span?.spanId || 'root',
        parentSpanId: ctx.span?.parentId,
        id: callId,
        reqHash,
        req,
        resHash,
        res,
        durationMs: Date.now() - start
      });

      return res;
    }
  };
}

export function loggingToolPort(
  inner: ToolPort,
  writer: ReplayLogWriter
): ToolPort {
  return {
    async call(tool: ToolCall, ctx: ExecContext): Promise<ToolResult> {
      const callId = generateCallId();
      const callHash = writer.storeBlob(tool);
      const start = Date.now();

      const res = await inner.call(tool, ctx);

      const resHash = writer.storeBlob(res);
      writer.record({
        tag: 'E_ToolCall',
        spanId: ctx.span?.spanId || 'root',
        parentSpanId: ctx.span?.parentId,
        id: callId,
        callHash,
        call: tool,
        resHash,
        res,
        durationMs: Date.now() - start
      });

      return res;
    }
  };
}

export function loggingStorePort(
  inner: StorePort,
  writer: ReplayLogWriter
): StorePort {
  return {
    async get(key: string, ctx: ExecContext): Promise<unknown | null> {
      const result = await inner.get(key, ctx);
      const hit = result !== null;

      writer.record({
        tag: 'E_StoreGet',
        spanId: ctx.span?.spanId || 'root',
        storeId: 'default',
        key,
        hit,
        valueHash: hit ? writer.storeBlob(result) : undefined
      });

      return result;
    },

    async put(key: string, value: unknown, ctx: ExecContext): Promise<void> {
      await inner.put(key, value, ctx);

      writer.record({
        tag: 'E_StorePut',
        spanId: ctx.span?.spanId || 'root',
        storeId: 'default',
        key,
        valueHash: writer.storeBlob(value)
      });
    }
  };
}

export function loggingClockPort(
  inner: ClockPort,
  writer: ReplayLogWriter
): ClockPort {
  return {
    nowMs(ctx: ExecContext): number {
      const value = inner.nowMs(ctx);

      writer.record({
        tag: 'E_ClockNow',
        spanId: ctx.span?.spanId || 'root',
        valueMs: value
      });

      return value;
    },

    async sleepMs(ms: number, ctx: ExecContext): Promise<void> {
      return inner.sleepMs(ms, ctx);
    }
  };
}

export function loggingRngPort(
  inner: RngPort,
  writer: ReplayLogWriter
): RngPort {
  return {
    nextU32(ctx: ExecContext): number {
      const value = inner.nextU32(ctx);

      writer.record({
        tag: 'E_RngNextU32',
        spanId: ctx.span?.spanId || 'root',
        value
      });

      return value;
    }
  };
}

/**
 * Replay adapters - return recorded values instead of calling real ports
 *
 * Reference: ARCHITECTURE-LANGUAGES-6.md §72.3
 * "Replay ports: ReplayPort(traceSource)"
 */

export class ReplayMismatchError extends Error {
  constructor(
    public readonly expected: unknown,
    public readonly actual: unknown,
    public readonly eventType: string
  ) {
    super(`Replay mismatch in ${eventType}: expected ${JSON.stringify(expected)}, got ${JSON.stringify(actual)}`);
    this.name = 'ReplayMismatchError';
  }
}

export function replayOraclePort(reader: ReplayLogReader): OraclePort {
  return {
    async infer(req: OracleRequest, ctx: ExecContext): Promise<OracleResponse> {
      const event = reader.nextEvent('E_OracleCall');

      if (!event) {
        throw new ReplayMismatchError(
          'OracleCall event',
          'end of log',
          'E_OracleCall'
        );
      }

      // Validate request matches (weak matching - ignores metadata)
      const reqMatches = weakRequestMatch(req, event.req);
      if (!reqMatches) {
        throw new ReplayMismatchError(event.req, req, 'E_OracleCall');
      }

      return event.res;
    }
  };
}

export function replayToolPort(reader: ReplayLogReader): ToolPort {
  return {
    async call(tool: ToolCall, ctx: ExecContext): Promise<ToolResult> {
      const event = reader.nextEvent('E_ToolCall');

      if (!event) {
        throw new ReplayMismatchError(
          'ToolCall event',
          'end of log',
          'E_ToolCall'
        );
      }

      // Validate call matches
      if (tool.name !== event.call.name ||
          JSON.stringify(tool.args) !== JSON.stringify(event.call.args)) {
        throw new ReplayMismatchError(event.call, tool, 'E_ToolCall');
      }

      return event.res;
    }
  };
}

export function replayStorePort(reader: ReplayLogReader): StorePort {
  return {
    async get(key: string, ctx: ExecContext): Promise<unknown | null> {
      const event = reader.nextEvent('E_StoreGet', e => e.key === key);

      if (!event) {
        // No recorded get - return null (store was empty)
        return null;
      }

      if (!event.hit) return null;
      return reader.getBlob(event.valueHash!);
    },

    async put(key: string, value: unknown, ctx: ExecContext): Promise<void> {
      const event = reader.nextEvent('E_StorePut', e => e.key === key);
      // In replay mode, we don't actually write - just verify event exists
      if (!event) {
        throw new ReplayMismatchError(
          `StorePut for key ${key}`,
          'no matching event',
          'E_StorePut'
        );
      }
    }
  };
}

export function replayClockPort(reader: ReplayLogReader): ClockPort {
  return {
    nowMs(ctx: ExecContext): number {
      const event = reader.nextEvent('E_ClockNow');

      if (!event) {
        throw new ReplayMismatchError(
          'ClockNow event',
          'end of log',
          'E_ClockNow'
        );
      }

      return event.valueMs;
    },

    async sleepMs(ms: number, ctx: ExecContext): Promise<void> {
      // In replay mode, don't actually sleep
      return;
    }
  };
}

export function replayRngPort(reader: ReplayLogReader): RngPort {
  return {
    nextU32(ctx: ExecContext): number {
      const event = reader.nextEvent('E_RngNextU32');

      if (!event) {
        throw new ReplayMismatchError(
          'RngNextU32 event',
          'end of log',
          'E_RngNextU32'
        );
      }

      return event.value;
    }
  };
}

/**
 * Weak request matching - ignores fields that don't affect semantics
 */
function weakRequestMatch(actual: OracleRequest, recorded: OracleRequest): boolean {
  return (
    actual.model === recorded.model &&
    actual.prompt === recorded.prompt &&
    JSON.stringify(actual.tools) === JSON.stringify(recorded.tools) &&
    JSON.stringify(actual.outputSchema) === JSON.stringify(recorded.outputSchema)
  );
}

let callIdCounter = 0;
function generateCallId(): string {
  return `call:${callIdCounter++}`;
}
```

### Replay Runner

```typescript
// src/core/replay/runner.ts

import { FlowIR, IRBundle } from '../frameir';
import { Outcome } from '../outcome';
import { ExecContext } from '../ports/context';
import { ReplayLog, ReplayLogWriter, ReplayLogReader,
         createReplayLogWriter, createReplayLogReader } from './log';
import { loggingOraclePort, loggingToolPort, loggingStorePort,
         loggingClockPort, loggingRngPort,
         replayOraclePort, replayToolPort, replayStorePort,
         replayClockPort, replayRngPort } from './adapters';

/**
 * Ports configuration for execution
 */
export interface PortsConfig {
  oracle: import('../ports/oracle').OraclePort;
  tool: import('../ports/tool').ToolPort;
  store: import('../ports/store').StorePort;
  clock: import('../ports/clock').ClockPort;
  rng: import('../ports/rng').RngPort;
}

/**
 * Replay-aware execution options
 */
export interface ReplayExecOptions {
  mode: 'record' | 'replay' | 'normal';
  log?: ReplayLog;           // Required for replay mode
  semanticSalt?: string;     // Config hash for record mode
}

/**
 * Execution result with optional replay log
 */
export interface ReplayExecResult<A> {
  outcome: Outcome<A>;
  log?: ReplayLog;           // Populated in record mode
  divergence?: {             // Populated if replay diverges
    eventIndex: number;
    expected: unknown;
    actual: unknown;
  };
}

/**
 * Execute a flow with replay support
 */
export async function executeWithReplay<A>(
  flow: FlowIR | IRBundle,
  ports: PortsConfig,
  ctx: ExecContext,
  options: ReplayExecOptions,
  kernel: { interpret: (flow: FlowIR | IRBundle, ctx: ExecContext, ports: PortsConfig) => Promise<Outcome<A>> }
): Promise<ReplayExecResult<A>> {
  switch (options.mode) {
    case 'record':
      return executeWithRecording(flow, ports, ctx, options, kernel);

    case 'replay':
      if (!options.log) {
        throw new Error('Replay mode requires a replay log');
      }
      return executeWithReplayLog(flow, options.log, ctx, kernel);

    case 'normal':
    default:
      const outcome = await kernel.interpret(flow, ctx, ports);
      return { outcome };
  }
}

async function executeWithRecording<A>(
  flow: FlowIR | IRBundle,
  ports: PortsConfig,
  ctx: ExecContext,
  options: ReplayExecOptions,
  kernel: { interpret: (flow: FlowIR | IRBundle, ctx: ExecContext, ports: PortsConfig) => Promise<Outcome<A>> }
): Promise<ReplayExecResult<A>> {
  const flowHash = computeFlowHash(flow);
  const semanticSalt = options.semanticSalt || 'default';
  const runId = generateRunId();

  const writer = createReplayLogWriter(runId, flowHash, semanticSalt, {
    rngSeed: Date.now()
  });

  // Wrap all ports with logging
  const loggingPorts: PortsConfig = {
    oracle: loggingOraclePort(ports.oracle, writer),
    tool: loggingToolPort(ports.tool, writer),
    store: loggingStorePort(ports.store, writer),
    clock: loggingClockPort(ports.clock, writer),
    rng: loggingRngPort(ports.rng, writer)
  };

  const outcome = await kernel.interpret(flow, ctx, loggingPorts);
  const log = writer.finalize();

  return { outcome, log };
}

async function executeWithReplayLog<A>(
  flow: FlowIR | IRBundle,
  log: ReplayLog,
  ctx: ExecContext,
  kernel: { interpret: (flow: FlowIR | IRBundle, ctx: ExecContext, ports: PortsConfig) => Promise<Outcome<A>> }
): Promise<ReplayExecResult<A>> {
  // Verify flow hash matches
  const flowHash = computeFlowHash(flow);
  if (flowHash !== log.flowHash) {
    return {
      outcome: {
        tag: 'Fail',
        failure: {
          reason: 'replay-flow-mismatch',
          message: `Flow hash mismatch: expected ${log.flowHash}, got ${flowHash}`,
          context: { expectedHash: log.flowHash, actualHash: flowHash }
        }
      } as any,
      divergence: {
        eventIndex: -1,
        expected: log.flowHash,
        actual: flowHash
      }
    };
  }

  const reader = createReplayLogReader(log);

  // Create replay ports
  const replayPorts: PortsConfig = {
    oracle: replayOraclePort(reader),
    tool: replayToolPort(reader),
    store: replayStorePort(reader),
    clock: replayClockPort(reader),
    rng: replayRngPort(reader)
  };

  try {
    const outcome = await kernel.interpret(flow, ctx, replayPorts);

    // Verify replay consumed all events
    if (!reader.isComplete()) {
      const progress = reader.progress();
      return {
        outcome,
        divergence: {
          eventIndex: progress.current,
          expected: `${progress.total} events`,
          actual: `${progress.current} events consumed`
        }
      };
    }

    return { outcome };
  } catch (error) {
    if (error instanceof Error && error.name === 'ReplayMismatchError') {
      const mismatch = error as any;
      return {
        outcome: {
          tag: 'Fail',
          failure: {
            reason: 'replay-divergence',
            message: error.message,
            context: { expected: mismatch.expected, actual: mismatch.actual }
          }
        } as any,
        divergence: {
          eventIndex: reader.progress().current,
          expected: mismatch.expected,
          actual: mismatch.actual
        }
      };
    }
    throw error;
  }
}

function computeFlowHash(flow: FlowIR | IRBundle): string {
  // Use canonical encoding + hash from frameir package
  const canonical = JSON.stringify(flow, Object.keys(flow as object).sort());
  return `sha256:${simpleHash(canonical)}`;
}

function simpleHash(str: string): string {
  let hash = 0;
  for (let i = 0; i < str.length; i++) {
    const char = str.charCodeAt(i);
    hash = ((hash << 5) - hash) + char;
    hash = hash & hash;
  }
  return Math.abs(hash).toString(16).padStart(8, '0');
}

function generateRunId(): string {
  return `run:${Date.now().toString(36)}-${Math.random().toString(36).substr(2, 9)}`;
}
```

### Determinism Guards

```typescript
// src/core/replay/guards.ts

/**
 * Determinism hazard detection
 *
 * Reference: ARCHITECTURE-LANGUAGES-6.md §72.4
 * Detect and prevent sources of non-determinism that break replay
 */

export interface DeterminismViolation {
  kind: 'ambient-time' | 'ambient-rng' | 'iteration-order' | 'float-repr';
  location: string;
  description: string;
}

/**
 * Guard against ambient Date.now() calls
 */
export function guardAmbientTime(): void {
  const originalNow = Date.now;
  const originalDate = Date;

  // In strict mode, throw if Date.now is called outside ClockPort
  (Date as any).now = function guardedNow() {
    if (!isInsideClockPort()) {
      console.warn('Ambient Date.now() call detected - this breaks replay determinism');
    }
    return originalNow.call(Date);
  };
}

/**
 * Guard against Math.random() calls
 */
export function guardAmbientRng(): void {
  const originalRandom = Math.random;

  Math.random = function guardedRandom() {
    if (!isInsideRngPort()) {
      console.warn('Ambient Math.random() call detected - this breaks replay determinism');
    }
    return originalRandom();
  };
}

/**
 * Canonical object iteration (sorted keys)
 */
export function canonicalIterate<T>(obj: Record<string, T>): [string, T][] {
  return Object.keys(obj).sort().map(k => [k, obj[k]]);
}

/**
 * Canonical JSON stringify that handles floats
 */
export function canonicalStringify(value: unknown): string {
  return JSON.stringify(value, (key, val) => {
    // Normalize floats to string representation
    if (typeof val === 'number' && !Number.isInteger(val)) {
      return val.toFixed(15).replace(/\.?0+$/, '');
    }
    // Sort object keys
    if (val && typeof val === 'object' && !Array.isArray(val)) {
      const sorted: Record<string, unknown> = {};
      for (const k of Object.keys(val).sort()) {
        sorted[k] = (val as Record<string, unknown>)[k];
      }
      return sorted;
    }
    return val;
  });
}

// Thread-local context tracking
let insideClockPort = false;
let insideRngPort = false;

export function enterClockPort(): void { insideClockPort = true; }
export function exitClockPort(): void { insideClockPort = false; }
export function isInsideClockPort(): boolean { return insideClockPort; }

export function enterRngPort(): void { insideRngPort = true; }
export function exitRngPort(): void { insideRngPort = false; }
export function isInsideRngPort(): boolean { return insideRngPort; }
```

## Output Files

```
src/core/replay/
  events.ts         # Replay event types
  log.ts            # Log writer/reader
  adapters.ts       # Logging and replay port adapters
  runner.ts         # Replay-aware execution
  guards.ts         # Determinism guards
  index.ts          # Public exports

test/replay/
  events.spec.ts
  log.spec.ts
  adapters.spec.ts
  runner.spec.ts
  determinism.spec.ts
  golden/           # Golden replay logs for regression
```

## Tasks

### Phase 1: Event Types and Log Format
- [ ] Create `src/core/replay/events.ts` with all event types
- [ ] Create `src/core/replay/log.ts` with writer/reader
- [ ] Add content-addressable blob storage

### Phase 2: Port Adapters
- [ ] Implement logging adapters for all ports
- [ ] Implement replay adapters for all ports
- [ ] Add request matching logic for weak matching

### Phase 3: Replay Runner
- [ ] Implement `executeWithReplay` function
- [ ] Add flow hash validation
- [ ] Add divergence detection

### Phase 4: Determinism Guards
- [ ] Implement ambient time guard
- [ ] Implement ambient RNG guard
- [ ] Implement canonical serialization

### Phase 5: Testing
- [ ] Create golden replay logs
- [ ] Test replay produces identical outcomes
- [ ] Test divergence detection
- [ ] Property test: record then replay = same result

## Verification Steps

```bash
# Run replay tests
npm run test -- test/replay/

# Test determinism
npm run test -- --grep "determinism"

# Run golden replay tests
npm run test -- test/replay/golden/

# Verify round-trip
npm run test -- --grep "replay round-trip"
```

## Checklist

- [ ] All replay event types defined
- [ ] Log writer records all port interactions
- [ ] Log reader provides sequential access
- [ ] Blob storage is content-addressed
- [ ] Logging adapters wrap all ports
- [ ] Replay adapters return recorded values
- [ ] Request matching validates critical fields
- [ ] Flow hash validation on replay
- [ ] Divergence detection with clear errors
- [ ] Determinism guards prevent ambient calls
- [ ] Canonical serialization handles floats
- [ ] Golden tests pass for recorded logs
- [ ] Round-trip: record → replay = identical outcome

## Determinism Hazards Checklist

Per ARCHITECTURE-LANGUAGES-6.md §72.4:

- [ ] **Non-deterministic iteration order** → use `canonicalIterate`
- [ ] **Floating point differences** → store floats as strings, use decimal library
- [ ] **Clock and RNG ambient usage** → forbid with guards, require ports
- [ ] **Concurrency nondeterminism** → log scheduler decisions

## References

- ARCHITECTURE-LANGUAGES-4.md §48 (Deterministic Replay and Time-Travel Debugging)
- ARCHITECTURE-LANGUAGES-6.md §72 (Replay Log Format)
- Job 011 (Port Abstractions)
- Job 012 (Outcome/Failure/Diagnostic ADTs)

---

## Test Plan

### Happy Path Tests

| ID | Test Case | Input | Expected Output |
|----|-----------|-------|-----------------|
| HP-1 | Record oracle call | OraclePort.infer() | E_OracleCall event in log |
| HP-2 | Record tool call | ToolPort.call() | E_ToolCall event in log |
| HP-3 | Record store ops | StorePort.get/put() | E_StoreGet/Put events |
| HP-4 | Record clock access | ClockPort.nowMs() | E_ClockNow event with value |
| HP-5 | Record RNG access | RngPort.nextU32() | E_RngNextU32 event with value |
| HP-6 | Replay oracle call | Recorded log | Returns recorded response |
| HP-7 | Replay tool call | Recorded log | Returns recorded result |
| HP-8 | Replay clock | Recorded log | Returns recorded time |
| HP-9 | Replay RNG | Recorded log | Returns recorded values |
| HP-10 | Full round-trip | Record then replay | Identical outcome |

### Edge Case Tests

| ID | Test Case | Input | Expected Output |
|----|-----------|-------|-----------------|
| EC-1 | Empty log | No events recorded | Empty replay succeeds |
| EC-2 | Multiple oracle calls | 10 sequential infers | All replayed in order |
| EC-3 | Interleaved port types | Oracle → Tool → Store → Oracle | Correct replay order |
| EC-4 | Large blob | 1MB response | Content-addressed correctly |
| EC-5 | Duplicate blobs | Same response twice | Single blob, two refs |
| EC-6 | Float precision | 0.1 + 0.2 | Canonical representation |
| EC-7 | Nested objects | Deep object keys | Canonical iteration order |
| EC-8 | Scheduler decisions | 5 concurrent fibers | Decisions replayed |

### Error Cases

| ID | Test Case | Input | Expected Error |
|----|-----------|-------|----------------|
| ERR-1 | Replay log exhausted | Fewer events than needed | ReplayMismatchError: end of log |
| ERR-2 | Request mismatch | Different prompt | ReplayMismatchError: request mismatch |
| ERR-3 | Tool mismatch | Different tool name | ReplayMismatchError: tool mismatch |
| ERR-4 | Flow hash mismatch | Different flow | Fail: replay-flow-mismatch |
| ERR-5 | Semantic salt mismatch | Different config | Warning or fail |
| ERR-6 | Ambient time access | Date.now() outside port | Warning logged |
| ERR-7 | Ambient RNG access | Math.random() outside port | Warning logged |

### Integration Tests

| ID | Test Case | Description |
|----|-----------|-------------|
| INT-1 | Golden replay | Replay known-good log, verify identical |
| INT-2 | Divergence detection | Intentionally diverge, verify detection |
| INT-3 | Progress tracking | reader.progress() accurate during replay |
| INT-4 | Partial replay | Replay to event N, then continue live |
| INT-5 | Cost-free re-execution | Replay skips sleep, no API calls |

---

## Notes

### Why Deterministic Replay (§48)

> "This is the dividing line between a clever runtime and an engineering-grade platform."

Deterministic replay enables:
1. **Debugging**: Step through execution without API costs
2. **Auditing**: Reproduce exactly what happened
3. **Regression testing**: Verify behavior doesn't change
4. **Cost-free re-execution**: Replay without calling LLMs

### What Must Be Recorded (§48)

1. **Scheduler decisions** - `extractDecisions(ledger)`
2. **All PortEffect interactions** - oracle, tool, store, sink, source
3. **Randomness** - RNG seeds and draws via RngPort
4. **Time** - Clock reads via ClockPort

### Content-Addressed Blobs (§72)

> "Store payload blobs separately keyed by hash. This avoids log bloat."

- Large payloads (responses, values) stored as blobs
- Events reference blobs by hash (claim checks)
- Identical blobs deduplicated automatically

### Determinism Hazards (§72.4)

| Hazard | Mitigation |
|--------|------------|
| Non-deterministic iteration order | Use `canonicalIterate` (sorted keys) |
| Floating point differences | Store as strings, use decimal library |
| Ambient Date.now() | Require ClockPort, guard warns on ambient |
| Ambient Math.random() | Require RngPort, guard warns on ambient |
| Concurrency nondeterminism | Log scheduler decisions |

---

## Proof of Completion

When this job is complete:

1. **File Structure Verified**
   ```bash
   ls src/core/replay/*.ts  # 6 files
   ls test/replay/*.ts      # 5+ test files
   ls test/replay/golden/   # Golden fixtures
   ```

2. **All Event Types Defined**
   ```bash
   grep "interface.*Event" src/core/replay/events.ts | wc -l  # Should be ≥ 10
   ```

3. **Round-Trip Works**
   ```typescript
   // Record
   const { outcome: o1, log } = await executeWithReplay(flow, ports, ctx, { mode: 'record' });

   // Replay
   const { outcome: o2 } = await executeWithReplay(flow, replayPorts, ctx, { mode: 'replay', log });

   // Identical
   assert.deepEqual(o1, o2);
   ```

4. **Divergence Detected**
   ```typescript
   const { divergence } = await executeWithReplay(differentFlow, ports, ctx, { mode: 'replay', log });
   assert(divergence !== undefined);
   ```

5. **Golden Tests Pass**
   ```bash
   npx vitest run test/replay/golden/
   ```

---

## Footer

| Field | Value |
|-------|-------|
| Created | 2025-01-20 |
| Last Updated | 2025-01-20 |
| Author | Claude |
| Related Docs | [ARCHITECTURE-LANGUAGES-4.md §48](../docs/ARCHITECTURE-LANGUAGES-4.md), [ARCHITECTURE-LANGUAGES-6.md §72](../docs/ARCHITECTURE-LANGUAGES-6.md) |
| Predecessors | [011-PORT-ABSTRACTIONS](./011-PORT-ABSTRACTIONS.md), [012-OUTCOME-FAILURE-DIAGNOSTIC](./012-OUTCOME-FAILURE-DIAGNOSTIC.md), [014-COMPILATION-PIPELINE](./014-COMPILATION-PIPELINE.md) |
| Successors | None (capstone job) |
