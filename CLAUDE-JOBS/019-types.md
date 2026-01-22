# JOB-019-types: Shared Types and Interfaces

## Context

This job defines all shared TypeScript interfaces and types for the Session Server architecture. These contracts enable parallel development of components and ensure consistent integration.

## Goal

Create a single `server/types.ts` file containing all interface definitions that other components will implement or depend on.

## Dependencies

- None (this is the foundation layer)

## Blockers

- None

## Files to Create

1. `server/types.ts` - All shared types and interfaces

## Implementation

```typescript
// server/types.ts

import type { ReplState } from '../omega-repl';
import type { State } from '../core/eval/machine';

// ─────────────────────────────────────────────────────────────────
// Core Result Types
// ─────────────────────────────────────────────────────────────────

export interface ExecuteResult {
  output: string;
  shouldExit: boolean;
}

// ─────────────────────────────────────────────────────────────────
// Session Metadata
// ─────────────────────────────────────────────────────────────────

export interface SessionMetadata {
  id: string;
  name: string;
  created: Date;
  lastAccessed: Date;
  eventCount: number;
  checkpointCount: number;
  size: number;
}

// ─────────────────────────────────────────────────────────────────
// Configuration
// ─────────────────────────────────────────────────────────────────

export interface ServerConfig {
  port: number;
  sessionDir: string;
  maxCachedSessions: number;
  saveIntervalMs: number;
  logLevel: 'debug' | 'info' | 'warn' | 'error';
}

// ─────────────────────────────────────────────────────────────────
// Session Instance Interface
// ─────────────────────────────────────────────────────────────────

export interface ISessionInstance {
  execute(command: string): Promise<ExecuteResult>;
  getState(): ReplState;
  getMetadata(): SessionMetadata;
  isDirty(): boolean;
  markClean(): void;
  getLastAccessTime(): number;
  getChangesSinceLastSave(): SessionEvent[];
  updateSavePoint(): void;
}

// ─────────────────────────────────────────────────────────────────
// Session Pool Interface
// ─────────────────────────────────────────────────────────────────

export interface ISessionPool {
  get(name: string): ISessionInstance | undefined;
  add(name: string, instance: ISessionInstance): void;
  remove(name: string): boolean;
  getAll(): ISessionInstance[];
  getAllMetadata(): SessionMetadata[];
  getDirty(): ISessionInstance[];
  clear(): void;
  size(): number;
}

// ─────────────────────────────────────────────────────────────────
// Persistence Interfaces
// ─────────────────────────────────────────────────────────────────

export interface IIncrementalWriter {
  appendEvents(events: SessionEvent[], state: ReplState): Promise<void>;
}

export interface ISessionPersister {
  save(session: ISessionInstance): Promise<void>;
  load(): Promise<ISessionInstance | null>;
  saveSync(session: ISessionInstance): void;
}

export interface IPersistenceManager {
  saveSession(session: ISessionInstance): Promise<void>;
  loadSession(name: string): Promise<ISessionInstance | null>;
  saveDirtySessions(): Promise<void>;
  forceSave(): Promise<void>;
  shutdown(): void;
}

// ─────────────────────────────────────────────────────────────────
// Session Manager Interface
// ─────────────────────────────────────────────────────────────────

export interface ISessionManager {
  getSession(name: string): Promise<ISessionInstance>;
  executeCommand(sessionName: string, command: string): Promise<ExecuteResult>;
  listSessions(): SessionMetadata[];
  saveDirtySessions(): Promise<void>;
  shutdown(): Promise<void>;
}

// ─────────────────────────────────────────────────────────────────
// Process Manager Interface
// ─────────────────────────────────────────────────────────────────

export interface IProcessManager {
  startServer(options?: Record<string, unknown>): Promise<number | undefined>;
  stopServer(): Promise<void>;
  isServerRunning(): Promise<boolean>;
  getServerPid(): Promise<number | null>;
}

// ─────────────────────────────────────────────────────────────────
// Client Interface
// ─────────────────────────────────────────────────────────────────

export interface ISessionClient {
  init(): Promise<void>;
  executeCommand(sessionName: string, command: string): Promise<ExecuteResult>;
  listSessions(): Promise<SessionMetadata[]>;
  getSession(name: string): Promise<SessionMetadata | null>;
  forceSave(sessionName: string): Promise<void>;
}

// ─────────────────────────────────────────────────────────────────
// Session Events (for persistence)
// ─────────────────────────────────────────────────────────────────

export type SessionEvent =
  | { type: 'session'; version: number; id: string; created: string }
  | { type: 'input'; seq: number; code: string; timestamp: string }
  | { type: 'output'; seq: number; value: string; timestamp: string }
  | { type: 'checkpoint'; seq: number; reason: string; stateId: string }
  | { type: 'llm_req'; seq: number; prompt: string; receiptKey: string }
  | { type: 'llm_resp'; seq: number; response: string; receiptKey: string }
  | { type: 'error'; seq: number; message: string; timestamp: string };

// ─────────────────────────────────────────────────────────────────
// API Types
// ─────────────────────────────────────────────────────────────────

export interface ApiExecuteRequest {
  command: string;
}

export interface ApiExecuteResponse {
  output: string;
  shouldExit: boolean;
}

export interface ApiSessionListResponse {
  sessions: SessionMetadata[];
}

export interface ApiErrorResponse {
  error: string;
}

// ─────────────────────────────────────────────────────────────────
// Advanced Features Types
// ─────────────────────────────────────────────────────────────────

export enum AccessMode {
  ReadOnly = 'readonly',
  ReadWrite = 'readwrite'
}

export interface SessionMetrics {
  commandCount: number;
  averageExecutionTime: number;
  peakMemoryUsage: number;
  lastAccessed: Date;
  sessionSize: number;
}

export interface SnapshotMetadata {
  id: string;
  sessionName: string;
  created: Date;
  description: string;
  tags: string[];
}

export interface CheckpointAnnotation {
  checkpointSeq: number;
  label: string;
  description: string;
  tags: string[];
  created: Date;
}
```

## Testing

```typescript
// server/types.spec.ts
import { SessionMetadata, ExecuteResult, ServerConfig } from './types';

describe('Types', () => {
  it('SessionMetadata has required fields', () => {
    const meta: SessionMetadata = {
      id: 'test',
      name: 'test',
      created: new Date(),
      lastAccessed: new Date(),
      eventCount: 0,
      checkpointCount: 0,
      size: 0
    };
    expect(meta.id).toBe('test');
  });

  it('ExecuteResult has required fields', () => {
    const result: ExecuteResult = {
      output: 'hello',
      shouldExit: false
    };
    expect(result.output).toBe('hello');
  });
});
```

## Success Criteria

1. All interfaces compile without errors
2. Interfaces are comprehensive enough for all components
3. No circular dependencies in type definitions
4. Types are exported and importable by other modules

## Estimated Effort

- Implementation: 1-2 hours
- Testing: 30 minutes
