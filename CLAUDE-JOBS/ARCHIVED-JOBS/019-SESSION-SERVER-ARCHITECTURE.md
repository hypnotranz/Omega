# JOB-019: Session Server Architecture

## Context & Motivation

The current OmegaLLM REPL has performance issues with session commands that use the file-based session serialization system. Operations like `:session list`, `:session checkpoints`, and `:session trace` are slow because:

1. They perform synchronous file I/O operations for every command
2. They serialize and deserialize the entire machine state frequently
3. They re-parse session files on every command invocation
4. They lack caching mechanisms for repeated operations

Users experience significant latency when working with sessions, particularly when using these features through the command-line interface.

## Goal

Design and implement a stateful Session Server that keeps session data in memory, handles persistence asynchronously, and provides a fast, responsive interface for REPL operations. The server should:

1. Maintain session state between commands for immediate access
2. Handle persistence in the background without blocking client operations
3. Provide a compatible CLI interface that feels native
4. Support all existing session operations with dramatically improved performance
5. Enable additional features like concurrent access that aren't feasible in the current architecture

## Architecture Overview

The proposed architecture follows a client-server model:

```
┌───────────────────┐     ┌─────────────────────────┐
│                   │     │                         │
│  REPL CLI Client  │◄────┤  Session Server         │
│                   │     │                         │
└───────────────────┘     │  ┌─────────────────┐    │
                          │  │ Session Manager │    │
                          │  └────────┬────────┘    │
                          │           │             │
                          │  ┌────────▼────────┐    │
                          │  │  Session Pool   │    │
                          │  └────────┬────────┘    │
                          │           │             │
                          │  ┌────────▼────────┐    │
                          │  │   Persistence   │    │
                          │  └─────────────────┘    │
                          │                         │
                          └─────────────────────────┘
```

## Layered Component Architecture

The implementation is organized into layers with clear interface boundaries to enable parallel development and independent testing:

```
LAYER 0: Interfaces & Types (no dependencies)
├── 019-types: All shared TypeScript interfaces and types

LAYER 1: Core Data Structures (depend only on types)
├── 019a1-config: ServerConfig, environment handling
├── 019a2-session-instance: SessionMetadata, SessionInstance wrapper
├── 019a3-session-pool: LRU cache for sessions
├── 019a4-incremental-writer: Optimized file writing

LAYER 2: Managers (depend on Layer 1)
├── 019b1-persistence-manager: Background saving coordination
├── 019b2-session-manager: Session lifecycle management
├── 019b3-process-manager: Server process lifecycle

LAYER 3: Server & API (depend on Layer 2)
├── 019c1-api-routes: REST endpoint definitions
├── 019c2-session-server: Express application

LAYER 4: Client (can develop against API contract)
├── 019d1-session-client: HTTP client for server communication
├── 019d2-client-process-manager: Auto-start server from client
├── 019d3-repl-integration: Adapt omega-repl.ts

LAYER 5: Advanced Features (independent of each other)
├── 019e1-concurrency-manager: Multi-client access control
├── 019e2-session-monitor: Metrics and statistics
├── 019e3-snapshot-manager: Session snapshots
├── 019e4-timeline-navigator: Enhanced time travel
```

## Dependency Graph for Parallel Execution

```
Layer 0:  [019-types] ─────────────────────────────────────────────┐
              │                                                     │
Layer 1:  [019a1] [019a2] [019a3] [019a4]  ← All parallel          │
              │      │       │       │                              │
Layer 2:  [019b1]────┴───────┴───────┘                             │
          [019b2]────────────────────────                          │
          [019b3]────────────────────────  ← Can run in parallel   │
              │                                                     │
Layer 3:  [019c1]────────────────────────                          │
          [019c2]────────────────────────  ← Sequential            │
              │                                                     │
Layer 4:  [019d1] [019d2] [019d3]          ← Can run in parallel   │
                                                                    │
Layer 5:  [019e1] [019e2] [019e3] [019e4]  ← All parallel ─────────┘
```

## Interface Contracts

Each component defines clear interfaces that other components depend on:

### ISessionInstance
```typescript
interface ISessionInstance {
  execute(command: string): Promise<ExecuteResult>;
  getState(): ReplState;
  getMetadata(): SessionMetadata;
  isDirty(): boolean;
  markClean(): void;
}
```

### ISessionPool
```typescript
interface ISessionPool {
  get(name: string): ISessionInstance | undefined;
  add(name: string, instance: ISessionInstance): void;
  remove(name: string): boolean;
  getDirty(): ISessionInstance[];
  size(): number;
}
```

### IPersistenceManager
```typescript
interface IPersistenceManager {
  saveSession(session: ISessionInstance): Promise<void>;
  loadSession(name: string): Promise<ISessionInstance | null>;
  forceSave(): Promise<void>;
  shutdown(): void;
}
```

### ISessionManager
```typescript
interface ISessionManager {
  getSession(name: string): Promise<ISessionInstance>;
  executeCommand(name: string, command: string): Promise<ExecuteResult>;
  listSessions(): SessionMetadata[];
  shutdown(): Promise<void>;
}
```

### ISessionClient
```typescript
interface ISessionClient {
  init(): Promise<void>;
  executeCommand(session: string, command: string): Promise<ExecuteResult>;
  listSessions(): Promise<SessionMetadata[]>;
  getSession(name: string): Promise<SessionMetadata | null>;
  forceSave(session: string): Promise<void>;
}
```

## Testing Strategy

Each component is independently testable:

1. **Unit Tests**: Each component tested in isolation with mocked dependencies
2. **Integration Tests**: Layer-by-layer integration testing
3. **Contract Tests**: Verify interface implementations match contracts
4. **End-to-End Tests**: Full client-server workflow testing

## Success Criteria

1. All session commands work with same interface as before
2. Session commands execute in <50ms (vs. seconds currently)
3. No data loss during normal operations
4. Graceful handling of errors and edge cases
5. Backward compatibility with existing session files
6. Each component passes independent unit tests
7. Clear interface contracts enable parallel development
