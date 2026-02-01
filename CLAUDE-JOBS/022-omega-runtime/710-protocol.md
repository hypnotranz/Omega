# 710: Protocol Server

## Status: COMPLETE ✅
- **Completed**: 2026-01-29
- **Location**: OmegaLLM-3/src/runtime/subsystems/ProtocolServer.ts (511 lines)
- **Operations**: protocol/operations/ (8 files: eval, info, complete, interrupt, snapshot, restore, debug, session)
- **Transports**: protocol/transports/ (4 files: index, StdioTransport, TCPTransport, WebSocketTransport)

## Purpose
Implements nREPL-style protocol for IDE integration, enabling evaluation, debugging, and introspection over a message-based interface.

## Dependencies
- 100-types.md ✅
- 210-execution.md ✅

## Source References
- ARCHITECTURE/08-PROTOCOL.md
- nREPL specification
- VS Code Debug Adapter Protocol (DAP)
- Language Server Protocol (LSP)

---

## Deliverables

```
src/runtime/protocol/
├── ProtocolServer.ts        # Main protocol server
├── MessageRouter.ts         # Message dispatch
├── SessionRegistry.ts       # Session management
├── handlers/
│   ├── EvalHandler.ts       # Evaluation requests
│   ├── DebugHandler.ts      # Debug operations
│   ├── CompletionHandler.ts # Code completion
│   └── IntrospectionHandler.ts # State inspection
└── transport/
    ├── StdioTransport.ts    # Stdin/stdout
    ├── TCPTransport.ts      # TCP socket
    └── WebSocketTransport.ts # WebSocket
```

---

## Key Types

```typescript
export interface ProtocolMessage {
  id: string;                // Request ID for correlation
  op: string;                // Operation name
  session?: string;          // Session ID
  [key: string]: unknown;    // Operation-specific data
}

export interface ProtocolResponse {
  id: string;                // Correlates to request
  status: ('done' | 'error' | 'need-input' | 'interrupted')[];
  [key: string]: unknown;    // Response data
}

export interface ProtocolSession {
  id: string;
  runtime: OmegaRuntime;
  createdAt: number;
  lastActiveAt: number;
}

export type TransportType = 'stdio' | 'tcp' | 'websocket';

export interface TransportConfig {
  type: TransportType;
  host?: string;
  port?: number;
}
```

---

## Key Interface

```typescript
export interface ProtocolServer {
  // ─── Lifecycle ───

  /**
   * Start the protocol server.
   */
  start(config: TransportConfig): Promise<void>;

  /**
   * Stop the server.
   */
  stop(): Promise<void>;

  /**
   * Check if running.
   */
  isRunning(): boolean;

  // ─── Session Management ───

  /**
   * Create a new session.
   */
  createSession(): ProtocolSession;

  /**
   * Get session by ID.
   */
  getSession(id: string): ProtocolSession | undefined;

  /**
   * Close a session.
   */
  closeSession(id: string): void;

  // ─── Message Handling ───

  /**
   * Handle incoming message.
   */
  handleMessage(message: ProtocolMessage): Promise<ProtocolResponse[]>;

  /**
   * Register custom operation handler.
   */
  registerHandler(op: string, handler: OperationHandler): void;

  // ─── Events ───

  /**
   * Emit message to client.
   */
  send(sessionId: string, message: ProtocolResponse): void;

  /**
   * Broadcast to all sessions.
   */
  broadcast(message: ProtocolResponse): void;
}

export type OperationHandler = (
  message: ProtocolMessage,
  session: ProtocolSession
) => Promise<ProtocolResponse[]>;
```

---

## Protocol Operations

### Core Operations

| Operation | Description | Request Fields | Response Fields |
|-----------|-------------|----------------|-----------------|
| `clone` | Create new session | - | `new-session` |
| `close` | Close session | - | - |
| `describe` | Server capabilities | - | `ops`, `versions` |
| `eval` | Evaluate code | `code`, `ns` | `value`, `err` |
| `interrupt` | Cancel evaluation | - | - |
| `load-file` | Load and eval file | `file`, `file-path` | `value` |

### Debug Operations

| Operation | Description | Request Fields | Response Fields |
|-----------|-------------|----------------|-----------------|
| `debug-start` | Start debug session | `break-on-entry` | - |
| `debug-stop` | Stop debug session | - | - |
| `set-breakpoints` | Set breakpoints | `source`, `breakpoints` | `breakpoints` |
| `continue` | Continue execution | - | - |
| `step-over` | Step over | - | - |
| `step-into` | Step into | - | - |
| `step-out` | Step out | - | - |
| `stack-trace` | Get stack trace | - | `stackFrames` |
| `scopes` | Get scopes | `frameId` | `scopes` |
| `variables` | Get variables | `variablesReference` | `variables` |

### Completion Operations

| Operation | Description | Request Fields | Response Fields |
|-----------|-------------|----------------|-----------------|
| `completions` | Get completions | `prefix`, `context` | `completions` |
| `info` | Get symbol info | `symbol`, `ns` | `info` |
| `doc` | Get documentation | `symbol` | `doc` |

---

## Message Flow

```
Client                          Server
   │                              │
   │  ─────────────────────────►  │
   │  { id: "1", op: "eval",      │
   │    code: "(+ 1 2)",          │
   │    session: "abc" }          │
   │                              │
   │  ◄─────────────────────────  │
   │  { id: "1",                  │
   │    value: "3",               │
   │    status: ["done"] }        │
   │                              │

   │  ─────────────────────────►  │
   │  { id: "2", op: "eval",      │
   │    code: "(slow-fn)" }       │
   │                              │
   │  ◄─────────────────────────  │
   │  { id: "2",                  │
   │    out: "Processing..." }    │  (streaming output)
   │                              │
   │  ◄─────────────────────────  │
   │  { id: "2",                  │
   │    value: "result",          │
   │    status: ["done"] }        │
```

---

## Implementation

```typescript
class ProtocolServerImpl implements ProtocolServer {
  private handlers = new Map<string, OperationHandler>();
  private sessions = new Map<string, ProtocolSession>();
  private transport: Transport;

  constructor() {
    // Register built-in handlers
    this.registerHandler('clone', this.handleClone.bind(this));
    this.registerHandler('close', this.handleClose.bind(this));
    this.registerHandler('describe', this.handleDescribe.bind(this));
    this.registerHandler('eval', this.handleEval.bind(this));
    this.registerHandler('interrupt', this.handleInterrupt.bind(this));
  }

  async handleMessage(message: ProtocolMessage): Promise<ProtocolResponse[]> {
    const handler = this.handlers.get(message.op);
    if (!handler) {
      return [{
        id: message.id,
        status: ['error', 'done'],
        err: `Unknown operation: ${message.op}`
      }];
    }

    const session = message.session
      ? this.sessions.get(message.session)
      : this.createSession();

    if (!session) {
      return [{
        id: message.id,
        status: ['error', 'done'],
        err: 'Unknown session'
      }];
    }

    session.lastActiveAt = Date.now();

    try {
      return await handler(message, session);
    } catch (error) {
      return [{
        id: message.id,
        status: ['error', 'done'],
        err: error.message,
        ex: error.stack
      }];
    }
  }

  private async handleEval(
    message: ProtocolMessage,
    session: ProtocolSession
  ): Promise<ProtocolResponse[]> {
    const responses: ProtocolResponse[] = [];

    // Stream output
    const outputHandler = (text: string) => {
      this.send(session.id, {
        id: message.id,
        out: text
      });
    };

    session.runtime.on('output', outputHandler);

    try {
      const result = await session.runtime.eval(message.code as string);

      responses.push({
        id: message.id,
        value: stringify(result.value),
        status: ['done']
      });
    } catch (error) {
      responses.push({
        id: message.id,
        err: error.message,
        ex: error.stack,
        status: ['error', 'done']
      });
    } finally {
      session.runtime.off('output', outputHandler);
    }

    return responses;
  }
}
```

---

## Test Requirements

### Unit Tests: `tests/runtime/protocol/ProtocolServer.test.ts`
- [ ] start() begins listening
- [ ] stop() closes connections
- [ ] createSession() creates new session
- [ ] handleMessage() routes correctly
- [ ] eval operation returns value
- [ ] eval operation streams output
- [ ] interrupt cancels evaluation
- [ ] describe returns capabilities
- [ ] Unknown operation returns error
- [ ] Session timeout cleanup works

### Integration Tests
- [ ] Full eval flow over TCP
- [ ] Debug operations work
- [ ] Completion operations work
- [ ] Multiple concurrent sessions
- [ ] Large output streaming

---

## Acceptance Criteria
1. nREPL-compatible message format
2. Stdio, TCP, and WebSocket transports
3. Session isolation
4. Streaming output support
5. Debug protocol integration
6. Extensible handler registration
