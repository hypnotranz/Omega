# 08: Protocol (Client-Server Interface)

## The Key Insight

```
┌─────────────────────────────────────────────────────────────────┐
│                                                                 │
│  The runtime is a SERVER. All clients use the SAME protocol.   │
│                                                                 │
│  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐           │
│  │ CLI     │  │ VS Code │  │ Browser │  │ Matrix  │           │
│  └────┬────┘  └────┬────┘  └────┬────┘  └────┬────┘           │
│       │            │            │            │                  │
│       └────────────┴─────┬──────┴────────────┘                  │
│                          │                                      │
│                    ┌─────▼─────┐                                │
│                    │ PROTOCOL  │                                │
│                    │ (nREPL)   │                                │
│                    └─────┬─────┘                                │
│                          │                                      │
│                    ┌─────▼─────┐                                │
│                    │ RUNTIME   │                                │
│                    └───────────┘                                │
│                                                                 │
│  CLI is NOT special. It's just another protocol client.        │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

This is how Clojure (nREPL), Emacs (SLIME), and Racket (DrRacket) work.

## Protocol Design

Based on nREPL (Network REPL) from Clojure:

```typescript
// protocol.ts

interface Request {
  id: string;           // Unique request ID
  op: string;           // Operation name
  session?: string;     // Session ID (for stateful operations)
  [key: string]: any;   // Operation-specific params
}

interface Response {
  id: string;           // Matches request ID
  status: string[];     // ["done"] or ["error"] or ["need-input"]
  [key: string]: any;   // Operation-specific results
}
```

## Core Operations

### eval - Evaluate Code

```typescript
// Request
{
  id: "1",
  op: "eval",
  code: "(+ 1 2)",
  ns: "user"           // Optional namespace
}

// Response (success)
{
  id: "1",
  status: ["done"],
  value: "3",
  ns: "user"
}

// Response (error)
{
  id: "1",
  status: ["error"],
  error: "Unbound variable: x",
  stacktrace: [...]
}
```

### info - Get Documentation

```typescript
// Request
{
  id: "2",
  op: "info",
  symbol: "map",
  ns: "user"
}

// Response
{
  id: "2",
  status: ["done"],
  doc: "Apply function to each element...",
  arglists: ["(map f coll)"],
  file: "stdlib/list.lisp",
  line: 42
}
```

### complete - Autocomplete

```typescript
// Request
{
  id: "3",
  op: "complete",
  prefix: "wor",
  ns: "user"
}

// Response
{
  id: "3",
  status: ["done"],
  completions: [
    { candidate: "world.read", type: "function" },
    { candidate: "world.write", type: "function" },
    { candidate: "world.list", type: "function" }
  ]
}
```

### interrupt - Stop Execution

```typescript
// Request
{
  id: "4",
  op: "interrupt",
  session: "sess-123"
}

// Response
{
  id: "4",
  status: ["done"]
}
```

## Time-Travel Operations

### snapshot - Capture State

```typescript
// Request
{
  id: "5",
  op: "snapshot",
  session: "sess-123"
}

// Response
{
  id: "5",
  status: ["done"],
  snapshotId: "snap-abc",
  context: {
    step: 42,
    status: "running"
  }
}
```

### restore - Restore State

```typescript
// Request
{
  id: "6",
  op: "restore",
  snapshotId: "snap-abc"
}

// Response
{
  id: "6",
  status: ["done"],
  session: "sess-456"  // New session from restored state
}
```

### navigate - Query History

```typescript
// Request
{
  id: "7",
  op: "navigate",
  action: "list-traces",
  limit: 10
}

// Response
{
  id: "7",
  status: ["done"],
  traces: [
    { id: "tr-1", steps: 42, status: "completed" },
    { id: "tr-2", steps: 15, status: "running" }
  ]
}
```

### fork - Branch Execution

```typescript
// Request
{
  id: "8",
  op: "fork",
  snapshotId: "snap-abc",
  label: "experiment-1"
}

// Response
{
  id: "8",
  status: ["done"],
  branchId: "branch-xyz",
  session: "sess-789"
}
```

## Middleware Architecture

Like nREPL, the protocol uses middleware for extensibility:

```typescript
// middleware.ts

type Handler = (request: Request, next: Handler) => Response | Promise<Response>;

interface Middleware {
  name: string;
  handles: string[];  // Operations this middleware handles
  handler: Handler;
}

class ProtocolServer {
  private middleware: Middleware[] = [];

  use(mw: Middleware): void {
    this.middleware.push(mw);
  }

  async handle(request: Request): Promise<Response> {
    // Build middleware chain
    let handler: Handler = () => ({
      id: request.id,
      status: ["unknown-op"],
      error: `Unknown operation: ${request.op}`
    });

    for (const mw of [...this.middleware].reverse()) {
      if (mw.handles.includes(request.op) || mw.handles.includes('*')) {
        const next = handler;
        handler = (req) => mw.handler(req, next);
      }
    }

    return handler(request, () => { throw new Error('No handler'); });
  }
}
```

### Example Middleware

```typescript
// Eval middleware
const evalMiddleware: Middleware = {
  name: 'eval',
  handles: ['eval'],
  handler: async (req, next) => {
    const { code, ns } = req;
    const session = getOrCreateSession(req.session);

    try {
      const result = await session.eval(code, ns);
      return {
        id: req.id,
        status: ['done'],
        value: print(result),
        ns: session.currentNamespace
      };
    } catch (e) {
      return {
        id: req.id,
        status: ['error'],
        error: e.message,
        stacktrace: formatStacktrace(e)
      };
    }
  }
};

// Logging middleware (wraps all operations)
const loggingMiddleware: Middleware = {
  name: 'logging',
  handles: ['*'],
  handler: async (req, next) => {
    console.log(`[${req.id}] ${req.op}`);
    const start = Date.now();
    const response = await next(req);
    console.log(`[${req.id}] done in ${Date.now() - start}ms`);
    return response;
  }
};

// Time-travel middleware
const timeTravelMiddleware: Middleware = {
  name: 'time-travel',
  handles: ['snapshot', 'restore', 'navigate', 'fork'],
  handler: async (req, next) => {
    switch (req.op) {
      case 'snapshot':
        return handleSnapshot(req);
      case 'restore':
        return handleRestore(req);
      case 'navigate':
        return handleNavigate(req);
      case 'fork':
        return handleFork(req);
      default:
        return next(req);
    }
  }
};
```

## Transport Layers

The protocol is transport-agnostic:

### stdio (CLI)

```typescript
// For CLI usage
const server = new ProtocolServer();
const readline = require('readline');

const rl = readline.createInterface({ input: process.stdin });

rl.on('line', async (line: string) => {
  const request = JSON.parse(line);
  const response = await server.handle(request);
  console.log(JSON.stringify(response));
});
```

### WebSocket (Browser)

```typescript
// For browser usage
const ws = new WebSocket('ws://localhost:8080');

ws.onmessage = async (event) => {
  const request = JSON.parse(event.data);
  const response = await server.handle(request);
  ws.send(JSON.stringify(response));
};
```

### HTTP (REST API)

```typescript
// For web API
import { serve } from 'bun';

serve({
  port: 8080,
  async fetch(req) {
    const request = await req.json();
    const response = await server.handle(request);
    return new Response(JSON.stringify(response), {
      headers: { 'Content-Type': 'application/json' }
    });
  }
});
```

### VS Code Extension

```typescript
// vscode/extension.ts
import * as vscode from 'vscode';

class LambdaLLMClient {
  private server: ProtocolServer;

  async eval(code: string): Promise<string> {
    const response = await this.server.handle({
      id: generateId(),
      op: 'eval',
      code
    });
    return response.value;
  }

  async complete(prefix: string): Promise<vscode.CompletionItem[]> {
    const response = await this.server.handle({
      id: generateId(),
      op: 'complete',
      prefix
    });
    return response.completions.map(c => new vscode.CompletionItem(c.candidate));
  }
}
```

## Session Management

Sessions track state across requests:

```typescript
interface Session {
  id: string;
  environment: Environment;
  world: World;
  continuation?: Continuation;  // For paused execution
  namespace: string;
  createdAt: number;
}

const sessions = new Map<string, Session>();

function getOrCreateSession(id?: string): Session {
  if (id && sessions.has(id)) {
    return sessions.get(id)!;
  }

  const session: Session = {
    id: generateId(),
    environment: createBaseEnvironment(),
    world: createWorld(),
    namespace: 'user',
    createdAt: Date.now()
  };

  sessions.set(session.id, session);
  return session;
}
```

## IDE Integration

### Language Server Protocol (LSP)

LSP is built on top of our protocol:

```typescript
// Map LSP requests to our protocol
class LSPAdapter {
  constructor(private protocol: ProtocolServer) {}

  // textDocument/completion → complete
  async onCompletion(params: CompletionParams): Promise<CompletionList> {
    const response = await this.protocol.handle({
      id: generateId(),
      op: 'complete',
      prefix: getWordAtPosition(params),
      ns: getNamespaceFromUri(params.textDocument.uri)
    });

    return {
      isIncomplete: false,
      items: response.completions.map(c => ({
        label: c.candidate,
        kind: c.type === 'function' ? CompletionItemKind.Function : CompletionItemKind.Variable
      }))
    };
  }

  // textDocument/hover → info
  async onHover(params: HoverParams): Promise<Hover> {
    const response = await this.protocol.handle({
      id: generateId(),
      op: 'info',
      symbol: getSymbolAtPosition(params)
    });

    return {
      contents: {
        kind: 'markdown',
        value: `**${response.symbol}**\n\n${response.doc}\n\n\`\`\`lisp\n${response.arglists.join('\n')}\n\`\`\``
      }
    };
  }
}
```

### Debug Adapter Protocol (DAP)

DAP maps to time-travel operations:

```typescript
// Map DAP requests to our protocol
class DAPAdapter {
  // setBreakpoint → internal tracking
  // continue → eval until breakpoint
  // stepOver → eval one step
  // stackTrace → navigate + introspect

  async onStackTrace(): Promise<StackTraceResponse> {
    const response = await this.protocol.handle({
      id: generateId(),
      op: 'navigate',
      action: 'call-stack'
    });

    return {
      stackFrames: response.frames.map((f, i) => ({
        id: i,
        name: f.name,
        source: { path: f.sourceLoc?.file },
        line: f.sourceLoc?.line ?? 0,
        column: f.sourceLoc?.column ?? 0
      }))
    };
  }
}
```

## Summary

The protocol gives us:

1. **One interface**: CLI, VS Code, browser all use same protocol
2. **Extensibility**: Middleware pattern for new operations
3. **Time-travel**: snapshot/restore/fork are just operations
4. **IDE integration**: Maps naturally to LSP/DAP
5. **Transport agnostic**: stdio, WebSocket, HTTP all work

This is how real language tooling works: the runtime is a server, and all tools are clients.
