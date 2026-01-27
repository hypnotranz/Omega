# OmegaLLM Debugger Implementation Plan

## The Insight: A UI is a UI

The REPL and the Web Debugger are both **UIs** that consume the same underlying **service layer**. They should share:
- The same state inspection APIs
- The same stepping/breakpoint logic
- The same session management

The difference is just **transport**:
- REPL: readline (stdin/stdout)
- Web: HTTP + WebSocket

---

## Target Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                              UIs (Consumers)                             │
├───────────────────────┬───────────────────────┬─────────────────────────┤
│     CLI REPL          │     Web Debugger      │    Future: VSCode       │
│   (bin/omega-repl)    │   (public/index.html) │    Extension, etc.      │
│   readline transport  │   HTTP/WS transport   │                         │
└───────────┬───────────┴───────────┬───────────┴───────────┬─────────────┘
            │                       │                       │
            ▼                       ▼                       ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                         SERVICE LAYER (src/service/)                     │
│                                                                          │
│  ┌────────────────────────────────────────────────────────────────────┐ │
│  │                    IDebugService (interface)                        │ │
│  │                                                                      │ │
│  │  Sessions:                                                           │ │
│  │    createSession(config) → sessionId                                 │ │
│  │    closeSession(sessionId)                                           │ │
│  │                                                                      │ │
│  │  Execution:                                                          │ │
│  │    loadCode(sessionId, code) → { success, error? }                   │ │
│  │    step(sessionId) → StepResult                                      │ │
│  │    continue(sessionId) → StepResult                                  │ │
│  │    resumeWithValue(sessionId, value) → StepResult                    │ │
│  │                                                                      │ │
│  │  Inspection:                                                         │ │
│  │    getSnapshot(sessionId) → MachineSnapshot                          │ │
│  │    getBinding(sessionId, name) → SerializedValue                     │ │
│  │    getCallStack(sessionId) → SerializedFrame[]                       │ │
│  │    evaluate(sessionId, expr) → { value } | { error }                 │ │
│  │                                                                      │ │
│  │  Breakpoints:                                                        │ │
│  │    addBreakpoint(sessionId, bp) → breakpointId                       │ │
│  │    removeBreakpoint(sessionId, bpId)                                 │ │
│  │    listBreakpoints(sessionId) → Breakpoint[]                         │ │
│  │                                                                      │ │
│  │  Time Travel:                                                        │ │
│  │    jumpToStep(sessionId, step) → MachineSnapshot                     │ │
│  │    getHistory(sessionId) → HistoryEntry[]                            │ │
│  │                                                                      │ │
│  │  OPR:                                                                │ │
│  │    listKernels() → KernelInfo[]                                      │ │
│  │    executeKernel(kernelId, program) → KernelResult                   │ │
│  └────────────────────────────────────────────────────────────────────┘ │
│                                                                          │
│  Implementation: DebugService (class)                                    │
│    - Manages multiple DebugSession instances                             │
│    - Implements IDebugService                                            │
│    - Transport-agnostic (no HTTP, no readline)                           │
└──────────────────────────────────┬──────────────────────────────────────┘
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                           CORE (src/core/)                               │
│                                                                          │
│  ┌──────────────────┐  ┌──────────────────┐  ┌──────────────────────┐   │
│  │   CESK Machine   │  │   OPR Runtime    │  │   Session Recording  │   │
│  │   (eval/)        │  │   (opr/)         │  │   (session/)         │   │
│  │                  │  │                  │  │                      │   │
│  │  - State         │  │  - Kernels       │  │  - SessionWriter     │   │
│  │  - stepOnce()    │  │  - Runtime       │  │  - SessionReader     │   │
│  │  - Frames        │  │  - Adapters      │  │  - JumpController    │   │
│  │  - Values        │  │  - Validation    │  │                      │   │
│  └──────────────────┘  └──────────────────┘  └──────────────────────┘   │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Package Structure

```
src/
├── core/                    # Engine (exists, no changes)
│   ├── eval/                # CESK machine
│   ├── opr/                 # OPR runtime
│   ├── effects/             # Effect system
│   ├── session/             # Session recording
│   └── ...
│
├── service/                 # NEW: Service layer (transport-agnostic)
│   ├── index.ts             # PUBLIC EXPORTS
│   ├── types.ts             # IDebugService, MachineSnapshot, etc.
│   ├── debugService.ts      # Implementation of IDebugService
│   ├── debugSession.ts      # Single session wrapper
│   └── stateSerializer.ts   # State → JSON conversion
│
├── server/                  # HTTP/WebSocket transport
│   ├── index.ts             # PUBLIC EXPORTS
│   ├── httpServer.ts        # Express routes
│   ├── wsServer.ts          # WebSocket handlers
│   └── debugServer.ts       # Combined server
│
├── repl/                    # CLI transport (to refactor)
│   ├── index.ts             # PUBLIC EXPORTS
│   ├── types.ts             # REPL-specific types
│   ├── replSession.ts       # REPL using IDebugService
│   └── commands/            # Command handlers
│       ├── index.ts
│       ├── step.ts
│       ├── inspect.ts
│       ├── opr.ts
│       └── ...
│
└── client/                  # Client SDK (optional)
    ├── index.ts             # PUBLIC EXPORTS
    └── debugClient.ts       # WebSocket client wrapper

bin/
├── omega-repl.ts            # CLI entry (uses src/repl/)
├── omega-server.ts          # Server entry (uses src/server/)
└── omega-debugger.ts        # Existing debugger (to merge into repl)

public/
├── index.html               # Web debugger UI
├── app.js                   # Frontend code
└── style.css                # Styles
```

---

## Implementation Phases

### Phase 1: Service Layer (CRITICAL PATH)
**Goal**: Single source of truth for debug functionality

1. **Create `src/service/types.ts`**
   - Move `IDebugService` interface here
   - Move `MachineSnapshot`, `StepResult`, etc.
   - Export ALL types needed by consumers

2. **Create `src/service/debugService.ts`**
   - Transport-agnostic implementation
   - Manages Map<sessionId, DebugSession>
   - NO HTTP, NO readline, NO WebSocket

3. **Create `src/service/debugSession.ts`**
   - Wraps CESK machine for a single session
   - Implements stepping, breakpoints, inspection
   - Uses `src/core/eval/stepOnce()`

4. **Create `src/service/stateSerializer.ts`**
   - Convert State → MachineSnapshot
   - Convert Val → SerializedValue
   - Convert Frame → SerializedFrame

5. **Create `src/service/index.ts`**
   - Export ONLY public interface
   - Clear package boundary

### Phase 2: HTTP/WebSocket Server
**Goal**: Web transport layer

1. **Create `src/server/httpServer.ts`**
   - Express routes
   - Consumes `IDebugService`

2. **Create `src/server/wsServer.ts`**
   - WebSocket connection handling
   - Real-time state updates

3. **Create `src/server/debugServer.ts`**
   - Combined HTTP + WS
   - Creates `DebugService` instance

4. **Create `bin/omega-server.ts`**
   - CLI entry point for server

### Phase 3: Refactor REPL
**Goal**: REPL uses same service layer

1. **Create `src/repl/replSession.ts`**
   - Uses `IDebugService` (not CESK directly)
   - readline transport

2. **Migrate commands**
   - `:step`, `:continue`, etc. → call service
   - `:opr-run`, `:opr-list` → call service

3. **Update `bin/omega-repl.ts`**
   - Use new repl package

### Phase 4: Web UI
**Goal**: Minimal functional web debugger

1. **Create `public/index.html`**
   - Code editor panel
   - Call stack panel
   - Environment panel
   - Control buttons (step, continue, etc.)

2. **Create `public/app.js`**
   - WebSocket connection
   - State rendering
   - Command sending

---

## Interface Contract (Phase 1 Deliverable)

```typescript
// src/service/types.ts

// ─── SESSION TYPES ───
export interface SessionConfig {
  name?: string;
  recording?: boolean;
  maxSteps?: number;
}

// ─── MACHINE STATE TYPES ───
export interface MachineSnapshot {
  snapshotId: string;
  step: number;
  timestamp: string;
  control: SerializedControl;
  environment: SerializedBinding[];
  callStack: SerializedFrame[];
  handlers: SerializedHandler[];
  store?: SerializedStoreEntry[];
  status: 'running' | 'paused' | 'done' | 'error' | 'effect';
  pendingEffect?: { op: string; args: SerializedValue[] };
  result?: SerializedValue;
  error?: { message: string; stack?: string };
}

export interface SerializedControl {
  tag: 'Expr' | 'Val';
  type: string;
  summary: string;
  source?: string;
}

export interface SerializedValue {
  tag: string;
  summary: string;
  details?: unknown;
}

export interface SerializedFrame {
  index: number;
  tag: string;
  description: string;
  waiting: string;
}

export interface SerializedBinding {
  name: string;
  value: SerializedValue;
  depth: number;
}

// ─── EXECUTION TYPES ───
export interface StepResult {
  snapshot: MachineSnapshot;
  outcome: 'stepped' | 'breakpoint' | 'effect' | 'done' | 'error';
  breakpointId?: string;
}

export interface Breakpoint {
  id: string;
  type: 'step' | 'expression' | 'effect' | 'binding';
  exprType?: string;
  effectOp?: string;
  bindingName?: string;
  condition?: string;
  enabled: boolean;
}

// ─── THE SERVICE CONTRACT ───
export interface IDebugService {
  // Session
  createSession(config?: SessionConfig): Promise<string>;
  listSessions(): Promise<SessionInfo[]>;
  getSession(id: string): Promise<{ config: SessionConfig; snapshot: MachineSnapshot }>;
  closeSession(id: string): Promise<void>;

  // Execution
  loadCode(sessionId: string, code: string): Promise<{ success: boolean; error?: string }>;
  step(sessionId: string): Promise<StepResult>;
  stepN(sessionId: string, n: number): Promise<StepResult>;
  continue(sessionId: string): Promise<StepResult>;
  run(sessionId: string, maxSteps?: number): Promise<StepResult>;
  resumeWithValue(sessionId: string, value: unknown): Promise<StepResult>;

  // Inspection
  getSnapshot(sessionId: string): Promise<MachineSnapshot>;
  getBinding(sessionId: string, name: string): Promise<SerializedValue | null>;
  getStoreEntry(sessionId: string, address: number): Promise<SerializedStoreEntry | null>;
  evaluate(sessionId: string, expr: string): Promise<{ value: SerializedValue } | { error: string }>;
  getCallStack(sessionId: string): Promise<SerializedFrame[]>;

  // Breakpoints
  addBreakpoint(sessionId: string, bp: Omit<Breakpoint, 'id'>): Promise<string>;
  removeBreakpoint(sessionId: string, bpId: string): Promise<void>;
  listBreakpoints(sessionId: string): Promise<Breakpoint[]>;

  // Time Travel
  jumpToStep(sessionId: string, step: number): Promise<MachineSnapshot>;
  getHistory(sessionId: string): Promise<HistoryEntry[]>;

  // OPR
  listKernels(): Promise<KernelInfo[]>;
  executeKernel(kernelId: string, program: unknown): Promise<KernelResult>;
}
```

---

## What We Have vs What We Need

| Component | Status | Notes |
|-----------|--------|-------|
| CESK Machine | ✅ Complete | `src/core/eval/` |
| OPR Runtime | ✅ Complete | `src/core/opr/` |
| Session Recording | ✅ Complete | `src/core/session/` |
| State Serializer | ✅ Created | `src/server/stateSerializer.ts` (move to service/) |
| Service Types | ✅ Created | `src/server/debugService.ts` (move to service/) |
| Debug Session | ✅ Created | `src/server/debugSession.ts` (move to service/) |
| HTTP Server | ✅ Created | `src/server/debugServer.ts` |
| REPL (old) | ⚠️ Exists | `bin/omega-repl.ts` - needs refactor |
| REPL (new) | ❌ TODO | `src/repl/` - use service layer |
| Web UI | ❌ TODO | `public/` |

---

## Next Steps

1. **Reorganize files** - Move service layer to `src/service/`
2. **Create service index.ts** - Clear package boundary
3. **Test service layer** - Verify CESK access works
4. **Create minimal web UI** - Prove HTTP/WS works
5. **Refactor REPL** - Use service layer

---

## The Key Insight

**The service layer is the product.**

The REPL and Web UI are just different views into the same capability. By extracting the service layer, we get:
- Testable business logic (no transport dependencies)
- Reusable across multiple UIs
- Clear contracts for future clients (VSCode extension, etc.)
- Separation of concerns
