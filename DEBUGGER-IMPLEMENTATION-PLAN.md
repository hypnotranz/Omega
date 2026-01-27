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

## BLOCK DIAGRAM 1: Overall Architecture (Layered View)

```
┌─────────────────────────────────────────────────────────────────────────────────┐
│                                                                                 │
│                              USER INTERFACES                                    │
│                                                                                 │
│   ┌─────────────────────┐   ┌─────────────────────┐   ┌─────────────────────┐  │
│   │                     │   │                     │   │                     │  │
│   │     CLI REPL        │   │   Web Debugger      │   │  VSCode Extension   │  │
│   │                     │   │                     │   │    (future)         │  │
│   │   ┌───────────┐     │   │   ┌───────────┐     │   │                     │  │
│   │   │ readline  │     │   │   │  Browser  │     │   │                     │  │
│   │   └─────┬─────┘     │   │   └─────┬─────┘     │   │                     │  │
│   │         │           │   │         │           │   │                     │  │
│   └─────────┼───────────┘   └─────────┼───────────┘   └─────────────────────┘  │
│             │                         │                                         │
└─────────────┼─────────────────────────┼─────────────────────────────────────────┘
              │                         │
              │    TRANSPORT LAYER      │
              │                         │
┌─────────────┼─────────────────────────┼─────────────────────────────────────────┐
│             │                         │                                         │
│   ┌─────────▼───────────┐   ┌─────────▼───────────┐                            │
│   │                     │   │                     │                            │
│   │   src/repl/         │   │   src/server/       │                            │
│   │                     │   │                     │                            │
│   │  ReplTransport      │   │  HTTP + WebSocket   │                            │
│   │  (stdin/stdout)     │   │  (Express + ws)     │                            │
│   │                     │   │                     │                            │
│   └─────────┬───────────┘   └─────────┬───────────┘                            │
│             │                         │                                         │
└─────────────┼─────────────────────────┼─────────────────────────────────────────┘
              │                         │
              └────────────┬────────────┘
                           │
                           │  SAME INTERFACE
                           │
┌──────────────────────────┼──────────────────────────────────────────────────────┐
│                          │                                                      │
│                          ▼                                                      │
│   ┌─────────────────────────────────────────────────────────────────────────┐  │
│   │                                                                         │  │
│   │                     IDebugService                                       │  │
│   │                     (THE CONTRACT)                                      │  │
│   │                                                                         │  │
│   │   createSession()    step()           getSnapshot()    addBreakpoint() │  │
│   │   closeSession()     continue()       getBinding()     jumpToStep()    │  │
│   │   loadCode()         resumeWithValue() getCallStack()  listKernels()   │  │
│   │                                                                         │  │
│   └─────────────────────────────────────────────────────────────────────────┘  │
│                          │                                                      │
│                          │  SERVICE LAYER (src/service/)                       │
│                          │                                                      │
│   ┌──────────────────────▼──────────────────────────────────────────────────┐  │
│   │                                                                         │  │
│   │                     DebugService                                        │  │
│   │                     (implementation)                                    │  │
│   │                                                                         │  │
│   │   ┌─────────────┐  ┌─────────────┐  ┌─────────────┐                    │  │
│   │   │ Session #1  │  │ Session #2  │  │ Session #N  │                    │  │
│   │   │DebugSession │  │DebugSession │  │DebugSession │                    │  │
│   │   └──────┬──────┘  └──────┬──────┘  └──────┬──────┘                    │  │
│   │          │                │                │                            │  │
│   └──────────┼────────────────┼────────────────┼────────────────────────────┘  │
│              │                │                │                                │
└──────────────┼────────────────┼────────────────┼────────────────────────────────┘
               │                │                │
               └────────────────┼────────────────┘
                                │
                                │  CORE ENGINE
                                │
┌───────────────────────────────┼─────────────────────────────────────────────────┐
│                               │                                                 │
│                               ▼                                                 │
│   ┌─────────────────────────────────────────────────────────────────────────┐  │
│   │                                                                         │  │
│   │                        CESK MACHINE                                     │  │
│   │                        (src/core/eval/)                                 │  │
│   │                                                                         │  │
│   │   stepOnce(state) → State | Done | Effect                              │  │
│   │                                                                         │  │
│   └─────────────────────────────────────────────────────────────────────────┘  │
│                                                                                 │
│   ┌───────────────────┐  ┌───────────────────┐  ┌───────────────────────────┐  │
│   │                   │  │                   │  │                           │  │
│   │   OPR Runtime     │  │  Session Record   │  │   Effects System          │  │
│   │   (src/core/opr/) │  │  (src/core/       │  │   (src/core/effects/)     │  │
│   │                   │  │   session/)       │  │                           │  │
│   │  - 10 Kernels     │  │                   │  │   - Oracle                │  │
│   │  - Validation     │  │  - Recording      │  │   - OPR Effects           │  │
│   │  - Retry          │  │  - Replay         │  │   - File I/O              │  │
│   │  - Receipts       │  │  - Time Travel    │  │                           │  │
│   │                   │  │                   │  │                           │  │
│   └───────────────────┘  └───────────────────┘  └───────────────────────────┘  │
│                                                                                 │
│                               CORE (src/core/)                                  │
└─────────────────────────────────────────────────────────────────────────────────┘
```

---

## BLOCK DIAGRAM 2: CESK Machine State (What We Expose)

```
┌─────────────────────────────────────────────────────────────────────────────────┐
│                                                                                 │
│                              CESK STATE                                         │
│                         (fully observable)                                      │
│                                                                                 │
│   ┌─────────────────────────────────────────────────────────────────────────┐  │
│   │  C = CONTROL                                                            │  │
│   │  ───────────────────────────────────────────────────────────────────    │  │
│   │                                                                         │  │
│   │  Either:  { tag: "Expr", e: Expression }   ← evaluating expression     │  │
│   │      or:  { tag: "Val",  v: Value }        ← have a value              │  │
│   │                                                                         │  │
│   │  Example: { tag: "Expr", e: { tag: "App", fn: ..., args: [...] } }     │  │
│   │                                                                         │  │
│   └─────────────────────────────────────────────────────────────────────────┘  │
│                                                                                 │
│   ┌─────────────────────────────────────────────────────────────────────────┐  │
│   │  E = ENVIRONMENT                                                        │  │
│   │  ───────────────────────────────────────────────────────────────────    │  │
│   │                                                                         │  │
│   │  Chain of scopes with variable bindings:                               │  │
│   │                                                                         │  │
│   │  ┌─────────────────┐                                                   │  │
│   │  │ Scope 0 (inner) │  x → 42, y → "hello"                              │  │
│   │  └────────┬────────┘                                                   │  │
│   │           │ parent                                                      │  │
│   │  ┌────────▼────────┐                                                   │  │
│   │  │ Scope 1         │  factorial → <closure>, + → <native>              │  │
│   │  └────────┬────────┘                                                   │  │
│   │           │ parent                                                      │  │
│   │  ┌────────▼────────┐                                                   │  │
│   │  │ Scope 2 (global)│  car → <native>, cdr → <native>, ...              │  │
│   │  └─────────────────┘                                                   │  │
│   │                                                                         │  │
│   └─────────────────────────────────────────────────────────────────────────┘  │
│                                                                                 │
│   ┌─────────────────────────────────────────────────────────────────────────┐  │
│   │  S = STORE (Heap)                                                       │  │
│   │  ───────────────────────────────────────────────────────────────────    │  │
│   │                                                                         │  │
│   │  Address → Value mapping:                                               │  │
│   │                                                                         │  │
│   │  ┌─────────┬─────────────────────────────────────┐                     │  │
│   │  │ Addr 0  │ { tag: "Vector", items: [...] }     │                     │  │
│   │  │ Addr 1  │ { tag: "Closure", params: [...] }   │                     │  │
│   │  │ Addr 2  │ { tag: "Str", s: "hello world" }    │                     │  │
│   │  │ ...     │ ...                                  │                     │  │
│   │  └─────────┴─────────────────────────────────────┘                     │  │
│   │                                                                         │  │
│   └─────────────────────────────────────────────────────────────────────────┘  │
│                                                                                 │
│   ┌─────────────────────────────────────────────────────────────────────────┐  │
│   │  K = KONTINUATION (Call Stack)                                          │  │
│   │  ───────────────────────────────────────────────────────────────────    │  │
│   │                                                                         │  │
│   │  Stack of frames (what to do next with the result):                    │  │
│   │                                                                         │  │
│   │  ┌─────────────────────────────────────────────────────────────────┐   │  │
│   │  │ [0] KAppArg    - evaluating args, 2/3 done, fn = <closure>     │   │  │
│   │  │ [1] KCall      - will return to caller's env                    │   │  │
│   │  │ [2] KDefine    - will bind result to "result"                   │   │  │
│   │  │ [3] KBegin     - 2 more expressions in sequence                 │   │  │
│   │  │ [4] KIf        - waiting for condition, then branch             │   │  │
│   │  └─────────────────────────────────────────────────────────────────┘   │  │
│   │        ↑                                                                │  │
│   │       TOP (most recent)                                                 │  │
│   │                                                                         │  │
│   └─────────────────────────────────────────────────────────────────────────┘  │
│                                                                                 │
│   ┌─────────────────────────────────────────────────────────────────────────┐  │
│   │  H = HANDLERS (Effect Handler Stack)                                    │  │
│   │  ───────────────────────────────────────────────────────────────────    │  │
│   │                                                                         │  │
│   │  ┌─────────────────────────────────────────────────────────────────┐   │  │
│   │  │ [0] Handler for "file.read", "file.write"                       │   │  │
│   │  │ [1] Handler for "oracle.apply"                                  │   │  │
│   │  │ [2] Handler for "opr.step.*"                                    │   │  │
│   │  └─────────────────────────────────────────────────────────────────┘   │  │
│   │                                                                         │  │
│   └─────────────────────────────────────────────────────────────────────────┘  │
│                                                                                 │
└─────────────────────────────────────────────────────────────────────────────────┘
```

---

## BLOCK DIAGRAM 3: Data Flow (Step Execution)

```
                          ┌──────────────────┐
                          │   User Action    │
                          │  (click Step)    │
                          └────────┬─────────┘
                                   │
                                   ▼
┌──────────────────────────────────────────────────────────────────────────────┐
│  TRANSPORT (HTTP or readline)                                                │
│                                                                              │
│    POST /session/abc/step                                                    │
│    or                                                                        │
│    > :step                                                                   │
│                                                                              │
└──────────────────────────────────┬───────────────────────────────────────────┘
                                   │
                                   ▼
┌──────────────────────────────────────────────────────────────────────────────┐
│  SERVICE: IDebugService.step("abc")                                          │
│                                                                              │
│    1. Get session "abc" from sessions map                                    │
│    2. Call session.step()                                                    │
│                                                                              │
└──────────────────────────────────┬───────────────────────────────────────────┘
                                   │
                                   ▼
┌──────────────────────────────────────────────────────────────────────────────┐
│  DEBUG SESSION                                                               │
│                                                                              │
│    1. Call stepOnce(this.state)                                              │
│    2. Check breakpoints                                                      │
│    3. Record to history                                                      │
│    4. Serialize state → MachineSnapshot                                      │
│                                                                              │
└──────────────────────────────────┬───────────────────────────────────────────┘
                                   │
                                   ▼
┌──────────────────────────────────────────────────────────────────────────────┐
│  CESK MACHINE: stepOnce(state)                                               │
│                                                                              │
│    Input:  State { control, env, store, kont, handlers }                     │
│                                                                              │
│    Output: StepOutcome                                                       │
│            ├─ { tag: "State", state: newState }     → keep stepping         │
│            ├─ { tag: "Done",  value: Val }          → execution complete    │
│            └─ { tag: "Op",    opcall: OpCall }      → effect encountered    │
│                                                                              │
└──────────────────────────────────┬───────────────────────────────────────────┘
                                   │
                                   ▼
┌──────────────────────────────────────────────────────────────────────────────┐
│  STATE SERIALIZER                                                            │
│                                                                              │
│    State (TypeScript)  ──────►  MachineSnapshot (JSON)                       │
│                                                                              │
│    ┌─────────────────┐          ┌─────────────────────────────────────────┐ │
│    │ control: {      │          │ {                                       │ │
│    │   tag: "Expr",  │    ►     │   "snapshotId": "snap_42",              │ │
│    │   e: {...}      │          │   "step": 15,                           │ │
│    │ }               │          │   "control": {                          │ │
│    │ env: Ctx        │          │     "tag": "Expr",                      │ │
│    │ store: COWStore │          │     "exprType": "App",                  │ │
│    │ kont: Frame[]   │          │     "summary": "(factorial 5)"          │ │
│    │ handlers: [...]│          │   },                                    │ │
│    └─────────────────┘          │   "callStack": [...],                   │ │
│                                 │   "environment": [...],                 │ │
│                                 │   "status": "paused"                    │ │
│                                 │ }                                       │ │
│                                 └─────────────────────────────────────────┘ │
│                                                                              │
└──────────────────────────────────┬───────────────────────────────────────────┘
                                   │
                                   ▼
┌──────────────────────────────────────────────────────────────────────────────┐
│  RESPONSE                                                                    │
│                                                                              │
│    {                                                                         │
│      "snapshot": { ... MachineSnapshot ... },                                │
│      "outcome": "stepped"                                                    │
│    }                                                                         │
│                                                                              │
│    Also broadcast via WebSocket to all connected clients                     │
│                                                                              │
└──────────────────────────────────────────────────────────────────────────────┘
```

---

## BLOCK DIAGRAM 4: Package Boundaries

```
┌─────────────────────────────────────────────────────────────────────────────────┐
│                                                                                 │
│  bin/                           ENTRY POINTS (executables)                      │
│  ────                                                                           │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐                 │
│  │ omega-repl.ts   │  │ omega-server.ts │  │omega-debugger.ts│                 │
│  │                 │  │                 │  │  (deprecated)   │                 │
│  │ uses: src/repl/ │  │uses: src/server/│  │                 │                 │
│  └────────┬────────┘  └────────┬────────┘  └─────────────────┘                 │
│           │                    │                                                │
└───────────┼────────────────────┼────────────────────────────────────────────────┘
            │                    │
            ▼                    ▼
┌───────────────────────────────────────────────────────────────────────────────┐
│                                                                               │
│  src/repl/                      src/server/                                   │
│  ─────────                      ───────────                                   │
│                                                                               │
│  PUBLIC EXPORTS:                PUBLIC EXPORTS:                               │
│  ┌─────────────────────┐       ┌─────────────────────────┐                   │
│  │ • startRepl()       │       │ • DebugServer           │                   │
│  │ • ReplConfig        │       │ • startDebugServer()    │                   │
│  └──────────┬──────────┘       └───────────┬─────────────┘                   │
│             │                              │                                  │
│             │    BOTH IMPORT FROM:         │                                  │
│             │                              │                                  │
│             └──────────────┬───────────────┘                                  │
│                            │                                                  │
└────────────────────────────┼──────────────────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────────────────────┐
│                                                                                 │
│  src/service/                   THE SERVICE LAYER                               │
│  ────────────                   (transport-agnostic)                            │
│                                                                                 │
│  ┌───────────────────────────────────────────────────────────────────────────┐ │
│  │                                                                           │ │
│  │  PUBLIC EXPORTS (index.ts):                                               │ │
│  │                                                                           │ │
│  │  TYPES:                                                                   │ │
│  │    • IDebugService          (the contract)                                │ │
│  │    • MachineSnapshot        (state at a point in time)                    │ │
│  │    • StepResult             (result of stepping)                          │ │
│  │    • Breakpoint             (breakpoint config)                           │ │
│  │    • SerializedValue        (JSON-safe value)                             │ │
│  │    • SerializedFrame        (JSON-safe stack frame)                       │ │
│  │    • SessionConfig          (session options)                             │ │
│  │                                                                           │ │
│  │  IMPLEMENTATION:                                                          │ │
│  │    • DebugService           (implements IDebugService)                    │ │
│  │    • createDebugService()   (factory function)                            │ │
│  │                                                                           │ │
│  └───────────────────────────────────────────────────────────────────────────┘ │
│                                                                                 │
│  INTERNAL (not exported):                                                       │
│    • DebugSession              (per-session wrapper)                           │
│    • stateSerializer           (State → JSON)                                  │
│                                                                                 │
│                            │                                                    │
└────────────────────────────┼────────────────────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────────────────────┐
│                                                                                 │
│  src/core/                      THE ENGINE                                      │
│  ─────────                      (no changes needed)                             │
│                                                                                 │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐                 │
│  │                 │  │                 │  │                 │                 │
│  │  src/core/eval/ │  │  src/core/opr/  │  │src/core/session/│                 │
│  │                 │  │                 │  │                 │                 │
│  │  • State        │  │  • OprRuntime   │  │  • Writer       │                 │
│  │  • stepOnce()   │  │  • Kernels      │  │  • Reader       │                 │
│  │  • Frame        │  │  • Adapters     │  │  • Jump         │                 │
│  │  • Val          │  │  • Bridge       │  │                 │                 │
│  │                 │  │                 │  │                 │                 │
│  └─────────────────┘  └─────────────────┘  └─────────────────┘                 │
│                                                                                 │
└─────────────────────────────────────────────────────────────────────────────────┘
```

---

## BLOCK DIAGRAM 5: OPR Integration (Full Detail)

```
┌─────────────────────────────────────────────────────────────────────────────────┐
│                                                                                 │
│                         OPR (OMEGA PROTOCOL RUNTIME)                            │
│                                                                                 │
│   The OPR is a first-class effect system that provides:                         │
│   - Typed protocol steps (kernels)                                              │
│   - Validation & retry logic                                                    │
│   - Deterministic receipts                                                      │
│                                                                                 │
└─────────────────────────────────────────────────────────────────────────────────┘

                           OPR ARCHITECTURE
┌─────────────────────────────────────────────────────────────────────────────────┐
│                                                                                 │
│   USER CODE (Omega Lisp)                                                        │
│   ──────────────────────                                                        │
│                                                                                 │
│   (opr-step "validate-json" data-expr)                                          │
│   (opr-step "llm-chat" prompt-expr)                                             │
│   (opr-step "parse-sexp" text-expr)                                             │
│                                                                                 │
│             │                                                                   │
│             │  evaluates to effect                                              │
│             ▼                                                                   │
│                                                                                 │
│   ┌─────────────────────────────────────────────────────────────────────────┐  │
│   │                                                                         │  │
│   │                    EFFECT: { op: "opr.step", args: [...] }              │  │
│   │                                                                         │  │
│   └─────────────────────────────────────────────────────────────────────────┘  │
│             │                                                                   │
│             │  caught by effect handler                                         │
│             ▼                                                                   │
│                                                                                 │
│   ┌─────────────────────────────────────────────────────────────────────────┐  │
│   │                                                                         │  │
│   │                   OPR EFFECT HANDLER                                    │  │
│   │                   (src/core/opr/handler.ts)                             │  │
│   │                                                                         │  │
│   │   1. Extract kernel name and program from args                          │  │
│   │   2. Look up kernel in registry                                         │  │
│   │   3. Call kernel.execute(program)                                       │  │
│   │   4. Convert result back to Val                                         │  │
│   │   5. Resume continuation with result                                    │  │
│   │                                                                         │  │
│   └─────────────────────────────────────────────────────────────────────────┘  │
│             │                                                                   │
│             │  dispatches to                                                    │
│             ▼                                                                   │
│                                                                                 │
└─────────────────────────────────────────────────────────────────────────────────┘

                           OPR KERNEL REGISTRY
┌─────────────────────────────────────────────────────────────────────────────────┐
│                                                                                 │
│   OprRuntime (src/core/opr/runtime.ts)                                          │
│   ──────────────────────────────────────                                        │
│                                                                                 │
│   ┌─────────────────────────────────────────────────────────────────────────┐  │
│   │                                                                         │  │
│   │   kernelRegistry: Map<string, OprKernel>                                │  │
│   │                                                                         │  │
│   │   ┌─────────────────────────────────────────────────────────────────┐   │  │
│   │   │                                                                 │   │  │
│   │   │   THE 10 KERNELS                                                │   │  │
│   │   │                                                                 │   │  │
│   │   │   ┌───────────────┬────────────────────────────────────────┐   │   │  │
│   │   │   │ Kernel        │ Purpose                                │   │   │  │
│   │   │   ├───────────────┼────────────────────────────────────────┤   │   │  │
│   │   │   │ validate-json │ Validate JSON against schema           │   │   │  │
│   │   │   │ parse-json    │ Parse JSON string to value             │   │   │  │
│   │   │   │ stringify-json│ Convert value to JSON string           │   │   │  │
│   │   │   │ parse-sexp    │ Parse S-expression to AST              │   │   │  │
│   │   │   │ emit-sexp     │ Convert AST to S-expression string     │   │   │  │
│   │   │   │ llm-chat      │ Call LLM with prompt                   │   │   │  │
│   │   │   │ llm-embed     │ Generate embeddings                    │   │   │  │
│   │   │   │ transform     │ Apply transformation pipeline          │   │   │  │
│   │   │   │ hash          │ Compute cryptographic hash             │   │   │  │
│   │   │   │ verify        │ Verify signature/hash                  │   │   │  │
│   │   │   └───────────────┴────────────────────────────────────────┘   │   │  │
│   │   │                                                                 │   │  │
│   │   └─────────────────────────────────────────────────────────────────┘   │  │
│   │                                                                         │  │
│   └─────────────────────────────────────────────────────────────────────────┘  │
│                                                                                 │
│   KERNEL INTERFACE:                                                             │
│   ┌─────────────────────────────────────────────────────────────────────────┐  │
│   │                                                                         │  │
│   │   interface OprKernel {                                                 │  │
│   │     id: string;              // "validate-json"                         │  │
│   │     op: string;              // "opr.step.validate-json"                │  │
│   │     inputSchema: ZodSchema;  // validation for input                    │  │
│   │     outputSchema: ZodSchema; // validation for output                   │  │
│   │     execute(program: unknown): Promise<OprResult>;                      │  │
│   │   }                                                                     │  │
│   │                                                                         │  │
│   │   interface OprResult {                                                 │  │
│   │     success: boolean;                                                   │  │
│   │     value?: unknown;         // on success                              │  │
│   │     error?: string;          // on failure                              │  │
│   │     receipt: OprReceipt;     // always present                          │  │
│   │   }                                                                     │  │
│   │                                                                         │  │
│   │   interface OprReceipt {                                                │  │
│   │     kernelId: string;                                                   │  │
│   │     inputHash: string;                                                  │  │
│   │     outputHash: string;                                                 │  │
│   │     timestamp: string;                                                  │  │
│   │     duration: number;                                                   │  │
│   │   }                                                                     │  │
│   │                                                                         │  │
│   └─────────────────────────────────────────────────────────────────────────┘  │
│                                                                                 │
└─────────────────────────────────────────────────────────────────────────────────┘

                           OPR ↔ CESK INTEGRATION
┌─────────────────────────────────────────────────────────────────────────────────┐
│                                                                                 │
│   When CESK machine encounters OPR effect:                                      │
│                                                                                 │
│   ┌─────────────────────────────────────────────────────────────────────────┐  │
│   │                                                                         │  │
│   │   STEP 1: Effect Raised                                                 │  │
│   │   ─────────────────────                                                 │  │
│   │                                                                         │  │
│   │   stepOnce(state) returns:                                              │  │
│   │   {                                                                     │  │
│   │     tag: "Op",                                                          │  │
│   │     opcall: {                                                           │  │
│   │       op: "opr.step",                                                   │  │
│   │       args: ["validate-json", { data: {...}, schema: {...} }],          │  │
│   │       resumption: <Resumption>     ← HOW TO CONTINUE                    │  │
│   │     },                                                                  │  │
│   │     state: <State at effect>                                            │  │
│   │   }                                                                     │  │
│   │                                                                         │  │
│   └─────────────────────────────────────────────────────────────────────────┘  │
│             │                                                                   │
│             │  debugger pauses, shows "effect pending"                          │
│             │  user can inspect state, then choose to resume                    │
│             ▼                                                                   │
│   ┌─────────────────────────────────────────────────────────────────────────┐  │
│   │                                                                         │  │
│   │   STEP 2: Effect Handled (automatic or manual)                          │  │
│   │   ───────────────────────────────────────────                           │  │
│   │                                                                         │  │
│   │   // Execute the kernel                                                 │  │
│   │   const result = await runtime.execute("validate-json", program);       │  │
│   │                                                                         │  │
│   │   // Convert result to Val                                              │  │
│   │   const valResult = jsonToVal(result);                                  │  │
│   │                                                                         │  │
│   │   // Resume the continuation with the result                            │  │
│   │   const newState = resumption.invoke(valResult);                        │  │
│   │                                                                         │  │
│   └─────────────────────────────────────────────────────────────────────────┘  │
│             │                                                                   │
│             │  CESK machine continues from new state                            │
│             ▼                                                                   │
│   ┌─────────────────────────────────────────────────────────────────────────┐  │
│   │                                                                         │  │
│   │   STEP 3: Execution Continues                                           │  │
│   │   ─────────────────────────                                             │  │
│   │                                                                         │  │
│   │   state.control = { tag: "Val", v: result-value }                       │  │
│   │   state.kont = ... (continuation processes result)                      │  │
│   │                                                                         │  │
│   └─────────────────────────────────────────────────────────────────────────┘  │
│                                                                                 │
└─────────────────────────────────────────────────────────────────────────────────┘

                           OPR IN DEBUGGER UI
┌─────────────────────────────────────────────────────────────────────────────────┐
│                                                                                 │
│   IDebugService OPR Methods                                                     │
│   ─────────────────────────                                                     │
│                                                                                 │
│   ┌─────────────────────────────────────────────────────────────────────────┐  │
│   │                                                                         │  │
│   │   // List available kernels (for UI dropdown/autocomplete)              │  │
│   │   listKernels(): Promise<Array<{                                        │  │
│   │     id: string;           // "validate-json"                            │  │
│   │     op: string;           // "opr.step.validate-json"                   │  │
│   │     description: string;  // "Validate JSON against schema"             │  │
│   │     inputSchema: object;  // JSON Schema for input                      │  │
│   │     outputSchema: object; // JSON Schema for output                     │  │
│   │   }>>                                                                   │  │
│   │                                                                         │  │
│   │   // Execute kernel directly (bypass CESK, useful for testing)          │  │
│   │   executeKernel(kernelId: string, program: unknown): Promise<{          │  │
│   │     success: boolean;                                                   │  │
│   │     value?: unknown;                                                    │  │
│   │     error?: string;                                                     │  │
│   │     receipt: OprReceipt;                                                │  │
│   │   }>                                                                    │  │
│   │                                                                         │  │
│   └─────────────────────────────────────────────────────────────────────────┘  │
│                                                                                 │
│   Debugger UI Features for OPR                                                  │
│   ──────────────────────────                                                    │
│                                                                                 │
│   ┌─────────────────────────────────────────────────────────────────────────┐  │
│   │                                                                         │  │
│   │   1. EFFECT INSPECTION                                                  │  │
│   │      When status = "effect" and pendingEffect.op starts with "opr.":   │  │
│   │      - Show kernel name                                                 │  │
│   │      - Show input program (formatted)                                   │  │
│   │      - Show input/output schemas                                        │  │
│   │                                                                         │  │
│   │   2. MANUAL RESUME                                                      │  │
│   │      - User can provide custom return value                             │  │
│   │      - Useful for testing/mocking LLM responses                         │  │
│   │                                                                         │  │
│   │   3. KERNEL PLAYGROUND                                                  │  │
│   │      - Test kernels independently via executeKernel()                   │  │
│   │      - Try different inputs, see outputs                                │  │
│   │                                                                         │  │
│   │   4. RECEIPT INSPECTION                                                 │  │
│   │      - View execution receipts                                          │  │
│   │      - Verify deterministic replay                                      │  │
│   │                                                                         │  │
│   └─────────────────────────────────────────────────────────────────────────┘  │
│                                                                                 │
└─────────────────────────────────────────────────────────────────────────────────┘

                           OPR DATA FLOW (COMPLETE)
┌─────────────────────────────────────────────────────────────────────────────────┐
│                                                                                 │
│   ┌──────────────────────────────────────────────────────────────────────────┐ │
│   │                                                                          │ │
│   │   (opr-step "llm-chat" (quote ((prompt "Hello, world!"))))               │ │
│   │                                                                          │ │
│   └────────────────────────────────────┬─────────────────────────────────────┘ │
│                                        │                                       │
│                                        ▼                                       │
│   ┌──────────────────────────────────────────────────────────────────────────┐ │
│   │  CESK MACHINE                                                            │ │
│   │                                                                          │ │
│   │  control: { tag: "Expr", e: { tag: "Effect", op: "opr.step", ... } }     │ │
│   │                            │                                             │ │
│   │                            ▼                                             │ │
│   │  stepOnce() → { tag: "Op", opcall: { op: "opr.step", ... } }             │ │
│   │                                                                          │ │
│   └────────────────────────────────────┬─────────────────────────────────────┘ │
│                                        │                                       │
│                                        ▼                                       │
│   ┌──────────────────────────────────────────────────────────────────────────┐ │
│   │  DEBUG SESSION (paused at effect)                                        │ │
│   │                                                                          │ │
│   │  snapshot.status = "effect"                                              │ │
│   │  snapshot.pendingEffect = {                                              │ │
│   │    op: "opr.step",                                                       │ │
│   │    args: [                                                               │ │
│   │      { tag: "Str", summary: "\"llm-chat\"" },                             │ │
│   │      { tag: "List", summary: "((prompt ...))" }                          │ │
│   │    ]                                                                     │ │
│   │  }                                                                       │ │
│   │                                                                          │ │
│   └────────────────────────────────────┬─────────────────────────────────────┘ │
│                                        │                                       │
│         ┌──────────────────────────────┴─────────────────────┐                 │
│         │                                                    │                 │
│         ▼                                                    ▼                 │
│   ┌─────────────────────────────┐  ┌─────────────────────────────────────────┐ │
│   │  OPTION A: Auto-resume      │  │  OPTION B: Manual resume                │ │
│   │                             │  │                                         │ │
│   │  session.continue()         │  │  session.resumeWithValue({              │ │
│   │  → executes kernel          │  │    response: "Mock LLM response"        │ │
│   │  → resumes with result      │  │  })                                     │ │
│   │                             │  │  → resumes with provided value          │ │
│   └─────────────────────────────┘  └─────────────────────────────────────────┘ │
│                                        │                                       │
│                                        ▼                                       │
│   ┌──────────────────────────────────────────────────────────────────────────┐ │
│   │  OPR RUNTIME                                                             │ │
│   │                                                                          │ │
│   │  1. Lookup kernel: kernelRegistry.get("llm-chat")                        │ │
│   │  2. Validate input: inputSchema.parse(program)                           │ │
│   │  3. Execute: kernel.execute({ prompt: "Hello, world!" })                 │ │
│   │  4. Validate output: outputSchema.parse(result)                          │ │
│   │  5. Generate receipt: { kernelId, inputHash, outputHash, ... }           │ │
│   │  6. Return: { success: true, value: {...}, receipt: {...} }              │ │
│   │                                                                          │ │
│   └────────────────────────────────────┬─────────────────────────────────────┘ │
│                                        │                                       │
│                                        ▼                                       │
│   ┌──────────────────────────────────────────────────────────────────────────┐ │
│   │  BRIDGE (src/core/opr/bridge.ts)                                         │ │
│   │                                                                          │ │
│   │  jsonToVal(result) → Val                                                 │ │
│   │                                                                          │ │
│   │  { success: true, value: "Hello!" } → { tag: "Map", entries: [...] }     │ │
│   │                                                                          │ │
│   └────────────────────────────────────┬─────────────────────────────────────┘ │
│                                        │                                       │
│                                        ▼                                       │
│   ┌──────────────────────────────────────────────────────────────────────────┐ │
│   │  CESK MACHINE (resumed)                                                  │ │
│   │                                                                          │ │
│   │  control: { tag: "Val", v: { tag: "Map", ... } }                         │ │
│   │  → continues with result value                                           │ │
│   │                                                                          │ │
│   └──────────────────────────────────────────────────────────────────────────┘ │
│                                                                                 │
└─────────────────────────────────────────────────────────────────────────────────┘
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

---

## CRITICAL ISSUES FOUND (Must Fix Before Proceeding)

**Assessment Date:** 2026-01-27

### Issue 1: Code Does Not Compile

Current `src/server/` files have TypeScript errors:

```
src/server/debugSession.ts(100,31): error TS2339: Property 'fork' does not exist on type 'COWStore'.
src/server/debugSession.ts(375,56): error TS2345: Argument of type '"idle"' is not assignable...
src/server/debugSession.ts(424,33): error TS2339: Property 'fork' does not exist on type 'Store'.
src/server/debugSession.ts(479,40): error TS2339: Property 'fork' does not exist on type 'COWStore'.
src/server/stateSerializer.ts(229-233): Expression types not in union (Bind, ConditionHandlerBind, etc.)
src/server/stateSerializer.ts(248): Property 'source' does not exist on type 'Expr'.
src/server/stateSerializer.ts(446-448): RuntimeBudget property names wrong.
```

**Root Causes:**
1. `COWStore` has `snapshot()` not `fork()` - wrong method name
2. `MachineSnapshot.status` union doesn't include `'idle'`
3. AST `Expr` type doesn't include all expression tags the serializer tries to handle
4. `Expr` doesn't have a `source` property
5. `RuntimeBudget` has different property names than assumed

**Fix Required:** Update debugSession.ts and stateSerializer.ts to match actual core types.

---

### Issue 2: Wrong Import Path for Primitives

```typescript
// debugSession.ts line 23 - WRONG:
import { installPrims } from '../../test/helpers/prims';

// SHOULD BE:
import { installPrims } from '../core/prims';
```

Production code should not import from test helpers.

---

### Issue 3: OPR Adapter Incomplete

```
src/core/opr/adapters/transportAdapter.ts(9,45): error TS2305: Module '"./types"' has no exported member 'OprUsageInfo'.
src/core/opr/adapters/transportAdapter.ts(12,14): error TS2420: Class 'TransportOprAdapter' incorrectly implements interface 'OprLLMAdapter'.
  Property 'supportsStreaming' is missing...
```

The OPR adapter types are out of sync.

---

## REVISED ACHIEVABILITY ASSESSMENT

| Phase | Status | Risk | Effort | Notes |
|-------|--------|------|--------|-------|
| **Phase 0: Fix Compilation** | ❌ BLOCKING | HIGH | 2-4h | Must fix before anything else works |
| **Phase 1: Service Layer** | ⚠️ Ready | LOW | 1-2h | Just file reorganization after Phase 0 |
| **Phase 2: Test Server** | ⚠️ Blocked | MEDIUM | 2-3h | Need integration tests |
| **Phase 3: REPL Refactor** | ⚠️ Blocked | MEDIUM | 4-8h | Preserve existing functionality |
| **Phase 4: Web UI** | ⚠️ Blocked | LOW | 4-8h | Standard HTML/JS work |

### What's Actually Working

| Component | Status | Confidence |
|-----------|--------|------------|
| CESK Machine (`stepOnce`) | ✅ Works | HIGH - Core engine, tested |
| OPR Runtime (`OprRuntime`) | ✅ Works | HIGH - Full implementation |
| OPR Kernels Registry | ✅ Exists | MEDIUM - `src/core/opr/kernels/` |
| OPR Bridge (`jsonToVal`) | ✅ Works | HIGH - Used in production |
| Primitives (`installPrims`) | ✅ Works | HIGH - `src/core/prims.ts` |
| State Types | ✅ Works | HIGH - Core types |
| Debug Session | ❌ Broken | LOW - Compilation errors |
| State Serializer | ❌ Broken | LOW - Type mismatches |
| Debug Server | ❌ Broken | LOW - Dependencies broken |

### Recommended Fix Order

```
1. Fix COWStore.fork() → COWStore.snapshot()  (3 locations)
2. Fix installPrims import path
3. Fix MachineSnapshot.status to include 'idle'
4. Fix stateSerializer expression handling (guard against unknown tags)
5. Remove source property access (or make optional)
6. Fix RuntimeBudget property access
7. Run tsc --noEmit to verify clean compile
8. Add basic smoke test
```

---

## HONEST SUMMARY

**The architecture is sound. The implementation has bugs.**

The CESK machine, OPR runtime, and core infrastructure all work. The debug server layer was written against assumed APIs that don't match the actual codebase.

**Time to working server:** ~4-6 hours of bug fixing + testing.

**Recommendation:** Fix compilation errors FIRST, then do a basic smoke test before any reorganization. Moving broken code into new directories accomplishes nothing.

---

## STATUS UPDATE (2026-01-27)

### ALL BUGS FIXED

| Bug | Fix Applied |
|-----|-------------|
| `COWStore.fork()` doesn't exist | Changed to `snapshot()` (3 locations) |
| Wrong import path for primitives | `'../../test/helpers/prims'` -> `'../core/prims'` |
| `'idle'` not in status union | Convert to `'paused'` before serialization |
| Unknown Expr types | Use default case instead of specific cases |
| `Expr.source` doesn't exist | Removed source property access |
| `RuntimeBudget` wrong props | Changed to `stepsLeft`, `inferCallsLeft`, etc. |
| `OprUsageInfo` doesn't exist | Changed to `LLMUsage` |
| `supportsStreaming` missing | Added method to adapter |
| `envGet` returns address not value | Read value from store using address |
| ESM `require.main` | Use `import.meta.url` for main detection |
| ESM `__dirname` | Use `fileURLToPath(import.meta.url)` |

### WEB UI COMPLETE

Beautiful dark-themed debugger UI at `public/`:
- **index.html**: Full layout with CESK machine panels
- **style.css**: Modern dark theme, responsive design
- **app.js**: Complete WebSocket client

Features:
- Session management (create, list)
- Code editor with Lisp syntax
- Step/Step10/Continue/Run controls
- CESK state visualization (C, E, S, K, H)
- Call stack with frame details
- Environment bindings display
- Effect handling with manual resume
- Time travel (click history to jump)
- Keyboard shortcuts (F10, F5, F8)
- Real-time WebSocket updates

### TESTED END-TO-END

```
POST /session -> {"sessionId":"session_3_mkwbdxz2"}
POST /session/:id/load {"code":"(define x 42) (+ x 10)"} -> {"success":true}
POST /session/:id/step -> step 1, control: (define x$bid#1 ...)
POST /session/:id/run -> {"outcome":"done","result":{"tag":"Num","summary":"52"}}
```

### COMMITS

- `3c45d4c` - feat: Fix debug server bugs and add beautiful web UI
- `aef9577` - docs: Add critical issues assessment to implementation plan
- `4dfbba1` - docs: Add comprehensive OPR block diagram to implementation plan

### TO RUN

```bash
cd OmegaLLM
npx tsx src/server/debugServer.ts
# Open http://localhost:3456
```
