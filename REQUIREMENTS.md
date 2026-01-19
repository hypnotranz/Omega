# OmegaLLM Kernel Completion Requirements

This document provides **full specifications** for every missing feature identified in the kernel audit. Each section includes:
- Problem statement
- Detailed requirements
- Interface definitions
- Implementation guidance
- Test specifications

---

## Table of Contents

1. [Budget Enforcement](#1-budget-enforcement)
2. [ReqTool Implementation](#2-reqtool-implementation)
3. [Tool Registry](#3-tool-registry)
4. [Real LLM Adapter](#4-real-llm-adapter)
5. [Nondet Best/Sample Modes](#5-nondet-bestsample-modes)
6. [Context Constraints](#6-context-constraints)
7. [Evidence Accumulation](#7-evidence-accumulation)
8. [REPL Debugger Commands](#8-repl-debugger-commands)
9. [REPL Pause/Resume/Step](#9-repl-pauseresumestep)
10. [REPL Oracle Interaction](#10-repl-oracle-interaction)
11. [REPL Polish Features](#11-repl-polish-features)
12. [Deterministic IDs](#12-deterministic-ids)

---

## 1. Budget Enforcement

### 1.1 Problem Statement

The budget system (`governance/budgets.ts`) defines `budgetConsumeEvalStep()`, `budgetConsumeOracleTurn()`, and `budgetConsumeToolCall()` functions that throw when limits are exceeded. However, these functions are **never called** during actual evaluation. The evaluator can run infinitely.

### 1.2 Requirements

| ID | Requirement |
|----|-------------|
| BUD-1 | Every call to `stepOnce()` MUST consume one eval step from budget |
| BUD-2 | Every oracle turn (yield in oracle session) MUST consume one oracle turn from budget |
| BUD-3 | Every tool call (ReqTool) MUST consume one tool call from budget |
| BUD-4 | Budget exhaustion MUST throw `BudgetExhaustedError` with type and counts |
| BUD-5 | Budget MUST be queryable at any point (remaining amounts) |
| BUD-6 | Budget MUST be passable to `runToCompletion()` as optional parameter |
| BUD-7 | Default behavior (no budget passed) MUST use unlimited budget |

### 1.3 Interface Definitions

```typescript
// src/core/governance/budgets.ts (additions)

export class BudgetExhaustedError extends Error {
  constructor(
    public readonly type: "evalSteps" | "oracleTurns" | "toolCalls",
    public readonly limit: number,
    public readonly consumed: number,
  ) {
    super(`Budget exhausted: ${type} (${consumed}/${limit})`);
    this.name = "BudgetExhaustedError";
  }
}

export interface BudgetTracker {
  /** Consume one eval step. Throws if exceeded. */
  consumeEvalStep(): void;

  /** Consume one oracle turn. Throws if exceeded. */
  consumeOracleTurn(): void;

  /** Consume one tool call. Throws if exceeded. */
  consumeToolCall(): void;

  /** Get remaining budget */
  remaining(): BudgetLimits;

  /** Get consumed amounts */
  consumed(): { evalSteps: number; oracleTurns: number; toolCalls: number };

  /** Check if budget allows more of given type */
  hasRemaining(type: "evalSteps" | "oracleTurns" | "toolCalls"): boolean;
}

export function createBudgetTracker(limits?: Partial<BudgetLimits>): BudgetTracker;

/** Unlimited budget that never throws */
export const UNLIMITED_BUDGET: BudgetTracker;
```

```typescript
// src/core/eval/run.ts (modifications)

export interface RunOptions {
  maxSteps?: number;
  budget?: BudgetTracker;
}

export async function runToCompletion(
  runtime: Runtime,
  state0: State,
  options?: RunOptions | number,  // number for backwards compat (maxSteps)
): Promise<Val>;
```

### 1.4 Implementation Guidance

**File: `src/core/eval/run.ts`**

```typescript
export async function runToCompletion(
  runtime: Runtime,
  state0: State,
  options?: RunOptions | number,
): Promise<Val> {
  // Normalize options
  const opts: RunOptions = typeof options === "number"
    ? { maxSteps: options }
    : options ?? {};

  const maxSteps = opts.maxSteps ?? 1_000_000;
  const budget = opts.budget ?? UNLIMITED_BUDGET;

  let st = state0;
  let steps = 0;

  while (true) {
    // Consume budget BEFORE step
    budget.consumeEvalStep();  // Throws BudgetExhaustedError if exceeded

    steps++;
    if (steps > maxSteps) {
      throw new Error(`maxSteps exceeded: ${steps}`);
    }

    const outcome = stepOnce(st);

    if (outcome.tag === "Done") {
      return outcome.value;
    }

    if (outcome.tag === "Op") {
      const dispatchResult = await runtime.dispatch(outcome.state, outcome.opcall);

      if (dispatchResult === "Uncaught") {
        throw new Error(`Uncaught effect: ${outcome.opcall.op}`);
      }

      st = dispatchResult;
      continue;
    }

    st = outcome.state;
  }
}
```

**File: `src/core/oracle/driver.ts`**

```typescript
export async function runOracleSession(
  session: OracleSession,
  portal: OraclePortal,
  budget?: BudgetTracker,  // Add parameter
): Promise<MeaningVal> {
  const bt = budget ?? UNLIMITED_BUDGET;

  let resp: OracleResp = { tag: "RespAck" };

  while (true) {
    bt.consumeOracleTurn();  // Consume on each yield

    const step = await session.next(resp);
    // ... rest unchanged
  }
}
```

### 1.5 Test Specifications

```typescript
// test/core/governance/budget-enforcement.spec.ts

describe("Budget Enforcement", () => {
  test("BUD-1: eval steps are consumed", async () => {
    const budget = createBudgetTracker({ maxEvalSteps: 10 });
    const expr = compileTextToExpr("(+ 1 2)");
    await runToCompletion(runtime, makeState(expr), { budget });
    expect(budget.consumed().evalSteps).toBeGreaterThan(0);
  });

  test("BUD-4: throws BudgetExhaustedError when exceeded", async () => {
    const budget = createBudgetTracker({ maxEvalSteps: 5 });
    const infiniteLoop = compileTextToExpr("((lambda (x) (x x)) (lambda (x) (x x)))");

    await expect(runToCompletion(runtime, makeState(infiniteLoop), { budget }))
      .rejects.toThrow(BudgetExhaustedError);
  });

  test("BUD-5: remaining is accurate", () => {
    const budget = createBudgetTracker({ maxEvalSteps: 100 });
    budget.consumeEvalStep();
    budget.consumeEvalStep();
    expect(budget.remaining().maxEvalSteps).toBe(98);
  });

  test("BUD-7: default is unlimited", async () => {
    const expr = compileTextToExpr("(+ 1 2 3 4 5)");
    // Should not throw even with many steps
    const result = await runToCompletion(runtime, makeState(expr));
    expect(result).toEqual({ tag: "Num", n: 15 });
  });
});
```

---

## 2. ReqTool Implementation

### 2.1 Problem Statement

`ReqTool` in `portalImpl.ts` returns an error stub: `"tool subsystem not wired yet"`. Oracle sessions cannot execute tools.

### 2.2 Requirements

| ID | Requirement |
|----|-------------|
| TOOL-1 | ReqTool MUST look up tool by name in registry |
| TOOL-2 | ReqTool MUST check capability `tool.<name>` before execution |
| TOOL-3 | ReqTool MUST consume tool call from budget |
| TOOL-4 | ReqTool MUST return tool result as RespVal |
| TOOL-5 | ReqTool MUST return RespError if tool not found |
| TOOL-6 | ReqTool MUST return RespError if tool throws |
| TOOL-7 | ReqTool MUST support async tool execution |
| TOOL-8 | Tool args MUST be passed as Val (converted from QExpr if needed) |

### 2.3 Interface Definitions

```typescript
// src/core/oracle/protocol.ts (existing, verify)

export interface ReqTool {
  tag: "ReqTool";
  toolName: string;
  args: Val;        // Tool arguments as a Val (typically Vector or Map)
  envRef: string;   // Context for tool execution
}
```

```typescript
// src/core/tool/types.ts (new file)

export interface Tool {
  /** Unique tool name */
  name: string;

  /** Human-readable description */
  description: string;

  /** JSON Schema for input validation (optional) */
  inputSchema?: object;

  /** Required capabilities to use this tool */
  requiredCaps?: string[];

  /** Execute the tool */
  call(args: Val, context: ToolContext): Promise<Val>;
}

export interface ToolContext {
  envRef: string;
  budget: BudgetTracker;
  profile: Profile;
}

export interface ToolRegistry {
  register(tool: Tool): void;
  get(name: string): Tool | undefined;
  list(): Tool[];
  has(name: string): boolean;
}
```

### 2.4 Implementation Guidance

**File: `src/core/oracle/portalImpl.ts`**

```typescript
// In handleRequest method, replace the ReqTool stub:

if (req.tag === "ReqTool") {
  const { toolName, args, envRef } = req;

  // TOOL-1: Look up tool
  const tool = this.toolRegistry.get(toolName);
  if (!tool) {
    // TOOL-5: Not found
    return { tag: "RespError", message: `unknown tool: ${toolName}` };
  }

  // TOOL-2: Check capability
  const requiredCap = `tool.${toolName}`;
  if (!capHas(this.profile.caps, requiredCap)) {
    return { tag: "RespError", message: `capability denied: ${requiredCap}` };
  }

  // TOOL-3: Consume budget
  try {
    this.budget.consumeToolCall();
  } catch (e) {
    return { tag: "RespError", message: (e as Error).message };
  }

  // TOOL-7: Execute async
  try {
    const context: ToolContext = {
      envRef,
      budget: this.budget,
      profile: this.profile,
    };

    // TOOL-4: Return result
    const result = await tool.call(args, context);
    return { tag: "RespVal", value: result, envRef };

  } catch (e) {
    // TOOL-6: Handle errors
    return { tag: "RespError", message: `tool error: ${(e as Error).message}` };
  }
}
```

### 2.5 Test Specifications

```typescript
// test/oracle/reqTool.spec.ts

describe("ReqTool", () => {
  let registry: ToolRegistry;
  let portal: PortalImpl;

  beforeEach(() => {
    registry = createToolRegistry();
    registry.register({
      name: "echo",
      description: "Returns its input",
      call: async (args) => args,
    });
    portal = new PortalImpl(runtime, snapshots, receipts, registry, budget, profile);
  });

  test("TOOL-1: looks up tool by name", async () => {
    const resp = await portal.handleRequest({
      tag: "ReqTool",
      toolName: "echo",
      args: { tag: "Str", s: "hello" },
      envRef: "E0",
    });
    expect(resp.tag).toBe("RespVal");
    expect((resp as any).value.s).toBe("hello");
  });

  test("TOOL-2: checks capability", async () => {
    const restrictedProfile = { ...profile, caps: [] };  // No caps
    const restrictedPortal = new PortalImpl(..., restrictedProfile);

    const resp = await restrictedPortal.handleRequest({
      tag: "ReqTool",
      toolName: "echo",
      args: { tag: "Unit" },
      envRef: "E0",
    });

    expect(resp.tag).toBe("RespError");
    expect((resp as any).message).toContain("capability denied");
  });

  test("TOOL-5: returns error for unknown tool", async () => {
    const resp = await portal.handleRequest({
      tag: "ReqTool",
      toolName: "nonexistent",
      args: { tag: "Unit" },
      envRef: "E0",
    });

    expect(resp.tag).toBe("RespError");
    expect((resp as any).message).toContain("unknown tool");
  });
});
```

---

## 3. Tool Registry

### 3.1 Problem Statement

No tool registry exists. ReqTool needs a way to look up and execute tools.

### 3.2 Requirements

| ID | Requirement |
|----|-------------|
| REG-1 | Registry MUST support registering tools by name |
| REG-2 | Registry MUST support looking up tools by name |
| REG-3 | Registry MUST support listing all tools |
| REG-4 | Registry MUST prevent duplicate registration (throw or overwrite) |
| REG-5 | Registry MUST support built-in tools (bash, read_file, write_file) |
| REG-6 | Registry MUST support MCP tool loading |
| REG-7 | Registry MUST be injectable into PortalImpl |

### 3.3 Interface Definitions

```typescript
// src/core/tool/registry.ts

export interface ToolRegistry {
  /** Register a tool. Throws if name already exists. */
  register(tool: Tool): void;

  /** Get tool by name, undefined if not found */
  get(name: string): Tool | undefined;

  /** Check if tool exists */
  has(name: string): boolean;

  /** List all registered tools */
  list(): Tool[];

  /** List tool names */
  names(): string[];

  /** Unregister a tool */
  unregister(name: string): boolean;
}

export function createToolRegistry(): ToolRegistry;
```

```typescript
// src/core/tool/builtins.ts

/** Create registry with built-in tools */
export function createBuiltinRegistry(): ToolRegistry;

/** Built-in: Execute bash command */
export const bashTool: Tool;

/** Built-in: Read file contents */
export const readFileTool: Tool;

/** Built-in: Write file contents */
export const writeFileTool: Tool;

/** Built-in: List directory */
export const listDirTool: Tool;

/** Built-in: Search files (grep) */
export const searchTool: Tool;
```

### 3.4 Implementation Guidance

**File: `src/core/tool/registry.ts`**

```typescript
export function createToolRegistry(): ToolRegistry {
  const tools = new Map<string, Tool>();

  return {
    register(tool: Tool): void {
      if (tools.has(tool.name)) {
        throw new Error(`Tool already registered: ${tool.name}`);
      }
      tools.set(tool.name, tool);
    },

    get(name: string): Tool | undefined {
      return tools.get(name);
    },

    has(name: string): boolean {
      return tools.has(name);
    },

    list(): Tool[] {
      return Array.from(tools.values());
    },

    names(): string[] {
      return Array.from(tools.keys());
    },

    unregister(name: string): boolean {
      return tools.delete(name);
    },
  };
}
```

**File: `src/core/tool/builtins.ts`**

```typescript
import { exec } from "child_process";
import { promisify } from "util";
import * as fs from "fs/promises";
import * as path from "path";

const execAsync = promisify(exec);

export const bashTool: Tool = {
  name: "bash",
  description: "Execute a bash command and return stdout",
  requiredCaps: ["tool.bash"],

  async call(args: Val, ctx: ToolContext): Promise<Val> {
    if (args.tag !== "Map" && args.tag !== "Str") {
      throw new Error("bash expects Map with 'command' or Str");
    }

    let command: string;
    if (args.tag === "Str") {
      command = args.s;
    } else {
      const cmdEntry = args.entries.find(([k]) => k.tag === "Str" && k.s === "command");
      if (!cmdEntry || cmdEntry[1].tag !== "Str") {
        throw new Error("bash expects 'command' string");
      }
      command = cmdEntry[1].s;
    }

    const { stdout, stderr } = await execAsync(command, {
      timeout: 30000,
      maxBuffer: 1024 * 1024,
    });

    return {
      tag: "Map",
      entries: [
        [{ tag: "Str", s: "stdout" }, { tag: "Str", s: stdout }],
        [{ tag: "Str", s: "stderr" }, { tag: "Str", s: stderr }],
      ],
    };
  },
};

export const readFileTool: Tool = {
  name: "read_file",
  description: "Read file contents",
  requiredCaps: ["tool.read_file"],

  async call(args: Val, ctx: ToolContext): Promise<Val> {
    let filePath: string;

    if (args.tag === "Str") {
      filePath = args.s;
    } else if (args.tag === "Map") {
      const pathEntry = args.entries.find(([k]) => k.tag === "Str" && k.s === "path");
      if (!pathEntry || pathEntry[1].tag !== "Str") {
        throw new Error("read_file expects 'path' string");
      }
      filePath = pathEntry[1].s;
    } else {
      throw new Error("read_file expects path string or Map with 'path'");
    }

    const content = await fs.readFile(filePath, "utf-8");
    return { tag: "Str", s: content };
  },
};

export const writeFileTool: Tool = {
  name: "write_file",
  description: "Write content to file",
  requiredCaps: ["tool.write_file"],

  async call(args: Val, ctx: ToolContext): Promise<Val> {
    if (args.tag !== "Map") {
      throw new Error("write_file expects Map with 'path' and 'content'");
    }

    const pathEntry = args.entries.find(([k]) => k.tag === "Str" && k.s === "path");
    const contentEntry = args.entries.find(([k]) => k.tag === "Str" && k.s === "content");

    if (!pathEntry || pathEntry[1].tag !== "Str") {
      throw new Error("write_file expects 'path' string");
    }
    if (!contentEntry || contentEntry[1].tag !== "Str") {
      throw new Error("write_file expects 'content' string");
    }

    await fs.writeFile(pathEntry[1].s, contentEntry[1].s, "utf-8");
    return { tag: "Unit" };
  },
};

export function createBuiltinRegistry(): ToolRegistry {
  const registry = createToolRegistry();
  registry.register(bashTool);
  registry.register(readFileTool);
  registry.register(writeFileTool);
  return registry;
}
```

### 3.5 Test Specifications

```typescript
// test/core/tool/registry.spec.ts

describe("ToolRegistry", () => {
  test("REG-1: registers tools", () => {
    const reg = createToolRegistry();
    reg.register({ name: "test", description: "test", call: async () => ({ tag: "Unit" }) });
    expect(reg.has("test")).toBe(true);
  });

  test("REG-4: prevents duplicate registration", () => {
    const reg = createToolRegistry();
    const tool = { name: "dup", description: "test", call: async () => ({ tag: "Unit" }) };
    reg.register(tool);
    expect(() => reg.register(tool)).toThrow(/already registered/);
  });

  test("REG-5: builtin registry has bash", () => {
    const reg = createBuiltinRegistry();
    expect(reg.has("bash")).toBe(true);
    expect(reg.has("read_file")).toBe(true);
    expect(reg.has("write_file")).toBe(true);
  });
});
```

---

## 4. Real LLM Adapter

### 4.1 Problem Statement

Only `ScriptedOracleAdapter` works. Anthropic and MCP adapters are stubs. Cannot use real LLMs.

### 4.2 Requirements

| ID | Requirement |
|----|-------------|
| LLM-1 | Adapter MUST implement OracleAdapter interface |
| LLM-2 | Adapter MUST convert oracle protocol to LLM API calls |
| LLM-3 | Adapter MUST parse LLM output as s-expression commands |
| LLM-4 | Adapter MUST handle ReqReturn to terminate session |
| LLM-5 | Adapter MUST respect maxTurns limit |
| LLM-6 | Adapter MUST support system prompt configuration |
| LLM-7 | Adapter MUST handle LLM errors gracefully |
| LLM-8 | Adapter MUST support multiple LLM providers (OpenAI, Anthropic, Ollama) |
| LLM-9 | Adapter MUST log transcript for debugging |

### 4.3 Interface Definitions

```typescript
// src/core/oracle/adapters/llmAdapter.ts

export interface LLMConfig {
  provider: "openai" | "anthropic" | "ollama";
  model: string;
  apiKey?: string;
  baseUrl?: string;
  maxTurns?: number;
  temperature?: number;
  systemPrompt?: string;
}

export interface LLMAdapterOptions {
  config: LLMConfig;
  verbose?: boolean;
  transcriptCallback?: (entry: TranscriptEntry) => void;
}

export interface TranscriptEntry {
  role: "oracle" | "runtime";
  content: string;
  timestamp: number;
}

export class LLMOracleAdapter implements OracleAdapter {
  constructor(options: LLMAdapterOptions);

  startSession(init: OracleInit): OracleSession;

  /** Get full transcript of last session */
  getTranscript(): TranscriptEntry[];
}
```

### 4.4 Implementation Guidance

**File: `src/core/oracle/adapters/llmAdapter.ts`**

```typescript
const DEFAULT_SYSTEM_PROMPT = `You are an Oracle for the Omega Lisp evaluator.
You communicate by emitting s-expression commands. Available commands:

(req-eval '(expr) envRef=E0)     ; evaluate expression
(req-test '((= (f 0) 1)) envRef=E0)  ; run tests
(req-snapshot E0)                 ; save state
(req-hydrate R0)                  ; restore state
(req-observe what=control)        ; observe state
(req-return (meaning ...))        ; return result

Rules:
- Emit ONE command per turn
- Wait for runtime response before next command
- When tests fail, use req-hydrate to rollback
- When done, emit req-return with a meaning

Example:
oracle> (req-eval '(define add1 (lambda (x) (+ x 1))) envRef=E0)
runtime> (resp-val unit envRef=E1)
oracle> (req-return (meaning (rewrite (define add1 ...)) (confidence 0.99)))
`;

export class LLMOracleAdapter implements OracleAdapter {
  private config: LLMConfig;
  private verbose: boolean;
  private transcript: TranscriptEntry[] = [];
  private transcriptCallback?: (entry: TranscriptEntry) => void;

  constructor(options: LLMAdapterOptions) {
    this.config = options.config;
    this.verbose = options.verbose ?? false;
    this.transcriptCallback = options.transcriptCallback;
  }

  getTranscript(): TranscriptEntry[] {
    return [...this.transcript];
  }

  startSession(init: OracleInit): OracleSession {
    const self = this;
    self.transcript = [];

    return (async function* (): OracleSession {
      if (init.tag !== "Infer") {
        return meaning({ denotation: { tag: "Unit" }, confidence: 0 });
      }

      const messages: Array<{ role: string; content: string }> = [
        { role: "system", content: self.config.systemPrompt ?? DEFAULT_SYSTEM_PROMPT },
        { role: "user", content: self.formatInitMessage(init) },
      ];

      let envRef = init.envRef;
      const maxTurns = self.config.maxTurns ?? 20;

      for (let turn = 0; turn < maxTurns; turn++) {
        // Call LLM
        const llmOutput = await self.callLLM(messages);
        messages.push({ role: "assistant", content: llmOutput });
        self.logTranscript("oracle", llmOutput);

        // Parse s-expression command
        const sexprStr = self.extractSExpr(llmOutput);
        if (!sexprStr) {
          messages.push({ role: "user", content: "Please emit a valid command." });
          continue;
        }

        // Parse to OracleReq
        const req = self.parseCommand(sexprStr, envRef);
        if (!req) {
          messages.push({ role: "user", content: "Command not recognized." });
          continue;
        }

        // Handle req-return
        if (req.tag === "ReqReturn") {
          return req.result;
        }

        // Yield request, get response
        const resp: OracleResp = yield req;

        // Update envRef
        if (resp.tag === "RespVal" && resp.envRef) {
          envRef = resp.envRef;
        }

        // Format response for LLM
        const respStr = self.formatResponse(resp);
        self.logTranscript("runtime", respStr);
        messages.push({ role: "user", content: `Runtime response:\n${respStr}` });
      }

      // Max turns exceeded
      return meaning({ denotation: { tag: "Unit" }, confidence: 0 });
    })();
  }

  private async callLLM(messages: Array<{ role: string; content: string }>): Promise<string> {
    switch (this.config.provider) {
      case "openai":
        return this.callOpenAI(messages);
      case "anthropic":
        return this.callAnthropic(messages);
      case "ollama":
        return this.callOllama(messages);
      default:
        throw new Error(`Unknown provider: ${this.config.provider}`);
    }
  }

  private async callOpenAI(messages: Array<{ role: string; content: string }>): Promise<string> {
    const response = await fetch("https://api.openai.com/v1/chat/completions", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        "Authorization": `Bearer ${this.config.apiKey}`,
      },
      body: JSON.stringify({
        model: this.config.model,
        messages,
        max_tokens: 1000,
        temperature: this.config.temperature ?? 0.3,
      }),
    });

    const data = await response.json() as any;
    if (data.error) throw new Error(data.error.message);
    return data.choices?.[0]?.message?.content ?? "";
  }

  private async callAnthropic(messages: Array<{ role: string; content: string }>): Promise<string> {
    const systemMsg = messages.find(m => m.role === "system")?.content ?? "";
    const userMsgs = messages.filter(m => m.role !== "system");

    const response = await fetch("https://api.anthropic.com/v1/messages", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        "x-api-key": this.config.apiKey!,
        "anthropic-version": "2023-06-01",
      },
      body: JSON.stringify({
        model: this.config.model,
        max_tokens: 1000,
        system: systemMsg,
        messages: userMsgs.map(m => ({
          role: m.role === "assistant" ? "assistant" : "user",
          content: m.content,
        })),
      }),
    });

    const data = await response.json() as any;
    if (data.error) throw new Error(data.error.message);
    return data.content?.[0]?.text ?? "";
  }

  private async callOllama(messages: Array<{ role: string; content: string }>): Promise<string> {
    const baseUrl = this.config.baseUrl ?? "http://localhost:11434";

    const response = await fetch(`${baseUrl}/api/chat`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        model: this.config.model,
        messages,
        stream: false,
      }),
    });

    const data = await response.json() as any;
    return data.message?.content ?? "";
  }

  // ... helper methods for parsing/formatting
}
```

### 4.5 Test Specifications

```typescript
// test/oracle/llmAdapter.spec.ts

describe("LLMOracleAdapter", () => {
  // Use mock fetch for testing
  const mockFetch = vi.fn();

  beforeEach(() => {
    global.fetch = mockFetch;
  });

  test("LLM-1: implements OracleAdapter", () => {
    const adapter = new LLMOracleAdapter({
      config: { provider: "openai", model: "gpt-4", apiKey: "test" },
    });
    expect(adapter.startSession).toBeDefined();
  });

  test("LLM-3: parses s-expression from LLM output", async () => {
    mockFetch.mockResolvedValueOnce({
      json: () => Promise.resolve({
        choices: [{ message: { content: "(req-eval '(+ 1 2) envRef=E0)" } }],
      }),
    });

    const adapter = new LLMOracleAdapter({
      config: { provider: "openai", model: "gpt-4", apiKey: "test" },
    });

    const session = adapter.startSession({
      tag: "Infer",
      payload: { tag: "Str", s: "test" },
      envRef: "E0",
      stateRef: "S0",
    });

    const step = await session.next({ tag: "RespAck" });
    expect(step.done).toBe(false);
    expect((step.value as any).tag).toBe("ReqEval");
  });
});
```

---

## 5. Nondet Best/Sample Modes

### 5.1 Problem Statement

`runNondet` only implements "first" and "all" frontier modes. "best" and "sample" modes are defined in types but not implemented.

### 5.2 Requirements

| ID | Requirement |
|----|-------------|
| ND-1 | "first" mode MUST return first successful result |
| ND-2 | "all" mode MUST return all successful results |
| ND-3 | "best" mode MUST score results and return highest scoring |
| ND-4 | "sample" mode MUST randomly sample from results by weight |
| ND-5 | Scoring function MUST be configurable |
| ND-6 | Default scoring MUST use confidence field of MeaningVal |
| ND-7 | Sample weights MUST be derived from scores |

### 5.3 Interface Definitions

```typescript
// src/core/effects/nondet/types.ts (additions)

export type ScoringFn = (result: Val) => number;

export interface NondetPolicy {
  frontier: "first" | "all" | "best" | "sample";
  maxBranches?: number;
  scoringFn?: ScoringFn;
  seed?: number;  // For deterministic sampling
}

/** Default scoring: use confidence if MeaningVal, else 1.0 */
export function defaultScoring(v: Val): number {
  if (v.tag === "Meaning" && typeof v.confidence === "number") {
    return v.confidence;
  }
  return 1.0;
}
```

### 5.4 Implementation Guidance

**File: `src/core/effects/nondet/runner.ts`**

```typescript
export async function runNondet(
  runtime: Runtime,
  state0: State,
  policy: NondetPolicy,
  budget?: BudgetTracker,
): Promise<Val[]> {
  const results: Array<{ value: Val; score: number }> = [];
  const scoringFn = policy.scoringFn ?? defaultScoring;

  // ... existing BFS/DFS logic to collect results ...

  // After collecting results:
  switch (policy.frontier) {
    case "first":
      return results.length > 0 ? [results[0].value] : [];

    case "all":
      return results.map(r => r.value);

    case "best":
      if (results.length === 0) return [];
      // Score all results
      const scored = results.map(r => ({ ...r, score: scoringFn(r.value) }));
      // Sort by score descending
      scored.sort((a, b) => b.score - a.score);
      return [scored[0].value];

    case "sample":
      if (results.length === 0) return [];
      // Score all results
      const withScores = results.map(r => ({ ...r, score: scoringFn(r.value) }));
      // Normalize to weights
      const totalScore = withScores.reduce((sum, r) => sum + r.score, 0);
      if (totalScore === 0) {
        // Uniform sampling if all scores are 0
        const idx = Math.floor(Math.random() * withScores.length);
        return [withScores[idx].value];
      }
      // Weighted random sample
      let rand = Math.random() * totalScore;
      for (const r of withScores) {
        rand -= r.score;
        if (rand <= 0) return [r.value];
      }
      return [withScores[withScores.length - 1].value];

    default:
      throw new Error(`Unknown frontier mode: ${policy.frontier}`);
  }
}
```

### 5.5 Test Specifications

```typescript
// test/nondet/modes.spec.ts

describe("Nondet Modes", () => {
  test("ND-3: best returns highest scoring", async () => {
    // Setup: amb.op returns multiple MeaningVals with different confidence
    const results = await runNondet(runtime, state, {
      frontier: "best",
      scoringFn: (v) => v.tag === "Meaning" ? v.confidence ?? 0 : 0,
    });

    expect(results.length).toBe(1);
    expect(results[0].confidence).toBe(0.99);  // Highest
  });

  test("ND-4: sample respects weights", async () => {
    // Statistical test: run many times, check distribution
    const counts = { high: 0, low: 0 };

    for (let i = 0; i < 100; i++) {
      const results = await runNondet(runtime, state, { frontier: "sample" });
      if (results[0].confidence > 0.5) counts.high++;
      else counts.low++;
    }

    // High confidence should be sampled more often
    expect(counts.high).toBeGreaterThan(counts.low);
  });
});
```

---

## 6. Context Constraints

### 6.1 Problem Statement

`ConstraintObs` types are defined but never checked during evaluation. Constraints like `NoNewFacts`, `DeterministicEnvelope`, `Sealed` are not enforced.

### 6.2 Requirements

| ID | Requirement |
|----|-------------|
| CTX-1 | Sealed context MUST prevent define and set! |
| CTX-2 | NoNewFacts constraint MUST prevent fact assertions |
| CTX-3 | DeterministicEnvelope MUST require receipt-backed operations |
| CTX-4 | Constraints MUST be checkable at context boundaries |
| CTX-5 | Constraint violations MUST throw ConstraintViolationError |

### 6.3 Interface Definitions

```typescript
// src/core/ctx/constraints.ts (new file)

export type Constraint =
  | { tag: "Sealed" }
  | { tag: "NoNewFacts" }
  | { tag: "DeterministicEnvelope"; receiptRef: string }
  | { tag: "ReadOnly" }
  | { tag: "MaxDepth"; depth: number };

export class ConstraintViolationError extends Error {
  constructor(
    public readonly constraint: Constraint,
    public readonly operation: string,
  ) {
    super(`Constraint violation: ${constraint.tag} denied ${operation}`);
    this.name = "ConstraintViolationError";
  }
}

export function checkConstraint(
  ctx: Ctx,
  operation: string,
  constraints: Constraint[],
): void;
```

### 6.4 Implementation Guidance

**File: `src/core/eval/machineStep.ts`**

```typescript
// In KDefine handling:
case "KDefine": {
  // Check sealed constraint
  if (fr.env.sealed) {
    throw new ConstraintViolationError({ tag: "Sealed" }, "define");
  }
  // ... rest of define logic
}

// In KSet handling:
case "KSet": {
  // Already checks sealed (line 252-254)
  if (fr.env.sealed) {
    throw new Error(`set! denied: context is sealed (name=${fr.name})`);
  }
  // ... rest of set logic
}
```

### 6.5 Test Specifications

```typescript
// test/core/ctx/constraints.spec.ts

describe("Context Constraints", () => {
  test("CTX-1: sealed prevents define", async () => {
    const sealedEnv = { ...env, sealed: true };
    const expr = compileTextToExpr("(define x 42)");

    await expect(runToCompletion(runtime, { ...state, env: sealedEnv }))
      .rejects.toThrow(/sealed/);
  });

  test("CTX-1: sealed prevents set!", async () => {
    // First define x, then seal, then try set!
    // ...
  });
});
```

---

## 7. Evidence Accumulation

### 7.1 Problem Statement

`ctxAddEvidence` exists but is never called. Evidence chains are not built.

### 7.2 Requirements

| ID | Requirement |
|----|-------------|
| EV-1 | Evidence MUST be added for oracle session results |
| EV-2 | Evidence MUST be added for test results |
| EV-3 | Evidence MUST be retrievable from context |
| EV-4 | Evidence MUST include provenance (source, timestamp) |
| EV-5 | Evidence chain MUST be exportable |

### 7.3 Interface Definitions

```typescript
// src/core/ctx/evidence.ts (new file)

export interface Evidence {
  id: string;
  type: "oracle" | "test" | "tool" | "assertion";
  source: string;
  timestamp: number;
  data: Val;
  parentIds?: string[];
}

export interface EvidenceChain {
  add(evidence: Evidence): void;
  get(id: string): Evidence | undefined;
  getByType(type: Evidence["type"]): Evidence[];
  export(): Evidence[];
}

export function createEvidenceChain(): EvidenceChain;
```

### 7.4 Implementation Guidance

**File: `src/core/oracle/portalImpl.ts`**

```typescript
// After ReqTest succeeds:
if (req.tag === "ReqTest") {
  // ... run tests ...

  if (allPassed) {
    this.evidenceChain.add({
      id: generateId(),
      type: "test",
      source: req.envRef,
      timestamp: Date.now(),
      data: { tag: "Str", s: "all tests passed" },
    });
  }

  return resp;
}

// After oracle session completes:
// In runOracleSession:
const meaning = await runOracleSession(session, portal);

evidenceChain.add({
  id: generateId(),
  type: "oracle",
  source: init.envRef,
  timestamp: Date.now(),
  data: meaning,
});
```

### 7.5 Test Specifications

```typescript
// test/core/ctx/evidence.spec.ts

describe("Evidence Accumulation", () => {
  test("EV-1: oracle results add evidence", async () => {
    const chain = createEvidenceChain();
    // Run oracle session
    // Check chain has oracle evidence
    expect(chain.getByType("oracle").length).toBe(1);
  });
});
```

---

## 8. REPL Debugger Commands

### 8.1 Problem Statement

REPL lacks debugger commands for inspecting machine state (`:stack`, `:frame`, `:env`).

### 8.2 Requirements

| ID | Requirement |
|----|-------------|
| DBG-1 | `:stack` MUST show all continuation frames |
| DBG-2 | `:frame N` MUST show details of frame N |
| DBG-3 | `:env` MUST show current environment bindings |
| DBG-4 | `:env N` MUST show bindings for frame N |
| DBG-5 | `:control` MUST show current control expression |
| DBG-6 | `:handlers` MUST show installed effect handlers |
| DBG-7 | `:store` MUST show store contents (with size limit) |

### 8.3 Interface Definitions

```typescript
// src/packages/repl/commands.ts

export interface ReplContext {
  state: State | null;
  runtime: Runtime;
  snapshots: SnapshotRepo;
  receipts: ReceiptStore;
}

export interface CommandResult {
  output: string;
  newState?: State;
}

export type CommandHandler = (args: string, ctx: ReplContext) => CommandResult | Promise<CommandResult>;

export const commands: Record<string, CommandHandler> = {
  stack: handleStack,
  frame: handleFrame,
  env: handleEnv,
  control: handleControl,
  handlers: handleHandlers,
  store: handleStore,
};

export function handleStack(args: string, ctx: ReplContext): CommandResult;
export function handleFrame(args: string, ctx: ReplContext): CommandResult;
export function handleEnv(args: string, ctx: ReplContext): CommandResult;
export function handleControl(args: string, ctx: ReplContext): CommandResult;
export function handleHandlers(args: string, ctx: ReplContext): CommandResult;
export function handleStore(args: string, ctx: ReplContext): CommandResult;
```

### 8.4 Implementation Guidance

**File: `src/packages/repl/commands.ts`**

```typescript
export function handleStack(args: string, ctx: ReplContext): CommandResult {
  if (!ctx.state) {
    return { output: "(no active state)" };
  }

  const { kont } = ctx.state;
  if (kont.length === 0) {
    return { output: "(empty stack)" };
  }

  const lines: string[] = ["Stack frames (top to bottom):"];

  for (let i = kont.length - 1; i >= 0; i--) {
    const frame = kont[i];
    lines.push(`  ${i}: [${frame.tag}] ${frameDescription(frame)}`);
  }

  return { output: lines.join("\n") };
}

function frameDescription(frame: Frame): string {
  switch (frame.tag) {
    case "KIf": return "waiting for test result";
    case "KBegin": return `${frame.rest.length} exprs remaining`;
    case "KDefine": return `defining ${frame.name}`;
    case "KSet": return `setting ${frame.name}`;
    case "KAppFun": return `${frame.args.length} args to evaluate`;
    case "KAppArg": return `${frame.pending.length} args pending`;
    case "KCall": return "returning from call";
    case "KEffect": return `effect ${frame.op}`;
    case "KHandleBoundary": return `handler boundary ${frame.hid}`;
    case "KHandleReturn": return "handler return";
    case "KMatch": return `${frame.clauses.length} clauses`;
    default: return "";
  }
}

export function handleFrame(args: string, ctx: ReplContext): CommandResult {
  if (!ctx.state) {
    return { output: "(no active state)" };
  }

  const idx = parseInt(args.trim(), 10);
  if (isNaN(idx) || idx < 0 || idx >= ctx.state.kont.length) {
    return { output: `Invalid frame index: ${args}` };
  }

  const frame = ctx.state.kont[idx];
  const lines: string[] = [
    `Frame ${idx}: ${frame.tag}`,
    `  Details: ${JSON.stringify(frame, null, 2)}`,
  ];

  return { output: lines.join("\n") };
}

export function handleEnv(args: string, ctx: ReplContext): CommandResult {
  if (!ctx.state) {
    return { output: "(no active state)" };
  }

  const { env, store } = ctx.state;
  const lines: string[] = ["Environment bindings:"];

  // Walk through env bindings
  for (const [name, addr] of env.bindings.entries()) {
    const val = store.read(addr);
    lines.push(`  ${name}: ${valToShortString(val)}`);
  }

  return { output: lines.join("\n") };
}

function valToShortString(v: Val): string {
  switch (v.tag) {
    case "Num": return String(v.n);
    case "Str": return `"${v.s.slice(0, 20)}${v.s.length > 20 ? "..." : ""}"`;
    case "Bool": return v.b ? "#t" : "#f";
    case "Unit": return "()";
    case "Closure": return `<closure (${v.params.join(" ")})>`;
    case "Native": return `<prim ${v.name}>`;
    case "Cont": return "<continuation>";
    default: return `<${v.tag}>`;
  }
}

export function handleControl(args: string, ctx: ReplContext): CommandResult {
  if (!ctx.state) {
    return { output: "(no active state)" };
  }

  const { control } = ctx.state;

  if (control.tag === "Val") {
    return { output: `Value: ${valToShortString(control.v)}` };
  } else {
    return { output: `Expr: ${exprToString(control.e)}` };
  }
}

export function handleHandlers(args: string, ctx: ReplContext): CommandResult {
  if (!ctx.state) {
    return { output: "(no active state)" };
  }

  const { handlers } = ctx.state;
  if (handlers.length === 0) {
    return { output: "(no handlers installed)" };
  }

  const lines: string[] = ["Installed handlers:"];

  for (let i = handlers.length - 1; i >= 0; i--) {
    const h = handlers[i];
    const ops = Array.from(h.on.keys()).join(", ");
    lines.push(`  ${i}: [${h.hid}] handles: ${ops}`);
  }

  return { output: lines.join("\n") };
}
```

### 8.5 Test Specifications

```typescript
// test/packages/repl/commands.spec.ts

describe("REPL Debugger Commands", () => {
  test("DBG-1: :stack shows frames", () => {
    const state = makeStateWithStack([
      { tag: "KIf", conseq: ..., alt: ..., env: ... },
      { tag: "KCall", savedEnv: ... },
    ]);

    const result = handleStack("", { state, ... });

    expect(result.output).toContain("KIf");
    expect(result.output).toContain("KCall");
  });

  test("DBG-3: :env shows bindings", () => {
    const result = handleEnv("", contextWithBindings({ x: 42, y: "hello" }));

    expect(result.output).toContain("x: 42");
    expect(result.output).toContain('y: "hello"');
  });
});
```

---

## 9. REPL Pause/Resume/Step

### 9.1 Problem Statement

REPL cannot pause execution at effect boundaries or step through evaluation.

### 9.2 Requirements

| ID | Requirement |
|----|-------------|
| STEP-1 | `:pause` MUST pause at next effect op |
| STEP-2 | `:resume` MUST continue from pause |
| STEP-3 | `:step` MUST execute one frame |
| STEP-4 | `:break OP` MUST set breakpoint on effect OP |
| STEP-5 | `:clear` MUST clear all breakpoints |
| STEP-6 | Paused state MUST be inspectable with debugger commands |
| STEP-7 | Paused state MUST allow eval in current env |

### 9.3 Interface Definitions

```typescript
// src/packages/repl/debugger.ts

export interface Debugger {
  /** Set pause on next effect */
  setPauseOnEffect(): void;

  /** Set breakpoint on specific effect op */
  setBreakpoint(op: string): void;

  /** Clear all breakpoints */
  clearBreakpoints(): void;

  /** Check if should pause at this op */
  shouldPause(op: string): boolean;

  /** Get current paused state */
  getPausedState(): State | null;

  /** Resume execution */
  resume(): Promise<Val>;

  /** Step one frame */
  step(): Promise<StepResult>;
}

export interface StepResult {
  state: State;
  outcome: "continued" | "done" | "effect";
  effectOp?: string;
}

export function createDebugger(
  runtime: Runtime,
  initialState: State,
): Debugger;
```

### 9.4 Implementation Guidance

**File: `src/packages/repl/debugger.ts`**

```typescript
export function createDebugger(runtime: Runtime, initialState: State): Debugger {
  let pauseOnNext = false;
  const breakpoints = new Set<string>();
  let pausedState: State | null = null;
  let pausedOp: string | null = null;

  return {
    setPauseOnEffect() {
      pauseOnNext = true;
    },

    setBreakpoint(op: string) {
      breakpoints.add(op);
    },

    clearBreakpoints() {
      breakpoints.clear();
      pauseOnNext = false;
    },

    shouldPause(op: string): boolean {
      if (pauseOnNext) {
        pauseOnNext = false;
        return true;
      }
      return breakpoints.has(op);
    },

    getPausedState(): State | null {
      return pausedState;
    },

    async resume(): Promise<Val> {
      if (!pausedState) {
        throw new Error("Not paused");
      }

      // Continue from paused state
      const result = await runToCompletionWithDebugger(
        runtime,
        pausedState,
        this,
      );

      pausedState = null;
      pausedOp = null;
      return result;
    },

    async step(): Promise<StepResult> {
      const st = pausedState ?? initialState;

      const outcome = stepOnce(st);

      if (outcome.tag === "Done") {
        pausedState = null;
        return { state: outcome.state, outcome: "done" };
      }

      if (outcome.tag === "Op") {
        pausedState = outcome.state;
        pausedOp = outcome.opcall.op;
        return {
          state: outcome.state,
          outcome: "effect",
          effectOp: outcome.opcall.op,
        };
      }

      pausedState = outcome.state;
      return { state: outcome.state, outcome: "continued" };
    },
  };
}

async function runToCompletionWithDebugger(
  runtime: Runtime,
  state0: State,
  debugger_: Debugger,
): Promise<Val> {
  let st = state0;

  while (true) {
    const outcome = stepOnce(st);

    if (outcome.tag === "Done") {
      return outcome.value;
    }

    if (outcome.tag === "Op") {
      // Check breakpoint
      if (debugger_.shouldPause(outcome.opcall.op)) {
        // Store state and return control to REPL
        throw new PausedAtEffectError(outcome.state, outcome.opcall.op);
      }

      const dispatchResult = await runtime.dispatch(outcome.state, outcome.opcall);
      if (dispatchResult === "Uncaught") {
        throw new Error(`Uncaught effect: ${outcome.opcall.op}`);
      }
      st = dispatchResult;
      continue;
    }

    st = outcome.state;
  }
}

export class PausedAtEffectError extends Error {
  constructor(
    public readonly state: State,
    public readonly op: string,
  ) {
    super(`Paused at effect: ${op}`);
    this.name = "PausedAtEffectError";
  }
}
```

### 9.5 Test Specifications

```typescript
// test/packages/repl/debugger.spec.ts

describe("REPL Debugger", () => {
  test("STEP-1: :pause stops at next effect", async () => {
    const dbg = createDebugger(runtime, state);
    dbg.setPauseOnEffect();

    await expect(dbg.resume()).rejects.toThrow(PausedAtEffectError);
    expect(dbg.getPausedState()).not.toBeNull();
  });

  test("STEP-3: :step advances one frame", async () => {
    const dbg = createDebugger(runtime, state);

    const result1 = await dbg.step();
    expect(result1.outcome).toBe("continued");

    const result2 = await dbg.step();
    // State changed
    expect(result2.state).not.toEqual(result1.state);
  });

  test("STEP-4: :break OP triggers on specific op", async () => {
    const dbg = createDebugger(runtime, stateWithEffect);
    dbg.setBreakpoint("my.op");

    await expect(dbg.resume()).rejects.toThrow(PausedAtEffectError);
    expect((dbg as any).pausedOp).toBe("my.op");
  });
});
```

---

## 10. REPL Oracle Interaction

### 10.1 Problem Statement

REPL cannot manually drive oracle sessions (send individual Req, see Resp).

### 10.2 Requirements

| ID | Requirement |
|----|-------------|
| ORC-1 | `:oracle-start` MUST begin manual oracle session |
| ORC-2 | `:req-eval EXPR` MUST send ReqEval and show response |
| ORC-3 | `:req-test TESTS` MUST send ReqTest and show response |
| ORC-4 | `:req-snapshot` MUST send ReqSnapshot and show receipt ID |
| ORC-5 | `:req-hydrate RID` MUST restore to receipt |
| ORC-6 | `:oracle-end MEANING` MUST end session with meaning |
| ORC-7 | `:transcript` MUST show session transcript |
| ORC-8 | Manual session MUST use same portal as automatic sessions |

### 10.3 Interface Definitions

```typescript
// src/packages/repl/oracleRepl.ts

export interface ManualOracleSession {
  /** Start a manual session */
  start(envRef: string): void;

  /** Send a request and get response */
  send(req: OracleReq): Promise<OracleResp>;

  /** End session with meaning */
  end(meaning: MeaningVal): void;

  /** Check if session is active */
  isActive(): boolean;

  /** Get transcript */
  getTranscript(): TranscriptEntry[];

  /** Current envRef */
  currentEnvRef(): string;
}

export function createManualOracleSession(
  portal: OraclePortal,
  snapshots: SnapshotRepo,
): ManualOracleSession;
```

### 10.4 Implementation Guidance

**File: `src/packages/repl/oracleRepl.ts`**

```typescript
export function createManualOracleSession(
  portal: OraclePortal,
  snapshots: SnapshotRepo,
): ManualOracleSession {
  let active = false;
  let envRef = "";
  const transcript: TranscriptEntry[] = [];

  return {
    start(initialEnvRef: string) {
      if (active) throw new Error("Session already active");
      active = true;
      envRef = initialEnvRef;
      transcript.length = 0;
      transcript.push({
        role: "system",
        content: `Session started with envRef=${envRef}`,
        timestamp: Date.now(),
      });
    },

    async send(req: OracleReq): Promise<OracleResp> {
      if (!active) throw new Error("No active session");

      // Log request
      transcript.push({
        role: "oracle",
        content: JSON.stringify(req),
        timestamp: Date.now(),
      });

      // Send to portal
      const resp = await portal.handleRequest(req);

      // Update envRef if applicable
      if (resp.tag === "RespVal" && resp.envRef) {
        envRef = resp.envRef;
      }

      // Log response
      transcript.push({
        role: "runtime",
        content: JSON.stringify(resp),
        timestamp: Date.now(),
      });

      return resp;
    },

    end(meaning: MeaningVal) {
      if (!active) throw new Error("No active session");
      active = false;
      transcript.push({
        role: "system",
        content: `Session ended with meaning: ${JSON.stringify(meaning)}`,
        timestamp: Date.now(),
      });
    },

    isActive(): boolean {
      return active;
    },

    getTranscript(): TranscriptEntry[] {
      return [...transcript];
    },

    currentEnvRef(): string {
      return envRef;
    },
  };
}
```

**REPL command handlers:**

```typescript
// In src/packages/repl/commands.ts

export function handleOracleStart(args: string, ctx: ReplContext): CommandResult {
  if (ctx.oracleSession?.isActive()) {
    return { output: "Oracle session already active. Use :oracle-end first." };
  }

  const envRef = ctx.snapshots.putEnv({ env: ctx.state!.env, store: ctx.state!.store });
  ctx.oracleSession!.start(envRef);

  return { output: `Oracle session started. envRef=${envRef}` };
}

export async function handleReqEval(args: string, ctx: ReplContext): Promise<CommandResult> {
  if (!ctx.oracleSession?.isActive()) {
    return { output: "No active oracle session. Use :oracle-start first." };
  }

  const req: OracleReq = {
    tag: "ReqEval",
    qexpr: args.trim() as any,
    envRef: ctx.oracleSession.currentEnvRef(),
  };

  const resp = await ctx.oracleSession.send(req);
  return { output: formatResponse(resp) };
}

export function handleTranscript(args: string, ctx: ReplContext): CommandResult {
  if (!ctx.oracleSession) {
    return { output: "(no oracle session)" };
  }

  const transcript = ctx.oracleSession.getTranscript();
  if (transcript.length === 0) {
    return { output: "(empty transcript)" };
  }

  const lines = transcript.map(e => `[${e.role}] ${e.content}`);
  return { output: lines.join("\n") };
}
```

### 10.5 Test Specifications

```typescript
// test/packages/repl/oracleRepl.spec.ts

describe("Manual Oracle Session", () => {
  test("ORC-1: :oracle-start begins session", () => {
    const session = createManualOracleSession(portal, snapshots);
    session.start("E0");
    expect(session.isActive()).toBe(true);
  });

  test("ORC-2: :req-eval sends and receives", async () => {
    const session = createManualOracleSession(portal, snapshots);
    session.start("E0");

    const resp = await session.send({
      tag: "ReqEval",
      qexpr: "(+ 1 2)" as any,
      envRef: "E0",
    });

    expect(resp.tag).toBe("RespVal");
  });

  test("ORC-7: transcript records all interactions", async () => {
    const session = createManualOracleSession(portal, snapshots);
    session.start("E0");
    await session.send({ tag: "ReqEval", qexpr: "(+ 1 2)" as any, envRef: "E0" });

    const transcript = session.getTranscript();
    expect(transcript.length).toBeGreaterThanOrEqual(3);  // start + req + resp
  });
});
```

---

## 11. REPL Polish Features

### 11.1 Problem Statement

REPL lacks polish features: tab completion, syntax highlighting, history, file loading.

### 11.2 Requirements

| ID | Requirement |
|----|-------------|
| POL-1 | Tab completion MUST complete symbols from environment |
| POL-2 | Tab completion MUST complete REPL commands |
| POL-3 | History MUST persist across sessions |
| POL-4 | `:load FILE` MUST load and evaluate file |
| POL-5 | `:save FILE` MUST save session definitions |
| POL-6 | Syntax highlighting SHOULD color parentheses by depth |
| POL-7 | Syntax highlighting SHOULD color keywords |

### 11.3 Interface Definitions

```typescript
// src/packages/repl/completion.ts

export interface Completer {
  complete(line: string, cursor: number): CompletionResult;
}

export interface CompletionResult {
  completions: string[];
  start: number;  // Position where completion starts
}

export function createCompleter(
  getSymbols: () => string[],
  commands: string[],
): Completer;
```

```typescript
// src/packages/repl/history.ts

export interface History {
  add(line: string): void;
  get(index: number): string | undefined;
  search(prefix: string): string[];
  save(path: string): Promise<void>;
  load(path: string): Promise<void>;
  length: number;
}

export function createHistory(maxSize?: number): History;
```

### 11.4 Implementation Guidance

**File: `src/packages/repl/completion.ts`**

```typescript
export function createCompleter(
  getSymbols: () => string[],
  commands: string[],
): Completer {
  return {
    complete(line: string, cursor: number): CompletionResult {
      // Find word being typed
      let start = cursor;
      while (start > 0 && /[a-zA-Z0-9_\-\.:]/.test(line[start - 1])) {
        start--;
      }

      const prefix = line.slice(start, cursor);

      if (prefix.startsWith(":")) {
        // Complete commands
        const matches = commands
          .filter(c => c.startsWith(prefix))
          .map(c => c);
        return { completions: matches, start };
      }

      // Complete symbols
      const symbols = getSymbols();
      const matches = symbols.filter(s => s.startsWith(prefix));
      return { completions: matches, start };
    },
  };
}
```

**File: `src/packages/repl/history.ts`**

```typescript
import * as fs from "fs/promises";

export function createHistory(maxSize = 1000): History {
  const items: string[] = [];

  return {
    add(line: string) {
      // Don't add duplicates of last entry
      if (items[items.length - 1] === line) return;
      items.push(line);
      if (items.length > maxSize) items.shift();
    },

    get(index: number): string | undefined {
      return items[index];
    },

    search(prefix: string): string[] {
      return items.filter(item => item.startsWith(prefix));
    },

    async save(path: string): Promise<void> {
      await fs.writeFile(path, items.join("\n"), "utf-8");
    },

    async load(path: string): Promise<void> {
      try {
        const content = await fs.readFile(path, "utf-8");
        items.length = 0;
        items.push(...content.split("\n").filter(Boolean));
      } catch {
        // File doesn't exist, ignore
      }
    },

    get length() {
      return items.length;
    },
  };
}
```

### 11.5 Test Specifications

```typescript
// test/packages/repl/completion.spec.ts

describe("Tab Completion", () => {
  test("POL-1: completes symbols", () => {
    const completer = createCompleter(() => ["define", "lambda", "let"], []);
    const result = completer.complete("(def", 4);
    expect(result.completions).toContain("define");
  });

  test("POL-2: completes commands", () => {
    const completer = createCompleter(() => [], [":help", ":env", ":stack"]);
    const result = completer.complete(":he", 3);
    expect(result.completions).toContain(":help");
  });
});
```

---

## 12. Deterministic IDs

### 12.1 Problem Statement

`uuid()` in machineStep.ts and capture.ts uses `Math.random()`, making tests non-deterministic.

### 12.2 Requirements

| ID | Requirement |
|----|-------------|
| DET-1 | ID generation MUST be deterministic when seed provided |
| DET-2 | Default behavior MUST remain random (backwards compat) |
| DET-3 | Test harness MUST be able to set deterministic mode |
| DET-4 | IDs MUST still be unique within a session |

### 12.3 Interface Definitions

```typescript
// src/core/util/id.ts

export interface IdGenerator {
  next(): string;
  reset(): void;
}

/** Create a seeded ID generator for deterministic tests */
export function createSeededIdGenerator(seed: number): IdGenerator;

/** Create a random ID generator (default) */
export function createRandomIdGenerator(): IdGenerator;

/** Global ID generator (can be swapped for testing) */
export let globalIdGenerator: IdGenerator;

export function setGlobalIdGenerator(gen: IdGenerator): void;
export function resetGlobalIdGenerator(): void;

/** Generate next ID using global generator */
export function generateId(): string;
```

### 12.4 Implementation Guidance

**File: `src/core/util/id.ts`**

```typescript
export interface IdGenerator {
  next(): string;
  reset(): void;
}

// Simple seeded PRNG (xorshift32)
function xorshift32(seed: number): () => number {
  let x = seed;
  return () => {
    x ^= x << 13;
    x ^= x >>> 17;
    x ^= x << 5;
    return x >>> 0;
  };
}

export function createSeededIdGenerator(seed: number): IdGenerator {
  const rand = xorshift32(seed);
  let counter = 0;

  return {
    next(): string {
      counter++;
      return `id-${counter.toString(16)}-${rand().toString(16)}`;
    },
    reset(): void {
      counter = 0;
    },
  };
}

export function createRandomIdGenerator(): IdGenerator {
  return {
    next(): string {
      return Math.random().toString(16).slice(2) + "-" + Date.now().toString(16);
    },
    reset(): void {
      // No-op for random
    },
  };
}

let globalIdGenerator: IdGenerator = createRandomIdGenerator();

export function setGlobalIdGenerator(gen: IdGenerator): void {
  globalIdGenerator = gen;
}

export function resetGlobalIdGenerator(): void {
  globalIdGenerator = createRandomIdGenerator();
}

export function generateId(): string {
  return globalIdGenerator.next();
}
```

**Update machineStep.ts and capture.ts to use `generateId()`:**

```typescript
// In machineStep.ts, replace:
// function uuid(): string { return Math.random()... }
// With:
import { generateId } from "../util/id";

// And use generateId() everywhere uuid() was used
```

### 12.5 Test Specifications

```typescript
// test/core/util/id.spec.ts

describe("ID Generation", () => {
  test("DET-1: seeded generator is deterministic", () => {
    const gen1 = createSeededIdGenerator(12345);
    const gen2 = createSeededIdGenerator(12345);

    expect(gen1.next()).toBe(gen2.next());
    expect(gen1.next()).toBe(gen2.next());
  });

  test("DET-4: IDs are unique", () => {
    const gen = createSeededIdGenerator(12345);
    const ids = new Set<string>();

    for (let i = 0; i < 1000; i++) {
      const id = gen.next();
      expect(ids.has(id)).toBe(false);
      ids.add(id);
    }
  });

  test("DET-3: test harness can set deterministic mode", () => {
    setGlobalIdGenerator(createSeededIdGenerator(42));
    const id1 = generateId();

    setGlobalIdGenerator(createSeededIdGenerator(42));
    const id2 = generateId();

    expect(id1).toBe(id2);

    resetGlobalIdGenerator();
  });
});
```

---

## Summary: Implementation Order

### Phase 1: Core Gaps (Block packages)
1. **Budget Enforcement** (2h) - Wire into run.ts
2. **Tool Registry** (2h) - Basic registry implementation
3. **ReqTool** (2h) - Wire into portalImpl.ts
4. **Deterministic IDs** (1h) - Support testing

### Phase 2: LLM Integration
5. **LLM Adapter** (4h) - OpenAI/Anthropic/Ollama support

### Phase 3: REPL Debugger
6. **Debugger Commands** (4h) - :stack, :frame, :env
7. **Pause/Resume/Step** (4h) - Interactive debugging
8. **Oracle Interaction** (3h) - Manual oracle sessions

### Phase 4: Polish
9. **Nondet Modes** (2h) - best/sample
10. **Context Constraints** (2h) - Sealed, NoNewFacts
11. **Evidence Chain** (2h) - Accumulation
12. **REPL Polish** (4h) - Completion, history, syntax

**Total Estimated Time: ~34 hours (~4-5 days)**

---

*Document Version: 1.0*
*Created: 2025-01-17*
