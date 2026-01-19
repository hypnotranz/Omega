// src/core/tools/registry.ts
// Tool Registry for executing oracle tool calls
// SOURCE: REQUIREMENTS.md Section 2-3

import type { ToolCall } from "../oracle/protocol";
import type { Val } from "../eval/values";
import { capRequire, capHas, type CapSet } from "../governance/caps";
import type { BudgetTracker } from "../governance/budgets";

/**
 * Tool execution result.
 */
export type ToolResult = {
  success: boolean;
  output?: string;
  error?: string;
  exitCode?: number;
  durationMs?: number;
};

/**
 * Tool definition.
 */
export type ToolDef = {
  name: string;
  description?: string;
  requiredCap?: string;  // Capability required to use this tool
  handler: (call: ToolCall) => Promise<ToolResult>;
};

/**
 * Tool Registry - manages tool registration and execution.
 */
export class ToolRegistry {
  private tools = new Map<string, ToolDef>();
  private caps?: CapSet;
  private budget?: BudgetTracker;

  constructor(opts?: { caps?: CapSet; budget?: BudgetTracker }) {
    this.caps = opts?.caps;
    this.budget = opts?.budget;
  }

  /** Set capabilities for tool authorization. */
  setCapSet(caps: CapSet): void {
    this.caps = caps;
  }

  /** Set budget tracker for tool call accounting. */
  setBudget(budget: BudgetTracker): void {
    this.budget = budget;
  }

  /** Register a tool. */
  register(def: ToolDef): void {
    this.tools.set(def.name, def);
  }

  /** Unregister a tool. */
  unregister(name: string): boolean {
    return this.tools.delete(name);
  }

  /** Check if a tool is registered. */
  has(name: string): boolean {
    return this.tools.has(name);
  }

  /** Get tool definition. */
  get(name: string): ToolDef | undefined {
    return this.tools.get(name);
  }

  /** List all registered tool names. */
  list(): string[] {
    return Array.from(this.tools.keys());
  }

  /**
   * Execute a tool call.
   * Enforces capabilities and budget constraints.
   */
  async execute(call: ToolCall): Promise<ToolResult> {
    const tool = this.tools.get(call.name);
    if (!tool) {
      return {
        success: false,
        error: `unknown tool: ${call.name}`,
      };
    }

    // Check capability
    if (tool.requiredCap && this.caps) {
      if (!capHas(this.caps, tool.requiredCap)) {
        return {
          success: false,
          error: `missing capability for tool ${call.name}: requires ${tool.requiredCap}`,
        };
      }
    }

    // Consume budget
    this.budget?.consumeToolCall();

    // Execute with timing
    const t0 = Date.now();
    try {
      const result = await tool.handler(call);
      result.durationMs = Date.now() - t0;
      return result;
    } catch (e: any) {
      return {
        success: false,
        error: e?.message ?? String(e),
        durationMs: Date.now() - t0,
      };
    }
  }
}

/**
 * Create a shell tool that executes commands.
 * This is a factory function - the actual execution is delegated to a shell adapter.
 */
export function createShellTool(
  shellAdapter: (cmd: string, opts: { cwd?: string; stdin?: string; timeoutMs?: number }) => Promise<{ stdout: string; stderr: string; exitCode: number }>
): ToolDef {
  return {
    name: "shell",
    description: "Execute a shell command",
    requiredCap: "tool.shell",
    handler: async (call: ToolCall): Promise<ToolResult> => {
      const cmd = [call.name, ...call.argv].join(" ");
      try {
        const { stdout, stderr, exitCode } = await shellAdapter(cmd, {
          cwd: call.cwd,
          stdin: call.stdin,
          timeoutMs: call.timeoutMs ?? 30000,
        });
        return {
          success: exitCode === 0,
          output: stdout + (stderr ? `\n[stderr]: ${stderr}` : ""),
          exitCode,
        };
      } catch (e: any) {
        return {
          success: false,
          error: e?.message ?? String(e),
        };
      }
    },
  };
}

/**
 * Create a read-file tool.
 */
export function createReadFileTool(
  readFile: (path: string) => Promise<string>
): ToolDef {
  return {
    name: "read_file",
    description: "Read contents of a file",
    requiredCap: "tool.fs.read",
    handler: async (call: ToolCall): Promise<ToolResult> => {
      const path = call.argv[0];
      if (!path) {
        return { success: false, error: "read_file requires a path argument" };
      }
      try {
        const content = await readFile(path);
        return { success: true, output: content };
      } catch (e: any) {
        return { success: false, error: e?.message ?? String(e) };
      }
    },
  };
}

/**
 * Create a write-file tool.
 */
export function createWriteFileTool(
  writeFile: (path: string, content: string) => Promise<void>
): ToolDef {
  return {
    name: "write_file",
    description: "Write contents to a file",
    requiredCap: "tool.fs.write",
    handler: async (call: ToolCall): Promise<ToolResult> => {
      const path = call.argv[0];
      const content = call.stdin ?? call.argv[1];
      if (!path) {
        return { success: false, error: "write_file requires a path argument" };
      }
      if (content === undefined) {
        return { success: false, error: "write_file requires content (stdin or second arg)" };
      }
      try {
        await writeFile(path, content);
        return { success: true, output: `wrote ${content.length} bytes to ${path}` };
      } catch (e: any) {
        return { success: false, error: e?.message ?? String(e) };
      }
    },
  };
}

/**
 * Create a grep tool for searching files.
 */
export function createGrepTool(
  grep: (pattern: string, paths: string[]) => Promise<{ matches: Array<{ file: string; line: number; text: string }> }>
): ToolDef {
  return {
    name: "grep",
    description: "Search for pattern in files",
    requiredCap: "tool.fs.read",
    handler: async (call: ToolCall): Promise<ToolResult> => {
      const [pattern, ...paths] = call.argv;
      if (!pattern) {
        return { success: false, error: "grep requires a pattern argument" };
      }
      try {
        const { matches } = await grep(pattern, paths.length > 0 ? paths : ["."]);
        const output = matches.map(m => `${m.file}:${m.line}: ${m.text}`).join("\n");
        return { success: true, output: output || "(no matches)" };
      } catch (e: any) {
        return { success: false, error: e?.message ?? String(e) };
      }
    },
  };
}

// Default global registry instance
let defaultRegistry: ToolRegistry | undefined;

/** Get or create the default global tool registry. */
export function getDefaultRegistry(): ToolRegistry {
  if (!defaultRegistry) {
    defaultRegistry = new ToolRegistry();
  }
  return defaultRegistry;
}

/** Set the default global tool registry. */
export function setDefaultRegistry(registry: ToolRegistry): void {
  defaultRegistry = registry;
}
