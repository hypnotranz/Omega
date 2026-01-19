// test/core/tools/registry.spec.ts
// Comprehensive tests for Tool Registry

import { describe, it, expect, beforeEach } from "vitest";
import {
  ToolRegistry,
  ToolDef,
  ToolResult,
  createReadFileTool,
  createWriteFileTool,
  createGrepTool,
  getDefaultRegistry,
  setDefaultRegistry,
} from "../../../src/core/tools/registry";
import { BudgetTracker } from "../../../src/core/governance/budgets";
import type { ToolCall } from "../../../src/core/oracle/protocol";

describe("ToolRegistry", () => {
  let registry: ToolRegistry;

  beforeEach(() => {
    registry = new ToolRegistry();
  });

  describe("registration", () => {
    it("registers a tool", () => {
      const tool: ToolDef = {
        name: "echo",
        handler: async (call) => ({ success: true, output: call.argv.join(" ") }),
      };
      registry.register(tool);
      expect(registry.has("echo")).toBe(true);
    });

    it("get() returns registered tool", () => {
      const tool: ToolDef = {
        name: "test",
        description: "A test tool",
        handler: async () => ({ success: true }),
      };
      registry.register(tool);
      const got = registry.get("test");
      expect(got?.name).toBe("test");
      expect(got?.description).toBe("A test tool");
    });

    it("get() returns undefined for unregistered tool", () => {
      expect(registry.get("nonexistent")).toBeUndefined();
    });

    it("has() returns false for unregistered tool", () => {
      expect(registry.has("nonexistent")).toBe(false);
    });

    it("list() returns all registered tool names", () => {
      registry.register({ name: "a", handler: async () => ({ success: true }) });
      registry.register({ name: "b", handler: async () => ({ success: true }) });
      registry.register({ name: "c", handler: async () => ({ success: true }) });

      const names = registry.list();
      expect(names).toContain("a");
      expect(names).toContain("b");
      expect(names).toContain("c");
      expect(names.length).toBe(3);
    });

    it("unregister() removes a tool", () => {
      registry.register({ name: "removeme", handler: async () => ({ success: true }) });
      expect(registry.has("removeme")).toBe(true);

      const removed = registry.unregister("removeme");
      expect(removed).toBe(true);
      expect(registry.has("removeme")).toBe(false);
    });

    it("unregister() returns false for nonexistent tool", () => {
      expect(registry.unregister("nonexistent")).toBe(false);
    });
  });

  describe("execution", () => {
    it("executes a simple tool", async () => {
      registry.register({
        name: "add",
        handler: async (call) => {
          const [a, b] = call.argv.map(Number);
          return { success: true, output: String(a + b) };
        },
      });

      const result = await registry.execute({ name: "add", argv: ["2", "3"] });
      expect(result.success).toBe(true);
      expect(result.output).toBe("5");
    });

    it("returns error for unknown tool", async () => {
      const result = await registry.execute({ name: "unknown", argv: [] });
      expect(result.success).toBe(false);
      expect(result.error).toContain("unknown tool");
    });

    it("captures tool execution duration", async () => {
      registry.register({
        name: "slow",
        handler: async () => {
          await new Promise(r => setTimeout(r, 50));
          return { success: true };
        },
      });

      const result = await registry.execute({ name: "slow", argv: [] });
      expect(result.durationMs).toBeGreaterThanOrEqual(50);
    });

    it("catches handler exceptions", async () => {
      registry.register({
        name: "throws",
        handler: async () => {
          throw new Error("intentional error");
        },
      });

      const result = await registry.execute({ name: "throws", argv: [] });
      expect(result.success).toBe(false);
      expect(result.error).toContain("intentional error");
    });

    it("passes all call properties to handler", async () => {
      let receivedCall: ToolCall | null = null;
      registry.register({
        name: "inspector",
        handler: async (call) => {
          receivedCall = call;
          return { success: true };
        },
      });

      await registry.execute({
        name: "inspector",
        argv: ["arg1", "arg2"],
        cwd: "/test/dir",
        stdin: "input data",
        timeoutMs: 5000,
      });

      expect(receivedCall).not.toBeNull();
      expect(receivedCall!.argv).toEqual(["arg1", "arg2"]);
      expect(receivedCall!.cwd).toBe("/test/dir");
      expect(receivedCall!.stdin).toBe("input data");
      expect(receivedCall!.timeoutMs).toBe(5000);
    });
  });

  describe("capability enforcement", () => {
    it("allows execution when capability is present", async () => {
      registry.setCapSet(["tool.read"]);
      registry.register({
        name: "reader",
        requiredCap: "tool.read",
        handler: async () => ({ success: true, output: "data" }),
      });

      const result = await registry.execute({ name: "reader", argv: [] });
      expect(result.success).toBe(true);
    });

    it("denies execution when capability is missing", async () => {
      registry.setCapSet(["tool.write"]);
      registry.register({
        name: "reader",
        requiredCap: "tool.read",
        handler: async () => ({ success: true, output: "data" }),
      });

      const result = await registry.execute({ name: "reader", argv: [] });
      expect(result.success).toBe(false);
      expect(result.error).toContain("missing capability");
      expect(result.error).toContain("tool.read");
    });

    it("allows execution with wildcard capability", async () => {
      registry.setCapSet(["*"]);
      registry.register({
        name: "anything",
        requiredCap: "tool.dangerous",
        handler: async () => ({ success: true }),
      });

      const result = await registry.execute({ name: "anything", argv: [] });
      expect(result.success).toBe(true);
    });

    it("allows execution with domain wildcard", async () => {
      registry.setCapSet(["tool.*"]);
      registry.register({
        name: "tooluser",
        requiredCap: "tool.specific",
        handler: async () => ({ success: true }),
      });

      const result = await registry.execute({ name: "tooluser", argv: [] });
      expect(result.success).toBe(true);
    });

    it("allows execution when no capability required", async () => {
      registry.setCapSet([]);
      registry.register({
        name: "open",
        // no requiredCap
        handler: async () => ({ success: true }),
      });

      const result = await registry.execute({ name: "open", argv: [] });
      expect(result.success).toBe(true);
    });

    it("allows execution when no caps are set on registry", async () => {
      // caps not set at all
      registry.register({
        name: "tool",
        requiredCap: "tool.any",
        handler: async () => ({ success: true }),
      });

      const result = await registry.execute({ name: "tool", argv: [] });
      expect(result.success).toBe(true);
    });
  });

  describe("budget tracking", () => {
    it("consumes budget on tool execution", async () => {
      const budget = new BudgetTracker({ maxToolCalls: 10 });
      registry.setBudget(budget);
      registry.register({
        name: "counted",
        handler: async () => ({ success: true }),
      });

      await registry.execute({ name: "counted", argv: [] });
      await registry.execute({ name: "counted", argv: [] });

      expect(budget.snapshot().consumed.toolCalls).toBe(2);
    });

    it("throws when tool budget exhausted", async () => {
      const budget = new BudgetTracker({ maxToolCalls: 1 });
      registry.setBudget(budget);
      registry.register({
        name: "limited",
        handler: async () => ({ success: true }),
      });

      await registry.execute({ name: "limited", argv: [] });

      await expect(registry.execute({ name: "limited", argv: [] })).rejects.toThrow(
        "budget exhausted: toolCalls"
      );
    });

    it("does not consume budget when tool not found", async () => {
      const budget = new BudgetTracker({ maxToolCalls: 10 });
      registry.setBudget(budget);

      await registry.execute({ name: "nonexistent", argv: [] });

      expect(budget.snapshot().consumed.toolCalls).toBe(0);
    });

    it("does not consume budget when capability denied", async () => {
      const budget = new BudgetTracker({ maxToolCalls: 10 });
      registry.setBudget(budget);
      registry.setCapSet([]);
      registry.register({
        name: "restricted",
        requiredCap: "tool.secret",
        handler: async () => ({ success: true }),
      });

      await registry.execute({ name: "restricted", argv: [] });

      expect(budget.snapshot().consumed.toolCalls).toBe(0);
    });
  });

  describe("constructor options", () => {
    it("accepts caps in constructor", async () => {
      const reg = new ToolRegistry({ caps: ["tool.read"] });
      reg.register({
        name: "reader",
        requiredCap: "tool.write",
        handler: async () => ({ success: true }),
      });

      const result = await reg.execute({ name: "reader", argv: [] });
      expect(result.success).toBe(false);
      expect(result.error).toContain("missing capability");
    });

    it("accepts budget in constructor", async () => {
      const budget = new BudgetTracker({ maxToolCalls: 5 });
      const reg = new ToolRegistry({ budget });
      reg.register({
        name: "test",
        handler: async () => ({ success: true }),
      });

      await reg.execute({ name: "test", argv: [] });
      expect(budget.snapshot().consumed.toolCalls).toBe(1);
    });
  });
});

describe("Tool factory functions", () => {
  describe("createReadFileTool", () => {
    it("creates a read file tool", async () => {
      const mockReadFile = async (path: string) => `content of ${path}`;
      const tool = createReadFileTool(mockReadFile);

      expect(tool.name).toBe("read_file");
      expect(tool.requiredCap).toBe("tool.fs.read");
    });

    it("reads file content", async () => {
      const mockReadFile = async (path: string) => `content of ${path}`;
      const tool = createReadFileTool(mockReadFile);

      const result = await tool.handler({ name: "read_file", argv: ["/path/to/file.txt"] });
      expect(result.success).toBe(true);
      expect(result.output).toBe("content of /path/to/file.txt");
    });

    it("returns error when path missing", async () => {
      const mockReadFile = async () => "data";
      const tool = createReadFileTool(mockReadFile);

      const result = await tool.handler({ name: "read_file", argv: [] });
      expect(result.success).toBe(false);
      expect(result.error).toContain("requires a path");
    });

    it("handles read errors", async () => {
      const mockReadFile = async () => { throw new Error("file not found"); };
      const tool = createReadFileTool(mockReadFile);

      const result = await tool.handler({ name: "read_file", argv: ["/nonexistent"] });
      expect(result.success).toBe(false);
      expect(result.error).toContain("file not found");
    });
  });

  describe("createWriteFileTool", () => {
    it("creates a write file tool", () => {
      const mockWriteFile = async () => {};
      const tool = createWriteFileTool(mockWriteFile);

      expect(tool.name).toBe("write_file");
      expect(tool.requiredCap).toBe("tool.fs.write");
    });

    it("writes content via stdin", async () => {
      let writtenPath: string | null = null;
      let writtenContent: string | null = null;
      const mockWriteFile = async (path: string, content: string) => {
        writtenPath = path;
        writtenContent = content;
      };
      const tool = createWriteFileTool(mockWriteFile);

      const result = await tool.handler({
        name: "write_file",
        argv: ["/path/to/output.txt"],
        stdin: "hello world",
      });

      expect(result.success).toBe(true);
      expect(writtenPath).toBe("/path/to/output.txt");
      expect(writtenContent).toBe("hello world");
    });

    it("writes content via second arg", async () => {
      let writtenContent: string | null = null;
      const mockWriteFile = async (_: string, content: string) => {
        writtenContent = content;
      };
      const tool = createWriteFileTool(mockWriteFile);

      const result = await tool.handler({
        name: "write_file",
        argv: ["/path.txt", "content from arg"],
      });

      expect(result.success).toBe(true);
      expect(writtenContent).toBe("content from arg");
    });

    it("returns error when path missing", async () => {
      const mockWriteFile = async () => {};
      const tool = createWriteFileTool(mockWriteFile);

      const result = await tool.handler({ name: "write_file", argv: [] });
      expect(result.success).toBe(false);
      expect(result.error).toContain("requires a path");
    });

    it("returns error when content missing", async () => {
      const mockWriteFile = async () => {};
      const tool = createWriteFileTool(mockWriteFile);

      const result = await tool.handler({ name: "write_file", argv: ["/path.txt"] });
      expect(result.success).toBe(false);
      expect(result.error).toContain("requires content");
    });
  });

  describe("createGrepTool", () => {
    it("creates a grep tool", () => {
      const mockGrep = async () => ({ matches: [] });
      const tool = createGrepTool(mockGrep);

      expect(tool.name).toBe("grep");
      expect(tool.requiredCap).toBe("tool.fs.read");
    });

    it("returns formatted matches", async () => {
      const mockGrep = async () => ({
        matches: [
          { file: "a.txt", line: 10, text: "found here" },
          { file: "b.txt", line: 20, text: "also here" },
        ],
      });
      const tool = createGrepTool(mockGrep);

      const result = await tool.handler({ name: "grep", argv: ["pattern", "a.txt", "b.txt"] });
      expect(result.success).toBe(true);
      expect(result.output).toContain("a.txt:10: found here");
      expect(result.output).toContain("b.txt:20: also here");
    });

    it("returns no matches message", async () => {
      const mockGrep = async () => ({ matches: [] });
      const tool = createGrepTool(mockGrep);

      const result = await tool.handler({ name: "grep", argv: ["pattern"] });
      expect(result.success).toBe(true);
      expect(result.output).toBe("(no matches)");
    });

    it("returns error when pattern missing", async () => {
      const mockGrep = async () => ({ matches: [] });
      const tool = createGrepTool(mockGrep);

      const result = await tool.handler({ name: "grep", argv: [] });
      expect(result.success).toBe(false);
      expect(result.error).toContain("requires a pattern");
    });

    it("uses current directory when no paths given", async () => {
      let receivedPaths: string[] = [];
      const mockGrep = async (_: string, paths: string[]) => {
        receivedPaths = paths;
        return { matches: [] };
      };
      const tool = createGrepTool(mockGrep);

      await tool.handler({ name: "grep", argv: ["pattern"] });
      expect(receivedPaths).toEqual(["."]);
    });
  });
});

describe("Default registry", () => {
  it("getDefaultRegistry returns same instance", () => {
    const reg1 = getDefaultRegistry();
    const reg2 = getDefaultRegistry();
    expect(reg1).toBe(reg2);
  });

  it("setDefaultRegistry replaces the default", () => {
    const custom = new ToolRegistry();
    custom.register({ name: "custom", handler: async () => ({ success: true }) });

    setDefaultRegistry(custom);

    expect(getDefaultRegistry().has("custom")).toBe(true);
  });
});
