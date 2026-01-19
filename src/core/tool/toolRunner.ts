// src/core/tool/toolRunner.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-18.md
// Tool runner with capability gating and budget enforcement

import { spawn } from "child_process";
import { promises as fs } from "fs";

import type { Budget } from "../governance/budgets";
import { budgetConsumeToolCall } from "../governance/budgets";
import type { CapSet } from "../governance/caps";
import { capRequire, capHas } from "../governance/caps";

// =========================================================================
// Tool Call Types
// =========================================================================

export type ToolCallBash = {
  tag: "ToolBash";
  argv: string[];
  cwd?: string;
  stdin?: string;
  timeoutMs?: number;
};

export type ToolCallFSRead = {
  tag: "ToolFSRead";
  path: string;
  encoding?: BufferEncoding;
};

export type ToolCallFSWrite = {
  tag: "ToolFSWrite";
  path: string;
  content: string;
  encoding?: BufferEncoding;
};

export type ToolCall = ToolCallBash | ToolCallFSRead | ToolCallFSWrite;

// =========================================================================
// Tool Result Types
// =========================================================================

export type ToolResultOk = {
  tag: "ToolOk";
  stdout?: string;
  stderr?: string;
  exitCode?: number;
};

export type ToolResultFile = {
  tag: "ToolFile";
  path: string;
  content: string;
};

export type ToolResultErr = {
  tag: "ToolErr";
  message: string;
  stderr?: string;
  exitCode?: number;
};

export type ToolResult = ToolResultOk | ToolResultFile | ToolResultErr;

// =========================================================================
// Tool Runner
// =========================================================================

export class ToolRunner {
  async run(
    call: ToolCall,
    caps: CapSet,
    budget: Budget,
  ): Promise<{ result: ToolResult; budget: Budget }> {
    // Capability checks - fine-grained per tool type
    if (call.tag === "ToolBash") {
      capRequire(caps, "tool.bash", "ReqTool(ToolBash)");
    }
    if (call.tag === "ToolFSRead") {
      capRequire(caps, "tool.fs.read", "ReqTool(ToolFSRead)");
    }
    if (call.tag === "ToolFSWrite") {
      capRequire(caps, "tool.fs.write", "ReqTool(ToolFSWrite)");
    }

    // Budget check
    const b = budgetConsumeToolCall(budget);

    // Execute the tool
    if (call.tag === "ToolFSRead") {
      try {
        const enc = call.encoding ?? "utf8";
        const content = await fs.readFile(call.path, { encoding: enc });
        return {
          result: { tag: "ToolFile", path: call.path, content: content.toString() },
          budget: b,
        };
      } catch (e: unknown) {
        const msg = e instanceof Error ? e.message : String(e);
        return {
          result: { tag: "ToolErr", message: `read failed: ${msg}` },
          budget: b,
        };
      }
    }

    if (call.tag === "ToolFSWrite") {
      try {
        const enc = call.encoding ?? "utf8";
        await fs.writeFile(call.path, call.content, { encoding: enc });
        return {
          result: { tag: "ToolOk", stdout: `wrote ${call.path}` },
          budget: b,
        };
      } catch (e: unknown) {
        const msg = e instanceof Error ? e.message : String(e);
        return {
          result: { tag: "ToolErr", message: `write failed: ${msg}` },
          budget: b,
        };
      }
    }

    if (call.tag === "ToolBash") {
      const res = await runCommand(call.argv, call.cwd, call.stdin, call.timeoutMs);
      if (!res.ok) {
        return {
          result: {
            tag: "ToolErr",
            message: "command failed",
            stderr: res.stderr,
            exitCode: res.code ?? undefined,
          },
          budget: b,
        };
      }
      return {
        result: {
          tag: "ToolOk",
          stdout: res.stdout,
          stderr: res.stderr,
          exitCode: res.code ?? undefined,
        },
        budget: b,
      };
    }

    return {
      result: { tag: "ToolErr", message: `unknown ToolCall tag ${(call as ToolCall).tag}` },
      budget: b,
    };
  }
}

// =========================================================================
// Command Runner Helper
// =========================================================================

async function runCommand(
  argv: string[],
  cwd?: string,
  stdin?: string,
  timeoutMs?: number
): Promise<{ ok: boolean; stdout: string; stderr: string; code: number | null }> {
  const [cmd, ...args] = argv;
  if (!cmd) return { ok: false, stdout: "", stderr: "empty argv", code: 127 };

  return await new Promise((resolve) => {
    const child = spawn(cmd, args, { cwd, stdio: "pipe", shell: process.platform === "win32" });

    let stdout = "";
    let stderr = "";

    child.stdout.setEncoding("utf8");
    child.stderr.setEncoding("utf8");

    child.stdout.on("data", (d) => { stdout += d; });
    child.stderr.on("data", (d) => { stderr += d; });

    if (stdin != null) {
      child.stdin.write(stdin);
      child.stdin.end();
    }

    let killed = false;
    let t: ReturnType<typeof setTimeout> | undefined;
    if (timeoutMs != null) {
      t = setTimeout(() => {
        killed = true;
        child.kill("SIGKILL");
      }, timeoutMs);
    }

    child.on("close", (code) => {
      if (t) clearTimeout(t);
      const ok = !killed && (code === 0);
      resolve({ ok, stdout, stderr, code });
    });

    child.on("error", (err) => {
      if (t) clearTimeout(t);
      resolve({ ok: false, stdout, stderr: err.message, code: null });
    });
  });
}

// =========================================================================
// Convenience Factory
// =========================================================================

export function createToolRunner(): ToolRunner {
  return new ToolRunner();
}
