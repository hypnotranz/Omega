import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { spawn } from "child_process";
import * as os from "os";
import * as fs from "fs";
import * as path from "path";

const REPL_TIMEOUT_MS = 120_000;
const REPL_COMMAND_DELAY_MS = 800;
const REPL_START_DELAY_MS = 800;

type ReplRun = { raw: string; lines: string[] };

let sessionDir = "";

function createSessionDir(): string {
  return fs.mkdtempSync(path.join(os.tmpdir(), "omega-session-test-"));
}

function cleanupSessionDir(dir: string): void {
  if (dir && fs.existsSync(dir)) {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

async function runRepl(commands: string[], targetDir: string): Promise<ReplRun> {
  return new Promise((resolve, reject) => {
    const npxCmd = process.platform === "win32" ? "npx.cmd" : "npx";
    const proc = spawn(npxCmd, ["tsx", "bin/omega-repl.ts"], {
      env: {
        ...process.env,
        OMEGA_SESSION_DIR: targetDir,
        OMEGA_SCRIPTED_ORACLE: "1",
        CI: "1",
      },
      shell: true,
    });

    let raw = "";
    proc.stdout.on("data", (d) => {
      raw += d.toString();
    });
    proc.stderr.on("data", (d) => {
      raw += d.toString();
    });

    const timeout = setTimeout(() => {
      proc.kill();
      reject(new Error("REPL timeout"));
    }, REPL_TIMEOUT_MS);

    proc.on("error", (err) => {
      clearTimeout(timeout);
      reject(err);
    });
    proc.on("close", () => {
      clearTimeout(timeout);
      const lines = raw.split(/\r?\n/).map(l => l.trim()).filter(Boolean);
      resolve({ raw, lines });
    });

    let idx = 0;
    const sendNext = () => {
      if (idx < commands.length) {
        proc.stdin.write(commands[idx] + "\n");
        idx++;
        setTimeout(sendNext, REPL_COMMAND_DELAY_MS);
      } else {
        proc.stdin.write(":quit\n");
        proc.stdin.end();
      }
    };

    setTimeout(sendNext, REPL_START_DELAY_MS);
  });
}

describe("Session integration (REPL)", () => {
  beforeEach(() => {
    sessionDir = createSessionDir();
  });
  afterEach(() => cleanupSessionDir(sessionDir));

  it("auto-saves session to current.jsonl", async () => {
    await runRepl([
      "(define x 42)",
      "(+ x 1)",
    ], sessionDir);

    const currentFile = path.join(sessionDir, "sessions", "current.jsonl");
    expect(fs.existsSync(currentFile)).toBe(true);

    const content = fs.readFileSync(currentFile, "utf8");
    expect(content).toContain("\"type\":\"input\"");
    expect(content).toContain("define x 42");
  });

  it("explicit save creates named session", async () => {
    await runRepl([
      "(define x 42)",
      ":session save my-test",
    ], sessionDir);

    const savedFile = path.join(sessionDir, "sessions", "my-test.jsonl");
    const indexFile = path.join(sessionDir, "sessions", "my-test.index.json");

    expect(fs.existsSync(savedFile)).toBe(true);
    expect(fs.existsSync(indexFile)).toBe(true);
  });

  it("fork creates a new session file", async () => {
    await runRepl([
      "(define x 1)",
      ":session fork fork-test",
    ], sessionDir);

    const forkFile = path.join(sessionDir, "sessions", "fork-test.jsonl");
    const forkIndex = path.join(sessionDir, "sessions", "fork-test.index.json");

    expect(fs.existsSync(forkFile)).toBe(true);
    expect(fs.existsSync(forkIndex)).toBe(true);
  });

  it("load restores environment bindings", async () => {
    await runRepl([
      "(define my-fn (lambda (x) (* x 2)))",
      ":session save env-test",
    ], sessionDir);

    const { lines } = await runRepl([
      ":session load env-test",
      ":session goto 999",
      "(my-fn 21)",
    ], sessionDir);

    expect(lines.some(l => l.includes("=>") && l.includes("42"))).toBe(true);
  });

  it("creates checkpoints at LLM boundaries", async () => {
    await runRepl([
      '(effect infer.op "test prompt")',
      ":session save llm-test",
    ], sessionDir);

    const index = JSON.parse(
      fs.readFileSync(
        path.join(sessionDir, "sessions", "llm-test.index.json"),
        "utf8"
      )
    );

    const llmCheckpoints = (index.checkpoints || []).filter((cp: any) => cp.reason === "llm_boundary");
    expect(llmCheckpoints.length).toBeGreaterThan(0);
  });

  it("jump restores exact state at checkpoint", async () => {
    await runRepl([
      "(define counter 0)",
      "(set! counter 1)",
      "(set! counter 2)",
      "(set! counter 3)",
      ":session save counter-test",
    ], sessionDir);

    const index = JSON.parse(
      fs.readFileSync(
        path.join(sessionDir, "sessions", "counter-test.index.json"),
        "utf8"
      )
    );
    const targetSeq = index.checkpoints?.[2]?.seq ?? index.checkpoints?.[index.checkpoints.length - 2]?.seq;

    const { lines } = await runRepl([
      ":session load counter-test",
      `:session goto ${targetSeq ?? 0}`,
      "counter",
    ], sessionDir);

    expect(lines.some(l => l.includes("=>") && l.includes("2"))).toBe(true);
  });

  it("resume uses cached receipts (no live LLM call)", async () => {
    await runRepl([
      '(effect infer.op "What is 2+2?")',
      ":session save receipt-test",
    ], sessionDir);

    const index = JSON.parse(
      fs.readFileSync(
        path.join(sessionDir, "sessions", "receipt-test.index.json"),
        "utf8"
      )
    );
    const resumeSeq = index.checkpoints?.[0]?.seq ?? 0;

    const { raw } = await runRepl([
      ":session load receipt-test",
      `:session goto ${resumeSeq}`,
      ":session resume",
    ], sessionDir);

    expect(/cached/i.test(raw)).toBe(true);
  });

  it("what-if: jump then run different code with same env", async () => {
    await runRepl([
      "(define (double x) (* x 2))",
      "(double 5)",
      ":session save what-if-test",
    ], sessionDir);

    const index = JSON.parse(
      fs.readFileSync(
        path.join(sessionDir, "sessions", "what-if-test.index.json"),
        "utf8"
      )
    );
    const seq = index.checkpoints?.[0]?.seq ?? 0;

    const { lines } = await runRepl([
      ":session load what-if-test",
      `:session goto ${seq}`,
      "(double 100)",
    ], sessionDir);

    expect(lines.some(l => l.includes("=>") && l.includes("200"))).toBe(true);
  });

  it("what-if: modify env at checkpoint then continue", async () => {
    await runRepl([
      "(define multiplier 2)",
      "(define (scale x) (* x multiplier))",
      "(scale 5)",
      ":session save modify-env-test",
    ], sessionDir);

    const index = JSON.parse(
      fs.readFileSync(
        path.join(sessionDir, "sessions", "modify-env-test.index.json"),
        "utf8"
      )
    );
    const seq = index.checkpoints?.[1]?.seq ?? index.checkpoints?.[0]?.seq ?? 0;

    const { lines } = await runRepl([
      ":session load modify-env-test",
      `:session goto ${seq}`,
      "(set! multiplier 10)",
      "(scale 5)",
    ], sessionDir);

    expect(lines.some(l => l.includes("=>") && l.includes("50"))).toBe(true);
  });

  it("trace output shows depth and source correctly", async () => {
    await runRepl([
      "(define (nested x) (+ x 1))",
      "(nested (nested 1))",
      ":session save trace-test",
    ], sessionDir);

    const { raw } = await runRepl([
      ":session load trace-test",
      ":session trace",
    ], sessionDir);

    expect(raw).toMatch(/\[\d+\]\s+REPL/);
    expect(raw).toMatch(/\[\d+\]\s+EVAL/);
    expect(raw).toMatch(/\[\d+\]\s+OUT/);
  });

  it("full state round-trips through serialization", async () => {
    await runRepl([
      "(define acc '())",
      "(define (push x) (set! acc (cons x acc)))",
      "(push 1)",
      "(push 2)",
      "(push 3)",
      ":session save roundtrip-test",
    ], sessionDir);

    const { lines } = await runRepl([
      ":session load roundtrip-test",
      ":session goto 999",
      "acc",
    ], sessionDir);

    const hasValues = lines.some(l => l.includes("=>") && l.includes("3") && l.includes("2") && l.includes("1"));
    expect(hasValues).toBe(true);
  });

  it("isolates parallel repl runs with distinct session dirs", async () => {
    const firstDir = createSessionDir();
    const secondDir = createSessionDir();

    try {
      await Promise.all([
        runRepl(["(define x 1)", "(+ x 1)"], firstDir),
        runRepl(["(define y 2)", "(+ y 2)"], secondDir),
      ]);

      expect(fs.existsSync(path.join(firstDir, "sessions", "current.jsonl"))).toBe(true);
      expect(fs.existsSync(path.join(secondDir, "sessions", "current.jsonl"))).toBe(true);
    } finally {
      cleanupSessionDir(firstDir);
      cleanupSessionDir(secondDir);
    }
  });
});
