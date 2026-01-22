import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { spawn } from "child_process";
import * as fs from "fs";
import * as path from "path";

const SESSION_DIR = ".omega-session-test";

type ReplRun = { raw: string; lines: string[] };

function cleanupSessionDir(): void {
  if (fs.existsSync(SESSION_DIR)) {
    fs.rmSync(SESSION_DIR, { recursive: true });
  }
}

async function runRepl(commands: string[]): Promise<ReplRun> {
  return new Promise((resolve, reject) => {
    const npxCmd = process.platform === "win32" ? "npx.cmd" : "npx";
    const proc = spawn(npxCmd, ["tsx", "bin/omega-repl.ts"], {
      env: {
        ...process.env,
        OMEGA_SESSION_DIR: SESSION_DIR,
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
    }, 40000);

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
        setTimeout(sendNext, 800);
      } else {
        proc.stdin.write(":quit\n");
        proc.stdin.end();
      }
    };

    setTimeout(sendNext, 800);
  });
}

describe("Session integration (REPL)", () => {
  beforeEach(() => cleanupSessionDir());
  afterEach(() => cleanupSessionDir());

  it("auto-saves session to current.jsonl", async () => {
    await runRepl([
      "(define x 42)",
      "(+ x 1)",
    ]);

    const currentFile = path.join(SESSION_DIR, "sessions", "current.jsonl");
    expect(fs.existsSync(currentFile)).toBe(true);

    const content = fs.readFileSync(currentFile, "utf8");
    expect(content).toContain("\"type\":\"input\"");
    expect(content).toContain("define x 42");
  });

  it("explicit save creates named session", async () => {
    await runRepl([
      "(define x 42)",
      ":session save my-test",
    ]);

    const savedFile = path.join(SESSION_DIR, "sessions", "my-test.jsonl");
    const indexFile = path.join(SESSION_DIR, "sessions", "my-test.index.json");

    expect(fs.existsSync(savedFile)).toBe(true);
    expect(fs.existsSync(indexFile)).toBe(true);
  });

  it("load restores environment bindings", async () => {
    await runRepl([
      "(define my-fn (lambda (x) (* x 2)))",
      ":session save env-test",
    ]);

    const { lines } = await runRepl([
      ":session load env-test",
      ":session goto 999",
      "(my-fn 21)",
    ]);

    expect(lines.some(l => l.includes("=>") && l.includes("42"))).toBe(true);
  });

  it("creates checkpoints at LLM boundaries", async () => {
    await runRepl([
      '(effect infer.op "test prompt")',
      ":session save llm-test",
    ]);

    const index = JSON.parse(
      fs.readFileSync(
        path.join(SESSION_DIR, "sessions", "llm-test.index.json"),
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
    ]);

    const index = JSON.parse(
      fs.readFileSync(
        path.join(SESSION_DIR, "sessions", "counter-test.index.json"),
        "utf8"
      )
    );
    const targetSeq = index.checkpoints?.[2]?.seq ?? index.checkpoints?.[index.checkpoints.length - 2]?.seq;

    const { lines } = await runRepl([
      ":session load counter-test",
      `:session goto ${targetSeq ?? 0}`,
      "counter",
    ]);

    expect(lines.some(l => l.includes("=>") && l.includes("2"))).toBe(true);
  });

  it("resume uses cached receipts (no live LLM call)", async () => {
    await runRepl([
      '(effect infer.op "What is 2+2?")',
      ":session save receipt-test",
    ]);

    const index = JSON.parse(
      fs.readFileSync(
        path.join(SESSION_DIR, "sessions", "receipt-test.index.json"),
        "utf8"
      )
    );
    const resumeSeq = index.checkpoints?.[0]?.seq ?? 0;

    const { raw } = await runRepl([
      ":session load receipt-test",
      `:session goto ${resumeSeq}`,
      ":session resume",
    ]);

    expect(/cached/i.test(raw)).toBe(true);
  });

  it("what-if: jump then run different code with same env", async () => {
    await runRepl([
      "(define (double x) (* x 2))",
      "(double 5)",
      ":session save what-if-test",
    ]);

    const index = JSON.parse(
      fs.readFileSync(
        path.join(SESSION_DIR, "sessions", "what-if-test.index.json"),
        "utf8"
      )
    );
    const seq = index.checkpoints?.[0]?.seq ?? 0;

    const { lines } = await runRepl([
      ":session load what-if-test",
      `:session goto ${seq}`,
      "(double 100)",
    ]);

    expect(lines.some(l => l.includes("=>") && l.includes("200"))).toBe(true);
  });

  it("what-if: modify env at checkpoint then continue", async () => {
    await runRepl([
      "(define multiplier 2)",
      "(define (scale x) (* x multiplier))",
      "(scale 5)",
      ":session save modify-env-test",
    ]);

    const index = JSON.parse(
      fs.readFileSync(
        path.join(SESSION_DIR, "sessions", "modify-env-test.index.json"),
        "utf8"
      )
    );
    const seq = index.checkpoints?.[1]?.seq ?? index.checkpoints?.[0]?.seq ?? 0;

    const { lines } = await runRepl([
      ":session load modify-env-test",
      `:session goto ${seq}`,
      "(set! multiplier 10)",
      "(scale 5)",
    ]);

    expect(lines.some(l => l.includes("=>") && l.includes("50"))).toBe(true);
  });

  it("trace output shows depth and source correctly", async () => {
    await runRepl([
      "(define (nested x) (+ x 1))",
      "(nested (nested 1))",
      ":session save trace-test",
    ]);

    const { raw } = await runRepl([
      ":session load trace-test",
      ":session trace",
    ]);

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
    ]);

    const { lines } = await runRepl([
      ":session load roundtrip-test",
      ":session goto 999",
      "acc",
    ]);

    const hasValues = lines.some(l => l.includes("=>") && l.includes("3") && l.includes("2") && l.includes("1"));
    expect(hasValues).toBe(true);
  });
});
