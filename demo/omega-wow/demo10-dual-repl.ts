// test/llm_repl_demo.ts
// Demo: LLM speaks s-expressions to the evaluator
// The "dual REPL" - both user and LLM use the same evaluator via Lisp commands

import { COWStore } from "../src/core/eval/store";
import { RuntimeImpl } from "../src/core/effects/runtimeImpl";
import { installPrims } from "./helpers/prims";
import type { State } from "../src/core/eval/machine";
import { runToCompletion } from "../src/core/eval/run";
import type { Val } from "../src/core/eval/values";
import { compileTextToExpr } from "../src/core/pipeline/compileText";
import { SnapshotRepo } from "../src/core/oracle/snapshots";
import { InMemoryReceiptStore } from "../src/core/oracle/receipts";
import type { OracleAdapter, OracleInit } from "../src/core/oracle/adapter";
import type { OracleSession, OracleReq, OracleResp, QExpr } from "../src/core/oracle/protocol";
import { meaning, type MeaningVal } from "../src/core/oracle/meaning";
import * as fs from "fs";
import * as path from "path";

// ═══════════════════════════════════════════════════════════════════════════
// CONFIG
// ═══════════════════════════════════════════════════════════════════════════

function loadApiKey(): string | undefined {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  try {
    const configPath = path.join(process.cwd(), "../LambdaRLM/config.yaml");
    const content = fs.readFileSync(configPath, "utf8");
    const match = content.match(/api_key:\s*(\S+)/);
    if (match?.[1]) return match[1];
  } catch { }
  return undefined;
}

// ═══════════════════════════════════════════════════════════════════════════
// S-EXPRESSION PARSER (minimal)
// ═══════════════════════════════════════════════════════════════════════════

type SExpr = string | number | SExpr[];

function parseSExpr(s: string): SExpr {
  s = s.trim();
  if (s.startsWith("'")) {
    return ["quote", parseSExpr(s.slice(1))];
  }
  if (!s.startsWith("(")) {
    // atom
    if (/^-?\d+(\.\d+)?$/.test(s)) return parseFloat(s);
    return s;
  }
  // list
  const items: SExpr[] = [];
  let depth = 0;
  let start = 1;
  let inString = false;
  for (let i = 1; i < s.length; i++) {
    const c = s[i];
    if (c === '"' && s[i-1] !== '\\') inString = !inString;
    if (inString) continue;
    if (c === '(') depth++;
    else if (c === ')') {
      if (depth === 0) {
        const sub = s.slice(start, i).trim();
        if (sub) items.push(parseSExpr(sub));
        break;
      }
      depth--;
    } else if (c === ' ' || c === '\n' || c === '\t') {
      if (depth === 0) {
        const sub = s.slice(start, i).trim();
        if (sub) items.push(parseSExpr(sub));
        start = i + 1;
      }
    }
  }
  return items;
}

function sexprToString(e: SExpr): string {
  if (e === undefined || e === null) return "nil";
  if (typeof e === "number") return String(e);
  if (typeof e === "string") return e;
  if (!Array.isArray(e)) return String(e);
  return "(" + e.map(sexprToString).join(" ") + ")";
}

// ═══════════════════════════════════════════════════════════════════════════
// CONVERT ORACLE REQ/RESP TO S-EXPRESSIONS
// ═══════════════════════════════════════════════════════════════════════════

function valToSExpr(v: Val): SExpr {
  switch (v.tag) {
    case "Num": return v.n;
    case "Str": return `"${v.s}"`;
    case "Bool": return v.b ? "#t" : "#f";
    case "Unit": return "unit";
    case "Sym": return v.name;
    case "Vector": return v.items.map(valToSExpr);
    case "Closure": return "<closure>";
    case "Native": return `<prim:${v.name}>`;
    default: return `<${v.tag}>`;
  }
}

function respToSExpr(resp: OracleResp): string {
  switch (resp.tag) {
    case "RespVal":
      return `(resp-val ${valToSExpr(resp.value)} envRef=${resp.envRef ?? "?"})`;
    case "RespObs":
      return `(resp-observe ${JSON.stringify(resp.data)})`;
    case "RespError":
      return `(resp-error "${resp.message}")`;
    case "RespAck":
      return "(resp-ack)";
    default:
      return `(resp-unknown ${(resp as any).tag})`;
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// PARSE LLM OUTPUT TO ORACLE REQ
// ═══════════════════════════════════════════════════════════════════════════

function parseOracleCommand(sexpr: SExpr, envRef: string): OracleReq | null {
  if (!Array.isArray(sexpr) || sexpr.length === 0) return null;

  const cmd = sexpr[0];

  if (cmd === "req-eval") {
    // (req-eval '(define foo ...) envRef=E0)
    let qexpr = sexpr[1];
    if (Array.isArray(qexpr) && qexpr[0] === "quote") {
      qexpr = qexpr[1];
    }
    return {
      tag: "ReqEval",
      qexpr: sexprToString(qexpr) as unknown as QExpr,
      envRef,
    };
  }

  if (cmd === "req-test") {
    // (req-test '((= (foo 0) 1) ...) envRef=E1)
    let tests = sexpr[1];
    if (Array.isArray(tests) && tests[0] === "quote") {
      tests = tests[1];
    }
    return {
      tag: "ReqTest",
      spec: { tag: "OmegaTests", cases: tests },
      envRef,
    };
  }

  if (cmd === "req-snapshot") {
    return {
      tag: "ReqSnapshot",
      envRef,
      meta: {},
    };
  }

  if (cmd === "req-hydrate") {
    const rid = String(sexpr[1] ?? "");
    return {
      tag: "ReqHydrate",
      receiptRef: rid,
    };
  }

  if (cmd === "req-return") {
    // (req-return (meaning (denotation ...) (confidence 0.9)))
    const meaningExpr = sexpr[1];
    return {
      tag: "ReqReturn",
      result: parseMeaning(meaningExpr),
    };
  }

  if (cmd === "req-observe") {
    return {
      tag: "ReqObserve",
      what: { tag: "Control" },
      stateRef: envRef,
    };
  }

  return null;
}

function parseMeaning(sexpr: SExpr): MeaningVal {
  if (!Array.isArray(sexpr)) {
    return meaning({ denotation: { tag: "Str", s: String(sexpr) }, confidence: 0.5 });
  }

  const m: Partial<MeaningVal> = { tag: "Meaning" };

  for (let i = 1; i < sexpr.length; i++) {
    const item = sexpr[i];
    if (Array.isArray(item) && item.length >= 2) {
      const key = item[0];
      const val = item.slice(1);
      if (key === "denotation" || key === "rewrite") {
        (m as any)[key] = { tag: "Str", s: sexprToString(val.length === 1 ? val[0] : val) };
      } else if (key === "confidence") {
        m.confidence = typeof val[0] === "number" ? val[0] : parseFloat(String(val[0]));
      } else if (key === "evidence") {
        m.evidence = { tag: "Str", s: sexprToString(val) };
      }
    }
  }

  return meaning(m as any);
}

// ═══════════════════════════════════════════════════════════════════════════
// LLM ORACLE ADAPTER - Makes LLM emit Lisp commands
// ═══════════════════════════════════════════════════════════════════════════

const SYSTEM_PROMPT = `You are an Oracle for the Omega Lisp evaluator.
You communicate by emitting s-expression commands. Available commands:

(req-eval '(begin (define foo ...) (foo 5)) envRef=E0)  ; evaluate with definitions and test inline
(req-return (meaning (rewrite ...) (confidence 0.9)))    ; return final result

The runtime responds with:
(resp-val <value> envRef=E1)
(resp-error "message")

CRITICAL CONSTRAINT:
- ALL definitions and their usage MUST be in a single (begin ...) block
- This is because the Lisp uses hygienic macros - definitions only exist within their compilation unit
- DO NOT split define and usage across multiple req-eval calls

CORRECT pattern:
  (req-eval '(begin
               (define add1 (lambda (x) (+ x 1)))
               (= (add1 5) 6))  ; test inline
             envRef=E0)

WRONG pattern (will fail with "binder did not resolve"):
  (req-eval '(define add1 (lambda (x) (+ x 1))) envRef=E0)  ; defines add1
  (req-eval '(add1 5) envRef=E1)  ; FAILS - add1 not known in new compile unit

Example session:
User task: synthesize add1 that passes (= (add1 0) 1) and (= (add1 5) 6)

oracle> (req-eval '(begin (define add1 (lambda (x) (+ x 1))) (if (= (add1 0) 1) (= (add1 5) 6) #f)) envRef=E0)
runtime> (resp-val #t envRef=E1)
oracle> (req-return (meaning (rewrite (define add1 (lambda (x) (+ x 1)))) (confidence 0.99)))
`;

async function askLLM(messages: Array<{role: string; content: string}>, apiKey: string): Promise<string> {
  const response = await fetch("https://api.openai.com/v1/chat/completions", {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      "Authorization": `Bearer ${apiKey}`,
    },
    body: JSON.stringify({
      model: "gpt-4o",
      messages,
      max_tokens: 500,
      temperature: 0.3,
    }),
  });
  const data = await response.json() as any;
  return data.choices?.[0]?.message?.content ?? "";
}

function extractSExpr(text: string): string | null {
  // Find the start of a command (req-...) and extract balanced parens
  const cmdStart = text.indexOf("(req-");
  const start = cmdStart >= 0 ? cmdStart : text.indexOf("(");
  if (start < 0) return null;

  let depth = 0;
  let inString = false;
  for (let i = start; i < text.length; i++) {
    const c = text[i];
    if (c === '"' && text[i-1] !== '\\') inString = !inString;
    if (inString) continue;
    if (c === "(") depth++;
    else if (c === ")") {
      depth--;
      if (depth === 0) return text.slice(start, i + 1);
    }
  }
  return null;
}

class LispSpeakingOracleAdapter implements OracleAdapter {
  private apiKey: string;
  private transcript: string[] = [];

  constructor(apiKey: string) {
    this.apiKey = apiKey;
  }

  getTranscript(): string[] {
    return this.transcript;
  }

  startSession(init: OracleInit): OracleSession {
    const self = this;
    const apiKey = this.apiKey;

    return (async function* (): OracleSession {
      if (init.tag !== "Infer") {
        return meaning({ denotation: { tag: "Unit" }, confidence: 0 });
      }

      const taskPayload = init.payload;
      let taskDesc = "";
      if (taskPayload.tag === "Str") {
        taskDesc = taskPayload.s;
      } else if (taskPayload.tag === "Map") {
        for (const [k, v] of taskPayload.entries) {
          if (k.tag === "Str" && v.tag === "Str") {
            taskDesc += `${k.s}: ${v.s}\n`;
          }
        }
      }

      self.transcript.push(`; --- oracle session begins (envRef=${init.envRef}) ---`);
      self.transcript.push(`; Task: ${taskDesc.replace(/\n/g, " ").slice(0, 100)}`);

      const messages: Array<{role: string; content: string}> = [
        { role: "system", content: SYSTEM_PROMPT },
        { role: "user", content: `Task: ${taskDesc}\n\nStart by emitting your first command.` },
      ];

      let envRef = init.envRef;
      const maxTurns = 10;

      for (let turn = 0; turn < maxTurns; turn++) {
        // Ask LLM for next command
        const llmOutput = await askLLM(messages, apiKey);
        messages.push({ role: "assistant", content: llmOutput });

        // Extract s-expression
        const sexprStr = extractSExpr(llmOutput);
        if (!sexprStr) {
          self.transcript.push(`; LLM output (no command found): ${llmOutput.slice(0, 100)}`);
          messages.push({ role: "user", content: "Please emit a valid command like (req-eval ...) or (req-return ...)" });
          continue;
        }

        self.transcript.push(`oracle> ${sexprStr}`);

        // Parse command
        const sexpr = parseSExpr(sexprStr);
        const req = parseOracleCommand(sexpr, envRef);

        if (!req) {
          self.transcript.push(`; (could not parse command)`);
          messages.push({ role: "user", content: "Command not recognized. Use req-eval, req-test, req-snapshot, req-hydrate, or req-return." });
          continue;
        }

        // Handle req-return specially
        if (req.tag === "ReqReturn") {
          self.transcript.push(`; --- session complete ---`);
          return req.result;
        }

        // Yield request and get response
        const resp: OracleResp = yield req;

        // Update envRef if response contains one
        if (resp.tag === "RespVal" && resp.envRef) {
          envRef = resp.envRef;
        }

        // Format response as s-expression
        const respStr = respToSExpr(resp);
        self.transcript.push(`runtime> ${respStr}`);

        // Send response to LLM
        messages.push({ role: "user", content: `Runtime response:\n${respStr}\n\nEmit your next command.` });
      }

      self.transcript.push(`; --- session exceeded max turns ---`);
      return meaning({ denotation: { tag: "Unit" }, confidence: 0 });
    })();
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// DEMO
// ═══════════════════════════════════════════════════════════════════════════

async function main() {
  const apiKey = loadApiKey();
  if (!apiKey) {
    console.log("No API key found. Set OPENAI_API_KEY or add to ../LambdaRLM/config.yaml");
    process.exit(1);
  }

  console.log("╔═══════════════════════════════════════════════════════════════╗");
  console.log("║  LLM SPEAKS LISP TO THE REPL                                  ║");
  console.log("║  The dual REPL: user and LLM both speak s-expressions         ║");
  console.log("╚═══════════════════════════════════════════════════════════════╝");
  console.log();

  // Create the oracle adapter
  const oracle = new LispSpeakingOracleAdapter(apiKey);
  const snapshots = new SnapshotRepo();
  const receipts = new InMemoryReceiptStore("off");

  // Create runtime
  const mockCommit = { async commit(): Promise<Val> { return { tag: "Unit" }; } };
  const runtime = new RuntimeImpl(oracle, snapshots, receipts, mockCommit);

  // Initial state with primitives
  const store0 = new COWStore();
  const prim = installPrims(store0);

  // Task: synthesize add1
  console.log("═══════════════════════════════════════════════════════════════");
  console.log("TASK: Synthesize add1 function");
  console.log("Tests: (= (add1 0) 1), (= (add1 5) 6)");
  console.log("═══════════════════════════════════════════════════════════════");
  console.log();

  // Create task payload
  const taskPayload: Val = {
    tag: "Str",
    s: `Synthesize a function 'add1' that adds 1 to its argument.
Tests that must pass:
  (= (add1 0) 1)
  (= (add1 5) 6)

REMEMBER: Put define AND tests in ONE (begin ...) block.
Use (if test1 test2 #f) for AND since 'and' isn't a primitive:
  (req-eval '(begin (define add1 ...) (if (= (add1 0) 1) (= (add1 5) 6) #f)) envRef=E0)

If result is #t, emit req-return with the working code.`
  };

  // Run oracle session
  const envRef = snapshots.putEnv({ env: prim.env, store: prim.store });
  const stateRef = snapshots.putState({ state: { control: { tag: "Val", v: { tag: "Unit" } }, env: prim.env, store: prim.store, kont: [], handlers: [] } });

  const session = oracle.startSession({
    tag: "Infer",
    payload: taskPayload,
    envRef,
    stateRef,
  });

  // Simple portal that handles eval and test
  async function handleReq(req: OracleReq): Promise<OracleResp> {
    if (req.tag === "ReqEval") {
      try {
        // Get current env
        const snap = snapshots.getEnv(req.envRef);

        // Compile and run - wrap in begin to handle define properly
        const code = req.qexpr as string;
        const wrappedCode = code.trim().startsWith("(define")
          ? `(begin ${code} (quote ok))`  // wrap define to return something
          : code;
        const expr = compileTextToExpr(wrappedCode);
        const st: State = {
          control: { tag: "Expr", e: expr },
          env: snap.env,
          store: snap.store,
          kont: [],
          handlers: [],
        };

        const result = await runToCompletion(runtime, st, 10000);

        // Save new env
        const newEnvRef = snapshots.putEnv({ env: st.env, store: st.store });

        return { tag: "RespVal", value: result, envRef: newEnvRef };
      } catch (e: any) {
        return { tag: "RespError", message: e.message };
      }
    }

    if (req.tag === "ReqTest") {
      try {
        const snap = snapshots.getEnv(req.envRef);
        const cases = (req.spec as any).cases;

        if (!Array.isArray(cases)) {
          return { tag: "RespObs", data: { passed: false, error: "invalid test spec" } };
        }

        const failures: string[] = [];

        for (const testCase of cases) {
          // testCase is like ["=", ["add1", 0], 1]
          const testStr = sexprToString(testCase);

          try {
            const expr = compileTextToExpr(testStr);
            const st: State = {
              control: { tag: "Expr", e: expr },
              env: snap.env,
              store: snap.store,
              kont: [],
              handlers: [],
            };

            const result = await runToCompletion(runtime, st, 10000);

            if (result.tag !== "Bool" || !result.b) {
              failures.push(`(case ${testStr} got ${JSON.stringify(result)})`);
            }
          } catch (e: any) {
            failures.push(`(case ${testStr} error: ${e.message})`);
          }
        }

        if (failures.length === 0) {
          return { tag: "RespObs", data: { passed: true, result: "PASS" } };
        } else {
          return { tag: "RespObs", data: { passed: false, result: "FAIL", failures } };
        }
      } catch (e: any) {
        return { tag: "RespError", message: e.message };
      }
    }

    if (req.tag === "ReqSnapshot") {
      const rid = `R${Date.now()}`;
      return { tag: "RespObs", data: { rid } };
    }

    if (req.tag === "ReqHydrate") {
      return { tag: "RespVal", value: { tag: "Unit" }, envRef: req.receiptRef };
    }

    return { tag: "RespAck" };
  }

  // Drive session
  let resp: OracleResp = { tag: "RespAck" };
  let result: MeaningVal | null = null;

  while (true) {
    const step = await session.next(resp);
    if (step.done) {
      result = step.value as MeaningVal;
      break;
    }
    resp = await handleReq(step.value as OracleReq);
  }

  // Show transcript
  console.log("ORACLE TRANSCRIPT (s-expressions):");
  console.log("─".repeat(60));
  for (const line of oracle.getTranscript()) {
    console.log(line);
  }
  console.log("─".repeat(60));
  console.log();

  // Show result
  console.log("RESULT (Meaning):");
  console.log("─".repeat(60));
  if (result) {
    console.log("  tag:", result.tag);
    if (result.denotation) console.log("  denotation:", JSON.stringify(result.denotation));
    if (result.rewrite) console.log("  rewrite:", JSON.stringify(result.rewrite));
    console.log("  confidence:", result.confidence);
  }
  console.log("─".repeat(60));
}

main().catch(e => {
  console.error("Error:", e);
  process.exit(1);
});
