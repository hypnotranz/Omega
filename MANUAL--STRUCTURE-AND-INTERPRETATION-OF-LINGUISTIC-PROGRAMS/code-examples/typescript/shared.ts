// demo/by-chapter/shared.ts
// Shared helpers for chapter demos: runtime bridge, oracle scripting, and demo factory.

import path from "path";
import { pathToFileURL } from "url";
import { OmegaRuntime } from "../../src/runtime";
import type { Val, ListVal } from "../../src/core/eval/values";
import type { OracleAdapter, OracleInit } from "../../src/core/oracle/adapter";
import type { OracleSession } from "../../src/core/oracle/protocol";
import type {
  DemoContext,
  DemoDefinition,
  DemoResult,
  DemoMetrics,
  InvariantSpec,
  InvariantResult,
} from "../harness/types";

// -----------------------------------------------------------------
// Oracle Adapter that forwards to the demo harness oracle
// -----------------------------------------------------------------

function valToPrompt(v: Val): string {
  switch (v.tag) {
    case "Str":
      return v.s;
    case "Sym":
      return v.name;
    case "Num":
      return String(v.n);
    case "Bool":
      return v.b ? "true" : "false";
    case "List":
      return v.elements.map(valToPrompt).join(" ");
    case "Vector":
      return v.items.map(valToPrompt).join(" ");
    case "Map":
      return v.entries
        .map(([k, val]) => `${valToPrompt(k)}:${valToPrompt(val)}`)
        .join(" ");
    case "Meaning":
      return valToPrompt(v.denotation as Val);
    default:
      return JSON.stringify(v);
  }
}

function jsToVal(value: unknown): Val {
  if (value === null || value === undefined) return { tag: "Unit" };
  if (typeof value === "string") return { tag: "Str", s: value };
  if (typeof value === "number") return { tag: "Num", n: value };
  if (typeof value === "boolean") return { tag: "Bool", b: value };

  if (Array.isArray(value)) {
    const elements = value.map(jsToVal);
    return { tag: "List", elements } as ListVal;
  }

  // Plain object -> Map
  if (typeof value === "object") {
    const entries: Array<[Val, Val]> = [];
    for (const [k, v] of Object.entries(value as Record<string, unknown>)) {
      entries.push([jsToVal(k), jsToVal(v)]);
    }
    return { tag: "Map", entries };
  }

  return { tag: "Str", s: String(value) };
}

export function valToNative(value: Val): unknown {
  switch (value.tag) {
    case "Str":
      return value.s;
    case "Bool":
      return value.b;
    case "Num":
      return value.n;
    case "Sym":
      return value.name;
    case "Unit":
      return null;
    case "List":
      return value.elements.map(valToNative);
    case "Vector":
      return value.items.map(valToNative);
    case "Map":
      return Object.fromEntries(value.entries.map(([k, v]) => [valToNative(k), valToNative(v)]));
    case "Meaning":
      return {
        denotation: value.denotation ? valToNative(value.denotation as Val) : undefined,
        confidence: value.confidence,
      };
    case "Dist":
      return {
        kind: value.meta?.kind ?? "search",
        support: value.support.map(item => ({
          value: valToNative(item.v),
          weight: item.w,
        })),
      };
    default:
      return value;
  }
}

class DemoOracleAdapter implements OracleAdapter {
  constructor(private readonly ctx: DemoContext) {}

  startSession(init: OracleInit): OracleSession {
    const ctx = this.ctx;

    if (init.tag === "Infer") {
      const prompt = valToPrompt(init.payload);
      const req = { op: "infer", prompt, args: [valToNative(init.payload)] };

      return (async function* (): OracleSession {
        ctx.ledger.record("infer.call", { prompt });
        const resp = ctx.oracle.handle("InferOp", req) as any;
        const denotation = jsToVal(resp?.value ?? resp ?? { tag: "Unit" });
        ctx.ledger.record("infer.result", { value: denotation });
        yield { tag: "ReqReturn", result: denotation };
        return denotation;
      })();
    }

    const spec = valToPrompt(init.proc);
    const args = init.args.map(valToNative);
    const req = { op: spec, prompt: `${spec}:${args.map(a => String(a)).join("|")}`, args };

    return (async function* (): OracleSession {
      ctx.ledger.record("oracle.request", { op: spec, args });
      const resp = ctx.oracle.handle("InferOp", req) as any;
      const denotation = jsToVal(resp?.value ?? resp ?? { tag: "Unit" });
      ctx.ledger.record("oracle.response", { op: spec, value: denotation });
      yield { tag: "ReqReturn", result: denotation };
      return denotation;
    })();
  }
}

export function createRuntime(ctx: DemoContext): OmegaRuntime {
  const adapter = new DemoOracleAdapter(ctx);
  return new OmegaRuntime({
    adapter,
    maxSteps: ctx.options.maxSteps ?? 200_000,
  });
}

// -----------------------------------------------------------------
// Oracle scripting helpers
// -----------------------------------------------------------------

export function registerCommonOracleScripts(ctx: DemoContext): void {
  const positiveSignals = ["love", "delight", "helpful", "glad", "grateful"];
  const negativeSignals = ["angry", "upset", "frustrated", "refund", "broken", "crash"];

  ctx.oracle.addScript({
    match: (_req, type) => type === "InferOp",
    respond: (req, context) => {
      const prompt = String((req as any).prompt ?? "");
      const op = String((req as any).op ?? "");
      const payloadText = prompt.toLowerCase();
      const args = (req as any).args ?? [];

      // Sentiment
      if (payloadText.includes("sentiment") || op.includes("sentiment")) {
        const joined = `${prompt} ${args.join(" ")}`.toLowerCase();
        const isNegative = negativeSignals.some(sig => joined.includes(sig));
        const label = isNegative ? "negative" : "positive";
        return { value: label, evidence: "sentiment-default" };
      }

      // Tone appropriateness for constraints
      if (payloadText.includes("tone appropriate")) {
        const lower = payloadText.toLowerCase();
        const ok = !(lower.includes("playful") && lower.includes("explain risk"));
        return { value: ok ? "yes" : "no", evidence: "tone-constraint" };
      }

      // Complaint detection
      if (payloadText.includes("complaint") || op.includes("complaint")) {
        const joined = `${prompt} ${args.join(" ")}`.toLowerCase();
        const isComplaint = negativeSignals.some(sig => joined.includes(sig)) || joined.includes("refund");
        return { value: isComplaint ? "yes" : "no", evidence: "complaint-check" };
      }

      // Professional tone checks
      if (payloadText.includes("professional")) {
        return { value: "yes", evidence: "professional-check" };
      }

      // Tone match
      if (payloadText.includes("tone") || op.includes("tone")) {
        if (payloadText.includes("apologetic") || prompt.includes("sorry")) {
          return { value: "yes", evidence: "tone-apologetic" };
        }
        if (payloadText.includes("formal")) {
          return { value: "formal", evidence: "tone-formal" };
        }
        return { value: "friendly", evidence: "tone-default" };
      }

      // Support ticket classification
      if (payloadText.includes("support ticket") || op.includes("classify-ticket")) {
        const text = `${prompt} ${args.join(" ")}`.toLowerCase();
        if (text.includes("crash") || text.includes("error") || text.includes("broken")) {
          return { value: "bug", evidence: "ticket-bug" };
        }
        if (text.includes("feature") || text.includes("new capability")) {
          return { value: "feature-request", evidence: "ticket-feature" };
        }
        if (text.includes("refund") || text.includes("angry") || text.includes("frustrated")) {
          return { value: "complaint", evidence: "ticket-complaint" };
        }
        return { value: "question", evidence: "ticket-question" };
      }

      // Same meaning check
      if (payloadText.includes("same meaning") || op.includes("same-meaning")) {
        const joined = `${prompt} ${args.join(" ")}`.toLowerCase();
        const synonyms = joined.includes("upset") && joined.includes("dissatisfied");
        return { value: synonyms ? "true" : "false", evidence: "meaning-check" };
      }

      // Style conversion / rewrite
      if (payloadText.includes("rewrite") || payloadText.includes("convert")) {
        const target = payloadText.includes("empathetic") ? "empathetic" : payloadText.includes("formal") ? "formal" : "concise";
        const variants = ["gentle", "direct", "warm", "calm"];
        const choice = context.random ? variants[Math.floor(context.random() * variants.length)] : variants[0];
        const source = args[0] ?? prompt;
        return {
          value: `[${choice}/${target}] ${String(source).slice(0, 80)}`,
          evidence: "rewrite-default",
        };
      }

      // Domain-specific summarization
      if (payloadText.includes("summarize") || op.includes("summarize")) {
        return { value: "Summary: key obligations acknowledged with calm tone.", evidence: "summary-default" };
      }

      // Validator checks
      if (payloadText.includes("haiku") || op.includes("is-haiku")) {
        return { value: "yes", evidence: "haiku-pass" };
      }

      // Stream follow-up generation
      if (op.includes("follow-up") || payloadText.includes("follow-up question")) {
        const suggestions = [
          "What impact did this issue have on your workflow?",
          "What timeline do you need for resolution?",
          "Do you consent to a temporary workaround?",
          "Which stakeholder should approve the next step?",
        ];
        const choice = Math.floor(context.random() * suggestions.length);
        return { value: suggestions[choice], evidence: "follow-up" };
      }

      // Fallback echo
      return {
        value: `ack:${prompt.slice(0, 70)}`,
        evidence: "fallback",
      };
    },
  });
}

// -----------------------------------------------------------------
// Demo factory
// -----------------------------------------------------------------

export type ChapterProgram = {
  label: string;
  code: string;
};

export type ChapterConfig = {
  id: string;
  name: string;
  description: string;
  tags: string[];
  programs: ChapterProgram[];
  setupOracle?: (ctx: DemoContext) => void;
  validate?: (outputs: unknown[], ctx: DemoContext) => { ok: boolean; detail?: string };
  extraInvariants?: InvariantSpec[];
};

export function createChapterDemo(config: ChapterConfig): DemoDefinition {
  const run = async (ctx: DemoContext): Promise<DemoResult> => {
    const started = Date.now();
    ctx.oracle.reset?.();
    config.setupOracle?.(ctx);
    registerCommonOracleScripts(ctx);

    const omega = createRuntime(ctx);
    const outputs: unknown[] = [];

    for (const program of config.programs) {
      const result = await omega.eval(`(begin ${program.code})`);
      if (!result.ok) {
        throw new Error(`Program "${program.label}" failed: ${result.error}`);
      }
      outputs.push(valToNative(result.value as Val));
    }

    const validation = config.validate ? config.validate(outputs, ctx) : { ok: true, detail: "" };

    const counts = ctx.oracle.getCounts();
    const metrics: DemoMetrics = {
      inferCalls: counts.InferOp,
      oracleReqEval: counts.ReqEval,
      oracleReqApply: counts.ReqApply,
      oracleReqObserve: counts.ReqObserve,
      oracleReqTest: counts.ReqTest,
      oracleReqReturn: counts.ReqReturn,
      steps: config.programs.length,
      wallMs: Date.now() - started,
      ambChoices: ctx.ledger.getEventsByType("amb.choose").length,
      backtracks: ctx.ledger.getEventsByType("amb.backtrack").length,
      scheduleDecisions: ctx.ledger.getEventsByType("schedule.decision").length,
    };

    return {
      outputs,
      success: validation.ok,
      error: validation.ok ? undefined : new Error(validation.detail ?? "validation failed"),
      metrics,
      transcript: ctx.oracle.getTranscript(),
    };
  };

  const baseInvariants: InvariantSpec[] = [
    {
      name: "demo-success",
      check: (r): InvariantResult => ({
        name: "demo-success",
        ok: r.success,
        detail: r.error?.message,
      }),
    },
    {
      name: "output-count",
      check: (r): InvariantResult => ({
        name: "output-count",
        ok: r.outputs.length === config.programs.length,
        detail: `expected ${config.programs.length} outputs, got ${r.outputs.length}`,
      }),
    },
  ];

  return {
    id: config.id,
    name: config.name,
    description: config.description,
    tags: config.tags,
    run,
    invariants: [...baseInvariants, ...(config.extraInvariants ?? [])],
  };
}

export function formatProgramPath(file: string): string {
  return path.join("demo", "by-chapter", file);
}

export async function runDemoCli(demo: DemoDefinition, moduleUrl: string): Promise<void> {
  const invoked = process.argv[1] ? pathToFileURL(path.resolve(process.argv[1])).href : "";
  if (invoked !== moduleUrl) return;

  const harness = await import("../harness");
  const report = await harness.runDemo(demo, "pragmatic", 7, { verbose: true });
  // eslint-disable-next-line no-console
  console.log(harness.formatReport(report, true));
}
