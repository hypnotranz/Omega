// Shared helpers for chapter demos
import { pathToFileURL } from "url";
import type {
  DemoContext,
  DemoDefinition,
  DemoMetrics,
  DemoResult,
  InvariantSpec,
  OracleScriptEntry,
} from "../harness/types";
import { runDemo } from "../harness/runner";

export function collectMetrics(ctx: DemoContext, steps = 0, startedAt?: number): DemoMetrics {
  return {
    inferCalls: ctx.oracle.getCount("InferOp"),
    oracleReqEval: ctx.oracle.getCount("ReqEval"),
    oracleReqApply: ctx.oracle.getCount("ReqApply"),
    oracleReqObserve: ctx.oracle.getCount("ReqObserve"),
    oracleReqTest: ctx.oracle.getCount("ReqTest"),
    oracleReqReturn: ctx.oracle.getCount("ReqReturn"),
    steps,
    wallMs: startedAt ? Date.now() - startedAt : 0,
    scheduleDecisions: ctx.ledger.getEventsByType("schedule.decision").length,
    ambChoices: ctx.ledger.getEventsByType("amb.choose").length,
    backtracks: ctx.ledger.getEventsByType("amb.backtrack").length,
    commits:
      ctx.ledger.getEventsByType("commit.success").length +
      ctx.ledger.getEventsByType("commit.denied").length,
    genericMiss: ctx.ledger.getEventsByType("generic.miss").length,
    methodsInstalled: ctx.ledger.getEventsByType("generic.install").length,
    macroExpansions: ctx.ledger.getEventsByType("macro.expand").length,
  };
}

export function basicSuccessInvariant(name = "demo-succeeds"): InvariantSpec {
  return {
    name,
    check: (result: DemoResult) => ({
      name,
      ok: result.success,
      detail: result.success ? "demo succeeded" : `error: ${result.error?.message ?? "unknown"}`,
    }),
  };
}

export async function runIfDirect(demo: DemoDefinition, moduleUrl: string): Promise<void> {
  const invoked = pathToFileURL(process.argv[1] ?? "").href;
  if (moduleUrl === invoked) {
    const report = await runDemo(demo, "pragmatic", 7);
    // eslint-disable-next-line no-console
    console.log(JSON.stringify(report, null, 2));
  }
}

export function scriptedInfer(
  mapping: Record<string, unknown>,
  defaultValue: unknown = "unhandled"
): OracleScriptEntry {
  return {
    match: (_req, type) => type === "InferOp",
    respond: (req) => {
      const prompt = stringifyReq(req);
      for (const [needle, value] of Object.entries(mapping)) {
        if (prompt.toLowerCase().includes(needle.toLowerCase())) {
          return { value, evidence: `matched:${needle}` };
        }
      }
      return { value: defaultValue, evidence: "default" };
    },
  };
}

function stringifyReq(req: unknown): string {
  if (typeof req === "string") return req;
  if (req && typeof req === "object" && "args" in (req as any)) {
    try {
      const args = (req as any).args ?? [];
      return Array.isArray(args)
        ? args
            .map((a: unknown) => (typeof a === "string" ? a : JSON.stringify(a)))
            .join(" ")
        : JSON.stringify(args);
    } catch {
      return JSON.stringify(req);
    }
  }
  return JSON.stringify(req);
}
