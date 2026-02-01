import type {
  DemoContext,
  DemoDefinition,
  DemoMetrics,
  DemoResult,
  InvariantResult,
  InvariantSpec,
} from "../harness/types";

export type ScenarioResult = {
  outputs: unknown[];
  success: boolean;
  detail?: string;
  steps?: number;
  wallMs?: number;
  ambChoices?: number;
  backtracks?: number;
  scheduleDecisions?: number;
};

export type DemoSpec = {
  id: string;
  number: number;
  title: string;
  description: string;
  tags?: string[];
  setupOracle?: (ctx: DemoContext) => void;
  runScenario: (ctx: DemoContext) => Promise<ScenarioResult>;
  invariants?: InvariantSpec[];
};

function buildMetrics(ctx: DemoContext, overrides?: ScenarioResult): DemoMetrics {
  const counts = ctx.oracle.getCounts();
  return {
    inferCalls: counts.InferOp ?? 0,
    oracleReqEval: counts.ReqEval ?? 0,
    oracleReqApply: counts.ReqApply ?? 0,
    oracleReqObserve: counts.ReqObserve ?? 0,
    oracleReqTest: counts.ReqTest ?? 0,
    oracleReqReturn: counts.ReqReturn ?? 0,
    steps: overrides?.steps ?? 0,
    wallMs: overrides?.wallMs ?? 0,
    scheduleDecisions: overrides?.scheduleDecisions,
    ambChoices: overrides?.ambChoices,
    backtracks: overrides?.backtracks,
  };
}

export function buildDemo(spec: DemoSpec): DemoDefinition {
  const name = `Chapter ${spec.number}: ${spec.title}`;
  const tags = spec.tags ?? [`chapter${String(spec.number).padStart(2, "0")}`];

  async function run(ctx: DemoContext): Promise<DemoResult> {
    spec.setupOracle?.(ctx);
    const scenario = await spec.runScenario(ctx);

    return {
      outputs: scenario.outputs,
      success: scenario.success,
      metrics: buildMetrics(ctx, scenario),
      transcript: ctx.oracle.getTranscript(),
    };
  }

  const defaultInvariant: InvariantSpec = {
    name: "scenario-succeeded",
    check: (result, _ctx): InvariantResult => ({
      name: "scenario-succeeded",
      ok: result.success,
      detail: scenarioDetail(result),
    }),
  };

  return {
    id: spec.id,
    name,
    description: spec.description,
    tags,
    run,
    invariants: [defaultInvariant, ...(spec.invariants ?? [])],
  };
}

function scenarioDetail(result: DemoResult): string | undefined {
  if (result.success) {
    return undefined;
  }
  if (result.error instanceof Error) {
    return result.error.message;
  }
  return typeof result.error === "string" ? result.error : undefined;
}
