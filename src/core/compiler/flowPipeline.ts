import type { FlowCompileConfig, FlowCompileResult } from "./types";
import { defaultFlowCompileConfig } from "./pipelineDefaults";
import { readForms } from "./reader";
import { createMacroEnv, expandModule } from "./expander";
import { desugarForm } from "./desugar";
import { createLowerEnv, lowerCoreForm } from "./lower";
import { normalizeFlowIR } from "./normalize";
import { buildSourceMap } from "./sourcemap";
import { CURRENT_IR_VERSION } from "../../frameir/version";
import type { FlowIR } from "../../frameir/flow";
import type { IRBundle } from "../../frameir/bundle";

export interface FlowCompilePipeline {
  compile(source: string, config?: Partial<FlowCompileConfig>): FlowCompileResult;
}

export function createFlowPipeline(): FlowCompilePipeline {
  return {
    compile(source: string, config?: Partial<FlowCompileConfig>) {
      return compileSource(source, { ...defaultFlowCompileConfig, ...config });
    },
  };
}

function compileSource(source: string, config: FlowCompileConfig): FlowCompileResult {
  const phases: FlowCompileResult["phases"] = {};
  const diagnostics: FlowCompileResult["diagnostics"] = [];

  // Read
  const readResult = readForms(source);
  phases.read = readResult;
  diagnostics.push(...readResult.diagnostics);
  if (!readResult.ok) return { ok: false, diagnostics, phases };

  // Expand
  const macroEnv = createMacroEnv();
  const expandResults = expandModule(readResult.forms, macroEnv);
  expandResults.forEach(r => diagnostics.push(...r.diagnostics));
  if (expandResults.some(r => !r.ok)) {
    phases.expand = expandResults[0];
    return { ok: false, diagnostics, phases };
  }
  const expandedForms = expandResults.map(r => r.form);
  phases.expand = expandResults[0];

  // Desugar (single form support for now)
  const desugarResults = expandedForms.map(f => desugarForm(f));
  desugarResults.forEach(r => diagnostics.push(...r.diagnostics));
  if (desugarResults.some(r => !r.ok)) {
    phases.desugar = desugarResults[0];
    return { ok: false, diagnostics, phases };
  }
  const coreForm = desugarResults[0].coreForm;
  phases.desugar = desugarResults[0];

  // Lower
  const lowerEnv = createLowerEnv();
  const lowerResult = lowerCoreForm(coreForm, lowerEnv);
  phases.lower = lowerResult;
  diagnostics.push(...lowerResult.diagnostics);
  if (!lowerResult.ok || (lowerResult.ir as FlowIR).tag?.startsWith?.("V")) {
    return { ok: false, diagnostics, phases };
  }

  let flow = lowerResult.ir as FlowIR;

  // Normalize
  const normalizeResult = normalizeFlowIR(flow, config.normalize);
  phases.normalize = normalizeResult;
  diagnostics.push(...normalizeResult.diagnostics);
  if (!normalizeResult.ok) return { ok: false, diagnostics, phases };
  flow = normalizeResult.ir;

  const bundle = buildBundle(flow, lowerResult.fnDefs);
  const sourceMap = config.sourceMap ? buildSourceMap(flow, lowerResult.fnDefs) : undefined;

  return {
    ok: diagnostics.length === 0,
    bundle,
    sourceMap,
    diagnostics,
    phases,
  };
}

function buildBundle(entry: FlowIR, fnDefs: IRBundle["fns"][string][]): IRBundle {
  const fns: IRBundle["fns"] = {};
  for (const fn of fnDefs) {
    fns[fn.fnId] = fn;
  }
  return {
    v: CURRENT_IR_VERSION,
    entry,
    fns,
    schemas: {},
    toolContracts: {},
  };
}
