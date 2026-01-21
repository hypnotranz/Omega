import type { Diagnostic } from "../../outcome/diagnostic";
import { errorDiag } from "../../outcome/diagnostic";
import type { FlowIR } from "../../frameir/flow";
import type { FnDefIR } from "../../frameir/bundle";
import type { PromptIR } from "../../frameir/prompt";
import type { ValueIR, VRef, VRecord, VList } from "../../frameir/value";
import { CURRENT_IR_VERSION } from "../../frameir/version";
import type { CoreForm, LowerEnv, LowerResult, ValueLiteral } from "./types";

const ERR_UNKNOWN = "E0001";
const ERR_TOOL_CONTRACT = "E0303";

export function createLowerEnv(): LowerEnv {
  return {
    fnDefs: new Map(),
    schemas: new Map(),
    toolContracts: new Map(),
    globals: new Map(),
    capturedVars: new Set(),
  };
}

export function lowerCoreForm(form: CoreForm, env: LowerEnv): LowerResult {
  const diagnostics: Diagnostic[] = [];

  if ((form as any).tag === "literal") {
    return { ok: true, ir: lowerLiteral(form as unknown as ValueLiteral), fnDefs: [], diagnostics };
  }

  switch (form.tag) {
    case "quote": {
      const value = lowerLiteral(form.args[0] as ValueLiteral);
      return { ok: true, ir: value, fnDefs: [], diagnostics };
    }
    case "pure": {
      const value = lowerCoreForm(form.args[0] as CoreForm, env);
      diagnostics.push(...value.diagnostics);
      return {
        ok: diagnostics.length === 0 && value.ok,
        ir: { v: CURRENT_IR_VERSION, tag: "FPure", value: value.ir as ValueIR, meta: { span: spanFor(form) } },
        fnDefs: value.fnDefs,
        diagnostics,
      };
    }
    case "bind": {
      const flowRes = lowerCoreForm(form.args[0] as CoreForm, env);
      const lambdaRes = lowerCoreForm(form.args[1] as CoreForm, env);
      diagnostics.push(...flowRes.diagnostics, ...lambdaRes.diagnostics);

      const fnDef = extractFnDef(lambdaRes);
      if (!fnDef) {
        diagnostics.push(errorDiag(ERR_UNKNOWN, "Bind expects lambda as continuation", { span: form.meta.span }));
        return { ok: false, ir: flowRes.ir as any, fnDefs: [...flowRes.fnDefs, ...lambdaRes.fnDefs], diagnostics };
      }

      const k: VRef = { v: CURRENT_IR_VERSION, tag: "VRef", ref: { kind: "Fn", id: fnDef.fnId } };
      const ir: FlowIR = {
        v: CURRENT_IR_VERSION,
        tag: "FBind",
        flow: flowRes.ir as FlowIR,
        k,
        meta: { span: spanFor(form) },
      };

      return {
        ok: diagnostics.length === 0 && flowRes.ok && lambdaRes.ok,
        ir,
        fnDefs: [...flowRes.fnDefs, ...lambdaRes.fnDefs],
        diagnostics,
      };
    }
    case "infer": {
      const promptRes = lowerCoreForm(form.args[0] as CoreForm, env);
      const opts = form.args[1] ? lowerCoreForm(form.args[1] as CoreForm, env) : null;
      diagnostics.push(...promptRes.diagnostics, ...(opts?.diagnostics ?? []));
      return {
        ok: diagnostics.length === 0 && promptRes.ok && (opts?.ok ?? true),
        ir: {
          v: CURRENT_IR_VERSION,
          tag: "FInfer",
          prompt: promptRes.ir as PromptIR,
          options: opts?.ir as ValueIR | undefined,
          meta: { span: spanFor(form) },
        },
        fnDefs: [...promptRes.fnDefs, ...(opts?.fnDefs ?? [])],
        diagnostics,
      };
    }
    case "tool-call": {
      const tool = lowerCoreForm(form.args[0] as CoreForm, env);
      const args = lowerCoreForm(form.args[1] as CoreForm, env);
      diagnostics.push(...tool.diagnostics, ...args.diagnostics);

      let contract: VRef | undefined;
      if (form.args[2]) {
        const name = (form.args[2] as any)?.args?.[0]?.value ?? (form.args[2] as any)?.value;
        if (!env.toolContracts.has(name)) {
          diagnostics.push(errorDiag(ERR_TOOL_CONTRACT, `Unknown tool contract: ${name}`, { span: form.meta.span }));
        } else {
          contract = { v: CURRENT_IR_VERSION, tag: "VRef", ref: { kind: "ToolContract", id: String(name) } };
        }
      }

      return {
        ok: diagnostics.length === 0 && tool.ok && args.ok,
        ir: {
          v: CURRENT_IR_VERSION,
          tag: "FToolCall",
          tool: tool.ir as ValueIR,
          args: args.ir as ValueIR,
          contract,
          meta: { span: spanFor(form) },
        },
        fnDefs: [...tool.fnDefs, ...args.fnDefs],
        diagnostics,
      };
    }
    case "with-budget":
    case "with-timeout": {
      const budgetRes = lowerCoreForm(form.args[0] as CoreForm, env);
      const flowRes = lowerCoreForm(form.args[1] as CoreForm, env);
      diagnostics.push(...budgetRes.diagnostics, ...flowRes.diagnostics);
      const base = {
        ok: diagnostics.length === 0 && budgetRes.ok && flowRes.ok,
        fnDefs: [...budgetRes.fnDefs, ...flowRes.fnDefs],
        diagnostics,
      };
      if (form.tag === "with-budget") {
        return {
          ...base,
          ir: {
            v: CURRENT_IR_VERSION,
            tag: "FWithBudget",
            budget: budgetRes.ir as ValueIR,
            flow: flowRes.ir as FlowIR,
            meta: { span: spanFor(form) },
          },
        };
      }
      return {
        ...base,
        ir: {
          v: CURRENT_IR_VERSION,
          tag: "FWithTimeout",
          ms: budgetRes.ir as ValueIR,
          flow: flowRes.ir as FlowIR,
          meta: { span: spanFor(form) },
        } as any,
      };
    }
    case "fail": {
      const reason = lowerCoreForm(form.args[0] as CoreForm, env);
      diagnostics.push(...reason.diagnostics);
      return {
        ok: diagnostics.length === 0 && reason.ok,
        ir: { v: CURRENT_IR_VERSION, tag: "FFail", reason: reason.ir as ValueIR, meta: { span: spanFor(form) } },
        fnDefs: reason.fnDefs,
        diagnostics,
      };
    }
    case "branch": {
      const [predForm, thenForm, elseForm] = form.args as CoreForm[];
      const pred = lowerCoreForm(predForm, env);
      const thn = lowerCoreForm(thenForm, env);
      const els = lowerCoreForm(elseForm, env);
      diagnostics.push(...pred.diagnostics, ...thn.diagnostics, ...els.diagnostics);
      return {
        ok: diagnostics.length === 0 && pred.ok && thn.ok && els.ok,
        ir: {
          v: CURRENT_IR_VERSION,
          tag: "FBranch",
          pred: pred.ir as ValueIR,
          then: thn.ir as FlowIR,
          else: els.ir as FlowIR,
          meta: { span: spanFor(form) },
        },
        fnDefs: [...pred.fnDefs, ...thn.fnDefs, ...els.fnDefs],
        diagnostics,
      };
    }
    case "sequence": {
      const children = form.args.map(arg => lowerCoreForm(arg as CoreForm, env));
      children.forEach(r => diagnostics.push(...r.diagnostics));
      return {
        ok: diagnostics.length === 0 && children.every(r => r.ok),
        ir: { v: CURRENT_IR_VERSION, tag: "FSequence", flows: children.map(r => r.ir as FlowIR), meta: { span: spanFor(form) } },
        fnDefs: children.flatMap(r => r.fnDefs),
        diagnostics,
      };
    }
    case "lambda": {
      const paramsLit = form.args[0] as ValueLiteral;
      const bodyForm = form.args[1] as CoreForm;
      const params = Array.isArray(paramsLit.value) ? (paramsLit.value as string[]) : [];
      const bodyRes = lowerCoreForm(bodyForm, env);
      diagnostics.push(...bodyRes.diagnostics);
      const fnId = `fn:${env.fnDefs.size}`;
      const captures = computeCaptures(bodyRes.ir as any, params, env);
      const fn: FnDefIR = {
        v: CURRENT_IR_VERSION,
        tag: "FnDef",
        fnId,
        params,
        body: bodyRes.ir as FlowIR | ValueIR,
        captures,
        meta: { span: spanFor(form) },
      };
      return {
        ok: diagnostics.length === 0 && bodyRes.ok,
        ir: { v: CURRENT_IR_VERSION, tag: "VRef", ref: { kind: "Fn", id: fnId } },
        fnDefs: [...bodyRes.fnDefs, fn],
        diagnostics,
      };
    }
    default:
      diagnostics.push(errorDiag(ERR_UNKNOWN, `Unknown core form: ${form.tag}`, { span: form.meta.span }));
      return { ok: false, ir: null as any, fnDefs: [], diagnostics };
  }
}

function lowerLiteral(lit: ValueLiteral): ValueIR {
  const value = lit.value;
  if (typeof value === "number") {
    return { v: CURRENT_IR_VERSION, tag: "VInt", value: String(value) };
  }
  if (typeof value === "boolean") {
    return { v: CURRENT_IR_VERSION, tag: "VBool", value };
  }
  if (value === null || typeof value === "undefined") {
    return { v: CURRENT_IR_VERSION, tag: "VNil" };
  }
  if (typeof value === "string") {
    if (value.startsWith(":")) return { v: CURRENT_IR_VERSION, tag: "VKeyword", name: value.slice(1) } as any;
    return { v: CURRENT_IR_VERSION, tag: "VStr", value };
  }
  if (Array.isArray(value)) {
    const items = value.map(v => lowerLiteral({ tag: "literal", meta: lit.meta, value: v }));
    return { v: CURRENT_IR_VERSION, tag: "VList", items } as VList;
  }
  if (typeof value === "object" && "tag" in (value as any)) {
    return value as ValueIR;
  }
  if (typeof value === "object") {
    const entries = Object.entries(value as Record<string, unknown>).map(([k, v]) => ({
      k: { v: CURRENT_IR_VERSION, tag: "VKeyword", name: k } as any,
      v: lowerLiteral({ tag: "literal", meta: lit.meta, value: v }),
    }));
    return { v: CURRENT_IR_VERSION, tag: "VRecord", entries } as VRecord;
  }
  return { v: CURRENT_IR_VERSION, tag: "VStr", value: String(value) };
}

function extractFnDef(res: LowerResult): FnDefIR | null {
  const fn = res.fnDefs.find(def => def.tag === "FnDef");
  return fn ?? null;
}

function computeCaptures(body: FlowIR | ValueIR, params: string[], env: LowerEnv): ValueIR | undefined {
  // Simple capture: use globals present in env.globals but not params
  const captures: Record<string, ValueIR> = {};
  for (const [name, value] of env.globals.entries()) {
    if (!params.includes(name)) {
      captures[name] = value;
    }
  }
  const entries = Object.entries(captures).map(([k, v]) => ({
    k: { v: CURRENT_IR_VERSION, tag: "VSymbol", name: k } as any,
    v,
  }));
  if (entries.length === 0) return undefined;
  return { v: CURRENT_IR_VERSION, tag: "VRecord", entries } as VRecord;
}

function spanFor(form: CoreForm | ValueLiteral): any {
  const meta: any = (form as any).meta;
  if (meta?.originalForm?.children?.[0]?.meta?.span) {
    return meta.originalForm.children[0].meta.span;
  }
  return meta?.span ?? undefined;
}
