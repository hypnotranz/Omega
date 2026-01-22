import type { Expr, Pattern } from "../ast";
import type { ConditionHandler, RestartBinding, RestartPoint, ConditionVal } from "../conditions/types";
import type { Ctx } from "../ctx/ctx";
import { isCtx } from "../ctx/ctx";
import type { Resumption } from "../effects/opcall";
import type { HandlerFrame, Frame, State, Control } from "../eval/machine";
import type { Store } from "../eval/store";
import { COWStore } from "../eval/store";
import type { DistVal } from "../eval/dist";
import type { MeaningVal, Obligation, RewriteStep } from "../oracle/meaning";
import type { Val, SolverVal } from "../eval/values";
import type { CapSet } from "../governance/caps";
import type { Profile } from "../governance/profile";
import type { Hash } from "../artifacts/hash";

export type SerializedVal = { tag: string; [key: string]: unknown };

export type SerializedCtx = {
  id: string;
  parentId: string | null;
  frameEntries: Array<[string, number]>;
  profile: string;
  caps: CapSet;
  budgets: Record<string, unknown>;
  constraints: unknown[];
  sealed: boolean;
  evidence: unknown[];
};

export type SerializedControl =
  | { tag: "Expr"; e: Expr }
  | { tag: "Val"; v: SerializedVal };

export type SerializedHandlerFrame = {
  hid: string;
  envId: string;
  on: [string, { op: string; params: string[]; k: string; body: Expr }][];
  ret?: { v: string; body: Expr };
  fin?: { body: Expr };
};

export type SerializedFrame = { tag: string; [key: string]: unknown };

export type SerializedResumption = {
  rid: string;
  baseState: SerializedState;
};

export type SerializedState = {
  control: SerializedControl;
  envId: string | null;
  storeEntries: Array<[number, SerializedVal]>;
  kont: SerializedFrame[];
  handlers: SerializedHandlerFrame[];
  ctxTable: Record<string, SerializedCtx>;
};

export function serializeState(state: State): SerializedState {
  const ctxTable: Record<string, SerializedCtx> = {};

  function collectCtx(ctx: Ctx): string {
    const id = (ctx as any).cid ?? `ctx-${Object.keys(ctxTable).length}`;
    if (ctxTable[id]) return id;

    const entry: SerializedCtx = {
      id,
      parentId: null,
      frameEntries: Array.from(ctx.frame?.entries?.() ?? []),
      profile: (ctx as any).profile ?? "",
      caps: Array.from((ctx as any).caps ?? []) as CapSet,
      budgets: (ctx as any).budgets ?? {},
      constraints: (ctx as any).constraints ?? [],
      sealed: !!(ctx as any).sealed,
      evidence: (ctx as any).evidence ?? [],
    };
    ctxTable[id] = entry;
    entry.parentId = ctx.parent ? collectCtx(ctx.parent) : null;

    return id;
  }

  function serializeMeaning(m: MeaningVal): SerializedVal {
    const serializeIfVal = (v: unknown) => (v && typeof v === "object" && (v as any).tag ? serializeVal(v as Val) : v);
    const mapRewriteStep = (step: RewriteStep): RewriteStep => ({
      ...step,
      before: serializeIfVal(step.before) as any,
      after: serializeIfVal(step.after) as any,
    });

    return {
      tag: "Meaning",
      denotation: serializeIfVal(m.denotation),
      residual: serializeIfVal(m.residual),
      rewrite: serializeIfVal(m.rewrite),
      invariants: serializeIfVal((m as any).invariants),
      effects: serializeIfVal((m as any).effects),
      cost: serializeIfVal((m as any).cost),
      paths: serializeIfVal((m as any).paths),
      deps: serializeIfVal((m as any).deps),
      memo: serializeIfVal((m as any).memo),
      obligation: serializeIfVal((m as any).obligation),
      obligations: (m.obligations as Obligation[] | undefined)?.map(o => ({
        ...o,
        domain: serializeIfVal((o as any).domain),
      })),
      evidence: (m.evidence as any) ?? undefined,
      confidence: m.confidence,
      trace: Array.isArray(m.trace) ? (m.trace as RewriteStep[]).map(mapRewriteStep) : serializeIfVal(m.trace as Val),
      adoptEnvRef: (m as any).adoptEnvRef,
      adoptStateRef: (m as any).adoptStateRef,
    };
  }

  function serializeExplanation(expl: any): SerializedVal {
    if (!expl || typeof expl !== "object") return { tag: "Explanation", kind: "unknown" } as SerializedVal;
    if (expl.tag !== "Explanation") return expl as SerializedVal;
    if (expl.kind === "assumption") {
      return {
        tag: "Explanation",
        kind: "assumption",
        conn: serializeVal(expl.conn),
        valueHash: expl.valueHash,
        because: serializeVal(expl.because as Val),
      } as SerializedVal;
    }
    if (expl.kind === "derived") {
      return {
        tag: "Explanation",
        kind: "derived",
        conn: serializeVal(expl.conn),
        valueHash: expl.valueHash,
        rule: expl.rule,
        deps: (expl.deps ?? []).map((d: any) => serializeExplanation(d)),
      } as SerializedVal;
    }
    if (expl.kind === "conflict") {
      return {
        tag: "Explanation",
        kind: "conflict",
        conn: serializeVal(expl.conn),
        left: serializeExplanation(expl.left),
        right: serializeExplanation(expl.right),
        message: expl.message,
      } as SerializedVal;
    }
    if (expl.kind === "denied") {
      return { tag: "Explanation", kind: "denied", op: expl.op, reason: expl.reason, profile: expl.profile } as SerializedVal;
    }
    return { tag: "Explanation", kind: expl.kind } as SerializedVal;
  }

  function serializeCondition(cond: ConditionVal): SerializedVal {
    return {
      tag: "Condition",
      type: cond.type && typeof cond.type === "symbol" ? cond.type.description ?? String(cond.type) : String(cond.type),
      message: cond.message,
      data: serializeVal(cond.data),
      restarts: cond.restarts.map(r => ({
        name: r.name && typeof r.name === "symbol" ? r.name.description ?? String(r.name) : String(r.name),
        description: r.description,
        kont: r.kont.map(serializeFrame),
        envId: isCtx(r.env) ? collectCtx(r.env) : "unknown-env",
        storeEntries: serializeStore(r.store),
        handlers: r.handlers.map(serializeHandler),
      })),
    } as SerializedVal;
  }

  function serializeStore(store: Store): Array<[number, SerializedVal]> {
    const entries: Array<[number, SerializedVal]> = [];
    for (let addr = 0; addr < store.next; addr++) {
      try {
        const val = store.read(addr);
        entries.push([addr, serializeVal(val)]);
      } catch {
        // Skip invalid addresses
      }
    }
    return entries;
  }

  function serializeHandler(h: HandlerFrame): SerializedHandlerFrame {
    return {
      hid: h.hid,
      envId: isCtx(h.env) ? collectCtx(h.env) : "unknown-env",
      on: Array.from(h.on?.entries?.() ?? []),
      ret: h.ret,
      fin: h.fin,
    };
  }

  function serializeResumption(resumption: Resumption): SerializedResumption {
    return {
      rid: resumption.rid,
      baseState: serializeState(resumption.base),
    };
  }

  function serializeFrame(f: Frame): SerializedFrame {
    switch (f.tag) {
      case "KIf":
        return { tag: "KIf", conseq: f.conseq, alt: f.alt, envId: collectCtx(f.env) };
      case "KBegin":
        return { tag: "KBegin", rest: f.rest, envId: collectCtx(f.env) };
      case "KDefine":
        return { tag: "KDefine", name: f.name, envId: collectCtx(f.env) };
      case "KSet":
        return { tag: "KSet", name: f.name, envId: collectCtx(f.env) };
      case "KAppFun":
        return { tag: "KAppFun", args: f.args, envId: collectCtx(f.env) };
      case "KAppArg":
        return { tag: "KAppArg", fnVal: serializeVal(f.fnVal), pending: f.pending, acc: f.acc.map(serializeVal), envId: collectCtx(f.env) };
      case "KAppArgLazy":
        return {
          tag: "KAppArgLazy",
          fnVal: serializeVal(f.fnVal),
          pending: f.pending,
          acc: f.acc.map(a => ({ idx: a.idx, val: serializeVal(a.val) })),
          envId: collectCtx(f.env),
          totalArgs: f.totalArgs,
          currentIdx: f.currentIdx,
        };
      case "KCall":
        return { tag: "KCall", savedEnvId: collectCtx(f.savedEnv) };
      case "KEffect":
        return { tag: "KEffect", op: f.op, pending: f.pending, acc: f.acc.map(serializeVal), envId: collectCtx(f.env) };
      case "KHandleBoundary":
        return {
          tag: "KHandleBoundary",
          hid: f.hid,
          savedHandlersDepth: f.savedHandlersDepth,
          resumeTo: f.resumeTo
            ? { kont: f.resumeTo.kont.map(serializeFrame), handlersDepth: f.resumeTo.handlersDepth }
            : undefined,
        };
      case "KHandleReturn":
        return {
          tag: "KHandleReturn",
          mode: f.mode,
          hid: f.hid,
          targetKont: f.targetKont.map(serializeFrame),
          targetHandlersDepth: f.targetHandlersDepth,
          savedHandlersDepth: f.savedHandlersDepth,
        };
      case "KPrompt":
        return {
          tag: "KPrompt",
          promptTag: serializeVal(f.promptTag),
          handler: serializeVal(f.handler),
          envId: collectCtx(f.env),
          savedKont: f.savedKont.map(serializeFrame),
          savedHandlersDepth: f.savedHandlersDepth,
        };
      case "KMatch":
        return { tag: "KMatch", clauses: f.clauses as Pattern[], envId: collectCtx(f.env) };
      case "KOracleLambda":
        return { tag: "KOracleLambda", params: f.params, envId: collectCtx(f.env) };
      case "KBind":
        return { tag: "KBind", fn: serializeVal(f.fn), envId: collectCtx(f.env) };
      case "KHandlerBind":
        return {
          tag: "KHandlerBind",
          handlers: (f.handlers ?? []).map(h => ({
            type: typeof h.type === "symbol" ? h.type.description ?? String(h.type) : h.type,
            handler: serializeVal(h.handler),
          })),
        };
      case "KRestartBind":
        return {
          tag: "KRestartBind",
          restarts: (f.restarts ?? []).map(r => ({
            name: typeof r.name === "symbol" ? r.name.description ?? String(r.name) : r.name,
            fn: serializeVal(r.fn),
            description: r.description,
          })),
          savedKont: f.savedKont.map(serializeFrame),
          envId: collectCtx(f.env),
          storeEntries: serializeStore(f.store),
          handlers: f.handlers.map(serializeHandler),
        };
      case "KSignaling":
        return { tag: "KSignaling", condition: serializeVal(f.condition as Val), required: f.required };
      default:
        throw new Error(`serializeFrame: unhandled tag ${(f as any).tag}`);
    }
  }

  function serializeVal(v: Val): SerializedVal {
    if (!v || typeof v !== "object") return { tag: "Unit" } as SerializedVal;
    switch ((v as any).tag) {
      case "Unit":
      case "Uninit":
      case "Bool":
      case "Num":
      case "Str":
      case "Sym":
      case "Err":
        return v as SerializedVal;
      case "Int":
        return { tag: "Int", value: (v as any).value.toString() } as SerializedVal;
      case "Pair":
        return { tag: "Pair", car: serializeVal((v as any).car), cdr: serializeVal((v as any).cdr) } as SerializedVal;
      case "Vector":
        return { tag: "Vector", items: (v as any).items.map(serializeVal) } as SerializedVal;
      case "List":
        return { tag: "List", elements: (v as any).elements.map(serializeVal) } as SerializedVal;
      case "Map":
        return {
          tag: "Map",
          entries: (v as any).entries.map(([k, val]: [Val, Val]) => [serializeVal(k), serializeVal(val)]),
        } as SerializedVal;
      case "Tagged":
        return { tag: "Tagged", typeTag: (v as any).typeTag, payload: serializeVal((v as any).payload) } as SerializedVal;
      case "Syntax":
        return { tag: "Syntax", stx: (v as any).stx } as SerializedVal;
      case "Dist": {
        const dist = v as DistVal;
        return {
          tag: "Dist",
          support: dist.support.map(it => ({ v: serializeVal(it.v), w: it.w })),
          normalized: dist.normalized,
          meta: (dist as any).meta,
        } as SerializedVal;
      }
      case "Closure":
        return { tag: "Closure", params: (v as any).params, body: (v as any).body, envId: collectCtx((v as any).env) } as SerializedVal;
      case "OracleProc":
        return {
          tag: "OracleProc",
          params: (v as any).params,
          spec: serializeVal((v as any).spec),
          envId: collectCtx((v as any).env),
          policyDigest: (v as any).policyDigest,
        } as SerializedVal;
      case "Continuation":
        return {
          tag: "Continuation",
          kont: (v as any).kont.map(serializeFrame),
          envId: collectCtx((v as any).env),
          storeEntries: serializeStore((v as any).store),
          handlers: (v as any).handlers.map(serializeHandler),
        } as SerializedVal;
      case "Machine":
        return {
          tag: "Machine",
          state: serializeState((v as any).state),
          label: (v as any).label,
          stepCount: (v as any).stepCount ?? 0,
          breakOnOps: (v as any).breakOnOps ? Array.from((v as any).breakOnOps as Set<string>) : undefined,
          breakOnPatterns: (v as any).breakOnPatterns,
          isDone: (v as any).isDone ?? false,
          parentId: (v as any).parentId,
          machineId: (v as any).machineId,
        } as SerializedVal;
      case "Native":
        return {
          tag: "Native",
          name: (v as any).name,
          arity: (v as any).arity,
          lazyArgs: (v as any).lazyArgs,
        } as SerializedVal;
      case "Cont":
        return {
          tag: "Cont",
          hid: (v as any).hid,
          boundaryIndex: (v as any).boundaryIndex,
          resumption: serializeResumption((v as any).resumption),
        } as SerializedVal;
      case "Ctx":
        return { tag: "Ctx", ctxId: collectCtx((v as any).ctx) } as SerializedVal;
      case "Module":
        return {
          tag: "Module",
          moduleId: (v as any).moduleId,
          sealedCtxId: collectCtx((v as any).sealedCtx),
          exports: Array.from((v as any).exports ?? []),
          meta: (v as any).meta,
        } as SerializedVal;
      case "ReceiptRef":
        return { tag: "ReceiptRef", rid: (v as any).rid, kind: (v as any).kind } as SerializedVal;
      case "ConnRef":
        return { tag: "ConnRef", id: (v as any).id, netId: (v as any).netId, name: (v as any).name } as SerializedVal;
      case "NetRef":
        return { tag: "NetRef", id: (v as any).id, name: (v as any).name } as SerializedVal;
      case "Explanation":
        return serializeExplanation(v as any);
      case "Contradiction":
        return {
          tag: "Contradiction",
          explanation: serializeVal((v as any).explanation as Val),
          constraintId: (v as any).constraintId,
          netId: (v as any).netId,
        } as SerializedVal;
      case "Condition":
        return serializeCondition(v as ConditionVal);
      case "Fiber":
      case "Mutex":
      case "IVar":
      case "Channel":
      case "Actor":
      case "GenericRegistry":
      case "Budget":
      case "CostEstimate":
        return v as SerializedVal;
      case "Profile":
        return { tag: "Profile", profileId: (v as any).profileId, profile: (v as any).profile as Profile } as SerializedVal;
      case "GenericMiss":
        return {
          tag: "GenericMiss",
          op: (v as any).op,
          signature: (v as any).signature,
          argsPreview: (v as any).argsPreview.map(serializeVal),
          registryId: (v as any).registryId,
          profileName: (v as any).profileName,
        } as SerializedVal;
      case "Promise":
        return { tag: "Promise", id: (v as any).id, label: (v as any).label } as SerializedVal;
      case "Stream":
        return {
          tag: "Stream",
          isEmpty: (v as any).isEmpty,
          head: (v as any).head ? serializeVal((v as any).head) : undefined,
          tail: (v as any).tail ? serializeVal((v as any).tail) : undefined,
        } as SerializedVal;
      case "IR":
        return {
          tag: "IR",
          form: (v as any).form,
          digest: (v as any).digest,
          irRef: (v as any).irRef,
          label: (v as any).label,
        } as SerializedVal;
      case "Result":
        return {
          tag: "Result",
          kind: (v as any).kind,
          solution: (v as any).solution ? serializeVal((v as any).solution) : undefined,
          remaining: (v as any).remaining ? serializeVal((v as any).remaining) : undefined,
          reason: (v as any).reason,
          cost: (v as any).cost,
        } as SerializedVal;
      case "FactStore":
        return {
          tag: "FactStore",
          factsEntries: Array.from((v as any).facts.entries()).map(([k, val]: [string, Val]) => [k, serializeVal(val)]),
        } as SerializedVal;
      case "Meaning":
        return serializeMeaning(v as MeaningVal);
      case "Solver":
        return { tag: "Solver", name: (v as any).name } as SerializedVal;
      default:
        throw new Error(`serializeVal: unhandled tag ${(v as any).tag}`);
    }
  }

  const controlNode: Control | undefined = (state as any).control ?? (state as any).ctrl;
  const control: SerializedControl = controlNode?.tag === "Expr"
    ? { tag: "Expr", e: (controlNode as any).e }
    : { tag: "Val", v: serializeVal((controlNode as any)?.v ?? { tag: "Unit" } as Val) };

  const envId = state.env && isCtx(state.env) ? collectCtx(state.env) : null;

  return {
    control,
    envId,
    storeEntries: serializeStore(state.store),
    kont: (state.kont ?? []).map(serializeFrame),
    handlers: (state.handlers ?? []).map(serializeHandler),
    ctxTable,
  };
}

export function deserializeState(serialized: SerializedState, nativeRegistry: Map<string, Val>): State {
  const ctxInstances: Record<string, Ctx> = {};

  function rebuildCtx(id: string): Ctx {
    if (ctxInstances[id]) return ctxInstances[id];
    const ser = serialized.ctxTable[id];
    if (!ser) throw new Error(`deserializeState: missing ctx ${id}`);

    const ctx: Ctx = {
      tag: "Ctx",
      cid: ser.id,
      parent: ser.parentId ? rebuildCtx(ser.parentId) : undefined,
      frame: new Map(ser.frameEntries),
      profile: ser.profile,
      caps: ser.caps as CapSet,
      budgets: ser.budgets as any,
      constraints: ser.constraints as any,
      sealed: !!ser.sealed,
      evidence: ser.evidence as any,
    };

    ctxInstances[id] = ctx;
    return ctx;
  }

  function deserializeStore(entries: Array<[number, SerializedVal]>): Store {
    const cells = new Map<number, Val>();
    let next = 0;
    for (const [addr, val] of entries) {
      cells.set(addr, deserializeVal(val));
      next = Math.max(next, addr + 1);
    }
    return new COWStore(next, cells);
  }

  function deserializeHandler(h: SerializedHandlerFrame): HandlerFrame {
    return {
      hid: h.hid,
      env: rebuildCtx(h.envId),
      on: new Map(h.on),
      ret: h.ret,
      fin: h.fin,
    };
  }

  function deserializeResumption(sr: SerializedResumption): Resumption {
    const base = deserializeState(sr.baseState, nativeRegistry);
    return {
      rid: sr.rid,
      base,
      invoke: (v: Val) => ({ ...base, control: { tag: "Val", v } }),
      digest: () => JSON.stringify({
        rid: sr.rid,
        store: (base.store as any)?.digest?.(),
        kontDepth: base.kont.length,
        handlersDepth: base.handlers.length,
      }),
    };
  }

  function deserializeFrame(f: SerializedFrame): Frame {
    switch (f.tag) {
      case "KIf":
        return { tag: "KIf", conseq: f.conseq as Expr, alt: f.alt as Expr, env: rebuildCtx(f.envId as string) };
      case "KBegin":
        return { tag: "KBegin", rest: f.rest as Expr[], env: rebuildCtx(f.envId as string) };
      case "KDefine":
        return { tag: "KDefine", name: f.name as string, env: rebuildCtx(f.envId as string) };
      case "KSet":
        return { tag: "KSet", name: f.name as string, env: rebuildCtx(f.envId as string) };
      case "KAppFun":
        return { tag: "KAppFun", args: f.args as Expr[], env: rebuildCtx(f.envId as string) };
      case "KAppArg":
        return {
          tag: "KAppArg",
          fnVal: deserializeVal(f.fnVal as SerializedVal),
          pending: f.pending as Expr[],
          acc: (f.acc as SerializedVal[]).map(deserializeVal),
          env: rebuildCtx(f.envId as string),
        };
      case "KAppArgLazy":
        return {
          tag: "KAppArgLazy",
          fnVal: deserializeVal(f.fnVal as SerializedVal),
          pending: f.pending as Array<{ expr: Expr; idx: number }>,
          acc: (f.acc as Array<{ idx: number; val: SerializedVal }>).map(a => ({ idx: a.idx, val: deserializeVal(a.val) })),
          env: rebuildCtx(f.envId as string),
          totalArgs: f.totalArgs as number,
          currentIdx: f.currentIdx as number,
        };
      case "KCall":
        return { tag: "KCall", savedEnv: rebuildCtx(f.savedEnvId as string) };
      case "KEffect":
        return {
          tag: "KEffect",
          op: f.op as string,
          pending: f.pending as Expr[],
          acc: (f.acc as SerializedVal[]).map(deserializeVal),
          env: rebuildCtx(f.envId as string),
        };
      case "KHandleBoundary":
        return {
          tag: "KHandleBoundary",
          hid: f.hid as string,
          savedHandlersDepth: f.savedHandlersDepth as number,
          resumeTo: f.resumeTo
            ? {
              kont: (f.resumeTo as any).kont.map(deserializeFrame),
              handlersDepth: (f.resumeTo as any).handlersDepth as number,
            }
            : undefined,
        };
      case "KHandleReturn":
        return {
          tag: "KHandleReturn",
          mode: f.mode as "exit" | "resume",
          hid: f.hid as string,
          targetKont: (f.targetKont as SerializedFrame[]).map(deserializeFrame),
          targetHandlersDepth: f.targetHandlersDepth as number,
          savedHandlersDepth: f.savedHandlersDepth as number,
        };
      case "KPrompt":
        return {
          tag: "KPrompt",
          promptTag: deserializeVal(f.promptTag as SerializedVal),
          handler: deserializeVal(f.handler as SerializedVal),
          env: rebuildCtx(f.envId as string),
          savedKont: (f.savedKont as SerializedFrame[]).map(deserializeFrame),
          savedHandlersDepth: f.savedHandlersDepth as number,
        };
      case "KMatch":
        return { tag: "KMatch", clauses: f.clauses as Array<{ pat: Pattern; body: Expr }>, env: rebuildCtx(f.envId as string) };
      case "KOracleLambda":
        return { tag: "KOracleLambda", params: f.params as string[], env: rebuildCtx(f.envId as string) };
      case "KBind":
        return { tag: "KBind", fn: deserializeVal(f.fn as SerializedVal), env: rebuildCtx(f.envId as string) };
      case "KHandlerBind":
        return {
          tag: "KHandlerBind",
          handlers: (f.handlers as any[]).map(h => ({
            type: (h as any).type === "*" ? "*" : Symbol.for((h as any).type),
            handler: deserializeVal((h as any).handler),
          })) as ConditionHandler[],
        };
      case "KRestartBind":
        return {
          tag: "KRestartBind",
          restarts: (f.restarts as any[]).map(r => ({
            name: Symbol.for((r as any).name),
            fn: deserializeVal((r as any).fn),
            description: (r as any).description,
          })) as RestartBinding[],
          savedKont: (f.savedKont as SerializedFrame[]).map(deserializeFrame),
          env: rebuildCtx(f.envId as string),
          store: deserializeStore(f.storeEntries as Array<[number, SerializedVal]>),
          handlers: (f.handlers as SerializedHandlerFrame[]).map(deserializeHandler),
        };
      case "KSignaling":
        return { tag: "KSignaling", condition: deserializeVal(f.condition as SerializedVal) as any, required: !!f.required };
      default:
        throw new Error(`deserializeFrame: unhandled tag ${(f as any).tag}`);
    }
  }

  function deserializeMeaning(m: any): MeaningVal {
    const deserializeIfVal = (v: unknown) => (v && typeof v === "object" && (v as any).tag ? deserializeVal(v as SerializedVal) : v);
    const trace = Array.isArray(m.trace)
      ? (m.trace as RewriteStep[]).map(step => ({
        ...step,
        before: deserializeIfVal(step.before) as any,
        after: deserializeIfVal(step.after) as any,
      }))
      : deserializeIfVal(m.trace);

    return {
      tag: "Meaning",
      denotation: deserializeIfVal(m.denotation) as any,
      residual: deserializeIfVal(m.residual) as any,
      rewrite: deserializeIfVal(m.rewrite) as any,
      invariants: deserializeIfVal(m.invariants) as any,
      effects: deserializeIfVal(m.effects) as any,
      cost: deserializeIfVal(m.cost) as any,
      paths: deserializeIfVal(m.paths) as any,
      deps: deserializeIfVal(m.deps) as any,
      memo: deserializeIfVal(m.memo) as any,
      obligation: deserializeIfVal(m.obligation) as any,
      obligations: (m.obligations as Obligation[] | undefined)?.map(o => ({
        ...o,
        domain: deserializeIfVal((o as any).domain) as any,
      })),
      evidence: m.evidence as any,
      confidence: m.confidence as number | undefined,
      trace: trace as any,
      adoptEnvRef: m.adoptEnvRef,
      adoptStateRef: m.adoptStateRef,
    } as MeaningVal;
  }

  function deserializeExplanation(expl: any): Val {
    if (!expl || expl.tag !== "Explanation") return expl as Val;
    if (expl.kind === "assumption") {
      return {
        tag: "Explanation",
        kind: "assumption",
        conn: deserializeVal(expl.conn as SerializedVal),
        valueHash: expl.valueHash,
        because: deserializeVal(expl.because as SerializedVal),
      } as Val;
    }
    if (expl.kind === "derived") {
      return {
        tag: "Explanation",
        kind: "derived",
        conn: deserializeVal(expl.conn as SerializedVal),
        valueHash: expl.valueHash,
        rule: expl.rule,
        deps: (expl.deps ?? []).map((d: any) => deserializeExplanation(d)) as any,
      } as Val;
    }
    if (expl.kind === "conflict") {
      return {
        tag: "Explanation",
        kind: "conflict",
        conn: deserializeVal(expl.conn as SerializedVal),
        left: deserializeExplanation(expl.left),
        right: deserializeExplanation(expl.right),
        message: expl.message,
      } as Val;
    }
    if (expl.kind === "denied") {
      return { tag: "Explanation", kind: "denied", op: expl.op, reason: expl.reason, profile: expl.profile } as Val;
    }
    return expl as Val;
  }

  function deserializeCondition(cond: any): ConditionVal {
    return {
      tag: "Condition",
      type: cond.type === "*" ? Symbol.for("*") : Symbol.for(cond.type ?? ""),
      message: cond.message,
      data: deserializeVal(cond.data as SerializedVal),
      restarts: (cond.restarts ?? []).map((r: any) => ({
        name: Symbol.for(r.name ?? ""),
        description: r.description,
        kont: (r.kont as SerializedFrame[]).map(deserializeFrame),
        env: rebuildCtx(r.envId as string),
        store: deserializeStore(r.storeEntries as Array<[number, SerializedVal]>),
        handlers: (r.handlers as SerializedHandlerFrame[]).map(deserializeHandler),
      })) as RestartPoint[],
    } as ConditionVal;
  }

  function deserializeVal(v: SerializedVal): Val {
    switch (v.tag) {
      case "Unit":
      case "Uninit":
      case "Bool":
      case "Num":
      case "Str":
      case "Sym":
      case "Err":
        return v as unknown as Val;
      case "Int":
        return { tag: "Int", value: BigInt(v.value as string) } as Val;
      case "Pair":
        return { tag: "Pair", car: deserializeVal(v.car as SerializedVal), cdr: deserializeVal(v.cdr as SerializedVal) } as Val;
      case "Vector":
        return { tag: "Vector", items: (v.items as SerializedVal[]).map(deserializeVal) } as Val;
      case "List":
        return { tag: "List", elements: (v.elements as SerializedVal[]).map(deserializeVal) } as Val;
      case "Map":
        return { tag: "Map", entries: (v.entries as Array<[SerializedVal, SerializedVal]>).map(([k, val]) => [deserializeVal(k), deserializeVal(val)]) } as Val;
      case "Tagged":
        return { tag: "Tagged", typeTag: v.typeTag as string, payload: deserializeVal(v.payload as SerializedVal) } as Val;
      case "Syntax":
        return { tag: "Syntax", stx: v.stx } as Val;
      case "Dist":
        return {
          tag: "Dist",
          support: (v.support as Array<{ v: SerializedVal; w: number }>).map(it => ({ v: deserializeVal(it.v), w: it.w })),
          normalized: v.normalized as boolean | undefined,
          meta: v.meta as any,
        } as Val;
      case "Closure":
        return { tag: "Closure", params: v.params as string[], body: v.body as Expr, env: rebuildCtx(v.envId as string) } as Val;
      case "OracleProc":
        return {
          tag: "OracleProc",
          params: v.params as string[],
          spec: deserializeVal(v.spec as SerializedVal),
          env: rebuildCtx(v.envId as string),
          policyDigest: v.policyDigest as string | undefined,
        } as Val;
      case "Continuation":
        return {
          tag: "Continuation",
          kont: (v.kont as SerializedFrame[]).map(deserializeFrame),
          env: rebuildCtx(v.envId as string),
          store: deserializeStore(v.storeEntries as Array<[number, SerializedVal]>),
          handlers: (v.handlers as SerializedHandlerFrame[]).map(deserializeHandler),
        } as Val;
      case "Machine":
        return {
          tag: "Machine",
          state: deserializeState(v.state as SerializedState, nativeRegistry),
          label: v.label as string | undefined,
          stepCount: (v.stepCount as number) ?? 0,
          breakOnOps: v.breakOnOps ? new Set(v.breakOnOps as string[]) : undefined,
          breakOnPatterns: v.breakOnPatterns as string[] | undefined,
          isDone: !!v.isDone,
          parentId: v.parentId as string | undefined,
          machineId: v.machineId as string,
        } as any;
      case "Native": {
        const native = nativeRegistry.get(v.name as string);
        if (!native) {
          throw new Error(`deserializeState: missing native '${v.name as string}'`);
        }
        return native;
      }
      case "Cont":
        return {
          tag: "Cont",
          hid: v.hid as string,
          boundaryIndex: v.boundaryIndex as number,
          resumption: deserializeResumption(v.resumption as SerializedResumption),
        } as any;
      case "Ctx":
        return { tag: "Ctx", ctx: rebuildCtx(v.ctxId as string) } as any;
      case "Module":
        return {
          tag: "Module",
          moduleId: v.moduleId as Hash,
          sealedCtx: rebuildCtx(v.sealedCtxId as string),
          exports: new Set(v.exports as string[]),
          meta: v.meta as any,
        } as any;
      case "ReceiptRef":
        return { tag: "ReceiptRef", rid: v.rid as Hash, kind: v.kind as any } as any;
      case "ConnRef":
        return { tag: "ConnRef", id: v.id as string, netId: v.netId as string, name: v.name as string | undefined } as any;
      case "NetRef":
        return { tag: "NetRef", id: v.id as string, name: v.name as string | undefined } as any;
      case "Explanation":
        return deserializeExplanation(v);
      case "Contradiction":
        return {
          tag: "Contradiction",
          explanation: deserializeVal(v.explanation as SerializedVal) as any,
          constraintId: v.constraintId as string | undefined,
          netId: v.netId as string | undefined,
        } as any;
      case "Condition":
        return deserializeCondition(v);
      case "Fiber":
      case "Mutex":
      case "IVar":
      case "Channel":
      case "Actor":
      case "GenericRegistry":
      case "Budget":
      case "CostEstimate":
        return v as unknown as Val;
      case "Profile":
        return { tag: "Profile", profileId: v.profileId as string, profile: v.profile as Profile } as any;
      case "GenericMiss":
        return {
          tag: "GenericMiss",
          op: v.op as string,
          signature: v.signature as string[],
          argsPreview: (v.argsPreview as SerializedVal[]).map(deserializeVal),
          registryId: v.registryId as string,
          profileName: v.profileName as string | undefined,
        } as any;
      case "Promise":
        return { tag: "Promise", id: v.id as string, label: v.label as string | undefined } as any;
      case "Stream":
        return {
          tag: "Stream",
          isEmpty: !!v.isEmpty,
          head: v.head ? deserializeVal(v.head as SerializedVal) : undefined,
          tail: v.tail ? deserializeVal(v.tail as SerializedVal) : undefined,
        } as any;
      case "IR":
        return { tag: "IR", form: v.form as any, digest: v.digest as string, irRef: v.irRef as string, label: v.label as string | undefined } as any;
      case "Result":
        return {
          tag: "Result",
          kind: v.kind as string,
          solution: v.solution ? deserializeVal(v.solution as SerializedVal) : undefined,
          remaining: v.remaining ? deserializeVal(v.remaining as SerializedVal) : undefined,
          reason: v.reason as string | undefined,
          cost: v.cost as number,
        } as any;
      case "FactStore":
        return {
          tag: "FactStore",
          facts: new Map<string, Val>((v.factsEntries as Array<[string, SerializedVal]>).map(([k, val]) => [k, deserializeVal(val)])),
        } as any;
      case "Meaning":
        return deserializeMeaning(v);
      case "Solver":
        return {
          tag: "Solver",
          name: v.name as string,
          solve: () => {
            throw new Error("Solver.fn not restored from serialized state");
          },
          estimate: () => {
            throw new Error("Solver.fn not restored from serialized state");
          },
        } as SolverVal;
      default:
        throw new Error(`deserializeVal: unhandled tag ${(v as any).tag}`);
    }
  }

  const control: Control = serialized.control.tag === "Expr"
    ? { tag: "Expr", e: serialized.control.e }
    : { tag: "Val", v: deserializeVal(serialized.control.v) };

  const env = serialized.envId ? rebuildCtx(serialized.envId) : (undefined as any);

  return {
    control,
    env,
    store: deserializeStore(serialized.storeEntries),
    kont: (serialized.kont ?? []).map(deserializeFrame),
    handlers: (serialized.handlers ?? []).map(deserializeHandler),
  } as State;
}
