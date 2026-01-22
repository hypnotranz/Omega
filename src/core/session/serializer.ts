import type { Expr } from "../ast";
import type { ConditionVal, RestartPoint } from "../conditions/types";
import type { Ctx } from "../ctx/ctx";
import type { Control, Frame, HandlerFrame, State } from "../eval/machine";
import type { Store } from "../eval/store";
import { COWStore } from "../eval/store";
import type { Val } from "../eval/values";

export type SerializedCtx = {
  id: string;
  parentId: string | null;
  frameEntries: Array<[string, number]>;
  profile: string;
  caps: string[];
  budgets: Record<string, unknown>;
  constraints: unknown[];
  sealed: boolean;
  evidence: unknown[];
};

export type SerializedHandlerFrame = {
  hid: string;
  envId: string;
  on: [string, { op: string; params: string[]; k: string; body: Expr }][];
  ret?: { v: string; body: Expr };
  fin?: { body: Expr };
};

export type SerializedRestart = {
  name: string;
  description?: string;
  kont: SerializedFrame[];
  envId: string;
  storeEntries: [number, SerializedVal][];
  handlers: SerializedHandlerFrame[];
};

export type SerializedFrame =
  | { tag: "KIf"; conseq: Expr; alt: Expr; envId: string }
  | { tag: "KBegin"; rest: Expr[]; envId: string }
  | { tag: "KDefine"; name: string; envId: string }
  | { tag: "KSet"; name: string; envId: string }
  | { tag: "KAppFun"; args: Expr[]; envId: string }
  | { tag: "KAppArg"; fnVal: SerializedVal; pending: Expr[]; acc: SerializedVal[]; envId: string }
  | { tag: "KAppArgLazy"; fnVal: SerializedVal; pending: Array<{ expr: Expr; idx: number }>; acc: Array<{ idx: number; val: SerializedVal }>; envId: string; totalArgs: number; currentIdx: number }
  | { tag: "KCall"; savedEnvId: string }
  | { tag: "KEffect"; op: string; pending: Expr[]; acc: SerializedVal[]; envId: string }
  | { tag: "KHandleBoundary"; hid: string; savedHandlersDepth: number; resumeTo?: { kont: SerializedFrame[]; handlersDepth: number } }
  | { tag: "KHandleReturn"; mode: "exit" | "resume"; hid: string; targetKont: SerializedFrame[]; targetHandlersDepth: number; savedHandlersDepth: number }
  | { tag: "KPrompt"; promptTag: SerializedVal; handler: SerializedVal; envId: string; savedKont: SerializedFrame[]; savedHandlersDepth: number }
  | { tag: "KMatch"; clauses: Array<{ pat: unknown; body: Expr }>; envId: string }
  | { tag: "KOracleLambda"; params: string[]; envId: string }
  | { tag: "KBind"; fn: SerializedVal; envId: string }
  | { tag: "KHandlerBind"; handlers: Array<{ type: string | "*"; handler: SerializedVal }> }
  | { tag: "KRestartBind"; restarts: SerializedRestart[]; savedKont: SerializedFrame[]; envId: string; storeEntries: [number, SerializedVal][]; handlers: SerializedHandlerFrame[] }
  | { tag: "KSignaling"; condition: SerializedVal; required: boolean }
  | { tag: string; [key: string]: unknown };

export type SerializedControl =
  | { tag: "Expr"; e: Expr }
  | { tag: "Val"; v: SerializedVal };

export type SerializedState = {
  control: SerializedControl;
  envId: string;
  storeEntries: [number, SerializedVal][];
  storeNext?: number;
  kont: SerializedFrame[];
  handlers: SerializedHandlerFrame[];
  ctxTable: Record<string, SerializedCtx>;
  profile?: unknown;
  budget?: unknown;
  sec?: { caps?: string[] };
};

export type SerializedVal =
  | { tag: "Unit" }
  | { tag: "Uninit" }
  | { tag: "Num"; n: number }
  | { tag: "Int"; value: string }
  | { tag: "Bool"; b: boolean }
  | { tag: "Str"; s: string }
  | { tag: "Sym"; name: string }
  | { tag: "Err"; message?: string }
  | { tag: "Pair"; car: SerializedVal; cdr: SerializedVal }
  | { tag: "Vector"; items: SerializedVal[] }
  | { tag: "List"; elements: SerializedVal[] }
  | { tag: "Map"; entries: Array<[SerializedVal, SerializedVal]> }
  | { tag: "Tagged"; typeTag: string; payload: SerializedVal }
  | { tag: "Syntax"; stx: unknown }
  | { tag: "Closure"; params: string[]; body: Expr; envId: string }
  | { tag: "Native"; name: string; arity: number | "variadic"; lazyArgs?: number[] }
  | { tag: "Cont"; hid: string; boundaryIndex: number; resumption: { rid: string; baseState: SerializedState } }
  | { tag: "OracleProc"; params: string[]; spec: SerializedVal; envId: string; policyDigest?: string }
  | { tag: "Continuation"; kont: SerializedFrame[]; envId: string; storeEntries: [number, SerializedVal][]; handlers: SerializedHandlerFrame[] }
  | { tag: "Machine"; state: SerializedState; label?: string; stepCount: number; breakOnOps?: string[]; breakOnPatterns?: string[]; lastOutcome?: unknown; isDone: boolean; machineId: string; parentId?: string }
  | { tag: "Dist"; support: Array<{ v: SerializedVal; w: number }>; normalized?: boolean; meta?: unknown }
  | { tag: "Meaning"; denotation?: SerializedVal; residual?: SerializedVal; rewrite?: SerializedVal; invariants?: SerializedVal; effects?: SerializedVal; cost?: SerializedVal; paths?: SerializedVal; deps?: SerializedVal; memo?: SerializedVal; obligation?: SerializedVal; obligations?: unknown[]; evidence?: unknown[]; confidence?: number; trace?: SerializedVal; adoptEnvRef?: string; adoptStateRef?: string }
  | { tag: "Profile"; profileId: string; profile: any }
  | { tag: "Ctx"; ctxId: string }
  | { tag: "Module"; moduleId: any; sealedCtxId: string; exports: string[]; meta?: any }
  | { tag: "ReceiptRef"; rid: any; kind: string }
  | { tag: "ConnRef"; id: string; netId: string; name?: string }
  | { tag: "NetRef"; id: string; name?: string }
  | { tag: "Explanation"; kind: string; conn?: SerializedVal; valueHash?: string; because?: SerializedVal; rule?: string; deps?: SerializedVal[]; left?: SerializedVal; right?: SerializedVal; message?: string; op?: string; reason?: string; profile?: string }
  | { tag: "Contradiction"; explanation: SerializedVal; constraintId?: string; netId?: string }
  | { tag: "Fiber"; id: number; name?: string }
  | { tag: "Mutex"; id: string; name?: string }
  | { tag: "IVar"; id: string; name?: string }
  | { tag: "Channel"; id: string; bufferSize: number; name?: string }
  | { tag: "Actor"; id: string; fiberId: number; name?: string }
  | { tag: "Promise"; id: string; label?: string }
  | { tag: "GenericRegistry"; id: string; name?: string }
  | { tag: "GenericMiss"; op: string; signature: string[]; argsPreview: SerializedVal[]; registryId: string; profileName?: string }
  | { tag: "Stream"; isEmpty: boolean; head?: SerializedVal; tail?: SerializedVal }
  | { tag: "IR"; form: string; digest: string; irRef: string; label?: string }
  | { tag: "Budget"; tokens: number; calls: number; time: number }
  | { tag: "Result"; kind: string; solution?: SerializedVal; remaining?: SerializedVal; reason?: string; cost: number }
  | { tag: "CostEstimate"; minCost: number; maxCost: number; expectedCost: number; confidence: number }
  | { tag: "Solver"; name: string }
  | { tag: "FactStore"; factsEntries: Array<[string, SerializedVal]> }
  | { tag: "Condition"; kind: string; message?: string; payload?: SerializedVal; restarts?: SerializedRestart[] }
  | { tag: string; [key: string]: unknown };

function serializeProfile(profile: any): any {
  if (!profile) return profile;
  return {
    ...profile,
    allowedCaps: profile.allowedCaps ? Array.from(profile.allowedCaps) : undefined,
    allowedOps: profile.allowedOps ? Array.from(profile.allowedOps) : undefined,
    allowedOracleReqTags: profile.allowedOracleReqTags ? Array.from(profile.allowedOracleReqTags) : undefined,
  };
}

function deserializeProfile(profile: any): any {
  if (!profile) return profile;
  return {
    ...profile,
    allowedCaps: profile.allowedCaps ? new Set(profile.allowedCaps) : undefined,
    allowedOps: profile.allowedOps ? new Set(profile.allowedOps) : undefined,
    allowedOracleReqTags: profile.allowedOracleReqTags ? new Set(profile.allowedOracleReqTags) : undefined,
  };
}

export function serializeState(state: State): SerializedState {
  const ctxTable: Record<string, SerializedCtx> = {};
  const ctxIds = new Map<Ctx, string>();

  const controlInput: Control | undefined = (state as any).control ?? (state as any).ctrl;
  if (!controlInput) {
    throw new Error("State missing control");
  }

  function collectCtx(ctx: Ctx | undefined): string {
    if (!ctx) return "null";
    const existing = ctxIds.get(ctx);
    if (existing) return existing;
    const id = (ctx as any).cid ?? `ctx-${ctxIds.size}`;
    ctxIds.set(ctx, id);
    ctxTable[id] = {
      id,
      parentId: (ctx as any).parent ? collectCtx((ctx as any).parent) : null,
      frameEntries: Array.from((ctx as any).frame?.entries?.() ?? []),
      profile: (ctx as any).profile,
      caps: Array.from((ctx as any).caps ?? []),
      budgets: (ctx as any).budgets ?? {},
      constraints: (ctx as any).constraints ?? [],
      sealed: !!(ctx as any).sealed,
      evidence: (ctx as any).evidence ?? [],
    };
    return id;
  }

  function serializeStore(store: Store | undefined): [number, SerializedVal][] {
    const entries: [number, SerializedVal][] = [];
    if (!store) return entries;
    const max = (store as any).next ?? 0;
    for (let addr = 0; addr < max; addr++) {
      try {
        const val = store.read(addr);
        entries.push([addr, serializeVal(val)]);
      } catch {
        // Ignore holes
      }
    }
    return entries;
  }

  function serializeRestart(r: RestartPoint): SerializedRestart {
    return {
      name: (r.name as any)?.toString?.() ?? "restart",
      description: r.description,
      kont: (r.kont ?? []).map(serializeFrame),
      envId: collectCtx(r.env as Ctx),
      storeEntries: serializeStore(r.store),
      handlers: (r.handlers ?? []).map(serializeHandler),
    };
  }

  function serializeCondition(v: ConditionVal): SerializedVal {
    return {
      tag: "Condition",
      kind: (v.type as any)?.toString?.() ?? "condition",
      message: v.message,
      payload: serializeVal(v.data),
      restarts: (v.restarts ?? []).map(serializeRestart),
    };
  }

  function serializeVal(v: Val): SerializedVal {
    if (!v || typeof v !== "object") {
      return { tag: "Unit" };
    }

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
        return { tag: "Int", value: (v as any).value.toString() };
      case "Pair":
        return { tag: "Pair", car: serializeVal((v as any).car), cdr: serializeVal((v as any).cdr) };
      case "Vector":
        return { tag: "Vector", items: ((v as any).items ?? []).map(serializeVal) };
      case "List":
        return { tag: "List", elements: ((v as any).elements ?? []).map(serializeVal) };
      case "Map":
        return {
          tag: "Map",
          entries: ((v as any).entries ?? []).map(([k, val]: [Val, Val]) => [serializeVal(k), serializeVal(val)]),
        };
      case "Tagged":
        return { tag: "Tagged", typeTag: (v as any).typeTag, payload: serializeVal((v as any).payload) };
      case "Syntax":
        return { tag: "Syntax", stx: (v as any).stx };
      case "Closure":
        return { tag: "Closure", params: (v as any).params, body: (v as any).body, envId: collectCtx((v as any).env) };
      case "Native":
        return { tag: "Native", name: (v as any).name, arity: (v as any).arity, lazyArgs: (v as any).lazyArgs };
      case "OracleProc":
        return {
          tag: "OracleProc",
          params: (v as any).params,
          spec: serializeVal((v as any).spec),
          envId: collectCtx((v as any).env),
          policyDigest: (v as any).policyDigest,
        };
      case "Continuation":
        return {
          tag: "Continuation",
          kont: ((v as any).kont ?? []).map(serializeFrame),
          envId: collectCtx((v as any).env),
          storeEntries: serializeStore((v as any).store),
          handlers: ((v as any).handlers ?? []).map(serializeHandler),
        };
      case "Machine":
        return {
          tag: "Machine",
          state: serializeState((v as any).state),
          label: (v as any).label,
          stepCount: (v as any).stepCount ?? 0,
          breakOnOps: (v as any).breakOnOps ? Array.from((v as any).breakOnOps) : undefined,
          breakOnPatterns: (v as any).breakOnPatterns,
          lastOutcome: (v as any).lastOutcome,
          isDone: !!(v as any).isDone,
          machineId: (v as any).machineId ?? "",
          parentId: (v as any).parentId,
        };
      case "Dist":
        return {
          tag: "Dist",
          support: ((v as any).support ?? []).map((it: any) => ({ v: serializeVal(it.v), w: it.w })),
          normalized: (v as any).normalized,
          meta: (v as any).meta,
        };
      case "Meaning": {
        const mv: any = v as any;
        return {
          tag: "Meaning",
          denotation: mv.denotation !== undefined ? serializeVal(mv.denotation as any) : undefined,
          residual: mv.residual !== undefined ? serializeVal(mv.residual as any) : undefined,
          rewrite: mv.rewrite !== undefined ? serializeVal(mv.rewrite as any) : undefined,
          invariants: mv.invariants !== undefined ? serializeVal(mv.invariants as any) : undefined,
          effects: mv.effects !== undefined ? serializeVal(mv.effects as any) : undefined,
          cost: mv.cost !== undefined ? serializeVal(mv.cost as any) : undefined,
          paths: mv.paths !== undefined ? serializeVal(mv.paths as any) : undefined,
          deps: mv.deps !== undefined ? serializeVal(mv.deps as any) : undefined,
          memo: mv.memo !== undefined ? serializeVal(mv.memo as any) : undefined,
          obligation: mv.obligation !== undefined ? serializeVal(mv.obligation as any) : undefined,
          obligations: mv.obligations,
          evidence: mv.evidence,
          confidence: mv.confidence,
          trace: mv.trace !== undefined ? serializeVal(mv.trace as any) : undefined,
          adoptEnvRef: mv.adoptEnvRef,
          adoptStateRef: mv.adoptStateRef,
        };
      }
      case "Profile":
        return { tag: "Profile", profileId: (v as any).profileId, profile: serializeProfile((v as any).profile) };
      case "Ctx": {
        const id = collectCtx((v as any).ctx);
        return { tag: "Ctx", ctxId: id };
      }
      case "Module":
        return {
          tag: "Module",
          moduleId: (v as any).moduleId,
          sealedCtxId: collectCtx((v as any).sealedCtx),
          exports: Array.from((v as any).exports ?? []),
          meta: (v as any).meta,
        };
      case "ReceiptRef":
        return { tag: "ReceiptRef", rid: (v as any).rid, kind: (v as any).kind };
      case "ConnRef":
        return { tag: "ConnRef", id: (v as any).id, netId: (v as any).netId, name: (v as any).name };
      case "NetRef":
        return { tag: "NetRef", id: (v as any).id, name: (v as any).name };
      case "Explanation":
        return {
          tag: "Explanation",
          kind: (v as any).kind,
          conn: (v as any).conn ? serializeVal((v as any).conn) : undefined,
          valueHash: (v as any).valueHash,
          because: (v as any).because ? serializeVal((v as any).because) : undefined,
          rule: (v as any).rule,
          deps: (v as any).deps ? (v as any).deps.map((d: any) => serializeVal(d)) : undefined,
          left: (v as any).left ? serializeVal((v as any).left) : undefined,
          right: (v as any).right ? serializeVal((v as any).right) : undefined,
          message: (v as any).message,
          op: (v as any).op,
          reason: (v as any).reason,
          profile: (v as any).profile,
        };
      case "Contradiction":
        return {
          tag: "Contradiction",
          explanation: serializeVal((v as any).explanation),
          constraintId: (v as any).constraintId,
          netId: (v as any).netId,
        };
      case "Fiber":
      case "Mutex":
      case "IVar":
      case "Channel":
      case "Actor":
      case "Promise":
      case "GenericRegistry":
      case "IR":
      case "Budget":
      case "CostEstimate":
      case "Stream":
      case "Result":
      case "GenericMiss":
        return {
          ...(v as any),
          head: (v as any).head ? serializeVal((v as any).head) : (v as any).head,
          tail: (v as any).tail ? serializeVal((v as any).tail) : (v as any).tail,
          solution: (v as any).solution ? serializeVal((v as any).solution) : (v as any).solution,
          remaining: (v as any).remaining ? serializeVal((v as any).remaining) : (v as any).remaining,
        } as SerializedVal;
      case "FactStore":
        return {
          tag: "FactStore",
          factsEntries: Array.from(((v as any).facts ?? new Map()).entries()).map(([k, val]) => [k, serializeVal(val as any)]),
        };
      case "Condition":
        return serializeCondition(v as ConditionVal);
      case "Cont":
        return {
          tag: "Cont",
          hid: (v as any).hid,
          boundaryIndex: (v as any).boundaryIndex,
          resumption: { rid: (v as any).resumption.rid, baseState: serializeState((v as any).resumption.base) },
        };
      case "Solver":
        return { tag: "Solver", name: (v as any).name };
      default: {
        const copy: any = {};
        for (const [key, val] of Object.entries(v as any)) {
          if (typeof val === "function") continue;
          if (val instanceof Map) {
            copy[key] = Array.from(val.entries());
          } else if (val instanceof Set) {
            copy[key] = Array.from(val);
          } else {
            copy[key] = val;
          }
        }
        copy.tag = (v as any).tag ?? "Unknown";
        return copy as SerializedVal;
      }
    }
  }

  function serializeFrame(f: Frame): SerializedFrame {
    switch (f.tag) {
      case "KIf":
        return { tag: "KIf", conseq: (f as any).conseq, alt: (f as any).alt, envId: collectCtx((f as any).env) };
      case "KBegin":
        return { tag: "KBegin", rest: (f as any).rest, envId: collectCtx((f as any).env) };
      case "KDefine":
        return { tag: "KDefine", name: (f as any).name, envId: collectCtx((f as any).env) };
      case "KSet":
        return { tag: "KSet", name: (f as any).name, envId: collectCtx((f as any).env) };
      case "KAppFun":
        return { tag: "KAppFun", args: (f as any).args, envId: collectCtx((f as any).env) };
      case "KAppArg":
        return {
          tag: "KAppArg",
          fnVal: serializeVal((f as any).fnVal),
          pending: (f as any).pending,
          acc: ((f as any).acc ?? []).map(serializeVal),
          envId: collectCtx((f as any).env),
        };
      case "KAppArgLazy":
        return {
          tag: "KAppArgLazy",
          fnVal: serializeVal((f as any).fnVal),
          pending: (f as any).pending,
          acc: ((f as any).acc ?? []).map((it: any) => ({ idx: it.idx, val: serializeVal(it.val) })),
          envId: collectCtx((f as any).env),
          totalArgs: (f as any).totalArgs,
          currentIdx: (f as any).currentIdx,
        };
      case "KCall":
        return { tag: "KCall", savedEnvId: collectCtx((f as any).savedEnv) };
      case "KEffect":
        return {
          tag: "KEffect",
          op: (f as any).op,
          pending: (f as any).pending,
          acc: ((f as any).acc ?? []).map(serializeVal),
          envId: collectCtx((f as any).env),
        };
      case "KHandleBoundary":
        return {
          tag: "KHandleBoundary",
          hid: (f as any).hid,
          savedHandlersDepth: (f as any).savedHandlersDepth,
          resumeTo: (f as any).resumeTo
            ? { kont: (f as any).resumeTo.kont.map(serializeFrame), handlersDepth: (f as any).resumeTo.handlersDepth }
            : undefined,
        };
      case "KHandleReturn":
        return {
          tag: "KHandleReturn",
          mode: (f as any).mode,
          hid: (f as any).hid,
          targetKont: (f as any).targetKont.map(serializeFrame),
          targetHandlersDepth: (f as any).targetHandlersDepth,
          savedHandlersDepth: (f as any).savedHandlersDepth,
        };
      case "KPrompt":
        return {
          tag: "KPrompt",
          promptTag: serializeVal((f as any).promptTag),
          handler: serializeVal((f as any).handler),
          envId: collectCtx((f as any).env),
          savedKont: (f as any).savedKont.map(serializeFrame),
          savedHandlersDepth: (f as any).savedHandlersDepth,
        };
      case "KMatch":
        return { tag: "KMatch", clauses: (f as any).clauses, envId: collectCtx((f as any).env) };
      case "KOracleLambda":
        return { tag: "KOracleLambda", params: (f as any).params, envId: collectCtx((f as any).env) };
      case "KBind":
        return { tag: "KBind", fn: serializeVal((f as any).fn), envId: collectCtx((f as any).env) };
      case "KHandlerBind":
        return {
          tag: "KHandlerBind",
          handlers: ((f as any).handlers ?? []).map((h: any) => ({
            type: (h.type as any)?.toString?.() ?? h.type,
            handler: serializeVal(h.handler),
          })),
        };
      case "KRestartBind":
        return {
          tag: "KRestartBind",
          restarts: ((f as any).restarts ?? []).map(serializeRestart),
          savedKont: (f as any).savedKont.map(serializeFrame),
          envId: collectCtx((f as any).env),
          storeEntries: serializeStore((f as any).store),
          handlers: ((f as any).handlers ?? []).map(serializeHandler),
        };
      case "KSignaling":
        return { tag: "KSignaling", condition: serializeVal((f as any).condition), required: (f as any).required };
      default:
        return f as any;
    }
  }

  function serializeHandler(h: HandlerFrame): SerializedHandlerFrame {
    return {
      hid: (h as any).hid,
      envId: collectCtx((h as any).env),
      on: Array.from(((h as any).on ?? new Map()).entries()),
      ret: (h as any).ret,
      fin: (h as any).fin,
    };
  }

  const control: SerializedControl =
    controlInput.tag === "Expr"
      ? { tag: "Expr", e: (controlInput as any).e }
      : { tag: "Val", v: serializeVal((controlInput as any).v) };

  const envId = state.env ? collectCtx(state.env as Ctx) : "null";
  const storeEntries = serializeStore(state.store);

  return {
    control,
    envId,
    storeEntries,
    storeNext: (state.store as any)?.next ?? storeEntries.length,
    kont: (state.kont ?? []).map(serializeFrame),
    handlers: (state.handlers ?? []).map(serializeHandler),
    ctxTable,
    profile: serializeProfile((state as any).profile),
    budget: (state as any).budget ? { ...(state as any).budget } : undefined,
    sec: (state as any).sec ? { caps: (state as any).sec.caps ? Array.from((state as any).sec.caps) : undefined } : undefined,
  };
}

export function deserializeState(s: SerializedState, nativeRegistry: Map<string, Val>): State {
  const ctxInstances: Record<string, Ctx> = {};

  function rebuildCtx(id: string | null | undefined): Ctx | undefined {
    if (!id || id === "null") return undefined;
    if (ctxInstances[id]) return ctxInstances[id];
    const ser = s.ctxTable[id];
    if (!ser) {
      throw new Error(`Missing ctx in table: ${id}`);
    }
    const ctx: Ctx = {
      tag: "Ctx",
      cid: ser.id,
      parent: ser.parentId ? rebuildCtx(ser.parentId) : undefined,
      frame: new Map(ser.frameEntries),
      profile: ser.profile,
      caps: new Set(ser.caps) as any,
      budgets: ser.budgets as any,
      constraints: ser.constraints as any,
      sealed: ser.sealed,
      evidence: ser.evidence as any,
    };
    ctxInstances[id] = ctx;
    return ctx;
  }

  function deserializeStore(entries: [number, SerializedVal][], storeNext?: number): Store {
    const cells = new Map<number, Val>();
    let max = -1;
    for (const [addr, val] of entries) {
      cells.set(addr, deserializeVal(val));
      if (addr > max) max = addr;
    }
    const next = storeNext !== undefined ? storeNext : max + 1;
    return new COWStore(next < 0 ? 0 : next, cells);
  }

  function deserializeRestart(r: SerializedRestart): RestartPoint {
    return {
      name: Symbol.for(r.name ?? "restart"),
      description: r.description,
      kont: (r.kont ?? []).map(deserializeFrame),
      env: rebuildCtx(r.envId) as any,
      store: deserializeStore(r.storeEntries ?? []),
      handlers: (r.handlers ?? []).map(deserializeHandler),
    } as any;
  }

  function deserializeCondition(v: any): ConditionVal {
    return {
      tag: "Condition",
      type: Symbol.for(v.kind ?? "condition"),
      message: v.message ?? "",
      data: v.payload ? deserializeVal(v.payload) : ({ tag: "Unit" } as Val),
      restarts: (v.restarts ?? []).map(deserializeRestart),
    } as any;
  }

  function restoreNative(name: string, arity: number | "variadic", lazyArgs?: number[]): Val {
    const native = nativeRegistry.get(name);
    if (native) return native;
    return {
      tag: "Native",
      name,
      arity,
      lazyArgs,
      fn: () => {
        throw new Error(`Native function not found: ${name}`);
      },
    } as any;
  }

  function deserializeVal(v: SerializedVal): Val {
    switch ((v as any).tag) {
      case "Unit":
      case "Uninit":
      case "Bool":
      case "Num":
      case "Str":
      case "Sym":
      case "Err":
        return v as Val;
      case "Int":
        return { tag: "Int", value: BigInt((v as any).value) } as any;
      case "Pair":
        return { tag: "Pair", car: deserializeVal((v as any).car), cdr: deserializeVal((v as any).cdr) } as Val;
      case "Vector":
        return { tag: "Vector", items: ((v as any).items ?? []).map(deserializeVal) } as Val;
      case "List":
        return { tag: "List", elements: ((v as any).elements ?? []).map(deserializeVal) } as Val;
      case "Map":
        return {
          tag: "Map",
          entries: ((v as any).entries ?? []).map(([k, val]: [SerializedVal, SerializedVal]) => [deserializeVal(k), deserializeVal(val)]),
        } as any;
      case "Tagged":
        return { tag: "Tagged", typeTag: (v as any).typeTag, payload: deserializeVal((v as any).payload) } as any;
      case "Syntax":
        return { tag: "Syntax", stx: (v as any).stx } as any;
      case "Closure":
        return {
          tag: "Closure",
          params: (v as any).params,
          body: (v as any).body,
          env: rebuildCtx((v as any).envId) as any,
        } as any;
      case "Native":
        return restoreNative((v as any).name, (v as any).arity, (v as any).lazyArgs);
      case "OracleProc":
        return {
          tag: "OracleProc",
          params: (v as any).params,
          spec: deserializeVal((v as any).spec),
          env: rebuildCtx((v as any).envId) as any,
          policyDigest: (v as any).policyDigest,
        } as any;
      case "Continuation":
        return {
          tag: "Continuation",
          kont: ((v as any).kont ?? []).map(deserializeFrame),
          env: rebuildCtx((v as any).envId) as any,
          store: deserializeStore((v as any).storeEntries ?? []),
          handlers: ((v as any).handlers ?? []).map(deserializeHandler),
        } as any;
      case "Machine":
        return {
          tag: "Machine",
          state: deserializeState((v as any).state, nativeRegistry),
          label: (v as any).label,
          stepCount: (v as any).stepCount ?? 0,
          breakOnOps: (v as any).breakOnOps ? new Set((v as any).breakOnOps) : undefined,
          breakOnPatterns: (v as any).breakOnPatterns,
          lastOutcome: (v as any).lastOutcome,
          isDone: !!(v as any).isDone,
          machineId: (v as any).machineId,
          parentId: (v as any).parentId,
        } as any;
      case "Dist":
        return {
          tag: "Dist",
          support: ((v as any).support ?? []).map((it: any) => ({ v: deserializeVal(it.v), w: it.w })),
          normalized: (v as any).normalized,
          meta: (v as any).meta,
        } as any;
      case "Meaning": {
        const mv: any = v;
        return {
          tag: "Meaning",
          denotation: mv.denotation !== undefined ? deserializeVal(mv.denotation as any) : undefined,
          residual: mv.residual !== undefined ? deserializeVal(mv.residual as any) : undefined,
          rewrite: mv.rewrite !== undefined ? deserializeVal(mv.rewrite as any) : undefined,
          invariants: mv.invariants !== undefined ? deserializeVal(mv.invariants as any) : undefined,
          effects: mv.effects !== undefined ? deserializeVal(mv.effects as any) : undefined,
          cost: mv.cost !== undefined ? deserializeVal(mv.cost as any) : undefined,
          paths: mv.paths !== undefined ? deserializeVal(mv.paths as any) : undefined,
          deps: mv.deps !== undefined ? deserializeVal(mv.deps as any) : undefined,
          memo: mv.memo !== undefined ? deserializeVal(mv.memo as any) : undefined,
          obligation: mv.obligation !== undefined ? deserializeVal(mv.obligation as any) : undefined,
          obligations: mv.obligations,
          evidence: mv.evidence,
          confidence: mv.confidence,
          trace: mv.trace !== undefined ? deserializeVal(mv.trace as any) : undefined,
          adoptEnvRef: mv.adoptEnvRef,
          adoptStateRef: mv.adoptStateRef,
        } as any;
      }
      case "Profile":
        return { tag: "Profile", profileId: (v as any).profileId, profile: deserializeProfile((v as any).profile) } as any;
      case "Ctx": {
        return { tag: "Ctx", ctx: rebuildCtx((v as any).ctxId) } as any;
      }
      case "Module":
        return {
          tag: "Module",
          moduleId: (v as any).moduleId,
          sealedCtx: rebuildCtx((v as any).sealedCtxId) as any,
          exports: new Set((v as any).exports ?? []),
          meta: (v as any).meta,
        } as any;
      case "ReceiptRef":
        return { tag: "ReceiptRef", rid: (v as any).rid, kind: (v as any).kind } as any;
      case "ConnRef":
        return { tag: "ConnRef", id: (v as any).id, netId: (v as any).netId, name: (v as any).name } as any;
      case "NetRef":
        return { tag: "NetRef", id: (v as any).id, name: (v as any).name } as any;
      case "Explanation":
        return {
          tag: "Explanation",
          kind: (v as any).kind,
          conn: (v as any).conn ? deserializeVal((v as any).conn) : undefined,
          valueHash: (v as any).valueHash,
          because: (v as any).because ? deserializeVal((v as any).because) : undefined,
          rule: (v as any).rule,
          deps: (v as any).deps ? (v as any).deps.map((d: any) => deserializeVal(d)) : undefined,
          left: (v as any).left ? deserializeVal((v as any).left) : undefined,
          right: (v as any).right ? deserializeVal((v as any).right) : undefined,
          message: (v as any).message,
          op: (v as any).op,
          reason: (v as any).reason,
          profile: (v as any).profile,
        } as any;
      case "Contradiction":
        return {
          tag: "Contradiction",
          explanation: deserializeVal((v as any).explanation),
          constraintId: (v as any).constraintId,
          netId: (v as any).netId,
        } as any;
      case "FactStore":
        return {
          tag: "FactStore",
          facts: new Map(((v as any).factsEntries ?? []).map(([k, val]: [string, SerializedVal]) => [k, deserializeVal(val)])),
        } as any;
      case "Stream":
        return {
          tag: "Stream",
          isEmpty: (v as any).isEmpty,
          head: (v as any).head ? deserializeVal((v as any).head) : undefined,
          tail: (v as any).tail ? deserializeVal((v as any).tail) : undefined,
        } as any;
      case "IR":
      case "Budget":
      case "CostEstimate":
      case "Fiber":
      case "Mutex":
      case "IVar":
      case "Channel":
      case "Actor":
      case "Promise":
      case "GenericRegistry":
      case "GenericMiss":
      case "Result":
        return {
          ...(v as any),
          head: (v as any).head ? deserializeVal((v as any).head) : (v as any).head,
          tail: (v as any).tail ? deserializeVal((v as any).tail) : (v as any).tail,
          solution: (v as any).solution ? deserializeVal((v as any).solution) : (v as any).solution,
          remaining: (v as any).remaining ? deserializeVal((v as any).remaining) : (v as any).remaining,
        } as any;
      case "Condition":
        return deserializeCondition(v);
      case "Cont": {
        const base = deserializeState((v as any).resumption.baseState, nativeRegistry);
        return {
          tag: "Cont",
          hid: (v as any).hid,
          boundaryIndex: (v as any).boundaryIndex,
          resumption: {
            rid: (v as any).resumption.rid,
            base,
            invoke: (val: Val) => {
              const nextStore = (base.store as any)?.snapshot ? (base.store as any).snapshot() : base.store;
              const resumed: State = { ...base, control: { tag: "Val", v: val }, store: nextStore };
              (resumed as any).ctrl = resumed.control;
              return resumed;
            },
            digest: () => ((base.store as any)?.digest ? (base.store as any).digest() : String((v as any).resumption.rid)),
          },
        } as any;
      }
      case "Solver":
        return restoreNative((v as any).name, "variadic");
      default:
        return v as any;
    }
  }

  function deserializeFrame(f: SerializedFrame): Frame {
    switch ((f as any).tag) {
      case "KIf":
        return { tag: "KIf", conseq: (f as any).conseq, alt: (f as any).alt, env: rebuildCtx((f as any).envId) as any };
      case "KBegin":
        return { tag: "KBegin", rest: (f as any).rest, env: rebuildCtx((f as any).envId) as any };
      case "KDefine":
        return { tag: "KDefine", name: (f as any).name, env: rebuildCtx((f as any).envId) as any };
      case "KSet":
        return { tag: "KSet", name: (f as any).name, env: rebuildCtx((f as any).envId) as any };
      case "KAppFun":
        return { tag: "KAppFun", args: (f as any).args, env: rebuildCtx((f as any).envId) as any };
      case "KAppArg":
        return {
          tag: "KAppArg",
          fnVal: deserializeVal((f as any).fnVal),
          pending: (f as any).pending,
          acc: ((f as any).acc ?? []).map(deserializeVal),
          env: rebuildCtx((f as any).envId) as any,
        };
      case "KAppArgLazy":
        return {
          tag: "KAppArgLazy",
          fnVal: deserializeVal((f as any).fnVal),
          pending: (f as any).pending,
          acc: ((f as any).acc ?? []).map((it: any) => ({ idx: it.idx, val: deserializeVal(it.val) })),
          env: rebuildCtx((f as any).envId) as any,
          totalArgs: (f as any).totalArgs,
          currentIdx: (f as any).currentIdx,
        };
      case "KCall":
        return { tag: "KCall", savedEnv: rebuildCtx((f as any).savedEnvId) as any };
      case "KEffect":
        return {
          tag: "KEffect",
          op: (f as any).op,
          pending: (f as any).pending,
          acc: ((f as any).acc ?? []).map(deserializeVal),
          env: rebuildCtx((f as any).envId) as any,
        };
      case "KHandleBoundary":
        return {
          tag: "KHandleBoundary",
          hid: (f as any).hid,
          savedHandlersDepth: (f as any).savedHandlersDepth,
          resumeTo: (f as any).resumeTo
            ? { kont: (f as any).resumeTo.kont.map(deserializeFrame), handlersDepth: (f as any).resumeTo.handlersDepth }
            : undefined,
        };
      case "KHandleReturn":
        return {
          tag: "KHandleReturn",
          mode: (f as any).mode,
          hid: (f as any).hid,
          targetKont: (f as any).targetKont.map(deserializeFrame),
          targetHandlersDepth: (f as any).targetHandlersDepth,
          savedHandlersDepth: (f as any).savedHandlersDepth,
        };
      case "KPrompt":
        return {
          tag: "KPrompt",
          promptTag: deserializeVal((f as any).promptTag),
          handler: deserializeVal((f as any).handler),
          env: rebuildCtx((f as any).envId) as any,
          savedKont: (f as any).savedKont.map(deserializeFrame),
          savedHandlersDepth: (f as any).savedHandlersDepth,
        };
      case "KMatch":
        return { tag: "KMatch", clauses: (f as any).clauses, env: rebuildCtx((f as any).envId) as any };
      case "KOracleLambda":
        return { tag: "KOracleLambda", params: (f as any).params, env: rebuildCtx((f as any).envId) as any };
      case "KBind":
        return { tag: "KBind", fn: deserializeVal((f as any).fn), env: rebuildCtx((f as any).envId) as any };
      case "KHandlerBind":
        return {
          tag: "KHandlerBind",
          handlers: ((f as any).handlers ?? []).map((h: any) => ({
            type: (h as any).type === "*" ? "*" : Symbol.for((h as any).type ?? "handler"),
            handler: deserializeVal((h as any).handler),
          })),
        } as any;
      case "KRestartBind":
        return {
          tag: "KRestartBind",
          restarts: ((f as any).restarts ?? []).map(deserializeRestart),
          savedKont: (f as any).savedKont.map(deserializeFrame),
          env: rebuildCtx((f as any).envId) as any,
          store: deserializeStore((f as any).storeEntries ?? []),
          handlers: ((f as any).handlers ?? []).map(deserializeHandler),
        } as any;
      case "KSignaling":
        return { tag: "KSignaling", condition: deserializeVal((f as any).condition), required: (f as any).required } as any;
      default:
        return f as any;
    }
  }

  function deserializeHandler(h: SerializedHandlerFrame): HandlerFrame {
    return {
      hid: (h as any).hid,
      env: rebuildCtx((h as any).envId) as any,
      on: new Map((h as any).on ?? []),
      ret: (h as any).ret,
      fin: (h as any).fin,
    } as any;
  }

  const control: Control =
    s.control.tag === "Expr"
      ? { tag: "Expr", e: (s.control as any).e }
      : { tag: "Val", v: deserializeVal((s.control as any).v) };

  const env = rebuildCtx(s.envId) as any;
  const store = deserializeStore(s.storeEntries, s.storeNext);
  const state: State = {
    control,
    env: env as any,
    store,
    kont: (s.kont ?? []).map(deserializeFrame),
    handlers: (s.handlers ?? []).map(deserializeHandler),
    profile: deserializeProfile(s.profile),
    budget: s.budget as any,
    sec: s.sec ? ({ caps: s.sec.caps ? new Set(s.sec.caps) : undefined } as any) : undefined,
  };
  (state as any).ctrl = control;
  return state;
}
