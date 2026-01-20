// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-4.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.
// Prompt 9: Integrated governance enforcement at chokepoints

import type { Expr, HandlerExpr, Pattern } from "../ast";
import type { State, StepOutcome, Frame, HandlerFrame } from "./machine";
import type { Env } from "./env";
import { envGet, envSet } from "./env";
import type { Val } from "./values";
import { VUnit, VFalse, VTrue } from "./values";
import { captureValueResumption } from "../effects/capture";
import {
  checkEffectAllowed,
  debitEffect,
  checkStepBudget,
  debitStep,
} from "../governance/enforcement";
import { UnhandledConditionError } from "../conditions/prims";

// Compatibility export for tests that expected StepResult alias
export type StepResult = StepOutcome;

function uuid(): string {
  return Math.random().toString(16).slice(2) + "-" + Date.now().toString(16);
}

function ctxDigest(env: Env): string {
  // Ctx has a content-addressed cid - use it directly
  return env.cid;
}

function push(kont: Frame[], fr: Frame): Frame[] {
  const k2 = kont.slice();
  k2.push(fr);
  return k2;
}

function pop(kont: Frame[]): [Frame | undefined, Frame[]] {
  if (kont.length === 0) return [undefined, kont];
  const k2 = kont.slice();
  const fr = k2.pop();
  return [fr, k2];
}

function isBool(v: Val): v is { tag: "Bool"; b: boolean } {
  return v.tag === "Bool";
}

function findHandlerIndexByHid(handlers: HandlerFrame[], hid: string): number {
  for (let i = handlers.length - 1; i >= 0; i--) {
    if (handlers[i].hid === hid) return i;
  }
  return -1;
}

/** Convert quoted datum to runtime value (minimal). Extend as needed.
 * Arrays are converted to proper cons-cell lists (Vector with 2 items each)
 * for compatibility with the list primitives (car, cdr, length, etc.)
 */
function datumToVal(d: unknown): Val {
  if (d === null) return { tag: "Unit" };
  if (typeof d === "number") return { tag: "Num", n: d };
  if (typeof d === "string") return { tag: "Str", s: d };
  if (typeof d === "boolean") return { tag: "Bool", b: d };
  if (Array.isArray(d)) {
    // Build proper cons-cell list: (a b c) -> [a, [b, [c, Unit]]]
    let result: Val = { tag: "Unit" };
    for (let i = d.length - 1; i >= 0; i--) {
      result = { tag: "Vector", items: [datumToVal(d[i]), result] };
    }
    return result;
  }
  // symbols in datum: represent as {tag:"Sym"} if encoded as {sym:"x"}
  if (typeof d === "object" && d !== null && "sym" in d) {
    return { tag: "Sym", name: (d as { sym: string }).sym };
  }
  return { tag: "Str", s: JSON.stringify(d) };
}

function buildHandlerFrame(handler: HandlerExpr, env: Env): HandlerFrame {
  const hid = uuid();
  const on = new Map<string, { op: string; params: string[]; k: string; body: Expr }>();
  for (const c of handler.on) on.set(c.op, c);
  return { hid, env, on, ret: handler.ret, fin: handler.fin };
}

/** Pattern matching helper for Match expressions */
function matchPatternValue(p: Pattern, v: Val, env = new Map<string, Val>()): Map<string, Val> | null {
  switch (p.tag) {
    case "PWild":
      return env;

    case "PVar": {
      const prev = env.get(p.name);
      if (!prev) {
        const e2 = new Map(env);
        e2.set(p.name, v);
        return e2;
      }
      // Same var bound twice: check equality
      return valEq(prev, v) ? env : null;
    }

    case "PLit": {
      if (p.value === null) return v.tag === "Unit" ? env : null;
      if (typeof p.value === "number") return v.tag === "Num" && v.n === p.value ? env : null;
      if (typeof p.value === "boolean") return v.tag === "Bool" && v.b === p.value ? env : null;
      if (typeof p.value === "string") {
        // String pattern can match either Str or Sym
        if (v.tag === "Str" && v.s === p.value) return env;
        if (v.tag === "Sym" && v.name === p.value) return env;
        return null;
      }
      return null;
    }

    case "PVector": {
      if (v.tag !== "Vector") return null;

      // Direct vector match (flat vectors)
      if (v.items.length === p.items.length) {
        let ecur: Map<string, Val> | null = env;
        for (let i = 0; i < p.items.length; i++) {
          ecur = matchPatternValue(p.items[i], v.items[i], ecur!);
          if (!ecur) return null;
        }
        return ecur;
      }

      // Try cons-cell list match: Vector[a, Vector[b, Vector[c, Unit]]] matches pattern (a b c)
      // A cons-cell is a 2-element vector where items[1] is another cons or Unit
      if (p.items.length > 0 && v.items.length === 2) {
        // Convert cons-cell list to flat array for matching
        const flatList: Val[] = [];
        let cur: Val = v;
        while (cur.tag === "Vector" && cur.items.length === 2) {
          flatList.push(cur.items[0]);
          cur = cur.items[1];
        }
        // Check if terminated by Unit (proper list)
        if (cur.tag !== "Unit") return null;

        // Now match pattern items against flat list
        if (flatList.length !== p.items.length) return null;
        let ecur: Map<string, Val> | null = env;
        for (let i = 0; i < p.items.length; i++) {
          ecur = matchPatternValue(p.items[i], flatList[i], ecur!);
          if (!ecur) return null;
        }
        return ecur;
      }

      return null;
    }
  }
}

function valEq(a: Val, b: Val): boolean {
  if (a.tag !== b.tag) return false;
  switch (a.tag) {
    case "Unit": return true;
    case "Num": return (b as any).n === a.n;
    case "Bool": return (b as any).b === a.b;
    case "Str": return (b as any).s === a.s;
    case "Sym": return (b as any).name === a.name;
    case "Vector": {
      const bb = b as any;
      if (bb.items.length !== a.items.length) return false;
      for (let i = 0; i < a.items.length; i++) if (!valEq(a.items[i], bb.items[i])) return false;
      return true;
    }
    default:
      return JSON.stringify(a) === JSON.stringify(b);
  }
}

/**
 * Apply a value as a function (closure/native/cont/oracle-proc).
 * Returns StepOutcome: either a state transition or an effect operation.
 */
function applyVal(fnVal: Val, args: Val[], st: State): StepOutcome {
  // Cont: delimited resumption call
  if (fnVal.tag === "Cont") {
    if (args.length !== 1) throw new Error(`Cont apply arity mismatch: got ${args.length}`);
    const callerKont = st.kont;               // continuation expecting result of the (k arg) call
    const callerHandlersDepth = st.handlers.length;

    const baseResumed = fnVal.resumption.invoke(args[0]); // resumes at op site with effect result = args[0]

    // Patch the handle boundary in resumed kont to "resume mode" returning to callerKont.
    const k = baseResumed.kont.slice();
    const idx = fnVal.boundaryIndex;
    const fr = k[idx];
    if (!fr || fr.tag !== "KHandleBoundary" || fr.hid !== fnVal.hid) {
      throw new Error("Cont invoke: boundary frame mismatch");
    }

    k[idx] = {
      ...fr,
      resumeTo: { kont: callerKont, handlersDepth: callerHandlersDepth },
    };

    return {
      tag: "State",
      state: {
        ...baseResumed,
        kont: k,
        // handlers remain as captured at op site; boundary frame will truncate on resume-return
      },
    };
  }

  // First-class continuation (call/cc, prompts)
  if (fnVal.tag === "Continuation") {
    if (args.length !== 1) {
      throw new Error(`Continuation apply arity mismatch: expected 1, got ${args.length}`);
    }
    return {
      tag: "State",
      state: {
        ...st,
        control: { tag: "Val", v: args[0] },
        env: fnVal.env,
        // Use the current store to preserve mutations performed before invoking the continuation.
        store: st.store,
        kont: fnVal.kont.slice(),
        handlers: fnVal.handlers.slice(),
      },
    };
  }

  // Native function: host-implemented (primitives, etc.)
  if (fnVal.tag === "Native") {
    const result = fnVal.fn(args, st);
    if ((result as any).tag === "State" || (result as any).tag === "Op" || (result as any).tag === "Done") {
      return result as StepOutcome;
    }
    return { tag: "State", state: result as State };
  }

  // Closure
  if (fnVal.tag === "Closure") {
    if (args.length !== fnVal.params.length) {
      throw new Error(`Closure apply arity mismatch: expected ${fnVal.params.length}, got ${args.length}`);
    }
    // allocate params in store
    let store = st.store;
    let env = fnVal.env;
    for (let i = 0; i < fnVal.params.length; i++) {
      const [store2, addr] = store.alloc(args[i]);
      store = store2;
      env = envSet(env, fnVal.params[i], addr);
    }
    // push call-return frame to restore env after body
    const kont = push(st.kont, { tag: "KCall", savedEnv: st.env });
    return {
      tag: "State",
      state: {
        ...st,
        control: { tag: "Expr", e: fnVal.body },
        env,
        store,
        kont,
      },
    };
  }

  // OracleProc: LLM in apply position - emit oracle.apply.op
  if (fnVal.tag === "OracleProc") {
    const suspended: State = { ...st, control: { tag: "Val", v: VUnit } };
    const resumption = captureValueResumption(suspended);
    const opcall = {
      op: "oracle.apply.op",
      args: [fnVal, { tag: "Vector", items: args } as Val],
      ctxDigest: ctxDigest(st.env),
      resumption,
    };
    return { tag: "Op", opcall, state: suspended };
  }

  throw new Error(`Attempted to apply non-callable value: ${fnVal.tag}`);
}

function applyFrame(fr: Frame, v: Val, st: State): StepOutcome {
  switch (fr.tag) {
    case "KIf": {
      if (!isBool(v)) throw new Error("if test must be boolean");
      return {
        tag: "State",
        state: {
          ...st,
          control: { tag: "Expr", e: v.b ? fr.conseq : fr.alt },
          env: fr.env,
        },
      };
    }

    case "KBegin": {
      if (fr.rest.length === 0) {
        return { tag: "State", state: { ...st, control: { tag: "Val", v } } };
      }
      const [e0, ...rest] = fr.rest;
      // IMPORTANT: Use st.env (not fr.env) to preserve bindings from define
      return {
        tag: "State",
        state: {
          ...st,
          control: { tag: "Expr", e: e0 },
          env: st.env,
          kont: push(st.kont, { tag: "KBegin", rest, env: st.env }),
        },
      };
    }

    case "KDefine": {
      // define returns Unit
      const addr0 = envGet(fr.env, fr.name);
      if (addr0 !== undefined) {
        const store2 = st.store.write(addr0, v);
        return {
          tag: "State",
          state: { ...st, control: { tag: "Val", v: VUnit }, env: fr.env, store: store2 },
        };
      } else {
        const [store2, addr] = st.store.alloc(v);
        const env2 = envSet(fr.env, fr.name, addr);
        return {
          tag: "State",
          state: { ...st, control: { tag: "Val", v: VUnit }, env: env2, store: store2 },
        };
      }
    }

    case "KSet": {
      // Enforce seal: set! denied if context is sealed
      if (fr.env.sealed) {
        throw new Error(`set! denied: context is sealed (name=${fr.name})`);
      }
      const addr = envGet(fr.env, fr.name);
      if (addr === undefined) throw new Error(`set!: unbound var ${fr.name}`);
      const store2 = st.store.write(addr, v);
      return {
        tag: "State",
        state: { ...st, control: { tag: "Val", v: VUnit }, env: fr.env, store: store2 },
      };
    }

    case "KAppFun": {
      // Native with lazy argument indices: wrap specified args as zero-arg thunks (closures)
      if (v.tag === "Native" && Array.isArray((v as any).lazyArgs) && (v as any).lazyArgs.length > 0) {
        const lazyIdx: number[] = (v as any).lazyArgs;
        const totalArgs = fr.args.length;
        const acc: Array<{ idx: number; val: Val }> = [];
        const pending: Array<{ expr: Expr; idx: number }> = [];

        fr.args.forEach((arg, idx) => {
          if (lazyIdx.includes(idx)) {
            const thunk: Val = { tag: "Closure", params: [], body: arg, env: fr.env };
            acc.push({ idx, val: thunk });
          } else {
            pending.push({ expr: arg, idx });
          }
        });

        if (pending.length === 0) {
          const ordered: Val[] = new Array(totalArgs);
          for (const { idx, val } of acc) ordered[idx] = val;
          return applyVal(v, ordered, { ...st, env: fr.env });
        }

        const [next, ...rest] = pending;
        const kontFrame: Frame = {
          tag: "KAppArgLazy",
          fnVal: v,
          pending: rest,
          acc,
          env: fr.env,
          totalArgs,
          currentIdx: next.idx,
        };
        return {
          tag: "State",
          state: {
            ...st,
            control: { tag: "Expr", e: next.expr },
            env: fr.env,
            kont: push(st.kont, kontFrame),
          },
        };
      }

      if (fr.args.length === 0) {
        // apply immediately - applyVal returns StepOutcome directly
        return applyVal(v, [], { ...st, env: fr.env });
      }
      const [a0, ...rest] = fr.args;
      return {
        tag: "State",
        state: {
          ...st,
          control: { tag: "Expr", e: a0 },
          env: fr.env,
          kont: push(st.kont, { tag: "KAppArg", fnVal: v, pending: rest, acc: [], env: fr.env }),
        },
      };
    }

    case "KAppArg": {
      const acc2 = fr.acc.concat([v]);
      if (fr.pending.length === 0) {
        // apply fnVal to accumulated args - applyVal returns StepOutcome directly
        return applyVal(fr.fnVal, acc2, { ...st, env: fr.env });
      }
      const [a0, ...rest] = fr.pending;
      return {
        tag: "State",
        state: {
          ...st,
          control: { tag: "Expr", e: a0 },
          env: fr.env,
          kont: push(st.kont, { tag: "KAppArg", fnVal: fr.fnVal, pending: rest, acc: acc2, env: fr.env }),
        },
      };
    }

    case "KAppArgLazy": {
      const acc2 = fr.acc.concat([{ idx: fr.currentIdx, val: v }]);
      if (fr.pending.length === 0) {
        const ordered: Val[] = new Array(fr.totalArgs);
        for (const { idx, val } of acc2) ordered[idx] = val;
        return applyVal(fr.fnVal, ordered, { ...st, env: fr.env });
      }
      const [next, ...rest] = fr.pending;
      const kontFrame: Frame = {
        ...fr,
        pending: rest,
        acc: acc2,
        currentIdx: next.idx,
      };
      return {
        tag: "State",
        state: {
          ...st,
          control: { tag: "Expr", e: next.expr },
          env: fr.env,
          kont: push(st.kont, kontFrame),
        },
      };
    }

    case "KCall": {
      // restore environment after closure body returns
      return { tag: "State", state: { ...st, control: { tag: "Val", v }, env: fr.savedEnv } };
    }

    case "KEffect": {
      const acc2 = fr.acc.concat([v]);
      if (fr.pending.length === 0) {
        // Prompt 9 Chokepoint A: Debit the effect when emitting
        const stDebited = debitEffect(st, fr.op);
        // perform operation now: emit OpCall with a multi-shot resumption
        const suspended: State = { ...stDebited, control: { tag: "Val", v: VUnit }, env: fr.env }; // placeholder
        const resumption = captureValueResumption(suspended);
        const opcall = { op: fr.op, args: acc2, ctxDigest: ctxDigest(fr.env), resumption };
        return { tag: "Op", opcall, state: suspended };
      }
      const [e0, ...rest] = fr.pending;
      return {
        tag: "State",
        state: {
          ...st,
          control: { tag: "Expr", e: e0 },
          env: fr.env,
          kont: push(st.kont, { tag: "KEffect", op: fr.op, pending: rest, acc: acc2, env: fr.env }),
        },
      };
    }

    case "KHandleBoundary": {
      // Body (or clause) has returned a value v to the handler boundary.
      const hIdx = findHandlerIndexByHid(st.handlers, fr.hid);
      if (hIdx < 0) throw new Error("KHandleBoundary: handler not found");
      const hf = st.handlers[hIdx];

      // Determine whether we are exiting handle or resuming to a caller continuation.
      const mode: "exit" | "resume" = fr.resumeTo ? "resume" : "exit";
      const targetKont = mode === "resume" ? fr.resumeTo!.kont : st.kont; // st.kont already has outer continuation after pop
      const targetHandlersDepth = mode === "resume" ? fr.resumeTo!.handlersDepth : fr.savedHandlersDepth;

      // Apply return clause (if any) by evaluating it under handler env with v bound.
      if (hf.ret) {
        const param = hf.ret.v;
        const body = hf.ret.body;

        // allocate param in store
        let store = st.store;
        const [store2, addr] = store.alloc(v);
        store = store2;
        const env2 = envSet(hf.env, param, addr);

        // After ret body computes v2, KHandleReturn decides whether to exit or resume.
        return {
          tag: "State",
          state: {
            ...st,
            control: { tag: "Expr", e: body },
            env: env2,
            store,
            kont: push(targetKont, {
              tag: "KHandleReturn",
              mode,
              hid: fr.hid,
              targetKont,
              targetHandlersDepth,
              savedHandlersDepth: fr.savedHandlersDepth,
            }),
            // handlers remain as-is during return-clause evaluation; KHandleReturn will truncate.
          },
        };
      }

      // No return clause: direct value
      const st2: State = {
        ...st,
        control: { tag: "Val", v },
        kont: targetKont,
        handlers: st.handlers.slice(0, targetHandlersDepth),
      };
      return { tag: "State", state: st2 };
    }

    case "KHandleReturn": {
      // v is result of return clause evaluation
      const st2: State = {
        ...st,
        control: { tag: "Val", v },
        kont: fr.targetKont,
        handlers: st.handlers.slice(0, fr.targetHandlersDepth),
      };
      return { tag: "State", state: st2 };
    }

    case "KMatch": {
      // v is the scrutinee; try each clause
      for (const cl of fr.clauses) {
        const binds = matchPatternValue(cl.pat, v);
        if (!binds) continue;

        // Bind matched values in env
        let store = st.store;
        let env2 = fr.env;
        for (const [name, val] of binds.entries()) {
          const [s2, addr] = store.alloc(val);
          store = s2;
          env2 = envSet(env2, name, addr);
        }

        return {
          tag: "State",
          state: { ...st, control: { tag: "Expr", e: cl.body }, env: env2, store },
        };
      }
      throw new Error("match: no clause matched");
    }

    case "KOracleLambda": {
      // v is the evaluated spec; create an OracleProc capturing lexical env
      const oracleProc: Val = {
        tag: "OracleProc",
        params: fr.params,
        spec: v,
        env: fr.env,
      };
      return { tag: "State", state: { ...st, control: { tag: "Val", v: oracleProc } } };
    }

    case "KPrompt": {
      const handlersTrunc = st.handlers.slice(0, fr.savedHandlersDepth);
      return {
        tag: "State",
        state: {
          ...st,
          control: { tag: "Val", v },
          env: fr.env,
          kont: fr.savedKont,
          handlers: handlersTrunc,
        },
      };
    }

    case "KHandlerBind": {
      return { tag: "State", state: { ...st, control: { tag: "Val", v } } };
    }

    case "KRestartBind": {
      return { tag: "State", state: { ...st, control: { tag: "Val", v } } };
    }

    case "KSignaling": {
      if (fr.required) {
        throw new UnhandledConditionError(fr.condition);
      }
      return { tag: "State", state: { ...st, control: { tag: "Val", v } } };
    }

    case "KBind": {
      return applyVal(fr.fn, [v], { ...st, env: fr.env });
    }

    default: {
      const _exh: never = fr;
      return _exh;
    }
  }
}

export function stepOnce(st: State): StepOutcome {
  // Prompt 9: Check step budget before each step
  checkStepBudget(st);

  // Expr step
  if (st.control.tag === "Expr") {
    const e = st.control.e;

    switch (e.tag) {
      case "Lit": {
        const v: Val =
          e.value === null ? VUnit :
          typeof e.value === "number" ? { tag: "Num", n: e.value } :
          typeof e.value === "boolean" ? { tag: "Bool", b: e.value } :
          { tag: "Str", s: String(e.value) };
        return { tag: "State", state: { ...st, control: { tag: "Val", v } } };
      }

      case "Var": {
        const addr = envGet(st.env, e.name);
        if (addr === undefined) throw new Error(`unbound var ${e.name}`);
        const v = st.store.read(addr);
        return { tag: "State", state: { ...st, control: { tag: "Val", v } } };
      }

      case "Lambda": {
        const v: Val = { tag: "Closure", params: e.params, body: e.body, env: st.env };
        return { tag: "State", state: { ...st, control: { tag: "Val", v } } };
      }

      case "OracleLambda": {
        // oracle-lambda creates an OracleProc that captures the lexical environment
        // The spec is evaluated to a value that the oracle will use as its goal/prompt
        const kont = push(st.kont, { tag: "KOracleLambda", params: e.params, env: st.env });
        return { tag: "State", state: { ...st, control: { tag: "Expr", e: e.spec }, kont } };
      }

      case "If": {
        const kont = push(st.kont, { tag: "KIf", conseq: e.conseq, alt: e.alt, env: st.env });
        return { tag: "State", state: { ...st, control: { tag: "Expr", e: e.test }, kont } };
      }

      case "Begin": {
        if (e.exprs.length === 0) return { tag: "State", state: { ...st, control: { tag: "Val", v: VUnit } } };
        const [e0, ...rest] = e.exprs;
        const kont = push(st.kont, { tag: "KBegin", rest, env: st.env });
        return { tag: "State", state: { ...st, control: { tag: "Expr", e: e0 }, kont } };
      }

      case "Define": {
        // Pre-allocate binding with placeholder to support recursive definitions
        // This ensures lambda bodies capture an env that includes the binding
        const existingAddr = envGet(st.env, e.name);
        if (existingAddr !== undefined) {
          // Already allocated (re-definition) - just evaluate RHS and overwrite
          const kont = push(st.kont, { tag: "KDefine", name: e.name, env: st.env });
          return { tag: "State", state: { ...st, control: { tag: "Expr", e: e.rhs }, kont } };
        }
        // Allocate placeholder, add to env, then evaluate RHS
        const placeholder: Val = { tag: "Unit" }; // will be overwritten
        const [store2, addr] = st.store.alloc(placeholder);
        const env2 = envSet(st.env, e.name, addr);
        const kont = push(st.kont, { tag: "KDefine", name: e.name, env: env2 });
        return { tag: "State", state: { ...st, control: { tag: "Expr", e: e.rhs }, env: env2, store: store2, kont } };
      }

      case "Set": {
        const kont = push(st.kont, { tag: "KSet", name: e.name, env: st.env });
        return { tag: "State", state: { ...st, control: { tag: "Expr", e: e.rhs }, kont } };
      }

      case "App": {
        const kont = push(st.kont, { tag: "KAppFun", args: e.args, env: st.env });
        return { tag: "State", state: { ...st, control: { tag: "Expr", e: e.fn }, kont } };
      }

      case "Quote": {
        const v = datumToVal(e.datum);
        return { tag: "State", state: { ...st, control: { tag: "Val", v } } };
      }

      case "Effect": {
        // Prompt 9 Chokepoint A: Check effect is allowed before emission
        checkEffectAllowed(st, e.op);

        if (e.args.length === 0) {
          // Debit the effect before emitting
          const stDebited = debitEffect(st, e.op);
          const suspended: State = { ...stDebited, control: { tag: "Val", v: VUnit } };
          const resumption = captureValueResumption(suspended);
          const opcall = { op: e.op, args: [], ctxDigest: ctxDigest(st.env), resumption };
          return { tag: "Op", opcall, state: suspended };
        }
        const [a0, ...rest] = e.args;
        const kont = push(st.kont, { tag: "KEffect", op: e.op, pending: rest, acc: [], env: st.env });
        return { tag: "State", state: { ...st, control: { tag: "Expr", e: a0 }, kont } };
      }

      case "Handle": {
        const hf = buildHandlerFrame(e.handler, st.env);
        const savedHandlersDepth = st.handlers.length;
        const handlers2 = st.handlers.concat([hf]);
        const boundary: Frame = { tag: "KHandleBoundary", hid: hf.hid, savedHandlersDepth };
        const kont2 = push(st.kont, boundary);
        return { tag: "State", state: { ...st, control: { tag: "Expr", e: e.body }, handlers: handlers2, kont: kont2 } };
      }

      case "Match": {
        // Evaluate the scrutinee first, then apply pattern matching in KMatch frame
        const kont = push(st.kont, { tag: "KMatch", clauses: e.clauses, env: st.env });
        return { tag: "State", state: { ...st, control: { tag: "Expr", e: e.scrutinee }, kont } };
      }

      case "QuoteSyntax": {
        const v: Val = { tag: "Str", s: JSON.stringify(e.datum) };
        return { tag: "State", state: { ...st, control: { tag: "Val", v } } };
      }

      case "Let":
      case "Letrec": {
        // Minimal handling: evaluate the body assuming bindings already processed elsewhere.
        return { tag: "State", state: { ...st, control: { tag: "Expr", e: (e as any).body } } };
      }

      default: {
        return { tag: "State", state: st };
      }
    }
  }

  // Val step: apply continuation frame or finish
  const v = st.control.v;
  const [fr, kont2] = pop(st.kont);
  if (!fr) {
    return { tag: "Done", value: v, state: st };
  }

  const st2: State = { ...st, kont: kont2 };
  return applyFrame(fr, v, st2);
}
