import type { Env } from "../eval/env";
import type { Store } from "../eval/store";
import type { State, Frame, HandlerFrame, StepOutcome } from "../eval/machine";
import type { Val } from "../eval/values";
import { VFalse, VTrue, VUnit } from "../eval/values";
import type { ConditionHandler, RestartBinding, ConditionVal, RestartPoint, CallableVal } from "./types";

export class UnhandledConditionError extends Error {
  condition: ConditionVal;

  constructor(condition: ConditionVal) {
    super(`Unhandled condition: ${String(condition.type)} - ${condition.message}`);
    this.name = "UnhandledConditionError";
    this.condition = condition;
  }
}

export type ConditionPrimHelpers = {
  applyProcedure: (proc: Val, procArgs: Val[], state: State) => State | StepOutcome;
  ensureArity: (proc: Val, expected: number, name: string) => void;
  isCallable: (proc: Val) => proc is Val;
};

type ActiveRestart = RestartBinding & {
  savedKont: Frame[];
  env: Env;
  store: Store;
  handlers: HandlerFrame[];
};

function asSymbolName(v: Val): string {
  if (v.tag === "Sym") return v.name;
  if (v.tag === "Str") return v.s;
  throw new Error("expected symbol");
}

function asSymbol(v: Val): symbol {
  return Symbol.for(asSymbolName(v));
}

function asString(v: Val): string {
  if (v.tag === "Str") return v.s;
  if (v.tag === "Sym") return v.name;
  if (v.tag === "Num") return String(v.n);
  if (v.tag === "Bool") return v.b ? "true" : "false";
  if (v.tag === "Unit") return "";
  throw new Error("expected string");
}

function listFromArray(items: Val[]): Val {
  let result: Val = VUnit;
  for (let i = items.length - 1; i >= 0; i--) {
    result = { tag: "Vector", items: [items[i], result] };
  }
  return result;
}

function listToArray(list: Val): Val[] {
  const out: Val[] = [];
  let cur: Val = list;
  while (cur.tag === "Vector" && cur.items.length === 2) {
    out.push(cur.items[0]);
    cur = cur.items[1];
  }
  if (cur.tag !== "Unit") {
    throw new Error("expected proper list");
  }
  return out;
}

function parseHandlerList(list: Val, helpers: ConditionPrimHelpers): ConditionHandler[] {
  if (list.tag === "Unit") return [];
  const entries = listToArray(list);
  const handlers: ConditionHandler[] = [];
  for (const entry of entries) {
    if (entry.tag !== "Vector" || entry.items.length < 2) {
      throw new Error("handler-bind: expected (type handler) pair");
    }
    const [typeVal, handlerVal] = entry.items;
    if (!helpers.isCallable(handlerVal)) {
      throw new Error("handler-bind: handler must be procedure");
    }
    const name = asSymbolName(typeVal);
    handlers.push({ type: name === "*" ? "*" : Symbol.for(name), handler: handlerVal });
  }
  return handlers;
}

function parseRestartList(list: Val, helpers: ConditionPrimHelpers): RestartBinding[] {
  if (list.tag === "Unit") return [];
  const entries = listToArray(list);
  const restarts: RestartBinding[] = [];
  for (const entry of entries) {
    if (entry.tag !== "Vector" || entry.items.length < 2) {
      throw new Error("restart-bind: expected (name fn) pair");
    }
    const [nameVal, fnVal] = entry.items;
    if (!helpers.isCallable(fnVal)) {
      throw new Error("restart-bind: restart must be procedure");
    }
    restarts.push({ name: asSymbol(nameVal), fn: fnVal });
  }
  return restarts;
}

function isHandlerFrame(fr: Frame): fr is { tag: "KHandlerBind"; handlers: ConditionHandler[] } {
  return (fr as any).tag === "KHandlerBind";
}

function isRestartFrame(fr: Frame): fr is { tag: "KRestartBind"; restarts: RestartBinding[]; savedKont: Frame[]; env: Env; store: Store; handlers: HandlerFrame[] } {
  return (fr as any).tag === "KRestartBind";
}

export function findConditionHandler(type: symbol, kont: Frame[]): ConditionHandler | null {
  for (let i = kont.length - 1; i >= 0; i--) {
    const fr = kont[i];
    if (isHandlerFrame(fr)) {
      for (const h of fr.handlers) {
        if (h.type === "*" || h.type === type) return h;
      }
    }
  }
  return null;
}

export function findRestart(name: symbol, kont: Frame[]): ActiveRestart | null {
  for (let i = kont.length - 1; i >= 0; i--) {
    const fr = kont[i];
    if (isRestartFrame(fr)) {
      const found = fr.restarts.find(r => r.name === name);
      if (found) {
        return { ...found, savedKont: fr.savedKont.slice(), env: fr.env, store: fr.store, handlers: fr.handlers.slice() };
      }
    }
  }
  return null;
}

export function collectActiveRestarts(state: State): RestartPoint[] {
  const points: RestartPoint[] = [];
  for (let i = state.kont.length - 1; i >= 0; i--) {
    const fr = state.kont[i];
    if (isRestartFrame(fr)) {
      for (const r of fr.restarts) {
        points.push({
          name: r.name,
          description: r.description,
          kont: fr.savedKont.slice(),
          env: fr.env,
          store: fr.store,
          handlers: fr.handlers.slice(),
        });
      }
    }
  }
  return points;
}

function makeCondition(args: Val[], state: State): ConditionVal {
  const [typeVal, messageVal, data] = args;
  return {
    tag: "Condition",
    type: asSymbol(typeVal),
    message: asString(messageVal),
    data,
    restarts: collectActiveRestarts(state),
  };
}

export function registerConditionPrims(def: (name: string, v: Val) => void, helpers: ConditionPrimHelpers): void {
  // (signal type message data)
  def("signal", {
    tag: "Native",
    name: "signal",
    arity: 3,
    fn: (args, state) => {
      const condition = makeCondition(args, state);
      const handler = findConditionHandler(condition.type, state.kont);
      if (!handler) {
        return { ...state, control: { tag: "Val", v: VUnit } };
      }
      const kont = state.kont.concat([{ tag: "KSignaling", condition, required: false } as Frame]);
      return helpers.applyProcedure(handler.handler, [condition], { ...state, kont });
    },
  });

  // (error type message data)
  def("error", {
    tag: "Native",
    name: "error",
    arity: 3,
    fn: (args, state) => {
      const condition = makeCondition(args, state);
      const handler = findConditionHandler(condition.type, state.kont);
      if (!handler) {
        throw new UnhandledConditionError(condition);
      }
      const kont = state.kont.concat([{ tag: "KSignaling", condition, required: true } as Frame]);
      return helpers.applyProcedure(handler.handler, [condition], { ...state, kont });
    },
  });

  // (handler-bind ((type handler) ...) thunk)
  def("handler-bind", {
    tag: "Native",
    name: "handler-bind",
    arity: 2,
    fn: (args, state) => {
      const [handlersVal, thunk] = args;
      if (!helpers.isCallable(thunk)) {
        throw new Error("handler-bind: expected thunk");
      }
      helpers.ensureArity(thunk, 0, "handler-bind");
      const handlers = parseHandlerList(handlersVal, helpers);
      const kont = state.kont.concat([{ tag: "KHandlerBind", handlers } as Frame]);
      return helpers.applyProcedure(thunk, [], { ...state, kont });
    },
  });

  // (restart-bind ((name fn) ...) thunk)
  def("restart-bind", {
    tag: "Native",
    name: "restart-bind",
    arity: 2,
    fn: (args, state) => {
      const [restartsVal, thunk] = args;
      if (!helpers.isCallable(thunk)) {
        throw new Error("restart-bind: expected thunk");
      }
      helpers.ensureArity(thunk, 0, "restart-bind");
      const restarts = parseRestartList(restartsVal, helpers);
      const frame: Frame = {
        tag: "KRestartBind",
        restarts,
        savedKont: state.kont.slice(),
        env: state.env,
        store: state.store,
        handlers: state.handlers.slice(),
      } as Frame;
      const kont = state.kont.concat([frame]);
      return helpers.applyProcedure(thunk, [], { ...state, kont });
    },
  });

  // (invoke-restart name value...)
  def("invoke-restart", {
    tag: "Native",
    name: "invoke-restart",
    arity: "variadic",
    fn: (args, state) => {
      if (args.length < 1) throw new Error("invoke-restart: expected at least 1 argument");
      const [nameVal, ...rest] = args;
      const restart = findRestart(asSymbol(nameVal), state.kont);
      if (!restart) {
        throw new Error(`invoke-restart: no restart named ${asSymbolName(nameVal)}`);
      }
      const jumpState: State = {
        ...state,
        env: restart.env,
        store: restart.store,
        kont: restart.savedKont,
        handlers: restart.handlers,
      };
      return helpers.applyProcedure(restart.fn, rest, jumpState);
    },
  });

  // (find-restart name)
  def("find-restart", {
    tag: "Native",
    name: "find-restart",
    arity: 1,
    fn: (args, state) => {
      const restart = findRestart(asSymbol(args[0]), state.kont);
      return { ...state, control: { tag: "Val", v: restart ? VTrue : VFalse } };
    },
  });

  // (compute-restarts)
  def("compute-restarts", {
    tag: "Native",
    name: "compute-restarts",
    arity: 0,
    fn: (_args, state) => {
      const names = collectActiveRestarts(state).map(r => ({ tag: "Sym", name: Symbol.keyFor(r.name) ?? String(r.name) } as Val));
      const list = listFromArray(names);
      return { ...state, control: { tag: "Val", v: list } };
    },
  });
}
