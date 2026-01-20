// src/core/prims.ts
// Production primitives for OmegaLLM CEKS machine
// Originally from test/helpers/prims.ts - moved to production

import type { Env } from "./eval/env";
import { envEmpty, envSet } from "./eval/env";
import type { Store } from "./eval/store";
import type { State, Frame } from "./eval/machine";
import type { Val } from "./eval/values";
import { VUnit, VTrue, VFalse } from "./eval/values";
import { rule, rewriteOnce, rewriteFixpoint, rewriteTrace, detectConflicts, substitute, type Rule, type Strategy } from "./oracle/trs";
import { matchAST } from "./oracle/match";
import { sha256JSON } from "./artifacts/hash";
import { isMeaning } from "./oracle/meaning";

export function installPrims(store: Store): { env: Env; store: Store } {
  let env: Env = envEmpty();
  let st: Store = store;

  function def(name: string, v: Val) {
    const [st2, addr] = st.alloc(v);
    st = st2;
    env = envSet(env, name, addr);
  }

  function isCallable(proc: Val): proc is { tag: "Native" | "Closure" } {
    return proc.tag === "Native" || proc.tag === "Closure";
  }

  function ensureArity(proc: Val, expected: number, name: string): void {
    if (proc.tag === "Native") {
      if (proc.arity !== "variadic" && proc.arity !== expected) {
        throw new Error(`${name}: expected procedure of arity ${expected}`);
      }
      return;
    }
    if (proc.tag === "Closure") {
      if (proc.params.length !== expected) {
        throw new Error(`${name}: expected procedure of arity ${expected}`);
      }
      return;
    }
    throw new Error(`${name}: expected a procedure`);
  }

  function applyProcedure(proc: Val, procArgs: Val[], state: State): State {
    if (proc.tag === "Native") {
      return proc.fn(procArgs, state);
    }
    if (proc.tag === "Closure") {
      if (proc.params.length !== procArgs.length) {
        throw new Error(`procedure arity mismatch: expected ${proc.params.length}, got ${procArgs.length}`);
      }
      let store = state.store;
      let env2 = proc.env;
      for (let i = 0; i < proc.params.length; i++) {
        const [store2, addr] = store.alloc(procArgs[i]);
        store = store2;
        env2 = envSet(env2, proc.params[i], addr);
      }
      const kont = state.kont.concat([{ tag: "KCall", savedEnv: state.env } as Frame]);
      return { ...state, control: { tag: "Expr", e: proc.body }, env: env2, store, kont };
    }
    throw new Error("expected procedure");
  }

  function promptKey(v: Val): string {
    switch (v.tag) {
      case "Sym": return `sym:${v.name}`;
      case "Str": return `str:${v.s}`;
      case "Num": return `num:${v.n}`;
      case "Bool": return `bool:${v.b}`;
      default:
        try {
          return `${v.tag}:${JSON.stringify(v)}`;
        } catch {
          return v.tag;
        }
    }
  }

  // *uninit*: placeholder for letrec uninitialized bindings
  // Returns Unit as a placeholder value that will be overwritten by set!
  def("*uninit*", { tag: "Native", name: "*uninit*", arity: 0, fn: (_args, s) => {
    return { ...s, control: { tag: "Val", v: VUnit } };
  }});

  // arithmetic and predicates (reference-grade)
  def("+", { tag: "Native", name: "+", arity: "variadic", fn: (args, s) => ({ ...s, control: { tag: "Val", v: { tag: "Num", n: args.reduce((a, x) => a + (x as any).n, 0) } } }) });
  def("-", { tag: "Native", name: "-", arity: "variadic", fn: (args, s) => {
    const ns = args.map(a => (a as any).n as number);
    const n = ns.length === 1 ? -ns[0] : ns.slice(1).reduce((a, x) => a - x, ns[0]);
    return { ...s, control: { tag: "Val", v: { tag: "Num", n } } };
  }});

  def("*", { tag: "Native", name: "*", arity: "variadic", fn: (args, s) => {
    const n = args.reduce((a, x) => a * (x as any).n, 1);
    return { ...s, control: { tag: "Val", v: { tag: "Num", n } } };
  }});

  def("=", { tag: "Native", name: "=", arity: 2, fn: (args, s) => {
    const a = (args[0] as any).n, b = (args[1] as any).n;
    return { ...s, control: { tag: "Val", v: a === b ? VTrue : VFalse } };
  }});

  def("<", { tag: "Native", name: "<", arity: 2, fn: (args, s) => {
    const a = (args[0] as any).n, b = (args[1] as any).n;
    return { ...s, control: { tag: "Val", v: a < b ? VTrue : VFalse } };
  }});

  def(">", { tag: "Native", name: ">", arity: 2, fn: (args, s) => {
    const a = (args[0] as any).n, b = (args[1] as any).n;
    return { ...s, control: { tag: "Val", v: a > b ? VTrue : VFalse } };
  }});

  def("<=", { tag: "Native", name: "<=", arity: 2, fn: (args, s) => {
    const a = (args[0] as any).n, b = (args[1] as any).n;
    return { ...s, control: { tag: "Val", v: a <= b ? VTrue : VFalse } };
  }});

  def(">=", { tag: "Native", name: ">=", arity: 2, fn: (args, s) => {
    const a = (args[0] as any).n, b = (args[1] as any).n;
    return { ...s, control: { tag: "Val", v: a >= b ? VTrue : VFalse } };
  }});

  def("/", { tag: "Native", name: "/", arity: 2, fn: (args, s) => {
    const a = (args[0] as any).n, b = (args[1] as any).n;
    return { ...s, control: { tag: "Val", v: { tag: "Num", n: a / b } } };
  }});

  def("modulo", { tag: "Native", name: "modulo", arity: 2, fn: (args, s) => {
    const a = (args[0] as any).n, b = (args[1] as any).n;
    return { ...s, control: { tag: "Val", v: { tag: "Num", n: a % b } } };
  }});

  def("not", { tag: "Native", name: "not", arity: 1, fn: (args, s) => {
    const b = (args[0] as any).b as boolean;
    return { ...s, control: { tag: "Val", v: (!b ? VTrue : VFalse) } };
  }});

  def("unit", { tag: "Native", name: "unit", arity: 0, fn: (_args, s) => ({ ...s, control: { tag: "Val", v: VUnit } }) });

  // ─────────────────────────────────────────────────────────────────
  // Continuations and prompts
  // ─────────────────────────────────────────────────────────────────
  def("call/cc", { tag: "Native", name: "call/cc", arity: 1, fn: (args, s: State) => {
    const proc = args[0];
    if (!isCallable(proc)) {
      throw new Error("call/cc expects a procedure");
    }
    ensureArity(proc, 1, "call/cc");

    const cont: Val = {
      tag: "Continuation",
      kont: s.kont.slice(),
      env: s.env,
      store: s.store,
      handlers: s.handlers.slice(),
    } as Val;

    return applyProcedure(proc, [cont], s);
  }});

  def("call-with-prompt", { tag: "Native", name: "call-with-prompt", arity: 3, fn: (args, s: State) => {
    const [tagVal, thunk, handler] = args;
    if (!isCallable(thunk)) throw new Error("call-with-prompt: expected thunk");
    if (!isCallable(handler)) throw new Error("call-with-prompt: expected handler");
    ensureArity(thunk, 0, "call-with-prompt");
    ensureArity(handler, 2, "call-with-prompt");

    const promptFrame: Frame = {
      tag: "KPrompt",
      promptTag: tagVal,
      handler,
      env: s.env,
      savedKont: s.kont.slice(),
      savedHandlersDepth: s.handlers.length,
    };

    const stWithPrompt: State = { ...s, kont: s.kont.concat([promptFrame]) };
    return applyProcedure(thunk, [], stWithPrompt);
  }});

  def("abort-to-prompt", { tag: "Native", name: "abort-to-prompt", arity: 2, fn: (args, s: State) => {
    const [tagVal, value] = args;
    const targetKey = promptKey(tagVal);

    let found = -1;
    for (let i = s.kont.length - 1; i >= 0; i--) {
      const fr = s.kont[i] as Frame;
      if (fr.tag === "KPrompt" && promptKey((fr as any).promptTag) === targetKey) {
        found = i;
        break;
      }
    }

    if (found < 0) {
      throw new Error(`abort-to-prompt: no prompt for tag ${targetKey}`);
    }

    const pf = s.kont[found] as any;
    const handlerState: State = {
      ...s,
      env: pf.env,
      kont: pf.savedKont,
      handlers: s.handlers.slice(0, pf.savedHandlersDepth),
    };

    const kVal: Val = {
      tag: "Continuation",
      kont: s.kont.slice(found),
      env: s.env,
      store: s.store,
      handlers: s.handlers.slice(0, pf.savedHandlersDepth),
    } as Val;

    return applyProcedure(pf.handler, [kVal, value], handlerState);
  }});

  // ─────────────────────────────────────────────────────────────────
  // Metacircular primitives (cons, car, cdr, null?, pair?)
  // With these you can build list, append, map, filter, etc.
  // ─────────────────────────────────────────────────────────────────

  // cons: construct a pair
  def("cons", { tag: "Native", name: "cons", arity: 2, fn: (args, s) => {
    const pair: Val = { tag: "Vector", items: [args[0], args[1]] };
    return { ...s, control: { tag: "Val", v: pair } };
  }});

  // car: first element of pair
  def("car", { tag: "Native", name: "car", arity: 1, fn: (args, s) => {
    const v = args[0] as any;
    if (v.tag === "Vector" && v.items.length >= 1) {
      return { ...s, control: { tag: "Val", v: v.items[0] } };
    }
    throw new Error("car: expected pair");
  }});

  // cdr: rest of pair
  def("cdr", { tag: "Native", name: "cdr", arity: 1, fn: (args, s) => {
    const v = args[0] as any;
    if (v.tag === "Vector" && v.items.length >= 2) {
      return { ...s, control: { tag: "Val", v: v.items[1] } };
    }
    throw new Error("cdr: expected pair");
  }});

  // null?: check if value is null/empty
  def("null?", { tag: "Native", name: "null?", arity: 1, fn: (args, s) => {
    const v = args[0] as any;
    const isNull = v.tag === "Unit" || (v.tag === "Vector" && v.items.length === 0);
    return { ...s, control: { tag: "Val", v: isNull ? VTrue : VFalse } };
  }});

  // pair?: check if value is a pair
  def("pair?", { tag: "Native", name: "pair?", arity: 1, fn: (args, s) => {
    const v = args[0] as any;
    const isPair = v.tag === "Vector" && v.items.length >= 2;
    return { ...s, control: { tag: "Val", v: isPair ? VTrue : VFalse } };
  }});

  // list: construct a list from arguments (variadic)
  def("list", { tag: "Native", name: "list", arity: "variadic", fn: (args, s) => {
    // Build proper list from args: (list 1 2 3) => (cons 1 (cons 2 (cons 3 null)))
    let result: Val = VUnit; // null terminator
    for (let i = args.length - 1; i >= 0; i--) {
      result = { tag: "Vector", items: [args[i], result] };
    }
    return { ...s, control: { tag: "Val", v: result } };
  }});

  // append: concatenate lists (variadic)
  def("append", { tag: "Native", name: "append", arity: "variadic", fn: (args, s) => {
    // Flatten all lists into one
    const items: Val[] = [];
    for (const arg of args) {
      let current = arg as any;
      while (current.tag === "Vector" && current.items.length >= 2) {
        items.push(current.items[0]);
        current = current.items[1];
      }
      // If it's a non-null atom at the end, include it (improper list)
      if (current.tag !== "Unit") {
        items.push(current);
      }
    }
    // Rebuild as proper list
    let result: Val = VUnit;
    for (let i = items.length - 1; i >= 0; i--) {
      result = { tag: "Vector", items: [items[i], result] };
    }
    return { ...s, control: { tag: "Val", v: result } };
  }});

  // eq?: identity/equality comparison
  def("eq?", { tag: "Native", name: "eq?", arity: 2, fn: (args, s) => {
    const a = args[0] as any, b = args[1] as any;
    let eq = false;
    if (a.tag === b.tag) {
      if (a.tag === "Num") eq = a.n === b.n;
      else if (a.tag === "Bool") eq = a.b === b.b;
      else if (a.tag === "Str") eq = a.s === b.s;
      else if (a.tag === "Sym") eq = a.name === b.name;
      else if (a.tag === "Unit") eq = true;
      else eq = a === b; // reference equality
    }
    return { ...s, control: { tag: "Val", v: eq ? VTrue : VFalse } };
  }});

  // NOTE: For nested LLM calls, use the built-in Oracle Protocol:
  //   (infer.op payload)  - invoke LLM with payload, returns Meaning
  //   (int.op payload)    - same as infer.op
  // The LLM can then use eval/apply/observe tools to interact with Lisp.

  // ─────────────────────────────────────────────────────────────────
  // Distribution primitives (Dist<Val>)
  // For working with nondeterministic distributions from oracle ops
  // ─────────────────────────────────────────────────────────────────

  // dist: create a point distribution from a single value
  def("dist", { tag: "Native", name: "dist", arity: 1, fn: (args, s) => {
    const v = args[0];
    const d: Val = { tag: "Dist", support: [{ v, w: 1 }], normalized: true };
    return { ...s, control: { tag: "Val", v: d } };
  }});

  // dist?: check if value is a distribution
  def("dist?", { tag: "Native", name: "dist?", arity: 1, fn: (args, s) => {
    const v = args[0] as any;
    const isDist = v.tag === "Dist";
    return { ...s, control: { tag: "Val", v: isDist ? VTrue : VFalse } };
  }});

  // dist-count: number of support elements
  def("dist-count", { tag: "Native", name: "dist-count", arity: 1, fn: (args, s) => {
    const d = args[0] as any;
    if (d.tag !== "Dist") throw new Error("dist-count: expected Dist");
    const n = d.support.length;
    return { ...s, control: { tag: "Val", v: { tag: "Num", n } } };
  }});

  // dist-value-at: get the value at index i
  def("dist-value-at", { tag: "Native", name: "dist-value-at", arity: 2, fn: (args, s) => {
    const d = args[0] as any;
    const i = (args[1] as any).n;
    if (d.tag !== "Dist") throw new Error("dist-value-at: expected Dist");
    if (i < 0 || i >= d.support.length) throw new Error("dist-value-at: index out of bounds");
    return { ...s, control: { tag: "Val", v: d.support[i].v } };
  }});

  // dist-weight-at: get the weight at index i
  def("dist-weight-at", { tag: "Native", name: "dist-weight-at", arity: 2, fn: (args, s) => {
    const d = args[0] as any;
    const i = (args[1] as any).n;
    if (d.tag !== "Dist") throw new Error("dist-weight-at: expected Dist");
    if (i < 0 || i >= d.support.length) throw new Error("dist-weight-at: index out of bounds");
    return { ...s, control: { tag: "Val", v: { tag: "Num", n: d.support[i].w } } };
  }});

  // dist-normalize: normalize distribution weights to sum to 1
  def("dist-normalize", { tag: "Native", name: "dist-normalize", arity: 1, fn: (args, s) => {
    const d = args[0] as any;
    if (d.tag !== "Dist") throw new Error("dist-normalize: expected Dist");
    const sum = d.support.reduce((a: number, it: any) => a + it.w, 0);
    if (sum <= 0) {
      const empty: Val = { tag: "Dist", support: [], normalized: true, meta: d.meta };
      return { ...s, control: { tag: "Val", v: empty } };
    }
    const support = d.support.map((it: any) => ({ v: it.v, w: it.w / sum }));
    const norm: Val = { tag: "Dist", support, normalized: true, meta: d.meta };
    return { ...s, control: { tag: "Val", v: norm } };
  }});

  // dist-sample: sample a value from distribution with seed
  def("dist-sample", { tag: "Native", name: "dist-sample", arity: 2, fn: (args, s) => {
    const d0 = args[0] as any;
    const seed = (args[1] as any).n;
    if (d0.tag !== "Dist") throw new Error("dist-sample: expected Dist");

    // Normalize if needed
    let d = d0;
    if (!d.normalized) {
      const sum = d.support.reduce((a: number, it: any) => a + it.w, 0);
      if (sum > 0) {
        d = { ...d, support: d.support.map((it: any) => ({ v: it.v, w: it.w / sum })), normalized: true };
      }
    }

    if (d.support.length === 0) throw new Error("dist-sample: empty support");

    // Mulberry32 PRNG
    let t = seed >>> 0;
    t += 0x6D2B79F5;
    let x = t;
    x = Math.imul(x ^ (x >>> 15), x | 1);
    x ^= x + Math.imul(x ^ (x >>> 7), x | 61);
    const r = ((x ^ (x >>> 14)) >>> 0) / 4294967296;

    let acc = 0;
    for (const it of d.support) {
      acc += it.w;
      if (r <= acc) return { ...s, control: { tag: "Val", v: it.v } };
    }
    return { ...s, control: { tag: "Val", v: d.support[d.support.length - 1].v } };
  }});

  // dist-topk: get top k elements by weight
  def("dist-topk", { tag: "Native", name: "dist-topk", arity: 2, fn: (args, s) => {
    const d0 = args[0] as any;
    const k = (args[1] as any).n;
    if (d0.tag !== "Dist") throw new Error("dist-topk: expected Dist");

    const sorted = d0.support.slice().sort((a: any, b: any) => b.w - a.w);
    const support = sorted.slice(0, Math.max(0, k));
    const topk: Val = { tag: "Dist", support, normalized: d0.normalized, meta: d0.meta };
    return { ...s, control: { tag: "Val", v: topk } };
  }});

  // dist-from-list: create distribution from list of (value weight) pairs
  def("dist-from-list", { tag: "Native", name: "dist-from-list", arity: 1, fn: (args, s) => {
    const lst = args[0] as any;
    const support: Array<{ v: Val; w: number }> = [];

    // Walk the list (list of pairs where each pair is (value . weight))
    let cur = lst;
    while (cur.tag === "Vector" && cur.items.length >= 2) {
      const pair = cur.items[0] as any;
      if (pair.tag === "Vector" && pair.items.length >= 2) {
        const v = pair.items[0];
        const w = (pair.items[1] as any).n ?? 1;
        support.push({ v, w });
      }
      cur = cur.items[1];
    }

    const d: Val = { tag: "Dist", support, normalized: false };
    return { ...s, control: { tag: "Val", v: d } };
  }});

  // dist-to-list: convert distribution to list of (value weight) pairs
  def("dist-to-list", { tag: "Native", name: "dist-to-list", arity: 1, fn: (args, s) => {
    const d = args[0] as any;
    if (d.tag !== "Dist") throw new Error("dist-to-list: expected Dist");

    // Build list of pairs
    let result: Val = VUnit;
    for (let i = d.support.length - 1; i >= 0; i--) {
      const it = d.support[i];
      const pair: Val = { tag: "Vector", items: [it.v, { tag: "Num", n: it.w }] };
      result = { tag: "Vector", items: [pair, result] };
    }
    return { ...s, control: { tag: "Val", v: result } };
  }});

  // ─────────────────────────────────────────────────────────────────
  // String primitives (semantic text toolbox)
  // These allow mechanical/extensional string processing
  // ─────────────────────────────────────────────────────────────────

  // string=?: string equality
  def("string=?", { tag: "Native", name: "string=?", arity: 2, fn: (args, s) => {
    const a = args[0] as any, b = args[1] as any;
    if (a.tag !== "Str" || b.tag !== "Str") throw new Error("string=?: expected strings");
    return { ...s, control: { tag: "Val", v: a.s === b.s ? VTrue : VFalse } };
  }});

  // string-contains?: check if string contains substring
  def("string-contains?", { tag: "Native", name: "string-contains?", arity: 2, fn: (args, s) => {
    const str = args[0] as any, sub = args[1] as any;
    if (str.tag !== "Str" || sub.tag !== "Str") throw new Error("string-contains?: expected strings");
    return { ...s, control: { tag: "Val", v: str.s.includes(sub.s) ? VTrue : VFalse } };
  }});

  // string-replace-all: replace all occurrences of substr with replacement
  def("string-replace-all", { tag: "Native", name: "string-replace-all", arity: 3, fn: (args, s) => {
    const str = args[0] as any, sub = args[1] as any, repl = args[2] as any;
    if (str.tag !== "Str" || sub.tag !== "Str" || repl.tag !== "Str") {
      throw new Error("string-replace-all: expected strings");
    }
    // Use split/join for global replace without regex
    const result = str.s.split(sub.s).join(repl.s);
    return { ...s, control: { tag: "Val", v: { tag: "Str", s: result } } };
  }});

  // string-split: split string by delimiter
  def("string-split", { tag: "Native", name: "string-split", arity: 2, fn: (args, s) => {
    const str = args[0] as any, delim = args[1] as any;
    if (str.tag !== "Str" || delim.tag !== "Str") throw new Error("string-split: expected strings");
    const parts = str.s.split(delim.s);
    // Build proper list
    let result: Val = VUnit;
    for (let i = parts.length - 1; i >= 0; i--) {
      result = { tag: "Vector", items: [{ tag: "Str", s: parts[i] }, result] };
    }
    return { ...s, control: { tag: "Val", v: result } };
  }});

  // string-join: join list of strings with delimiter
  def("string-join", { tag: "Native", name: "string-join", arity: 2, fn: (args, s) => {
    const lst = args[0] as any, delim = args[1] as any;
    if (delim.tag !== "Str") throw new Error("string-join: delimiter must be string");
    // Walk the list
    const parts: string[] = [];
    let cur = lst;
    while (cur.tag === "Vector" && cur.items.length >= 2) {
      const elem = cur.items[0] as any;
      if (elem.tag === "Str") parts.push(elem.s);
      cur = cur.items[1];
    }
    return { ...s, control: { tag: "Val", v: { tag: "Str", s: parts.join(delim.s) } } };
  }});

  // string-trim: remove leading/trailing whitespace
  def("string-trim", { tag: "Native", name: "string-trim", arity: 1, fn: (args, s) => {
    const str = args[0] as any;
    if (str.tag !== "Str") throw new Error("string-trim: expected string");
    return { ...s, control: { tag: "Val", v: { tag: "Str", s: str.s.trim() } } };
  }});

  // string-downcase: convert to lowercase
  def("string-downcase", { tag: "Native", name: "string-downcase", arity: 1, fn: (args, s) => {
    const str = args[0] as any;
    if (str.tag !== "Str") throw new Error("string-downcase: expected string");
    return { ...s, control: { tag: "Val", v: { tag: "Str", s: str.s.toLowerCase() } } };
  }});

  // string-upcase: convert to uppercase
  def("string-upcase", { tag: "Native", name: "string-upcase", arity: 1, fn: (args, s) => {
    const str = args[0] as any;
    if (str.tag !== "Str") throw new Error("string-upcase: expected string");
    return { ...s, control: { tag: "Val", v: { tag: "Str", s: str.s.toUpperCase() } } };
  }});

  // string-length: get length of string
  def("string-length", { tag: "Native", name: "string-length", arity: 1, fn: (args, s) => {
    const str = args[0] as any;
    if (str.tag !== "Str") throw new Error("string-length: expected string");
    return { ...s, control: { tag: "Val", v: { tag: "Num", n: str.s.length } } };
  }});

  // string-append: concatenate strings
  def("string-append", { tag: "Native", name: "string-append", arity: "variadic", fn: (args, s) => {
    let result = "";
    for (const arg of args) {
      if ((arg as any).tag !== "Str") throw new Error("string-append: expected strings");
      result += (arg as any).s;
    }
    return { ...s, control: { tag: "Val", v: { tag: "Str", s: result } } };
  }});

  // substring: extract substring
  def("substring", { tag: "Native", name: "substring", arity: 3, fn: (args, s) => {
    const str = args[0] as any, start = args[1] as any, end = args[2] as any;
    if (str.tag !== "Str") throw new Error("substring: expected string");
    if (start.tag !== "Num" || end.tag !== "Num") throw new Error("substring: expected numbers for indices");
    return { ...s, control: { tag: "Val", v: { tag: "Str", s: str.s.substring(start.n, end.n) } } };
  }});

  // ─────────────────────────────────────────────────────────────────
  // Symbol and type predicates
  // ─────────────────────────────────────────────────────────────────

  // symbol?: check if value is a symbol
  def("symbol?", { tag: "Native", name: "symbol?", arity: 1, fn: (args, s) => {
    return { ...s, control: { tag: "Val", v: args[0].tag === "Sym" ? VTrue : VFalse } };
  }});

  // string?: check if value is a string
  def("string?", { tag: "Native", name: "string?", arity: 1, fn: (args, s) => {
    return { ...s, control: { tag: "Val", v: args[0].tag === "Str" ? VTrue : VFalse } };
  }});

  // number?: check if value is a number
  def("number?", { tag: "Native", name: "number?", arity: 1, fn: (args, s) => {
    return { ...s, control: { tag: "Val", v: args[0].tag === "Num" ? VTrue : VFalse } };
  }});

  // boolean?: check if value is a boolean
  def("boolean?", { tag: "Native", name: "boolean?", arity: 1, fn: (args, s) => {
    return { ...s, control: { tag: "Val", v: args[0].tag === "Bool" ? VTrue : VFalse } };
  }});

  // procedure?: check if value is callable
  def("procedure?", { tag: "Native", name: "procedure?", arity: 1, fn: (args, s) => {
    const v = args[0];
    const isProc = v.tag === "Closure" || v.tag === "Native" || v.tag === "OracleProc";
    return { ...s, control: { tag: "Val", v: isProc ? VTrue : VFalse } };
  }});

  // ─────────────────────────────────────────────────────────────────
  // List utilities (additional helpers for SICP-style programming)
  // ─────────────────────────────────────────────────────────────────

  // length: get length of list
  def("length", { tag: "Native", name: "length", arity: 1, fn: (args, s) => {
    let count = 0;
    let cur = args[0] as any;
    while (cur.tag === "Vector" && cur.items.length >= 2) {
      count++;
      cur = cur.items[1];
    }
    return { ...s, control: { tag: "Val", v: { tag: "Num", n: count } } };
  }});

  // list-ref: get nth element
  def("list-ref", { tag: "Native", name: "list-ref", arity: 2, fn: (args, s) => {
    const lst = args[0] as any;
    const n = (args[1] as any).n;
    let cur = lst;
    for (let i = 0; i < n; i++) {
      if (cur.tag !== "Vector" || cur.items.length < 2) throw new Error("list-ref: index out of bounds");
      cur = cur.items[1];
    }
    if (cur.tag !== "Vector" || cur.items.length < 1) throw new Error("list-ref: index out of bounds");
    return { ...s, control: { tag: "Val", v: cur.items[0] } };
  }});

  // reverse: reverse a list
  def("reverse", { tag: "Native", name: "reverse", arity: 1, fn: (args, s) => {
    const items: Val[] = [];
    let cur = args[0] as any;
    while (cur.tag === "Vector" && cur.items.length >= 2) {
      items.push(cur.items[0]);
      cur = cur.items[1];
    }
    // Build reversed list
    let result: Val = VUnit;
    for (const item of items) {
      result = { tag: "Vector", items: [item, result] };
    }
    return { ...s, control: { tag: "Val", v: result } };
  }});

  // cadr, caddr, etc. (convenience accessors)
  def("cadr", { tag: "Native", name: "cadr", arity: 1, fn: (args, s) => {
    const v = args[0] as any;
    if (v.tag !== "Vector" || v.items.length < 2) throw new Error("cadr: expected pair");
    const cdr = v.items[1] as any;
    if (cdr.tag !== "Vector" || cdr.items.length < 1) throw new Error("cadr: expected pair in cdr");
    return { ...s, control: { tag: "Val", v: cdr.items[0] } };
  }});

  def("caddr", { tag: "Native", name: "caddr", arity: 1, fn: (args, s) => {
    const v = args[0] as any;
    if (v.tag !== "Vector" || v.items.length < 2) throw new Error("caddr: expected pair");
    const cdr = v.items[1] as any;
    if (cdr.tag !== "Vector" || cdr.items.length < 2) throw new Error("caddr: expected pair in cdr");
    const cddr = cdr.items[1] as any;
    if (cddr.tag !== "Vector" || cddr.items.length < 1) throw new Error("caddr: expected pair in cddr");
    return { ...s, control: { tag: "Val", v: cddr.items[0] } };
  }});

  // equal?: deep structural equality
  def("equal?", { tag: "Native", name: "equal?", arity: 2, fn: (args, s) => {
    function deepEq(a: Val, b: Val): boolean {
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
          for (let i = 0; i < a.items.length; i++) {
            if (!deepEq(a.items[i], bb.items[i])) return false;
          }
          return true;
        }
        default:
          return JSON.stringify(a) === JSON.stringify(b);
      }
    }
    return { ...s, control: { tag: "Val", v: deepEq(args[0], args[1]) ? VTrue : VFalse } };
  }});

  // ─────────────────────────────────────────────────────────────────
  // Higher-Order Function combinators (HOF)
  // These accept Native | Closure | OracleProc as arguments
  // ─────────────────────────────────────────────────────────────────

  // Helper to apply a procedure (Native only for now, Closure/OracleProc need machine stepping)
  function applyNative(proc: Val, procArgs: Val[], state: any): Val | null {
    if (proc.tag === "Native") {
      const result = (proc as any).fn(procArgs, state);
      if (result.control?.tag === "Val") {
        return result.control.v;
      }
    }
    return null;
  }

  // map: apply f to each element of list
  def("map", { tag: "Native", name: "map", arity: 2, fn: (args, s) => {
    const f = args[0];
    const lst = args[1] as any;

    // Collect list elements
    const items: Val[] = [];
    let cur = lst;
    while (cur.tag === "Vector" && cur.items.length >= 2) {
      items.push(cur.items[0]);
      cur = cur.items[1];
    }

    // Apply f to each element
    const results: Val[] = [];
    for (const item of items) {
      const result = applyNative(f, [item], s);
      if (result === null) {
        // For Closure/OracleProc, we'd need proper machine stepping
        throw new Error("map: only Native functions supported in primitive; use recursive map for Closure/OracleProc");
      }
      results.push(result);
    }

    // Build result list
    let resultList: Val = VUnit;
    for (let i = results.length - 1; i >= 0; i--) {
      resultList = { tag: "Vector", items: [results[i], resultList] };
    }
    return { ...s, control: { tag: "Val", v: resultList } };
  }});

  // filter: keep elements where predicate returns true
  def("filter", { tag: "Native", name: "filter", arity: 2, fn: (args, s) => {
    const pred = args[0];
    const lst = args[1] as any;

    // Collect list elements
    const items: Val[] = [];
    let cur = lst;
    while (cur.tag === "Vector" && cur.items.length >= 2) {
      items.push(cur.items[0]);
      cur = cur.items[1];
    }

    // Filter elements
    const results: Val[] = [];
    for (const item of items) {
      const result = applyNative(pred, [item], s);
      if (result === null) {
        throw new Error("filter: only Native functions supported in primitive; use recursive filter for Closure/OracleProc");
      }
      // Check if truthy (Bool true, or non-false value)
      if (result.tag === "Bool" && result.b) {
        results.push(item);
      } else if (result.tag !== "Bool" && result.tag !== "Unit") {
        // Non-boolean, non-unit values are truthy
        results.push(item);
      }
    }

    // Build result list
    let resultList: Val = VUnit;
    for (let i = results.length - 1; i >= 0; i--) {
      resultList = { tag: "Vector", items: [results[i], resultList] };
    }
    return { ...s, control: { tag: "Val", v: resultList } };
  }});

  // fold (foldl): left fold - (fold f init xs) applies (f acc x) for each x
  def("fold", { tag: "Native", name: "fold", arity: 3, fn: (args, s) => {
    const f = args[0];
    let acc = args[1];
    const lst = args[2] as any;

    // Walk the list
    let cur = lst;
    while (cur.tag === "Vector" && cur.items.length >= 2) {
      const item = cur.items[0];
      const result = applyNative(f, [acc, item], s);
      if (result === null) {
        throw new Error("fold: only Native functions supported in primitive; use recursive fold for Closure/OracleProc");
      }
      acc = result;
      cur = cur.items[1];
    }

    return { ...s, control: { tag: "Val", v: acc } };
  }});

  // foldr: right fold - (foldr f init xs) applies (f x acc) from right to left
  def("foldr", { tag: "Native", name: "foldr", arity: 3, fn: (args, s) => {
    const f = args[0];
    const init = args[1];
    const lst = args[2] as any;

    // Collect elements
    const items: Val[] = [];
    let cur = lst;
    while (cur.tag === "Vector" && cur.items.length >= 2) {
      items.push(cur.items[0]);
      cur = cur.items[1];
    }

    // Fold from right
    let acc = init;
    for (let i = items.length - 1; i >= 0; i--) {
      const result = applyNative(f, [items[i], acc], s);
      if (result === null) {
        throw new Error("foldr: only Native functions supported in primitive; use recursive foldr for Closure/OracleProc");
      }
      acc = result;
    }

    return { ...s, control: { tag: "Val", v: acc } };
  }});

  // compose: (compose f g) => (lambda (x) (f (g x)))
  // Returns a new Native that composes f and g
  def("compose", { tag: "Native", name: "compose", arity: 2, fn: (args, s) => {
    const f = args[0];
    const g = args[1];

    const composed: Val = {
      tag: "Native",
      name: "composed",
      arity: 1,
      fn: (innerArgs: Val[], innerS: any) => {
        // Apply g first
        const gResult = applyNative(g, innerArgs, innerS);
        if (gResult === null) {
          throw new Error("compose: inner function (g) must be Native");
        }
        // Then apply f
        const fResult = applyNative(f, [gResult], innerS);
        if (fResult === null) {
          throw new Error("compose: outer function (f) must be Native");
        }
        return { ...innerS, control: { tag: "Val", v: fResult } };
      }
    };

    return { ...s, control: { tag: "Val", v: composed } };
  }});

  // pipe: (pipe f g) => (lambda (x) (g (f x)))
  // Opposite of compose - f first, then g
  def("pipe", { tag: "Native", name: "pipe", arity: 2, fn: (args, s) => {
    const f = args[0];
    const g = args[1];

    const piped: Val = {
      tag: "Native",
      name: "piped",
      arity: 1,
      fn: (innerArgs: Val[], innerS: any) => {
        // Apply f first
        const fResult = applyNative(f, innerArgs, innerS);
        if (fResult === null) {
          throw new Error("pipe: first function (f) must be Native");
        }
        // Then apply g
        const gResult = applyNative(g, [fResult], innerS);
        if (gResult === null) {
          throw new Error("pipe: second function (g) must be Native");
        }
        return { ...innerS, control: { tag: "Val", v: gResult } };
      }
    };

    return { ...s, control: { tag: "Val", v: piped } };
  }});

  // partial: (partial f a) => (lambda (b ...) (f a b ...))
  // Partially apply first argument
  def("partial", { tag: "Native", name: "partial", arity: 2, fn: (args, s) => {
    const f = args[0];
    const bound = args[1];

    const partialFn: Val = {
      tag: "Native",
      name: "partial",
      arity: "variadic",
      fn: (innerArgs: Val[], innerS: any) => {
        // Prepend bound arg
        const allArgs = [bound, ...innerArgs];
        const result = applyNative(f, allArgs, innerS);
        if (result === null) {
          throw new Error("partial: function must be Native");
        }
        return { ...innerS, control: { tag: "Val", v: result } };
      }
    };

    return { ...s, control: { tag: "Val", v: partialFn } };
  }});

  // apply: (apply f args-list) - call procedure with argument list
  // This is essential for metacircular evaluators
  def("apply", { tag: "Native", name: "apply", arity: 2, fn: (args, s) => {
    const f = args[0];
    const argList = args[1] as any;

    // Convert list to array
    const procArgs: Val[] = [];
    let cur = argList;
    while (cur.tag === "Vector" && cur.items.length >= 2) {
      procArgs.push(cur.items[0]);
      cur = cur.items[1];
    }

    // Apply based on procedure type
    if (f.tag === "Native") {
      const result = applyNative(f, procArgs, s);
      if (result === null) {
        throw new Error("apply: Native function did not return a value");
      }
      return { ...s, control: { tag: "Val", v: result } };
    }

    if (f.tag === "Closure") {
      // For Closure, we need to set up the machine state for application
      const closure = f as any;
      // Create a new environment with params bound to args
      const frame = new Map<string, number>();
      let currentStore = s.store;
      for (let i = 0; i < closure.params.length && i < procArgs.length; i++) {
        // alloc returns [Store, Addr] tuple - pass value to allocate
        const [newStore, addr] = currentStore.alloc(procArgs[i]);
        currentStore = newStore;
        frame.set(closure.params[i], addr);
      }
      const envWithArgs = { tag: "Ctx", frame, parent: closure.env };
      // Evaluate the body in the new environment
      return { ...s, store: currentStore, control: { tag: "Expr", e: closure.body }, env: envWithArgs };
    }

    if (f.tag === "OracleProc") {
      // OracleProc application would need effect handling - not directly available here
      throw new Error("apply: OracleProc requires effect handling; use host evaluator");
    }

    throw new Error(`apply: expected procedure, got ${f.tag}`);
  }});

  // identity: (identity x) => x
  def("identity", { tag: "Native", name: "identity", arity: 1, fn: (args, s) => {
    return { ...s, control: { tag: "Val", v: args[0] } };
  }});

  // constantly: (constantly x) => (lambda (y) x)
  def("constantly", { tag: "Native", name: "constantly", arity: 1, fn: (args, s) => {
    const x = args[0];
    const constFn: Val = {
      tag: "Native",
      name: "const",
      arity: 1,
      fn: (_innerArgs: Val[], innerS: any) => {
        return { ...innerS, control: { tag: "Val", v: x } };
      }
    };
    return { ...s, control: { tag: "Val", v: constFn } };
  }});

  // andmap: like all - returns true if predicate is true for all elements
  def("andmap", { tag: "Native", name: "andmap", arity: 2, fn: (args, s) => {
    const pred = args[0];
    const lst = args[1] as any;

    let cur = lst;
    while (cur.tag === "Vector" && cur.items.length >= 2) {
      const item = cur.items[0];
      const result = applyNative(pred, [item], s);
      if (result === null) {
        throw new Error("andmap: only Native functions supported");
      }
      if (result.tag === "Bool" && !result.b) {
        return { ...s, control: { tag: "Val", v: VFalse } };
      }
      cur = cur.items[1];
    }
    return { ...s, control: { tag: "Val", v: VTrue } };
  }});

  // ormap: like any - returns true if predicate is true for any element
  def("ormap", { tag: "Native", name: "ormap", arity: 2, fn: (args, s) => {
    const pred = args[0];
    const lst = args[1] as any;

    let cur = lst;
    while (cur.tag === "Vector" && cur.items.length >= 2) {
      const item = cur.items[0];
      const result = applyNative(pred, [item], s);
      if (result === null) {
        throw new Error("ormap: only Native functions supported");
      }
      if (result.tag === "Bool" && result.b) {
        return { ...s, control: { tag: "Val", v: VTrue } };
      }
      cur = cur.items[1];
    }
    return { ...s, control: { tag: "Val", v: VFalse } };
  }});

  // ─────────────────────────────────────────────────────────────────
  // Promises (delay/force with memoization)
  // For lazy evaluation and streams
  // ─────────────────────────────────────────────────────────────────

  // make-promise: create a memoizing thunk from a procedure
  // The Promise value stores: {tag: "Promise", thunk, forced: bool, value?: Val}
  def("make-promise", { tag: "Native", name: "make-promise", arity: 1, fn: (args, s) => {
    const thunk = args[0];
    if (thunk.tag !== "Closure" && thunk.tag !== "Native") {
      throw new Error("make-promise: expected procedure");
    }
    const promise: Val = {
      tag: "Promise",
      thunk,
      forced: false,
      value: undefined,
    } as any;
    return { ...s, control: { tag: "Val", v: promise } };
  }});

  // force: evaluate a promise (memoized)
  // For Closure thunks, we return a state that evaluates the closure body.
  // The result will flow to the continuation; memoization happens via KMemoize frame.
  def("force", { tag: "Native", name: "force", arity: 1, fn: (args, s) => {
    const p = args[0] as any;
    if (p.tag !== "Promise") {
      // If not a promise, return as-is (idempotent)
      return { ...s, control: { tag: "Val", v: args[0] } };
    }
    if (p.forced) {
      // Already forced, return cached value
      return { ...s, control: { tag: "Val", v: p.value } };
    }
    // Need to force: call the thunk
    const thunk = p.thunk;
    if (thunk.tag === "Native") {
      const result = (thunk as any).fn([], s);
      if (result.control?.tag === "Val") {
        // Memoize the result
        p.forced = true;
        p.value = result.control.v;
        return result;
      }
    }
    // For Closure thunks, inline the body evaluation
    if (thunk.tag === "Closure") {
      // Add a KMemoize frame to cache the result when body evaluates
      // For now, use a simpler approach: wrap in a continuation that caches
      const memoizeFrame: Frame = {
        tag: "KCall",
        savedEnv: s.env,
        // We hack in memoization by also storing the promise reference
        // Actually, we can use a simpler trick: evaluate body, and the
        // Promise is mutated in-place after first force completes.
        // For simplicity, just evaluate without memoization for now.
      };

      // Return state that evaluates closure body with closure env
      return {
        ...s,
        control: { tag: "Expr", e: thunk.body },
        env: thunk.env,
        kont: [...s.kont, memoizeFrame],
      };
    }
    throw new Error(`force: unsupported thunk type ${thunk.tag}`);
  }});

  // promise?: check if value is a promise
  def("promise?", { tag: "Native", name: "promise?", arity: 1, fn: (args, s) => {
    const v = args[0] as any;
    return { ...s, control: { tag: "Val", v: v.tag === "Promise" ? VTrue : VFalse } };
  }});

  // promise-forced?: check if promise has been forced
  def("promise-forced?", { tag: "Native", name: "promise-forced?", arity: 1, fn: (args, s) => {
    const p = args[0] as any;
    if (p.tag !== "Promise") return { ...s, control: { tag: "Val", v: VFalse } };
    return { ...s, control: { tag: "Val", v: p.forced ? VTrue : VFalse } };
  }});

  // ─────────────────────────────────────────────────────────────────
  // Stream primitives
  // Streams are pairs where cdr is a promise
  // ─────────────────────────────────────────────────────────────────

  // the-empty-stream: sentinel for empty stream
  def("the-empty-stream", VUnit);

  // stream-null?: check if stream is empty
  def("stream-null?", { tag: "Native", name: "stream-null?", arity: 1, fn: (args, s) => {
    const v = args[0] as any;
    const isEmpty = v.tag === "Unit" || (v.tag === "Vector" && v.items.length === 0);
    return { ...s, control: { tag: "Val", v: isEmpty ? VTrue : VFalse } };
  }});

  // stream-car: get first element (same as car)
  def("stream-car", { tag: "Native", name: "stream-car", arity: 1, fn: (args, s) => {
    const v = args[0] as any;
    if (v.tag === "Vector" && v.items.length >= 1) {
      return { ...s, control: { tag: "Val", v: v.items[0] } };
    }
    throw new Error("stream-car: expected stream pair");
  }});

  // stream-cdr: force the tail and return it
  def("stream-cdr", { tag: "Native", name: "stream-cdr", arity: 1, fn: (args, s) => {
    const v = args[0] as any;
    if (v.tag !== "Vector" || v.items.length < 2) {
      throw new Error("stream-cdr: expected stream pair");
    }
    const tail = v.items[1] as any;
    // Force the promise
    if (tail.tag === "Promise") {
      if (tail.forced) {
        return { ...s, control: { tag: "Val", v: tail.value } };
      }
      const thunk = tail.thunk;
      // Force native thunk
      if (thunk.tag === "Native") {
        const result = (thunk as any).fn([], s);
        if (result.control?.tag === "Val") {
          tail.forced = true;
          tail.value = result.control.v;
          return { ...s, control: { tag: "Val", v: tail.value } };
        }
      }
      // For Closure thunks, inline the body evaluation
      if (thunk.tag === "Closure") {
        const memoizeFrame: Frame = { tag: "KCall", savedEnv: s.env };
        return {
          ...s,
          control: { tag: "Expr", e: thunk.body },
          env: thunk.env,
          kont: [...s.kont, memoizeFrame],
        };
      }
      throw new Error(`stream-cdr: unsupported thunk type ${thunk.tag}`);
    }
    // If not a promise, return as-is
    return { ...s, control: { tag: "Val", v: tail } };
  }});

  // list->stream: convert list to stream
  def("list->stream", { tag: "Native", name: "list->stream", arity: 1, fn: (args, s) => {
    // Already a list (cons cells), just need to wrap tails as promises
    // For simplicity, return the list directly - streams and lists share structure
    // The difference is conceptual: stream-cdr forces, cdr doesn't
    return { ...s, control: { tag: "Val", v: args[0] } };
  }});

  // stream->list: convert stream to list (eager, forces all)
  // (stream->list stream n) - take n elements from stream as a list
  def("stream->list", { tag: "Native", name: "stream->list", arity: 2, fn: (args, s) => {
    let stream = args[0] as any;
    const n = (args[1] as any).n;
    const items: Val[] = [];

    for (let i = 0; i < n; i++) {
      // Check if empty
      if (stream.tag === "Unit" || (stream.tag === "Vector" && stream.items.length === 0)) {
        break;
      }
      if (stream.tag !== "Vector" || stream.items.length < 2) {
        break;
      }
      // Get head
      items.push(stream.items[0]);
      // Force and get tail
      const tail = stream.items[1] as any;
      if (tail.tag === "Promise") {
        if (tail.forced) {
          stream = tail.value;
        } else if (tail.thunk.tag === "Native") {
          const result = (tail.thunk as any).fn([], s);
          if (result.control?.tag === "Val") {
            tail.forced = true;
            tail.value = result.control.v;
            stream = tail.value;
          } else {
            break;
          }
        } else {
          throw new Error("stream->list: Closure thunks require machine-level handling");
        }
      } else {
        stream = tail;
      }
    }

    // Build result list
    let result: Val = VUnit;
    for (let i = items.length - 1; i >= 0; i--) {
      result = { tag: "Vector", items: [items[i], result] };
    }
    return { ...s, control: { tag: "Val", v: result } };
  }});

  // stream-take: take n elements from stream, returns a new stream
  // (stream-take stream n) - returns stream of first n elements
  def("stream-take", { tag: "Native", name: "stream-take", arity: 2, fn: (args, s) => {
    const stream = args[0];
    const n = (args[1] as any).n;
    // Reuse stream->list logic
    const items: Val[] = [];
    let cur = stream as any;

    for (let i = 0; i < n; i++) {
      if (cur.tag === "Unit") break;
      if (cur.tag !== "Vector" || cur.items.length < 2) break;
      items.push(cur.items[0]);
      const tail = cur.items[1] as any;
      if (tail.tag === "Promise") {
        if (tail.forced) {
          cur = tail.value;
        } else if (tail.thunk.tag === "Native") {
          const result = (tail.thunk as any).fn([], s);
          if (result.control?.tag === "Val") {
            tail.forced = true;
            tail.value = result.control.v;
            cur = tail.value;
          } else break;
        } else {
          throw new Error("stream-take: Closure thunks require machine-level handling");
        }
      } else {
        cur = tail;
      }
    }

    let result: Val = VUnit;
    for (let i = items.length - 1; i >= 0; i--) {
      result = { tag: "Vector", items: [items[i], result] };
    }
    return { ...s, control: { tag: "Val", v: result } };
  }});

  // stream-map: apply f to each element of stream (lazy)
  def("stream-map", { tag: "Native", name: "stream-map", arity: 2, fn: (args, s) => {
    const f = args[0];
    const stream = args[1] as any;

    if (stream.tag === "Unit") {
      return { ...s, control: { tag: "Val", v: VUnit } };
    }
    if (stream.tag !== "Vector" || stream.items.length < 2) {
      return { ...s, control: { tag: "Val", v: VUnit } };
    }

    // Apply f to head
    const head = stream.items[0];
    const mappedHead = applyNative(f, [head], s);
    if (mappedHead === null) {
      throw new Error("stream-map: only Native functions supported in primitive");
    }

    // Create lazy tail
    const tailThunk: Val = {
      tag: "Native",
      name: "stream-map-thunk",
      arity: 0,
      fn: (_: Val[], innerS: any) => {
        // Force original tail
        const origTail = stream.items[1] as any;
        let forcedTail: Val;
        if (origTail.tag === "Promise") {
          if (origTail.forced) {
            forcedTail = origTail.value;
          } else if (origTail.thunk.tag === "Native") {
            const r = (origTail.thunk as any).fn([], innerS);
            if (r.control?.tag === "Val") {
              origTail.forced = true;
              origTail.value = r.control.v;
              forcedTail = origTail.value;
            } else {
              return r;
            }
          } else {
            throw new Error("stream-map: Closure thunks in stream tail");
          }
        } else {
          forcedTail = origTail;
        }
        // Recursively stream-map on tail
        if (forcedTail.tag === "Unit") {
          return { ...innerS, control: { tag: "Val", v: VUnit } };
        }
        // Return mapped tail
        const mapFn = env; // closure over f
        // This is tricky - we need the stream-map function
        // For simplicity, inline the logic
        return streamMapRec(f, forcedTail, innerS);
      }
    } as any;

    function streamMapRec(fn: Val, str: Val, st: any): any {
      if ((str as any).tag === "Unit") {
        return { ...st, control: { tag: "Val", v: VUnit } };
      }
      const strAny = str as any;
      if (strAny.tag !== "Vector" || strAny.items.length < 2) {
        return { ...st, control: { tag: "Val", v: VUnit } };
      }
      const h = applyNative(fn, [strAny.items[0]], st);
      if (h === null) throw new Error("stream-map: Native required");
      const thunk: Val = {
        tag: "Native",
        name: "smap-thunk",
        arity: 0,
        fn: (_: Val[], is: any) => {
          const t = strAny.items[1] as any;
          let ft: Val;
          if (t.tag === "Promise") {
            if (t.forced) ft = t.value;
            else if (t.thunk.tag === "Native") {
              const rr = (t.thunk as any).fn([], is);
              if (rr.control?.tag === "Val") {
                t.forced = true;
                t.value = rr.control.v;
                ft = t.value;
              } else return rr;
            } else throw new Error("stream-map thunk");
          } else ft = t;
          return streamMapRec(fn, ft, is);
        }
      } as any;
      const promise: Val = { tag: "Promise", thunk, forced: false, value: undefined } as any;
      const result: Val = { tag: "Vector", items: [h, promise] };
      return { ...st, control: { tag: "Val", v: result } };
    }

    const promise: Val = { tag: "Promise", thunk: tailThunk, forced: false, value: undefined } as any;
    const result: Val = { tag: "Vector", items: [mappedHead, promise] };
    return { ...s, control: { tag: "Val", v: result } };
  }});

  // stream-filter: filter stream by predicate (lazy)
  def("stream-filter", { tag: "Native", name: "stream-filter", arity: 2, fn: (args, s) => {
    const pred = args[0];
    const stream = args[1];

    function streamFilterRec(p: Val, str: Val, st: any): any {
      if ((str as any).tag === "Unit") {
        return { ...st, control: { tag: "Val", v: VUnit } };
      }
      const strAny = str as any;
      if (strAny.tag !== "Vector" || strAny.items.length < 2) {
        return { ...st, control: { tag: "Val", v: VUnit } };
      }

      const head = strAny.items[0];
      const testResult = applyNative(p, [head], st);
      if (testResult === null) {
        throw new Error("stream-filter: only Native functions supported");
      }

      // Force tail
      const tail = strAny.items[1] as any;
      let forcedTail: Val;
      if (tail.tag === "Promise") {
        if (tail.forced) {
          forcedTail = tail.value;
        } else if (tail.thunk.tag === "Native") {
          const r = (tail.thunk as any).fn([], st);
          if (r.control?.tag === "Val") {
            tail.forced = true;
            tail.value = r.control.v;
            forcedTail = tail.value;
          } else {
            throw new Error("stream-filter: thunk error");
          }
        } else {
          throw new Error("stream-filter: Closure thunks in stream tail");
        }
      } else {
        forcedTail = tail;
      }

      const keep = testResult.tag === "Bool" && testResult.b;
      if (keep) {
        // Include this element, create lazy filtered tail
        const thunk: Val = {
          tag: "Native",
          name: "filter-thunk",
          arity: 0,
          fn: (_: Val[], is: any) => streamFilterRec(p, forcedTail, is)
        } as any;
        const promise: Val = { tag: "Promise", thunk, forced: false, value: undefined } as any;
        const result: Val = { tag: "Vector", items: [head, promise] };
        return { ...st, control: { tag: "Val", v: result } };
      } else {
        // Skip this element, recurse on tail
        return streamFilterRec(p, forcedTail, st);
      }
    }

    return streamFilterRec(pred, stream, s);
  }});

  // ─────────────────────────────────────────────────────────────────
  // B1: Op-Table Primitives (SICP-style data-directed programming)
  // Operation tables map (op-name, type-tags...) -> procedure
  // ─────────────────────────────────────────────────────────────────

  // make-op-table: create a new operation table (returns a mutable Map-based value)
  def("make-op-table", { tag: "Native", name: "make-op-table", arity: 0, fn: (_args, s) => {
    const table: Val = {
      tag: "OpTable",
      entries: new Map<string, Val>()
    } as any;
    return { ...s, control: { tag: "Val", v: table } };
  }});

  // op-table-put: install a method - (op-table-put table op-name type-tags proc)
  // type-tags is a list of symbols, proc is a procedure
  def("op-table-put", { tag: "Native", name: "op-table-put", arity: 4, fn: (args, s) => {
    const table = args[0] as any;
    const opName = args[1] as any;
    const typeTags = args[2] as any;
    const proc = args[3];

    if (table.tag !== "OpTable") throw new Error("op-table-put: expected OpTable");
    if (opName.tag !== "Sym" && opName.tag !== "Str") throw new Error("op-table-put: op-name must be symbol or string");

    // Convert type-tags list to key string
    const tags: string[] = [];
    let cur = typeTags;
    while (cur.tag === "Vector" && cur.items.length >= 2) {
      const tag = cur.items[0] as any;
      tags.push(tag.tag === "Sym" ? tag.name : tag.s ?? String(tag));
      cur = cur.items[1];
    }
    // Handle single symbol
    if (typeTags.tag === "Sym") {
      tags.push(typeTags.name);
    }

    const key = `${opName.tag === "Sym" ? opName.name : opName.s}:${tags.join(",")}`;
    table.entries.set(key, proc);

    return { ...s, control: { tag: "Val", v: VUnit } };
  }});

  // op-table-get: lookup a method - (op-table-get table op-name type-tags)
  def("op-table-get", { tag: "Native", name: "op-table-get", arity: 3, fn: (args, s) => {
    const table = args[0] as any;
    const opName = args[1] as any;
    const typeTags = args[2] as any;

    if (table.tag !== "OpTable") throw new Error("op-table-get: expected OpTable");
    if (opName.tag !== "Sym" && opName.tag !== "Str") throw new Error("op-table-get: op-name must be symbol or string");

    // Convert type-tags list to key string
    const tags: string[] = [];
    let cur = typeTags;
    while (cur.tag === "Vector" && cur.items.length >= 2) {
      const tag = cur.items[0] as any;
      tags.push(tag.tag === "Sym" ? tag.name : tag.s ?? String(tag));
      cur = cur.items[1];
    }
    // Handle single symbol
    if (typeTags.tag === "Sym") {
      tags.push(typeTags.name);
    }

    const key = `${opName.tag === "Sym" ? opName.name : opName.s}:${tags.join(",")}`;
    const proc = table.entries.get(key);

    if (proc) {
      return { ...s, control: { tag: "Val", v: proc } };
    }
    return { ...s, control: { tag: "Val", v: VFalse } };
  }});

  // op-table?: check if value is an op-table
  def("op-table?", { tag: "Native", name: "op-table?", arity: 1, fn: (args, s) => {
    const v = args[0] as any;
    return { ...s, control: { tag: "Val", v: v.tag === "OpTable" ? VTrue : VFalse } };
  }});

  // ─────────────────────────────────────────────────────────────────
  // B2: Tagged Data Protocol (attach-tag, type-tag, contents)
  // Tagged data: (type-symbol . contents)
  // ─────────────────────────────────────────────────────────────────

  // attach-tag: create tagged datum - (attach-tag type-tag contents)
  def("attach-tag", { tag: "Native", name: "attach-tag", arity: 2, fn: (args, s) => {
    const typeTag = args[0] as any;
    const contents = args[1];

    if (typeTag.tag !== "Sym" && typeTag.tag !== "Str") {
      throw new Error("attach-tag: type-tag must be symbol or string");
    }

    // Tagged datum is a pair: (type-tag . contents)
    const tagged: Val = {
      tag: "Tagged",
      typeTag: typeTag.tag === "Sym" ? typeTag.name : typeTag.s,
      contents
    } as any;
    return { ...s, control: { tag: "Val", v: tagged } };
  }});

  // type-tag: get the type tag of a tagged datum
  def("type-tag", { tag: "Native", name: "type-tag", arity: 1, fn: (args, s) => {
    const datum = args[0] as any;

    if (datum.tag === "Tagged") {
      return { ...s, control: { tag: "Val", v: { tag: "Sym", name: datum.typeTag } } };
    }
    // For primitive types, return their tag as symbol
    return { ...s, control: { tag: "Val", v: { tag: "Sym", name: datum.tag.toLowerCase() } } };
  }});

  // contents: get the contents of a tagged datum
  def("contents", { tag: "Native", name: "contents", arity: 1, fn: (args, s) => {
    const datum = args[0] as any;

    if (datum.tag === "Tagged") {
      return { ...s, control: { tag: "Val", v: datum.contents } };
    }
    // For primitive types, return the value itself
    return { ...s, control: { tag: "Val", v: args[0] } };
  }});

  // tagged?: check if value is a tagged datum
  def("tagged?", { tag: "Native", name: "tagged?", arity: 1, fn: (args, s) => {
    const v = args[0] as any;
    return { ...s, control: { tag: "Val", v: v.tag === "Tagged" ? VTrue : VFalse } };
  }});

  // ─────────────────────────────────────────────────────────────────
  // B3: apply-generic (with coercion fallback)
  // ─────────────────────────────────────────────────────────────────

  // apply-generic: dispatch based on type tags
  // (apply-generic op-table op-name . args)
  // First tries exact match, then attempts coercion
  def("apply-generic", { tag: "Native", name: "apply-generic", arity: "variadic", fn: (args, s) => {
    if (args.length < 2) throw new Error("apply-generic: expected at least (table op-name)");

    const table = args[0] as any;
    const opName = args[1] as any;
    const opArgs = args.slice(2);

    if (table.tag !== "OpTable") throw new Error("apply-generic: expected OpTable");

    // Extract type tags from arguments
    const tags: string[] = opArgs.map(arg => {
      const a = arg as any;
      if (a.tag === "Tagged") return a.typeTag;
      return a.tag.toLowerCase();
    });

    // Look up method
    const key = `${opName.tag === "Sym" ? opName.name : opName.s}:${tags.join(",")}`;
    const proc = table.entries.get(key);

    if (proc) {
      // Extract contents for tagged data
      const procArgs = opArgs.map(arg => {
        const a = arg as any;
        return a.tag === "Tagged" ? a.contents : arg;
      });

      // Apply the procedure
      const result = applyNative(proc, procArgs, s);
      if (result === null) {
        throw new Error("apply-generic: method must be Native function");
      }
      return { ...s, control: { tag: "Val", v: result } };
    }

    // No exact match - return false (caller can handle coercion)
    return { ...s, control: { tag: "Val", v: VFalse } };
  }});

  // apply-generic-coerced: dispatch with coercion table
  // (apply-generic-coerced op-table coerce-table op-name . args)
  def("apply-generic-coerced", { tag: "Native", name: "apply-generic-coerced", arity: "variadic", fn: (args, s) => {
    if (args.length < 3) throw new Error("apply-generic-coerced: expected at least (op-table coerce-table op-name)");

    const opTable = args[0] as any;
    const coerceTable = args[1] as any;
    const opName = args[2] as any;
    const opArgs = args.slice(3);

    if (opTable.tag !== "OpTable") throw new Error("apply-generic-coerced: expected OpTable");
    if (coerceTable.tag !== "CoercionTable") throw new Error("apply-generic-coerced: expected CoercionTable");

    // Extract type tags
    const tags: string[] = opArgs.map(arg => {
      const a = arg as any;
      if (a.tag === "Tagged") return a.typeTag;
      return a.tag.toLowerCase();
    });

    // Try exact match first
    const exactKey = `${opName.tag === "Sym" ? opName.name : opName.s}:${tags.join(",")}`;
    const exactProc = opTable.entries.get(exactKey);

    if (exactProc) {
      const procArgs = opArgs.map(arg => {
        const a = arg as any;
        return a.tag === "Tagged" ? a.contents : arg;
      });
      const result = applyNative(exactProc, procArgs, s);
      if (result === null) throw new Error("apply-generic-coerced: method must be Native");
      return { ...s, control: { tag: "Val", v: result } };
    }

    // Try coercion: for 2 args, try coercing arg1 to type of arg2 or vice versa
    if (opArgs.length === 2) {
      const type1 = tags[0], type2 = tags[1];

      // Try coercing type1 to type2
      const coerce1to2Key = `${type1}:${type2}`;
      const coerce1to2 = coerceTable.entries.get(coerce1to2Key);
      if (coerce1to2) {
        // Coerce first arg
        const coercedArg1 = applyNative(coerce1to2, [opArgs[0]], s);
        if (coercedArg1 !== null) {
          // Retry with coerced arg
          const newKey = `${opName.tag === "Sym" ? opName.name : opName.s}:${type2},${type2}`;
          const proc = opTable.entries.get(newKey);
          if (proc) {
            const procArgs = [coercedArg1, opArgs[1]].map(arg => {
              const a = arg as any;
              return a.tag === "Tagged" ? a.contents : arg;
            });
            const result = applyNative(proc, procArgs, s);
            if (result !== null) return { ...s, control: { tag: "Val", v: result } };
          }
        }
      }

      // Try coercing type2 to type1
      const coerce2to1Key = `${type2}:${type1}`;
      const coerce2to1 = coerceTable.entries.get(coerce2to1Key);
      if (coerce2to1) {
        const coercedArg2 = applyNative(coerce2to1, [opArgs[1]], s);
        if (coercedArg2 !== null) {
          const newKey = `${opName.tag === "Sym" ? opName.name : opName.s}:${type1},${type1}`;
          const proc = opTable.entries.get(newKey);
          if (proc) {
            const procArgs = [opArgs[0], coercedArg2].map(arg => {
              const a = arg as any;
              return a.tag === "Tagged" ? a.contents : arg;
            });
            const result = applyNative(proc, procArgs, s);
            if (result !== null) return { ...s, control: { tag: "Val", v: result } };
          }
        }
      }
    }

    // No method found
    throw new Error(`apply-generic-coerced: no method for ${exactKey}`);
  }});

  // ─────────────────────────────────────────────────────────────────
  // B4: Coercion Graph Utilities
  // ─────────────────────────────────────────────────────────────────

  // make-coercion-table: create a new coercion table
  def("make-coercion-table", { tag: "Native", name: "make-coercion-table", arity: 0, fn: (_args, s) => {
    const table: Val = {
      tag: "CoercionTable",
      entries: new Map<string, Val>(),  // "from:to" -> coercion procedure
      edges: new Map<string, string[]>(), // adjacency list for path finding
    } as any;
    return { ...s, control: { tag: "Val", v: table } };
  }});

  // put-coercion: install a coercion - (put-coercion table from-type to-type proc)
  def("put-coercion", { tag: "Native", name: "put-coercion", arity: 4, fn: (args, s) => {
    const table = args[0] as any;
    const fromType = args[1] as any;
    const toType = args[2] as any;
    const proc = args[3];

    if (table.tag !== "CoercionTable") throw new Error("put-coercion: expected CoercionTable");

    const from = fromType.tag === "Sym" ? fromType.name : fromType.s;
    const to = toType.tag === "Sym" ? toType.name : toType.s;
    const key = `${from}:${to}`;

    table.entries.set(key, proc);

    // Update edges for path finding
    if (!table.edges.has(from)) {
      table.edges.set(from, []);
    }
    if (!table.edges.get(from)!.includes(to)) {
      table.edges.get(from)!.push(to);
    }

    return { ...s, control: { tag: "Val", v: VUnit } };
  }});

  // get-coercion: lookup a coercion
  def("get-coercion", { tag: "Native", name: "get-coercion", arity: 3, fn: (args, s) => {
    const table = args[0] as any;
    const fromType = args[1] as any;
    const toType = args[2] as any;

    if (table.tag !== "CoercionTable") throw new Error("get-coercion: expected CoercionTable");

    const from = fromType.tag === "Sym" ? fromType.name : fromType.s;
    const to = toType.tag === "Sym" ? toType.name : toType.s;
    const key = `${from}:${to}`;

    const proc = table.entries.get(key);
    return { ...s, control: { tag: "Val", v: proc ?? VFalse } };
  }});

  // coercion-table?: check if value is a coercion table
  def("coercion-table?", { tag: "Native", name: "coercion-table?", arity: 1, fn: (args, s) => {
    const v = args[0] as any;
    return { ...s, control: { tag: "Val", v: v.tag === "CoercionTable" ? VTrue : VFalse } };
  }});

  // find-coercion-path: find a path from one type to another
  // Returns a list of types from source to target, or false if no path
  def("find-coercion-path", { tag: "Native", name: "find-coercion-path", arity: 3, fn: (args, s) => {
    const table = args[0] as any;
    const fromType = args[1] as any;
    const toType = args[2] as any;

    if (table.tag !== "CoercionTable") throw new Error("find-coercion-path: expected CoercionTable");

    const from = fromType.tag === "Sym" ? fromType.name : fromType.s;
    const to = toType.tag === "Sym" ? toType.name : toType.s;

    if (from === to) {
      // No coercion needed
      let result: Val = VUnit;
      result = { tag: "Vector", items: [{ tag: "Sym", name: from }, result] };
      return { ...s, control: { tag: "Val", v: result } };
    }

    // BFS to find shortest path
    const visited = new Set<string>();
    const queue: Array<{ node: string; path: string[] }> = [{ node: from, path: [from] }];

    while (queue.length > 0) {
      const { node, path } = queue.shift()!;
      if (visited.has(node)) continue;
      visited.add(node);

      const neighbors = table.edges.get(node) ?? [];
      for (const neighbor of neighbors) {
        if (neighbor === to) {
          // Found path
          const fullPath = [...path, neighbor];
          let result: Val = VUnit;
          for (let i = fullPath.length - 1; i >= 0; i--) {
            result = { tag: "Vector", items: [{ tag: "Sym", name: fullPath[i] }, result] };
          }
          return { ...s, control: { tag: "Val", v: result } };
        }
        if (!visited.has(neighbor)) {
          queue.push({ node: neighbor, path: [...path, neighbor] });
        }
      }
    }

    // No path found
    return { ...s, control: { tag: "Val", v: VFalse } };
  }});

  // find-all-coercion-paths: find ALL paths from one type to another (for detecting ambiguity)
  // Returns a list of paths, where each path is a list of types
  def("find-all-coercion-paths", { tag: "Native", name: "find-all-coercion-paths", arity: 3, fn: (args, s) => {
    const table = args[0] as any;
    const fromType = args[1] as any;
    const toType = args[2] as any;

    if (table.tag !== "CoercionTable") throw new Error("find-all-coercion-paths: expected CoercionTable");

    const from = fromType.tag === "Sym" ? fromType.name : fromType.s;
    const to = toType.tag === "Sym" ? toType.name : toType.s;

    const allPaths: string[][] = [];

    // DFS to find all paths
    function dfs(node: string, path: string[], visited: Set<string>) {
      if (node === to) {
        allPaths.push([...path]);
        return;
      }
      visited.add(node);
      const neighbors = table.edges.get(node) ?? [];
      for (const neighbor of neighbors) {
        if (!visited.has(neighbor)) {
          path.push(neighbor);
          dfs(neighbor, path, visited);
          path.pop();
        }
      }
      visited.delete(node);
    }

    dfs(from, [from], new Set());

    // Convert to list of lists
    let result: Val = VUnit;
    for (let i = allPaths.length - 1; i >= 0; i--) {
      let pathList: Val = VUnit;
      for (let j = allPaths[i].length - 1; j >= 0; j--) {
        pathList = { tag: "Vector", items: [{ tag: "Sym", name: allPaths[i][j] }, pathList] };
      }
      result = { tag: "Vector", items: [pathList, result] };
    }

    return { ...s, control: { tag: "Val", v: result } };
  }});

  // coerce-value: apply coercion along a path
  // (coerce-value table value from-type to-type)
  def("coerce-value", { tag: "Native", name: "coerce-value", arity: 4, fn: (args, s) => {
    const table = args[0] as any;
    let value = args[1];
    const fromType = args[2] as any;
    const toType = args[3] as any;

    if (table.tag !== "CoercionTable") throw new Error("coerce-value: expected CoercionTable");

    const from = fromType.tag === "Sym" ? fromType.name : fromType.s;
    const to = toType.tag === "Sym" ? toType.name : toType.s;

    if (from === to) {
      return { ...s, control: { tag: "Val", v: value } };
    }

    // Find path
    const visited = new Set<string>();
    const queue: Array<{ node: string; path: string[] }> = [{ node: from, path: [from] }];
    let coercionPath: string[] | null = null;

    while (queue.length > 0) {
      const { node, path } = queue.shift()!;
      if (visited.has(node)) continue;
      visited.add(node);

      const neighbors = table.edges.get(node) ?? [];
      for (const neighbor of neighbors) {
        if (neighbor === to) {
          coercionPath = [...path, neighbor];
          break;
        }
        if (!visited.has(neighbor)) {
          queue.push({ node: neighbor, path: [...path, neighbor] });
        }
      }
      if (coercionPath) break;
    }

    if (!coercionPath) {
      throw new Error(`coerce-value: no coercion path from ${from} to ${to}`);
    }

    // Apply coercions along the path
    for (let i = 0; i < coercionPath.length - 1; i++) {
      const key = `${coercionPath[i]}:${coercionPath[i + 1]}`;
      const coerceProc = table.entries.get(key);
      if (!coerceProc) {
        throw new Error(`coerce-value: missing coercion ${key}`);
      }
      const result = applyNative(coerceProc, [value], s);
      if (result === null) {
        throw new Error("coerce-value: coercion function must be Native");
      }
      value = result;
    }

    return { ...s, control: { tag: "Val", v: value } };
  }});

  // ─────────────────────────────────────────────────────────────────
  // B1 (Prompt 6): Term Rewriting System Primitives
  // Pattern-based AST transformation with strategies
  // ─────────────────────────────────────────────────────────────────

  // Store for rule collections (keyed by name)
  const ruleStore = new Map<string, Rule[]>();

  // make-rule: create a rewrite rule
  // (make-rule name pattern template) -> Rule
  def("make-rule", { tag: "Native", name: "make-rule", arity: 3, fn: (args, s) => {
    const nameVal = args[0] as any;
    const pattern = args[1];
    const template = args[2];

    if (nameVal.tag !== "Sym" && nameVal.tag !== "Str") {
      throw new Error("make-rule: name must be symbol or string");
    }
    const name = nameVal.tag === "Sym" ? nameVal.name : nameVal.s;

    const ruleVal: Val = {
      tag: "Rule",
      name,
      pattern,
      template
    } as any;

    return { ...s, control: { tag: "Val", v: ruleVal } };
  }});

  // make-rule-where: create a rule with a where predicate
  // (make-rule-where name pattern template pred) -> Rule
  // pred is a Native function that takes bindings map and returns bool
  def("make-rule-where", { tag: "Native", name: "make-rule-where", arity: 4, fn: (args, s) => {
    const nameVal = args[0] as any;
    const pattern = args[1];
    const template = args[2];
    const pred = args[3] as any;

    if (nameVal.tag !== "Sym" && nameVal.tag !== "Str") {
      throw new Error("make-rule-where: name must be symbol or string");
    }
    const name = nameVal.tag === "Sym" ? nameVal.name : nameVal.s;

    const ruleVal: Val = {
      tag: "Rule",
      name,
      pattern,
      template,
      wherePred: pred
    } as any;

    return { ...s, control: { tag: "Val", v: ruleVal } };
  }});

  // rule?: check if value is a rule
  def("rule?", { tag: "Native", name: "rule?", arity: 1, fn: (args, s) => {
    const v = args[0] as any;
    return { ...s, control: { tag: "Val", v: v.tag === "Rule" ? VTrue : VFalse } };
  }});

  // Helper to convert our Rule Val to the TRS Rule type
  function valToRule(v: Val): Rule {
    const r = v as any;
    if (r.tag !== "Rule") throw new Error("expected Rule");

    // Create where predicate if present
    let where: ((bindings: Record<string, unknown>) => boolean) | undefined;
    if (r.wherePred && r.wherePred.tag === "Native") {
      where = (bindings: Record<string, unknown>) => {
        // Convert bindings to a Map-like Val
        const entries: Array<[Val, Val]> = [];
        for (const [k, v] of Object.entries(bindings)) {
          entries.push([{ tag: "Sym", name: k }, v as Val]);
        }
        const bindingsVal: Val = { tag: "Map", entries };
        const result = (r.wherePred as any).fn([bindingsVal], {} as any);
        if (result.control?.tag === "Val") {
          const rv = result.control.v as any;
          return rv.tag === "Bool" && rv.b;
        }
        return false;
      };
    }

    return rule(r.name, r.pattern, r.template, where);
  }

  // Helper to convert list of Rule Vals to Rule[]
  function valListToRules(lst: Val): Rule[] {
    const rules: Rule[] = [];
    let cur = lst as any;
    while (cur.tag === "Vector" && cur.items.length >= 2) {
      rules.push(valToRule(cur.items[0]));
      cur = cur.items[1];
    }
    // Handle single rule
    if ((lst as any).tag === "Rule") {
      rules.push(valToRule(lst));
    }
    return rules;
  }

  // rewrite-once: apply rules once at first match
  // (rewrite-once rules expr) -> expr' or #f if no match
  // (rewrite-once rules expr 'topdown) -> with strategy
  def("rewrite-once", { tag: "Native", name: "rewrite-once", arity: "variadic", fn: (args, s) => {
    if (args.length < 2) throw new Error("rewrite-once: expected (rules expr [strategy])");

    const rulesVal = args[0];
    const expr = args[1];
    const strategyVal = args[2] as any;

    const rules = valListToRules(rulesVal);
    const strategy: Strategy = strategyVal?.tag === "Sym" && strategyVal.name === "bottomup"
      ? "bottomup" : "topdown";

    const result = rewriteOnce(rules, expr, strategy);

    if (result.changed) {
      // Return a pair: (expr' . rule-name)
      const resultPair: Val = {
        tag: "Vector",
        items: [
          result.result,
          { tag: "Sym", name: result.ruleName ?? "unknown" }
        ]
      };
      return { ...s, control: { tag: "Val", v: resultPair } };
    }

    return { ...s, control: { tag: "Val", v: VFalse } };
  }});

  // rewrite-fixpoint: apply rules until no more changes or fuel exhausted
  // (rewrite-fixpoint rules expr) -> expr'
  // (rewrite-fixpoint rules expr fuel) -> with fuel limit
  // (rewrite-fixpoint rules expr fuel 'bottomup) -> with strategy
  def("rewrite-fixpoint", { tag: "Native", name: "rewrite-fixpoint", arity: "variadic", fn: (args, s) => {
    if (args.length < 2) throw new Error("rewrite-fixpoint: expected (rules expr [fuel] [strategy])");

    const rulesVal = args[0];
    const expr = args[1];
    const fuelVal = args[2] as any;
    const strategyVal = args[3] as any;

    const rules = valListToRules(rulesVal);
    const fuel = fuelVal?.tag === "Num" ? fuelVal.n : 100;
    const strategy: Strategy = strategyVal?.tag === "Sym" && strategyVal.name === "bottomup"
      ? "bottomup" : "topdown";

    const result = rewriteFixpoint(rules, expr, strategy, fuel);

    // Return a record with result, steps, and whether fixpoint was reached
    const resultVal: Val = {
      tag: "Map",
      entries: [
        [{ tag: "Sym", name: "result" }, result.result],
        [{ tag: "Sym", name: "steps" }, { tag: "Num", n: result.steps }],
        [{ tag: "Sym", name: "fixpoint?" }, result.reachedFixpoint ? VTrue : VFalse]
      ]
    };

    return { ...s, control: { tag: "Val", v: resultVal } };
  }});

  // rewrite-trace: apply rules and return full trace
  // (rewrite-trace rules expr) -> list of intermediate expressions
  def("rewrite-trace", { tag: "Native", name: "rewrite-trace", arity: "variadic", fn: (args, s) => {
    if (args.length < 2) throw new Error("rewrite-trace: expected (rules expr [fuel] [strategy])");

    const rulesVal = args[0];
    const expr = args[1];
    const fuelVal = args[2] as any;
    const strategyVal = args[3] as any;

    const rules = valListToRules(rulesVal);
    const fuel = fuelVal?.tag === "Num" ? fuelVal.n : 100;
    const strategy: Strategy = strategyVal?.tag === "Sym" && strategyVal.name === "bottomup"
      ? "bottomup" : "topdown";

    const result = rewriteTrace(rules, expr, strategy, fuel);

    // Convert trace to list
    let traceList: Val = VUnit;
    for (let i = result.trace.length - 1; i >= 0; i--) {
      const step = result.trace[i];
      const stepVal: Val = {
        tag: "Map",
        entries: [
          [{ tag: "Sym", name: "expr" }, step.expr],
          [{ tag: "Sym", name: "rule" }, { tag: "Sym", name: step.ruleName ?? "initial" }],
          [{ tag: "Sym", name: "position" }, { tag: "Str", s: step.position ?? "" }]
        ]
      };
      traceList = { tag: "Vector", items: [stepVal, traceList] };
    }

    return { ...s, control: { tag: "Val", v: traceList } };
  }});

  // rewrite-conflicts: detect potential non-confluence in rules
  // (rewrite-conflicts rules) -> list of conflict reports
  def("rewrite-conflicts", { tag: "Native", name: "rewrite-conflicts", arity: 1, fn: (args, s) => {
    const rulesVal = args[0];
    const rules = valListToRules(rulesVal);

    const conflicts = detectConflicts(rules);

    // Convert to list of reports
    let conflictList: Val = VUnit;
    for (let i = conflicts.length - 1; i >= 0; i--) {
      const c = conflicts[i];
      const reportVal: Val = {
        tag: "Map",
        entries: [
          [{ tag: "Sym", name: "rule1" }, { tag: "Sym", name: c.rule1 }],
          [{ tag: "Sym", name: "rule2" }, { tag: "Sym", name: c.rule2 }],
          [{ tag: "Sym", name: "overlap" }, { tag: "Sym", name: c.overlap }],
          [{ tag: "Sym", name: "description" }, { tag: "Str", s: c.description }]
        ]
      };
      conflictList = { tag: "Vector", items: [reportVal, conflictList] };
    }

    return { ...s, control: { tag: "Val", v: conflictList } };
  }});

  // match-pattern: test if a pattern matches an expression
  // (match-pattern pattern expr) -> bindings map or #f
  def("match-pattern", { tag: "Native", name: "match-pattern", arity: 2, fn: (args, s) => {
    const pattern = args[0];
    const expr = args[1];

    const result = matchAST(pattern, expr);

    if (result.ok) {
      // Convert bindings to Map Val
      const entries: Array<[Val, Val]> = [];
      for (const [k, v] of Object.entries(result.bindings)) {
        entries.push([{ tag: "Sym", name: k }, v as Val]);
      }
      return { ...s, control: { tag: "Val", v: { tag: "Map", entries } } };
    }

    return { ...s, control: { tag: "Val", v: VFalse } };
  }});

  // substitute-template: apply bindings to a template
  // (substitute-template template bindings) -> expr
  def("substitute-template", { tag: "Native", name: "substitute-template", arity: 2, fn: (args, s) => {
    const template = args[0];
    const bindingsVal = args[1] as any;

    // Convert Map Val to bindings object
    const bindings: Record<string, unknown> = {};
    if (bindingsVal.tag === "Map") {
      for (const [k, v] of bindingsVal.entries) {
        const key = (k as any).tag === "Sym" ? (k as any).name : (k as any).s;
        bindings[key] = v;
      }
    }

    const result = substitute(template, bindings);
    return { ...s, control: { tag: "Val", v: result } };
  }});

  // ─────────────────────────────────────────────────────────────────
  // Evidence primitives
  // ─────────────────────────────────────────────────────────────────
  def("evidence-id", { tag: "Native", name: "evidence-id", arity: 1, fn: (args, s) => {
    const v = args[0];
    if (isMeaning(v) && v.evidence && v.evidence.length > 0) {
      const id = sha256JSON(v.evidence);
      return { ...s, control: { tag: "Val", v: { tag: "Str", s: id } } };
    }
    return { ...s, control: { tag: "Val", v: VFalse } };
  }});

  def("verify-evidence", { tag: "Native", name: "verify-evidence", arity: 1, fn: (args, s) => {
    const v = args[0];
    const ok = isMeaning(v) && Array.isArray(v.evidence) && v.evidence.length > 0;
    return { ...s, control: { tag: "Val", v: ok ? VTrue : VFalse } };
  }});

  def("evidence-stale?", { tag: "Native", name: "evidence-stale?", arity: 1, fn: (args, s) => {
    const v = args[0];
    if (isMeaning(v) && v.evidence && v.evidence.length > 0) {
      return { ...s, control: { tag: "Val", v: VFalse } };
    }
    return { ...s, control: { tag: "Val", v: VTrue } };
  }});

  // ─────────────────────────────────────────────────────────────────
  // Machine primitives (Prompt 8): Reified execution state
  // These enable stepping, forking, breakpoints, and time-travel debugging
  // ─────────────────────────────────────────────────────────────────

  // machine-new: Create a reified machine from an expression
  // (machine-new expr) -> Machine
  // (machine-new expr label) -> Machine with label
  def("machine-new", { tag: "Native", name: "machine-new", arity: "variadic", fn: (args, s) => {
    if (args.length < 1) throw new Error("machine-new: expected at least 1 argument");
    const exprVal = args[0];
    const label = args.length > 1 && args[1].tag === "Str" ? args[1].s : undefined;

    // Convert Val to an expression we can evaluate
    // For now, we expect a Syntax value that can be lowered to Expr
    // Or we can create a state that evaluates the value
    const machineId = `m${Date.now().toString(16)}${Math.random().toString(16).slice(2, 6)}`;

    const initialState: any = {
      control: { tag: "Val", v: exprVal },
      env: s.env,
      store: s.store,
      kont: [],
      handlers: s.handlers || [],
    };

    const machineVal: Val = {
      tag: "Machine",
      state: initialState,
      label,
      stepCount: 0,
      isDone: false,
      machineId,
    };

    return { ...s, control: { tag: "Val", v: machineVal } };
  }});

  // machine-step: Single-step a machine
  // (machine-step machine) -> Machine (updated)
  def("machine-step", { tag: "Native", name: "machine-step", arity: 1, fn: (args, s) => {
    const m = args[0] as any;
    if (m.tag !== "Machine") throw new Error("machine-step: expected Machine");
    if (m.isDone) {
      // Already done - return as-is
      return { ...s, control: { tag: "Val", v: m } };
    }

    // Import stepOnce dynamically to avoid circular deps
    const { stepOnce } = require("../../src/core/eval/machineStep");
    const outcome = stepOnce(m.state);

    const updated: Val = {
      ...m,
      stepCount: m.stepCount + 1,
      lastOutcome: outcome,
      isDone: outcome.tag === "Done",
      state: outcome.tag === "State" ? outcome.state :
             outcome.tag === "Done" ? outcome.state :
             outcome.tag === "Op" ? outcome.state : m.state,
    };

    return { ...s, control: { tag: "Val", v: updated } };
  }});

  // machine-run: Run machine to completion or breakpoint
  // (machine-run machine) -> Machine
  // (machine-run machine max-steps) -> Machine
  def("machine-run", { tag: "Native", name: "machine-run", arity: "variadic", fn: (args, s) => {
    if (args.length < 1) throw new Error("machine-run: expected at least 1 argument");
    const m = args[0] as any;
    if (m.tag !== "Machine") throw new Error("machine-run: expected Machine");
    if (m.isDone) return { ...s, control: { tag: "Val", v: m } };

    const maxSteps = args.length > 1 && args[1].tag === "Num" ? args[1].n : 10000;
    const { stepOnce } = require("../../src/core/eval/machineStep");

    let current = m;
    for (let i = 0; i < maxSteps; i++) {
      if (current.isDone) break;

      const outcome = stepOnce(current.state);

      // Check for effect breakpoints
      if (outcome.tag === "Op" && current.breakOnOps?.has(outcome.opcall.op)) {
        current = {
          ...current,
          stepCount: current.stepCount + 1,
          lastOutcome: outcome,
          state: outcome.state,
        };
        break;
      }

      // Update machine
      current = {
        ...current,
        stepCount: current.stepCount + 1,
        lastOutcome: outcome,
        isDone: outcome.tag === "Done",
        state: outcome.tag === "State" ? outcome.state :
               outcome.tag === "Done" ? outcome.state :
               outcome.tag === "Op" ? outcome.state : current.state,
      };

      // Stop on effect emission (for debugging)
      if (outcome.tag === "Op") break;
    }

    return { ...s, control: { tag: "Val", v: current } };
  }});

  // machine-stack: Get the continuation stack as a list
  // (machine-stack machine) -> list of frame descriptions
  def("machine-stack", { tag: "Native", name: "machine-stack", arity: 1, fn: (args, s) => {
    const m = args[0] as any;
    if (m.tag !== "Machine") throw new Error("machine-stack: expected Machine");

    // Convert kont frames to list of symbols describing them
    const frames = m.state.kont || [];
    let result: Val = VUnit;
    for (let i = frames.length - 1; i >= 0; i--) {
      const fr = frames[i];
      const frameDesc: Val = { tag: "Sym", name: fr.tag };
      result = { tag: "Vector", items: [frameDesc, result] };
    }

    return { ...s, control: { tag: "Val", v: result } };
  }});

  // machine-control: Get the current control value
  // (machine-control machine) -> control (Expr or Val)
  def("machine-control", { tag: "Native", name: "machine-control", arity: 1, fn: (args, s) => {
    const m = args[0] as any;
    if (m.tag !== "Machine") throw new Error("machine-control: expected Machine");

    const ctrl = m.state.control;
    if (ctrl.tag === "Val") {
      return { ...s, control: { tag: "Val", v: ctrl.v } };
    }
    // For Expr, return a description
    return { ...s, control: { tag: "Val", v: { tag: "Sym", name: `Expr:${ctrl.e.tag}` } } };
  }});

  // machine-done?: Check if machine is in terminal state
  // (machine-done? machine) -> boolean
  def("machine-done?", { tag: "Native", name: "machine-done?", arity: 1, fn: (args, s) => {
    const m = args[0] as any;
    if (m.tag !== "Machine") throw new Error("machine-done?: expected Machine");
    return { ...s, control: { tag: "Val", v: m.isDone ? VTrue : VFalse } };
  }});

  // machine-value: Get the final value (if done)
  // (machine-value machine) -> value or error
  def("machine-value", { tag: "Native", name: "machine-value", arity: 1, fn: (args, s) => {
    const m = args[0] as any;
    if (m.tag !== "Machine") throw new Error("machine-value: expected Machine");
    if (!m.isDone) throw new Error("machine-value: machine not done");

    if (m.lastOutcome?.tag === "Done") {
      return { ...s, control: { tag: "Val", v: m.lastOutcome.value } };
    }
    if (m.state.control.tag === "Val") {
      return { ...s, control: { tag: "Val", v: m.state.control.v } };
    }
    throw new Error("machine-value: no value available");
  }});

  // machine-fork: Clone a machine for multi-shot exploration
  // (machine-fork machine) -> new Machine
  // (machine-fork machine label) -> new Machine with label
  def("machine-fork", { tag: "Native", name: "machine-fork", arity: "variadic", fn: (args, s) => {
    if (args.length < 1) throw new Error("machine-fork: expected at least 1 argument");
    const m = args[0] as any;
    if (m.tag !== "Machine") throw new Error("machine-fork: expected Machine");
    const label = args.length > 1 && args[1].tag === "Str" ? args[1].s : undefined;

    const newId = `m${Date.now().toString(16)}${Math.random().toString(16).slice(2, 6)}`;

    const forked: Val = {
      tag: "Machine",
      state: JSON.parse(JSON.stringify(m.state)), // Deep clone
      label: label || m.label,
      stepCount: m.stepCount,
      breakOnOps: m.breakOnOps ? new Set(m.breakOnOps) : undefined,
      breakOnPatterns: m.breakOnPatterns?.slice(),
      lastOutcome: m.lastOutcome,
      isDone: m.isDone,
      parentId: m.machineId,
      machineId: newId,
    };

    return { ...s, control: { tag: "Val", v: forked } };
  }});

  // machine-resume: Resume a machine from an effect with a value
  // (machine-resume machine value) -> Machine
  def("machine-resume", { tag: "Native", name: "machine-resume", arity: 2, fn: (args, s) => {
    const m = args[0] as any;
    const value = args[1];
    if (m.tag !== "Machine") throw new Error("machine-resume: expected Machine");

    // Set the control to the provided value and clear effect state
    const resumed: Val = {
      ...m,
      state: {
        ...m.state,
        control: { tag: "Val", v: value },
      },
      lastOutcome: undefined,
    };

    return { ...s, control: { tag: "Val", v: resumed } };
  }});

  // machine-add-breakpoint: Add an effect breakpoint
  // (machine-add-breakpoint machine op-name) -> Machine
  def("machine-add-breakpoint", { tag: "Native", name: "machine-add-breakpoint", arity: 2, fn: (args, s) => {
    const m = args[0] as any;
    const opName = args[1] as any;
    if (m.tag !== "Machine") throw new Error("machine-add-breakpoint: expected Machine");
    if (opName.tag !== "Str" && opName.tag !== "Sym") {
      throw new Error("machine-add-breakpoint: op-name must be string or symbol");
    }

    const name = opName.tag === "Str" ? opName.s : opName.name;
    const breakpoints = m.breakOnOps ? new Set(m.breakOnOps) : new Set<string>();
    breakpoints.add(name);

    const updated: Val = {
      ...m,
      breakOnOps: breakpoints,
    };

    return { ...s, control: { tag: "Val", v: updated } };
  }});

  // machine-step-count: Get the step count
  // (machine-step-count machine) -> number
  def("machine-step-count", { tag: "Native", name: "machine-step-count", arity: 1, fn: (args, s) => {
    const m = args[0] as any;
    if (m.tag !== "Machine") throw new Error("machine-step-count: expected Machine");
    return { ...s, control: { tag: "Val", v: { tag: "Num", n: m.stepCount } } };
  }});

  // machine-last-op: Get the last operation (if any)
  // (machine-last-op machine) -> op-name or #f
  def("machine-last-op", { tag: "Native", name: "machine-last-op", arity: 1, fn: (args, s) => {
    const m = args[0] as any;
    if (m.tag !== "Machine") throw new Error("machine-last-op: expected Machine");

    if (m.lastOutcome?.tag === "Op") {
      return { ...s, control: { tag: "Val", v: { tag: "Sym", name: m.lastOutcome.opcall.op } } };
    }
    return { ...s, control: { tag: "Val", v: VFalse } };
  }});

  // machine?: Check if value is a Machine
  // (machine? x) -> boolean
  def("machine?", { tag: "Native", name: "machine?", arity: 1, fn: (args, s) => {
    return { ...s, control: { tag: "Val", v: args[0].tag === "Machine" ? VTrue : VFalse } };
  }});

  return { env, store: st };
}
