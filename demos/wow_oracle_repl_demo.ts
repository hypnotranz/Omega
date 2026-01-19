// test/wow_oracle_repl_demo.ts
//
// Drop this file into OmegaLLM/test/ and run:
//
//   npx tsx test/wow_oracle_repl_demo.ts
//
// This demo is intentionally "end-to-end" and self-contained (except for evalOmega).
// It demonstrates, concretely:
//
// 1) An interactive OracleSession (async generator) that issues requests:
//    - ReqEval (REPL re-entry)
//    - ReqTest (validator loop)
//    - ReqSnapshot / ReqHydrate (multi-shot branching / rollback)
//    - ReqCompress / ReqObserve (context rocket-staging via receipts)
//    - ReqMatch (structural pattern matching with ?vars and ... ellipses)
//
// 2) A Portal that *mediates* those requests into extensional Omega evaluation.
//
// Importantly: this is not "one API call" — it is a multi-turn protocol where the
// oracle iteratively tests and revises a definition before returning.
//
// NOTE: This demo uses evalOmega as the extensional evaluator.
// It models envRef as a persistent, content-addressed list of prior (define ...) forms.

import { createHash } from "crypto";
import { evalOmega } from "./helpers/omegaHarness";
import { ModelSelectorAdapter } from "../src/core/oracle/plugins";
import type { MeaningVal } from "../src/core/oracle/meaning";
import * as fs from "fs";
import * as path from "path";

// -----------------------------
// LLM Integration
// -----------------------------
function loadApiKey(): string | undefined {
  // First try environment variables
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  if (process.env.ANTHROPIC_API_KEY) return process.env.ANTHROPIC_API_KEY;

  // Then try config file (relative to cwd, which is OmegaLLM/)
  try {
    const configPath = path.join(process.cwd(), "../LambdaRLM/config.yaml");
    const content = fs.readFileSync(configPath, "utf8");
    // Match api_key: value (handles nested YAML)
    const match = content.match(/api_key:\s*(\S+)/);
    if (match?.[1]) return match[1];
  } catch (e) {
    // Config file not found, that's okay
  }

  return undefined;
}

async function askLLMForCode(prompt: string): Promise<string> {
  const apiKey = loadApiKey();
  if (!apiKey) throw new Error("No API key found - set OPENAI_API_KEY or ANTHROPIC_API_KEY");

  const selector = new ModelSelectorAdapter({
    defaultModel: "gpt-4o-mini",
    defaultPlugin: "openai",
    sharedConfig: { apiKey, maxTokens: 200 },
  });

  const session = selector.startSession({
    tag: "Infer",
    payload: { tag: "Str", s: prompt },
    envRef: "llm-env" as any,
    stateRef: "llm-state" as any,
  });

  let result: MeaningVal | undefined;
  let resp: any = { tag: "RespAck" };

  for (let i = 0; i < 5; i++) {
    const step = await session.next(resp);
    if (step.done) {
      result = step.value;
      break;
    }
    resp = { tag: "RespAck" };
  }

  if (result?.denotation?.tag === "Str") {
    return result.denotation.s.trim();
  }
  throw new Error("LLM did not return a string");
}

// -----------------------------
// Hash
// -----------------------------
type Hash = string & { readonly __brand: "Hash" };

function sha256(s: string): Hash {
  return createHash("sha256").update(s).digest("hex") as Hash;
}

// -----------------------------
// Sexp (tiny, deterministic) + parser/printer
// -----------------------------
type Sexp =
  | { tag: "Sym"; name: string }
  | { tag: "Num"; n: number }
  | { tag: "Str"; s: string }
  | { tag: "Bool"; b: boolean }
  | { tag: "List"; items: Sexp[] };

function sym(name: string): Sexp { return { tag: "Sym", name }; }
function list(items: Sexp[]): Sexp { return { tag: "List", items }; }

function sexpToString(x: Sexp): string {
  switch (x.tag) {
    case "Sym": return x.name;
    case "Num": return String(x.n);
    case "Str": return JSON.stringify(x.s);
    case "Bool": return x.b ? "true" : "false";
    case "List": {
      if (x.items.length === 2 && x.items[0]?.tag === "Sym" && x.items[0].name === "quote") {
        return `'${sexpToString(x.items[1]!)}`;
      }
      return `(${x.items.map(sexpToString).join(" ")})`;
    }
  }
}

function sexpEq(a: Sexp, b: Sexp): boolean {
  if (a.tag !== b.tag) return false;
  switch (a.tag) {
    case "Sym": return a.name === (b as any).name;
    case "Num": return a.n === (b as any).n;
    case "Str": return a.s === (b as any).s;
    case "Bool": return a.b === (b as any).b;
    case "List": {
      const aa = a.items, bb = (b as any).items as Sexp[];
      if (aa.length !== bb.length) return false;
      for (let i = 0; i < aa.length; i++) if (!sexpEq(aa[i]!, bb[i]!)) return false;
      return true;
    }
  }
}

type Tok =
  | { tag: "LP" }
  | { tag: "RP" }
  | { tag: "QUOTE" }
  | { tag: "STR"; s: string }
  | { tag: "ATOM"; s: string };

function tokenize(src: string): Tok[] {
  const out: Tok[] = [];
  let i = 0;
  const isWS = (c: string) => c === " " || c === "\t" || c === "\n" || c === "\r";

  while (i < src.length) {
    const c = src[i]!;
    if (isWS(c)) { i++; continue; }

    // ; comments
    if (c === ";") {
      while (i < src.length && src[i] !== "\n") i++;
      continue;
    }

    if (c === "(") { out.push({ tag: "LP" }); i++; continue; }
    if (c === ")") { out.push({ tag: "RP" }); i++; continue; }
    if (c === "'") { out.push({ tag: "QUOTE" }); i++; continue; }

    if (c === "\"") {
      i++;
      let s = "";
      while (i < src.length) {
        const d = src[i]!;
        if (d === "\"") { i++; break; }
        if (d === "\\") {
          i++;
          if (i >= src.length) throw new Error("unterminated escape");
          const e = src[i]!;
          if (e === "n") s += "\n";
          else if (e === "t") s += "\t";
          else if (e === "r") s += "\r";
          else if (e === "\"") s += "\"";
          else if (e === "\\") s += "\\";
          else s += e;
          i++;
          continue;
        }
        s += d;
        i++;
      }
      out.push({ tag: "STR", s });
      continue;
    }

    let a = "";
    while (i < src.length) {
      const d = src[i]!;
      if (isWS(d) || d === "(" || d === ")" || d === "'" || d === ";") break;
      a += d;
      i++;
    }
    if (a.length === 0) throw new Error("lexer error");
    out.push({ tag: "ATOM", s: a });
  }

  return out;
}

function atomToSexp(a: string): Sexp {
  if (a === "true" || a === "#t") return { tag: "Bool", b: true };
  if (a === "false" || a === "#f") return { tag: "Bool", b: false };
  if (/^[+-]?\d+(\.\d+)?$/.test(a)) return { tag: "Num", n: Number(a) };
  return { tag: "Sym", name: a };
}

function parseSexp(src: string): Sexp {
  const toks = tokenize(src);
  let i = 0;

  function peek(): Tok | undefined { return toks[i]; }
  function take(): Tok {
    const t = toks[i];
    if (!t) throw new Error("unexpected EOF");
    i++;
    return t;
  }

  function parseOne(): Sexp {
    const t = take();
    if (t.tag === "LP") {
      const items: Sexp[] = [];
      while (true) {
        const p = peek();
        if (!p) throw new Error("unterminated list");
        if (p.tag === "RP") { take(); break; }
        items.push(parseOne());
      }
      return list(items);
    }
    if (t.tag === "RP") throw new Error("unexpected ')'");
    if (t.tag === "QUOTE") {
      const inner = parseOne();
      return list([sym("quote"), inner]);
    }
    if (t.tag === "STR") return { tag: "Str", s: t.s };
    return atomToSexp(t.s);
  }

  const out = parseOne();
  if (i !== toks.length) throw new Error("trailing tokens after first expression");
  return out;
}

// -----------------------------
// Pattern match: _, ?x vars, ... ellipsis
// -----------------------------
type Bindings = Map<string, Sexp>;
type MatchResult =
  | { ok: true; bindings: Bindings }
  | { ok: false; reason: string };

function matchSexp(pattern: Sexp, value: Sexp): MatchResult {
  const b = new Map<string, Sexp>();
  return match(pattern, value, b, false);
}

function match(p: Sexp, v: Sexp, b: Bindings, inEllipsis: boolean): MatchResult {
  // wildcard _
  if (p.tag === "Sym" && p.name === "_") return { ok: true, bindings: b };

  // pattern var ?x
  if (p.tag === "Sym" && p.name.startsWith("?") && p.name.length > 1) {
    return bindVar(p.name.slice(1), v, b, inEllipsis);
  }

  // list (with ellipsis)
  if (p.tag === "List") {
    if (v.tag !== "List") return { ok: false, reason: `expected list, got ${v.tag}` };
    return matchList(p.items, v.items, b);
  }

  // atom equality
  if (!sexpEq(p, v)) return { ok: false, reason: `literal mismatch ${sexpToString(p)} != ${sexpToString(v)}` };
  return { ok: true, bindings: b };
}

function matchList(pats: Sexp[], vals: Sexp[], b: Bindings): MatchResult {
  function cloneBindings(b0: Bindings): Bindings {
    const out = new Map<string, Sexp>();
    for (const [k, v] of b0.entries()) {
      if (v.tag === "List") out.set(k, { tag: "List", items: [...v.items] });
      else out.set(k, v);
    }
    return out;
  }

  function go(pi: number, vi: number, b0: Bindings): MatchResult {
    if (pi === pats.length && vi === vals.length) return { ok: true, bindings: b0 };
    if (pi === pats.length) return { ok: false, reason: "pattern ended early" };

    const p = pats[pi]!;
    const p2 = pats[pi + 1];
    if (p2 && p2.tag === "Sym" && p2.name === "...") {
      const rest = pats.slice(pi + 2);
      for (let k = 0; k <= (vals.length - vi); k++) {
        const bTry = cloneBindings(b0);

        let ok = true;
        for (let j = 0; j < k; j++) {
          const r1 = match(p, vals[vi + j]!, bTry, true);
          if (!r1.ok) { ok = false; break; }
        }
        if (!ok) continue;

        const r2 = matchList(rest, vals.slice(vi + k), bTry);
        if (r2.ok) return r2;
      }
      return { ok: false, reason: "ellipsis backtracking exhausted" };
    }

    if (vi >= vals.length) return { ok: false, reason: "value list ended early" };
    const r = match(p, vals[vi]!, b0, false);
    if (!r.ok) return r;
    return go(pi + 1, vi + 1, r.bindings);
  }

  return go(0, 0, b);
}

function bindVar(name: string, v: Sexp, b: Bindings, inEllipsis: boolean): MatchResult {
  const existing = b.get(name);

  if (!existing) {
    if (inEllipsis) b.set(name, { tag: "List", items: [v] });
    else b.set(name, v);
    return { ok: true, bindings: b };
  }

  if (existing.tag === "List") {
    if (!inEllipsis) {
      if (existing.items.length !== 1) return { ok: false, reason: `?${name} bound as repetition; used as scalar` };
      if (!sexpEq(existing.items[0]!, v)) return { ok: false, reason: `?${name} mismatch` };
      return { ok: true, bindings: b };
    }
    existing.items.push(v);
    return { ok: true, bindings: b };
  }

  if (inEllipsis) {
    b.set(name, { tag: "List", items: [existing, v] });
    return { ok: true, bindings: b };
  }

  if (!sexpEq(existing, v)) return { ok: false, reason: `?${name} mismatch` };
  return { ok: true, bindings: b };
}

// -----------------------------
// EnvRepo: envRef = hash(defs), defs = list of (define ...) forms.
// This gives persistent, branchable environments without mutating global state.
// -----------------------------
type EnvState = { defs: string[] };

class EnvRepo {
  private readonly envs = new Map<Hash, EnvState>();

  constructor() {
    const root: EnvState = { defs: [] };
    const rootRef = this.hashEnv(root);
    this.envs.set(rootRef, root);
  }

  root(): Hash {
    // find the one with defs=[]
    for (const [k, v] of this.envs.entries()) {
      if (v.defs.length === 0) return k;
    }
    throw new Error("no root env");
  }

  get(ref: Hash): EnvState {
    const e = this.envs.get(ref);
    if (!e) throw new Error(`unknown envRef ${ref}`);
    return e;
  }

  put(e: EnvState): Hash {
    const ref = this.hashEnv(e);
    if (!this.envs.has(ref)) this.envs.set(ref, e);
    return ref;
  }

  private hashEnv(e: EnvState): Hash {
    return sha256(JSON.stringify(e.defs));
  }
}

// -----------------------------
// Receipts: snapshot/compress produce receiptIds that can be observed or hydrated.
// -----------------------------
type CompressPolicy =
  | { tag: "Shallow"; maxDefs: number }
  | { tag: "BudgetChars"; maxChars: number };

type Receipt =
  | { tag: "ReceiptSnapshot"; rid: Hash; createdAt: number; originalEnvRef: Hash; view: string[]; digest: Hash }
  | { tag: "ReceiptCompress"; rid: Hash; createdAt: number; originalEnvRef: Hash; view: string[]; digest: Hash; policy: CompressPolicy }
  | { tag: "ReceiptTranscript"; rid: Hash; createdAt: number; events: Array<{ req: any; resp: any }> };

class ReceiptStore {
  private readonly m = new Map<Hash, Receipt>();

  put(r: Omit<Receipt, "rid">): Receipt {
    const rid = sha256(JSON.stringify(r, deterministicReplacer));
    const full = { ...(r as any), rid } as Receipt;
    this.m.set(rid, full);
    return full;
  }

  get(rid: Hash): Receipt | undefined { return this.m.get(rid); }
  has(rid: Hash): boolean { return this.m.has(rid); }
}

function deterministicReplacer(_k: string, v: any) {
  if (v && typeof v === "object" && !Array.isArray(v)) {
    const out: Record<string, any> = {};
    for (const k of Object.keys(v).sort()) out[k] = v[k];
    return out;
  }
  return v;
}

function snapshot(receipts: ReceiptStore, repo: EnvRepo, envRef: Hash): Hash {
  const env = repo.get(envRef);
  const view = [...env.defs];
  const digest = sha256(JSON.stringify(view));
  const r = receipts.put({ tag: "ReceiptSnapshot", createdAt: Date.now(), originalEnvRef: envRef, view, digest });
  return r.rid;
}

function compress(receipts: ReceiptStore, repo: EnvRepo, envRef: Hash, policy: CompressPolicy): Hash {
  const env = repo.get(envRef);
  let view: string[] = [];

  if (policy.tag === "Shallow") {
    view = env.defs.slice(0, policy.maxDefs);
  } else {
    let used = 0;
    for (const d of env.defs) {
      const c = d.length;
      if (used + c > policy.maxChars) continue;
      used += c;
      view.push(d);
    }
  }

  const digest = sha256(JSON.stringify(view));
  const r = receipts.put({ tag: "ReceiptCompress", createdAt: Date.now(), originalEnvRef: envRef, view, digest, policy });
  return r.rid;
}

function hydrate(receipts: ReceiptStore, receiptId: Hash): Hash {
  const r = receipts.get(receiptId);
  if (!r) throw new Error(`unknown receiptId ${receiptId}`);
  if (r.tag === "ReceiptSnapshot") return r.originalEnvRef;
  if (r.tag === "ReceiptCompress") return r.originalEnvRef;
  throw new Error(`cannot hydrate receipt tag ${r.tag}`);
}

// -----------------------------
// Oracle protocol (demo-level, but real multi-turn coroutine)
// -----------------------------
type ProjectionSchema =
  | { tag: "Keys" }
  | { tag: "DigestOnly" }
  | { tag: "ShallowDefs"; max?: number };

type TestSpec = {
  tag: "OmegaTests";
  cases: Array<{ name: string; expr: string; expect: string }>;
};

type TestReport = {
  tag: "TestReport";
  passed: boolean;
  cases: Array<{ name: string; passed: boolean; actual?: string; expected?: string; error?: string }>;
};

type OracleReq =
  | { tag: "ReqEval"; qexpr: string; envRef: Hash }
  | { tag: "ReqTest"; spec: TestSpec; envRef: Hash }
  | { tag: "ReqSnapshot"; envRef: Hash }
  | { tag: "ReqCompress"; envRef: Hash; policy: CompressPolicy }
  | { tag: "ReqHydrate"; receiptId: Hash }
  | { tag: "ReqObserve"; ctxRef: Hash; schema: ProjectionSchema }
  | { tag: "ReqMatch"; qexpr: string; pattern: string }
  | { tag: "ReqReturn"; result: any }
  | { tag: "ReqFail"; reason: string };

type OracleResp =
  | { tag: "RespVal"; v: any; envRef?: Hash }
  | { tag: "RespTest"; report: TestReport }
  | { tag: "RespSnapshot"; receiptId: Hash }
  | { tag: "RespObserve"; v: any }
  | { tag: "RespMatch"; ok: boolean; bindings?: Record<string, string>; reason?: string }
  | { tag: "RespError"; err: string };

type OracleSession = AsyncGenerator<OracleReq, { tag: "Return"; value: any }, OracleResp>;

// -----------------------------
// Portal: mediates oracle reqs -> extensional Omega eval, receipts, match, tests.
// -----------------------------
class Portal {
  private beginOk: boolean | null = null;

  constructor(
    private readonly repo: EnvRepo,
    private readonly receipts: ReceiptStore,
  ) {}

  async handle(req: OracleReq): Promise<OracleResp> {
    try {
      switch (req.tag) {
        case "ReqEval": {
          const { v, envRef } = await this.evalInEnvRef(req.qexpr, req.envRef);
          return { tag: "RespVal", v, envRef };
        }
        case "ReqTest": {
          const report = await this.runTests(req.spec, req.envRef);
          return { tag: "RespTest", report };
        }
        case "ReqSnapshot": {
          const rid = snapshot(this.receipts, this.repo, req.envRef);
          return { tag: "RespSnapshot", receiptId: rid };
        }
        case "ReqCompress": {
          const rid = compress(this.receipts, this.repo, req.envRef, req.policy);
          return { tag: "RespSnapshot", receiptId: rid };
        }
        case "ReqHydrate": {
          const envRef = hydrate(this.receipts, req.receiptId);
          return { tag: "RespVal", v: { tag: "Unit" }, envRef };
        }
        case "ReqObserve": {
          const v = this.observe(req.ctxRef, req.schema);
          return { tag: "RespObserve", v };
        }
        case "ReqMatch": {
          const q = parseSexp(req.qexpr);
          const p = parseSexp(req.pattern);
          const r = matchSexp(p, q);
          if (!r.ok) return { tag: "RespMatch", ok: false, reason: r.reason };
          const bindings: Record<string, string> = {};
          for (const [k, v] of r.bindings.entries()) bindings[k] = sexpToString(v);
          return { tag: "RespMatch", ok: true, bindings };
        }
        case "ReqReturn":
          return { tag: "RespVal", v: req.result };
        case "ReqFail":
          return { tag: "RespError", err: `oracle failed: ${req.reason}` };
      }
    } catch (e: any) {
      return { tag: "RespError", err: String(e?.message ?? e) };
    }
  }

  private observe(ctxRef: Hash, schema: ProjectionSchema): any {
    // ctxRef may be envRef or receiptId
    if (this.receipts.has(ctxRef)) {
      const r = this.receipts.get(ctxRef)!;
      if (schema.tag === "DigestOnly") {
        if (r.tag === "ReceiptSnapshot" || r.tag === "ReceiptCompress") return { tag: "Digest", digest: r.digest, kind: r.tag };
        return { tag: "Digest", digest: sha256(JSON.stringify(r)), kind: r.tag };
      }
      if (schema.tag === "Keys") {
        if (r.tag === "ReceiptSnapshot" || r.tag === "ReceiptCompress") return { tag: "Keys", keys: r.view.map((_d, i) => `def#${i}`), kind: r.tag };
        return { tag: "Keys", keys: [], kind: r.tag };
      }
      // ShallowDefs
      if (r.tag === "ReceiptSnapshot" || r.tag === "ReceiptCompress") {
        const max = schema.max ?? 10;
        return { tag: "ReceiptView", kind: r.tag, defs: r.view.slice(0, max), digest: r.digest };
      }
      return { tag: "ReceiptView", kind: r.tag };
    }

    // envRef
    const env = this.repo.get(ctxRef);
    if (schema.tag === "DigestOnly") return { tag: "Digest", digest: sha256(JSON.stringify(env.defs)) };
    if (schema.tag === "Keys") return { tag: "Keys", keys: env.defs.map((_d, i) => `def#${i}`) };
    const max = schema.max ?? 10;
    return { tag: "EnvView", defs: env.defs.slice(0, max), totalDefs: env.defs.length };
  }

  private async beginSupported(): Promise<boolean> {
    if (this.beginOk != null) return this.beginOk;
    try {
      const r = await evalOmega("(begin 1 2)");
      // expect 2
      this.beginOk = (r === 2);
      return this.beginOk;
    } catch {
      this.beginOk = false;
      return false;
    }
  }

  private isTopDefine(src: string): boolean {
    // We parse as sexp and check (define ...)
    try {
      const s = parseSexp(src);
      return (s.tag === "List" && s.items[0]?.tag === "Sym" && s.items[0].name === "define");
    } catch {
      // fallback heuristic
      return src.trim().startsWith("(define");
    }
  }

  private async evalInEnvRef(qexpr: string, envRef: Hash): Promise<{ v: any; envRef: Hash }> {
    const env = this.repo.get(envRef);
    const isDef = this.isTopDefine(qexpr);

    const defs = isDef ? [...env.defs, qexpr] : env.defs;
    const newRef = this.repo.put({ defs });

    const beginOk = await this.beginSupported();

    // Evaluate in the *new* env (so defines take effect before any follow-on expression).
    const program = beginOk
      ? `(begin\n${defs.join("\n")}\n${isDef ? "null" : qexpr}\n)`
      : `${defs.join("\n")}\n${isDef ? "null" : qexpr}\n`;

    const v = await evalOmega(program);
    return { v, envRef: newRef };
  }

  private showJs(v: any): string {
    if (v == null) return "null";
    if (typeof v === "number" || typeof v === "boolean") return String(v);
    if (typeof v === "string") return JSON.stringify(v);
    // fall back: stable-ish stringify
    try { return JSON.stringify(v); } catch { return String(v); }
  }

  private async runTests(spec: TestSpec, envRef: Hash): Promise<TestReport> {
    const cases: TestReport["cases"] = [];
    for (const tc of spec.cases) {
      try {
        const a = await this.evalInEnvRef(tc.expr, envRef);
        const e = await this.evalInEnvRef(tc.expect, envRef);
        const aS = this.showJs(a.v);
        const eS = this.showJs(e.v);
        const ok = aS === eS;
        cases.push({ name: tc.name, passed: ok, actual: aS, expected: eS });
      } catch (err: any) {
        cases.push({ name: tc.name, passed: false, error: String(err?.message ?? err) });
      }
    }
    return { tag: "TestReport", passed: cases.every(c => c.passed), cases };
  }
}

// -----------------------------
// Oracle driver: runs session, records transcript receipt
// -----------------------------
async function runOracleSession(
  portal: Portal,
  receipts: ReceiptStore,
  session: OracleSession,
): Promise<{ value: any; transcriptId: Hash }> {
  const events: Array<{ req: any; resp: any }> = [];

  let step = await session.next(undefined as any);
  while (!step.done) {
    const req = step.value;
    const resp = await portal.handle(req);
    events.push({ req, resp });
    step = await session.next(resp);
  }

  const value = step.value?.value;
  const r = receipts.put({ tag: "ReceiptTranscript", createdAt: Date.now(), events });
  return { value, transcriptId: r.rid };
}

// -----------------------------
// Demo OracleSession
//
// This is the "wow" loop:
// - snapshot base envRef
// - define bad add1
// - test -> fail
// - hydrate snapshot (rollback)
// - define good add1
// - test -> pass
// - compress env -> observe receipt view
// - structural match demo (+ ?x ...)
// - return Meaning-ish object
// -----------------------------
function mkOracle(env0: Hash, tests: TestSpec): OracleSession {
  return (async function* () {
    // Snapshot base env
    const s0 = (yield { tag: "ReqSnapshot", envRef: env0 }) as any;
    if (s0.tag !== "RespSnapshot") return { tag: "Return", value: { tag: "Fail", why: "snapshot failed" } };
    const snapRid: Hash = s0.receiptId;

    // Candidate 1 (wrong)
    const bad = "(define add1 (lambda (x) (+ x 2)))";
    const r1 = (yield { tag: "ReqEval", qexpr: bad, envRef: env0 }) as any;
    const envBad: Hash = r1.envRef ?? env0;

    const t1 = (yield { tag: "ReqTest", spec: tests, envRef: envBad }) as any;
    const rep1 = t1.report as TestReport;

    console.log("=== CANDIDATE 1 (wrong) ===");
    console.log("Code:", bad);
    console.log("Test result:", rep1?.passed ? "PASS" : "FAIL");
    if (rep1?.cases) {
      for (const c of rep1.cases) {
        console.log(`  ${c.name}: ${c.passed ? "✓" : "✗"} (got ${c.actual}, expected ${c.expected})`);
      }
    }

    // If failing, rollback via hydrate(snapshot)
    let envGoodBase: Hash = envBad;
    if (!rep1?.passed) {
      console.log("\n>>> ROLLBACK via hydrate(snapshot) <<<\n");
      const h = (yield { tag: "ReqHydrate", receiptId: snapRid }) as any;
      envGoodBase = h.envRef ?? env0;
    }

    // Candidate 2 (correct)
    const good = "(define add1 (lambda (x) (+ x 1)))";
    const r2 = (yield { tag: "ReqEval", qexpr: good, envRef: envGoodBase }) as any;
    const envGood: Hash = r2.envRef ?? envGoodBase;

    const t2 = (yield { tag: "ReqTest", spec: tests, envRef: envGood }) as any;
    const rep2 = t2.report as TestReport;

    console.log("=== CANDIDATE 2 (correct) ===");
    console.log("Code:", good);
    console.log("Test result:", rep2?.passed ? "PASS" : "FAIL");
    if (rep2?.cases) {
      for (const c of rep2.cases) {
        console.log(`  ${c.name}: ${c.passed ? "✓" : "✗"} (got ${c.actual}, expected ${c.expected})`);
      }
    }

    // Compress and observe staged context
    const c = (yield { tag: "ReqCompress", envRef: envGood, policy: { tag: "Shallow", maxDefs: 1 } }) as any;
    const rid = c.receiptId as Hash;

    const obs = (yield { tag: "ReqObserve", ctxRef: rid, schema: { tag: "ShallowDefs", max: 5 } }) as any;

    // Pattern match demo (ellipsis)
    const m = (yield { tag: "ReqMatch", qexpr: "(+ 1 2 3 4)", pattern: "(+ ?x ...)" }) as any;

    console.log("\n=== PATTERN MATCH DEMO ===");
    console.log("Pattern: (+ ?x ...)");
    console.log("Value: (+ 1 2 3 4)");
    console.log("Result:", m.ok ? "MATCH" : "NO MATCH");
    if (m.bindings) console.log("Bindings:", m.bindings);

    // Return structured "Meaning-ish"
    return {
      tag: "Return",
      value: {
        tag: "Meaning",
        confidence: rep2?.passed ? 0.99 : 0.2,
        rewrite: good,
        obligation: { tag: "OblTests", passed: !!rep2?.passed, report: rep2 },
        evidence: {
          snapshot: snapRid,
          compressReceipt: rid,
          observe: obs,
          match: m,
        },
      },
    };
  })();
}

// -----------------------------
// LLM-driven OracleSession
// This one ACTUALLY USES THE LLM to generate code!
// -----------------------------
function mkOracleWithLLM(env0: Hash, tests: TestSpec, maxAttempts: number = 3): OracleSession {
  return (async function* () {
    // Snapshot base env for rollback
    const s0 = (yield { tag: "ReqSnapshot", envRef: env0 }) as any;
    if (s0.tag !== "RespSnapshot") return { tag: "Return", value: { tag: "Fail", why: "snapshot failed" } };
    const snapRid: Hash = s0.receiptId;

    let currentEnv = env0;
    let lastReport: TestReport | null = null;
    let finalCode: string | null = null;

    for (let attempt = 1; attempt <= maxAttempts; attempt++) {
      console.log(`\n${"─".repeat(50)}`);
      console.log(`ATTEMPT ${attempt}/${maxAttempts} - ASKING LLM FOR CODE`);
      console.log("─".repeat(50));

      // Build prompt for LLM
      let prompt: string;
      if (attempt === 1) {
        prompt = `Write a Lisp function called add1 that takes a number x and returns x + 1.
Return ONLY the (define ...) s-expression, nothing else.
Example: (define add1 (lambda (x) (+ x 1)))`;
      } else {
        // Show LLM the test failures so it can fix them
        const failures = lastReport?.cases.filter(c => !c.passed) ?? [];
        const failureInfo = failures.map(c =>
          `  ${c.name}: expected ${c.expected}, got ${c.actual}`
        ).join("\n");

        prompt = `Your previous Lisp code failed these tests:
${failureInfo}

Fix the add1 function. Return ONLY the corrected (define ...) s-expression.`;
      }

      console.log("Prompt:", prompt.slice(0, 80) + "...");

      // Ask LLM
      let code: string;
      try {
        code = await askLLMForCode(prompt);
        // Extract just the (define ...) if there's extra text
        const match = code.match(/\(define\s+\S+\s+\(lambda[^)]*\)[^)]*\)/s);
        if (match) code = match[0];
      } catch (e: any) {
        console.log("LLM error:", e.message);
        continue;
      }

      console.log("LLM returned:", code);

      // Rollback to clean env before applying new code
      if (attempt > 1) {
        console.log("Rolling back to clean environment...");
        const h = (yield { tag: "ReqHydrate", receiptId: snapRid }) as any;
        currentEnv = h.envRef ?? env0;
      }

      // Apply the code via REAL evaluation
      const r = (yield { tag: "ReqEval", qexpr: code, envRef: currentEnv }) as any;
      currentEnv = r.envRef ?? currentEnv;

      // Run REAL tests
      const t = (yield { tag: "ReqTest", spec: tests, envRef: currentEnv }) as any;
      lastReport = t.report as TestReport;

      console.log("\nTest results:", lastReport?.passed ? "PASS ✓" : "FAIL ✗");
      if (lastReport?.cases) {
        for (const c of lastReport.cases) {
          console.log(`  ${c.name}: ${c.passed ? "✓" : "✗"} (got ${c.actual}, expected ${c.expected})`);
        }
      }

      if (lastReport?.passed) {
        finalCode = code;
        console.log("\n🎉 SUCCESS! LLM found working code.");
        break;
      }

      console.log("\n⟳ Tests failed, asking LLM to try again...");
    }

    return {
      tag: "Return",
      value: {
        tag: "Meaning",
        confidence: lastReport?.passed ? 0.99 : 0.2,
        rewrite: finalCode,
        obligation: { tag: "OblTests", passed: !!lastReport?.passed, report: lastReport },
        usedLLM: true,
      },
    };
  })();
}

// -----------------------------
// Main
// -----------------------------
async function main() {
  const useLLM = loadApiKey() !== undefined;

  console.log("═".repeat(60));
  console.log(useLLM
    ? "WOW DEMO: REAL LLM Generating Code with Test-Driven Loop"
    : "WOW DEMO: Scripted Oracle (no API key, using mock)");
  console.log("═".repeat(60));
  console.log();

  const repo = new EnvRepo();
  const receipts = new ReceiptStore();
  const portal = new Portal(repo, receipts);

  const env0 = repo.root();

  const tests: TestSpec = {
    tag: "OmegaTests",
    cases: [
      { name: "add1(0)=1", expr: "(add1 0)", expect: "1" },
      { name: "add1(41)=42", expr: "(add1 41)", expect: "42" },
    ],
  };

  // Use REAL LLM if API key available, otherwise fall back to scripted demo
  const oracle = useLLM
    ? mkOracleWithLLM(env0, tests, 3)  // Real LLM with up to 3 attempts
    : mkOracle(env0, tests);           // Scripted fallback

  const { value, transcriptId } = await runOracleSession(portal, receipts, oracle);

  console.log("\n" + "═".repeat(60));
  console.log("ORACLE RETURN");
  console.log("═".repeat(60));
  console.log(JSON.stringify(value, null, 2));

  console.log("\nTranscript Receipt ID:", transcriptId);

  // Validate resulting env by directly evaluating in the envRef that contains the "good" define.
  // We recover it from the transcript (last RespVal with envRef).
  const tr = receipts.get(transcriptId)!;
  let finalEnvRef: Hash | null = null;
  if (tr.tag === "ReceiptTranscript") {
    for (const ev of tr.events) {
      if (ev.resp?.tag === "RespVal" && ev.resp.envRef) finalEnvRef = ev.resp.envRef;
    }
  }

  if (finalEnvRef) {
    const beginOk = await (async () => {
      try { return (await evalOmega("(begin 1 2)")) === 2; } catch { return false; }
    })();

    const env = repo.get(finalEnvRef);
    const program = beginOk
      ? `(begin\n${env.defs.join("\n")}\n(add1 5)\n)`
      : `${env.defs.join("\n")}\n(add1 5)\n`;

    const v = await evalOmega(program);
    console.log("\n" + "═".repeat(60));
    console.log("POST-COMMIT VERIFICATION");
    console.log("═".repeat(60));
    console.log("(add1 5) =>", v);
    console.log(v === 6 ? "✓ Correct!" : "✗ Wrong!");
  } else {
    console.log("\n(no final envRef found in transcript; this should not happen)");
  }
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
