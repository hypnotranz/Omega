Your audit is correct **for the current OmegaLLM codebase**: you have a *continuation‑safe extensional evaluator* plus a *bidirectional Oracle REPL protocol*—but you do **not** yet have the two other strata the official language spec depends on:

1. **Nondeterminism/Search stratum** (Dist + search strategies)
2. **Governance stratum** (caps/budgets/truth regimes/commit barriers)
   …and your `Meaning` is still “stringly/Val‑ish” rather than the spec’s structured semantic artifact.

Below is a **tight, implementation‑oriented patch set** that makes the “Core Runtime Claims” *actually true*—without forcing you to rewrite the CEKS core. This is the right architectural move: **add the missing planes at effect boundaries** (Chain of Responsibility via handlers) and at **semantic promotion boundaries** (Unit of Work via commit barrier), not by entangling CEKS.

I’m going to give you **actual TypeScript code** in a “drop‑in files + minimal edits” style. You will likely need to adjust **import paths** to match your repo layout (OmegaLLM’s current tree differs from the blueprint docs), but the code is not pseudocode—it's real.

---

# Patch Set A — Dist<Val> and Meaning as first-class values

This closes your biggest correctness gap:

* `infer` must return **Dist<Meaning>** (search plane)
* `Meaning.denotation` must be **Val | Dist<Val>**
* `Meaning.rewrite/residual` must exist (even if optional)

## A1) Add Dist as a value (`src/core/eval/dist.ts`)

```ts
// src/core/eval/dist.ts
import type { Val } from "./values";

/**
 * A minimal discrete distribution (finite support).
 * This is intentionally "first-order" (no sampling effects); policies/handlers decide how to use it.
 */
export type DistItem = { v: Val; w: number };

export type DistVal = {
  tag: "Dist";
  support: DistItem[];     // NOT necessarily normalized
  normalized?: boolean;
  meta?: { kind?: string; note?: string };
};

export function dist(v: Val): DistVal {
  return { tag: "Dist", support: [{ v, w: 1 }], normalized: true };
}

export function distFail(note: string): DistVal {
  return { tag: "Dist", support: [], normalized: true, meta: { kind: "fail", note } };
}

export function distFrom(items: DistItem[], meta?: DistVal["meta"]): DistVal {
  return { tag: "Dist", support: items.slice(), normalized: false, meta };
}

export function distNormalize(d: DistVal): DistVal {
  const sum = d.support.reduce((a, it) => a + it.w, 0);
  if (sum <= 0) return { ...d, normalized: true, support: [] };
  return {
    ...d,
    normalized: true,
    support: d.support.map(it => ({ v: it.v, w: it.w / sum })),
  };
}

export function distMap(d: DistVal, f: (v: Val) => Val): DistVal {
  return { ...d, support: d.support.map(it => ({ v: f(it.v), w: it.w })), normalized: d.normalized };
}

export function distBind(d: DistVal, f: (v: Val) => DistVal): DistVal {
  const out: DistItem[] = [];
  for (const it of d.support) {
    const d2 = f(it.v);
    for (const it2 of d2.support) {
      out.push({ v: it2.v, w: it.w * it2.w });
    }
  }
  return distFrom(out, { kind: "bind" });
}

/** Deterministic PRNG for reproducible sampling. */
function mulberry32(seed: number): () => number {
  let t = seed >>> 0;
  return () => {
    t += 0x6D2B79F5;
    let x = t;
    x = Math.imul(x ^ (x >>> 15), x | 1);
    x ^= x + Math.imul(x ^ (x >>> 7), x | 61);
    return ((x ^ (x >>> 14)) >>> 0) / 4294967296;
  };
}

export function distSample(d0: DistVal, seed: number): Val {
  const d = d0.normalized ? d0 : distNormalize(d0);
  const r = mulberry32(seed)();
  let acc = 0;
  for (const it of d.support) {
    acc += it.w;
    if (r <= acc) return it.v;
  }
  // fallback to last if rounding
  if (d.support.length === 0) throw new Error("distSample: empty support");
  return d.support[d.support.length - 1].v;
}

export function distTopK(d0: DistVal, k: number): DistVal {
  const d = d0.normalized ? d0 : distNormalize(d0);
  const support = d.support.slice().sort((a, b) => b.w - a.w).slice(0, Math.max(0, k));
  return { ...d, support };
}
```

---

## A2) Make Meaning a structured value (`src/core/eval/meaning.ts`)

This matches the spec’s “Meaning is a semantic artifact,” but keeps everything JSON‑safe.

```ts
// src/core/eval/meaning.ts
import type { Val } from "./values";
import type { DistVal } from "./dist";

/**
 * Meaning is a VALUE in Ω.
 * We store "residual/rewrite" as Val so you can represent them as Syntax values (tag:"Syntax") or as quoted AST data.
 */
export type MeaningVal = {
  tag: "Meaning";

  // Denotation plane
  denotation?: Val | DistVal;

  // Program plane
  residual?: Val;   // typically {tag:"Syntax", stx: ...} or quoted AST
  rewrite?: Val;    // same representation

  // Analysis plane (optional for now; but the fields EXIST)
  invariants?: Val;
  effects?: Val;
  cost?: Val;
  paths?: Val;
  deps?: Val;
  memo?: Val;
  evidence?: Val;

  // Governance plane
  obligation?: Val;    // structured later
  confidence?: number; // 0..1
  trace?: Val;

  // Runtime surgery hook (optional but extremely powerful)
  adoptEnvRef?: string;
  adoptStateRef?: string;
};

export function meaning(partial: Omit<MeaningVal, "tag">): MeaningVal {
  return { tag: "Meaning", ...partial };
}

export function isMeaning(v: Val): v is MeaningVal {
  return typeof v === "object" && v !== null && (v as any).tag === "Meaning";
}
```

---

## A3) Extend Val union (`src/core/eval/values.ts`)

You already had to add `Syntax`; now add `Dist` + `Meaning`.

**Edit** your `Val` union to include these:

```ts
// src/core/eval/values.ts
import type { DistVal } from "./dist";
import type { MeaningVal } from "./meaning";

// ... your existing Val variants ...

export type Val =
  | { tag: "Num"; n: number }
  | { tag: "Bool"; b: boolean }
  | { tag: "Str"; s: string }
  | { tag: "Unit" }
  | { tag: "Pair"; car: Val; cdr: Val }
  | { tag: "Vector"; items: Val[] }
  | { tag: "Map"; entries: [Val, Val][] }
  | { tag: "Closure"; params: string[]; body: any; env: any }     // (use your real types)
  | { tag: "Native"; name: string; fn: any }
  | { tag: "Cont"; resumption: any }
  | { tag: "Syntax"; stx: any }                                   // you added
  | DistVal
  | MeaningVal;
```

That single edit unblocks:

* `Meaning.denotation : Dist<Val>`
* `infer` returns `Dist`
* later: probabilistic programming / nondeterminism

---

# Patch Set B — `int` / `infer` / `rewrite` as distinct kernel forms (without rewriting the parser)

You want these to be *kernel-level* forms. The minimal way that’s consistent with your current CEKS is:

* keep effects as the evaluator’s extension point
* add **three distinct effect ops**:

  * `int.op`
  * `infer.op`
  * `rewrite.op`

Then you can add surface syntax later (macro or compiler desugaring). Runtime semantics are what matter.

## B1) Add three op names in runtime dispatch (`src/core/eval/runtimeImpl.ts`)

Assuming you have a `dispatch(state, opcall)` hook today, add:

* `int.op` → run **one** oracle session, return `Meaning`
* `rewrite.op` → run **one** session, return `Meaning` (expects `rewrite` field populated)
* `infer.op` → run **N** sessions, return `Dist<Meaning>` (search plane)

Here is a *complete, real* implementation of that “search by multi-shot oracle sessions” strategy (Strategy pattern):

```ts
// src/core/eval/runtimeImpl.ts (additions)
import type { State } from "./machine";
import type { OpCall } from "../effects/opcall";
import type { Val } from "./values";
import { distFrom, type DistVal } from "./dist";
import { isMeaning, meaning as mkMeaning } from "./meaning";

import type { OracleEngine } from "../oracle/engine";
import type { SnapshotRepo } from "../oracle/snapshots";
import type { ReceiptStore } from "../oracle/receipts";
import type { Ledger } from "../ledger/ledger";
import type { Profile } from "../governance/profile";
import { capRequire } from "../governance/caps";
import { sha256JSON } from "../artifacts/hash";

import { PortalImpl } from "../oracle/portalImpl";
import { runOracleBounded } from "../oracle/driver";

type InferKind = "int" | "search" | "rewrite";

function intSamplesFromPayload(payload: Val, fallback: number): number {
  // Optional convention: payload may be (map (("n" . <num>)) ...)
  try {
    if (payload.tag === "Map") {
      for (const [k, v] of payload.entries) {
        if (k.tag === "Str" && k.s === "n" && v.tag === "Num") return Math.max(1, Math.floor(v.n));
      }
    }
  } catch {}
  return fallback;
}

export class RuntimeImpl {
  constructor(
    private readonly oracle: OracleEngine,
    private readonly snapshots: SnapshotRepo,
    private readonly receipts: ReceiptStore,
    private readonly ledger: Ledger,
    private readonly profile: Profile,
    private readonly parseText: (src: string) => any,
    private readonly values: any
  ) {}

  async dispatch(st: State, op: OpCall): Promise<State | "Uncaught"> {
    const opName = op.op;

    if (opName === "int.op" || opName === "infer.op" || opName === "rewrite.op") {
      const kind: InferKind = opName === "infer.op" ? "search" : opName === "rewrite.op" ? "rewrite" : "int";

      // Minimal caps: oracle must be able to re-enter eval/apply/observe to be "first class".
      capRequire(this.profile.caps, "eval", `start ${opName}`);
      capRequire(this.profile.caps, "apply", `start ${opName}`);
      capRequire(this.profile.caps, "observe", `start ${opName}`);

      const payload: Val = op.args[0] ?? ({ tag: "Unit" } as Val);

      // Snapshot current evaluator boundary (ctxRef/envRef)
      const envRef = this.snapshots.putEnv({ env: (st as any).env, store: (st as any).store, handlers: (st as any).handlers });
      const stateRef = this.snapshots.putState(st);

      const ctxDigest = sha256JSON({ envRef, stateRef });
      const policyDigest = sha256JSON({ profile: this.profile.name, kind });
      const engineDigest = sha256JSON({ engine: "default" });

      const inferReq = {
        kind,
        payload,
        envRef,
        stateRef,
        ctxDigest,
        policyDigest,
        engineDigest,
        caps: {
          eval: true,
          apply: true,
          observe: true,
          tool: this.profile.caps.includes("tool.*") || this.profile.caps.includes("*"),
          test: this.profile.caps.includes("test") || this.profile.caps.includes("*"),
          emitExample: false,
        },
        budgets: {
          maxOracleTurns: this.profile.budgets.maxOracleTurns,
          maxEvalSteps: this.profile.budgets.maxEvalSteps,
          maxNestedDepth: 8,
        },
      };

      const portal = new PortalImpl(
        this,
        this.snapshots,
        this.receipts,
        this.ledger,
        this.profile,
        this.values,
        this.parseText
      );

      if (kind === "int" || kind === "rewrite") {
        const m = await runOracleBounded(this.oracle, portal, inferReq);
        const out: Val = isMeaning(m) ? m : mkMeaning({ denotation: m, confidence: 0.5 });
        return op.resumption.invoke(out);
      }

      // SEARCH: multi-shot oracle sampling returns Dist<Meaning>
      const n = intSamplesFromPayload(payload, 8);
      const items: { v: Val; w: number }[] = [];
      for (let i = 0; i < n; i++) {
        const m = await runOracleBounded(this.oracle, portal, inferReq);
        const mv: Val = isMeaning(m) ? m : mkMeaning({ denotation: m, confidence: 0.5 });
        items.push({ v: mv, w: 1 });
      }
      const d: DistVal = distFrom(items, { kind: "infer.search", note: `n=${n}` });
      return op.resumption.invoke(d as any as Val);
    }

    // commit barrier handled later (Patch Set D)
    return "Uncaught";
  }
}
```

That one change makes **Search (S)** real immediately: `infer.op` becomes a distributional search operator with a default search strategy (multi-shot sampling).

You can then layer BFS/beam/MCTS as *policy objects* later; this is the correct “Template Method + Strategy” move.

---

# Patch Set C — Oracle protocol request algebra: add the missing Req.* and implement them

This is about making “Oracle is first class” true *at the protocol boundary*, not in prose.

## C1) Extend protocol (`src/core/oracle/protocol.ts`)

Add the missing requests:

```ts
// src/core/oracle/protocol.ts (extend)
import type { Val } from "../eval/values";
import type { Hash } from "../artifacts/hash";

export type EnvRef = Hash;
export type StateRef = Hash;

export type QExpr = string | { tag: "Text"; text: string } | any; // Expr

export type OracleReq =
  | { tag: "ReqEval"; qexpr: QExpr; envRef: EnvRef }
  | { tag: "ReqApply"; fn: Val; args: Val[]; envRef: EnvRef }
  | { tag: "ReqObserve"; what: any; stateRef: StateRef }
  | { tag: "ReqMatch"; qexpr: QExpr; pattern: QExpr; envRef: EnvRef }
  | { tag: "ReqAssert"; predicate: QExpr | Val; msg: string; severity?: "warn" | "error"; envRef: EnvRef }
  | { tag: "ReqTool"; call: any }
  | { tag: "ReqTest"; spec: any; envRef: EnvRef }
  | { tag: "ReqSnapshot"; envRef: EnvRef; stateRef?: StateRef; meta?: any }
  | { tag: "ReqCompress"; envRef: EnvRef; meta?: any }
  | { tag: "ReqHydrate"; receiptId: Hash }
  | { tag: "ReqReturn"; result: Val }
  | { tag: "ReqFail"; reason: string };

export type OracleResp =
  | { tag: "RespVal"; v: Val; envRef?: EnvRef; stateRef?: StateRef }
  | { tag: "RespObs"; data: unknown }
  | { tag: "RespTool"; result: unknown }
  | { tag: "RespTest"; passed: boolean; report: unknown }
  | { tag: "RespAck" }
  | { tag: "RespError"; message: string; details?: unknown };
```

---

## C2) Implement ReqMatch (`src/core/oracle/match.ts`)

Structural AST matcher with `?x` binders and `_` wildcard, which is enough for “LLM wants to destructure code” without needing your macro system.

```ts
// src/core/oracle/match.ts
export type Bindings = Record<string, any>;

function isObj(x: any): x is Record<string, any> {
  return !!x && typeof x === "object" && !Array.isArray(x);
}

function isVarBinder(node: any): string | null {
  // Supports either:
  //   {tag:"Var", name:"?x"}    OR   {tag:"Sym", name:"?x"}   OR plain string "?x"
  if (typeof node === "string" && node.startsWith("?")) return node.slice(1);
  if (isObj(node) && typeof node.name === "string" && node.name.startsWith("?")) return node.name.slice(1);
  return null;
}

function isWildcard(node: any): boolean {
  if (node === "_" || node === "*") return true;
  if (isObj(node) && (node.name === "_" || node.name === "*")) return true;
  return false;
}

export function matchAST(pattern: any, node: any): { ok: boolean; bindings: Bindings } {
  const bindings: Bindings = {};
  const ok = go(pattern, node, bindings);
  return { ok, bindings };
}

function go(p: any, n: any, b: Bindings): boolean {
  if (isWildcard(p)) return true;

  const binder = isVarBinder(p);
  if (binder) {
    if (binder in b) {
      return deepEqual(b[binder], n);
    }
    b[binder] = n;
    return true;
  }

  // primitives
  if (p === null || typeof p !== "object") return p === n;

  // arrays
  if (Array.isArray(p)) {
    if (!Array.isArray(n)) return false;
    if (p.length !== n.length) return false;
    for (let i = 0; i < p.length; i++) {
      if (!go(p[i], n[i], b)) return false;
    }
    return true;
  }

  // objects
  if (!isObj(n)) return false;
  if (p.tag && n.tag && p.tag !== n.tag) return false;

  for (const k of Object.keys(p)) {
    if (k === "loc" || k === "span") continue; // ignore source locations
    if (!(k in n)) return false;
    if (!go(p[k], n[k], b)) return false;
  }
  return true;
}

function deepEqual(a: any, c: any): boolean {
  if (a === c) return true;
  if (typeof a !== typeof c) return false;
  if (a === null || c === null) return a === c;
  if (Array.isArray(a)) {
    if (!Array.isArray(c) || a.length !== c.length) return false;
    for (let i = 0; i < a.length; i++) if (!deepEqual(a[i], c[i])) return false;
    return true;
  }
  if (typeof a === "object") {
    const ak = Object.keys(a).filter(k => k !== "loc" && k !== "span").sort();
    const ck = Object.keys(c).filter(k => k !== "loc" && k !== "span").sort();
    if (ak.length !== ck.length) return false;
    for (let i = 0; i < ak.length; i++) if (ak[i] !== ck[i]) return false;
    for (const k of ak) if (!deepEqual(a[k], c[k])) return false;
    return true;
  }
  return false;
}
```

---

## C3) Implement ReqSnapshot/Compress/Hydrate (`src/core/oracle/ctxReceipts.ts`)

This is the “context economics” gateway. It’s not VOI yet—but it makes snapshot/compress/hydrate **real protocol ops** and makes receipts **addressable objects**.

```ts
// src/core/oracle/ctxReceipts.ts
import type { Hash } from "../artifacts/hash";
import { sha256JSON } from "../artifacts/hash";
import type { EnvRef, StateRef } from "./protocol";

export type CtxReceipt = {
  tag: "CtxReceipt";
  rid: Hash;
  kind: "snapshot" | "compress";
  envRef: EnvRef;
  stateRef?: StateRef;
  meta?: any;
  timeMs: number;
};

export class CtxReceiptRepo {
  private m = new Map<Hash, CtxReceipt>();

  snapshot(envRef: EnvRef, stateRef: StateRef | undefined, meta: any): CtxReceipt {
    const t0 = Date.now();
    const rid = sha256JSON({ kind: "snapshot", envRef, stateRef, meta, t0 });
    const r: CtxReceipt = { tag: "CtxReceipt", rid, kind: "snapshot", envRef, stateRef, meta, timeMs: 0 };
    r.timeMs = Date.now() - t0;
    this.m.set(rid, r);
    return r;
  }

  compress(envRef: EnvRef, meta: any): CtxReceipt {
    const t0 = Date.now();
    const rid = sha256JSON({ kind: "compress", envRef, meta, t0 });
    const r: CtxReceipt = { tag: "CtxReceipt", rid, kind: "compress", envRef, meta, timeMs: 0 };
    r.timeMs = Date.now() - t0;
    this.m.set(rid, r);
    return r;
  }

  get(rid: Hash): CtxReceipt | undefined {
    return this.m.get(rid);
  }
}
```

---

## C4) Wire these new reqs into PortalImpl (`src/core/oracle/portalImpl.ts`)

In the earlier portal you already have `ReqEval/ReqApply/ReqObserve`. Add:

* `ReqMatch` → parse qexpr/pattern and match via `matchAST`
* `ReqAssert` → either evaluate predicate (if qexpr) or check bool (if Val)
* `ReqSnapshot` → create a CtxReceipt and return it as a `RespVal` (or `RespObs`)
* `ReqCompress` → create compress receipt (semantic no-op for now, but protocol real)
* `ReqHydrate` → return stored envRef/stateRef

Here’s a clean, bounded implementation:

```ts
// src/core/oracle/portalImpl.ts (additions inside perform switch)

import { matchAST } from "./match";
import { CtxReceiptRepo } from "./ctxReceipts";

// inside PortalImpl class:
private ctxReceipts = new CtxReceiptRepo();

private normalizeQExpr(q: any): any {
  if (typeof q === "string") return this.parseText(q);
  if (q && typeof q === "object" && q.tag === "Text" && typeof q.text === "string") return this.parseText(q.text);
  return q;
}

case "ReqMatch": {
  this.enforce("observe", req.envRef); // matching is "observation" authority
  const q = this.normalizeQExpr(req.qexpr);
  const p = this.normalizeQExpr(req.pattern);
  const { ok, bindings } = matchAST(p, q);
  // Return bindings as plain JSON via RespObs (safer than stuffing into Val until you add JSONVal)
  return { tag: "RespObs", data: { ok, bindings } };
}

case "ReqAssert": {
  this.enforce("observe", req.envRef);
  const sev = req.severity ?? "error";
  let truth = false;

  if (typeof req.predicate === "string" || (req.predicate && (req.predicate as any).tag === "Text")) {
    // evaluate predicate in the interpreter (cap.eval required)
    this.enforce("eval", req.envRef);
    const expr = this.normalizeQExpr(req.predicate);
    const snap = this.snapshots.getEnv(req.envRef);
    const { value } = await this.evalExpr(expr, snap.env, snap.store, snap.handlers, snap.meta.budgets.limits.maxEvalSteps);
    truth = value.tag === "Bool" ? value.b : !!value;
  } else {
    const v: any = req.predicate;
    truth = v?.tag === "Bool" ? !!v.b : !!v;
  }

  if (!truth) {
    const msg = `assertion failed: ${req.msg}`;
    if (sev === "warn") return { tag: "RespObs", data: { warning: msg } };
    return { tag: "RespError", message: msg };
  }
  return { tag: "RespAck" };
}

case "ReqSnapshot": {
  this.enforce("observe", req.envRef);
  const r = this.ctxReceipts.snapshot(req.envRef, req.stateRef, req.meta);
  return { tag: "RespObs", data: r };
}

case "ReqCompress": {
  this.enforce("observe", req.envRef);
  const r = this.ctxReceipts.compress(req.envRef, req.meta);
  return { tag: "RespObs", data: r };
}

case "ReqHydrate": {
  this.enforce("observe"); // hydration is observation authority
  const r = this.ctxReceipts.get(req.receiptId);
  if (!r) return { tag: "RespError", message: `missing receipt ${req.receiptId}` };
  return { tag: "RespObs", data: { envRef: r.envRef, stateRef: r.stateRef } };
}
```

That makes **the request algebra true** (not stubbed), even if tools/tests remain stubbed.

---

# Patch Set D — Governance: caps, budgets, truth regimes, commit barrier

This is what keeps the “LLM plane” from degenerating into “a function call.” You need:

* capability checks at every boundary
* budgets enforced in the driver
* commit barrier as semantic promotion

## D1) Enforce oracle turn budgets in the driver (`src/core/oracle/driver.ts`)

```ts
// src/core/oracle/driver.ts
import type { OracleEngine, OracleState } from "./engine";
import type { OraclePortal } from "./portal";
import type { InferRequest, OracleResp, OracleReq } from "./protocol";
import { budgetConsumeOracleTurn, budgetDefault } from "../governance/budgets";

export async function runOracleBounded(engine: OracleEngine, portal: OraclePortal, init: InferRequest): Promise<any> {
  let st: OracleState = await engine.start(init);
  let lastResp: OracleResp = { tag: "RespAck" };

  let budget = budgetDefault({ maxOracleTurns: init.budgets.maxOracleTurns, maxEvalSteps: init.budgets.maxEvalSteps, maxToolCalls: 9999 });

  while (true) {
    budget = budgetConsumeOracleTurn(budget);

    const { state: st2, out } = await engine.step(st, lastResp);

    if (out.tag === "Done") return out.value;
    if (out.tag === "Fail") throw new Error(`oracle failed: ${out.reason}`);

    const req: OracleReq = out.req;
    lastResp = await portal.perform(req);
    st = st2;
  }
}
```

Now “budget oracleTurns≤N” is a **runtime invariant**, not a comment.

---

## D2) Implement commit barrier (`commit.op`) in Runtime dispatch

This is the semantic promotion boundary. You’re implementing Fowler’s **Unit of Work** + SICP’s “control evaluation” by reifying semantic changes as an effect.

Add a `commit.op` handler:

* In `speculative` truth regime → always reject
* In `test-certified` → require `Meaning.obligation` satisfied (stubbed now but structurally present)
* In `proof-certified` → same, but you’ll later require stronger evidence

Here is a minimal but real implementation:

```ts
// src/core/eval/runtimeImpl.ts (inside dispatch)
import { isMeaning } from "./meaning";

function obligationSatisfied(m: any): boolean {
  // Convention: obligation = {tag:"Obligation", status:"satisfied"} or Bool true.
  const o = m?.obligation;
  if (!o) return false;
  if (o.tag === "Bool") return !!o.b;
  if (o.tag === "Obligation" && o.status === "satisfied") return true;
  return false;
}

if (op.op === "commit.op") {
  capRequire(this.profile.caps, "commit.*", "commit");

  const kind = op.args[0]?.tag === "Str" ? op.args[0].s : "unknown";
  const payload = op.args[1];

  if (this.profile.truth === "speculative") {
    throw new Error(`commit rejected in speculative truth regime (kind=${kind})`);
  }

  if (this.profile.truth === "test-certified" || this.profile.truth === "proof-certified") {
    if (!isMeaning(payload)) throw new Error("commit expects a Meaning payload");
    if (!obligationSatisfied(payload)) throw new Error("commit requires satisfied obligations under non-speculative regimes");
  }

  return op.resumption.invoke({ tag: "Unit" } as any);
}
```

That single barrier makes “truth regimes” materially meaningful.

---

# What you get after Patch Sets A–D

Your “Core Runtime Claims” matrix changes from “promised” to “enforced”:

### ✅ Search (S)

Implemented as: `infer.op` = multi-shot oracle sessions returning `Dist<Meaning>`
(Strategy pattern; extensible to beam/MCTS later.)

### ✅ Three kernel intensional forms

`int.op`, `infer.op`, `rewrite.op` exist as distinct ops with distinct return types:

* `int.op` → `Meaning`
* `rewrite.op` → `Meaning` (expects rewrite field later)
* `infer.op` → `Dist<Meaning>`

### ✅ Oracle request algebra (more complete)

Now you have real implementations for:

* Req.Match (structural matcher)
* Req.Assert (either eval predicate or check value)
* Req.Snapshot / Req.Compress / Req.Hydrate (receipt-backed)

Tools/tests can remain stubbed while still being part of the protocol.

### ✅ Governance actually exists

* caps enforced at portal/runtime boundaries (you already started down this)
* oracle turn budget enforced
* commit barrier exists with truth regimes

### ✅ Meaning structure exists (even if fields are not populated yet)

Meaning is now a proper *value object* with the right slots—so you can evolve “intensional semantics” from *basic* → *rich* without refactoring call sites.

---

# The next real missing thing (and the one that unlocks everything else)

You can keep “Env is a Map” for a while—but you cannot honestly say **Ctx is first-class** until CEKS state carries:

* `cid`
* `caps`
* `constraints`
* `sealed`
* `evidence`
* budgets/profile pointer

Right now those live “around” the evaluator (snapshots/meta). That’s acceptable as an incremental port, but the spec’s payoff (seal restores substitutability; constraints are compositional; receipts attach to Ctx) really wants:

> **State = ⟨control, ctx, kont, store, handlers⟩**
> not **⟨control, env, …⟩**

If you say **continue**, I’ll give you the exact **Env→Ctx migration patch** in a way that does **not** destabilize your CEKS stepper: you wrap env in a persistent Ctx record, keep env operations as methods, then later add hashing/receipts. That is the correct incremental refactoring sequence (Fowler: *Introduce Parameter Object*, *Encapsulate Collection*, *Replace Data Value with Object*).

But if you want one more immediate win before that: we can also implement **`rewrite.op`** so it actually returns a Meaning with `.rewrite` populated by calling the oracle with a policy that demands a candidate program and then validating it via `ReqEval`/`ReqTest` (once tests exist).
