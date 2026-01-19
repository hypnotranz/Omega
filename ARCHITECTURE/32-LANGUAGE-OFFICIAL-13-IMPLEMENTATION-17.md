Alright. This “continue” is the **context‑economics tranche** you asked for: **receipt‑backed snapshot/compress/hydrate** with a **VOI‑driven selector**, fully wired into the **Oracle request algebra** and into **Ctx** as a first‑class runtime entity.

This is the exact layer that turns “we have pausable evaluation + oracle REPL” into the spec’s promise:

* **Snapshot** is a true **Memento** (and produces a real receipt artifact).
* **Compress** is a true **CQRS read‑model extraction** + **context staging** (rocket stages), guided by **VOI**.
* **Hydrate** is a true **reconstitution** of dropped context material (without rewinding program state).
* All of it is **governed**: capabilities enforced at the boundary; receipts are addressable; compression is visible in observations.

I’m going to give you code in 3 layers:

1. `Ctx` extended with **CtxView** (what “compressed context” *means*).
2. A `CtxReceiptRepo` + `ctxCompress/ctxHydrate/ctxSnapshot` implementation.
3. Portal wiring for `ReqSnapshot/ReqCompress/ReqHydrate` + observation of compressed ctx.

You will need to adjust import paths to match your repo (OmegaLLM vs LambdaLLM naming), but the code is concrete and consistent.

---

# 0) Capability names for context economics

We’re going to introduce three caps (fine‑grained, object‑capability style):

* `ctx.snapshot`
* `ctx.compress`
* `ctx.hydrate`

Profiles should grant them (especially pragmatic/strict/airgap). Explore with `*` already covers them.

---

# 1) Extend `Ctx` with a first-class “compressed view” (CtxView)

## 1.1 Patch: `src/core/ctx/ctx.ts`

Add `CtxView` + optional `view?: CtxView` and update hashing/projection.

```ts
// src/core/ctx/ctx.ts
import type { Hash } from "../artifacts/hash";
import { sha256JSON } from "../artifacts/hash";
import type { CapSet } from "../governance/caps";
import type { BudgetLimits } from "../governance/budgets";
import type { Profile } from "../governance/profile";
import type { Addr } from "../eval/store";

export type Constraint =
  | { tag: "NoNewFacts" }
  | { tag: "DeterministicEnvelope" }
  | { tag: "Sealed" };

export type Evidence =
  | { tag: "Note"; text: string }
  | { tag: "ReceiptRef"; rid: Hash }
  | { tag: "ToolRef"; callId: Hash };

/**
 * CtxView is the *context-economics* plane:
 * It defines what the oracle is allowed to "see" cheaply.
 * Hydration removes/replaces this view.
 */
export type CtxView = {
  tag: "CtxView";
  visibleKeys: string[];      // keys that appear in ctxProject frameKeys
  hiddenCount: number;        // how many keys are hidden
  receiptId: Hash;            // receipt that can rehydrate full visibility + dropped evidence
  keyBudgetTokens: number;
  evidenceBudgetTokens: number;
  policy: any;
};

export type Ctx = {
  tag: "Ctx";
  cid: Hash;

  parent?: Ctx;
  frame: Map<string, Addr>;

  profile: string;
  caps: CapSet;
  budgets: BudgetLimits;
  constraints: Constraint[];
  sealed: boolean;

  evidence: Evidence[];

  view?: CtxView;
};

function stableFrameJSON(frame: Map<string, Addr>): [string, Addr][] {
  // Deterministic ordering by key; addr should be JSON-safe (number|string)
  return Array.from(frame.entries()).sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0));
}

function ctxStableJSON(ctx: Omit<Ctx, "cid">): any {
  const caps = Array.from(ctx.caps).slice().sort();
  const constraints = ctx.constraints.map(c => c.tag);

  const view = ctx.view
    ? {
        tag: "CtxView",
        visibleKeys: ctx.view.visibleKeys.slice(), // preserve ordering
        hiddenCount: ctx.view.hiddenCount,
        receiptId: ctx.view.receiptId,
        keyBudgetTokens: ctx.view.keyBudgetTokens,
        evidenceBudgetTokens: ctx.view.evidenceBudgetTokens,
        policy: ctx.view.policy ?? null,
      }
    : null;

  return {
    parent: ctx.parent ? ctx.parent.cid : null,
    frame: stableFrameJSON(ctx.frame),
    profile: ctx.profile,
    caps,
    budgets: ctx.budgets,
    constraints,
    sealed: ctx.sealed,
    evidence: ctx.evidence,
    view,
  };
}

export function ctxRehash(ctx: Omit<Ctx, "cid">): Ctx {
  const cid = sha256JSON(ctxStableJSON(ctx));
  return { ...ctx, cid };
}

export function ctxRootFromProfile(p: Profile): Ctx {
  const constraints: Constraint[] = [];
  if ((p as any).noNewFacts) constraints.push({ tag: "NoNewFacts" });
  if ((p as any).deterministicEnvelope) constraints.push({ tag: "DeterministicEnvelope" });

  const base: Omit<Ctx, "cid"> = {
    tag: "Ctx",
    parent: undefined,
    frame: new Map<string, Addr>(),
    profile: p.name,
    caps: p.caps,
    budgets: p.budgets,
    constraints,
    sealed: false,
    evidence: [],
    view: undefined,
  };
  return ctxRehash(base);
}

export function ctxLookup(ctx: Ctx, name: string): Addr | undefined {
  for (let cur: Ctx | undefined = ctx; cur; cur = cur.parent) {
    const hit = cur.frame.get(name);
    if (hit !== undefined) return hit;
  }
  return undefined;
}

export function ctxDefine(ctx: Ctx, name: string, addr: Addr): Ctx {
  if (ctx.sealed) throw new Error(`ctxDefine denied: context is sealed (name=${name})`);
  const frame = new Map(ctx.frame);
  frame.set(name, addr);
  return ctxRehash({ ...ctx, frame });
}

export function ctxExtend(ctx: Ctx, binds: Array<[string, Addr]>): Ctx {
  const frame = new Map<string, Addr>();
  for (const [k, a] of binds) frame.set(k, a);

  return ctxRehash({
    tag: "Ctx",
    parent: ctx,
    frame,
    profile: ctx.profile,
    caps: ctx.caps,
    budgets: ctx.budgets,
    constraints: ctx.constraints,
    sealed: ctx.sealed,
    evidence: ctx.evidence,
    view: ctx.view,
  });
}

export function ctxSeal(ctx: Ctx): Ctx {
  if (ctx.sealed) return ctx;
  const constraints = ctx.constraints.some(c => c.tag === "Sealed")
    ? ctx.constraints
    : [...ctx.constraints, { tag: "Sealed" }];

  return ctxRehash({ ...ctx, sealed: true, constraints });
}

export function ctxAddEvidence(ctx: Ctx, ev: Evidence): Ctx {
  return ctxRehash({ ...ctx, evidence: [...ctx.evidence, ev] });
}

export function ctxApplyProfile(ctx: Ctx, p: Profile): Ctx {
  const caps = intersectCaps(ctx.caps, p.caps);
  const budgets = {
    maxOracleTurns: Math.min(ctx.budgets.maxOracleTurns, p.budgets.maxOracleTurns),
    maxEvalSteps: Math.min(ctx.budgets.maxEvalSteps, p.budgets.maxEvalSteps),
    maxToolCalls: Math.min(ctx.budgets.maxToolCalls, p.budgets.maxToolCalls),
  };

  const constraints = [...ctx.constraints];
  if ((p as any).noNewFacts && !constraints.some(c => c.tag === "NoNewFacts")) constraints.push({ tag: "NoNewFacts" });
  if ((p as any).deterministicEnvelope && !constraints.some(c => c.tag === "DeterministicEnvelope"))
    constraints.push({ tag: "DeterministicEnvelope" });

  return ctxRehash({
    ...ctx,
    profile: p.name,
    caps,
    budgets,
    constraints,
  });
}

function intersectCaps(a: CapSet, b: CapSet): CapSet {
  if (a.includes("*") && b.includes("*")) return ["*"];
  if (a.includes("*")) return b;
  if (b.includes("*")) return a;
  const setB = new Set(b);
  return a.filter(x => setB.has(x));
}

/** Gather all visible keys across lexical chain, with lexical proximity order (nearest first). */
export function ctxAllKeysWithDepth(ctx: Ctx): Array<{ name: string; depth: number }> {
  const seen = new Set<string>();
  const out: Array<{ name: string; depth: number }> = [];
  let depth = 0;
  for (let cur: Ctx | undefined = ctx; cur; cur = cur.parent) {
    for (const name of cur.frame.keys()) {
      if (seen.has(name)) continue;
      seen.add(name);
      out.push({ name, depth });
    }
    depth++;
  }
  return out;
}

/**
 * Default projection: should be safe to show to oracle.
 * This is where compression matters: if ctx.view exists, it controls what gets listed.
 */
export function ctxProject(ctx: Ctx, opts?: { maxKeys?: number }): any {
  const maxKeys = opts?.maxKeys ?? 200;

  const all = ctxAllKeysWithDepth(ctx).map(x => x.name).sort();
  let keys: string[];

  if (ctx.view) {
    // show only visible subset (preserve order from view), plus an ellipsis marker
    keys = ctx.view.visibleKeys.slice();
    if (ctx.view.hiddenCount > 0) keys.push(`… (${ctx.view.hiddenCount} hidden; hydrate=${ctx.view.receiptId})`);
  } else {
    keys = all;
    if (keys.length > maxKeys) keys = keys.slice(0, maxKeys).concat(["…"]);
  }

  return {
    tag: "CtxProjection",
    cid: ctx.cid,
    profile: ctx.profile,
    sealed: ctx.sealed,
    caps: Array.from(ctx.caps).slice().sort(),
    budgets: ctx.budgets,
    constraints: ctx.constraints.map(c => c.tag),
    frameKeys: keys,
    depth: ctxDepth(ctx),
    evidenceCount: ctx.evidence.length,
    view: ctx.view
      ? {
          receiptId: ctx.view.receiptId,
          keyBudgetTokens: ctx.view.keyBudgetTokens,
          evidenceBudgetTokens: ctx.view.evidenceBudgetTokens,
        }
      : null,
  };
}

function ctxDepth(ctx: Ctx): number {
  let d = 0;
  for (let cur: Ctx | undefined = ctx; cur; cur = cur.parent) d++;
  return d;
}
```

This is the semantic hook: **compression is not “delete env”**; it is **establish a bounded observation view** + **receipt for reconstitution**.

---

# 2) Receipt-backed Snapshot / Compress / Hydrate + VOI selection

Now we implement the actual economics, as demanded.

* VOI selection for:

  * visible keys (oracle “cheap view”)
  * evidence to keep inline vs drop into receipt
* Receipt stores:

  * dropped evidence
  * hidden keys list
  * summary + policy
* Hydration merges dropped evidence back and removes the view restriction.

## 2.1 New file: `src/core/ctx/receipts.ts`

```ts
// src/core/ctx/receipts.ts
import type { Hash } from "../artifacts/hash";
import { sha256JSON } from "../artifacts/hash";
import type { Evidence, Ctx } from "./ctx";

export type CompressPolicy = {
  tag: "CompressPolicy";
  keyBudgetTokens: number;         // approximate “LLM context tokens” allocated to binding names
  evidenceBudgetTokens: number;    // same, but for evidence payload
  maxVisibleKeys: number;          // hard cap (even if budget allows)
  maxEvidenceItems: number;        // hard cap
  mode: "keys" | "evidence" | "both";
  keepReceiptRefs: boolean;        // keep existing ReceiptRefs always
};

export function defaultCompressPolicy(partial?: Partial<CompressPolicy>): CompressPolicy {
  return {
    tag: "CompressPolicy",
    keyBudgetTokens: partial?.keyBudgetTokens ?? 400,
    evidenceBudgetTokens: partial?.evidenceBudgetTokens ?? 600,
    maxVisibleKeys: partial?.maxVisibleKeys ?? 150,
    maxEvidenceItems: partial?.maxEvidenceItems ?? 40,
    mode: partial?.mode ?? "both",
    keepReceiptRefs: partial?.keepReceiptRefs ?? true,
  };
}

export type CtxSnapshotReceipt = {
  tag: "Receipt";
  kind: "snapshot";
  rid: Hash;

  createdAtMs: number;

  // receipt subject
  ctxCid: Hash;

  // Useful payload for oracle
  projection: any;
  meta?: any;
};

export type CtxCompressReceipt = {
  tag: "Receipt";
  kind: "compress";
  rid: Hash;

  createdAtMs: number;

  // receipt subject (ctx before compress)
  baseCtxCid: Hash;

  policy: CompressPolicy;

  hiddenKeys: string[];        // keys hidden from view
  droppedEvidence: Evidence[]; // evidence removed from ctx.evidence

  summary: string;
};

export type Receipt = CtxSnapshotReceipt | CtxCompressReceipt;

export class CtxReceiptRepo {
  private m = new Map<Hash, Receipt>();

  putSnapshot(ctx: Ctx, projection: any, meta?: any): CtxSnapshotReceipt {
    // content address
    const rid = sha256JSON({
      kind: "snapshot",
      ctxCid: ctx.cid,
      projection,
      meta: meta ?? null,
    });
    const r: CtxSnapshotReceipt = {
      tag: "Receipt",
      kind: "snapshot",
      rid,
      createdAtMs: Date.now(),
      ctxCid: ctx.cid,
      projection,
      meta,
    };
    this.m.set(rid, r);
    return r;
  }

  putCompress(baseCtx: Ctx, policy: CompressPolicy, hiddenKeys: string[], droppedEvidence: Evidence[], summary: string): CtxCompressReceipt {
    const rid = sha256JSON({
      kind: "compress",
      baseCtxCid: baseCtx.cid,
      policy,
      hiddenKeys,
      droppedEvidence,
      summary,
    });
    const r: CtxCompressReceipt = {
      tag: "Receipt",
      kind: "compress",
      rid,
      createdAtMs: Date.now(),
      baseCtxCid: baseCtx.cid,
      policy,
      hiddenKeys,
      droppedEvidence,
      summary,
    };
    this.m.set(rid, r);
    return r;
  }

  get(rid: Hash): Receipt | undefined {
    return this.m.get(rid);
  }
}
```

---

## 2.2 New file: `src/core/ctx/compress.ts`

This is the actual VOI selector + compression/hydration logic.

```ts
// src/core/ctx/compress.ts
import type { Ctx, Evidence, CtxView } from "./ctx";
import { ctxAllKeysWithDepth, ctxRehash } from "./ctx";
import type { CtxReceiptRepo, CompressPolicy } from "./receipts";
import { defaultCompressPolicy } from "./receipts";

/**
 * Heuristic token estimator.
 * We cannot know real tokenizer costs here; we approximate:
 *  - ~4 chars per token + constant overhead.
 */
function approxTokens(s: string): number {
  return Math.max(1, Math.ceil(s.length / 4));
}

function keyCostTokens(name: string): number {
  // binder overhead
  return 1 + approxTokens(name);
}

function evidenceCostTokens(ev: Evidence): number {
  switch (ev.tag) {
    case "ToolRef": return 4;
    case "ReceiptRef": return 3;
    case "Note": return 2 + approxTokens(ev.text);
    default: return 4;
  }
}

function evidenceUtility(ev: Evidence, ageIndex: number): number {
  // recency bias: last items have ageIndex near 0
  const recency = 1 / (1 + ageIndex * 0.15);

  switch (ev.tag) {
    case "ToolRef": return 4.0 * recency;
    case "ReceiptRef": return 2.5 * recency;
    case "Note": {
      const lenBoost = Math.min(2.0, ev.text.length / 200);
      return (1.0 + lenBoost) * recency;
    }
    default: return 1.0 * recency;
  }
}

function keyUtility(name: string, depth: number): number {
  // lexical proximity is a strong proxy for VOI in SICP’s environment model:
  // nearer frames are more likely to matter in current computation.
  const proximity = 5 / (1 + depth);
  // tiny boost for "public-ish" identifiers (longer names tend to be more descriptive)
  const descriptiveness = Math.min(1.0, name.length / 20);
  return proximity + descriptiveness;
}

type Scored<T> = { item: T; u: number; c: number; score: number };

/**
 * Greedy VOI selector: maximize Σ u under token budget by score=u/c.
 * This is a classic fractional-knapsack heuristic (works well enough for context staging).
 */
function selectGreedy<T>(items: Scored<T>[], budgetTokens: number, maxItems: number): { kept: T[]; dropped: T[] } {
  const xs = items.slice().sort((a, b) => b.score - a.score);
  const kept: T[] = [];
  const dropped: T[] = [];
  let used = 0;

  for (const x of xs) {
    if (kept.length >= maxItems) {
      dropped.push(x.item);
      continue;
    }
    if (used + x.c <= budgetTokens) {
      kept.push(x.item);
      used += x.c;
    } else {
      dropped.push(x.item);
    }
  }
  return { kept, dropped };
}

function uniqAppend<T>(base: T[], extra: T[], eq: (a: T, b: T) => boolean): T[] {
  const out = base.slice();
  for (const x of extra) {
    if (!out.some(y => eq(x, y))) out.push(x);
  }
  return out;
}

function evidenceEq(a: Evidence, b: Evidence): boolean {
  if (a.tag !== b.tag) return false;
  if (a.tag === "Note") return (b as any).text === a.text;
  if (a.tag === "ReceiptRef") return (b as any).rid === (a as any).rid;
  if (a.tag === "ToolRef") return (b as any).callId === (a as any).callId;
  return false;
}

function keyEq(a: string, b: string): boolean {
  return a === b;
}

/**
 * Compress a context:
 *  - compute visible key set under VOI budget
 *  - drop low-VOI evidence into a receipt
 *  - install CtxView so oracle observation is bounded
 *  - append ReceiptRef to evidence for provenance
 */
export function ctxCompress(ctx: Ctx, repo: CtxReceiptRepo, policy0?: Partial<CompressPolicy>): { ctx2: Ctx; receiptId: string } {
  const policy = defaultCompressPolicy(policy0);

  const keysWithDepth = ctxAllKeysWithDepth(ctx);

  // Always expose keys in current frame (depth 0) — they are the “local variables” of the current locus.
  const locked = keysWithDepth.filter(k => k.depth === 0).map(k => k.name);
  const lockedCost = locked.reduce((a, k) => a + keyCostTokens(k), 0);

  const scoredKeys: Scored<string>[] = [];
  for (const kd of keysWithDepth) {
    if (locked.includes(kd.name)) continue;
    const u = keyUtility(kd.name, kd.depth);
    const c = keyCostTokens(kd.name);
    scoredKeys.push({ item: kd.name, u, c, score: u / c });
  }

  const keyBudget = Math.max(0, policy.keyBudgetTokens - lockedCost);
  const { kept: keptExtraKeys, dropped: droppedKeys } = policy.mode === "evidence"
    ? { kept: [] as string[], dropped: scoredKeys.map(x => x.item) }
    : selectGreedy(scoredKeys, keyBudget, Math.max(0, policy.maxVisibleKeys - locked.length));

  const visibleKeys = policy.mode === "evidence"
    ? locked.slice().sort()
    : uniqAppend(locked.slice(), keptExtraKeys, keyEq);

  // Evidence compression
  const evidence = ctx.evidence ?? [];
  const alwaysKeep: Evidence[] = [];
  const candidates: Evidence[] = [];

  for (const ev of evidence) {
    if (policy.keepReceiptRefs && ev.tag === "ReceiptRef") alwaysKeep.push(ev);
    else candidates.push(ev);
  }

  const scoredEvidence: Scored<Evidence>[] = [];
  // ageIndex = 0 for newest (last element), so reverse iterate
  for (let i = 0; i < candidates.length; i++) {
    const ev = candidates[candidates.length - 1 - i];
    const u = evidenceUtility(ev, i);
    const c = evidenceCostTokens(ev);
    scoredEvidence.push({ item: ev, u, c, score: u / c });
  }

  const { kept: keptEv, dropped: droppedEv } = policy.mode === "keys"
    ? { kept: [] as Evidence[], dropped: candidates.slice() }
    : selectGreedy(scoredEvidence, policy.evidenceBudgetTokens, policy.maxEvidenceItems);

  // keptEv is selected from reverse order; restore stable order as in original evidence list
  const keptEvSet = new Set(keptEv.map(ev => JSON.stringify(ev)));
  const keptEvOrdered = candidates.filter(ev => keptEvSet.has(JSON.stringify(ev)));

  const hiddenKeys = keysWithDepth.map(k => k.name).filter(k => !visibleKeys.includes(k));
  const droppedEvidence = droppedEv;

  const summary = `compress ctx=${ctx.cid} hiddenKeys=${hiddenKeys.length} droppedEvidence=${droppedEvidence.length}`;

  const receipt = repo.putCompress(ctx, policy, hiddenKeys, droppedEvidence, summary);

  const view: CtxView = {
    tag: "CtxView",
    visibleKeys: visibleKeys.slice(),
    hiddenCount: hiddenKeys.length,
    receiptId: receipt.rid,
    keyBudgetTokens: policy.keyBudgetTokens,
    evidenceBudgetTokens: policy.evidenceBudgetTokens,
    policy,
  };

  // New evidence list: keep compact refs + chosen evidence + new receipt ref
  const evidence2 = uniqAppend(
    uniqAppend(alwaysKeep, keptEvOrdered, evidenceEq),
    [{ tag: "ReceiptRef", rid: receipt.rid }],
    evidenceEq
  );

  const ctx2 = ctxRehash({ ...ctx, evidence: evidence2, view });

  return { ctx2, receiptId: receipt.rid };
}

/**
 * Hydrate reverses a compress receipt:
 *  - removes view restriction
 *  - merges droppedEvidence back into ctx.evidence
 *
 * NOTE: This is *not* time-travel. It does not revert store/state.
 * It is reconstitution of “hidden context” for the oracle.
 */
export function ctxHydrate(ctx: Ctx, receipt: { rid: string; kind: "compress"; droppedEvidence: Evidence[]; hiddenKeys: string[] }): Ctx {
  // Applicability checks: either view refers to receipt, or evidence contains ReceiptRef.
  const okView = ctx.view?.receiptId === receipt.rid;
  const okEvidence = (ctx.evidence ?? []).some(ev => ev.tag === "ReceiptRef" && ev.rid === receipt.rid);

  if (!okView && !okEvidence) {
    throw new Error(`ctxHydrate: receipt ${receipt.rid} not applicable to ctx ${ctx.cid}`);
  }

  const evidenceMerged = uniqAppend(ctx.evidence ?? [], receipt.droppedEvidence ?? [], evidenceEq);
  const ctx2 = ctxRehash({ ...ctx, evidence: evidenceMerged, view: undefined });
  return ctx2;
}
```

This is the “rocket staging” mechanism in concrete form:

* **compression** = reduced cheap view + receipt reference + evidence trimming
* **hydration** = restore dropped evidence + remove view restriction

---

# 3) Wire protocol ops in the Oracle Portal

You already have `ReqEval/ReqApply/ReqObserve`. Now you implement:

* `ReqSnapshot` → create and store a snapshot receipt (projection)
* `ReqCompress` → compress the current ctx into a new envRef
* `ReqHydrate` → hydrate the current ctx using the receipt and produce a new envRef

To do that correctly, the portal must track an **active envRef** for the session (because `ReqHydrate(receiptId)` doesn’t carry an envRef).

## 3.1 Patch: `src/core/oracle/portalImpl.ts`

This is the exact wiring. Search for your existing `PortalImpl` and apply these deltas.

### Add imports

```ts
import type { EnvRef } from "./protocol";
import { ctxProject } from "../ctx/ctx";
import { CtxReceiptRepo, defaultCompressPolicy } from "../ctx/receipts";
import { ctxCompress, ctxHydrate } from "../ctx/compress";
import { capRequire, capIntersect } from "../governance/caps";
```

### Add fields

```ts
private activeEnvRef?: EnvRef;
private ctxReceipts = new CtxReceiptRepo();
```

### Add a setter (called by runtime when session starts)

```ts
setActiveEnvRef(envRef: EnvRef): void {
  this.activeEnvRef = envRef;
}
```

### Update `perform` to maintain activeEnvRef when returning a new envRef

Right before you `return resp;`, if `resp.tag === "RespVal" && resp.envRef`, set it:

```ts
if ((resp as any).tag === "RespVal" && (resp as any).envRef) {
  this.activeEnvRef = (resp as any).envRef;
}
```

### Implement caps for ctx.* using intersection

Add helper:

```ts
private effectiveCaps(envRef: EnvRef): string[] {
  const snap = this.snapshots.getEnv(envRef);
  const ctxCaps = (snap.env as any).caps ?? [];
  return capIntersect(ctxCaps, this.sessionProfile.caps) as any;
}

private enforce(cap: string, envRef?: EnvRef): void {
  const caps = envRef ? this.effectiveCaps(envRef) : this.sessionProfile.caps;
  capRequire(caps as any, cap, `portal/${cap}`);
}
```

### Implement `ReqObserve` for `{tag:"Ctx"}`

Inside your `ReqObserve` switch:

```ts
case "ReqObserve": {
  this.enforce("observe");
  const st = this.snapshots.getState(req.stateRef).state;
  const what = req.what;

  if (what?.tag === "Ctx") {
    const env = (st as any).env;
    return { tag: "RespObs", data: ctxProject(env, { maxKeys: what.maxKeys ?? 200 }) };
  }

  // existing Stack / Control / Handlers observers...
  return { tag: "RespObs", data: { error: "unknown observe spec", what } };
}
```

### Implement `ReqSnapshot`

```ts
case "ReqSnapshot": {
  this.enforce("ctx.snapshot", req.envRef);
  this.enforce("observe", req.envRef);

  const snap = this.snapshots.getEnv(req.envRef);
  const ctx = snap.env as any;

  const projection = ctxProject(ctx, { maxKeys: req.meta?.maxKeys ?? 200 });
  const receipt = this.ctxReceipts.putSnapshot(ctx, projection, req.meta);

  // NOTE: snapshot does not mutate ctx. It’s a pure Memento emission.
  return { tag: "RespObs", data: receipt };
}
```

### Implement `ReqCompress`

```ts
case "ReqCompress": {
  this.enforce("ctx.compress", req.envRef);
  this.enforce("observe", req.envRef);

  const snap = this.snapshots.getEnv(req.envRef);
  const ctx = snap.env as any;

  // policy is JSON; allow req.meta to partially specify it
  const policy = defaultCompressPolicy(req.meta?.policy ?? req.meta ?? {});

  const { ctx2, receiptId } = ctxCompress(ctx, this.ctxReceipts, policy);

  const envRef2 = this.snapshots.putEnv({ env: ctx2, store: snap.store, handlers: snap.handlers });
  this.activeEnvRef = envRef2;

  return { tag: "RespObs", data: { receiptId, envRef: envRef2, ctxCid: ctx2.cid } };
}
```

### Implement `ReqHydrate`

```ts
case "ReqHydrate": {
  this.enforce("ctx.hydrate");
  this.enforce("observe");

  const rid = req.receiptId;
  const receipt = this.ctxReceipts.get(rid);

  if (!receipt) return { tag: "RespError", message: `missing receipt ${rid}` };

  // Snapshot receipts: hydration returns the receipt payload (projection/meta)
  if (receipt.kind === "snapshot") {
    return { tag: "RespObs", data: receipt };
  }

  // Compress receipts: hydrate into current active env snapshot.
  const targetEnvRef = this.activeEnvRef;
  if (!targetEnvRef) return { tag: "RespError", message: `hydrate has no active envRef` };

  const snap = this.snapshots.getEnv(targetEnvRef);
  const ctx = snap.env as any;

  const ctx2 = ctxHydrate(ctx, receipt);
  const envRef2 = this.snapshots.putEnv({ env: ctx2, store: snap.store, handlers: snap.handlers });
  this.activeEnvRef = envRef2;

  return { tag: "RespObs", data: { envRef: envRef2, ctxCid: ctx2.cid } };
}
```

That completes the Oracle request algebra features for context economics:

* snapshot → receipt
* compress → new envRef + receipt
* hydrate → new envRef restoring visibility/evidence

---

# 4) Update profiles to include ctx.* capabilities

## 4.1 Patch: `src/core/governance/profile.ts`

Add `"ctx.*"` to pragmatic/strict/airgap.

```ts
export const profilePragmatic: Profile = {
  name: "pragmatic",
  caps: ["eval", "apply", "observe", "ctx.*", "tool.*", "commit.*", "train.emit"],
  budgets: { maxOracleTurns: 250, maxEvalSteps: 350_000, maxToolCalls: 100 },
  truth: "test-certified",
  audit: "full",
  noNewFacts: true,
};

export const profileStrict: Profile = {
  name: "strict",
  caps: ["eval", "apply", "observe", "ctx.*", "tool.*", "commit.*", "train.emit"],
  budgets: { maxOracleTurns: 200, maxEvalSteps: 300_000, maxToolCalls: 50 },
  truth: "proof-certified",
  audit: "forensic",
  noNewFacts: true,
  deterministicEnvelope: true,
};

export const profileAirgap: Profile = {
  name: "airgap",
  caps: ["eval", "apply", "observe", "ctx.*"],
  budgets: { maxOracleTurns: 150, maxEvalSteps: 250_000, maxToolCalls: 0 },
  truth: "test-certified",
  audit: "full",
  noNewFacts: true,
  deterministicEnvelope: true,
};
```

---

# 5) Make commit barriers attach evidence to the live context

This is critical: commits must become **semantic promotion events** that are visible in the context’s provenance.

You already have ledger events. Use the **event hash** as a receipt id and push it into `ctx.evidence` as `ReceiptRef`.

## 5.1 Patch: commit handler in `RuntimeImpl.dispatch`

Inside your `commit.op` handling branch:

```ts
import { ctxAddEvidence } from "../ctx/ctx"; // adjust path
// ...
if (op.op === "commit.op") {
  capRequire(this.profile.caps, "commit.*", "commit");

  const kind = op.args[0]?.tag === "Str" ? op.args[0].s : "unknown";
  const payload = op.args[1];

  // reject under speculative regime
  if (this.profile.truth === "speculative") {
    const h = this.ledger.append({ tag: "CommitRejected", kind, payloadDigest: sha256JSON(payload), reason: "speculative", timeMs: Date.now() } as any);
    throw new Error(`commit rejected in speculative regime (event=${h})`);
  }

  // accept
  const evHash = this.ledger.append({ tag: "CommitAccepted", kind, payloadDigest: sha256JSON(payload), timeMs: Date.now() } as any);

  // resume computation
  const st2 = op.resumption.invoke({ tag: "Unit" } as any);

  // attach provenance to context (Env is Ctx)
  const ctx0 = (st as any).env;
  const ctx1 = ctxAddEvidence(ctx0, { tag: "ReceiptRef", rid: evHash });

  (st2 as any).env = ctx1;
  return st2;
}
```

Now commit is not “just an op” — it’s an explicit traceable promotion that composes with context economics.

---

# 6) What this gives you, concretely

After this tranche, you have:

✅ **Req.Snapshot / Req.Compress / Req.Hydrate implemented**
✅ **Receipts exist as first-class artifacts** (addressable by hash rid)
✅ **Compression is VOI-guided** (greedy knapsack by u/c + hard caps)
✅ **Hydration is semantic** (restores dropped evidence + removes view)
✅ **Oracle observation shows compressed view + hydrate pointer**
✅ **Commits leave provenance in ctx.evidence** (ReceiptRef to ledger event hash)

This makes your Section 15 promises materially true:

* SnapshotRepo is still the storage for actual evaluator states (env+store+handlers).
* Receipts are the CQRS read-model / audit artifact for context staging.
* Compression/hydration are no longer stubs — they are a deterministic mechanism.

---

# 7) Next tranche (the one you actually want next)

You now have the *mechanism*. The next tranche is the *policy sophistication* and *integration correctness*:

1. **VOI driven by usage stats** (not just lexical proximity + recency)

   * instrument `envLookup` to increment a (bounded) counter map in ctx.view policy metadata
   * use access frequency as VOI

2. **Receipt invalidation** (dependency changes)

   * attach `deps` to receipts (tool hashes, ledger head)
   * refuse hydrate if deps mismatch in strict profile

3. **Search semantics for infer**

   * plug in beam/MCTS policies; use compress/hydrate to manage oracle prompt budget during search

If you say **CONTINUE** again, I’ll implement **usage-instrumented VOI** (counts + decay) and a proper **receipt applicability/invalidation** rule that is strict-profile compliant (this is where “no new facts” starts to actually bite).
Alright. This “continue” is the **context‑economics tranche** you asked for: **receipt‑backed snapshot/compress/hydrate** with a **VOI‑driven selector**, fully wired into the **Oracle request algebra** and into **Ctx** as a first‑class runtime entity.

This is the exact layer that turns “we have pausable evaluation + oracle REPL” into the spec’s promise:

* **Snapshot** is a true **Memento** (and produces a real receipt artifact).
* **Compress** is a true **CQRS read‑model extraction** + **context staging** (rocket stages), guided by **VOI**.
* **Hydrate** is a true **reconstitution** of dropped context material (without rewinding program state).
* All of it is **governed**: capabilities enforced at the boundary; receipts are addressable; compression is visible in observations.

I’m going to give you code in 3 layers:

1. `Ctx` extended with **CtxView** (what “compressed context” *means*).
2. A `CtxReceiptRepo` + `ctxCompress/ctxHydrate/ctxSnapshot` implementation.
3. Portal wiring for `ReqSnapshot/ReqCompress/ReqHydrate` + observation of compressed ctx.

You will need to adjust import paths to match your repo (OmegaLLM vs LambdaLLM naming), but the code is concrete and consistent.

---

# 0) Capability names for context economics

We’re going to introduce three caps (fine‑grained, object‑capability style):

* `ctx.snapshot`
* `ctx.compress`
* `ctx.hydrate`

Profiles should grant them (especially pragmatic/strict/airgap). Explore with `*` already covers them.

---

# 1) Extend `Ctx` with a first-class “compressed view” (CtxView)

## 1.1 Patch: `src/core/ctx/ctx.ts`

Add `CtxView` + optional `view?: CtxView` and update hashing/projection.

```ts
// src/core/ctx/ctx.ts
import type { Hash } from "../artifacts/hash";
import { sha256JSON } from "../artifacts/hash";
import type { CapSet } from "../governance/caps";
import type { BudgetLimits } from "../governance/budgets";
import type { Profile } from "../governance/profile";
import type { Addr } from "../eval/store";

export type Constraint =
  | { tag: "NoNewFacts" }
  | { tag: "DeterministicEnvelope" }
  | { tag: "Sealed" };

export type Evidence =
  | { tag: "Note"; text: string }
  | { tag: "ReceiptRef"; rid: Hash }
  | { tag: "ToolRef"; callId: Hash };

/**
 * CtxView is the *context-economics* plane:
 * It defines what the oracle is allowed to "see" cheaply.
 * Hydration removes/replaces this view.
 */
export type CtxView = {
  tag: "CtxView";
  visibleKeys: string[];      // keys that appear in ctxProject frameKeys
  hiddenCount: number;        // how many keys are hidden
  receiptId: Hash;            // receipt that can rehydrate full visibility + dropped evidence
  keyBudgetTokens: number;
  evidenceBudgetTokens: number;
  policy: any;
};

export type Ctx = {
  tag: "Ctx";
  cid: Hash;

  parent?: Ctx;
  frame: Map<string, Addr>;

  profile: string;
  caps: CapSet;
  budgets: BudgetLimits;
  constraints: Constraint[];
  sealed: boolean;

  evidence: Evidence[];

  view?: CtxView;
};

function stableFrameJSON(frame: Map<string, Addr>): [string, Addr][] {
  // Deterministic ordering by key; addr should be JSON-safe (number|string)
  return Array.from(frame.entries()).sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0));
}

function ctxStableJSON(ctx: Omit<Ctx, "cid">): any {
  const caps = Array.from(ctx.caps).slice().sort();
  const constraints = ctx.constraints.map(c => c.tag);

  const view = ctx.view
    ? {
        tag: "CtxView",
        visibleKeys: ctx.view.visibleKeys.slice(), // preserve ordering
        hiddenCount: ctx.view.hiddenCount,
        receiptId: ctx.view.receiptId,
        keyBudgetTokens: ctx.view.keyBudgetTokens,
        evidenceBudgetTokens: ctx.view.evidenceBudgetTokens,
        policy: ctx.view.policy ?? null,
      }
    : null;

  return {
    parent: ctx.parent ? ctx.parent.cid : null,
    frame: stableFrameJSON(ctx.frame),
    profile: ctx.profile,
    caps,
    budgets: ctx.budgets,
    constraints,
    sealed: ctx.sealed,
    evidence: ctx.evidence,
    view,
  };
}

export function ctxRehash(ctx: Omit<Ctx, "cid">): Ctx {
  const cid = sha256JSON(ctxStableJSON(ctx));
  return { ...ctx, cid };
}

export function ctxRootFromProfile(p: Profile): Ctx {
  const constraints: Constraint[] = [];
  if ((p as any).noNewFacts) constraints.push({ tag: "NoNewFacts" });
  if ((p as any).deterministicEnvelope) constraints.push({ tag: "DeterministicEnvelope" });

  const base: Omit<Ctx, "cid"> = {
    tag: "Ctx",
    parent: undefined,
    frame: new Map<string, Addr>(),
    profile: p.name,
    caps: p.caps,
    budgets: p.budgets,
    constraints,
    sealed: false,
    evidence: [],
    view: undefined,
  };
  return ctxRehash(base);
}

export function ctxLookup(ctx: Ctx, name: string): Addr | undefined {
  for (let cur: Ctx | undefined = ctx; cur; cur = cur.parent) {
    const hit = cur.frame.get(name);
    if (hit !== undefined) return hit;
  }
  return undefined;
}

export function ctxDefine(ctx: Ctx, name: string, addr: Addr): Ctx {
  if (ctx.sealed) throw new Error(`ctxDefine denied: context is sealed (name=${name})`);
  const frame = new Map(ctx.frame);
  frame.set(name, addr);
  return ctxRehash({ ...ctx, frame });
}

export function ctxExtend(ctx: Ctx, binds: Array<[string, Addr]>): Ctx {
  const frame = new Map<string, Addr>();
  for (const [k, a] of binds) frame.set(k, a);

  return ctxRehash({
    tag: "Ctx",
    parent: ctx,
    frame,
    profile: ctx.profile,
    caps: ctx.caps,
    budgets: ctx.budgets,
    constraints: ctx.constraints,
    sealed: ctx.sealed,
    evidence: ctx.evidence,
    view: ctx.view,
  });
}

export function ctxSeal(ctx: Ctx): Ctx {
  if (ctx.sealed) return ctx;
  const constraints = ctx.constraints.some(c => c.tag === "Sealed")
    ? ctx.constraints
    : [...ctx.constraints, { tag: "Sealed" }];

  return ctxRehash({ ...ctx, sealed: true, constraints });
}

export function ctxAddEvidence(ctx: Ctx, ev: Evidence): Ctx {
  return ctxRehash({ ...ctx, evidence: [...ctx.evidence, ev] });
}

export function ctxApplyProfile(ctx: Ctx, p: Profile): Ctx {
  const caps = intersectCaps(ctx.caps, p.caps);
  const budgets = {
    maxOracleTurns: Math.min(ctx.budgets.maxOracleTurns, p.budgets.maxOracleTurns),
    maxEvalSteps: Math.min(ctx.budgets.maxEvalSteps, p.budgets.maxEvalSteps),
    maxToolCalls: Math.min(ctx.budgets.maxToolCalls, p.budgets.maxToolCalls),
  };

  const constraints = [...ctx.constraints];
  if ((p as any).noNewFacts && !constraints.some(c => c.tag === "NoNewFacts")) constraints.push({ tag: "NoNewFacts" });
  if ((p as any).deterministicEnvelope && !constraints.some(c => c.tag === "DeterministicEnvelope"))
    constraints.push({ tag: "DeterministicEnvelope" });

  return ctxRehash({
    ...ctx,
    profile: p.name,
    caps,
    budgets,
    constraints,
  });
}

function intersectCaps(a: CapSet, b: CapSet): CapSet {
  if (a.includes("*") && b.includes("*")) return ["*"];
  if (a.includes("*")) return b;
  if (b.includes("*")) return a;
  const setB = new Set(b);
  return a.filter(x => setB.has(x));
}

/** Gather all visible keys across lexical chain, with lexical proximity order (nearest first). */
export function ctxAllKeysWithDepth(ctx: Ctx): Array<{ name: string; depth: number }> {
  const seen = new Set<string>();
  const out: Array<{ name: string; depth: number }> = [];
  let depth = 0;
  for (let cur: Ctx | undefined = ctx; cur; cur = cur.parent) {
    for (const name of cur.frame.keys()) {
      if (seen.has(name)) continue;
      seen.add(name);
      out.push({ name, depth });
    }
    depth++;
  }
  return out;
}

/**
 * Default projection: should be safe to show to oracle.
 * This is where compression matters: if ctx.view exists, it controls what gets listed.
 */
export function ctxProject(ctx: Ctx, opts?: { maxKeys?: number }): any {
  const maxKeys = opts?.maxKeys ?? 200;

  const all = ctxAllKeysWithDepth(ctx).map(x => x.name).sort();
  let keys: string[];

  if (ctx.view) {
    // show only visible subset (preserve order from view), plus an ellipsis marker
    keys = ctx.view.visibleKeys.slice();
    if (ctx.view.hiddenCount > 0) keys.push(`… (${ctx.view.hiddenCount} hidden; hydrate=${ctx.view.receiptId})`);
  } else {
    keys = all;
    if (keys.length > maxKeys) keys = keys.slice(0, maxKeys).concat(["…"]);
  }

  return {
    tag: "CtxProjection",
    cid: ctx.cid,
    profile: ctx.profile,
    sealed: ctx.sealed,
    caps: Array.from(ctx.caps).slice().sort(),
    budgets: ctx.budgets,
    constraints: ctx.constraints.map(c => c.tag),
    frameKeys: keys,
    depth: ctxDepth(ctx),
    evidenceCount: ctx.evidence.length,
    view: ctx.view
      ? {
          receiptId: ctx.view.receiptId,
          keyBudgetTokens: ctx.view.keyBudgetTokens,
          evidenceBudgetTokens: ctx.view.evidenceBudgetTokens,
        }
      : null,
  };
}

function ctxDepth(ctx: Ctx): number {
  let d = 0;
  for (let cur: Ctx | undefined = ctx; cur; cur = cur.parent) d++;
  return d;
}
```

This is the semantic hook: **compression is not “delete env”**; it is **establish a bounded observation view** + **receipt for reconstitution**.

---

# 2) Receipt-backed Snapshot / Compress / Hydrate + VOI selection

Now we implement the actual economics, as demanded.

* VOI selection for:

  * visible keys (oracle “cheap view”)
  * evidence to keep inline vs drop into receipt
* Receipt stores:

  * dropped evidence
  * hidden keys list
  * summary + policy
* Hydration merges dropped evidence back and removes the view restriction.

## 2.1 New file: `src/core/ctx/receipts.ts`

```ts
// src/core/ctx/receipts.ts
import type { Hash } from "../artifacts/hash";
import { sha256JSON } from "../artifacts/hash";
import type { Evidence, Ctx } from "./ctx";

export type CompressPolicy = {
  tag: "CompressPolicy";
  keyBudgetTokens: number;         // approximate “LLM context tokens” allocated to binding names
  evidenceBudgetTokens: number;    // same, but for evidence payload
  maxVisibleKeys: number;          // hard cap (even if budget allows)
  maxEvidenceItems: number;        // hard cap
  mode: "keys" | "evidence" | "both";
  keepReceiptRefs: boolean;        // keep existing ReceiptRefs always
};

export function defaultCompressPolicy(partial?: Partial<CompressPolicy>): CompressPolicy {
  return {
    tag: "CompressPolicy",
    keyBudgetTokens: partial?.keyBudgetTokens ?? 400,
    evidenceBudgetTokens: partial?.evidenceBudgetTokens ?? 600,
    maxVisibleKeys: partial?.maxVisibleKeys ?? 150,
    maxEvidenceItems: partial?.maxEvidenceItems ?? 40,
    mode: partial?.mode ?? "both",
    keepReceiptRefs: partial?.keepReceiptRefs ?? true,
  };
}

export type CtxSnapshotReceipt = {
  tag: "Receipt";
  kind: "snapshot";
  rid: Hash;

  createdAtMs: number;

  // receipt subject
  ctxCid: Hash;

  // Useful payload for oracle
  projection: any;
  meta?: any;
};

export type CtxCompressReceipt = {
  tag: "Receipt";
  kind: "compress";
  rid: Hash;

  createdAtMs: number;

  // receipt subject (ctx before compress)
  baseCtxCid: Hash;

  policy: CompressPolicy;

  hiddenKeys: string[];        // keys hidden from view
  droppedEvidence: Evidence[]; // evidence removed from ctx.evidence

  summary: string;
};

export type Receipt = CtxSnapshotReceipt | CtxCompressReceipt;

export class CtxReceiptRepo {
  private m = new Map<Hash, Receipt>();

  putSnapshot(ctx: Ctx, projection: any, meta?: any): CtxSnapshotReceipt {
    // content address
    const rid = sha256JSON({
      kind: "snapshot",
      ctxCid: ctx.cid,
      projection,
      meta: meta ?? null,
    });
    const r: CtxSnapshotReceipt = {
      tag: "Receipt",
      kind: "snapshot",
      rid,
      createdAtMs: Date.now(),
      ctxCid: ctx.cid,
      projection,
      meta,
    };
    this.m.set(rid, r);
    return r;
  }

  putCompress(baseCtx: Ctx, policy: CompressPolicy, hiddenKeys: string[], droppedEvidence: Evidence[], summary: string): CtxCompressReceipt {
    const rid = sha256JSON({
      kind: "compress",
      baseCtxCid: baseCtx.cid,
      policy,
      hiddenKeys,
      droppedEvidence,
      summary,
    });
    const r: CtxCompressReceipt = {
      tag: "Receipt",
      kind: "compress",
      rid,
      createdAtMs: Date.now(),
      baseCtxCid: baseCtx.cid,
      policy,
      hiddenKeys,
      droppedEvidence,
      summary,
    };
    this.m.set(rid, r);
    return r;
  }

  get(rid: Hash): Receipt | undefined {
    return this.m.get(rid);
  }
}
```

---

## 2.2 New file: `src/core/ctx/compress.ts`

This is the actual VOI selector + compression/hydration logic.

```ts
// src/core/ctx/compress.ts
import type { Ctx, Evidence, CtxView } from "./ctx";
import { ctxAllKeysWithDepth, ctxRehash } from "./ctx";
import type { CtxReceiptRepo, CompressPolicy } from "./receipts";
import { defaultCompressPolicy } from "./receipts";

/**
 * Heuristic token estimator.
 * We cannot know real tokenizer costs here; we approximate:
 *  - ~4 chars per token + constant overhead.
 */
function approxTokens(s: string): number {
  return Math.max(1, Math.ceil(s.length / 4));
}

function keyCostTokens(name: string): number {
  // binder overhead
  return 1 + approxTokens(name);
}

function evidenceCostTokens(ev: Evidence): number {
  switch (ev.tag) {
    case "ToolRef": return 4;
    case "ReceiptRef": return 3;
    case "Note": return 2 + approxTokens(ev.text);
    default: return 4;
  }
}

function evidenceUtility(ev: Evidence, ageIndex: number): number {
  // recency bias: last items have ageIndex near 0
  const recency = 1 / (1 + ageIndex * 0.15);

  switch (ev.tag) {
    case "ToolRef": return 4.0 * recency;
    case "ReceiptRef": return 2.5 * recency;
    case "Note": {
      const lenBoost = Math.min(2.0, ev.text.length / 200);
      return (1.0 + lenBoost) * recency;
    }
    default: return 1.0 * recency;
  }
}

function keyUtility(name: string, depth: number): number {
  // lexical proximity is a strong proxy for VOI in SICP’s environment model:
  // nearer frames are more likely to matter in current computation.
  const proximity = 5 / (1 + depth);
  // tiny boost for "public-ish" identifiers (longer names tend to be more descriptive)
  const descriptiveness = Math.min(1.0, name.length / 20);
  return proximity + descriptiveness;
}

type Scored<T> = { item: T; u: number; c: number; score: number };

/**
 * Greedy VOI selector: maximize Σ u under token budget by score=u/c.
 * This is a classic fractional-knapsack heuristic (works well enough for context staging).
 */
function selectGreedy<T>(items: Scored<T>[], budgetTokens: number, maxItems: number): { kept: T[]; dropped: T[] } {
  const xs = items.slice().sort((a, b) => b.score - a.score);
  const kept: T[] = [];
  const dropped: T[] = [];
  let used = 0;

  for (const x of xs) {
    if (kept.length >= maxItems) {
      dropped.push(x.item);
      continue;
    }
    if (used + x.c <= budgetTokens) {
      kept.push(x.item);
      used += x.c;
    } else {
      dropped.push(x.item);
    }
  }
  return { kept, dropped };
}

function uniqAppend<T>(base: T[], extra: T[], eq: (a: T, b: T) => boolean): T[] {
  const out = base.slice();
  for (const x of extra) {
    if (!out.some(y => eq(x, y))) out.push(x);
  }
  return out;
}

function evidenceEq(a: Evidence, b: Evidence): boolean {
  if (a.tag !== b.tag) return false;
  if (a.tag === "Note") return (b as any).text === a.text;
  if (a.tag === "ReceiptRef") return (b as any).rid === (a as any).rid;
  if (a.tag === "ToolRef") return (b as any).callId === (a as any).callId;
  return false;
}

function keyEq(a: string, b: string): boolean {
  return a === b;
}

/**
 * Compress a context:
 *  - compute visible key set under VOI budget
 *  - drop low-VOI evidence into a receipt
 *  - install CtxView so oracle observation is bounded
 *  - append ReceiptRef to evidence for provenance
 */
export function ctxCompress(ctx: Ctx, repo: CtxReceiptRepo, policy0?: Partial<CompressPolicy>): { ctx2: Ctx; receiptId: string } {
  const policy = defaultCompressPolicy(policy0);

  const keysWithDepth = ctxAllKeysWithDepth(ctx);

  // Always expose keys in current frame (depth 0) — they are the “local variables” of the current locus.
  const locked = keysWithDepth.filter(k => k.depth === 0).map(k => k.name);
  const lockedCost = locked.reduce((a, k) => a + keyCostTokens(k), 0);

  const scoredKeys: Scored<string>[] = [];
  for (const kd of keysWithDepth) {
    if (locked.includes(kd.name)) continue;
    const u = keyUtility(kd.name, kd.depth);
    const c = keyCostTokens(kd.name);
    scoredKeys.push({ item: kd.name, u, c, score: u / c });
  }

  const keyBudget = Math.max(0, policy.keyBudgetTokens - lockedCost);
  const { kept: keptExtraKeys, dropped: droppedKeys } = policy.mode === "evidence"
    ? { kept: [] as string[], dropped: scoredKeys.map(x => x.item) }
    : selectGreedy(scoredKeys, keyBudget, Math.max(0, policy.maxVisibleKeys - locked.length));

  const visibleKeys = policy.mode === "evidence"
    ? locked.slice().sort()
    : uniqAppend(locked.slice(), keptExtraKeys, keyEq);

  // Evidence compression
  const evidence = ctx.evidence ?? [];
  const alwaysKeep: Evidence[] = [];
  const candidates: Evidence[] = [];

  for (const ev of evidence) {
    if (policy.keepReceiptRefs && ev.tag === "ReceiptRef") alwaysKeep.push(ev);
    else candidates.push(ev);
  }

  const scoredEvidence: Scored<Evidence>[] = [];
  // ageIndex = 0 for newest (last element), so reverse iterate
  for (let i = 0; i < candidates.length; i++) {
    const ev = candidates[candidates.length - 1 - i];
    const u = evidenceUtility(ev, i);
    const c = evidenceCostTokens(ev);
    scoredEvidence.push({ item: ev, u, c, score: u / c });
  }

  const { kept: keptEv, dropped: droppedEv } = policy.mode === "keys"
    ? { kept: [] as Evidence[], dropped: candidates.slice() }
    : selectGreedy(scoredEvidence, policy.evidenceBudgetTokens, policy.maxEvidenceItems);

  // keptEv is selected from reverse order; restore stable order as in original evidence list
  const keptEvSet = new Set(keptEv.map(ev => JSON.stringify(ev)));
  const keptEvOrdered = candidates.filter(ev => keptEvSet.has(JSON.stringify(ev)));

  const hiddenKeys = keysWithDepth.map(k => k.name).filter(k => !visibleKeys.includes(k));
  const droppedEvidence = droppedEv;

  const summary = `compress ctx=${ctx.cid} hiddenKeys=${hiddenKeys.length} droppedEvidence=${droppedEvidence.length}`;

  const receipt = repo.putCompress(ctx, policy, hiddenKeys, droppedEvidence, summary);

  const view: CtxView = {
    tag: "CtxView",
    visibleKeys: visibleKeys.slice(),
    hiddenCount: hiddenKeys.length,
    receiptId: receipt.rid,
    keyBudgetTokens: policy.keyBudgetTokens,
    evidenceBudgetTokens: policy.evidenceBudgetTokens,
    policy,
  };

  // New evidence list: keep compact refs + chosen evidence + new receipt ref
  const evidence2 = uniqAppend(
    uniqAppend(alwaysKeep, keptEvOrdered, evidenceEq),
    [{ tag: "ReceiptRef", rid: receipt.rid }],
    evidenceEq
  );

  const ctx2 = ctxRehash({ ...ctx, evidence: evidence2, view });

  return { ctx2, receiptId: receipt.rid };
}

/**
 * Hydrate reverses a compress receipt:
 *  - removes view restriction
 *  - merges droppedEvidence back into ctx.evidence
 *
 * NOTE: This is *not* time-travel. It does not revert store/state.
 * It is reconstitution of “hidden context” for the oracle.
 */
export function ctxHydrate(ctx: Ctx, receipt: { rid: string; kind: "compress"; droppedEvidence: Evidence[]; hiddenKeys: string[] }): Ctx {
  // Applicability checks: either view refers to receipt, or evidence contains ReceiptRef.
  const okView = ctx.view?.receiptId === receipt.rid;
  const okEvidence = (ctx.evidence ?? []).some(ev => ev.tag === "ReceiptRef" && ev.rid === receipt.rid);

  if (!okView && !okEvidence) {
    throw new Error(`ctxHydrate: receipt ${receipt.rid} not applicable to ctx ${ctx.cid}`);
  }

  const evidenceMerged = uniqAppend(ctx.evidence ?? [], receipt.droppedEvidence ?? [], evidenceEq);
  const ctx2 = ctxRehash({ ...ctx, evidence: evidenceMerged, view: undefined });
  return ctx2;
}
```

This is the “rocket staging” mechanism in concrete form:

* **compression** = reduced cheap view + receipt reference + evidence trimming
* **hydration** = restore dropped evidence + remove view restriction

---

# 3) Wire protocol ops in the Oracle Portal

You already have `ReqEval/ReqApply/ReqObserve`. Now you implement:

* `ReqSnapshot` → create and store a snapshot receipt (projection)
* `ReqCompress` → compress the current ctx into a new envRef
* `ReqHydrate` → hydrate the current ctx using the receipt and produce a new envRef

To do that correctly, the portal must track an **active envRef** for the session (because `ReqHydrate(receiptId)` doesn’t carry an envRef).

## 3.1 Patch: `src/core/oracle/portalImpl.ts`

This is the exact wiring. Search for your existing `PortalImpl` and apply these deltas.

### Add imports

```ts
import type { EnvRef } from "./protocol";
import { ctxProject } from "../ctx/ctx";
import { CtxReceiptRepo, defaultCompressPolicy } from "../ctx/receipts";
import { ctxCompress, ctxHydrate } from "../ctx/compress";
import { capRequire, capIntersect } from "../governance/caps";
```

### Add fields

```ts
private activeEnvRef?: EnvRef;
private ctxReceipts = new CtxReceiptRepo();
```

### Add a setter (called by runtime when session starts)

```ts
setActiveEnvRef(envRef: EnvRef): void {
  this.activeEnvRef = envRef;
}
```

### Update `perform` to maintain activeEnvRef when returning a new envRef

Right before you `return resp;`, if `resp.tag === "RespVal" && resp.envRef`, set it:

```ts
if ((resp as any).tag === "RespVal" && (resp as any).envRef) {
  this.activeEnvRef = (resp as any).envRef;
}
```

### Implement caps for ctx.* using intersection

Add helper:

```ts
private effectiveCaps(envRef: EnvRef): string[] {
  const snap = this.snapshots.getEnv(envRef);
  const ctxCaps = (snap.env as any).caps ?? [];
  return capIntersect(ctxCaps, this.sessionProfile.caps) as any;
}

private enforce(cap: string, envRef?: EnvRef): void {
  const caps = envRef ? this.effectiveCaps(envRef) : this.sessionProfile.caps;
  capRequire(caps as any, cap, `portal/${cap}`);
}
```

### Implement `ReqObserve` for `{tag:"Ctx"}`

Inside your `ReqObserve` switch:

```ts
case "ReqObserve": {
  this.enforce("observe");
  const st = this.snapshots.getState(req.stateRef).state;
  const what = req.what;

  if (what?.tag === "Ctx") {
    const env = (st as any).env;
    return { tag: "RespObs", data: ctxProject(env, { maxKeys: what.maxKeys ?? 200 }) };
  }

  // existing Stack / Control / Handlers observers...
  return { tag: "RespObs", data: { error: "unknown observe spec", what } };
}
```

### Implement `ReqSnapshot`

```ts
case "ReqSnapshot": {
  this.enforce("ctx.snapshot", req.envRef);
  this.enforce("observe", req.envRef);

  const snap = this.snapshots.getEnv(req.envRef);
  const ctx = snap.env as any;

  const projection = ctxProject(ctx, { maxKeys: req.meta?.maxKeys ?? 200 });
  const receipt = this.ctxReceipts.putSnapshot(ctx, projection, req.meta);

  // NOTE: snapshot does not mutate ctx. It’s a pure Memento emission.
  return { tag: "RespObs", data: receipt };
}
```

### Implement `ReqCompress`

```ts
case "ReqCompress": {
  this.enforce("ctx.compress", req.envRef);
  this.enforce("observe", req.envRef);

  const snap = this.snapshots.getEnv(req.envRef);
  const ctx = snap.env as any;

  // policy is JSON; allow req.meta to partially specify it
  const policy = defaultCompressPolicy(req.meta?.policy ?? req.meta ?? {});

  const { ctx2, receiptId } = ctxCompress(ctx, this.ctxReceipts, policy);

  const envRef2 = this.snapshots.putEnv({ env: ctx2, store: snap.store, handlers: snap.handlers });
  this.activeEnvRef = envRef2;

  return { tag: "RespObs", data: { receiptId, envRef: envRef2, ctxCid: ctx2.cid } };
}
```

### Implement `ReqHydrate`

```ts
case "ReqHydrate": {
  this.enforce("ctx.hydrate");
  this.enforce("observe");

  const rid = req.receiptId;
  const receipt = this.ctxReceipts.get(rid);

  if (!receipt) return { tag: "RespError", message: `missing receipt ${rid}` };

  // Snapshot receipts: hydration returns the receipt payload (projection/meta)
  if (receipt.kind === "snapshot") {
    return { tag: "RespObs", data: receipt };
  }

  // Compress receipts: hydrate into current active env snapshot.
  const targetEnvRef = this.activeEnvRef;
  if (!targetEnvRef) return { tag: "RespError", message: `hydrate has no active envRef` };

  const snap = this.snapshots.getEnv(targetEnvRef);
  const ctx = snap.env as any;

  const ctx2 = ctxHydrate(ctx, receipt);
  const envRef2 = this.snapshots.putEnv({ env: ctx2, store: snap.store, handlers: snap.handlers });
  this.activeEnvRef = envRef2;

  return { tag: "RespObs", data: { envRef: envRef2, ctxCid: ctx2.cid } };
}
```

That completes the Oracle request algebra features for context economics:

* snapshot → receipt
* compress → new envRef + receipt
* hydrate → new envRef restoring visibility/evidence

---

# 4) Update profiles to include ctx.* capabilities

## 4.1 Patch: `src/core/governance/profile.ts`

Add `"ctx.*"` to pragmatic/strict/airgap.

```ts
export const profilePragmatic: Profile = {
  name: "pragmatic",
  caps: ["eval", "apply", "observe", "ctx.*", "tool.*", "commit.*", "train.emit"],
  budgets: { maxOracleTurns: 250, maxEvalSteps: 350_000, maxToolCalls: 100 },
  truth: "test-certified",
  audit: "full",
  noNewFacts: true,
};

export const profileStrict: Profile = {
  name: "strict",
  caps: ["eval", "apply", "observe", "ctx.*", "tool.*", "commit.*", "train.emit"],
  budgets: { maxOracleTurns: 200, maxEvalSteps: 300_000, maxToolCalls: 50 },
  truth: "proof-certified",
  audit: "forensic",
  noNewFacts: true,
  deterministicEnvelope: true,
};

export const profileAirgap: Profile = {
  name: "airgap",
  caps: ["eval", "apply", "observe", "ctx.*"],
  budgets: { maxOracleTurns: 150, maxEvalSteps: 250_000, maxToolCalls: 0 },
  truth: "test-certified",
  audit: "full",
  noNewFacts: true,
  deterministicEnvelope: true,
};
```

---

# 5) Make commit barriers attach evidence to the live context

This is critical: commits must become **semantic promotion events** that are visible in the context’s provenance.

You already have ledger events. Use the **event hash** as a receipt id and push it into `ctx.evidence` as `ReceiptRef`.

## 5.1 Patch: commit handler in `RuntimeImpl.dispatch`

Inside your `commit.op` handling branch:

```ts
import { ctxAddEvidence } from "../ctx/ctx"; // adjust path
// ...
if (op.op === "commit.op") {
  capRequire(this.profile.caps, "commit.*", "commit");

  const kind = op.args[0]?.tag === "Str" ? op.args[0].s : "unknown";
  const payload = op.args[1];

  // reject under speculative regime
  if (this.profile.truth === "speculative") {
    const h = this.ledger.append({ tag: "CommitRejected", kind, payloadDigest: sha256JSON(payload), reason: "speculative", timeMs: Date.now() } as any);
    throw new Error(`commit rejected in speculative regime (event=${h})`);
  }

  // accept
  const evHash = this.ledger.append({ tag: "CommitAccepted", kind, payloadDigest: sha256JSON(payload), timeMs: Date.now() } as any);

  // resume computation
  const st2 = op.resumption.invoke({ tag: "Unit" } as any);

  // attach provenance to context (Env is Ctx)
  const ctx0 = (st as any).env;
  const ctx1 = ctxAddEvidence(ctx0, { tag: "ReceiptRef", rid: evHash });

  (st2 as any).env = ctx1;
  return st2;
}
```

Now commit is not “just an op” — it’s an explicit traceable promotion that composes with context economics.

---

# 6) What this gives you, concretely

After this tranche, you have:

✅ **Req.Snapshot / Req.Compress / Req.Hydrate implemented**
✅ **Receipts exist as first-class artifacts** (addressable by hash rid)
✅ **Compression is VOI-guided** (greedy knapsack by u/c + hard caps)
✅ **Hydration is semantic** (restores dropped evidence + removes view)
✅ **Oracle observation shows compressed view + hydrate pointer**
✅ **Commits leave provenance in ctx.evidence** (ReceiptRef to ledger event hash)

This makes your Section 15 promises materially true:

* SnapshotRepo is still the storage for actual evaluator states (env+store+handlers).
* Receipts are the CQRS read-model / audit artifact for context staging.
* Compression/hydration are no longer stubs — they are a deterministic mechanism.

---

# 7) Next tranche (the one you actually want next)

You now have the *mechanism*. The next tranche is the *policy sophistication* and *integration correctness*:

1. **VOI driven by usage stats** (not just lexical proximity + recency)

   * instrument `envLookup` to increment a (bounded) counter map in ctx.view policy metadata
   * use access frequency as VOI

2. **Receipt invalidation** (dependency changes)

   * attach `deps` to receipts (tool hashes, ledger head)
   * refuse hydrate if deps mismatch in strict profile

3. **Search semantics for infer**

   * plug in beam/MCTS policies; use compress/hydrate to manage oracle prompt budget during search

If you say **CONTINUE** again, I’ll implement **usage-instrumented VOI** (counts + decay) and a proper **receipt applicability/invalidation** rule that is strict-profile compliant (this is where “no new facts” starts to actually bite).
