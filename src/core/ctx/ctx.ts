// src/core/ctx/ctx.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-17.md
// First-class Context: cid, caps, constraints, sealed, evidence, budgets

import type { Hash } from "../artifacts/hash";
import { sha256JSON } from "../artifacts/hash";
import type { CapSet } from "../governance/caps";
import type { BudgetLimits } from "../governance/budgets";
import type { Profile } from "../governance/profile";
import type { StoreAddr } from "../eval/store";

// Use StoreAddr as our Addr type
export type Addr = StoreAddr;

/**
 * Constraints are runtime-enforceable semantic invariants (Design by Contract at the context boundary).
 */
export type Constraint =
  | { tag: "NoNewFacts" }
  | { tag: "DeterministicEnvelope" }
  | { tag: "Sealed" };

/**
 * ProjectionSchema: Controls what parts of context can be observed by the oracle.
 *
 * This is the "privacy firewall" between code execution and oracle observation.
 * Different profiles can use different schemas to restrict what the oracle sees.
 *
 * Prompt 10: ReqObserve uses this to determine what's visible.
 */
export type ProjectionSchema = {
  /** Maximum number of frame keys to include (default: 200) */
  maxKeys?: number;
  /** Include cid (content address)? */
  includeCid?: boolean;
  /** Include parent chain depth? */
  includeDepth?: boolean;
  /** Include capability set? (may be redacted for security) */
  includeCaps?: boolean;
  /** Include budget information? */
  includeBudgets?: boolean;
  /** Include constraint tags? */
  includeConstraints?: boolean;
  /** Include evidence count? */
  includeEvidenceCount?: boolean;
  /** Include sealed status? */
  includeSealed?: boolean;
  /** Redact sensitive keys (regex patterns) */
  redactKeys?: string[];
  /** Maximum total size in bytes (for compression estimation) */
  maxSizeBytes?: number;
};

/**
 * Default projection schema - permissive for debugging.
 */
export const DEFAULT_PROJECTION_SCHEMA: ProjectionSchema = {
  maxKeys: 200,
  includeCid: true,
  includeDepth: true,
  includeCaps: true,
  includeBudgets: true,
  includeConstraints: true,
  includeEvidenceCount: true,
  includeSealed: true,
};

/**
 * Restricted projection schema - for airgap/strict profiles.
 */
export const RESTRICTED_PROJECTION_SCHEMA: ProjectionSchema = {
  maxKeys: 50,
  includeCid: true,
  includeDepth: true,
  includeCaps: false,  // Caps redacted for security
  includeBudgets: false,
  includeConstraints: false,
  includeEvidenceCount: false,
  includeSealed: true,
  redactKeys: ["secret", "token", "password", "key", "auth"],
};

/**
 * Evidence is append-only metadata used by truth regimes, receipts, and provenance.
 * Keep it JSON-safe.
 */
export type Evidence =
  | { tag: "Note"; text: string }
  | { tag: "ReceiptRef"; rid: Hash }
  | { tag: "ToolRef"; callId: Hash };

/**
 * Ctx is the lexical environment + governance capsule.
 * This is the object that makes inference "first class" rather than "an API call".
 */
export type Ctx = {
  tag: "Ctx";

  // Content address of this ctx node (parent+frame+governance metadata)
  cid: Hash;

  // Lexical scoping
  parent?: Ctx;
  frame: Map<string, Addr>;

  // Governance surface
  profile: string;
  caps: CapSet;
  budgets: BudgetLimits;
  constraints: Constraint[];
  sealed: boolean;

  // Provenance
  evidence: Evidence[];
};

/** Stable JSON projection for hashing. */
function ctxStableJSON(ctx: Omit<Ctx, "cid">): unknown {
  const frame = Array.from(ctx.frame.entries()).sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0));
  const caps = Array.from(ctx.caps).slice().sort();
  const constraints = ctx.constraints.map(c => c.tag);

  return {
    parent: ctx.parent ? ctx.parent.cid : null,
    frame,
    profile: ctx.profile,
    caps,
    budgets: ctx.budgets,
    constraints,
    sealed: ctx.sealed,
    evidence: ctx.evidence,
  };
}

export function ctxRehash(ctx: Omit<Ctx, "cid">): Ctx {
  const cid = sha256JSON(ctxStableJSON(ctx));
  return { ...ctx, cid } as Ctx;
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

/**
 * Define/bind into the *current* frame (not a new child frame).
 * This is what Scheme's internal definition mechanics want.
 */
export function ctxDefine(ctx: Ctx, name: string, addr: Addr): Ctx {
  if (ctx.sealed) throw new Error(`ctxDefine denied: context is sealed (name=${name})`);
  const frame = new Map(ctx.frame);
  frame.set(name, addr);
  return ctxRehash({ ...ctx, frame });
}

/**
 * Extend: create a new child frame (lexical block / call frame).
 */
export function ctxExtend(ctx: Ctx, binds: Array<[string, Addr]>): Ctx {
  const frame = new Map<string, Addr>();
  for (const [k, a] of binds) frame.set(k, a);

  // child inherits governance by default (you can later implement "cap attenuation" here).
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
  });
}

/**
 * Seal: freeze the context so operations that mutate bindings/state can be forbidden.
 * We enforce seal on ctxDefine and on set! at eval time.
 */
export function ctxSeal(ctx: Ctx): Ctx {
  if (ctx.sealed) return ctx;
  const constraints = ctx.constraints.some(c => c.tag === "Sealed")
    ? ctx.constraints
    : [...ctx.constraints, { tag: "Sealed" } as Constraint];

  return ctxRehash({ ...ctx, sealed: true, constraints });
}

export function ctxAddEvidence(ctx: Ctx, ev: Evidence): Ctx {
  return ctxRehash({ ...ctx, evidence: [...ctx.evidence, ev] });
}

// ─────────────────────────────────────────────────────────────────
// Capability Attenuation (Prompt 10)
// ─────────────────────────────────────────────────────────────────

/**
 * Attenuate capabilities: create a child context with restricted caps.
 * Caps can only be restricted, never expanded (monotonic attenuation).
 *
 * This is the core security primitive for sandboxing.
 */
export function ctxAttenuateCaps(ctx: Ctx, newCaps: CapSet): Ctx {
  // Intersection: newCaps must be subset of ctx.caps
  const attenuated = intersectCaps(ctx.caps, newCaps);
  return ctxRehash({ ...ctx, caps: attenuated });
}

/**
 * Create a child context with attenuated capabilities and bindings.
 * This is the secure version of ctxExtend for cross-trust-boundary calls.
 */
export function ctxExtendAttenuated(
  ctx: Ctx,
  binds: Array<[string, Addr]>,
  newCaps: CapSet
): Ctx {
  const frame = new Map<string, Addr>();
  for (const [k, a] of binds) frame.set(k, a);

  // Attenuate caps (can only restrict)
  const attenuated = intersectCaps(ctx.caps, newCaps);

  return ctxRehash({
    tag: "Ctx",
    parent: ctx,
    frame,
    profile: ctx.profile,
    caps: attenuated,
    budgets: ctx.budgets,
    constraints: ctx.constraints,
    sealed: ctx.sealed,
    evidence: ctx.evidence,
  });
}

/**
 * Check if a capability is held by the context.
 */
export function ctxHasCap(ctx: Ctx, cap: string): boolean {
  if (ctx.caps.includes("*")) return true;
  if (ctx.caps.includes(cap)) return true;
  const domain = cap.split(".")[0];
  return ctx.caps.includes(`${domain}.*`);
}

/**
 * Require a capability, throwing if not present.
 */
export function ctxRequireCap(ctx: Ctx, cap: string, context: string): void {
  if (!ctxHasCap(ctx, cap)) {
    throw new Error(`capability denied: ${cap} (context: ${context})`);
  }
}

// ─────────────────────────────────────────────────────────────────
// Module Sealing (Prompt 10)
// ─────────────────────────────────────────────────────────────────

/**
 * Create a sealed module from a context.
 * The module captures the context at seal-time with specified exports.
 *
 * Returns: { moduleId, sealedCtx, exports }
 */
export function ctxSealAsModule(
  ctx: Ctx,
  exportNames: string[],
  meta?: { name?: string; version?: string; description?: string }
): { moduleId: Hash; sealedCtx: Ctx; exports: Set<string>; meta?: typeof meta } {
  // Seal the context
  const sealed = ctxSeal(ctx);

  // Compute module ID from cid + exports
  const moduleId = sha256JSON({
    cid: sealed.cid,
    exports: exportNames.slice().sort(),
    meta,
  });

  return {
    moduleId,
    sealedCtx: sealed,
    exports: new Set(exportNames),
    meta,
  };
}

/**
 * Import a binding from a sealed module into current context.
 * This is the only way to access module internals.
 */
export function ctxImportFromModule(
  ctx: Ctx,
  sealedCtx: Ctx,
  exportedNames: Set<string>,
  name: string
): Addr | undefined {
  if (!exportedNames.has(name)) {
    throw new Error(`module import denied: '${name}' is not exported`);
  }
  return ctxLookup(sealedCtx, name);
}

/**
 * Apply a profile as a restriction (caps intersect, budgets min).
 * This is a Policy Object application (GoF Strategy/Policy).
 */
export function ctxApplyProfile(ctx: Ctx, p: Profile): Ctx {
  const caps = intersectCaps(ctx.caps, p.caps);
  const budgets: BudgetLimits = {
    maxOracleTurns: Math.min(ctx.budgets.maxOracleTurns, p.budgets.maxOracleTurns),
    maxEvalSteps: Math.min(ctx.budgets.maxEvalSteps, p.budgets.maxEvalSteps),
    maxToolCalls: Math.min(ctx.budgets.maxToolCalls, p.budgets.maxToolCalls),
  };

  // constraints: merge
  const constraints = [...ctx.constraints];
  if ((p as any).noNewFacts && !constraints.some(c => c.tag === "NoNewFacts"))
    constraints.push({ tag: "NoNewFacts" });
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
  // conservative intersection with wildcard handling
  if (a.includes("*") && b.includes("*")) return ["*"];
  if (a.includes("*")) return b;
  if (b.includes("*")) return a;
  const setB = new Set(b);
  return a.filter(x => setB.has(x));
}

/**
 * Projection for oracle observation/debugging — avoid exfiltrating the entire store.
 * Now uses ProjectionSchema for fine-grained control (Prompt 10).
 */
export function ctxProject(ctx: Ctx, schema: ProjectionSchema = DEFAULT_PROJECTION_SCHEMA): unknown {
  const maxKeys = schema.maxKeys ?? 200;
  let keys = Array.from(ctx.frame.keys()).slice().sort();

  // Apply redaction patterns if specified
  if (schema.redactKeys && schema.redactKeys.length > 0) {
    const patterns = schema.redactKeys.map(p => new RegExp(p, "i"));
    keys = keys.filter(k => !patterns.some(p => p.test(k)));
  }

  const clipped = keys.length > maxKeys ? keys.slice(0, maxKeys).concat(["…"]) : keys;

  const projection: Record<string, unknown> = {
    tag: "CtxProjection",
    profile: ctx.profile,
  };

  if (schema.includeCid !== false) projection.cid = ctx.cid;
  if (schema.includeSealed !== false) projection.sealed = ctx.sealed;
  if (schema.includeCaps !== false) projection.caps = Array.from(ctx.caps).slice().sort();
  if (schema.includeBudgets !== false) projection.budgets = ctx.budgets;
  if (schema.includeConstraints !== false) projection.constraints = ctx.constraints.map(c => c.tag);
  if (schema.includeDepth !== false) projection.depth = ctxDepth(ctx);
  if (schema.includeEvidenceCount !== false) projection.evidenceCount = ctx.evidence.length;

  projection.frameKeys = clipped;

  return projection;
}

/**
 * Estimate the byte size of a context projection.
 * Used for compression decisions and budget tracking.
 */
export function ctxEstimateSize(ctx: Ctx): number {
  // Rough estimation: count bindings and parent chain
  let size = 0;
  for (let cur: Ctx | undefined = ctx; cur; cur = cur.parent) {
    size += cur.frame.size * 100; // ~100 bytes per binding estimate
    size += 200; // overhead per frame
  }
  size += ctx.evidence.length * 50; // ~50 bytes per evidence entry
  return size;
}

function ctxDepth(ctx: Ctx): number {
  let d = 0;
  for (let cur: Ctx | undefined = ctx; cur; cur = cur.parent) d++;
  return d;
}

/**
 * Check if ctx is a Ctx (vs old-style Map)
 */
export function isCtx(x: unknown): x is Ctx {
  return typeof x === "object" && x !== null && (x as any).tag === "Ctx";
}

// ─────────────────────────────────────────────────────────────────
// Compression & Hydration (Prompt 10)
// ─────────────────────────────────────────────────────────────────

/**
 * Compressed context representation for oracle transmission.
 * This is what gets sent over the wire instead of full Ctx.
 */
export type CompressedCtx = {
  tag: "CompressedCtx";
  /** Content address of original context */
  cid: Hash;
  /** Profile name */
  profile: string;
  /** Sealed status */
  sealed: boolean;
  /** Summary of capabilities (not full set, for privacy) */
  capSummary: "full" | "restricted" | "none";
  /** Number of bindings in current frame */
  frameSize: number;
  /** Depth of parent chain */
  depth: number;
  /** Estimated size in bytes */
  estimatedBytes: number;
  /** Evidence count */
  evidenceCount: number;
  /** Constraint tags */
  constraints: string[];
};

/**
 * Compress a context for oracle transmission.
 * Returns a lightweight representation that can be hydrated later.
 */
export function ctxCompress(ctx: Ctx): CompressedCtx {
  const capSummary: "full" | "restricted" | "none" =
    ctx.caps.includes("*") ? "full" :
    ctx.caps.length > 0 ? "restricted" : "none";

  return {
    tag: "CompressedCtx",
    cid: ctx.cid,
    profile: ctx.profile,
    sealed: ctx.sealed,
    capSummary,
    frameSize: ctx.frame.size,
    depth: ctxDepth(ctx),
    estimatedBytes: ctxEstimateSize(ctx),
    evidenceCount: ctx.evidence.length,
    constraints: ctx.constraints.map(c => c.tag),
  };
}

/**
 * Context store for hydration.
 * Maps cid -> Ctx for rehydrating compressed contexts.
 */
export class CtxStore {
  private store = new Map<Hash, Ctx>();

  /** Store a context, returning its cid. */
  put(ctx: Ctx): Hash {
    this.store.set(ctx.cid, ctx);
    return ctx.cid;
  }

  /** Retrieve a context by cid. */
  get(cid: Hash): Ctx | undefined {
    return this.store.get(cid);
  }

  /** Check if a context is stored. */
  has(cid: Hash): boolean {
    return this.store.has(cid);
  }

  /** Clear the store. */
  clear(): void {
    this.store.clear();
  }

  /** Get store size. */
  size(): number {
    return this.store.size;
  }
}

/** Global context store for hydration. */
export const globalCtxStore = new CtxStore();

/**
 * Hydrate a compressed context back to full Ctx.
 * Requires the original context to be in the store.
 *
 * @throws Error if context not found in store
 */
export function ctxHydrate(compressed: CompressedCtx, store: CtxStore = globalCtxStore): Ctx {
  const ctx = store.get(compressed.cid);
  if (!ctx) {
    throw new Error(`ctxHydrate failed: context ${compressed.cid} not found in store`);
  }
  return ctx;
}

/**
 * Hydrate by cid directly.
 */
export function ctxHydrateById(cid: Hash, store: CtxStore = globalCtxStore): Ctx {
  const ctx = store.get(cid);
  if (!ctx) {
    throw new Error(`ctxHydrate failed: context ${cid} not found in store`);
  }
  return ctx;
}
