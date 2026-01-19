// src/core/eval/dist.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Patch Set A1: Dist<Val> as first-class value

import type { Val } from "./values";

/**
 * A minimal discrete distribution (finite support).
 * This is intentionally "first-order" (no sampling effects); policies/handlers decide how to use it.
 */
export type DistItem<T = Val> = { v: T; w: number };

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

export function isDist(v: Val): v is DistVal {
  return typeof v === "object" && v !== null && (v as any).tag === "Dist";
}

// ─────────────────────────────────────────────────────────────────
// Prompt 11: Extended Dist algebra for search
// ─────────────────────────────────────────────────────────────────

/**
 * Filter distribution by predicate.
 * Keeps only items where predicate returns true.
 */
export function distFilter(d: DistVal, p: (v: Val) => boolean): DistVal {
  const filtered = d.support.filter(it => p(it.v));
  return { ...d, support: filtered, normalized: false, meta: { kind: "filter" } };
}

/**
 * Take first n items from distribution (by weight order).
 */
export function distTake(d: DistVal, n: number): DistVal {
  const sorted = d.support.slice().sort((a, b) => b.w - a.w);
  const taken = sorted.slice(0, Math.max(0, n));
  return { ...d, support: taken, normalized: false, meta: { kind: "take" } };
}

/**
 * Get best item from distribution (highest weight).
 */
export function distBest(d: DistVal): Val | undefined {
  if (d.support.length === 0) return undefined;
  let best = d.support[0];
  for (const it of d.support) {
    if (it.w > best.w) best = it;
  }
  return best.v;
}

/**
 * Get best k items from distribution with custom score function.
 */
export function distBestK(d: DistVal, k: number, scoreFn?: (v: Val) => number): DistVal {
  const score = scoreFn ?? ((v: Val) => {
    const it = d.support.find(i => i.v === v);
    return it?.w ?? 0;
  });

  const scored = d.support.map(it => ({ ...it, score: score(it.v) }));
  scored.sort((a, b) => b.score - a.score);

  const support = scored.slice(0, Math.max(0, k)).map(({ v, w }) => ({ v, w }));
  return { ...d, support, normalized: false, meta: { kind: "bestK" } };
}

/**
 * Concatenate two distributions.
 */
export function distConcat(d1: DistVal, d2: DistVal): DistVal {
  return {
    tag: "Dist",
    support: [...d1.support, ...d2.support],
    normalized: false,
    meta: { kind: "concat" },
  };
}

/**
 * Check if distribution is empty.
 */
export function distIsEmpty(d: DistVal): boolean {
  return d.support.length === 0;
}

/**
 * Get the size of the distribution support.
 */
export function distSize(d: DistVal): number {
  return d.support.length;
}

/**
 * Get all values from distribution (ignoring weights).
 */
export function distValues(d: DistVal): Val[] {
  return d.support.map(it => it.v);
}

/**
 * Create uniform distribution from array of values.
 */
export function distUniform(values: Val[]): DistVal {
  const w = values.length > 0 ? 1 / values.length : 0;
  return {
    tag: "Dist",
    support: values.map(v => ({ v, w })),
    normalized: true,
    meta: { kind: "uniform" },
  };
}

/**
 * Create weighted distribution from value-weight pairs.
 */
export function distWeighted(pairs: Array<[Val, number]>): DistVal {
  return {
    tag: "Dist",
    support: pairs.map(([v, w]) => ({ v, w })),
    normalized: false,
    meta: { kind: "weighted" },
  };
}
