// src/core/oracle/meaning.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Patch Set A2: Meaning as structured first-class value
// Patch Set 6: Enhanced obligations for term rewriting

import type { Val } from "../eval/values";
import type { DistVal } from "../eval/dist";
import type { Expr } from "../ast";
import type { Evidence } from "../provenance/evidence";
import { evidenceToVal } from "../provenance/evidence";

export type { Evidence, OracleEvidence, TransformEvidence, DerivedEvidence } from "../provenance/evidence";

/**
 * Obligation types for governing program transformations.
 * These must be discharged before a commit can succeed.
 */
export type Obligation =
  | { tag: "OblTests"; tests: Expr[]; envRef: string }
  | { tag: "OblIdempotent"; f: Expr; domain?: Val }              // f(f(x)) = f(x)
  | { tag: "OblNoMatch"; pattern: Expr; scope: "output" | "all" } // pattern must not appear
  | { tag: "OblEqExt"; original: Expr; candidate: Expr; tests: Expr[]; envRef: string }; // extensional equivalence

/**
 * Rewrite trace step for debugging and audit.
 */
export type RewriteStep = {
  rule: string;         // rule name
  before: Val;          // AST before
  after: Val;           // AST after
  position?: string;    // path in AST where rewrite occurred
};

/**
 * Meaning is a VALUE in Omega.
 * We store "residual/rewrite" as Val so you can represent them as Syntax values (tag:"Syntax") or as quoted AST data.
 */
export type MeaningVal = {
  tag: "Meaning";

  // Denotation plane
  denotation?: Val | DistVal;

  // Program plane
  residual?: Val;   // typically {tag:"Syntax", stx: ...} or quoted AST (partially evaluated)
  rewrite?: Val;    // candidate transformed program

  // Analysis plane (optional for now; but the fields EXIST)
  invariants?: Val;
  effects?: Val;
  cost?: Val;
  paths?: Val;
  deps?: Val;
  memo?: Val;

  // Governance plane (Prompt 6 enhanced)
  obligation?: Val;        // legacy single obligation (backward compat)
  obligations?: Obligation[];  // structured obligations array
  evidence?: Evidence[];   // discharge evidence
  confidence?: number;     // 0..1
  trace?: Val | RewriteStep[];  // rewrite trace

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

// Helper to convert old-style protocol Meaning to MeaningVal
function vStr(s: string): Val {
  return { tag: "Str", s };
}
function vNum(n: number): Val {
  return { tag: "Num", n };
}

/**
 * Convert MeaningVal to Map representation for backward compatibility.
 * Prefer using MeaningVal directly as it's now a first-class value.
 */
export function meaningToVal(m: MeaningVal): Val {
  const entries: Array<[Val, Val]> = [];
  entries.push([vStr("tag"), vStr("Meaning")]);

  if (m.denotation) entries.push([vStr("denotation"), m.denotation as Val]);
  if (m.confidence !== undefined) entries.push([vStr("confidence"), vNum(m.confidence)]);
  if (m.adoptEnvRef) entries.push([vStr("adoptEnvRef"), vStr(m.adoptEnvRef)]);
  if (m.residual) entries.push([vStr("residual"), m.residual]);
  if (m.rewrite) entries.push([vStr("rewrite"), m.rewrite]);
  if (m.obligation) entries.push([vStr("obligation"), m.obligation]);
  // Convert Evidence[] to Val (vector of maps) if present
  if (m.evidence) {
    const evidenceItems: Val[] = m.evidence.map(e => evidenceToVal(e));
    entries.push([vStr("evidence"), { tag: "Vector", items: evidenceItems }]);
  }
  if (m.trace) entries.push([vStr("trace"), m.trace as Val]);

  return { tag: "Map", entries };
}
