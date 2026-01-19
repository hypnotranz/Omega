// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-11.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import type { Scope, SIdent } from "./syntax";

export type BindingKind = "value" | "syntax";

export type Binding = {
  bid: string;
  name: string;
  scopes: Scope[];
  phase: number;
  kind: BindingKind;
  value: unknown;
};

export type Env = Binding[];

function subset(a: Scope[], b: Scope[]): boolean {
  for (const sc of a) if (!b.includes(sc)) return false;
  return true;
}

/**
 * Backwards-compatible resolver:
 *   - old: resolveIdent(id, env, phase)
 *   - new: resolveIdent(id, env, phase, kind)
 */
export function resolveIdent(id: SIdent, env: Env, phase: number, kind: BindingKind = "value"): Binding | null {
  const candidates = env.filter(b =>
    b.phase === phase &&
    b.kind === kind &&
    b.name === id.name &&
    subset(b.scopes, id.scopes)
  );

  if (candidates.length === 0) return null;
  candidates.sort((a, b) => b.scopes.length - a.scopes.length);
  const best = candidates[0];
  const second = candidates[1];
  if (second && second.scopes.length === best.scopes.length) {
    throw new Error(`resolveIdent ambiguity: ${id.name} at phase ${phase} kind ${kind}`);
  }
  return best;
}

/**
 * Backwards-compatible free-identifier=? used by syntax-rules literals.
 * Signature kept as: (idDef, envDef, phase, idUse, envUse)
 */
export function freeIdentifierEq(
  id1: SIdent, env1: Env, phase: number,
  id2: SIdent, env2: Env,
  kind: BindingKind = "value"
): boolean {
  const b1 = resolveIdent(id1, env1, phase, kind);
  const b2 = resolveIdent(id2, env2, phase, kind);
  if (b1 && b2) return b1.bid === b2.bid;
  if (!b1 && !b2) return id1.name === id2.name;
  return false;
}