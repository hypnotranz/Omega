// src/core/macro/hygiene.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Prompt 15: Set-of-scopes hygiene implementation

import type { Syntax, Scope, SIdent } from "../syntax/syntax";
import { isIdent, isList, addScope, freshScope } from "../syntax/syntax";
import type { Env, Binding, BindingKind } from "../syntax/binding";
import { resolveIdent } from "../syntax/binding";
import type { ScopeId, MacroEnv, MacroBinding, SourceLocation } from "./types";

// ─────────────────────────────────────────────────────────────────
// Scope Management
// ─────────────────────────────────────────────────────────────────

let globalScopeCounter = { n: 0 };

/**
 * Generate a fresh scope ID.
 */
export function freshScopeId(counter?: { n: number }): ScopeId {
  const c = counter ?? globalScopeCounter;
  c.n += 1;
  return `scope#${c.n}`;
}

/**
 * Reset the global scope counter (for testing).
 */
export function resetScopeCounter(): void {
  globalScopeCounter.n = 0;
}

/**
 * Generate a definition scope for a macro.
 */
export function makeDefScope(macroName: string, counter?: { n: number }): ScopeId {
  const c = counter ?? globalScopeCounter;
  c.n += 1;
  return `def#${macroName}#${c.n}`;
}

/**
 * Generate a use scope for a macro invocation.
 */
export function makeUseScope(macroName: string, counter?: { n: number }): ScopeId {
  const c = counter ?? globalScopeCounter;
  c.n += 1;
  return `use#${macroName}#${c.n}`;
}

/**
 * Generate a binding scope for introduced bindings.
 */
export function makeBindScope(binderName: string, counter?: { n: number }): ScopeId {
  const c = counter ?? globalScopeCounter;
  c.n += 1;
  return `bind#${binderName}#${c.n}`;
}

/**
 * Generate an introducer scope for macro-introduced identifiers.
 */
export function makeIntroducerScope(macroName: string, counter?: { n: number }): ScopeId {
  const c = counter ?? globalScopeCounter;
  c.n += 1;
  return `intro#${macroName}#${c.n}`;
}

// ─────────────────────────────────────────────────────────────────
// Scope Set Operations
// ─────────────────────────────────────────────────────────────────

/**
 * Check if scope set A is a subset of scope set B.
 */
export function scopeSubset(a: Scope[], b: Scope[]): boolean {
  const bSet = new Set(b);
  for (const s of a) {
    if (!bSet.has(s)) return false;
  }
  return true;
}

/**
 * Compute intersection of two scope sets.
 */
export function scopeIntersection(a: Scope[], b: Scope[]): Scope[] {
  const bSet = new Set(b);
  return a.filter(s => bSet.has(s));
}

/**
 * Compute union of two scope sets.
 */
export function scopeUnion(a: Scope[], b: Scope[]): Scope[] {
  const result = new Set(a);
  for (const s of b) result.add(s);
  return Array.from(result);
}

/**
 * Compute difference of two scope sets (A - B).
 */
export function scopeDifference(a: Scope[], b: Scope[]): Scope[] {
  const bSet = new Set(b);
  return a.filter(s => !bSet.has(s));
}

/**
 * Check if two scope sets are equal.
 */
export function scopeEqual(a: Scope[], b: Scope[]): boolean {
  if (a.length !== b.length) return false;
  const aSet = new Set(a);
  for (const s of b) {
    if (!aSet.has(s)) return false;
  }
  return true;
}

// ─────────────────────────────────────────────────────────────────
// Syntax Manipulation
// ─────────────────────────────────────────────────────────────────

/**
 * Add a scope to all identifiers in a syntax tree.
 */
export function addScopeToSyntax(stx: Syntax, scope: Scope): Syntax {
  return addScope(stx, scope);
}

/**
 * Remove a scope from all identifiers in a syntax tree.
 */
export function removeScopeFromSyntax(stx: Syntax, scope: Scope): Syntax {
  switch (stx.tag) {
    case "Atom":
      return { ...stx, scopes: stx.scopes.filter(s => s !== scope) };
    case "Ident":
      return { ...stx, scopes: stx.scopes.filter(s => s !== scope) };
    case "List":
      return {
        ...stx,
        scopes: stx.scopes.filter(s => s !== scope),
        items: stx.items.map(it => removeScopeFromSyntax(it, scope)),
      };
  }
}

/**
 * Flip a scope on all identifiers (add if missing, remove if present).
 */
export function flipScopeOnSyntax(stx: Syntax, scope: Scope): Syntax {
  switch (stx.tag) {
    case "Atom": {
      const scopes = stx.scopes.includes(scope)
        ? stx.scopes.filter(s => s !== scope)
        : stx.scopes.concat([scope]);
      return { ...stx, scopes };
    }
    case "Ident": {
      const scopes = stx.scopes.includes(scope)
        ? stx.scopes.filter(s => s !== scope)
        : stx.scopes.concat([scope]);
      return { ...stx, scopes };
    }
    case "List": {
      const scopes = stx.scopes.includes(scope)
        ? stx.scopes.filter(s => s !== scope)
        : stx.scopes.concat([scope]);
      return {
        ...stx,
        scopes,
        items: stx.items.map(it => flipScopeOnSyntax(it, scope)),
      };
    }
  }
}

/**
 * Get the identifier name from a syntax object.
 */
export function syntaxIdentName(stx: Syntax): string | null {
  if (stx.tag === "Ident") return stx.name;
  return null;
}

/**
 * Get the scopes from a syntax object.
 */
export function syntaxScopes(stx: Syntax): Scope[] {
  return stx.scopes;
}

/**
 * Create an identifier syntax with given name and scopes.
 */
export function makeIdent(name: string, scopes: Scope[]): SIdent {
  return { tag: "Ident", name, scopes };
}

/**
 * Create an atom syntax with given value and scopes.
 */
export function makeAtom(value: any, scopes: Scope[]): Syntax {
  return { tag: "Atom", value, scopes };
}

/**
 * Create a list syntax with given items and scopes.
 */
export function makeList(items: Syntax[], scopes: Scope[]): Syntax {
  return { tag: "List", items, scopes };
}

// ─────────────────────────────────────────────────────────────────
// Identifier Resolution
// ─────────────────────────────────────────────────────────────────

/**
 * Resolve an identifier in both value and syntax (macro) environments.
 */
export function resolveIdentFull(
  id: SIdent,
  runtimeEnv: Env,
  macroEnv: MacroEnv,
  phase: number
): { kind: "value"; binding: Binding } | { kind: "syntax"; binding: MacroBinding } | null {
  // First, check macro environment at phase 1
  if (phase === 1) {
    const macroBinding = resolveMacroIdent(id, macroEnv, phase);
    if (macroBinding) {
      return { kind: "syntax", binding: macroBinding };
    }
  }

  // Then check runtime environment
  const runtimeBinding = resolveIdent(id, runtimeEnv, phase);
  if (runtimeBinding) {
    return { kind: "value", binding: runtimeBinding };
  }

  // Check macro environment at any phase for syntax bindings
  const macroBinding = resolveMacroIdent(id, macroEnv, phase);
  if (macroBinding) {
    return { kind: "syntax", binding: macroBinding };
  }

  return null;
}

/**
 * Resolve an identifier in the macro environment.
 */
export function resolveMacroIdent(
  id: SIdent,
  macroEnv: MacroEnv,
  phase: number
): MacroBinding | null {
  const candidates: MacroBinding[] = [];

  for (const [_, binding] of macroEnv) {
    if (
      binding.name === id.name &&
      binding.phase === phase &&
      scopeSubset(binding.scopes, id.scopes)
    ) {
      candidates.push(binding);
    }
  }

  if (candidates.length === 0) return null;

  // Sort by scope set size (most specific first)
  candidates.sort((a, b) => b.scopes.length - a.scopes.length);

  const best = candidates[0];
  const second = candidates[1];

  // Check for ambiguity
  if (second && second.scopes.length === best.scopes.length) {
    throw new Error(`Macro identifier ambiguity: ${id.name} at phase ${phase}`);
  }

  return best;
}

/**
 * Check if two identifiers are free-identifier=? (same binding).
 */
export function freeIdentifierEqual(
  id1: SIdent,
  env1: Env,
  id2: SIdent,
  env2: Env,
  phase: number
): boolean {
  const b1 = resolveIdent(id1, env1, phase);
  const b2 = resolveIdent(id2, env2, phase);

  if (b1 && b2) return b1.bid === b2.bid;
  if (!b1 && !b2) return id1.name === id2.name;
  return false;
}

/**
 * Check if two identifiers are bound-identifier=? (same name and scopes).
 */
export function boundIdentifierEqual(id1: SIdent, id2: SIdent): boolean {
  return id1.name === id2.name && scopeEqual(id1.scopes, id2.scopes);
}

// ─────────────────────────────────────────────────────────────────
// Syntax Conversion
// ─────────────────────────────────────────────────────────────────

/**
 * Convert syntax to datum (strip scopes).
 */
export function syntaxToDatum(stx: Syntax): any {
  switch (stx.tag) {
    case "Atom":
      return stx.value;
    case "Ident":
      return Symbol.for(stx.name);
    case "List":
      return stx.items.map(syntaxToDatum);
  }
}

/**
 * Convert datum to syntax with context from another syntax object.
 */
export function datumToSyntax(ctx: Syntax | null, datum: any, srcloc?: SourceLocation): Syntax {
  const scopes = ctx?.scopes ?? [];

  if (datum === null || datum === undefined) {
    return { tag: "Atom", value: datum, scopes };
  }

  if (typeof datum === "symbol") {
    return { tag: "Ident", name: datum.description ?? String(datum), scopes };
  }

  if (Array.isArray(datum)) {
    return {
      tag: "List",
      items: datum.map(d => datumToSyntax(ctx, d, srcloc)),
      scopes,
    };
  }

  if (typeof datum === "string" && datum.startsWith(":")) {
    // Keyword or identifier starting with :
    return { tag: "Ident", name: datum, scopes };
  }

  // Primitives
  return { tag: "Atom", value: datum, scopes };
}

// ─────────────────────────────────────────────────────────────────
// Hygiene Helpers for Macro Expansion
// ─────────────────────────────────────────────────────────────────

/**
 * Apply hygiene transformation to macro output.
 *
 * This implements the set-of-scopes model:
 * 1. Add useScope to the macro input
 * 2. Apply the macro transformer
 * 3. Add introScope to macro-introduced identifiers
 * 4. Flip useScope on the result (removes from substituted, keeps on introduced)
 */
export function applyHygiene(
  macroName: string,
  input: Syntax,
  output: Syntax,
  useScope: Scope,
  introScope: Scope,
  substitutedVars: Set<string>
): Syntax {
  // The useScope was added to input before transformation.
  // Now flip it on the output to:
  // - Remove from identifiers that came from input (pattern variables)
  // - Add to identifiers introduced by the macro
  const result = flipScopeOnSyntax(output, useScope);

  // Add introScope to truly introduced identifiers
  return addIntroducerToIntroduced(result, introScope, substitutedVars);
}

/**
 * Add introducer scope only to identifiers that weren't substituted.
 */
function addIntroducerToIntroduced(
  stx: Syntax,
  introScope: Scope,
  substitutedVars: Set<string>
): Syntax {
  switch (stx.tag) {
    case "Atom":
      return stx;
    case "Ident":
      // If this identifier was substituted from input, don't add intro scope
      if (substitutedVars.has(stx.name)) {
        return stx;
      }
      return addScope(stx, introScope);
    case "List":
      return {
        ...stx,
        items: stx.items.map(it => addIntroducerToIntroduced(it, introScope, substitutedVars)),
      };
  }
}

/**
 * Track which identifiers in a template are pattern variables.
 */
export function collectPatternVariables(pattern: Syntax): Set<string> {
  const vars = new Set<string>();
  collectPatternVarsRec(pattern, vars);
  return vars;
}

function collectPatternVarsRec(pat: Syntax, vars: Set<string>): void {
  switch (pat.tag) {
    case "Atom":
      return;
    case "Ident":
      // Skip wildcard and ellipsis
      if (pat.name !== "_" && pat.name !== "...") {
        vars.add(pat.name);
      }
      return;
    case "List":
      for (const item of pat.items) {
        collectPatternVarsRec(item, vars);
      }
      return;
  }
}

// ─────────────────────────────────────────────────────────────────
// Alpha Normalization (for testing equality)
// ─────────────────────────────────────────────────────────────────

/**
 * Alpha-normalize a syntax tree for comparison.
 *
 * Replaces all generated scope names with canonical names.
 */
export function alphaNormalize(stx: Syntax): Syntax {
  const scopeMap = new Map<string, string>();
  let counter = 0;

  function normalizeScope(scope: Scope): Scope {
    if (!scopeMap.has(scope)) {
      scopeMap.set(scope, `α${counter++}`);
    }
    return scopeMap.get(scope)!;
  }

  function normalize(s: Syntax): Syntax {
    const normalizedScopes = s.scopes.map(normalizeScope);
    switch (s.tag) {
      case "Atom":
        return { ...s, scopes: normalizedScopes };
      case "Ident":
        return { ...s, scopes: normalizedScopes };
      case "List":
        return {
          ...s,
          scopes: normalizedScopes,
          items: s.items.map(normalize),
        };
    }
  }

  return normalize(stx);
}

/**
 * Check if two syntax trees are alpha-equivalent.
 */
export function alphaEquivalent(stx1: Syntax, stx2: Syntax): boolean {
  const norm1 = alphaNormalize(stx1);
  const norm2 = alphaNormalize(stx2);
  return JSON.stringify(norm1) === JSON.stringify(norm2);
}
