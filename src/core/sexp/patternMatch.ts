// src/core/sexp/patternMatch.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-19.md
// Pattern matcher with _, ?x, and ... ellipsis support (backtracking)

import type { Sexp } from "./sexp";
import { sexpEq, sexpToString } from "./sexp";

export type Bindings = Map<string, Sexp>;

export type MatchResult =
  | { ok: true; bindings: Bindings }
  | { ok: false; reason: string };

export function matchSexp(pattern: Sexp, value: Sexp): MatchResult {
  const b = new Map<string, Sexp>();
  const r = match(pattern, value, b, /*inEllipsis*/ false);
  if (!r.ok) return r;
  return { ok: true, bindings: r.bindings };
}

function match(p: Sexp, v: Sexp, b: Bindings, inEllipsis: boolean): MatchResult {
  // Wildcard _
  if (p.tag === "Sym" && p.name === "_") return { ok: true, bindings: b };

  // Pattern var ?x
  if (p.tag === "Sym" && p.name.startsWith("?") && p.name.length > 1) {
    return bindVar(p.name.slice(1), v, b, inEllipsis);
  }

  // Lists: handle ellipsis
  if (p.tag === "List") {
    if (v.tag !== "List") return fail(`expected list, got ${v.tag}`);
    return matchListWithContext(p.items, v.items, b, inEllipsis);
  }

  // Atoms: structural equality
  if (!sexpEq(p, v)) return fail(`literal mismatch: ${sexpToString(p)} != ${sexpToString(v)}`);
  return { ok: true, bindings: b };
}

function matchList(pats: Sexp[], vals: Sexp[], b: Bindings): MatchResult {
  return matchListWithContext(pats, vals, b, false);
}

function matchListWithContext(pats: Sexp[], vals: Sexp[], b: Bindings, parentInEllipsis: boolean): MatchResult {
  // classic recursive descent with ellipsis: (p ... rest)
  function go(pi: number, vi: number, b0: Bindings): MatchResult {
    // done
    if (pi === pats.length && vi === vals.length) return { ok: true, bindings: b0 };
    if (pi === pats.length) return fail("pattern ended early");

    // ellipsis form: pats[pi], pats[pi+1] == Sym("...")
    const p = pats[pi]!;
    const p2 = pats[pi + 1];
    if (p2 && p2.tag === "Sym" && p2.name === "...") {
      const rest = pats.slice(pi + 2);

      // Extract all pattern variables from p to initialize them
      const varsInPattern = extractPatternVars(p);

      // try k repetitions, backtracking
      for (let k = 0; k <= (vals.length - vi); k++) {
        const bTry = cloneBindings(b0);

        // Initialize all pattern vars to empty lists if k=0
        // (they will be accumulated during matching if k>0)
        for (const varName of varsInPattern) {
          if (!bTry.has(varName)) {
            bTry.set(varName, { tag: "List", items: [] });
          }
        }

        // match p repeated k times against vals[vi .. vi+k)
        let okRep = true;
        for (let j = 0; j < k; j++) {
          const r1 = match(p, vals[vi + j]!, bTry, /*inEllipsis*/ true);
          if (!r1.ok) { okRep = false; break; }
        }
        if (!okRep) continue;

        // then match rest against remainder (use parentInEllipsis for continuity)
        const r2 = matchListWithContext(rest, vals.slice(vi + k), bTry, parentInEllipsis);
        if (r2.ok) return r2;
      }

      return fail("ellipsis backtracking exhausted");
    }

    // normal element - propagate parent ellipsis context
    if (vi >= vals.length) return fail("value list ended early");
    const r = match(p, vals[vi]!, b0, /*inEllipsis*/ parentInEllipsis);
    if (!r.ok) return r;
    return go(pi + 1, vi + 1, r.bindings);
  }

  return go(0, 0, b);
}

/**
 * Extract all pattern variable names (e.g., ?x -> "x") from a pattern.
 */
function extractPatternVars(p: Sexp): string[] {
  const vars: string[] = [];

  function walk(s: Sexp): void {
    if (s.tag === "Sym" && s.name.startsWith("?") && s.name.length > 1) {
      vars.push(s.name.slice(1));
    } else if (s.tag === "List") {
      for (const item of s.items) {
        // Skip the ellipsis symbol itself
        if (item.tag === "Sym" && item.name === "...") continue;
        walk(item);
      }
    }
  }

  walk(p);
  return vars;
}

function bindVar(name: string, v: Sexp, b: Bindings, inEllipsis: boolean): MatchResult {
  const existing = b.get(name);

  if (!existing) {
    // If we're inside ellipsis, accumulate as a list
    if (inEllipsis) {
      b.set(name, { tag: "List", items: [v] });
    } else {
      b.set(name, v);
    }
    return { ok: true, bindings: b };
  }

  // Existing binding - check if it's a list (for ellipsis accumulation)
  if (existing.tag === "List") {
    if (inEllipsis) {
      // Inside ellipsis: append to the list
      existing.items.push(v);
      return { ok: true, bindings: b };
    }

    // Outside ellipsis with an existing list binding
    // This happens when a var was used in an ellipsis earlier but now used as scalar
    if (existing.items.length !== 1) {
      return fail(`var ?${name} bound to repeated list, used as scalar`);
    }
    if (!sexpEq(existing.items[0]!, v)) {
      return fail(`var ?${name} mismatch`);
    }
    return { ok: true, bindings: b };
  }

  // Scalar binding
  if (inEllipsis) {
    // Upgrade scalar -> list (first occurrence + new)
    b.set(name, { tag: "List", items: [existing, v] });
    return { ok: true, bindings: b };
  }

  if (!sexpEq(existing, v)) return fail(`var ?${name} mismatch`);
  return { ok: true, bindings: b };
}

function cloneBindings(b: Bindings): Bindings {
  const out = new Map<string, Sexp>();
  for (const [k, v] of b.entries()) {
    // Deep-copy only lists (so ellipsis append doesn't mutate other branches)
    if (v.tag === "List") out.set(k, { tag: "List", items: [...v.items] });
    else out.set(k, v);
  }
  return out;
}

function fail(reason: string): MatchResult {
  return { ok: false, reason };
}

// Re-export for convenience
export { sexpToString } from "./sexp";
