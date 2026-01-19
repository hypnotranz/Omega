// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-11.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import type { Syntax, SIdent } from "../syntax/syntax";
import { isIdent, isList, addScope, freshScope } from "../syntax/syntax";
import type { Env } from "../syntax/binding";
import { freeIdentifierEq } from "../syntax/binding";

export type SRRule = { pat: Syntax; tmpl: Syntax };

export type SRTransformer = {
  phaseOut: number;
  envDefOut: Env;
  literals: SIdent[];
  rules: SRRule[];
};

export function compileSyntaxRules(
  phaseOut: number,
  envDefOut: Env,
  literals: SIdent[],
  rules: SRRule[]
): SRTransformer {
  return { phaseOut, envDefOut, literals, rules };
}

type SubstVal =
  | { tag: "One"; stx: Syntax }
  | { tag: "Many"; items: Array<{ tag: "One"; stx: Syntax }> };

type Subst = Map<string, SubstVal>;

function isEllipsis(stx: Syntax): boolean {
  return isIdent(stx) && stx.name === "...";
}

function isWildcard(id: SIdent): boolean {
  return id.name === "_";
}

function isLiteralIdent(id: SIdent, tr: SRTransformer, envUseOut: Env): boolean {
  for (const lit of tr.literals) {
    if (freeIdentifierEq(lit, tr.envDefOut, tr.phaseOut, id, envUseOut)) return true;
  }
  return false;
}

function extendOne(σ: Subst, name: string, stx: Syntax): Subst | null {
  const prev = σ.get(name);
  if (!prev) {
    const σ2 = new Map(σ);
    σ2.set(name, { tag: "One", stx });
    return σ2;
  }
  if (prev.tag === "One") {
    // same binding required
    // for minimal soundness, accept structural equality by JSON
    if (JSON.stringify(prev.stx) !== JSON.stringify(stx)) return null;
    return σ;
  }
  // previously Many, cannot unify with One
  return null;
}

function extendMany(σ: Subst, name: string, one: { tag: "One"; stx: Syntax }): Subst {
  const prev = σ.get(name);
  const σ2 = new Map(σ);
  if (!prev) {
    σ2.set(name, { tag: "Many", items: [one] });
    return σ2;
  }
  if (prev.tag === "Many") {
    σ2.set(name, { tag: "Many", items: prev.items.concat([one]) });
    return σ2;
  }
  // prev One: upgrade to Many(One, newOne) is unsound in general; treat as error
  throw new Error(`syntax-rules: variable ${name} used inconsistently with ellipses`);
}

function matchPattern(
  pat: Syntax,
  inp: Syntax,
  σ: Subst,
  tr: SRTransformer,
  envUseOut: Env
): Subst | null {
  if (pat.tag === "Atom") {
    if (inp.tag !== "Atom") return null;
    return JSON.stringify(pat.value) === JSON.stringify(inp.value) ? σ : null;
  }

  if (pat.tag === "Ident") {
    if (isWildcard(pat)) return σ;

    if (isLiteralIdent(pat, tr, envUseOut)) {
      if (inp.tag !== "Ident") return null;
      return freeIdentifierEq(pat, tr.envDefOut, tr.phaseOut, inp, envUseOut) ? σ : null;
    }

    // pattern variable
    return extendOne(σ, pat.name, inp);
  }

  // list pattern
  if (pat.tag === "List") {
    if (inp.tag !== "List") return null;
    return matchList(pat.items, inp.items, σ, tr, envUseOut);
  }

  return null;
}

function matchList(
  pats: Syntax[],
  inps: Syntax[],
  σ: Subst,
  tr: SRTransformer,
  envUseOut: Env
): Subst | null {
  // Support only suffix ellipses for baseline correctness:
  //   (a b ... c) is complex; implement the common (x ...) and nested forms used in practice.

  let pi = 0;
  let ii = 0;
  let σcur: Subst | null = σ;

  while (pi < pats.length) {
    const p = pats[pi];
    const pNext = pats[pi + 1];

    if (pNext && isEllipsis(pNext)) {
      // repeating segment p* matches zero or more input items (greedy but safe if only at end)
      // For correctness with nested ellipses, we allow it anywhere but require that remaining pattern
      // after ellipsis is empty (suffix ellipses). If not, throw for now.
      if (pi + 2 < pats.length) {
        throw new Error("syntax-rules: non-suffix ellipses not supported in this baseline matcher");
      }

      // match the remainder of inps as repeats of p
      while (ii < inps.length) {
        // gather σ segment bindings as Many
        const segσ = matchPattern(p, inps[ii], new Map(), tr, envUseOut);
        if (!segσ) return null;

        // merge segσ into σcur as Many
        for (const [k, v] of segσ.entries()) {
          if (v.tag !== "One") throw new Error("internal: segment match produced Many unexpectedly");
          σcur = extendMany(σcur!, k, v);
        }
        ii++;
      }
      return σcur;
    }

    // non-repeating
    if (ii >= inps.length) return null;
    σcur = matchPattern(p, inps[ii], σcur!, tr, envUseOut);
    if (!σcur) return null;
    pi++;
    ii++;
  }

  return ii === inps.length ? σcur : null;
}

/**
 * Expand template with correct hygiene:
 * - substituted ids: do NOT add introducer scope
 * - introduced ids: add introducer scope
 */
function expandTemplateHygienic(tmpl: Syntax, σ: Subst, introducer: string): Syntax {
  if (tmpl.tag === "Atom") return tmpl;

  if (tmpl.tag === "Ident") {
    const v = σ.get(tmpl.name);
    if (v) {
      if (v.tag !== "One") throw new Error(`template var ${tmpl.name} expected One`);
      return v.stx;
    }
    return addScope(tmpl, introducer) as any;
  }

  // list template with ellipses (suffix only baseline)
  const outItems: Syntax[] = [];
  const items = tmpl.items;

  for (let i = 0; i < items.length; i++) {
    const t0 = items[i];
    const t1 = items[i + 1];

    if (t1 && isEllipsis(t1)) {
      const k = segmentRepLen(t0, σ);
      for (let j = 0; j < k; j++) {
        const σj = substProject(σ, j);
        outItems.push(expandTemplateHygienic(t0, σj, introducer));
      }
      i += 1;
      continue;
    }

    outItems.push(expandTemplateHygienic(t0, σ, introducer));
  }

  return { ...tmpl, items: outItems };
}

function collectTemplateVars(t: Syntax, vars: Set<string>) {
  if (t.tag === "Ident") vars.add(t.name);
  else if (t.tag === "List") for (const it of t.items) collectTemplateVars(it, vars);
}

function segmentRepLen(segment: Syntax, σ: Subst): number {
  const vars = new Set<string>();
  collectTemplateVars(segment, vars);

  let len: number | null = null;
  for (const x of vars) {
    const v = σ.get(x);
    if (!v) continue;
    if (v.tag === "Many") {
      const k = v.items.length;
      if (len === null) len = k;
      else if (len !== k) throw new Error(`ellipsis zip mismatch on ${x}`);
    }
  }
  return len ?? 0;
}

function substProject(σ: Subst, i: number): Subst {
  const out: Subst = new Map();
  for (const [k, v] of σ.entries()) {
    out.set(k, v.tag === "One" ? v : v.items[i]);
  }
  return out;
}

export function applySyntaxRules(
  tr: SRTransformer,
  callStx: Syntax,
  envUseOut: Env,
  scopeCounter: { n: number }
): Syntax {
  const U = freshScope(scopeCounter);
  const callU = addScope(callStx, U);

  for (const rule of tr.rules) {
    const σ0: Subst = new Map();
    const σ = matchPattern(rule.pat, callU, σ0, tr, envUseOut);
    if (!σ) continue;

    const I = freshScope(scopeCounter);
    return expandTemplateHygienic(rule.tmpl, σ, I);
  }

  throw new Error("syntax-rules: no rule matched");
}