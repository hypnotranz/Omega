## Ω Language Specification v1.0

### Part 17 — The Missing Link Completed: Reader → Syntax (Scoped) → Hygienic Expander (`define-syntax` + `syntax-rules`) → Binder‑Scope Insertion (`lambda`/`let`) → Hygienic Lowering (Bid‑Renamed Vars) → Core `Expr` (Runnable H1–H5)

This is the “wire it all together” layer that turns the Part 16 test placeholders into **actual runnable integration tests**.

You asked for “not an API call,” and this is exactly that: the LLM/inference plane isn’t involved here at all; what you’re getting is the **fully formal compilation path** for the hygienic macro system and the core evaluator pipeline.

**Deliverables in this part:**

1. A deterministic **S-expression reader** (tokenizer + parser)
2. Datum → Syntax (with module scope)
3. A **hygienic expander** with:

   * `begin` sequences that install `define` and `define-syntax` incrementally (like a real top-level)
   * macro application via `syntax-rules`
   * binder-scope insertion for `lambda` and `let`
4. A **hygienic lowering** pass that converts identifiers → internal names (derived from `bid`) so runtime evaluation cannot accidentally capture
5. Wiring into `omegaHarness.parseTextToExpr`, so the hygiene tests in Part 16 now execute end-to-end

> **Critical correction vs Part 15 reference**: `syntax-rules` introducer scope must **NOT** be applied to substituted pattern variables. If you apply introducer scope to substituted identifiers, you destroy hygiene (H1 fails). The template expander below is implemented correctly.

---

# 154. Reader: Tokenizer + Parser for S-Expressions

We implement a deterministic reader with:

* parentheses lists
* symbols
* numbers
* strings
* booleans `#t/#f`
* quote sugar `'x` → `(quote x)`
* line comments `; ...`

## 154.1 `src/core/reader/datum.ts`

```ts
// src/core/reader/datum.ts
export type Sym = { sym: string };

export type Datum =
  | number
  | string
  | boolean
  | null
  | Sym
  | Datum[];

export const sym = (s: string): Sym => ({ sym: s });
export const isSym = (d: Datum): d is Sym => typeof d === "object" && d !== null && !Array.isArray(d) && "sym" in d;
```

## 154.2 `src/core/reader/tokenize.ts`

```ts
// src/core/reader/tokenize.ts
export type Tok =
  | { tag: "LParen" }
  | { tag: "RParen" }
  | { tag: "Quote" }
  | { tag: "Str"; s: string }
  | { tag: "Atom"; s: string };

export function tokenize(src: string): Tok[] {
  const toks: Tok[] = [];
  let i = 0;

  const isWS = (c: string) => c === " " || c === "\t" || c === "\n" || c === "\r";

  while (i < src.length) {
    const c = src[i];

    // comments
    if (c === ";") {
      while (i < src.length && src[i] !== "\n") i++;
      continue;
    }

    if (isWS(c)) { i++; continue; }

    if (c === "(") { toks.push({ tag: "LParen" }); i++; continue; }
    if (c === ")") { toks.push({ tag: "RParen" }); i++; continue; }
    if (c === "'") { toks.push({ tag: "Quote" }); i++; continue; }

    if (c === "\"") {
      i++;
      let s = "";
      while (i < src.length) {
        const d = src[i];
        if (d === "\"") { i++; break; }
        if (d === "\\") {
          const e = src[i + 1];
          if (e === "n") { s += "\n"; i += 2; continue; }
          if (e === "t") { s += "\t"; i += 2; continue; }
          s += e ?? "";
          i += 2;
          continue;
        }
        s += d;
        i++;
      }
      toks.push({ tag: "Str", s });
      continue;
    }

    // atom: read until whitespace or delimiter
    let a = "";
    while (i < src.length) {
      const d = src[i];
      if (isWS(d) || d === "(" || d === ")" || d === "'" || d === ";") break;
      a += d;
      i++;
    }
    if (a.length === 0) throw new Error("tokenize: internal error");
    toks.push({ tag: "Atom", s: a });
  }

  return toks;
}
```

## 154.3 `src/core/reader/parse.ts`

```ts
// src/core/reader/parse.ts
import type { Tok } from "./tokenize";
import type { Datum } from "./datum";
import { sym } from "./datum";

export function parseAll(toks: Tok[]): Datum[] {
  const out: Datum[] = [];
  let i = 0;

  function parseOne(): Datum {
    const t = toks[i];
    if (!t) throw new Error("parse: unexpected EOF");

    if (t.tag === "Quote") {
      i++;
      const d = parseOne();
      return [sym("quote"), d];
    }

    if (t.tag === "LParen") {
      i++;
      const items: Datum[] = [];
      while (true) {
        const u = toks[i];
        if (!u) throw new Error("parse: missing ')'");
        if (u.tag === "RParen") { i++; break; }
        items.push(parseOne());
      }
      return items;
    }

    if (t.tag === "RParen") {
      throw new Error("parse: unexpected ')'");
    }

    if (t.tag === "Str") {
      i++;
      return t.s;
    }

    // Atom: booleans, numbers, or symbol
    if (t.tag === "Atom") {
      i++;
      const s = t.s;

      if (s === "#t") return true;
      if (s === "#f") return false;

      // number
      if (/^-?\d+(\.\d+)?$/.test(s)) return Number(s);

      // null-ish
      if (s === "null") return null;

      return sym(s);
    }

    throw new Error("parse: unreachable");
  }

  while (i < toks.length) {
    out.push(parseOne());
  }
  return out;
}
```

---

# 155. Datum → Syntax (Scoped)

We convert the reader datum into `Syntax` nodes (from Part 15), then apply a **module scope** `M0` to the entire syntax tree. This is the *def-site lexical anchor* that makes hygiene meaningful.

## 155.1 `src/core/reader/toSyntax.ts`

```ts
// src/core/reader/toSyntax.ts
import type { Datum } from "./datum";
import { isSym } from "./datum";
import type { Syntax } from "../syntax/syntax";

export function datumToSyntax(d: Datum): Syntax {
  if (isSym(d)) {
    return { tag: "Ident", name: d.sym, scopes: [] };
  }
  if (Array.isArray(d)) {
    return { tag: "List", items: d.map(datumToSyntax), scopes: [] };
  }
  // atoms
  return { tag: "Atom", value: d, scopes: [] };
}
```

---

# 156. Fixing `syntax-rules` Hygiene: Introducer Scope Only on Introduced Identifiers

This section replaces the “reference-grade” introducer behavior with the **correct** one.

> Rule: During template expansion, if an identifier is substituted from a pattern variable, **do not** apply introducer scope. Otherwise, apply introducer scope (and preserve def-site scopes already present in template).

## 156.1 Patch: `src/core/expand/syntaxRules.ts` (correct `expandTemplate`)

This assumes you already have the Part 15 `compileSyntaxRules`, matcher, and `applySyntaxRules` structure. Replace the template expansion portion with the following:

```ts
// src/core/expand/syntaxRules.ts (PATCH)

import type { Syntax, SIdent } from "../syntax/syntax";
import { isIdent, isList, addScope, freshScope } from "../syntax/syntax";

// ... keep SubstVal, Subst, matchPattern, matchList, matchRepeat, etc. from Part 15 ...

function isEllipsis(stx: Syntax): boolean {
  return isIdent(stx) && stx.name === "...";
}

/**
 * Expand a template under substitution σ, applying introducer scope I
 * ONLY to introduced identifiers (i.e., identifiers not substituted).
 */
function expandTemplateHygienic(tmpl: Syntax, σ: Subst, introducer: string): Syntax {
  if (tmpl.tag === "Atom") return tmpl;

  if (tmpl.tag === "Ident") {
    const v = σ.get(tmpl.name);
    if (v) {
      if (v.tag !== "One") throw new Error(`template var ${tmpl.name} expected One`);
      // SUBSTITUTED PIECE: DO NOT add introducer scope.
      return v.stx;
    }
    // INTRODUCED IDENTIFIER: add introducer scope
    return addScope(tmpl, introducer) as any;
  }

  // list template
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

/** As in Part 15: determine repetition length by zip semantics. */
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

function collectTemplateVars(t: Syntax, vars: Set<string>) {
  if (t.tag === "Ident") vars.add(t.name);
  else if (t.tag === "List") for (const it of t.items) collectTemplateVars(it, vars);
}

function substProject(σ: Subst, i: number): Subst {
  const out: Subst = new Map();
  for (const [k, v] of σ.entries()) {
    out.set(k, v.tag === "One" ? v : v.items[i]);
  }
  return out;
}

/**
 * Apply syntax-rules: use-site scope U on call syntax; introducer scope I on introduced ids.
 */
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
    const σ = matchPattern(rule.pat, callU, σ0, { transformer: tr, envUseOut });
    if (!σ) continue;

    const I = freshScope(scopeCounter);
    return expandTemplateHygienic(rule.tmpl, σ, I);
  }

  throw new Error("syntax-rules: no rule matched");
}
```

This is the hygiene-critical fix that makes H1 and H2 behave correctly.

---

# 157. The Hygienic Expander (Stateful, Sequential Defines)

We implement a **compilation environment Γ** as a global binding table (`Binding[]`) that is *monotonic* (append-only). This is intentionally closer to a compiler symbol table than a runtime lexical env.

Lexical correctness is preserved by the scope-set subset resolution rule; bindings “outside scope” are simply non-applicable.

## 157.1 Compile-time binding type

Use the `Binding` and `resolveIdent` from Part 15 `src/core/syntax/binding.ts` (bid + scopes + phase + kind + value).

We will use:

* phase 0 `kind:"value"` with `value = internalName: string`
* phase 1 `kind:"syntax"` with `value = SRTransformer`

## 157.2 `src/core/pipeline/compileText.ts`

```ts
// src/core/pipeline/compileText.ts
import { tokenize } from "../reader/tokenize";
import { parseAll } from "../reader/parse";
import { datumToSyntax } from "../reader/toSyntax";
import type { Syntax, SIdent, SList } from "../syntax/syntax";
import { isIdent, isList, addScope, freshScope } from "../syntax/syntax";
import type { Binding, Env } from "../syntax/binding";
import { resolveIdent } from "../syntax/binding";
import type { Expr, HandlerExpr } from "../ast";
import { compileSyntaxRules, applySyntaxRules, type SRRule, type SRTransformer } from "../expand/syntaxRules";
import { sym, type Datum, isSym } from "../reader/datum";

/** Deterministic counters: no Math.random in the compiler core (reproducibility). */
type Counters = { scope: { n: number }, bid: { n: number } };

function freshBid(c: Counters): string {
  c.bid.n += 1;
  return `bid#${c.bid.n}`;
}

function internalName(name: string, bid: string): string {
  return `${name}$${bid}`;
}

/** Create a new value binding for an identifier syntax object. */
function bindValue(env: Env, id: SIdent, bid: string, internal: string): Env {
  const b: Binding = {
    bid,
    name: id.name,
    scopes: id.scopes.slice(),
    phase: 0,
    kind: "value",
    value: internal,
  };
  return env.concat([b]);
}

/** Create a new syntax binding (macro). */
function bindSyntax(env: Env, id: SIdent, bid: string, tr: SRTransformer): Env {
  const b: Binding = {
    bid,
    name: id.name,
    scopes: id.scopes.slice(),
    phase: 1,
    kind: "syntax",
    value: tr,
  };
  return env.concat([b]);
}

function headName(stx: Syntax): string | null {
  if (!isList(stx)) return null;
  const items = stx.items;
  if (items.length === 0) return null;
  const h = items[0];
  return isIdent(h) ? h.name : null;
}

function expectList(stx: Syntax, msg: string): SList {
  if (!isList(stx)) throw new Error(msg);
  return stx;
}

function expectIdent(stx: Syntax, msg: string): SIdent {
  if (!isIdent(stx)) throw new Error(msg);
  return stx;
}

/**
 * Initialize compile-time env with primitive bindings whose runtime internal names match.
 * This must align with installPrims(...) in tests.
 */
function initialEnv(moduleScope: string): Env {
  const prims = ["+", "-", "=", "not", "unit"];
  let env: Env = [];
  for (const p of prims) {
    env = env.concat([{
      bid: `prim:${p}`,
      name: p,
      scopes: [moduleScope],  // imported into module scope
      phase: 0,
      kind: "value",
      value: p,               // runtime internal name = symbol name
    } satisfies Binding]);
  }
  return env;
}

/**
 * Compile text to core Expr:
 *   read → syntax(with module scope) → expand (define/define-syntax) → lower to Expr
 */
export function compileTextToExpr(src: string): Expr {
  const toks = tokenize(src);
  const ds = parseAll(toks);

  // If multiple forms, wrap in (begin ...)
  const topDatum: Datum =
    ds.length === 1 ? ds[0] : [sym("begin"), ...ds];

  // Datum → Syntax
  let stx = datumToSyntax(topDatum);

  // Counters + module scope
  const c: Counters = { scope: { n: 0 }, bid: { n: 0 } };
  const M0 = freshScope(c.scope);
  stx = addScope(stx, M0);

  // Compile-time env Γ (phases included in one list, filtered by resolveIdent)
  let env: Env = initialEnv(M0);

  // Expand top-level expression (stateful over begin sequence)
  const expanded = expandTop(stx, env, c);
  env = expanded.env;

  // Lower to core Expr using global env table (scope sets disambiguate)
  return lowerSyntax(expanded.stx, env);
}

/** Result carrying updated env (monotonic binding table). */
type ExpandRes = { stx: Syntax; env: Env };

/** Expand top-level expression; begin processes definitions sequentially. */
function expandTop(stx: Syntax, env: Env, c: Counters): ExpandRes {
  if (isList(stx) && headName(stx) === "begin") {
    const items = stx.items.slice(1);
    const seq = expandSequence(items, env, c);
    // Rewrap into begin
    return { stx: { ...stx, items: [stx.items[0], ...seq.items] }, env: seq.env };
  }
  // Otherwise treat as expression (no define-syntax at top level in this minimal harness)
  const e = expandExpr(stx, env, c);
  return e;
}

function expandSequence(items: Syntax[], env: Env, c: Counters): { items: Syntax[]; env: Env } {
  const out: Syntax[] = [];
  let Γ = env;

  for (const form of items) {
    const hn = headName(form);
    if (hn === "define-syntax") {
      // compile and install macro; remove from runtime output
      const r = expandDefineSyntax(form, Γ, c);
      Γ = r.env;
      continue;
    }

    if (hn === "define") {
      const r = expandDefine(form, Γ, c);
      Γ = r.env;
      out.push(r.stx);
      continue;
    }

    // ordinary expression
    const r = expandExpr(form, Γ, c);
    Γ = r.env;
    out.push(r.stx);
  }

  return { items: out, env: Γ };
}

/**
 * Expand an expression, performing macro expansion and binder-scope insertion
 * for reserved binding forms (lambda, let).
 */
function expandExpr(stx: Syntax, env: Env, c: Counters): ExpandRes {
  if (!isList(stx)) return { stx, env };

  if (stx.items.length === 0) return { stx, env };

  const h = stx.items[0];
  if (!isIdent(h)) {
    // Expand children
    let Γ = env;
    const items2 = [];
    for (const it of stx.items) {
      const r = expandExpr(it, Γ, c);
      Γ = r.env;
      items2.push(r.stx);
    }
    return { stx: { ...stx, items: items2 }, env: Γ };
  }

  // Reserved forms (keywords)
  switch (h.name) {
    case "quote":
      // do not expand under quote
      return { stx, env };

    case "begin": {
      const seq = expandSequence(stx.items.slice(1), env, c);
      return { stx: { ...stx, items: [h, ...seq.items] }, env: seq.env };
    }

    case "if": {
      if (stx.items.length !== 4) throw new Error("if: expected (if test conseq alt)");
      const r1 = expandExpr(stx.items[1], env, c);
      const r2 = expandExpr(stx.items[2], r1.env, c);
      const r3 = expandExpr(stx.items[3], r2.env, c);
      return { stx: { ...stx, items: [h, r1.stx, r2.stx, r3.stx] }, env: r3.env };
    }

    case "lambda":
      return expandLambda(stx, env, c);

    case "let":
      return expandLet(stx, env, c);

    case "set!": {
      if (stx.items.length !== 3) throw new Error("set!: expected (set! x rhs)");
      const rhs = expandExpr(stx.items[2], env, c);
      return { stx: { ...stx, items: [h, stx.items[1], rhs.stx] }, env: rhs.env };
    }

    case "effect": {
      // (effect op arg...)
      let Γ = env;
      const items2: Syntax[] = [h, stx.items[1]];
      for (let i = 2; i < stx.items.length; i++) {
        const r = expandExpr(stx.items[i], Γ, c);
        Γ = r.env;
        items2.push(r.stx);
      }
      return { stx: { ...stx, items: items2 }, env: Γ };
    }

    case "handle": {
      // (handle body (on op (x k) ...) (return (v) ...) (finally ...))
      // Expand body and clause bodies. We treat clause syntax structurally; no macro expansion in op names.
      if (stx.items.length < 2) throw new Error("handle: expected (handle body ...clauses)");
      const bodyR = expandExpr(stx.items[1], env, c);

      let Γ = bodyR.env;
      const clauses2: Syntax[] = [h, bodyR.stx];

      for (let i = 2; i < stx.items.length; i++) {
        const cl = stx.items[i];
        // Expand clause body conservatively:
        // (on op (x k) body) etc. We'll expand the body expression position.
        if (isList(cl) && cl.items.length >= 2 && isIdent(cl.items[0])) {
          const tag = (cl.items[0] as SIdent).name;
          if (tag === "on") {
            const bodyIdx = cl.items.length - 1;
            const b = expandExpr(cl.items[bodyIdx], Γ, c);
            Γ = b.env;
            const cl2 = { ...cl, items: cl.items.slice(0, bodyIdx).concat([b.stx]) };
            clauses2.push(cl2);
            continue;
          }
          if (tag === "return") {
            const bodyIdx = cl.items.length - 1;
            const b = expandExpr(cl.items[bodyIdx], Γ, c);
            Γ = b.env;
            const cl2 = { ...cl, items: cl.items.slice(0, bodyIdx).concat([b.stx]) };
            clauses2.push(cl2);
            continue;
          }
          if (tag === "finally") {
            const bodyIdx = cl.items.length - 1;
            const b = expandExpr(cl.items[bodyIdx], Γ, c);
            Γ = b.env;
            const cl2 = { ...cl, items: cl.items.slice(0, bodyIdx).concat([b.stx]) };
            clauses2.push(cl2);
            continue;
          }
        }
        // unknown clause shape: expand it as ordinary expression
        const r = expandExpr(cl, Γ, c);
        Γ = r.env;
        clauses2.push(r.stx);
      }

      return { stx: { ...stx, items: clauses2 }, env: Γ };
    }
  }

  // Macro application: resolve head at phase 1
  const macroBinding = resolveIdent(h, env, 1);
  if (macroBinding && macroBinding.kind === "syntax") {
    const tr = macroBinding.value as SRTransformer;
    // IMPORTANT: we do NOT “add surrounding binder scopes” to the expansion output.
    // Hygiene is preserved because only substituted pieces carry call-site scopes.
    const out = applySyntaxRules(tr, stx, env, c.scope);
    return expandExpr(out, env, c); // macro chaining
  }

  // Not a macro: recursively expand children (function position and args)
  let Γ = env;
  const items2: Syntax[] = [];
  for (const it of stx.items) {
    const r = expandExpr(it, Γ, c);
    Γ = r.env;
    items2.push(r.stx);
  }
  return { stx: { ...stx, items: items2 }, env: Γ };
}

function expandLambda(stx: SList, env: Env, c: Counters): ExpandRes {
  // (lambda (x ...) body...)
  if (stx.items.length < 3) throw new Error("lambda: expected (lambda (params) body...)");
  const paramsList = expectList(stx.items[1], "lambda: params must be list");
  const bodyForms = stx.items.slice(2);

  const B = freshScope(c.scope);

  // Scope params with binder scope
  const paramsB: SIdent[] = paramsList.items.map(p => expectIdent(p, "lambda param must be ident"))
    .map(p => addScope(p, B) as SIdent);

  // Extend env with param bindings (monotonic table)
  let Γ = env;
  for (const p of paramsB) {
    const bid = freshBid(c);
    Γ = bindValue(Γ, p, bid, internalName(p.name, bid));
  }

  // Add binder scope to body BEFORE expanding
  const bodyScoped = bodyForms.map(b => addScope(b, B));
  const bodyExpanded: Syntax[] = [];
  let Γ2 = Γ;
  for (const b of bodyScoped) {
    const r = expandExpr(b, Γ2, c);
    Γ2 = r.env;
    bodyExpanded.push(r.stx);
  }

  const stx2: Syntax = {
    ...stx,
    items: [
      stx.items[0],
      { ...paramsList, items: paramsB },
      ...bodyExpanded,
    ],
  };

  return { stx: stx2, env: Γ2 };
}

function expandLet(stx: SList, env: Env, c: Counters): ExpandRes {
  // (let ((x e) ...) body...)
  if (stx.items.length < 3) throw new Error("let: expected (let ((x e) ...) body...)");
  const bindsList = expectList(stx.items[1], "let: bindings must be list");
  const bodyForms = stx.items.slice(2);

  const B = freshScope(c.scope);

  // Expand initializers first (non-recursive let): do NOT add binder scope B
  let Γ = env;
  const bindPairs: Array<{ idB: SIdent; init: Syntax }> = [];

  for (const bp of bindsList.items) {
    const pair = expectList(bp, "let: binding must be list");
    if (pair.items.length !== 2) throw new Error("let: binding must be (x init)");
    const id = expectIdent(pair.items[0], "let: binder must be ident");
    const init0 = pair.items[1];

    const initR = expandExpr(init0, Γ, c);
    Γ = initR.env;

    const idB = addScope(id, B) as SIdent;
    bindPairs.push({ idB, init: initR.stx });
  }

  // Now install binder bindings into env
  for (const { idB } of bindPairs) {
    const bid = freshBid(c);
    Γ = bindValue(Γ, idB, bid, internalName(idB.name, bid));
  }

  // Add binder scope to body BEFORE expanding
  const bodyScoped = bodyForms.map(b => addScope(b, B));
  const bodyExpanded: Syntax[] = [];
  let Γ2 = Γ;

  for (const b of bodyScoped) {
    const r = expandExpr(b, Γ2, c);
    Γ2 = r.env;
    bodyExpanded.push(r.stx);
  }

  const bindsOut: Syntax = {
    ...bindsList,
    items: bindPairs.map(({ idB, init }) => ({
      tag: "List",
      scopes: [],
      items: [idB, init],
    })),
  };

  const stx2: Syntax = {
    ...stx,
    items: [stx.items[0], bindsOut, ...bodyExpanded],
  };

  return { stx: stx2, env: Γ2 };
}

function expandDefine(stx: SList, env: Env, c: Counters): ExpandRes {
  // (define x rhs) or (define (f args...) body...)
  if (stx.items.length < 3) throw new Error("define: expected (define x rhs) or (define (f ...) ...)");
  const target = stx.items[1];

  // function definition sugar
  if (isList(target)) {
    const sig = target;
    if (sig.items.length < 1 || !isIdent(sig.items[0])) throw new Error("define: bad function signature");
    const f = sig.items[0] as SIdent;
    const args = sig.items.slice(1);
    const lambdaForm: Syntax = {
      tag: "List",
      scopes: [],
      items: [
        { tag: "Ident", name: "lambda", scopes: (stx.items[0] as any).scopes ?? [] },
        { tag: "List", scopes: [], items: args },
        ...stx.items.slice(2),
      ],
    };
    const rewritten: Syntax = {
      ...stx,
      items: [stx.items[0], f, lambdaForm],
    };
    return expandDefine(rewritten as SList, env, c);
  }

  const id = expectIdent(target, "define: name must be ident");
  // Install binding before expanding RHS (supports self recursion)
  let Γ = env;
  const bid = freshBid(c);
  Γ = bindValue(Γ, id, bid, internalName(id.name, bid));

  const rhs0 = stx.items[2];
  const rhsR = expandExpr(rhs0, Γ, c);
  Γ = rhsR.env;

  const stx2: Syntax = { ...stx, items: [stx.items[0], id, rhsR.stx] };
  return { stx: stx2, env: Γ };
}

function expandDefineSyntax(stx: SList, env: Env, c: Counters): ExpandRes {
  // (define-syntax m (syntax-rules (lits...) (pat tmpl) ...))
  if (stx.items.length !== 3) throw new Error("define-syntax: expected (define-syntax name transformer)");
  const id = expectIdent(stx.items[1], "define-syntax: name must be ident");
  const rhs = stx.items[2];

  const tr = compileTransformer(rhs, env);
  const bid = freshBid(c);
  const Γ = bindSyntax(env, id, bid, tr);
  // define-syntax disappears at runtime
  return { stx: { tag: "Atom", value: null, scopes: [] }, env: Γ };
}

function compileTransformer(rhs: Syntax, envDefOut: Env): SRTransformer {
  // only syntax-rules supported in this harness
  const r = expectList(rhs, "define-syntax: transformer must be list");
  if (r.items.length < 3) throw new Error("syntax-rules: expected (syntax-rules (lits) (pat tmpl)...)");

  const h = expectIdent(r.items[0], "syntax-rules: head must be ident");
  if (h.name !== "syntax-rules") throw new Error("define-syntax: only syntax-rules supported");

  const litsList = expectList(r.items[1], "syntax-rules: literal list must be list");
  const literals = litsList.items.map(x => expectIdent(x, "syntax-rules literal must be ident"));

  const rules: SRRule[] = [];
  for (let i = 2; i < r.items.length; i++) {
    const rr = expectList(r.items[i], "syntax-rules: rule must be list");
    if (rr.items.length !== 2) throw new Error("syntax-rules: each rule must be (pat tmpl)");
    rules.push({ pat: rr.items[0], tmpl: rr.items[1] });
  }

  // phaseOut = 0 for normal macros
  return compileSyntaxRules(0, envDefOut.slice(), literals, rules);
}
```

### Why this expander is hygienic in the “important way”

* We add binder scope **before** expanding body forms (`let`/`lambda`).
  This ensures call-site identifiers get lexical scope, while macro-introduced identifiers do **not** get captured by call-site binders.
* Macro template introducer scope is applied **only to identifiers not substituted**.
* All bindings are recorded in Γ with their scope sets and bids; later resolution uses the subset+maximality rule.

---

# 158. Hygienic Lowering: Syntax → Core `Expr` via Binding Resolution to Internal Names

This is the final crucial step: runtime evaluation uses simple string variable names, so hygiene is enforced by **alpha-renaming to internal names** derived from `bid`.

## 158.1 `src/core/pipeline/lower.ts`

```ts
// src/core/pipeline/lower.ts
import type { Syntax, SIdent, SList } from "../syntax/syntax";
import { isIdent, isList } from "../syntax/syntax";
import type { Env } from "../syntax/binding";
import { resolveIdent } from "../syntax/binding";
import type { Expr, HandlerExpr } from "../ast";

function expectIdent(stx: Syntax, msg: string): SIdent {
  if (!isIdent(stx)) throw new Error(msg);
  return stx;
}
function expectList(stx: Syntax, msg: string): SList {
  if (!isList(stx)) throw new Error(msg);
  return stx;
}

function lowerVar(id: SIdent, env: Env): Expr {
  const b = resolveIdent(id, env, 0);
  if (!b || b.kind !== "value") {
    // If you want strict compile mode, throw here.
    return { tag: "Var", name: id.name };
  }
  return { tag: "Var", name: b.value as string };
}

export function lowerSyntax(stx: Syntax, env: Env): Expr {
  if (stx.tag === "Atom") {
    // number/string/bool/null
    const v = stx.value;
    if (typeof v === "number" || typeof v === "string" || typeof v === "boolean" || v === null) {
      return { tag: "Lit", value: v };
    }
    // Reader Sym encoded as object -> treat as quote datum? If it leaks, encode as string.
    return { tag: "Lit", value: String(v) };
  }

  if (stx.tag === "Ident") {
    return lowerVar(stx, env);
  }

  // list
  const items = stx.items;
  if (items.length === 0) {
    return { tag: "Quote", datum: [] };
  }

  const h = items[0];
  if (isIdent(h)) {
    switch (h.name) {
      case "quote": {
        if (items.length !== 2) throw new Error("quote: expected (quote datum)");
        // store datum from syntax by stripping scopes (cheap)
        return { tag: "Quote", datum: stripDatum(items[1]) };
      }

      case "begin": {
        const exprs = items.slice(1).map(x => lowerSyntax(x, env));
        return { tag: "Begin", exprs };
      }

      case "if": {
        if (items.length !== 4) throw new Error("if: expected (if t c a)");
        return {
          tag: "If",
          test: lowerSyntax(items[1], env),
          conseq: lowerSyntax(items[2], env),
          alt: lowerSyntax(items[3], env),
        };
      }

      case "lambda": {
        if (items.length < 3) throw new Error("lambda: expected (lambda (params) body...)");
        const paramsList = expectList(items[1], "lambda: params must be list");
        const params = paramsList.items.map(p => {
          const id = expectIdent(p, "lambda param must be ident");
          const b = resolveIdent(id, env, 0);
          if (!b || b.kind !== "value") throw new Error("lambda param did not resolve to value binding");
          return b.value as string;
        });
        const bodyForms = items.slice(2).map(x => lowerSyntax(x, env));
        const body: Expr = bodyForms.length === 1 ? bodyForms[0] : { tag: "Begin", exprs: bodyForms };
        return { tag: "Lambda", params, body };
      }

      case "let": {
        // lower let to ((lambda (x...) body...) init...)
        if (items.length < 3) throw new Error("let: expected (let ((x e) ...) body...)");
        const bindsList = expectList(items[1], "let: bindings must be list");
        const bindPairs = bindsList.items.map(bp => {
          const pair = expectList(bp, "let: binding must be list");
          if (pair.items.length !== 2) throw new Error("let: binding must be (x init)");
          const id = expectIdent(pair.items[0], "let: binder must be ident");
          const b = resolveIdent(id, env, 0);
          if (!b || b.kind !== "value") throw new Error("let binder did not resolve");
          const internal = b.value as string;
          const init = lowerSyntax(pair.items[1], env);
          return { internal, init };
        });

        const params = bindPairs.map(p => p.internal);
        const args = bindPairs.map(p => p.init);

        const bodyForms = items.slice(2).map(x => lowerSyntax(x, env));
        const body: Expr = bodyForms.length === 1 ? bodyForms[0] : { tag: "Begin", exprs: bodyForms };

        const lam: Expr = { tag: "Lambda", params, body };
        return { tag: "App", fn: lam, args };
      }

      case "define": {
        if (items.length !== 3) throw new Error("define: expected (define x rhs)");
        const id = expectIdent(items[1], "define: name must be ident");
        const b = resolveIdent(id, env, 0);
        if (!b || b.kind !== "value") throw new Error("define binder did not resolve");
        return { tag: "Define", name: b.value as string, rhs: lowerSyntax(items[2], env) };
      }

      case "set!": {
        if (items.length !== 3) throw new Error("set!: expected (set! x rhs)");
        const id = expectIdent(items[1], "set!: target must be ident");
        const b = resolveIdent(id, env, 0);
        if (!b || b.kind !== "value") throw new Error("set! target did not resolve");
        return { tag: "Set", name: b.value as string, rhs: lowerSyntax(items[2], env) };
      }

      case "effect": {
        if (items.length < 2) throw new Error("effect: expected (effect op arg...)");
        const opStx = items[1];
        let op: string;
        if (opStx.tag === "Ident") op = opStx.name;
        else if (opStx.tag === "Atom" && typeof opStx.value === "string") op = opStx.value;
        else throw new Error("effect: op must be ident or string atom");

        const args = items.slice(2).map(x => lowerSyntax(x, env));
        return { tag: "Effect", op, args };
      }

      case "handle": {
        if (items.length < 2) throw new Error("handle: expected (handle body clauses...)");
        const body = lowerSyntax(items[1], env);
        const handler = lowerHandler(items.slice(2), env);
        return { tag: "Handle", body, handler };
      }
    }
  }

  // application
  return {
    tag: "App",
    fn: lowerSyntax(items[0], env),
    args: items.slice(1).map(x => lowerSyntax(x, env)),
  };
}

/** Strip syntax to datum (for quote). */
function stripDatum(stx: Syntax): unknown {
  if (stx.tag === "Atom") return stx.value;
  if (stx.tag === "Ident") return { sym: stx.name };
  return stx.items.map(stripDatum);
}

function lowerHandler(clauses: Syntax[], env: Env): HandlerExpr {
  const on: HandlerExpr["on"] = [];
  let ret: HandlerExpr["ret"] | undefined;
  let fin: HandlerExpr["fin"] | undefined;

  for (const cl of clauses) {
    const lst = expectList(cl, "handle clause must be list");
    if (lst.items.length < 2) throw new Error("handle clause too short");
    const tag = expectIdent(lst.items[0], "handle clause tag must be ident").name;

    if (tag === "on") {
      // (on op (x k) body)
      if (lst.items.length !== 4) throw new Error("on: expected (on op (x k) body)");
      const op = expectIdent(lst.items[1], "on: op must be ident").name;
      const argsList = expectList(lst.items[2], "on: (x k) must be list");
      if (argsList.items.length !== 2) throw new Error("on: expected (x k)");
      const x = expectIdent(argsList.items[0], "on: x must be ident").name;
      const k = expectIdent(argsList.items[1], "on: k must be ident").name;
      const body = lowerSyntax(lst.items[3], env);
      on.push({ op, params: [x], k, body });
      continue;
    }

    if (tag === "return") {
      // (return (v) body)
      if (lst.items.length !== 3) throw new Error("return: expected (return (v) body)");
      const vList = expectList(lst.items[1], "return: (v) must be list");
      if (vList.items.length !== 1) throw new Error("return: expected one var");
      const v = expectIdent(vList.items[0], "return var must be ident").name;
      const body = lowerSyntax(lst.items[2], env);
      ret = { v, body };
      continue;
    }

    if (tag === "finally") {
      if (lst.items.length !== 2) throw new Error("finally: expected (finally body)");
      fin = { body: lowerSyntax(lst.items[1], env) };
      continue;
    }

    throw new Error(`unknown handle clause: ${tag}`);
  }

  return { on, ret, fin };
}
```

Now update `compileTextToExpr` to import `lowerSyntax` from this file (instead of inlining). In `compileTextToExpr.ts`, change:

```ts
import { lowerSyntax } from "./lower";
```

---

# 159. Wire the Harness: `omegaHarness.parseTextToExpr` Now Works

Replace the placeholder in Part 16’s harness with a real call to `compileTextToExpr`.

## 159.1 `test/helpers/omegaHarness.ts` (patched)

```ts
// test/helpers/omegaHarness.ts
import { COWStore } from "../../src/core/eval/store";
import { RuntimeImpl } from "../../src/core/effects/runtimeImpl";
import { mockOracle, mockCommit } from "./runtime";
import { installPrims } from "./prims";
import type { State } from "../../src/core/eval/machine";
import { runToCompletion } from "../../src/core/eval/run";
import type { Expr } from "../../src/core/ast";
import { compileTextToExpr } from "../../src/core/pipeline/compileText";

function initialState(expr: Expr): State {
  const store0 = new COWStore();
  const prim = installPrims(store0);
  return { control: { tag: "Expr", e: expr }, env: prim.env, store: prim.store, kont: [], handlers: [] };
}

export async function evalOmega(src: string): Promise<any> {
  const expr = compileTextToExpr(src);
  const runtime = new RuntimeImpl(mockOracle, mockCommit);
  const v = await runToCompletion(runtime, initialState(expr), 500_000);

  if (v.tag === "Num") return v.n;
  if (v.tag === "Bool") return v.b;
  if (v.tag === "Str") return v.s;
  if (v.tag === "Unit") return null;
  return v;
}
```

---

# 160. Update `datumToVal` (Quote Semantics): Symbols Become `Sym`

Your CEKS `datumToVal` in Part 16 currently doesn’t recognize `{ sym: "x" }`. Patch it so quoted symbols work.

## 160.1 Patch: `src/core/eval/machineStep.ts` — `datumToVal`

Replace the previous `datumToVal` with:

```ts
function datumToVal(d: unknown): Val {
  if (d === null) return { tag: "Unit" };
  if (typeof d === "number") return { tag: "Num", n: d };
  if (typeof d === "string") return { tag: "Str", s: d };
  if (typeof d === "boolean") return { tag: "Bool", b: d };

  if (typeof d === "object" && d !== null && !Array.isArray(d) && "sym" in d) {
    return { tag: "Sym", name: (d as any).sym };
  }

  if (Array.isArray(d)) {
    // Reference-grade: treat as Vector. You can encode list-as-Pairs later.
    return { tag: "Vector", items: d.map(datumToVal) };
  }

  return { tag: "Str", s: JSON.stringify(d) };
}
```

---

# 161. Now Your Macro Hygiene Tests H1–H2 Are Fully Runnable

With Part 17 in place, the Part 16 macro hygiene tests are no longer placeholders.

Your two core acceptance tests:

* **H1** (introduced binder should NOT capture user identifier)
* **H2** (user binder should NOT capture macro-introduced temp)

…should both pass if and only if:

* introducer scope is applied only to introduced identifiers
* binder-scope is applied to body pre-expansion (and not retroactively applied to macro output)
* lowering resolves identifiers to bid-derived internal names

If any of those invariants are violated, H1 or H2 fails immediately.

---

# 162. Extend to H3–H5 (Optional but Strongly Recommended)

Now that you have the full pipeline, you can add:

## H3 — Macro expands into binding form that binds introduced references

Example: a macro that expands into a `let` that introduces a binder and uses it.

## H4 — Macro expands into another macro invocation (macro chaining)

Example: `m1` expands into `(m2 ...)` and ensure hygiene remains stable.

## H5 — Shadowing of primitives does not hijack macro-introduced primitives

The classic check:

```scheme
(begin
  (define-syntax m (syntax-rules () ((_ x) (+ x 1))))
  (let ((+ (lambda (a b) 999)))
    (m 2)))
;; must be 3, not 999
```

Under this Part 17 expander design, **this will pass**, because:

* `let` binder scope is applied to body *before* macro expansion,
* macro-introduced `+` does not receive the binder scope,
* thus it resolves to def-site primitive binding.

---

# 163. Continue

If you say **continue**, Part 18 will take this from “tests pass” to “language completeness”:

* implement `let*`, `letrec`, and **scan-out-defines** (internal `define` desugaring) in the expander (SICP Chapter 4 discipline)
* implement `cond`, `and`, `or`, `when`, `unless` as **standard library macros** (written in Ω) using your `syntax-rules`
* add a minimal module system (`module`, `require`, `provide`, `require-for-syntax`) plus phase separation (Env[p])
* and show how to **self-host the expander** using Ω₀ boot modules and differential tests
