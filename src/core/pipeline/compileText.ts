// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-5.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import { tokenize } from "../reader/tokenize";
import { parseAll } from "../reader/parse";
import { datumToSyntax } from "../reader/toSyntax";
import type { Syntax, SIdent, SList } from "../syntax/syntax";
import { isIdent, isList, addScope, freshScope } from "../syntax/syntax";
import type { Binding, Env } from "../syntax/binding";
import { resolveIdent } from "../syntax/binding";
import type { Expr, HandlerExpr } from "../ast";
import { compileSyntaxRules, applySyntaxRules, type SRRule, type SRTransformer } from "../expand/syntaxRules";
import { lowerSyntax } from "./lower";
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
 * This MUST align with installPrims() in src/core/prims.ts
 */
function initialEnv(moduleScope: string): Env {
  // All 117 primitives from src/core/prims.ts
  const prims = [
    // Arithmetic
    "+", "-", "*", "/", "modulo",
    // Comparison
    "=", "<", ">", "<=", ">=",
    // Logic
    "not",
    // Special values
    "unit", "*uninit*",
    // Continuations
    "call/cc", "call-with-prompt", "abort-to-prompt",
    // List operations
    "cons", "car", "cdr", "null?", "pair?", "list", "append", "length", "reverse", "list-ref",
    "cadr", "caddr",
    // Equality
    "eq?", "equal?",
    // Type predicates
    "symbol?", "string?", "number?", "boolean?", "procedure?",
    // String operations
    "string=?", "string-contains?", "string-replace-all", "string-split", "string-join",
    "string-trim", "string-downcase", "string-upcase", "string-length", "string-append", "substring",
    // Higher-order functions
    "map", "filter", "fold", "foldr", "andmap", "ormap",
    "apply", "compose", "partial", "pipe", "identity", "constantly",
    // Distribution operations
    "dist", "dist?", "dist-count", "dist-value-at", "dist-weight-at", "dist-normalize",
    "dist-sample", "dist-topk", "dist-from-list", "dist-to-list",
    // Stream operations
    "the-empty-stream", "stream-null?", "stream-car", "stream-cdr",
    "stream-map", "stream-filter", "stream-take", "stream->list", "list->stream",
    // Promise/delay operations
    "make-promise", "force", "promise?", "promise-forced?",
    // Generic dispatch / tagged data
    "attach-tag", "type-tag", "contents", "tagged?",
    "make-op-table", "op-table?", "op-table-get", "op-table-put",
    "apply-generic", "apply-generic-coerced",
    "make-coercion-table", "coercion-table?", "get-coercion", "put-coercion",
    "find-coercion-path", "find-all-coercion-paths", "coerce-value",
    // Pattern matching
    "match-pattern", "substitute-template",
    // Rule/rewriting
    "make-rule", "make-rule-where", "rule?",
    "rewrite-once", "rewrite-fixpoint", "rewrite-trace", "rewrite-conflicts",
    // Evidence
    "evidence-id", "verify-evidence", "evidence-stale?",
    // Machine introspection (debugging)
    "machine-new", "machine?", "machine-step", "machine-run", "machine-done?",
    "machine-value", "machine-control", "machine-stack", "machine-step-count",
    "machine-fork", "machine-resume", "machine-add-breakpoint", "machine-last-op",
  ];
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
      // headName returning non-null implies form is a list
      const r = expandDefineSyntax(form as SList, Γ, c);
      Γ = r.env;
      continue;
    }

    if (hn === "define") {
      // headName returning non-null implies form is a list
      const r = expandDefine(form as SList, Γ, c);
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

    case "oracle-lambda":
      return expandOracleLambda(stx, env, c);

    case "let":
      return expandLet(stx, env, c);

    case "letrec":
      return expandLetrec(stx, env, c);

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

  // Macro application: resolve head at phase 1 in "syntax" binding space
  const macroBinding = resolveIdent(h, env, 1, "syntax");
  if (macroBinding) {
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

function expandOracleLambda(stx: SList, env: Env, c: Counters): ExpandRes {
  // (oracle-lambda (x ...) spec)
  // Similar to lambda but params are scoped for use in spec (the prompt/goal)
  if (stx.items.length !== 3) throw new Error("oracle-lambda: expected (oracle-lambda (params) spec)");
  const paramsList = expectList(stx.items[1], "oracle-lambda: params must be list");
  const specForm = stx.items[2];

  const B = freshScope(c.scope);

  // Scope params with binder scope
  const paramsB: SIdent[] = paramsList.items.map(p => expectIdent(p, "oracle-lambda param must be ident"))
    .map(p => addScope(p, B) as SIdent);

  // Extend env with param bindings
  let Γ = env;
  for (const p of paramsB) {
    const bid = freshBid(c);
    Γ = bindValue(Γ, p, bid, internalName(p.name, bid));
  }

  // Add binder scope to spec and expand
  const specScoped = addScope(specForm, B);
  const specR = expandExpr(specScoped, Γ, c);

  const stx2: Syntax = {
    ...stx,
    items: [
      stx.items[0],
      { ...paramsList, items: paramsB },
      specR.stx,
    ],
  };

  return { stx: stx2, env: specR.env };
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

function expandLetrec(stx: SList, env: Env, c: Counters): ExpandRes {
  // (letrec ((x e) ...) body...)
  // Unlike let, bindings ARE in scope for their own init expressions (recursive)
  if (stx.items.length < 3) throw new Error("letrec: expected (letrec ((x e) ...) body...)");
  const bindsList = expectList(stx.items[1], "letrec: bindings must be list");
  const bodyForms = stx.items.slice(2);

  const B = freshScope(c.scope);

  // For letrec, we need to add bindings BEFORE expanding init expressions
  // First, collect all the identifiers and add binder scope
  const bindersWithScope: SIdent[] = [];
  let Γ = env;

  for (const bp of bindsList.items) {
    const pair = expectList(bp, "letrec: binding must be list");
    if (pair.items.length !== 2) throw new Error("letrec: binding must be (x init)");
    const id = expectIdent(pair.items[0], "letrec: binder must be ident");
    const idB = addScope(id, B) as SIdent;
    bindersWithScope.push(idB);

    // Install binding NOW (before processing inits)
    const bid = freshBid(c);
    Γ = bindValue(Γ, idB, bid, internalName(idB.name, bid));
  }

  // Now expand init expressions WITH bindings in scope
  const bindPairs: Array<{ idB: SIdent; init: Syntax }> = [];

  for (let i = 0; i < bindsList.items.length; i++) {
    const pair = expectList(bindsList.items[i], "letrec: binding must be list");
    const init0 = pair.items[1];
    // Add binder scope to init expression (so refs can see the bindings)
    const initScoped = addScope(init0, B);
    const initR = expandExpr(initScoped, Γ, c);
    Γ = initR.env;
    bindPairs.push({ idB: bindersWithScope[i], init: initR.stx });
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
