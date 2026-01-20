// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-3.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import type { Expr } from "../ast";

type Env = Map<string, string>;

type Supply = { n: number };

function fresh(s: Supply): string {
  const name = `x${s.n}`;
  s.n += 1;
  return name;
}

export function alphaNormalize(e: Expr): Expr {
  const supply: Supply = { n: 0 };
  return norm(e, new Map(), supply);
}

function norm(e: Expr, env: Env, supply: Supply): Expr {
  switch (e.tag) {
    case "Lit":
    case "Quote":
      return e;

    case "Var": {
      const n = env.get(e.name);
      return n ? { ...e, name: n } : e;
    }

    case "Lambda": {
      const env2 = new Map(env);
      const params2: string[] = [];
      for (const p of e.params) {
        const p2 = fresh(supply);
        env2.set(p, p2);
        params2.push(p2);
      }
      return { ...e, params: params2, body: norm(e.body, env2, supply) };
    }

    case "If":
      return { ...e, test: norm(e.test, env, supply), conseq: norm(e.conseq, env, supply), alt: norm(e.alt, env, supply) };

    case "Begin":
      return { ...e, exprs: e.exprs.map(x => norm(x, env, supply)) };

    case "App":
      return { ...e, fn: norm(e.fn, env, supply), args: e.args.map(a => norm(a, env, supply)) };

    case "Effect":
      return { ...e, args: e.args.map(a => norm(a, env, supply)) };

    case "Set":
      // set! targets an existing binding; rename if bound
      return { ...e, name: env.get(e.name) ?? e.name, rhs: norm(e.rhs, env, supply) };

    case "Define":
      // For alpha-eq of *expressions*, you often keep Define names stable.
      // For module-level comparisons, you may normalize top-level defines too, but be careful about exports.
      return { ...e, rhs: norm(e.rhs, env, supply) };

    case "Handle":
      // If handler contains expressions, normalize them too (depends on handler IR).
      return { ...e, body: norm(e.body, env, supply) };

    case "Match": {
      // If patterns bind vars, you need to extend env accordingly per clause.
      // Reference-grade: treat patterns as opaque; normalize bodies under same env.
      return {
        ...e,
        scrutinee: norm(e.scrutinee, env, supply),
        clauses: e.clauses.map(c => ({ ...c, body: norm(c.body, env, supply) })),
      };
    }

    case "QuoteSyntax":
      return e;

    case "Let":
    case "Letrec":
      return {
        ...e,
        bindings: e.bindings.map(b => ({ ...b, init: norm(b.init, env, supply) })),
        body: norm(e.body, env, supply),
      };

    default:
      return e;
  }
}

export function alphaEqual(a: Expr, b: Expr): boolean {
  const an = alphaNormalize(a);
  const bn = alphaNormalize(b);
  return JSON.stringify(an) === JSON.stringify(bn);
}
