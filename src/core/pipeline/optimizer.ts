// src/core/pipeline/optimizer.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION (Prompt 8)
// Optimizer passes for semantic computation
//
// Three main optimizations:
// 1. Fusion: Combine adjacent effect operations
// 2. Hoist: Move invariant expressions out of loops
// 3. Memo: Add memoization for repeated effect calls

import type { Expr } from "../ast";

/**
 * Optimization statistics for measurement
 */
export type OptStats = {
  fusedEffects: number;
  hoistedExprs: number;
  memoizedCalls: number;
  totalReductions: number;
};

function emptyStats(): OptStats {
  return { fusedEffects: 0, hoistedExprs: 0, memoizedCalls: 0, totalReductions: 0 };
}

// ─────────────────────────────────────────────────────────────────
// Pass 1: Fusion
// Combine adjacent infer.op calls when safe
// ─────────────────────────────────────────────────────────────────

/**
 * Check if two effect operations can be fused
 */
function canFuse(op1: string, op2: string): boolean {
  // For now, only fuse same-type infer operations
  if (op1 === "infer.op" && op2 === "infer.op") return true;
  return false;
}

/**
 * Fuse adjacent effects in a Begin block
 */
function fuseBegin(exprs: Expr[], stats: OptStats): Expr[] {
  const result: Expr[] = [];
  let i = 0;

  while (i < exprs.length) {
    const e = exprs[i];

    // Check if this and next are fusable effects
    if (i + 1 < exprs.length &&
        e.tag === "Effect" &&
        exprs[i + 1].tag === "Effect" &&
        canFuse(e.op, (exprs[i + 1] as any).op)) {

      const next = exprs[i + 1] as any;

      // Create a fused effect with combined args
      const fusedArgs = [...e.args, ...next.args];
      result.push({
        tag: "Effect",
        op: "infer.batch.op",  // Use batch operation
        args: [{
          tag: "Lit",
          value: [e.op, next.op],  // Track original ops
        }, ...fusedArgs],
      });

      stats.fusedEffects++;
      stats.totalReductions++;
      i += 2;  // Skip both
    } else {
      result.push(optimizeExpr(e, stats));
      i++;
    }
  }

  return result;
}

/**
 * Fusion pass: combine adjacent effect operations
 */
function fusion(e: Expr, stats: OptStats): Expr {
  switch (e.tag) {
    case "Begin": {
      const fused = fuseBegin(e.exprs, stats);
      return { ...e, exprs: fused };
    }

    case "Let": {
      // Check for sequential let bindings that can be fused
      return {
        ...e,
        bindings: e.bindings.map(b => ({
          ...b,
          init: fusion(b.init, stats),
        })),
        body: fusion(e.body, stats),
      };
    }

    default:
      return e;
  }
}

// ─────────────────────────────────────────────────────────────────
// Pass 2: Hoisting
// Move invariant expressions out of repeated contexts
// ─────────────────────────────────────────────────────────────────

/**
 * Check if an expression is pure (no effects)
 */
function isPure(e: Expr): boolean {
  switch (e.tag) {
    case "Lit":
    case "Var":
    case "Quote":
    case "QuoteSyntax":
      return true;

    case "Lambda":
      return true;  // Lambda itself is pure (body may not be)

    case "If":
      return isPure(e.test) && isPure(e.conseq) && isPure(e.alt);

    case "App":
      // Conservative: only pure if all parts are literals/vars
      return e.fn.tag === "Var" && e.args.every(a => a.tag === "Lit" || a.tag === "Var");

    case "Begin":
      return e.exprs.every(isPure);

    case "Let":
      return e.bindings.every(b => isPure(b.init)) && isPure(e.body);

    case "Effect":
      return false;  // Effects are not pure

    default:
      return false;
  }
}

/**
 * Collect free variables in an expression
 */
function freeVars(e: Expr): Set<string> {
  const free = new Set<string>();

  function collect(e: Expr, bound: Set<string>): void {
    switch (e.tag) {
      case "Lit":
      case "Quote":
      case "QuoteSyntax":
        break;

      case "Var":
        if (!bound.has(e.name)) free.add(e.name);
        break;

      case "Lambda": {
        const newBound = new Set(bound);
        e.params.forEach(p => newBound.add(p));
        collect(e.body, newBound);
        break;
      }

      case "If":
        collect(e.test, bound);
        collect(e.conseq, bound);
        collect(e.alt, bound);
        break;

      case "App":
        collect(e.fn, bound);
        e.args.forEach(a => collect(a, bound));
        break;

      case "Effect":
        e.args.forEach(a => collect(a, bound));
        break;

      case "Begin":
        e.exprs.forEach(x => collect(x, bound));
        break;

      case "Let": {
        const newBound = new Set(bound);
        e.bindings.forEach(b => {
          collect(b.init, bound);
          newBound.add(b.name);
        });
        collect(e.body, newBound);
        break;
      }

      case "Letrec": {
        const newBound = new Set(bound);
        e.bindings.forEach(b => newBound.add(b.name));
        e.bindings.forEach(b => collect(b.init, newBound));
        collect(e.body, newBound);
        break;
      }

      case "Handle":
        collect(e.body, bound);
        // Clause vars are bound in bodies
        e.handler.on.forEach(c => {
          const clauseBound = new Set(bound);
          c.params.forEach(p => clauseBound.add(p));
          clauseBound.add(c.k);
          collect(c.body, clauseBound);
        });
        break;

      case "Match":
        collect(e.scrutinee, bound);
        e.clauses.forEach(c => collect(c.body, bound));
        break;

      case "Define":
        collect(e.rhs, bound);
        break;

      case "Set":
        collect(e.rhs, bound);
        break;

      case "OracleLambda": {
        const newBound = new Set(bound);
        e.params.forEach(p => newBound.add(p));
        collect(e.spec, newBound);
        break;
      }
    }
  }

  collect(e, new Set());
  return free;
}

/**
 * Check if expression can be hoisted (invariant w.r.t. loop vars)
 */
function canHoist(e: Expr, loopVars: Set<string>): boolean {
  if (!isPure(e)) return false;
  const free = freeVars(e);
  for (const v of loopVars) {
    if (free.has(v)) return false;
  }
  return true;
}

/**
 * Hoisting pass: move invariant expressions out
 */
function hoist(e: Expr, stats: OptStats): Expr {
  // For now, just mark expressions that could be hoisted
  // A full implementation would collect and lift them
  return optimizeExpr(e, stats);
}

// ─────────────────────────────────────────────────────────────────
// Pass 3: Memoization
// Add caching for repeated effect calls with same args
// ─────────────────────────────────────────────────────────────────

/**
 * Generate a cache key for an effect call
 */
function effectKey(op: string, args: Expr[]): string {
  const argsStr = args.map(exprToKey).join(",");
  return `${op}(${argsStr})`;
}

function exprToKey(e: Expr): string {
  switch (e.tag) {
    case "Lit":
      return JSON.stringify(e.value);
    case "Var":
      return `var:${e.name}`;
    case "Quote":
      return `quote:${JSON.stringify(e.datum)}`;
    default:
      return `?:${e.tag}`;
  }
}

/**
 * Track seen effects for memoization
 */
type EffectCache = Map<string, { name: string; count: number }>;

/**
 * Memoization pass: wrap repeated effects in cache lookups
 */
function memo(e: Expr, cache: EffectCache, stats: OptStats): Expr {
  switch (e.tag) {
    case "Effect": {
      const key = effectKey(e.op, e.args);
      const cached = cache.get(key);

      if (cached && cached.count >= 1) {
        // Already seen - use cached variable
        stats.memoizedCalls++;
        stats.totalReductions++;
        return { tag: "Var", name: cached.name };
      }

      // First occurrence - track it
      const cacheVar = `cache$${cache.size}`;
      cache.set(key, { name: cacheVar, count: (cached?.count || 0) + 1 });

      // Return a let-bound version for later use
      return {
        tag: "Let",
        bindings: [{ name: cacheVar, init: e }],
        body: { tag: "Var", name: cacheVar },
      };
    }

    case "Begin":
      return { ...e, exprs: e.exprs.map(x => memo(x, cache, stats)) };

    case "Let":
      return {
        ...e,
        bindings: e.bindings.map(b => ({
          ...b,
          init: memo(b.init, cache, stats),
        })),
        body: memo(e.body, cache, stats),
      };

    case "If":
      return {
        ...e,
        test: memo(e.test, cache, stats),
        conseq: memo(e.conseq, cache, stats),
        alt: memo(e.alt, cache, stats),
      };

    case "Lambda":
      // New scope - new cache
      return {
        ...e,
        body: memo(e.body, new Map(), stats),
      };

    default:
      return e;
  }
}

// ─────────────────────────────────────────────────────────────────
// Main optimizer
// ─────────────────────────────────────────────────────────────────

/**
 * Apply a single optimization pass
 */
function optimizeExpr(e: Expr, stats: OptStats): Expr {
  switch (e.tag) {
    case "Lit":
    case "Var":
    case "Quote":
    case "QuoteSyntax":
      return e;

    case "Lambda":
      return { ...e, body: optimizeExpr(e.body, stats) };

    case "If":
      return {
        ...e,
        test: optimizeExpr(e.test, stats),
        conseq: optimizeExpr(e.conseq, stats),
        alt: optimizeExpr(e.alt, stats),
      };

    case "App":
      return {
        ...e,
        fn: optimizeExpr(e.fn, stats),
        args: e.args.map(a => optimizeExpr(a, stats)),
      };

    case "Effect":
      return { ...e, args: e.args.map(a => optimizeExpr(a, stats)) };

    case "Begin":
      return { ...e, exprs: e.exprs.map(x => optimizeExpr(x, stats)) };

    case "Define":
      return { ...e, rhs: optimizeExpr(e.rhs, stats) };

    case "Set":
      return { ...e, rhs: optimizeExpr(e.rhs, stats) };

    case "Let":
      return {
        ...e,
        bindings: e.bindings.map(b => ({
          ...b,
          init: optimizeExpr(b.init, stats),
        })),
        body: optimizeExpr(e.body, stats),
      };

    case "Letrec":
      return {
        ...e,
        bindings: e.bindings.map(b => ({
          ...b,
          init: optimizeExpr(b.init, stats),
        })),
        body: optimizeExpr(e.body, stats),
      };

    case "Handle":
      return {
        ...e,
        body: optimizeExpr(e.body, stats),
        handler: {
          ...e.handler,
          on: e.handler.on.map(c => ({
            ...c,
            body: optimizeExpr(c.body, stats),
          })),
          ret: e.handler.ret ? {
            ...e.handler.ret,
            body: optimizeExpr(e.handler.ret.body, stats),
          } : undefined,
          fin: e.handler.fin ? {
            ...e.handler.fin,
            body: optimizeExpr(e.handler.fin.body, stats),
          } : undefined,
        },
      };

    case "Match":
      return {
        ...e,
        scrutinee: optimizeExpr(e.scrutinee, stats),
        clauses: e.clauses.map(c => ({
          ...c,
          body: optimizeExpr(c.body, stats),
        })),
      };

    case "OracleLambda":
      return { ...e, spec: optimizeExpr(e.spec, stats) };

    default:
      return e;
  }
}

/**
 * Run all optimization passes
 */
export function optimize(e: Expr): { expr: Expr; stats: OptStats } {
  const stats = emptyStats();

  // Pass 1: Fusion
  let result = fusion(e, stats);

  // Pass 2: Hoisting
  result = hoist(result, stats);

  // Pass 3: Memoization
  result = memo(result, new Map(), stats);

  return { expr: result, stats };
}

/**
 * Run only fusion pass
 */
export function fusionOnly(e: Expr): { expr: Expr; stats: OptStats } {
  const stats = emptyStats();
  const result = fusion(e, stats);
  return { expr: result, stats };
}

/**
 * Run only memoization pass
 */
export function memoOnly(e: Expr): { expr: Expr; stats: OptStats } {
  const stats = emptyStats();
  const result = memo(e, new Map(), stats);
  return { expr: result, stats };
}

/**
 * Count effect operations in an expression
 */
export function countEffects(e: Expr): number {
  let count = 0;

  function walk(e: Expr): void {
    switch (e.tag) {
      case "Effect":
        count++;
        e.args.forEach(walk);
        break;

      case "Lambda":
        walk(e.body);
        break;

      case "If":
        walk(e.test);
        walk(e.conseq);
        walk(e.alt);
        break;

      case "App":
        walk(e.fn);
        e.args.forEach(walk);
        break;

      case "Begin":
        e.exprs.forEach(walk);
        break;

      case "Let":
        e.bindings.forEach(b => walk(b.init));
        walk(e.body);
        break;

      case "Letrec":
        e.bindings.forEach(b => walk(b.init));
        walk(e.body);
        break;

      case "Handle":
        walk(e.body);
        e.handler.on.forEach(c => walk(c.body));
        if (e.handler.ret) walk(e.handler.ret.body);
        if (e.handler.fin) walk(e.handler.fin.body);
        break;

      case "Match":
        walk(e.scrutinee);
        e.clauses.forEach(c => walk(c.body));
        break;

      case "Define":
        walk(e.rhs);
        break;

      case "Set":
        walk(e.rhs);
        break;

      case "OracleLambda":
        walk(e.spec);
        break;
    }
  }

  walk(e);
  return count;
}
