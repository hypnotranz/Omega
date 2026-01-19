// src/core/compiler/anf.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-17.md
// Prompt 17: ANF (A-Normal Form) conversion from Core AST

import type { Expr, Pattern, HandlerExpr } from "../ast";
import type { Val } from "../eval/values";
import { VUnit, VTrue, VFalse } from "../eval/values";
import type {
  ANFAtom,
  ANFPrim,
  ANFExpr,
  ANFProgram,
  ANFPattern,
  ANFHandler,
  ANFMatchClause,
  SourceLocation,
  SourceMap,
  SourceMapEntry,
} from "./types";

// ─────────────────────────────────────────────────────────────────
// ANF Conversion Context
// ─────────────────────────────────────────────────────────────────

/**
 * Context for ANF conversion, tracking fresh names and source locations.
 */
type ANFContext = {
  /** Counter for fresh variable names */
  counter: number;
  /** Source map entries being built */
  sourceMapEntries: SourceMapEntry[];
  /** Current source location */
  currentLoc?: SourceLocation;
  /** Free variables encountered */
  freeVars: Set<string>;
  /** Bound variables in current scope */
  boundVars: Set<string>;
};

/**
 * Create a fresh ANF context.
 */
function createANFContext(): ANFContext {
  return {
    counter: 0,
    sourceMapEntries: [],
    freeVars: new Set(),
    boundVars: new Set(),
  };
}

/**
 * Generate a fresh variable name.
 */
function freshName(ctx: ANFContext, prefix: string = "t"): string {
  return `${prefix}$${ctx.counter++}`;
}

// ─────────────────────────────────────────────────────────────────
// Value to ANF Literal
// ─────────────────────────────────────────────────────────────────

/**
 * Convert a literal value to a Val for ANF.
 */
function litToVal(value: number | string | boolean | null): Val {
  if (value === null) return VUnit;
  if (typeof value === "boolean") return value ? VTrue : VFalse;
  if (typeof value === "number") return { tag: "Num", n: value };
  if (typeof value === "string") return { tag: "Str", s: value };
  return VUnit;
}

/**
 * Convert a datum (quote) to a Val.
 */
function datumToVal(datum: unknown): Val {
  if (datum === null || datum === undefined) return VUnit;
  if (typeof datum === "boolean") return datum ? VTrue : VFalse;
  if (typeof datum === "number") return { tag: "Num", n: datum };
  if (typeof datum === "string") return { tag: "Str", s: datum };
  if (typeof datum === "symbol") {
    return { tag: "Sym", name: datum.description ?? "symbol" };
  }
  if (Array.isArray(datum)) {
    return { tag: "Vector", items: datum.map(datumToVal) };
  }
  // Fallback for objects - treat as symbol
  return { tag: "Sym", name: String(datum) };
}

// ─────────────────────────────────────────────────────────────────
// Pattern Conversion
// ─────────────────────────────────────────────────────────────────

/**
 * Convert an AST pattern to an ANF pattern.
 */
function convertPattern(pat: Pattern): ANFPattern {
  switch (pat.tag) {
    case "PWild":
      return { tag: "PWild" };
    case "PVar":
      return { tag: "PVar", name: pat.name };
    case "PLit":
      return { tag: "PLit", v: litToVal(pat.value) };
    case "PVector":
      return { tag: "PVector", items: pat.items.map(convertPattern) };
  }
}

// ─────────────────────────────────────────────────────────────────
// ANF Conversion (CPS-style transformation)
// ─────────────────────────────────────────────────────────────────

/**
 * Main ANF conversion: transform an expression, calling continuation with atom result.
 *
 * The continuation receives an ANFAtom and returns the ANFExpr to use it in.
 * This CPS-style ensures all intermediate values get let-bound.
 */
function anfConvert(
  ctx: ANFContext,
  expr: Expr,
  k: (atom: ANFAtom) => ANFExpr
): ANFExpr {
  switch (expr.tag) {
    case "Lit": {
      const atom: ANFAtom = { tag: "Lit", v: litToVal(expr.value) };
      return k(atom);
    }

    case "Var": {
      // Track free vs bound variables
      if (!ctx.boundVars.has(expr.name)) {
        ctx.freeVars.add(expr.name);
      }
      const atom: ANFAtom = { tag: "Var", name: expr.name };
      return k(atom);
    }

    case "Quote": {
      // Quote creates a literal value
      const name = freshName(ctx, "q");
      const rhs: ANFPrim = { tag: "Quote", datum: expr.datum };
      return {
        tag: "Let",
        name,
        rhs,
        body: k({ tag: "Var", name }),
        loc: ctx.currentLoc,
      };
    }

    case "Lambda": {
      const name = freshName(ctx, "fn");

      // Save bound vars and add params
      const savedBound = new Set(ctx.boundVars);
      for (const p of expr.params) {
        ctx.boundVars.add(p);
      }

      // Convert body
      const body = anfConvert(ctx, expr.body, (a) => ({ tag: "Return", v: a }));

      // Restore bound vars
      ctx.boundVars = savedBound;

      const rhs: ANFPrim = { tag: "Lambda", params: expr.params, body };
      return {
        tag: "Let",
        name,
        rhs,
        body: k({ tag: "Var", name }),
        loc: ctx.currentLoc,
      };
    }

    case "OracleLambda": {
      // Oracle lambdas become Effect calls when invoked
      // For now, convert spec and create a marker closure
      const name = freshName(ctx, "oracle");
      return anfConvert(ctx, expr.spec, (specAtom) => {
        const rhs: ANFPrim = {
          tag: "Effect",
          op: "oracle.define",
          args: [specAtom, { tag: "Lit", v: { tag: "Vector", items: expr.params.map(p => ({ tag: "Str", s: p })) } }],
        };
        return {
          tag: "Let",
          name,
          rhs,
          body: k({ tag: "Var", name }),
          loc: ctx.currentLoc,
        };
      });
    }

    case "If": {
      return anfConvert(ctx, expr.test, (testAtom) => {
        // Simplified If: both branches return directly, continuation applied to result
        const thn = anfConvert(ctx, expr.conseq, k);
        const els = anfConvert(ctx, expr.alt, k);

        return {
          tag: "If",
          test: testAtom,
          thn,
          els,
          loc: ctx.currentLoc,
        };
      });
    }

    case "Begin": {
      if (expr.exprs.length === 0) {
        return k({ tag: "Lit", v: VUnit });
      }
      if (expr.exprs.length === 1) {
        return anfConvert(ctx, expr.exprs[0], k);
      }

      // Convert first expression, ignoring result, then continue with rest
      const [first, ...rest] = expr.exprs;
      return anfConvert(ctx, first, (_atom) => {
        return anfConvert(ctx, { tag: "Begin", exprs: rest }, k);
      });
    }

    case "Define": {
      // Define is like a let that adds to environment
      ctx.boundVars.add(expr.name);
      return anfConvert(ctx, expr.rhs, (rhsAtom) => {
        // Create a binding and continue
        return {
          tag: "Let",
          name: expr.name,
          rhs: { tag: "Prim", name: "identity", args: [rhsAtom] },
          body: k({ tag: "Var", name: expr.name }),
          loc: ctx.currentLoc,
        };
      });
    }

    case "Set": {
      return anfConvert(ctx, expr.rhs, (rhsAtom) => {
        return {
          tag: "Set",
          name: expr.name,
          rhs: rhsAtom,
          body: k({ tag: "Lit", v: VUnit }),
          loc: ctx.currentLoc,
        };
      });
    }

    case "App": {
      // Convert function, then convert all args, then call
      return anfConvert(ctx, expr.fn, (fnAtom) => {
        return anfConvertArgs(ctx, expr.args, [], (argAtoms) => {
          const callName = freshName(ctx, "call");
          const rhs: ANFPrim = { tag: "Call", fn: fnAtom, args: argAtoms };
          return {
            tag: "Let",
            name: callName,
            rhs,
            body: k({ tag: "Var", name: callName }),
            loc: ctx.currentLoc,
          };
        });
      });
    }

    case "Effect": {
      // Effect operations are preserved!
      return anfConvertArgs(ctx, expr.args, [], (argAtoms) => {
        const effectName = freshName(ctx, "eff");
        const rhs: ANFPrim = { tag: "Effect", op: expr.op, args: argAtoms };
        return {
          tag: "Let",
          name: effectName,
          rhs,
          body: k({ tag: "Var", name: effectName }),
          loc: ctx.currentLoc,
        };
      });
    }

    case "Handle": {
      const bodyExpr = anfConvert(ctx, expr.body, (a) => ({ tag: "Return", v: a }));
      const handler = convertHandler(ctx, expr.handler);

      const handleName = freshName(ctx, "handle");
      const rhs: ANFPrim = { tag: "Handle", body: bodyExpr, handler };
      return {
        tag: "Let",
        name: handleName,
        rhs,
        body: k({ tag: "Var", name: handleName }),
        loc: ctx.currentLoc,
      };
    }

    case "Match": {
      return anfConvert(ctx, expr.scrutinee, (scrutAtom) => {
        const clauses: ANFMatchClause[] = expr.clauses.map((c) => {
          // Add pattern variables to scope
          const savedBound = new Set(ctx.boundVars);
          addPatternVars(ctx, c.pat);

          const body = anfConvert(ctx, c.body, (a) => ({ tag: "Return", v: a }));

          ctx.boundVars = savedBound;

          return {
            pat: convertPattern(c.pat),
            body,
          };
        });

        const matchName = freshName(ctx, "match");
        const rhs: ANFPrim = { tag: "Match", scrut: scrutAtom, clauses };
        return {
          tag: "Let",
          name: matchName,
          rhs,
          body: k({ tag: "Var", name: matchName }),
          loc: ctx.currentLoc,
        };
      });
    }
  }
}

/**
 * Convert a list of argument expressions to ANF atoms.
 */
function anfConvertArgs(
  ctx: ANFContext,
  args: Expr[],
  acc: ANFAtom[],
  k: (atoms: ANFAtom[]) => ANFExpr
): ANFExpr {
  if (args.length === 0) {
    return k(acc);
  }

  const [first, ...rest] = args;
  return anfConvert(ctx, first, (atom) => {
    return anfConvertArgs(ctx, rest, [...acc, atom], k);
  });
}

/**
 * Convert handler expression to ANF handler.
 */
function convertHandler(ctx: ANFContext, handler: HandlerExpr): ANFHandler {
  const anfHandler: ANFHandler = {
    on: handler.on.map((clause) => {
      // Add params and k to scope
      const savedBound = new Set(ctx.boundVars);
      for (const p of clause.params) {
        ctx.boundVars.add(p);
      }
      ctx.boundVars.add(clause.k);

      const body = anfConvert(ctx, clause.body, (a) => ({ tag: "Return", v: a }));

      ctx.boundVars = savedBound;

      return {
        op: clause.op,
        params: clause.params,
        k: clause.k,
        body,
      };
    }),
  };

  if (handler.ret) {
    const savedBound = new Set(ctx.boundVars);
    ctx.boundVars.add(handler.ret.v);
    anfHandler.ret = {
      v: handler.ret.v,
      body: anfConvert(ctx, handler.ret.body, (a) => ({ tag: "Return", v: a })),
    };
    ctx.boundVars = savedBound;
  }

  if (handler.fin) {
    anfHandler.fin = {
      body: anfConvert(ctx, handler.fin.body, (a) => ({ tag: "Return", v: a })),
    };
  }

  return anfHandler;
}

/**
 * Add pattern variables to bound vars.
 */
function addPatternVars(ctx: ANFContext, pat: Pattern): void {
  switch (pat.tag) {
    case "PWild":
    case "PLit":
      break;
    case "PVar":
      ctx.boundVars.add(pat.name);
      break;
    case "PVector":
      for (const item of pat.items) {
        addPatternVars(ctx, item);
      }
      break;
  }
}

// ─────────────────────────────────────────────────────────────────
// Public API
// ─────────────────────────────────────────────────────────────────

/**
 * Convert a Core AST expression to ANF.
 */
export function toANF(expr: Expr, sourceLabel?: string): ANFProgram {
  const ctx = createANFContext();

  if (sourceLabel) {
    ctx.currentLoc = { source: sourceLabel, line: 1, column: 1 };
  }

  const body = anfConvert(ctx, expr, (a) => ({ tag: "Return", v: a }));

  return {
    body,
    freeVars: Array.from(ctx.freeVars),
  };
}

/**
 * Create source map from ANF context.
 */
export function createSourceMap(entries: SourceMapEntry[]): SourceMap {
  return { entries };
}

// ─────────────────────────────────────────────────────────────────
// ANF Utilities
// ─────────────────────────────────────────────────────────────────

/**
 * Count the number of let bindings in an ANF expression.
 */
export function countBindings(expr: ANFExpr): number {
  switch (expr.tag) {
    case "Return":
      return 0;
    case "Let":
      return 1 + countBindings(expr.body);
    case "LetRec":
      return expr.bindings.length + countBindings(expr.body);
    case "If":
      return countBindings(expr.thn) + countBindings(expr.els);
    case "Seq":
      return countBindings(expr.first) + countBindings(expr.second);
    case "Set":
      return countBindings(expr.body);
  }
}

/**
 * Count effect operations in an ANF expression.
 */
export function countEffects(expr: ANFExpr): number {
  let count = 0;

  function visitPrim(prim: ANFPrim): void {
    if (prim.tag === "Effect") {
      count++;
    } else if (prim.tag === "Lambda") {
      visitExpr(prim.body);
    } else if (prim.tag === "Handle") {
      visitExpr(prim.body);
      for (const clause of prim.handler.on) {
        visitExpr(clause.body);
      }
      if (prim.handler.ret) visitExpr(prim.handler.ret.body);
      if (prim.handler.fin) visitExpr(prim.handler.fin.body);
    } else if (prim.tag === "Match") {
      for (const clause of prim.clauses) {
        visitExpr(clause.body);
      }
    }
  }

  function visitExpr(e: ANFExpr): void {
    switch (e.tag) {
      case "Return":
        break;
      case "Let":
        visitPrim(e.rhs);
        visitExpr(e.body);
        break;
      case "LetRec":
        for (const b of e.bindings) {
          visitPrim(b.rhs);
        }
        visitExpr(e.body);
        break;
      case "If":
        visitExpr(e.thn);
        visitExpr(e.els);
        break;
      case "Seq":
        visitExpr(e.first);
        visitExpr(e.second);
        break;
      case "Set":
        visitExpr(e.body);
        break;
    }
  }

  visitExpr(expr);
  return count;
}

/**
 * Find all effect operation names in an ANF expression.
 */
export function findEffectOps(expr: ANFExpr): Set<string> {
  const ops = new Set<string>();

  function visitPrim(prim: ANFPrim): void {
    if (prim.tag === "Effect") {
      ops.add(prim.op);
    } else if (prim.tag === "Lambda") {
      visitExpr(prim.body);
    } else if (prim.tag === "Handle") {
      visitExpr(prim.body);
      for (const clause of prim.handler.on) {
        visitExpr(clause.body);
      }
      if (prim.handler.ret) visitExpr(prim.handler.ret.body);
      if (prim.handler.fin) visitExpr(prim.handler.fin.body);
    } else if (prim.tag === "Match") {
      for (const clause of prim.clauses) {
        visitExpr(clause.body);
      }
    }
  }

  function visitExpr(e: ANFExpr): void {
    switch (e.tag) {
      case "Return":
        break;
      case "Let":
        visitPrim(e.rhs);
        visitExpr(e.body);
        break;
      case "LetRec":
        for (const b of e.bindings) {
          visitPrim(b.rhs);
        }
        visitExpr(e.body);
        break;
      case "If":
        visitExpr(e.thn);
        visitExpr(e.els);
        break;
      case "Seq":
        visitExpr(e.first);
        visitExpr(e.second);
        break;
      case "Set":
        visitExpr(e.body);
        break;
    }
  }

  visitExpr(expr);
  return ops;
}

/**
 * Pretty-print an ANF expression (for debugging).
 */
export function anfToString(expr: ANFExpr, indent: number = 0): string {
  const pad = "  ".repeat(indent);

  function atomStr(a: ANFAtom): string {
    return a.tag === "Lit" ? JSON.stringify(a.v) : a.name;
  }

  function primStr(p: ANFPrim): string {
    switch (p.tag) {
      case "Lambda":
        return `(lambda (${p.params.join(" ")}) ...)`;
      case "Call":
        return `(${atomStr(p.fn)} ${p.args.map(atomStr).join(" ")})`;
      case "Effect":
        return `(effect ${p.op} ${p.args.map(atomStr).join(" ")})`;
      case "Quote":
        return `(quote ${JSON.stringify(p.datum)})`;
      case "Prim":
        return `(prim ${p.name} ${p.args.map(atomStr).join(" ")})`;
      case "Match":
        return `(match ${atomStr(p.scrut)} ...)`;
      case "Handle":
        return `(handle ...)`;
      case "MakeClosure":
        return `(make-closure ...)`;
    }
  }

  switch (expr.tag) {
    case "Return":
      return `${pad}(return ${atomStr(expr.v)})`;
    case "Let":
      return `${pad}(let ${expr.name} = ${primStr(expr.rhs)}\n${anfToString(expr.body, indent + 1)})`;
    case "LetRec":
      const binds = expr.bindings.map(b => `${b.name} = ${primStr(b.rhs)}`).join(", ");
      return `${pad}(letrec [${binds}]\n${anfToString(expr.body, indent + 1)})`;
    case "If":
      return `${pad}(if ${atomStr(expr.test)}\n${anfToString(expr.thn, indent + 1)}\n${anfToString(expr.els, indent + 1)})`;
    case "Seq":
      return `${pad}(seq\n${anfToString(expr.first, indent + 1)}\n${anfToString(expr.second, indent + 1)})`;
    case "Set":
      return `${pad}(set! ${expr.name} ${atomStr(expr.rhs)}\n${anfToString(expr.body, indent + 1)})`;
  }
}
