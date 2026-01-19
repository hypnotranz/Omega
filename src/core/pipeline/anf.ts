// src/core/pipeline/anf.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION (Prompt 8)
// ANF Transform: Convert expressions to A-Normal Form
//
// In ANF:
// - All intermediate values are bound to names
// - Effect points are explicit (each effect is in tail position of a let)
// - Enables optimization passes (fusion, hoisting, memoization)

import type { Expr } from "../ast";

/**
 * Counter for generating fresh names
 */
let anfCounter = 0;
function freshVar(prefix: string = "t"): string {
  return `${prefix}$${++anfCounter}`;
}

/**
 * Reset the counter (for testing)
 */
export function resetAnfCounter(): void {
  anfCounter = 0;
}

/**
 * Check if an expression is atomic (doesn't need binding)
 * Atomic: Lit, Var, Lambda
 */
function isAtomic(e: Expr): boolean {
  return e.tag === "Lit" || e.tag === "Var" || e.tag === "Lambda";
}

/**
 * Convert an expression to ANF.
 * Returns a pair: [bindings, result]
 * where bindings is a list of (name, expr) pairs
 * and result is an atomic expression
 */
type Binding = { name: string; expr: Expr };

function anfExpr(e: Expr): { bindings: Binding[]; result: Expr } {
  switch (e.tag) {
    case "Lit":
    case "Var":
      return { bindings: [], result: e };

    case "Lambda": {
      // Transform body but keep lambda atomic
      const body = anfToExpr(anfExpr(e.body));
      return {
        bindings: [],
        result: { ...e, body },
      };
    }

    case "If": {
      // Transform test, but branches stay as expressions
      const testAnf = anfExpr(e.test);
      const conseq = anfToExpr(anfExpr(e.conseq));
      const alt = anfToExpr(anfExpr(e.alt));

      if (isAtomic(testAnf.result)) {
        return {
          bindings: testAnf.bindings,
          result: { ...e, test: testAnf.result, conseq, alt },
        };
      }

      // Bind test to a variable
      const testVar = freshVar("test");
      return {
        bindings: [
          ...testAnf.bindings,
          { name: testVar, expr: testAnf.result },
        ],
        result: { ...e, test: { tag: "Var", name: testVar }, conseq, alt },
      };
    }

    case "App": {
      // Transform fn and all args
      const fnAnf = anfExpr(e.fn);
      const argsAnf = e.args.map(anfExpr);

      const bindings: Binding[] = [...fnAnf.bindings];

      // Bind fn if not atomic
      let fnResult = fnAnf.result;
      if (!isAtomic(fnResult)) {
        const fnVar = freshVar("fn");
        bindings.push({ name: fnVar, expr: fnResult });
        fnResult = { tag: "Var", name: fnVar };
      }

      // Bind args if not atomic
      const argResults: Expr[] = [];
      for (const argAnf of argsAnf) {
        bindings.push(...argAnf.bindings);
        if (isAtomic(argAnf.result)) {
          argResults.push(argAnf.result);
        } else {
          const argVar = freshVar("arg");
          bindings.push({ name: argVar, expr: argAnf.result });
          argResults.push({ tag: "Var", name: argVar });
        }
      }

      // The application itself may need binding if used in complex context
      const appExpr: Expr = { tag: "App", fn: fnResult, args: argResults };
      return { bindings, result: appExpr };
    }

    case "Effect": {
      // Transform args
      const argsAnf = e.args.map(anfExpr);

      const bindings: Binding[] = [];
      const argResults: Expr[] = [];

      for (const argAnf of argsAnf) {
        bindings.push(...argAnf.bindings);
        if (isAtomic(argAnf.result)) {
          argResults.push(argAnf.result);
        } else {
          const argVar = freshVar("arg");
          bindings.push({ name: argVar, expr: argAnf.result });
          argResults.push({ tag: "Var", name: argVar });
        }
      }

      const effectExpr: Expr = { tag: "Effect", op: e.op, args: argResults };
      return { bindings, result: effectExpr };
    }

    case "Begin": {
      // Transform each expression in sequence
      const exprsAnf = e.exprs.map(anfExpr);

      const allBindings: Binding[] = [];
      const results: Expr[] = [];

      for (const exprAnf of exprsAnf) {
        allBindings.push(...exprAnf.bindings);
        results.push(exprAnf.result);
      }

      // Begin with transformed expressions
      return {
        bindings: allBindings,
        result: { tag: "Begin", exprs: results },
      };
    }

    case "Define": {
      // Transform RHS
      const rhsAnf = anfExpr(e.rhs);
      const rhs = anfToExpr(rhsAnf);
      return {
        bindings: [],
        result: { ...e, rhs },
      };
    }

    case "Set": {
      // Transform RHS
      const rhsAnf = anfExpr(e.rhs);
      const bindings = [...rhsAnf.bindings];
      let rhs = rhsAnf.result;

      if (!isAtomic(rhs)) {
        const rhsVar = freshVar("rhs");
        bindings.push({ name: rhsVar, expr: rhs });
        rhs = { tag: "Var", name: rhsVar };
      }

      return {
        bindings,
        result: { ...e, rhs },
      };
    }

    case "Handle": {
      // Transform body and clause bodies
      const body = anfToExpr(anfExpr(e.body));
      const handler = {
        ...e.handler,
        on: e.handler.on.map(clause => ({
          ...clause,
          body: anfToExpr(anfExpr(clause.body)),
        })),
        ret: e.handler.ret ? {
          ...e.handler.ret,
          body: anfToExpr(anfExpr(e.handler.ret.body)),
        } : undefined,
        fin: e.handler.fin ? {
          ...e.handler.fin,
          body: anfToExpr(anfExpr(e.handler.fin.body)),
        } : undefined,
      };

      return {
        bindings: [],
        result: { ...e, body, handler },
      };
    }

    case "Let": {
      // Transform bindings and body
      const bindingsAnf = e.bindings.map(b => ({
        name: b.name,
        init: anfToExpr(anfExpr(b.init)),
      }));
      const body = anfToExpr(anfExpr(e.body));

      return {
        bindings: [],
        result: { ...e, bindings: bindingsAnf, body },
      };
    }

    case "Letrec": {
      // Transform bindings and body
      const bindingsAnf = e.bindings.map(b => ({
        name: b.name,
        init: anfToExpr(anfExpr(b.init)),
      }));
      const body = anfToExpr(anfExpr(e.body));

      return {
        bindings: [],
        result: { ...e, bindings: bindingsAnf, body },
      };
    }

    case "Match": {
      // Transform scrutinee and clause bodies
      const scrutineeAnf = anfExpr(e.scrutinee);
      const bindings = [...scrutineeAnf.bindings];

      let scrutinee = scrutineeAnf.result;
      if (!isAtomic(scrutinee)) {
        const scrutVar = freshVar("scrut");
        bindings.push({ name: scrutVar, expr: scrutinee });
        scrutinee = { tag: "Var", name: scrutVar };
      }

      const clauses = e.clauses.map(c => ({
        ...c,
        body: anfToExpr(anfExpr(c.body)),
      }));

      return {
        bindings,
        result: { ...e, scrutinee, clauses },
      };
    }

    case "OracleLambda": {
      // Transform spec
      const spec = anfToExpr(anfExpr(e.spec));
      return {
        bindings: [],
        result: { ...e, spec },
      };
    }

    case "Quote":
    case "QuoteSyntax":
      return { bindings: [], result: e };

    default:
      // Unknown expression type - return as-is
      return { bindings: [], result: e };
  }
}

/**
 * Convert ANF result back to expression with let bindings
 */
function anfToExpr(anf: { bindings: Binding[]; result: Expr }): Expr {
  if (anf.bindings.length === 0) {
    return anf.result;
  }

  // Wrap in nested lets
  let result = anf.result;
  for (let i = anf.bindings.length - 1; i >= 0; i--) {
    const b = anf.bindings[i];
    result = {
      tag: "Let",
      bindings: [{ name: b.name, init: b.expr }],
      body: result,
    };
  }

  return result;
}

/**
 * Transform an expression to ANF
 */
export function toAnf(e: Expr): Expr {
  resetAnfCounter();
  return anfToExpr(anfExpr(e));
}

/**
 * Check if an expression is in ANF
 * (useful for testing)
 */
export function isAnf(e: Expr): boolean {
  switch (e.tag) {
    case "Lit":
    case "Var":
    case "Quote":
    case "QuoteSyntax":
      return true;

    case "Lambda":
      return isAnf(e.body);

    case "If":
      return isAtomic(e.test) && isAnf(e.conseq) && isAnf(e.alt);

    case "App":
      return isAtomic(e.fn) && e.args.every(isAtomic);

    case "Effect":
      return e.args.every(isAtomic);

    case "Begin":
      return e.exprs.every(isAnf);

    case "Define":
      return isAnf(e.rhs);

    case "Set":
      return isAtomic(e.rhs);

    case "Let":
      return e.bindings.every(b => isAnf(b.init)) && isAnf(e.body);

    case "Letrec":
      return e.bindings.every(b => isAnf(b.init)) && isAnf(e.body);

    case "Handle":
      return isAnf(e.body);

    case "Match":
      return isAtomic(e.scrutinee) && e.clauses.every(c => isAnf(c.body));

    case "OracleLambda":
      return isAnf(e.spec);

    default:
      return false;
  }
}
