// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-11.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import type { Syntax, SIdent, SList } from "../syntax/syntax";
import { isIdent, isList } from "../syntax/syntax";
import type { Env } from "../syntax/binding";
import { resolveIdent } from "../syntax/binding";
import type { Expr, HandlerExpr, Pattern } from "../ast";

function expectIdent(stx: Syntax, msg: string): SIdent {
  if (!isIdent(stx)) throw new Error(msg);
  return stx;
}
function expectList(stx: Syntax, msg: string): SList {
  if (!isList(stx)) throw new Error(msg);
  return stx;
}

function lowerVar(id: SIdent, env: Env, phase: number): Expr {
  const b = resolveIdent(id, env, phase);
  if (!b) return { tag: "Var", name: id.name };
  return { tag: "Var", name: b.value as string };
}

export function lowerSyntax(stx: Syntax, env: Env, phase = 0): Expr {
  if (stx.tag === "Atom") {
    const v = stx.value;
    if (typeof v === "number" || typeof v === "string" || typeof v === "boolean" || v === null) {
      return { tag: "Lit", value: v };
    }
    return { tag: "Lit", value: String(v) };
  }

  if (stx.tag === "Ident") return lowerVar(stx, env, phase);

  const items = stx.items;
  if (items.length === 0) return { tag: "Quote", datum: [] };

  const h = items[0];

  if (isIdent(h)) {
    switch (h.name) {
      case "quote": {
        if (items.length !== 2) throw new Error("quote: expected (quote datum)");
        return { tag: "Quote", datum: stripDatum(items[1]) };
      }

      case "begin": {
        return { tag: "Begin", exprs: items.slice(1).map(x => lowerSyntax(x, env, phase)) };
      }

      case "if": {
        if (items.length !== 4) throw new Error("if: expected (if t c a)");
        return {
          tag: "If",
          test: lowerSyntax(items[1], env, phase),
          conseq: lowerSyntax(items[2], env, phase),
          alt: lowerSyntax(items[3], env, phase),
        };
      }

      case "lambda": {
        if (items.length < 3) throw new Error("lambda: expected (lambda (params) body...)");
        const paramsList = expectList(items[1], "lambda: params must be list");
        const params = paramsList.items.map(p => {
          const id = expectIdent(p, "lambda param must be ident");
          const b = resolveIdent(id, env, phase);
          if (!b) throw new Error("lambda param did not resolve");
          return b.value as string;
        });
        const bodyForms = items.slice(2).map(x => lowerSyntax(x, env, phase));
        const body: Expr = bodyForms.length === 1 ? bodyForms[0] : { tag: "Begin", exprs: bodyForms };
        return { tag: "Lambda", params, body };
      }

      case "oracle-lambda": {
        // (oracle-lambda (params...) spec)
        // Creates an OracleProc - first-class inference procedure
        if (items.length !== 3) throw new Error("oracle-lambda: expected (oracle-lambda (params) spec)");
        const paramsList = expectList(items[1], "oracle-lambda: params must be list");
        const params = paramsList.items.map(p => {
          const id = expectIdent(p, "oracle-lambda param must be ident");
          const b = resolveIdent(id, env, phase);
          if (!b) throw new Error("oracle-lambda param did not resolve");
          return b.value as string;
        });
        const spec = lowerSyntax(items[2], env, phase);
        return { tag: "OracleLambda", params, spec };
      }

      case "let": {
        if (items.length < 3) throw new Error("let: expected (let ((x e) ...) body...)");
        const bindsList = expectList(items[1], "let: bindings must be list");
        const bindPairs = bindsList.items.map(bp0 => {
          const bp = expectList(bp0, "let: binding must be list");
          if (bp.items.length !== 2) throw new Error("let: binding must be (x init)");
          const id = expectIdent(bp.items[0], "let: binder must be ident");
          const b = resolveIdent(id, env, phase);
          if (!b) throw new Error("let binder did not resolve");
          return { internal: b.value as string, init: lowerSyntax(bp.items[1], env, phase) };
        });

        const params = bindPairs.map(p => p.internal);
        const args = bindPairs.map(p => p.init);

        const bodyForms = items.slice(2).map(x => lowerSyntax(x, env, phase));
        const body: Expr = bodyForms.length === 1 ? bodyForms[0] : { tag: "Begin", exprs: bodyForms };

        return { tag: "App", fn: { tag: "Lambda", params, body }, args };
      }

      case "letrec": {
        if (items.length < 3) throw new Error("letrec: expected (letrec ((x e) ...) body...)");
        const bindsList = expectList(items[1], "letrec: bindings must be list");

        const bindPairs = bindsList.items.map(bp0 => {
          const bp = expectList(bp0, "letrec: binding must be list");
          if (bp.items.length !== 2) throw new Error("letrec: binding must be (x init)");
          const id = expectIdent(bp.items[0], "letrec: binder must be ident");
          const b = resolveIdent(id, env, phase);
          if (!b) throw new Error("letrec binder did not resolve");
          return { internal: b.value as string, init: lowerSyntax(bp.items[1], env, phase) };
        });

        const params = bindPairs.map(p => p.internal);

        const placeholder: Expr = { tag: "App", fn: { tag: "Var", name: "*uninit*" }, args: [] };
        const args = bindPairs.map(_ => placeholder);

        const sets: Expr[] = bindPairs.map(p => ({ tag: "Set", name: p.internal, rhs: p.init }));
        const loweredBodyForms = items.slice(2).map(x => lowerSyntax(x, env, phase));
        const bodySeq = sets.concat(loweredBodyForms);
        const body: Expr = bodySeq.length === 1 ? bodySeq[0] : { tag: "Begin", exprs: bodySeq };

        return { tag: "App", fn: { tag: "Lambda", params, body }, args };
      }

      case "let*": {
        // (let* ((x1 e1) (x2 e2) ...) body...)
        // Desugars to nested let: (let ((x1 e1)) (let ((x2 e2)) ... body))
        if (items.length < 3) throw new Error("let*: expected (let* ((x e) ...) body...)");
        const bindsList = expectList(items[1], "let*: bindings must be list");
        const bodyForms = items.slice(2);

        if (bindsList.items.length === 0) {
          // Empty bindings: just evaluate body
          const loweredBody = bodyForms.map(x => lowerSyntax(x, env, phase));
          return loweredBody.length === 1 ? loweredBody[0] : { tag: "Begin", exprs: loweredBody };
        }

        // Build nested let from inside out
        let result: Expr;
        const loweredBody = bodyForms.map(x => lowerSyntax(x, env, phase));
        result = loweredBody.length === 1 ? loweredBody[0] : { tag: "Begin", exprs: loweredBody };

        for (let i = bindsList.items.length - 1; i >= 0; i--) {
          const bp = expectList(bindsList.items[i], "let*: binding must be list");
          if (bp.items.length !== 2) throw new Error("let*: binding must be (x init)");
          const id = expectIdent(bp.items[0], "let*: binder must be ident");
          const b = resolveIdent(id, env, phase);
          if (!b) throw new Error("let* binder did not resolve");
          const internal = b.value as string;
          const init = lowerSyntax(bp.items[1], env, phase);

          // Wrap in: ((lambda (x) result) init)
          result = {
            tag: "App",
            fn: { tag: "Lambda", params: [internal], body: result },
            args: [init],
          };
        }

        return result;
      }

      case "define": {
        if (items.length !== 3) throw new Error("define: expected (define x rhs)");
        const id = expectIdent(items[1], "define: name must be ident");
        const b = resolveIdent(id, env, phase);
        if (!b) throw new Error("define binder did not resolve");
        return { tag: "Define", name: b.value as string, rhs: lowerSyntax(items[2], env, phase) };
      }

      case "set!": {
        if (items.length !== 3) throw new Error("set!: expected (set! x rhs)");
        const id = expectIdent(items[1], "set!: target must be ident");
        const b = resolveIdent(id, env, phase);
        if (!b) throw new Error("set! target did not resolve");
        return { tag: "Set", name: b.value as string, rhs: lowerSyntax(items[2], env, phase) };
      }

      case "effect": {
        if (items.length < 2) throw new Error("effect: expected (effect op arg...)");
        const opStx = items[1];
        const op = opStx.tag === "Ident" ? opStx.name : String((opStx as any).value);
        return { tag: "Effect", op, args: items.slice(2).map(x => lowerSyntax(x, env, phase)) };
      }

      case "handle": {
        if (items.length < 2) throw new Error("handle: expected (handle body clauses...)");
        const body = lowerSyntax(items[1], env, phase);
      const handler = lowerHandler(items.slice(2), env, phase);
      return { tag: "Handle", body, handler };
    }

    case "handler-bind": {
      if (items.length < 3) throw new Error("handler-bind: expected (handler-bind ((type handler) ...) body...)");
      const bindsList = expectList(items[1], "handler-bind: bindings must be list");
      const handlers = bindsList.items.map(bp0 => {
        const bp = expectList(bp0, "handler-bind: binding must be list");
        if (bp.items.length !== 2) throw new Error("handler-bind: binding must be (type handler)");
        const typeId = expectIdent(bp.items[0], "handler-bind: type must be ident");
        const handlerExpr = lowerSyntax(bp.items[1], env, phase);
        return { type: typeId.name, handler: handlerExpr };
      });
      const bodyForms = items.slice(2).map(x => lowerSyntax(x, env, phase));
      const body: Expr = bodyForms.length === 1 ? bodyForms[0] : { tag: "Begin", exprs: bodyForms };
      const handlerPairs: Expr[] = handlers.map(h => ({
        tag: "App",
        fn: { tag: "Var", name: "cons" },
        args: [
          { tag: "Quote", datum: { sym: h.type } },
          h.handler,
        ],
      }));
      const handlerList: Expr = { tag: "App", fn: { tag: "Var", name: "list" }, args: handlerPairs };
      const thunk: Expr = { tag: "Lambda", params: [], body };
      return { tag: "App", fn: { tag: "Var", name: "handler-bind" }, args: [handlerList, thunk] };
    }

    case "restart-bind": {
      if (items.length < 3) throw new Error("restart-bind: expected (restart-bind ((name fn) ...) body...)");
      const bindsList = expectList(items[1], "restart-bind: bindings must be list");
      const restarts = bindsList.items.map(bp0 => {
        const bp = expectList(bp0, "restart-bind: binding must be list");
        if (bp.items.length !== 2) throw new Error("restart-bind: binding must be (name fn)");
        const nameId = expectIdent(bp.items[0], "restart-bind: name must be ident");
        const fnExpr = lowerSyntax(bp.items[1], env, phase);
        return { name: nameId.name, fn: fnExpr };
      });
      const bodyForms = items.slice(2).map(x => lowerSyntax(x, env, phase));
      const body: Expr = bodyForms.length === 1 ? bodyForms[0] : { tag: "Begin", exprs: bodyForms };
      const restartPairs: Expr[] = restarts.map(r => ({
        tag: "App",
        fn: { tag: "Var", name: "cons" },
        args: [
          { tag: "Quote", datum: { sym: r.name } },
          r.fn,
        ],
      }));
      const restartsList: Expr = { tag: "App", fn: { tag: "Var", name: "list" }, args: restartPairs };
      const thunk: Expr = { tag: "Lambda", params: [], body };
      return { tag: "App", fn: { tag: "Var", name: "restart-bind" }, args: [restartsList, thunk] };
    }

    case "match": {
      // (match scrutinee (pat1 body1) (pat2 body2) ...)
      if (items.length < 2) throw new Error("match: expected (match scrutinee clauses...)");
      const scrutinee = lowerSyntax(items[1], env, phase);
        const clauses = items.slice(2).map(cl => {
          const clauseList = expectList(cl, "match clause must be list");
          if (clauseList.items.length < 2) throw new Error("match clause must have pattern and body");
          const pat = lowerPattern(clauseList.items[0], env, phase);
          const bodyForms = clauseList.items.slice(1).map(x => lowerSyntax(x, env, phase));
          const body: Expr = bodyForms.length === 1 ? bodyForms[0] : { tag: "Begin", exprs: bodyForms };
          return { pat, body };
        });
        return { tag: "Match", scrutinee, clauses };
      }

      case "amb": {
        // (amb e1 e2 ... en) - nondeterministic choice
        // Desugar to: (effect amb.choose (list (lambda () e1) (lambda () e2) ...))
        // Each alternative is wrapped in a thunk to preserve laziness
        if (items.length < 2) throw new Error("amb: expected at least one alternative");
        const thunks: Expr[] = items.slice(1).map(alt => ({
          tag: "Lambda",
          params: [],
          body: lowerSyntax(alt, env, phase),
        }));
        // Build (list thunk1 thunk2 ...)
        const listExpr: Expr = {
          tag: "App",
          fn: { tag: "Var", name: "list" },
          args: thunks,
        };
        return { tag: "Effect", op: "amb.choose", args: [listExpr] };
      }

      case "mplus": {
        // (mplus e1 e2 ... en) - binary/variadic monadic choice (lazy)
        // Desugar to amb.choose over zero-arg thunks to avoid eager branch evaluation
        if (items.length === 1) {
          return { tag: "Effect", op: "amb.fail", args: [] };
        }
        const thunks: Expr[] = items.slice(1).map(alt => ({
          tag: "Lambda",
          params: [],
          body: lowerSyntax(alt, env, phase),
        }));
        const listExpr: Expr = { tag: "App", fn: { tag: "Var", name: "list" }, args: thunks };
        return { tag: "Effect", op: "amb.choose", args: [listExpr] };
      }

      case "require": {
        // (require pred) - fail if predicate is false
        // Desugar to: (if pred unit (effect amb.fail "require failed"))
        if (items.length !== 2) throw new Error("require: expected (require pred)");
        const pred = lowerSyntax(items[1], env, phase);
        return {
          tag: "If",
          test: pred,
          conseq: { tag: "Lit", value: null }, // unit
          alt: { tag: "Effect", op: "amb.fail", args: [{ tag: "Lit", value: "require failed" }] },
        };
      }

      case "delay": {
        // (delay expr) - create a memoizing thunk
        // Desugar to: (make-promise (lambda () expr))
        if (items.length !== 2) throw new Error("delay: expected (delay expr)");
        const body = lowerSyntax(items[1], env, phase);
        const thunk: Expr = { tag: "Lambda", params: [], body };
        return {
          tag: "App",
          fn: { tag: "Var", name: "make-promise" },
          args: [thunk],
        };
      }

      case "cons-stream": {
        // (cons-stream a b) - create stream pair with delayed tail
        // Desugar to: (cons a (delay b))
        if (items.length !== 3) throw new Error("cons-stream: expected (cons-stream head tail)");
        const head = lowerSyntax(items[1], env, phase);
        const tail = lowerSyntax(items[2], env, phase);
        const delayedTail: Expr = {
          tag: "App",
          fn: { tag: "Var", name: "make-promise" },
          args: [{ tag: "Lambda", params: [], body: tail }],
        };
        return {
          tag: "App",
          fn: { tag: "Var", name: "cons" },
          args: [head, delayedTail],
        };
      }

      case "cond": {
        // (cond (test1 e1...) (test2 e2...) ... (else en...))
        // Desugars to nested if: (if test1 (begin e1...) (if test2 (begin e2...) ...))
        const clauses = items.slice(1);
        if (clauses.length === 0) {
          // Empty cond returns #f
          return { tag: "Lit", value: false };
        }

        // Process clauses from right to left to build nested if
        let result: Expr = { tag: "Lit", value: false }; // default: #f if no else

        for (let i = clauses.length - 1; i >= 0; i--) {
          const clause = clauses[i];
          const clauseList = expectList(clause, "cond clause must be list");
          if (clauseList.items.length === 0) {
            throw new Error("cond: empty clause");
          }

          const testStx = clauseList.items[0];
          const bodyItems = clauseList.items.slice(1);

          // Check for 'else' clause
          const isElse = isIdent(testStx) && testStx.name === "else";

          if (isElse) {
            // (else e1 e2 ...) → (begin e1 e2 ...)
            if (bodyItems.length === 0) {
              result = { tag: "Lit", value: null }; // unit
            } else if (bodyItems.length === 1) {
              result = lowerSyntax(bodyItems[0], env, phase);
            } else {
              result = { tag: "Begin", exprs: bodyItems.map(b => lowerSyntax(b, env, phase)) };
            }
          } else {
            // (test e1 e2 ...) → (if test (begin e1 e2 ...) <rest>)
            let body: Expr;
            if (bodyItems.length === 0) {
              // (test) with no body - return test value (like Scheme)
              body = lowerSyntax(testStx, env, phase);
            } else if (bodyItems.length === 1) {
              body = lowerSyntax(bodyItems[0], env, phase);
            } else {
              body = { tag: "Begin", exprs: bodyItems.map(b => lowerSyntax(b, env, phase)) };
            }

            result = {
              tag: "If",
              test: lowerSyntax(testStx, env, phase),
              conseq: body,
              alt: result,
            };
          }
        }

        return result;
      }
    }
  }

  return { tag: "App", fn: lowerSyntax(items[0], env, phase), args: items.slice(1).map(x => lowerSyntax(x, env, phase)) };
}

function stripDatum(stx: Syntax): unknown {
  if (stx.tag === "Atom") return stx.value;
  if (stx.tag === "Ident") return { sym: stx.name };
  return stx.items.map(stripDatum);
}

function lowerHandler(clauses: Syntax[], env: Env, phase: number): HandlerExpr {
  const on: HandlerExpr["on"] = [];
  let ret: HandlerExpr["ret"] | undefined;
  let fin: HandlerExpr["fin"] | undefined;

  for (const cl of clauses) {
    const lst = expectList(cl, "handle clause must be list");
    if (lst.items.length < 2) throw new Error("handle clause too short");
    const tag = expectIdent(lst.items[0], "handle clause tag must be ident").name;

    if (tag === "on") {
      if (lst.items.length !== 4) throw new Error("on: expected (on op (x k) body)");
      const op = expectIdent(lst.items[1], "on: op must be ident").name;
      const argsList = expectList(lst.items[2], "on: (x k) must be list");
      if (argsList.items.length !== 2) throw new Error("on: expected (x k)");
      const x = expectIdent(argsList.items[0], "on: x must be ident").name;
      const k = expectIdent(argsList.items[1], "on: k must be ident").name;
      const body = lowerSyntax(lst.items[3], env, phase);
      on.push({ op, params: [x], k, body });
      continue;
    }

    if (tag === "return") {
      if (lst.items.length !== 3) throw new Error("return: expected (return (v) body)");
      const vList = expectList(lst.items[1], "return: (v) must be list");
      if (vList.items.length !== 1) throw new Error("return: expected one var");
      const v = expectIdent(vList.items[0], "return var must be ident").name;
      const body = lowerSyntax(lst.items[2], env, phase);
      ret = { v, body };
      continue;
    }

    if (tag === "finally") {
      if (lst.items.length !== 2) throw new Error("finally: expected (finally body)");
      fin = { body: lowerSyntax(lst.items[1], env, phase) };
      continue;
    }

    throw new Error(`unknown handle clause: ${tag}`);
  }

  return { on, ret, fin };
}

/**
 * Lower a pattern syntax to a Pattern AST node.
 * Supports:
 * - _ (wildcard)
 * - ?x (pattern variable binding)
 * - literals (numbers, strings, booleans, null)
 * - symbols (quoted identifiers become literal symbols)
 * - (p1 p2 ...) or [p1 p2 ...] (list/vector patterns)
 * - else (wildcard, same as _)
 */
function lowerPattern(stx: Syntax, env: Env, phase: number): Pattern {
  // Atom: literal pattern (number, string, boolean, null)
  if (stx.tag === "Atom") {
    const v = stx.value;
    if (typeof v === "number" || typeof v === "string" || typeof v === "boolean" || v === null) {
      return { tag: "PLit", value: v };
    }
    return { tag: "PLit", value: String(v) };
  }

  // Identifier: check for special patterns
  if (stx.tag === "Ident") {
    const name = stx.name;

    // Wildcard patterns
    if (name === "_" || name === "else") {
      return { tag: "PWild" };
    }

    // Pattern variable: starts with ?
    if (name.startsWith("?")) {
      const varName = name.slice(1);
      // Resolve through binding env if possible, otherwise use as-is
      const b = resolveIdent({ ...stx, name: varName }, env, phase);
      return { tag: "PVar", name: b ? (b.value as string) : varName };
    }

    // Quoted symbol pattern: match a symbol with this name
    // For bare identifiers in patterns, treat as literal symbol match
    return { tag: "PLit", value: name };
  }

  // List: vector pattern
  if (stx.tag === "List") {
    // Check for special pattern forms
    if (stx.items.length > 0) {
      const head = stx.items[0];
      if (isIdent(head)) {
        // (quote x) in pattern means literal value
        if (head.name === "quote" && stx.items.length === 2) {
          const datum = stripPatternDatum(stx.items[1]);
          return datumToPattern(datum);
        }
      }
    }

    // Regular list/vector pattern
    const items = stx.items.map(item => lowerPattern(item, env, phase));
    return { tag: "PVector", items };
  }

  throw new Error(`Cannot lower pattern: ${JSON.stringify(stx)}`);
}

/** Convert a stripped datum to a pattern */
function datumToPattern(d: unknown): Pattern {
  if (d === null) return { tag: "PLit", value: null };
  if (typeof d === "number") return { tag: "PLit", value: d };
  if (typeof d === "boolean") return { tag: "PLit", value: d };
  if (typeof d === "string") return { tag: "PLit", value: d };
  if (typeof d === "object" && d !== null && "sym" in d) {
    // Symbol: match as literal symbol name
    return { tag: "PLit", value: (d as { sym: string }).sym };
  }
  if (Array.isArray(d)) {
    return { tag: "PVector", items: d.map(datumToPattern) };
  }
  return { tag: "PLit", value: JSON.stringify(d) };
}

function stripPatternDatum(stx: Syntax): unknown {
  if (stx.tag === "Atom") return stx.value;
  if (stx.tag === "Ident") return { sym: stx.name };
  return stx.items.map(stripPatternDatum);
}
