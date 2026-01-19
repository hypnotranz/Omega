// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-8.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import type { Val } from "./values";
import type { State } from "./machine";
import { VTrue, VFalse, VUnit } from "./values";
import type { Syntax } from "../syntax/syntax";
import { addScope, isIdent, isList } from "../syntax/syntax";

function isSyntaxVal(v: Val): v is { tag: "Syntax"; stx: Syntax } {
  return v.tag === "Syntax";
}

function valToDatum(v: Val): unknown {
  switch (v.tag) {
    case "Unit": return null;
    case "Num": return v.n;
    case "Bool": return v.b;
    case "Str": return v.s;
    case "Sym": return { sym: v.name };
    case "Vector": return v.items.map(valToDatum);
    case "Syntax": return syntaxToDatum(v.stx);
    default:
      return { opaque: v.tag };
  }
}

function datumToVal(d: unknown): Val {
  if (d === null) return { tag: "Unit" };
  if (typeof d === "number") return { tag: "Num", n: d };
  if (typeof d === "string") return { tag: "Str", s: d };
  if (typeof d === "boolean") return { tag: "Bool", b: d };
  if (typeof d === "object" && d !== null && !Array.isArray(d) && "sym" in d) {
    return { tag: "Sym", name: (d as any).sym };
  }
  if (Array.isArray(d)) return { tag: "Vector", items: d.map(datumToVal) };
  return { tag: "Str", s: JSON.stringify(d) };
}

function syntaxToDatum(stx: Syntax): unknown {
  if (stx.tag === "Atom") return stx.value;
  if (stx.tag === "Ident") return { sym: stx.name };
  return stx.items.map(syntaxToDatum);
}

function datumToSyntaxWithScopes(d: unknown, scopes: string[]): Syntax {
  if (d === null || typeof d === "number" || typeof d === "string" || typeof d === "boolean") {
    return { tag: "Atom", value: d as any, scopes: scopes.slice() };
  }
  if (typeof d === "object" && d !== null && !Array.isArray(d) && "sym" in d) {
    return { tag: "Ident", name: (d as any).sym, scopes: scopes.slice() };
  }
  if (Array.isArray(d)) {
    return { tag: "List", items: d.map(x => datumToSyntaxWithScopes(x, scopes)), scopes: scopes.slice() };
  }
  // fallback
  return { tag: "Atom", value: JSON.stringify(d), scopes: scopes.slice() };
}

export function syntaxPrims(): Array<{ name: string; arity: number | "variadic"; fn: (args: Val[], st: State) => State }> {
  return [
    {
      name: "syntax?",
      arity: 1,
      fn: ([v], st) => ({ ...st, control: { tag: "Val", v: isSyntaxVal(v) ? VTrue : VFalse } }),
    },
    {
      name: "syntax->datum",
      arity: 1,
      fn: ([v], st) => {
        if (!isSyntaxVal(v)) throw new Error("syntax->datum: expected Syntax");
        return { ...st, control: { tag: "Val", v: datumToVal(syntaxToDatum(v.stx)) } };
      },
    },
    {
      name: "datum->syntax",
      arity: 2,
      fn: ([ctx, dat], st) => {
        if (!isSyntaxVal(ctx)) throw new Error("datum->syntax: ctx must be Syntax");
        const scopes = ctx.stx.scopes;
        const d = valToDatum(dat);
        const out: Syntax = datumToSyntaxWithScopes(d, scopes);
        return { ...st, control: { tag: "Val", v: { tag: "Syntax", stx: out } } };
      },
    },
    {
      name: "syntax-ident?",
      arity: 1,
      fn: ([v], st) => {
        if (!isSyntaxVal(v)) return { ...st, control: { tag: "Val", v: VFalse } };
        return { ...st, control: { tag: "Val", v: isIdent(v.stx) ? VTrue : VFalse } };
      },
    },
    {
      name: "syntax-list?",
      arity: 1,
      fn: ([v], st) => {
        if (!isSyntaxVal(v)) return { ...st, control: { tag: "Val", v: VFalse } };
        return { ...st, control: { tag: "Val", v: isList(v.stx) ? VTrue : VFalse } };
      },
    },
    {
      name: "syntax-ident-name",
      arity: 1,
      fn: ([v], st) => {
        if (!isSyntaxVal(v) || v.stx.tag !== "Ident") throw new Error("syntax-ident-name: expected identifier syntax");
        return { ...st, control: { tag: "Val", v: { tag: "Str", s: v.stx.name } } };
      },
    },
    {
      name: "syntax-list-items",
      arity: 1,
      fn: ([v], st) => {
        if (!isSyntaxVal(v) || v.stx.tag !== "List") throw new Error("syntax-list-items: expected list syntax");
        const items: Val[] = v.stx.items.map(it => ({ tag: "Syntax", stx: it }));
        return { ...st, control: { tag: "Val", v: { tag: "Vector", items } } };
      },
    },
    {
      name: "syntax-add-scope",
      arity: 2,
      fn: ([v, sc], st) => {
        if (!isSyntaxVal(v)) throw new Error("syntax-add-scope: expected Syntax");
        if (sc.tag !== "Str") throw new Error("syntax-add-scope: expected string scope");
        return { ...st, control: { tag: "Val", v: { tag: "Syntax", stx: addScope(v.stx, sc.s) } } };
      },
    },
    {
      name: "syntax-scopes",
      arity: 1,
      fn: ([v], st) => {
        if (!isSyntaxVal(v)) throw new Error("syntax-scopes: expected Syntax");
        return { ...st, control: { tag: "Val", v: { tag: "Vector", items: v.stx.scopes.map(s => ({ tag: "Str", s })) } } };
      },
    },
    {
      name: "syntax-local-introduce",
      arity: 1,
      fn: ([v], st) => {
        // reference-grade: identity; in a full system this would add the current introducer scope.
        if (!isSyntaxVal(v)) throw new Error("syntax-local-introduce: expected Syntax");
        return { ...st, control: { tag: "Val", v } };
      },
    },
    {
      name: "unit",
      arity: 0,
      fn: (_args, st) => ({ ...st, control: { tag: "Val", v: VUnit } }),
    },
  ];
}