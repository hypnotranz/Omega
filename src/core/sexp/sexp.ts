// src/core/sexp/sexp.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-19.md
// S-expression parser, printer, and structural equality

export type Sexp =
  | { tag: "Sym"; name: string }
  | { tag: "Num"; n: number }
  | { tag: "Str"; s: string }
  | { tag: "Bool"; b: boolean }
  | { tag: "List"; items: Sexp[] };

export function sym(name: string): Sexp { return { tag: "Sym", name }; }
export function num(n: number): Sexp { return { tag: "Num", n }; }
export function str(s: string): Sexp { return { tag: "Str", s }; }
export function bool(b: boolean): Sexp { return { tag: "Bool", b }; }
export function list(items: Sexp[]): Sexp { return { tag: "List", items }; }

export function sexpEq(a: Sexp, b: Sexp): boolean {
  if (a.tag !== b.tag) return false;
  switch (a.tag) {
    case "Sym": return a.name === (b as typeof a).name;
    case "Num": return a.n === (b as typeof a).n;
    case "Str": return a.s === (b as typeof a).s;
    case "Bool": return a.b === (b as typeof a).b;
    case "List": {
      const aa = a.items, bb = (b as typeof a).items;
      if (aa.length !== bb.length) return false;
      for (let i = 0; i < aa.length; i++) if (!sexpEq(aa[i]!, bb[i]!)) return false;
      return true;
    }
  }
}

export function sexpToString(x: Sexp): string {
  switch (x.tag) {
    case "Sym": return x.name;
    case "Num": return Number.isFinite(x.n) ? String(x.n) : "nan";
    case "Str": return JSON.stringify(x.s);
    case "Bool": return x.b ? "#t" : "#f";
    case "List": {
      // Pretty-print quote sugar when possible
      if (x.items.length === 2 && x.items[0]?.tag === "Sym" && x.items[0].name === "quote") {
        return `'${sexpToString(x.items[1]!)}`;
      }
      return `(${x.items.map(sexpToString).join(" ")})`;
    }
  }
}

// ----- Parser -----

type Tok =
  | { tag: "LP" }
  | { tag: "RP" }
  | { tag: "QUOTE" }
  | { tag: "STR"; s: string }
  | { tag: "ATOM"; s: string };

export function parseSexp(src: string): Sexp {
  const toks = tokenize(src);
  let i = 0;

  function peek(): Tok | undefined { return toks[i]; }
  function take(): Tok {
    const t = toks[i];
    if (!t) throw new Error("unexpected EOF");
    i++;
    return t;
  }

  function parseOne(): Sexp {
    const t = take();
    if (t.tag === "LP") {
      const items: Sexp[] = [];
      while (true) {
        const p = peek();
        if (!p) throw new Error("unterminated list");
        if (p.tag === "RP") { take(); break; }
        items.push(parseOne());
      }
      return list(items);
    }
    if (t.tag === "RP") throw new Error("unexpected ')'");
    if (t.tag === "QUOTE") {
      const inner = parseOne();
      return list([sym("quote"), inner]);
    }
    if (t.tag === "STR") return str(t.s);

    // ATOM
    return atomToSexp(t.s);
  }

  const out = parseOne();
  if (i !== toks.length) throw new Error("trailing tokens after first expression");
  return out;
}

function atomToSexp(a: string): Sexp {
  if (a === "#t" || a === "true") return bool(true);
  if (a === "#f" || a === "false") return bool(false);

  // Number (int or float)
  if (/^[+-]?\d+(\.\d+)?$/.test(a)) return num(Number(a));

  // Symbol
  return sym(a);
}

function tokenize(src: string): Tok[] {
  const out: Tok[] = [];
  let i = 0;

  function isWS(c: string) { return c === " " || c === "\t" || c === "\n" || c === "\r"; }

  while (i < src.length) {
    const c = src[i]!;
    // Skip whitespace
    if (isWS(c)) { i++; continue; }

    // Comments ;... to end of line
    if (c === ";") {
      while (i < src.length && src[i] !== "\n") i++;
      continue;
    }

    if (c === "(") { out.push({ tag: "LP" }); i++; continue; }
    if (c === ")") { out.push({ tag: "RP" }); i++; continue; }
    if (c === "'") { out.push({ tag: "QUOTE" }); i++; continue; }

    // String
    if (c === "\"") {
      i++;
      let s = "";
      while (i < src.length) {
        const d = src[i]!;
        if (d === "\"") { i++; break; }
        if (d === "\\") {
          i++;
          if (i >= src.length) throw new Error("unterminated escape");
          const e = src[i]!;
          if (e === "n") s += "\n";
          else if (e === "t") s += "\t";
          else if (e === "r") s += "\r";
          else if (e === "\"") s += "\"";
          else if (e === "\\") s += "\\";
          else s += e;
          i++;
          continue;
        }
        s += d;
        i++;
      }
      out.push({ tag: "STR", s });
      continue;
    }

    // Atom
    let a = "";
    while (i < src.length) {
      const d = src[i]!;
      if (isWS(d) || d === "(" || d === ")" || d === "'" || d === ";" ) break;
      a += d;
      i++;
    }
    if (a.length === 0) throw new Error("lexer error");
    out.push({ tag: "ATOM", s: a });
  }

  return out;
}
