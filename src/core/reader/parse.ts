// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-5.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

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