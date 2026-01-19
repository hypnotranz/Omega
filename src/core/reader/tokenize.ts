// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-5.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

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