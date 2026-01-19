// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-5.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import type { Datum } from "./datum";
import { isSym } from "./datum";
import type { Syntax } from "../syntax/syntax";

export function datumToSyntax(d: Datum): Syntax {
  if (isSym(d)) {
    return { tag: "Ident", name: d.sym, scopes: [] };
  }
  if (Array.isArray(d)) {
    return { tag: "List", items: d.map(datumToSyntax), scopes: [] };
  }
  // atoms
  return { tag: "Atom", value: d, scopes: [] };
}