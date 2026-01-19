// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-11.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.
// Prompt 15: Extended with source location support

export type Scope = string;

/**
 * SourceLocation: Source file location for syntax objects.
 * Used for error reporting and debugging.
 */
export type SourceLocation = {
  file?: string;
  line?: number;
  col?: number;
  span?: number;
};

/**
 * Syntax: First-class syntax object with scopes for hygiene.
 *
 * This implements the set-of-scopes model from Racket:
 * - Each syntax object carries a set of scopes
 * - Identifier resolution uses scope sets to find bindings
 * - Macros add/flip scopes to maintain hygiene
 */
export type Syntax =
  | { tag: "Atom"; value: any; scopes: Scope[]; srcloc?: SourceLocation }
  | { tag: "Ident"; name: string; scopes: Scope[]; srcloc?: SourceLocation }
  | { tag: "List"; items: Syntax[]; scopes: Scope[]; srcloc?: SourceLocation };

export type SIdent = Extract<Syntax, { tag: "Ident" }>;
export type SList = Extract<Syntax, { tag: "List" }>;

export function isIdent(x: Syntax): x is SIdent {
  return x.tag === "Ident";
}

export function isList(x: Syntax): x is SList {
  return x.tag === "List";
}

/** Add scope to every node (so identifier nodes carry scopes). */
export function addScope(stx: Syntax, sc: Scope): Syntax {
  switch (stx.tag) {
    case "Atom":
      return { ...stx, scopes: stx.scopes.concat([sc]) };
    case "Ident":
      return { ...stx, scopes: stx.scopes.concat([sc]) };
    case "List":
      return { ...stx, scopes: stx.scopes.concat([sc]), items: stx.items.map(it => addScope(it, sc)) };
  }
}

/** Deterministic fresh scope ids using an explicit counter object. */
export function freshScope(counter: { n: number }): Scope {
  counter.n += 1;
  return `s#${counter.n}`;
}

/**
 * Check if a syntax object is an atom.
 */
export function isAtom(x: Syntax): x is Extract<Syntax, { tag: "Atom" }> {
  return x.tag === "Atom";
}

/**
 * Create a syntax object from a datum (symbol, number, string, list).
 */
export function datum(value: any, scopes: Scope[] = [], srcloc?: SourceLocation): Syntax {
  if (value === null || value === undefined) {
    return { tag: "Atom", value, scopes, srcloc };
  }

  if (typeof value === "symbol") {
    return { tag: "Ident", name: value.description ?? String(value), scopes, srcloc };
  }

  if (Array.isArray(value)) {
    return {
      tag: "List",
      items: value.map(v => datum(v, scopes, srcloc)),
      scopes,
      srcloc,
    };
  }

  return { tag: "Atom", value, scopes, srcloc };
}

/**
 * Remove a scope from a syntax object.
 */
export function removeScope(stx: Syntax, sc: Scope): Syntax {
  switch (stx.tag) {
    case "Atom":
      return { ...stx, scopes: stx.scopes.filter(s => s !== sc) };
    case "Ident":
      return { ...stx, scopes: stx.scopes.filter(s => s !== sc) };
    case "List":
      return {
        ...stx,
        scopes: stx.scopes.filter(s => s !== sc),
        items: stx.items.map(it => removeScope(it, sc)),
      };
  }
}

/**
 * Flip a scope on a syntax object (add if missing, remove if present).
 */
export function flipScope(stx: Syntax, sc: Scope): Syntax {
  switch (stx.tag) {
    case "Atom": {
      const scopes = stx.scopes.includes(sc)
        ? stx.scopes.filter(s => s !== sc)
        : stx.scopes.concat([sc]);
      return { ...stx, scopes };
    }
    case "Ident": {
      const scopes = stx.scopes.includes(sc)
        ? stx.scopes.filter(s => s !== sc)
        : stx.scopes.concat([sc]);
      return { ...stx, scopes };
    }
    case "List": {
      const scopes = stx.scopes.includes(sc)
        ? stx.scopes.filter(s => s !== sc)
        : stx.scopes.concat([sc]);
      return {
        ...stx,
        scopes,
        items: stx.items.map(it => flipScope(it, sc)),
      };
    }
  }
}

/**
 * Get the source location of a syntax object.
 */
export function getSrcloc(stx: Syntax): SourceLocation | undefined {
  return stx.srcloc;
}

/**
 * Set the source location of a syntax object.
 */
export function withSrcloc(stx: Syntax, srcloc: SourceLocation): Syntax {
  return { ...stx, srcloc };
}

/**
 * Copy scopes from one syntax object to another.
 */
export function copyScopes(from: Syntax, to: Syntax): Syntax {
  return { ...to, scopes: [...from.scopes] };
}