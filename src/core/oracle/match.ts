// src/core/oracle/match.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Patch Set C2: Structural AST matcher with ?x binders and _ wildcard

export type Bindings = Record<string, unknown>;

function isObj(x: unknown): x is Record<string, unknown> {
  return !!x && typeof x === "object" && !Array.isArray(x);
}

function isVarBinder(node: unknown): string | null {
  // Supports either:
  //   {tag:"Var", name:"?x"}    OR   {tag:"Sym", name:"?x"}   OR plain string "?x"
  if (typeof node === "string" && node.startsWith("?")) return node.slice(1);
  if (isObj(node) && typeof node.name === "string" && node.name.startsWith("?")) return node.name.slice(1);
  return null;
}

function isWildcard(node: unknown): boolean {
  if (node === "_" || node === "*") return true;
  if (isObj(node) && (node.name === "_" || node.name === "*")) return true;
  return false;
}

export function matchAST(pattern: unknown, node: unknown): { ok: boolean; bindings: Bindings } {
  const bindings: Bindings = {};
  const ok = go(pattern, node, bindings);
  return { ok, bindings };
}

function go(p: unknown, n: unknown, b: Bindings): boolean {
  if (isWildcard(p)) return true;

  const binder = isVarBinder(p);
  if (binder) {
    if (binder in b) {
      return deepEqual(b[binder], n);
    }
    b[binder] = n;
    return true;
  }

  // primitives
  if (p === null || typeof p !== "object") return p === n;

  // arrays
  if (Array.isArray(p)) {
    if (!Array.isArray(n)) return false;
    if (p.length !== n.length) return false;
    for (let i = 0; i < p.length; i++) {
      if (!go(p[i], n[i], b)) return false;
    }
    return true;
  }

  // objects
  if (!isObj(n)) return false;
  const pObj = p as Record<string, unknown>;
  if (pObj.tag && n.tag && pObj.tag !== n.tag) return false;

  for (const k of Object.keys(pObj)) {
    if (k === "loc" || k === "span") continue; // ignore source locations
    if (!(k in n)) return false;
    if (!go(pObj[k], (n as Record<string, unknown>)[k], b)) return false;
  }
  return true;
}

function deepEqual(a: unknown, c: unknown): boolean {
  if (a === c) return true;
  if (typeof a !== typeof c) return false;
  if (a === null || c === null) return a === c;
  if (Array.isArray(a)) {
    if (!Array.isArray(c) || a.length !== c.length) return false;
    for (let i = 0; i < a.length; i++) if (!deepEqual(a[i], c[i])) return false;
    return true;
  }
  if (typeof a === "object") {
    const aObj = a as Record<string, unknown>;
    const cObj = c as Record<string, unknown>;
    const ak = Object.keys(aObj).filter(k => k !== "loc" && k !== "span").sort();
    const ck = Object.keys(cObj).filter(k => k !== "loc" && k !== "span").sort();
    if (ak.length !== ck.length) return false;
    for (let i = 0; i < ak.length; i++) if (ak[i] !== ck[i]) return false;
    for (const k of ak) if (!deepEqual(aObj[k], cObj[k])) return false;
    return true;
  }
  return false;
}
