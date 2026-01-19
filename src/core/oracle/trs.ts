// src/core/oracle/trs.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Patch Set 6-A2: Deterministic Term Rewriting System engine

import type { Val } from "../eval/values";
import { matchAST, type Bindings } from "./match";

/**
 * A rewrite rule: pattern → template, optionally guarded by a predicate.
 */
export type Rule = {
  name: string;
  pattern: Val;
  template: Val;
  /** Optional predicate over bindings. Returns true if rule is applicable. */
  where?: (bindings: Bindings) => boolean;
};

/**
 * Traversal strategy for applying rewrite rules.
 */
export type Strategy = "topdown" | "bottomup";

/**
 * Result of a single rewrite step.
 */
export type RewriteResult = {
  changed: boolean;
  result: Val;
  ruleName?: string;
  position?: string;
};

/**
 * Conflict report from confluence analysis.
 */
export type ConflictReport = {
  rule1: string;
  rule2: string;
  overlap: "same-head" | "embeds" | "similar-shape";
  description: string;
};

// -----------------------------------------------------------------------------
// Pattern substitution: apply bindings to a template
// -----------------------------------------------------------------------------

function isObj(x: unknown): x is Record<string, unknown> {
  return !!x && typeof x === "object" && !Array.isArray(x);
}

function isVarRef(node: Val): string | null {
  // Template variable: {tag:"Sym", name:"?x"} or {tag:"Var", name:"?x"}
  if (isObj(node) && typeof (node as any).name === "string") {
    const name = (node as any).name as string;
    if (name.startsWith("?")) return name.slice(1);
  }
  return null;
}

/**
 * Substitute bindings into a template, producing a new Val.
 */
export function substitute(template: Val, bindings: Bindings): Val {
  const ref = isVarRef(template);
  if (ref !== null) {
    if (ref in bindings) {
      return bindings[ref] as Val;
    }
    // Unbound variable stays as-is (for partial templates)
    return template;
  }

  if (template === null || typeof template !== "object") {
    return template;
  }

  if (Array.isArray(template)) {
    return template.map(item => substitute(item as Val, bindings)) as unknown as Val;
  }

  // Object (Val node)
  const obj = template as Record<string, unknown>;
  const result: Record<string, unknown> = {};
  for (const k of Object.keys(obj)) {
    if (k === "loc" || k === "span") {
      // Don't recurse into source locations
      result[k] = obj[k];
    } else {
      result[k] = substitute(obj[k] as Val, bindings);
    }
  }
  return result as Val;
}

// -----------------------------------------------------------------------------
// Tree traversal helpers
// -----------------------------------------------------------------------------

function getChildren(node: Val): { key: string; child: Val }[] {
  if (node === null || typeof node !== "object") return [];
  if (Array.isArray(node)) {
    return node.map((item, i) => ({ key: String(i), child: item as Val }));
  }

  const obj = node as Record<string, unknown>;
  const children: { key: string; child: Val }[] = [];

  // Recurse into known Val structural fields
  switch (obj.tag) {
    case "Pair":
      children.push({ key: "car", child: obj.car as Val });
      children.push({ key: "cdr", child: obj.cdr as Val });
      break;
    case "Vector":
      (obj.items as Val[]).forEach((item, i) => {
        children.push({ key: `items[${i}]`, child: item });
      });
      break;
    case "Map":
      (obj.entries as Array<[Val, Val]>).forEach(([k, v], i) => {
        children.push({ key: `entries[${i}][0]`, child: k });
        children.push({ key: `entries[${i}][1]`, child: v });
      });
      break;
    case "Syntax":
      children.push({ key: "stx", child: obj.stx as Val });
      break;
    // For quoted Exprs, recurse into their structure
    default:
      // Generic object: recurse into non-special fields
      for (const k of Object.keys(obj)) {
        if (k === "tag" || k === "loc" || k === "span") continue;
        const v = obj[k];
        if (v && typeof v === "object") {
          children.push({ key: k, child: v as Val });
        }
      }
  }
  return children;
}

function setChild(node: Val, key: string, newChild: Val): Val {
  if (Array.isArray(node)) {
    const idx = parseInt(key, 10);
    const copy = [...node];
    copy[idx] = newChild;
    return copy as unknown as Val;
  }

  const obj = node as Record<string, unknown>;
  const copy = { ...obj };

  // Handle indexed keys like "items[0]" or "entries[0][1]"
  const arrayMatch = key.match(/^(\w+)\[(\d+)\](?:\[(\d+)\])?$/);
  if (arrayMatch) {
    const [, field, idx1, idx2] = arrayMatch;
    const arr = [...(copy[field] as unknown[])];
    if (idx2 !== undefined) {
      // entries[i][j]
      const inner = [...(arr[parseInt(idx1, 10)] as unknown[])];
      inner[parseInt(idx2, 10)] = newChild;
      arr[parseInt(idx1, 10)] = inner;
    } else {
      arr[parseInt(idx1, 10)] = newChild;
    }
    copy[field] = arr;
  } else {
    copy[key] = newChild;
  }

  return copy as Val;
}

// -----------------------------------------------------------------------------
// Core rewrite engine
// -----------------------------------------------------------------------------

/**
 * Try to apply a single rule at the root of the node.
 * Returns the rewritten node if the rule matched, or null otherwise.
 */
function tryApplyRule(rule: Rule, node: Val): Val | null {
  const { ok, bindings } = matchAST(rule.pattern, node);
  if (!ok) return null;
  if (rule.where && !rule.where(bindings)) return null;
  return substitute(rule.template, bindings);
}

/**
 * Apply the first matching rule at the root.
 * Returns { changed: true, result, ruleName } if a rule matched,
 * or { changed: false, result: node } otherwise.
 */
function applyRulesAtRoot(rules: Rule[], node: Val): RewriteResult {
  for (const rule of rules) {
    const result = tryApplyRule(rule, node);
    if (result !== null) {
      return { changed: true, result, ruleName: rule.name };
    }
  }
  return { changed: false, result: node };
}

/**
 * Rewrite once using topdown strategy: try root first, then children.
 */
function rewriteOnceTopdown(rules: Rule[], node: Val, path: string = ""): RewriteResult {
  // Try root first
  const rootResult = applyRulesAtRoot(rules, node);
  if (rootResult.changed) {
    return { ...rootResult, position: path || "root" };
  }

  // Try children
  const children = getChildren(node);
  for (const { key, child } of children) {
    const childPath = path ? `${path}.${key}` : key;
    const childResult = rewriteOnceTopdown(rules, child, childPath);
    if (childResult.changed) {
      const newNode = setChild(node, key, childResult.result);
      return { changed: true, result: newNode, ruleName: childResult.ruleName, position: childResult.position };
    }
  }

  return { changed: false, result: node };
}

/**
 * Rewrite once using bottomup strategy: try children first, then root.
 */
function rewriteOnceBottomup(rules: Rule[], node: Val, path: string = ""): RewriteResult {
  // Try children first
  const children = getChildren(node);
  let currentNode = node;
  let anyChildChanged = false;

  for (const { key, child } of children) {
    const childPath = path ? `${path}.${key}` : key;
    const childResult = rewriteOnceBottomup(rules, child, childPath);
    if (childResult.changed) {
      currentNode = setChild(currentNode, key, childResult.result);
      anyChildChanged = true;
      // Return on first change (outermost bottomup)
      return { changed: true, result: currentNode, ruleName: childResult.ruleName, position: childResult.position };
    }
  }

  // Try root after children
  const rootResult = applyRulesAtRoot(rules, currentNode);
  if (rootResult.changed) {
    return { ...rootResult, position: path || "root" };
  }

  return { changed: anyChildChanged, result: currentNode };
}

/**
 * Apply rules once at the first matching position.
 */
export function rewriteOnce(rules: Rule[], expr: Val, strategy: Strategy = "topdown"): RewriteResult {
  if (strategy === "topdown") {
    return rewriteOnceTopdown(rules, expr);
  } else {
    return rewriteOnceBottomup(rules, expr);
  }
}

/**
 * Apply rules repeatedly until fixpoint or fuel exhausted.
 * @param fuel Maximum number of rewrite steps (default 100)
 */
export function rewriteFixpoint(
  rules: Rule[],
  expr: Val,
  strategy: Strategy = "topdown",
  fuel: number = 100
): { result: Val; steps: number; reachedFixpoint: boolean } {
  let current = expr;
  let steps = 0;

  while (steps < fuel) {
    const { changed, result } = rewriteOnce(rules, current, strategy);
    if (!changed) {
      return { result: current, steps, reachedFixpoint: true };
    }
    current = result;
    steps++;
  }

  return { result: current, steps, reachedFixpoint: false };
}

/**
 * Apply rules and return the full trace of intermediate results.
 */
export function rewriteTrace(
  rules: Rule[],
  expr: Val,
  strategy: Strategy = "topdown",
  fuel: number = 100
): { trace: Array<{ expr: Val; ruleName?: string; position?: string }>; reachedFixpoint: boolean } {
  const trace: Array<{ expr: Val; ruleName?: string; position?: string }> = [{ expr }];
  let current = expr;
  let steps = 0;

  while (steps < fuel) {
    const { changed, result, ruleName, position } = rewriteOnce(rules, current, strategy);
    if (!changed) {
      return { trace, reachedFixpoint: true };
    }
    current = result;
    trace.push({ expr: current, ruleName, position });
    steps++;
  }

  return { trace, reachedFixpoint: false };
}

// -----------------------------------------------------------------------------
// Confluence diagnostic: critical pair detection
// -----------------------------------------------------------------------------

/**
 * Extract the "head shape" of a pattern for overlap detection.
 * For Val nodes, this is the tag + direct structure.
 */
function getPatternHead(pattern: Val): string {
  if (pattern === null || typeof pattern !== "object") {
    return String(pattern);
  }
  if (Array.isArray(pattern)) {
    return `Array[${pattern.length}]`;
  }
  const obj = pattern as Record<string, unknown>;
  if (obj.tag) {
    return String(obj.tag);
  }
  return "Object";
}

/**
 * Check if two patterns have structural overlap that could cause non-confluence.
 */
function patternsOverlap(p1: Val, p2: Val): "same-head" | "embeds" | "similar-shape" | null {
  const head1 = getPatternHead(p1);
  const head2 = getPatternHead(p2);

  // Different head tags - no overlap
  if (head1 !== head2) return null;

  // Same head tag - need deeper analysis
  if (isObj(p1) && isObj(p2)) {
    const obj1 = p1 as Record<string, unknown>;
    const obj2 = p2 as Record<string, unknown>;

    // For App nodes, check if the function names differ
    if (obj1.tag === "App" && obj2.tag === "App") {
      const fn1 = obj1.fn as Record<string, unknown> | undefined;
      const fn2 = obj2.fn as Record<string, unknown> | undefined;

      // If both have concrete (non-variable) function names that differ, no overlap
      if (fn1 && fn2 &&
          fn1.tag === "Var" && fn2.tag === "Var" &&
          typeof fn1.name === "string" && typeof fn2.name === "string" &&
          !fn1.name.startsWith("?") && !fn2.name.startsWith("?") &&
          fn1.name !== fn2.name) {
        return null;
      }
    }

    // For Var nodes with different concrete names, no overlap
    if (obj1.tag === "Var" && obj2.tag === "Var") {
      const name1 = obj1.name as string | undefined;
      const name2 = obj2.name as string | undefined;
      if (name1 && name2 &&
          !name1.startsWith("?") && !name2.startsWith("?") &&
          name1 !== name2) {
        return null;
      }
    }

    // For Num nodes with different values, no overlap
    if (obj1.tag === "Num" && obj2.tag === "Num") {
      if (obj1.n !== obj2.n) {
        return null;
      }
    }

    // For Sym nodes with different concrete names, no overlap
    if (obj1.tag === "Sym" && obj2.tag === "Sym") {
      const name1 = obj1.name as string | undefined;
      const name2 = obj2.name as string | undefined;
      if (name1 && name2 &&
          !name1.startsWith("?") && !name2.startsWith("?") &&
          name1 !== name2) {
        return null;
      }
    }

    // Count pattern variables to check for embedding
    const vars1 = countPatternVars(p1);
    const vars2 = countPatternVars(p2);

    // If one has more concrete structure (fewer vars), it might embed
    if (Math.abs(vars1 - vars2) > 0) {
      return "embeds";
    }

    // Same structure - potential conflict
    return "same-head";
  }

  // Primitive types with same head
  if (head1 !== "Object") {
    return "same-head";
  }

  return null;
}

function countPatternVars(node: unknown): number {
  if (node === null) return 0;
  if (typeof node === "string") return node.startsWith("?") ? 1 : 0;
  if (typeof node !== "object") return 0;

  if (Array.isArray(node)) {
    return node.reduce((acc, item) => acc + countPatternVars(item), 0);
  }

  const obj = node as Record<string, unknown>;
  if (typeof obj.name === "string" && obj.name.startsWith("?")) return 1;

  let count = 0;
  for (const k of Object.keys(obj)) {
    if (k !== "tag" && k !== "loc" && k !== "span") {
      count += countPatternVars(obj[k]);
    }
  }
  return count;
}

/**
 * Detect potential non-confluence in a rule set.
 * Returns conflict reports for pairs of rules that might produce different results.
 */
export function detectConflicts(rules: Rule[]): ConflictReport[] {
  const conflicts: ConflictReport[] = [];

  for (let i = 0; i < rules.length; i++) {
    for (let j = i + 1; j < rules.length; j++) {
      const r1 = rules[i];
      const r2 = rules[j];
      const overlap = patternsOverlap(r1.pattern, r2.pattern);

      if (overlap) {
        conflicts.push({
          rule1: r1.name,
          rule2: r2.name,
          overlap,
          description: `Rules '${r1.name}' and '${r2.name}' have overlapping patterns (${overlap}). ` +
            `This may cause non-confluent rewriting depending on rule order.`
        });
      }
    }
  }

  return conflicts;
}

// -----------------------------------------------------------------------------
// Rule construction helpers
// -----------------------------------------------------------------------------

/**
 * Create a rewrite rule.
 */
export function rule(name: string, pattern: Val, template: Val, where?: (bindings: Bindings) => boolean): Rule {
  return { name, pattern, template, where };
}

/**
 * Create a rule from pattern/template Val objects.
 */
export function makeRule(
  name: string,
  pattern: Val,
  template: Val,
  wherePred?: Val
): Rule {
  // wherePred would need to be evaluated in context; for now, leave it as optional JS function
  return { name, pattern, template };
}
