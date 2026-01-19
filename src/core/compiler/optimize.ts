// src/core/compiler/optimize.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-17.md
// Prompt 17: Optimization passes for ANF - CSE, fusion, and oracle cost reduction

import { sha256JSON } from "../artifacts/hash";
import type {
  ANFAtom,
  ANFPrim,
  ANFExpr,
  ANFProgram,
  CompilerObligation,
  CSECandidate,
  OptimizationResult,
  ProgramArtifact,
} from "./types";
import { countBindings, findEffectOps } from "./anf";

// ─────────────────────────────────────────────────────────────────
// CSE (Common Subexpression Elimination) for Oracle Calls
// ─────────────────────────────────────────────────────────────────

/**
 * Compute a digest for an ANF primitive (for CSE comparison).
 */
function primDigest(prim: ANFPrim): string {
  // For CSE, we need structural comparison
  // Effects are identified by (op, args) tuple
  if (prim.tag === "Effect") {
    return sha256JSON({ tag: "Effect", op: prim.op, args: prim.args });
  }
  if (prim.tag === "Call") {
    return sha256JSON({ tag: "Call", fn: prim.fn, args: prim.args });
  }
  if (prim.tag === "Prim") {
    return sha256JSON({ tag: "Prim", name: prim.name, args: prim.args });
  }
  // Other primitives (Lambda, Quote, etc.) are not candidates for CSE
  return sha256JSON(prim);
}

/**
 * Check if a primitive is a CSE candidate (pure or idempotent).
 */
function isCSECandidate(prim: ANFPrim): boolean {
  switch (prim.tag) {
    case "Prim":
      // Most built-in primitives are pure
      return true;
    case "Call":
      // Calls to unknown functions are not safe
      return false;
    case "Effect":
      // Effects with these ops are safe to dedupe (idempotent inference)
      return prim.op === "infer.op" || prim.op === "oracle.classify";
    default:
      return false;
  }
}

/**
 * Collect all let bindings with their primitives for CSE analysis.
 */
function collectBindings(
  expr: ANFExpr,
  acc: Array<{ name: string; prim: ANFPrim; digest: string }>
): void {
  switch (expr.tag) {
    case "Let":
      if (isCSECandidate(expr.rhs)) {
        acc.push({ name: expr.name, prim: expr.rhs, digest: primDigest(expr.rhs) });
      }
      collectBindings(expr.body, acc);
      break;
    case "LetRec":
      for (const b of expr.bindings) {
        if (isCSECandidate(b.rhs)) {
          acc.push({ name: b.name, prim: b.rhs, digest: primDigest(b.rhs) });
        }
      }
      collectBindings(expr.body, acc);
      break;
    case "If":
      collectBindings(expr.thn, acc);
      collectBindings(expr.els, acc);
      break;
    case "Seq":
      collectBindings(expr.first, acc);
      collectBindings(expr.second, acc);
      break;
    case "Set":
      collectBindings(expr.body, acc);
      break;
    case "Return":
      // No bindings
      break;
  }
}

/**
 * Find CSE candidates - expressions that appear multiple times.
 */
export function findCSECandidates(program: ANFProgram): CSECandidate[] {
  const bindings: Array<{ name: string; prim: ANFPrim; digest: string }> = [];
  collectBindings(program.body, bindings);

  // Group by digest
  const groups = new Map<string, typeof bindings>();
  for (const b of bindings) {
    const existing = groups.get(b.digest);
    if (existing) {
      existing.push(b);
    } else {
      groups.set(b.digest, [b]);
    }
  }

  // Find duplicates
  const candidates: CSECandidate[] = [];
  for (const [digest, group] of groups) {
    if (group.length > 1) {
      // Check if this is an oracle/effect call
      const isOracleCall = group[0].prim.tag === "Effect" &&
        (group[0].prim.op === "infer.op" || group[0].prim.op.startsWith("oracle"));

      candidates.push({
        exprDigest: digest,
        locations: group.map(b => b.name),
        estimatedSaving: isOracleCall ? group.length - 1 : 0,
        safe: true,
      });
    }
  }

  return candidates;
}

/**
 * Apply CSE transformation to eliminate duplicate expressions.
 */
export function applyCSE(program: ANFProgram): { program: ANFProgram; eliminated: number } {
  const candidates = findCSECandidates(program);

  if (candidates.length === 0) {
    return { program, eliminated: 0 };
  }

  // Build substitution map: later occurrences -> first occurrence
  const substitutions = new Map<string, string>();
  let eliminated = 0;

  for (const candidate of candidates) {
    if (candidate.locations.length > 1) {
      const first = candidate.locations[0];
      for (let i = 1; i < candidate.locations.length; i++) {
        substitutions.set(candidate.locations[i], first);
        eliminated++;
      }
    }
  }

  // Transform the program
  const transformedBody = transformExprCSE(program.body, substitutions);

  return {
    program: {
      body: transformedBody,
      freeVars: program.freeVars,
      defs: program.defs,
    },
    eliminated,
  };
}

/**
 * Transform an ANF expression applying CSE substitutions.
 */
function transformExprCSE(
  expr: ANFExpr,
  substitutions: Map<string, string>
): ANFExpr {
  switch (expr.tag) {
    case "Let": {
      // If this binding is a duplicate, replace with reference to first
      const replacement = substitutions.get(expr.name);
      if (replacement) {
        // Skip this let and substitute in body
        const transformedBody = transformExprCSE(expr.body, substitutions);
        return substituteVar(transformedBody, expr.name, { tag: "Var", name: replacement });
      }

      return {
        tag: "Let",
        name: expr.name,
        rhs: expr.rhs,
        body: transformExprCSE(expr.body, substitutions),
        loc: expr.loc,
      };
    }

    case "LetRec": {
      // Filter out eliminated bindings
      const filteredBindings = expr.bindings.filter(b => !substitutions.has(b.name));

      return {
        tag: "LetRec",
        bindings: filteredBindings,
        body: transformExprCSE(expr.body, substitutions),
        loc: expr.loc,
      };
    }

    case "If":
      return {
        tag: "If",
        test: expr.test,
        thn: transformExprCSE(expr.thn, substitutions),
        els: transformExprCSE(expr.els, substitutions),
        loc: expr.loc,
      };

    case "Seq":
      return {
        tag: "Seq",
        first: transformExprCSE(expr.first, substitutions),
        second: transformExprCSE(expr.second, substitutions),
        loc: expr.loc,
      };

    case "Set":
      return {
        tag: "Set",
        name: expr.name,
        rhs: expr.rhs,
        body: transformExprCSE(expr.body, substitutions),
        loc: expr.loc,
      };

    case "Return":
      return expr;
  }
}

/**
 * Substitute a variable in an ANF expression.
 */
function substituteVar(expr: ANFExpr, oldName: string, newAtom: ANFAtom): ANFExpr {
  function substAtom(atom: ANFAtom): ANFAtom {
    if (atom.tag === "Var" && atom.name === oldName) {
      return newAtom;
    }
    return atom;
  }

  function substPrim(prim: ANFPrim): ANFPrim {
    switch (prim.tag) {
      case "Call":
        return {
          ...prim,
          fn: substAtom(prim.fn),
          args: prim.args.map(substAtom),
        };
      case "Effect":
        return {
          ...prim,
          args: prim.args.map(substAtom),
        };
      case "Prim":
        return {
          ...prim,
          args: prim.args.map(substAtom),
        };
      case "Match":
        return {
          ...prim,
          scrut: substAtom(prim.scrut),
          clauses: prim.clauses.map(c => ({
            ...c,
            body: substituteVar(c.body, oldName, newAtom),
          })),
        };
      case "Handle":
        return {
          ...prim,
          body: substituteVar(prim.body, oldName, newAtom),
          handler: {
            ...prim.handler,
            on: prim.handler.on.map(clause => ({
              ...clause,
              body: substituteVar(clause.body, oldName, newAtom),
            })),
            ret: prim.handler.ret ? {
              ...prim.handler.ret,
              body: substituteVar(prim.handler.ret.body, oldName, newAtom),
            } : undefined,
            fin: prim.handler.fin ? {
              ...prim.handler.fin,
              body: substituteVar(prim.handler.fin.body, oldName, newAtom),
            } : undefined,
          },
        };
      case "Lambda":
        // Don't substitute if the lambda binds the variable
        if (prim.params.includes(oldName)) {
          return prim;
        }
        return {
          ...prim,
          body: substituteVar(prim.body, oldName, newAtom),
        };
      default:
        return prim;
    }
  }

  switch (expr.tag) {
    case "Let":
      // Don't substitute if this let shadows the variable
      if (expr.name === oldName) {
        return {
          ...expr,
          rhs: substPrim(expr.rhs),
        };
      }
      return {
        ...expr,
        rhs: substPrim(expr.rhs),
        body: substituteVar(expr.body, oldName, newAtom),
      };

    case "LetRec":
      // Check if any binding shadows the variable
      if (expr.bindings.some(b => b.name === oldName)) {
        return expr;
      }
      return {
        ...expr,
        bindings: expr.bindings.map(b => ({
          ...b,
          rhs: substPrim(b.rhs),
        })),
        body: substituteVar(expr.body, oldName, newAtom),
      };

    case "If":
      return {
        ...expr,
        test: substAtom(expr.test),
        thn: substituteVar(expr.thn, oldName, newAtom),
        els: substituteVar(expr.els, oldName, newAtom),
      };

    case "Seq":
      return {
        ...expr,
        first: substituteVar(expr.first, oldName, newAtom),
        second: substituteVar(expr.second, oldName, newAtom),
      };

    case "Set":
      return {
        ...expr,
        rhs: substAtom(expr.rhs),
        body: substituteVar(expr.body, oldName, newAtom),
      };

    case "Return":
      return {
        ...expr,
        v: substAtom(expr.v),
      };
  }
}

// ─────────────────────────────────────────────────────────────────
// Oracle Call Counting
// ─────────────────────────────────────────────────────────────────

/**
 * Count potential oracle calls in an ANF program.
 */
export function countOracleCalls(program: ANFProgram): number {
  let count = 0;

  function countPrim(prim: ANFPrim): void {
    if (prim.tag === "Effect" && (prim.op === "infer.op" || prim.op.startsWith("oracle"))) {
      count++;
    }
    if (prim.tag === "Lambda") {
      countExpr(prim.body);
    }
    if (prim.tag === "Handle") {
      countExpr(prim.body);
      for (const clause of prim.handler.on) {
        countExpr(clause.body);
      }
      if (prim.handler.ret) countExpr(prim.handler.ret.body);
      if (prim.handler.fin) countExpr(prim.handler.fin.body);
    }
    if (prim.tag === "Match") {
      for (const clause of prim.clauses) {
        countExpr(clause.body);
      }
    }
  }

  function countExpr(expr: ANFExpr): void {
    switch (expr.tag) {
      case "Let":
        countPrim(expr.rhs);
        countExpr(expr.body);
        break;
      case "LetRec":
        for (const b of expr.bindings) {
          countPrim(b.rhs);
        }
        countExpr(expr.body);
        break;
      case "If":
        countExpr(expr.thn);
        countExpr(expr.els);
        break;
      case "Seq":
        countExpr(expr.first);
        countExpr(expr.second);
        break;
      case "Set":
        countExpr(expr.body);
        break;
      case "Return":
        break;
    }
  }

  countExpr(program.body);
  return count;
}

// ─────────────────────────────────────────────────────────────────
// Optimization Result Computation
// ─────────────────────────────────────────────────────────────────

/**
 * Run CSE optimization and compute result with metrics.
 */
export function optimizeCSE(program: ANFProgram): OptimizationResult {
  const beforeOracleCalls = countOracleCalls(program);
  const beforeBindings = countBindings(program.body);
  const candidates = findCSECandidates(program);

  const { program: optimized, eliminated } = applyCSE(program);

  const afterOracleCalls = countOracleCalls(optimized);
  const afterBindings = countBindings(optimized.body);

  // Generate obligations
  const obligations: CompilerObligation[] = [
    {
      kind: "differential-test",
      description: "CSE optimization must preserve output",
      status: "pending",
    },
    {
      kind: "effect-preservation",
      description: "CSE must not eliminate observable effects",
      status: "pending",
    },
  ];

  return {
    passName: "CSE",
    before: { oracleCalls: beforeOracleCalls, bindings: beforeBindings },
    after: { oracleCalls: afterOracleCalls, bindings: afterBindings },
    candidates,
    obligations,
  };
}

// ─────────────────────────────────────────────────────────────────
// Dead Code Elimination
// ─────────────────────────────────────────────────────────────────

/**
 * Find all variables used in an ANF expression.
 */
function findUsedVars(expr: ANFExpr): Set<string> {
  const used = new Set<string>();

  function addAtom(atom: ANFAtom): void {
    if (atom.tag === "Var") {
      used.add(atom.name);
    }
  }

  function visitPrim(prim: ANFPrim): void {
    switch (prim.tag) {
      case "Call":
        addAtom(prim.fn);
        prim.args.forEach(addAtom);
        break;
      case "Effect":
        prim.args.forEach(addAtom);
        break;
      case "Prim":
        prim.args.forEach(addAtom);
        break;
      case "Lambda":
        visitExpr(prim.body);
        break;
      case "Match":
        addAtom(prim.scrut);
        for (const clause of prim.clauses) {
          visitExpr(clause.body);
        }
        break;
      case "Handle":
        visitExpr(prim.body);
        for (const clause of prim.handler.on) {
          visitExpr(clause.body);
        }
        if (prim.handler.ret) visitExpr(prim.handler.ret.body);
        if (prim.handler.fin) visitExpr(prim.handler.fin.body);
        break;
      case "Quote":
      case "MakeClosure":
        break;
    }
  }

  function visitExpr(e: ANFExpr): void {
    switch (e.tag) {
      case "Let":
        visitPrim(e.rhs);
        visitExpr(e.body);
        break;
      case "LetRec":
        for (const b of e.bindings) {
          visitPrim(b.rhs);
        }
        visitExpr(e.body);
        break;
      case "If":
        addAtom(e.test);
        visitExpr(e.thn);
        visitExpr(e.els);
        break;
      case "Seq":
        visitExpr(e.first);
        visitExpr(e.second);
        break;
      case "Set":
        addAtom(e.rhs);
        visitExpr(e.body);
        break;
      case "Return":
        addAtom(e.v);
        break;
    }
  }

  visitExpr(expr);
  return used;
}

/**
 * Remove dead (unused) bindings.
 */
export function eliminateDeadCode(program: ANFProgram): ANFProgram {
  const used = findUsedVars(program.body);

  function transformExpr(expr: ANFExpr): ANFExpr {
    switch (expr.tag) {
      case "Let":
        // Keep binding if used or if RHS has effects
        if (used.has(expr.name) || hasEffects(expr.rhs)) {
          return {
            ...expr,
            body: transformExpr(expr.body),
          };
        }
        // Skip this binding
        return transformExpr(expr.body);

      case "LetRec":
        const keptBindings = expr.bindings.filter(
          b => used.has(b.name) || hasEffects(b.rhs)
        );
        if (keptBindings.length === 0) {
          return transformExpr(expr.body);
        }
        return {
          ...expr,
          bindings: keptBindings,
          body: transformExpr(expr.body),
        };

      case "If":
        return {
          ...expr,
          thn: transformExpr(expr.thn),
          els: transformExpr(expr.els),
        };

      case "Seq":
        return {
          ...expr,
          first: transformExpr(expr.first),
          second: transformExpr(expr.second),
        };

      case "Set":
        return {
          ...expr,
          body: transformExpr(expr.body),
        };

      case "Return":
        return expr;
    }
  }

  return {
    body: transformExpr(program.body),
    freeVars: program.freeVars,
    defs: program.defs,
  };
}

/**
 * Check if a primitive has side effects.
 */
function hasEffects(prim: ANFPrim): boolean {
  return prim.tag === "Effect" || prim.tag === "Call";
}

// ─────────────────────────────────────────────────────────────────
// Combined Optimization Pipeline
// ─────────────────────────────────────────────────────────────────

/**
 * Run all optimization passes.
 */
export function optimizeANF(
  program: ANFProgram,
  options: { enableCSE?: boolean; enableDCE?: boolean } = {}
): { program: ANFProgram; results: OptimizationResult[] } {
  const results: OptimizationResult[] = [];
  let current = program;

  // CSE pass
  if (options.enableCSE !== false) {
    const cseResult = optimizeCSE(current);
    const { program: cseProgram } = applyCSE(current);
    results.push(cseResult);
    current = cseProgram;
  }

  // DCE pass
  if (options.enableDCE !== false) {
    const beforeBindings = countBindings(current.body);
    current = eliminateDeadCode(current);
    const afterBindings = countBindings(current.body);

    results.push({
      passName: "DCE",
      before: { oracleCalls: countOracleCalls(program), bindings: beforeBindings },
      after: { oracleCalls: countOracleCalls(current), bindings: afterBindings },
      candidates: [],
      obligations: [
        {
          kind: "differential-test",
          description: "DCE must preserve output",
          status: "pending",
        },
      ],
    });
  }

  return { program: current, results };
}
