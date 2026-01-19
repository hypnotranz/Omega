// src/core/meta/dsl.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-18.md
// Prompt 18: DSL Definition and Interpretation

import type {
  Omega0Expr,
  Omega0Val,
  Omega0Env,
  Omega0Meaning,
  Omega0Prim,
  EvalContext,
} from "./types";
import {
  makePrim,
  makeMeaning,
  omega0ToString,
  createEvalContext,
  isSymbol,
  isPair,
  sym,
  arrayToList,
  listToArray,
} from "./types";
import { defineVar, extendEnv, emptyEnv } from "./env";
import { eval0, parseSimpleSExpr } from "./eval0";
import { int0, createIntContext, createFallbackOracle } from "./int0";
import type { IntContext, OracleHandler } from "./int0";
import { createBaseEnv } from "./primitives";

// ─────────────────────────────────────────────────────────────────
// DSL Types
// ─────────────────────────────────────────────────────────────────

/**
 * DSL syntax rule - defines how to parse/recognize a form.
 */
export type SyntaxRule = {
  /** Pattern to match (symbol for keyword, or structure) */
  pattern: Omega0Expr;
  /** Names of bound variables in the pattern */
  bindings: string[];
  /** Description of this syntax form */
  description: string;
};

/**
 * DSL semantic rule - defines meaning of a form.
 */
export type SemanticRule = {
  /** Name of the syntax form this handles */
  formName: string;
  /** Extensional semantics: transform to Ω₀ */
  extensional: (args: Omega0Val[], env: Omega0Env) => Omega0Expr;
  /** Intensional semantics: predict meaning */
  intensional?: (args: Omega0Val[], env: Omega0Env, ctx: IntContext) => Omega0Meaning;
};

/**
 * DSL definition - a complete language specification.
 */
export type DSLDefinition = {
  /** Name of the DSL */
  name: string;
  /** Description */
  description: string;
  /** Syntax rules */
  syntax: SyntaxRule[];
  /** Semantic rules */
  semantics: SemanticRule[];
  /** Base environment additions */
  baseEnv?: Map<string, Omega0Val>;
  /** Parent DSL (for extension) */
  parent?: DSLDefinition;
};

/**
 * DSL instance - a language ready for use.
 */
export type DSLInstance = {
  /** The definition */
  definition: DSLDefinition;
  /** Compiled environment */
  env: Omega0Env;
  /** Syntax matcher */
  match: (expr: Omega0Expr) => SyntaxMatch | null;
  /** Extensional evaluator */
  eval: (expr: Omega0Expr, env?: Omega0Env) => Omega0Val;
  /** Intensional evaluator */
  int: (expr: Omega0Expr, env?: Omega0Env, ctx?: IntContext) => Omega0Meaning;
};

/**
 * Result of syntax matching.
 */
export type SyntaxMatch = {
  rule: SyntaxRule;
  bindings: Map<string, Omega0Val>;
  semanticRule: SemanticRule;
};

// ─────────────────────────────────────────────────────────────────
// DSL Builder
// ─────────────────────────────────────────────────────────────────

/**
 * Create a new DSL definition.
 */
export function defineDSL(
  name: string,
  description: string,
  parent?: DSLDefinition
): DSLDefinition {
  return {
    name,
    description,
    syntax: parent ? [...parent.syntax] : [],
    semantics: parent ? [...parent.semantics] : [],
    baseEnv: parent?.baseEnv ? new Map(parent.baseEnv) : new Map(),
    parent,
  };
}

/**
 * Add a syntax rule to a DSL.
 */
export function addSyntax(
  dsl: DSLDefinition,
  pattern: string | Omega0Expr,
  bindings: string[],
  description: string
): DSLDefinition {
  const parsedPattern = typeof pattern === "string"
    ? parseSimpleSExpr(pattern)
    : pattern;

  return {
    ...dsl,
    syntax: [
      ...dsl.syntax,
      { pattern: parsedPattern, bindings, description },
    ],
  };
}

/**
 * Add a semantic rule to a DSL.
 */
export function addSemantics(
  dsl: DSLDefinition,
  formName: string,
  extensional: (args: Omega0Val[], env: Omega0Env) => Omega0Expr,
  intensional?: (args: Omega0Val[], env: Omega0Env, ctx: IntContext) => Omega0Meaning
): DSLDefinition {
  return {
    ...dsl,
    semantics: [
      ...dsl.semantics,
      { formName, extensional, intensional },
    ],
  };
}

/**
 * Add a base binding to a DSL.
 */
export function addBinding(
  dsl: DSLDefinition,
  name: string,
  value: Omega0Val
): DSLDefinition {
  const newBaseEnv = new Map(dsl.baseEnv);
  newBaseEnv.set(name, value);
  return {
    ...dsl,
    baseEnv: newBaseEnv,
  };
}

/**
 * Add a primitive to a DSL.
 */
export function addPrimitive(
  dsl: DSLDefinition,
  name: string,
  fn: (args: Omega0Val[]) => Omega0Val
): DSLDefinition {
  return addBinding(dsl, name, makePrim(name, fn));
}

// ─────────────────────────────────────────────────────────────────
// DSL Instantiation
// ─────────────────────────────────────────────────────────────────

/**
 * Instantiate a DSL definition, creating a usable language.
 */
export function instantiateDSL(definition: DSLDefinition): DSLInstance {
  // Create base environment
  let env = createBaseEnv();

  // Add DSL-specific bindings
  if (definition.baseEnv) {
    for (const [name, value] of definition.baseEnv) {
      env = defineVar(env, name, value);
    }
  }

  // Create syntax matcher
  const match = (expr: Omega0Expr): SyntaxMatch | null => {
    for (const rule of definition.syntax) {
      const bindings = matchPattern(rule.pattern, expr, rule.bindings);
      if (bindings) {
        const semanticRule = definition.semantics.find(
          s => s.formName === getFormName(rule.pattern)
        );
        if (semanticRule) {
          return { rule, bindings, semanticRule };
        }
      }
    }
    return null;
  };

  // Create extensional evaluator
  const evalFn = (expr: Omega0Expr, customEnv?: Omega0Env): Omega0Val => {
    const evalEnv = customEnv ?? env;
    const matched = match(expr);

    if (matched) {
      const args = matched.rule.bindings.map(
        b => matched.bindings.get(b) ?? null
      );
      const transformed = matched.semanticRule.extensional(args, evalEnv);
      return eval0(transformed, evalEnv, createEvalContext());
    }

    // Fall back to base Ω₀ evaluation
    return eval0(expr, evalEnv, createEvalContext());
  };

  // Create intensional evaluator
  const intFn = (
    expr: Omega0Expr,
    customEnv?: Omega0Env,
    ctx?: IntContext
  ): Omega0Meaning => {
    const evalEnv = customEnv ?? env;
    const intCtx = ctx ?? createIntContext(createFallbackOracle());
    const matched = match(expr);

    if (matched && matched.semanticRule.intensional) {
      const args = matched.rule.bindings.map(
        b => matched.bindings.get(b) ?? null
      );
      return matched.semanticRule.intensional(args, evalEnv, intCtx);
    }

    if (matched) {
      // Use extensional semantics with meaning wrapper
      const args = matched.rule.bindings.map(
        b => matched.bindings.get(b) ?? null
      );
      const transformed = matched.semanticRule.extensional(args, evalEnv);
      return int0(transformed, evalEnv, intCtx);
    }

    // Fall back to base Ω₀ intensional evaluation
    return int0(expr, evalEnv, intCtx);
  };

  return {
    definition,
    env,
    match,
    eval: evalFn,
    int: intFn,
  };
}

/**
 * Get the form name from a pattern (first symbol).
 */
function getFormName(pattern: Omega0Expr): string {
  if (Array.isArray(pattern) && pattern.length > 0 && isSymbol(pattern[0])) {
    return pattern[0].name;
  }
  if (isSymbol(pattern)) {
    return pattern.name;
  }
  return "";
}

/**
 * Match a pattern against an expression, extracting bindings.
 */
function matchPattern(
  pattern: Omega0Expr,
  expr: Omega0Expr,
  bindingNames: string[]
): Map<string, Omega0Val> | null {
  const bindings = new Map<string, Omega0Val>();

  function match(p: Omega0Expr, e: Omega0Expr): boolean {
    // Symbol in pattern: check if it's a binding variable
    if (isSymbol(p)) {
      if (bindingNames.includes(p.name)) {
        bindings.set(p.name, e as Omega0Val);
        return true;
      }
      // Otherwise must match exactly
      return isSymbol(e) && p.name === e.name;
    }

    // Primitive types must match exactly
    if (typeof p === "number" || typeof p === "string" || typeof p === "boolean") {
      return p === e;
    }

    // Null matches null
    if (p === null) {
      return e === null;
    }

    // Array patterns
    if (Array.isArray(p) && Array.isArray(e)) {
      if (p.length !== e.length) return false;
      for (let i = 0; i < p.length; i++) {
        if (!match(p[i], e[i])) return false;
      }
      return true;
    }

    return false;
  }

  return match(pattern, expr) ? bindings : null;
}

// ─────────────────────────────────────────────────────────────────
// Built-in DSL: Simple Calculator
// ─────────────────────────────────────────────────────────────────

/**
 * Create a simple calculator DSL for demonstration.
 */
export function createCalculatorDSL(): DSLInstance {
  let dsl = defineDSL("calculator", "Simple arithmetic calculator");

  // Syntax: (calc expr)
  dsl = addSyntax(dsl, "(calc x)", ["x"], "Calculate an arithmetic expression");

  // Semantics: transform to direct evaluation
  dsl = addSemantics(
    dsl,
    "calc",
    (args) => args[0] as Omega0Expr,
    (args, env, ctx) => {
      // Intensional: predict with high confidence for simple arithmetic
      const expr = args[0] as Omega0Expr;
      return int0(expr, env, ctx);
    }
  );

  return instantiateDSL(dsl);
}

// ─────────────────────────────────────────────────────────────────
// Built-in DSL: Query Language
// ─────────────────────────────────────────────────────────────────

/**
 * Create a simple query DSL.
 */
export function createQueryDSL(): DSLInstance {
  let dsl = defineDSL("query", "Simple query language");

  // Syntax: (select fields from source where pred)
  dsl = addSyntax(
    dsl,
    "(select fields from source where pred)",
    ["fields", "source", "pred"],
    "Select fields from a source where predicate holds"
  );

  // Syntax: (select fields from source)
  dsl = addSyntax(
    dsl,
    "(select fields from source)",
    ["fields", "source"],
    "Select fields from a source"
  );

  // Semantics for select-where
  dsl = addSemantics(
    dsl,
    "select",
    (args, _env) => {
      const [fields, _from, source, _where, pred] = args;
      // Transform to filter + map
      return [
        sym("map"),
        [sym("lambda"), [sym("item")],
          [sym("list"), ...(listToArray(fields as Omega0Val).map(f =>
            [sym("item"), f]
          ))]
        ],
        [sym("filter"), pred, source]
      ] as Omega0Expr;
    }
  );

  // Add filter primitive
  dsl = addPrimitive(dsl, "filter", (args) => {
    if (args.length < 2) return null;
    // Simplified filter - in real impl would apply predicate
    return args[1];
  });

  return instantiateDSL(dsl);
}

// ─────────────────────────────────────────────────────────────────
// Built-in DSL: State Machine
// ─────────────────────────────────────────────────────────────────

/**
 * Create a state machine DSL.
 */
export function createStateMachineDSL(): DSLInstance {
  let dsl = defineDSL("state-machine", "Simple state machine language");

  // Syntax: (machine name initial-state transitions)
  dsl = addSyntax(
    dsl,
    "(machine name initial transitions)",
    ["name", "initial", "transitions"],
    "Define a state machine"
  );

  // Syntax: (transition from event to)
  dsl = addSyntax(
    dsl,
    "(transition from on to)",
    ["from", "on", "to"],
    "Define a state transition"
  );

  // Semantics: machine creates a closure
  dsl = addSemantics(
    dsl,
    "machine",
    (args) => {
      const [name, initial, transitions] = args;
      // Create a closure that tracks state
      return [
        sym("lambda"), [sym("event")],
        [sym("begin"),
          [sym("display"), [sym("string-append"),
            "Machine ", omega0ToString(name as Omega0Val),
            " processing: ", [sym("event")]
          ]],
          initial  // Simplified: just return initial state
        ]
      ] as Omega0Expr;
    }
  );

  return instantiateDSL(dsl);
}

// ─────────────────────────────────────────────────────────────────
// DSL Composition
// ─────────────────────────────────────────────────────────────────

/**
 * Compose two DSLs into a new one.
 */
export function composeDSLs(
  name: string,
  description: string,
  ...dsls: DSLDefinition[]
): DSLDefinition {
  const composed: DSLDefinition = {
    name,
    description,
    syntax: [],
    semantics: [],
    baseEnv: new Map(),
  };

  for (const dsl of dsls) {
    composed.syntax.push(...dsl.syntax);
    composed.semantics.push(...dsl.semantics);
    if (dsl.baseEnv) {
      for (const [k, v] of dsl.baseEnv) {
        composed.baseEnv!.set(k, v);
      }
    }
  }

  return composed;
}

// ─────────────────────────────────────────────────────────────────
// DSL Analysis
// ─────────────────────────────────────────────────────────────────

/**
 * Analyze a DSL definition for potential issues.
 */
export function analyzeDSL(dsl: DSLDefinition): DSLAnalysis {
  const issues: string[] = [];
  const stats = {
    syntaxRules: dsl.syntax.length,
    semanticRules: dsl.semantics.length,
    hasIntensional: 0,
    baseBindings: dsl.baseEnv?.size ?? 0,
  };

  // Check for syntax rules without semantics
  for (const syntax of dsl.syntax) {
    const formName = getFormName(syntax.pattern);
    const hasSemantics = dsl.semantics.some(s => s.formName === formName);
    if (!hasSemantics) {
      issues.push(`Syntax rule '${formName}' has no semantic rule`);
    }
  }

  // Count intensional rules
  stats.hasIntensional = dsl.semantics.filter(s => s.intensional).length;

  // Check for semantic rules without syntax
  for (const sem of dsl.semantics) {
    const hasSyntax = dsl.syntax.some(
      s => getFormName(s.pattern) === sem.formName
    );
    if (!hasSyntax) {
      issues.push(`Semantic rule '${sem.formName}' has no syntax rule`);
    }
  }

  return { dsl, issues, stats };
}

/**
 * DSL analysis result.
 */
export type DSLAnalysis = {
  dsl: DSLDefinition;
  issues: string[];
  stats: {
    syntaxRules: number;
    semanticRules: number;
    hasIntensional: number;
    baseBindings: number;
  };
};

// ─────────────────────────────────────────────────────────────────
// DSL Serialization
// ─────────────────────────────────────────────────────────────────

/**
 * Serialize a DSL definition (without functions) for storage/transmission.
 */
export function serializeDSL(dsl: DSLDefinition): object {
  return {
    name: dsl.name,
    description: dsl.description,
    syntaxCount: dsl.syntax.length,
    semanticsCount: dsl.semantics.length,
    syntax: dsl.syntax.map(s => ({
      pattern: omega0ToString(s.pattern as Omega0Val),
      bindings: s.bindings,
      description: s.description,
    })),
    // Note: semantic functions cannot be serialized
    semanticNames: dsl.semantics.map(s => s.formName),
  };
}

/**
 * Create a DSL documentation string.
 */
export function documentDSL(dsl: DSLDefinition): string {
  const lines: string[] = [];

  lines.push(`# ${dsl.name}`);
  lines.push(`${dsl.description}`);
  lines.push("");
  lines.push("## Syntax Forms");

  for (const syntax of dsl.syntax) {
    lines.push(`- ${omega0ToString(syntax.pattern as Omega0Val)}`);
    lines.push(`  ${syntax.description}`);
    if (syntax.bindings.length > 0) {
      lines.push(`  Bindings: ${syntax.bindings.join(", ")}`);
    }
  }

  if (dsl.baseEnv && dsl.baseEnv.size > 0) {
    lines.push("");
    lines.push("## Built-in Bindings");
    for (const [name] of dsl.baseEnv) {
      lines.push(`- ${name}`);
    }
  }

  return lines.join("\n");
}
