// src/core/macro/expander.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Prompt 15: Macro expansion engine

import type { Syntax, Scope, SIdent, SList } from "../syntax/syntax";
import { isIdent, isList, addScope, freshScope } from "../syntax/syntax";
import type { Env } from "../syntax/binding";
import { resolveIdent } from "../syntax/binding";
import { applySyntaxRules, type SRTransformer } from "../expand/syntaxRules";
import type {
  MacroTransformer,
  MacroEnv,
  MacroBinding,
  ExpansionResult,
  ExpansionContext,
  ExpansionStep,
  ExpansionTrace,
  ObligationRef,
  EvidenceRef,
  MacroProfile,
  SemanticMacroConfig,
  DEFAULT_SEMANTIC_CONFIG,
} from "./types";
import { getMacroProfile, MACRO_PROFILES } from "./types";
import {
  makeUseScope,
  makeIntroducerScope,
  resolveMacroIdent,
  addScopeToSyntax,
  flipScopeOnSyntax,
  collectPatternVariables,
  applyHygiene,
} from "./hygiene";

// ─────────────────────────────────────────────────────────────────
// Expansion State
// ─────────────────────────────────────────────────────────────────

/**
 * Global expansion state for tracking across nested expansions.
 */
let expansionEvents: ExpansionStep[] = [];
let totalExpansionSteps = 0;

/**
 * Clear expansion state (for testing).
 */
export function clearExpansionState(): void {
  expansionEvents = [];
  totalExpansionSteps = 0;
}

/**
 * Get expansion events (for debugging/replay).
 */
export function getExpansionEvents(): ExpansionStep[] {
  return [...expansionEvents];
}

// ─────────────────────────────────────────────────────────────────
// Core Expansion Functions
// ─────────────────────────────────────────────────────────────────

/**
 * Create a default expansion context.
 */
export function makeExpansionContext(
  runtimeEnv: Env,
  macroEnv: MacroEnv,
  profileName: string = "pragmatic"
): ExpansionContext {
  const profile = getMacroProfile(profileName);
  return {
    phase: 1,
    runtimeEnv,
    macroEnv,
    scopeCounter: { n: 0 },
    profileName,
    caps: new Set(["cap.macro.expand"]),
    stepBudget: profile.stepBudget,
    stepCount: 0,
    tracing: false,
    trace: [],
  };
}

/**
 * Expand syntax to fixed point.
 *
 * This is the main entry point for macro expansion.
 */
export function expand(stx: Syntax, ctx: ExpansionContext): ExpansionResult {
  const obligations: ObligationRef[] = [];
  const evidence: EvidenceRef[] = [];
  let semantic = false;
  let current = stx;
  let expanded = true;

  while (expanded && ctx.stepCount < ctx.stepBudget) {
    const result = macroexpand1(current, ctx);
    if (result.expanded === current) {
      expanded = false;
    } else {
      current = result.expanded;
      obligations.push(...result.obligations);
      evidence.push(...result.evidence);
      semantic = semantic || result.semantic;
    }
  }

  // Recursively expand subforms
  const finalResult = expandSubforms(current, ctx);
  obligations.push(...finalResult.obligations);
  evidence.push(...finalResult.evidence);
  semantic = semantic || finalResult.semantic;

  return {
    expanded: finalResult.expanded,
    obligations,
    evidence,
    semantic,
    trace: ctx.tracing ? { steps: ctx.trace, totalSteps: ctx.stepCount, macrosInvoked: [], semanticCalls: 0 } : undefined,
  };
}

/**
 * Perform a single expansion step.
 */
export function macroexpand1(stx: Syntax, ctx: ExpansionContext): ExpansionResult {
  const noChange: ExpansionResult = {
    expanded: stx,
    obligations: [],
    evidence: [],
    semantic: false,
  };

  // Only lists can be macro applications
  if (!isList(stx) || stx.items.length === 0) {
    return noChange;
  }

  const head = stx.items[0];
  if (!isIdent(head)) {
    return noChange;
  }

  // Look up macro binding
  const macroBinding = resolveMacroIdent(head, ctx.macroEnv, ctx.phase);
  if (!macroBinding) {
    return noChange;
  }

  // Check profile permissions
  const profile = getMacroProfile(ctx.profileName ?? "pragmatic");
  const transformer = macroBinding.transformer;

  if (transformer.tag === "SemanticMacro" && !profile.canExpandSemantic) {
    return {
      expanded: stx,
      obligations: [],
      evidence: [],
      semantic: false,
    };
  }

  // Apply the macro transformer
  ctx.stepCount++;
  totalExpansionSteps++;

  const useScope = makeUseScope(macroBinding.name, ctx.scopeCounter);
  const introScope = makeIntroducerScope(macroBinding.name, ctx.scopeCounter);

  // Add use scope to input
  const inputWithScope = addScopeToSyntax(stx, useScope);

  let result: ExpansionResult;

  switch (transformer.tag) {
    case "SyntaxRules":
      result = expandSyntaxRules(
        transformer.transformer,
        inputWithScope,
        useScope,
        introScope,
        ctx
      );
      break;

    case "ProcMacro":
      result = expandProcMacro(
        transformer,
        inputWithScope,
        useScope,
        introScope,
        ctx
      );
      break;

    case "SemanticMacro":
      result = expandSemanticMacro(
        transformer,
        inputWithScope,
        useScope,
        introScope,
        ctx
      );
      break;
  }

  // Record expansion step
  if (ctx.tracing) {
    ctx.trace.push({
      stepNum: ctx.stepCount,
      macro: macroBinding.name,
      input: stx,
      output: result.expanded,
      timestamp: Date.now(),
      semantic: result.semantic,
    });
  }

  expansionEvents.push({
    stepNum: totalExpansionSteps,
    macro: macroBinding.name,
    input: stx,
    output: result.expanded,
    timestamp: Date.now(),
    semantic: result.semantic,
  });

  return result;
}

/**
 * Expand to fixed point.
 */
export function macroexpand(stx: Syntax, ctx: ExpansionContext): ExpansionResult {
  const obligations: ObligationRef[] = [];
  const evidence: EvidenceRef[] = [];
  let semantic = false;
  let current = stx;
  let maxIter = ctx.stepBudget;

  while (maxIter-- > 0) {
    const result = macroexpand1(current, ctx);
    if (result.expanded === current) {
      break;
    }
    current = result.expanded;
    obligations.push(...result.obligations);
    evidence.push(...result.evidence);
    semantic = semantic || result.semantic;
  }

  return {
    expanded: current,
    obligations,
    evidence,
    semantic,
  };
}

// ─────────────────────────────────────────────────────────────────
// Transformer Application
// ─────────────────────────────────────────────────────────────────

/**
 * Expand using syntax-rules transformer.
 */
function expandSyntaxRules(
  tr: SRTransformer,
  input: Syntax,
  useScope: Scope,
  introScope: Scope,
  ctx: ExpansionContext
): ExpansionResult {
  try {
    // Apply syntax-rules (already handles hygiene)
    const output = applySyntaxRules(tr, input, ctx.runtimeEnv, ctx.scopeCounter);

    // Flip use scope and add intro scope
    const hygienic = flipScopeOnSyntax(output, useScope);

    return {
      expanded: hygienic,
      obligations: [],
      evidence: [],
      semantic: false,
    };
  } catch (e) {
    // No rule matched - return unchanged
    return {
      expanded: input,
      obligations: [],
      evidence: [],
      semantic: false,
    };
  }
}

/**
 * Expand using proc macro (procedural macro).
 */
function expandProcMacro(
  transformer: Extract<MacroTransformer, { tag: "ProcMacro" }>,
  input: Syntax,
  useScope: Scope,
  introScope: Scope,
  ctx: ExpansionContext
): ExpansionResult {
  // For now, proc macros are not fully implemented
  // They would invoke the proc closure with the input syntax
  return {
    expanded: input,
    obligations: [],
    evidence: [],
    semantic: false,
  };
}

/**
 * Expand using semantic macro (with inference support).
 */
function expandSemanticMacro(
  transformer: Extract<MacroTransformer, { tag: "SemanticMacro" }>,
  input: Syntax,
  useScope: Scope,
  introScope: Scope,
  ctx: ExpansionContext
): ExpansionResult {
  const profile = getMacroProfile(ctx.profileName ?? "pragmatic");

  // Check if semantic expansion is allowed
  if (!profile.canExpandSemantic) {
    return {
      expanded: input,
      obligations: [{
        id: `denied-${Date.now()}`,
        kind: "contract",
        description: "Semantic macro expansion denied by profile",
        satisfied: false,
      }],
      evidence: [],
      semantic: true,
    };
  }

  // For now, semantic macros return input with obligations
  // Full implementation would invoke inference
  const obligations: ObligationRef[] = [];

  if (transformer.obligations) {
    for (const kind of transformer.obligations.required) {
      obligations.push({
        id: `obligation-${kind}-${Date.now()}`,
        kind,
        description: `Required ${kind} obligation for semantic macro`,
        satisfied: false,
      });
    }
  }

  return {
    expanded: input,
    obligations,
    evidence: [],
    semantic: true,
  };
}

// ─────────────────────────────────────────────────────────────────
// Subform Expansion
// ─────────────────────────────────────────────────────────────────

/**
 * Recursively expand subforms of a syntax tree.
 */
function expandSubforms(stx: Syntax, ctx: ExpansionContext): ExpansionResult {
  switch (stx.tag) {
    case "Atom":
    case "Ident":
      return { expanded: stx, obligations: [], evidence: [], semantic: false };

    case "List": {
      if (stx.items.length === 0) {
        return { expanded: stx, obligations: [], evidence: [], semantic: false };
      }

      const head = stx.items[0];

      // Check for special forms that don't expand their bodies normally
      if (isIdent(head)) {
        const name = head.name;

        // quote doesn't expand its body
        if (name === "quote") {
          return { expanded: stx, obligations: [], evidence: [], semantic: false };
        }

        // define-syntax processes differently
        if (name === "define-syntax") {
          return expandDefineSyntax(stx, ctx);
        }

        // define-syntax/semantic processes differently
        if (name === "define-syntax/semantic") {
          return expandDefineSemanticSyntax(stx, ctx);
        }

        // lambda/define expand their bodies
        if (name === "lambda" || name === "λ") {
          return expandLambda(stx, ctx);
        }

        if (name === "define") {
          return expandDefine(stx, ctx);
        }

        if (name === "let" || name === "let*" || name === "letrec") {
          return expandLet(stx, name, ctx);
        }
      }

      // For other lists, expand all subforms
      const obligations: ObligationRef[] = [];
      const evidence: EvidenceRef[] = [];
      let semantic = false;
      const newItems: Syntax[] = [];

      for (const item of stx.items) {
        const result = expand(item, ctx);
        newItems.push(result.expanded);
        obligations.push(...result.obligations);
        evidence.push(...result.evidence);
        semantic = semantic || result.semantic;
      }

      return {
        expanded: { ...stx, items: newItems },
        obligations,
        evidence,
        semantic,
      };
    }
  }
}

/**
 * Expand define-syntax form.
 */
function expandDefineSyntax(stx: SList, ctx: ExpansionContext): ExpansionResult {
  // (define-syntax name transformer)
  if (stx.items.length < 3) {
    return { expanded: stx, obligations: [], evidence: [], semantic: false };
  }

  const nameStx = stx.items[1];
  const transformerStx = stx.items[2];

  if (!isIdent(nameStx)) {
    return { expanded: stx, obligations: [], evidence: [], semantic: false };
  }

  // For now, just return the form as-is
  // The actual binding happens at a higher level
  return { expanded: stx, obligations: [], evidence: [], semantic: false };
}

/**
 * Expand define-syntax/semantic form.
 */
function expandDefineSemanticSyntax(stx: SList, ctx: ExpansionContext): ExpansionResult {
  // (define-syntax/semantic name transformer :obligations spec)
  return {
    expanded: stx,
    obligations: [{
      id: `semantic-def-${Date.now()}`,
      kind: "test",
      description: "Semantic macro definition requires test obligations",
      satisfied: false,
    }],
    evidence: [],
    semantic: true,
  };
}

/**
 * Expand lambda form with hygiene for parameters.
 */
function expandLambda(stx: SList, ctx: ExpansionContext): ExpansionResult {
  // (lambda (params...) body...)
  if (stx.items.length < 3) {
    return { expanded: stx, obligations: [], evidence: [], semantic: false };
  }

  const paramsStx = stx.items[1];
  const bodyItems = stx.items.slice(2);

  if (!isList(paramsStx)) {
    return { expanded: stx, obligations: [], evidence: [], semantic: false };
  }

  // Expand body forms
  const obligations: ObligationRef[] = [];
  const evidence: EvidenceRef[] = [];
  let semantic = false;
  const expandedBody: Syntax[] = [];

  for (const body of bodyItems) {
    const result = expand(body, ctx);
    expandedBody.push(result.expanded);
    obligations.push(...result.obligations);
    evidence.push(...result.evidence);
    semantic = semantic || result.semantic;
  }

  return {
    expanded: {
      ...stx,
      items: [stx.items[0], paramsStx, ...expandedBody],
    },
    obligations,
    evidence,
    semantic,
  };
}

/**
 * Expand define form.
 */
function expandDefine(stx: SList, ctx: ExpansionContext): ExpansionResult {
  // (define name expr) or (define (name params...) body...)
  if (stx.items.length < 3) {
    return { expanded: stx, obligations: [], evidence: [], semantic: false };
  }

  const secondItem = stx.items[1];

  if (isList(secondItem)) {
    // Function shorthand: (define (name params...) body...)
    const bodyItems = stx.items.slice(2);
    const obligations: ObligationRef[] = [];
    const evidence: EvidenceRef[] = [];
    let semantic = false;
    const expandedBody: Syntax[] = [];

    for (const body of bodyItems) {
      const result = expand(body, ctx);
      expandedBody.push(result.expanded);
      obligations.push(...result.obligations);
      evidence.push(...result.evidence);
      semantic = semantic || result.semantic;
    }

    return {
      expanded: {
        ...stx,
        items: [stx.items[0], secondItem, ...expandedBody],
      },
      obligations,
      evidence,
      semantic,
    };
  }

  // Simple define: (define name expr)
  const exprResult = expand(stx.items[2], ctx);
  return {
    expanded: {
      ...stx,
      items: [stx.items[0], stx.items[1], exprResult.expanded],
    },
    obligations: exprResult.obligations,
    evidence: exprResult.evidence,
    semantic: exprResult.semantic,
  };
}

/**
 * Expand let, let*, and letrec forms.
 */
function expandLet(stx: SList, letKind: string, ctx: ExpansionContext): ExpansionResult {
  // (let ((name val) ...) body...)
  if (stx.items.length < 3) {
    return { expanded: stx, obligations: [], evidence: [], semantic: false };
  }

  const bindingsStx = stx.items[1];
  const bodyItems = stx.items.slice(2);

  if (!isList(bindingsStx)) {
    return { expanded: stx, obligations: [], evidence: [], semantic: false };
  }

  const obligations: ObligationRef[] = [];
  const evidence: EvidenceRef[] = [];
  let semantic = false;

  // Expand binding values
  const expandedBindings: Syntax[] = [];
  for (const binding of bindingsStx.items) {
    if (!isList(binding) || binding.items.length < 2) {
      expandedBindings.push(binding);
      continue;
    }

    const nameStx = binding.items[0];
    const valStx = binding.items[1];
    const valResult = expand(valStx, ctx);

    expandedBindings.push({
      ...binding,
      items: [nameStx, valResult.expanded],
    });
    obligations.push(...valResult.obligations);
    evidence.push(...valResult.evidence);
    semantic = semantic || valResult.semantic;
  }

  // Expand body
  const expandedBody: Syntax[] = [];
  for (const body of bodyItems) {
    const result = expand(body, ctx);
    expandedBody.push(result.expanded);
    obligations.push(...result.obligations);
    evidence.push(...result.evidence);
    semantic = semantic || result.semantic;
  }

  return {
    expanded: {
      ...stx,
      items: [
        stx.items[0],
        { ...bindingsStx, items: expandedBindings },
        ...expandedBody,
      ],
    },
    obligations,
    evidence,
    semantic,
  };
}

// ─────────────────────────────────────────────────────────────────
// Macro Environment Management
// ─────────────────────────────────────────────────────────────────

let nextBindingId = 0;

/**
 * Create an empty macro environment.
 */
export function createMacroEnv(): MacroEnv {
  return new Map();
}

/**
 * Add a macro binding to the environment.
 */
export function addMacroBinding(
  env: MacroEnv,
  name: string,
  transformer: MacroTransformer,
  phase: number,
  scopes: Scope[]
): MacroEnv {
  const bindingId = `macro-${name}-${nextBindingId++}`;
  const binding: MacroBinding = {
    name,
    transformer,
    phase,
    scopes,
    bindingId,
  };

  const newEnv = new Map(env);
  newEnv.set(bindingId, binding);
  return newEnv;
}

/**
 * Reset binding ID counter (for testing).
 */
export function resetBindingIds(): void {
  nextBindingId = 0;
}

// ─────────────────────────────────────────────────────────────────
// Convenience Functions
// ─────────────────────────────────────────────────────────────────

/**
 * Create a syntax-rules macro transformer.
 */
export function makeSyntaxRulesTransformer(
  tr: SRTransformer,
  defScope: Scope,
  name?: string
): MacroTransformer {
  return {
    tag: "SyntaxRules",
    transformer: tr,
    defScope,
    name,
  };
}

/**
 * Create a semantic macro transformer.
 */
export function makeSemanticTransformer(
  proc: import("../eval/values").Val,
  defScope: Scope,
  name?: string,
  obligations?: import("./types").ObligationSpec
): MacroTransformer {
  return {
    tag: "SemanticMacro",
    proc,
    defScope,
    obligations,
    name,
  };
}

/**
 * Check if expansion is cacheable (deterministic).
 */
export function isExpansionCacheable(result: ExpansionResult): boolean {
  // Semantic expansions are not cacheable (involve inference)
  return !result.semantic;
}

/**
 * Generate expansion receipt hash.
 */
export function generateExpansionReceipt(
  input: Syntax,
  result: ExpansionResult
): string {
  // Simple hash for now
  const data = JSON.stringify({
    input,
    output: result.expanded,
    semantic: result.semantic,
  });

  // Simple string hash
  let hash = 0;
  for (let i = 0; i < data.length; i++) {
    hash = ((hash << 5) - hash) + data.charCodeAt(i);
    hash |= 0;
  }
  return `receipt-${Math.abs(hash).toString(16)}`;
}
