// src/core/macro/types.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Prompt 15: Hygienic macros + phase separation + semantic macros

import type { Val } from "../eval/values";
import type { Syntax, Scope } from "../syntax/syntax";
import type { Env, Binding } from "../syntax/binding";
import type { Hash } from "../artifacts/hash";
import type { SRTransformer } from "../expand/syntaxRules";

// ─────────────────────────────────────────────────────────────────
// Core Macro Types
// ─────────────────────────────────────────────────────────────────

/**
 * ScopeId: Unique identifier for a scope in set-of-scopes hygiene.
 */
export type ScopeId = string;

/**
 * SourceLocation: Source file location for syntax objects.
 */
export type SourceLocation = {
  file?: string;
  line?: number;
  col?: number;
  span?: number;
};

/**
 * MacroTransformer: A macro transformer (pure or semantic).
 *
 * Pure macros (syntax-rules) are deterministic syntax-to-syntax transforms.
 * Semantic macros can invoke inference/rewrite under governance.
 */
export type MacroTransformer =
  | {
      tag: "SyntaxRules";
      transformer: SRTransformer;
      defScope: ScopeId;
      name?: string;
    }
  | {
      tag: "ProcMacro";
      proc: Val;
      defScope: ScopeId;
      allowInfer: boolean;
      name?: string;
    }
  | {
      tag: "SemanticMacro";
      proc: Val;
      defScope: ScopeId;
      obligations?: ObligationSpec;
      name?: string;
    };

/**
 * MacroEnv: Compile-time environment mapping macro names to transformers.
 *
 * Separate from runtime Env (phase separation).
 */
export type MacroEnv = Map<string, MacroBinding>;

/**
 * MacroBinding: A binding in the macro environment.
 */
export type MacroBinding = {
  name: string;
  transformer: MacroTransformer;
  phase: number;
  scopes: Scope[];
  bindingId: string;
};

// ─────────────────────────────────────────────────────────────────
// Expansion Types
// ─────────────────────────────────────────────────────────────────

/**
 * ExpansionResult: Result of macro expansion.
 *
 * Includes expanded syntax and any obligations/evidence for semantic macros.
 */
export type ExpansionResult = {
  /** The expanded syntax */
  expanded: Syntax;
  /** Obligations generated during expansion (for semantic macros) */
  obligations: ObligationRef[];
  /** Evidence collected during expansion */
  evidence: EvidenceRef[];
  /** Whether expansion was semantic (involved inference) */
  semantic: boolean;
  /** Expansion trace for debugging/replay */
  trace?: ExpansionTrace;
};

/**
 * ExpansionTrace: Trace of macro expansion steps for debugging/replay.
 */
export type ExpansionTrace = {
  steps: ExpansionStep[];
  totalSteps: number;
  macrosInvoked: string[];
  semanticCalls: number;
};

/**
 * ExpansionStep: A single step in macro expansion.
 */
export type ExpansionStep = {
  /** Step number */
  stepNum: number;
  /** Macro name invoked */
  macro: string;
  /** Input syntax (before expansion) */
  input: Syntax;
  /** Output syntax (after expansion) */
  output: Syntax;
  /** Timestamp */
  timestamp: number;
  /** Whether this was a semantic expansion */
  semantic: boolean;
  /** Source location */
  srcloc?: SourceLocation;
};

/**
 * ExpansionContext: Context for macro expansion.
 */
export type ExpansionContext = {
  /** Current phase (0 = runtime, 1 = macro expansion) */
  phase: number;
  /** Runtime environment */
  runtimeEnv: Env;
  /** Macro environment */
  macroEnv: MacroEnv;
  /** Scope counter for generating fresh scopes */
  scopeCounter: { n: number };
  /** Profile name for governance */
  profileName?: string;
  /** Capabilities for this expansion */
  caps: Set<string>;
  /** Budget for expansion steps */
  stepBudget: number;
  /** Current step count */
  stepCount: number;
  /** Whether to trace expansion */
  tracing: boolean;
  /** Accumulated trace */
  trace: ExpansionStep[];
};

// ─────────────────────────────────────────────────────────────────
// Obligation Types (for semantic macros)
// ─────────────────────────────────────────────────────────────────

/**
 * ObligationSpec: Specification of obligations for a semantic macro.
 */
export type ObligationSpec = {
  /** Required obligation kinds */
  required: ObligationKind[];
  /** Optional obligation kinds */
  optional: ObligationKind[];
  /** Custom obligation descriptions */
  custom?: string[];
};

/**
 * ObligationKind: Kind of obligation.
 */
export type ObligationKind =
  | "test"
  | "metamorphic"
  | "invariant"
  | "type-check"
  | "contract"
  | "review";

/**
 * ObligationRef: Reference to an obligation.
 */
export type ObligationRef = {
  id: string;
  kind: ObligationKind;
  description?: string;
  satisfied: boolean;
  evidence?: EvidenceRef;
};

/**
 * EvidenceRef: Reference to evidence supporting an obligation.
 */
export type EvidenceRef = {
  id: string;
  kind: "test-pass" | "proof" | "review" | "inference";
  hash?: Hash;
  description?: string;
};

// ─────────────────────────────────────────────────────────────────
// Oracle Protocol Types (for semantic macro expansion)
// ─────────────────────────────────────────────────────────────────

/**
 * ReqExpand: Request to expand syntax (for Oracle protocol).
 */
export type ReqExpand = {
  tag: "ReqExpand";
  qstx: Syntax;
  envRef: Hash;
  mode: "1" | "*";
  profileName?: string;
};

/**
 * RespSyntax: Response with expanded syntax.
 */
export type RespSyntax = {
  tag: "RespSyntax";
  stx: Syntax;
  obligations?: ObligationRef[];
  trace?: ExpansionTrace;
};

// ─────────────────────────────────────────────────────────────────
// Macro Definition Types
// ─────────────────────────────────────────────────────────────────

/**
 * MacroDefinition: Parsed macro definition.
 */
export type MacroDefinition = {
  name: string;
  kind: "syntax-rules" | "proc-macro" | "semantic";
  transformer: MacroTransformer;
  defSite?: SourceLocation;
};

/**
 * SemanticMacroConfig: Configuration for semantic macro expansion.
 */
export type SemanticMacroConfig = {
  /** Allow inference during expansion */
  allowInfer: boolean;
  /** Allow rewrite proposals during expansion */
  allowRewrite: boolean;
  /** Require tests before commit */
  requireTests: boolean;
  /** Maximum inference calls */
  maxInferenceCalls: number;
  /** Timeout for expansion (ms) */
  timeout: number;
};

export const DEFAULT_SEMANTIC_CONFIG: SemanticMacroConfig = {
  allowInfer: true,
  allowRewrite: true,
  requireTests: true,
  maxInferenceCalls: 5,
  timeout: 30000,
};

// ─────────────────────────────────────────────────────────────────
// Profile Integration Types
// ─────────────────────────────────────────────────────────────────

/**
 * MacroProfile: Profile-specific macro expansion settings.
 */
export type MacroProfile = {
  name: string;
  /** Can expand pure macros */
  canExpandPure: boolean;
  /** Can expand semantic macros */
  canExpandSemantic: boolean;
  /** Can commit expanded definitions */
  canCommit: boolean;
  /** Can invoke inference */
  canInfer: boolean;
  /** Step budget */
  stepBudget: number;
};

/**
 * Default macro profiles.
 */
export const MACRO_PROFILES: Record<string, MacroProfile> = {
  "explore": {
    name: "explore",
    canExpandPure: true,
    canExpandSemantic: true,
    canCommit: false,
    canInfer: true,
    stepBudget: 1000,
  },
  "pragmatic": {
    name: "pragmatic",
    canExpandPure: true,
    canExpandSemantic: true,
    canCommit: true,
    canInfer: true,
    stepBudget: 5000,
  },
  "strict": {
    name: "strict",
    canExpandPure: true,
    canExpandSemantic: true,
    canCommit: true,
    canInfer: true,
    stepBudget: 10000,
  },
  "airgap": {
    name: "airgap",
    canExpandPure: true,
    canExpandSemantic: false,
    canCommit: false,
    canInfer: false,
    stepBudget: 100,
  },
  "macro/pure": {
    name: "macro/pure",
    canExpandPure: true,
    canExpandSemantic: false,
    canCommit: false,
    canInfer: false,
    stepBudget: 1000,
  },
  "macro/semantic": {
    name: "macro/semantic",
    canExpandPure: true,
    canExpandSemantic: true,
    canCommit: false,
    canInfer: true,
    stepBudget: 5000,
  },
};

/**
 * Get macro profile by name.
 */
export function getMacroProfile(name: string): MacroProfile {
  return MACRO_PROFILES[name] ?? MACRO_PROFILES["pragmatic"];
}
