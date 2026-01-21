/**
 * Effect kinds for capability calculus.
 * Closed set for predictable analysis.
 */
export type Effect =
  | "Pure"
  | "Oracle"
  | "Tool"
  | "Store"
  | "Sink"
  | "Source"
  | "Clock"
  | "Concurrency"
  | "Constraint"
  | "Nondet"
  | "Control";

/**
 * Type signature for primitives.
 */
export interface TypeSig {
  params: Array<{
    name: string;
    type: string;
    optional?: boolean;
  }>;
  returns: string;
}

/**
 * Cost model for budgeting.
 */
export interface CostModel {
  estimate?: {
    llmCalls?: number | string;
    tokens?: number | string;
    timeMs?: number | string;
    toolCalls?: number | string;
  };
  estimator?: (args: unknown[], ctx: unknown) => {
    llmCalls?: number;
    tokens?: number;
    timeMs?: number;
    toolCalls?: number;
  };
}

/**
 * Lowering rule describing how a primitive maps to IR.
 */
export interface LoweringRule {
  kind: "Intrinsic" | "MacroExpand" | "LowerHook";
  irTag?: string;
  hook?: string;
}

/**
 * Lint-time constraints derived from architecture spec.
 */
export interface LintConstraints {
  mustBeDominatedByBudget?: boolean;
  mustBeDominatedByTimeout?: boolean;
  requiresToolContract?: boolean;
  requiresSchema?: boolean;
}

/**
 * Documentation surface for primitives.
 */
export interface PrimitiveDoc {
  summary: string;
  detail?: string;
  laws?: string[];
  examples?: Array<{
    input: string;
    output: string;
    description?: string;
  }>;
}

export interface DeprecationInfo {
  since: string;
  replacedBy?: string;
  note?: string;
}

/**
 * Complete descriptor for a primitive operation.
 */
export interface PrimitiveDescriptor {
  id: string;
  layer: "FrameLisp" | "LambdaLLM" | "OmegaLLM" | "LambdaRLM";
  kind: "SpecialForm" | "Function" | "Macro" | "ProtocolMethod";
  signature: TypeSig;
  effects: Effect[];
  resources?: CostModel;
  doc: PrimitiveDoc;
  lowering?: LoweringRule;
  runtime?: { implementer?: string };
  constraints?: LintConstraints;
  version: string;
  deprecated?: DeprecationInfo;
}
