/**
 * Effect kinds for capability calculus.
 * Closed set - all effects must be one of these.
 */
export type Effect =
  | "Pure"           // No effects, referentially transparent
  | "Oracle"         // LLM inference (requires OracleCap)
  | "Tool"           // External tool call (requires ToolCap + contract)
  | "Store"          // Persistent storage (requires StoreCap)
  | "Sink"           // Emit to stream/output (requires SinkCap)
  | "Source"         // Observe from input (requires SourceCap)
  | "Clock"          // Time access (requires ClockPort for determinism)
  | "Concurrency"    // Fiber spawning/scheduling
  | "Constraint"     // Constraint propagation
  | "Nondet"         // Nondeterministic choice
  | "Control";       // Control flow effects (bind, catch, loop)

/**
 * Type signature for primitives.
 * Initially string-based; can upgrade to full type system later.
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
 * Cost model for budget estimation.
 */
export interface CostModel {
  /**
   * Static cost estimate (or formula string).
   */
  estimate?: {
    llmCalls?: number | string;   // e.g., 1 or "promptTokens / 1000"
    tokens?: number | string;
    timeMs?: number | string;
    toolCalls?: number | string;
  };
  /**
   * Dynamic estimator function (for runtime estimation).
   */
  estimator?: (args: unknown[], ctx: unknown) => {
    llmCalls?: number;
    tokens?: number;
    timeMs?: number;
    toolCalls?: number;
  };
}

/**
 * Lowering rule: how surface form compiles to IR.
 */
export interface LoweringRule {
  kind: "Intrinsic" | "MacroExpand" | "LowerHook";
  irTag?: string;              // For Intrinsic: which FlowIR tag
  hook?: string;               // For LowerHook: hook function id
}

/**
 * Lint constraints for static analysis.
 */
export interface LintConstraints {
  mustBeDominatedByBudget?: boolean;
  mustBeDominatedByTimeout?: boolean;
  requiresToolContract?: boolean;
  requiresSchema?: boolean;
}

/**
 * Documentation for the primitive.
 */
export interface PrimitiveDoc {
  summary: string;
  detail?: string;
  laws?: string[];             // Equational laws (crucial for refactoring)
  examples?: Array<{
    input: string;
    output: string;
    description?: string;
  }>;
}

/**
 * Deprecation info.
 */
export interface DeprecationInfo {
  since: string;
  replacedBy?: string;
  note?: string;
}

/**
 * Complete primitive descriptor.
 */
export interface PrimitiveDescriptor {
  /** Canonical namespaced ID, e.g., "framelisp/infer" */
  id: string;

  /** Which layer this primitive belongs to */
  layer: "FrameLisp" | "LambdaLLM" | "OmegaLLM" | "LambdaRLM";

  /** Kind of primitive */
  kind: "SpecialForm" | "Function" | "Macro" | "ProtocolMethod";

  /** Type signature */
  signature: TypeSig;

  /** Effect requirements (closed set) */
  effects: Effect[];

  /** Cost model for budget planning */
  resources?: CostModel;

  /** Documentation */
  doc: PrimitiveDoc;

  /** How to compile to IR */
  lowering?: LoweringRule;

  /** Runtime implementation reference */
  runtime?: {
    implementer?: string;      // e.g., "omega-kernel/prim/add"
  };

  /** Lint constraints */
  constraints?: LintConstraints;

  /** Semantic version */
  version: string;

  /** Deprecation info if deprecated */
  deprecated?: DeprecationInfo;
}
