// src/core/macro/semantic.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Prompt 15: Semantic macros with inference and commit barriers

import type { Val } from "../eval/values";
import { VUnit, VTrue, VFalse } from "../eval/values";
import type { Syntax, Scope, SIdent, SList } from "../syntax/syntax";
import { isIdent, isList } from "../syntax/syntax";
import type { Env } from "../syntax/binding";
import type { Hash } from "../artifacts/hash";
import { sha256JSON } from "../artifacts/hash";
import type {
  MacroTransformer,
  MacroEnv,
  ExpansionResult,
  ExpansionContext,
  ObligationRef,
  ObligationKind,
  ObligationSpec,
  EvidenceRef,
  SemanticMacroConfig,
  MacroProfile,
  ReqExpand,
  RespSyntax,
} from "./types";
import { getMacroProfile, DEFAULT_SEMANTIC_CONFIG } from "./types";
import { makeSemanticTransformer, makeExpansionContext } from "./expander";
import { makeDefScope, makeIdent, makeList, makeAtom } from "./hygiene";

// ─────────────────────────────────────────────────────────────────
// Semantic Macro Expansion
// ─────────────────────────────────────────────────────────────────

/**
 * SemanticExpansionState: State during semantic macro expansion.
 */
export type SemanticExpansionState = {
  /** Configuration */
  config: SemanticMacroConfig;
  /** Inference calls made */
  inferenceCalls: number;
  /** Rewrite proposals collected */
  rewriteProposals: RewriteProposal[];
  /** Obligations collected */
  obligations: ObligationRef[];
  /** Evidence collected */
  evidence: EvidenceRef[];
  /** Profile constraints */
  profile: MacroProfile;
  /** Start time for timeout */
  startTime: number;
};

/**
 * RewriteProposal: A proposed expansion from inference.
 */
export type RewriteProposal = {
  id: string;
  expanded: Syntax;
  confidence: number;
  source: "inference" | "rewrite" | "template";
  obligations: ObligationRef[];
};

/**
 * Create semantic expansion state.
 */
export function createSemanticState(
  profileName: string,
  config: Partial<SemanticMacroConfig> = {}
): SemanticExpansionState {
  return {
    config: { ...DEFAULT_SEMANTIC_CONFIG, ...config },
    inferenceCalls: 0,
    rewriteProposals: [],
    obligations: [],
    evidence: [],
    profile: getMacroProfile(profileName),
    startTime: Date.now(),
  };
}

/**
 * Apply a semantic macro.
 */
export async function applySemanticMacro(
  transformer: Extract<MacroTransformer, { tag: "SemanticMacro" }>,
  input: Syntax,
  ctx: ExpansionContext,
  state: SemanticExpansionState
): Promise<ExpansionResult> {
  // Check profile constraints
  if (!state.profile.canExpandSemantic) {
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

  // Check timeout
  if (Date.now() - state.startTime > state.config.timeout) {
    return {
      expanded: input,
      obligations: [{
        id: `timeout-${Date.now()}`,
        kind: "contract",
        description: "Semantic macro expansion timed out",
        satisfied: false,
      }],
      evidence: [],
      semantic: true,
    };
  }

  // Apply the semantic transformer
  // This would typically invoke inference, but for now we return the input
  // with required obligations
  const obligations: ObligationRef[] = [];

  if (transformer.obligations) {
    for (const kind of transformer.obligations.required) {
      obligations.push(createObligation(kind, `Required by semantic macro ${transformer.name}`));
    }
    for (const kind of transformer.obligations.optional) {
      obligations.push(createObligation(kind, `Optional for semantic macro ${transformer.name}`));
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
// Inference Integration
// ─────────────────────────────────────────────────────────────────

/**
 * InferenceFn: Type for inference functions.
 */
export type InferenceFn = (prompt: Val) => Promise<Val>;

/**
 * RewriteFn: Type for rewrite functions.
 */
export type RewriteFn = (pattern: Val, replacement: Val) => Promise<Val>;

/**
 * Request inference for macro expansion.
 */
export async function requestInference(
  state: SemanticExpansionState,
  prompt: Val,
  inferenceFn?: InferenceFn
): Promise<Val | null> {
  // Check if inference is allowed
  if (!state.profile.canInfer) {
    return null;
  }

  // Check inference call limit
  if (state.inferenceCalls >= state.config.maxInferenceCalls) {
    return null;
  }

  state.inferenceCalls++;

  // If no inference function provided, return null
  if (!inferenceFn) {
    return null;
  }

  try {
    return await inferenceFn(prompt);
  } catch {
    return null;
  }
}

/**
 * Request a rewrite proposal.
 */
export async function requestRewrite(
  state: SemanticExpansionState,
  pattern: Syntax,
  replacement: Syntax,
  rewriteFn?: RewriteFn
): Promise<RewriteProposal | null> {
  if (!state.config.allowRewrite) {
    return null;
  }

  const proposal: RewriteProposal = {
    id: `rewrite-${state.rewriteProposals.length}`,
    expanded: replacement,
    confidence: 0.8,
    source: "rewrite",
    obligations: [
      createObligation("test", "Rewrite requires regression tests"),
    ],
  };

  state.rewriteProposals.push(proposal);
  return proposal;
}

// ─────────────────────────────────────────────────────────────────
// Obligation Management
// ─────────────────────────────────────────────────────────────────

let nextObligationId = 0;

/**
 * Create an obligation.
 */
export function createObligation(
  kind: ObligationKind,
  description?: string
): ObligationRef {
  return {
    id: `obligation-${nextObligationId++}`,
    kind,
    description,
    satisfied: false,
  };
}

/**
 * Create obligations from a specification.
 */
export function createObligationsFromSpec(spec: ObligationSpec): ObligationRef[] {
  const obligations: ObligationRef[] = [];

  for (const kind of spec.required) {
    obligations.push(createObligation(kind, `Required: ${kind}`));
  }

  for (const kind of spec.optional) {
    obligations.push(createObligation(kind, `Optional: ${kind}`));
  }

  if (spec.custom) {
    for (const desc of spec.custom) {
      obligations.push(createObligation("contract", desc));
    }
  }

  return obligations;
}

/**
 * Satisfy an obligation with evidence.
 */
export function satisfyObligation(
  obligation: ObligationRef,
  evidence: EvidenceRef
): ObligationRef {
  return {
    ...obligation,
    satisfied: true,
    evidence,
  };
}

/**
 * Check if all obligations are satisfied.
 */
export function allObligationsSatisfied(obligations: ObligationRef[]): boolean {
  return obligations.every(o => o.satisfied);
}

/**
 * Get unsatisfied obligations.
 */
export function getUnsatisfiedObligations(obligations: ObligationRef[]): ObligationRef[] {
  return obligations.filter(o => !o.satisfied);
}

/**
 * Reset obligation ID counter (for testing).
 */
export function resetObligationIds(): void {
  nextObligationId = 0;
}

// ─────────────────────────────────────────────────────────────────
// Evidence Management
// ─────────────────────────────────────────────────────────────────

let nextEvidenceId = 0;

/**
 * Create evidence reference.
 */
export function createEvidence(
  kind: EvidenceRef["kind"],
  description?: string,
  hash?: Hash
): EvidenceRef {
  return {
    id: `evidence-${nextEvidenceId++}`,
    kind,
    description,
    hash,
  };
}

/**
 * Reset evidence ID counter (for testing).
 */
export function resetEvidenceIds(): void {
  nextEvidenceId = 0;
}

// ─────────────────────────────────────────────────────────────────
// Commit Barriers
// ─────────────────────────────────────────────────────────────────

/**
 * CommitRequest: Request to commit a semantic expansion.
 */
export type CommitRequest = {
  expansion: ExpansionResult;
  profileName: string;
  obligations: ObligationRef[];
};

/**
 * CommitResult: Result of commit attempt.
 */
export type CommitResult =
  | { tag: "committed"; hash: Hash }
  | { tag: "denied"; reason: string; missingCaps?: string[] }
  | { tag: "pending"; unsatisfied: ObligationRef[] };

/**
 * Attempt to commit a semantic expansion.
 */
export function commitSemanticExpansion(
  request: CommitRequest,
  availableCaps: Set<string>
): CommitResult {
  const profile = getMacroProfile(request.profileName);

  // Check commit permission
  if (!profile.canCommit) {
    return {
      tag: "denied",
      reason: `Profile '${request.profileName}' does not allow commits`,
    };
  }

  // Check required capabilities
  const requiredCaps = ["cap.macro.commit"];
  const missingCaps = requiredCaps.filter(c => !availableCaps.has(c));
  if (missingCaps.length > 0) {
    return {
      tag: "denied",
      reason: "Missing required capabilities",
      missingCaps,
    };
  }

  // Check obligations
  const unsatisfied = getUnsatisfiedObligations(request.obligations);
  if (unsatisfied.length > 0) {
    return {
      tag: "pending",
      unsatisfied,
    };
  }

  // Generate commit hash
  const hash = sha256JSON({
    expanded: request.expansion.expanded,
    obligations: request.obligations.map(o => o.id),
    timestamp: Date.now(),
  });

  return {
    tag: "committed",
    hash,
  };
}

// ─────────────────────────────────────────────────────────────────
// Oracle Protocol Integration
// ─────────────────────────────────────────────────────────────────

/**
 * ExpandFunctions: Functions passed to handleReqExpand to avoid circular dependency.
 */
export type ExpandFunctions = {
  expand: (stx: Syntax, ctx: ExpansionContext) => ExpansionResult;
  macroexpand1: (stx: Syntax, ctx: ExpansionContext) => ExpansionResult;
};

/**
 * Handle ReqExpand from oracle protocol.
 *
 * Note: expandFns must be provided to avoid circular dependency.
 * If not provided, returns the input unchanged.
 */
export function handleReqExpand(
  req: ReqExpand,
  runtimeEnv: Env,
  macroEnv: MacroEnv,
  expandFns?: ExpandFunctions
): RespSyntax {
  const ctx = makeExpansionContext(runtimeEnv, macroEnv, req.profileName);

  // If no expand functions provided, return input unchanged
  if (!expandFns) {
    return {
      tag: "RespSyntax",
      stx: req.qstx,
      obligations: [],
    };
  }

  let result: ExpansionResult;
  if (req.mode === "1") {
    result = expandFns.macroexpand1(req.qstx, ctx);
  } else {
    result = expandFns.expand(req.qstx, ctx);
  }

  return {
    tag: "RespSyntax",
    stx: result.expanded,
    obligations: result.obligations,
    trace: result.trace,
  };
}

// ─────────────────────────────────────────────────────────────────
// Semantic Function Definition
// ─────────────────────────────────────────────────────────────────

/**
 * SemanticFunctionSpec: Specification for a semantic function.
 */
export type SemanticFunctionSpec = {
  name: string;
  params: string[];
  goal: GoalSpec;
  contract?: string;
  fallback?: Val;
};

/**
 * GoalSpec: Specification for a semantic goal.
 */
export type GoalSpec = {
  kind: string;
  forbid?: string[];
  require?: string[];
};

/**
 * Parse a defsemantic form.
 */
export function parseDefSemantic(stx: SList): SemanticFunctionSpec | null {
  // (defsemantic (name params...) :goal {...} :contract name :fallback expr)
  if (stx.items.length < 3) return null;

  const head = stx.items[0];
  if (!isIdent(head) || head.name !== "defsemantic") return null;

  const sig = stx.items[1];
  if (!isList(sig) || sig.items.length < 1) return null;

  const nameStx = sig.items[0];
  if (!isIdent(nameStx)) return null;

  const params: string[] = [];
  for (let i = 1; i < sig.items.length; i++) {
    const p = sig.items[i];
    if (!isIdent(p)) return null;
    params.push(p.name);
  }

  // Parse keyword arguments
  let goal: GoalSpec = { kind: "generic" };
  let contract: string | undefined;
  let fallback: Val | undefined;

  for (let i = 2; i < stx.items.length; i++) {
    const item = stx.items[i];
    if (isIdent(item)) {
      const kw = item.name;
      const nextItem = stx.items[i + 1];

      if (kw === ":goal" && nextItem && isList(nextItem)) {
        goal = parseGoalSpec(nextItem);
        i++;
      } else if (kw === ":contract" && nextItem && isIdent(nextItem)) {
        contract = nextItem.name;
        i++;
      } else if (kw === ":fallback" && nextItem) {
        // Fallback would be parsed as Val elsewhere
        i++;
      }
    }
  }

  return {
    name: nameStx.name,
    params,
    goal,
    contract,
    fallback,
  };
}

/**
 * Parse a goal specification.
 */
function parseGoalSpec(stx: SList): GoalSpec {
  const goal: GoalSpec = { kind: "generic" };

  for (let i = 0; i < stx.items.length; i++) {
    const item = stx.items[i];
    if (isIdent(item)) {
      const key = item.name;
      const nextItem = stx.items[i + 1];

      if (key === "kind" && nextItem && (nextItem.tag === "Atom" || isIdent(nextItem))) {
        goal.kind = nextItem.tag === "Atom" ? String(nextItem.value) : nextItem.name;
        i++;
      } else if (key === "forbid" && nextItem && isList(nextItem)) {
        goal.forbid = nextItem.items
          .filter(isIdent)
          .map(id => id.name);
        i++;
      } else if (key === "require" && nextItem && isList(nextItem)) {
        goal.require = nextItem.items
          .filter(isIdent)
          .map(id => id.name);
        i++;
      }
    }
  }

  return goal;
}

/**
 * Create a semantic function transformer.
 */
export function createSemanticFunction(
  spec: SemanticFunctionSpec,
  scopeCounter: { n: number }
): MacroTransformer {
  const defScope = makeDefScope(spec.name, scopeCounter);

  const obligations: ObligationSpec = {
    required: ["test"],
    optional: ["metamorphic"],
    custom: spec.contract ? [`Contract: ${spec.contract}`] : undefined,
  };

  // Create a placeholder proc
  const proc: Val = {
    tag: "Closure",
    params: spec.params,
    body: { tag: "Lit", value: VUnit },
    env: [],
  };

  return makeSemanticTransformer(proc, defScope, spec.name, obligations);
}

// ─────────────────────────────────────────────────────────────────
// Expansion Replay
// ─────────────────────────────────────────────────────────────────

/**
 * ExpansionReceipt: Receipt for replaying an expansion.
 */
export type ExpansionReceipt = {
  id: string;
  inputHash: Hash;
  outputHash: Hash;
  profileName: string;
  semantic: boolean;
  obligations: string[];
  timestamp: number;
};

/**
 * Create an expansion receipt.
 */
export function createExpansionReceipt(
  input: Syntax,
  result: ExpansionResult,
  profileName: string
): ExpansionReceipt {
  return {
    id: `receipt-${Date.now()}`,
    inputHash: sha256JSON(input),
    outputHash: sha256JSON(result.expanded),
    profileName,
    semantic: result.semantic,
    obligations: result.obligations.map(o => o.id),
    timestamp: Date.now(),
  };
}

/**
 * Verify an expansion against a receipt.
 */
export function verifyExpansionReceipt(
  input: Syntax,
  result: ExpansionResult,
  receipt: ExpansionReceipt
): boolean {
  const inputHash = sha256JSON(input);
  const outputHash = sha256JSON(result.expanded);

  return inputHash === receipt.inputHash && outputHash === receipt.outputHash;
}

// ─────────────────────────────────────────────────────────────────
// Pipeline Macro Support
// ─────────────────────────────────────────────────────────────────

/**
 * PipelineStage: A stage in a semantic pipeline.
 */
export type PipelineStage = {
  name: string;
  kind: "plan" | "fetch" | "act" | "verify";
  capabilities?: string[];
  snapshot?: boolean;
};

/**
 * Parse a pipeline macro definition.
 */
export function parsePipelineMacro(stx: SList): PipelineStage[] | null {
  // (pipeline (stage1 :kind plan) (stage2 :kind fetch) ...)
  if (!isList(stx) || stx.items.length < 2) return null;

  const head = stx.items[0];
  if (!isIdent(head) || head.name !== "pipeline") return null;

  const stages: PipelineStage[] = [];

  for (let i = 1; i < stx.items.length; i++) {
    const stageStx = stx.items[i];
    if (!isList(stageStx) || stageStx.items.length < 1) continue;

    const nameStx = stageStx.items[0];
    if (!isIdent(nameStx)) continue;

    const stage: PipelineStage = {
      name: nameStx.name,
      kind: "act",
    };

    // Parse stage options
    for (let j = 1; j < stageStx.items.length; j++) {
      const opt = stageStx.items[j];
      if (isIdent(opt) && opt.name.startsWith(":")) {
        const key = opt.name.slice(1);
        const val = stageStx.items[j + 1];

        if (key === "kind" && val && isIdent(val)) {
          const kindVal = val.name as PipelineStage["kind"];
          if (["plan", "fetch", "act", "verify"].includes(kindVal)) {
            stage.kind = kindVal;
          }
          j++;
        } else if (key === "snapshot" && val && val.tag === "Atom") {
          stage.snapshot = Boolean(val.value);
          j++;
        }
      }
    }

    stages.push(stage);
  }

  return stages;
}

// ─────────────────────────────────────────────────────────────────
// Reset State (for testing)
// ─────────────────────────────────────────────────────────────────

/**
 * Reset all semantic macro state.
 */
export function resetSemanticState(): void {
  resetObligationIds();
  resetEvidenceIds();
}
