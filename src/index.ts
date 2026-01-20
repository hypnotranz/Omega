// src/index.ts
// Omega Language - Public API
//
// Clean interface for VS Code extensions, CLI tools, and external integrations.

// ═══════════════════════════════════════════════════════════════════════════════
// CORE RUNTIME
// ═══════════════════════════════════════════════════════════════════════════════

export { OmegaRuntime, evalOmegaCode, type OmegaConfig, type EvalResult, type InferResult } from "./runtime";

// ═══════════════════════════════════════════════════════════════════════════════
// FRAME IR PACKAGE
// ═══════════════════════════════════════════════════════════════════════════════

export {
  canonicalJson,
  merkleHash,
  merkleize,
  type FrameValue,
  type FlowIR,
  type FlowStepIR,
  type MerkleArrayNode,
  type MerkleNode,
  type MerkleObjectEntry,
  type MerkleObjectNode,
  type MerkleValueNode,
  type MessageIR,
  type PromptIR,
  type ScalarIR,
  type ValueIR,
} from "./frameir";

// ═══════════════════════════════════════════════════════════════════════════════
// VALUES & TYPES
// ═══════════════════════════════════════════════════════════════════════════════

export type { Val } from "./core/eval/values";
export type { DistVal, DistItem } from "./core/eval/dist";
export { dist, distNormalize, distSample, distTopK, distFrom, isDist } from "./core/eval/dist";

// ═══════════════════════════════════════════════════════════════════════════════
// ORACLE PROTOCOL
// ═══════════════════════════════════════════════════════════════════════════════

export type { OracleReq, OracleResp, Meaning, QExpr } from "./core/oracle/protocol";
export type { OracleAdapter, OracleInit } from "./core/oracle/adapter";
export type { MeaningVal } from "./core/oracle/meaning";
export { meaning, isMeaning } from "./core/oracle/meaning";
export { matchAST } from "./core/oracle/match";

// ═══════════════════════════════════════════════════════════════════════════════
// ADAPTERS (LLM Backends)
// ═══════════════════════════════════════════════════════════════════════════════

export type { LLMConfig, AdapterCaps, ToolDef, OracleAdapterWithCaps } from "./core/oracle/adapters/types";
export { DepthTrackingAdapter, TracingAdapter } from "./core/oracle/adapters/types";
export { AnthropicAdapter, createAnthropicAdapter } from "./core/oracle/adapters/anthropicAdapter";
export { MCPClientAdapter, OmegaMCPServer, BidirectionalMCPAdapter } from "./core/oracle/adapters/mcpAdapter";

// ═══════════════════════════════════════════════════════════════════════════════
// GOVERNANCE
// ═══════════════════════════════════════════════════════════════════════════════

export type { Cap, CapSet } from "./core/governance/caps";
export { capHas, capRequire, DEFAULT_CAPS, FULL_CAPS } from "./core/governance/caps";
export type { BudgetLimits, Budget } from "./core/governance/budgets";
export { budgetDefault, budgetRemaining, budgetConsumeOracleTurn, budgetConsumeEvalStep, budgetConsumeToolCall } from "./core/governance/budgets";
export type { Profile, TruthRegime } from "./core/governance/profile";
export { PROFILE_SPECULATIVE, PROFILE_TEST_CERTIFIED, PROFILE_PROOF_CERTIFIED, DEFAULT_PROFILE } from "./core/governance/profile";

// ═══════════════════════════════════════════════════════════════════════════════
// CONTEXT (Env = Ctx)
// ═══════════════════════════════════════════════════════════════════════════════

export type { Ctx, Constraint, Evidence } from "./core/ctx/ctx";
export {
  ctxDefine,
  ctxLookup,
  ctxExtend,
  ctxSeal,
  ctxAddEvidence,
  ctxApplyProfile,
  ctxRootFromProfile,
  ctxProject,
  isCtx,
} from "./core/ctx/ctx";

// ═══════════════════════════════════════════════════════════════════════════════
// CONTEXT RECEIPTS (Snapshot/Compress/Hydrate)
// ═══════════════════════════════════════════════════════════════════════════════

export type { CtxReceipt } from "./core/oracle/ctxReceipts";
export { CtxReceiptRepo } from "./core/oracle/ctxReceipts";

// ═══════════════════════════════════════════════════════════════════════════════
// PROVENANCE
// ═══════════════════════════════════════════════════════════════════════════════

export { ProvenanceGraph } from "./core/provenance/graph";
export type { ProvenanceGraphData, StalenessReport, StaleItem, SourceChecker } from "./core/provenance/graph";
export { evidenceId, computeSourceHash } from "./core/provenance/evidence";
export { FileProvenanceStore } from "./core/provenance/store/file";
export type { ProvenanceStore, StoredReceipt, ReceiptFilter } from "./core/provenance/store/interface";
