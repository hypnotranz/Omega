import type { NodeBase } from "./meta";
import type { VRef } from "./value";

export interface ToolContractIR extends NodeBase {
  tag: "ToolContract";
  id: string;
  name: string;
  version: string;

  inputSchema: VRef;
  outputSchema: VRef;
  errorSchema?: VRef;

  idempotency: "idempotent" | "non-idempotent" | "unknown";
  capabilityTag: string;
  quotaGroup?: string;

  resourceModel?: {
    typicalTimeMs?: number;
    worstTimeMs?: number;
    typicalTokens?: number;
  };

  provenancePolicy?: {
    mustAttachEvidence?: boolean;
    evidenceMode?: Array<"observed" | "measured" | "derived">;
    stalenessInputs?: Array<"toolContract" | "schema" | "sourceFingerprint" | "oracleConfig">;
  };
}
