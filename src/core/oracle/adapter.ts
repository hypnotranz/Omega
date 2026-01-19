// src/core/oracle/adapter.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-14.md

import type { Val } from "../eval/values";
import type { EnvRef, StateRef, OracleSession } from "./protocol";

export type OracleInit =
  | { tag: "Infer"; payload: Val; envRef: EnvRef; stateRef: StateRef; policyDigest?: string }
  | { tag: "Apply"; proc: Val; args: Val[]; envRef: EnvRef; stateRef: StateRef; policyDigest?: string };

export interface OracleAdapter {
  startSession(init: OracleInit): OracleSession;
}
