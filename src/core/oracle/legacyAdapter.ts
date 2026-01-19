// src/core/oracle/legacyAdapter.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-14.md
// Compatibility wrapper to convert old-style simple oracle to new session-based oracle

import type { OracleAdapter, OracleInit } from "./adapter";
import type { OracleSession, OracleResp, Meaning } from "./protocol";
import type { Val } from "../eval/values";

/**
 * Interface for the old-style simple oracle (just infer).
 * Used for backward compatibility with existing tests and LLM adapters.
 */
export interface LegacyOracleAdapter {
  infer(payload: Val, ctxDigest: string): Promise<Val>;
}

/**
 * Wraps a legacy (simple) oracle to work with the new session-based protocol.
 * The session just calls infer() once and returns a Meaning with the result.
 */
export class LegacyOracleWrapper implements OracleAdapter {
  constructor(private readonly legacy: LegacyOracleAdapter, private readonly ctxDigest = "legacy") {}

  startSession(init: OracleInit): OracleSession {
    const legacy = this.legacy;
    const ctxDigest = this.ctxDigest;

    return (async function* (): OracleSession {
      // For infer: just call the legacy infer and return
      if (init.tag === "Infer") {
        const result = await legacy.infer(init.payload, ctxDigest);
        const meaning: Meaning = {
          tag: "Meaning",
          denotation: result,
          confidence: 1.0,
        };
        yield { tag: "ReqReturn", result: meaning };
        return meaning;
      }

      // For apply: call infer with the args (basic compatibility)
      if (init.tag === "Apply") {
        const payload: Val = { tag: "Vector", items: init.args };
        const result = await legacy.infer(payload, ctxDigest);
        const meaning: Meaning = {
          tag: "Meaning",
          denotation: result,
          confidence: 1.0,
        };
        yield { tag: "ReqReturn", result: meaning };
        return meaning;
      }

      // Fallback
      const meaning: Meaning = { tag: "Meaning", confidence: 0 };
      yield { tag: "ReqReturn", result: meaning };
      return meaning;
    })();
  }
}
