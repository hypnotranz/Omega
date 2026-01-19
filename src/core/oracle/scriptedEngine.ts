// src/core/oracle/scriptedEngine.ts
// SOURCE: LambdaLLM/ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-18.md
// Scripted oracle engine for deterministic testing and replay

import type { OracleReq, OracleResp } from "./protocol";
import type { MeaningVal } from "./meaning";

/**
 * An OracleSession is an async generator that:
 * - Yields OracleReq (requests to the runtime)
 * - Receives OracleResp (responses from runtime)
 * - Returns a final value (typically MeaningVal)
 */
export type OracleSession = AsyncGenerator<OracleReq, { tag: "Return"; value: MeaningVal }, OracleResp>;

export type SessionFactory = () => OracleSession;

/**
 * ScriptedOracleEngine wraps a session factory.
 * This allows deterministic, replayable oracle sessions.
 */
export class ScriptedOracleEngine {
  constructor(private readonly mk: SessionFactory) {}

  session(): OracleSession {
    return this.mk();
  }
}

/**
 * Helper: create a scripted oracle from an async generator function.
 *
 * Usage:
 * ```ts
 * const oracle = scripted(async function* () {
 *   // Request evaluation
 *   const evalResp = yield { tag: "ReqEval", qexpr: "(+ 1 2)", envRef };
 *
 *   // Request tests
 *   const testResp = yield { tag: "ReqTest", spec: tests, envRef };
 *
 *   // Return final meaning
 *   return { tag: "Return", value: meaning };
 * });
 * ```
 */
export function scripted(fn: SessionFactory): ScriptedOracleEngine {
  return new ScriptedOracleEngine(fn);
}

/**
 * Run an oracle session to completion, dispatching requests through a handler.
 * Returns the final MeaningVal.
 */
export async function runSession(
  session: OracleSession,
  handler: (req: OracleReq) => Promise<OracleResp>
): Promise<MeaningVal> {
  let next = await session.next(undefined as any);

  while (!next.done) {
    const req = next.value;
    const resp = await handler(req);
    next = await session.next(resp);
  }

  return next.value.value;
}

/**
 * Record a session's request/response pairs for replay.
 */
export type SessionTranscript = Array<{ req: OracleReq; resp: OracleResp }>;

export async function recordSession(
  session: OracleSession,
  handler: (req: OracleReq) => Promise<OracleResp>
): Promise<{ result: MeaningVal; transcript: SessionTranscript }> {
  const transcript: SessionTranscript = [];
  let next = await session.next(undefined as any);

  while (!next.done) {
    const req = next.value;
    const resp = await handler(req);
    transcript.push({ req, resp });
    next = await session.next(resp);
  }

  return { result: next.value.value, transcript };
}

/**
 * Replay a session from a transcript (deterministic).
 */
export function replaySession(transcript: SessionTranscript): SessionFactory {
  return async function* () {
    let i = 0;
    for (const { req, resp } of transcript) {
      const actualResp = yield req;
      // In replay mode, we ignore the actual response and use the recorded one
      // But we could assert they match for validation
      i++;
    }
    // The transcript doesn't include the final return, so this is a limitation
    throw new Error("Replay session reached end without return");
  };
}
