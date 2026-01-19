// src/core/oracle/driver.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-14.md

import type { OraclePortal } from "./portal";
import type { OracleSession, OracleReq, OracleResp, OracleReturn } from "./protocol";

export type OracleDriverOptions = {
  maxTurns: number;
};

const DEFAULT_OPTS: OracleDriverOptions = { maxTurns: 10_000 };

export async function runOracleSession(
  session: OracleSession,
  portal: OraclePortal,
  opts: OracleDriverOptions = DEFAULT_OPTS
): Promise<OracleReturn> {
  let resp: OracleResp = { tag: "RespAck" };

  for (let turns = 0; turns < opts.maxTurns; turns++) {
    const step = await session.next(resp);

    if (step.done) return step.value;

    // TypeScript doesn't narrow generator yields properly, so assert the type
    const req = step.value as OracleReq;

    // Spec: ReqReturn/ReqFail are "terminal requests"
    if (req.tag === "ReqReturn") return req.result;
    if (req.tag === "ReqFail") throw new Error(`Oracle failed: ${req.reason}`);

    resp = await portal.perform(req);
  }

  throw new Error(`Oracle session exceeded maxTurns=${opts.maxTurns}`);
}
