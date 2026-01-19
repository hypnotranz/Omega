// src/core/oracle/portal.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-14.md

import type { OracleReq, OracleResp } from "./protocol";

export interface OraclePortal {
  perform(req: OracleReq): Promise<OracleResp>;
}
