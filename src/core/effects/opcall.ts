// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-4.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import type { Val } from "../eval/values";
import type { State } from "../eval/machine";

export type Resumption = {
  readonly rid: string;
  readonly base: State;             // store MUST be snapshotted/persistent
  invoke(v: Val): State;            // inject v as effect result
  digest(): string;
};

export type OpCall = {
  readonly op: string;
  readonly args: Val[];
  readonly ctxDigest: string;
  readonly resumption: Resumption;
};