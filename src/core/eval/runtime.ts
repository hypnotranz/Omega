// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-4.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import type { State } from "./machine";
import type { OpCall } from "../effects/opcall";

export type DispatchResult = State | "Uncaught";

export interface Runtime {
  dispatch(state: State, opcall: OpCall): Promise<DispatchResult>;
}