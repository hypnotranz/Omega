// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-8.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import type { Env as RTEnv } from "../eval/env";
import type { Store } from "../eval/store";

export type PhaseInstance = {
  env: RTEnv;     // runtime environment (internalName -> addr)
  store: Store;   // runtime store
};

export type InstanceCache = Map<string, Map<number, PhaseInstance>>; // moduleName -> phase -> instance